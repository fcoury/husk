//! LSP backend implementing the LanguageServer trait.

use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::{debug, info, instrument, warn};

use crate::diagnostics::analyze_and_publish_diagnostics;
use crate::document::Document;

/// The Husk LSP backend that handles all LSP requests.
pub struct HuskBackend {
    client: Client,
    documents: DashMap<Url, Document>,
}

impl HuskBackend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
        }
    }

    #[instrument(skip(self, text), fields(uri = %uri, version = version, text_len = text.len()))]
    async fn on_document_change(&self, uri: Url, text: String, version: i32) {
        debug!("Document changed");

        // Store the document
        self.documents
            .insert(uri.clone(), Document::new(text.clone(), version));

        // Analyze and publish diagnostics
        analyze_and_publish_diagnostics(&self.client, uri, &text).await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for HuskBackend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "husk-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("Husk LSP server initialized");
        self.client
            .log_message(MessageType::INFO, "Husk LSP server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let version = params.text_document.version;
        self.on_document_change(uri, text, version).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        // With FULL sync, we get the entire document content
        if let Some(change) = params.content_changes.into_iter().last() {
            self.on_document_change(uri, change.text, version).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;

        // Remove document from our store
        self.documents.remove(&uri);

        // Clear diagnostics for this file
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        // Get the document
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        // Find the word at the position
        let offset = doc.position_to_offset(position);
        let word = doc.word_at_offset(offset);

        if let Some(word) = word {
            // For now, just return the word as hover info
            // TODO: Look up type information from semantic analysis
            Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("```husk\n{}\n```", word),
                }),
                range: None,
            }))
        } else {
            Ok(None)
        }
    }

    #[instrument(skip(self, params), fields(
        uri = %params.text_document_position_params.text_document.uri,
        line = params.text_document_position_params.position.line,
        character = params.text_document_position_params.position.character
    ))]
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        debug!("goto_definition request");

        // Get the document
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => {
                warn!("Document not found in store");
                return Ok(None);
            }
        };

        // Find the word at the position
        let offset = doc.position_to_offset(position);
        debug!(offset = offset, "Computed byte offset");

        let word = match doc.word_at_offset(offset) {
            Some(w) => {
                debug!(word = %w, "Found word at cursor");
                w
            }
            None => {
                debug!("No word found at cursor position");
                return Ok(None);
            }
        };

        // Parse and analyze to find definitions
        let text = doc.text();
        let parse_result = husk_parser::parse_str(&text);

        if parse_result.file.is_none() {
            warn!(
                errors = parse_result.errors.len(),
                "Parse failed, no AST available"
            );
            for err in &parse_result.errors {
                debug!(error = ?err, "Parse error");
            }
            return Ok(None);
        }

        let file = parse_result.file.unwrap();
        debug!(item_count = file.items.len(), "Parsed file, searching for '{}'", word);

        // Look for function/struct definitions matching the word
        for item in &file.items {
            match &item.kind {
                husk_ast::ItemKind::Fn { name, .. } if name.name == word => {
                    debug!(name = %name.name, "Found matching function definition");
                    let start = doc.offset_to_position(name.span.range.start);
                    let end = doc.offset_to_position(name.span.range.end);
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri: uri.clone(),
                        range: Range { start, end },
                    })));
                }
                husk_ast::ItemKind::Struct { name, .. } if name.name == word => {
                    debug!(name = %name.name, "Found matching struct definition");
                    let start = doc.offset_to_position(name.span.range.start);
                    let end = doc.offset_to_position(name.span.range.end);
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri: uri.clone(),
                        range: Range { start, end },
                    })));
                }
                husk_ast::ItemKind::Enum { name, .. } if name.name == word => {
                    debug!(name = %name.name, "Found matching enum definition");
                    let start = doc.offset_to_position(name.span.range.start);
                    let end = doc.offset_to_position(name.span.range.end);
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri: uri.clone(),
                        range: Range { start, end },
                    })));
                }
                husk_ast::ItemKind::TypeAlias { name, .. } if name.name == word => {
                    debug!(name = %name.name, "Found matching type alias definition");
                    let start = doc.offset_to_position(name.span.range.start);
                    let end = doc.offset_to_position(name.span.range.end);
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri: uri.clone(),
                        range: Range { start, end },
                    })));
                }
                husk_ast::ItemKind::Trait(t) if t.name.name == word => {
                    debug!(name = %t.name.name, "Found matching trait definition");
                    let start = doc.offset_to_position(t.name.span.range.start);
                    let end = doc.offset_to_position(t.name.span.range.end);
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri: uri.clone(),
                        range: Range { start, end },
                    })));
                }
                _ => {}
            }
        }

        debug!("No definition found for '{}'", word);
        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;

        // Get the document
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        let text = doc.text();
        let parse_result = husk_parser::parse_str(&text);

        if let Some(file) = parse_result.file {
            let mut symbols = Vec::new();

            for item in &file.items {
                let (name, kind, span) = match &item.kind {
                    husk_ast::ItemKind::Fn { name, .. } => {
                        (name.name.clone(), SymbolKind::FUNCTION, &name.span)
                    }
                    husk_ast::ItemKind::Struct { name, .. } => {
                        (name.name.clone(), SymbolKind::STRUCT, &name.span)
                    }
                    husk_ast::ItemKind::Enum { name, .. } => {
                        (name.name.clone(), SymbolKind::ENUM, &name.span)
                    }
                    husk_ast::ItemKind::TypeAlias { name, .. } => {
                        (name.name.clone(), SymbolKind::TYPE_PARAMETER, &name.span)
                    }
                    husk_ast::ItemKind::Trait(t) => {
                        (t.name.name.clone(), SymbolKind::INTERFACE, &t.name.span)
                    }
                    husk_ast::ItemKind::Impl(_) => continue, // Skip impl blocks
                    husk_ast::ItemKind::ExternBlock { .. } => continue,
                    husk_ast::ItemKind::Use { .. } => continue,
                };

                let start = doc.offset_to_position(span.range.start);
                let end = doc.offset_to_position(span.range.end);

                #[allow(deprecated)]
                symbols.push(SymbolInformation {
                    name,
                    kind,
                    tags: None,
                    deprecated: None,
                    location: Location {
                        uri: uri.clone(),
                        range: Range { start, end },
                    },
                    container_name: None,
                });
            }

            Ok(Some(DocumentSymbolResponse::Flat(symbols)))
        } else {
            Ok(None)
        }
    }
}
