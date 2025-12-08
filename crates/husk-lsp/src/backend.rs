//! LSP backend implementing the LanguageServer trait.

use std::collections::{HashMap, HashSet};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use dashmap::DashMap;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::{debug, info, instrument, warn};

use husk_lang::load::{
    ContentProvider, LoadError, ModuleGraph, assemble_root, load_graph_with_provider,
    module_path_to_file,
};
use husk_lexer::Token;
use husk_semantic::{SemanticOptions, SemanticResult};

use crate::diagnostics::{
    analyze_and_publish_diagnostics, parse_error_to_diagnostic, semantic_error_to_diagnostic,
};
use crate::document::Document;

/// Content provider that checks open documents first, then falls back to filesystem.
struct LspContentProvider<'a> {
    documents: &'a DashMap<Url, Document>,
}

impl ContentProvider for LspContentProvider<'_> {
    fn read_file(&self, path: &Path) -> std::result::Result<String, io::Error> {
        // Check open documents first
        if let Ok(uri) = Url::from_file_path(path) {
            if let Some(doc) = self.documents.get(&uri) {
                return Ok(doc.text().to_string());
            }
        }
        // Fall back to filesystem
        std::fs::read_to_string(path)
    }
}

/// The Husk LSP backend that handles all LSP requests.
pub struct HuskBackend {
    client: Client,
    documents: DashMap<Url, Document>,
    // Workspace state for multi-file support
    workspace_root: Arc<RwLock<Option<PathBuf>>>,
    entry_point: Arc<RwLock<Option<PathBuf>>>,
    module_graph: Arc<RwLock<Option<ModuleGraph>>>,
    semantic_result: Arc<RwLock<Option<SemanticResult>>>,
}

impl HuskBackend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
            workspace_root: Arc::new(RwLock::new(None)),
            entry_point: Arc::new(RwLock::new(None)),
            module_graph: Arc::new(RwLock::new(None)),
            semantic_result: Arc::new(RwLock::new(None)),
        }
    }

    /// Find project root and entry point by walking up from the given file path.
    /// Priority: 1) husk.toml (reads entry from config), 2) main.hk, 3) src/main.hk
    fn detect_project_entry(file_path: &Path) -> Option<PathBuf> {
        // Start from the file's directory
        let mut dir = file_path.parent()?;

        loop {
            // Priority 1: Check for husk.toml (explicit project marker)
            let husk_toml = dir.join("husk.toml");
            if husk_toml.exists() {
                // Try to read entry point from husk.toml
                if let Some(entry) = Self::read_entry_from_husk_toml(&husk_toml, dir) {
                    return Some(entry);
                }
                // If husk.toml exists but no entry specified, fall through to defaults
            }

            // Priority 2: Check for main.hk in this directory
            let main_hk = dir.join("main.hk");
            if main_hk.exists() {
                return Some(main_hk);
            }

            // Priority 3: Check for src/main.hk pattern
            let src_main = dir.join("src/main.hk");
            if src_main.exists() {
                return Some(src_main);
            }

            // Move up to parent directory
            dir = match dir.parent() {
                Some(p) => p,
                None => break,
            };
        }

        None
    }

    /// Read entry point from husk.toml [build] section
    fn read_entry_from_husk_toml(toml_path: &Path, project_root: &Path) -> Option<PathBuf> {
        let content = std::fs::read_to_string(toml_path).ok()?;

        // Simple TOML parsing for entry field - look for entry = "..."
        for line in content.lines() {
            let line = line.trim();
            // Check if this line defines the "entry" key (not "entry_point" etc.)
            if let Some(key) = line.split('=').next() {
                if key.trim() == "entry" {
                    if let Some(value) = line.split('=').nth(1) {
                        let value = value.trim().trim_matches('"');
                        let entry_path = project_root.join(value);
                        if entry_path.exists() {
                            return Some(entry_path);
                        }
                    }
                }
            }
        }

        None
    }

    /// Convert module path ["crate", "express"] to file URI
    fn module_path_to_uri(&self, module_path: &[String], root: &Path) -> Option<Url> {
        let file_path = module_path_to_file(root, module_path)?;
        Url::from_file_path(file_path).ok()
    }

    /// Analyze a multi-file project and publish diagnostics for all files
    async fn analyze_project(&self, entry: &Path) {
        debug!("analyze_project: starting for {:?}", entry);
        let provider = LspContentProvider {
            documents: &self.documents,
        };

        // Build module graph
        let graph = match load_graph_with_provider(entry, &provider) {
            Ok(g) => {
                debug!(
                    "Module graph loaded with {} modules: {:?}",
                    g.modules.len(),
                    g.modules.keys().collect::<Vec<_>>()
                );
                g
            }
            Err(err) => {
                debug!("Module graph failed to load: {:?}", err);
                self.publish_load_error(&err).await;
                return;
            }
        };

        // Assemble combined AST
        let merged_ast = match assemble_root(&graph) {
            Ok(f) => {
                debug!("Assembled root with {} items", f.items.len());
                f
            }
            Err(err) => {
                debug!("assemble_root failed: {:?}", err);
                self.publish_load_error(&err).await;
                return;
            }
        };

        // Run semantic analysis
        let semantic = husk_semantic::analyze_file_with_options(
            &merged_ast,
            SemanticOptions {
                prelude: true,
                cfg_flags: HashSet::new(),
            },
        );
        debug!(
            "Semantic analysis: {} symbol errors, {} type errors",
            semantic.symbols.errors.len(),
            semantic.type_errors.len()
        );

        // Get the project root (entry point's parent directory)
        // This is the same root used by load_graph_with_provider for module resolution
        let project_root = match entry.parent() {
            Some(p) => p.to_path_buf(),
            None => return,
        };

        // Publish diagnostics for ALL files in graph
        self.publish_project_diagnostics(&graph, &semantic, &project_root)
            .await;

        // Cache graph and semantic result for hover/goto-definition
        *self.module_graph.write().await = Some(graph);
        *self.semantic_result.write().await = Some(semantic);
    }

    /// Publish diagnostics for all files in the module graph
    async fn publish_project_diagnostics(
        &self,
        graph: &ModuleGraph,
        semantic: &SemanticResult,
        root: &Path,
    ) {
        // Collect diagnostics per file - start with empty diagnostics for all files
        let mut file_diagnostics: HashMap<Url, Vec<Diagnostic>> = HashMap::new();

        // Initialize empty diagnostics for all files in graph
        for (module_path, _) in &graph.modules {
            if let Some(uri) = self.module_path_to_uri(module_path, root) {
                file_diagnostics.insert(uri, Vec::new());
            }
        }

        // For now, publish all errors to the entry point file
        // TODO: Track source file info in semantic errors for precise location
        let entry = self.entry_point.read().await.clone();
        if let Some(entry_path) = entry {
            if let Ok(entry_uri) = Url::from_file_path(&entry_path) {
                // Get the entry file's text for span-to-position conversion
                let text = if let Some(doc) = self.documents.get(&entry_uri) {
                    doc.text().to_string()
                } else if let Ok(content) = std::fs::read_to_string(&entry_path) {
                    content
                } else {
                    String::new()
                };

                let mut diagnostics = Vec::new();

                // Convert symbol resolution errors
                for error in &semantic.symbols.errors {
                    diagnostics.push(semantic_error_to_diagnostic(&text, error));
                }

                // Convert type errors
                for error in &semantic.type_errors {
                    diagnostics.push(semantic_error_to_diagnostic(&text, error));
                }

                file_diagnostics.insert(entry_uri, diagnostics);
            }
        }

        // Publish to each file
        for (uri, diagnostics) in file_diagnostics {
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    /// Publish load errors as diagnostics
    async fn publish_load_error(&self, error: &LoadError) {
        match error {
            LoadError::Parse {
                path,
                errors,
                source_code,
            } => {
                // Show parse errors in the broken file
                if let Ok(error_uri) = Url::from_file_path(path) {
                    let diagnostics: Vec<_> = errors
                        .iter()
                        .map(|e| parse_error_to_diagnostic(source_code, e))
                        .collect();
                    self.client
                        .publish_diagnostics(error_uri, diagnostics, None)
                        .await;
                }
            }
            LoadError::Missing(module) => {
                // Show at workspace level via log message
                self.client
                    .log_message(MessageType::ERROR, format!("Module '{}' not found", module))
                    .await;
            }
            LoadError::Cycle(module) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("Circular dependency detected: {}", module),
                    )
                    .await;
            }
            LoadError::Io(path, msg) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("Failed to read '{}': {}", path, msg),
                    )
                    .await;
            }
        }
    }

    #[instrument(skip(self, text), fields(uri = %uri, version = version, text_len = text.len()))]
    async fn on_document_change(&self, uri: Url, text: String, version: i32) {
        debug!("Document changed");

        // Store the document
        self.documents
            .insert(uri.clone(), Document::new(text.clone(), version));

        // Get file path from URI
        let file_path = match uri.to_file_path() {
            Ok(p) => p,
            Err(_) => {
                // Can't get file path - fall back to single-file mode
                analyze_and_publish_diagnostics(&self.client, uri, &text).await;
                return;
            }
        };

        // Try to detect project entry point by walking up from file
        // Priority: husk.toml > main.hk > src/main.hk
        let entry = match Self::detect_project_entry(&file_path) {
            Some(e) => {
                debug!("Detected project entry: {:?}", e);
                e
            }
            None => {
                // No project found - fall back to single-file mode
                debug!(
                    "No husk project found for {:?}, using single-file mode",
                    file_path
                );
                analyze_and_publish_diagnostics(&self.client, uri, &text).await;
                return;
            }
        };

        // Cache the entry point
        *self.entry_point.write().await = Some(entry.clone());

        // Multi-file mode
        debug!("Running multi-file analysis with entry: {:?}", entry);
        self.analyze_project(&entry).await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for HuskBackend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Extract workspace root from InitializeParams
        let root = params
            .workspace_folders
            .as_ref()
            .and_then(|folders| folders.first())
            .and_then(|folder| folder.uri.to_file_path().ok())
            .or_else(|| params.root_uri.as_ref()?.to_file_path().ok());

        if let Some(root_path) = root {
            debug!("Workspace root: {:?}", root_path);
            *self.workspace_root.write().await = Some(root_path);
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
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

        // Find the word and its span at the position
        let offset = doc.position_to_offset(position);
        let (word, start, end) = match doc.word_span_at_offset(offset) {
            Some(w) => w,
            None => return Ok(None),
        };

        // Try to use cached semantic result first (from multi-file analysis)
        let semantic_result = self.semantic_result.read().await;
        if let Some(semantic) = semantic_result.as_ref() {
            if let Some(info) = semantic.hover_info.get(&(start, end)) {
                let parse_result = husk_parser::parse_str(&doc.text());
                let doc_map = build_doc_map(&parse_result.tokens);
                let markdown = format_hover_markdown(info, &doc_map);
                let range_start = doc.offset_to_position(start);
                let range_end = doc.offset_to_position(end);
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: markdown,
                    }),
                    range: Some(Range {
                        start: range_start,
                        end: range_end,
                    }),
                }));
            }
        }
        drop(semantic_result);

        // Fall back to single-file analysis
        let text = doc.text();
        let parse_result = husk_parser::parse_str(&text);

        let file = match parse_result.file {
            Some(f) => f,
            None => {
                // Fallback to just showing the word
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("```husk\n{}\n```", word),
                    }),
                    range: None,
                }));
            }
        };

        // Run semantic analysis to get type information
        let semantic_result = husk_semantic::analyze_file(&file);

        // Build doc map from tokens
        let doc_map = build_doc_map(&parse_result.tokens);

        // Look up hover info for this span
        if let Some(info) = semantic_result.hover_info.get(&(start, end)) {
            let markdown = format_hover_markdown(info, &doc_map);
            let range_start = doc.offset_to_position(start);
            let range_end = doc.offset_to_position(end);
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: markdown,
                }),
                range: Some(Range {
                    start: range_start,
                    end: range_end,
                }),
            }));
        }

        // Fallback: just show the word
        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("```husk\n{}\n```", word),
            }),
            range: None,
        }))
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

        // Try cross-file search first using cached module graph
        let graph = self.module_graph.read().await;
        let entry = self.entry_point.read().await.clone();
        // Get project root from entry point (same as used in module loading)
        let project_root = entry
            .as_ref()
            .and_then(|e| e.parent().map(|p| p.to_path_buf()));

        if let (Some(graph), Some(root)) = (graph.as_ref(), project_root) {
            // Search ALL modules for definition
            for (module_path, module) in &graph.modules {
                for item in &module.file.items {
                    if let Some((name_ident, span)) = item_definition_info(item, &word) {
                        if let Some(target_uri) = self.module_path_to_uri(module_path, &root) {
                            // Need to read the target file to convert spans
                            if let Some(target_doc) = self.documents.get(&target_uri) {
                                let start = target_doc.offset_to_position(span.range.start);
                                let end = target_doc.offset_to_position(span.range.end);
                                debug!(
                                    name = %name_ident,
                                    module = module_path.join("::"),
                                    "Found cross-file definition"
                                );
                                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                                    uri: target_uri,
                                    range: Range { start, end },
                                })));
                            } else {
                                // File not open, try to read it for position calculation
                                if let Some(file_path) = module_path_to_file(&root, module_path) {
                                    if let Ok(content) = std::fs::read_to_string(&file_path) {
                                        let temp_doc = Document::new(content, 0);
                                        let start = temp_doc.offset_to_position(span.range.start);
                                        let end = temp_doc.offset_to_position(span.range.end);
                                        debug!(
                                            name = %name_ident,
                                            module = module_path.join("::"),
                                            "Found cross-file definition (file not open)"
                                        );
                                        return Ok(Some(GotoDefinitionResponse::Scalar(
                                            Location {
                                                uri: target_uri,
                                                range: Range { start, end },
                                            },
                                        )));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        drop(graph);

        // Fall back to current file search
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
        debug!(
            item_count = file.items.len(),
            "Parsed file, searching for '{}'", word
        );

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

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = &params.text_document.uri;
        let position = params.position;

        // Get the document
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        // Find the word at the position
        let offset = doc.position_to_offset(position);
        let (word, start, end) = match doc.word_span_at_offset(offset) {
            Some(w) => w,
            None => return Ok(None),
        };

        // Check if this is a renamable symbol (not a keyword)
        if is_keyword(&word) {
            return Ok(None);
        }

        // Check if this word corresponds to a known symbol
        let semantic_result = self.semantic_result.read().await;
        if let Some(semantic) = semantic_result.as_ref() {
            // Check if there are any references for this symbol
            let has_references = semantic.references.iter().any(|((name, _), refs)| {
                name == &word && !refs.is_empty()
            });

            if !has_references {
                // Not a known symbol definition or reference
                return Ok(None);
            }
        }
        drop(semantic_result);

        // Return the range of the symbol that can be renamed
        let start_pos = doc.offset_to_position(start);
        let end_pos = doc.offset_to_position(end);

        Ok(Some(PrepareRenameResponse::Range(Range {
            start: start_pos,
            end: end_pos,
        })))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = &params.new_name;

        // Validate new name
        if !is_valid_identifier(new_name) {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(
                "Invalid identifier: must be alphanumeric with underscores, cannot start with digit",
            ));
        }

        // Get the document
        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        // Find the word at the position
        let offset = doc.position_to_offset(position);
        let (word, _, _) = match doc.word_span_at_offset(offset) {
            Some(w) => w,
            None => return Ok(None),
        };

        // Get semantic result with references
        let semantic_result = self.semantic_result.read().await;
        let semantic = match semantic_result.as_ref() {
            Some(s) => s,
            None => return Ok(None),
        };

        // Find all references for this symbol
        // We need to find the right ReferenceKind - try each one
        let reference_kinds = [
            husk_semantic::ReferenceKind::Function,
            husk_semantic::ReferenceKind::Struct,
            husk_semantic::ReferenceKind::Enum,
            husk_semantic::ReferenceKind::TypeAlias,
            husk_semantic::ReferenceKind::Trait,
            husk_semantic::ReferenceKind::Variable,
        ];

        let mut all_refs = Vec::new();
        for kind in &reference_kinds {
            if let Some(refs) = semantic.references.get(&(word.clone(), *kind)) {
                all_refs.extend(refs.iter().cloned());
            }
        }

        if all_refs.is_empty() {
            return Ok(None);
        }

        // Get entry point for cross-file resolution
        let entry = self.entry_point.read().await.clone();
        let project_root = entry.as_ref().and_then(|e| e.parent().map(|p| p.to_path_buf()));

        // Build text edits grouped by URI
        let mut changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();

        for reference in &all_refs {
            // Determine the URI for this reference
            let ref_uri = if let Some(ref module_path) = reference.module_path {
                // Cross-file reference - convert module path to URI
                if let Some(ref root) = project_root {
                    match self.module_path_to_uri(module_path, root) {
                        Some(u) => u,
                        None => uri.clone(), // Fall back to current file
                    }
                } else {
                    uri.clone()
                }
            } else {
                // Same file reference
                uri.clone()
            };

            // Get the document for this URI to convert spans to positions
            let ref_doc = if ref_uri == *uri {
                doc.clone()
            } else if let Some(d) = self.documents.get(&ref_uri) {
                d.clone()
            } else {
                // Try to read the file from disk
                if let Ok(path) = ref_uri.to_file_path() {
                    if let Ok(content) = std::fs::read_to_string(&path) {
                        Document::new(content, 0)
                    } else {
                        continue; // Skip if we can't read the file
                    }
                } else {
                    continue;
                }
            };

            let start_pos = ref_doc.offset_to_position(reference.span.range.start);
            let end_pos = ref_doc.offset_to_position(reference.span.range.end);

            let edit = TextEdit {
                range: Range {
                    start: start_pos,
                    end: end_pos,
                },
                new_text: new_name.clone(),
            };

            changes.entry(ref_uri).or_default().push(edit);
        }

        drop(semantic_result);

        Ok(Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }))
    }
}

/// Check if a word is a Husk keyword
fn is_keyword(word: &str) -> bool {
    matches!(
        word,
        "fn" | "let" | "mut" | "if" | "else" | "match" | "while" | "for" | "in"
            | "return" | "break" | "continue" | "struct" | "enum" | "trait" | "impl"
            | "pub" | "use" | "mod" | "extern" | "type" | "const" | "static"
            | "true" | "false" | "self" | "Self"
    )
}

/// Check if a string is a valid Husk identifier
fn is_valid_identifier(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    let first = s.chars().next().unwrap();
    if !first.is_alphabetic() && first != '_' {
        return false;
    }

    s.chars().all(|c| c.is_alphanumeric() || c == '_') && !is_keyword(s)
}

/// Extract definition info from an item if it matches the given name
fn item_definition_info<'a>(
    item: &'a husk_ast::Item,
    name: &str,
) -> Option<(&'a str, &'a husk_ast::Span)> {
    match &item.kind {
        husk_ast::ItemKind::Fn { name: n, .. } if n.name == name => Some((&n.name, &n.span)),
        husk_ast::ItemKind::Struct { name: n, .. } if n.name == name => Some((&n.name, &n.span)),
        husk_ast::ItemKind::Enum { name: n, .. } if n.name == name => Some((&n.name, &n.span)),
        husk_ast::ItemKind::TypeAlias { name: n, .. } if n.name == name => Some((&n.name, &n.span)),
        husk_ast::ItemKind::Trait(t) if t.name.name == name => Some((&t.name.name, &t.name.span)),
        husk_ast::ItemKind::ExternBlock { items, .. } => {
            // Search within extern block
            for ext_item in items {
                match &ext_item.kind {
                    husk_ast::ExternItemKind::Struct { name: n, .. } if n.name == name => {
                        return Some((&n.name, &n.span));
                    }
                    husk_ast::ExternItemKind::Fn { name: n, .. } if n.name == name => {
                        return Some((&n.name, &n.span));
                    }
                    _ => {}
                }
            }
            None
        }
        _ => None,
    }
}

/// Build a map from token start positions to concatenated doc comments.
/// Doc comments are `/// ...` style comments that appear before definitions.
fn build_doc_map(tokens: &[Token]) -> HashMap<usize, String> {
    let mut doc_map = HashMap::new();

    for token in tokens {
        let docs: Vec<&str> = token
            .leading_trivia
            .iter()
            .filter_map(|t| t.doc_content())
            .collect();

        if !docs.is_empty() {
            doc_map.insert(token.span.range.start, docs.join("\n"));
        }
    }

    doc_map
}

/// Format hover info as markdown (rust-analyzer style).
fn format_hover_markdown(
    info: &husk_semantic::HoverInfo,
    doc_map: &HashMap<usize, String>,
) -> String {
    let mut parts = Vec::new();

    // Code block with signature
    parts.push(format!("```husk\n{}\n```", info.signature));

    // Doc comments: first try definition_span lookup, then fall back to stored docs
    let docs = info
        .definition_span
        .as_ref()
        .and_then(|span| doc_map.get(&span.range.start))
        .or(info.docs.as_ref());

    if let Some(doc_text) = docs {
        parts.push("---".to_string());
        parts.push(doc_text.clone());
    }

    parts.join("\n\n")
}
