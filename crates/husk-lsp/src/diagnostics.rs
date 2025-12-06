//! Diagnostics conversion and publishing for LSP.
//!
//! Converts Husk parse and semantic errors to LSP diagnostics and publishes them.

use tower_lsp::Client;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Url};

use husk_parser::{ParseError, parse_str};
use husk_semantic::{SemanticError, analyze_file};

/// Analyze a document and publish diagnostics to the client.
pub async fn analyze_and_publish_diagnostics(client: &Client, uri: Url, text: &str) {
    let diagnostics = analyze_document(text);
    client.publish_diagnostics(uri, diagnostics, None).await;
}

/// Analyze a document and return LSP diagnostics.
fn analyze_document(text: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Parse the document
    let parse_result = parse_str(text);

    // Convert parse errors to diagnostics
    for error in &parse_result.errors {
        diagnostics.push(parse_error_to_diagnostic(text, error));
    }

    // If we got an AST, run semantic analysis
    if let Some(file) = &parse_result.file {
        let semantic_result = analyze_file(file);

        // Convert symbol resolution errors
        for error in &semantic_result.symbols.errors {
            diagnostics.push(semantic_error_to_diagnostic(text, error));
        }

        // Convert type errors
        for error in &semantic_result.type_errors {
            diagnostics.push(semantic_error_to_diagnostic(text, error));
        }
    }

    diagnostics
}

/// Convert a parse error to an LSP diagnostic.
pub fn parse_error_to_diagnostic(text: &str, error: &ParseError) -> Diagnostic {
    let range = span_to_range(text, error.span.range.start, error.span.range.end);

    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("husk".to_string()),
        message: error.message.clone(),
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Convert a semantic error to an LSP diagnostic.
pub fn semantic_error_to_diagnostic(text: &str, error: &SemanticError) -> Diagnostic {
    let range = span_to_range(text, error.span.range.start, error.span.range.end);

    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("husk".to_string()),
        message: error.message.clone(),
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Convert a byte span to an LSP range.
pub fn span_to_range(text: &str, start: usize, end: usize) -> Range {
    Range {
        start: offset_to_position(text, start),
        end: offset_to_position(text, end),
    }
}

/// Convert a byte offset to an LSP position.
fn offset_to_position(text: &str, offset: usize) -> Position {
    let offset = offset.min(text.len());

    let mut line = 0u32;
    let mut line_start = 0usize;

    for (i, byte) in text.bytes().enumerate() {
        if i == offset {
            break;
        }
        if byte == b'\n' {
            line += 1;
            line_start = i + 1;
        }
    }

    let character = (offset - line_start) as u32;

    Position { line, character }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_to_position() {
        let text = "fn main() {\n    let x = 42;\n}\n";

        // Offset 0 -> line 0, char 0
        assert_eq!(
            offset_to_position(text, 0),
            Position {
                line: 0,
                character: 0
            }
        );

        // Offset 3 -> line 0, char 3 (after "fn ")
        assert_eq!(
            offset_to_position(text, 3),
            Position {
                line: 0,
                character: 3
            }
        );

        // Offset 12 -> line 1, char 0 (start of line 1, after "\n")
        assert_eq!(
            offset_to_position(text, 12),
            Position {
                line: 1,
                character: 0
            }
        );

        // Offset 16 -> line 1, char 4 (the "l" in "let")
        assert_eq!(
            offset_to_position(text, 16),
            Position {
                line: 1,
                character: 4
            }
        );
    }

    #[test]
    fn test_parse_error_diagnostic() {
        let text = "fn main( {\n}\n";
        let diagnostics = analyze_document(text);

        assert!(!diagnostics.is_empty());
        assert_eq!(diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diagnostics[0].source, Some("husk".to_string()));
    }

    #[test]
    fn test_valid_document_no_diagnostics() {
        let text = "fn main() {\n    let x = 42;\n}\n";
        let diagnostics = analyze_document(text);

        // Should have no parse errors
        // (May have semantic errors if types aren't right, but structure is valid)
        let parse_errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("expected"))
            .collect();
        assert!(parse_errors.is_empty());
    }
}
