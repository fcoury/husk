//! Deterministic code formatter for the Husk language.
//!
//! This crate provides formatting functionality that preserves comments
//! and produces consistent, canonical output.

use husk_lexer::{Lexer, Token};
use husk_parser::parse_str;
use std::path::Path;

pub use config::FormatConfig;
pub use error::FormatError;

mod config;
mod error;
mod trivia_map;
mod visitor;

pub use trivia_map::TriviaMap;

/// Format source code, preserving comments and normalizing whitespace.
///
/// # Example
///
/// ```ignore
/// use husk_fmt::{format_str, FormatConfig};
///
/// let source = "fn   main(){}";
/// let formatted = format_str(source, &FormatConfig::default())?;
/// assert_eq!(formatted, "fn main() {\n}\n");
/// ```
pub fn format_str(source: &str, config: &FormatConfig) -> Result<String, FormatError> {
    // Collect tokens with trivia
    let tokens: Vec<Token> = Lexer::new(source).collect();
    let trivia_map = TriviaMap::from_tokens(&tokens);

    // Parse to AST
    let result = parse_str(source);
    if !result.errors.is_empty() {
        return Err(FormatError::Parse(result.errors));
    }

    let file = result.file.ok_or(FormatError::NoAst)?;

    // Format with trivia preservation
    let formatted = visitor::Formatter::new(config, &trivia_map).format_file(&file);

    Ok(formatted)
}

/// Format a file, preserving comments and normalizing whitespace.
pub fn format_file(path: &Path, config: &FormatConfig) -> Result<String, FormatError> {
    let source = std::fs::read_to_string(path).map_err(FormatError::Io)?;
    format_str(&source, config)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_simple_fn() {
        let source = "fn main() {}";
        let result = format_str(source, &FormatConfig::default());
        assert!(result.is_ok());
    }

    #[test]
    fn test_format_preserves_comments() {
        let source = "// comment\nfn main() {}";
        let result = format_str(source, &FormatConfig::default()).unwrap();
        assert!(result.contains("// comment"));
    }

    #[test]
    fn test_trailing_comment_does_not_eat_next_statement() {
        // Regression test: trailing comments were being placed after the newline,
        // causing them to concatenate with the next statement
        let source = r#"fn main() {
    let x = 10;  // comment
    assert(x == 10);
}"#;
        let result = format_str(source, &FormatConfig::default()).unwrap();
        // The comment should stay on the same line as the let statement
        assert!(result.contains("let x = 10; // comment"), "Trailing comment should be on same line as statement. Got:\n{}", result);
        // The assert should be on its own line, not concatenated with the comment
        assert!(result.contains("\n    assert(x == 10);"), "assert should be on its own line. Got:\n{}", result);
    }

    #[test]
    fn test_leading_comment_before_attributed_item() {
        // Regression test: leading comments before items with attributes were being lost
        // because trivia was attached to the first attribute's token, not the item's span
        let source = r#"// Test comment
#[test]
fn foo() {}"#;
        let result = format_str(source, &FormatConfig::default()).unwrap();
        assert!(result.contains("// Test comment"), "Leading comment should be preserved. Got:\n{}", result);
        assert!(result.starts_with("// Test comment"), "Leading comment should be at the start. Got:\n{}", result);
    }

    #[test]
    fn test_doc_comment_in_extern_block() {
        // Regression test: doc comments inside extern blocks were being lost
        // because ExternItem span started at the identifier, not the fn keyword
        let source = r#"extern "js" {
    /// Doc comment
    fn foo();
}"#;
        let result = format_str(source, &FormatConfig::default()).unwrap();
        assert!(result.contains("/// Doc comment"), "Doc comment should be preserved. Got:\n{}", result);
    }

    #[test]
    fn test_doc_comment_before_pub_item() {
        // Regression test: doc comments before pub items were being lost
        // because the item span started at struct/enum/fn, not at the pub keyword
        let source = r#"/// A user struct
pub struct User {
    name: String,
}"#;
        let result = format_str(source, &FormatConfig::default()).unwrap();
        assert!(result.contains("/// A user struct"), "Doc comment should be preserved. Got:\n{}", result);
    }
}
