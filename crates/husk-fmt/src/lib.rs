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
}
