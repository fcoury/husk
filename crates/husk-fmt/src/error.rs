//! Error types for the formatter.

use thiserror::Error;

/// Errors that can occur during formatting.
#[derive(Debug, Error)]
pub enum FormatError {
    /// Source code failed to parse.
    #[error("parse error: {0:?}")]
    Parse(Vec<husk_parser::ParseError>),

    /// Parser returned no AST (unexpected state).
    #[error("parser produced no AST")]
    NoAst,

    /// I/O error reading or writing files.
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
}
