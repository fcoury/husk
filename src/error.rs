use std::fmt;

use thiserror::Error;

use crate::span::Span;

#[derive(Error, Debug)]
pub enum Error {
    Semantic(String, Span),
    Parse(String, Span),
    Runtime(String, Span),
    Transpiler(String, Span),
}

#[allow(dead_code)]
impl Error {
    pub fn new_semantic(message: impl Into<String>, span: Span) -> Self {
        Error::Semantic(message.into(), span)
    }

    pub fn new_parse(message: impl Into<String>, span: Span) -> Self {
        Error::Parse(message.into(), span)
    }

    pub fn new_runtime(message: impl Into<String>, span: Span) -> Self {
        Error::Runtime(message.into(), span)
    }

    pub fn new_transpile(message: impl Into<String>, span: Span) -> Self {
        Error::Transpiler(message.into(), span)
    }

    pub fn pretty_print(&self, code: impl Into<String>) -> String {
        match self {
            Error::Semantic(message, span) => pretty_print(code, message, span),
            Error::Parse(message, span) => pretty_print(code, message, span),
            Error::Runtime(message, span) => pretty_print(code, message, span),
            Error::Transpiler(message, span) => pretty_print(code, message, span),
        }
    }

    fn span(&self) -> &Span {
        match self {
            Error::Semantic(_, span) => span,
            Error::Parse(_, span) => span,
            Error::Runtime(_, span) => span,
            Error::Transpiler(_, span) => span,
        }
    }

    fn message(&self) -> &str {
        match self {
            Error::Semantic(message, _) => message,
            Error::Parse(message, _) => message,
            Error::Runtime(message, _) => message,
            Error::Transpiler(message, _) => message,
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Semantic(message, span) => {
                write!(f, "Semantic error: {} at {:?}", message, span)
            }
            Error::Parse(message, span) => write!(f, "Parse error: {} at {:?}", message, span),
            Error::Runtime(message, span) => write!(f, "Runtime error: {} at {:?}", message, span),
            Error::Transpiler(message, span) => {
                write!(f, "Transpiler error: {} at {:?}", message, span)
            }
        }
    }
}

fn pretty_print(code: impl Into<String>, message: impl Into<String>, span: &Span) -> String {
    let code: String = code.into();
    let message: String = message.into();
    format!(
        "error: {}:{} - {}\n{}",
        span.line_number(&code),
        span.column_number(&code),
        message,
        span.pretty_print(&code)
    )
}
