//! TypeScript declaration file (.d.ts) parser for Husk.
//!
//! This crate provides a recursive descent parser for TypeScript declaration
//! files, converting them into an AST that can be used to generate Husk
//! `extern "js"` declarations.

mod ast;
mod codegen;
mod lexer;
mod parser;

pub use ast::*;
pub use codegen::{generate, CodegenOptions, CodegenResult, Warning, WarningKind};
pub use lexer::{Lexer, Token, TokenKind};
pub use parser::{parse, ParseError, ParseResult};
