//! TypeScript declaration file (.d.ts) parser for Husk.
//!
//! This crate provides a recursive descent parser for TypeScript declaration
//! files, converting them into an AST that can be used to generate Husk
//! `extern "js"` declarations.

mod ast;
pub mod builder;
mod codegen;
pub mod diagnostics;
mod lexer;
pub mod oxc_parser;
mod parser;
pub mod resolver;
pub mod unions;
pub mod utility_types;

pub use ast::*;
pub use codegen::{generate, CodegenOptions, CodegenResult, Warning, WarningKind};
pub use lexer::{Lexer, Token, TokenKind};
pub use parser::{parse, ParseError, ParseResult};
