//! TypeScript declaration file (.d.ts) parser for Husk.
//!
//! This crate provides a recursive descent parser for TypeScript declaration
//! files, converting them into an AST that can be used to generate Husk
//! `extern "js"` declarations.

#![allow(
    clippy::collapsible_if,
    clippy::collapsible_else_if,
    clippy::len_zero,
    clippy::double_ended_iterator_last,
    clippy::manual_map
)]

mod ast;
mod codegen;
mod lexer;
mod oxc_convert;
mod oxc_frontend;
mod parser;
mod resolver;

pub use ast::*;
pub use codegen::{
    CodegenOptions, CodegenResult, Warning, WarningKind, generate, generate_simple,
    prepare_module_metadata,
};
pub use lexer::{Lexer, Token, TokenKind};
pub use oxc_convert::convert_program as convert_oxc_program;
pub use oxc_frontend::{OxcDtsParser, OxcProgram};
pub use parser::{ParseError, ParseResult, parse};
pub use resolver::{ModuleKind, ResolvedModule, resolve_module};

/// Dts generation diagnostics, currently a minimal placeholder.
#[derive(Default, Debug, Clone)]
pub struct DtsDiagnostics {
    pub warnings: Vec<String>,
}
