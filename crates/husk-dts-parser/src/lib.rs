//! TypeScript declaration file (.d.ts) parser for Husk.
//!
//! This crate provides a recursive descent parser for TypeScript declaration
//! files, converting them into an AST that can be used to generate Husk
//! `extern "js"` declarations.

mod ast;
pub mod builder;
mod codegen;
pub mod diagnostics;
pub mod generation_gap;
mod lexer;
pub mod oxc_parser;
mod parser;
pub mod resolver;
pub mod unions;
pub mod utility_types;

pub use ast::*;
pub use codegen::{
    CodegenOptions, CodegenResult, UnionStrategy, Warning, WarningKind, generate,
    generate_from_module,
};
pub use lexer::{Lexer, Token, TokenKind};
pub use parser::{ParseError, ParseResult, parse};
pub use resolver::{ModuleIdentity, ResolvedModule};
