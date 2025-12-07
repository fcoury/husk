//! Experimental Oxc-backed parser wrapper for `.d.ts` files.
//!
//! This keeps the arena allocator alive for the returned AST, mirroring the
//! approach outlined in the v2 DTS importer plan. It currently just exposes
//! Oxc's `ParserReturn`; mapping into Husk AST/codegen will follow.

use oxc_allocator::Allocator;
use oxc_parser::{ParseOptions, Parser, ParserReturn};
use oxc_span::SourceType;

/// Thin wrapper owning the Oxc allocator so lifetimes stay valid while callers
/// inspect the parsed program.
#[derive(Default)]
pub struct OxcDtsParser {
    allocator: Allocator,
    source_type: SourceType,
}

impl OxcDtsParser {
    pub fn new() -> Self {
        Self {
            allocator: Allocator::default(),
            source_type: SourceType::d_ts(),
        }
    }

    /// Parse with default options; returns Oxc's AST plus accumulated errors.
    pub fn parse<'a>(&'a self, source: &'a str) -> ParserReturn<'a> {
        Parser::new(&self.allocator, source, self.source_type).parse()
    }

    /// Parse with custom parser options (e.g., Stage 3 decorators when needed).
    pub fn parse_with_options<'a>(
        &'a self,
        source: &'a str,
        options: ParseOptions,
    ) -> ParserReturn<'a> {
        Parser::new(&self.allocator, source, self.source_type)
            .with_options(options)
            .parse()
    }
}

/// Convenience alias for the parsed program.
pub type OxcProgram<'a> = oxc_ast::ast::Program<'a>;

#[cfg(test)]
mod tests {
    use super::*;
    use oxc_ast::ast::Statement;

    #[test]
    fn parses_simple_interface() {
        let parser = OxcDtsParser::new();
        let ret = parser.parse("declare interface Foo { bar: string; }");

        assert!(
            ret.errors.is_empty(),
            "Oxc should parse a trivial interface"
        );
        assert_eq!(ret.program.body.len(), 1);

        let interface_count = ret
            .program
            .body
            .iter()
            .filter(|stmt| matches!(stmt, Statement::TSInterfaceDeclaration(_)))
            .count();

        assert_eq!(
            interface_count, 1,
            "Should see one TS interface decl in the AST"
        );
    }
}
