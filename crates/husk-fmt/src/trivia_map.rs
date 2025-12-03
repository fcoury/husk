//! TriviaMap: span-based lookup for comments and whitespace.

use husk_lexer::{Token, Trivia};
use std::collections::HashMap;

/// Maps AST spans to their associated trivia (comments, blank lines).
///
/// Trivia is attached to tokens during lexing. This map allows the formatter
/// to look up trivia by the span of an AST node, enabling comment preservation.
#[derive(Debug, Default)]
pub struct TriviaMap {
    /// Maps span start position to leading trivia
    leading: HashMap<usize, Vec<Trivia>>,
    /// Maps span end position to trailing trivia
    trailing: HashMap<usize, Vec<Trivia>>,
}

impl TriviaMap {
    /// Build a TriviaMap from a sequence of tokens.
    pub fn from_tokens(tokens: &[Token]) -> Self {
        let mut map = Self::default();

        for token in tokens {
            if !token.leading_trivia.is_empty() {
                map.leading
                    .insert(token.span.range.start, token.leading_trivia.clone());
            }
            if !token.trailing_trivia.is_empty() {
                map.trailing
                    .insert(token.span.range.end, token.trailing_trivia.clone());
            }
        }

        map
    }

    /// Get leading trivia for a position (comments/blank lines before this node).
    pub fn leading_at(&self, start: usize) -> &[Trivia] {
        self.leading.get(&start).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Get trailing trivia for a position (end-of-line comments after this node).
    pub fn trailing_at(&self, end: usize) -> &[Trivia] {
        self.trailing.get(&end).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Check if a position has leading comments.
    pub fn has_leading_comments_at(&self, start: usize) -> bool {
        self.leading_at(start).iter().any(|t| t.is_comment())
    }

    /// Check if a position has a trailing comment.
    pub fn has_trailing_comment_at(&self, end: usize) -> bool {
        self.trailing_at(end).iter().any(|t| t.is_comment())
    }

    /// Count blank lines before a position.
    pub fn leading_blank_lines_at(&self, start: usize) -> usize {
        let newline_count = self.leading_at(start).iter().filter(|t| t.is_newline()).count();
        // 2 newlines = 1 blank line, 3 = 2, etc.
        newline_count.saturating_sub(1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use husk_lexer::Lexer;

    #[test]
    fn test_trivia_map_from_tokens() {
        let src = "// comment\nfn main() {}";
        let tokens: Vec<Token> = Lexer::new(src).collect();
        let map = TriviaMap::from_tokens(&tokens);

        // The 'fn' token should have leading trivia
        let fn_token = &tokens[0];
        assert!(map.has_leading_comments_at(fn_token.span.range.start));
    }

    #[test]
    fn test_trivia_map_trailing() {
        let src = "fn main() {} // comment\n";
        let tokens: Vec<Token> = Lexer::new(src).collect();
        let map = TriviaMap::from_tokens(&tokens);

        // The '}' token should have trailing trivia
        let rbrace_token = tokens
            .iter()
            .find(|t| matches!(t.kind, husk_lexer::TokenKind::RBrace))
            .unwrap();
        assert!(map.has_trailing_comment_at(rbrace_token.span.range.end));
    }
}
