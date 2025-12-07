//! Lexer for TypeScript declaration files.

use std::iter::Peekable;
use std::str::CharIndices;

/// Token kinds for TypeScript declaration files.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Identifiers and literals
    Ident(String),
    StringLiteral(String),
    NumberLiteral(String),

    // Keywords
    Declare,
    Function,
    Interface,
    Class,
    Type,
    Namespace,
    Module,
    Export,
    Import,
    Const,
    Let,
    Var,
    Readonly,
    Extends,
    Implements,
    New,
    Typeof,
    Keyof,
    Infer,
    As,
    Is,
    From,
    Default,
    Static,
    Public,
    Private,
    Protected,
    Abstract,
    In,
    Out,
    This,

    // Primitive type keywords
    String_,
    Number_,
    Boolean_,
    Void_,
    Null_,
    Undefined_,
    Any_,
    Unknown_,
    Never_,
    Object_,
    Symbol_,
    BigInt_,
    True_,
    False_,

    // Punctuation
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    LAngle,    // <
    RAngle,    // >
    Comma,     // ,
    Semicolon, // ;
    Colon,     // :
    Dot,       // .
    DotDotDot, // ...
    Question,  // ?
    Pipe,      // |
    Amp,       // &
    Arrow,     // =>
    Eq,        // =
    Plus,      // +
    Minus,     // -
    Star,      // *
    Slash,     // /
    Backtick,  // `

    // Special
    TripleSlashRef(String), // /// <reference path="..." /> or /// <reference types="..." />
    Eof,
}

/// A token with its kind and position.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
}

impl Token {
    pub fn new(kind: TokenKind, start: usize, end: usize) -> Self {
        Self { kind, start, end }
    }
}

/// Lexer for TypeScript declaration files.
pub struct Lexer<'src> {
    src: &'src str,
    chars: Peekable<CharIndices<'src>>,
    pos: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            chars: src.char_indices().peekable(),
            pos: 0,
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn advance(&mut self) -> Option<char> {
        if let Some((pos, ch)) = self.chars.next() {
            self.pos = pos + ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_line_comment(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == '\n' {
                break;
            }
            self.advance();
        }
    }

    fn skip_block_comment(&mut self) {
        loop {
            match self.advance() {
                Some('*') if self.peek() == Some('/') => {
                    self.advance();
                    break;
                }
                None => break,
                _ => {}
            }
        }
    }

    fn read_string(&mut self, quote: char) -> String {
        let mut s = String::new();
        loop {
            match self.advance() {
                Some('\\') => {
                    if let Some(escaped) = self.advance() {
                        match escaped {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            'r' => s.push('\r'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            '\'' => s.push('\''),
                            _ => {
                                s.push('\\');
                                s.push(escaped);
                            }
                        }
                    }
                }
                Some(c) if c == quote => break,
                Some(c) => s.push(c),
                None => break,
            }
        }
        s
    }

    fn read_number(&mut self, first: char) -> String {
        let mut s = String::new();
        s.push(first);
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() || ch == '.' || ch == 'e' || ch == 'E' || ch == '-' || ch == '+'
            {
                s.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        s
    }

    fn read_ident(&mut self, first: char) -> String {
        let mut s = String::new();
        s.push(first);
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == '_' || ch == '$' {
                s.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        s
    }

    fn read_triple_slash_ref(&mut self) -> Option<String> {
        // We've already consumed "///"
        // Look for <reference path="..." /> or <reference types="..." />
        self.skip_whitespace();

        // Read until end of line
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if ch == '\n' {
                break;
            }
            self.advance();
        }
        let content = &self.src[start..self.pos];

        // Parse the reference
        if content.trim_start().starts_with("<reference") {
            // Extract path or types attribute
            if let Some(path_start) = content.find("path=\"") {
                let rest = &content[path_start + 6..];
                if let Some(path_end) = rest.find('"') {
                    return Some(format!("path:{}", &rest[..path_end]));
                }
            }
            if let Some(types_start) = content.find("types=\"") {
                let rest = &content[types_start + 7..];
                if let Some(types_end) = rest.find('"') {
                    return Some(format!("types:{}", &rest[..types_end]));
                }
            }
        }
        None
    }

    fn keyword_or_ident(&self, s: &str) -> TokenKind {
        match s {
            "declare" => TokenKind::Declare,
            "function" => TokenKind::Function,
            "interface" => TokenKind::Interface,
            "class" => TokenKind::Class,
            "type" => TokenKind::Type,
            "namespace" => TokenKind::Namespace,
            "module" => TokenKind::Module,
            "export" => TokenKind::Export,
            "import" => TokenKind::Import,
            "const" => TokenKind::Const,
            "let" => TokenKind::Let,
            "var" => TokenKind::Var,
            "readonly" => TokenKind::Readonly,
            "extends" => TokenKind::Extends,
            "implements" => TokenKind::Implements,
            "new" => TokenKind::New,
            "typeof" => TokenKind::Typeof,
            "keyof" => TokenKind::Keyof,
            "infer" => TokenKind::Infer,
            "as" => TokenKind::As,
            "is" => TokenKind::Is,
            "from" => TokenKind::From,
            "default" => TokenKind::Default,
            "static" => TokenKind::Static,
            "public" => TokenKind::Public,
            "private" => TokenKind::Private,
            "protected" => TokenKind::Protected,
            "abstract" => TokenKind::Abstract,
            "in" => TokenKind::In,
            "out" => TokenKind::Out,
            "this" => TokenKind::This,
            // Primitives
            "string" => TokenKind::String_,
            "number" => TokenKind::Number_,
            "boolean" => TokenKind::Boolean_,
            "void" => TokenKind::Void_,
            "null" => TokenKind::Null_,
            "undefined" => TokenKind::Undefined_,
            "any" => TokenKind::Any_,
            "unknown" => TokenKind::Unknown_,
            "never" => TokenKind::Never_,
            "object" => TokenKind::Object_,
            "symbol" => TokenKind::Symbol_,
            "bigint" => TokenKind::BigInt_,
            "true" => TokenKind::True_,
            "false" => TokenKind::False_,
            _ => TokenKind::Ident(s.to_string()),
        }
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            self.skip_whitespace();

            let start = self.pos;

            let ch = match self.advance() {
                Some(c) => c,
                None => return Token::new(TokenKind::Eof, start, start),
            };

            let kind = match ch {
                '(' => TokenKind::LParen,
                ')' => TokenKind::RParen,
                '{' => TokenKind::LBrace,
                '}' => TokenKind::RBrace,
                '[' => TokenKind::LBracket,
                ']' => TokenKind::RBracket,
                '<' => TokenKind::LAngle,
                '>' => TokenKind::RAngle,
                ',' => TokenKind::Comma,
                ';' => TokenKind::Semicolon,
                ':' => TokenKind::Colon,
                '?' => TokenKind::Question,
                '|' => TokenKind::Pipe,
                '&' => TokenKind::Amp,
                '+' => TokenKind::Plus,
                '-' => TokenKind::Minus,
                '*' => TokenKind::Star,
                '`' => TokenKind::Backtick,
                '.' => {
                    if self.peek() == Some('.') {
                        self.advance();
                        if self.peek() == Some('.') {
                            self.advance();
                            TokenKind::DotDotDot
                        } else {
                            // Just ".." - treat as two dots (error recovery)
                            TokenKind::Dot
                        }
                    } else {
                        TokenKind::Dot
                    }
                }
                '=' => {
                    if self.peek() == Some('>') {
                        self.advance();
                        TokenKind::Arrow
                    } else {
                        TokenKind::Eq
                    }
                }
                '/' => {
                    if self.peek() == Some('/') {
                        self.advance();
                        // Check for triple-slash reference
                        if self.peek() == Some('/') {
                            self.advance();
                            if let Some(reference) = self.read_triple_slash_ref() {
                                TokenKind::TripleSlashRef(reference)
                            } else {
                                self.skip_line_comment();
                                continue;
                            }
                        } else {
                            self.skip_line_comment();
                            continue;
                        }
                    } else if self.peek() == Some('*') {
                        self.advance();
                        self.skip_block_comment();
                        continue;
                    } else {
                        TokenKind::Slash
                    }
                }
                '"' | '\'' => {
                    let s = self.read_string(ch);
                    TokenKind::StringLiteral(s)
                }
                c if c.is_ascii_digit() => {
                    let n = self.read_number(c);
                    TokenKind::NumberLiteral(n)
                }
                c if c.is_ascii_alphabetic() || c == '_' || c == '$' => {
                    let ident = self.read_ident(c);
                    self.keyword_or_ident(&ident)
                }
                _ => {
                    // Skip unknown characters
                    continue;
                }
            };

            return Token::new(kind, start, self.pos);
        }
    }

    /// Collect all tokens into a vector.
    pub fn tokenize(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_function() {
        let src = "declare function add(a: number, b: number): number;";
        let tokens = Lexer::new(src).tokenize();

        assert_eq!(tokens[0].kind, TokenKind::Declare);
        assert_eq!(tokens[1].kind, TokenKind::Function);
        assert_eq!(tokens[2].kind, TokenKind::Ident("add".to_string()));
        assert_eq!(tokens[3].kind, TokenKind::LParen);
        assert_eq!(tokens[4].kind, TokenKind::Ident("a".to_string()));
        assert_eq!(tokens[5].kind, TokenKind::Colon);
        assert_eq!(tokens[6].kind, TokenKind::Number_);
    }

    #[test]
    fn test_interface() {
        let src = "interface Foo<T> extends Bar { x: string; }";
        let tokens = Lexer::new(src).tokenize();

        assert_eq!(tokens[0].kind, TokenKind::Interface);
        assert_eq!(tokens[1].kind, TokenKind::Ident("Foo".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::LAngle);
        assert_eq!(tokens[3].kind, TokenKind::Ident("T".to_string()));
        assert_eq!(tokens[4].kind, TokenKind::RAngle);
        assert_eq!(tokens[5].kind, TokenKind::Extends);
    }

    #[test]
    fn test_arrow_function_type() {
        let src = "(a: string) => void";
        let tokens = Lexer::new(src).tokenize();

        assert_eq!(tokens[0].kind, TokenKind::LParen);
        assert_eq!(tokens[1].kind, TokenKind::Ident("a".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::Colon);
        assert_eq!(tokens[3].kind, TokenKind::String_);
        assert_eq!(tokens[4].kind, TokenKind::RParen);
        assert_eq!(tokens[5].kind, TokenKind::Arrow);
        assert_eq!(tokens[6].kind, TokenKind::Void_);
    }

    #[test]
    fn test_union_type() {
        let src = "string | number | null";
        let tokens = Lexer::new(src).tokenize();

        assert_eq!(tokens[0].kind, TokenKind::String_);
        assert_eq!(tokens[1].kind, TokenKind::Pipe);
        assert_eq!(tokens[2].kind, TokenKind::Number_);
        assert_eq!(tokens[3].kind, TokenKind::Pipe);
        assert_eq!(tokens[4].kind, TokenKind::Null_);
    }

    #[test]
    fn test_triple_slash_reference() {
        let src = r#"/// <reference path="./types.d.ts" />
declare function foo(): void;"#;
        let tokens = Lexer::new(src).tokenize();

        assert_eq!(
            tokens[0].kind,
            TokenKind::TripleSlashRef("path:./types.d.ts".to_string())
        );
        assert_eq!(tokens[1].kind, TokenKind::Declare);
    }

    #[test]
    fn test_rest_params() {
        let src = "function foo(...args: string[]): void";
        let tokens = Lexer::new(src).tokenize();

        assert_eq!(tokens[0].kind, TokenKind::Function);
        assert_eq!(tokens[1].kind, TokenKind::Ident("foo".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::LParen);
        assert_eq!(tokens[3].kind, TokenKind::DotDotDot);
        assert_eq!(tokens[4].kind, TokenKind::Ident("args".to_string()));
    }

    #[test]
    fn test_string_literals() {
        let src = r#""hello" 'world'"#;
        let tokens = Lexer::new(src).tokenize();

        assert_eq!(
            tokens[0].kind,
            TokenKind::StringLiteral("hello".to_string())
        );
        assert_eq!(
            tokens[1].kind,
            TokenKind::StringLiteral("world".to_string())
        );
    }
}
