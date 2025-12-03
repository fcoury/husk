//! Lexical analysis: convert source text into a stream of tokens.

use std::ops::Range;

/// List of all Husk keywords.
pub const KEYWORDS: &[&str] = &[
    "as", "pub", "use", "fn", "let", "mod", "mut", "struct", "enum", "type", "extern", "if",
    "else", "while", "match", "return", "true", "false", "break", "continue", "trait", "impl",
    "for", "Self", "static", "in", "global",
];

/// Check if a string is a Husk reserved keyword.
pub fn is_keyword(name: &str) -> bool {
    KEYWORDS.contains(&name)
}

/// Check if a string is a valid Husk identifier.
///
/// A valid identifier:
/// - Starts with an ASCII letter or underscore
/// - Contains only ASCII alphanumeric characters or underscores
/// - Is not a reserved keyword
pub fn is_valid_identifier(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }
    let mut chars = name.chars();
    let first = chars.next().unwrap();
    if !first.is_ascii_alphabetic() && first != '_' {
        return false;
    }
    for ch in chars {
        if !ch.is_ascii_alphanumeric() && ch != '_' {
            return false;
        }
    }
    !is_keyword(name)
}

/// A span in the source file, represented as a byte range.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub range: Range<usize>,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { range: start..end }
    }
}

/// Language keywords (subset for the MVP).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    As,
    Pub,
    Use,
    Fn,
    Let,
    Mut,
    Mod,
    Struct,
    Enum,
    Type,
    Extern,
    If,
    Else,
    While,
    Match,
    Return,
    True,
    False,
    Break,
    Continue,
    Trait,
    Impl,
    For,
    In,
    SelfType, // `Self` keyword (capital S)
    Static,
    Global,
}

/// Token kinds produced by the lexer.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Ident(String),
    IntLiteral(String),
    FloatLiteral(String),
    StringLiteral(String),
    Keyword(Keyword),
    // Punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Colon,
    ColonColon,
    Semicolon,
    Dot,
    DotDot,   // ..  (exclusive range)
    DotDotEq, // ..= (inclusive range)
    Arrow,    // ->
    FatArrow, // =>
    Eq,       // =
    EqEq,     // ==
    Bang,     // !
    BangEq,   // !=
    Lt,       // <
    Gt,       // >
    Le,       // <=
    Ge,       // >=
    AndAnd,   // &&
    Amp,      // & (single ampersand for references/self receivers)
    OrOr,     // ||
    Pipe,     // | (single pipe for closures)
    Plus,
    PlusEq,   // +=
    Minus,
    MinusEq,  // -=
    Star,
    Slash,
    Percent,   // %
    PercentEq, // %=
    // Attribute-related tokens
    Hash,      // #
    LBracket,  // [
    RBracket,  // ]
    // End of input
    Eof,
}

/// A token with its kind and source span.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// Simple lexer over a UTF-8 string.
pub struct Lexer<'src> {
    src: &'src str,
    chars: std::str::CharIndices<'src>,
    peeked: Option<(usize, char)>,
    end: usize,
    finished: bool,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        let end = src.len();
        Self {
            src,
            chars: src.char_indices(),
            peeked: None,
            end,
            finished: false,
        }
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        if let Some(p) = self.peeked.take() {
            Some(p)
        } else {
            self.chars.next()
        }
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        if self.peeked.is_none() {
            self.peeked = self.chars.next();
        }
        self.peeked
    }

    fn make_span(&self, start: usize, end: usize) -> Span {
        Span::new(start, end)
    }

    fn consume_while<F>(&mut self, start: usize, mut pred: F) -> (Span, &'src str)
    where
        F: FnMut(char) -> bool,
    {
        let mut last = start;
        let mut saw_any = false;
        while let Some((idx, ch)) = self.peek() {
            if !pred(ch) {
                break;
            }
            saw_any = true;
            last = idx;
            self.bump();
        }
        let end = if saw_any { last + 1 } else { start + 1 };
        let span = self.make_span(start, end);
        let lexeme = &self.src[span.range.clone()];
        (span, lexeme)
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            let mut progressed = false;
            while let Some((_, ch)) = self.peek() {
                if ch.is_whitespace() {
                    progressed = true;
                    self.bump();
                } else {
                    break;
                }
            }
            // Line comment: //
            if let Some((start, '/')) = self.peek() {
                let mut clone = self.chars.clone();
                if let Some((_, '/')) = clone.next() {
                    // consume the first '/'
                    self.bump();
                    // consume the second '/'
                    self.bump();
                    // consume until end of line
                    while let Some((_, ch)) = self.peek() {
                        if ch == '\n' {
                            break;
                        }
                        self.bump();
                    }
                    continue;
                } else {
                    // not actually a comment
                    let _ = start;
                }
            }
            if !progressed {
                break;
            }
        }
    }

    fn classify_ident_or_keyword(&self, span: Span, text: &str) -> Token {
        let kind = match text {
            "as" => TokenKind::Keyword(Keyword::As),
            "pub" => TokenKind::Keyword(Keyword::Pub),
            "use" => TokenKind::Keyword(Keyword::Use),
            "fn" => TokenKind::Keyword(Keyword::Fn),
            "let" => TokenKind::Keyword(Keyword::Let),
            "mod" => TokenKind::Keyword(Keyword::Mod),
            "mut" => TokenKind::Keyword(Keyword::Mut),
            "struct" => TokenKind::Keyword(Keyword::Struct),
            "enum" => TokenKind::Keyword(Keyword::Enum),
            "type" => TokenKind::Keyword(Keyword::Type),
            "extern" => TokenKind::Keyword(Keyword::Extern),
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            "while" => TokenKind::Keyword(Keyword::While),
            "match" => TokenKind::Keyword(Keyword::Match),
            "break" => TokenKind::Keyword(Keyword::Break),
            "continue" => TokenKind::Keyword(Keyword::Continue),
            "return" => TokenKind::Keyword(Keyword::Return),
            "true" => TokenKind::Keyword(Keyword::True),
            "false" => TokenKind::Keyword(Keyword::False),
            "trait" => TokenKind::Keyword(Keyword::Trait),
            "impl" => TokenKind::Keyword(Keyword::Impl),
            "for" => TokenKind::Keyword(Keyword::For),
            "in" => TokenKind::Keyword(Keyword::In),
            "Self" => TokenKind::Keyword(Keyword::SelfType),
            "static" => TokenKind::Keyword(Keyword::Static),
            "global" => TokenKind::Keyword(Keyword::Global),
            _ => TokenKind::Ident(text.to_string()),
        };
        Token { kind, span }
    }

    fn lex_number(&mut self, start: usize, first_ch: char) -> Token {
        let (span, _text) = self.consume_while(start, |c| c.is_ascii_digit());
        let mut end = if span.range.start == span.range.end {
            // only first_ch
            start + first_ch.len_utf8()
        } else {
            span.range.end
        };

        // Check for decimal point followed by digits (float literal)
        let mut is_float = false;
        if let Some((dot_idx, '.')) = self.peek() {
            // Look ahead to see if there's a digit after the dot
            // We need to check if the next character after '.' is a digit
            let after_dot = self.src.get(dot_idx + 1..dot_idx + 2);
            if let Some(ch_str) = after_dot {
                if let Some(ch) = ch_str.chars().next() {
                    if ch.is_ascii_digit() {
                        // Consume the dot
                        self.bump();
                        // Consume the fractional digits
                        let (frac_span, _) = self.consume_while(dot_idx + 1, |c| c.is_ascii_digit());
                        end = frac_span.range.end;
                        is_float = true;
                    }
                }
            }
        }

        let full_span = Span::new(start, end);
        let lexeme = &self.src[full_span.range.clone()];
        Token {
            kind: if is_float {
                TokenKind::FloatLiteral(lexeme.to_string())
            } else {
                TokenKind::IntLiteral(lexeme.to_string())
            },
            span: full_span,
        }
    }

    fn lex_ident_or_keyword(&mut self, start: usize) -> Token {
        let (span, text) = self.consume_while(start, |c| c.is_alphanumeric() || c == '_');
        self.classify_ident_or_keyword(span, text)
    }

    fn lex_string(&mut self, start: usize) -> Token {
        // Assumes opening quote has already been consumed.
        let mut end = start;
        while let Some((idx, ch)) = self.bump() {
            if ch == '"' {
                end = idx + 1;
                break;
            }
        }
        let span = self.make_span(start, end);
        let text = &self.src[span.range.clone()];
        // Strip quotes if present
        let value = text
            .strip_prefix('"')
            .and_then(|s| s.strip_suffix('"'))
            .unwrap_or("")
            .to_string();
        Token {
            kind: TokenKind::StringLiteral(value),
            span,
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }
        self.skip_whitespace_and_comments();
        let (start, ch) = match self.bump() {
            Some(pair) => pair,
            None => {
                let span = Span::new(self.end, self.end);
                self.finished = true;
                return Some(Token {
                    kind: TokenKind::Eof,
                    span,
                });
            }
        };

        let token = match ch {
            c if c.is_ascii_alphabetic() || c == '_' => self.lex_ident_or_keyword(start),
            c if c.is_ascii_digit() => self.lex_number(start, c),
            '"' => self.lex_string(start),
            '(' => Token {
                kind: TokenKind::LParen,
                span: Span::new(start, start + 1),
            },
            ')' => Token {
                kind: TokenKind::RParen,
                span: Span::new(start, start + 1),
            },
            '{' => Token {
                kind: TokenKind::LBrace,
                span: Span::new(start, start + 1),
            },
            '}' => Token {
                kind: TokenKind::RBrace,
                span: Span::new(start, start + 1),
            },
            ',' => Token {
                kind: TokenKind::Comma,
                span: Span::new(start, start + 1),
            },
            ':' => {
                if let Some((idx2, ':')) = self.peek() {
                    // consume second ':'
                    self.bump();
                    Token {
                        kind: TokenKind::ColonColon,
                        span: Span::new(start, idx2 + 1),
                    }
                } else {
                    Token {
                        kind: TokenKind::Colon,
                        span: Span::new(start, start + 1),
                    }
                }
            }
            ';' => Token {
                kind: TokenKind::Semicolon,
                span: Span::new(start, start + 1),
            },
            '.' => {
                if let Some((idx2, '.')) = self.peek() {
                    self.bump(); // consume second '.'

                    if let Some((idx3, '=')) = self.peek() {
                        self.bump(); // consume '='
                        Token {
                            kind: TokenKind::DotDotEq,
                            span: Span::new(start, idx3 + 1),
                        }
                    } else {
                        Token {
                            kind: TokenKind::DotDot,
                            span: Span::new(start, idx2 + 1),
                        }
                    }
                } else {
                    Token {
                        kind: TokenKind::Dot,
                        span: Span::new(start, start + 1),
                    }
                }
            }
            '-' => {
                if let Some((idx2, '>')) = self.peek() {
                    // consume '>'
                    self.bump();
                    Token {
                        kind: TokenKind::Arrow,
                        span: Span::new(start, idx2 + 1),
                    }
                } else if let Some((idx2, '=')) = self.peek() {
                    self.bump();
                    Token {
                        kind: TokenKind::MinusEq,
                        span: Span::new(start, idx2 + 1),
                    }
                } else {
                    Token {
                        kind: TokenKind::Minus,
                        span: Span::new(start, start + 1),
                    }
                }
            }
            '=' => {
                if let Some((idx2, next)) = self.peek() {
                    match next {
                        '>' => {
                            self.bump();
                            Token {
                                kind: TokenKind::FatArrow,
                                span: Span::new(start, idx2 + 1),
                            }
                        }
                        '=' => {
                            self.bump();
                            Token {
                                kind: TokenKind::EqEq,
                                span: Span::new(start, idx2 + 1),
                            }
                        }
                        _ => Token {
                            kind: TokenKind::Eq,
                            span: Span::new(start, start + 1),
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Eq,
                        span: Span::new(start, start + 1),
                    }
                }
            }
            '+' => {
                if let Some((idx2, '=')) = self.peek() {
                    self.bump();
                    Token {
                        kind: TokenKind::PlusEq,
                        span: Span::new(start, idx2 + 1),
                    }
                } else {
                    Token {
                        kind: TokenKind::Plus,
                        span: Span::new(start, start + 1),
                    }
                }
            }
            '*' => Token {
                kind: TokenKind::Star,
                span: Span::new(start, start + 1),
            },
            '/' => Token {
                kind: TokenKind::Slash,
                span: Span::new(start, start + 1),
            },
            '%' => {
                if let Some((idx2, '=')) = self.peek() {
                    self.bump();
                    Token {
                        kind: TokenKind::PercentEq,
                        span: Span::new(start, idx2 + 1),
                    }
                } else {
                    Token {
                        kind: TokenKind::Percent,
                        span: Span::new(start, start + 1),
                    }
                }
            }
            '!' => {
                if let Some((idx2, '=')) = self.peek() {
                    self.bump();
                    Token {
                        kind: TokenKind::BangEq,
                        span: Span::new(start, idx2 + 1),
                    }
                } else {
                    Token {
                        kind: TokenKind::Bang,
                        span: Span::new(start, start + 1),
                    }
                }
            }
            '<' => {
                if let Some((idx2, '=')) = self.peek() {
                    self.bump();
                    Token {
                        kind: TokenKind::Le,
                        span: Span::new(start, idx2 + 1),
                    }
                } else {
                    Token {
                        kind: TokenKind::Lt,
                        span: Span::new(start, start + 1),
                    }
                }
            }
            '>' => {
                if let Some((idx2, '=')) = self.peek() {
                    self.bump();
                    Token {
                        kind: TokenKind::Ge,
                        span: Span::new(start, idx2 + 1),
                    }
                } else {
                    Token {
                        kind: TokenKind::Gt,
                        span: Span::new(start, start + 1),
                    }
                }
            }
            '&' => {
                if let Some((idx2, '&')) = self.peek() {
                    self.bump();
                    Token {
                        kind: TokenKind::AndAnd,
                        span: Span::new(start, idx2 + 1),
                    }
                } else {
                    // Single '&' for references and self receivers
                    Token {
                        kind: TokenKind::Amp,
                        span: Span::new(start, start + 1),
                    }
                }
            }
            '|' => {
                if let Some((idx2, '|')) = self.peek() {
                    self.bump();
                    Token {
                        kind: TokenKind::OrOr,
                        span: Span::new(start, idx2 + 1),
                    }
                } else {
                    // Single '|' for closure parameter delimiters
                    Token {
                        kind: TokenKind::Pipe,
                        span: Span::new(start, start + 1),
                    }
                }
            }
            '#' => Token {
                kind: TokenKind::Hash,
                span: Span::new(start, start + 1),
            },
            '[' => Token {
                kind: TokenKind::LBracket,
                span: Span::new(start, start + 1),
            },
            ']' => Token {
                kind: TokenKind::RBracket,
                span: Span::new(start, start + 1),
            },
            _ => {
                // Unknown character, skip for now; in the future we will emit diagnostics.
                Token {
                    kind: TokenKind::Eof,
                    span: Span::new(start, start + 1),
                }
            }
        };

        Some(token)
    }
}
