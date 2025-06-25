use std::fmt;

use crate::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    #[allow(unused)]
    pub fn new(kind: TokenKind, start: usize, end: usize) -> Token {
        Token {
            kind,
            span: Span::new(start, end),
        }
    }
}

pub const EOF: Token = Token {
    kind: TokenKind::Eof,
    span: Span { start: 0, end: 0 },
};

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Function,
    Struct,
    Impl,
    Enum,
    Let,
    Use,
    Pub,
    Extern,
    Async,
    As,
    If,
    Else,
    Match,
    Loop,
    While,
    For,
    In,
    Break,
    Continue,
    Return,
    Identifier(String),
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Equals,
    DblEquals,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LSquare,
    RSquare,
    Comma,
    Arrow,
    FatArrow,
    Type(String),
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    PlusEquals,
    MinusEquals,
    StarEquals,
    SlashEquals,
    PercentEquals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
    BangEquals,
    DblAmpersand,
    DblPipe,
    Pipe,
    Colon,
    DblColon,
    Dot,
    DblDot,
    DblDotEquals,
    Underscore,
    Bang,
    Question,
    Error(String),
    Eof,
}

impl TokenKind {
    pub fn is_identifier(&self) -> bool {
        matches!(self, TokenKind::Identifier(_))
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            TokenKind::Function => "Function",
            TokenKind::Struct => "Struct",
            TokenKind::Impl => "Impl",
            TokenKind::Enum => "Enum",
            TokenKind::Let => "Let",
            TokenKind::Use => "Use",
            TokenKind::Pub => "Pub",
            TokenKind::Extern => "Extern",
            TokenKind::Async => "Async",
            TokenKind::As => "As",
            TokenKind::If => "If",
            TokenKind::Else => "Else",
            TokenKind::Match => "Match",
            TokenKind::Loop => "Loop",
            TokenKind::While => "While",
            TokenKind::For => "For",
            TokenKind::In => "In",
            TokenKind::Break => "Break",
            TokenKind::Continue => "Continue",
            TokenKind::Return => "Return",
            TokenKind::Identifier(s) => s,
            TokenKind::Int(i) => return write!(f, "{}", i),
            TokenKind::Float(fl) => return write!(f, "{}", fl),
            TokenKind::String(s) => s,
            TokenKind::Bool(b) => return write!(f, "{}", b),
            TokenKind::Equals => "=",
            TokenKind::DblEquals => "==",
            TokenKind::Semicolon => ";",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::LSquare => "[",
            TokenKind::RSquare => "]",
            TokenKind::Comma => ",",
            TokenKind::Arrow => "->",
            TokenKind::FatArrow => "=>",
            TokenKind::Type(s) => s,
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Asterisk => "*",
            TokenKind::Slash => "/",
            TokenKind::Percent => "%",
            TokenKind::PlusEquals => "+=",
            TokenKind::MinusEquals => "-=",
            TokenKind::StarEquals => "*=",
            TokenKind::SlashEquals => "/=",
            TokenKind::PercentEquals => "%=",
            TokenKind::LessThan => "<",
            TokenKind::LessThanEquals => "<=",
            TokenKind::GreaterThan => ">",
            TokenKind::GreaterThanEquals => ">=",
            TokenKind::BangEquals => "!=",
            TokenKind::DblAmpersand => "&&",
            TokenKind::DblPipe => "||",
            TokenKind::Pipe => "|",
            TokenKind::Colon => ":",
            TokenKind::DblColon => "::",
            TokenKind::Dot => ".",
            TokenKind::DblDot => "..",
            TokenKind::DblDotEquals => "..=",
            TokenKind::Underscore => "_",
            TokenKind::Bang => "!",
            TokenKind::Question => "?",
            TokenKind::Error(s) => s,
            TokenKind::Eof => "EOF",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
    start_position: usize,
}

impl Lexer {
    pub fn new(input: impl Into<String>) -> Lexer {
        let input = input.into();
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
            start_position: 0,
        };
        l.read_char();
        l
    }

    pub fn lex_all(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        tokens
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = Some(self.input.chars().nth(self.read_position).unwrap());
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input.chars().nth(self.read_position).unwrap())
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.ch {
            if c.is_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        // Token generation logic with line and column information
        self.skip_whitespace();
        self.start_position = self.position;
        let token = match self.ch {
            Some('=') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    self.create_token(TokenKind::DblEquals)
                } else if self.peek_char() == Some('>') {
                    self.read_char();
                    self.create_token(TokenKind::FatArrow)
                } else {
                    self.create_token(TokenKind::Equals)
                }
            }
            Some(';') => self.create_token(TokenKind::Semicolon),
            Some('(') => self.create_token(TokenKind::LParen),
            Some(')') => self.create_token(TokenKind::RParen),
            Some('{') => self.create_token(TokenKind::LBrace),
            Some('}') => self.create_token(TokenKind::RBrace),
            Some('[') => self.create_token(TokenKind::LSquare),
            Some(']') => self.create_token(TokenKind::RSquare),
            Some('>') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    self.create_token(TokenKind::GreaterThanEquals)
                } else {
                    self.create_token(TokenKind::GreaterThan)
                }
            }
            Some('<') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    self.create_token(TokenKind::LessThanEquals)
                } else {
                    self.create_token(TokenKind::LessThan)
                }
            }
            Some(',') => self.create_token(TokenKind::Comma),
            Some('+') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    self.create_token(TokenKind::PlusEquals)
                } else {
                    self.create_token(TokenKind::Plus)
                }
            }
            Some('-') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    self.create_token(TokenKind::MinusEquals)
                } else if self.peek_char() == Some('>') {
                    self.read_char();
                    self.create_token(TokenKind::Arrow)
                } else {
                    self.create_token(TokenKind::Minus)
                }
            }
            Some('*') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    self.create_token(TokenKind::StarEquals)
                } else {
                    self.create_token(TokenKind::Asterisk)
                }
            }
            Some(':') => {
                if self.peek_char() == Some(':') {
                    self.read_char();
                    self.create_token(TokenKind::DblColon)
                } else {
                    self.create_token(TokenKind::Colon)
                }
            }
            Some('/') => {
                if self.peek_char() == Some('/') {
                    while let Some(c) = self.ch {
                        if c == '\n' {
                            break;
                        }
                        self.read_char();
                    }
                    return self.next_token();
                } else if self.peek_char() == Some('*') {
                    while let Some(c) = self.ch {
                        if c == '*' && self.peek_char() == Some('/') {
                            self.read_char();
                            self.read_char();
                            break;
                        }
                        self.read_char();
                    }
                    return self.next_token();
                } else if self.peek_char() == Some('=') {
                    self.read_char();
                    self.create_token(TokenKind::SlashEquals)
                } else {
                    self.create_token(TokenKind::Slash)
                }
            }
            Some('%') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    self.create_token(TokenKind::PercentEquals)
                } else {
                    self.create_token(TokenKind::Percent)
                }
            }
            Some('.') => {
                let start = self.position;
                if self.peek_char() == Some('.') {
                    self.read_char();

                    if self.peek_char() == Some('=') {
                        self.read_char();
                        self.read_char();
                        return Token::new(TokenKind::DblDotEquals, start, self.position);
                    }

                    self.read_char();
                    return Token::new(TokenKind::DblDot, start, self.position);
                } else {
                    self.create_token(TokenKind::Dot)
                }
            }
            Some('"') => self.read_string(),
            Some('_') => {
                if self.is_identifier_start(self.peek_char()) {
                    self.read_identifier_or_type()
                } else {
                    self.create_token(TokenKind::Underscore)
                }
            }
            Some('!') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    self.create_token(TokenKind::BangEquals)
                } else {
                    self.create_token(TokenKind::Bang)
                }
            }
            Some('&') => {
                if self.peek_char() == Some('&') {
                    self.read_char();
                    self.create_token(TokenKind::DblAmpersand)
                } else {
                    self.create_token(TokenKind::Error("Unexpected character: &".to_string()))
                }
            }
            Some('|') => {
                if self.peek_char() == Some('|') {
                    self.read_char();
                    self.create_token(TokenKind::DblPipe)
                } else {
                    self.create_token(TokenKind::Pipe)
                }
            }
            Some('?') => self.create_token(TokenKind::Question),
            Some(c) => {
                if c.is_alphabetic() {
                    let token = self.read_identifier_or_type();
                    return token;
                } else if c.is_ascii_digit() {
                    return self.read_number();
                } else {
                    self.create_token(TokenKind::Error(format!("Unexpected character: {}", c)))
                }
            }
            None => self.create_token_no_advance(TokenKind::Eof),
        };
        self.read_char();
        token
    }

    fn is_identifier_start(&self, c: Option<char>) -> bool {
        c.map_or(false, |c| c.is_alphabetic() || c == '_')
    }

    fn is_identifier_char(&self, c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    fn create_token(&self, kind: TokenKind) -> Token {
        Token {
            kind,
            span: Span::new(self.start_position, self.position + 1),
        }
    }

    fn create_token_no_advance(&self, kind: TokenKind) -> Token {
        Token {
            kind,
            span: Span::new(self.start_position, self.position),
        }
    }

    fn read_identifier_or_type(&mut self) -> Token {
        let start_position = self.position;
        while let Some(c) = self.ch {
            if self.is_identifier_char(c) {
                self.read_char();
            } else {
                break;
            }
        }
        let mut identifier: String = self.input[start_position..self.position].to_string();

        // Special handling for format! - check if identifier is "format" followed by "!"
        if identifier == "format" && self.ch == Some('!') {
            self.read_char(); // consume the '!'
            identifier.push('!');
        }

        let kind = match identifier.as_str() {
            "struct" => TokenKind::Struct,
            "impl" => TokenKind::Impl,
            "enum" => TokenKind::Enum,
            "false" => TokenKind::Bool(false),
            "true" => TokenKind::Bool(true),
            "fn" => TokenKind::Function,
            "let" => TokenKind::Let,
            "use" => TokenKind::Use,
            "pub" => TokenKind::Pub,
            "extern" => TokenKind::Extern,
            "async" => TokenKind::Async,
            "as" => TokenKind::As,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "match" => TokenKind::Match,
            "loop" => TokenKind::Loop,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "return" => TokenKind::Return,
            "int" | "float" | "bool" | "string" => TokenKind::Type(identifier),
            "_" => TokenKind::Underscore,
            _ => TokenKind::Identifier(identifier),
        };

        self.create_token_no_advance(kind)
    }

    fn read_string(&mut self) -> Token {
        self.read_char(); // Skip opening quote
        let mut string = String::new();

        while let Some(c) = self.ch {
            if c == '"' {
                break;
            } else if c == '\\' {
                // Handle escape sequences
                self.read_char(); // Consume backslash
                match self.ch {
                    Some('n') => string.push('\n'),
                    Some('t') => string.push('\t'),
                    Some('r') => string.push('\r'),
                    Some('\\') => string.push('\\'),
                    Some('"') => string.push('"'),
                    Some('\'') => string.push('\''),
                    Some('0') => string.push('\0'),
                    Some(ch) => {
                        // Unknown escape sequence, just include as-is
                        string.push('\\');
                        string.push(ch);
                    }
                    None => {
                        // String ended with backslash
                        string.push('\\');
                        break;
                    }
                }
            } else {
                string.push(c);
            }
            self.read_char();
        }

        self.create_token(TokenKind::String(string))
    }

    fn read_number(&mut self) -> Token {
        let position = self.position;
        while let Some(c) = self.ch {
            if c == '.' && self.peek_char() == Some('.') {
                break;
            }
            if !c.is_ascii_digit() && !(c == '.') {
                break;
            }
            self.read_char();
        }

        let str = self.input[position..self.position].to_string();

        if str.contains('.') {
            let num = str.parse().unwrap();
            return self.create_token_no_advance(TokenKind::Float(num));
        }

        let num = str.parse().unwrap();
        self.create_token_no_advance(TokenKind::Int(num))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_float() {
        let input = "let x = 10.5;";
        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("x".to_string()),
            TokenKind::Equals,
            TokenKind::Float(10.5),
            TokenKind::Semicolon,
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token.kind, expected);
        }
    }

    #[test]
    fn test_next_token() {
        let input = r#"
            let five = 5;
            fn add(x, y) {
                x + y;
            }
        "#;
        let mut lexer = Lexer::new(input.to_string());

        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("five".to_string()),
            TokenKind::Equals,
            TokenKind::Int(5),
            TokenKind::Semicolon,
            TokenKind::Function,
            TokenKind::Identifier("add".to_string()),
            TokenKind::LParen,
            TokenKind::Identifier("x".to_string()),
            TokenKind::Comma,
            TokenKind::Identifier("y".to_string()),
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Identifier("x".to_string()),
            TokenKind::Plus,
            TokenKind::Identifier("y".to_string()),
            TokenKind::Semicolon,
            TokenKind::RBrace,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token.kind, expected);
        }
    }

    #[test]
    fn test_lex_let_int() {
        let input = r#"let age = 1;"#;
        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenKind::Let, 0, 3),
            Token::new(TokenKind::Identifier("age".to_string()), 4, 7),
            Token::new(TokenKind::Equals, 8, 9),
            Token::new(TokenKind::Int(1), 10, 11),
            Token::new(TokenKind::Semicolon, 11, 12),
            Token::new(TokenKind::Eof, 12, 12),
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected);
        }
    }

    #[test]
    fn test_lex_let_string() {
        let input = r#"let name = "Felipe";"#;
        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenKind::Let, 0, 3),
            Token::new(TokenKind::Identifier("name".to_string()), 4, 8),
            Token::new(TokenKind::Equals, 9, 10),
            Token::new(TokenKind::String("Felipe".to_string()), 11, 19),
            Token::new(TokenKind::Semicolon, 19, 20),
            Token::new(TokenKind::Eof, 20, 20),
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected);
        }
    }

    #[test]
    fn test_lex_let_bool() {
        let input = r#"let is_true = true;"#;
        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenKind::Let, 0, 3),
            Token::new(TokenKind::Identifier("is_true".to_string()), 4, 11),
            Token::new(TokenKind::Equals, 12, 13),
            Token::new(TokenKind::Bool(true), 14, 18),
            Token::new(TokenKind::Semicolon, 18, 19),
            Token::new(TokenKind::Eof, 19, 19),
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected);
        }
    }

    #[test]
    fn test_lex_let_string_newline() {
        let input = r#"
            let name = "Felipe";
        "#;
        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenKind::Let, 13, 16),
            Token::new(TokenKind::Identifier("name".to_string()), 17, 21),
            Token::new(TokenKind::Equals, 22, 23),
            Token::new(TokenKind::String("Felipe".to_string()), 24, 32),
            Token::new(TokenKind::Semicolon, 32, 33),
            Token::new(TokenKind::Eof, 42, 42),
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected);
        }
    }

    #[test]
    fn test_lex_if() {
        let input = r#"
            if true {
                let x = 10;
            }
        "#;

        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenKind::If, 13, 15),
            Token::new(TokenKind::Bool(true), 16, 20),
            Token::new(TokenKind::LBrace, 21, 22),
            Token::new(TokenKind::Let, 39, 42),
            Token::new(TokenKind::Identifier("x".to_string()), 43, 44),
            Token::new(TokenKind::Equals, 45, 46),
            Token::new(TokenKind::Int(10), 47, 49),
            Token::new(TokenKind::Semicolon, 49, 50),
            Token::new(TokenKind::RBrace, 63, 64),
            Token::new(TokenKind::Eof, 73, 73),
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected);
        }
    }

    #[test]
    fn test_lex_if_else() {
        let input = r#"
            if true {
                let x = 10;
            } else {
                let x = 20;
            }
        "#;

        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenKind::If, 13, 15),
            Token::new(TokenKind::Bool(true), 16, 20),
            Token::new(TokenKind::LBrace, 21, 22),
            Token::new(TokenKind::Let, 39, 42),
            Token::new(TokenKind::Identifier("x".to_string()), 43, 44),
            Token::new(TokenKind::Equals, 45, 46),
            Token::new(TokenKind::Int(10), 47, 49),
            Token::new(TokenKind::Semicolon, 49, 50),
            Token::new(TokenKind::RBrace, 63, 64),
            Token::new(TokenKind::Else, 65, 69),
            Token::new(TokenKind::LBrace, 70, 71),
            Token::new(TokenKind::Let, 88, 91),
            Token::new(TokenKind::Identifier("x".to_string()), 92, 93),
            Token::new(TokenKind::Equals, 94, 95),
            Token::new(TokenKind::Int(20), 96, 98),
            Token::new(TokenKind::Semicolon, 98, 99),
            Token::new(TokenKind::RBrace, 112, 113),
            Token::new(TokenKind::Eof, 122, 122),
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected);
        }
    }

    #[test]
    fn test_lex_struct() {
        let input = r#"
            struct Person {
                name: string,
                age: int,
            }
        "#;

        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenKind::Struct, 13, 19),
            Token::new(TokenKind::Identifier("Person".to_string()), 20, 26),
            Token::new(TokenKind::LBrace, 27, 28),
            Token::new(TokenKind::Identifier("name".to_string()), 45, 49),
            Token::new(TokenKind::Colon, 49, 50),
            Token::new(TokenKind::Type("string".to_string()), 51, 57),
            Token::new(TokenKind::Comma, 57, 58),
            Token::new(TokenKind::Identifier("age".to_string()), 75, 78),
            Token::new(TokenKind::Colon, 78, 79),
            Token::new(TokenKind::Type("int".to_string()), 80, 83),
            Token::new(TokenKind::Comma, 83, 84),
            Token::new(TokenKind::RBrace, 97, 98),
            Token::new(TokenKind::Eof, 107, 107),
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected);
        }
    }

    #[test]
    fn test_lex_struct_instance() {
        let input = r#"
            struct Person {
                name: string,
                age: int,
            }

            let p = Person {
                name: "Felipe",
                age: 30,
            };
        "#;

        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenKind::Struct, 13, 19),
            Token::new(TokenKind::Identifier("Person".to_string()), 20, 26),
            Token::new(TokenKind::LBrace, 27, 28),
            Token::new(TokenKind::Identifier("name".to_string()), 45, 49),
            Token::new(TokenKind::Colon, 49, 50),
            Token::new(TokenKind::Type("string".to_string()), 51, 57),
            Token::new(TokenKind::Comma, 57, 58),
            Token::new(TokenKind::Identifier("age".to_string()), 75, 78),
            Token::new(TokenKind::Colon, 78, 79),
            Token::new(TokenKind::Type("int".to_string()), 80, 83),
            Token::new(TokenKind::Comma, 83, 84),
            Token::new(TokenKind::RBrace, 97, 98),
            Token::new(TokenKind::Let, 112, 115),
            Token::new(TokenKind::Identifier("p".to_string()), 116, 117),
            Token::new(TokenKind::Equals, 118, 119),
            Token::new(TokenKind::Identifier("Person".to_string()), 120, 126),
            Token::new(TokenKind::LBrace, 127, 128),
            Token::new(TokenKind::Identifier("name".to_string()), 145, 149),
            Token::new(TokenKind::Colon, 149, 150),
            Token::new(TokenKind::String("Felipe".to_string()), 151, 159),
            Token::new(TokenKind::Comma, 159, 160),
            Token::new(TokenKind::Identifier("age".to_string()), 177, 180),
            Token::new(TokenKind::Colon, 180, 181),
            Token::new(TokenKind::Int(30), 182, 184),
            Token::new(TokenKind::Comma, 184, 185),
            Token::new(TokenKind::RBrace, 198, 199),
            Token::new(TokenKind::Semicolon, 199, 200),
            Token::new(TokenKind::Eof, 209, 209),
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected);
        }
    }

    #[test]
    fn test_lex_struct_field_access() {
        let input = "p.name";

        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenKind::Identifier("p".to_string()), 0, 1),
            Token::new(TokenKind::Dot, 1, 2),
            Token::new(TokenKind::Identifier("name".to_string()), 2, 6),
            Token::new(TokenKind::Eof, 6, 6),
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected);
        }
    }

    #[test]
    fn test_lex_struct_field_set() {
        let input = "client.age = 12;";

        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenKind::Identifier("client".to_string()), 0, 6),
            Token::new(TokenKind::Dot, 6, 7),
            Token::new(TokenKind::Identifier("age".to_string()), 7, 10),
            Token::new(TokenKind::Equals, 11, 12),
            Token::new(TokenKind::Int(12), 13, 15),
            Token::new(TokenKind::Semicolon, 15, 16),
            Token::new(TokenKind::Eof, 16, 16),
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected);
        }
    }

    #[test]
    fn test_lex_eq() {
        let input = "x == 10;";

        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            Token::new(TokenKind::Identifier("x".to_string()), 0, 1),
            Token::new(TokenKind::DblEquals, 2, 4),
            Token::new(TokenKind::Int(10), 5, 7),
            Token::new(TokenKind::Semicolon, 7, 8),
            Token::new(TokenKind::Eof, 8, 8),
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token, expected);
        }
    }

    #[test]
    fn test_lex_enum() {
        let input = r#"
            enum Option {
                Some(String),
                None,
            }
        "#;

        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            TokenKind::Enum,
            TokenKind::Identifier("Option".to_string()),
            TokenKind::LBrace,
            TokenKind::Identifier("Some".to_string()),
            TokenKind::LParen,
            TokenKind::Identifier("String".to_string()),
            TokenKind::RParen,
            TokenKind::Comma,
            TokenKind::Identifier("None".to_string()),
            TokenKind::Comma,
            TokenKind::RBrace,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_enum_variant() {
        let input = r"let c = Color::Red;";

        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("c".to_string()),
            TokenKind::Equals,
            TokenKind::Identifier("Color".to_string()),
            TokenKind::DblColon,
            TokenKind::Identifier("Red".to_string()),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_match_enum() {
        let code = r#"
            match n {
                Name::Existing(name) => name,
                Name::NotExisting => "Not existing",
            }
        "#;

        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Match,
            TokenKind::Identifier("n".to_string()),
            TokenKind::LBrace,
            TokenKind::Identifier("Name".to_string()),
            TokenKind::DblColon,
            TokenKind::Identifier("Existing".to_string()),
            TokenKind::LParen,
            TokenKind::Identifier("name".to_string()),
            TokenKind::RParen,
            TokenKind::FatArrow,
            TokenKind::Identifier("name".to_string()),
            TokenKind::Comma,
            TokenKind::Identifier("Name".to_string()),
            TokenKind::DblColon,
            TokenKind::Identifier("NotExisting".to_string()),
            TokenKind::FatArrow,
            TokenKind::String("Not existing".to_string()),
            TokenKind::Comma,
            TokenKind::RBrace,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_line_comments() {
        let code = r#"
            // This is a comment
            let x = 10; // Another comment
        "#;

        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("x".to_string()),
            TokenKind::Equals,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_block_comments() {
        let code = r#"
            /* This is a multi
             * line comment */
            let x = 10; /* Another comment */
        "#;

        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("x".to_string()),
            TokenKind::Equals,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_array() {
        let code = r#"
            let arr = [1,2,3,4,5];
            print(arr);
        "#;

        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("arr".to_string()),
            TokenKind::Equals,
            TokenKind::LSquare,
            TokenKind::Int(1),
            TokenKind::Comma,
            TokenKind::Int(2),
            TokenKind::Comma,
            TokenKind::Int(3),
            TokenKind::Comma,
            TokenKind::Int(4),
            TokenKind::Comma,
            TokenKind::Int(5),
            TokenKind::RSquare,
            TokenKind::Semicolon,
            TokenKind::Identifier("print".to_string()),
            TokenKind::LParen,
            TokenKind::Identifier("arr".to_string()),
            TokenKind::RParen,
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_range() {
        let code = "let range = 1..10;";
        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("range".to_string()),
            TokenKind::Equals,
            TokenKind::Int(1),
            TokenKind::DblDot,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_inclusive_range() {
        let code = "let range = 1..=10;";
        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("range".to_string()),
            TokenKind::Equals,
            TokenKind::Int(1),
            TokenKind::DblDotEquals,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_from_start_range() {
        let code = "let range = ..10;";
        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("range".to_string()),
            TokenKind::Equals,
            TokenKind::DblDot,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_from_start_inclusive_range() {
        let code = "let range = ..=10;";
        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("range".to_string()),
            TokenKind::Equals,
            TokenKind::DblDotEquals,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_until_end_range() {
        let code = "let range = 1..;";
        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("range".to_string()),
            TokenKind::Equals,
            TokenKind::Int(1),
            TokenKind::DblDot,
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_until_end_inclusive_range() {
        let code = "let range = 1..=;";
        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("range".to_string()),
            TokenKind::Equals,
            TokenKind::Int(1),
            TokenKind::DblDotEquals,
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_plus_equals() {
        let code = "x += 10;";
        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Identifier("x".to_string()),
            TokenKind::PlusEquals,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_less_equals() {
        let code = "x -= 10;";
        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Identifier("x".to_string()),
            TokenKind::MinusEquals,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_times_equals() {
        let code = "x *= 10;";
        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Identifier("x".to_string()),
            TokenKind::StarEquals,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_divide_equals() {
        let code = "x /= 10;";
        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Identifier("x".to_string()),
            TokenKind::SlashEquals,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_mod_equals() {
        let code = "x %= 10;";
        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Identifier("x".to_string()),
            TokenKind::PercentEquals,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_comparison_operators() {
        let code = "x > 10; x < 10; x >= 10; x <= 10;";
        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Identifier("x".to_string()),
            TokenKind::GreaterThan,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Identifier("x".to_string()),
            TokenKind::LessThan,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Identifier("x".to_string()),
            TokenKind::GreaterThanEquals,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Identifier("x".to_string()),
            TokenKind::LessThanEquals,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_while() {
        let code = r#"
            while x < 10 {
                x += 1;
            }
        "#;

        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::While,
            TokenKind::Identifier("x".to_string()),
            TokenKind::LessThan,
            TokenKind::Int(10),
            TokenKind::LBrace,
            TokenKind::Identifier("x".to_string()),
            TokenKind::PlusEquals,
            TokenKind::Int(1),
            TokenKind::Semicolon,
            TokenKind::RBrace,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_loop() {
        let code = r#"
            loop {
                x += 1;
            }
        "#;

        let mut lexer = Lexer::new(code);
        let expected_tokens = vec![
            TokenKind::Loop,
            TokenKind::LBrace,
            TokenKind::Identifier("x".to_string()),
            TokenKind::PlusEquals,
            TokenKind::Int(1),
            TokenKind::Semicolon,
            TokenKind::RBrace,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let kind = lexer.next_token().kind;
            assert_eq!(kind, expected);
        }
    }

    #[test]
    fn test_lex_impl() {
        let input = r#"
            impl Point {
                fn new(x: int, y: int) -> Point {
                    Point { x: x, y: y }
                }
            }
        "#;

        let mut lexer = Lexer::new(input.to_string());
        let expected_tokens = vec![
            TokenKind::Impl,
            TokenKind::Identifier("Point".to_string()),
            TokenKind::LBrace,
            TokenKind::Function,
            TokenKind::Identifier("new".to_string()),
            TokenKind::LParen,
            TokenKind::Identifier("x".to_string()),
            TokenKind::Colon,
            TokenKind::Type("int".to_string()),
            TokenKind::Comma,
            TokenKind::Identifier("y".to_string()),
            TokenKind::Colon,
            TokenKind::Type("int".to_string()),
            TokenKind::RParen,
            TokenKind::Arrow,
            TokenKind::Identifier("Point".to_string()),
            TokenKind::LBrace,
            TokenKind::Identifier("Point".to_string()),
            TokenKind::LBrace,
            TokenKind::Identifier("x".to_string()),
            TokenKind::Colon,
            TokenKind::Identifier("x".to_string()),
            TokenKind::Comma,
            TokenKind::Identifier("y".to_string()),
            TokenKind::Colon,
            TokenKind::Identifier("y".to_string()),
            TokenKind::RBrace,
            TokenKind::RBrace,
            TokenKind::RBrace,
            TokenKind::Eof,
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token.kind, expected);
        }
    }

    #[test]
    fn lex_method_call() {
        let code = "Point::new(3, 4)";
        let mut lexer = Lexer::new(code.to_string());

        let expected_tokens = vec![
            TokenKind::Identifier("Point".to_string()),
            TokenKind::DblColon,
            TokenKind::Identifier("new".to_string()),
            TokenKind::LParen,
            TokenKind::Int(3),
            TokenKind::Comma,
            TokenKind::Int(4),
            TokenKind::RParen,
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token.kind, expected);
        }
    }

    #[test]
    fn test_use_and_pub_keywords() {
        let input = r#"
            use local::models::User;
            pub fn hello() {
                println("Hello");
            }
        "#;
        let mut lexer = Lexer::new(input.to_string());

        let expected_tokens = vec![
            TokenKind::Use,
            TokenKind::Identifier("local".to_string()),
            TokenKind::DblColon,
            TokenKind::Identifier("models".to_string()),
            TokenKind::DblColon,
            TokenKind::Identifier("User".to_string()),
            TokenKind::Semicolon,
            TokenKind::Pub,
            TokenKind::Function,
            TokenKind::Identifier("hello".to_string()),
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Identifier("println".to_string()),
            TokenKind::LParen,
            TokenKind::String("Hello".to_string()),
            TokenKind::RParen,
            TokenKind::Semicolon,
            TokenKind::RBrace,
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token.kind, expected);
        }
    }
}
