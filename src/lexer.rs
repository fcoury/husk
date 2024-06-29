use crate::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
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
    Let,
    Identifier(String),
    Int(i64),
    Float(f64),
    String(String),
    Equals,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Arrow,
    Type(String),
    Plus,
    Minus,
    Asterisk,
    Slash,
    Colon,
    Error(String),
    Eof,
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
    pub fn new(input: String) -> Lexer {
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
            Some('=') => self.create_token(TokenKind::Equals),
            Some(';') => self.create_token(TokenKind::Semicolon),
            Some('(') => self.create_token(TokenKind::LParen),
            Some(')') => self.create_token(TokenKind::RParen),
            Some('{') => self.create_token(TokenKind::LBrace),
            Some('}') => self.create_token(TokenKind::RBrace),
            Some(',') => self.create_token(TokenKind::Comma),
            Some('+') => self.create_token(TokenKind::Plus),
            Some('-') => {
                if self.peek_char() == Some('>') {
                    self.read_char();
                    self.create_token(TokenKind::Arrow)
                } else {
                    self.create_token(TokenKind::Minus)
                }
            }
            Some('*') => self.create_token(TokenKind::Asterisk),
            Some(':') => self.create_token(TokenKind::Colon),
            Some('/') => self.create_token(TokenKind::Slash),
            Some('"') => self.read_string(),
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
            if c.is_alphanumeric() || c == '_' {
                self.read_char();
            } else {
                break;
            }
        }
        let identifier: String = self.input[start_position..self.position].to_string();
        let kind = match identifier.as_str() {
            "fn" => TokenKind::Function,
            "let" => TokenKind::Let,
            "int" | "float" | "bool" | "string" => TokenKind::Type(identifier),
            _ => TokenKind::Identifier(identifier),
        };

        self.create_token_no_advance(kind)
    }

    fn read_string(&mut self) -> Token {
        self.read_char();
        let position = self.position;
        while let Some(c) = self.ch {
            if c == '"' {
                break;
            }
            self.read_char();
        }

        let string = self.input[position..self.position].to_string();
        self.create_token(TokenKind::String(string))
    }

    fn read_number(&mut self) -> Token {
        let position = self.position;
        while let Some(c) = self.ch {
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
}
