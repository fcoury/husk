use crate::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub const EOF: Token = Token {
    kind: TokenKind::EOF,
    span: Span { start: 0, end: 0 },
};

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Function,
    Let,
    Identifier(String),
    Number(i64),
    Equals,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Error(String),
    EOF,
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
            if token.kind == TokenKind::EOF {
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

    // fn peek_char(&self) -> Option<char> {
    //     if self.read_position >= self.input.len() {
    //         None
    //     } else {
    //         Some(self.input.chars().nth(self.read_position).unwrap())
    //     }
    // }

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
            Some('-') => self.create_token(TokenKind::Minus),
            Some('*') => self.create_token(TokenKind::Asterisk),
            Some('/') => self.create_token(TokenKind::Slash),
            Some(c) => {
                if c.is_alphabetic() {
                    return self.read_identifier();
                } else if c.is_digit(10) {
                    return self.read_number();
                } else {
                    self.create_token(TokenKind::Error(format!("Unexpected character: {}", c)))
                }
            }
            None => self.create_token(TokenKind::EOF),
        };
        self.read_char();
        token
    }

    fn create_token(&self, kind: TokenKind) -> Token {
        Token {
            kind,
            span: Span::new(self.start_position, self.position),
        }
    }

    fn read_identifier(&mut self) -> Token {
        let position = self.position;
        while let Some(c) = self.ch {
            if !c.is_alphabetic() {
                break;
            }
            self.read_char();
        }

        let ident = self.input[position..self.position].to_string();
        let kind = match ident.as_str() {
            "fn" => TokenKind::Function,
            "let" => TokenKind::Let,
            _ => TokenKind::Identifier(ident),
        };

        self.create_token(kind)
    }

    fn read_number(&mut self) -> Token {
        let position = self.position;
        while let Some(c) = self.ch {
            if !c.is_digit(10) {
                break;
            }
            self.read_char();
        }

        let num = self.input[position..self.position].parse().unwrap();
        self.create_token(TokenKind::Number(num))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            TokenKind::Number(5),
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
            TokenKind::EOF,
        ];

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token.kind, expected);
        }
    }
}