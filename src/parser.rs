use crate::{
    lexer::{Token, TokenKind, EOF},
    span::Span,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    Number(i64, Span),
    Identifier(String, Span),
    BinaryOp(Box<Expr>, Operator, Box<Expr>, Span),
    FunctionCall(String, Vec<Expr>, Span),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stmt {
    Let(String, Expr),
    Function(String, Vec<String>, Vec<Stmt>, Span),
    Expression(Expr),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
        }
    }

    fn current_token(&self) -> &Token {
        &self.tokens.get(self.position).unwrap_or(&EOF)
    }

    fn advance(&mut self) {
        if self.position < self.tokens.len() {
            self.position += 1;
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        while self.position < self.tokens.len() {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            } else {
                break;
            }
        }
        statements
    }

    fn parse_statement(&mut self) -> Option<Stmt> {
        match self.current_token().kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Function => self.parse_function(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Stmt> {
        self.advance(); // Consume 'let'

        let name = match &self.current_token().kind {
            TokenKind::Identifier(name) => name,
            _ => return None,
        }
        .clone();
        self.advance(); // Consume identifier

        if self.current_token().kind != TokenKind::Equals {
            return None;
        }
        self.advance(); // Consume '='

        let expr = self.parse_expression()?;

        if self.current_token().kind != TokenKind::Semicolon {
            return None;
        }
        self.advance(); // Consume ';'

        Some(Stmt::Let(name.to_string(), expr))
    }

    fn parse_function(&mut self) -> Option<Stmt> {
        let start_span = self.current_token().span.start;

        // Consume 'fn'
        self.advance();

        // Parse function name
        let name = match &self.current_token().kind {
            TokenKind::Identifier(name) => name,
            _ => return None,
        }
        .clone();
        self.advance(); // Consume identifier

        // Consume '('
        if self.current_token().kind != TokenKind::LParen {
            return None;
        }
        self.advance();

        let mut params = Vec::new();
        while self.current_token().kind != TokenKind::RParen {
            if let TokenKind::Identifier(param) = &self.current_token().kind {
                params.push(param.to_string());
                self.advance(); // Consume parameter
                if self.current_token().kind == TokenKind::Comma {
                    self.advance(); // Consume ','
                }
            }
        }
        self.advance(); // Consume ')'

        if self.current_token().kind == TokenKind::LBrace {
            self.advance(); // Consume '{'
            let mut body = Vec::new();
            while self.current_token().kind != TokenKind::RBrace {
                if let Some(stmt) = self.parse_statement() {
                    body.push(stmt);
                }
            }
            self.advance(); // Consume '}'
            let end_span = self.current_token().span.end;
            return Some(Stmt::Function(
                name.to_string(),
                params,
                body,
                Span::new(start_span, end_span),
            ));
        }

        None
    }

    fn parse_expression_statement(&mut self) -> Option<Stmt> {
        if let Some(expr) = self.parse_expression() {
            if self.current_token().kind == TokenKind::Semicolon {
                self.advance(); // Consume ';'
                return Some(Stmt::Expression(expr));
            }
        }
        None
    }

    fn parse_expression(&mut self) -> Option<Expr> {
        self.parse_binary_expression()
    }

    fn parse_binary_expression(&mut self) -> Option<Expr> {
        let start_span = self.current_token().span.start;
        let mut left = self.parse_primary_expression()?;
        while let Some(op) = self.parse_operator() {
            self.advance(); // Consume operator
            let right = self.parse_primary_expression()?;
            let end_span = self.current_token().span.end;
            let span = Span::new(start_span, end_span);
            left = Expr::BinaryOp(Box::new(left), op, Box::new(right), span);
        }
        Some(left)
    }

    fn parse_primary_expression(&mut self) -> Option<Expr> {
        let start_span = self.current_token().span.start;
        match self.current_token().kind {
            TokenKind::Number(value) => {
                self.advance(); // Consume number
                let end_span = self.current_token().span.end;
                let span = Span::new(start_span, end_span);
                Some(Expr::Number(value, span))
            }
            TokenKind::Identifier(ref name) => {
                let name = name.clone();
                self.advance(); // Consume identifier
                if self.current_token().kind == TokenKind::LParen {
                    self.advance(); // Consume '('
                    let mut args = Vec::new();
                    while self.current_token().kind != TokenKind::RParen {
                        if let Some(arg) = self.parse_expression() {
                            args.push(arg);
                            if self.current_token().kind == TokenKind::Comma {
                                self.advance(); // Consume ','
                            }
                        }
                    }
                    self.advance(); // Consume ')'
                    let end_span = self.current_token().span.end;
                    Some(Expr::FunctionCall(
                        name,
                        args,
                        Span::new(start_span, end_span),
                    ))
                } else {
                    let end_span = self.current_token().span.end;
                    let span = Span::new(start_span, end_span);
                    Some(Expr::Identifier(name, span))
                }
            }
            _ => None,
        }
    }

    fn parse_operator(&self) -> Option<Operator> {
        match self.current_token().kind {
            TokenKind::Plus => Some(Operator::Plus),
            TokenKind::Minus => Some(Operator::Minus),
            TokenKind::Asterisk => Some(Operator::Multiply),
            TokenKind::Slash => Some(Operator::Divide),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_parse_let_statement() {
        let input = "let x = 5;";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all(); // Assume you have a method to lex all tokens
        let mut parser = Parser::new(tokens);

        let ast = parser.parse();
        let expected_ast = vec![Stmt::Let("x".to_string(), Expr::Number(5, Span::new(8, 9)))];

        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn test_parse_function() {
        let input = "fn add(x, y) { x + y; }";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);

        let ast = parser.parse();
        let expected_ast = vec![Stmt::Function(
            "add".to_string(),
            vec!["x".to_string(), "y".to_string()],
            vec![Stmt::Expression(Expr::BinaryOp(
                Box::new(Expr::Identifier("x".to_string(), Span::new(15, 17))),
                Operator::Plus,
                Box::new(Expr::Identifier("y".to_string(), Span::new(19, 20))),
                Span::new(15, 20),
            ))],
            Span::new(0, 0),
        )];

        assert_eq!(ast, expected_ast);
    }
}
