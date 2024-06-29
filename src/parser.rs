use crate::{
    error::{Error, Result},
    lexer::{Token, TokenKind, EOF},
    span::Span,
};

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64, Span),
    Float(f64, Span),
    String(String, Span),
    Bool(bool, Span),
    Identifier(String, Span),
    BinaryOp(Box<Expr>, Operator, Box<Expr>, Span),
    FunctionCall(String, Vec<Expr>, Span),
    StructInit(String, Vec<(String, Expr)>, Span),
    MemberAccess(Box<Expr>, String, Span),
    Assign(Box<Expr>, Box<Expr>, Span),
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.span() == other.span()
    }
}

impl Eq for Expr {}

impl PartialOrd for Expr {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Expr {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.span().cmp(&other.span())
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(_, span) => span.clone(),
            Expr::Float(_, span) => span.clone(),
            Expr::String(_, span) => span.clone(),
            Expr::Bool(_, span) => span.clone(),
            Expr::Identifier(_, span) => span.clone(),
            Expr::BinaryOp(_, _, _, span) => span.clone(),
            Expr::FunctionCall(_, _, span) => span.clone(),
            Expr::StructInit(_, _, span) => span.clone(),
            Expr::MemberAccess(_, _, span) => span.clone(),
            Expr::Assign(_, _, span) => span.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stmt {
    Let(String, Expr, Span),
    Function(String, Vec<(String, String)>, String, Vec<Stmt>, Span),
    If(Expr, Vec<Stmt>, Vec<Stmt>, Span),
    ReturnExpression(Expr),
    Expression(Expr),
    Struct(String, Vec<(String, String)>, Span),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
        self.tokens.get(self.position).unwrap_or(&EOF)
    }

    fn advance(&mut self) {
        if self.position < self.tokens.len() {
            self.position += 1;
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = Vec::new();
        while self.position < self.tokens.len() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => return Err(e),
            }
        }
        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Stmt> {
        match self.current_token().kind {
            TokenKind::Struct => self.parse_struct(),
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Function => self.parse_function(),
            TokenKind::If => self.parse_if_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = Vec::new();
        while self.current_token().kind != TokenKind::RBrace {
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }

    fn parse_struct(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'struct'

        let Some(name) = self.consume_identifier()? else {
            return Err(Error::new_parse(
                "Expected struct name".to_string(),
                self.current_token().span,
            ));
        };

        if self.current_token().kind != TokenKind::LBrace {
            return Err(Error::new_parse(
                format!(
                    "Expected '{{' after struct name, found {:?}",
                    self.current_token()
                ),
                self.current_token().span,
            ));
        }
        self.advance(); // Consume '{'

        let mut fields = Vec::new();
        while self.current_token().kind != TokenKind::RBrace {
            let field_name = self.consume_identifier()?.ok_or_else(|| {
                Error::new_parse("Expected field name".to_string(), self.current_token().span)
            })?;
            if self.current_token().kind != TokenKind::Colon {
                return Err(Error::new_parse(
                    "Expected ':' after field name".to_string(),
                    self.current_token().span,
                ));
            }
            self.advance(); // Consume ':'
            let field_type = self.consume_type()?.ok_or_else(|| {
                Error::new_parse(
                    "Expected type after ':'".to_string(),
                    self.current_token().span,
                )
            })?;
            fields.push((field_name, field_type));

            if self.current_token().kind == TokenKind::Comma {
                self.advance(); // Consume ','
            } else if self.current_token().kind != TokenKind::RBrace {
                return Err(Error::new_parse(
                    "Expected ',' or '}' in field list".to_string(),
                    self.current_token().span,
                ));
            }
        }

        let end_span = self.current_token().span.end;
        self.advance(); // Consume '}'
        Ok(Stmt::Struct(name, fields, Span::new(start_span, end_span)))
    }

    fn parse_let_statement(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'let'

        if let Some(name) = self.consume_identifier()? {
            if self.current_token().kind == TokenKind::Equals {
                self.advance(); // Consume '='
                if let Ok(expr) = self.parse_expression() {
                    if self.current_token().kind == TokenKind::Semicolon {
                        let end_span = self.current_token().span.end;
                        self.advance(); // Consume ';'
                        return Ok(Stmt::Let(name, expr, Span::new(start_span, end_span)));
                    } else {
                        return Err(Error::new_parse(
                            format!(
                                "Expected ';' after expression, found {:?}",
                                self.current_token()
                            ),
                            self.current_token().span,
                        ));
                    }
                } else {
                    return Err(Error::new_parse(
                        "Invalid expression in let statement".to_string(),
                        self.current_token().span,
                    ));
                }
            }
        }
        Err(Error::new_parse(
            "Invalid let statement".to_string(),
            Span {
                start: start_span,
                end: self.current_token().span.end,
            },
        ))
    }

    fn parse_if_statement(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'if'

        let Ok(condition) = self.parse_expression() else {
            return Err(Error::new_parse(
                "Invalid expression in if statement".to_string(),
                self.current_token().span,
            ));
        };

        if self.current_token().kind != TokenKind::LBrace {
            return Err(Error::new_parse(
                "Expected '{' after if condition".to_string(),
                self.current_token().span,
            ));
        }

        self.advance(); // Consume '{'
        let mut then_block = Vec::new();
        while self.current_token().kind != TokenKind::RBrace {
            then_block.push(self.parse_statement()?);
        }

        let mut else_block = Vec::new();
        let mut end_span = self.current_token().span.end;

        self.advance(); // Consume '}'

        if self.current_token().kind == TokenKind::Else {
            self.advance(); // Consume 'else'

            if self.current_token().kind != TokenKind::LBrace {
                return Err(Error::new_parse(
                    "Expected '{' after else condition".to_string(),
                    self.current_token().span,
                ));
            }

            self.advance(); // Consume '{'
            else_block = self.parse_block()?;

            if self.current_token().kind != TokenKind::RBrace {
                return Err(Error::new_parse(
                    "Expected '}' after else block".to_string(),
                    self.current_token().span,
                ));
            }
            end_span = self.current_token().span.end;
            self.advance(); // Consume '}'
        }

        return Ok(Stmt::If(
            condition,
            then_block,
            else_block,
            Span::new(start_span, end_span),
        ));
    }

    fn consume_identifier(&mut self) -> Result<Option<String>> {
        if let TokenKind::Identifier(name) = &self.current_token().kind {
            let name = name.clone();
            self.advance(); // Consume identifier
            Ok(Some(name))
        } else {
            Err(Error::new_parse(
                "Expected identifier".to_string(),
                self.current_token().span,
            ))
        }
    }

    fn parse_function(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;

        // Consume 'fn'
        self.advance();

        // --- Parse function name ---

        // Parse function name
        let name = match &self.current_token().kind {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(Error::new_parse(
                    "Invalid function name".to_string(),
                    self.current_token().span,
                ))
            }
        }
        .clone();
        self.advance(); // Consume identifier

        // --- Parse function parameters ---

        // Consume '('
        if self.current_token().kind != TokenKind::LParen {
            return Err(Error::new_parse(
                "Expected '('. Invalid function declaration".to_string(),
                self.current_token().span,
            ));
        }
        self.advance();

        let mut params = Vec::new();
        while self.current_token().kind != TokenKind::RParen {
            let param_name = self.consume_identifier()?.ok_or_else(|| {
                Error::new_parse(
                    "Expected parameter name".to_string(),
                    self.current_token().span,
                )
            })?;
            if self.current_token().kind != TokenKind::Colon {
                return Err(Error::new_parse(
                    "Expected ':' after parameter name".to_string(),
                    self.current_token().span,
                ));
            }
            self.advance(); // Consume ':'
            let param_type = self.consume_type()?.ok_or_else(|| {
                Error::new_parse(
                    "Expected type after ':'".to_string(),
                    self.current_token().span,
                )
            })?;
            params.push((param_name, param_type));

            if self.current_token().kind == TokenKind::Comma {
                self.advance(); // Consume ','
            } else if self.current_token().kind != TokenKind::RParen {
                return Err(Error::new_parse(
                    "Expected ',' or ')' in parameter list".to_string(),
                    self.current_token().span,
                ));
            }
        }
        self.advance(); // Consume ')'

        // --- Parse return type ---

        let return_type = if self.current_token().kind == TokenKind::Arrow {
            self.advance(); // Consume '->'
            self.consume_type()?.ok_or_else(|| {
                Error::new_parse(
                    "Expected return type after '->'".to_string(),
                    self.current_token().span,
                )
            })?
        } else {
            "void".to_string() // Default return type
        };

        // --- Parse function body ---

        if self.current_token().kind != TokenKind::LBrace {
            return Err(Error::new_parse(
                "Expected '{' after function declaration".to_string(),
                self.current_token().span,
            ));
        }
        self.advance(); // Consume '{'

        let mut body = Vec::new();
        while self.current_token().kind != TokenKind::RBrace {
            body.push(self.parse_statement()?);
        }
        let end_span = self.current_token().span.end;
        self.advance(); // Consume '}'
        Ok(Stmt::Function(
            name,
            params,
            return_type,
            body,
            Span {
                start: start_span,
                end: end_span,
            },
        ))
    }

    fn consume_type(&mut self) -> Result<Option<String>> {
        if let TokenKind::Type(ref type_name) = self.current_token().kind {
            let type_name = type_name.clone();
            self.advance(); // Consume type
            Ok(Some(type_name))
        } else {
            Ok(None)
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt> {
        if let Ok(expr) = self.parse_expression() {
            if self.current_token().kind == TokenKind::Semicolon {
                self.advance(); // Consume ';'
                return Ok(Stmt::Expression(expr));
            } else if self.current_token().kind == TokenKind::Equals {
                self.advance(); // Consume '='
                let value = self.parse_expression()?;

                if self.current_token().kind == TokenKind::Semicolon {
                    self.advance(); // Consume ';'
                    return Ok(Stmt::Expression(Expr::Assign(
                        Box::new(expr.clone()),
                        Box::new(value.clone()),
                        Span::new(expr.span().start, value.span().end),
                    )));
                }
                return Err(Error::new_parse(
                    "Expected ';' after assignment".to_string(),
                    self.current_token().span,
                ));
            }
            return Ok(Stmt::ReturnExpression(expr));
        }

        Err(Error::new_parse(
            "Invalid expression statement".to_string(),
            self.current_token().span,
        ))
    }

    fn parse_expression(&mut self) -> Result<Expr> {
        self.parse_binary_expression()
    }

    fn parse_binary_expression(&mut self) -> Result<Expr> {
        let mut left = self.parse_primary_expression()?;
        while let Some(op) = self.parse_operator() {
            let start_span = left.span().start;
            let end_span = self.current_token().span.end;

            self.advance(); // Consume operator
            let right = self.parse_primary_expression()?;
            left = Expr::BinaryOp(
                Box::new(left),
                op,
                Box::new(right),
                Span::new(start_span, end_span),
            );
        }
        Ok(left)
    }

    fn parse_primary_expression(&mut self) -> Result<Expr> {
        let span = self.current_token().span;
        match self.current_token().kind {
            TokenKind::Int(value) => {
                self.advance(); // Consume number
                Ok(Expr::Int(value, span))
            }
            TokenKind::Float(value) => {
                self.advance(); // Consume number
                Ok(Expr::Float(value, span))
            }
            TokenKind::String(ref value) => {
                let value = value.clone();
                self.advance(); // Consume string
                Ok(Expr::String(value, span))
            }
            TokenKind::Bool(value) => {
                self.advance(); // Consume boolean
                Ok(Expr::Bool(value, span))
            }
            TokenKind::Identifier(ref name) => {
                let name = name.clone();
                self.advance(); // Consume identifier

                match self.current_token().kind {
                    TokenKind::Dot => {
                        self.advance(); // Consume '.'
                        let field_name = self.consume_identifier()?.ok_or_else(|| {
                            Error::new_parse(
                                "Expected field name after '.'".to_string(),
                                self.current_token().span,
                            )
                        })?;
                        Ok(Expr::MemberAccess(
                            Box::new(Expr::Identifier(name, span)),
                            field_name,
                            Span::new(span.start, self.current_token().span.end),
                        ))
                    }
                    TokenKind::LParen => self.parse_function_call(name, span),
                    TokenKind::LBrace => self.parse_struct_init(name, span),
                    _ => Ok(Expr::Identifier(name, span)),
                }
            }
            _ => Err(Error::new_parse(
                "Invalid primary expression".to_string(),
                span,
            )),
        }
    }

    fn parse_function_call(&mut self, name: String, start_span: Span) -> Result<Expr> {
        self.advance(); // Consume '('
        let mut args = Vec::new();
        while self.current_token().kind != TokenKind::RParen {
            let arg = self.parse_expression()?;
            args.push(arg);
            if self.current_token().kind == TokenKind::Comma {
                self.advance(); // Consume ','
            }
        }
        let end_span = self.current_token().span;
        self.advance(); // Consume ')'
        Ok(Expr::FunctionCall(
            name,
            args,
            Span::new(start_span.start, end_span.end),
        ))
    }

    fn parse_struct_init(&mut self, name: String, start_span: Span) -> Result<Expr> {
        let mut fields = Vec::new();
        self.advance(); // Consume '{'

        while self.current_token().kind != TokenKind::RBrace {
            let field_name = self.consume_identifier()?.ok_or_else(|| {
                Error::new_parse("Expected field name".to_string(), self.current_token().span)
            })?;
            if self.current_token().kind != TokenKind::Colon {
                return Err(Error::new_parse(
                    "Expected ':' after field name".to_string(),
                    self.current_token().span,
                ));
            }
            self.advance(); // Consume ':'
            let field_value = self.parse_expression()?;
            fields.push((field_name, field_value));

            if self.current_token().kind == TokenKind::Comma {
                self.advance(); // Consume ','
            } else if self.current_token().kind != TokenKind::RBrace {
                return Err(Error::new_parse(
                    "Expected ',' or '}' in field list".to_string(),
                    self.current_token().span,
                ));
            }
        }
        let end_span = self.current_token().span;
        self.advance(); // Consume '}'
        Ok(Expr::StructInit(
            name,
            fields,
            Span::new(start_span.start, end_span.end),
        ))
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
    fn test_parse_let_float() {
        let input = "let x = 5.0;";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);

        let ast = parser.parse().unwrap();
        let expected_ast = vec![Stmt::Let(
            "x".to_string(),
            Expr::Float(5.0, Span::new(8, 11)),
            Span::new(0, 12),
        )];

        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn test_parse_let_int() {
        let input = "let x = 5;";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all(); // Assume you have a method to lex all tokens
        let mut parser = Parser::new(tokens);

        let ast = parser.parse().unwrap();
        let expected_ast = vec![Stmt::Let(
            "x".to_string(),
            Expr::Int(5, Span::new(8, 9)),
            Span::new(0, 10),
        )];

        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn test_parse_string() {
        let input = r#"let x = "Hello, World!";"#;
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(
            ast[0],
            Stmt::Let(
                "x".to_string(),
                Expr::String("Hello, World!".to_string(), Span::new(8, 23)),
                Span::new(0, 24),
            )
        );
    }

    #[test]
    fn test_parse_bool() {
        let input = "let x = true;";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);

        let ast = parser.parse().unwrap();
        let expected_ast = vec![Stmt::Let(
            "x".to_string(),
            Expr::Identifier("true".to_string(), Span::new(8, 12)),
            Span::new(0, 13),
        )];

        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn test_parse_let_string_and_int() {
        let input = r#"
            let name = "Felipe";
            let y = 5;
        "#;
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(
            ast[0],
            Stmt::Let(
                "name".to_string(),
                Expr::String("Felipe".to_string(), Span::new(24, 32)),
                Span::new(13, 33),
            )
        );
    }

    #[test]
    fn test_parse_if() {
        let input = r#"
            if true {
                let x = 5;
            }
        "#;
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(
            ast[0],
            Stmt::If(
                Expr::Bool(true, Span::new(16, 20)),
                vec![Stmt::Let(
                    "x".to_string(),
                    Expr::Int(5, Span::new(47, 48)),
                    Span::new(39, 49),
                )],
                vec![],
                Span::new(13, 63),
            )
        );
    }

    #[test]
    fn test_parse_if_else() {
        let input = r#"
            if true {
                let x = 5;
            } else {
                let y = 10;
            }
        "#;
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(
            ast[0],
            Stmt::If(
                Expr::Bool(true, Span::new(16, 20)),
                vec![Stmt::Let(
                    "x".to_string(),
                    Expr::Int(5, Span::new(47, 48)),
                    Span::new(39, 49),
                )],
                vec![Stmt::Let(
                    "y".to_string(),
                    Expr::Int(10, Span::new(95, 97)),
                    Span::new(87, 98),
                )],
                Span::new(13, 112),
            )
        );
    }

    #[test]
    fn test_parse_struct() {
        let input = r#"
            struct Person {
                name: string,
                age: int,
            }
        "#;

        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(
            ast[0],
            Stmt::Struct(
                "Person".to_string(),
                vec![
                    ("name".to_string(), "string".to_string()),
                    ("age".to_string(), "int".to_string()),
                ],
                Span::new(13, 98),
            )
        );
    }

    #[test]
    fn test_parse_struct_instance() {
        let input = r#"
            let p = Person {
                name: "Felipe",
                age: 30,
            };
        "#;

        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(
            ast[0],
            Stmt::Let(
                "p".to_string(),
                Expr::StructInit(
                    "Person".to_string(),
                    vec![
                        (
                            "name".to_string(),
                            Expr::String("Felipe".to_string(), Span::new(52, 60))
                        ),
                        ("age".to_string(), Expr::Int(30, Span::new(83, 85))),
                    ],
                    Span::new(21, 100),
                ),
                Span::new(13, 101),
            )
        );
    }

    #[test]
    fn test_parse_struct_field_access() {
        let input = "let name = p.name;";

        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(
            ast[0],
            Stmt::Let(
                "name".to_string(),
                Expr::MemberAccess(
                    Box::new(Expr::Identifier("p".to_string(), Span::new(11, 12))),
                    "name".to_string(),
                    Span::new(11, 18),
                ),
                Span::new(0, 18),
            )
        );
    }

    #[test]
    fn test_parse_struct_field_set() {
        let input = "client.age = 12;";

        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(
            ast[0],
            Stmt::Expression(Expr::Assign(
                Box::new(Expr::MemberAccess(
                    Box::new(Expr::Identifier("client".to_string(), Span::new(0, 6))),
                    "age".to_string(),
                    Span::new(0, 15),
                )),
                Box::new(Expr::Int(12, Span::new(13, 15))),
                Span::new(0, 15),
            ))
        );
    }

    // #[test]
    // fn test_parse_function() {
    //     let input = "fn add(x, y) { x + y; }";
    //     let mut lexer = Lexer::new(input.to_string());
    //     let tokens = lexer.lex_all();
    //     let mut parser = Parser::new(tokens);
    //
    //     let ast = parser.parse().unwrap();
    //     let expected_ast = vec![Stmt::Function(
    //         "add".to_string(),
    //         vec!["x".to_string(), "y".to_string()],
    //         vec![Stmt::Expression(Expr::BinaryOp(
    //             Box::new(Expr::Identifier("x".to_string(), Span::new(15, 17))),
    //             Operator::Plus,
    //             Box::new(Expr::Identifier("y".to_string(), Span::new(19, 20))),
    //             Span::new(15, 20),
    //         ))],
    //         Span::new(0, 0),
    //     )];
    //
    //     assert_eq!(ast, expected_ast);
    // }
}
