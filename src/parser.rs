use std::fmt;

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
    EnumVariant {
        name: String,
        variant: String,
        value: Option<Box<Expr>>,
        span: Span,
    },
    Array(Vec<Expr>, Span),
    ArrayIndex(Box<Expr>, Box<Expr>, Span),
    Identifier(String, Span),
    BinaryOp(Box<Expr>, Operator, Box<Expr>, Span),
    FunctionCall(String, Vec<Expr>, Span),
    StructInit(String, Vec<(String, Expr)>, Span),
    MemberAccess(Box<Expr>, String, Span),
    Assign(Box<Expr>, Box<Expr>, Span),
    CompoundAssign(Box<Expr>, Operator, Box<Expr>, Span),
    Range(Option<Box<Expr>>, Option<Box<Expr>>, bool, Span),
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Expr::Int(value, _) => {
                if let Expr::Int(other_value, _) = other {
                    value == other_value
                } else {
                    false
                }
            }
            Expr::Float(value, _) => {
                if let Expr::Float(other_value, _) = other {
                    value == other_value
                } else {
                    false
                }
            }
            Expr::String(value, _) => {
                if let Expr::String(other_value, _) = other {
                    value == other_value
                } else {
                    false
                }
            }
            Expr::Bool(value, _) => {
                if let Expr::Bool(other_value, _) = other {
                    value == other_value
                } else {
                    false
                }
            }
            Expr::EnumVariant {
                name,
                variant,
                value,
                ..
            } => {
                if let Expr::EnumVariant {
                    name: other_name,
                    variant: other_variant,
                    value: other_value,
                    ..
                } = other
                {
                    name == other_name && variant == other_variant && value == other_value
                } else {
                    false
                }
            }
            Expr::Array(elements, _) => {
                if let Expr::Array(other_elements, _) = other {
                    elements == other_elements
                } else {
                    false
                }
            }
            Expr::ArrayIndex(array, index, _) => {
                if let Expr::ArrayIndex(other_array, other_index, _) = other {
                    array == other_array && index == other_index
                } else {
                    false
                }
            }
            Expr::Identifier(name, _) => {
                if let Expr::Identifier(other_name, _) = other {
                    name == other_name
                } else {
                    false
                }
            }
            Expr::BinaryOp(left, op, right, _) => {
                if let Expr::BinaryOp(other_left, other_op, other_right, _) = other {
                    left == other_left && op == other_op && right == other_right
                } else {
                    false
                }
            }
            Expr::FunctionCall(name, args, _) => {
                if let Expr::FunctionCall(other_name, other_args, _) = other {
                    name == other_name && args == other_args
                } else {
                    false
                }
            }
            Expr::StructInit(name, fields, _) => {
                if let Expr::StructInit(other_name, other_fields, _) = other {
                    name == other_name && fields == other_fields
                } else {
                    false
                }
            }
            Expr::MemberAccess(expr, field, _) => {
                if let Expr::MemberAccess(other_expr, other_field, _) = other {
                    expr == other_expr && field == other_field
                } else {
                    false
                }
            }
            Expr::Assign(left, right, _) => {
                if let Expr::Assign(other_left, other_right, _) = other {
                    left == other_left && right == other_right
                } else {
                    false
                }
            }
            Expr::CompoundAssign(left, op, right, _) => {
                if let Expr::CompoundAssign(other_left, other_op, other_right, _) = other {
                    left == other_left && op == other_op && right == other_right
                } else {
                    false
                }
            }
            Expr::Range(start, end, inclusive, _) => {
                if let Expr::Range(other_start, other_end, other_inclusive, _) = other {
                    start == other_start && end == other_end && inclusive == other_inclusive
                } else {
                    false
                }
            }
        }
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
            Expr::EnumVariant {
                name: _,
                variant: _,
                value: _,
                span,
            } => span.clone(),
            Expr::Identifier(_, span) => span.clone(),
            Expr::BinaryOp(_, _, _, span) => span.clone(),
            Expr::FunctionCall(_, _, span) => span.clone(),
            Expr::StructInit(_, _, span) => span.clone(),
            Expr::MemberAccess(_, _, span) => span.clone(),
            Expr::Assign(_, _, span) => span.clone(),
            Expr::CompoundAssign(_, _, _, span) => span.clone(),
            Expr::Array(_, span) => span.clone(),
            Expr::ArrayIndex(_, _, span) => span.clone(),
            Expr::Range(_, _, _, span) => span.clone(),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Int(value, _) => write!(f, "{}", value),
            Expr::Float(value, _) => write!(f, "{}", value),
            Expr::String(value, _) => write!(f, "\"{}\"", value),
            Expr::Bool(value, _) => write!(f, "{}", value),
            Expr::EnumVariant {
                name,
                variant,
                value: Some(expr),
                ..
            } => write!(f, "{}::{}({})", name, variant, expr),
            Expr::EnumVariant {
                name,
                variant,
                value: None,
                ..
            } => write!(f, "{}::{}", name, variant),
            Expr::Identifier(name, _) => write!(f, "{}", name),
            Expr::BinaryOp(left, op, right, _) => write!(f, "({} {:?} {})", left, op, right),
            Expr::FunctionCall(name, args, _) => {
                write!(
                    f,
                    "{}({})",
                    name,
                    args.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expr::StructInit(name, fields, _) => {
                write!(
                    f,
                    "{} {{ {} }}",
                    name,
                    fields
                        .iter()
                        .map(|(name, value)| format!("{}: {}", name, value))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expr::MemberAccess(expr, field, _) => write!(f, "{}.{}", expr, field),
            Expr::Assign(left, right, _) => write!(f, "{} = {}", left, right),
            Expr::CompoundAssign(left, op, right, _) => write!(f, "{} {:?}= {}", left, op, right),
            Expr::Array(elements, _) => {
                write!(
                    f,
                    "[{}]",
                    elements
                        .iter()
                        .map(|element| element.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expr::ArrayIndex(array, index, _) => write!(f, "{}[{}]", array, index),
            Expr::Range(start, end, inclusive, _) => {
                if *inclusive {
                    write!(f, "{:?}..={:?}", start, end)?;
                } else {
                    write!(f, "{:?}..{:?}", start, end)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stmt {
    Let(String, Expr, Span),
    Function(String, Vec<(String, String)>, String, Vec<Stmt>, Span),
    If(Expr, Vec<Stmt>, Vec<Stmt>, Span),
    Match(Expr, Vec<(Expr, Vec<Stmt>)>, Span),
    Expression(Expr),
    Struct(String, Vec<(String, String)>, Span),
    Enum(String, Vec<(String, String)>, Span),
    ForLoop(String, Expr, Vec<Stmt>, Span),
    Break(Span),
    Continue(Span),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equals,
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
            TokenKind::Enum => self.parse_enum(),
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Function => self.parse_function(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::Match => self.parse_match_statement(),
            TokenKind::For => self.parse_for_loop(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Continue => self.parse_continue(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_break(&mut self) -> Result<Stmt> {
        let span = self.current_token().span;
        self.advance(); // Consume 'break'
        if self.current_token().kind != TokenKind::Semicolon {
            return Err(Error::new_parse(
                "Expected ';' after 'break'".to_string(),
                self.current_token().span,
            ));
        }
        self.advance(); // Consume ';'
        Ok(Stmt::Break(span))
    }

    fn parse_continue(&mut self) -> Result<Stmt> {
        let span = self.current_token().span;
        self.advance(); // Consume 'continue'
        if self.current_token().kind != TokenKind::Semicolon {
            return Err(Error::new_parse(
                "Expected ';' after 'continue'".to_string(),
                self.current_token().span,
            ));
        }
        self.advance(); // Consume ';'
        Ok(Stmt::Continue(span))
    }

    fn parse_for_loop(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span;
        self.advance(); // Consume 'for'

        let variable = self.consume_identifier()?.ok_or_else(|| {
            Error::new_parse(
                "Expected identifier after 'for'".to_string(),
                self.current_token().span,
            )
        })?;

        if self.current_token().kind != TokenKind::In {
            return Err(Error::new_parse(
                "Expected 'in' after for loop variable".to_string(),
                self.current_token().span,
            ));
        }
        self.advance(); // Consume 'in'

        let iterable = self.parse_expression()?;

        if self.current_token().kind != TokenKind::LBrace {
            return Err(Error::new_parse(
                "Expected '{' after for loop iterable".to_string(),
                self.current_token().span,
            ));
        }
        self.advance(); // Consume '{'

        let body = self.parse_block()?;

        let end_span = self.current_token().span;
        self.advance(); // Consume '}'

        Ok(Stmt::ForLoop(
            variable,
            iterable,
            body,
            Span::new(start_span.start, end_span.end),
        ))
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
            let field_type = self.consume_type().unwrap();
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

    fn parse_enum(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'enum'

        let Some(name) = self.consume_identifier()? else {
            return Err(Error::new_parse(
                "Expected enum name".to_string(),
                self.current_token().span,
            ));
        };

        if self.current_token().kind != TokenKind::LBrace {
            return Err(Error::new_parse(
                format!(
                    "Expected '{{' after enum name, found {:?}",
                    self.current_token()
                ),
                self.current_token().span,
            ));
        }
        self.advance(); // Consume '{'

        let mut variants = Vec::new();
        while self.current_token().kind != TokenKind::RBrace {
            let variant_name = self.consume_identifier()?.ok_or_else(|| {
                Error::new_parse(
                    "Expected variant name".to_string(),
                    self.current_token().span,
                )
            })?;

            let variant_type = if self.current_token().kind == TokenKind::LParen {
                self.advance(); // Consume '('
                let variant_type = self.consume_type().unwrap();
                if self.current_token().kind != TokenKind::RParen {
                    return Err(Error::new_parse(
                        "Expected ')' after variant type".to_string(),
                        self.current_token().span,
                    ));
                }
                self.advance(); // Consume ')'
                variant_type
            } else {
                "unit".to_string()
            };
            variants.push((variant_name, variant_type));

            if self.current_token().kind == TokenKind::Comma {
                self.advance(); // Consume ','
            } else if self.current_token().kind != TokenKind::RBrace {
                return Err(Error::new_parse(
                    "Expected ',' or '}' in variant list".to_string(),
                    self.current_token().span,
                ));
            }
        }

        let end_span = self.current_token().span.end;
        self.advance(); // Consume '}'
        Ok(Stmt::Enum(name, variants, Span::new(start_span, end_span)))
    }

    fn parse_let_statement(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'let'

        let Some(name) = self.consume_identifier()? else {
            return Err(Error::new_parse(
                "Expected identifier after 'let'".to_string(),
                self.current_token().span,
            ));
        };

        if self.current_token().kind != TokenKind::Equals {
            return Err(Error::new_parse(
                format!(
                    "Expected '=' after identifier, found {:?}",
                    self.current_token()
                ),
                self.current_token().span,
            ));
        }

        self.advance(); // Consume '='
        let expr = self.parse_expression()?;

        if self.current_token().kind != TokenKind::Semicolon {
            return Err(Error::new_parse(
                format!(
                    "Expected ';' after expression, found {:?}",
                    self.current_token()
                ),
                self.current_token().span,
            ));
        }

        let end_span = self.current_token().span.end;
        self.advance(); // Consume ';'

        Ok(Stmt::Let(name, expr, Span::new(start_span, end_span)))
    }

    fn parse_if_statement(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'if'

        let condition = self.parse_expression()?;

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

    fn parse_match_statement(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'match'

        let expr = self.parse_expression()?;

        if self.current_token().kind != TokenKind::LBrace {
            return Err(Error::new_parse(
                "Expected '{' after match expression".to_string(),
                self.current_token().span,
            ));
        }

        self.advance(); // Consume '{'

        let mut arms = Vec::new();
        while self.current_token().kind != TokenKind::RBrace {
            let pattern = self.parse_expression()?;

            if self.current_token().kind != TokenKind::FatArrow {
                return Err(Error::new_parse(
                    "Expected '=>' after pattern".to_string(),
                    self.current_token().span,
                ));
            }
            self.advance(); // Consume '=>'

            if self.current_token().kind == TokenKind::LBrace {
                self.advance(); // Consume '{'
                let body = self.parse_block()?;
                arms.push((pattern, body));
            } else {
                let stmt = self.parse_expression()?;
                arms.push((pattern, vec![Stmt::Expression(stmt)]));
            }

            // TODO: refine need for comma after match arm
            if self.current_token().kind == TokenKind::Comma {
                self.advance(); // Consume ','
            }
        }

        let end_span = self.current_token().span.end;
        self.advance(); // Consume '}'

        Ok(Stmt::Match(expr, arms, Span::new(start_span, end_span)))
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
            let param_type = self.consume_type().unwrap();
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
            self.consume_type().unwrap()
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

    fn consume_type(&mut self) -> Option<String> {
        let typ = match self.current_token().kind {
            TokenKind::Type(ref type_name) => Some(type_name.clone()),
            TokenKind::Identifier(ref name) => Some(name.clone()),
            _ => return None,
        };
        self.advance(); // Consume type
        typ
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
            return Ok(Stmt::Expression(expr));
        }

        Err(Error::new_parse(
            "Invalid expression statement".to_string(),
            self.current_token().span,
        ))
    }

    fn parse_expression(&mut self) -> Result<Expr> {
        let start_span = self.current_token().span.start;
        let mut left = self.parse_primary_expression()?;

        loop {
            match self.current_token().kind {
                TokenKind::PlusEquals | TokenKind::MinusEquals => {
                    let op = match self.current_token().kind {
                        TokenKind::PlusEquals => Operator::Plus,
                        TokenKind::MinusEquals => Operator::Minus,
                        _ => unreachable!(),
                    };
                    self.advance();
                    let right = self.parse_primary_expression()?;
                    let end_span = right.span().end;
                    left = Expr::CompoundAssign(
                        Box::new(left),
                        op,
                        Box::new(right),
                        Span::new(start_span, end_span),
                    );
                }
                _ => break,
            }
        }

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

        if self.current_token().kind == TokenKind::DblDot
            || self.current_token().kind == TokenKind::DblDotEquals
        {
            let inclusive = self.current_token().kind == TokenKind::DblDotEquals;
            left = self.parse_range_expression(Some(left.clone()), inclusive, start_span)?;
        }
        Ok(left)
    }

    fn parse_range_expression(
        &mut self,
        left_expr: Option<Expr>,
        inclusive: bool,
        start_span: usize,
    ) -> Result<Expr> {
        let mut end_span = self.current_token().span.end;
        self.advance(); // Consume '..'

        let kind = &self.current_token().kind;
        let right_expr = if *kind == TokenKind::Semicolon || *kind == TokenKind::RSquare {
            None
        } else {
            let expr = self.parse_expression()?;
            end_span = self.current_token().span.end;
            Some(Box::new(expr))
        };

        let left_expr = left_expr.map(Box::new);
        Ok(Expr::Range(
            left_expr,
            right_expr,
            inclusive,
            Span::new(start_span, end_span),
        ))
    }

    fn parse_primary_expression(&mut self) -> Result<Expr> {
        let span = self.current_token().span;
        let mut expr = match self.current_token().kind {
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
            TokenKind::DblDot | TokenKind::DblDotEquals => {
                let inclusive = self.current_token().kind == TokenKind::DblDotEquals;
                self.parse_range_expression(None, inclusive, span.start)
            }
            TokenKind::LSquare => self.parse_array(),
            TokenKind::Identifier(ref name) => {
                let name = name.clone();
                let start_pos = self.position;
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
                    TokenKind::LBrace if self.lookahead_for_struct_initialization(start_pos) => {
                        self.parse_struct_init(name, span)
                    }
                    TokenKind::Colon => self.parse_enum_expression(name, span),
                    _ => Ok(Expr::Identifier(name, span)),
                }
            }
            _ => Err(Error::new_parse(
                "Invalid primary expression".to_string(),
                span,
            )),
        }?;

        while self.current_token().kind == TokenKind::LSquare {
            let start_span = expr.span();
            self.advance(); // Consume '['
            let index = self.parse_expression()?;
            let end_span = self.current_token().span;

            if self.current_token().kind != TokenKind::RSquare {
                return Err(Error::new_parse(
                    "Expected ']' after array index".to_string(),
                    self.current_token().span,
                ));
            }
            self.advance(); // Consume ']'

            expr = Expr::ArrayIndex(
                Box::new(expr),
                Box::new(index),
                Span::new(start_span.start, end_span.end),
            );
        }

        Ok(expr)
    }

    fn parse_enum_expression(&mut self, name: String, span: Span) -> Result<Expr> {
        self.advance(); // Consume first ':'

        if self.current_token().kind != TokenKind::Colon {
            return Err(Error::new_parse(
                format!(
                    "Expected '::' after enum name, got {:?}",
                    self.current_token()
                ),
                self.current_token().span,
            ));
        }
        self.advance(); // Consume second ':'

        let variant_name = self.consume_identifier()?.ok_or_else(|| {
            Error::new_parse(
                "Expected enum variant name".to_string(),
                self.current_token().span,
            )
        })?;

        // variant value
        if self.current_token().kind == TokenKind::LParen {
            self.advance(); // Consume '('
            let expr = self.parse_expression()?;
            let span = Span::new(span.start, self.current_token().span.end);

            if self.current_token().kind != TokenKind::RParen {
                return Err(Error::new_parse(
                    "Expected ')' after enum variant value".to_string(),
                    self.current_token().span,
                ));
            }

            self.advance(); // Consume ')'
            return Ok(Expr::EnumVariant {
                name,
                variant: variant_name,
                value: Some(Box::new(expr)),
                span,
            });
        }

        let span = Span::new(span.start, self.current_token().span.end);
        Ok(Expr::EnumVariant {
            name,
            variant: variant_name,
            value: None,
            span,
        })
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

    fn lookahead_for_struct_initialization(&self, start_pos: usize) -> bool {
        if start_pos > 1 {
            let lookback_index = self.position - 2;
            let before_identifier = &self.tokens[lookback_index];
            if before_identifier.kind == TokenKind::If
                || before_identifier.kind == TokenKind::For
                || before_identifier.kind == TokenKind::In
                || is_operator(&before_identifier.kind)
            {
                return false;
            }
        }
        let mut lookahead_index = self.position + 1;
        while lookahead_index < self.tokens.len() {
            match self.tokens[lookahead_index].kind {
                TokenKind::FatArrow => return false,
                TokenKind::RBrace => return true,
                _ => false,
            };
            lookahead_index += 1;
        }
        false
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

    fn parse_operator(&mut self) -> Option<Operator> {
        match self.current_token().kind {
            TokenKind::Plus => Some(Operator::Plus),
            TokenKind::Minus => Some(Operator::Minus),
            TokenKind::Asterisk => Some(Operator::Multiply),
            TokenKind::Slash => Some(Operator::Divide),
            TokenKind::DblEquals => return Some(Operator::Equals),
            _ => None,
        }
    }

    fn parse_array(&mut self) -> std::result::Result<Expr, Error> {
        let start = self.current_token().span.start;
        self.advance(); // Consume '['

        let mut elements = Vec::new();
        while self.current_token().kind != TokenKind::RSquare {
            elements.push(self.parse_expression()?);

            if self.current_token().kind == TokenKind::Comma {
                self.advance(); // Consume ','
            } else if self.current_token().kind != TokenKind::RSquare {
                return Err(Error::new_parse(
                    "Expected ',' or ']' in array".to_string(),
                    self.current_token().span,
                ));
            }
        }

        self.advance(); // Consume ']'
        let end = self.current_token().span.end;

        Ok(Expr::Array(elements, Span::new(start, end)))
    }
}

fn is_operator(kind: &TokenKind) -> bool {
    match kind {
        TokenKind::Plus
        | TokenKind::Minus
        | TokenKind::Asterisk
        | TokenKind::Slash
        | TokenKind::DblEquals => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse(code: &str) -> Vec<Stmt> {
        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        match parser.parse() {
            Ok(ast) => ast,
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

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
            Expr::Bool(true, Span::new(8, 12)),
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

    #[test]
    fn test_parse_function() {
        let input = "fn add(x: int, y: int) -> int { x + y }";
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);

        let ast = parser.parse().unwrap();

        assert_eq!(
            ast[0],
            Stmt::Function(
                "add".to_string(),
                vec![
                    ("x".to_string(), "int".to_string()),
                    ("y".to_string(), "int".to_string()),
                ],
                "int".to_string(),
                vec![Stmt::Expression(Expr::BinaryOp(
                    Box::new(Expr::Identifier("x".to_string(), Span::new(32, 33))),
                    Operator::Plus,
                    Box::new(Expr::Identifier("y".to_string(), Span::new(36, 37))),
                    Span::new(32, 35),
                ))],
                Span::new(0, 39),
            )
        );
    }

    #[test]
    fn test_parse_function_returning_struct() {
        let input = r#"
            fn create_person(name: string, age: int) -> Person {
                Person {
                    name: name,
                    age: age,
                }
            }
        "#;

        let ast = parse(input);

        assert_eq!(
            ast[0],
            Stmt::Function(
                "create_person".to_string(),
                vec![
                    ("name".to_string(), "string".to_string()),
                    ("age".to_string(), "int".to_string()),
                ],
                "Person".to_string(),
                vec![Stmt::Expression(Expr::StructInit(
                    "Person".to_string(),
                    vec![
                        (
                            "name".to_string(),
                            Expr::Identifier("name".to_string(), Span::new(117, 121))
                        ),
                        (
                            "age".to_string(),
                            Expr::Identifier("age".to_string(), Span::new(148, 151))
                        ),
                    ],
                    Span::new(82, 170),
                ))],
                Span::new(13, 184),
            )
        );
    }

    #[test]
    fn test_eq() {
        let code = "x == y;";

        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(
            ast[0],
            Stmt::Expression(Expr::BinaryOp(
                Box::new(Expr::Identifier("x".to_string(), Span::new(0, 1))),
                Operator::Equals,
                Box::new(Expr::Identifier("y".to_string(), Span::new(5, 6))),
                Span::new(0, 4),
            ))
        );
    }

    #[test]
    fn test_if_expr_eq() {
        let code = r#"
            if x == y {
                1
            } else {
                0
            }
        "#;

        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(
            ast[0],
            Stmt::If(
                Expr::BinaryOp(
                    Box::new(Expr::Identifier("x".to_string(), Span::new(16, 17))),
                    Operator::Equals,
                    Box::new(Expr::Identifier("y".to_string(), Span::new(21, 22))),
                    Span::new(16, 20),
                ),
                vec![Stmt::Expression(Expr::Int(1, Span::new(41, 42)))],
                vec![Stmt::Expression(Expr::Int(0, Span::new(80, 81)))],
                Span::new(13, 95),
            )
        );
    }

    #[test]
    fn test_if_vs_struct() {
        let code = r#"
        let p = Person { name: "Felipe" };
        if y == x {
            println(p);
        }
        "#;

        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        assert!(parser.parse().is_ok());
    }

    #[test]
    fn test_parse_enum() {
        let code = r#"
            enum Color {
                Red,
                Green,
                Blue,
            }
        "#;

        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        let Stmt::Enum(enum_name, variants, _) = &ast[0] else {
            panic!("Expected Enum statement");
        };

        assert_eq!(enum_name, "Color");
        assert_eq!(
            variants,
            &[
                ("Red".to_string(), "unit".to_string()),
                ("Green".to_string(), "unit".to_string()),
                ("Blue".to_string(), "unit".to_string()),
            ]
        );
    }

    #[test]
    fn test_parse_enum_with_associated_value() {
        let code = r#"
            enum Name {
                Existing(string),
                New(string),
            }
        "#;

        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        let Stmt::Enum(enum_name, variants, _) = &ast[0] else {
            panic!("Expected Enum statement");
        };

        assert_eq!(enum_name, "Name");
        assert_eq!(
            variants,
            &[
                ("Existing".to_string(), "string".to_string()),
                ("New".to_string(), "string".to_string()),
            ]
        );
    }

    #[test]
    fn test_parse_enum_variant() {
        let code = r"let c = Color::Red;";

        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);

        match parser.parse() {
            Ok(ast) => {
                assert_eq!(
                    ast[0],
                    Stmt::Let(
                        "c".to_string(),
                        Expr::EnumVariant {
                            name: "Color".to_string(),
                            variant: "Red".to_string(),
                            value: None,
                            span: Span::new(8, 19)
                        },
                        Span::new(0, 19),
                    )
                );
            }
            Err(e) => {
                panic!("{}", e.pretty_print(code))
            }
        }
    }

    #[test]
    fn test_parse_enum_variant_with_associated_value() {
        let code = r#"let n = Name::Existing("Alice");"#;
        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Let(
                "n".to_string(),
                Expr::EnumVariant {
                    name: "Name".to_string(),
                    variant: "Existing".to_string(),
                    value: Some(Box::new(Expr::String(
                        "Alice".to_string(),
                        Span::new(23, 30)
                    ))),
                    span: Span::new(8, 31),
                },
                Span::new(0, 32),
            )
        );
    }

    #[test]
    fn test_parse_match_enum() {
        let code = r#"
            match n {
                Name::Existing(name) => name,
                Name::NotExisting => "Not existing",
            }
        "#;

        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);

        match parser.parse() {
            Ok(ast) => {
                println!("{:?}", ast);
            }
            Err(e) => {
                panic!("{}", e.pretty_print(code))
            }
        }
    }

    #[test]
    fn test_parse_match_pattern_as_expression() {
        let code = "Name::Existing(name)";
        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Expression(Expr::EnumVariant {
                name: "Name".to_string(),
                variant: "Existing".to_string(),
                value: Some(Box::new(Expr::Identifier(
                    "name".to_string(),
                    Span::new(15, 19)
                ))),
                span: Span::new(0, 20),
            })
        );
    }

    #[test]
    fn test_line_comments() {
        let code = r#"
            // This is a comment
            let x = 5; // Another comment
            // let y = 10;
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Let(
                "x".to_string(),
                Expr::Int(5, Span::new(54, 55)),
                Span::new(46, 56),
            )
        );
    }

    #[test]
    fn test_block_comments() {
        let code = r#"
            /* This is a comment */
            let x = 5; /* Another comment */
            /* let y = 10; */
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Let(
                "x".to_string(),
                Expr::Int(5, Span::new(57, 58)),
                Span::new(49, 59),
            )
        );
    }

    #[test]
    fn test_parse_array_decl() {
        let code = r#"
            let arr = [1,2,3,4,5];
            print(arr);
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Let(
                "arr".to_string(),
                Expr::Array(
                    vec![
                        Expr::Int(1, Span::new(24, 25)),
                        Expr::Int(2, Span::new(26, 27)),
                        Expr::Int(3, Span::new(28, 29)),
                        Expr::Int(4, Span::new(30, 31)),
                        Expr::Int(5, Span::new(32, 33)),
                    ],
                    Span::new(23, 35),
                ),
                Span::new(13, 35),
            )
        );
    }

    #[test]
    fn test_parse_array_index() {
        let code = r#"
            let arr = [1,2,3,4,5];
            let x = arr[0];
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[1],
            Stmt::Let(
                "x".to_string(),
                Expr::ArrayIndex(
                    Box::new(Expr::Identifier("arr".to_string(), Span::new(56, 59))),
                    Box::new(Expr::Int(0, Span::new(60, 61))),
                    Span::new(56, 62),
                ),
                Span::new(48, 63),
            )
        );
    }

    #[test]
    fn test_parse_range() {
        let code = r#"
            let range = 1..5;
            let x = range[0];
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Let(
                "range".to_string(),
                Expr::Range(
                    Option::Some(Box::new(Expr::Int(1, Span::new(25, 26)))),
                    Option::Some(Box::new(Expr::Int(5, Span::new(28, 29)))),
                    false,
                    Span::new(25, 29)
                ),
                Span::new(13, 30),
            )
        );
    }

    #[test]
    fn test_parse_inclusive_range() {
        let code = r#"
            let range = 1..=5;
            let x = range[0];
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Let(
                "range".to_string(),
                Expr::Range(
                    Option::Some(Box::new(Expr::Int(1, Span::new(25, 26)))),
                    Option::Some(Box::new(Expr::Int(5, Span::new(29, 30)))),
                    true,
                    Span::new(25, 30)
                ),
                Span::new(13, 31),
            )
        );
    }

    #[test]
    fn test_from_start_range() {
        let code = r#"
            let range = ..5;
            let x = range[0];
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Let(
                "range".to_string(),
                Expr::Range(
                    Option::None,
                    Option::Some(Box::new(Expr::Int(5, Span::new(27, 28)))),
                    false,
                    Span::new(25, 28)
                ),
                Span::new(13, 29),
            )
        );
    }

    #[test]
    fn test_from_start_inclusive_range() {
        let code = r#"
            let range = ..=5;
            let x = range[0];
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Let(
                "range".to_string(),
                Expr::Range(
                    Option::None,
                    Option::Some(Box::new(Expr::Int(5, Span::new(28, 29)))),
                    true,
                    Span::new(25, 29)
                ),
                Span::new(13, 30),
            )
        );
    }

    #[test]
    fn test_until_end_range() {
        let code = r#"
            let range = 1..;
            let x = range[0];
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Let(
                "range".to_string(),
                Expr::Range(
                    Option::Some(Box::new(Expr::Int(1, Span::new(25, 26)))),
                    None,
                    false,
                    Span::new(25, 28)
                ),
                Span::new(13, 29),
            )
        );
    }

    #[test]
    fn test_until_end_inclusve_range() {
        let code = r#"
            let range = 1..=;
            let x = range[0];
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Let(
                "range".to_string(),
                Expr::Range(
                    Option::Some(Box::new(Expr::Int(1, Span::new(25, 26)))),
                    None,
                    true,
                    Span::new(25, 29)
                ),
                Span::new(13, 30),
            )
        );
    }

    #[test]
    fn test_parse_for_loop() {
        let input = r#"
            for x in 1..5 {
                print(x);
            }
        "#;
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);

        let ast = parser.parse().unwrap();
        assert!(matches!(ast[0], Stmt::ForLoop(_, _, _, _)));
    }

    #[test]
    fn test_parse_compound_assignment_plus() {
        let code = "x += 1;";

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Expression(Expr::CompoundAssign(
                Box::new(Expr::Identifier("x".to_string(), Span::new(13, 14))),
                Operator::Plus,
                Box::new(Expr::Int(1, Span::new(17, 18))),
                Span::new(13, 22),
            ))
        );
    }

    #[test]
    fn test_parse_compound_assignment_minus() {
        let code = "x -= 1;";

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Expression(Expr::CompoundAssign(
                Box::new(Expr::Identifier("x".to_string(), Span::new(13, 14))),
                Operator::Minus,
                Box::new(Expr::Int(1, Span::new(17, 18))),
                Span::new(13, 22),
            ))
        );
    }
}
