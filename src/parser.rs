use std::fmt;

use crate::{
    ast::{TypeAnnotation, TypeAnnotationRef},
    error::{Error, Result},
    lexer::{Token, TokenKind, EOF},
    span::Span,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Attribute {
    pub name: String,
    pub args: Vec<String>,
    pub span: Span,
}

impl Attribute {
    pub fn new(name: String, span: Span) -> Self {
        Attribute {
            name,
            args: vec![],
            span,
        }
    }

    pub fn with_args(name: String, args: Vec<String>, span: Span) -> Self {
        Attribute { name, args, span }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64, Span),
    Float(f64, Span),
    String(String, Span),
    Bool(bool, Span),
    Unit(Span),
    Array(Vec<Expr>, Span),
    ArrayIndex(Box<Expr>, Box<Expr>, Span),
    Identifier(String, Span),
    BinaryOp(Box<Expr>, Operator, Box<Expr>, Span),
    UnaryOp(UnaryOp, Box<Expr>, Span),
    FunctionCall(String, Vec<Expr>, Span),
    StructInit(String, Vec<(String, Expr)>, Span),
    MemberAccess(Box<Expr>, String, Span),
    Assign(Box<Expr>, Box<Expr>, Span),
    CompoundAssign(Box<Expr>, Operator, Box<Expr>, Span),
    Range(Option<Box<Expr>>, Option<Box<Expr>>, bool, Span),
    Tuple(Vec<Expr>, Span),
    EnumVariantOrMethodCall {
        target: Box<Expr>,
        call: String,
        args: Vec<Expr>,
        span: Span,
        type_annotation: TypeAnnotationRef,
    },
    Block(Vec<Stmt>, Span),
    If(Box<Expr>, Vec<Stmt>, Vec<Stmt>, Span),
    Match(Box<Expr>, Vec<(Expr, Vec<Stmt>)>, Span),
    Await(Box<Expr>, Span),
    Try(Box<Expr>, Span),
    AwaitTry(Box<Expr>, Span),
    Closure(
        Vec<(String, Option<String>)>,
        Option<String>,
        Box<Expr>,
        Span,
    ),
    MethodCall(Box<Expr>, String, Vec<Expr>, Span), // object.method(args)
    Cast(Box<Expr>, String, Span),                  // expr as type
    StructPattern(String, Vec<(String, Option<String>)>, Span), // EnumVariant::Name { field1: var1, field2, ... }
    ObjectLiteral(Vec<(String, Expr)>, Span),                   // { key: value, ... }
    MacroCall(String, Vec<Expr>, Span),                         // macro!(args)
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
            Expr::Unit(_) => {
                matches!(other, Expr::Unit(_))
            }
            Expr::Array(elements, _) => {
                if let Expr::Array(other_elements, _) = other {
                    elements == other_elements
                } else {
                    false
                }
            }
            Expr::Tuple(elements, _) => {
                if let Expr::Tuple(other_elements, _) = other {
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
            Expr::UnaryOp(op, expr, _) => {
                if let Expr::UnaryOp(other_op, other_expr, _) = other {
                    op == other_op && expr == other_expr
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
            Expr::EnumVariantOrMethodCall {
                target,
                call,
                args,
                span: _,
                type_annotation: _,
            } => {
                if let Expr::EnumVariantOrMethodCall {
                    target: other_target,
                    call: other_call,
                    args: other_args,
                    span: _,
                    type_annotation: _,
                } = other
                {
                    target == other_target && call == other_call && args == other_args
                } else {
                    false
                }
            }
            Expr::Block(stmts, _) => {
                if let Expr::Block(other_stmts, _) = other {
                    stmts == other_stmts
                } else {
                    false
                }
            }
            Expr::If(cond, then_block, else_block, _) => {
                if let Expr::If(other_cond, other_then, other_else, _) = other {
                    cond == other_cond && then_block == other_then && else_block == other_else
                } else {
                    false
                }
            }
            Expr::Await(expr, _) => {
                if let Expr::Await(other_expr, _) = other {
                    expr == other_expr
                } else {
                    false
                }
            }
            Expr::Try(expr, _) => {
                if let Expr::Try(other_expr, _) = other {
                    expr == other_expr
                } else {
                    false
                }
            }
            Expr::AwaitTry(expr, _) => {
                if let Expr::AwaitTry(other_expr, _) = other {
                    expr == other_expr
                } else {
                    false
                }
            }
            Expr::Match(expr, arms, _) => {
                if let Expr::Match(other_expr, other_arms, _) = other {
                    expr == other_expr && arms == other_arms
                } else {
                    false
                }
            }
            Expr::Closure(params, ret_type, body, _) => {
                if let Expr::Closure(other_params, other_ret_type, other_body, _) = other {
                    params == other_params && ret_type == other_ret_type && body == other_body
                } else {
                    false
                }
            }
            Expr::MethodCall(obj, method, args, _) => {
                if let Expr::MethodCall(other_obj, other_method, other_args, _) = other {
                    obj == other_obj && method == other_method && args == other_args
                } else {
                    false
                }
            }
            Expr::Cast(expr, target_type, _) => {
                if let Expr::Cast(other_expr, other_type, _) = other {
                    expr == other_expr && target_type == other_type
                } else {
                    false
                }
            }
            Expr::StructPattern(variant, fields, _) => {
                if let Expr::StructPattern(other_variant, other_fields, _) = other {
                    variant == other_variant && fields == other_fields
                } else {
                    false
                }
            }
            Expr::ObjectLiteral(fields, _) => {
                if let Expr::ObjectLiteral(other_fields, _) = other {
                    fields == other_fields
                } else {
                    false
                }
            }
            Expr::MacroCall(name, args, _) => {
                if let Expr::MacroCall(other_name, other_args, _) = other {
                    name == other_name && args == other_args
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
            Expr::Int(_, span) => *span,
            Expr::Float(_, span) => *span,
            Expr::String(_, span) => *span,
            Expr::Bool(_, span) => *span,
            Expr::Unit(span) => *span,
            Expr::Identifier(_, span) => *span,
            Expr::BinaryOp(_, _, _, span) => *span,
            Expr::UnaryOp(_, _, span) => *span,
            Expr::FunctionCall(_, _, span) => *span,
            Expr::StructInit(_, _, span) => *span,
            Expr::MemberAccess(_, _, span) => *span,
            Expr::Assign(_, _, span) => *span,
            Expr::CompoundAssign(_, _, _, span) => *span,
            Expr::Array(_, span) => *span,
            Expr::ArrayIndex(_, _, span) => *span,
            Expr::Range(_, _, _, span) => *span,
            Expr::Tuple(_, span) => *span,
            Expr::EnumVariantOrMethodCall { span, .. } => *span,
            Expr::Block(_, span) => *span,
            Expr::If(_, _, _, span) => *span,
            Expr::Await(_, span) => *span,
            Expr::Try(_, span) => *span,
            Expr::AwaitTry(_, span) => *span,
            Expr::Match(_, _, span) => *span,
            Expr::Closure(_, _, _, span) => *span,
            Expr::MethodCall(_, _, _, span) => *span,
            Expr::Cast(_, _, span) => *span,
            Expr::StructPattern(_, _, span) => *span,
            Expr::ObjectLiteral(_, span) => *span,
            Expr::MacroCall(_, _, span) => *span,
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
            Expr::Unit(_) => write!(f, "()"),
            Expr::EnumVariantOrMethodCall {
                target, call, args, ..
            } => {
                if args.is_empty() {
                    write!(f, "{}::{}", target, call)
                } else {
                    write!(
                        f,
                        "{}::{}({})",
                        target,
                        call,
                        args.iter()
                            .map(|arg| arg.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                }
            }
            Expr::Identifier(name, _) => write!(f, "{}", name),
            Expr::BinaryOp(left, op, right, _) => write!(f, "({} {:?} {})", left, op, right),
            Expr::UnaryOp(op, expr, _) => write!(f, "({:?} {})", op, expr),
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
            Expr::Tuple(elements, _) => {
                write!(
                    f,
                    "({})",
                    elements
                        .iter()
                        .map(|element| element.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expr::Range(start, end, inclusive, _) => {
                if *inclusive {
                    write!(f, "{:?}..={:?}", start, end)?;
                } else {
                    write!(f, "{:?}..{:?}", start, end)?;
                }
                Ok(())
            }
            Expr::Block(_stmts, _) => {
                write!(f, "{{ ... }}") // Simple representation for now
            }
            Expr::If(_cond, _then, _else, _) => {
                write!(f, "if ... {{ ... }}") // Simple representation for now
            }
            Expr::Await(expr, _) => write!(f, "{}.await", expr),
            Expr::Try(expr, _) => write!(f, "{}?", expr),
            Expr::AwaitTry(expr, _) => write!(f, "{}.await?", expr),
            Expr::Match(_expr, _arms, _) => {
                write!(f, "match ... {{ ... }}") // Simple representation for now
            }
            Expr::Closure(params, ret_type, _body, _) => {
                let param_str = params
                    .iter()
                    .map(|(name, ty)| {
                        if let Some(ty) = ty {
                            format!("{}: {}", name, ty)
                        } else {
                            name.clone()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                if let Some(ret) = ret_type {
                    write!(f, "|{}| -> {} {{ ... }}", param_str, ret)
                } else {
                    write!(f, "|{}| {{ ... }}", param_str)
                }
            }
            Expr::MethodCall(obj, method, args, _) => {
                write!(
                    f,
                    "{}.{}({})",
                    obj,
                    method,
                    args.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expr::Cast(expr, target_type, _) => write!(f, "{} as {}", expr, target_type),
            Expr::StructPattern(variant, fields, _) => {
                let field_strings: Vec<String> = fields
                    .iter()
                    .map(|(field, rename)| match rename {
                        Some(var_name) => format!("{}: {}", field, var_name),
                        None => field.clone(),
                    })
                    .collect();
                write!(f, "{} {{ {} }}", variant, field_strings.join(", "))
            }
            Expr::ObjectLiteral(fields, _) => {
                let field_strings: Vec<String> = fields
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect();
                write!(f, "{{ {} }}", field_strings.join(", "))
            }
            Expr::MacroCall(name, args, _) => {
                write!(
                    f,
                    "{}!({})",
                    name,
                    args.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stmt {
    Let(String, Expr, Span),
    Function(
        Vec<Attribute>,
        bool,
        String,
        Vec<String>,
        Vec<(String, String)>,
        String,
        Vec<Stmt>,
        Span,
    ), // attributes, visibility flag, name, generic params, params, return type, body, span
    Match(Expr, Vec<(Expr, Vec<Stmt>)>, Span),
    Expression(Expr, bool), // Second bool indicates if there's a semicolon
    Struct(String, Vec<String>, Vec<(String, String)>, Span), // Added generic params
    Impl(String, Vec<Stmt>, Span),
    Enum(String, Vec<String>, Vec<EnumVariant>, Span), // Added generic params
    ForLoop(Expr, Expr, Vec<Stmt>, Span),              // pattern, iterable, body, span
    While(Expr, Vec<Stmt>, Span),
    Loop(Vec<Stmt>, Span),
    Break(Span),
    Continue(Span),
    Return(Option<Expr>, Span),
    Use(UsePath, UseItems, Span),
    ExternFunction(String, Vec<String>, Vec<(String, String)>, String, Span), // Added generic params
    ExternMod(String, Vec<ExternItem>, Span),
    ExternType(String, Vec<String>, Span), // name, generic params, span
    AsyncFunction(
        Vec<Attribute>,
        bool,
        String,
        Vec<String>,
        Vec<(String, String)>,
        String,
        Vec<Stmt>,
        Span,
    ), // attributes, visibility flag, name, generic params, params, return type, body, span
    Module(Vec<Attribute>, String, Vec<Stmt>, Span), // attributes, name, body, span
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum EnumVariant {
    Unit(String),                          // Simple variant: Color
    Tuple(String, String),                 // Tuple variant: Color(string)
    Struct(String, Vec<(String, String)>), // Struct variant: Color { r: int, g: int, b: int }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExternItem {
    Function(String, Vec<String>, Vec<(String, String)>, String), // Added generic params
    Type(String, Vec<String>), // Added generic params for type aliases
    Mod(String, Vec<ExternItem>),
    Impl(String, Vec<ExternItem>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UsePath {
    pub prefix: UsePrefix,
    pub segments: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum UsePrefix {
    None,       // External package
    Local,      // local::
    Super(u32), // super:: (count for nested super)
    Self_,      // self::
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum UseItems {
    All,                                  // use module::*;
    Single,                               // use module::item;
    Named(Vec<(String, Option<String>)>), // use module::{a, b as c};
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equals,
    NotEquals,
    Modulo,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg, // -
    Not, // !
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

    fn peek_token(&self) -> &Token {
        self.tokens.get(self.position + 1).unwrap_or(&EOF)
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

    fn parse_attributes(&mut self) -> Result<Vec<Attribute>> {
        let mut attributes = Vec::new();

        while self.current_token().kind == TokenKind::Hash {
            let start_span = self.current_token().span;
            self.advance(); // consume #

            // Expect [
            if self.current_token().kind != TokenKind::LSquare {
                return Err(Error::new_parse(
                    "Expected '[' after '#' in attribute".to_string(),
                    self.current_token().span,
                ));
            }
            self.advance(); // consume [

            // Parse attribute name
            let name = match &self.current_token().kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => {
                    return Err(Error::new_parse(
                        "Expected attribute name".to_string(),
                        self.current_token().span,
                    ))
                }
            };
            self.advance();

            let mut args = Vec::new();

            // Check for attribute arguments
            if self.current_token().kind == TokenKind::LParen {
                self.advance(); // consume (

                // Parse arguments
                while self.current_token().kind != TokenKind::RParen {
                    match &self.current_token().kind {
                        TokenKind::Identifier(arg) => {
                            args.push(arg.clone());
                            self.advance();

                            if self.current_token().kind == TokenKind::Comma {
                                self.advance();
                            } else if self.current_token().kind != TokenKind::RParen {
                                return Err(Error::new_parse(
                                    "Expected ',' or ')' in attribute arguments".to_string(),
                                    self.current_token().span,
                                ));
                            }
                        }
                        _ => {
                            return Err(Error::new_parse(
                                "Expected identifier in attribute arguments".to_string(),
                                self.current_token().span,
                            ))
                        }
                    }
                }

                self.advance(); // consume )
            }

            // Expect ]
            if self.current_token().kind != TokenKind::RSquare {
                return Err(Error::new_parse(
                    "Expected ']' to close attribute".to_string(),
                    self.current_token().span,
                ));
            }
            let end_span = self.current_token().span;
            self.advance(); // consume ]

            let span = Span::new(start_span.start, end_span.end);
            attributes.push(Attribute::with_args(name, args, span));
        }

        Ok(attributes)
    }

    fn parse_statement(&mut self) -> Result<Stmt> {
        // Parse attributes first
        let attributes = self.parse_attributes()?;

        // Check for pub keyword
        let is_pub = if self.current_token().kind == TokenKind::Pub {
            self.advance(); // Consume 'pub'
            true
        } else {
            false
        };

        // Validate that attributes are only used where allowed
        if !attributes.is_empty() {
            match self.current_token().kind {
                TokenKind::Function | TokenKind::Async | TokenKind::Mod => {
                    // Attributes are allowed
                }
                _ => {
                    return Err(Error::new_parse(
                        "Attributes can only be applied to functions and modules".to_string(),
                        attributes[0].span,
                    ));
                }
            }
        }

        let stmt = match self.current_token().kind {
            TokenKind::Struct => self.parse_struct(),
            TokenKind::Impl => self.parse_impl(),
            TokenKind::Enum => self.parse_enum(),
            TokenKind::Let => {
                if is_pub {
                    return Err(Error::new_parse(
                        "'pub' cannot be used with 'let' statements".to_string(),
                        self.current_token().span,
                    ));
                }
                self.parse_let_statement()
            }
            TokenKind::Use => {
                if is_pub {
                    return Err(Error::new_parse(
                        "'pub' cannot be used with 'use' statements".to_string(),
                        self.current_token().span,
                    ));
                }
                self.parse_use_statement()
            }
            TokenKind::Function => self.parse_function(attributes, is_pub),
            TokenKind::Async => {
                if is_pub {
                    // pub async fn is allowed
                }
                self.parse_async_function(attributes, is_pub)
            }
            TokenKind::Mod => {
                if !attributes.is_empty() {
                    // Handle #[cfg(test)] mod tests { ... }
                    self.parse_module(attributes)
                } else {
                    return Err(Error::new_parse(
                        "Module declarations require attributes".to_string(),
                        self.current_token().span,
                    ));
                }
            }
            TokenKind::Extern => {
                if is_pub {
                    return Err(Error::new_parse(
                        "'pub' cannot be used with 'extern' declarations".to_string(),
                        self.current_token().span,
                    ));
                }
                self.parse_extern()
            }
            TokenKind::Match => {
                if is_pub {
                    return Err(Error::new_parse(
                        "'pub' cannot be used with 'match' statements".to_string(),
                        self.current_token().span,
                    ));
                }
                self.parse_match_statement()
            }
            TokenKind::For => {
                if is_pub {
                    return Err(Error::new_parse(
                        "'pub' cannot be used with 'for' loops".to_string(),
                        self.current_token().span,
                    ));
                }
                self.parse_for_loop()
            }
            TokenKind::While => {
                if is_pub {
                    return Err(Error::new_parse(
                        "'pub' cannot be used with 'while' loops".to_string(),
                        self.current_token().span,
                    ));
                }
                self.parse_while_statement()
            }
            TokenKind::Loop => {
                if is_pub {
                    return Err(Error::new_parse(
                        "'pub' cannot be used with 'loop' statements".to_string(),
                        self.current_token().span,
                    ));
                }
                self.parse_loop_statement()
            }
            TokenKind::Break => {
                if is_pub {
                    return Err(Error::new_parse(
                        "'pub' cannot be used with 'break' statements".to_string(),
                        self.current_token().span,
                    ));
                }
                self.parse_break()
            }
            TokenKind::Continue => {
                if is_pub {
                    return Err(Error::new_parse(
                        "'pub' cannot be used with 'continue' statements".to_string(),
                        self.current_token().span,
                    ));
                }
                self.parse_continue()
            }
            TokenKind::Return => {
                if is_pub {
                    return Err(Error::new_parse(
                        "'pub' cannot be used with 'return' statements".to_string(),
                        self.current_token().span,
                    ));
                }
                self.parse_return()
            }
            _ => {
                if is_pub {
                    return Err(Error::new_parse(
                        "'pub' can only be used with functions, structs, enums, or impl blocks"
                            .to_string(),
                        self.current_token().span,
                    ));
                }
                self.parse_expression_statement()
            }
        };

        // TODO: Store visibility information in AST once we enhance the structure
        // For now, we just parse and ignore pub
        stmt
    }

    fn parse_loop_statement(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'loop'

        if self.current_token().kind != TokenKind::LBrace {
            return Err(Error::new_parse(
                "Expected '{' after 'loop'".to_string(),
                self.current_token().span,
            ));
        }

        self.advance(); // Consume '{'
        let body = self.parse_block()?;

        let end_span = self.current_token().span.end;
        self.advance(); // Consume '}'

        Ok(Stmt::Loop(body, Span::new(start_span, end_span)))
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

    fn parse_return(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'return'

        // Check if this is `return;` (no expression)
        if self.current_token().kind == TokenKind::Semicolon {
            let end_span = self.current_token().span.end;
            self.advance(); // Consume ';'
            return Ok(Stmt::Return(None, Span::new(start_span, end_span)));
        }

        // Parse return expression
        let expr = self.parse_expression()?;
        let end_span = self.current_token().span.end;

        // Expect semicolon after return expression
        if self.current_token().kind != TokenKind::Semicolon {
            return Err(Error::new_parse(
                "Expected ';' after return expression".to_string(),
                self.current_token().span,
            ));
        }
        self.advance(); // Consume ';'

        Ok(Stmt::Return(Some(expr), Span::new(start_span, end_span)))
    }

    fn parse_for_loop(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span;
        self.advance(); // Consume 'for'

        // Parse the loop pattern (can be identifier or tuple)
        let pattern = if self.current_token().kind == TokenKind::LParen {
            // Parse tuple pattern like (a, b)
            let start_span = self.current_token().span;
            self.advance(); // Consume '('

            // Check for empty tuple
            if self.current_token().kind == TokenKind::RParen {
                return Err(Error::new_parse(
                    "Empty tuple patterns are not allowed in for loops".to_string(),
                    self.current_token().span,
                ));
            }

            let mut elements = Vec::new();

            // Parse first identifier
            let first_id = self.consume_identifier("parse_for_loop")?.ok_or_else(|| {
                Error::new_parse(
                    "Expected identifier in tuple pattern".to_string(),
                    self.current_token().span,
                )
            })?;
            elements.push(Expr::Identifier(first_id, self.current_token().span));

            // Parse remaining identifiers
            while self.current_token().kind == TokenKind::Comma {
                self.advance(); // Consume ','

                // Allow trailing comma
                if self.current_token().kind == TokenKind::RParen {
                    break;
                }

                let id = self.consume_identifier("parse_for_loop")?.ok_or_else(|| {
                    Error::new_parse(
                        "Expected identifier in tuple pattern".to_string(),
                        self.current_token().span,
                    )
                })?;
                elements.push(Expr::Identifier(id, self.current_token().span));
            }

            if self.current_token().kind != TokenKind::RParen {
                return Err(Error::new_parse(
                    "Expected ')' after tuple pattern".to_string(),
                    self.current_token().span,
                ));
            }
            let end_span = self.current_token().span.end;
            self.advance(); // Consume ')'

            Expr::Tuple(elements, Span::new(start_span.start, end_span))
        } else {
            // Parse single identifier
            let variable = self.consume_identifier("parse_for_loop")?.ok_or_else(|| {
                Error::new_parse(
                    "Expected identifier or tuple pattern after 'for'".to_string(),
                    self.current_token().span,
                )
            })?;
            Expr::Identifier(variable, self.current_token().span)
        };

        if self.current_token().kind != TokenKind::In {
            return Err(Error::new_parse(
                "Expected 'in' after for loop pattern".to_string(),
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
            pattern,
            iterable,
            body,
            Span::new(start_span.start, end_span.end),
        ))
    }

    fn parse_while_statement(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'while'

        let condition = self.parse_expression()?;

        if self.current_token().kind != TokenKind::LBrace {
            return Err(Error::new_parse(
                "Expected '{' after while condition".to_string(),
                self.current_token().span,
            ));
        }

        self.advance(); // Consume '{'
        let body = self.parse_block()?;

        let end_span = self.current_token().span.end;
        self.advance(); // Consume '}'

        Ok(Stmt::While(
            condition,
            body,
            Span::new(start_span, end_span),
        ))
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = Vec::new();
        while self.current_token().kind != TokenKind::RBrace {
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }

    fn parse_block_expression(&mut self) -> Result<Expr> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume '{'

        let statements = self.parse_block()?;

        if self.current_token().kind != TokenKind::RBrace {
            return Err(Error::new_parse(
                "Expected '}' after block".to_string(),
                self.current_token().span,
            ));
        }
        let end_span = self.current_token().span.end;
        self.advance(); // Consume '}'

        Ok(Expr::Block(statements, Span::new(start_span, end_span)))
    }

    fn parse_object_literal_or_block(&mut self) -> Result<Expr> {
        let start_span = self.current_token().span.start;
        let start_position = self.position;

        // Look ahead to determine if this is an object literal or a block
        self.advance(); // Consume '{'

        // Empty braces could be either an empty object {} or empty block {}
        // We'll treat it as an empty block for backward compatibility
        if self.current_token().kind == TokenKind::RBrace {
            let end_span = self.current_token().span.end;
            self.advance(); // Consume '}'
            return Ok(Expr::Block(vec![], Span::new(start_span, end_span)));
        }

        // Check if this looks like an object literal
        // Object literal starts with: identifier/string/type-keyword followed by colon
        let is_object_literal = match &self.current_token().kind {
            TokenKind::Identifier(_) | TokenKind::String(_) | TokenKind::Type(_) => {
                // Look ahead for colon
                let next_position = self.position + 1;
                if next_position < self.tokens.len() {
                    self.tokens[next_position].kind == TokenKind::Colon
                } else {
                    false
                }
            }
            _ => false,
        };

        if is_object_literal {
            // Parse as object literal
            let mut fields = Vec::new();

            loop {
                // Parse key
                let key = match &self.current_token().kind {
                    TokenKind::Identifier(name) => {
                        let key = name.clone();
                        self.advance();
                        key
                    }
                    TokenKind::String(value) => {
                        let key = value.clone();
                        self.advance();
                        key
                    }
                    TokenKind::Type(type_name) => {
                        // Allow type keywords as object keys (e.g., { string: "value" })
                        let key = type_name.clone();
                        self.advance();
                        key
                    }
                    _ => {
                        return Err(Error::new_parse(
                            "Expected identifier or string as object key".to_string(),
                            self.current_token().span,
                        ));
                    }
                };

                // Expect colon
                if self.current_token().kind != TokenKind::Colon {
                    return Err(Error::new_parse(
                        "Expected ':' after object key".to_string(),
                        self.current_token().span,
                    ));
                }
                self.advance(); // Consume ':'

                // Parse value
                let value = self.parse_expression()?;
                fields.push((key, value));

                // Check for comma or end
                match self.current_token().kind {
                    TokenKind::Comma => {
                        self.advance(); // Consume ','
                                        // Allow trailing comma
                        if self.current_token().kind == TokenKind::RBrace {
                            break;
                        }
                    }
                    TokenKind::RBrace => break,
                    _ => {
                        return Err(Error::new_parse(
                            "Expected ',' or '}' in object literal".to_string(),
                            self.current_token().span,
                        ));
                    }
                }
            }

            if self.current_token().kind != TokenKind::RBrace {
                return Err(Error::new_parse(
                    "Expected '}' after object literal".to_string(),
                    self.current_token().span,
                ));
            }
            let end_span = self.current_token().span.end;
            self.advance(); // Consume '}'

            Ok(Expr::ObjectLiteral(fields, Span::new(start_span, end_span)))
        } else {
            // Reset position and parse as block
            self.position = start_position;
            self.parse_block_expression()
        }
    }

    fn expect_token(&mut self, expected: TokenKind) -> Result<()> {
        if self.current_token().kind == expected {
            self.advance();
            Ok(())
        } else {
            Err(Error::new_parse(
                format!(
                    "Expected {:?}, found {:?}",
                    expected,
                    self.current_token().kind
                ),
                self.current_token().span,
            ))
        }
    }

    fn parse_function_params(&mut self) -> Result<Vec<(String, String)>> {
        let mut params = Vec::new();

        while self.current_token().kind != TokenKind::RParen {
            let param_name = self.consume_identifier("parameter")?.ok_or_else(|| {
                Error::new_parse(
                    "Expected parameter name".to_string(),
                    self.current_token().span,
                )
            })?;

            self.expect_token(TokenKind::Colon)?;
            let param_type = self.consume_type().unwrap_or_else(|| "Unknown".to_string());

            params.push((param_name, param_type));

            if self.current_token().kind != TokenKind::RParen {
                self.expect_token(TokenKind::Comma)?;
            }
        }

        Ok(params)
    }

    fn parse_module(&mut self, attributes: Vec<Attribute>) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'mod'

        let name = self.consume_identifier("module")?.ok_or_else(|| {
            Error::new_parse(
                "Expected module name".to_string(),
                self.current_token().span,
            )
        })?;

        if self.current_token().kind != TokenKind::LBrace {
            return Err(Error::new_parse(
                "Expected '{' after module name".to_string(),
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

        Ok(Stmt::Module(
            attributes,
            name,
            body,
            Span::new(start_span, end_span),
        ))
    }

    fn parse_async_function(&mut self, attributes: Vec<Attribute>, is_pub: bool) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'async'

        if self.current_token().kind != TokenKind::Function {
            return Err(Error::new_parse(
                "Expected 'fn' after 'async'".to_string(),
                self.current_token().span,
            ));
        }

        // Parse the function normally
        let func_stmt = self.parse_function(attributes.clone(), is_pub)?;

        // Convert Function to AsyncFunction
        if let Stmt::Function(
            attributes,
            is_pub,
            name,
            generic_params,
            params,
            return_type,
            body,
            func_span,
        ) = func_stmt
        {
            Ok(Stmt::AsyncFunction(
                attributes,
                is_pub,
                name,
                generic_params,
                params,
                return_type,
                body,
                Span::new(start_span, func_span.end),
            ))
        } else {
            unreachable!("parse_function should return a Function statement")
        }
    }

    fn parse_extern(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span;
        self.advance(); // Consume 'extern'

        match &self.current_token().kind {
            TokenKind::Function => {
                // extern fn name(params) -> return_type;
                self.advance(); // Consume 'fn'
                let name = self.consume_identifier("extern function")?.ok_or_else(|| {
                    Error::new_parse(
                        "Expected function name after 'fn'".to_string(),
                        self.current_token().span,
                    )
                })?;

                // Parse generic type parameters <T, U>
                let generic_params = self.parse_generic_parameters()?;

                self.expect_token(TokenKind::LParen)?;
                let params = self.parse_function_params()?;
                self.expect_token(TokenKind::RParen)?;

                // Parse return type
                let return_type = if self.current_token().kind == TokenKind::Arrow {
                    self.advance(); // Consume '->'
                    self.consume_type().ok_or_else(|| {
                        Error::new_parse(
                            "Expected return type after '->'".to_string(),
                            self.current_token().span,
                        )
                    })?
                } else {
                    "unit".to_string()
                };

                self.expect_token(TokenKind::Semicolon)?;

                Ok(Stmt::ExternFunction(
                    name,
                    generic_params,
                    params,
                    return_type,
                    start_span,
                ))
            }
            TokenKind::Mod => {
                // extern mod name { ... }
                self.advance(); // Consume 'mod'
                let name = self.consume_identifier("extern module")?.ok_or_else(|| {
                    Error::new_parse(
                        "Expected module name after 'mod'".to_string(),
                        self.current_token().span,
                    )
                })?;
                self.expect_token(TokenKind::LBrace)?;

                let mut items = Vec::new();
                while self.current_token().kind != TokenKind::RBrace {
                    items.push(self.parse_extern_item()?);
                }

                self.expect_token(TokenKind::RBrace)?;

                Ok(Stmt::ExternMod(name, items, start_span))
            }
            TokenKind::Identifier(s) if s == "type" => {
                // extern type Name<T, U>;
                self.advance(); // Consume 'type'
                let name = self.consume_identifier("extern type")?.ok_or_else(|| {
                    Error::new_parse(
                        "Expected type name after 'type'".to_string(),
                        self.current_token().span,
                    )
                })?;

                // Parse generic type parameters <T, U>
                let generic_params = self.parse_generic_parameters()?;

                self.expect_token(TokenKind::Semicolon)?;
                Ok(Stmt::ExternType(name, generic_params, start_span))
            }
            _ => Err(Error::new_parse(
                "Expected 'fn', 'mod', or 'type' after 'extern'".to_string(),
                self.current_token().span,
            )),
        }
    }

    fn parse_extern_item(&mut self) -> Result<ExternItem> {
        match &self.current_token().kind {
            TokenKind::Function => {
                // fn name(params) -> return_type;
                self.advance(); // Consume 'fn'
                let name = self.consume_identifier("extern function")?.ok_or_else(|| {
                    Error::new_parse(
                        "Expected function name after 'fn'".to_string(),
                        self.current_token().span,
                    )
                })?;

                // Parse generic type parameters <T, U>
                let generic_params = self.parse_generic_parameters()?;

                self.expect_token(TokenKind::LParen)?;
                let params = self.parse_function_params()?;
                self.expect_token(TokenKind::RParen)?;

                let return_type = if self.current_token().kind == TokenKind::Arrow {
                    self.advance(); // Consume '->'
                    self.consume_type().ok_or_else(|| {
                        Error::new_parse(
                            "Expected return type after '->'".to_string(),
                            self.current_token().span,
                        )
                    })?
                } else {
                    "unit".to_string()
                };

                self.expect_token(TokenKind::Semicolon)?;

                Ok(ExternItem::Function(
                    name,
                    generic_params,
                    params,
                    return_type,
                ))
            }
            TokenKind::Identifier(s) if s == "type" => {
                // type Name<T, U>;
                self.advance(); // Consume 'type'
                let name = self.consume_identifier("extern type")?.ok_or_else(|| {
                    Error::new_parse(
                        "Expected identifier after 'type'".to_string(),
                        self.current_token().span,
                    )
                })?;

                // Parse generic type parameters <T, U>
                let generic_params = self.parse_generic_parameters()?;

                self.expect_token(TokenKind::Semicolon)?;
                Ok(ExternItem::Type(name, generic_params))
            }
            TokenKind::Mod => {
                // mod name { ... }
                self.advance(); // Consume 'mod'
                let name = self.consume_identifier("extern module")?.ok_or_else(|| {
                    Error::new_parse(
                        "Expected module name after 'mod'".to_string(),
                        self.current_token().span,
                    )
                })?;
                self.expect_token(TokenKind::LBrace)?;

                let mut items = Vec::new();
                while self.current_token().kind != TokenKind::RBrace {
                    items.push(self.parse_extern_item()?);
                }

                self.expect_token(TokenKind::RBrace)?;

                Ok(ExternItem::Mod(name, items))
            }
            TokenKind::Impl => {
                // impl Type { ... }
                self.advance(); // Consume 'impl'
                let type_name = self.consume_identifier("extern impl")?.ok_or_else(|| {
                    Error::new_parse(
                        "Expected type name after 'impl'".to_string(),
                        self.current_token().span,
                    )
                })?;
                self.expect_token(TokenKind::LBrace)?;

                let mut items = Vec::new();
                while self.current_token().kind != TokenKind::RBrace {
                    items.push(self.parse_extern_item()?);
                }

                self.expect_token(TokenKind::RBrace)?;

                Ok(ExternItem::Impl(type_name, items))
            }
            _ => Err(Error::new_parse(
                "Expected 'fn', 'type', 'mod', or 'impl' in extern block".to_string(),
                self.current_token().span,
            )),
        }
    }

    fn parse_struct(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'struct'

        let Some(name) = self.consume_identifier("parse_struct")? else {
            return Err(Error::new_parse(
                "Expected struct name".to_string(),
                self.current_token().span,
            ));
        };

        // Parse generic type parameters <T, U>
        let generic_params = self.parse_generic_parameters()?;

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
            let field_name = self
                .consume_identifier("parse_struct_after")?
                .ok_or_else(|| {
                    Error::new_parse("Expected field name".to_string(), self.current_token().span)
                })?;
            if self.current_token().kind != TokenKind::Colon {
                eprintln!("DEBUG parse_struct: Expected ':' after field name '{}', but got {:?} at position {}", 
                    field_name, self.current_token().kind, self.current_token().span.start);
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
        Ok(Stmt::Struct(
            name,
            generic_params,
            fields,
            Span::new(start_span, end_span),
        ))
    }

    fn parse_enum(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'enum'

        let Some(name) = self.consume_identifier("parse_enum")? else {
            return Err(Error::new_parse(
                "Expected enum name".to_string(),
                self.current_token().span,
            ));
        };

        // Parse generic type parameters <T, U>
        let generic_params = self.parse_generic_parameters()?;

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
            let variant_name = self
                .consume_identifier("parse_enum_after")?
                .ok_or_else(|| {
                    Error::new_parse(
                        "Expected variant name".to_string(),
                        self.current_token().span,
                    )
                })?;

            let variant = if self.current_token().kind == TokenKind::LParen {
                // Tuple variant: Color(string)
                self.advance(); // Consume '('
                let variant_type = self.consume_type().unwrap();
                if self.current_token().kind != TokenKind::RParen {
                    return Err(Error::new_parse(
                        "Expected ')' after associated value".to_string(),
                        self.current_token().span,
                    ));
                }
                self.advance(); // Consume ')'
                EnumVariant::Tuple(variant_name, variant_type)
            } else if self.current_token().kind == TokenKind::LBrace {
                // Struct variant: Color { r: int, g: int, b: int }
                self.advance(); // Consume '{'

                let mut fields = Vec::new();
                while self.current_token().kind != TokenKind::RBrace {
                    let field_name =
                        self.consume_identifier("enum struct field")?
                            .ok_or_else(|| {
                                Error::new_parse(
                                    "Expected field name in enum struct variant".to_string(),
                                    self.current_token().span,
                                )
                            })?;

                    if self.current_token().kind != TokenKind::Colon {
                        return Err(Error::new_parse(
                            "Expected ':' after field name in enum struct variant".to_string(),
                            self.current_token().span,
                        ));
                    }
                    self.advance(); // Consume ':'

                    let field_type = self.consume_type().ok_or_else(|| {
                        Error::new_parse(
                            "Expected field type after ':' in enum struct variant".to_string(),
                            self.current_token().span,
                        )
                    })?;

                    fields.push((field_name, field_type));

                    if self.current_token().kind == TokenKind::Comma {
                        self.advance(); // Consume ','
                    } else if self.current_token().kind != TokenKind::RBrace {
                        return Err(Error::new_parse(
                            "Expected ',' or '}' in enum struct variant".to_string(),
                            self.current_token().span,
                        ));
                    }
                }

                self.advance(); // Consume '}'
                EnumVariant::Struct(variant_name, fields)
            } else {
                // Unit variant: Color
                EnumVariant::Unit(variant_name)
            };
            variants.push(variant);

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
        Ok(Stmt::Enum(
            name,
            generic_params,
            variants,
            Span::new(start_span, end_span),
        ))
    }

    fn parse_let_statement(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'let'

        let Some(name) = self.consume_identifier("parse_let_statement")? else {
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

    fn parse_use_statement(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'use'

        // Parse the path with prefix
        let (prefix, segments) = self.parse_use_path()?;
        let use_path = UsePath { prefix, segments };

        // Parse what to import (::item, ::{items}, or nothing for the whole module)
        let use_items = if self.current_token().kind == TokenKind::DblColon {
            self.advance(); // Consume '::'

            if self.current_token().kind == TokenKind::LBrace {
                // use module::{a, b, c}
                self.parse_use_items()?
            } else if self.current_token().kind == TokenKind::Asterisk {
                // use module::*
                self.advance(); // Consume '*'
                UseItems::All
            } else {
                // use module::item
                UseItems::Single
            }
        } else {
            // use module; (import the whole module)
            UseItems::Single
        };

        if self.current_token().kind != TokenKind::Semicolon {
            return Err(Error::new_parse(
                format!(
                    "Expected ';' after use statement, found {:?}",
                    self.current_token()
                ),
                self.current_token().span,
            ));
        }

        let end_span = self.current_token().span.end;
        self.advance(); // Consume ';'

        Ok(Stmt::Use(
            use_path,
            use_items,
            Span::new(start_span, end_span),
        ))
    }

    fn parse_use_path(&mut self) -> Result<(UsePrefix, Vec<String>)> {
        let mut prefix = UsePrefix::None;
        let mut segments = Vec::new();

        // Check for prefix (local::, self::, super::)
        if let TokenKind::Identifier(first) = &self.current_token().kind {
            match first.as_str() {
                "local" if self.peek_token().kind == TokenKind::DblColon => {
                    prefix = UsePrefix::Local;
                    self.advance(); // Consume 'local'
                    self.advance(); // Consume '::'
                }
                "self" if self.peek_token().kind == TokenKind::DblColon => {
                    prefix = UsePrefix::Self_;
                    self.advance(); // Consume 'self'
                    self.advance(); // Consume '::'
                }
                "super" if self.peek_token().kind == TokenKind::DblColon => {
                    // Count consecutive super::
                    let mut super_count = 1;
                    self.advance(); // Consume 'super'
                    self.advance(); // Consume '::'

                    while let TokenKind::Identifier(next) = &self.current_token().kind {
                        if next == "super" && self.peek_token().kind == TokenKind::DblColon {
                            super_count += 1;
                            self.advance(); // Consume 'super'
                            self.advance(); // Consume '::'
                        } else {
                            break;
                        }
                    }
                    prefix = UsePrefix::Super(super_count);
                }
                _ => {} // External package, no prefix
            }
        }

        // Parse the rest of the path segments
        loop {
            if let TokenKind::Identifier(segment) = &self.current_token().kind {
                segments.push(segment.clone());
                self.advance();

                if self.current_token().kind == TokenKind::DblColon {
                    // Check what comes after ::
                    if matches!(
                        self.peek_token().kind,
                        TokenKind::LBrace | TokenKind::Asterisk
                    ) {
                        // This is the path before items like use react::{...}
                        break;
                    } else if let TokenKind::Identifier(_) = self.peek_token().kind {
                        // Continue parsing path segments
                        self.advance(); // Consume '::'
                    } else {
                        return Err(Error::new_parse(
                            "Expected identifier, '{', or '*' after '::'".to_string(),
                            self.peek_token().span,
                        ));
                    }
                } else {
                    // No more :: means we're done with the path
                    break;
                }
            } else {
                return Err(Error::new_parse(
                    "Expected identifier in use path".to_string(),
                    self.current_token().span,
                ));
            }
        }

        if segments.is_empty() {
            return Err(Error::new_parse(
                "Expected at least one path segment in use statement".to_string(),
                self.current_token().span,
            ));
        }

        Ok((prefix, segments))
    }

    fn parse_use_items(&mut self) -> Result<UseItems> {
        self.advance(); // Consume '{'

        let mut items = Vec::new();

        loop {
            if let TokenKind::Identifier(name) = &self.current_token().kind {
                let item_name = name.clone();
                self.advance();

                // Check for 'as' alias
                let alias = if self.current_token().kind == TokenKind::As {
                    self.advance(); // Consume 'as'

                    if let TokenKind::Identifier(alias_name) = &self.current_token().kind {
                        let alias = alias_name.clone();
                        self.advance();
                        Some(alias)
                    } else {
                        return Err(Error::new_parse(
                            "Expected identifier after 'as'".to_string(),
                            self.current_token().span,
                        ));
                    }
                } else {
                    None
                };

                items.push((item_name, alias));

                if self.current_token().kind == TokenKind::Comma {
                    self.advance(); // Consume ','
                } else {
                    break;
                }
            } else {
                return Err(Error::new_parse(
                    "Expected identifier in use items".to_string(),
                    self.current_token().span,
                ));
            }
        }

        if self.current_token().kind != TokenKind::RBrace {
            return Err(Error::new_parse(
                "Expected '}' after use items".to_string(),
                self.current_token().span,
            ));
        }
        self.advance(); // Consume '}'

        Ok(UseItems::Named(items))
    }

    fn parse_impl(&mut self) -> Result<Stmt> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'impl'

        let struct_name = self.consume_identifier("parse_impl")?.ok_or_else(|| {
            Error::new_parse(
                "Expected struct name after 'impl'".to_string(),
                self.current_token().span,
            )
        })?;

        if self.current_token().kind != TokenKind::LBrace {
            return Err(Error::new_parse(
                "Expected '{' after struct name in impl block".to_string(),
                self.current_token().span,
            ));
        }
        self.advance(); // Consume '{'

        let mut methods = Vec::new();
        while self.current_token().kind != TokenKind::RBrace {
            methods.push(self.parse_statement()?);
        }

        let end_span = self.current_token().span.end;
        self.advance(); // Consume '}'

        Ok(Stmt::Impl(
            struct_name,
            methods,
            Span::new(start_span, end_span),
        ))
    }

    fn parse_if_expression(&mut self) -> Result<Expr> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'if'

        let condition = Box::new(self.parse_expression()?);

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

            // Check if this is an 'else if'
            if self.current_token().kind == TokenKind::If {
                // Parse the if expression as a single statement in the else block
                let else_if_expr = self.parse_if_expression()?;
                end_span = else_if_expr.span().end;
                else_block = vec![Stmt::Expression(else_if_expr, false)];
            } else if self.current_token().kind == TokenKind::LBrace {
                // Regular else block
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
            } else {
                return Err(Error::new_parse(
                    "Expected '{' or 'if' after else".to_string(),
                    self.current_token().span,
                ));
            }
        }

        Ok(Expr::If(
            condition,
            then_block,
            else_block,
            Span::new(start_span, end_span),
        ))
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
            let pattern = self.parse_match_pattern()?;

            if self.current_token().kind != TokenKind::FatArrow {
                return Err(Error::new_parse(
                    format!(
                        "expected `=>` after pattern, found `{}`",
                        self.current_token().kind
                    ),
                    self.current_token().span,
                ));
            }
            self.advance(); // Consume '=>'

            let is_block_arm = self.current_token().kind == TokenKind::LBrace;

            let body = if is_block_arm {
                self.advance(); // Consume '{'
                let stmts = self.parse_block()?;
                self.advance(); // Consume '}'
                stmts
            } else {
                // Check if this is a statement keyword (return, break, continue)
                match self.current_token().kind {
                    TokenKind::Return | TokenKind::Break | TokenKind::Continue => {
                        vec![self.parse_statement()?]
                    }
                    _ => vec![Stmt::Expression(self.parse_expression()?, false)],
                }
            };

            arms.push((pattern, body));

            // Check comma rules
            if self.current_token().kind != TokenKind::RBrace {
                // Not the last arm - check comma rules
                if is_block_arm {
                    // Block expression - comma should NOT be present
                    if self.current_token().kind == TokenKind::Comma {
                        return Err(Error::new_parse(
                            "Unexpected comma after block expression in match arm".to_string(),
                            self.current_token().span,
                        ));
                    }
                } else {
                    // Non-block expression - comma is required
                    if self.current_token().kind != TokenKind::Comma {
                        return Err(Error::new_parse(
                            "Expected comma after match arm expression".to_string(),
                            self.current_token().span,
                        ));
                    }
                    self.advance(); // Consume ','
                }
            } else {
                // Last arm - consume optional comma
                if self.current_token().kind == TokenKind::Comma {
                    self.advance();
                }
            }
        }

        let end_span = self.current_token().span.end;
        self.advance(); // Consume '}'

        Ok(Stmt::Match(expr, arms, Span::new(start_span, end_span)))
    }

    fn parse_closure_expression(&mut self) -> Result<Expr> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume '|'

        // Parse parameters
        let mut params = Vec::new();

        while self.current_token().kind != TokenKind::Pipe {
            // Handle empty parameter list
            if params.is_empty() && self.current_token().kind == TokenKind::Pipe {
                break;
            }

            let param_name = self
                .consume_identifier("closure parameter")?
                .ok_or_else(|| {
                    Error::new_parse(
                        "Expected parameter name".to_string(),
                        self.current_token().span,
                    )
                })?;

            // Optional type annotation
            let param_type = if self.current_token().kind == TokenKind::Colon {
                self.advance(); // Consume ':'
                Some(self.consume_type().ok_or_else(|| {
                    Error::new_parse(
                        "Expected type after ':'".to_string(),
                        self.current_token().span,
                    )
                })?)
            } else {
                None
            };

            params.push((param_name, param_type));

            if self.current_token().kind == TokenKind::Comma {
                self.advance(); // Consume ','
            } else if self.current_token().kind != TokenKind::Pipe {
                return Err(Error::new_parse(
                    "Expected ',' or '|' in closure parameters".to_string(),
                    self.current_token().span,
                ));
            }
        }

        self.advance(); // Consume closing '|'

        // Optional return type
        let return_type = if self.current_token().kind == TokenKind::Arrow {
            self.advance(); // Consume '->'
            Some(self.consume_type().ok_or_else(|| {
                Error::new_parse(
                    "Expected return type after '->'".to_string(),
                    self.current_token().span,
                )
            })?)
        } else {
            None
        };

        // Parse body - can be a single expression or a block
        let body = if self.current_token().kind == TokenKind::LBrace {
            self.parse_block_expression()?
        } else {
            self.parse_expression()?
        };

        let end_span = body.span().end;

        Ok(Expr::Closure(
            params,
            return_type,
            Box::new(body),
            Span::new(start_span, end_span),
        ))
    }

    fn parse_match_expression(&mut self) -> Result<Expr> {
        let start_span = self.current_token().span.start;
        self.advance(); // Consume 'match'

        let expr = Box::new(self.parse_expression()?);

        if self.current_token().kind != TokenKind::LBrace {
            return Err(Error::new_parse(
                "Expected '{' after match expression".to_string(),
                self.current_token().span,
            ));
        }

        self.advance(); // Consume '{'

        let mut arms = Vec::new();
        while self.current_token().kind != TokenKind::RBrace {
            let pattern = self.parse_match_pattern()?;

            if self.current_token().kind != TokenKind::FatArrow {
                return Err(Error::new_parse(
                    format!(
                        "expected `=>` after pattern, found `{}`",
                        self.current_token().kind
                    ),
                    self.current_token().span,
                ));
            }
            self.advance(); // Consume '=>'

            let is_block_arm = self.current_token().kind == TokenKind::LBrace;

            let body = if is_block_arm {
                self.advance(); // Consume '{'
                let stmts = self.parse_block()?;
                self.advance(); // Consume '}'
                stmts
            } else {
                // Check if this is a statement keyword (return, break, continue)
                match self.current_token().kind {
                    TokenKind::Return | TokenKind::Break | TokenKind::Continue => {
                        vec![self.parse_statement()?]
                    }
                    _ => vec![Stmt::Expression(self.parse_expression()?, false)],
                }
            };

            arms.push((pattern, body));

            // Check comma rules
            if self.current_token().kind != TokenKind::RBrace {
                // Not the last arm - check comma rules
                if is_block_arm {
                    // Block expression - comma should NOT be present
                    if self.current_token().kind == TokenKind::Comma {
                        return Err(Error::new_parse(
                            "Unexpected comma after block expression in match arm".to_string(),
                            self.current_token().span,
                        ));
                    }
                } else {
                    // Non-block expression - comma is required
                    if self.current_token().kind != TokenKind::Comma {
                        return Err(Error::new_parse(
                            "Expected comma after match arm expression".to_string(),
                            self.current_token().span,
                        ));
                    }
                    self.advance(); // Consume ','
                }
            } else {
                // Last arm - consume optional comma
                if self.current_token().kind == TokenKind::Comma {
                    self.advance();
                }
            }
        }

        let end_span = self.current_token().span.end;
        self.advance(); // Consume '}'

        Ok(Expr::Match(expr, arms, Span::new(start_span, end_span)))
    }

    fn parse_match_pattern(&mut self) -> Result<Expr> {
        match &self.current_token().kind {
            TokenKind::Underscore => {
                let span = self.current_token().span;
                self.advance(); // Consume '_'
                Ok(Expr::Identifier("_".to_string(), span))
            }
            TokenKind::LParen => {
                // Parse tuple pattern: (pattern1, pattern2, ...)
                let start_span = self.current_token().span;
                self.advance(); // Consume '('

                let mut patterns = Vec::new();

                // Check for empty tuple
                if self.current_token().kind == TokenKind::RParen {
                    self.advance(); // Consume ')'
                    return Ok(Expr::Tuple(
                        vec![],
                        Span::new(start_span.start, self.current_token().span.end),
                    ));
                }

                // Parse patterns
                loop {
                    patterns.push(self.parse_match_pattern()?);

                    if self.current_token().kind == TokenKind::Comma {
                        self.advance(); // Consume ','

                        // Allow trailing comma
                        if self.current_token().kind == TokenKind::RParen {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                if self.current_token().kind != TokenKind::RParen {
                    return Err(Error::new_parse(
                        "Expected ')' after tuple pattern".to_string(),
                        self.current_token().span,
                    ));
                }
                let end_span = self.current_token().span.end;
                self.advance(); // Consume ')'

                Ok(Expr::Tuple(patterns, Span::new(start_span.start, end_span)))
            }
            TokenKind::Identifier(_) => {
                let start_span = self.current_token().span;
                let name = self.consume_identifier("parse_match_pattern")?.unwrap();

                // Check for implicit Result/Option variants in patterns
                match name.as_str() {
                    "Ok" | "Err" if self.current_token().kind == TokenKind::LParen => {
                        self.advance(); // Consume '('
                        let value = self.parse_expression()?;
                        self.expect_token(TokenKind::RParen)?;

                        return Ok(Expr::EnumVariantOrMethodCall {
                            target: Box::new(Expr::Identifier("Result".to_string(), start_span)),
                            call: name,
                            args: vec![value],
                            span: Span::new(start_span.start, self.current_token().span.end),
                            type_annotation: TypeAnnotation::new(),
                        });
                    }
                    "Some" if self.current_token().kind == TokenKind::LParen => {
                        self.advance(); // Consume '('
                        let value = self.parse_expression()?;
                        self.expect_token(TokenKind::RParen)?;

                        return Ok(Expr::EnumVariantOrMethodCall {
                            target: Box::new(Expr::Identifier("Option".to_string(), start_span)),
                            call: name,
                            args: vec![value],
                            span: Span::new(start_span.start, self.current_token().span.end),
                            type_annotation: TypeAnnotation::new(),
                        });
                    }
                    "None" => {
                        return Ok(Expr::EnumVariantOrMethodCall {
                            target: Box::new(Expr::Identifier("Option".to_string(), start_span)),
                            call: name,
                            args: vec![],
                            span: start_span,
                            type_annotation: TypeAnnotation::new(),
                        });
                    }
                    _ => {}
                }

                let target = Expr::Identifier(name.clone(), start_span);

                if self.current_token().kind == TokenKind::DblColon {
                    self.advance(); // Consume '::'
                    let call = self
                        .consume_identifier("parse_match_pattern DblColon")?
                        .unwrap();
                    let target = Box::new(target);

                    if self.current_token().kind == TokenKind::LParen {
                        self.advance(); // Consume '('

                        let value = self.parse_expression()?;
                        if self.current_token().kind != TokenKind::RParen {
                            return Err(Error::new_parse(
                                "Expected ')' after enum variant value".to_string(),
                                self.current_token().span,
                            ));
                        }
                        self.advance(); // Consume ')'

                        Ok(Expr::EnumVariantOrMethodCall {
                            target,
                            call,
                            args: vec![value],
                            span: Span::new(start_span.start, self.current_token().span.end),
                            type_annotation: TypeAnnotation::new(),
                        })
                    } else if self.current_token().kind == TokenKind::LBrace {
                        // Parse struct destructuring pattern: EnumName::Variant { field1, field2: renamed, ... }
                        self.advance(); // Consume '{'

                        let mut fields = Vec::new();

                        while self.current_token().kind != TokenKind::RBrace {
                            // Check for rest pattern ..
                            if self.current_token().kind == TokenKind::DblDot {
                                self.advance(); // Consume '..'
                                                // .. must be the last element
                                if self.current_token().kind == TokenKind::Comma {
                                    return Err(Error::new_parse(
                                        "Rest pattern '..' must be the last element in struct pattern".to_string(),
                                        self.current_token().span,
                                    ));
                                }
                                // Mark that we have a rest pattern by adding a special field
                                fields.push(("..".to_string(), None));
                                break;
                            }

                            let field_name = self
                                .consume_identifier("struct pattern field")?
                                .ok_or_else(|| {
                                    Error::new_parse(
                                        "Expected field name in struct pattern".to_string(),
                                        self.current_token().span,
                                    )
                                })?;

                            let var_name = if self.current_token().kind == TokenKind::Colon {
                                self.advance(); // Consume ':'
                                Some(self.consume_identifier("struct pattern variable")?.ok_or_else(|| {
                                    Error::new_parse(
                                        "Expected variable name after ':' in struct pattern".to_string(),
                                        self.current_token().span,
                                    )
                                })?)
                            } else {
                                None
                            };

                            fields.push((field_name, var_name));

                            if self.current_token().kind == TokenKind::Comma {
                                self.advance(); // Consume ','
                            } else if self.current_token().kind != TokenKind::RBrace {
                                return Err(Error::new_parse(
                                    "Expected ',' or '}' in struct pattern".to_string(),
                                    self.current_token().span,
                                ));
                            }
                        }

                        self.advance(); // Consume '}'

                        // Create the full variant name including the enum prefix
                        let full_variant = format!("{}::{}", target, call);

                        Ok(Expr::StructPattern(
                            full_variant,
                            fields,
                            Span::new(start_span.start, self.current_token().span.end),
                        ))
                    } else {
                        Ok(Expr::EnumVariantOrMethodCall {
                            target,
                            call,
                            args: vec![],
                            span: Span::new(start_span.start, self.current_token().span.end),
                            type_annotation: TypeAnnotation::new(),
                        })
                    }
                } else if self.current_token().kind == TokenKind::LBrace {
                    // Plain struct pattern: Config { field1, field2, .. }
                    self.advance(); // Consume '{'

                    let mut fields = Vec::new();

                    while self.current_token().kind != TokenKind::RBrace {
                        // Check for rest pattern ..
                        if self.current_token().kind == TokenKind::DblDot {
                            self.advance(); // Consume '..'
                                            // .. must be the last element
                            if self.current_token().kind == TokenKind::Comma {
                                return Err(Error::new_parse(
                                    "Rest pattern '..' must be the last element in struct pattern"
                                        .to_string(),
                                    self.current_token().span,
                                ));
                            }
                            // Mark that we have a rest pattern by adding a special field
                            fields.push(("..".to_string(), None));
                            break;
                        }

                        let field_name = self
                            .consume_identifier("struct pattern field")?
                            .ok_or_else(|| {
                                Error::new_parse(
                                    "Expected field name in struct pattern".to_string(),
                                    self.current_token().span,
                                )
                            })?;

                        let var_name = if self.current_token().kind == TokenKind::Colon {
                            self.advance(); // Consume ':'
                            Some(
                                self.consume_identifier("struct pattern variable")?
                                    .ok_or_else(|| {
                                        Error::new_parse(
                                            "Expected variable name after ':' in struct pattern"
                                                .to_string(),
                                            self.current_token().span,
                                        )
                                    })?,
                            )
                        } else {
                            None
                        };

                        fields.push((field_name, var_name));

                        if self.current_token().kind == TokenKind::Comma {
                            self.advance(); // Consume ','
                        } else if self.current_token().kind != TokenKind::RBrace {
                            return Err(Error::new_parse(
                                "Expected ',' or '}' in struct pattern".to_string(),
                                self.current_token().span,
                            ));
                        }
                    }

                    self.advance(); // Consume '}'

                    Ok(Expr::StructPattern(
                        name,
                        fields,
                        Span::new(start_span.start, self.current_token().span.end),
                    ))
                } else {
                    Ok(target)
                }
            }
            _ => self.parse_expression(),
        }
    }

    fn consume_identifier(&mut self, from: &str) -> Result<Option<String>> {
        if let TokenKind::Identifier(name) = &self.current_token().kind {
            let name = name.clone();
            self.advance(); // Consume identifier
            Ok(Some(name))
        } else if let TokenKind::Error(err) = &self.current_token().kind {
            println!("{}", err);
            Err(Error::new_parse(err, self.current_token().span))
        } else {
            Err(Error::new_parse(
                format!(
                    "Expected identifier, got {:?} from {}",
                    self.current_token().kind,
                    from,
                ),
                self.current_token().span,
            ))
        }
    }

    fn parse_function(&mut self, attributes: Vec<Attribute>, is_pub: bool) -> Result<Stmt> {
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

        // --- Parse generic parameters ---
        let generic_params = self.parse_generic_parameters()?;

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
            let param_name = self.consume_identifier("parse_function")?.ok_or_else(|| {
                Error::new_parse(
                    "Expected parameter name".to_string(),
                    self.current_token().span,
                )
            })?;
            if param_name == "self" {
                params.push(("self".to_string(), "self".to_string()));
                // Check for comma after self
                if self.current_token().kind == TokenKind::Comma {
                    self.advance(); // Consume ','
                } else if self.current_token().kind != TokenKind::RParen {
                    return Err(Error::new_parse(
                        "Expected ',' or ')' after 'self' parameter".to_string(),
                        self.current_token().span,
                    ));
                }
                continue;
            }
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
            attributes,
            is_pub,
            name,
            generic_params,
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
        match &self.current_token().kind {
            TokenKind::Type(type_name) => {
                let result = type_name.clone();
                self.advance(); // Consume type
                Some(result)
            }
            TokenKind::Identifier(name) => {
                let mut result = name.clone();
                self.advance(); // Consume identifier

                // Check for qualified types (e.g., express::Request)
                while self.current_token().kind == TokenKind::DblColon {
                    self.advance(); // Consume '::'
                    result.push_str("::");

                    if let TokenKind::Identifier(segment) = &self.current_token().kind {
                        result.push_str(segment);
                        self.advance(); // Consume segment
                    } else {
                        return None; // Expected identifier after '::'
                    }
                }

                // Check for generic type parameters (e.g., Promise<T>)
                if self.current_token().kind == TokenKind::LessThan {
                    self.advance(); // Consume '<'
                    result.push('<');

                    // Allow multiple generic parameters separated by commas
                    let mut first = true;
                    loop {
                        if !first {
                            if self.current_token().kind == TokenKind::Comma {
                                self.advance(); // Consume ','
                                result.push_str(", ");
                            } else {
                                break;
                            }
                        }
                        first = false;

                        if let Some(inner_type) = self.consume_type() {
                            result.push_str(&inner_type);
                        } else {
                            return None;
                        }

                        // Check if we have more parameters
                        if self.current_token().kind != TokenKind::Comma {
                            break;
                        }
                    }

                    if self.current_token().kind == TokenKind::GreaterThan {
                        self.advance(); // Consume '>'
                        result.push('>');
                    } else {
                        return None; // Expected '>'
                    }
                }

                Some(result)
            }
            TokenKind::LParen => {
                // Handle unit type ()
                self.advance(); // Consume '('
                if self.current_token().kind == TokenKind::RParen {
                    self.advance(); // Consume ')'
                    Some("()".to_string())
                } else {
                    // This might be a parenthesized type expression or tuple type
                    // For now, we don't support these, so return None
                    None
                }
            }
            TokenKind::Function => {
                // Parse function type: fn() or fn(params) -> return_type
                self.advance(); // Consume 'fn'

                if self.current_token().kind != TokenKind::LParen {
                    return None;
                }

                let mut fn_type = "fn(".to_string();
                self.advance(); // Consume '('

                // Parse parameter types
                let mut first = true;
                while self.current_token().kind != TokenKind::RParen {
                    if !first {
                        if self.current_token().kind != TokenKind::Comma {
                            return None;
                        }
                        self.advance(); // Consume ','
                        fn_type.push_str(", ");
                    }
                    first = false;

                    if let Some(param_type) = self.consume_type() {
                        fn_type.push_str(&param_type);
                    } else {
                        return None;
                    }
                }

                self.advance(); // Consume ')'
                fn_type.push(')');

                // Check for return type
                if self.current_token().kind == TokenKind::Arrow {
                    self.advance(); // Consume '->'
                    fn_type.push_str(" -> ");
                    if let Some(return_type) = self.consume_type() {
                        fn_type.push_str(&return_type);
                    } else {
                        return None;
                    }
                }

                Some(fn_type)
            }
            _ => None,
        }
    }

    fn parse_generic_parameters(&mut self) -> Result<Vec<String>> {
        let mut generic_params = Vec::new();

        if self.current_token().kind != TokenKind::LessThan {
            return Ok(generic_params); // No generic parameters
        }

        self.advance(); // Consume '<'

        while self.current_token().kind != TokenKind::GreaterThan {
            let type_param = self.consume_identifier("type parameter")?.ok_or_else(|| {
                Error::new_parse(
                    "Expected type parameter name".to_string(),
                    self.current_token().span,
                )
            })?;
            generic_params.push(type_param);

            if self.current_token().kind == TokenKind::Comma {
                self.advance(); // Consume ','
            } else if self.current_token().kind != TokenKind::GreaterThan {
                return Err(Error::new_parse(
                    "Expected ',' or '>' in generic type parameters".to_string(),
                    self.current_token().span,
                ));
            }
        }

        if self.current_token().kind != TokenKind::GreaterThan {
            return Err(Error::new_parse(
                "Expected '>' to close generic type parameters".to_string(),
                self.current_token().span,
            ));
        }

        self.advance(); // Consume '>'
        Ok(generic_params)
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt> {
        if let Ok(expr) = self.parse_expression() {
            if self.current_token().kind == TokenKind::Semicolon {
                self.advance(); // Consume ';'
                return Ok(Stmt::Expression(expr, true)); // true = has semicolon
            } else if self.current_token().kind == TokenKind::Equals {
                self.advance(); // Consume '='
                let value = self.parse_expression()?;

                if self.current_token().kind == TokenKind::Semicolon {
                    self.advance(); // Consume ';'
                    return Ok(Stmt::Expression(
                        Expr::Assign(
                            Box::new(expr.clone()),
                            Box::new(value.clone()),
                            Span::new(expr.span().start, value.span().end),
                        ),
                        true, // true = has semicolon
                    ));
                }
                return Err(Error::new_parse(
                    "Expected ';' after assignment".to_string(),
                    self.current_token().span,
                ));
            }
            return Ok(Stmt::Expression(expr, false)); // false = no semicolon
        }

        Err(Error::new_parse(
            format!("Invalid expression statement: {:?}", self.current_token()),
            self.current_token().span,
        ))
    }

    fn parse_expression(&mut self) -> Result<Expr> {
        self.parse_or_expression()
    }

    fn parse_or_expression(&mut self) -> Result<Expr> {
        let mut left = self.parse_and_expression()?;

        while self.current_token().kind == TokenKind::DblPipe {
            let start_span = left.span().start;
            self.advance(); // Consume '||'
            let right = self.parse_and_expression()?;
            let end_span = right.span().end;
            left = Expr::BinaryOp(
                Box::new(left),
                Operator::Or,
                Box::new(right),
                Span::new(start_span, end_span),
            );
        }

        Ok(left)
    }

    fn parse_and_expression(&mut self) -> Result<Expr> {
        let mut left = self.parse_comparison_expression()?;

        while self.current_token().kind == TokenKind::DblAmpersand {
            let start_span = left.span().start;
            self.advance(); // Consume '&&'
            let right = self.parse_comparison_expression()?;
            let end_span = right.span().end;
            left = Expr::BinaryOp(
                Box::new(left),
                Operator::And,
                Box::new(right),
                Span::new(start_span, end_span),
            );
        }

        Ok(left)
    }

    fn parse_comparison_expression(&mut self) -> Result<Expr> {
        let mut left = self.parse_cast_expression()?;

        while let Some(op) = self.parse_comparison_operator() {
            let start_span = left.span().start;
            self.advance(); // Consume operator
            let right = self.parse_cast_expression()?;
            let end_span = right.span().end;
            left = Expr::BinaryOp(
                Box::new(left),
                op,
                Box::new(right),
                Span::new(start_span, end_span),
            );
        }

        Ok(left)
    }

    fn parse_cast_expression(&mut self) -> Result<Expr> {
        let mut left = self.parse_additive_expression()?;

        while self.current_token().kind == TokenKind::As {
            let start_span = left.span().start;
            self.advance(); // Consume 'as'

            // Save the position before parsing type
            let type_start_pos = self.position;

            // Parse the target type
            let target_type = self.consume_type().ok_or_else(|| {
                Error::new_parse(
                    "Expected type after 'as'".to_string(),
                    self.current_token().span,
                )
            })?;

            // Use the previous token's end position
            let end_span = if self.position > 0 && self.position <= self.tokens.len() {
                self.tokens[self.position - 1].span.end
            } else {
                // Fallback to the type start position's span
                self.tokens[type_start_pos].span.end
            };

            left = Expr::Cast(Box::new(left), target_type, Span::new(start_span, end_span));
        }

        Ok(left)
    }

    fn parse_comparison_operator(&mut self) -> Option<Operator> {
        match self.current_token().kind {
            TokenKind::DblEquals => Some(Operator::Equals),
            TokenKind::BangEquals => Some(Operator::NotEquals),
            TokenKind::LessThan => Some(Operator::LessThan),
            TokenKind::GreaterThan => Some(Operator::GreaterThan),
            TokenKind::LessThanEquals => Some(Operator::LessThanEquals),
            TokenKind::GreaterThanEquals => Some(Operator::GreaterThanEquals),
            _ => None,
        }
    }

    fn parse_additive_expression(&mut self) -> Result<Expr> {
        let mut left = self.parse_multiplicative_expression()?;

        while let Some(op) = self.parse_additive_operator() {
            let start_span = left.span().start;
            self.advance(); // Consume operator
            let right = self.parse_multiplicative_expression()?;
            let end_span = right.span().end;
            left = Expr::BinaryOp(
                Box::new(left),
                op,
                Box::new(right),
                Span::new(start_span, end_span),
            );
        }

        Ok(left)
    }

    fn parse_additive_operator(&mut self) -> Option<Operator> {
        match self.current_token().kind {
            TokenKind::Plus => Some(Operator::Plus),
            TokenKind::Minus => Some(Operator::Minus),
            _ => None,
        }
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Expr> {
        let mut left = self.parse_postfix_expression()?;

        while let Some(op) = self.parse_multiplicative_operator() {
            let start_span = left.span().start;
            self.advance(); // Consume operator
            let right = self.parse_postfix_expression()?;
            let end_span = right.span().end;
            left = Expr::BinaryOp(
                Box::new(left),
                op,
                Box::new(right),
                Span::new(start_span, end_span),
            );
        }

        Ok(left)
    }

    fn parse_multiplicative_operator(&mut self) -> Option<Operator> {
        match self.current_token().kind {
            TokenKind::Asterisk => Some(Operator::Multiply),
            TokenKind::Slash => Some(Operator::Divide),
            TokenKind::Percent => Some(Operator::Modulo),
            _ => None,
        }
    }

    fn parse_postfix_expression(&mut self) -> Result<Expr> {
        let start_span = self.current_token().span.start;
        let mut left = self.parse_unary_expression()?;

        // Handle postfix operations
        loop {
            match self.current_token().kind {
                // Function call on expression
                TokenKind::LParen => {
                    // Check if the expression can be called
                    match &left {
                        Expr::EnumVariantOrMethodCall {
                            target, call, args, ..
                        } if args.is_empty() => {
                            // This is a method reference like Point::origin, convert to a call
                            self.advance(); // Consume '('
                            let mut call_args = Vec::new();

                            // Parse arguments
                            if self.current_token().kind != TokenKind::RParen {
                                loop {
                                    call_args.push(self.parse_expression()?);

                                    match self.current_token().kind {
                                        TokenKind::Comma => self.advance(),
                                        TokenKind::RParen => break,
                                        _ => {
                                            return Err(Error::new_parse(
                                                "Expected ',' or ')' in argument list".to_string(),
                                                self.current_token().span,
                                            ))
                                        }
                                    }
                                }
                            }

                            let end = self.current_token().span.end;
                            self.advance(); // Consume ')'

                            left = Expr::EnumVariantOrMethodCall {
                                target: target.clone(),
                                call: call.clone(),
                                args: call_args,
                                span: Span::new(start_span, end),
                                type_annotation: TypeAnnotation::new(),
                            };
                        }
                        _ => break, // Can't call this expression
                    }
                }
                // Compound assignment
                TokenKind::PlusEquals
                | TokenKind::MinusEquals
                | TokenKind::StarEquals
                | TokenKind::SlashEquals
                | TokenKind::PercentEquals => {
                    let op = match self.current_token().kind {
                        TokenKind::PlusEquals => Operator::Plus,
                        TokenKind::MinusEquals => Operator::Minus,
                        TokenKind::StarEquals => Operator::Multiply,
                        TokenKind::SlashEquals => Operator::Divide,
                        TokenKind::PercentEquals => Operator::Modulo,
                        _ => unreachable!(),
                    };
                    self.advance();
                    let right = self.parse_expression()?;
                    let end_span = right.span().end;
                    left = Expr::CompoundAssign(
                        Box::new(left),
                        op,
                        Box::new(right),
                        Span::new(start_span, end_span),
                    );
                }
                // Member access and .await
                TokenKind::Dot => {
                    self.advance(); // Consume '.'

                    // Check if this is .await
                    if let TokenKind::Identifier(field) = &self.current_token().kind {
                        if field == "await" {
                            self.advance(); // Consume 'await'

                            // Check if this is .await?
                            if self.current_token().kind == TokenKind::Question {
                                let question_span = self.current_token().span;
                                self.advance(); // Consume '?'
                                left = Expr::AwaitTry(
                                    Box::new(left),
                                    Span::new(start_span, question_span.end),
                                );
                            } else {
                                let end_span = self.current_token().span.start;
                                left = Expr::Await(Box::new(left), Span::new(start_span, end_span));
                            }
                            continue;
                        }
                    }

                    // Regular member access or method call
                    let field_name =
                        self.consume_identifier("member access")?.ok_or_else(|| {
                            Error::new_parse(
                                "Expected field name after '.'".to_string(),
                                self.current_token().span,
                            )
                        })?;

                    // Check if this is a method call
                    if self.current_token().kind == TokenKind::LParen {
                        self.advance(); // Consume '('
                        let mut args = Vec::new();

                        // Parse arguments
                        if self.current_token().kind != TokenKind::RParen {
                            loop {
                                args.push(self.parse_expression()?);

                                match self.current_token().kind {
                                    TokenKind::Comma => self.advance(),
                                    TokenKind::RParen => break,
                                    _ => {
                                        return Err(Error::new_parse(
                                            "Expected ',' or ')' in argument list".to_string(),
                                            self.current_token().span,
                                        ))
                                    }
                                }
                            }
                        }

                        let end_span = self.current_token().span.end;
                        self.advance(); // Consume ')'

                        left = Expr::MethodCall(
                            Box::new(left),
                            field_name,
                            args,
                            Span::new(start_span, end_span),
                        );
                    } else {
                        let end_span = self.current_token().span.start;
                        left = Expr::MemberAccess(
                            Box::new(left),
                            field_name,
                            Span::new(start_span, end_span),
                        );
                    }
                }
                // Array indexing
                TokenKind::LSquare => {
                    self.advance(); // Consume '['
                    let index = self.parse_expression()?;

                    if self.current_token().kind != TokenKind::RSquare {
                        return Err(Error::new_parse(
                            "Expected ']' after array index".to_string(),
                            self.current_token().span,
                        ));
                    }

                    let end_span = self.current_token().span.end;
                    self.advance(); // Consume ']'

                    left = Expr::ArrayIndex(
                        Box::new(left),
                        Box::new(index),
                        Span::new(start_span, end_span),
                    );
                }
                // Try operator (?)
                TokenKind::Question => {
                    self.advance(); // Consume '?'
                    let end_span = self.current_token().span.start;
                    left = Expr::Try(Box::new(left), Span::new(start_span, end_span));
                }
                _ => break,
            }
        }

        // Handle range expressions
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

    fn parse_unary_expression(&mut self) -> Result<Expr> {
        let start_span = self.current_token().span.start;

        match self.current_token().kind {
            TokenKind::Minus => {
                self.advance(); // Consume '-'
                let expr = self.parse_unary_expression()?; // Allow chaining like --5
                let end_span = expr.span().end;
                Ok(Expr::UnaryOp(
                    UnaryOp::Neg,
                    Box::new(expr),
                    Span::new(start_span, end_span),
                ))
            }
            TokenKind::Bang => {
                self.advance(); // Consume '!'
                let expr = self.parse_unary_expression()?; // Allow chaining like !!true
                let end_span = expr.span().end;
                Ok(Expr::UnaryOp(
                    UnaryOp::Not,
                    Box::new(expr),
                    Span::new(start_span, end_span),
                ))
            }
            _ => self.parse_primary_expression(),
        }
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

                // Member access and method calls are handled in parse_postfix_expression
                match self.current_token().kind {
                    TokenKind::Bang => {
                        // Parse macro call
                        self.advance(); // Consume '!'
                        if self.current_token().kind != TokenKind::LParen {
                            return Err(Error::new_parse(
                                format!(
                                    "Expected '(' after macro '{}!', found {:?}",
                                    name,
                                    self.current_token()
                                ),
                                self.current_token().span,
                            ));
                        }
                        self.parse_macro_call(name, span)
                    }
                    TokenKind::LParen => {
                        // Check if this is an implicit Result/Option variant
                        match name.as_str() {
                            "Ok" | "Err" => self.parse_implicit_result_variant(name, span),
                            "Some" | "None" => self.parse_implicit_option_variant(name, span),
                            _ => self.parse_function_call(name, span),
                        }
                    }
                    TokenKind::LBrace if self.lookahead_for_struct_initialization(start_pos) => {
                        self.parse_struct_init(name, span)
                    }
                    TokenKind::DblColon => self.parse_enum_or_method_call_expression(name, span),
                    _ => {
                        // Check if this is None (which doesn't need parentheses)
                        if name == "None" {
                            Ok(Expr::EnumVariantOrMethodCall {
                                target: Box::new(Expr::Identifier("Option".to_string(), span)),
                                call: name,
                                args: vec![],
                                span,
                                type_annotation: TypeAnnotation::new(),
                            })
                        } else {
                            Ok(Expr::Identifier(name, span))
                        }
                    }
                }
            }
            TokenKind::LBrace => self.parse_object_literal_or_block(),
            TokenKind::LParen => {
                let start_span = span;
                self.advance(); // Consume '('

                // Check for unit value ()
                if self.current_token().kind == TokenKind::RParen {
                    let end_span = self.current_token().span.end;
                    self.advance(); // Consume ')'
                    return Ok(Expr::Unit(Span::new(start_span.start, end_span)));
                }

                // Parse first expression
                let first_expr = self.parse_expression()?;

                // Check if this is a tuple
                if self.current_token().kind == TokenKind::Comma {
                    let mut elements = vec![first_expr];

                    while self.current_token().kind == TokenKind::Comma {
                        self.advance(); // Consume ','

                        // Allow trailing comma
                        if self.current_token().kind == TokenKind::RParen {
                            break;
                        }

                        elements.push(self.parse_expression()?);
                    }

                    if self.current_token().kind != TokenKind::RParen {
                        return Err(Error::new_parse(
                            "Expected ')' after tuple elements".to_string(),
                            self.current_token().span,
                        ));
                    }
                    let end_span = self.current_token().span.end;
                    self.advance(); // Consume ')'

                    Ok(Expr::Tuple(elements, Span::new(start_span.start, end_span)))
                } else {
                    // Single parenthesized expression
                    if self.current_token().kind != TokenKind::RParen {
                        return Err(Error::new_parse(
                            "Expected ')' after expression".to_string(),
                            self.current_token().span,
                        ));
                    }
                    self.advance(); // Consume ')'
                    Ok(first_expr)
                }
            }
            TokenKind::If => self.parse_if_expression(),
            TokenKind::Match => self.parse_match_expression(),
            TokenKind::Underscore => {
                self.advance(); // Consume '_'
                Ok(Expr::Identifier("_".to_string(), span))
            }
            TokenKind::Pipe => self.parse_closure_expression(),
            TokenKind::DblPipe => {
                // Handle empty closure parameters ||
                let start_span = self.current_token().span.start;
                self.advance(); // Consume '||'

                // Optional return type
                let return_type = if self.current_token().kind == TokenKind::Arrow {
                    self.advance(); // Consume '->'
                    Some(self.consume_type().ok_or_else(|| {
                        Error::new_parse(
                            "Expected return type after '->'".to_string(),
                            self.current_token().span,
                        )
                    })?)
                } else {
                    None
                };

                // Parse body
                let body = if self.current_token().kind == TokenKind::LBrace {
                    self.parse_block_expression()?
                } else {
                    self.parse_expression()?
                };

                let end_span = body.span().end;

                Ok(Expr::Closure(
                    Vec::new(), // Empty parameters
                    return_type,
                    Box::new(body),
                    Span::new(start_span, end_span),
                ))
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

    fn parse_implicit_result_variant(&mut self, variant: String, span: Span) -> Result<Expr> {
        self.advance(); // Consume '('

        // Parse the single argument for Ok/Err
        let arg = if self.current_token().kind == TokenKind::RParen {
            // Empty Ok() or Err() - use unit value
            Expr::Unit(span)
        } else {
            self.parse_expression()?
        };

        self.expect_token(TokenKind::RParen)?;

        Ok(Expr::EnumVariantOrMethodCall {
            target: Box::new(Expr::Identifier("Result".to_string(), span)),
            call: variant,
            args: vec![arg],
            span,
            type_annotation: TypeAnnotation::new(),
        })
    }

    fn parse_implicit_option_variant(&mut self, variant: String, span: Span) -> Result<Expr> {
        if variant == "None" {
            // None doesn't take arguments
            self.advance(); // Consume '('
            self.expect_token(TokenKind::RParen)?;
            Ok(Expr::EnumVariantOrMethodCall {
                target: Box::new(Expr::Identifier("Option".to_string(), span)),
                call: variant,
                args: vec![],
                span,
                type_annotation: TypeAnnotation::new(),
            })
        } else {
            // Some(value)
            self.advance(); // Consume '('
            let arg = self.parse_expression()?;
            self.expect_token(TokenKind::RParen)?;

            Ok(Expr::EnumVariantOrMethodCall {
                target: Box::new(Expr::Identifier("Option".to_string(), span)),
                call: variant,
                args: vec![arg],
                span,
                type_annotation: TypeAnnotation::new(),
            })
        }
    }

    fn parse_enum_or_method_call_expression(&mut self, name: String, span: Span) -> Result<Expr> {
        let start = span.start;
        self.advance(); // Consume '::'

        let target_name = self
            .consume_identifier("parse_enum_or_method_call_expression")?
            .ok_or_else(|| {
                Error::new_parse("Expected identifier".to_string(), self.current_token().span)
            })?;

        // Check if there are parentheses for method call or braces for struct-like enum variant
        if self.current_token().kind == TokenKind::LParen {
            // This is a method call like Type::method(args)
            self.advance(); // Consume '('

            let mut exprs = Vec::new();

            // Parse arguments if any
            if self.current_token().kind != TokenKind::RParen {
                loop {
                    exprs.push(self.parse_expression()?);

                    match self.current_token().kind {
                        TokenKind::RParen => break,
                        TokenKind::Comma => self.advance(),
                        _ => {
                            return Err(Error::new_parse(
                                "Expected ')' after arguments".to_string(),
                                self.current_token().span,
                            ))
                        }
                    }
                }
            }

            let end = self.current_token().span.end;
            self.advance(); // Consume ')'
            return Ok(Expr::EnumVariantOrMethodCall {
                target: Box::new(Expr::Identifier(name, span)),
                call: target_name,
                args: exprs,
                span: Span::new(start, end),
                type_annotation: TypeAnnotation::new(),
            });
        } else if self.current_token().kind == TokenKind::LBrace {
            // This is a struct-like enum variant construction like Command::Process { field: value, ... }
            // But we need to make sure this { is actually part of the enum variant, not a following block
            // Check if this looks like a struct initialization by peeking ahead
            let is_struct_init = self.peek_ahead_for_struct_field();
            if !is_struct_init {
                // This { is not part of the enum variant, return the variant without args
                let new_span = Span::new(start, self.current_token().span.start);
                return Ok(Expr::EnumVariantOrMethodCall {
                    target: Box::new(Expr::Identifier(name, span)),
                    call: target_name,
                    args: vec![],
                    span: new_span,
                    type_annotation: TypeAnnotation::new(),
                });
            }

            let mut fields = Vec::new();
            self.advance(); // Consume '{'

            while self.current_token().kind != TokenKind::RBrace {
                let field_name = self
                    .consume_identifier("parse_enum_struct_variant")?
                    .ok_or_else(|| {
                        Error::new_parse(
                            "Expected field name".to_string(),
                            self.current_token().span,
                        )
                    })?;
                let field_value = if self.current_token().kind == TokenKind::Colon {
                    // Regular field syntax: field: expr
                    self.advance(); // Consume ':'
                    self.parse_expression()?
                } else if self.current_token().kind == TokenKind::Comma
                    || self.current_token().kind == TokenKind::RBrace
                {
                    // Shorthand field syntax: just field name
                    // Create an identifier expression with the same name
                    Expr::Identifier(field_name.clone(), self.current_token().span)
                } else {
                    return Err(Error::new_parse(
                        "Expected ':', ',' or '}' after field name".to_string(),
                        self.current_token().span,
                    ));
                };

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
            let end = self.current_token().span.end;
            self.advance(); // Consume '}'

            // Create a struct-like variant name
            let variant_name = format!("{}::{}", name, target_name);

            return Ok(Expr::StructInit(
                variant_name,
                fields,
                Span::new(start, end),
            ));
        }

        // This is a method reference like Type::method (without parentheses)
        let new_span = Span::new(start, self.current_token().span.end);
        Ok(Expr::EnumVariantOrMethodCall {
            target: Box::new(Expr::Identifier(name, span)),
            call: target_name,
            args: vec![], // Empty args indicates this is a method reference, not a call
            span: new_span,
            type_annotation: TypeAnnotation::new(),
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

    fn parse_macro_call(&mut self, name: String, start_span: Span) -> Result<Expr> {
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
        Ok(Expr::MacroCall(
            name,
            args,
            Span::new(start_span.start, end_span.end),
        ))
    }

    fn peek_ahead_for_struct_field(&self) -> bool {
        // Look ahead to see if the next tokens look like struct field initialization
        // We expect either:
        // 1. identifier : expression
        // 2. identifier , (shorthand)
        // 3. identifier } (shorthand, last field)
        // 4. } (empty struct)
        // 5. .. (rest pattern)

        if self.position + 1 >= self.tokens.len() {
            return false;
        }

        let next_token = &self.tokens[self.position + 1];
        match next_token.kind {
            TokenKind::Identifier(_) => {
                // Check what comes after the identifier
                if self.position + 2 < self.tokens.len() {
                    let after_ident = &self.tokens[self.position + 2];
                    matches!(
                        after_ident.kind,
                        TokenKind::Colon | TokenKind::Comma | TokenKind::RBrace
                    )
                } else {
                    false
                }
            }
            TokenKind::RBrace => true, // Empty struct
            TokenKind::DblDot => true, // Rest pattern
            _ => false,
        }
    }

    fn lookahead_for_struct_initialization(&self, start_pos: usize) -> bool {
        if start_pos > 1 {
            let lookback_index = self.position - 2;
            let before_identifier = &self.tokens[lookback_index];
            if before_identifier.kind == TokenKind::If
                || before_identifier.kind == TokenKind::For
                || before_identifier.kind == TokenKind::In
                || before_identifier.kind == TokenKind::Bang
                || before_identifier.kind == TokenKind::Match  // Add this check!
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
            let field_name = self
                .consume_identifier("parse_struct_init")?
                .ok_or_else(|| {
                    Error::new_parse("Expected field name".to_string(), self.current_token().span)
                })?;

            let field_value = if self.current_token().kind == TokenKind::Colon {
                // Regular field syntax: field: expr
                self.advance(); // Consume ':'
                self.parse_expression()?
            } else if self.current_token().kind == TokenKind::Comma
                || self.current_token().kind == TokenKind::RBrace
            {
                // Shorthand field syntax: just field name
                // Create an identifier expression with the same name
                Expr::Identifier(field_name.clone(), self.current_token().span)
            } else {
                return Err(Error::new_parse(
                    "Expected ':', ',' or '}' after field name".to_string(),
                    self.current_token().span,
                ));
            };

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
    matches!(
        kind,
        TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Asterisk
            | TokenKind::Slash
            | TokenKind::DblEquals
    )
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
            Stmt::Expression(
                Expr::If(
                    Box::new(Expr::Bool(true, Span::new(16, 20))),
                    vec![Stmt::Let(
                        "x".to_string(),
                        Expr::Int(5, Span::new(47, 48)),
                        Span::new(39, 49),
                    )],
                    vec![],
                    Span::new(13, 63),
                ),
                false
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
            Stmt::Expression(
                Expr::If(
                    Box::new(Expr::Bool(true, Span::new(16, 20))),
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
                ),
                false
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
                vec![], // Generic params
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
            Stmt::Expression(
                Expr::Assign(
                    Box::new(Expr::MemberAccess(
                        Box::new(Expr::Identifier("client".to_string(), Span::new(0, 6))),
                        "age".to_string(),
                        Span::new(0, 15),
                    )),
                    Box::new(Expr::Int(12, Span::new(13, 15))),
                    Span::new(0, 15),
                ),
                true
            )
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
                vec![], // attributes
                false,  // Not public
                "add".to_string(),
                vec![], // Generic params
                vec![
                    ("x".to_string(), "int".to_string()),
                    ("y".to_string(), "int".to_string()),
                ],
                "int".to_string(),
                vec![Stmt::Expression(
                    Expr::BinaryOp(
                        Box::new(Expr::Identifier("x".to_string(), Span::new(32, 33))),
                        Operator::Plus,
                        Box::new(Expr::Identifier("y".to_string(), Span::new(36, 37))),
                        Span::new(32, 35),
                    ),
                    false
                )],
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
                vec![], // attributes
                false,  // Not public
                "create_person".to_string(),
                vec![], // Generic params
                vec![
                    ("name".to_string(), "string".to_string()),
                    ("age".to_string(), "int".to_string()),
                ],
                "Person".to_string(),
                vec![Stmt::Expression(
                    Expr::StructInit(
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
                    ),
                    false
                )],
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
            Stmt::Expression(
                Expr::BinaryOp(
                    Box::new(Expr::Identifier("x".to_string(), Span::new(0, 1))),
                    Operator::Equals,
                    Box::new(Expr::Identifier("y".to_string(), Span::new(5, 6))),
                    Span::new(0, 4),
                ),
                true
            )
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
            Stmt::Expression(
                Expr::If(
                    Box::new(Expr::BinaryOp(
                        Box::new(Expr::Identifier("x".to_string(), Span::new(16, 17))),
                        Operator::Equals,
                        Box::new(Expr::Identifier("y".to_string(), Span::new(21, 22))),
                        Span::new(16, 20),
                    )),
                    vec![Stmt::Expression(Expr::Int(1, Span::new(41, 42)), false)],
                    vec![Stmt::Expression(Expr::Int(0, Span::new(80, 81)), false)],
                    Span::new(13, 95),
                ),
                false
            )
        );
    }

    #[test]
    fn test_if_vs_struct() {
        let code = r#"
        let p = Person { name: "Felipe" };
        if y == x {
            println!(p);
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

        let Stmt::Enum(enum_name, _, variants, _) = &ast[0] else {
            panic!("Expected Enum statement");
        };

        assert_eq!(enum_name, "Color");
        assert_eq!(
            variants,
            &[
                EnumVariant::Unit("Red".to_string()),
                EnumVariant::Unit("Green".to_string()),
                EnumVariant::Unit("Blue".to_string()),
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

        let Stmt::Enum(enum_name, _, variants, _) = &ast[0] else {
            panic!("Expected Enum statement");
        };

        assert_eq!(enum_name, "Name");
        assert_eq!(
            variants,
            &[
                EnumVariant::Tuple("Existing".to_string(), "string".to_string()),
                EnumVariant::Tuple("New".to_string(), "string".to_string()),
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
                        Expr::EnumVariantOrMethodCall {
                            target: Box::new(Expr::Identifier(
                                "Color".to_string(),
                                Span::default()
                            )),
                            call: "Red".to_string(),
                            args: vec![],
                            span: Span::new(8, 19),
                            type_annotation: TypeAnnotation::new(),
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
                Expr::EnumVariantOrMethodCall {
                    target: Box::new(Expr::Identifier("Name".to_string(), Span::default())),
                    call: "Existing".to_string(),
                    args: vec![Expr::String("Alice".to_string(), Span::new(23, 30))],
                    span: Span::new(8, 31),
                    type_annotation: TypeAnnotation::new(),
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
            Stmt::Expression(
                Expr::EnumVariantOrMethodCall {
                    target: Box::new(Expr::Identifier("Name".to_string(), Span::default())),
                    call: "Existing".to_string(),
                    args: vec![Expr::Identifier("name".to_string(), Span::new(15, 19))],
                    span: Span::new(0, 20),
                    type_annotation: TypeAnnotation::new(),
                },
                false
            )
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
            Stmt::Expression(
                Expr::CompoundAssign(
                    Box::new(Expr::Identifier("x".to_string(), Span::new(13, 14))),
                    Operator::Plus,
                    Box::new(Expr::Int(1, Span::new(17, 18))),
                    Span::new(13, 22),
                ),
                true
            )
        );
    }

    #[test]
    fn test_parse_compound_assignment_minus() {
        let code = "x -= 1;";

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Expression(
                Expr::CompoundAssign(
                    Box::new(Expr::Identifier("x".to_string(), Span::new(13, 14))),
                    Operator::Minus,
                    Box::new(Expr::Int(1, Span::new(17, 18))),
                    Span::new(13, 22),
                ),
                true
            )
        );
    }

    #[test]
    fn test_parse_operators() {
        let code = r#"
            let x = 1 + 2;
            let y = 3 - 4;
            let z = 5 * 6;
            let w = 7 / 8;
            let a = 9 % 10;
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Let(
                "x".to_string(),
                Expr::BinaryOp(
                    Box::new(Expr::Int(1, Span::new(21, 22))),
                    Operator::Plus,
                    Box::new(Expr::Int(2, Span::new(25, 26))),
                    Span::new(21, 24),
                ),
                Span::new(13, 27),
            )
        );

        assert_eq!(
            ast[1],
            Stmt::Let(
                "y".to_string(),
                Expr::BinaryOp(
                    Box::new(Expr::Int(3, Span::new(48, 49))),
                    Operator::Minus,
                    Box::new(Expr::Int(4, Span::new(52, 53))),
                    Span::new(48, 51),
                ),
                Span::new(40, 54),
            )
        );

        assert_eq!(
            ast[2],
            Stmt::Let(
                "z".to_string(),
                Expr::BinaryOp(
                    Box::new(Expr::Int(5, Span::new(75, 76))),
                    Operator::Multiply,
                    Box::new(Expr::Int(6, Span::new(79, 80))),
                    Span::new(75, 78),
                ),
                Span::new(67, 81),
            )
        );

        assert_eq!(
            ast[3],
            Stmt::Let(
                "w".to_string(),
                Expr::BinaryOp(
                    Box::new(Expr::Int(7, Span::new(102, 103))),
                    Operator::Divide,
                    Box::new(Expr::Int(8, Span::new(106, 107))),
                    Span::new(102, 105),
                ),
                Span::new(94, 108),
            )
        );

        assert_eq!(
            ast[4],
            Stmt::Let(
                "a".to_string(),
                Expr::BinaryOp(
                    Box::new(Expr::Int(9, Span::new(129, 130))),
                    Operator::Modulo,
                    Box::new(Expr::Int(10, Span::new(133, 135))),
                    Span::new(129, 134),
                ),
                Span::new(121, 136),
            )
        );
    }

    #[test]
    fn test_parse_compound_operators() {
        let code = r#"
            x += 2;
            y -= 4;
            z *= 6;
            w /= 8;
            a %= 10;
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Expression(
                Expr::CompoundAssign(
                    Box::new(Expr::Identifier(
                        "x".to_string(),
                        Span { start: 13, end: 14 }
                    )),
                    Operator::Plus,
                    Box::new(Expr::Int(2, Span { start: 18, end: 19 })),
                    Span { start: 13, end: 19 }
                ),
                true
            )
        );
    }

    #[test]
    fn test_parse_while() {
        let code = r#"
            while x < 10 {
                print(x);
                x += 1;
            }
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::While(
                Expr::BinaryOp(
                    Box::new(Expr::Identifier("x".to_string(), Span::new(19, 20))),
                    Operator::LessThan,
                    Box::new(Expr::Int(10, Span::new(23, 25))),
                    Span::new(19, 22),
                ),
                vec![
                    Stmt::Expression(
                        Expr::FunctionCall(
                            "print".to_string(),
                            vec![Expr::Identifier("x".to_string(), Span::new(50, 51))],
                            Span::new(44, 52),
                        ),
                        true
                    ),
                    Stmt::Expression(
                        Expr::CompoundAssign(
                            Box::new(Expr::Identifier("x".to_string(), Span::new(70, 71))),
                            Operator::Plus,
                            Box::new(Expr::Int(1, Span::new(75, 76))),
                            Span::new(70, 76),
                        ),
                        true
                    ),
                ],
                Span::new(13, 91),
            )
        );
    }

    #[test]
    fn test_parse_loop() {
        let code = r#"
            loop {
                print("Hello");
            }
        "#;

        let ast = parse(code);

        assert_eq!(
            ast[0],
            Stmt::Loop(
                vec![Stmt::Expression(
                    Expr::FunctionCall(
                        "print".to_string(),
                        vec![Expr::String("Hello".to_string(), Span::new(42, 49))],
                        Span::new(36, 50),
                    ),
                    true
                )],
                Span::new(13, 65),
            )
        );
    }

    #[test]
    fn test_match_wildcard_enum() {
        let code = r#"
            match n {
                Name::Existing => "Existing",
                _ => "New",
            }
        "#;

        let _ = parse(code);
    }

    #[test]
    fn parse_struct_method() {
        let code = r#"
            impl Point {
                fn new(x: int, y: int) -> Point {
                    Point { x: x, y: y }
                }
            }
        "#;
        let x = parse(code);
        println!("{:?}", x);
    }

    #[test]
    fn parse_method_call() {
        let code = "Point::new(3, 4)";
        println!("{:?}", parse(code));
    }

    fn create_parser_for_expr(input: &str) -> Parser {
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        Parser::new(tokens)
    }

    #[test]
    fn test_unary_negation() {
        let mut parser = create_parser_for_expr("-5");
        let expr = parser.parse_unary_expression().unwrap();
        assert_eq!(
            expr,
            Expr::UnaryOp(
                UnaryOp::Neg,
                Box::new(Expr::Int(5, Span::new(1, 2))),
                Span::new(0, 2)
            )
        );
    }

    #[test]
    fn test_unary_negation_float() {
        let mut parser = create_parser_for_expr("-3.15");
        let expr = parser.parse_unary_expression().unwrap();
        assert_eq!(
            expr,
            Expr::UnaryOp(
                UnaryOp::Neg,
                Box::new(Expr::Float(3.15, Span::new(1, 5))),
                Span::new(0, 5)
            )
        );
    }

    #[test]
    fn test_unary_not() {
        let mut parser = create_parser_for_expr("!true");
        let expr = parser.parse_unary_expression().unwrap();
        assert_eq!(
            expr,
            Expr::UnaryOp(
                UnaryOp::Not,
                Box::new(Expr::Bool(true, Span::new(1, 5))),
                Span::new(0, 5)
            )
        );
    }

    #[test]
    fn test_unary_double_negation() {
        let mut parser = create_parser_for_expr("--5");
        let expr = parser.parse_unary_expression().unwrap();
        assert_eq!(
            expr,
            Expr::UnaryOp(
                UnaryOp::Neg,
                Box::new(Expr::UnaryOp(
                    UnaryOp::Neg,
                    Box::new(Expr::Int(5, Span::new(2, 3))),
                    Span::new(1, 3)
                )),
                Span::new(0, 3)
            )
        );
    }

    #[test]
    fn test_unary_in_expression() {
        let mut parser = create_parser_for_expr("2 + -3");
        let expr = parser.parse_expression().unwrap();
        assert_eq!(
            expr,
            Expr::BinaryOp(
                Box::new(Expr::Int(2, Span::new(0, 1))),
                Operator::Plus,
                Box::new(Expr::UnaryOp(
                    UnaryOp::Neg,
                    Box::new(Expr::Int(3, Span::new(5, 6))),
                    Span::new(4, 6)
                )),
                Span::new(0, 3)
            )
        );
    }

    #[test]
    fn test_parse_pub_keyword() {
        // Test pub function
        let ast = parse("pub fn hello() { println!(\"Hello\"); }");
        assert_eq!(ast.len(), 1);
        // Check that pub is properly stored in AST
        if let Stmt::Function(_, is_pub, name, _, params, ret_type, body, _) = &ast[0] {
            assert!(*is_pub);
            assert_eq!(name, "hello");
            assert_eq!(params.len(), 0);
            assert_eq!(ret_type, "void");
            assert_eq!(body.len(), 1);
        } else {
            panic!("Expected Function statement");
        }

        // Test pub struct
        let ast = parse("pub struct User { name: string, age: int }");
        assert_eq!(ast.len(), 1);
        if let Stmt::Struct(name, _, fields, _) = &ast[0] {
            assert_eq!(name, "User");
            assert_eq!(fields.len(), 2);
        } else {
            panic!("Expected Struct statement");
        }

        // Test pub enum
        let ast = parse("pub enum Status { Ok, Error }");
        assert_eq!(ast.len(), 1);
        if let Stmt::Enum(name, _, variants, _) = &ast[0] {
            assert_eq!(name, "Status");
            assert_eq!(variants.len(), 2);
        } else {
            panic!("Expected Enum statement");
        }

        // Test pub impl
        let ast = parse("pub impl Point { fn new() -> Point { Point { x: 0, y: 0 } } }");
        assert_eq!(ast.len(), 1);
        if let Stmt::Impl(name, methods, _) = &ast[0] {
            assert_eq!(name, "Point");
            assert_eq!(methods.len(), 1);
        } else {
            panic!("Expected Impl statement");
        }
    }

    #[test]
    fn test_parse_pub_errors() {
        // Test that pub cannot be used with let
        let mut lexer = Lexer::new("pub let x = 5;".to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_err());
        if let Err(e) = result {
            let error_str = e.to_string();
            assert!(error_str.contains("'pub' cannot be used with 'let' statements"));
        }

        // Test that pub cannot be used with use
        let mut lexer = Lexer::new("pub use std::io;".to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_err());
        if let Err(e) = result {
            let error_str = e.to_string();
            assert!(error_str.contains("'pub' cannot be used with 'use' statements"));
        }

        // Test that pub cannot be used with loops
        let mut lexer = Lexer::new("pub for i in 1..10 { }".to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_err());
        if let Err(e) = result {
            let error_str = e.to_string();
            assert!(error_str.contains("'pub' cannot be used with 'for' loops"));
        }
    }

    #[test]
    fn test_parse_use_statements() {
        // Test external package import
        let ast = parse("use express::express;");
        assert_eq!(ast.len(), 1);
        if let Stmt::Use(path, items, _) = &ast[0] {
            assert_eq!(path.prefix, UsePrefix::None);
            assert_eq!(path.segments, vec!["express", "express"]);
            assert_eq!(items, &UseItems::Single);
        } else {
            panic!("Expected Use statement");
        }

        // Test local import
        let ast = parse("use local::models::User;");
        assert_eq!(ast.len(), 1);
        if let Stmt::Use(path, items, _) = &ast[0] {
            assert_eq!(path.prefix, UsePrefix::Local);
            assert_eq!(path.segments, vec!["models", "User"]);
            assert_eq!(items, &UseItems::Single);
        } else {
            panic!("Expected Use statement");
        }

        // Test self import
        let ast = parse("use self::helper;");
        assert_eq!(ast.len(), 1);
        if let Stmt::Use(path, items, _) = &ast[0] {
            assert_eq!(path.prefix, UsePrefix::Self_);
            assert_eq!(path.segments, vec!["helper"]);
            assert_eq!(items, &UseItems::Single);
        } else {
            panic!("Expected Use statement");
        }

        // Test super import
        let ast = parse("use super::shared;");
        assert_eq!(ast.len(), 1);
        if let Stmt::Use(path, items, _) = &ast[0] {
            assert_eq!(path.prefix, UsePrefix::Super(1));
            assert_eq!(path.segments, vec!["shared"]);
            assert_eq!(items, &UseItems::Single);
        } else {
            panic!("Expected Use statement");
        }

        // Test multiple super
        let ast = parse("use super::super::utils;");
        assert_eq!(ast.len(), 1);
        if let Stmt::Use(path, items, _) = &ast[0] {
            assert_eq!(path.prefix, UsePrefix::Super(2));
            assert_eq!(path.segments, vec!["utils"]);
            assert_eq!(items, &UseItems::Single);
        } else {
            panic!("Expected Use statement");
        }

        // Test named imports
        let ast = parse("use react::{React, useState, useEffect};");
        assert_eq!(ast.len(), 1);
        if let Stmt::Use(path, items, _) = &ast[0] {
            assert_eq!(path.prefix, UsePrefix::None);
            assert_eq!(path.segments, vec!["react"]);
            if let UseItems::Named(named) = items {
                assert_eq!(named.len(), 3);
                assert_eq!(named[0], ("React".to_string(), None));
                assert_eq!(named[1], ("useState".to_string(), None));
                assert_eq!(named[2], ("useEffect".to_string(), None));
            } else {
                panic!("Expected Named items");
            }
        } else {
            panic!("Expected Use statement");
        }

        // Test imports with aliases
        let ast = parse("use fs::promises::{readFile as read, writeFile as write};");
        assert_eq!(ast.len(), 1);
        if let Stmt::Use(path, items, _) = &ast[0] {
            assert_eq!(path.prefix, UsePrefix::None);
            assert_eq!(path.segments, vec!["fs", "promises"]);
            if let UseItems::Named(named) = items {
                assert_eq!(named.len(), 2);
                assert_eq!(named[0], ("readFile".to_string(), Some("read".to_string())));
                assert_eq!(
                    named[1],
                    ("writeFile".to_string(), Some("write".to_string()))
                );
            } else {
                panic!("Expected Named items");
            }
        } else {
            panic!("Expected Use statement");
        }

        // Test wildcard import
        let ast = parse("use std::collections::*;");
        assert_eq!(ast.len(), 1);
        if let Stmt::Use(path, items, _) = &ast[0] {
            assert_eq!(path.prefix, UsePrefix::None);
            assert_eq!(path.segments, vec!["std", "collections"]);
            assert_eq!(items, &UseItems::All);
        } else {
            panic!("Expected Use statement");
        }
    }

    // ===== New Syntax Feature Tests =====

    #[test]
    fn test_parse_type_cast_simple() {
        let ast = parse("42 as int;");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(Expr::Cast(inner, target_type, _), _) => {
                assert!(matches!(**inner, Expr::Int(42, _)));
                assert_eq!(target_type, "int");
            }
            _ => panic!("Expected Cast expression"),
        }
    }

    #[test]
    fn test_parse_type_cast_chain() {
        let ast = parse("3.15 as int as float;");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(Expr::Cast(inner, target_type, _), _) => {
                assert_eq!(target_type, "float");
                match &**inner {
                    Expr::Cast(inner2, target_type2, _) => {
                        assert!(matches!(**inner2, Expr::Float(f, _) if f == 3.15));
                        assert_eq!(target_type2, "int");
                    }
                    _ => panic!("Expected nested Cast expression"),
                }
            }
            _ => panic!("Expected Cast expression"),
        }
    }

    #[test]
    fn test_parse_type_cast_in_expression() {
        let ast = parse("x + (y as int) * 2;");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(expr, _) => match expr {
                Expr::BinaryOp(left, op, right, _) => {
                    assert!(matches!(**left, Expr::Identifier(ref name, _) if name == "x"));
                    assert!(matches!(op, Operator::Plus));
                    match &**right {
                        Expr::BinaryOp(left2, op2, right2, _) => {
                            assert!(matches!(op2, Operator::Multiply));
                            assert!(matches!(**right2, Expr::Int(2, _)));
                            match &**left2 {
                                Expr::Cast(inner, target_type, _) => {
                                    assert!(
                                        matches!(**inner, Expr::Identifier(ref name, _) if name == "y")
                                    );
                                    assert_eq!(target_type, "int");
                                }
                                _ => panic!("Expected Cast expression"),
                            }
                        }
                        _ => panic!("Expected BinaryOp"),
                    }
                }
                _ => panic!("Expected BinaryOp"),
            },
            _ => panic!("Expected Expression statement"),
        }
    }

    #[test]
    fn test_parse_qualified_type_in_function() {
        let ast = parse("fn handler(req: express::Request, res: express::Response) {}");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Function(_, _, name, _, params, _, _, _) => {
                assert_eq!(name, "handler");
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].0, "req");
                assert_eq!(params[0].1, "express::Request");
                assert_eq!(params[1].0, "res");
                assert_eq!(params[1].1, "express::Response");
            }
            _ => panic!("Expected Function statement"),
        }
    }

    #[test]
    fn test_parse_qualified_return_type() {
        let ast = parse("fn create_app() -> framework::Application {}");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Function(_, _, name, _, _, return_type, _, _) => {
                assert_eq!(name, "create_app");
                assert_eq!(return_type, "framework::Application");
            }
            _ => panic!("Expected Function statement"),
        }
    }

    #[test]
    fn test_parse_extern_fn() {
        let ast = parse("extern fn parseInt(s: string) -> int;");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::ExternFunction(name, generics, params, return_type, _) => {
                assert_eq!(name, "parseInt");
                assert_eq!(generics.len(), 0);
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].0, "s");
                assert_eq!(params[0].1, "string");
                assert_eq!(return_type, "int");
            }
            _ => panic!("Expected ExternFunction statement"),
        }
    }

    #[test]
    fn test_parse_extern_fn_generic() {
        let ast = parse("extern fn map<T, U>(arr: array<T>, f: fn(T) -> U) -> array<U>;");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::ExternFunction(name, generics, params, return_type, _) => {
                assert_eq!(name, "map");
                assert_eq!(generics.len(), 2);
                assert_eq!(generics[0], "T");
                assert_eq!(generics[1], "U");
                assert_eq!(params.len(), 2);
                assert_eq!(return_type, "array<U>");
            }
            _ => panic!("Expected ExternFunction statement"),
        }
    }

    #[test]
    fn test_parse_extern_mod() {
        let input = r#"
        extern mod express {
            type Application;
            fn express() -> Application;
            impl Application {
                fn listen(port: int);
            }
        }
        "#;
        let ast = parse(input);
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::ExternMod(name, declarations, _) => {
                assert_eq!(name, "express");
                assert_eq!(declarations.len(), 3);

                // Check type declaration
                match &declarations[0] {
                    ExternItem::Type(type_name, generics) => {
                        assert_eq!(type_name, "Application");
                        assert_eq!(generics.len(), 0);
                    }
                    _ => panic!("Expected Type declaration"),
                }

                // Check function declaration
                match &declarations[1] {
                    ExternItem::Function(fn_name, _, _, return_type) => {
                        assert_eq!(fn_name, "express");
                        assert_eq!(return_type, "Application");
                    }
                    _ => panic!("Expected Function declaration"),
                }

                // Check impl block
                match &declarations[2] {
                    ExternItem::Impl(impl_type, methods) => {
                        assert_eq!(impl_type, "Application");
                        assert_eq!(methods.len(), 1);
                        match &methods[0] {
                            ExternItem::Function(method_name, _, _, _) => {
                                assert_eq!(method_name, "listen");
                            }
                            _ => panic!("Expected method Function"),
                        }
                    }
                    _ => panic!("Expected Impl declaration"),
                }
            }
            _ => panic!("Expected ExternMod statement"),
        }
    }

    #[test]
    fn test_parse_async_function() {
        let ast = parse("async fn fetch_data() -> string {}");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::AsyncFunction(_, _, name, _, _, return_type, _, _) => {
                assert_eq!(name, "fetch_data");
                assert_eq!(return_type, "string");
            }
            _ => panic!("Expected AsyncFunction statement"),
        }
    }

    #[test]
    fn test_parse_await_expression() {
        let ast = parse("fetch(url).await;");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(Expr::Await(inner, _), _) => match &**inner {
                Expr::FunctionCall(name, _, _) => {
                    assert_eq!(name, "fetch");
                }
                _ => panic!("Expected FunctionCall inside Await"),
            },
            _ => panic!("Expected Await expression"),
        }
    }

    #[test]
    fn test_parse_await_chain() {
        let ast = parse("fetch(url).await.json().await;");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(Expr::Await(inner, _), _) => match &**inner {
                Expr::MethodCall(obj, method, _, _) => {
                    assert_eq!(method, "json");
                    match &**obj {
                        Expr::Await(inner2, _) => match &**inner2 {
                            Expr::FunctionCall(name, _, _) => {
                                assert_eq!(name, "fetch");
                            }
                            _ => panic!("Expected FunctionCall"),
                        },
                        _ => panic!("Expected Await"),
                    }
                }
                _ => panic!("Expected MethodCall"),
            },
            _ => panic!("Expected Await expression"),
        }
    }

    #[test]
    fn test_parse_simple_closure() {
        let ast = parse("|x| x * 2");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(Expr::Closure(params, return_type, body, _), _) => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].0, "x");
                assert!(params[0].1.is_none());
                assert!(return_type.is_none());
                match &**body {
                    Expr::BinaryOp(left, op, right, _) => {
                        assert!(matches!(**left, Expr::Identifier(ref name, _) if name == "x"));
                        assert!(matches!(op, Operator::Multiply));
                        assert!(matches!(**right, Expr::Int(2, _)));
                    }
                    _ => panic!("Expected BinaryOp in closure body"),
                }
            }
            _ => panic!("Expected Closure expression"),
        }
    }

    #[test]
    fn test_parse_typed_closure() {
        let ast = parse("|x: int, y: int| -> int { x + y }");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(Expr::Closure(params, return_type, _body, _), _) => {
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].0, "x");
                assert_eq!(params[0].1.as_ref().unwrap(), "int");
                assert_eq!(params[1].0, "y");
                assert_eq!(params[1].1.as_ref().unwrap(), "int");
                assert_eq!(return_type.as_ref().unwrap(), "int");
            }
            _ => panic!("Expected Closure expression"),
        }
    }

    #[test]
    fn test_parse_simple_enum_pattern() {
        let code = r#"
            match x {
                Help => println!("help"),
            }
        "#;

        let result = parse(code);
        if result.is_empty() {
            panic!("Failed to parse: no AST returned");
        }

        // If we get here without panicking, the parse succeeded
        // The parse() function internally handles errors
    }

    #[test]
    fn test_parse_method_call_simple() {
        let ast = parse("str.len();");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(Expr::MethodCall(obj, method, args, _), _) => {
                assert!(matches!(**obj, Expr::Identifier(ref name, _) if name == "str"));
                assert_eq!(method, "len");
                assert_eq!(args.len(), 0);
            }
            _ => panic!("Expected MethodCall expression"),
        }
    }

    #[test]
    fn test_parse_method_call_with_args() {
        let ast = parse("text.split(\",\");");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(Expr::MethodCall(obj, method, args, _), _) => {
                assert!(matches!(**obj, Expr::Identifier(ref name, _) if name == "text"));
                assert_eq!(method, "split");
                assert_eq!(args.len(), 1);
                assert!(matches!(args[0], Expr::String(ref s, _) if s == ","));
            }
            _ => panic!("Expected MethodCall expression"),
        }
    }

    #[test]
    fn test_parse_chained_method_calls() {
        let ast = parse("input.trim().to_lowercase();");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(Expr::MethodCall(obj, method, args, _), _) => {
                assert_eq!(method, "to_lowercase");
                assert_eq!(args.len(), 0);
                match &**obj {
                    Expr::MethodCall(obj2, method2, args2, _) => {
                        assert!(matches!(**obj2, Expr::Identifier(ref name, _) if name == "input"));
                        assert_eq!(method2, "trim");
                        assert_eq!(args2.len(), 0);
                    }
                    _ => panic!("Expected nested MethodCall"),
                }
            }
            _ => panic!("Expected MethodCall expression"),
        }
    }

    #[test]
    fn test_parse_format_macro_simple() {
        let ast = parse("format!(\"Hello, {}!\", name);");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(Expr::FunctionCall(name, args, _), _) => {
                assert_eq!(name, "format!");
                assert_eq!(args.len(), 2);
                assert!(matches!(args[0], Expr::String(ref s, _) if s == "Hello, {}!"));
                assert!(matches!(args[1], Expr::Identifier(ref n, _) if n == "name"));
            }
            _ => panic!("Expected FunctionCall for format! macro"),
        }
    }

    #[test]
    fn test_parse_format_macro_multiple_args() {
        let ast = parse("format!(\"{} + {} = {}\", a, b, a + b);");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(Expr::FunctionCall(name, args, _), _) => {
                assert_eq!(name, "format!");
                assert_eq!(args.len(), 4);
                assert!(matches!(args[0], Expr::String(ref s, _) if s == "{} + {} = {}"));
                assert!(matches!(args[1], Expr::Identifier(ref n, _) if n == "a"));
                assert!(matches!(args[2], Expr::Identifier(ref n, _) if n == "b"));
                assert!(matches!(args[3], Expr::BinaryOp(_, _, _, _)));
            }
            _ => panic!("Expected FunctionCall for format! macro"),
        }
    }

    #[test]
    fn test_parse_method_chain_with_cast() {
        let ast = parse("value.toString().len() as float;");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(Expr::Cast(inner, target_type, _), _) => {
                assert_eq!(target_type, "float");
                match &**inner {
                    Expr::MethodCall(obj, method, _, _) => {
                        assert_eq!(method, "len");
                        match &**obj {
                            Expr::MethodCall(obj2, method2, _, _) => {
                                assert_eq!(method2, "toString");
                                assert!(
                                    matches!(**obj2, Expr::Identifier(ref n, _) if n == "value")
                                );
                            }
                            _ => panic!("Expected nested MethodCall"),
                        }
                    }
                    _ => panic!("Expected MethodCall"),
                }
            }
            _ => panic!("Expected Cast expression"),
        }
    }

    #[test]
    fn test_parse_extern_type_in_closure() {
        let ast = parse("|req: express::Request, res: express::Response| res.send(\"OK\")");
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Expression(Expr::Closure(params, _, _, _), _) => {
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].1.as_ref().unwrap(), "express::Request");
                assert_eq!(params[1].1.as_ref().unwrap(), "express::Response");
            }
            _ => panic!("Expected Closure expression"),
        }
    }

    #[test]
    fn test_parse_async_closure() {
        // Async closures are not yet supported, so let's test a regular closure
        let input = r#"
        let fetch_user = |id: int| {
            let response = fetch(format!("/api/users/{}", id));
            response.json()
        };
        "#;
        let ast = parse(input);
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Let(name, expr, _) => {
                assert_eq!(name, "fetch_user");
                // Verify it's a closure
                assert!(matches!(expr, Expr::Closure(_, _, _, _)));
            }
            _ => panic!("Expected Let statement"),
        }
    }

    #[test]
    fn test_parse_object_literal() {
        // Test simple object literal
        let input = r#"let obj = { name: "John", age: 30 };"#;
        let ast = parse(input);
        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Stmt::Let(name, expr, _) => {
                assert_eq!(name, "obj");
                match expr {
                    Expr::ObjectLiteral(fields, _) => {
                        assert_eq!(fields.len(), 2);
                        assert_eq!(fields[0].0, "name");
                        assert_eq!(fields[1].0, "age");
                    }
                    _ => panic!("Expected ObjectLiteral"),
                }
            }
            _ => panic!("Expected Let statement"),
        }

        // Test nested object literal
        let input2 = r#"let config = { server: { host: "localhost", port: 3000 }, debug: true };"#;
        let ast2 = parse(input2);
        assert_eq!(ast2.len(), 1);
        match &ast2[0] {
            Stmt::Let(name, expr, _) => {
                assert_eq!(name, "config");
                match expr {
                    Expr::ObjectLiteral(fields, _) => {
                        assert_eq!(fields.len(), 2);
                        assert_eq!(fields[0].0, "server");
                        // Check nested object
                        match &fields[0].1 {
                            Expr::ObjectLiteral(nested_fields, _) => {
                                assert_eq!(nested_fields.len(), 2);
                                assert_eq!(nested_fields[0].0, "host");
                                assert_eq!(nested_fields[1].0, "port");
                            }
                            _ => panic!("Expected nested ObjectLiteral"),
                        }
                        assert_eq!(fields[1].0, "debug");
                    }
                    _ => panic!("Expected ObjectLiteral"),
                }
            }
            _ => panic!("Expected Let statement"),
        }

        // Test empty object literal
        let input3 = "let empty = {};";
        let ast3 = parse(input3);
        assert_eq!(ast3.len(), 1);
        match &ast3[0] {
            Stmt::Let(name, expr, _) => {
                assert_eq!(name, "empty");
                match expr {
                    Expr::Block(stmts, _) => {
                        // Empty {} is parsed as empty block for backward compatibility
                        assert_eq!(stmts.len(), 0);
                    }
                    _ => panic!("Expected Block for empty braces"),
                }
            }
            _ => panic!("Expected Let statement"),
        }
    }
}
