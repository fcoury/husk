//! Parser for TypeScript declaration files.

use crate::ast::*;
use crate::lexer::{Lexer, Token, TokenKind};

/// A parse error.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub pos: usize,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "parse error at {}: {}", self.pos, self.message)
    }
}

impl std::error::Error for ParseError {}

/// Result type for parsing operations.
pub type ParseResult<T> = Result<T, ParseError>;

/// Parse a .d.ts source string into a DtsFile.
pub fn parse(src: &str) -> ParseResult<DtsFile> {
    let mut parser = Parser::new(src);
    parser.parse_file()
}

/// Parser for TypeScript declaration files.
struct Parser<'src> {
    tokens: Vec<Token>,
    pos: usize,
    #[allow(dead_code)]
    src: &'src str,
}

impl<'src> Parser<'src> {
    fn new(src: &'src str) -> Self {
        let tokens = Lexer::new(src).tokenize();
        Self {
            tokens,
            pos: 0,
            src,
        }
    }

    fn current(&self) -> &Token {
        &self.tokens[self.pos.min(self.tokens.len() - 1)]
    }

    fn peek(&self) -> &TokenKind {
        &self.current().kind
    }

    fn peek_nth(&self, n: usize) -> &TokenKind {
        let idx = (self.pos + n).min(self.tokens.len() - 1);
        &self.tokens[idx].kind
    }

    fn advance(&mut self) -> Token {
        let token = self.current().clone();
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
        token
    }

    fn check(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(kind)
    }

    fn matches(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: &TokenKind) -> ParseResult<Token> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(ParseError {
                message: format!("expected {:?}, found {:?}", kind, self.peek()),
                pos: self.current().start,
            })
        }
    }

    fn expect_ident(&mut self) -> ParseResult<String> {
        match self.peek().clone() {
            TokenKind::Ident(name) => {
                self.advance();
                Ok(name)
            }
            // Also accept some keywords as identifiers in certain contexts
            TokenKind::Type
            | TokenKind::Module
            | TokenKind::Namespace
            | TokenKind::Default
            | TokenKind::From
            | TokenKind::As
            | TokenKind::Is
            | TokenKind::In
            | TokenKind::Out
            | TokenKind::This => {
                let name = self.keyword_as_ident();
                self.advance();
                Ok(name)
            }
            _ => Err(ParseError {
                message: format!("expected identifier, found {:?}", self.peek()),
                pos: self.current().start,
            }),
        }
    }

    fn keyword_as_ident(&self) -> String {
        match self.peek() {
            TokenKind::Type => "type".to_string(),
            TokenKind::Module => "module".to_string(),
            TokenKind::Namespace => "namespace".to_string(),
            TokenKind::Default => "default".to_string(),
            TokenKind::From => "from".to_string(),
            TokenKind::As => "as".to_string(),
            TokenKind::Is => "is".to_string(),
            TokenKind::In => "in".to_string(),
            TokenKind::Out => "out".to_string(),
            TokenKind::This => "this".to_string(),
            _ => panic!("keyword_as_ident called on non-keyword"),
        }
    }

    /// Check if current token is any keyword that can be used as a property name.
    /// Per TypeScript spec, PropertyName uses IdentifierName which allows all keywords.
    fn is_keyword_token(&self) -> bool {
        matches!(
            self.peek(),
            TokenKind::Declare
                | TokenKind::Function
                | TokenKind::Interface
                | TokenKind::Class
                | TokenKind::Type
                | TokenKind::Namespace
                | TokenKind::Module
                | TokenKind::Export
                | TokenKind::Import
                | TokenKind::Const
                | TokenKind::Let
                | TokenKind::Var
                | TokenKind::Readonly
                | TokenKind::Extends
                | TokenKind::Implements
                | TokenKind::New
                | TokenKind::Typeof
                | TokenKind::Keyof
                | TokenKind::Infer
                | TokenKind::As
                | TokenKind::Is
                | TokenKind::From
                | TokenKind::Default
                | TokenKind::Static
                | TokenKind::Public
                | TokenKind::Private
                | TokenKind::Protected
                | TokenKind::Abstract
                | TokenKind::In
                | TokenKind::Out
                | TokenKind::This
                | TokenKind::String_
                | TokenKind::Number_
                | TokenKind::Boolean_
                | TokenKind::Void_
                | TokenKind::Null_
                | TokenKind::Undefined_
                | TokenKind::Any_
                | TokenKind::Unknown_
                | TokenKind::Never_
                | TokenKind::Object_
                | TokenKind::Symbol_
                | TokenKind::BigInt_
                | TokenKind::True_
                | TokenKind::False_
        )
    }

    /// Convert any keyword token to its string representation for use as property name.
    fn token_to_ident_name(&self) -> String {
        match self.peek() {
            TokenKind::Declare => "declare".to_string(),
            TokenKind::Function => "function".to_string(),
            TokenKind::Interface => "interface".to_string(),
            TokenKind::Class => "class".to_string(),
            TokenKind::Type => "type".to_string(),
            TokenKind::Namespace => "namespace".to_string(),
            TokenKind::Module => "module".to_string(),
            TokenKind::Export => "export".to_string(),
            TokenKind::Import => "import".to_string(),
            TokenKind::Const => "const".to_string(),
            TokenKind::Let => "let".to_string(),
            TokenKind::Var => "var".to_string(),
            TokenKind::Readonly => "readonly".to_string(),
            TokenKind::Extends => "extends".to_string(),
            TokenKind::Implements => "implements".to_string(),
            TokenKind::New => "new".to_string(),
            TokenKind::Typeof => "typeof".to_string(),
            TokenKind::Keyof => "keyof".to_string(),
            TokenKind::Infer => "infer".to_string(),
            TokenKind::As => "as".to_string(),
            TokenKind::Is => "is".to_string(),
            TokenKind::From => "from".to_string(),
            TokenKind::Default => "default".to_string(),
            TokenKind::Static => "static".to_string(),
            TokenKind::Public => "public".to_string(),
            TokenKind::Private => "private".to_string(),
            TokenKind::Protected => "protected".to_string(),
            TokenKind::Abstract => "abstract".to_string(),
            TokenKind::In => "in".to_string(),
            TokenKind::Out => "out".to_string(),
            TokenKind::This => "this".to_string(),
            TokenKind::String_ => "string".to_string(),
            TokenKind::Number_ => "number".to_string(),
            TokenKind::Boolean_ => "boolean".to_string(),
            TokenKind::Void_ => "void".to_string(),
            TokenKind::Null_ => "null".to_string(),
            TokenKind::Undefined_ => "undefined".to_string(),
            TokenKind::Any_ => "any".to_string(),
            TokenKind::Unknown_ => "unknown".to_string(),
            TokenKind::Never_ => "never".to_string(),
            TokenKind::Object_ => "object".to_string(),
            TokenKind::Symbol_ => "symbol".to_string(),
            TokenKind::BigInt_ => "bigint".to_string(),
            TokenKind::True_ => "true".to_string(),
            TokenKind::False_ => "false".to_string(),
            _ => panic!("token_to_ident_name called on non-keyword: {:?}", self.peek()),
        }
    }

    /// Accept any keyword or identifier as a property name.
    /// Per TypeScript spec, PropertyName uses IdentifierName which allows keywords.
    fn expect_property_name(&mut self) -> ParseResult<String> {
        match self.peek().clone() {
            TokenKind::Ident(name) => {
                self.advance();
                Ok(name)
            }
            // Accept string literals as property names
            TokenKind::StringLiteral(s) => {
                self.advance();
                Ok(s)
            }
            // Accept ANY keyword as property name
            _ if self.is_keyword_token() => {
                let name = self.token_to_ident_name();
                self.advance();
                Ok(name)
            }
            _ => Err(ParseError {
                message: format!("expected property name, found {:?}", self.peek()),
                pos: self.current().start,
            }),
        }
    }

    /// Check if current modifier keyword is actually a property name.
    /// Returns true if it's a modifier (followed by another identifier/keyword that is NOT a punctuation).
    /// Returns false if it's a property name (followed by : or ? or ( or <).
    fn is_modifier_not_property(&self) -> bool {
        // Check next token to disambiguate
        let next = self.peek_nth(1);
        // If followed by : or ? or ( or <, the current token is a property/method name, not a modifier
        // Example: `readonly: boolean` -> readonly is property name
        // Example: `readonly name: string` -> readonly is modifier
        // Example: `readonly(flag: bool): bool` -> readonly is method name
        // Example: `readonly<T>(x: T): T` -> readonly is method name
        !matches!(next, TokenKind::Colon | TokenKind::Question | TokenKind::LParen | TokenKind::LAngle)
    }

    fn parse_file(&mut self) -> ParseResult<DtsFile> {
        let mut items = Vec::new();

        while !self.check(&TokenKind::Eof) {
            // Skip triple-slash references (store for later if needed)
            if let TokenKind::TripleSlashRef(_) = self.peek() {
                self.advance();
                continue;
            }

            if let Some(item) = self.parse_item()? {
                items.push(item);
            }
        }

        Ok(DtsFile { items })
    }

    fn parse_item(&mut self) -> ParseResult<Option<DtsItem>> {
        // Skip export/declare modifiers
        let mut has_export = false;

        loop {
            match self.peek() {
                TokenKind::Export => {
                    has_export = true;
                    self.advance();
                }
                TokenKind::Declare => {
                    self.advance();
                }
                _ => break,
            }
        }

        // Handle `export = foo;` and `export default foo;`
        if has_export {
            if self.matches(&TokenKind::Eq) {
                let name = self.expect_ident()?;
                self.matches(&TokenKind::Semicolon);
                return Ok(Some(DtsItem::Export(DtsExport::Equals(name))));
            }
            if self.matches(&TokenKind::Default) {
                let name = self.expect_ident()?;
                self.matches(&TokenKind::Semicolon);
                return Ok(Some(DtsItem::Export(DtsExport::Default(name))));
            }
        }

        match self.peek() {
            TokenKind::Function => {
                let func = self.parse_function()?;
                Ok(Some(DtsItem::Function(func)))
            }
            TokenKind::Interface => {
                let iface = self.parse_interface()?;
                Ok(Some(DtsItem::Interface(iface)))
            }
            TokenKind::Class => {
                let class = self.parse_class()?;
                Ok(Some(DtsItem::Class(class)))
            }
            TokenKind::Type => {
                let alias = self.parse_type_alias()?;
                Ok(Some(DtsItem::TypeAlias(alias)))
            }
            TokenKind::Const | TokenKind::Let | TokenKind::Var => {
                let var = self.parse_variable()?;
                Ok(Some(DtsItem::Variable(var)))
            }
            TokenKind::Namespace => {
                let ns = self.parse_namespace()?;
                Ok(Some(DtsItem::Namespace(ns)))
            }
            TokenKind::Module => {
                let module = self.parse_module()?;
                Ok(Some(DtsItem::Module(module)))
            }
            TokenKind::LBrace if has_export => {
                // export { name, name2 as alias };
                let exports = self.parse_named_exports()?;
                Ok(Some(DtsItem::Export(DtsExport::Named(exports))))
            }
            TokenKind::Import => {
                // Skip import statements
                self.skip_until_semicolon();
                Ok(None)
            }
            TokenKind::Semicolon => {
                self.advance();
                Ok(None)
            }
            TokenKind::Eof => Ok(None),
            _ => {
                // Skip unknown token
                self.advance();
                Ok(None)
            }
        }
    }

    fn skip_until_semicolon(&mut self) {
        while !self.check(&TokenKind::Semicolon) && !self.check(&TokenKind::Eof) {
            self.advance();
        }
        self.matches(&TokenKind::Semicolon);
    }

    fn parse_named_exports(&mut self) -> ParseResult<Vec<(String, Option<String>)>> {
        self.expect(&TokenKind::LBrace)?;
        let mut exports = Vec::new();

        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            let name = self.expect_ident()?;
            let alias = if self.matches(&TokenKind::As) {
                Some(self.expect_ident()?)
            } else {
                None
            };
            exports.push((name, alias));

            if !self.matches(&TokenKind::Comma) {
                break;
            }
        }

        self.expect(&TokenKind::RBrace)?;
        self.matches(&TokenKind::Semicolon);
        Ok(exports)
    }

    fn parse_function(&mut self) -> ParseResult<DtsFunction> {
        self.expect(&TokenKind::Function)?;
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;
        let params = self.parse_params()?;
        let return_type = if self.matches(&TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };
        self.matches(&TokenKind::Semicolon);

        Ok(DtsFunction {
            name,
            type_params,
            params,
            return_type,
        })
    }

    fn parse_interface(&mut self) -> ParseResult<DtsInterface> {
        self.expect(&TokenKind::Interface)?;
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;

        let mut extends = Vec::new();
        if self.matches(&TokenKind::Extends) {
            loop {
                extends.push(self.parse_type()?);
                if !self.matches(&TokenKind::Comma) {
                    break;
                }
            }
        }

        let members = self.parse_interface_body()?;

        Ok(DtsInterface {
            name,
            type_params,
            extends,
            members,
        })
    }

    fn parse_interface_body(&mut self) -> ParseResult<Vec<InterfaceMember>> {
        self.expect(&TokenKind::LBrace)?;
        let mut members = Vec::new();

        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            if let Some(member) = self.parse_interface_member()? {
                members.push(member);
            }
        }

        self.expect(&TokenKind::RBrace)?;
        Ok(members)
    }

    fn parse_interface_member(&mut self) -> ParseResult<Option<InterfaceMember>> {
        // Handle readonly modifier with disambiguation
        // readonly: boolean (property named "readonly") vs readonly name: string (modifier)
        let readonly = if self.check(&TokenKind::Readonly) && self.is_modifier_not_property() {
            self.advance();
            true
        } else {
            false
        };

        // Check for index signature: [key: Type]: Type
        if self.check(&TokenKind::LBracket) {
            let sig = self.parse_index_signature(readonly)?;
            return Ok(Some(InterfaceMember::IndexSignature(sig)));
        }

        // Check for call signature: (params): Type
        if self.check(&TokenKind::LParen) || self.check(&TokenKind::LAngle) {
            let type_params = self.parse_type_params()?;
            let params = self.parse_params()?;
            let return_type = if self.matches(&TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.matches(&TokenKind::Semicolon);
            self.matches(&TokenKind::Comma);
            return Ok(Some(InterfaceMember::CallSignature(CallSignature {
                type_params,
                params,
                return_type,
            })));
        }

        // Check for construct signature: new (params): Type
        // But only if "new" is followed by ( - otherwise it might be a property named "new"
        if self.check(&TokenKind::New) && self.is_modifier_not_property() {
            self.advance();
            let type_params = self.parse_type_params()?;
            let params = self.parse_params()?;
            let return_type = if self.matches(&TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.matches(&TokenKind::Semicolon);
            self.matches(&TokenKind::Comma);
            return Ok(Some(InterfaceMember::ConstructSignature(
                ConstructSignature {
                    type_params,
                    params,
                    return_type,
                },
            )));
        }

        // Property or method - use expect_property_name to accept keywords
        let name = match self.expect_property_name() {
            Ok(n) => n,
            Err(_) => {
                // Skip unknown token
                self.advance();
                return Ok(None);
            }
        };

        let optional = self.matches(&TokenKind::Question);

        // Check if it's a method
        if self.check(&TokenKind::LParen) || self.check(&TokenKind::LAngle) {
            let type_params = self.parse_type_params()?;
            let params = self.parse_params()?;
            let return_type = if self.matches(&TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.matches(&TokenKind::Semicolon);
            self.matches(&TokenKind::Comma);
            return Ok(Some(InterfaceMember::Method(MethodMember {
                name,
                type_params,
                params,
                return_type,
                optional,
            })));
        }

        // It's a property
        self.expect(&TokenKind::Colon)?;
        let ty = self.parse_type()?;
        self.matches(&TokenKind::Semicolon);
        self.matches(&TokenKind::Comma);

        Ok(Some(InterfaceMember::Property(PropertyMember {
            name,
            ty,
            optional,
            readonly,
        })))
    }

    fn parse_index_signature(&mut self, readonly: bool) -> ParseResult<IndexSignature> {
        self.expect(&TokenKind::LBracket)?;
        let key_name = self.expect_ident()?;
        self.expect(&TokenKind::Colon)?;
        let key_type = self.parse_type()?;
        self.expect(&TokenKind::RBracket)?;
        self.expect(&TokenKind::Colon)?;
        let value_type = self.parse_type()?;
        self.matches(&TokenKind::Semicolon);

        Ok(IndexSignature {
            key_name,
            key_type,
            value_type,
            readonly,
        })
    }

    fn parse_class(&mut self) -> ParseResult<DtsClass> {
        self.expect(&TokenKind::Class)?;
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;

        let extends = if self.matches(&TokenKind::Extends) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let mut implements = Vec::new();
        if self.matches(&TokenKind::Implements) {
            loop {
                implements.push(self.parse_type()?);
                if !self.matches(&TokenKind::Comma) {
                    break;
                }
            }
        }

        let members = self.parse_class_body()?;

        Ok(DtsClass {
            name,
            type_params,
            extends,
            implements,
            members,
        })
    }

    fn parse_class_body(&mut self) -> ParseResult<Vec<ClassMember>> {
        self.expect(&TokenKind::LBrace)?;
        let mut members = Vec::new();

        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            if let Some(member) = self.parse_class_member()? {
                members.push(member);
            }
        }

        self.expect(&TokenKind::RBrace)?;
        Ok(members)
    }

    fn parse_class_member(&mut self) -> ParseResult<Option<ClassMember>> {
        // Parse modifiers with disambiguation
        // e.g., `static: boolean` (property named "static") vs `static name: string` (modifier)
        let mut visibility = Visibility::Public;
        let mut is_static = false;
        let mut readonly = false;

        loop {
            match self.peek() {
                TokenKind::Public if self.is_modifier_not_property() => {
                    visibility = Visibility::Public;
                    self.advance();
                }
                TokenKind::Private if self.is_modifier_not_property() => {
                    visibility = Visibility::Private;
                    self.advance();
                }
                TokenKind::Protected if self.is_modifier_not_property() => {
                    visibility = Visibility::Protected;
                    self.advance();
                }
                TokenKind::Static if self.is_modifier_not_property() => {
                    is_static = true;
                    self.advance();
                }
                TokenKind::Readonly if self.is_modifier_not_property() => {
                    readonly = true;
                    self.advance();
                }
                TokenKind::Abstract if self.is_modifier_not_property() => {
                    // Skip abstract modifier
                    self.advance();
                }
                _ => break,
            }
        }

        // Constructor - but only if "new" is followed by ( or <, otherwise it's a property
        if self.check(&TokenKind::New) && self.is_modifier_not_property() {
            self.advance();
            // constructor(params)
            let type_params = self.parse_type_params()?;
            let params = self.parse_params()?;
            let return_type = if self.matches(&TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.matches(&TokenKind::Semicolon);
            return Ok(Some(ClassMember::Constructor(ConstructSignature {
                type_params,
                params,
                return_type,
            })));
        }

        // Constructor with just parens (no "new")
        if self.check(&TokenKind::LParen) {
            let type_params = self.parse_type_params()?;
            let params = self.parse_params()?;
            let return_type = if self.matches(&TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.matches(&TokenKind::Semicolon);
            return Ok(Some(ClassMember::Constructor(ConstructSignature {
                type_params,
                params,
                return_type,
            })));
        }

        // Index signature
        if self.check(&TokenKind::LBracket) {
            let sig = self.parse_index_signature(readonly)?;
            return Ok(Some(ClassMember::IndexSignature(sig)));
        }

        // Property or method - use expect_property_name to accept keywords
        let name = match self.expect_property_name() {
            Ok(n) => n,
            Err(_) => {
                self.advance();
                return Ok(None);
            }
        };

        let optional = self.matches(&TokenKind::Question);

        // Check if it's a method
        if self.check(&TokenKind::LParen) || self.check(&TokenKind::LAngle) {
            let type_params = self.parse_type_params()?;
            let params = self.parse_params()?;
            let return_type = if self.matches(&TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.matches(&TokenKind::Semicolon);
            return Ok(Some(ClassMember::Method(ClassMethod {
                name,
                type_params,
                params,
                return_type,
                is_static,
                visibility,
            })));
        }

        // Property
        let ty = if self.matches(&TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };
        self.matches(&TokenKind::Semicolon);

        Ok(Some(ClassMember::Property(ClassProperty {
            name,
            ty,
            optional,
            readonly,
            is_static,
            visibility,
        })))
    }

    fn parse_type_alias(&mut self) -> ParseResult<DtsTypeAlias> {
        self.expect(&TokenKind::Type)?;
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;
        self.expect(&TokenKind::Eq)?;
        let ty = self.parse_type()?;
        self.matches(&TokenKind::Semicolon);

        Ok(DtsTypeAlias {
            name,
            type_params,
            ty,
        })
    }

    fn parse_variable(&mut self) -> ParseResult<DtsVariable> {
        let is_const = self.matches(&TokenKind::Const);
        if !is_const {
            self.matches(&TokenKind::Let);
            self.matches(&TokenKind::Var);
        }

        // Variable name can be a keyword (e.g., `var static: Type`)
        let name = self.expect_property_name()?;
        self.expect(&TokenKind::Colon)?;
        let ty = self.parse_type()?;
        self.matches(&TokenKind::Semicolon);

        Ok(DtsVariable { name, ty, is_const })
    }

    fn parse_namespace(&mut self) -> ParseResult<DtsNamespace> {
        self.expect(&TokenKind::Namespace)?;
        let name = self.expect_ident()?;
        self.expect(&TokenKind::LBrace)?;

        let mut items = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            if let Some(item) = self.parse_item()? {
                items.push(item);
            }
        }

        self.expect(&TokenKind::RBrace)?;
        Ok(DtsNamespace { name, items })
    }

    fn parse_module(&mut self) -> ParseResult<DtsModule> {
        self.expect(&TokenKind::Module)?;

        let name = match self.peek().clone() {
            TokenKind::StringLiteral(s) => {
                self.advance();
                s
            }
            TokenKind::Ident(s) => {
                self.advance();
                s
            }
            _ => {
                return Err(ParseError {
                    message: "expected module name".to_string(),
                    pos: self.current().start,
                })
            }
        };

        self.expect(&TokenKind::LBrace)?;

        let mut items = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            if let Some(item) = self.parse_item()? {
                items.push(item);
            }
        }

        self.expect(&TokenKind::RBrace)?;
        Ok(DtsModule { name, items })
    }

    fn parse_type_params(&mut self) -> ParseResult<Vec<TypeParam>> {
        if !self.matches(&TokenKind::LAngle) {
            return Ok(Vec::new());
        }

        let mut params = Vec::new();
        loop {
            // Skip variance modifiers (in/out)
            self.matches(&TokenKind::In);
            self.matches(&TokenKind::Out);

            let name = self.expect_ident()?;

            let constraint = if self.matches(&TokenKind::Extends) {
                Some(self.parse_type()?)
            } else {
                None
            };

            let default = if self.matches(&TokenKind::Eq) {
                Some(self.parse_type()?)
            } else {
                None
            };

            params.push(TypeParam {
                name,
                constraint,
                default,
            });

            if !self.matches(&TokenKind::Comma) {
                break;
            }
        }

        self.expect(&TokenKind::RAngle)?;
        Ok(params)
    }

    fn parse_params(&mut self) -> ParseResult<Vec<Param>> {
        self.expect(&TokenKind::LParen)?;

        let mut params = Vec::new();
        while !self.check(&TokenKind::RParen) && !self.check(&TokenKind::Eof) {
            // Check for `this` parameter
            if self.check(&TokenKind::This) {
                self.advance();
                self.expect(&TokenKind::Colon)?;
                let _this_type = self.parse_type()?;
                // Skip this parameter (handled separately in FunctionType)
                if !self.matches(&TokenKind::Comma) {
                    break;
                }
                continue;
            }

            let rest = self.matches(&TokenKind::DotDotDot);

            let name = self.expect_ident()?;
            let optional = self.matches(&TokenKind::Question);

            let ty = if self.matches(&TokenKind::Colon) {
                self.parse_type()?
            } else {
                DtsType::Primitive(Primitive::Any)
            };

            params.push(Param {
                name,
                ty,
                optional,
                rest,
            });

            if !self.matches(&TokenKind::Comma) {
                break;
            }
        }

        self.expect(&TokenKind::RParen)?;
        Ok(params)
    }

    fn parse_type(&mut self) -> ParseResult<DtsType> {
        self.parse_union_type()
    }

    fn parse_union_type(&mut self) -> ParseResult<DtsType> {
        // Handle leading pipe
        self.matches(&TokenKind::Pipe);

        let mut types = vec![self.parse_intersection_type()?];

        while self.matches(&TokenKind::Pipe) {
            types.push(self.parse_intersection_type()?);
        }

        if types.len() == 1 {
            Ok(types.remove(0))
        } else {
            Ok(DtsType::Union(types))
        }
    }

    fn parse_intersection_type(&mut self) -> ParseResult<DtsType> {
        let mut types = vec![self.parse_postfix_type()?];

        while self.matches(&TokenKind::Amp) {
            types.push(self.parse_postfix_type()?);
        }

        if types.len() == 1 {
            Ok(types.remove(0))
        } else {
            Ok(DtsType::Intersection(types))
        }
    }

    fn parse_postfix_type(&mut self) -> ParseResult<DtsType> {
        let mut ty = self.parse_primary_type()?;

        loop {
            // Array suffix: T[]
            if self.check(&TokenKind::LBracket) && self.peek_nth(1) == &TokenKind::RBracket {
                self.advance(); // [
                self.advance(); // ]
                ty = DtsType::Array(Box::new(ty));
                continue;
            }

            // Index access: T[K]
            if self.check(&TokenKind::LBracket) {
                self.advance();
                let index = self.parse_type()?;
                self.expect(&TokenKind::RBracket)?;
                ty = DtsType::IndexAccess {
                    object: Box::new(ty),
                    index: Box::new(index),
                };
                continue;
            }

            break;
        }

        Ok(ty)
    }

    fn parse_primary_type(&mut self) -> ParseResult<DtsType> {
        match self.peek().clone() {
            // Primitives
            TokenKind::String_ => {
                self.advance();
                Ok(DtsType::Primitive(Primitive::String))
            }
            TokenKind::Number_ => {
                self.advance();
                Ok(DtsType::Primitive(Primitive::Number))
            }
            TokenKind::Boolean_ => {
                self.advance();
                Ok(DtsType::Primitive(Primitive::Boolean))
            }
            TokenKind::Void_ => {
                self.advance();
                Ok(DtsType::Primitive(Primitive::Void))
            }
            TokenKind::Null_ => {
                self.advance();
                Ok(DtsType::Primitive(Primitive::Null))
            }
            TokenKind::Undefined_ => {
                self.advance();
                Ok(DtsType::Primitive(Primitive::Undefined))
            }
            TokenKind::Any_ => {
                self.advance();
                Ok(DtsType::Primitive(Primitive::Any))
            }
            TokenKind::Unknown_ => {
                self.advance();
                Ok(DtsType::Primitive(Primitive::Unknown))
            }
            TokenKind::Never_ => {
                self.advance();
                Ok(DtsType::Primitive(Primitive::Never))
            }
            TokenKind::Object_ => {
                self.advance();
                Ok(DtsType::Primitive(Primitive::Object))
            }
            TokenKind::Symbol_ => {
                self.advance();
                Ok(DtsType::Primitive(Primitive::Symbol))
            }
            TokenKind::BigInt_ => {
                self.advance();
                Ok(DtsType::Primitive(Primitive::BigInt))
            }
            TokenKind::True_ => {
                self.advance();
                Ok(DtsType::BooleanLiteral(true))
            }
            TokenKind::False_ => {
                self.advance();
                Ok(DtsType::BooleanLiteral(false))
            }

            // String literal type
            TokenKind::StringLiteral(s) => {
                let s = s.clone();
                self.advance();
                Ok(DtsType::StringLiteral(s))
            }

            // Number literal type
            TokenKind::NumberLiteral(n) => {
                let n = n.clone();
                self.advance();
                Ok(DtsType::NumberLiteral(n))
            }

            // typeof
            TokenKind::Typeof => {
                self.advance();
                let name = self.expect_ident()?;
                Ok(DtsType::TypeOf(name))
            }

            // keyof
            TokenKind::Keyof => {
                self.advance();
                let ty = self.parse_primary_type()?;
                Ok(DtsType::KeyOf(Box::new(ty)))
            }

            // infer
            TokenKind::Infer => {
                self.advance();
                let name = self.expect_ident()?;
                Ok(DtsType::Infer(name))
            }

            // this
            TokenKind::This => {
                self.advance();
                Ok(DtsType::This)
            }

            // Parenthesized type or function type
            TokenKind::LParen => self.parse_paren_or_function_type(),

            // Object literal type
            TokenKind::LBrace => self.parse_object_type(),

            // Tuple type
            TokenKind::LBracket => self.parse_tuple_type(),

            // Named type (including generics)
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                self.parse_named_type(name)
            }

            // Function type with type params: <T>(x: T) => T
            TokenKind::LAngle => self.parse_generic_function_type(),

            // Conditional type prefix handled after primary parsing
            _ => Err(ParseError {
                message: format!("unexpected token in type position: {:?}", self.peek()),
                pos: self.current().start,
            }),
        }
    }

    fn parse_named_type(&mut self, name: String) -> ParseResult<DtsType> {
        let type_args = if self.check(&TokenKind::LAngle) {
            self.parse_type_args()?
        } else {
            Vec::new()
        };

        // Check for conditional type: T extends U ? X : Y
        if self.matches(&TokenKind::Extends) {
            let extends = self.parse_primary_type()?;
            self.expect(&TokenKind::Question)?;
            let true_type = self.parse_type()?;
            self.expect(&TokenKind::Colon)?;
            let false_type = self.parse_type()?;

            return Ok(DtsType::Conditional {
                check: Box::new(DtsType::Named { name, type_args }),
                extends: Box::new(extends),
                true_type: Box::new(true_type),
                false_type: Box::new(false_type),
            });
        }

        Ok(DtsType::Named { name, type_args })
    }

    fn parse_type_args(&mut self) -> ParseResult<Vec<DtsType>> {
        self.expect(&TokenKind::LAngle)?;
        let mut args = Vec::new();

        loop {
            args.push(self.parse_type()?);
            if !self.matches(&TokenKind::Comma) {
                break;
            }
        }

        self.expect(&TokenKind::RAngle)?;
        Ok(args)
    }

    fn parse_paren_or_function_type(&mut self) -> ParseResult<DtsType> {
        // Look ahead to determine if this is a function type or parenthesized type
        // Function type: (x: T) => R or () => R
        // Parenthesized: (T)

        let start_pos = self.pos;
        self.expect(&TokenKind::LParen)?;

        // Empty parens => function type
        if self.check(&TokenKind::RParen) {
            self.advance();
            if self.matches(&TokenKind::Arrow) {
                let return_type = self.parse_type()?;
                return Ok(DtsType::Function(Box::new(FunctionType {
                    type_params: Vec::new(),
                    params: Vec::new(),
                    return_type: Box::new(return_type),
                    this_param: None,
                })));
            }
            // Empty parens without arrow - error
            return Err(ParseError {
                message: "expected => after ()".to_string(),
                pos: self.current().start,
            });
        }

        // Check if first token looks like a parameter name followed by : or ?:
        let is_function = match self.peek() {
            TokenKind::DotDotDot => true,
            TokenKind::This => true,
            TokenKind::Ident(_) => {
                matches!(self.peek_nth(1), TokenKind::Colon | TokenKind::Question)
            }
            _ => false,
        };

        if is_function {
            // Parse as function type
            // Reset and parse params properly
            self.pos = start_pos;
            let params = self.parse_params()?;
            self.expect(&TokenKind::Arrow)?;
            let return_type = self.parse_type()?;

            return Ok(DtsType::Function(Box::new(FunctionType {
                type_params: Vec::new(),
                params,
                return_type: Box::new(return_type),
                this_param: None,
            })));
        }

        // Parenthesized type
        let inner = self.parse_type()?;
        self.expect(&TokenKind::RParen)?;

        // Check if this is actually a function type: (T) => R
        if self.matches(&TokenKind::Arrow) {
            // Single-param function type without name
            let return_type = self.parse_type()?;
            return Ok(DtsType::Function(Box::new(FunctionType {
                type_params: Vec::new(),
                params: vec![Param {
                    name: "_".to_string(),
                    ty: inner,
                    optional: false,
                    rest: false,
                }],
                return_type: Box::new(return_type),
                this_param: None,
            })));
        }

        Ok(DtsType::Parenthesized(Box::new(inner)))
    }

    fn parse_generic_function_type(&mut self) -> ParseResult<DtsType> {
        let type_params = self.parse_type_params()?;
        let params = self.parse_params()?;
        self.expect(&TokenKind::Arrow)?;
        let return_type = self.parse_type()?;

        Ok(DtsType::Function(Box::new(FunctionType {
            type_params,
            params,
            return_type: Box::new(return_type),
            this_param: None,
        })))
    }

    fn parse_object_type(&mut self) -> ParseResult<DtsType> {
        self.expect(&TokenKind::LBrace)?;

        // Check for mapped type: { [K in keyof T]: T[K] }
        if self.check(&TokenKind::LBracket) {
            // Could be index signature or mapped type
            let next = self.peek_nth(1);
            if let TokenKind::Ident(_) = next {
                let third = self.peek_nth(2);
                if matches!(third, TokenKind::In) {
                    return self.parse_mapped_type();
                }
            }
        }

        let mut members = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            if let Some(member) = self.parse_object_member()? {
                members.push(member);
            }
        }

        self.expect(&TokenKind::RBrace)?;
        Ok(DtsType::Object(members))
    }

    fn parse_object_member(&mut self) -> ParseResult<Option<ObjectMember>> {
        // Handle readonly modifier with disambiguation
        let readonly = if self.check(&TokenKind::Readonly) && self.is_modifier_not_property() {
            self.advance();
            true
        } else {
            false
        };

        // Index signature
        if self.check(&TokenKind::LBracket) {
            let sig = self.parse_index_signature(readonly)?;
            return Ok(Some(ObjectMember::IndexSignature(sig)));
        }

        // Call signature
        if self.check(&TokenKind::LParen) || self.check(&TokenKind::LAngle) {
            let type_params = self.parse_type_params()?;
            let params = self.parse_params()?;
            let return_type = if self.matches(&TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.matches(&TokenKind::Semicolon);
            self.matches(&TokenKind::Comma);
            return Ok(Some(ObjectMember::CallSignature(CallSignature {
                type_params,
                params,
                return_type,
            })));
        }

        // Construct signature - but only if "new" is followed by (, not by : or ?
        if self.check(&TokenKind::New) && self.is_modifier_not_property() {
            self.advance();
            let type_params = self.parse_type_params()?;
            let params = self.parse_params()?;
            let return_type = if self.matches(&TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.matches(&TokenKind::Semicolon);
            self.matches(&TokenKind::Comma);
            return Ok(Some(ObjectMember::ConstructSignature(ConstructSignature {
                type_params,
                params,
                return_type,
            })));
        }

        // Property or method - use expect_property_name to accept keywords
        let name = match self.expect_property_name() {
            Ok(n) => n,
            Err(_) => {
                self.advance();
                return Ok(None);
            }
        };

        let optional = self.matches(&TokenKind::Question);

        // Check if method
        if self.check(&TokenKind::LParen) || self.check(&TokenKind::LAngle) {
            let type_params = self.parse_type_params()?;
            let params = self.parse_params()?;
            let return_type = if self.matches(&TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.matches(&TokenKind::Semicolon);
            self.matches(&TokenKind::Comma);
            return Ok(Some(ObjectMember::Method {
                name,
                type_params,
                params,
                return_type,
                optional,
            }));
        }

        // Property
        self.expect(&TokenKind::Colon)?;
        let ty = self.parse_type()?;
        self.matches(&TokenKind::Semicolon);
        self.matches(&TokenKind::Comma);

        Ok(Some(ObjectMember::Property {
            name,
            ty,
            optional,
            readonly,
        }))
    }

    fn parse_mapped_type(&mut self) -> ParseResult<DtsType> {
        // We've already seen { and need to parse [K in keyof T]: T[K]

        // Optional readonly modifier
        let readonly = if self.matches(&TokenKind::Readonly) {
            Some(MappedModifier::Preserve)
        } else if self.matches(&TokenKind::Plus) {
            self.expect(&TokenKind::Readonly)?;
            Some(MappedModifier::Add)
        } else if self.matches(&TokenKind::Minus) {
            self.expect(&TokenKind::Readonly)?;
            Some(MappedModifier::Remove)
        } else {
            None
        };

        self.expect(&TokenKind::LBracket)?;
        let key_name = self.expect_ident()?;
        self.expect(&TokenKind::In)?;
        let key_constraint = self.parse_type()?;
        self.expect(&TokenKind::RBracket)?;

        // Optional ?/+?/-? modifier
        let optional = if self.matches(&TokenKind::Question) {
            Some(MappedModifier::Preserve)
        } else if self.matches(&TokenKind::Plus) {
            self.expect(&TokenKind::Question)?;
            Some(MappedModifier::Add)
        } else if self.matches(&TokenKind::Minus) {
            self.expect(&TokenKind::Question)?;
            Some(MappedModifier::Remove)
        } else {
            None
        };

        self.expect(&TokenKind::Colon)?;
        let value_type = self.parse_type()?;
        self.matches(&TokenKind::Semicolon);
        self.expect(&TokenKind::RBrace)?;

        Ok(DtsType::Mapped {
            key_name,
            key_constraint: Box::new(key_constraint),
            value_type: Box::new(value_type),
            readonly,
            optional,
        })
    }

    fn parse_tuple_type(&mut self) -> ParseResult<DtsType> {
        self.expect(&TokenKind::LBracket)?;

        let mut elements = Vec::new();
        while !self.check(&TokenKind::RBracket) && !self.check(&TokenKind::Eof) {
            let rest = self.matches(&TokenKind::DotDotDot);

            // Check for named tuple element: name: Type or name?: Type
            let (name, optional, ty) = if let TokenKind::Ident(_) = self.peek() {
                if matches!(self.peek_nth(1), TokenKind::Colon | TokenKind::Question) {
                    let n = self.expect_ident()?;
                    let opt = self.matches(&TokenKind::Question);
                    self.expect(&TokenKind::Colon)?;
                    let t = self.parse_type()?;
                    (Some(n), opt, t)
                } else {
                    (None, false, self.parse_type()?)
                }
            } else {
                (None, false, self.parse_type()?)
            };

            elements.push(TupleElement {
                ty,
                name,
                optional,
                rest,
            });

            if !self.matches(&TokenKind::Comma) {
                break;
            }
        }

        self.expect(&TokenKind::RBracket)?;
        Ok(DtsType::Tuple(elements))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_function() {
        let src = "declare function add(a: number, b: number): number;";
        let file = parse(src).unwrap();

        assert_eq!(file.items.len(), 1);
        if let DtsItem::Function(f) = &file.items[0] {
            assert_eq!(f.name, "add");
            assert_eq!(f.params.len(), 2);
            assert_eq!(f.params[0].name, "a");
            assert!(matches!(
                f.return_type,
                Some(DtsType::Primitive(Primitive::Number))
            ));
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_generic_function() {
        let src = "declare function identity<T>(x: T): T;";
        let file = parse(src).unwrap();

        if let DtsItem::Function(f) = &file.items[0] {
            assert_eq!(f.name, "identity");
            assert_eq!(f.type_params.len(), 1);
            assert_eq!(f.type_params[0].name, "T");
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_interface() {
        let src = r#"
            interface Person {
                name: string;
                age?: number;
                greet(msg: string): void;
            }
        "#;
        let file = parse(src).unwrap();

        if let DtsItem::Interface(i) = &file.items[0] {
            assert_eq!(i.name, "Person");
            assert_eq!(i.members.len(), 3);
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn test_parse_class() {
        let src = r#"
            declare class Server {
                constructor(options?: ServerOptions);
                listen(port: number): void;
                private readonly id: string;
            }
        "#;
        let file = parse(src).unwrap();

        if let DtsItem::Class(c) = &file.items[0] {
            assert_eq!(c.name, "Server");
            assert!(c.members.len() >= 2);
        } else {
            panic!("expected class");
        }
    }

    #[test]
    fn test_parse_type_alias() {
        let src = "type Callback<T> = (value: T) => void;";
        let file = parse(src).unwrap();

        if let DtsItem::TypeAlias(t) = &file.items[0] {
            assert_eq!(t.name, "Callback");
            assert_eq!(t.type_params.len(), 1);
        } else {
            panic!("expected type alias");
        }
    }

    #[test]
    fn test_parse_union_type() {
        let src = "type Result = string | number | null;";
        let file = parse(src).unwrap();

        if let DtsItem::TypeAlias(t) = &file.items[0] {
            if let DtsType::Union(types) = &t.ty {
                assert_eq!(types.len(), 3);
            } else {
                panic!("expected union type");
            }
        } else {
            panic!("expected type alias");
        }
    }

    #[test]
    fn test_parse_array_type() {
        let src = "type Names = string[];";
        let file = parse(src).unwrap();

        if let DtsItem::TypeAlias(t) = &file.items[0] {
            if let DtsType::Array(inner) = &t.ty {
                assert!(matches!(inner.as_ref(), DtsType::Primitive(Primitive::String)));
            } else {
                panic!("expected array type");
            }
        } else {
            panic!("expected type alias");
        }
    }

    #[test]
    fn test_parse_function_type() {
        let src = "type Handler = (req: Request, res: Response) => void;";
        let file = parse(src).unwrap();

        if let DtsItem::TypeAlias(t) = &file.items[0] {
            if let DtsType::Function(f) = &t.ty {
                assert_eq!(f.params.len(), 2);
            } else {
                panic!("expected function type");
            }
        } else {
            panic!("expected type alias");
        }
    }

    #[test]
    fn test_parse_optional_params() {
        let src = "declare function foo(a: string, b?: number): void;";
        let file = parse(src).unwrap();

        if let DtsItem::Function(f) = &file.items[0] {
            assert!(!f.params[0].optional);
            assert!(f.params[1].optional);
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_rest_params() {
        let src = "declare function foo(...args: string[]): void;";
        let file = parse(src).unwrap();

        if let DtsItem::Function(f) = &file.items[0] {
            assert!(f.params[0].rest);
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_namespace() {
        let src = r#"
            declare namespace Express {
                interface Request {}
                interface Response {}
            }
        "#;
        let file = parse(src).unwrap();

        if let DtsItem::Namespace(ns) = &file.items[0] {
            assert_eq!(ns.name, "Express");
            assert_eq!(ns.items.len(), 2);
        } else {
            panic!("expected namespace");
        }
    }

    #[test]
    fn test_parse_export_equals() {
        let src = "export = express;";
        let file = parse(src).unwrap();

        if let DtsItem::Export(DtsExport::Equals(name)) = &file.items[0] {
            assert_eq!(name, "express");
        } else {
            panic!("expected export =");
        }
    }
}
