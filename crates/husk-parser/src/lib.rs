//! Parser: consume tokens and produce an AST.
//!
//! This is a hand-written recursive-descent parser for the MVP syntax.

use husk_ast::{
    AssignOp, Attribute, BinaryOp, Block, ClosureParam, EnumVariant, EnumVariantFields, Expr,
    ExprKind, ExternProperty, File, FormatPlaceholder, FormatSegment, FormatSpec, FormatString,
    Ident, ImplBlock, ImplItem, ImplItemKind, ImplMethod, Item, ItemKind, Literal, LiteralKind,
    MatchArm, Param, Pattern, PatternKind, SelfReceiver, Span, Stmt, StmtKind, StructField,
    TraitDef, TraitItem, TraitItemKind, TraitMethod, TypeExpr, TypeExprKind, TypeParam,
};
use husk_lexer::{Keyword, Lexer, Token, TokenKind};

fn debug_log(msg: &str) {
    match std::env::var("HUSKC_DEBUG") {
        Ok(val) if val == "1" || val.eq_ignore_ascii_case("true") => {
            eprintln!("{msg}");
        }
        _ => {}
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParseResult {
    pub file: Option<File>,
    pub errors: Vec<ParseError>,
}

/// Parse a source string into an AST `File` and a list of parse errors.
pub fn parse_str(source: &str) -> ParseResult {
    debug_log("[huskc-parser] lexing");
    let tokens: Vec<Token> = Lexer::new(source).collect();
    debug_log(&format!("[huskc-parser] lexed {} tokens", tokens.len()));
    let mut parser = Parser::new(tokens);
    let file = parser.parse_file();
    ParseResult {
        file: Some(file),
        errors: parser.errors,
    }
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    pub errors: Vec<ParseError>,
    /// When false, struct literal expressions like `Name { ... }` are not allowed.
    /// This is set to false when parsing contexts where `{` has special meaning
    /// (e.g., match/if/while scrutinee).
    allow_struct_expr: bool,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            errors: Vec::new(),
            allow_struct_expr: true,
        }
    }

    /// Parse an expression in a context where struct literals are not allowed
    /// (e.g., match/if/while scrutinee where `{` starts a block).
    fn parse_expr_no_struct(&mut self) -> Option<Expr> {
        let saved = self.allow_struct_expr;
        self.allow_struct_expr = false;
        let result = self.parse_expr();
        self.allow_struct_expr = saved;
        result
    }

    fn is_at_end(&self) -> bool {
        matches!(self.current().kind, TokenKind::Eof)
    }

    fn current(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .unwrap_or(self.tokens.last().expect("tokens not empty"))
    }

    fn previous(&self) -> &Token {
        if self.pos == 0 {
            self.current()
        } else {
            &self.tokens[self.pos - 1]
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.pos += 1;
        }
        self.previous()
    }

    fn matches_keyword(&mut self, kw: Keyword) -> bool {
        match self.current().kind {
            TokenKind::Keyword(k) if k == kw => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn matches_token(&mut self, kind: &TokenKind) -> bool {
        if std::mem::discriminant(&self.current().kind) == std::mem::discriminant(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn ast_span_from(&self, span: &husk_lexer::Span) -> Span {
        Span {
            range: span.range.clone(),
        }
    }

    fn error_at_token(&mut self, token: &Token, message: impl Into<String>) {
        let span = self.ast_span_from(&token.span);
        self.errors.push(ParseError {
            message: message.into(),
            span,
        });
    }

    fn error_here(&mut self, message: impl Into<String>) {
        let tok = self.current().clone();
        self.error_at_token(&tok, message);
    }

    /// Synchronize after an error by skipping tokens until we reach a likely
    /// item boundary.
    fn synchronize_item(&mut self) {
        while !self.is_at_end() {
            match &self.current().kind {
                TokenKind::Semicolon => {
                    self.advance();
                    return;
                }
                TokenKind::Keyword(
                    Keyword::Fn
                    | Keyword::Struct
                    | Keyword::Enum
                    | Keyword::Type
                    | Keyword::Extern
                    | Keyword::Use
                    | Keyword::Pub
                    | Keyword::Trait
                    | Keyword::Impl,
                ) => {
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    fn parse_file(&mut self) -> File {
        let mut items = Vec::new();
        while !self.is_at_end() {
            if let Some(item) = self.parse_item() {
                items.push(item);
            } else {
                self.synchronize_item();
            }
        }
        File { items }
    }

    fn parse_item(&mut self) -> Option<Item> {
        let mut visibility = husk_ast::Visibility::Private;
        if self.matches_keyword(Keyword::Pub) {
            visibility = husk_ast::Visibility::Public;
        }

        let mut item = match self.current().kind {
            TokenKind::Keyword(Keyword::Use) => self.parse_use_item(),
            TokenKind::Keyword(Keyword::Fn) => self.parse_fn_item(),
            TokenKind::Keyword(Keyword::Struct) => self.parse_struct_item(),
            TokenKind::Keyword(Keyword::Enum) => self.parse_enum_item(),
            TokenKind::Keyword(Keyword::Type) => self.parse_type_alias_item(),
            TokenKind::Keyword(Keyword::Extern) => self.parse_extern_block_item(),
            TokenKind::Keyword(Keyword::Trait) => self.parse_trait_item(),
            TokenKind::Keyword(Keyword::Impl) => self.parse_impl_item(),
            TokenKind::Eof => None,
            _ => {
                self.error_here(
                    "expected item (`fn`, `struct`, `enum`, `type`, `extern`, `use`, `trait`, or `impl`)",
                );
                None
            }
        }?;

        item.visibility = visibility;
        Some(item)
    }

    fn parse_fn_item(&mut self) -> Option<Item> {
        let fn_tok = self.advance().clone(); // consume `fn`
        let name = self.parse_ident("expected function name after `fn`")?;

        let type_params = self.parse_type_params();

        // Parameter list
        if !self.matches_token(&TokenKind::LParen) {
            self.error_here("expected `(` after function name");
            return None;
        }
        let params = self.parse_param_list();

        // Optional return type
        let ret_type = if self.matches_token(&TokenKind::Arrow) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        let body_block = self.parse_block()?;
        let span = Span {
            range: fn_tok.span.range.start..body_block.span.range.end,
        };

        Some(Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::Fn {
                name,
                type_params,
                params,
                ret_type,
                body: body_block.stmts,
            },
            span,
        })
    }

    fn parse_struct_item(&mut self) -> Option<Item> {
        let struct_tok = self.advance().clone(); // consume `struct`
        let name = self.parse_ident("expected struct name")?;
        let type_params = self.parse_type_params();

        if !self.matches_token(&TokenKind::LBrace) {
            self.error_here("expected `{` after struct name");
            return None;
        }

        let mut fields = Vec::new();
        while !self.is_at_end() && !self.matches_token(&TokenKind::RBrace) {
            let field_name = self.parse_ident("expected field name in struct")?;
            if !self.matches_token(&TokenKind::Colon) {
                self.error_here("expected `:` after field name");
                return None;
            }
            let ty = self.parse_type_expr()?;
            fields.push(StructField {
                name: field_name,
                ty,
            });

            // Optional trailing comma
            let _ = self.matches_token(&TokenKind::Comma);
        }

        let end = self.previous().span.range.end;
        let span = Span {
            range: struct_tok.span.range.start..end,
        };

        Some(Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::Struct {
                name,
                type_params,
                fields,
            },
            span,
        })
    }

    fn parse_enum_item(&mut self) -> Option<Item> {
        let enum_tok = self.advance().clone(); // consume `enum`
        let name = self.parse_ident("expected enum name")?;
        let type_params = self.parse_type_params();

        if !self.matches_token(&TokenKind::LBrace) {
            self.error_here("expected `{` after enum name");
            return None;
        }

        let mut variants = Vec::new();
        while !self.is_at_end() && !self.matches_token(&TokenKind::RBrace) {
            let variant_name = self.parse_ident("expected enum variant name")?;

            let fields = if self.matches_token(&TokenKind::LParen) {
                // Tuple-like variant
                let mut tys = Vec::new();
                if !self.matches_token(&TokenKind::RParen) {
                    loop {
                        let ty = self.parse_type_expr()?;
                        tys.push(ty);
                        if self.matches_token(&TokenKind::RParen) {
                            break;
                        }
                        if !self.matches_token(&TokenKind::Comma) {
                            self.error_here("expected `,` or `)` in tuple variant");
                            return None;
                        }
                    }
                }
                EnumVariantFields::Tuple(tys)
            } else if self.matches_token(&TokenKind::LBrace) {
                // Struct-like variant
                let mut fields_vec = Vec::new();
                while !self.is_at_end() && !self.matches_token(&TokenKind::RBrace) {
                    let field_name = self.parse_ident("expected field name in enum variant")?;
                    if !self.matches_token(&TokenKind::Colon) {
                        self.error_here("expected `:` after field name in enum variant");
                        return None;
                    }
                    let ty = self.parse_type_expr()?;
                    fields_vec.push(StructField {
                        name: field_name,
                        ty,
                    });
                    let _ = self.matches_token(&TokenKind::Comma);
                }
                EnumVariantFields::Struct(fields_vec)
            } else {
                // Unit variant
                EnumVariantFields::Unit
            };

            variants.push(EnumVariant {
                name: variant_name,
                fields,
            });

            // Optional trailing comma between variants
            let _ = self.matches_token(&TokenKind::Comma);
        }

        let end = self.previous().span.range.end;
        let span = Span {
            range: enum_tok.span.range.start..end,
        };

        Some(Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::Enum {
                name,
                type_params,
                variants,
            },
            span,
        })
    }

    fn parse_type_alias_item(&mut self) -> Option<Item> {
        let type_tok = self.advance().clone(); // consume `type`
        let name = self.parse_ident("expected type alias name")?;
        if !self.matches_token(&TokenKind::Eq) {
            self.error_here("expected `=` in type alias");
            return None;
        }
        let ty = self.parse_type_expr()?;
        if !self.matches_token(&TokenKind::Semicolon) {
            self.error_here("expected `;` after type alias");
            return None;
        }
        let end = self.previous().span.range.end;
        let span = Span {
            range: type_tok.span.range.start..end,
        };
        Some(Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::TypeAlias { name, ty },
            span,
        })
    }

    fn parse_extern_block_item(&mut self) -> Option<Item> {
        let extern_tok = self.advance().clone(); // consume `extern`
        // Expect "js" string literal for MVP
        let abi = if let TokenKind::StringLiteral(ref s) = self.current().kind {
            let s = s.clone();
            self.advance();
            s
        } else {
            self.error_here("expected string literal ABI after `extern` (e.g., \"js\")");
            "js".to_string()
        };

        if !self.matches_token(&TokenKind::LBrace) {
            self.error_here("expected `{` to start extern block");
            return None;
        }

        let mut items = Vec::new();
        while !self.is_at_end() && !self.matches_token(&TokenKind::RBrace) {
            // Check for `mod` declaration:
            // - `mod identifier;` - identifier is both package and binding
            // - `mod "package-name";` - string literal, derive binding from package
            // - `mod "package-name" as alias;` - string literal with explicit alias
            if self.matches_keyword(Keyword::Mod) {
                let mod_start = self.previous().span.range.start;

                // Parse package name (identifier or string literal)
                let (package, default_binding) =
                    if let TokenKind::StringLiteral(ref s) = self.current().kind {
                        let pkg = s.clone();
                        let binding_name = derive_binding_from_package(&pkg);
                        let tok = self.advance().clone();
                        (
                            pkg,
                            Ident {
                                name: binding_name,
                                span: Span {
                                    range: tok.span.range.clone(),
                                },
                            },
                        )
                    } else if let Some(id) =
                        self.parse_ident("expected module name or string literal after `mod`")
                    {
                        (id.name.clone(), id)
                    } else {
                        self.synchronize_item();
                        continue;
                    };

                // Check for optional `as alias`
                let binding = if self.matches_keyword(Keyword::As) {
                    match self.parse_ident("expected alias identifier after `as`") {
                        Some(alias) => alias,
                        None => {
                            self.synchronize_item();
                            continue;
                        }
                    }
                } else {
                    default_binding
                };

                // Check for block `{ ... }` with nested function declarations
                // or semicolon `;` for simple module import
                let mod_items = if self.matches_token(&TokenKind::LBrace) {
                    let mut nested = Vec::new();
                    while !self.is_at_end() && !self.matches_token(&TokenKind::RBrace) {
                        if !self.matches_keyword(Keyword::Fn) {
                            self.error_here("expected `fn` inside mod block");
                            self.synchronize_item();
                            continue;
                        }
                        let fn_name = match self.parse_ident("expected function name in mod block")
                        {
                            Some(id) => id,
                            None => {
                                self.synchronize_item();
                                continue;
                            }
                        };

                        if !self.matches_token(&TokenKind::LParen) {
                            self.error_here("expected `(` after function name");
                            self.synchronize_item();
                            continue;
                        }
                        let fn_params = self.parse_param_list();

                        let fn_ret_type = if self.matches_token(&TokenKind::Arrow) {
                            self.parse_type_expr()
                        } else {
                            None
                        };

                        if !self.matches_token(&TokenKind::Semicolon) {
                            self.error_here("expected `;` after function declaration in mod block");
                            self.synchronize_item();
                            continue;
                        }

                        let fn_span = Span {
                            range: fn_name.span.range.start..self.previous().span.range.end,
                        };
                        nested.push(husk_ast::ModItem {
                            kind: husk_ast::ModItemKind::Fn {
                                name: fn_name,
                                params: fn_params,
                                ret_type: fn_ret_type,
                            },
                            span: fn_span,
                        });
                    }
                    nested
                } else if self.matches_token(&TokenKind::Semicolon) {
                    Vec::new()
                } else {
                    self.error_here("expected `{` or `;` after module declaration");
                    self.synchronize_item();
                    continue;
                };

                let span = Span {
                    range: mod_start..self.previous().span.range.end,
                };
                items.push(husk_ast::ExternItem {
                    kind: husk_ast::ExternItemKind::Mod {
                        package,
                        binding,
                        items: mod_items,
                    },
                    span,
                });
                continue;
            }

            // Check for `struct` declaration (extern struct)
            if self.matches_keyword(Keyword::Struct) {
                let struct_start = self.previous().span.range.start;
                let name = match self.parse_ident("expected struct name after `struct`") {
                    Some(id) => id,
                    None => {
                        self.synchronize_item();
                        continue;
                    }
                };

                // Parse optional type parameters: struct JsArray<T>;
                let type_params = if self.matches_token(&TokenKind::Lt) {
                    let mut params = Vec::new();
                    loop {
                        if let Some(param) = self.parse_ident("expected type parameter") {
                            params.push(param);
                        }
                        if !self.matches_token(&TokenKind::Comma) {
                            break;
                        }
                    }
                    if !self.matches_token(&TokenKind::Gt) {
                        self.error_here("expected `>` after type parameters");
                    }
                    params
                } else {
                    Vec::new()
                };

                if !self.matches_token(&TokenKind::Semicolon) {
                    self.error_here("expected `;` after extern struct declaration");
                    self.synchronize_item();
                    continue;
                }

                let span = Span {
                    range: struct_start..self.previous().span.range.end,
                };
                items.push(husk_ast::ExternItem {
                    kind: husk_ast::ExternItemKind::Struct { name, type_params },
                    span,
                });
                continue;
            }

            // Check for `static` declaration (extern static variable)
            if self.matches_keyword(Keyword::Static) {
                let static_start = self.previous().span.range.start;
                let name = match self.parse_ident("expected variable name after `static`") {
                    Some(id) => id,
                    None => {
                        self.synchronize_item();
                        continue;
                    }
                };

                if !self.matches_token(&TokenKind::Colon) {
                    self.error_here("expected `:` after static variable name");
                    self.synchronize_item();
                    continue;
                }

                let ty = match self.parse_type_expr() {
                    Some(t) => t,
                    None => {
                        self.synchronize_item();
                        continue;
                    }
                };

                if !self.matches_token(&TokenKind::Semicolon) {
                    self.error_here("expected `;` after static declaration");
                    self.synchronize_item();
                    continue;
                }

                let span = Span {
                    range: static_start..self.previous().span.range.end,
                };
                items.push(husk_ast::ExternItem {
                    kind: husk_ast::ExternItemKind::Static { name, ty },
                    span,
                });
                continue;
            }

            // Check for `fn` declaration
            if !self.matches_keyword(Keyword::Fn) {
                self.error_here("expected `fn`, `mod`, `struct`, or `static` inside extern block");
                self.synchronize_item();
                continue;
            }
            let name = match self.parse_ident("expected function name in extern block") {
                Some(id) => id,
                None => {
                    self.synchronize_item();
                    continue;
                }
            };

            if !self.matches_token(&TokenKind::LParen) {
                self.error_here("expected `(` after extern function name");
                self.synchronize_item();
                continue;
            }
            let params = self.parse_param_list();

            let ret_type = if self.matches_token(&TokenKind::Arrow) {
                Some(self.parse_type_expr()?)
            } else {
                None
            };

            if !self.matches_token(&TokenKind::Semicolon) {
                self.error_here("expected `;` after extern function declaration");
                self.synchronize_item();
                continue;
            }

            let span = Span {
                range: name.span.range.start..self.previous().span.range.end,
            };
            items.push(husk_ast::ExternItem {
                kind: husk_ast::ExternItemKind::Fn {
                    name,
                    params,
                    ret_type,
                },
                span,
            });
        }

        let end = self.previous().span.range.end;
        let span = Span {
            range: extern_tok.span.range.start..end,
        };

        Some(Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::ExternBlock { abi, items },
            span,
        })
    }

    /// Parse a trait definition: `trait Name { fn method(&self); }`
    fn parse_trait_item(&mut self) -> Option<Item> {
        let trait_tok = self.advance().clone(); // consume `trait`
        let name = self.parse_ident("expected trait name after `trait`")?;

        // Parse optional type parameters with bounds
        let type_params = self.parse_type_params_with_bounds();

        if !self.matches_token(&TokenKind::LBrace) {
            self.error_here("expected `{` after trait name");
            return None;
        }

        let mut items = Vec::new();
        while !self.is_at_end() && !self.matches_token(&TokenKind::RBrace) {
            if let Some(item) = self.parse_trait_method() {
                items.push(item);
            } else {
                self.synchronize_item();
            }
        }

        let end = self.previous().span.range.end;
        let span = Span {
            range: trait_tok.span.range.start..end,
        };

        Some(Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::Trait(TraitDef {
                name,
                type_params,
                items,
                span: span.clone(),
            }),
            span,
        })
    }

    /// Parse a method inside a trait: `fn method(&self) -> RetType;` or with default body
    fn parse_trait_method(&mut self) -> Option<TraitItem> {
        if !self.matches_keyword(Keyword::Fn) {
            self.error_here("expected `fn` inside trait");
            return None;
        }
        let fn_start = self.previous().span.range.start;
        let name = self.parse_ident("expected method name after `fn`")?;

        if !self.matches_token(&TokenKind::LParen) {
            self.error_here("expected `(` after method name");
            return None;
        }

        // Parse self receiver and regular parameters
        let (receiver, params) = self.parse_method_param_list();

        // Optional return type
        let ret_type = if self.matches_token(&TokenKind::Arrow) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        // Either `;` (required method) or `{ ... }` (default implementation)
        let default_body = if self.matches_token(&TokenKind::Semicolon) {
            None
        } else if self.matches_token(&TokenKind::LBrace) {
            self.pos -= 1; // move back to parse_block
            let block = self.parse_block()?;
            Some(block.stmts)
        } else {
            self.error_here("expected `;` or `{` after method signature");
            return None;
        };

        let end = self.previous().span.range.end;
        Some(TraitItem {
            kind: TraitItemKind::Method(TraitMethod {
                name,
                receiver,
                params,
                ret_type,
                default_body,
            }),
            span: Span {
                range: fn_start..end,
            },
        })
    }

    /// Parse an impl block: `impl Trait for Type { ... }` or `impl Type { ... }`
    fn parse_impl_item(&mut self) -> Option<Item> {
        let impl_tok = self.advance().clone(); // consume `impl`

        // Parse optional type parameters with bounds
        let type_params = self.parse_type_params_with_bounds();

        // Parse the first type (could be trait or self_ty)
        let first_ty = self.parse_type_expr()?;

        // Check for `for` keyword to distinguish `impl Trait for Type` from `impl Type`
        let (trait_ref, self_ty) = if self.matches_keyword(Keyword::For) {
            let self_ty = self.parse_type_expr()?;
            (Some(first_ty), self_ty)
        } else {
            (None, first_ty)
        };

        if !self.matches_token(&TokenKind::LBrace) {
            self.error_here("expected `{` after impl header");
            return None;
        }

        let mut items = Vec::new();
        while !self.is_at_end() && !self.matches_token(&TokenKind::RBrace) {
            if let Some(item) = self.parse_impl_method() {
                items.push(item);
            } else {
                self.synchronize_item();
            }
        }

        let end = self.previous().span.range.end;
        let span = Span {
            range: impl_tok.span.range.start..end,
        };

        Some(Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::Impl(ImplBlock {
                type_params,
                trait_ref,
                self_ty,
                items,
                span: span.clone(),
            }),
            span,
        })
    }

    /// Parse a single attribute like `#[getter]` or `#[js_name = "innerHTML"]`.
    /// Returns None if no attribute is found (not an error).
    fn parse_attribute(&mut self) -> Option<Attribute> {
        if !self.matches_token(&TokenKind::Hash) {
            return None;
        }
        let start = self.previous().span.range.start;

        if !self.matches_token(&TokenKind::LBracket) {
            self.error_here("expected `[` after `#`");
            return None;
        }

        let name = self.parse_ident("expected attribute name")?;

        // Check for `= "value"` syntax
        let value = if self.matches_token(&TokenKind::Eq) {
            if let TokenKind::StringLiteral(ref s) = self.current().kind {
                let val = s.clone();
                self.advance();
                Some(val)
            } else {
                self.error_here("expected string literal after `=` in attribute");
                None
            }
        } else {
            None
        };

        if !self.matches_token(&TokenKind::RBracket) {
            self.error_here("expected `]` to close attribute");
            return None;
        }

        let end = self.previous().span.range.end;
        Some(Attribute {
            name,
            value,
            span: Span { range: start..end },
        })
    }

    /// Parse zero or more attributes: `#[getter] #[setter] #[js_name = "foo"]`
    fn parse_attributes(&mut self) -> Vec<Attribute> {
        let mut attrs = Vec::new();
        while let TokenKind::Hash = self.current().kind {
            if let Some(attr) = self.parse_attribute() {
                attrs.push(attr);
            } else {
                break;
            }
        }
        attrs
    }

    /// Parse a method or property inside an impl block.
    /// Methods: `fn method(&self) -> RetType { ... }` or `extern "js" fn method(&self) -> RetType;`
    /// Properties: `#[getter] extern "js" body: JsValue;`
    fn parse_impl_method(&mut self) -> Option<ImplItem> {
        // First, parse any attributes
        let attributes = self.parse_attributes();

        // Check for extern "js" (could be method or property)
        let is_extern = if self.matches_keyword(Keyword::Extern) {
            // Expect "js" string literal
            if let TokenKind::StringLiteral(ref s) = self.current().kind {
                if s != "js" {
                    self.error_here("only \"js\" ABI is supported for extern methods");
                }
                self.advance();
            } else {
                self.error_here("expected string literal ABI after `extern`");
            }
            true
        } else {
            false
        };

        // If extern and NOT followed by `fn`, this is a property declaration
        if is_extern && !matches!(self.current().kind, TokenKind::Keyword(Keyword::Fn)) {
            // Parse property: `name: Type;`
            return self.parse_extern_property(attributes);
        }

        if !self.matches_keyword(Keyword::Fn) {
            self.error_here("expected `fn` inside impl");
            return None;
        }
        let fn_start = self.previous().span.range.start;
        let name = self.parse_ident("expected method name after `fn`")?;

        if !self.matches_token(&TokenKind::LParen) {
            self.error_here("expected `(` after method name");
            return None;
        }

        // Parse self receiver and regular parameters
        let (receiver, params) = self.parse_method_param_list();

        // Optional return type
        let ret_type = if self.matches_token(&TokenKind::Arrow) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        if is_extern {
            // Extern methods end with semicolon, no body
            if !self.matches_token(&TokenKind::Semicolon) {
                self.error_here("expected `;` after extern method declaration");
                return None;
            }
            let end = self.previous().span.range.end;
            Some(ImplItem {
                kind: ImplItemKind::Method(ImplMethod {
                    name,
                    receiver,
                    params,
                    ret_type,
                    body: Vec::new(), // Extern methods have no body
                    is_extern: true,
                }),
                span: Span {
                    range: fn_start..end,
                },
            })
        } else {
            // Regular method - body is required
            let body_block = self.parse_block()?;
            let end = body_block.span.range.end;
            Some(ImplItem {
                kind: ImplItemKind::Method(ImplMethod {
                    name,
                    receiver,
                    params,
                    ret_type,
                    body: body_block.stmts,
                    is_extern: false,
                }),
                span: Span {
                    range: fn_start..end,
                },
            })
        }
    }

    /// Parse an extern property declaration: `name: Type;`
    /// Called after attributes and `extern "js"` have been parsed.
    fn parse_extern_property(&mut self, attributes: Vec<Attribute>) -> Option<ImplItem> {
        // Get start position from first attribute if present, otherwise use current position
        let start = attributes
            .first()
            .map(|a| a.span.range.start)
            .unwrap_or_else(|| self.current().span.range.start);

        // Parse property name
        let name = self.parse_ident("expected property name")?;

        // Expect `:`
        if !self.matches_token(&TokenKind::Colon) {
            self.error_here("expected `:` after property name");
            return None;
        }

        // Parse type
        let ty = self.parse_type_expr()?;

        // Expect `;`
        if !self.matches_token(&TokenKind::Semicolon) {
            self.error_here("expected `;` after extern property declaration");
            return None;
        }

        let end = self.previous().span.range.end;
        Some(ImplItem {
            kind: ImplItemKind::Property(ExternProperty {
                attributes,
                name,
                ty,
                span: Span { range: start..end },
            }),
            span: Span { range: start..end },
        })
    }

    /// Parse method parameter list, handling `self`, `&self`, `&mut self` as first param
    fn parse_method_param_list(&mut self) -> (Option<SelfReceiver>, Vec<Param>) {
        let mut receiver = None;
        let mut params = Vec::new();

        if self.matches_token(&TokenKind::RParen) {
            return (receiver, params);
        }

        // Check for self receiver
        if self.matches_token(&TokenKind::Amp) {
            // `&self` or `&mut self`
            if self.matches_keyword(Keyword::Mut) {
                // `&mut self`
                if !self.check_ident("self") {
                    self.error_here("expected `self` after `&mut`");
                } else {
                    self.advance(); // consume `self`
                    receiver = Some(SelfReceiver::RefMut);
                }
            } else if self.check_ident("self") {
                // `&self`
                self.advance(); // consume `self`
                receiver = Some(SelfReceiver::Ref);
            } else {
                self.error_here("expected `self` or `mut self` after `&`");
            }
        } else if self.check_ident("self") {
            // `self` by value
            self.advance(); // consume `self`
            receiver = Some(SelfReceiver::Value);
        }

        // If we got a receiver, check for comma or end
        if receiver.is_some() {
            if self.matches_token(&TokenKind::RParen) {
                return (receiver, params);
            }
            if !self.matches_token(&TokenKind::Comma) {
                self.error_here("expected `,` or `)` after self parameter");
                return (receiver, params);
            }
        }

        // Parse remaining parameters
        if self.matches_token(&TokenKind::RParen) {
            return (receiver, params);
        }

        while let Some(name) = self.parse_ident("expected parameter name") {
            if !self.matches_token(&TokenKind::Colon) {
                self.error_here("expected `:` after parameter name");
                break;
            }
            let ty = match self.parse_type_expr() {
                Some(t) => t,
                None => break,
            };
            params.push(Param { name, ty });

            if self.matches_token(&TokenKind::RParen) {
                break;
            }
            if !self.matches_token(&TokenKind::Comma) {
                self.error_here("expected `,` or `)` in parameter list");
                break;
            }
        }

        (receiver, params)
    }

    /// Check if current token is a specific identifier (without consuming)
    fn check_ident(&self, name: &str) -> bool {
        matches!(&self.current().kind, TokenKind::Ident(s) if s == name)
    }

    /// Parse type parameters with optional bounds: `<T, U: Foo + Bar>`
    fn parse_type_params_with_bounds(&mut self) -> Vec<TypeParam> {
        let mut params = Vec::new();
        if !self.matches_token(&TokenKind::Lt) {
            return params;
        }

        loop {
            let name = match self.parse_ident("expected type parameter name") {
                Some(id) => id,
                None => break,
            };

            // Parse optional bounds: `: Foo + Bar`
            let bounds = if self.matches_token(&TokenKind::Colon) {
                self.parse_trait_bounds()
            } else {
                Vec::new()
            };

            params.push(TypeParam { name, bounds });

            if self.matches_token(&TokenKind::Gt) {
                break;
            }
            if !self.matches_token(&TokenKind::Comma) {
                self.error_here("expected `,` or `>` in type parameter list");
                break;
            }
        }

        params
    }

    /// Parse trait bounds: `Foo + Bar + Baz`
    fn parse_trait_bounds(&mut self) -> Vec<TypeExpr> {
        let mut bounds = Vec::new();

        loop {
            if let Some(ty) = self.parse_type_expr() {
                bounds.push(ty);
            } else {
                break;
            }

            if !self.matches_token(&TokenKind::Plus) {
                break;
            }
        }

        bounds
    }

    fn parse_type_params(&mut self) -> Vec<Ident> {
        let mut params = Vec::new();
        if !self.matches_token(&TokenKind::Lt) {
            return params;
        }
        while let Some(id) = self.parse_ident("expected type parameter name") {
            params.push(id);
            if self.matches_token(&TokenKind::Gt) {
                break;
            }
            if !self.matches_token(&TokenKind::Comma) {
                self.error_here("expected `,` or `>` in type parameter list");
                break;
            }
        }
        params
    }

    fn parse_param_list(&mut self) -> Vec<Param> {
        let mut params = Vec::new();
        if self.matches_token(&TokenKind::RParen) {
            return params;
        }

        while let Some(name) = self.parse_ident("expected parameter name") {
            if !self.matches_token(&TokenKind::Colon) {
                self.error_here("expected `:` after parameter name");
                break;
            }
            let ty = match self.parse_type_expr() {
                Some(t) => t,
                None => break,
            };
            params.push(Param { name, ty });

            if self.matches_token(&TokenKind::RParen) {
                break;
            }
            if !self.matches_token(&TokenKind::Comma) {
                self.error_here("expected `,` or `)` in parameter list");
                break;
            }
        }
        params
    }

    fn parse_type_expr(&mut self) -> Option<TypeExpr> {
        let tok = self.current().clone();
        match tok.kind {
            // Function type: `fn(T, U) -> V`
            TokenKind::Keyword(Keyword::Fn) => {
                let start = tok.span.range.start;
                self.advance(); // consume `fn`

                if !self.matches_token(&TokenKind::LParen) {
                    self.error_here("expected `(` after `fn` in function type");
                    return None;
                }

                // Parse parameter types
                let mut params = Vec::new();
                if !self.matches_token(&TokenKind::RParen) {
                    loop {
                        let param_ty = self.parse_type_expr()?;
                        params.push(param_ty);
                        if self.matches_token(&TokenKind::RParen) {
                            break;
                        }
                        if !self.matches_token(&TokenKind::Comma) {
                            self.error_here("expected `,` or `)` in function type parameters");
                            break;
                        }
                    }
                }

                // Return type is required: `-> Type`
                if !self.matches_token(&TokenKind::Arrow) {
                    self.error_here("expected `->` after function type parameters");
                    return None;
                }

                let ret = self.parse_type_expr()?;
                let end = ret.span.range.end;

                Some(TypeExpr {
                    kind: TypeExprKind::Function {
                        params,
                        ret: Box::new(ret),
                    },
                    span: Span { range: start..end },
                })
            }
            // Unit type: `()`
            TokenKind::LParen => {
                let start = tok.span.range.start;
                self.advance(); // consume `(`
                if !self.matches_token(&TokenKind::RParen) {
                    self.error_here("expected `)` for unit type `()`");
                    return None;
                }
                let end = self.previous().span.range.end;
                // Unit type is represented as a named type "()"
                Some(TypeExpr {
                    kind: TypeExprKind::Named(Ident {
                        name: "()".to_string(),
                        span: Span { range: start..end },
                    }),
                    span: Span { range: start..end },
                })
            }
            // Array type: `[ElementType]`
            TokenKind::LBracket => {
                let start = tok.span.range.start;
                self.advance(); // consume '['

                let elem_type = self.parse_type_expr()?;

                if !self.matches_token(&TokenKind::RBracket) {
                    self.error_here("expected `]` after array element type");
                    return None;
                }

                let end = self.previous().span.range.end;

                Some(TypeExpr {
                    kind: TypeExprKind::Array(Box::new(elem_type)),
                    span: Span { range: start..end },
                })
            }
            TokenKind::Ident(ref name) => {
                self.advance();
                let ident = Ident {
                    name: name.clone(),
                    span: self.ast_span_from(&tok.span),
                };
                // Generic application: Name<...>
                if self.matches_token(&TokenKind::Lt) {
                    let mut args = Vec::new();
                    loop {
                        let arg = self.parse_type_expr()?;
                        args.push(arg);
                        if self.matches_token(&TokenKind::Gt) {
                            break;
                        }
                        if !self.matches_token(&TokenKind::Comma) {
                            self.error_here("expected `,` or `>` in generic type arguments");
                            break;
                        }
                    }
                    let span = Span {
                        range: ident.span.range.start..self.previous().span.range.end,
                    };
                    Some(TypeExpr {
                        kind: TypeExprKind::Generic { name: ident, args },
                        span,
                    })
                } else {
                    let span = ident.span.clone();
                    Some(TypeExpr {
                        kind: TypeExprKind::Named(ident),
                        span,
                    })
                }
            }
            _ => {
                self.error_at_token(&tok, "expected type");
                None
            }
        }
    }

    fn parse_ident(&mut self, msg: &str) -> Option<Ident> {
        let tok = self.current().clone();
        if let TokenKind::Ident(ref name) = tok.kind {
            self.advance();
            Some(Ident {
                name: name.clone(),
                span: self.ast_span_from(&tok.span),
            })
        } else {
            self.error_here(msg);
            None
        }
    }

    /// After consuming an initial identifier, parse any following `::segment` path components.
    fn parse_path_segments(&mut self, first: Ident) -> Vec<Ident> {
        let mut path = vec![first];
        // Check for path segments `Foo::Bar`.
        while self.matches_token(&TokenKind::ColonColon) {
            let seg = match self.parse_ident("expected identifier after `::` in path") {
                Some(id) => id,
                None => break,
            };
            path.push(seg);
        }
        path
    }

    // ---------------- Statements and blocks ----------------

    fn parse_block(&mut self) -> Option<Block> {
        let start_tok = self.current().clone();
        if !self.matches_token(&TokenKind::LBrace) {
            self.error_here("expected `{` to start block");
            return None;
        }
        let start = start_tok.span.range.start;

        let mut stmts = Vec::new();
        while !self.is_at_end() && !self.matches_token(&TokenKind::RBrace) {
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            } else {
                // Attempt to recover by skipping to next semicolon or closing brace
                while !self.is_at_end() {
                    if matches!(
                        self.current().kind,
                        TokenKind::Semicolon | TokenKind::RBrace
                    ) {
                        break;
                    }
                    self.advance();
                }
                let _ = self.matches_token(&TokenKind::Semicolon);
            }
        }

        let end = self.previous().span.range.end;
        Some(Block {
            stmts,
            span: Span { range: start..end },
        })
    }

    fn parse_use_item(&mut self) -> Option<Item> {
        let use_tok = self.advance().clone(); // consume `use`
        let first = self.parse_ident("expected path after `use`")?;
        let path = self.parse_path_segments(first);
        if !self.matches_token(&TokenKind::Semicolon) {
            self.error_here("expected `;` after use path");
        }
        let end = self.previous().span.range.end;
        Some(Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::Use { path },
            span: Span {
                range: use_tok.span.range.start..end,
            },
        })
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.current().kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_let_stmt(),
            TokenKind::Keyword(Keyword::Return) => self.parse_return_stmt(),
            TokenKind::Keyword(Keyword::If) => self.parse_if_stmt(),
            TokenKind::Keyword(Keyword::While) => self.parse_while_stmt(),
            TokenKind::Keyword(Keyword::For) => self.parse_for_in_stmt(),
            TokenKind::Keyword(Keyword::Break) => self.parse_break_stmt(),
            TokenKind::Keyword(Keyword::Continue) => self.parse_continue_stmt(),
            TokenKind::LBrace => {
                let block = self.parse_block()?;
                let span = block.span.clone();
                Some(Stmt {
                    kind: StmtKind::Block(block),
                    span,
                })
            }
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        let let_tok = self.advance().clone(); // consume `let`
        let mut mutable = false;
        if self.matches_keyword(Keyword::Mut) {
            mutable = true;
        }
        let name = self.parse_ident("expected binding name after `let`")?;

        let ty = if self.matches_token(&TokenKind::Colon) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        let value = if self.matches_token(&TokenKind::Eq) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        if !self.matches_token(&TokenKind::Semicolon) {
            self.error_here("expected `;` after let binding");
        }

        let end = self.previous().span.range.end;
        Some(Stmt {
            kind: StmtKind::Let {
                mutable,
                name,
                ty,
                value,
            },
            span: Span {
                range: let_tok.span.range.start..end,
            },
        })
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        let ret_tok = self.advance().clone(); // consume `return`
        let value = if matches!(self.current().kind, TokenKind::Semicolon) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        if !self.matches_token(&TokenKind::Semicolon) {
            self.error_here("expected `;` after return");
        }
        let end = self.previous().span.range.end;
        Some(Stmt {
            kind: StmtKind::Return { value },
            span: Span {
                range: ret_tok.span.range.start..end,
            },
        })
    }

    fn parse_if_stmt(&mut self) -> Option<Stmt> {
        let if_tok = self.advance().clone(); // consume `if`
        // Use parse_expr_no_struct so that `if x { ... }` doesn't try to
        // parse `x { ... }` as a struct literal.
        let cond = self.parse_expr_no_struct()?;
        let then_branch = self.parse_block().unwrap_or(Block {
            stmts: Vec::new(),
            span: self.ast_span_from(&if_tok.span),
        });

        let else_branch = if self.matches_keyword(Keyword::Else) {
            if matches!(self.current().kind, TokenKind::Keyword(Keyword::If)) {
                // else if ...
                Some(Box::new(self.parse_if_stmt()?))
            } else {
                // else { ... }
                let block = self.parse_block().unwrap_or(Block {
                    stmts: Vec::new(),
                    span: self.ast_span_from(&if_tok.span),
                });
                Some(Box::new(Stmt {
                    kind: StmtKind::Block(block.clone()),
                    span: block.span.clone(),
                }))
            }
        } else {
            None
        };

        let end = self.previous().span.range.end;
        Some(Stmt {
            kind: StmtKind::If {
                cond,
                then_branch,
                else_branch,
            },
            span: Span {
                range: if_tok.span.range.start..end,
            },
        })
    }

    fn parse_while_stmt(&mut self) -> Option<Stmt> {
        let while_tok = self.advance().clone(); // consume `while`
        // Use parse_expr_no_struct so that `while x { ... }` doesn't try to
        // parse `x { ... }` as a struct literal.
        let cond = self.parse_expr_no_struct()?;
        let body = self.parse_block().unwrap_or(Block {
            stmts: Vec::new(),
            span: self.ast_span_from(&while_tok.span),
        });

        let end = body.span.range.end;
        Some(Stmt {
            kind: StmtKind::While { cond, body },
            span: Span {
                range: while_tok.span.range.start..end,
            },
        })
    }

    fn parse_for_in_stmt(&mut self) -> Option<Stmt> {
        let for_tok = self.advance().clone(); // consume `for`
        let binding = self.parse_ident("expected binding name after `for`")?;

        if !self.matches_keyword(Keyword::In) {
            self.error_here("expected `in` after for loop binding");
            return None;
        }

        // Use parse_expr_no_struct so that `for x in arr { ... }` doesn't try to
        // parse `arr { ... }` as a struct literal.
        let iterable = self.parse_expr_no_struct()?;
        let body = self.parse_block().unwrap_or(Block {
            stmts: Vec::new(),
            span: self.ast_span_from(&for_tok.span),
        });

        let end = body.span.range.end;
        Some(Stmt {
            kind: StmtKind::ForIn {
                binding,
                iterable,
                body,
            },
            span: Span {
                range: for_tok.span.range.start..end,
            },
        })
    }

    fn parse_break_stmt(&mut self) -> Option<Stmt> {
        let tok = self.advance().clone(); // consume `break`
        if !self.matches_token(&TokenKind::Semicolon) {
            self.error_here("expected `;` after `break`");
        }
        let end = self.previous().span.range.end;
        Some(Stmt {
            kind: StmtKind::Break,
            span: Span {
                range: tok.span.range.start..end,
            },
        })
    }

    fn parse_continue_stmt(&mut self) -> Option<Stmt> {
        let tok = self.advance().clone(); // consume `continue`
        if !self.matches_token(&TokenKind::Semicolon) {
            self.error_here("expected `;` after `continue`");
        }
        let end = self.previous().span.range.end;
        Some(Stmt {
            kind: StmtKind::Continue,
            span: Span {
                range: tok.span.range.start..end,
            },
        })
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let start = self.current().span.range.start;
        let expr = self.parse_expr()?;

        // Check for assignment operators
        let assign_op = match self.current().kind {
            TokenKind::Eq => Some(AssignOp::Assign),
            TokenKind::PlusEq => Some(AssignOp::AddAssign),
            TokenKind::MinusEq => Some(AssignOp::SubAssign),
            TokenKind::PercentEq => Some(AssignOp::ModAssign),
            _ => None,
        };

        if let Some(op) = assign_op {
            self.advance(); // consume assignment operator
            let value = self.parse_expr()?;

            if !self.matches_token(&TokenKind::Semicolon) {
                self.error_here("expected `;` after assignment");
            }

            let end = self.previous().span.range.end;
            return Some(Stmt {
                kind: StmtKind::Assign {
                    target: expr,
                    op,
                    value,
                },
                span: Span { range: start..end },
            });
        }

        // Regular expression statement
        let mut span = expr.span.clone();
        let kind = if self.matches_token(&TokenKind::Semicolon) {
            span.range.end = self.previous().span.range.end;
            StmtKind::Semi(expr)
        } else {
            StmtKind::Expr(expr)
        };
        Some(Stmt { kind, span })
    }

    // ---------------- Expressions ----------------

    fn parse_expr(&mut self) -> Option<Expr> {
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> Option<Expr> {
        let mut expr = self.parse_logical_and()?;
        loop {
            if self.matches_token(&TokenKind::OrOr) {
                let op_span = self.previous().span.clone();
                let right = self.parse_logical_and()?;
                let span = Span {
                    range: expr.span.range.start..right.span.range.end,
                };
                expr = Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::Or,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                    span,
                };
                let _ = op_span; // suppress unused warning
            } else {
                break;
            }
        }
        Some(expr)
    }

    fn parse_logical_and(&mut self) -> Option<Expr> {
        let mut expr = self.parse_equality()?;
        loop {
            if self.matches_token(&TokenKind::AndAnd) {
                let right = self.parse_equality()?;
                let span = Span {
                    range: expr.span.range.start..right.span.range.end,
                };
                expr = Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::And,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                    span,
                };
            } else {
                break;
            }
        }
        Some(expr)
    }

    fn parse_equality(&mut self) -> Option<Expr> {
        let mut expr = self.parse_range()?;
        loop {
            if self.matches_token(&TokenKind::EqEq) {
                let right = self.parse_range()?;
                let span = Span {
                    range: expr.span.range.start..right.span.range.end,
                };
                expr = Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::Eq,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                    span,
                };
            } else if self.matches_token(&TokenKind::BangEq) {
                let right = self.parse_range()?;
                let span = Span {
                    range: expr.span.range.start..right.span.range.end,
                };
                expr = Expr {
                    kind: ExprKind::Binary {
                        op: BinaryOp::NotEq,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                    span,
                };
            } else {
                break;
            }
        }
        Some(expr)
    }

    fn parse_range(&mut self) -> Option<Expr> {
        let expr = self.parse_comparison()?;

        if self.matches_token(&TokenKind::DotDot) {
            let end = self.parse_comparison()?;
            let span = Span {
                range: expr.span.range.start..end.span.range.end,
            };
            return Some(Expr {
                kind: ExprKind::Range {
                    start: Box::new(expr),
                    end: Box::new(end),
                    inclusive: false,
                },
                span,
            });
        } else if self.matches_token(&TokenKind::DotDotEq) {
            let end = self.parse_comparison()?;
            let span = Span {
                range: expr.span.range.start..end.span.range.end,
            };
            return Some(Expr {
                kind: ExprKind::Range {
                    start: Box::new(expr),
                    end: Box::new(end),
                    inclusive: true,
                },
                span,
            });
        }

        Some(expr)
    }

    fn parse_comparison(&mut self) -> Option<Expr> {
        let mut expr = self.parse_additive()?;
        loop {
            let op = if self.matches_token(&TokenKind::Lt) {
                Some(BinaryOp::Lt)
            } else if self.matches_token(&TokenKind::Le) {
                Some(BinaryOp::Le)
            } else if self.matches_token(&TokenKind::Gt) {
                Some(BinaryOp::Gt)
            } else if self.matches_token(&TokenKind::Ge) {
                Some(BinaryOp::Ge)
            } else {
                None
            };

            if let Some(op) = op {
                let right = self.parse_additive()?;
                let span = Span {
                    range: expr.span.range.start..right.span.range.end,
                };
                expr = Expr {
                    kind: ExprKind::Binary {
                        op,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                    span,
                };
            } else {
                break;
            }
        }
        Some(expr)
    }

    fn parse_additive(&mut self) -> Option<Expr> {
        let mut expr = self.parse_multiplicative()?;
        loop {
            let op = if self.matches_token(&TokenKind::Plus) {
                Some(BinaryOp::Add)
            } else if self.matches_token(&TokenKind::Minus) {
                Some(BinaryOp::Sub)
            } else {
                None
            };

            if let Some(op) = op {
                let right = self.parse_multiplicative()?;
                let span = Span {
                    range: expr.span.range.start..right.span.range.end,
                };
                expr = Expr {
                    kind: ExprKind::Binary {
                        op,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                    span,
                };
            } else {
                break;
            }
        }
        Some(expr)
    }

    fn parse_multiplicative(&mut self) -> Option<Expr> {
        let mut expr = self.parse_unary()?;
        loop {
            let op = if self.matches_token(&TokenKind::Star) {
                Some(BinaryOp::Mul)
            } else if self.matches_token(&TokenKind::Slash) {
                Some(BinaryOp::Div)
            } else if self.matches_token(&TokenKind::Percent) {
                Some(BinaryOp::Mod)
            } else {
                None
            };

            if let Some(op) = op {
                let right = self.parse_unary()?;
                let span = Span {
                    range: expr.span.range.start..right.span.range.end,
                };
                expr = Expr {
                    kind: ExprKind::Binary {
                        op,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                    span,
                };
            } else {
                break;
            }
        }
        Some(expr)
    }

    fn parse_unary(&mut self) -> Option<Expr> {
        if self.matches_token(&TokenKind::Bang) {
            let op_tok = self.previous().clone();
            let expr = self.parse_unary()?;
            let span = Span {
                range: op_tok.span.range.start..expr.span.range.end,
            };
            return Some(Expr {
                kind: ExprKind::Unary {
                    op: husk_ast::UnaryOp::Not,
                    expr: Box::new(expr),
                },
                span,
            });
        }
        if self.matches_token(&TokenKind::Minus) {
            let op_tok = self.previous().clone();
            let expr = self.parse_unary()?;
            let span = Span {
                range: op_tok.span.range.start..expr.span.range.end,
            };
            return Some(Expr {
                kind: ExprKind::Unary {
                    op: husk_ast::UnaryOp::Neg,
                    expr: Box::new(expr),
                },
                span,
            });
        }
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Option<Expr> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.matches_token(&TokenKind::LParen) {
                // Function call - check if it's a println or format with format string
                if let ExprKind::Ident(ref id) = expr.kind {
                    if id.name == "println" {
                        if let Some(format_expr) = self.try_parse_format_print(&expr) {
                            expr = format_expr;
                            continue;
                        }
                    } else if id.name == "format" {
                        if let Some(format_expr) = self.try_parse_format(&expr) {
                            expr = format_expr;
                            continue;
                        }
                    }
                }

                // Regular function call
                let args = self.parse_argument_list();
                let span = Span {
                    range: expr.span.range.start..self.previous().span.range.end,
                };
                expr = Expr {
                    kind: ExprKind::Call {
                        callee: Box::new(expr),
                        args,
                    },
                    span,
                };
            } else if self.matches_token(&TokenKind::Dot) {
                // Field or method
                let name_tok = self.current().clone();
                let ident = match &name_tok.kind {
                    TokenKind::Ident(s) => {
                        self.advance();
                        Ident {
                            name: s.clone(),
                            span: self.ast_span_from(&name_tok.span),
                        }
                    }
                    _ => {
                        self.error_here("expected identifier after `.`");
                        break;
                    }
                };

                if self.matches_token(&TokenKind::LParen) {
                    // Method call: expr.ident(args...)
                    let args = self.parse_argument_list();
                    let span = Span {
                        range: expr.span.range.start..self.previous().span.range.end,
                    };
                    expr = Expr {
                        kind: ExprKind::MethodCall {
                            receiver: Box::new(expr),
                            method: ident,
                            args,
                        },
                        span,
                    };
                } else {
                    // Field access: expr.ident
                    let span = Span {
                        range: expr.span.range.start..ident.span.range.end,
                    };
                    expr = Expr {
                        kind: ExprKind::Field {
                            base: Box::new(expr),
                            member: ident,
                        },
                        span,
                    };
                }
            } else if self.matches_token(&TokenKind::LBracket) {
                // Array indexing: expr[index]
                let index_expr = self.parse_expr()?;
                if !self.matches_token(&TokenKind::RBracket) {
                    self.error_here("expected `]` after array index");
                    return None;
                }
                let span = Span {
                    range: expr.span.range.start..self.previous().span.range.end,
                };
                expr = Expr {
                    kind: ExprKind::Index {
                        base: Box::new(expr),
                        index: Box::new(index_expr),
                    },
                    span,
                };
            } else {
                break;
            }
        }
        Some(expr)
    }

    fn parse_argument_list(&mut self) -> Vec<Expr> {
        let mut args = Vec::new();
        if self.matches_token(&TokenKind::RParen) {
            return args;
        }
        while let Some(arg) = self.parse_expr() {
            args.push(arg);
            if self.matches_token(&TokenKind::RParen) {
                break;
            }
            if !self.matches_token(&TokenKind::Comma) {
                self.error_here("expected `,` or `)` in argument list");
                break;
            }
        }
        args
    }

    /// Try to parse a println call as a FormatPrint expression.
    /// Returns None if first argument is not a string literal with format placeholders,
    /// in which case it falls back to a regular Call.
    /// We're positioned right after the `(` of `println(...)`.
    fn try_parse_format_print(&mut self, callee_expr: &Expr) -> Option<Expr> {
        let start = callee_expr.span.range.start;

        // Check if first token is a string literal
        let format_tok = self.current().clone();
        let format_str = match &format_tok.kind {
            TokenKind::StringLiteral(s) => s.clone(),
            _ => return None, // Not a string literal, fall back to regular call
        };

        // Parse the format string to check if it has placeholders
        let format_span = self.ast_span_from(&format_tok.span);
        let parsed_format = self.parse_format_string(&format_str, &format_span);

        // Advance past the format string
        self.advance();

        // Collect remaining arguments
        let mut args = Vec::new();
        if !self.matches_token(&TokenKind::RParen) {
            // There are more arguments after the format string
            if !self.matches_token(&TokenKind::Comma) {
                self.error_here("expected `,` or `)` after format string");
                return None;
            }
            while let Some(arg) = self.parse_expr() {
                args.push(arg);
                if self.matches_token(&TokenKind::RParen) {
                    break;
                }
                if !self.matches_token(&TokenKind::Comma) {
                    self.error_here("expected `,` or `)` in argument list");
                    break;
                }
            }
        }

        let end = self.previous().span.range.end;

        Some(Expr {
            kind: ExprKind::FormatPrint {
                format: parsed_format,
                args,
            },
            span: Span { range: start..end },
        })
    }

    /// Try to parse a format call as a Format expression.
    /// Returns None if first argument is not a string literal with format placeholders,
    /// in which case it falls back to a regular Call.
    /// We're positioned right after the `(` of `format(...)`.
    fn try_parse_format(&mut self, callee_expr: &Expr) -> Option<Expr> {
        let start = callee_expr.span.range.start;

        // Check if first token is a string literal
        let format_tok = self.current().clone();
        let format_str = match &format_tok.kind {
            TokenKind::StringLiteral(s) => s.clone(),
            _ => return None, // Not a string literal, fall back to regular call
        };

        // Parse the format string to check if it has placeholders
        let format_span = self.ast_span_from(&format_tok.span);
        let parsed_format = self.parse_format_string(&format_str, &format_span);

        // Advance past the format string
        self.advance();

        // Collect remaining arguments
        let mut args = Vec::new();
        if !self.matches_token(&TokenKind::RParen) {
            // There are more arguments after the format string
            if !self.matches_token(&TokenKind::Comma) {
                self.error_here("expected `,` or `)` after format string");
                return None;
            }
            while let Some(arg) = self.parse_expr() {
                args.push(arg);
                if self.matches_token(&TokenKind::RParen) {
                    break;
                }
                if !self.matches_token(&TokenKind::Comma) {
                    self.error_here("expected `,` or `)` in argument list");
                    break;
                }
            }
        }

        let end = self.previous().span.range.end;

        Some(Expr {
            kind: ExprKind::Format {
                format: parsed_format,
                args,
            },
            span: Span { range: start..end },
        })
    }

    /// Parse a format string like "Hello, {}! Value: {:x}" into segments.
    fn parse_format_string(&mut self, s: &str, span: &Span) -> FormatString {
        let mut segments = Vec::new();
        let mut chars = s.chars().peekable();
        let mut current_literal = String::new();
        let mut char_offset = 0usize;

        while let Some(c) = chars.next() {
            if c == '{' {
                // Check for escaped brace {{
                if chars.peek() == Some(&'{') {
                    chars.next();
                    current_literal.push('{');
                    char_offset += 2;
                    continue;
                }

                // Start of placeholder - save any accumulated literal
                if !current_literal.is_empty() {
                    segments.push(FormatSegment::Literal(current_literal.clone()));
                    current_literal.clear();
                }

                // Parse the placeholder contents
                let placeholder_start = char_offset;
                let mut placeholder_content = String::new();
                char_offset += 1; // for the '{'

                while let Some(&next) = chars.peek() {
                    if next == '}' {
                        chars.next();
                        char_offset += 1;
                        break;
                    }
                    placeholder_content.push(chars.next().unwrap());
                    char_offset += 1;
                }

                let placeholder_span = Span {
                    range: span.range.start + placeholder_start..span.range.start + char_offset,
                };
                let placeholder = self.parse_placeholder(&placeholder_content, &placeholder_span);
                segments.push(FormatSegment::Placeholder(placeholder));
            } else if c == '}' {
                // Check for escaped brace }}
                if chars.peek() == Some(&'}') {
                    chars.next();
                    current_literal.push('}');
                    char_offset += 2;
                    continue;
                }
                // Unmatched } - treat as error but continue
                current_literal.push('}');
                char_offset += 1;
            } else {
                current_literal.push(c);
                char_offset += 1;
            }
        }

        // Add any remaining literal
        if !current_literal.is_empty() {
            segments.push(FormatSegment::Literal(current_literal));
        }

        FormatString {
            span: span.clone(),
            segments,
        }
    }

    /// Parse placeholder content like "", "0", "name", ":?", ":x", "0:08x", etc.
    fn parse_placeholder(&self, content: &str, span: &Span) -> FormatPlaceholder {
        let mut position: Option<usize> = None;
        let mut name: Option<String> = None;
        let mut spec = FormatSpec::default();

        // Split on ':' to separate argument from format spec
        let (arg_part, spec_part) = match content.find(':') {
            Some(idx) => (&content[..idx], Some(&content[idx + 1..])),
            None => (content, None),
        };

        // Parse the argument part (position or name)
        if !arg_part.is_empty() {
            if let Ok(pos) = arg_part.parse::<usize>() {
                position = Some(pos);
            } else if arg_part.chars().all(|c| c.is_alphanumeric() || c == '_') {
                name = Some(arg_part.to_string());
            }
        }

        // Parse the format spec part
        if let Some(spec_str) = spec_part {
            spec = self.parse_format_spec(spec_str);
        }

        FormatPlaceholder {
            position,
            name,
            spec,
            span: span.clone(),
        }
    }

    /// Parse format spec like "?", "#?", "x", "08x", "<10", "*^20.5", etc.
    /// Format: [[fill]align][sign]['#']['0'][width]['.' precision][type]
    fn parse_format_spec(&self, s: &str) -> FormatSpec {
        let mut spec = FormatSpec::default();
        let mut chars = s.chars().peekable();

        // Parse fill and align
        // Fill is any character followed by an alignment (<, >, ^)
        // Or just alignment alone
        if let Some(&first) = chars.peek() {
            let mut lookahead = chars.clone();
            lookahead.next();
            if let Some(&second) = lookahead.peek() {
                if second == '<' || second == '>' || second == '^' {
                    spec.fill = Some(first);
                    spec.align = Some(second);
                    chars.next();
                    chars.next();
                } else if first == '<' || first == '>' || first == '^' {
                    spec.align = Some(first);
                    chars.next();
                }
            } else if first == '<' || first == '>' || first == '^' {
                spec.align = Some(first);
                chars.next();
            }
        }

        // Parse sign (+)
        if chars.peek() == Some(&'+') {
            spec.sign = true;
            chars.next();
        }

        // Parse alternate form (#)
        if chars.peek() == Some(&'#') {
            spec.alternate = true;
            chars.next();
        }

        // Parse zero padding (0) - must come before width
        if chars.peek() == Some(&'0') {
            // Peek ahead to see if there are more digits (width) or if this is just '0'
            let mut lookahead = chars.clone();
            lookahead.next();
            match lookahead.peek() {
                Some(&c) if c.is_ascii_digit() => {
                    // This is zero-padding prefix followed by width
                    spec.zero_pad = true;
                    chars.next();
                }
                Some(&'.') | Some(&'?') | Some(&'x') | Some(&'X') | Some(&'b') | Some(&'o')
                | None => {
                    // Just a width of 0, or followed by precision/type
                    // Don't consume, let width parsing handle it
                }
                _ => {
                    // Zero padding
                    spec.zero_pad = true;
                    chars.next();
                }
            }
        }

        // Parse width (digits)
        let mut width_str = String::new();
        while let Some(&c) = chars.peek() {
            if c.is_ascii_digit() {
                width_str.push(c);
                chars.next();
            } else {
                break;
            }
        }
        if !width_str.is_empty() {
            spec.width = width_str.parse().ok();
        }

        // Parse precision (.N)
        if chars.peek() == Some(&'.') {
            chars.next();
            let mut prec_str = String::new();
            while let Some(&c) = chars.peek() {
                if c.is_ascii_digit() {
                    prec_str.push(c);
                    chars.next();
                } else {
                    break;
                }
            }
            if !prec_str.is_empty() {
                spec.precision = prec_str.parse().ok();
            }
        }

        // Parse type specifier (?, x, X, b, o, e, E)
        if let Some(&c) = chars.peek() {
            match c {
                '?' | 'x' | 'X' | 'b' | 'o' | 'e' | 'E' => {
                    spec.ty = Some(c);
                    chars.next();
                }
                _ => {}
            }
        }

        spec
    }

    fn parse_match_expr(&mut self) -> Option<Expr> {
        let match_tok = self.advance().clone(); // consume `match`
        // Use parse_expr_no_struct so that `match x { ... }` doesn't try to
        // parse `x { ... }` as a struct literal.
        let scrutinee = self.parse_expr_no_struct()?;
        if !self.matches_token(&TokenKind::LBrace) {
            self.error_here("expected `{` after `match` scrutinee");
            return None;
        }

        let mut arms = Vec::new();
        while !self.is_at_end() && !self.matches_token(&TokenKind::RBrace) {
            let pat = self.parse_pattern()?;
            if !self.matches_token(&TokenKind::FatArrow) {
                self.error_here("expected `=>` after match pattern");
                return None;
            }
            // Arm body: either a block `{ ... }` or an expression.
            let arm_expr = if self.matches_token(&TokenKind::LBrace) {
                // We already consumed `{`, so parse the block body.
                // parse_block expects `{` still current, so we need to rewind one step:
                // Instead, construct a block manually:
                // For simplicity, treat `{ expr }` as a block expression by re-parsing.
                self.pos -= 1; // move back to '{'
                let block = self.parse_block()?;
                Expr {
                    kind: ExprKind::Block(block.clone()),
                    span: block.span.clone(),
                }
            } else {
                self.parse_expr()?
            };

            arms.push(MatchArm {
                pattern: pat,
                expr: arm_expr,
            });

            // Optional trailing comma between arms.
            let _ = self.matches_token(&TokenKind::Comma);
        }

        let end = self.previous().span.range.end;
        Some(Expr {
            kind: ExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            },
            span: Span {
                range: match_tok.span.range.start..end,
            },
        })
    }

    fn parse_pattern(&mut self) -> Option<Pattern> {
        let tok = self.current().clone();
        let kind = match &tok.kind {
            TokenKind::Ident(name) => {
                // Could be binding (`x`) or enum path (`Enum::Variant`).
                let first = Ident {
                    name: name.clone(),
                    span: self.ast_span_from(&tok.span),
                };
                self.advance();

                let mut path = self.parse_path_segments(first);

                if path.len() == 1 {
                    // Single identifier: treat as binding pattern.
                    PatternKind::Binding(path.pop().unwrap())
                } else {
                    // Enum unit pattern: Enum::Variant
                    PatternKind::EnumUnit { path }
                }
            }
            _ => {
                // Wildcard `_`
                if let TokenKind::Ident(ref name) = tok.kind {
                    if name == "_" {
                        self.advance();
                        PatternKind::Wildcard
                    } else {
                        self.error_here("unexpected token in pattern");
                        return None;
                    }
                } else {
                    self.error_here("unexpected token in pattern");
                    return None;
                }
            }
        };

        Some(Pattern {
            kind,
            span: self.ast_span_from(&tok.span),
        })
    }
    /// Parse a closure expression: `|x, y| expr` or `|x: i32| -> i32 { expr }` or `|| expr`
    fn parse_closure_expr(&mut self) -> Option<Expr> {
        let start_tok = self.current().clone();
        let start = start_tok.span.range.start;

        // Handle `||` (empty param list) or `|` (start of param list)
        let params = if self.matches_token(&TokenKind::OrOr) {
            // `|| expr` - no parameters
            Vec::new()
        } else if self.matches_token(&TokenKind::Pipe) {
            // `|x, y| expr` - parse parameter list
            let params = self.parse_closure_param_list();
            if !self.matches_token(&TokenKind::Pipe) {
                self.error_here("expected `|` after closure parameters");
                return None;
            }
            params
        } else {
            self.error_here("expected `|` or `||` to start closure");
            return None;
        };

        // Optional return type: `-> Type`
        let ret_type = if self.matches_token(&TokenKind::Arrow) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        // Body: either a block `{ ... }` or a single expression
        let body = if matches!(self.current().kind, TokenKind::LBrace) {
            let block = self.parse_block()?;
            Expr {
                kind: ExprKind::Block(block.clone()),
                span: block.span.clone(),
            }
        } else {
            self.parse_expr()?
        };

        let end = body.span.range.end;

        Some(Expr {
            kind: ExprKind::Closure {
                params,
                ret_type,
                body: Box::new(body),
            },
            span: Span { range: start..end },
        })
    }

    /// Parse closure parameter list: `x, y: i32, z`
    fn parse_closure_param_list(&mut self) -> Vec<ClosureParam> {
        let mut params = Vec::new();

        // Handle empty param list (just `||`)
        if matches!(self.current().kind, TokenKind::Pipe) {
            return params;
        }

        loop {
            let name = match self.parse_ident("expected parameter name in closure") {
                Some(id) => id,
                None => break,
            };

            // Optional type annotation: `: Type`
            let ty = if self.matches_token(&TokenKind::Colon) {
                self.parse_type_expr()
            } else {
                None
            };

            params.push(ClosureParam { name, ty });

            // Check for closing pipe or comma
            if matches!(self.current().kind, TokenKind::Pipe) {
                break;
            }
            if !self.matches_token(&TokenKind::Comma) {
                break;
            }
        }

        params
    }

    fn parse_primary(&mut self) -> Option<Expr> {
        let tok = self.current().clone();
        match tok.kind {
            // Closure expressions: `|x, y| expr` or `|| expr`
            TokenKind::Pipe | TokenKind::OrOr => self.parse_closure_expr(),
            TokenKind::IntLiteral(ref s) => {
                self.advance();
                let value = s.parse::<i64>().unwrap_or(0);
                Some(Expr {
                    kind: ExprKind::Literal(Literal {
                        kind: LiteralKind::Int(value),
                        span: self.ast_span_from(&tok.span),
                    }),
                    span: self.ast_span_from(&tok.span),
                })
            }
            TokenKind::FloatLiteral(ref s) => {
                self.advance();
                let value = s.parse::<f64>().unwrap_or(0.0);
                Some(Expr {
                    kind: ExprKind::Literal(Literal {
                        kind: LiteralKind::Float(value),
                        span: self.ast_span_from(&tok.span),
                    }),
                    span: self.ast_span_from(&tok.span),
                })
            }
            TokenKind::StringLiteral(ref s) => {
                self.advance();
                Some(Expr {
                    kind: ExprKind::Literal(Literal {
                        kind: LiteralKind::String(s.clone()),
                        span: self.ast_span_from(&tok.span),
                    }),
                    span: self.ast_span_from(&tok.span),
                })
            }
            TokenKind::Keyword(Keyword::True) => {
                self.advance();
                Some(Expr {
                    kind: ExprKind::Literal(Literal {
                        kind: LiteralKind::Bool(true),
                        span: self.ast_span_from(&tok.span),
                    }),
                    span: self.ast_span_from(&tok.span),
                })
            }
            TokenKind::Keyword(Keyword::False) => {
                self.advance();
                Some(Expr {
                    kind: ExprKind::Literal(Literal {
                        kind: LiteralKind::Bool(false),
                        span: self.ast_span_from(&tok.span),
                    }),
                    span: self.ast_span_from(&tok.span),
                })
            }
            TokenKind::Ident(ref name) => {
                // Could be a simple identifier, a path like `Enum::Variant`,
                // or a struct literal like `Point { x: 1, y: 2 }`.
                let first = Ident {
                    name: name.clone(),
                    span: self.ast_span_from(&tok.span),
                };
                self.advance();
                let path = self.parse_path_segments(first.clone());

                // Check for struct literal: `Name { ... }` or `Path::Name { ... }`
                // Only parse as struct literal if allowed in this context.
                if self.allow_struct_expr && self.current().kind == TokenKind::LBrace {
                    return self.parse_struct_expr(path);
                }

                if path.len() == 1 {
                    Some(Expr {
                        kind: ExprKind::Ident(first.clone()),
                        span: first.span,
                    })
                } else {
                    let start = path
                        .first()
                        .map(|id| id.span.range.start)
                        .unwrap_or(tok.span.range.start);
                    let end = path
                        .last()
                        .map(|id| id.span.range.end)
                        .unwrap_or(tok.span.range.end);
                    Some(Expr {
                        kind: ExprKind::Path { segments: path },
                        span: Span { range: start..end },
                    })
                }
            }
            TokenKind::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                if !self.matches_token(&TokenKind::RParen) {
                    self.error_here("expected `)` after expression");
                }
                Some(expr)
            }
            TokenKind::LBracket => self.parse_array_expr(),
            TokenKind::Keyword(Keyword::Match) => self.parse_match_expr(),
            _ => {
                self.error_at_token(&tok, "expected expression");
                None
            }
        }
    }

    fn parse_array_expr(&mut self) -> Option<Expr> {
        let start = self.current().span.range.start;
        self.advance(); // consume '['

        let mut elements = Vec::new();

        // Handle empty array or elements
        if self.current().kind != TokenKind::RBracket {
            loop {
                elements.push(self.parse_expr()?);

                if !self.matches_token(&TokenKind::Comma) {
                    break;
                }

                // Allow trailing comma
                if self.current().kind == TokenKind::RBracket {
                    break;
                }
            }
        }

        let end = self.current().span.range.end;
        if !self.matches_token(&TokenKind::RBracket) {
            self.error_here("expected `]` after array elements");
            return None;
        }

        Some(Expr {
            kind: ExprKind::Array { elements },
            span: Span { range: start..end },
        })
    }

    /// Parse a struct instantiation expression: `Name { field: value, ... }`.
    /// The `name` path has already been parsed; we're positioned at `{`.
    fn parse_struct_expr(&mut self, name: Vec<Ident>) -> Option<Expr> {
        let start = name
            .first()
            .map(|id| id.span.range.start)
            .unwrap_or(self.current().span.range.start);

        // Consume the `{`
        self.advance();

        let mut fields = Vec::new();

        // Parse field initializers: `field: expr, ...`
        while self.current().kind != TokenKind::RBrace && self.current().kind != TokenKind::Eof {
            let field_name = self.parse_ident("expected field name")?;

            if !self.matches_token(&TokenKind::Colon) {
                self.error_here("expected `:` after field name");
                return None;
            }

            let value = self.parse_expr()?;

            fields.push(husk_ast::FieldInit {
                name: field_name,
                value,
            });

            // Optional trailing comma
            if !self.matches_token(&TokenKind::Comma) {
                break;
            }
        }

        if !self.matches_token(&TokenKind::RBrace) {
            self.error_here("expected `}` after struct fields");
            return None;
        }

        let end = self.previous().span.range.end;

        Some(Expr {
            kind: ExprKind::Struct { name, fields },
            span: Span { range: start..end },
        })
    }
}

/// Derive a valid Husk identifier from an npm package name.
///
/// This function converts npm package names into valid Husk identifiers by:
/// - Stripping scopes (e.g., `@scope/pkg` -> `pkg`)
/// - Replacing hyphens with underscores (e.g., `lodash-es` -> `lodash_es`)
/// - Prefixing with `_` if the result starts with a digit
/// - Falling back to `"pkg"` if the result would be empty
///
/// # Examples
/// - `express` -> `express`
/// - `lodash-es` -> `lodash_es`
/// - `@scope/pkg` -> `pkg`
/// - `@scope/my-pkg` -> `my_pkg`
/// - `3d-viewer` -> `_3d_viewer`
pub fn derive_binding_from_package(package: &str) -> String {
    // If scoped package (@scope/name), use only the name part
    let name = if let Some(slash_pos) = package.rfind('/') {
        &package[slash_pos + 1..]
    } else {
        package
    };

    // Replace hyphens with underscores to make it a valid identifier
    let mut result = String::new();
    for ch in name.chars() {
        if ch == '-' {
            result.push('_');
        } else if ch.is_alphanumeric() || ch == '_' {
            result.push(ch);
        }
        // Skip other characters (like @, /)
    }

    // Ensure the identifier doesn't start with a digit
    if result
        .chars()
        .next()
        .map(|c| c.is_ascii_digit())
        .unwrap_or(false)
    {
        result.insert(0, '_');
    }

    // If empty, use a default
    if result.is_empty() {
        result = "pkg".to_string();
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn derive_binding_from_simple_package() {
        assert_eq!(derive_binding_from_package("lodash"), "lodash");
        assert_eq!(derive_binding_from_package("express"), "express");
    }

    #[test]
    fn derive_binding_from_hyphenated_package() {
        assert_eq!(derive_binding_from_package("lodash-es"), "lodash_es");
        assert_eq!(derive_binding_from_package("my-cool-pkg"), "my_cool_pkg");
    }

    #[test]
    fn derive_binding_from_scoped_package() {
        assert_eq!(derive_binding_from_package("@scope/pkg"), "pkg");
        assert_eq!(derive_binding_from_package("@myorg/my-lib"), "my_lib");
    }

    #[test]
    fn derive_binding_handles_numeric_prefix() {
        assert_eq!(derive_binding_from_package("3d-viewer"), "_3d_viewer");
    }

    #[test]
    fn parses_mod_declaration_with_identifier() {
        let src = r#"extern "js" { mod express; }"#;
        let result = parse_str(src);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let file = result.file.unwrap();
        assert_eq!(file.items.len(), 1);
        if let ItemKind::ExternBlock { items, .. } = &file.items[0].kind {
            assert_eq!(items.len(), 1);
            if let husk_ast::ExternItemKind::Mod {
                package,
                binding,
                items,
            } = &items[0].kind
            {
                assert_eq!(package, "express");
                assert_eq!(binding.name, "express");
                assert!(items.is_empty());
            } else {
                panic!("expected Mod item");
            }
        } else {
            panic!("expected ExternBlock");
        }
    }

    #[test]
    fn parses_mod_declaration_with_string_literal() {
        let src = r#"extern "js" { mod "lodash-es"; }"#;
        let result = parse_str(src);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let file = result.file.unwrap();
        if let ItemKind::ExternBlock { items, .. } = &file.items[0].kind {
            if let husk_ast::ExternItemKind::Mod {
                package,
                binding,
                items,
            } = &items[0].kind
            {
                assert_eq!(package, "lodash-es");
                assert_eq!(binding.name, "lodash_es");
                assert!(items.is_empty());
            } else {
                panic!("expected Mod item");
            }
        } else {
            panic!("expected ExternBlock");
        }
    }

    #[test]
    fn parses_mod_declaration_with_alias() {
        let src = r#"extern "js" { mod "@myorg/my-lib" as mylib; }"#;
        let result = parse_str(src);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let file = result.file.unwrap();
        if let ItemKind::ExternBlock { items, .. } = &file.items[0].kind {
            if let husk_ast::ExternItemKind::Mod {
                package,
                binding,
                items,
            } = &items[0].kind
            {
                assert_eq!(package, "@myorg/my-lib");
                assert_eq!(binding.name, "mylib");
                assert!(items.is_empty());
            } else {
                panic!("expected Mod item");
            }
        } else {
            panic!("expected ExternBlock");
        }
    }

    #[test]
    fn parses_mod_block_with_functions() {
        let src = r#"extern "js" {
            mod nanoid {
                fn nanoid() -> String;
            }
        }"#;
        let result = parse_str(src);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let file = result.file.unwrap();
        if let ItemKind::ExternBlock { items, .. } = &file.items[0].kind {
            assert_eq!(items.len(), 1);
            if let husk_ast::ExternItemKind::Mod {
                package,
                binding,
                items,
            } = &items[0].kind
            {
                assert_eq!(package, "nanoid");
                assert_eq!(binding.name, "nanoid");
                assert_eq!(items.len(), 1);
                // ModItemKind has only Fn variant (MVP scope)
                let husk_ast::ModItemKind::Fn {
                    name,
                    params,
                    ret_type,
                } = &items[0].kind;
                assert_eq!(name.name, "nanoid");
                assert!(params.is_empty());
                assert!(ret_type.is_some());
            } else {
                panic!("expected Mod item");
            }
        } else {
            panic!("expected ExternBlock");
        }
    }

    #[test]
    fn parses_format_basic() {
        let src = r#"fn main() { let s = format("hello"); }"#;
        let result = parse_str(src);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let file = result.file.unwrap();
        if let ItemKind::Fn { body, .. } = &file.items[0].kind {
            if let husk_ast::StmtKind::Let { value: Some(val), .. } = &body[0].kind {
                assert!(
                    matches!(val.kind, ExprKind::Format { .. }),
                    "expected Format expression, got {:?}",
                    val.kind
                );
            } else {
                panic!("expected Let statement with value");
            }
        } else {
            panic!("expected Fn item");
        }
    }

    #[test]
    fn parses_format_with_placeholder() {
        let src = r#"fn main() { let s = format("hello {}", name); }"#;
        let result = parse_str(src);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let file = result.file.unwrap();
        if let ItemKind::Fn { body, .. } = &file.items[0].kind {
            if let husk_ast::StmtKind::Let { value: Some(val), .. } = &body[0].kind {
                if let ExprKind::Format { format, args } = &val.kind {
                    assert_eq!(args.len(), 1);
                    // Check that we have a placeholder segment
                    let has_placeholder = format.segments.iter().any(|s| {
                        matches!(s, husk_ast::FormatSegment::Placeholder(_))
                    });
                    assert!(has_placeholder, "expected at least one placeholder");
                } else {
                    panic!("expected Format expression");
                }
            } else {
                panic!("expected Let statement with value");
            }
        } else {
            panic!("expected Fn item");
        }
    }

    #[test]
    fn parses_format_with_multiple_args() {
        let src = r#"fn main() { let s = format("{} + {} = {}", a, b, c); }"#;
        let result = parse_str(src);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let file = result.file.unwrap();
        if let ItemKind::Fn { body, .. } = &file.items[0].kind {
            if let husk_ast::StmtKind::Let { value: Some(val), .. } = &body[0].kind {
                if let ExprKind::Format { args, .. } = &val.kind {
                    assert_eq!(args.len(), 3, "expected 3 arguments");
                } else {
                    panic!("expected Format expression");
                }
            } else {
                panic!("expected Let statement with value");
            }
        } else {
            panic!("expected Fn item");
        }
    }

    #[test]
    fn parses_format_with_format_specifiers() {
        let src = r#"fn main() { let s = format("{:x} {:?}", num, val); }"#;
        let result = parse_str(src);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let file = result.file.unwrap();
        if let ItemKind::Fn { body, .. } = &file.items[0].kind {
            if let husk_ast::StmtKind::Let { value: Some(val), .. } = &body[0].kind {
                if let ExprKind::Format { format, args } = &val.kind {
                    assert_eq!(args.len(), 2);
                    // Check format specifiers
                    let placeholders: Vec<_> = format.segments.iter().filter_map(|s| {
                        if let husk_ast::FormatSegment::Placeholder(ph) = s {
                            Some(ph)
                        } else {
                            None
                        }
                    }).collect();
                    assert_eq!(placeholders.len(), 2);
                    assert_eq!(placeholders[0].spec.ty, Some('x'));
                    assert_eq!(placeholders[1].spec.ty, Some('?'));
                } else {
                    panic!("expected Format expression");
                }
            } else {
                panic!("expected Let statement with value");
            }
        } else {
            panic!("expected Fn item");
        }
    }
}
