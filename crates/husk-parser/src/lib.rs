//! Parser: consume tokens and produce an AST.
//!
//! This is a hand-written recursive-descent parser for the MVP syntax.

use husk_ast::{
    BinaryOp, Block, EnumVariant, EnumVariantFields, Expr, ExprKind, File, Ident, Item, ItemKind,
    Literal, LiteralKind, MatchArm, Param, Pattern, PatternKind, Span, Stmt, StmtKind, StructField,
    TypeExpr, TypeExprKind,
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
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            errors: Vec::new(),
        }
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
                    Keyword::Fn | Keyword::Struct | Keyword::Enum | Keyword::Type | Keyword::Extern,
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
        match self.current().kind {
            TokenKind::Keyword(Keyword::Fn) => self.parse_fn_item(),
            TokenKind::Keyword(Keyword::Struct) => self.parse_struct_item(),
            TokenKind::Keyword(Keyword::Enum) => self.parse_enum_item(),
            TokenKind::Keyword(Keyword::Type) => self.parse_type_alias_item(),
            TokenKind::Keyword(Keyword::Extern) => self.parse_extern_block_item(),
            TokenKind::Eof => None,
            _ => {
                self.error_here("expected item (`fn`, `struct`, `enum`, `type`, or `extern`)");
                None
            }
        }
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
                let (package, default_binding) = if let TokenKind::StringLiteral(ref s) = self.current().kind {
                    let pkg = s.clone();
                    let binding_name = derive_binding_from_package(&pkg);
                    let tok = self.advance().clone();
                    (pkg, Ident {
                        name: binding_name,
                        span: Span { range: tok.span.range.clone() },
                    })
                } else if let Some(id) = self.parse_ident("expected module name or string literal after `mod`") {
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

                if !self.matches_token(&TokenKind::Semicolon) {
                    self.error_here("expected `;` after module declaration");
                    self.synchronize_item();
                    continue;
                }

                let span = Span {
                    range: mod_start..self.previous().span.range.end,
                };
                items.push(husk_ast::ExternItem {
                    kind: husk_ast::ExternItemKind::Mod { package, binding },
                    span,
                });
                continue;
            }

            // Check for `fn` declaration
            if !self.matches_keyword(Keyword::Fn) {
                self.error_here("expected `fn` or `mod` inside extern block");
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
            kind: ItemKind::ExternBlock { abi, items },
            span,
        })
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
        while self.matches_token(&TokenKind::Colon) {
            if !self.matches_token(&TokenKind::Colon) {
                self.error_here("expected `::` after `:` in path");
                break;
            }
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

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.current().kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_let_stmt(),
            TokenKind::Keyword(Keyword::Return) => self.parse_return_stmt(),
            TokenKind::Keyword(Keyword::If) => self.parse_if_stmt(),
            TokenKind::Keyword(Keyword::While) => self.parse_while_stmt(),
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
        let cond = self.parse_expr()?;
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
        let cond = self.parse_expr()?;
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
        let expr = self.parse_expr()?;
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
        let mut expr = self.parse_comparison()?;
        loop {
            if self.matches_token(&TokenKind::EqEq) {
                let right = self.parse_comparison()?;
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
                let right = self.parse_comparison()?;
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
                // Function call
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

    fn parse_match_expr(&mut self) -> Option<Expr> {
        let match_tok = self.advance().clone(); // consume `match`
        let scrutinee = self.parse_expr()?;
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
    fn parse_primary(&mut self) -> Option<Expr> {
        let tok = self.current().clone();
        match tok.kind {
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
                // Could be a simple identifier or a path like `Enum::Variant`.
                let first = Ident {
                    name: name.clone(),
                    span: self.ast_span_from(&tok.span),
                };
                self.advance();
                let path = self.parse_path_segments(first.clone());
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
            TokenKind::Keyword(Keyword::Match) => self.parse_match_expr(),
            _ => {
                self.error_at_token(&tok, "expected expression");
                None
            }
        }
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
    if result.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
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
            if let husk_ast::ExternItemKind::Mod { package, binding } = &items[0].kind {
                assert_eq!(package, "express");
                assert_eq!(binding.name, "express");
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
            if let husk_ast::ExternItemKind::Mod { package, binding } = &items[0].kind {
                assert_eq!(package, "lodash-es");
                assert_eq!(binding.name, "lodash_es");
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
            if let husk_ast::ExternItemKind::Mod { package, binding } = &items[0].kind {
                assert_eq!(package, "@myorg/my-lib");
                assert_eq!(binding.name, "mylib");
            } else {
                panic!("expected Mod item");
            }
        } else {
            panic!("expected ExternBlock");
        }
    }
}
