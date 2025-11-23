//! Parser: consume tokens and produce an AST.
//!
//! This is a hand-written recursive-descent parser for the MVP syntax.

use husk_ast::{
    BinaryOp, Block, EnumVariant, EnumVariantFields, Expr, ExprKind, File, Ident, Item, ItemKind,
    Literal, LiteralKind, Param, Span, Stmt, StmtKind, StructField, TypeExpr, TypeExprKind,
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
            if !self.matches_keyword(Keyword::Fn) {
                self.error_here("expected `fn` inside extern block");
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
                self.advance();
                let ident = Ident {
                    name: name.clone(),
                    span: self.ast_span_from(&tok.span),
                };
                Some(Expr {
                    kind: ExprKind::Ident(ident.clone()),
                    span: ident.span,
                })
            }
            TokenKind::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                if !self.matches_token(&TokenKind::RParen) {
                    self.error_here("expected `)` after expression");
                }
                Some(expr)
            }
            TokenKind::Keyword(Keyword::Match) => {
                // `match` expression parsing is planned but not implemented yet.
                self.error_here("`match` expressions are not yet supported in the parser");
                self.advance();
                None
            }
            _ => {
                self.error_at_token(&tok, "expected expression");
                None
            }
        }
    }
}
