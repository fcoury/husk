//! Minimal JavaScript AST and pretty-printer for Husk codegen.
//!
//! This crate currently defines:
//! - A small JS AST (expressions, statements, modules).
//! - A pretty-printer that renders the AST to a JS source string.
//! - A minimal lowering from Husk AST to this JS AST for simple functions.

use husk_ast::{Expr, ExprKind, File, ItemKind, LiteralKind, Param, Stmt, StmtKind};
use husk_runtime_js::std_preamble_js;

/// A JavaScript module (ES module) consisting of a list of statements.
#[derive(Debug, Clone, PartialEq)]
pub struct JsModule {
    pub body: Vec<JsStmt>,
}

/// JavaScript statements (subset).
#[derive(Debug, Clone, PartialEq)]
pub enum JsStmt {
    /// `function name(params) { body }`
    Function {
        name: String,
        params: Vec<String>,
        body: Vec<JsStmt>,
    },
    /// `return expr;`
    Return(JsExpr),
    /// `let name = expr;`
    Let { name: String, init: Option<JsExpr> },
    /// Expression statement: `expr;`
    Expr(JsExpr),
}

/// JavaScript expressions (subset).
#[derive(Debug, Clone, PartialEq)]
pub enum JsExpr {
    Ident(String),
    Number(i64),
    Bool(bool),
    String(String),
    Call {
        callee: Box<JsExpr>,
        args: Vec<JsExpr>,
    },
    Binary {
        op: JsBinaryOp,
        left: Box<JsExpr>,
        right: Box<JsExpr>,
    },
}

/// Binary operators in JS (subset we need).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JsBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    EqEq,
    NotEq,
    Lt,
    Gt,
    Le,
    Ge,
}

/// Lower a Husk AST file into a JS module.
///
/// This is intentionally minimal for now and only supports:
/// - Top-level functions.
/// - Simple statements (`let`, `return expr`, expression statements).
/// - Basic expressions (literals, identifiers, calls, and arithmetic/boolean binary ops).
pub fn lower_file_to_js(file: &File) -> JsModule {
    let mut body = Vec::new();
    for item in &file.items {
        if let ItemKind::Fn {
            name,
            params,
            body: fn_body,
            ..
        } = &item.kind
        {
            body.push(lower_fn(&name.name, params, fn_body));
        }
    }
    JsModule { body }
}

fn lower_fn(name: &str, params: &[Param], body: &[Stmt]) -> JsStmt {
    let js_params: Vec<String> = params.iter().map(|p| p.name.name.clone()).collect();
    let js_body = body.iter().map(lower_stmt).collect();
    JsStmt::Function {
        name: name.to_string(),
        params: js_params,
        body: js_body,
    }
}

fn lower_stmt(stmt: &Stmt) -> JsStmt {
    match &stmt.kind {
        StmtKind::Let {
            mutable: _,
            name,
            ty: _,
            value,
        } => JsStmt::Let {
            name: name.name.clone(),
            init: value.as_ref().map(lower_expr),
        },
        StmtKind::Expr(expr) | StmtKind::Semi(expr) => JsStmt::Expr(lower_expr(expr)),
        StmtKind::Return { value } => {
            let expr = value
                .as_ref()
                .map(lower_expr)
                // Represent `return;` as `return undefined;` for now.
                .unwrap_or_else(|| JsExpr::Ident("undefined".to_string()));
            JsStmt::Return(expr)
        }
        StmtKind::Block(block) => {
            // Flatten block into a single function body-like statement sequence.
            // For now we represent it as an expression statement of the last
            // expression, discarding internal structure. This is a placeholder
            // until we add proper JS block statements.
            if let Some(last) = block.stmts.last() {
                lower_stmt(last)
            } else {
                JsStmt::Expr(JsExpr::Ident("undefined".to_string()))
            }
        }
        // Control-flow constructs are not yet supported in codegen.
        StmtKind::If { .. } | StmtKind::While { .. } | StmtKind::Break | StmtKind::Continue => {
            JsStmt::Expr(JsExpr::Ident("undefined".to_string()))
        }
    }
}

fn lower_expr(expr: &Expr) -> JsExpr {
    match &expr.kind {
        ExprKind::Literal(lit) => match &lit.kind {
            LiteralKind::Int(n) => JsExpr::Number(*n),
            LiteralKind::Bool(b) => JsExpr::Bool(*b),
            LiteralKind::String(s) => JsExpr::String(s.clone()),
        },
        ExprKind::Ident(id) => JsExpr::Ident(id.name.clone()),
        ExprKind::Call { callee, args } => JsExpr::Call {
            callee: Box::new(lower_expr(callee)),
            args: args.iter().map(lower_expr).collect(),
        },
        ExprKind::Binary { op, left, right } => {
            use husk_ast::BinaryOp::*;
            let js_op = match op {
                Add => JsBinaryOp::Add,
                Sub => JsBinaryOp::Sub,
                Mul => JsBinaryOp::Mul,
                Div => JsBinaryOp::Div,
                Eq => JsBinaryOp::EqEq,
                NotEq => JsBinaryOp::NotEq,
                Lt => JsBinaryOp::Lt,
                Gt => JsBinaryOp::Gt,
                Le => JsBinaryOp::Le,
                Ge => JsBinaryOp::Ge,
                And | Or => {
                    // Logical operators not yet mapped; approximate as `&&`/`||` by
                    // reusing equality operators is incorrect, so for now mark as `==`
                    // until we extend JsBinaryOp. This is intentionally conservative.
                    JsBinaryOp::EqEq
                }
            };
            JsExpr::Binary {
                op: js_op,
                left: Box::new(lower_expr(left)),
                right: Box::new(lower_expr(right)),
            }
        }
        // Not yet supported in codegen.
        ExprKind::Field { .. }
        | ExprKind::MethodCall { .. }
        | ExprKind::Unary { .. }
        | ExprKind::Match { .. }
        | ExprKind::Block(_) => JsExpr::Ident("undefined".to_string()),
    }
}

impl JsModule {
    /// Render the module to a JavaScript source string.
    pub fn to_source(&self) -> String {
        let mut out = String::new();
        for stmt in &self.body {
            write_stmt(stmt, 0, &mut out);
            out.push('\n');
        }
        out
    }

    /// Render the module to a JavaScript source string with the Husk preamble.
    pub fn to_source_with_preamble(&self) -> String {
        let mut out = String::new();
        out.push_str(std_preamble_js());
        if !out.ends_with('\n') {
            out.push('\n');
        }
        out.push('\n');
        out.push_str(&self.to_source());
        out
    }
}

fn indent(level: usize, out: &mut String) {
    for _ in 0..level {
        out.push_str("    ");
    }
}

fn write_stmt(stmt: &JsStmt, indent_level: usize, out: &mut String) {
    match stmt {
        JsStmt::Function { name, params, body } => {
            indent(indent_level, out);
            out.push_str("function ");
            out.push_str(name);
            out.push('(');
            for (i, p) in params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(p);
            }
            out.push_str(") {\n");
            for s in body {
                write_stmt(s, indent_level + 1, out);
                out.push('\n');
            }
            indent(indent_level, out);
            out.push('}');
        }
        JsStmt::Return(expr) => {
            indent(indent_level, out);
            out.push_str("return ");
            write_expr(expr, out);
            out.push(';');
        }
        JsStmt::Let { name, init } => {
            indent(indent_level, out);
            out.push_str("let ");
            out.push_str(name);
            if let Some(expr) = init {
                out.push_str(" = ");
                write_expr(expr, out);
            }
            out.push(';');
        }
        JsStmt::Expr(expr) => {
            indent(indent_level, out);
            write_expr(expr, out);
            out.push(';');
        }
    }
}

fn write_expr(expr: &JsExpr, out: &mut String) {
    match expr {
        JsExpr::Ident(name) => out.push_str(name),
        JsExpr::Number(n) => out.push_str(&n.to_string()),
        JsExpr::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        JsExpr::String(s) => {
            out.push('"');
            out.push_str(&s.replace('"', "\\\""));
            out.push('"');
        }
        JsExpr::Call { callee, args } => {
            write_expr(callee, out);
            out.push('(');
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_expr(arg, out);
            }
            out.push(')');
        }
        JsExpr::Binary { op, left, right } => {
            write_expr(left, out);
            out.push(' ');
            out.push_str(match op {
                JsBinaryOp::Add => "+",
                JsBinaryOp::Sub => "-",
                JsBinaryOp::Mul => "*",
                JsBinaryOp::Div => "/",
                JsBinaryOp::EqEq => "==",
                JsBinaryOp::NotEq => "!=",
                JsBinaryOp::Lt => "<",
                JsBinaryOp::Gt => ">",
                JsBinaryOp::Le => "<=",
                JsBinaryOp::Ge => ">=",
            });
            out.push(' ');
            write_expr(right, out);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prints_simple_function() {
        let module = JsModule {
            body: vec![JsStmt::Function {
                name: "main".to_string(),
                params: vec!["a".into(), "b".into()],
                body: vec![
                    JsStmt::Let {
                        name: "x".to_string(),
                        init: Some(JsExpr::Binary {
                            op: JsBinaryOp::Add,
                            left: Box::new(JsExpr::Ident("a".into())),
                            right: Box::new(JsExpr::Ident("b".into())),
                        }),
                    },
                    JsStmt::Return(JsExpr::Ident("x".into())),
                ],
            }],
        };

        let src = module.to_source();
        assert!(src.contains("function main"));
        assert!(src.contains("let x = a + b;"));
        assert!(src.contains("return x;"));
    }
}
