//! Minimal JavaScript AST and pretty-printer for Husk codegen.
//!
//! This crate currently defines:
//! - A small JS AST (expressions, statements, modules).
//! - A pretty-printer that renders the AST to a JS source string.
//! - A minimal lowering from Husk AST to this JS AST for simple functions.
//! - Source map generation for debugging.

use husk_ast::{
    EnumVariantFields, Expr, ExprKind, ExternItemKind, File, Ident, ItemKind, LiteralKind, Param,
    Span, Stmt, StmtKind, StructField, TypeExpr, TypeExprKind,
};
use husk_runtime_js::std_preamble_js;
use sourcemap::SourceMapBuilder;

/// A JavaScript module (ES module) consisting of a list of statements.
#[derive(Debug, Clone, PartialEq)]
pub struct JsModule {
    pub body: Vec<JsStmt>,
}

/// Source span for mapping back to Husk source.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SourceSpan {
    /// Source line (0-indexed)
    pub line: u32,
    /// Source column (0-indexed)
    pub column: u32,
}

/// Compute line and column (0-indexed) from a byte offset in source text.
pub fn offset_to_line_col(source: &str, offset: usize) -> (u32, u32) {
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    (line, col)
}

/// JavaScript statements (subset).
#[derive(Debug, Clone, PartialEq)]
pub enum JsStmt {
    /// `import name from "source";`
    Import { name: String, source: String },
    /// `import { name1, name2, ... } from "source";`
    NamedImport { names: Vec<String>, source: String },
    /// `const name = require("source");`
    Require { name: String, source: String },
    /// `const { name1, name2, ... } = require("source");`
    NamedRequire { names: Vec<String>, source: String },
    /// `export { name1, name2, ... };`
    ExportNamed { names: Vec<String> },
    /// `function name(params) { body }`
    Function {
        name: String,
        params: Vec<String>,
        body: Vec<JsStmt>,
        /// Optional source span for source map generation.
        source_span: Option<SourceSpan>,
    },
    /// `return expr;`
    Return(JsExpr),
    /// `let name = expr;`
    Let { name: String, init: Option<JsExpr> },
    /// Expression statement: `expr;`
    Expr(JsExpr),
    /// `try { ... } catch (e) { ... }`
    TryCatch {
        try_block: Vec<JsStmt>,
        catch_ident: String,
        catch_block: Vec<JsStmt>,
    },
    /// `if (cond) { ... } else { ... }`
    If {
        cond: JsExpr,
        then_block: Vec<JsStmt>,
        else_block: Option<Vec<JsStmt>>,
    },
}

/// JavaScript expressions (subset).
#[derive(Debug, Clone, PartialEq)]
pub enum JsExpr {
    Ident(String),
    Number(i64),
    Bool(bool),
    String(String),
    /// Object literal: `{ key: value, ... }`.
    Object(Vec<(String, JsExpr)>),
    /// Property access: `object.property`.
    Member {
        object: Box<JsExpr>,
        property: String,
    },
    /// Assignment expression: `left = right`.
    Assignment {
        left: Box<JsExpr>,
        right: Box<JsExpr>,
    },
    /// Conditional (ternary) expression: `test ? then_branch : else_branch`.
    Conditional {
        test: Box<JsExpr>,
        then_branch: Box<JsExpr>,
        else_branch: Box<JsExpr>,
    },
    Call {
        callee: Box<JsExpr>,
        args: Vec<JsExpr>,
    },
    Binary {
        op: JsBinaryOp,
        left: Box<JsExpr>,
        right: Box<JsExpr>,
    },
    /// Immediately Invoked Function Expression: `(function() { body })()`.
    Iife {
        body: Vec<JsStmt>,
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

/// Output target style.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JsTarget {
    Esm,
    Cjs,
}

/// Lower a Husk AST file into a JS module.
///
/// This is intentionally minimal for now and only supports:
/// - Top-level functions.
/// - Simple statements (`let`, `return expr`, expression statements).
/// - Basic expressions (literals, identifiers, calls, and arithmetic/boolean binary ops).
///
/// `call_main` controls whether a zero-argument `main` function is
/// automatically invoked at the end of the module. Library-style
/// consumers should set this to `false` so `main` can be called
/// explicitly by a host.
pub fn lower_file_to_js(file: &File, call_main: bool, target: JsTarget) -> JsModule {
    lower_file_to_js_with_source(file, call_main, target, None)
}

/// Lower a Husk AST file into a JS module with source text for accurate source maps.
///
/// When `source` is provided, source spans will have accurate line/column info.
pub fn lower_file_to_js_with_source(
    file: &File,
    call_main: bool,
    target: JsTarget,
    source: Option<&str>,
) -> JsModule {
    let mut imports = Vec::new();
    let mut body = Vec::new();
    let mut has_main_entry = false;
    let mut fn_names: Vec<String> = Vec::new();

    // First pass: collect module imports
    for item in &file.items {
        if let ItemKind::ExternBlock { abi, items } = &item.kind {
            if abi == "js" {
                for ext in items {
                    if let ExternItemKind::Mod {
                        package,
                        binding,
                        items: mod_items,
                    } = &ext.kind
                    {
                        if mod_items.is_empty() {
                            // Simple default import: import binding from "package";
                            match target {
                                JsTarget::Esm => imports.push(JsStmt::Import {
                                    name: binding.name.clone(),
                                    source: package.clone(),
                                }),
                                JsTarget::Cjs => imports.push(JsStmt::Require {
                                    name: binding.name.clone(),
                                    source: package.clone(),
                                }),
                            }
                        } else {
                            // Named imports: import { fn1, fn2 } from "package";
                            let names: Vec<String> = mod_items
                                .iter()
                                .filter_map(|mi| match &mi.kind {
                                    husk_ast::ModItemKind::Fn { name, .. } => {
                                        Some(name.name.clone())
                                    }
                                })
                                .collect();
                            match target {
                                JsTarget::Esm => imports.push(JsStmt::NamedImport {
                                    names,
                                    source: package.clone(),
                                }),
                                JsTarget::Cjs => imports.push(JsStmt::NamedRequire {
                                    names,
                                    source: package.clone(),
                                }),
                            }
                        }
                    }
                }
            }
        }
    }

    // Second pass: process functions and extern functions
    for item in &file.items {
        match &item.kind {
            ItemKind::Fn {
                name,
                params,
                body: fn_body,
                ..
            } => {
                body.push(lower_fn_with_span(&name.name, params, fn_body, &item.span, source));
                fn_names.push(name.name.clone());
                if name.name == "main" && params.is_empty() {
                    has_main_entry = true;
                }
            }
            ItemKind::ExternBlock { abi, items } => {
                if abi == "js" {
                    for ext in items {
                        match &ext.kind {
                            ExternItemKind::Fn {
                                name,
                                params,
                                ret_type,
                            } => {
                                body.push(lower_extern_fn(name, params, ret_type.as_ref()));
                            }
                            ExternItemKind::Mod { .. } => {
                                // Already handled in first pass
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    if has_main_entry && call_main {
        body.push(JsStmt::Expr(JsExpr::Call {
            callee: Box::new(JsExpr::Ident("main".to_string())),
            args: Vec::new(),
        }));
    }

    // Choose export style based on whether we have imports (ESM) or not (CommonJS)
    if !fn_names.is_empty() {
        match target {
            JsTarget::Esm => body.push(JsStmt::ExportNamed { names: fn_names }),
            JsTarget::Cjs => {
                let exports_obj = JsExpr::Object(
                    fn_names
                        .into_iter()
                        .map(|name| (name.clone(), JsExpr::Ident(name)))
                        .collect(),
                );
                let assign = JsExpr::Assignment {
                    left: Box::new(JsExpr::Member {
                        object: Box::new(JsExpr::Ident("module".to_string())),
                        property: "exports".to_string(),
                    }),
                    right: Box::new(exports_obj),
                };
                body.push(JsStmt::Expr(assign));
            }
        }
    }

    // Combine imports at the top, then the rest of the body
    let mut full_body = imports;
    full_body.append(&mut body);

    JsModule { body: full_body }
}

fn lower_fn_with_span(
    name: &str,
    params: &[Param],
    body: &[Stmt],
    span: &Span,
    source: Option<&str>,
) -> JsStmt {
    let js_params: Vec<String> = params.iter().map(|p| p.name.name.clone()).collect();
    let mut js_body = Vec::new();
    for (i, stmt) in body.iter().enumerate() {
        let is_last = i + 1 == body.len();
        if is_last {
            js_body.push(lower_tail_stmt(stmt));
        } else {
            js_body.push(lower_stmt(stmt));
        }
    }

    // Convert byte offset to line/column if source is provided
    let source_span = source.map(|src| {
        let (line, column) = offset_to_line_col(src, span.range.start);
        SourceSpan { line, column }
    });

    JsStmt::Function {
        name: name.to_string(),
        params: js_params,
        body: js_body,
        source_span,
    }
}

fn lower_tail_stmt(stmt: &Stmt) -> JsStmt {
    match &stmt.kind {
        // Treat a trailing expression statement as an implicit `return expr;`,
        // to mirror Rust-style expression-bodied functions.
        StmtKind::Expr(expr) => JsStmt::Return(lower_expr(expr)),
        // For if statements in tail position, wrap branch results in returns.
        StmtKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let js_cond = lower_expr(cond);
            let then_block: Vec<JsStmt> = then_branch
                .stmts
                .iter()
                .enumerate()
                .map(|(i, s)| {
                    if i + 1 == then_branch.stmts.len() {
                        lower_tail_stmt(s)
                    } else {
                        lower_stmt(s)
                    }
                })
                .collect();
            let else_block = else_branch.as_ref().map(|else_stmt| {
                // else_branch is a Box<Stmt>, which may be another If or a Block
                match &else_stmt.kind {
                    StmtKind::Block(block) => block
                        .stmts
                        .iter()
                        .enumerate()
                        .map(|(i, s)| {
                            if i + 1 == block.stmts.len() {
                                lower_tail_stmt(s)
                            } else {
                                lower_stmt(s)
                            }
                        })
                        .collect(),
                    StmtKind::If { .. } => vec![lower_tail_stmt(else_stmt)],
                    _ => vec![lower_tail_stmt(else_stmt)],
                }
            });
            JsStmt::If {
                cond: js_cond,
                then_block,
                else_block,
            }
        }
        _ => lower_stmt(stmt),
    }
}

fn lower_extern_fn(
    name: &husk_ast::Ident,
    params: &[Param],
    ret_type: Option<&TypeExpr>,
) -> JsStmt {
    let js_params: Vec<String> = params.iter().map(|p| p.name.name.clone()).collect();
    let body = lower_extern_body(&name.name, &js_params, ret_type);
    JsStmt::Function {
        name: name.name.clone(),
        params: js_params,
        body,
        source_span: None,
    }
}

fn is_result_type(ret_type: &TypeExpr) -> bool {
    match &ret_type.kind {
        TypeExprKind::Generic { name, args } => name.name == "Result" && args.len() == 2,
        _ => false,
    }
}

fn lower_extern_body(
    name: &str,
    param_names: &[String],
    ret_type: Option<&TypeExpr>,
) -> Vec<JsStmt> {
    // Underlying JS function is expected on globalThis under the same name.
    let callee = JsExpr::Member {
        object: Box::new(JsExpr::Ident("globalThis".to_string())),
        property: name.to_string(),
    };
    let args: Vec<JsExpr> = param_names.iter().cloned().map(JsExpr::Ident).collect();
    let call = JsExpr::Call {
        callee: Box::new(callee),
        args,
    };

    if let Some(ret_ty) = ret_type {
        if is_result_type(ret_ty) {
            // Wrap in Ok/Err with try/catch.
            let ok_call = JsExpr::Call {
                callee: Box::new(JsExpr::Ident("Ok".to_string())),
                args: vec![call],
            };
            let try_block = vec![JsStmt::Return(ok_call)];

            let err_call = JsExpr::Call {
                callee: Box::new(JsExpr::Ident("Err".to_string())),
                args: vec![JsExpr::Ident("e".to_string())],
            };
            let catch_block = vec![JsStmt::Return(err_call)];

            vec![JsStmt::TryCatch {
                try_block,
                catch_ident: "e".to_string(),
                catch_block,
            }]
        } else {
            vec![JsStmt::Return(call)]
        }
    } else {
        vec![JsStmt::Expr(call)]
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
        StmtKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let js_cond = lower_expr(cond);
            let then_block: Vec<JsStmt> = then_branch.stmts.iter().map(lower_stmt).collect();
            let else_block = else_branch.as_ref().map(|else_stmt| {
                // else_branch is a Box<Stmt>, which may be another If or a Block
                match &else_stmt.kind {
                    StmtKind::Block(block) => block.stmts.iter().map(lower_stmt).collect(),
                    StmtKind::If { .. } => vec![lower_stmt(else_stmt)],
                    _ => vec![lower_stmt(else_stmt)],
                }
            });
            JsStmt::If {
                cond: js_cond,
                then_block,
                else_block,
            }
        }
        // While/Break/Continue not yet supported in codegen.
        StmtKind::While { .. } | StmtKind::Break | StmtKind::Continue => {
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
        ExprKind::Path { segments } => {
            // MVP: treat `Enum::Variant` as a tagged union value `{ tag: "Variant" }`.
            // We ignore the enum name and any intermediate segments here.
            let variant = segments
                .last()
                .map(|id| id.name.clone())
                .unwrap_or_else(|| "Unknown".to_string());
            JsExpr::Object(vec![("tag".to_string(), JsExpr::String(variant))])
        }
        ExprKind::Field { base, member } => JsExpr::Member {
            object: Box::new(lower_expr(base)),
            property: member.name.clone(),
        },
        ExprKind::MethodCall {
            receiver,
            method,
            args,
        } => JsExpr::Call {
            callee: Box::new(JsExpr::Member {
                object: Box::new(lower_expr(receiver)),
                property: method.name.clone(),
            }),
            args: args.iter().map(lower_expr).collect(),
        },
        ExprKind::Call { callee, args } => {
            // Handle built-in functions like println -> console.log
            if let ExprKind::Ident(ref id) = callee.kind {
                if id.name == "println" {
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Member {
                            object: Box::new(JsExpr::Ident("console".to_string())),
                            property: "log".to_string(),
                        }),
                        args: args.iter().map(lower_expr).collect(),
                    };
                }
            }
            JsExpr::Call {
                callee: Box::new(lower_expr(callee)),
                args: args.iter().map(lower_expr).collect(),
            }
        }
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
        ExprKind::Match { scrutinee, arms } => lower_match_expr(scrutinee, arms),
        ExprKind::Struct { name: _, fields } => {
            // Lower struct instantiation to a JS object literal.
            // `Point { x: 1, y: 2 }` -> `{ x: 1, y: 2 }`
            JsExpr::Object(
                fields
                    .iter()
                    .map(|f| (f.name.name.clone(), lower_expr(&f.value)))
                    .collect(),
            )
        }
        ExprKind::Block(block) => {
            // Lower a block expression to an IIFE (Immediately Invoked Function Expression).
            // This allows blocks to be used as expressions in JavaScript.
            let mut body: Vec<JsStmt> = block.stmts.iter().map(lower_stmt).collect();
            // Wrap the last statement in a return if it's an expression.
            if let Some(last) = body.pop() {
                match last {
                    JsStmt::Expr(expr) => body.push(JsStmt::Return(expr)),
                    other => body.push(other),
                }
            }
            JsExpr::Iife { body }
        }
        // Unary operators not yet supported in codegen.
        ExprKind::Unary { .. } => JsExpr::Ident("undefined".to_string()),
    }
}

fn lower_match_expr(scrutinee: &Expr, arms: &[husk_ast::MatchArm]) -> JsExpr {
    use husk_ast::PatternKind;

    let scrutinee_js = lower_expr(scrutinee);

    // Build nested conditional expression from the arms, folding from the end.
    // For a catch-all (wildcard or binding) arm, we treat its body as the final `else`.
    fn pattern_test(pattern: &husk_ast::Pattern, scrutinee_js: &JsExpr) -> Option<JsExpr> {
        match &pattern.kind {
            PatternKind::EnumUnit { path } => {
                // Use the last path segment as the variant tag string.
                let variant = path
                    .last()
                    .map(|id| id.name.clone())
                    .unwrap_or_else(|| "Unknown".to_string());
                let tag_access = JsExpr::Member {
                    object: Box::new(scrutinee_js.clone()),
                    property: "tag".to_string(),
                };
                Some(JsExpr::Binary {
                    op: JsBinaryOp::EqEq,
                    left: Box::new(tag_access),
                    right: Box::new(JsExpr::String(variant)),
                })
            }
            PatternKind::Wildcard | PatternKind::Binding(_) => None,
            // Tuple/struct patterns are not yet supported in codegen.
            PatternKind::EnumTuple { .. } | PatternKind::EnumStruct { .. } => None,
        }
    }

    if arms.is_empty() {
        return JsExpr::Ident("undefined".to_string());
    }

    // Start from the last arm as the innermost expression.
    let mut iter = arms.iter().rev();
    let last_arm = iter.next().unwrap();
    let mut acc = lower_expr(&last_arm.expr);

    for arm in iter {
        if let Some(test) = pattern_test(&arm.pattern, &scrutinee_js) {
            let then_expr = lower_expr(&arm.expr);
            acc = JsExpr::Conditional {
                test: Box::new(test),
                then_branch: Box::new(then_expr),
                else_branch: Box::new(acc),
            };
        } else {
            // Catch-all arm: replace accumulator with its expression.
            acc = lower_expr(&arm.expr);
        }
    }

    acc
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
    /// Import/require statements are placed at the very top, followed by the preamble,
    /// then the rest of the code.
    pub fn to_source_with_preamble(&self) -> String {
        let mut out = String::new();

        // First, output all import/require statements at the top
        let mut has_imports = false;
        for stmt in &self.body {
            if is_import_stmt(stmt) {
                write_stmt(stmt, 0, &mut out);
                out.push('\n');
                has_imports = true;
            }
        }
        if has_imports {
            out.push('\n');
        }

        // Then output the preamble
        out.push_str(std_preamble_js());
        if !out.ends_with('\n') {
            out.push('\n');
        }
        out.push('\n');

        // Finally output the rest of the code (excluding imports/requires)
        for stmt in &self.body {
            if !is_import_stmt(stmt) {
                write_stmt(stmt, 0, &mut out);
                out.push('\n');
            }
        }

        out
    }

    /// Render the module to JavaScript source with a source map.
    /// Returns (js_source, source_map_json).
    pub fn to_source_with_sourcemap(&self, source_file: &str, source_content: &str) -> (String, String) {
        let mut out = String::new();
        let mut builder = SourceMapBuilder::new(Some(source_file));

        // Add source file to the source map
        let source_id = builder.add_source(source_file);
        builder.set_source_contents(source_id, Some(source_content));

        // Count preamble lines
        let preamble = std_preamble_js();
        let preamble_lines = preamble.lines().count() as u32;

        // First, output all import/require statements at the top
        let mut has_imports = false;
        for stmt in &self.body {
            if is_import_stmt(stmt) {
                write_stmt(stmt, 0, &mut out);
                out.push('\n');
                has_imports = true;
            }
        }
        let import_lines = if has_imports {
            out.lines().count() as u32 + 1 // +1 for blank line
        } else {
            0
        };
        if has_imports {
            out.push('\n');
        }

        // Then output the preamble
        out.push_str(preamble);
        if !out.ends_with('\n') {
            out.push('\n');
        }
        out.push('\n');

        // Track line number as we output code
        let mut current_line = import_lines + preamble_lines + 1; // +1 for blank line after preamble

        // Finally output the rest of the code (excluding imports/requires)
        for stmt in &self.body {
            if !is_import_stmt(stmt) {
                // Add source mapping for functions
                if let JsStmt::Function { name, source_span: Some(span), .. } = stmt {
                    builder.add(
                        current_line,
                        0,
                        span.line,
                        span.column,
                        Some(source_file),
                        Some(name.as_str()),
                        false,
                    );
                }
                write_stmt(stmt, 0, &mut out);
                out.push('\n');
                current_line += count_newlines_in_stmt(stmt) + 1;
            }
        }

        let source_map = builder.into_sourcemap();
        let mut sm_out = Vec::new();
        source_map.to_writer(&mut sm_out).expect("failed to write source map");
        let sm_json = String::from_utf8(sm_out).expect("source map is utf8");

        (out, sm_json)
    }
}

/// Count newlines in a statement (for line tracking in source maps)
fn count_newlines_in_stmt(stmt: &JsStmt) -> u32 {
    match stmt {
        JsStmt::Function { body, .. } => {
            // function header + body lines + closing brace
            1 + body.iter().map(|s| count_newlines_in_stmt(s) + 1).sum::<u32>()
        }
        JsStmt::TryCatch { try_block, catch_block, .. } => {
            // try { + try body + } catch { + catch body + }
            2 + try_block.iter().map(|s| count_newlines_in_stmt(s) + 1).sum::<u32>()
              + catch_block.iter().map(|s| count_newlines_in_stmt(s) + 1).sum::<u32>()
        }
        JsStmt::If { then_block, else_block, .. } => {
            // if { + then body + } else { + else body + }
            let then_lines = then_block.iter().map(|s| count_newlines_in_stmt(s) + 1).sum::<u32>();
            let else_lines = else_block.as_ref().map_or(0, |eb| {
                1 + eb.iter().map(|s| count_newlines_in_stmt(s) + 1).sum::<u32>()
            });
            1 + then_lines + else_lines
        }
        _ => 0, // single-line statements
    }
}

fn indent(level: usize, out: &mut String) {
    for _ in 0..level {
        out.push_str("    ");
    }
}

/// Check if a statement is an import/require statement.
fn is_import_stmt(stmt: &JsStmt) -> bool {
    matches!(
        stmt,
        JsStmt::Import { .. }
            | JsStmt::NamedImport { .. }
            | JsStmt::Require { .. }
            | JsStmt::NamedRequire { .. }
    )
}

fn write_stmt(stmt: &JsStmt, indent_level: usize, out: &mut String) {
    match stmt {
        JsStmt::Import { name, source } => {
            indent(indent_level, out);
            out.push_str("import ");
            out.push_str(name);
            out.push_str(" from \"");
            out.push_str(&source.replace('"', "\\\""));
            out.push_str("\";");
        }
        JsStmt::NamedImport { names, source } => {
            // Universal pattern that works with both CJS and ESM packages:
            // import * as __pkg from "pkg";
            // const _pkg = __pkg.default || __pkg;
            // const { a, b, c } = _pkg;
            let safe_name = source.replace('-', "_").replace('@', "").replace('/', "_");
            let star_name = format!("__{}", safe_name);
            let pkg_name = format!("_{}", safe_name);

            indent(indent_level, out);
            out.push_str("import * as ");
            out.push_str(&star_name);
            out.push_str(" from \"");
            out.push_str(&source.replace('"', "\\\""));
            out.push_str("\";\n");

            indent(indent_level, out);
            out.push_str("const ");
            out.push_str(&pkg_name);
            out.push_str(" = ");
            out.push_str(&star_name);
            out.push_str(".default || ");
            out.push_str(&star_name);
            out.push_str(";\n");

            indent(indent_level, out);
            out.push_str("const { ");
            for (i, name) in names.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(name);
            }
            out.push_str(" } = ");
            out.push_str(&pkg_name);
            out.push(';');
        }
        JsStmt::Require { name, source } => {
            indent(indent_level, out);
            out.push_str("const ");
            out.push_str(name);
            out.push_str(" = require(\"");
            out.push_str(&source.replace('"', "\\\""));
            out.push_str("\");");
        }
        JsStmt::NamedRequire { names, source } => {
            indent(indent_level, out);
            out.push_str("const { ");
            for (i, name) in names.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(name);
            }
            out.push_str(" } = require(\"");
            out.push_str(&source.replace('"', "\\\""));
            out.push_str("\");");
        }
        JsStmt::ExportNamed { names } => {
            indent(indent_level, out);
            out.push_str("export { ");
            for (i, name) in names.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(name);
            }
            out.push_str(" };");
        }
        JsStmt::Function { name, params, body, .. } => {
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
        JsStmt::TryCatch {
            try_block,
            catch_ident,
            catch_block,
        } => {
            indent(indent_level, out);
            out.push_str("try {\n");
            for s in try_block {
                write_stmt(s, indent_level + 1, out);
                out.push('\n');
            }
            indent(indent_level, out);
            out.push_str("} catch (");
            out.push_str(catch_ident);
            out.push_str(") {\n");
            for s in catch_block {
                write_stmt(s, indent_level + 1, out);
                out.push('\n');
            }
            indent(indent_level, out);
            out.push('}');
        }
        JsStmt::If {
            cond,
            then_block,
            else_block,
        } => {
            indent(indent_level, out);
            out.push_str("if (");
            write_expr(cond, out);
            out.push_str(") {\n");
            for s in then_block {
                write_stmt(s, indent_level + 1, out);
                out.push('\n');
            }
            indent(indent_level, out);
            out.push('}');
            if let Some(else_stmts) = else_block {
                // Check if the else block contains a single If statement (else if)
                if else_stmts.len() == 1 {
                    if let JsStmt::If { .. } = &else_stmts[0] {
                        out.push_str(" else ");
                        // Write without indent since it's an else-if chain
                        write_stmt(&else_stmts[0], 0, out);
                        // Trim leading whitespace that write_stmt added
                        return;
                    }
                }
                out.push_str(" else {\n");
                for s in else_stmts {
                    write_stmt(s, indent_level + 1, out);
                    out.push('\n');
                }
                indent(indent_level, out);
                out.push('}');
            }
        }
    }
}

fn write_expr(expr: &JsExpr, out: &mut String) {
    match expr {
        JsExpr::Ident(name) => out.push_str(name),
        JsExpr::Number(n) => out.push_str(&n.to_string()),
        JsExpr::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        JsExpr::Object(props) => {
            out.push('{');
            for (i, (key, value)) in props.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(key);
                out.push_str(": ");
                write_expr(value, out);
            }
            out.push('}');
        }
        JsExpr::Member { object, property } => {
            write_expr(object, out);
            out.push('.');
            out.push_str(property);
        }
        JsExpr::String(s) => {
            out.push('"');
            out.push_str(&s.replace('"', "\\\""));
            out.push('"');
        }
        JsExpr::Assignment { left, right } => {
            write_expr(left, out);
            out.push_str(" = ");
            write_expr(right, out);
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
        JsExpr::Conditional {
            test,
            then_branch,
            else_branch,
        } => {
            out.push('(');
            write_expr(test, out);
            out.push_str(" ? ");
            write_expr(then_branch, out);
            out.push_str(" : ");
            write_expr(else_branch, out);
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
        JsExpr::Iife { body } => {
            out.push_str("(function() {\n");
            for s in body {
                write_stmt(s, 1, out);
                out.push('\n');
            }
            out.push_str("})()");
        }
    }
}

// ========== TypeScript .d.ts emission ==========

/// Convert a Husk AST file into a `.d.ts` TypeScript declaration string.
pub fn file_to_dts(file: &File) -> String {
    let mut out = String::new();

    for item in &file.items {
        match &item.kind {
            ItemKind::Struct {
                name,
                type_params,
                fields,
            } => {
                write_struct_dts(name, type_params, fields, &mut out);
                out.push('\n');
            }
            ItemKind::Enum {
                name,
                type_params,
                variants,
            } => {
                write_enum_dts(name, type_params, variants, &mut out);
                out.push('\n');
            }
            ItemKind::Fn {
                name,
                type_params,
                params,
                ret_type,
                ..
            } => {
                write_fn_dts(name, type_params, params, ret_type.as_ref(), &mut out);
                out.push('\n');
            }
            _ => {}
        }
    }

    out
}

fn write_struct_dts(name: &Ident, type_params: &[Ident], fields: &[StructField], out: &mut String) {
    out.push_str("export interface ");
    out.push_str(&name.name);
    write_type_params(type_params, out);
    out.push_str(" {\n");
    for field in fields {
        out.push_str("    ");
        out.push_str(&field.name.name);
        out.push_str(": ");
        write_type_expr(&field.ty, out);
        out.push_str(";\n");
    }
    out.push_str("}\n");
}

fn write_enum_dts(
    name: &Ident,
    type_params: &[Ident],
    variants: &[husk_ast::EnumVariant],
    out: &mut String,
) {
    out.push_str("export type ");
    out.push_str(&name.name);
    write_type_params(type_params, out);
    out.push_str(" =\n");

    for (i, variant) in variants.iter().enumerate() {
        out.push_str("  | { tag: \"");
        out.push_str(&variant.name.name);
        out.push('"');

        match &variant.fields {
            EnumVariantFields::Unit => {}
            EnumVariantFields::Tuple(tys) => {
                if tys.len() == 1 {
                    out.push_str("; value: ");
                    write_type_expr(&tys[0], out);
                } else {
                    for (idx, ty) in tys.iter().enumerate() {
                        out.push_str("; value");
                        out.push(char::from(b'0' + (idx as u8)));
                        out.push_str(": ");
                        write_type_expr(ty, out);
                    }
                }
            }
            EnumVariantFields::Struct(fields) => {
                for field in fields {
                    out.push_str("; ");
                    out.push_str(&field.name.name);
                    out.push_str(": ");
                    write_type_expr(&field.ty, out);
                }
            }
        }

        out.push_str(" }");
        if i + 1 < variants.len() {
            out.push('\n');
        } else {
            out.push_str(";\n");
        }
    }
}

fn write_fn_dts(
    name: &Ident,
    type_params: &[Ident],
    params: &[Param],
    ret_type: Option<&TypeExpr>,
    out: &mut String,
) {
    out.push_str("export function ");
    out.push_str(&name.name);
    write_type_params(type_params, out);
    out.push('(');
    for (i, param) in params.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(&param.name.name);
        out.push_str(": ");
        write_type_expr(&param.ty, out);
    }
    out.push(')');
    out.push_str(": ");
    if let Some(ret) = ret_type {
        write_type_expr(ret, out);
    } else {
        out.push_str("void");
    }
    out.push_str(";\n");
}

fn write_type_params(type_params: &[Ident], out: &mut String) {
    if !type_params.is_empty() {
        out.push('<');
        for (i, tp) in type_params.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            out.push_str(&tp.name);
        }
        out.push('>');
    }
}

fn write_type_expr(ty: &TypeExpr, out: &mut String) {
    match &ty.kind {
        TypeExprKind::Named(id) => match id.name.as_str() {
            "i32" => out.push_str("number"),
            "bool" => out.push_str("boolean"),
            "String" => out.push_str("string"),
            "()" => out.push_str("void"),
            other => out.push_str(other),
        },
        TypeExprKind::Generic { name, args } => {
            out.push_str(&name.name);
            out.push('<');
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_type_expr(arg, out);
            }
            out.push('>');
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use husk_ast::{
        ExprKind as HuskExprKind, Ident as HuskIdent, Literal as HuskLiteral,
        LiteralKind as HuskLiteralKind, MatchArm as HuskMatchArm, Pattern as HuskPattern,
        PatternKind as HuskPatternKind, Span as HuskSpan, TypeExpr as HuskTypeExpr,
        TypeExprKind as HuskTypeExprKind,
    };

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
                source_span: None,
            }],
        };

        let src = module.to_source();
        assert!(src.contains("function main"));
        assert!(src.contains("let x = a + b;"));
        assert!(src.contains("return x;"));
    }

    #[test]
    fn appends_module_exports_for_top_level_functions() {
        // file with two top-level functions: main and helper
        let span = |s: usize, e: usize| HuskSpan { range: s..e };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let main_fn = husk_ast::Item {
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("main", 0),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: Vec::new(),
            },
            span: span(0, 10),
        };
        let helper_fn = husk_ast::Item {
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("helper", 20),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: Vec::new(),
            },
            span: span(20, 30),
        };

        let file = husk_ast::File {
            items: vec![main_fn, helper_fn],
        };

        let module = lower_file_to_js(&file, true, JsTarget::Cjs);
        let src = module.to_source();

        assert!(src.contains("function main()"));
        assert!(src.contains("function helper()"));
        assert!(
            src.contains("module.exports = {main: main, helper: helper};")
                || src.contains("module.exports = {main: main, helper: helper}")
        );
    }

    #[test]
    fn prints_object_and_member_access() {
        let obj = JsExpr::Object(vec![
            ("tag".to_string(), JsExpr::String("Red".to_string())),
            ("value".to_string(), JsExpr::Number(1)),
        ]);
        let member = JsExpr::Member {
            object: Box::new(JsExpr::Ident("color".into())),
            property: "tag".to_string(),
        };

        let mut out = String::new();
        write_expr(&obj, &mut out);
        assert!(out.contains("{tag: \"Red\", value: 1}"));

        out.clear();
        write_expr(&member, &mut out);
        assert_eq!(out, "color.tag");
    }

    #[test]
    fn lowers_simple_enum_match_to_conditional() {
        // Construct a minimal AST for:
        // enum Color { Red, Blue }
        // fn f(c: Color) -> i32 {
        //     match c {
        //         Color::Red => 1,
        //         Color::Blue => 2,
        //     }
        // }

        let span = |s: usize, e: usize| HuskSpan { range: s..e };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let c_ident = ident("c", 0);
        let scrutinee = husk_ast::Expr {
            kind: HuskExprKind::Ident(c_ident.clone()),
            span: c_ident.span.clone(),
        };

        let color_ident = ident("Color", 10);
        let red_pat = HuskPattern {
            kind: HuskPatternKind::EnumUnit {
                path: vec![color_ident.clone(), ident("Red", 20)],
            },
            span: span(10, 23),
        };
        let blue_pat = HuskPattern {
            kind: HuskPatternKind::EnumUnit {
                path: vec![color_ident.clone(), ident("Blue", 30)],
            },
            span: span(24, 38),
        };

        let arm_red = HuskMatchArm {
            pattern: red_pat,
            expr: husk_ast::Expr {
                kind: HuskExprKind::Literal(HuskLiteral {
                    kind: HuskLiteralKind::Int(1),
                    span: span(40, 41),
                }),
                span: span(40, 41),
            },
        };
        let arm_blue = HuskMatchArm {
            pattern: blue_pat,
            expr: husk_ast::Expr {
                kind: HuskExprKind::Literal(HuskLiteral {
                    kind: HuskLiteralKind::Int(2),
                    span: span(50, 51),
                }),
                span: span(50, 51),
            },
        };

        let match_expr = husk_ast::Expr {
            kind: HuskExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms: vec![arm_red, arm_blue],
            },
            span: span(0, 60),
        };

        let js = lower_expr(&match_expr);
        let mut out = String::new();
        write_expr(&js, &mut out);

        // Should produce something like: (c.tag == "Red" ? 1 : 2)
        assert!(out.contains("c.tag"));
        assert!(out.contains("=="));
        assert!(out.contains("\"Red\""));
        assert!(out.contains(" ? "));
        assert!(out.contains(" : "));
    }

    #[test]
    fn adds_main_call_for_zero_arg_main_function() {
        // Build a minimal Husk file with:
        // fn main() { }
        let span = |s: usize, e: usize| HuskSpan { range: s..e };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let main_ident = ident("main", 0);
        let fn_item = husk_ast::Item {
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: main_ident.clone(),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: Vec::new(),
            },
            span: span(0, 10),
        };

        let file = husk_ast::File {
            items: vec![fn_item],
        };

        let module = lower_file_to_js(&file, true, JsTarget::Cjs);
        let src = module.to_source();

        assert!(src.contains("function main()"));
        assert!(src.contains("main();"));
    }

    #[test]
    fn emits_basic_dts_for_struct_enum_and_function() {
        // struct User { name: String, id: i32 }
        // enum Color { Red, Blue }
        // fn add(a: i32, b: i32) -> i32 { a + b }

        let span = |s: usize, e: usize| HuskSpan { range: s..e };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        // struct User { name: String, id: i32 }
        let user_ident = ident("User", 0);
        let name_field_ident = ident("name", 10);
        let id_field_ident = ident("id", 20);
        let string_ty_ident = ident("String", 30);
        let i32_ty_ident = ident("i32", 40);

        let string_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Named(string_ty_ident.clone()),
            span: string_ty_ident.span.clone(),
        };
        let i32_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Named(i32_ty_ident.clone()),
            span: i32_ty_ident.span.clone(),
        };

        let user_struct = husk_ast::Item {
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Struct {
                name: user_ident.clone(),
                type_params: Vec::new(),
                fields: vec![
                    husk_ast::StructField {
                        name: name_field_ident.clone(),
                        ty: string_ty.clone(),
                    },
                    husk_ast::StructField {
                        name: id_field_ident.clone(),
                        ty: i32_ty.clone(),
                    },
                ],
            },
            span: span(0, 50),
        };

        // enum Color { Red, Blue }
        let color_ident = ident("Color", 60);
        let enum_item = husk_ast::Item {
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Enum {
                name: color_ident.clone(),
                type_params: Vec::new(),
                variants: vec![
                    husk_ast::EnumVariant {
                        name: ident("Red", 70),
                        fields: husk_ast::EnumVariantFields::Unit,
                    },
                    husk_ast::EnumVariant {
                        name: ident("Blue", 80),
                        fields: husk_ast::EnumVariantFields::Unit,
                    },
                ],
            },
            span: span(60, 100),
        };

        // fn add(a: i32, b: i32) -> i32 { a + b }
        let add_ident = ident("add", 110);
        let a_ident = ident("a", 120);
        let b_ident = ident("b", 130);

        let a_param = husk_ast::Param {
            name: a_ident.clone(),
            ty: i32_ty.clone(),
        };
        let b_param = husk_ast::Param {
            name: b_ident.clone(),
            ty: i32_ty.clone(),
        };
        let ret_ty = i32_ty.clone();

        let add_fn = husk_ast::Item {
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: add_ident.clone(),
                type_params: Vec::new(),
                params: vec![a_param, b_param],
                ret_type: Some(ret_ty),
                body: Vec::new(),
            },
            span: span(110, 150),
        };

        let file = husk_ast::File {
            items: vec![user_struct, enum_item, add_fn],
        };

        let dts = file_to_dts(&file);

        assert!(dts.contains("export interface User"));
        assert!(dts.contains("name: string;"));
        assert!(dts.contains("id: number;"));

        assert!(dts.contains("export type Color"));
        assert!(dts.contains("{ tag: \"Red\" }"));
        assert!(dts.contains("{ tag: \"Blue\" }"));

        assert!(dts.contains("export function add"));
        assert!(dts.contains("a: number"));
        assert!(dts.contains("b: number"));
        assert!(dts.contains("): number;"));
    }

    #[test]
    fn lowers_extern_js_result_function_with_try_catch() {
        // extern "js" { fn parse(text: String) -> Result<i32, String>; }
        // Expect:
        // function parse(text) {
        //   try {
        //     return Ok(globalThis.parse(text));
        //   } catch (e) {
        //     return Err(e);
        //   }
        // }

        let span = |s: usize, e: usize| HuskSpan { range: s..e };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let name = ident("parse", 0);
        let param_ident = ident("text", 10);
        let string_ty_ident = ident("String", 20);
        let result_ident = ident("Result", 30);
        let i32_ident = ident("i32", 40);
        let err_ident = ident("String", 50);

        let string_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Named(string_ty_ident.clone()),
            span: string_ty_ident.span.clone(),
        };
        let i32_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Named(i32_ident.clone()),
            span: i32_ident.span.clone(),
        };
        let err_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Named(err_ident.clone()),
            span: err_ident.span.clone(),
        };

        let result_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Generic {
                name: result_ident.clone(),
                args: vec![i32_ty, err_ty],
            },
            span: result_ident.span.clone(),
        };

        let param = husk_ast::Param {
            name: param_ident.clone(),
            ty: string_ty,
        };

        let ext_item = husk_ast::ExternItem {
            kind: husk_ast::ExternItemKind::Fn {
                name: name.clone(),
                params: vec![param],
                ret_type: Some(result_ty),
            },
            span: span(0, 60),
        };

        let file = husk_ast::File {
            items: vec![husk_ast::Item {
                visibility: husk_ast::Visibility::Private,
                kind: husk_ast::ItemKind::ExternBlock {
                    abi: "js".to_string(),
                    items: vec![ext_item],
                },
                span: span(0, 60),
            }],
        };

        let module = lower_file_to_js(&file, true, JsTarget::Cjs);
        let src = module.to_source();

        assert!(src.contains("function parse"));
        assert!(src.contains("try {"));
        assert!(src.contains("return Ok("));
        assert!(src.contains("globalThis.parse"));
        assert!(src.contains("} catch (e) {"));
        assert!(src.contains("return Err(e);"));
    }

    #[test]
    fn generates_esm_imports_for_extern_mod_declarations() {
        // extern "js" {
        //     mod express;
        //     mod fs;
        // }
        // fn main() { }

        let span = |s: usize, e: usize| HuskSpan { range: s..e };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let express_mod = husk_ast::ExternItem {
            kind: husk_ast::ExternItemKind::Mod {
                package: "express".to_string(),
                binding: ident("express", 0),
                items: Vec::new(),
            },
            span: span(0, 10),
        };

        let fs_mod = husk_ast::ExternItem {
            kind: husk_ast::ExternItemKind::Mod {
                package: "fs".to_string(),
                binding: ident("fs", 20),
                items: Vec::new(),
            },
            span: span(20, 25),
        };

        let main_fn = husk_ast::Item {
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("main", 40),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: Vec::new(),
            },
            span: span(40, 50),
        };

        let file = husk_ast::File {
            items: vec![
                husk_ast::Item {
                    visibility: husk_ast::Visibility::Private,
                    kind: husk_ast::ItemKind::ExternBlock {
                        abi: "js".to_string(),
                        items: vec![express_mod, fs_mod],
                    },
                    span: span(0, 30),
                },
                main_fn,
            ],
        };

        let module = lower_file_to_js(&file, true, JsTarget::Esm);
        let src = module.to_source();

        // Check for ESM imports at the top
        assert!(src.contains("import express from \"express\";"));
        assert!(src.contains("import fs from \"fs\";"));

        // Check for ESM exports instead of CommonJS (explicit ESM target)
        assert!(src.contains("export { main }"));
        assert!(!src.contains("module.exports"));
    }

    #[test]
    fn esm_exports_even_without_imports_when_target_forced() {
        let span = |s: usize, e: usize| HuskSpan { range: s..e };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let main_fn = husk_ast::Item {
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("main", 0),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: Vec::new(),
            },
            span: span(0, 10),
        };

        let file = husk_ast::File {
            items: vec![main_fn],
        };
        let module = lower_file_to_js(&file, true, JsTarget::Esm);
        let src = module.to_source();

        assert!(src.contains("export { main }"));
        assert!(!src.contains("module.exports"));
    }

    #[test]
    fn generates_cjs_requires_for_extern_mod_declarations() {
        // extern "js" { mod express; } targeting CommonJS.

        let span = |s: usize, e: usize| HuskSpan { range: s..e };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let express_mod = husk_ast::ExternItem {
            kind: husk_ast::ExternItemKind::Mod {
                package: "express".to_string(),
                binding: ident("express", 0),
                items: Vec::new(),
            },
            span: span(0, 10),
        };

        let main_fn = husk_ast::Item {
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("main", 20),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: Vec::new(),
            },
            span: span(20, 30),
        };

        let file = husk_ast::File {
            items: vec![
                husk_ast::Item {
                    visibility: husk_ast::Visibility::Private,
                    kind: husk_ast::ItemKind::ExternBlock {
                        abi: "js".to_string(),
                        items: vec![express_mod],
                    },
                    span: span(0, 15),
                },
                main_fn,
            ],
        };

        let module = lower_file_to_js(&file, true, JsTarget::Cjs);
        let src = module.to_source();

        assert!(src.contains("const express = require(\"express\");"));
        assert!(src.contains("module.exports"));
        assert!(!src.contains("export {"));
    }
}
