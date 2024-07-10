use crate::parser::{Expr, Operator, Stmt};

pub struct JsTranspiler;

impl JsTranspiler {
    pub fn new() -> Self {
        JsTranspiler
    }

    pub fn generate(&self, stmts: &[Stmt]) -> String {
        let mut output = String::new();
        output.push_str("function println(...args) { console.log(...args); }\n");

        for stmt in stmts {
            output.push_str(&self.generate_stmt(stmt));
            output.push_str(";\n");
        }
        output
    }

    fn generate_stmt(&self, stmt: &Stmt) -> String {
        match stmt {
            Stmt::Let(name, expr, _) => format!("let {} = {};", name, self.generate_expr(expr)),
            Stmt::Function(name, params, _, body, _) => {
                let params_str = params
                    .iter()
                    .map(|(name, _)| name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                let body_str = body
                    .iter()
                    .map(|s| self.generate_stmt(s))
                    .collect::<Vec<_>>()
                    .join("\n");
                format!("function {}({}) {{\n{}\n}}", name, params_str, body_str)
            }
            Stmt::Enum(name, variants, _) => self.generate_enum(name, variants),
            Stmt::Expression(expr) => self.generate_expr(expr),
            Stmt::Match(expr, cases, _) => self.generate_match(expr, cases),
            Stmt::If(condition, body, else_body, _) => {
                let condition_str = self.generate_expr(condition);
                let body_str = self.generate_body(body);
                let else_str = if else_body.len() > 0 {
                    format!(" else {{\n{}\n}}", self.generate_body(else_body))
                } else {
                    String::new()
                };
                format!("if ({}) {{\n{}\n}}{}", condition_str, body_str, else_str)
            }
            Stmt::ForLoop(var, iter, body, _) => match iter {
                Expr::Range(start, end, inclusive, _) => match (start, end) {
                    (Some(start), Some(end)) => {
                        let start_str = self.generate_expr(start);
                        let end_str = self.generate_expr(end);
                        let op = if *inclusive { "<=" } else { "<" };
                        let body_str = body
                            .iter()
                            .map(|s| self.generate_stmt(s))
                            .collect::<Vec<_>>()
                            .join("\n");
                        format!(
                            "for (let {var} = {start_str}; {var} {op} {end_str}; {var}++) {{\n{body_str}\n}}",
                        )
                    }
                    _ => "/* Invalid range */".to_string(),
                },
                _ => {
                    let iter_str = self.generate_expr(iter);
                    let body_str = body
                        .iter()
                        .map(|s| self.generate_stmt(s))
                        .collect::<Vec<_>>()
                        .join("\n");
                    format!("for (const {} of {}) {{\n{}\n}}", var, iter_str, body_str)
                }
            },
            Stmt::Loop(body, _) => {
                let body_str = body
                    .iter()
                    .map(|s| self.generate_stmt(s))
                    .collect::<Vec<_>>()
                    .join("\n");
                format!("while (true) {{\n{}\n}}", body_str)
            }
            Stmt::While(condition, body, _) => {
                let condition_str = self.generate_expr(condition);
                let body_str = body
                    .iter()
                    .map(|s| self.generate_stmt(s))
                    .collect::<Vec<_>>()
                    .join("\n");
                format!("while ({}) {{\n{}\n}}", condition_str, body_str)
            }
            Stmt::Continue(_) => "continue;".to_string(),
            Stmt::Break(_) => "break;".to_string(),
            Stmt::Struct(_name, _fields, _) => {
                // does nothing

                // let fields_str = fields
                //     .iter()
                //     .map(|(name, ty)| format!("{}: {}", name, ty))
                //     .collect::<Vec<_>>()
                //     .join(", ");
                // format!("const {} = {{ {} }};", name, fields_str)

                String::new()
            }
            Stmt::Impl(_, _, _) => todo!(),
        }
    }

    fn generate_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Int(n, _) => n.to_string(),
            Expr::Float(f, _) => f.to_string(),
            Expr::String(s, _) => format!("\"{}\"", s),
            Expr::Bool(b, _) => b.to_string(),
            Expr::Identifier(name, _) => name.clone(),
            Expr::BinaryOp(left, op, right, _) => {
                format!(
                    "({} {} {})",
                    self.generate_expr(left),
                    self.generate_op(op),
                    self.generate_expr(right)
                )
            }
            Expr::EnumVariantOrMethodCall {
                target,
                call,
                args,
                span: _,
            } => {
                if let Some(value) = args.get(0) {
                    format!("new {}.{}({})", target, call, self.generate_expr(value))
                } else {
                    format!("{}.{}", target, call)
                }
            }
            Expr::FunctionCall(name, args, _) => {
                let args_str = args
                    .iter()
                    .map(|arg| self.generate_expr(arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", name, args_str)
            }
            Expr::CompoundAssign(name, op, expr, _) => {
                format!(
                    "{} {}= {}",
                    name,
                    self.generate_op(op),
                    self.generate_expr(expr)
                )
            }
            Expr::Array(items, _) => {
                let items_str = items
                    .iter()
                    .map(|item| self.generate_expr(item))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", items_str)
            }
            Expr::ArrayIndex(array, index, _) => match index.as_ref() {
                Expr::Range(start, end, inclusive, _) => {
                    let mut end_str = String::new();
                    if let Some(end) = end {
                        end_str = self.generate_expr(&*end);
                        if *inclusive {
                            end_str = format!("{} + 1", end_str);
                        }
                    }

                    match (start, end) {
                        (Some(start), Some(_)) => {
                            let start_str = self.generate_expr(&*start);
                            format!(
                                "{}.slice({start_str}, {end_str})",
                                self.generate_expr(array),
                                start_str = start_str,
                                end_str = end_str
                            )
                        }
                        (Some(start), None) => {
                            let start_str = self.generate_expr(&*start);
                            format!(
                                "{}.slice({start_str})",
                                self.generate_expr(array),
                                start_str = start_str
                            )
                        }
                        (None, Some(_)) => {
                            format!(
                                "{}.slice(0, {end_str})",
                                self.generate_expr(array),
                                end_str = end_str
                            )
                        }
                        (None, None) => format!("{}", self.generate_expr(array)),
                    }
                }
                _ => format!(
                    "{}[{}]",
                    self.generate_expr(array),
                    self.generate_expr(index)
                ),
            },
            Expr::StructInit(_name, fields, _) => {
                // FIXME: this generates anonymous objects, maybe not be what we want?
                let fields_str = fields
                    .iter()
                    .map(|(name, value)| format!("{}: {}", name, self.generate_expr(value)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{ {} }}", fields_str)
            }
            Expr::MemberAccess(expr, member, _) => {
                format!("{}.{}", self.generate_expr(expr), member)
            }
            Expr::Assign(into, from, _) => format!(
                "{} = {}",
                self.generate_expr(into),
                self.generate_expr(from)
            ),
            // Add more expression types here...
            e => format!("/* Unsupported expression {:?} */", e),
        }
    }

    fn generate_op(&self, op: &Operator) -> &'static str {
        match op {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::Equals => "===",
            Operator::Modulo => "%",
            Operator::LessThan => "<",
            Operator::LessThanEquals => "<=",
            Operator::GreaterThan => ">",
            Operator::GreaterThanEquals => ">=",
        }
    }

    fn generate_body(&self, stmts: &[Stmt]) -> String {
        let mut res = String::new();
        for (i, stmt) in stmts.iter().enumerate() {
            let stmt_str = self.generate_stmt(stmt);
            if i < stmts.len() - 1 {
                res.push_str(&format!("{};", stmt_str));
            } else {
                res.push_str(&format!("return {};", stmt_str));
            }
        }

        res
    }

    fn generate_match(&self, expr: &Expr, cases: &[(Expr, Vec<Stmt>)]) -> String {
        let expr_str = self.generate_expr(expr);
        let cases_str = cases
            .iter()
            .map(|(pattern, stmts)| {
                let (condition, binding) = self.generate_match_condition(expr, pattern);
                let stmts_str = stmts
                    .iter()
                    .map(|s| self.generate_stmt(s))
                    .collect::<Vec<_>>()
                    .join("\n");
                format!("if ({}) {{\n{}{}\n}}", condition, binding, stmts_str)
            })
            .collect::<Vec<_>>()
            .join(" else ");
        format!(
            "(() => {{\nconst _matched = {};\n{}\n}})();",
            expr_str, cases_str
        )
    }

    fn generate_match_condition(&self, _expr: &Expr, pattern: &Expr) -> (String, String) {
        match pattern {
            Expr::EnumVariantOrMethodCall {
                target, call, args, ..
            } => {
                if let Some(value) = args.get(0) {
                    if let Expr::Identifier(bind_name, _) = value {
                        (
                            format!("_matched instanceof {}.{}", target, call),
                            format!("const {} = _matched.value;\n", bind_name),
                        )
                    } else {
                        (
                            format!("_matched instanceof {}.{}", target, call),
                            String::new(),
                        )
                    }
                } else {
                    (format!("_matched === {}.{}", target, call), String::new())
                }
            }
            _ => (
                format!("_matched === {}", self.generate_expr(pattern)),
                String::new(),
            ),
        }
    }

    fn generate_enum(&self, name: &str, variants: &[(String, String)]) -> String {
        let mut output = String::new();
        output.push_str(&format!("const {} = {{\n", name));

        for (variant, value_type) in variants {
            if value_type == "unit" {
                output.push_str(&format!("  {}: Symbol(\"{}\"),\n", variant, variant));
            } else {
                output.push_str(&format!("  {}: class {}_{} {{\n", variant, name, variant));
                output.push_str(&format!("    constructor(value) {{\n"));
                output.push_str(&format!("      this.value = value;\n"));
                output.push_str(&format!("    }}\n"));
                output.push_str(&format!("  }},\n"));
            }
        }

        output.push_str("}");
        output
    }
}