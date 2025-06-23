use crate::{
    ast::visitor::AstVisitor,
    error::{Error, Result},
    parser::{Expr, Operator, Stmt},
    span::Span,
};

pub struct JsTranspiler {
    indent_level: usize,
}

impl JsTranspiler {
    pub fn new() -> Self {
        Self { indent_level: 0 }
    }
    
    pub fn generate(&mut self, stmts: &[Stmt]) -> Result<String> {
        self.transpile(stmts)
    }

    pub fn transpile(&mut self, stmts: &[Stmt]) -> Result<String> {
        let mut output = String::new();
        output.push_str("function println(...args) { console.log(...args); }\n");
        
        for stmt in stmts {
            let js_code = self.visit_stmt(stmt)?;
            output.push_str(&js_code);
            output.push_str(";\n");
        }
        
        Ok(output)
    }

    fn indent(&self) -> String {
        "  ".repeat(self.indent_level)
    }

    fn generate_body(&mut self, stmts: &[Stmt]) -> Result<String> {
        let mut result = String::new();
        for (i, stmt) in stmts.iter().enumerate() {
            let stmt_str = self.visit_stmt(stmt)?;
            let is_last = i == stmts.len() - 1;
            
            // Check if this is a control flow statement that shouldn't be returned
            let is_control_flow = stmt_str.contains("break") || stmt_str.contains("continue") || stmt_str.contains("return");
            
            // Check if this is an expression statement without semicolon (should be returned)
            let should_return = is_last && !is_control_flow && matches!(stmt, Stmt::Expression(_, false));
            
            if should_return {
                result.push_str(&format!("return {};", stmt_str));
            } else {
                result.push_str(&format!("{};", stmt_str));
            }
        }
        Ok(result)
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
                    (
                        format!("_matched === {}.{}", target, call),
                        String::new(),
                    )
                }
            }
            Expr::Identifier(var_name, _) => {
                (
                    "true".to_string(),
                    format!("const {} = _matched;\n", var_name),
                )
            }
            _ => ("true".to_string(), String::new()),
        }
    }

    fn generate_enum(&self, name: &str, variants: &[(String, String)]) -> String {
        let mut output = String::new();
        
        // Generate base enum class
        output.push_str(&format!("class {} {{\n", name));
        output.push_str("  constructor(variant, value) {\n");
        output.push_str("    this.variant = variant;\n");
        output.push_str("    this.value = value;\n");
        output.push_str("  }\n");
        output.push_str("}\n");
        
        // Generate variant constructors
        for (variant, _) in variants {
            output.push_str(&format!("{}.{} = class extends {} {{", name, variant, name));
            output.push_str(&format!("constructor(value) {{ super('{}', value); }}", variant));
            output.push_str("};\n");
            
            // Generate static instance for variants without data
            output.push_str(&format!("{}.{} = new {}.{}();\n", name, variant, name, variant));
        }
        
        output
    }
}

impl AstVisitor<String> for JsTranspiler {
    type Error = Error;

    // ===== Expression visit methods =====
    
    fn visit_int(&mut self, value: i64, _span: &Span) -> Result<String> {
        Ok(value.to_string())
    }

    fn visit_float(&mut self, value: f64, _span: &Span) -> Result<String> {
        Ok(value.to_string())
    }

    fn visit_string(&mut self, value: &str, _span: &Span) -> Result<String> {
        Ok(format!("\"{}\"", value))
    }

    fn visit_bool(&mut self, value: bool, _span: &Span) -> Result<String> {
        Ok(value.to_string())
    }

    fn visit_identifier(&mut self, name: &str, _span: &Span) -> Result<String> {
        Ok(name.to_string())
    }

    fn visit_binary_op(&mut self, left: &Expr, op: &Operator, right: &Expr, _span: &Span) -> Result<String> {
        let left_str = self.visit_expr(left)?;
        let right_str = self.visit_expr(right)?;
        let op_str = match op {
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
        };
        Ok(format!("({} {} {})", left_str, op_str, right_str))
    }

    fn visit_assign(&mut self, left: &Expr, right: &Expr, _span: &Span) -> Result<String> {
        let left_str = self.visit_expr(left)?;
        let right_str = self.visit_expr(right)?;
        Ok(format!("{} = {}", left_str, right_str))
    }

    fn visit_compound_assign(&mut self, left: &Expr, op: &Operator, right: &Expr, _span: &Span) -> Result<String> {
        let left_str = self.visit_expr(left)?;
        let right_str = self.visit_expr(right)?;
        let op_str = match op {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::Modulo => "%",
            _ => return Err(Error::new_transpile(
                format!("Invalid compound assignment operator: {:?}", op),
                *_span,
            )),
        };
        Ok(format!("{} {}= {}", left_str, op_str, right_str))
    }

    fn visit_function_call(&mut self, name: &str, args: &[Expr], _span: &Span) -> Result<String> {
        let args_str = args
            .iter()
            .map(|arg| self.visit_expr(arg))
            .collect::<Result<Vec<_>>>()?
            .join(", ");
        
        // Handle method calls with dot notation
        if name.contains('.') {
            let (obj, method) = name.split_once('.').unwrap();
            Ok(format!("{}.{}({})", obj, method, args_str))
        } else {
            Ok(format!("{}({})", name, args_str))
        }
    }

    fn visit_struct_init(&mut self, name: &str, fields: &[(String, Expr)], _span: &Span) -> Result<String> {
        let mut result = String::new();
        result.push_str("(function() {");
        result.push_str(&format!(
            "const __INSTANCE__ = Object.create({}.prototype);",
            name
        ));
        
        for (field_name, field_expr) in fields {
            let field_value = self.visit_expr(field_expr)?;
            result.push_str(&format!(
                "__INSTANCE__.{} = {};",
                field_name,
                field_value
            ));
        }
        
        result.push_str("return __INSTANCE__;");
        result.push_str("})()");
        Ok(result)
    }

    fn visit_member_access(&mut self, object: &Expr, field: &str, _span: &Span) -> Result<String> {
        let obj_str = self.visit_expr(object)?;
        Ok(format!("{}.{}", obj_str, field))
    }

    fn visit_enum_variant_or_method_call(&mut self, target: &Expr, call: &str, args: &[Expr], _span: &Span) -> Result<String> {
        let target_str = self.visit_expr(target)?;
        
        if args.is_empty() {
            Ok(format!("{}.{}", target_str, call))
        } else {
            let args_str = args
                .iter()
                .map(|arg| self.visit_expr(arg))
                .collect::<Result<Vec<_>>>()?
                .join(", ");
            // Check if it's a static method call (like Point::new) or enum variant
            // For now, we'll assume static methods don't need 'new' prefix
            if call == "new" || call.chars().next().unwrap().is_lowercase() {
                Ok(format!("{}.{}({})", target_str, call, args_str))
            } else {
                Ok(format!("new {}.{}({})", target_str, call, args_str))
            }
        }
    }

    fn visit_array(&mut self, elements: &[Expr], _span: &Span) -> Result<String> {
        let elements_str = elements
            .iter()
            .map(|elem| self.visit_expr(elem))
            .collect::<Result<Vec<_>>>()?
            .join(", ");
        Ok(format!("[{}]", elements_str))
    }

    fn visit_array_index(&mut self, array: &Expr, index: &Expr, _span: &Span) -> Result<String> {
        let array_str = self.visit_expr(array)?;
        
        match index {
            Expr::Range(start, end, inclusive, _) => {
                let mut end_str = String::new();
                if let Some(end_expr) = end {
                    end_str = self.visit_expr(end_expr)?;
                    if *inclusive {
                        end_str = format!("{} + 1", end_str);
                    }
                }
                
                match (start, end) {
                    (Some(start_expr), Some(_)) => {
                        let start_str = self.visit_expr(start_expr)?;
                        Ok(format!("{}.slice({}, {})", array_str, start_str, end_str))
                    }
                    (Some(start_expr), None) => {
                        let start_str = self.visit_expr(start_expr)?;
                        Ok(format!("{}.slice({})", array_str, start_str))
                    }
                    (None, Some(_)) => {
                        Ok(format!("{}.slice(0, {})", array_str, end_str))
                    }
                    (None, None) => Ok(array_str),
                }
            }
            _ => {
                let index_str = self.visit_expr(index)?;
                Ok(format!("{}[{}]", array_str, index_str))
            }
        }
    }

    fn visit_range(&mut self, _start: Option<&Expr>, _end: Option<&Expr>, _inclusive: bool, _span: &Span) -> Result<String> {
        // Ranges are handled in context (for loops, array slicing)
        Ok("/* range expression */".to_string())
    }

    fn visit_block(&mut self, stmts: &[Stmt], _span: &Span) -> Result<String> {
        // In JavaScript, we can use an IIFE (Immediately Invoked Function Expression)
        // to create a block that returns a value
        let mut result = "(() => {\n".to_string();
        
        self.indent_level += 1;
        
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            let indent = self.indent();
            
            match stmt {
                // If the last statement is an expression without semicolon,
                // we need to return its value
                Stmt::Expression(expr, false) if is_last => {
                    let expr_str = self.visit_expr(expr)?;
                    result.push_str(&format!("{}return {};\n", indent, expr_str));
                }
                // Otherwise, generate the statement normally
                _ => {
                    let stmt_str = self.visit_stmt(stmt)?;
                    result.push_str(&format!("{}{}\n", indent, stmt_str));
                }
            }
        }
        
        self.indent_level -= 1;
        result.push_str(&format!("{}}})()", self.indent()));
        
        Ok(result)
    }

    // ===== Statement visit methods =====

    fn visit_let(&mut self, name: &str, expr: &Expr, _span: &Span) -> Result<String> {
        let value = self.visit_expr(expr)?;
        Ok(format!("let {} = {}", name, value))
    }

    fn visit_function(&mut self, name: &str, params: &[(String, String)], _return_type: &str, body: &[Stmt], _span: &Span) -> Result<String> {
        let params_str = params
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>()
            .join(", ");
        
        self.indent_level += 1;
        let body_str = self.generate_body(body)?;
        self.indent_level -= 1;
        
        Ok(format!("function {}({}) {{\n{}\n}}", name, params_str, body_str))
    }


    fn visit_while(&mut self, condition: &Expr, body: &[Stmt], _span: &Span) -> Result<String> {
        let condition_str = self.visit_expr(condition)?;
        
        self.indent_level += 1;
        let body_str = body
            .iter()
            .map(|stmt| self.visit_stmt(stmt))
            .collect::<Result<Vec<_>>>()?
            .join("\n");
        self.indent_level -= 1;
        
        Ok(format!("while ({}) {{\n{}\n}}", condition_str, body_str))
    }

    fn visit_loop(&mut self, body: &[Stmt], _span: &Span) -> Result<String> {
        self.indent_level += 1;
        let body_str = body
            .iter()
            .map(|stmt| self.visit_stmt(stmt))
            .collect::<Result<Vec<_>>>()?
            .join("\n");
        self.indent_level -= 1;
        
        Ok(format!("while (true) {{\n{}\n}}", body_str))
    }

    fn visit_for_loop(&mut self, var: &str, iterable: &Expr, body: &[Stmt], _span: &Span) -> Result<String> {
        match iterable {
            Expr::Range(start, end, inclusive, _) => {
                match (start, end) {
                    (Some(start_expr), Some(end_expr)) => {
                        let start_str = self.visit_expr(start_expr)?;
                        let end_str = self.visit_expr(end_expr)?;
                        let op = if *inclusive { "<=" } else { "<" };
                        
                        self.indent_level += 1;
                        let body_str = body
                            .iter()
                            .map(|stmt| self.visit_stmt(stmt))
                            .collect::<Result<Vec<_>>>()?
                            .join("\n");
                        self.indent_level -= 1;
                        
                        Ok(format!(
                            "for (let {} = {}; {} {} {}; {}++) {{\n{}\n}}",
                            var, start_str, var, op, end_str, var, body_str
                        ))
                    }
                    _ => Ok("/* Invalid range */".to_string()),
                }
            }
            _ => {
                let iter_str = self.visit_expr(iterable)?;
                
                self.indent_level += 1;
                let body_str = body
                    .iter()
                    .map(|stmt| self.visit_stmt(stmt))
                    .collect::<Result<Vec<_>>>()?
                    .join("\n");
                self.indent_level -= 1;
                
                Ok(format!("for (const {} of {}) {{\n{}\n}}", var, iter_str, body_str))
            }
        }
    }

    fn visit_break(&mut self, _span: &Span) -> Result<String> {
        Ok("break".to_string())
    }

    fn visit_continue(&mut self, _span: &Span) -> Result<String> {
        Ok("continue".to_string())
    }

    fn visit_return(&mut self, expr: Option<&Expr>, _span: &Span) -> Result<String> {
        match expr {
            Some(return_expr) => {
                let expr_js = self.visit_expr(return_expr)?;
                Ok(format!("return {}", expr_js))
            }
            None => Ok("return".to_string()),
        }
    }

    fn visit_expression_stmt(&mut self, expr: &Expr, has_semicolon: bool) -> Result<String> {
        let expr_js = self.visit_expr(expr)?;
        
        // If the expression has a semicolon, it's a statement that shouldn't return a value
        // If no semicolon, it can return its value (for block expressions)
        if has_semicolon {
            // Use void operator to ensure no return value in expression context
            Ok(format!("void ({})", expr_js))
        } else {
            Ok(expr_js)
        }
    }

    fn visit_struct(&mut self, name: &str, fields: &[(String, String)], _span: &Span) -> Result<String> {
        let params = fields
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>()
            .join(", ");
        
        let mut result = String::new();
        result.push_str(&format!("function {}({}) {{", name, params));
        
        for (field_name, _) in fields {
            result.push_str(&format!("this.{} = {};", field_name, field_name));
        }
        
        result.push_str("}");
        Ok(result)
    }

    fn visit_enum(&mut self, name: &str, variants: &[(String, String)], _span: &Span) -> Result<String> {
        Ok(self.generate_enum(name, variants))
    }

    fn visit_impl(&mut self, struct_name: &str, methods: &[Stmt], span: &Span) -> Result<String> {
        let mut output = String::new();
        
        for method in methods {
            let Stmt::Function(name, params, _return_type, body, _) = method else {
                return Err(Error::new_transpile(
                    "Impl methods must be function definitions",
                    *span,
                ));
            };
            
            let result = if !params.is_empty() && params[0].0 == "self" {
                // Instance method
                let fn_params = params[1..]
                    .iter()
                    .map(|(name, _)| name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                
                self.indent_level += 1;
                let fn_body = self.generate_body(body)?;
                self.indent_level -= 1;
                
                let fn_body = format!("const self = this;\n{}", fn_body);
                format!(
                    "{}.prototype.{} = function({}) {{\n{}\n}};\n",
                    struct_name, name, fn_params, fn_body
                )
            } else {
                // Static method
                let fn_params = params
                    .iter()
                    .map(|(name, _)| name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                
                self.indent_level += 1;
                let fn_body = self.generate_body(body)?;
                self.indent_level -= 1;
                
                format!(
                    "{}.{} = function({}) {{\n{}\n}};\n",
                    struct_name, name, fn_params, fn_body
                )
            };
            
            output.push_str(&result);
        }
        
        Ok(output)
    }

    fn visit_match(&mut self, expr: &Expr, arms: &[(Expr, Vec<Stmt>)], _span: &Span) -> Result<String> {
        let expr_str = self.visit_expr(expr)?;
        
        let cases_str = arms
            .iter()
            .map(|(pattern, stmts)| {
                let (condition, binding) = self.generate_match_condition(expr, pattern);
                
                self.indent_level += 1;
                // Use generate_body to ensure proper return handling
                let body_str = self.generate_body(stmts)?;
                self.indent_level -= 1;
                
                Ok(format!(
                    "if ({}) {{\n{}{}\n}}",
                    condition, binding, body_str
                ))
            })
            .collect::<Result<Vec<_>>>()?
            .join(" else ");
        
        Ok(format!(
            "(() => {{\nconst _matched = {};\n{}\n}})()",
            expr_str, cases_str
        ))
    }

    fn visit_if_expr(&mut self, condition: &Expr, then_block: &[Stmt], else_block: &[Stmt], _span: &Span) -> Result<String> {
        let condition_str = self.visit_expr(condition)?;
        
        // Generate an IIFE for the if expression
        let mut result = "(() => {\n".to_string();
        result.push_str(&format!("  if ({}) {{\n", condition_str));
        
        self.indent_level += 2;
        let then_str = self.generate_body(then_block)?;
        self.indent_level -= 2;
        
        result.push_str(&format!("    {}\n", then_str));
        result.push_str("  }");
        
        if !else_block.is_empty() {
            result.push_str(" else {\n");
            
            self.indent_level += 2;
            let else_str = self.generate_body(else_block)?;
            self.indent_level -= 2;
            
            result.push_str(&format!("    {}\n", else_str));
            result.push_str("  }");
        }
        
        result.push_str("\n})()");
        Ok(result)
    }
}