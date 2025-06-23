use crate::{
    ast::visitor::AstVisitor,
    error::{Error, Result},
    parser::{Expr, Operator, Stmt, UnaryOp},
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
            Operator::NotEquals => "!==",
            Operator::Modulo => "%",
            Operator::LessThan => "<",
            Operator::LessThanEquals => "<=",
            Operator::GreaterThan => ">",
            Operator::GreaterThanEquals => ">=",
            Operator::And => "&&",
            Operator::Or => "||",
        };
        Ok(format!("({} {} {})", left_str, op_str, right_str))
    }

    fn visit_unary_op(&mut self, op: &UnaryOp, expr: &Expr, _span: &Span) -> Result<String> {
        let expr_str = self.visit_expr(expr)?;
        match op {
            UnaryOp::Neg => Ok(format!("(-{})", expr_str)),
            UnaryOp::Not => Ok(format!("(!{})", expr_str)),
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn transpile_expr(input: &str) -> Result<String> {
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse()?;
        
        // Extract the first expression from the parsed statements
        if let Some(Stmt::Expression(expr, _)) = stmts.first() {
            let mut transpiler = JsTranspiler::new();
            transpiler.visit_expr(expr)
        } else {
            Err(Error::new_parse("Expected expression".to_string(), Span::new(0, 0)))
        }
    }

    fn transpile_stmt(input: &str) -> Result<String> {
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse()?;
        
        // Get the first statement
        if let Some(stmt) = stmts.first() {
            let mut transpiler = JsTranspiler::new();
            transpiler.visit_stmt(stmt)
        } else {
            Err(Error::new_parse("Expected statement".to_string(), Span::new(0, 0)))
        }
    }

    fn transpile_program(input: &str) -> Result<String> {
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let program = parser.parse()?;
        let mut transpiler = JsTranspiler::new();
        transpiler.transpile(&program)
    }

    #[test]
    fn test_transpile_literals() {
        assert_eq!(transpile_expr("42").unwrap(), "42");
        assert_eq!(transpile_expr("3.14").unwrap(), "3.14");
        assert_eq!(transpile_expr("true").unwrap(), "true");
        assert_eq!(transpile_expr("false").unwrap(), "false");
        assert_eq!(transpile_expr("\"hello\"").unwrap(), "\"hello\"");
    }

    #[test]
    fn test_transpile_identifiers() {
        assert_eq!(transpile_expr("x").unwrap(), "x");
        assert_eq!(transpile_expr("myVar").unwrap(), "myVar");
    }

    #[test]
    fn test_transpile_binary_operations() {
        assert_eq!(transpile_expr("1 + 2").unwrap(), "(1 + 2)");
        assert_eq!(transpile_expr("5 - 3").unwrap(), "(5 - 3)");
        assert_eq!(transpile_expr("4 * 6").unwrap(), "(4 * 6)");
        assert_eq!(transpile_expr("10 / 2").unwrap(), "(10 / 2)");
        assert_eq!(transpile_expr("7 % 3").unwrap(), "(7 % 3)");
    }

    #[test]
    fn test_transpile_comparison_operations() {
        assert_eq!(transpile_expr("a == b").unwrap(), "(a === b)");
        assert_eq!(transpile_expr("x < y").unwrap(), "(x < y)");
        assert_eq!(transpile_expr("x <= y").unwrap(), "(x <= y)");
        assert_eq!(transpile_expr("x > y").unwrap(), "(x > y)");
        assert_eq!(transpile_expr("x >= y").unwrap(), "(x >= y)");
    }

    #[test]
    fn test_transpile_array_literal() {
        assert_eq!(transpile_expr("[1, 2, 3]").unwrap(), "[1, 2, 3]");
        assert_eq!(transpile_expr("[\"a\", \"b\"]").unwrap(), "[\"a\", \"b\"]");
        assert_eq!(transpile_expr("[]").unwrap(), "[]");
    }

    #[test]
    fn test_transpile_array_access() {
        assert_eq!(transpile_expr("arr[0]").unwrap(), "arr[0]");
        assert_eq!(transpile_expr("matrix[i]").unwrap(), "matrix[i]");
    }

    #[test]
    fn test_transpile_array_slicing() {
        assert_eq!(transpile_expr("arr[1..3]").unwrap(), "arr.slice(1, 3)");
        assert_eq!(transpile_expr("arr[1..=3]").unwrap(), "arr.slice(1, 3 + 1)");
        assert_eq!(transpile_expr("arr[2..]").unwrap(), "arr.slice(2)");
        assert_eq!(transpile_expr("arr[..5]").unwrap(), "arr.slice(0, 5)");
    }

    #[test]
    fn test_transpile_function_call() {
        assert_eq!(transpile_expr("foo()").unwrap(), "foo()");
        assert_eq!(transpile_expr("add(1, 2)").unwrap(), "add(1, 2)");
        assert_eq!(transpile_expr("println(\"hello\")").unwrap(), "println(\"hello\")");
    }

    #[test]
    fn test_transpile_member_access() {
        assert_eq!(transpile_expr("obj.field").unwrap(), "obj.field");
        assert_eq!(transpile_expr("person.name").unwrap(), "person.name");
    }

    #[test]
    fn test_transpile_assignment() {
        // Assignments need semicolons when parsed as statements
        assert_eq!(transpile_stmt("x = 5;").unwrap(), "void (x = 5)");
        assert_eq!(transpile_stmt("arr[0] = 10;").unwrap(), "void (arr[0] = 10)");
        assert_eq!(transpile_stmt("obj.field = \"value\";").unwrap(), "void (obj.field = \"value\")");
    }

    #[test]
    fn test_transpile_let_statement() {
        assert_eq!(transpile_stmt("let x = 5;").unwrap(), "let x = 5");
        assert_eq!(transpile_stmt("let name = \"Alice\";").unwrap(), "let name = \"Alice\"");
    }

    #[test]
    fn test_transpile_function_definition() {
        let input = "fn add(a: int, b: int) -> int { a + b }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("function add(a, b)"));
        assert!(result.contains("return (a + b);"));
    }

    #[test]
    fn test_transpile_while_loop() {
        let input = "while x < 10 { x = x + 1; }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("while ((x < 10))"));
        assert!(result.contains("x = (x + 1)"));
    }

    #[test]
    fn test_transpile_for_loop_range() {
        let input = "for i in 0..5 { println(i); }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("for (let i = 0; i < 5; i++)"));
        assert!(result.contains("println(i)"));
    }

    #[test]
    fn test_transpile_for_loop_array() {
        let input = "for x in arr { println(x); }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("for (const x of arr)"));
        assert!(result.contains("println(x)"));
    }

    #[test]
    fn test_transpile_infinite_loop() {
        let input = "loop { break; }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("while (true)"));
        assert!(result.contains("break"));
    }

    #[test]
    fn test_transpile_break_continue() {
        assert_eq!(transpile_stmt("break;").unwrap(), "break");
        assert_eq!(transpile_stmt("continue;").unwrap(), "continue");
    }

    #[test]
    fn test_transpile_return_statement() {
        assert_eq!(transpile_stmt("return;").unwrap(), "return");
        assert_eq!(transpile_stmt("return 42;").unwrap(), "return 42");
        assert_eq!(transpile_stmt("return x + y;").unwrap(), "return (x + y)");
    }

    #[test]
    fn test_transpile_struct_definition() {
        let input = "struct Point { x: int, y: int }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("function Point(x, y)"));
        assert!(result.contains("this.x = x;"));
        assert!(result.contains("this.y = y;"));
    }

    #[test]
    fn test_transpile_struct_instantiation() {
        let input = "Point { x: 10, y: 20 }";
        let result = transpile_expr(input).unwrap();
        assert!(result.contains("Object.create(Point.prototype)"));
        assert!(result.contains("__INSTANCE__.x = 10;"));
        assert!(result.contains("__INSTANCE__.y = 20;"));
    }

    #[test]
    fn test_transpile_enum_definition() {
        let input = "enum Option { Some(int), None }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("class Option"));
        assert!(result.contains("Option.Some = class extends Option"));
        assert!(result.contains("Option.None = class extends Option"));
        assert!(result.contains("Option.None = new Option.None()"));
    }

    #[test]
    fn test_transpile_enum_variant() {
        assert_eq!(transpile_expr("Option::None").unwrap(), "Option.None");
        assert_eq!(transpile_expr("Option::Some(5)").unwrap(), "new Option.Some(5)");
    }

    #[test]
    fn test_transpile_impl_block() {
        let input = r#"
            impl Rectangle {
                fn area(self) -> int {
                    self.width * self.height
                }
                
                fn new(w: int, h: int) -> Rectangle {
                    Rectangle { width: w, height: h }
                }
            }
        "#;
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        let mut transpiler = JsTranspiler::new();
        
        // Find the impl statement
        for stmt in &program {
            if let Stmt::Impl(_, _, _) = stmt {
                let result = transpiler.visit_stmt(stmt).unwrap();
                assert!(result.contains("Rectangle.prototype.area = function()"));
                assert!(result.contains("const self = this;"));
                assert!(result.contains("Rectangle.new = function(w, h)"));
                break;
            }
        }
    }

    #[test]
    fn test_transpile_match_expression() {
        // Match needs to be in a function context for proper parsing
        let input = r#"
            fn test_match() -> int {
                match opt {
                    Option::Some(n) => n,
                    Option::None => 0,
                }
            }
        "#;
        let result = transpile_program(input).unwrap();
        assert!(result.contains("const _matched = opt;"));
        assert!(result.contains("if (_matched instanceof Option.Some)"));
        assert!(result.contains("const n = _matched.value;"));
        assert!(result.contains("return n;"));
        assert!(result.contains("else if (_matched === Option.None)"));
        assert!(result.contains("return 0;"));
    }

    #[test]
    fn test_transpile_if_expression() {
        let input = "if x > 0 { x } else { 0 }";
        let result = transpile_expr(input).unwrap();
        assert!(result.contains("(() => {"));
        assert!(result.contains("if ((x > 0))"));
        assert!(result.contains("return x;"));
        assert!(result.contains("return 0;"));
        assert!(result.contains("})()"));
    }

    #[test]
    fn test_transpile_block_expression() {
        // Block expressions need to be parsed as complete programs
        let input = "fn test() -> int { let x = 5; x + 1 }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("let x = 5;"));
        assert!(result.contains("return (x + 1);"));
    }

    #[test]
    fn test_transpile_expression_with_semicolon() {
        let input = "x = 5;";
        let result = transpile_stmt(input).unwrap();
        assert_eq!(result, "void (x = 5)");
    }

    #[test]
    fn test_transpile_parentheses() {
        assert_eq!(transpile_expr("(1 + 2) * 3").unwrap(), "((1 + 2) * 3)");
        assert_eq!(transpile_expr("(x)").unwrap(), "x");
    }

    #[test]
    fn test_transpile_complete_program() {
        let input = r#"
            fn main() -> void {
                let x = 5;
                if x > 0 {
                    println("positive");
                } else {
                    println("non-positive");
                }
            }
        "#;
        let result = transpile_program(input).unwrap();
        assert!(result.contains("function println(...args)"));
        assert!(result.contains("function main()"));
        assert!(result.contains("let x = 5;"));
    }

    #[test]
    fn test_transpile_nested_expressions() {
        let input = "arr[if cond { 0 } else { 1 }]";
        let result = transpile_expr(input).unwrap();
        assert!(result.contains("arr[(() => {"));
        assert!(result.contains("if (cond)"));
    }

    #[test]
    fn test_transpile_method_call() {
        // Test static method call
        assert_eq!(transpile_expr("Point::new(1, 2)").unwrap(), "Point.new(1, 2)");
        
        // Test method call on dot notation (for future implementation)
        let input = "obj.method(arg)";
        let result = transpile_expr(input).unwrap();
        assert_eq!(result, "obj.method(arg)");
    }

    #[test]
    fn test_transpile_match_with_wildcard() {
        let input = r#"
            fn test_wildcard() -> string {
                match x {
                    1 => "one",
                    2 => "two",
                    _ => "other",
                }
            }
        "#;
        let result = transpile_program(input).unwrap();
        assert!(result.contains("const _matched = x;"));
        assert!(result.contains("if (true)"));
        assert!(result.contains("const _ = _matched;"));
    }

    #[test]
    fn test_transpile_for_inclusive_range() {
        let input = "for i in 1..=5 { println(i); }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("for (let i = 1; i <= 5; i++)"));
    }

    #[test]
    fn test_transpile_compound_assignment() {
        // Compound assignment needs to be parsed as a statement
        assert_eq!(transpile_stmt("x += 1;").unwrap(), "void (x += 1)");
        assert_eq!(transpile_stmt("x -= 1;").unwrap(), "void (x -= 1)");
        assert_eq!(transpile_stmt("x *= 2;").unwrap(), "void (x *= 2)");
        assert_eq!(transpile_stmt("x /= 2;").unwrap(), "void (x /= 2)");
        assert_eq!(transpile_stmt("x %= 3;").unwrap(), "void (x %= 3)");
    }

    #[test]
    fn test_transpile_function_with_return() {
        let input = r#"
            fn factorial(n: int) -> int {
                if n <= 1 {
                    return 1;
                }
                return n * factorial(n - 1);
            }
        "#;
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        let mut transpiler = JsTranspiler::new();
        
        for stmt in &program {
            if let Stmt::Function(_, _, _, _, _) = stmt {
                let result = transpiler.visit_stmt(stmt).unwrap();
                assert!(result.contains("function factorial(n)"));
                assert!(result.contains("return 1;"));
                assert!(result.contains("return (n * factorial((n - 1)));"));
                break;
            }
        }
    }

    #[test]
    fn test_transpile_unary_negation() {
        assert_eq!(transpile_expr("-5").unwrap(), "(-5)");
        assert_eq!(transpile_expr("-3.14").unwrap(), "(-3.14)");
        assert_eq!(transpile_expr("-x").unwrap(), "(-x)");
    }

    #[test]
    fn test_transpile_unary_not() {
        assert_eq!(transpile_expr("!true").unwrap(), "(!true)");
        assert_eq!(transpile_expr("!false").unwrap(), "(!false)");
        assert_eq!(transpile_expr("!flag").unwrap(), "(!flag)");
    }

    #[test]
    fn test_transpile_unary_double() {
        assert_eq!(transpile_expr("--5").unwrap(), "(-(-5))");
        assert_eq!(transpile_expr("!!true").unwrap(), "(!(!true))");
    }

    #[test]
    fn test_transpile_unary_in_expressions() {
        assert_eq!(transpile_expr("5 + -3").unwrap(), "(5 + (-3))");
        assert_eq!(transpile_expr("-5 * 2").unwrap(), "((-5) * 2)");
        assert_eq!(transpile_expr("!true == false").unwrap(), "((!true) === false)");
    }

    #[test]
    fn test_transpile_unary_with_parentheses() {
        assert_eq!(transpile_expr("-(5 + 3)").unwrap(), "(-(5 + 3))");
        assert_eq!(transpile_expr("!(x > 5)").unwrap(), "(!(x > 5))");
    }
}