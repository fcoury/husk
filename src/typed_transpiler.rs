use crate::{
    error::Result,
    parser::Operator,
    typed_ast::{TypedExpr, TypedStmt},
};

/// Transpiler that works with typed AST
pub struct TypedTranspiler {
    indent_level: usize,
}

impl TypedTranspiler {
    pub fn new() -> Self {
        Self { indent_level: 0 }
    }
    
    pub fn transpile(&mut self, stmts: &[TypedStmt]) -> Result<String> {
        let mut output = String::new();
        output.push_str("function println(...args) { console.log(...args); }\n");
        
        for stmt in stmts {
            let js_code = self.transpile_stmt(stmt)?;
            output.push_str(&js_code);
            output.push_str(";\n");
        }
        
        Ok(output)
    }
    
    fn transpile_stmt(&self, stmt: &TypedStmt) -> Result<String> {
        match stmt {
            TypedStmt::Let { name, expr, .. } => {
                let expr_js = self.transpile_expr(expr)?;
                Ok(format!("let {} = {}", name, expr_js))
            }
            TypedStmt::Expression(expr, _) => {
                self.transpile_expr(expr)
            }
            // TODO: Implement other statements
            _ => todo!("Transpile other statement types"),
        }
    }
    
    fn transpile_expr(&self, expr: &TypedExpr) -> Result<String> {
        match expr {
            TypedExpr::Int(n, _) => Ok(n.to_string()),
            TypedExpr::Float(f, _) => Ok(f.to_string()),
            TypedExpr::String(s, _) => Ok(format!("\"{}\"", s)),
            TypedExpr::Bool(b, _) => Ok(b.to_string()),
            
            TypedExpr::Identifier(name, _, _) => Ok(name.clone()),
            
            TypedExpr::BinaryOp(left, op, right, _, _) => {
                let left_js = self.transpile_expr(left)?;
                let right_js = self.transpile_expr(right)?;
                let op_js = self.transpile_op(op);
                Ok(format!("({} {} {})", left_js, op_js, right_js))
            }
            
            // This is the key benefit - we know exactly what this is!
            TypedExpr::EnumVariant { enum_name, variant, data, .. } => {
                if let Some(data_expr) = data {
                    let data_js = self.transpile_expr(data_expr)?;
                    Ok(format!("new {}.{}({})", enum_name, variant, data_js))
                } else {
                    Ok(format!("{}.{}", enum_name, variant))
                }
            }
            
            TypedExpr::StaticMethodCall { type_name, method, args, .. } => {
                let args_js = args.iter()
                    .map(|arg| self.transpile_expr(arg))
                    .collect::<Result<Vec<_>>>()?
                    .join(", ");
                Ok(format!("{}.{}({})", type_name, method, args_js))
            }
            
            TypedExpr::FunctionCall { name, args, .. } => {
                let args_js = args.iter()
                    .map(|arg| self.transpile_expr(arg))
                    .collect::<Result<Vec<_>>>()?
                    .join(", ");
                Ok(format!("{}({})", name, args_js))
            }
            
            TypedExpr::Block(stmts, _, _) => {
                // Use IIFE pattern for blocks that return values
                let mut result = "(() => {\n".to_string();
                
                for (i, stmt) in stmts.iter().enumerate() {
                    let is_last = i == stmts.len() - 1;
                    
                    match stmt {
                        // If the last statement is an expression without semicolon,
                        // we need to return its value
                        TypedStmt::Expression(expr, false) if is_last => {
                            let expr_js = self.transpile_expr(expr)?;
                            result.push_str(&format!("  return {};\n", expr_js));
                        }
                        // Otherwise, generate the statement normally
                        _ => {
                            let stmt_js = self.transpile_stmt(stmt)?;
                            result.push_str(&format!("  {};\n", stmt_js));
                        }
                    }
                }
                
                result.push_str("})()");
                Ok(result)
            }
            
            // TODO: Implement other expressions
            _ => todo!("Transpile other expression types"),
        }
    }
    
    fn transpile_op(&self, op: &Operator) -> &'static str {
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
}