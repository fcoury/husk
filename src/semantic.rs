use std::collections::HashMap;

use crate::{
    parser::{Expr, Stmt},
    span::Span,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SemanticError {
    pub message: String,
    pub span: Span,
}

impl SemanticError {
    pub fn pretty_print(&self, code: impl Into<String>) -> String {
        let code: String = code.into();
        format!(
            "Semantic error at {}:{} - {}\n{}",
            self.span.line_number(&code),
            self.span.column_number(&code),
            self.message,
            self.span.pretty_print(&code)
        )
    }
}

impl SemanticError {
    pub fn new(message: String, span: Span) -> Self {
        SemanticError { message, span }
    }
}

pub struct SemanticAnalyzer {
    symbol_table: HashMap<String, String>, // Variable name to type mapping
    functions: HashMap<String, (Vec<String>, Span)>, // Function name to parameter types mapping
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            symbol_table: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn analyze(&mut self, stmts: &Vec<Stmt>) -> Result<(), SemanticError> {
        for stmt in stmts {
            self.analyze_stmt(stmt)?;
        }
        Ok(())
    }

    fn analyze_stmt(&mut self, stmt: &Stmt) -> Result<(), SemanticError> {
        match stmt {
            Stmt::Let(name, expr) => {
                let expr_type = self.analyze_expr(expr)?;
                self.symbol_table.insert(name.clone(), expr_type);
                Ok(())
            }
            Stmt::Function(name, params, body, span) => {
                let param_types = params.iter().map(|_| "int".to_string()).collect::<Vec<_>>(); // Assuming all params are int for simplicity
                self.functions
                    .insert(name.clone(), (param_types.clone(), *span));

                // Add parameters to the symbol table
                for param in params {
                    self.symbol_table.insert(param.clone(), "int".to_string()); // Assuming all params are int for simplicity
                }

                // Analyze function body
                for stmt in body {
                    self.analyze_stmt(stmt)?;
                }

                // Remove parameters from the symbol table after analysis
                for param in params {
                    self.symbol_table.remove(param);
                }

                Ok(())
            }
            Stmt::Expression(expr) => {
                self.analyze_expr(expr)?;
                Ok(())
            }
        }
    }

    fn analyze_expr(&self, expr: &Expr) -> Result<String, SemanticError> {
        match expr {
            Expr::Number(_, _) => Ok("int".to_string()),
            Expr::Identifier(name, span) => {
                if let Some(var_type) = self.symbol_table.get(name) {
                    Ok(var_type.clone())
                } else {
                    Err(SemanticError::new(
                        format!("Undefined variable: {}", name),
                        *span,
                    ))
                }
            }
            Expr::BinaryOp(left, _, right, span) => {
                let left_type = self.analyze_expr(left)?;
                let right_type = self.analyze_expr(right)?;
                if left_type == right_type {
                    Ok(left_type)
                } else {
                    Err(SemanticError::new(
                        "Type mismatch in binary operation".to_string(),
                        *span,
                    ))
                }
            }
            Expr::FunctionCall(name, args, span) => {
                if let Some((param_types, _func_span)) = self.functions.get(name) {
                    if args.len() != param_types.len() {
                        return Err(SemanticError::new(
                            format!("function '{}' called with wrong number of arguments", name),
                            *span,
                        ));
                    }
                    for (arg, param_type) in args.iter().zip(param_types.iter()) {
                        let arg_type = self.analyze_expr(arg)?;
                        if arg_type != *param_type {
                            return Err(SemanticError::new(
                                format!(
                                    "Type mismatch in function call {}: expected {}, found {}",
                                    name, param_type, arg_type
                                ),
                                *span,
                            ));
                        }
                    }
                    Ok("void".to_string()) // Placeholder return type, adjust as needed
                } else {
                    Err(SemanticError::new(
                        format!("Undefined function: {}", name),
                        *span,
                    ))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Operator;

    use super::*;

    #[test]
    fn test_variable_declaration() {
        let stmts = vec![
            Stmt::Let("x".to_string(), Expr::Number(5, Span { start: 4, end: 5 })),
            Stmt::Expression(Expr::Identifier("x".to_string(), Span { start: 6, end: 7 })),
        ];

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&stmts);

        assert!(result.is_ok());
    }

    #[test]
    fn test_undefined_variable() {
        let stmts = vec![Stmt::Expression(Expr::Identifier(
            "x".to_string(),
            Span { start: 0, end: 1 },
        ))];

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&stmts);

        assert!(result.is_err());
        let error = result.err().unwrap();
        assert_eq!(error.message, "Undefined variable: x");
        assert_eq!(error.span, Span { start: 0, end: 1 });
    }

    #[test]
    fn test_type_mismatch_in_binary_operation() {
        let stmts = vec![
            Stmt::Let("x".to_string(), Expr::Number(5, Span { start: 4, end: 5 })),
            Stmt::Expression(Expr::BinaryOp(
                Box::new(Expr::Identifier("x".to_string(), Span { start: 6, end: 7 })),
                Operator::Plus,
                Box::new(Expr::Identifier(
                    "y".to_string(),
                    Span { start: 10, end: 11 },
                )),
                Span { start: 8, end: 9 },
            )),
        ];

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&stmts);

        assert!(result.is_err());
        let error = result.err().unwrap();
        assert_eq!(error.message, "Undefined variable: y");
        assert_eq!(error.span, Span { start: 10, end: 11 });
    }

    #[test]
    fn test_function_declaration_and_call() {
        let stmts = vec![
            Stmt::Function(
                "add".to_string(),
                vec!["a".to_string(), "b".to_string()],
                vec![Stmt::Expression(Expr::BinaryOp(
                    Box::new(Expr::Identifier(
                        "a".to_string(),
                        Span { start: 12, end: 13 },
                    )),
                    Operator::Plus,
                    Box::new(Expr::Identifier(
                        "b".to_string(),
                        Span { start: 16, end: 17 },
                    )),
                    Span { start: 14, end: 15 },
                ))],
                Span { start: 0, end: 1 },
            ),
            Stmt::Expression(Expr::FunctionCall(
                "add".to_string(),
                vec![
                    Expr::Number(1, Span { start: 18, end: 19 }),
                    Expr::Number(2, Span { start: 20, end: 21 }),
                ],
                Span { start: 22, end: 23 },
            )),
        ];

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&stmts);

        assert!(result.is_ok());
    }

    #[test]
    fn test_function_call_with_wrong_number_of_arguments() {
        let stmts = vec![
            Stmt::Function(
                "add".to_string(),
                vec!["a".to_string(), "b".to_string()],
                vec![Stmt::Expression(Expr::BinaryOp(
                    Box::new(Expr::Identifier(
                        "a".to_string(),
                        Span { start: 12, end: 13 },
                    )),
                    Operator::Plus,
                    Box::new(Expr::Identifier(
                        "b".to_string(),
                        Span { start: 16, end: 17 },
                    )),
                    Span { start: 14, end: 15 },
                ))],
                Span { start: 0, end: 1 },
            ),
            Stmt::Expression(Expr::FunctionCall(
                "add".to_string(),
                vec![Expr::Number(1, Span { start: 18, end: 19 })],
                Span { start: 20, end: 21 },
            )),
        ];

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&stmts);

        assert!(result.is_err());
        let error = result.err().unwrap();
        assert_eq!(
            error.message,
            "function 'add' called with wrong number of arguments"
        );
        assert_eq!(error.span, Span { start: 20, end: 21 });
    }
}
