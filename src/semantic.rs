use std::collections::HashMap;

use crate::{
    error::{Error, Result},
    parser::{Expr, Stmt},
    span::Span,
};

pub struct SemanticAnalyzer {
    symbol_table: HashMap<String, String>, // Variable name to type mapping
    functions: HashMap<String, (Vec<(String, String)>, String, Span)>, // Function name to parameter types mapping
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut analyzer = SemanticAnalyzer {
            symbol_table: HashMap::new(),
            functions: HashMap::new(),
        };
        analyzer.init_standard_library();
        analyzer
    }

    fn init_standard_library(&mut self) {
        self.functions.insert(
            "print".to_string(),
            (vec![], "void".to_string(), Span { start: 0, end: 0 }),
        );
        self.functions.insert(
            "println".to_string(),
            (vec![], "void".to_string(), Span { start: 0, end: 0 }),
        );
    }

    pub fn analyze(&mut self, stmts: &Vec<Stmt>) -> Result<()> {
        for stmt in stmts {
            self.analyze_stmt(stmt)?;
        }
        Ok(())
    }

    fn analyze_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Let(name, expr, _span) => {
                let expr_type = self.analyze_expr(expr)?;
                self.symbol_table.insert(name.clone(), expr_type);
                Ok(())
            }
            Stmt::Function(name, params, return_type, body, span) => {
                let param_types: Vec<(String, String)> = params.clone();
                self.functions.insert(
                    name.clone(),
                    (param_types.clone(), return_type.clone(), *span),
                );

                // Add parameters to the symbol table
                for (param, param_type) in params {
                    self.symbol_table.insert(param.clone(), param_type.clone());
                }

                // Analyze function body
                for stmt in body {
                    self.analyze_stmt(stmt)?;
                }

                // Check if return type matches the last expression in the body (if any)
                if let Some(Stmt::Expression(expr)) = body.last() {
                    let expr_type = self.analyze_expr(expr)?;
                    if &expr_type != return_type {
                        return Err(Error::new_semantic(
                            format!(
                                "Function {} return type mismatch: expected {}, found {}",
                                name, return_type, expr_type
                            ),
                            *span,
                        ));
                    }
                }

                // Remove parameters from the symbol table after analysis
                for (param, _) in params {
                    self.symbol_table.remove(param);
                }

                Ok(())
            }
            Stmt::ReturnExpression(expr) | Stmt::Expression(expr) => {
                self.analyze_expr(expr)?;
                Ok(())
            }
        }
    }

    fn analyze_expr(&self, expr: &Expr) -> Result<String> {
        match expr {
            Expr::Int(_, _) => Ok("int".to_string()),
            Expr::Float(_, _) => Ok("float".to_string()),
            Expr::String(_, _) => Ok("string".to_string()),
            Expr::Identifier(name, span) => {
                if let Some(var_type) = self.symbol_table.get(name) {
                    Ok(var_type.clone())
                } else {
                    Err(Error::new_semantic(
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
                    Err(Error::new_semantic(
                        "Type mismatch in binary operation".to_string(),
                        *span,
                    ))
                }
            }
            Expr::FunctionCall(name, args, span) => {
                if let Some((param_types, return_type, _func_span)) = self.functions.get(name) {
                    if name == "print" || name == "println" {
                        // Allow any number and type of arguments for print and println
                        Ok(return_type.clone())
                    } else {
                        if args.len() != param_types.len() {
                            return Err(Error::new_semantic(
                                format!("Function {} called with wrong number of arguments", name),
                                *span,
                            ));
                        }
                        for (_, (arg, (param_name, param_type))) in
                            args.iter().zip(param_types.iter()).enumerate()
                        {
                            let arg_type = self.analyze_expr(arg)?;
                            if arg_type != *param_type {
                                return Err(Error::new_semantic(
                                format!(
                                    "Type mismatch in function call {}: expected {} for parameter {}, found {}",
                                    name, param_type, param_name, arg_type
                                ),
                                *span,
                            ));
                            }
                        }
                        Ok(return_type.clone())
                    }
                } else {
                    Err(Error::new_semantic(
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
    // use super::*;

    // #[test]
    // fn test_variable_declaration() {
    //     let stmts = vec![
    //         Stmt::Let("x".to_string(), Expr::Number(5, Span { start: 4, end: 5 })),
    //         Stmt::Expression(Expr::Identifier("x".to_string(), Span { start: 6, end: 7 })),
    //     ];
    //
    //     let mut analyzer = SemanticAnalyzer::new();
    //     let result = analyzer.analyze(&stmts);
    //
    //     assert!(result.is_ok());
    // }

    // #[test]
    // fn test_undefined_variable() {
    //     let stmts = vec![Stmt::Expression(Expr::Identifier(
    //         "x".to_string(),
    //         Span { start: 0, end: 1 },
    //     ))];
    //
    //     let mut analyzer = SemanticAnalyzer::new();
    //     let result = analyzer.analyze(&stmts);
    //
    //     assert!(result.is_err());
    //     let error = result.err().unwrap();
    //     assert_eq!(error.message, "Undefined variable: x");
    //     assert_eq!(error.span, Span { start: 0, end: 1 });
    // }

    // #[test]
    // fn test_type_mismatch_in_binary_operation() {
    //     let stmts = vec![
    //         Stmt::Let("x".to_string(), Expr::Number(5, Span { start: 4, end: 5 })),
    //         Stmt::Expression(Expr::BinaryOp(
    //             Box::new(Expr::Identifier("x".to_string(), Span { start: 6, end: 7 })),
    //             Operator::Plus,
    //             Box::new(Expr::Identifier(
    //                 "y".to_string(),
    //                 Span { start: 10, end: 11 },
    //             )),
    //             Span { start: 8, end: 9 },
    //         )),
    //     ];
    //
    //     let mut analyzer = SemanticAnalyzer::new();
    //     let result = analyzer.analyze(&stmts);
    //
    //     assert!(result.is_err());
    //     let error = result.err().unwrap();
    //     assert_eq!(error.message, "Undefined variable: y");
    //     assert_eq!(error.span, Span { start: 10, end: 11 });
    // }

    // #[test]
    // fn test_function_declaration_and_call() {
    //     let stmts = vec![
    //         Stmt::Function(
    //             "add".to_string(),
    //             vec!["a".to_string(), "b".to_string()],
    //             vec![Stmt::Expression(Expr::BinaryOp(
    //                 Box::new(Expr::Identifier(
    //                     "a".to_string(),
    //                     Span { start: 12, end: 13 },
    //                 )),
    //                 Operator::Plus,
    //                 Box::new(Expr::Identifier(
    //                     "b".to_string(),
    //                     Span { start: 16, end: 17 },
    //                 )),
    //                 Span { start: 14, end: 15 },
    //             ))],
    //             Span { start: 0, end: 1 },
    //         ),
    //         Stmt::Expression(Expr::FunctionCall(
    //             "add".to_string(),
    //             vec![
    //                 Expr::Number(1, Span { start: 18, end: 19 }),
    //                 Expr::Number(2, Span { start: 20, end: 21 }),
    //             ],
    //             Span { start: 22, end: 23 },
    //         )),
    //     ];
    //
    //     let mut analyzer = SemanticAnalyzer::new();
    //     let result = analyzer.analyze(&stmts);
    //
    //     assert!(result.is_ok());
    // }

    // #[test]
    // fn test_function_call_with_wrong_number_of_arguments() {
    //     let stmts = vec![
    //         Stmt::Function(
    //             "add".to_string(),
    //             vec!["a".to_string(), "b".to_string()],
    //             vec![Stmt::Expression(Expr::BinaryOp(
    //                 Box::new(Expr::Identifier(
    //                     "a".to_string(),
    //                     Span { start: 12, end: 13 },
    //                 )),
    //                 Operator::Plus,
    //                 Box::new(Expr::Identifier(
    //                     "b".to_string(),
    //                     Span { start: 16, end: 17 },
    //                 )),
    //                 Span { start: 14, end: 15 },
    //             ))],
    //             Span { start: 0, end: 1 },
    //         ),
    //         Stmt::Expression(Expr::FunctionCall(
    //             "add".to_string(),
    //             vec![Expr::Number(1, Span { start: 18, end: 19 })],
    //             Span { start: 20, end: 21 },
    //         )),
    //     ];
    //
    //     let mut analyzer = SemanticAnalyzer::new();
    //     let result = analyzer.analyze(&stmts);
    //
    //     assert!(result.is_err());
    //     let error = result.err().unwrap();
    //     assert_eq!(
    //         error.message,
    //         "function 'add' called with wrong number of arguments"
    //     );
    //     assert_eq!(error.span, Span { start: 20, end: 21 });
    // }
}
