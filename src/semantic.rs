use std::collections::HashMap;

use crate::{
    error::{Error, Result},
    parser::{Expr, Stmt},
    span::Span,
};

pub struct SemanticAnalyzer {
    symbol_table: HashMap<String, String>, // Variable name to type mapping
    structs: HashMap<String, HashMap<String, String>>, // Struct name to field name to type mapping
    functions: HashMap<String, (Vec<(String, String)>, String, Span)>, // Function name to parameter types mapping
    enums: HashMap<String, HashMap<String, String>>, // Enum name to variant name to type mapping
    loop_depth: u32,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut analyzer = SemanticAnalyzer {
            symbol_table: HashMap::new(),
            structs: HashMap::new(),
            functions: HashMap::new(),
            enums: HashMap::new(),
            loop_depth: 0,
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
            Stmt::Struct(name, fields, _span) => {
                self.symbol_table.insert(name.clone(), "struct".to_string());

                let mut struct_fields = HashMap::new();
                for (field_name, field_type) in fields {
                    struct_fields.insert(field_name.clone(), field_type.clone());
                }

                self.structs.insert(name.clone(), struct_fields);
                Ok(())
            }
            Stmt::Enum(name, variants, _span) => {
                self.symbol_table.insert(name.clone(), "enum".to_string());

                let mut enum_variants = HashMap::new();
                for (name, typ) in variants {
                    enum_variants.insert(name.to_string(), typ.to_string());
                }

                self.enums.insert(name.clone(), enum_variants);
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
            Stmt::If(condition, then_block, else_block, _span) => {
                self.analyze_expr(condition)?;
                for stmt in then_block {
                    self.analyze_stmt(stmt)?;
                }
                for stmt in else_block {
                    self.analyze_stmt(stmt)?;
                }
                Ok(())
            }
            Stmt::Match(expr, arms, _span) => {
                let _expr_type = self.analyze_expr(expr)?;
                for (pattern, stmts) in arms {
                    match pattern {
                        Expr::EnumVariant {
                            name,
                            variant,
                            value,
                            span,
                        } => {
                            let enum_variants = self.enums.get(name).ok_or_else(|| {
                                Error::new_semantic(format!("Undefined enum: {}", name), *span)
                            })?;
                            if let Some(expected_type) = enum_variants.get(variant) {
                                if let Some(value) = value {
                                    if let Expr::Identifier(var_name, _) = value.as_ref() {
                                        // Add the bound variable to the symbol table
                                        self.symbol_table
                                            .insert(var_name.clone(), expected_type.clone());
                                    }
                                }
                            } else {
                                return Err(Error::new_semantic(
                                    format!("Undefined variant: {}", variant),
                                    *span,
                                ));
                            }
                        }
                        _ => {
                            return Err(Error::new_semantic(
                                "Invalid match pattern".to_string(),
                                pattern.span(),
                            ))
                        }
                    }

                    for stmt in stmts {
                        self.analyze_stmt(stmt)?;
                    }
                }
                Ok(())
            }
            Stmt::Expression(expr) => {
                self.analyze_expr(expr)?;
                Ok(())
            }
            Stmt::ForLoop(variable, iterable, body, span) => {
                let iterable_type = self.analyze_expr(iterable)?;
                if !iterable_type.starts_with("array<") && iterable_type != "range" {
                    return Err(Error::new_semantic(
                        format!(
                            "For loop iterable must be an array or range, found: {}",
                            iterable_type
                        ),
                        *span,
                    ));
                }

                let element_type = if iterable_type.starts_with("array<") {
                    iterable_type
                        .trim_start_matches("array<")
                        .trim_end_matches('>')
                        .to_string()
                } else {
                    "int".to_string() // Range elements are always integers
                };

                // Create a new scope for the loop body
                self.symbol_table.insert(variable.clone(), element_type);

                self.loop_depth += 1;
                for stmt in body {
                    self.analyze_stmt(stmt)?;
                }
                self.loop_depth -= 1;

                // Remove the loop variable from the symbol table after analyzing the body
                self.symbol_table.remove(variable);

                Ok(())
            }
            Stmt::Break(span) => {
                if self.loop_depth == 0 {
                    return Err(Error::new_semantic(
                        "'break' outside of loop".to_string(),
                        *span,
                    ));
                }
                Ok(())
            }
            Stmt::Continue(span) => {
                if self.loop_depth == 0 {
                    return Err(Error::new_semantic(
                        "'continue' outside of loop".to_string(),
                        *span,
                    ));
                }
                Ok(())
            }
        }
    }

    fn analyze_expr(&self, expr: &Expr) -> Result<String> {
        match expr {
            Expr::Int(_, _) => Ok("int".to_string()),
            Expr::Float(_, _) => Ok("float".to_string()),
            Expr::Bool(_, _) => Ok("bool".to_string()),
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
            Expr::Assign(left, right, span) => {
                let expected_type = self.analyze_expr(left)?;
                let actual_type = self.analyze_expr(right)?;

                if expected_type == actual_type {
                    Ok(expected_type)
                } else {
                    Err(Error::new_semantic(
                        format!(
                            "Type mismatch in assignment: expected {}, found {}",
                            expected_type, actual_type
                        ),
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
            Expr::StructInit(name, fields, span) => {
                let Some(symbol_type) = self.symbol_table.get(name) else {
                    return Err(Error::new_semantic(
                        format!("Undefined struct: {}", name),
                        *span,
                    ));
                };

                if symbol_type != "struct" {
                    return Err(Error::new_semantic(
                        format!("Type mismatch: expected struct, found {}", symbol_type),
                        *span,
                    ));
                }

                let struct_fields = self.structs.get(name).expect(
                    "struct exists in symbol table with type struct but not in structs hashmap",
                );

                let mut missing_fields = struct_fields
                    .keys()
                    .filter(|field| !fields.iter().find(|(name, _)| name == *field).is_some());

                if let Some(missing_field) = missing_fields.next() {
                    return Err(Error::new_semantic(
                        format!("Missing field '{}' in {}", missing_field, name),
                        *span,
                    ));
                }
                for (field_name, field_expr) in fields {
                    let found_type = self.analyze_expr(field_expr)?;
                    let Some(expected_type) = struct_fields.get(field_name) else {
                        return Err(Error::new_semantic(
                            format!("Struct {} has no field named '{}'", name, field_name),
                            *span,
                        ));
                    };

                    if found_type != *expected_type {
                        return Err(Error::new_semantic(
                            format!(
                                "Type mismatch in {name}: field '{field_name}' should be {expected_type}, found {found_type}"
                            ),
                            *span,
                        ));
                    }
                }
                Ok(name.clone())
            }
            Expr::MemberAccess(struct_expr, field_name, span) => {
                let struct_type = self.analyze_expr(struct_expr)?;

                let Some(struct_fields) = self.structs.get(&struct_type) else {
                    return Err(Error::new_semantic(
                        format!("Undefined struct: {}", struct_type),
                        *span,
                    ));
                };

                if let Some(field_type) = struct_fields.get(field_name) {
                    Ok(field_type.clone())
                } else {
                    return Err(Error::new_semantic(
                        format!("Undefined field: {}", field_name),
                        *span,
                    ));
                }
            }
            Expr::EnumVariant {
                name: _,
                variant,
                value: _,
                span: _,
            } => Ok(variant.to_string()),
            Expr::Array(elements, span) => {
                if elements.is_empty() {
                    return Ok("array".to_string());
                }
                let first_element_type = self.analyze_expr(&elements[0])?;
                for element in elements.iter().skip(1) {
                    let element_type = self.analyze_expr(element)?;
                    if element_type != first_element_type {
                        return Err(Error::new_semantic(
                            format!(
                                "Inconsistent array element types: expected {}, found {}",
                                first_element_type, element_type
                            ),
                            *span,
                        ));
                    }
                }
                Ok(format!("array<{}>", first_element_type))
            }
            Expr::ArrayIndex(array_expr, index_expr, span) => {
                let array_type = self.analyze_expr(array_expr)?;
                let index_type = self.analyze_expr(index_expr)?;

                if !array_type.starts_with("array<") {
                    return Err(Error::new_semantic(
                        format!(
                            "Cannot perform array access on non-array type: {}",
                            array_type
                        ),
                        *span,
                    ));
                }

                if index_type != "int" {
                    return Err(Error::new_semantic(
                        format!("Array index must be of type int, found: {}", index_type),
                        *span,
                    ));
                }

                // Extract the element type from array<T>
                let element_type = array_type
                    .trim_start_matches("array<")
                    .trim_end_matches('>');
                Ok(element_type.to_string())
            }
            Expr::Range(start, end, _, span) => {
                if let Some(start) = start {
                    let start_type = self.analyze_expr(start)?;
                    if start_type != "int" {
                        return Err(Error::new_semantic(
                            format!("Range start must be of type int, found: {}", start_type),
                            *span,
                        ));
                    }
                }
                if let Some(end) = end {
                    let end_type = self.analyze_expr(end)?;
                    if end_type != "int" {
                        return Err(Error::new_semantic(
                            format!("Range end must be of type int, found: {}", end_type),
                            *span,
                        ));
                    }
                }
                Ok("range".to_string())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Lexer, Parser};

    use super::*;

    fn analyze(code: impl Into<String>) -> Result<()> {
        let code = code.into();

        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&ast)
    }

    #[test]
    fn test_break_outside_loop() {
        let result = analyze("break;");

        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("'break' outside of loop"));
    }

    #[test]
    fn test_break_outside_loop_false_positive() {
        let result = analyze(
            r#"
                for i in 1..10 {
                  if i == 3 {
                    break;
                  }
                  println(i)
                }
            "#,
        );

        assert!(result.is_ok());
    }

    #[test]
    fn test_continue_outside_loop() {
        let result = analyze("continue;");

        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("'continue' outside of loop"));
    }

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
