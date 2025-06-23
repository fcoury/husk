#[cfg(test)]
mod tests {
    use super::super::visitor::AstVisitor;
    use crate::{
        parser::{Expr, Stmt, Operator, UnaryOp, UsePath, UseItems},
        span::Span,
    };
    use std::collections::HashMap;

    #[derive(Debug, PartialEq)]
    enum VisitError {
        TestError(String),
    }

    /// A mock visitor that tracks which methods are called
    struct TrackingVisitor {
        visits: HashMap<String, usize>,
        should_fail: Option<String>,
    }

    impl TrackingVisitor {
        fn new() -> Self {
            Self {
                visits: HashMap::new(),
                should_fail: None,
            }
        }

        fn with_failure(method: &str) -> Self {
            Self {
                visits: HashMap::new(),
                should_fail: Some(method.to_string()),
            }
        }

        fn track(&mut self, method: &str) -> Result<String, VisitError> {
            *self.visits.entry(method.to_string()).or_insert(0) += 1;
            
            if self.should_fail.as_ref() == Some(&method.to_string()) {
                Err(VisitError::TestError(format!("Simulated error in {}", method)))
            } else {
                Ok(format!("visited_{}", method))
            }
        }

        fn was_called(&self, method: &str) -> bool {
            self.visits.contains_key(method)
        }

        fn call_count(&self, method: &str) -> usize {
            self.visits.get(method).copied().unwrap_or(0)
        }
    }

    impl AstVisitor<String> for TrackingVisitor {
        type Error = VisitError;

        fn visit_int(&mut self, value: i64, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("int_{}", value))
        }

        fn visit_float(&mut self, value: f64, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("float_{}", value))
        }

        fn visit_bool(&mut self, value: bool, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("bool_{}", value))
        }

        fn visit_string(&mut self, value: &str, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("string_{}", value))
        }

        fn visit_identifier(&mut self, name: &str, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("identifier_{}", name))
        }

        fn visit_array(&mut self, elements: &[Expr], _span: &Span) -> Result<String, Self::Error> {
            self.track("array")?;
            for elem in elements {
                self.visit_expr(elem)?;
            }
            Ok("visited_array".to_string())
        }

        fn visit_array_index(&mut self, array: &Expr, index: &Expr, _span: &Span) -> Result<String, Self::Error> {
            self.track("array_index")?;
            self.visit_expr(array)?;
            self.visit_expr(index)?;
            Ok("visited_array_index".to_string())
        }

        fn visit_range(&mut self, start: Option<&Expr>, end: Option<&Expr>, inclusive: bool, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("range_inclusive_{}", inclusive))?;
            if let Some(s) = start {
                self.visit_expr(s)?;
            }
            if let Some(e) = end {
                self.visit_expr(e)?;
            }
            Ok("visited_range".to_string())
        }

        fn visit_binary_op(&mut self, left: &Expr, op: &Operator, right: &Expr, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("binary_op_{:?}", op))?;
            self.visit_expr(left)?;
            self.visit_expr(right)?;
            Ok("visited_binary_op".to_string())
        }

        fn visit_unary_op(&mut self, op: &UnaryOp, expr: &Expr, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("unary_op_{:?}", op))?;
            self.visit_expr(expr)?;
            Ok("visited_unary_op".to_string())
        }

        fn visit_assign(&mut self, left: &Expr, right: &Expr, _span: &Span) -> Result<String, Self::Error> {
            self.track("assign")?;
            self.visit_expr(left)?;
            self.visit_expr(right)?;
            Ok("visited_assign".to_string())
        }

        fn visit_compound_assign(&mut self, left: &Expr, op: &Operator, right: &Expr, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("compound_assign_{:?}", op))?;
            self.visit_expr(left)?;
            self.visit_expr(right)?;
            Ok("visited_compound_assign".to_string())
        }

        fn visit_function_call(&mut self, name: &str, args: &[Expr], _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("function_call_{}", name))?;
            for arg in args {
                self.visit_expr(arg)?;
            }
            Ok("visited_function_call".to_string())
        }

        fn visit_struct_init(&mut self, name: &str, fields: &[(String, Expr)], _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("struct_init_{}", name))?;
            for (_, expr) in fields {
                self.visit_expr(expr)?;
            }
            Ok("visited_struct_init".to_string())
        }

        fn visit_member_access(&mut self, object: &Expr, field: &str, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("member_access_{}", field))?;
            self.visit_expr(object)?;
            Ok("visited_member_access".to_string())
        }

        fn visit_enum_variant_or_method_call(&mut self, target: &Expr, call: &str, args: &[Expr], _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("enum_variant_or_method_{}", call))?;
            self.visit_expr(target)?;
            for arg in args {
                self.visit_expr(arg)?;
            }
            Ok("visited_enum_variant_or_method".to_string())
        }

        fn visit_block(&mut self, stmts: &[Stmt], _span: &Span) -> Result<String, Self::Error> {
            self.track("block")?;
            for stmt in stmts {
                self.visit_stmt(stmt)?;
            }
            Ok("visited_block".to_string())
        }

        fn visit_if_expr(&mut self, condition: &Expr, then_block: &[Stmt], else_block: &[Stmt], _span: &Span) -> Result<String, Self::Error> {
            self.track("if_expr")?;
            self.visit_expr(condition)?;
            for stmt in then_block {
                self.visit_stmt(stmt)?;
            }
            for stmt in else_block {
                self.visit_stmt(stmt)?;
            }
            Ok("visited_if_expr".to_string())
        }

        fn visit_let(&mut self, name: &str, expr: &Expr, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("let_{}", name))?;
            self.visit_expr(expr)?;
            Ok("visited_let".to_string())
        }

        fn visit_function(&mut self, name: &str, _generic_params: &[String], _params: &[(String, String)], _return_type: &str, body: &[Stmt], _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("function_{}", name))?;
            for stmt in body {
                self.visit_stmt(stmt)?;
            }
            Ok("visited_function".to_string())
        }

        fn visit_struct(&mut self, name: &str, _generic_params: &[String], _fields: &[(String, String)], _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("struct_{}", name))
        }

        fn visit_enum(&mut self, name: &str, _generic_params: &[String], _variants: &[(String, String)], _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("enum_{}", name))
        }

        fn visit_impl(&mut self, struct_name: &str, methods: &[Stmt], _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("impl_{}", struct_name))?;
            for method in methods {
                self.visit_stmt(method)?;
            }
            Ok("visited_impl".to_string())
        }

        fn visit_match(&mut self, expr: &Expr, arms: &[(Expr, Vec<Stmt>)], _span: &Span) -> Result<String, Self::Error> {
            self.track("match")?;
            self.visit_expr(expr)?;
            for (pattern, stmts) in arms {
                self.visit_expr(pattern)?;
                for stmt in stmts {
                    self.visit_stmt(stmt)?;
                }
            }
            Ok("visited_match".to_string())
        }

        fn visit_for_loop(&mut self, variable: &str, iterable: &Expr, body: &[Stmt], _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("for_loop_{}", variable))?;
            self.visit_expr(iterable)?;
            for stmt in body {
                self.visit_stmt(stmt)?;
            }
            Ok("visited_for_loop".to_string())
        }

        fn visit_while(&mut self, condition: &Expr, body: &[Stmt], _span: &Span) -> Result<String, Self::Error> {
            self.track("while")?;
            self.visit_expr(condition)?;
            for stmt in body {
                self.visit_stmt(stmt)?;
            }
            Ok("visited_while".to_string())
        }

        fn visit_loop(&mut self, body: &[Stmt], _span: &Span) -> Result<String, Self::Error> {
            self.track("loop")?;
            for stmt in body {
                self.visit_stmt(stmt)?;
            }
            Ok("visited_loop".to_string())
        }

        fn visit_break(&mut self, _span: &Span) -> Result<String, Self::Error> {
            self.track("break")
        }

        fn visit_continue(&mut self, _span: &Span) -> Result<String, Self::Error> {
            self.track("continue")
        }

        fn visit_return(&mut self, expr: Option<&Expr>, _span: &Span) -> Result<String, Self::Error> {
            self.track("return")?;
            if let Some(e) = expr {
                self.visit_expr(e)?;
            }
            Ok("visited_return".to_string())
        }

        fn visit_expression_stmt(&mut self, expr: &Expr, has_semicolon: bool) -> Result<String, Self::Error> {
            self.track(&format!("expression_stmt_semicolon_{}", has_semicolon))?;
            self.visit_expr(expr)?;
            Ok("visited_expression_stmt".to_string())
        }

        fn visit_use(&mut self, path: &UsePath, items: &UseItems, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("use_{:?}_{:?}", path.prefix, items))?;
            Ok("visited_use".to_string())
        }
        
        fn visit_extern_function(&mut self, name: &str, _generic_params: &[String], _params: &[(String, String)], _return_type: &str, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("extern_function_{}", name))
        }
        
        fn visit_extern_mod(&mut self, name: &str, _items: &[crate::parser::ExternItem], _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("extern_mod_{}", name))
        }
        
        fn visit_async_function(&mut self, name: &str, _generic_params: &[String], _params: &[(String, String)], _return_type: &str, body: &[Stmt], _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("async_function_{}", name))?;
            for stmt in body {
                self.visit_stmt(stmt)?;
            }
            Ok("visited_async_function".to_string())
        }
        
        fn visit_match_expr(&mut self, expr: &Expr, arms: &[(Expr, Vec<Stmt>)], _span: &Span) -> Result<String, Self::Error> {
            self.track("match_expr")?;
            self.visit_expr(expr)?;
            for (pattern, stmts) in arms {
                self.visit_expr(pattern)?;
                for stmt in stmts {
                    self.visit_stmt(stmt)?;
                }
            }
            Ok("visited_match_expr".to_string())
        }
        
        fn visit_await(&mut self, expr: &Expr, _span: &Span) -> Result<String, Self::Error> {
            self.track("await")?;
            self.visit_expr(expr)?;
            Ok("visited_await".to_string())
        }

        fn visit_try(&mut self, expr: &Expr, _span: &Span) -> Result<String, Self::Error> {
            self.track("try")?;
            self.visit_expr(expr)?;
            Ok("visited_try".to_string())
        }

        fn visit_await_try(&mut self, expr: &Expr, _span: &Span) -> Result<String, Self::Error> {
            self.track("await_try")?;
            self.visit_expr(expr)?;
            Ok("visited_await_try".to_string())
        }
        
        fn visit_closure(&mut self, params: &[(String, Option<String>)], _ret_type: &Option<String>, body: &Expr, _span: &Span) -> Result<String, Self::Error> {
            self.track(&format!("closure_{}_params", params.len()))?;
            self.visit_expr(body)?;
            Ok("visited_closure".to_string())
        }
    }

    #[test]
    fn test_visit_literals() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        // Test int
        let expr = Expr::Int(42, span);
        let result = visitor.visit_expr(&expr);
        assert_eq!(result, Ok("visited_int_42".to_string()));
        assert!(visitor.was_called("int_42"));

        // Test float
        let expr = Expr::Float(3.14, span);
        let result = visitor.visit_expr(&expr);
        assert_eq!(result, Ok("visited_float_3.14".to_string()));
        assert!(visitor.was_called("float_3.14"));

        // Test bool
        let expr = Expr::Bool(true, span);
        let result = visitor.visit_expr(&expr);
        assert_eq!(result, Ok("visited_bool_true".to_string()));
        assert!(visitor.was_called("bool_true"));

        // Test string
        let expr = Expr::String("hello".to_string(), span);
        let result = visitor.visit_expr(&expr);
        assert_eq!(result, Ok("visited_string_hello".to_string()));
        assert!(visitor.was_called("string_hello"));
    }

    #[test]
    fn test_visit_identifier() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        let expr = Expr::Identifier("x".to_string(), span);
        let result = visitor.visit_expr(&expr);
        assert_eq!(result, Ok("visited_identifier_x".to_string()));
        assert!(visitor.was_called("identifier_x"));
    }

    #[test]
    fn test_visit_array() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        let elements = vec![
            Expr::Int(1, span),
            Expr::Int(2, span),
            Expr::Int(3, span),
        ];
        let expr = Expr::Array(elements, span);
        let result = visitor.visit_expr(&expr);
        
        assert_eq!(result, Ok("visited_array".to_string()));
        assert!(visitor.was_called("array"));
        assert!(visitor.was_called("int_1"));
        assert!(visitor.was_called("int_2"));
        assert!(visitor.was_called("int_3"));
    }

    #[test]
    fn test_visit_binary_op() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        let left = Box::new(Expr::Int(5, span));
        let right = Box::new(Expr::Int(3, span));
        let expr = Expr::BinaryOp(left, Operator::Plus, right, span);
        
        let result = visitor.visit_expr(&expr);
        assert_eq!(result, Ok("visited_binary_op".to_string()));
        assert!(visitor.was_called("binary_op_Plus"));
        assert!(visitor.was_called("int_5"));
        assert!(visitor.was_called("int_3"));
    }

    #[test]
    fn test_visit_unary_op() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        let inner = Box::new(Expr::Bool(true, span));
        let expr = Expr::UnaryOp(UnaryOp::Not, inner, span);
        
        let result = visitor.visit_expr(&expr);
        assert_eq!(result, Ok("visited_unary_op".to_string()));
        assert!(visitor.was_called("unary_op_Not"));
        assert!(visitor.was_called("bool_true"));
    }

    #[test]
    fn test_visit_if_expr() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        let condition = Box::new(Expr::Bool(true, span));
        let then_block = vec![
            Stmt::Expression(Expr::Int(1, span), true),
        ];
        let else_block = vec![
            Stmt::Expression(Expr::Int(2, span), true),
        ];
        
        let expr = Expr::If(condition, then_block, else_block, span);
        let result = visitor.visit_expr(&expr);
        
        assert_eq!(result, Ok("visited_if_expr".to_string()));
        assert!(visitor.was_called("if_expr"));
        assert!(visitor.was_called("bool_true"));
        assert!(visitor.was_called("expression_stmt_semicolon_true"));
        assert!(visitor.was_called("int_1"));
        assert!(visitor.was_called("int_2"));
    }

    #[test]
    fn test_visit_statements() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        // Test let statement
        let stmt = Stmt::Let("x".to_string(), Expr::Int(42, span), span);
        let result = visitor.visit_stmt(&stmt);
        assert_eq!(result, Ok("visited_let".to_string()));
        assert!(visitor.was_called("let_x"));
        assert!(visitor.was_called("int_42"));

        // Test break statement
        let stmt = Stmt::Break(span);
        let result = visitor.visit_stmt(&stmt);
        assert_eq!(result, Ok("visited_break".to_string()));
        assert!(visitor.was_called("break"));

        // Test continue statement
        let stmt = Stmt::Continue(span);
        let result = visitor.visit_stmt(&stmt);
        assert_eq!(result, Ok("visited_continue".to_string()));
        assert!(visitor.was_called("continue"));
    }

    #[test]
    fn test_visit_function() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        let params = vec![
            ("x".to_string(), "int".to_string()),
            ("y".to_string(), "int".to_string()),
        ];
        let body = vec![
            Stmt::Expression(Expr::BinaryOp(
                Box::new(Expr::Identifier("x".to_string(), span)),
                Operator::Plus,
                Box::new(Expr::Identifier("y".to_string(), span)),
                span
            ), false),
        ];
        
        let stmt = Stmt::Function(false, "add".to_string(), vec![], params, "int".to_string(), body, span);
        let result = visitor.visit_stmt(&stmt);
        
        assert_eq!(result, Ok("visited_function".to_string()));
        assert!(visitor.was_called("function_add"));
        assert!(visitor.was_called("expression_stmt_semicolon_false"));
        assert!(visitor.was_called("binary_op_Plus"));
    }

    #[test]
    fn test_visit_for_loop() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        let iterable = Expr::Array(vec![Expr::Int(1, span), Expr::Int(2, span)], span);
        let body = vec![
            Stmt::Expression(Expr::Identifier("i".to_string(), span), true),
        ];
        
        let stmt = Stmt::ForLoop("i".to_string(), iterable, body, span);
        let result = visitor.visit_stmt(&stmt);
        
        assert_eq!(result, Ok("visited_for_loop".to_string()));
        assert!(visitor.was_called("for_loop_i"));
        assert!(visitor.was_called("array"));
        assert_eq!(visitor.call_count("expression_stmt_semicolon_true"), 1);
    }

    #[test]
    fn test_error_propagation() {
        let mut visitor = TrackingVisitor::with_failure("int_42");
        let span = Span::new(0, 0);

        let expr = Expr::Int(42, span);
        let result = visitor.visit_expr(&expr);
        
        assert!(matches!(result, Err(VisitError::TestError(_))));
        assert!(visitor.was_called("int_42"));
    }

    #[test]
    fn test_error_propagation_in_nested() {
        let mut visitor = TrackingVisitor::with_failure("int_2");
        let span = Span::new(0, 0);

        let elements = vec![
            Expr::Int(1, span),
            Expr::Int(2, span), // This will fail
            Expr::Int(3, span),
        ];
        let expr = Expr::Array(elements, span);
        let result = visitor.visit_expr(&expr);
        
        assert!(matches!(result, Err(VisitError::TestError(_))));
        assert!(visitor.was_called("array"));
        assert!(visitor.was_called("int_1"));
        assert!(visitor.was_called("int_2"));
        assert!(!visitor.was_called("int_3")); // Should not reach this
    }

    #[test]
    fn test_visit_match() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        let expr = Expr::Identifier("x".to_string(), span);
        let arms = vec![
            (Expr::Int(1, span), vec![Stmt::Expression(Expr::String("one".to_string(), span), true)]),
            (Expr::Int(2, span), vec![Stmt::Expression(Expr::String("two".to_string(), span), true)]),
        ];
        
        let stmt = Stmt::Match(expr, arms, span);
        let result = visitor.visit_stmt(&stmt);
        
        assert_eq!(result, Ok("visited_match".to_string()));
        assert!(visitor.was_called("match"));
        assert!(visitor.was_called("identifier_x"));
        assert!(visitor.was_called("int_1"));
        assert!(visitor.was_called("int_2"));
        assert!(visitor.was_called("string_one"));
        assert!(visitor.was_called("string_two"));
    }

    #[test]
    fn test_visit_return() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        // Return with expression
        let stmt = Stmt::Return(Some(Expr::Int(42, span)), span);
        let result = visitor.visit_stmt(&stmt);
        assert_eq!(result, Ok("visited_return".to_string()));
        assert!(visitor.was_called("return"));
        assert!(visitor.was_called("int_42"));

        // Return without expression
        let mut visitor2 = TrackingVisitor::new();
        let stmt = Stmt::Return(None, span);
        let result = visitor2.visit_stmt(&stmt);
        assert_eq!(result, Ok("visited_return".to_string()));
        assert!(visitor2.was_called("return"));
        assert_eq!(visitor2.visits.len(), 1); // Only return was called
    }

    #[test]
    fn test_visit_range() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        // Full range
        let start = Some(Box::new(Expr::Int(1, span)));
        let end = Some(Box::new(Expr::Int(10, span)));
        let expr = Expr::Range(start, end, false, span);
        let result = visitor.visit_expr(&expr);
        
        assert_eq!(result, Ok("visited_range".to_string()));
        assert!(visitor.was_called("range_inclusive_false"));
        assert!(visitor.was_called("int_1"));
        assert!(visitor.was_called("int_10"));

        // Inclusive range
        let mut visitor2 = TrackingVisitor::new();
        let start = Some(Box::new(Expr::Int(0, span)));
        let end = Some(Box::new(Expr::Int(5, span)));
        let expr = Expr::Range(start, end, true, span);
        let result = visitor2.visit_expr(&expr);
        
        assert_eq!(result, Ok("visited_range".to_string()));
        assert!(visitor2.was_called("range_inclusive_true"));

        // Open-ended range
        let mut visitor3 = TrackingVisitor::new();
        let expr = Expr::Range(None, None, false, span);
        let result = visitor3.visit_expr(&expr);
        
        assert_eq!(result, Ok("visited_range".to_string()));
        assert!(visitor3.was_called("range_inclusive_false"));
        assert_eq!(visitor3.visits.len(), 1); // Only range was called
    }

    #[test]
    fn test_visit_compound_assign() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        let left = Box::new(Expr::Identifier("x".to_string(), span));
        let right = Box::new(Expr::Int(5, span));
        let expr = Expr::CompoundAssign(left, Operator::Plus, right, span);
        
        let result = visitor.visit_expr(&expr);
        assert_eq!(result, Ok("visited_compound_assign".to_string()));
        assert!(visitor.was_called("compound_assign_Plus"));
        assert!(visitor.was_called("identifier_x"));
        assert!(visitor.was_called("int_5"));
    }

    #[test]
    fn test_visit_all_operators() {
        let mut visitor = TrackingVisitor::new();
        let span = Span::new(0, 0);

        let operators = vec![
            Operator::Plus,
            Operator::Minus,
            Operator::Multiply,
            Operator::Divide,
            Operator::Modulo,
            Operator::Equals,
            Operator::NotEquals,
            Operator::LessThan,
            Operator::GreaterThan,
            Operator::LessThanEquals,
            Operator::GreaterThanEquals,
            Operator::And,
            Operator::Or,
        ];

        for op in operators {
            let left = Box::new(Expr::Int(1, span));
            let right = Box::new(Expr::Int(2, span));
            let expr = Expr::BinaryOp(left, op.clone(), right, span);
            visitor.visit_expr(&expr).unwrap();
            assert!(visitor.was_called(&format!("binary_op_{:?}", op)));
        }
    }
}