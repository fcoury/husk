//! Tests for built-in Option and Result types

#[cfg(test)]
mod tests {
    use crate::interpreter::InterpreterVisitor;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semantic::SemanticVisitor;
    use crate::transpiler::JsTranspiler;

    #[test]
    fn test_option_some() {
        let program = r#"
            let x = Option::Some(42);
            match x {
                Option::Some(n) => n,
                Option::None => 0,
            }
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");

        let mut interpreter = InterpreterVisitor::new();
        let result = interpreter.interpret(&ast).expect("Interpretation failed");

        assert_eq!(result, crate::interpreter::Value::Int(42));
    }

    #[test]
    fn test_option_none() {
        let program = r#"
            let x = Option::None;
            match x {
                Option::Some(n) => n,
                Option::None => 999,
            }
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");

        let mut interpreter = InterpreterVisitor::new();
        let result = interpreter.interpret(&ast).expect("Interpretation failed");

        assert_eq!(result, crate::interpreter::Value::Int(999));
    }

    #[test]
    fn test_result_ok() {
        let program = r#"
            let x = Result::Ok(42);
            match x {
                Result::Ok(n) => n,
                Result::Err(_) => 0,
            }
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");

        let mut interpreter = InterpreterVisitor::new();
        let result = interpreter.interpret(&ast).expect("Interpretation failed");

        assert_eq!(result, crate::interpreter::Value::Int(42));
    }

    #[test]
    fn test_result_err() {
        let program = r#"
            let x = Result::Err("error message");
            match x {
                Result::Ok(_) => 0,
                Result::Err(msg) => 999,
            }
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");

        let mut interpreter = InterpreterVisitor::new();
        let result = interpreter.interpret(&ast).expect("Interpretation failed");

        assert_eq!(result, crate::interpreter::Value::Int(999));
    }

    #[test]
    fn test_option_transpilation() {
        let program = r#"
            let x = Option::Some(42);
            match x {
                Option::Some(n) => n,
                Option::None => 0,
            }
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");

        let mut transpiler = JsTranspiler::new();
        let js = transpiler.generate(&ast).expect("Transpilation failed");

        // Should generate null checks
        assert!(js.contains("null") || js.contains("undefined"));
    }

    #[test]
    fn test_result_transpilation() {
        let program = r#"
            let a = 10;
            let b = 0;
            let result = if b == 0 {
                Result::Err("Division by zero")
            } else {
                Result::Ok(a / b)
            };
            result
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");

        let mut transpiler = JsTranspiler::new();
        let js = transpiler.generate(&ast).expect("Transpilation failed");
        // Should generate Result structure with type field
        assert!(js.contains("type: 'Ok'"));
        assert!(js.contains("type: 'Err'"));
    }

    #[test]
    fn test_nested_option_result() {
        let program = r#"
            let x = Option::Some(Result::Ok(42));
            match x {
                Option::Some(r) => match r {
                    Result::Ok(n) => n,
                    Result::Err(_) => 0,
                },
                Option::None => -1,
            }
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");

        let mut interpreter = InterpreterVisitor::new();
        let result = interpreter.interpret(&ast).expect("Interpretation failed");

        assert_eq!(result, crate::interpreter::Value::Int(42));
    }

    #[test]
    fn test_nested_match_all_cases() {
        // Test Option::Some(Result::Ok)
        let program1 = r#"
            let x = Option::Some(Result::Ok(100));
            match x {
                Option::Some(r) => match r {
                    Result::Ok(n) => n,
                    Result::Err(_) => -1,
                },
                Option::None => -2,
            }
        "#;

        let mut lexer = Lexer::new(program1);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
        let mut interpreter = InterpreterVisitor::new();
        let result = interpreter.interpret(&ast).expect("Interpretation failed");
        assert_eq!(result, crate::interpreter::Value::Int(100));

        // Test Option::Some(Result::Err)
        let program2 = r#"
            let x = Option::Some(Result::Err("error"));
            match x {
                Option::Some(r) => match r {
                    Result::Ok(_) => -1,
                    Result::Err(msg) => 200,
                },
                Option::None => -2,
            }
        "#;

        let mut lexer = Lexer::new(program2);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
        let mut interpreter = InterpreterVisitor::new();
        let result = interpreter.interpret(&ast).expect("Interpretation failed");
        assert_eq!(result, crate::interpreter::Value::Int(200));

        // Test Option::None
        let program3 = r#"
            let x = Option::None;
            match x {
                Option::Some(r) => match r {
                    Result::Ok(_) => -1,
                    Result::Err(_) => -1,
                },
                Option::None => 300,
            }
        "#;

        let mut lexer = Lexer::new(program3);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
        let mut interpreter = InterpreterVisitor::new();
        let result = interpreter.interpret(&ast).expect("Interpretation failed");
        assert_eq!(result, crate::interpreter::Value::Int(300));
    }

    #[test]
    fn test_nested_match_transpilation() {
        let program = r#"
            let x = Option::Some(Result::Ok(42));
            match x {
                Option::Some(r) => match r {
                    Result::Ok(n) => n,
                    Result::Err(_) => 0,
                },
                Option::None => -1,
            }
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");

        let mut transpiler = JsTranspiler::new();
        let js = transpiler.generate(&ast).expect("Transpilation failed");

        // Should contain nested match structures
        assert!(js.contains("_matched"));
        assert!(js.contains("type: 'Ok'") || js.contains("type === 'Ok'"));
    }
}
