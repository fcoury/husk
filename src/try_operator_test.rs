#[cfg(test)]
mod tests {
    use crate::{
        interpreter::InterpreterVisitor,
        lexer::Lexer,
        parser::Parser,
        semantic::SemanticVisitor,
        transpiler::JsTranspiler,
    };

    #[test]
    fn test_try_operator_parsing() {
        let input = "let x = some_result?;";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        
        assert!(result.is_ok(), "Should parse try operator successfully");
    }

    #[test]
    fn test_try_operator_semantic_analysis() {
        let program = r#"
            let ok_result = Result::Ok(42);
            let value = ok_result?;
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let statements = parser.parse().expect("Parse should succeed");
        
        let mut analyzer = SemanticVisitor::new();
        let result = analyzer.analyze(&statements);
        
        assert!(result.is_ok(), "Semantic analysis should succeed for Result types");
    }

    #[test]
    fn test_try_operator_transpilation() {
        let program = r#"
            let ok_result = Result::Ok(42);
            let value = ok_result?;
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let statements = parser.parse().expect("Parse should succeed");
        
        let mut transpiler = JsTranspiler::new();
        let result = transpiler.generate(&statements);
        
        assert!(result.is_ok(), "Transpilation should succeed");
        let js_code = result.unwrap();
        assert!(js_code.contains("__result"), "Should contain try operator logic");
        assert!(js_code.contains("type === 'Err'"), "Should contain error checking");
    }

    #[test]
    fn test_try_operator_interpreter_ok() {
        let program = r#"
            let ok_result = Result::Ok(42);
            let value = ok_result?;
            value
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let statements = parser.parse().expect("Parse should succeed");
        
        let mut interpreter = InterpreterVisitor::new();
        let result = interpreter.interpret(&statements);
        
        assert!(result.is_ok(), "Should succeed with Result::Ok");
        // Note: In real implementation, we'd verify the unwrapped value is 42
    }

    #[test]
    fn test_try_operator_interpreter_err() {
        let program = r#"
            let err_result = Result::Err("error");
            let value = err_result?;
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let statements = parser.parse().expect("Parse should succeed");
        
        let mut interpreter = InterpreterVisitor::new();
        let result = interpreter.interpret(&statements);
        
        assert!(result.is_err(), "Should fail with Result::Err");
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("error propagation"), "Should mention error propagation");
    }

    #[test]
    fn test_try_operator_invalid_type() {
        let program = r#"
            let not_result = 42;
            let value = not_result?;
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let statements = parser.parse().expect("Parse should succeed");
        
        let mut interpreter = InterpreterVisitor::new();
        let result = interpreter.interpret(&statements);
        
        assert!(result.is_err(), "Should fail on non-Result types");
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("Result types"), "Should mention Result types requirement");
    }

    #[test]
    fn test_chained_try_operators() {
        let program = r#"
            let result1 = Result::Ok(Result::Ok(42));
            let value = result1??;
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        
        if let Err(e) = &result {
            println!("Parse error: {}", e);
        }
        assert!(result.is_ok(), "Should parse chained try operators");
    }
}