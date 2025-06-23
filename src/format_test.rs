//! Tests for format! macro functionality

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semantic::SemanticVisitor;
    use crate::interpreter::InterpreterVisitor;
    use crate::transpiler::JsTranspiler;
    
    #[test]
    fn test_format_simple_string() {
        let program = r#"
            let message = format!("Hello, World!");
            println(message);
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");
        
        // Semantic analysis
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
        
        // Interpreter
        let mut interpreter = InterpreterVisitor::new();
        let result = interpreter.interpret(&ast).expect("Interpretation failed");
        
        // Check output was printed
        assert_eq!(result, crate::interpreter::Value::Unit);
    }
    
    #[test]
    fn test_format_with_placeholders() {
        let program = r#"
            let name = "Alice";
            let age = 30;
            let message = format!("Hello, {}! You are {} years old.", name, age);
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");
        
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
        
        let mut interpreter = InterpreterVisitor::new();
        interpreter.interpret(&ast).expect("Interpretation failed");
    }
    
    #[test]
    fn test_format_transpilation() {
        let program = r#"
            let name = "Bob";
            let count = 5;
            let message = format!("{} has {} apples", name, count);
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");
        
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
        
        let mut transpiler = JsTranspiler::new();
        let js = transpiler.generate(&ast).expect("Transpilation failed");
        
        // Should transpile to template literal
        assert!(js.contains("`${name} has ${count} apples`"));
    }
    
    #[test]
    fn test_format_multiple_types() {
        let program = r#"
            let int_val = 42;
            let float_val = 3.14;
            let bool_val = true;
            let str_val = "test";
            let message = format!("int: {}, float: {}, bool: {}, str: {}", 
                                int_val, float_val, bool_val, str_val);
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");
        
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
    }
    
    #[test]
    fn test_format_escaped_braces() {
        let program = r#"
            let message = format!("Use {{}} for placeholders");
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");
        
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
    }
    
    #[test]
    fn test_format_no_args() {
        let program = r#"
            let message = format!("Just a plain string");
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");
        
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
    }
    
    #[test]
    fn test_format_wrong_arg_count() {
        let program = r#"
            let name = "Alice";
            // Has two placeholders but only one argument
            let message = format!("Hello, {}! You are {} years old.", name);
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");
        
        let mut analyzer = SemanticVisitor::new();
        let result = analyzer.analyze(&ast);
        
        // Should fail with argument count mismatch
        assert!(result.is_err());
    }
}