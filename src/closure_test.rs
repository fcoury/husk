//! Tests for closure/lambda syntax

#[cfg(test)]
mod tests {
    use crate::interpreter::InterpreterVisitor;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semantic::SemanticVisitor;
    use crate::transpiler::JsTranspiler;

    #[test]
    fn test_simple_closure_parsing() {
        let program = r#"
            let add = |x, y| x + y;
            let result = add(5, 3);
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(ast.len(), 2); // Two let statements
    }

    #[test]
    fn test_closure_with_types() {
        let program = r#"
            let add = |x: int, y: int| -> int { x + y };
            let multiply = |x: int, y: int| -> int x * y;
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse closure with types");

        // Semantic analysis
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
    }

    #[test]
    fn test_closure_capturing() {
        let program = r#"
            let x = 10;
            let add_x = |y| x + y;
            let result = add_x(5);
        "#;

        // Full pipeline test
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");

        let mut interpreter = InterpreterVisitor::new();
        let result = interpreter.interpret(&ast).expect("Interpretation failed");

        // Should return unit (last statement is a let)
        assert_eq!(result, crate::interpreter::Value::Unit);
    }

    #[test]
    fn test_closure_transpilation() {
        let program = r#"
            let add = |x, y| x + y;
            let multiply = |x: int, y: int| -> int { 
                let result = x * y;
                result
            };
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");

        let mut transpiler = JsTranspiler::new();
        let js = transpiler.generate(&ast).expect("Transpilation failed");

        // Check that arrow functions are generated
        assert!(js.contains("(x, y) =>"));
        assert!(js.contains("x + y"));
        assert!(js.contains("let multiply ="));
    }

    #[test]
    fn test_closure_as_function_argument() {
        let program = r#"
            // Simplified map - just returns the input array for now
            fn map(arr: array<int>, f: fn(int) -> int) -> array<int> {
                arr
            }
            
            let numbers = [1, 2, 3];
            let doubled = map(numbers, |x: int| x * 2);
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
    }

    #[test]
    fn test_nested_closures() {
        let program = r#"
            let make_adder = |x| {
                |y| x + y
            };
            
            let add5 = make_adder(5);
            let result = add5(3);
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse nested closures");

        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
    }

    #[test]
    fn test_empty_closure_params() {
        let program = r#"
            let get_value = || 42;
            let result = get_value();
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse empty param closure");

        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
    }
}
