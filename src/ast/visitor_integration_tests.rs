#[cfg(test)]
mod tests {
    use crate::{
        error::Error, execute_script, lexer::Lexer, parser::Parser, semantic::SemanticVisitor,
        transpile_to_js,
    };

    #[test]
    fn test_visitor_pattern_with_semantic_analysis() {
        let code = r#"
            let x = 5;
            let y = x + 10;
            if y > 10 {
                println!(y);
            }
        "#;

        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        // Semantic analysis uses the visitor pattern
        let mut analyzer = SemanticVisitor::new();
        let result = analyzer.analyze(&ast);

        assert!(result.is_ok());
    }

    #[test]
    fn test_visitor_pattern_with_interpreter() {
        let code = r#"
            let sum = 0;
            for i in 1..4 {
                sum = sum + i;
            }
            sum
        "#;

        let result = execute_script(code);
        assert!(result.is_ok());

        match result.unwrap() {
            crate::Value::Int(n) => assert_eq!(n, 6), // 1 + 2 + 3
            _ => panic!("Expected Int result"),
        }
    }

    #[test]
    fn test_visitor_pattern_with_transpiler() {
        let code = r#"
            fn add(a: int, b: int) -> int {
                a + b
            }
            let result = add(5, 3);
            println!(result);
        "#;

        let js = transpile_to_js(code).unwrap();

        // Verify the transpiler correctly visited all nodes
        assert!(js.contains("function add(a, b)"));
        assert!(js.contains("return (a + b)"));
        assert!(js.contains("let result = add(5, 3)"));
        assert!(js.contains("console.log(result)"));
    }

    #[test]
    fn test_visitor_pattern_error_propagation() {
        // Test that errors in visitor methods are properly propagated
        let code = r#"
            let x = y + 5; // Undefined variable
        "#;

        let result = execute_script(code);
        assert!(result.is_err());

        match result {
            Err(Error::Semantic(msg, _)) => {
                assert!(msg.contains("Undefined") || msg.contains("y"));
            }
            _ => panic!("Expected semantic error"),
        }
    }

    #[test]
    fn test_visitor_pattern_with_complex_ast() {
        let code = r#"
            struct Point { x: int, y: int }
            
            enum Color {
                Red,
                Green,
                Blue,
            }
            
            fn distance(p1: Point, p2: Point) -> int {
                let dx = p1.x - p2.x;
                let dy = p1.y - p2.y;
                dx * dx + dy * dy
            }
            
            let p1 = Point { x: 0, y: 0 };
            let p2 = Point { x: 3, y: 4 };
            let d = distance(p1, p2);
            
            match Color::Red {
                Color::Red => println!("It's red!"),
                Color::Green => println!("It's green!"),
                Color::Blue => println!("It's blue!"),
            }
            
            d
        "#;

        // This tests that all visitor methods work correctly together
        let result = execute_script(code);
        if let Err(e) = &result {
            eprintln!("Error executing script: {e:?}");
        }
        assert!(result.is_ok());

        match result.unwrap() {
            crate::Value::Int(n) => assert_eq!(n, 25), // 3*3 + 4*4
            _ => panic!("Expected Int result"),
        }
    }

    #[test]
    fn test_visitor_pattern_with_all_expression_types() {
        let code = r#"
            // Test all expression types
            let int_val = 42;
            let float_val = 3.14;
            let bool_val = true;
            let string_val = "hello";
            let array_val = [1, 2, 3];
            let range_val = 1..10;
            
            // Test unary operators
            let neg = -5;
            let not = !false;
            
            // Test binary operators
            let add = 5 + 3;
            let mult = 4 * 2;
            let comp = 5 > 3;
            let logical = true && false;
            
            // Test compound assignment
            let x = 10;
            x += 5;
            
            // Test array indexing
            let elem = array_val[1];
            
            // Test if expression
            let result = if comp { 1 } else { 0 };
            
            result
        "#;

        let result = execute_script(code);
        assert!(result.is_ok());

        match result.unwrap() {
            crate::Value::Int(n) => assert_eq!(n, 1),
            _ => panic!("Expected Int result"),
        }
    }

    #[test]
    fn test_visitor_pattern_with_all_statement_types() {
        let code = r#"
            // Test various statement types
            let x = 5;  // Let statement
            
            // Function statement
            fn double(n: int) -> int {
                n * 2
            }
            
            // Struct statement
            struct Data { value: int }
            
            // Enum statement
            enum Option {
                Some(int),
                None,
            }
            
            // For loop statement
            let sum = 0;
            for i in 1..4 {
                sum = sum + i;
            }
            
            // While loop statement
            let count = 0;
            while count < 3 {
                count = count + 1;
            }
            
            // Loop statement with break
            let n = 0;
            loop {
                n = n + 1;
                if n >= 5 {
                    break;
                }
            }
            
            // Match statement
            let opt = Option::Some(42);
            match opt {
                Option::Some(v) => v,
                Option::None => 0,
            }
        "#;

        let result = execute_script(code);
        assert!(result.is_ok());

        match result.unwrap() {
            crate::Value::Int(n) => assert_eq!(n, 42),
            _ => panic!("Expected Int result"),
        }
    }

    #[test]
    fn test_visitor_pattern_with_nested_structures() {
        let code = r#"
            // Test deeply nested structures
            let result = if true {
                let arr = [1, 2, 3];
                let sum = 0;
                for i in arr {
                    if i > 1 {
                        sum = sum + i;
                    }
                }
                sum
            } else {
                0
            };
            result
        "#;

        let result = execute_script(code);
        assert!(result.is_ok());

        match result.unwrap() {
            crate::Value::Int(n) => assert_eq!(n, 5), // 2 + 3
            _ => panic!("Expected Int result"),
        }
    }

    #[test]
    fn test_transpiler_visitor_all_constructs() {
        let code = r#"
            struct Point { x: int, y: int }
            
            fn main() -> int {
                let p = Point { x: 10, y: 20 };
                let arr = [1, 2, 3];
                let sum = 0;
                
                for i in arr {
                    sum = sum + i;
                }
                
                if sum > 5 {
                    p.x + p.y
                } else {
                    0
                }
            }
            
            main()
        "#;

        let js = transpile_to_js(code).unwrap();

        // Verify key constructs were properly transpiled
        assert!(js.contains("function Point"));
        assert!(js.contains("function main"));
        // Struct instantiation sets properties
        assert!(js.contains("10") && js.contains("20"));
        assert!(js.contains("[1, 2, 3]"));
        // For loop is transpiled
        assert!(js.contains("for"));
        // If condition is transpiled
        assert!(js.contains("if"));
    }

    #[test]
    fn test_visitor_pattern_handles_return_correctly() {
        let code = r#"
            fn test() -> int {
                return 42;
                99  // This should not be reached
            }
            test()
        "#;

        let result = execute_script(code);
        assert!(result.is_ok());

        match result.unwrap() {
            crate::Value::Int(n) => assert_eq!(n, 42),
            _ => panic!("Expected Int result"),
        }
    }
}
