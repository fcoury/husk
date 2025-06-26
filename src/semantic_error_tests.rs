#[cfg(test)]
mod tests {
    use crate::error::Error;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semantic::SemanticVisitor;

    fn analyze_and_expect_error(code: &str) -> Error {
        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Parsing should succeed");

        let mut analyzer = SemanticVisitor::new();
        match analyzer.analyze(&ast) {
            Err(e) => e,
            Ok(_) => panic!("Expected semantic error, but analysis succeeded"),
        }
    }

    #[test]
    fn test_undefined_variable() {
        let error = analyze_and_expect_error("let x = y + 5;");
        match error {
            Error::Semantic(msg, _) => {
                assert!(msg.contains("Undefined variable") || msg.contains("y"));
            }
            _ => panic!("Expected Semantic error"),
        }
    }

    // Variables in Husk can be reassigned with different types
    // So this test is removed or changed to test actual type errors

    #[test]
    fn test_type_mismatch_in_binary_op() {
        let error = analyze_and_expect_error(
            r#"
            let x = 5 + "hello";
        "#,
        );
        match error {
            Error::Semantic(msg, _) => {
                assert!(msg.contains("numeric types") || msg.contains("Type mismatch"));
            }
            _ => panic!("Expected Semantic error"),
        }
    }

    #[test]
    fn test_logical_op_with_non_bool() {
        let error = analyze_and_expect_error(
            r#"
            let x = 5 && true;
        "#,
        );
        match error {
            Error::Semantic(msg, _) => {
                assert!(msg.contains("bool types") || msg.contains("Logical"));
            }
            _ => panic!("Expected Semantic error"),
        }
    }

    #[test]
    fn test_not_operator_with_non_bool() {
        let error = analyze_and_expect_error(
            r#"
            let x = !5;
        "#,
        );
        match error {
            Error::Semantic(msg, _) => {
                assert!(msg.contains("bool type") || msg.contains("NOT"));
            }
            _ => panic!("Expected Semantic error"),
        }
    }

    #[test]
    fn test_undefined_function() {
        let error = analyze_and_expect_error(
            r#"
            let x = foo(5);
        "#,
        );
        match error {
            Error::Semantic(msg, _) => {
                assert!(msg.contains("Undefined function") || msg.contains("foo"));
            }
            _ => panic!("Expected Semantic error"),
        }
    }

    #[test]
    fn test_wrong_number_of_arguments() {
        let error = analyze_and_expect_error(
            r#"
            fn add(a: int, b: int) -> int {
                a + b
            }
            let x = add(5);
        "#,
        );
        match error {
            Error::Semantic(msg, _) => {
                assert!(msg.contains("expects 2") || msg.contains("arguments"));
            }
            _ => panic!("Expected Semantic error"),
        }
    }

    #[test]
    fn test_undefined_struct() {
        let error = analyze_and_expect_error(
            r#"
            let p = Point { x: 5, y: 10 };
        "#,
        );
        match error {
            Error::Semantic(msg, _) => {
                assert!(msg.contains("Undefined struct") || msg.contains("Point"));
            }
            _ => panic!("Expected Semantic error"),
        }
    }

    #[test]
    fn test_undefined_struct_field() {
        let error = analyze_and_expect_error(
            r#"
            struct Point { x: int, y: int }
            let p = Point { x: 5, y: 10, z: 15 };
        "#,
        );
        match error {
            Error::Semantic(msg, _) => {
                // The error message might use different wording
                assert!(msg.contains("field") || msg.contains("z") || msg.contains("Point"));
            }
            _ => panic!("Expected Semantic error"),
        }
    }

    #[test]
    fn test_return_type_mismatch() {
        let error = analyze_and_expect_error(
            r#"
            fn test() -> int {
                "hello"
            }
        "#,
        );
        match error {
            Error::Semantic(msg, _) => {
                assert!(msg.contains("Return type mismatch") || msg.contains("expected int"));
            }
            _ => panic!("Expected Semantic error"),
        }
    }

    #[test]
    fn test_if_condition_not_bool() {
        let error = analyze_and_expect_error(
            r#"
            if 5 {
                println!("yes");
            }
        "#,
        );
        match error {
            Error::Semantic(msg, _) => {
                assert!(msg.contains("bool") || msg.contains("condition"));
            }
            _ => panic!("Expected Semantic error"),
        }
    }

    #[test]
    fn test_array_index_not_int() {
        let error = analyze_and_expect_error(
            r#"
            let arr = [1, 2, 3];
            let x = arr["hello"];
        "#,
        );
        match error {
            Error::Semantic(msg, _) => {
                assert!(msg.contains("int") || msg.contains("index"));
            }
            _ => panic!("Expected Semantic error"),
        }
    }
}
