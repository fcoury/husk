#[cfg(test)]
mod tests {
    use crate::error::Error;
    use crate::transpile_to_js;

    fn transpile_and_expect_error(code: &str) -> Error {
        match transpile_to_js(code) {
            Err(e) => e,
            Ok(_) => panic!("Expected transpiler error, but transpilation succeeded"),
        }
    }

    #[test]
    fn test_transpile_semantic_errors_first() {
        // Transpiler should catch semantic errors before attempting to transpile
        let error = transpile_and_expect_error("let x = y + 5;");
        match error {
            Error::Semantic(msg, _) => {
                assert!(msg.contains("Undefined") || msg.contains("y"));
            }
            _ => panic!("Expected Semantic error"),
        }
    }

    #[test]
    fn test_transpile_type_errors() {
        let error = transpile_and_expect_error(
            r#"
            let x = 5 + "hello";
        "#,
        );
        match error {
            Error::Semantic(msg, _) => {
                assert!(msg.contains("numeric types"));
            }
            _ => panic!("Expected Semantic error"),
        }
    }

    // Most transpiler-specific errors would be for unsupported features,
    // but our transpiler is currently quite complete. Let's add tests
    // for edge cases that might cause transpiler-specific issues.

    #[test]
    fn test_transpile_complex_program() {
        // This should succeed, just testing that complex programs transpile
        let code = r#"
            struct Point { x: int, y: int }
            
            fn new_point(x: int, y: int) -> Point {
                Point { x: x, y: y }
            }
            
            let p1 = new_point(0, 0);
            let p2 = new_point(3, 4);
            println(p1.x);
            println(p2.y);
        "#;

        match transpile_to_js(code) {
            Ok(js) => {
                assert!(js.contains("function Point")); // Transpiler uses functions, not classes
                assert!(js.contains("new_point"));
                assert!(js.contains("console.log"));
            }
            Err(e) => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn test_transpile_all_operators() {
        let code = r#"
            let a = true && false;
            let b = true || false;
            let c = !true;
            let d = 5 != 3;
            let e = 10 % 3;
            let f = -5;
        "#;

        match transpile_to_js(code) {
            Ok(js) => {
                assert!(js.contains("&&"));
                assert!(js.contains("||"));
                assert!(js.contains("!true"));
                assert!(js.contains("!=="));
                assert!(js.contains("%"));
                assert!(js.contains("(-5)"));
            }
            Err(e) => panic!("Unexpected error: {:?}", e),
        }
    }
}
