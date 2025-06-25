#[cfg(test)]
mod tests {
    use crate::error::{Error, Result};
    use crate::span::Span;

    #[test]
    fn test_semantic_error_creation() {
        let span = Span::new(10, 20);
        let error = Error::new_semantic("Type mismatch", span);

        match error {
            Error::Semantic(msg, s) => {
                assert_eq!(msg, "Type mismatch");
                assert_eq!(s.start, 10);
                assert_eq!(s.end, 20);
            }
            _ => panic!("Expected Semantic error"),
        }
    }

    #[test]
    fn test_parse_error_creation() {
        let span = Span::new(5, 15);
        let error = Error::new_parse("Unexpected token", span);

        match error {
            Error::Parse(msg, s) => {
                assert_eq!(msg, "Unexpected token");
                assert_eq!(s.start, 5);
                assert_eq!(s.end, 15);
            }
            _ => panic!("Expected Parse error"),
        }
    }

    #[test]
    fn test_runtime_error_creation() {
        let span = Span::new(0, 10);
        let error = Error::new_runtime("Division by zero", span);

        match error {
            Error::Runtime(msg, s) => {
                assert_eq!(msg, "Division by zero");
                assert_eq!(s.start, 0);
                assert_eq!(s.end, 10);
            }
            _ => panic!("Expected Runtime error"),
        }
    }

    #[test]
    fn test_transpiler_error_creation() {
        let span = Span::new(25, 30);
        let error = Error::new_transpile("Unsupported feature", span);

        match error {
            Error::Transpiler(msg, s) => {
                assert_eq!(msg, "Unsupported feature");
                assert_eq!(s.start, 25);
                assert_eq!(s.end, 30);
            }
            _ => panic!("Expected Transpiler error"),
        }
    }

    #[test]
    fn test_error_display() {
        let span = Span::new(10, 20);
        let error = Error::new_semantic("Type mismatch", span);
        let display = format!("{}", error);
        assert!(display.contains("Semantic error"));
        assert!(display.contains("Type mismatch"));
        // The actual format uses Debug for span which shows start and end
        assert!(display.contains("10") && display.contains("20"));
    }

    #[test]
    fn test_error_pretty_print() {
        let code = "let x = 5;\nlet y = \"hello\";\nx = y;";
        let span = Span::new(28, 33); // "x = y"
        let error = Error::new_semantic("Type mismatch: cannot assign string to int", span);

        let pretty = error.pretty_print(code);
        // Line numbering starts at 0, so third line is line 2
        assert!(pretty.contains("error: 2:1"));
        assert!(pretty.contains("Type mismatch"));
        assert!(pretty.contains("x = y;"));
        assert!(pretty.contains("^^^^^")); // Should show the span
    }

    #[test]
    fn test_result_type() {
        fn might_fail(should_fail: bool) -> Result<i32> {
            if should_fail {
                Err(Error::new_runtime("Something went wrong", Span::new(0, 0)))
            } else {
                Ok(42)
            }
        }

        assert!(might_fail(false).is_ok());
        assert!(might_fail(true).is_err());

        match might_fail(true) {
            Err(Error::Runtime(msg, _)) => assert_eq!(msg, "Something went wrong"),
            _ => panic!("Expected Runtime error"),
        }
    }

    #[test]
    fn test_error_chaining() {
        fn inner_function() -> Result<i32> {
            Err(Error::new_runtime("Inner error", Span::new(5, 10)))
        }

        fn outer_function() -> Result<i32> {
            inner_function().map_err(|_| Error::new_runtime("Outer error", Span::new(0, 15)))
        }

        match outer_function() {
            Err(Error::Runtime(msg, span)) => {
                assert_eq!(msg, "Outer error");
                assert_eq!(span.start, 0);
                assert_eq!(span.end, 15);
            }
            _ => panic!("Expected Runtime error"),
        }
    }
}
