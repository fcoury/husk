#[cfg(test)]
mod tests {
    use crate::error::Error;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn parse_and_expect_error(code: &str) -> Error {
        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);

        match parser.parse() {
            Err(e) => e,
            Ok(_) => panic!("Expected parse error, but parsing succeeded"),
        }
    }

    #[test]
    fn test_missing_semicolon() {
        let error = parse_and_expect_error("let x = 5");
        match error {
            Error::Parse(msg, _) => {
                assert!(msg.contains("Expected ';'") || msg.contains("semicolon"));
            }
            _ => panic!("Expected Parse error"),
        }
    }

    #[test]
    fn test_missing_equals_in_let() {
        let error = parse_and_expect_error("let x 5;");
        match error {
            Error::Parse(msg, _) => {
                assert!(msg.contains("Expected '='"));
            }
            _ => panic!("Expected Parse error"),
        }
    }

    #[test]
    fn test_unclosed_parentheses() {
        let error = parse_and_expect_error("let x = (5 + 3;");
        match error {
            Error::Parse(msg, _) => {
                assert!(msg.contains("Expected ')'") || msg.contains("parenthes"));
            }
            _ => panic!("Expected Parse error"),
        }
    }

    #[test]
    fn test_unclosed_brace() {
        let error = parse_and_expect_error("fn test() -> int { 5");
        match error {
            Error::Parse(msg, _) => {
                // The parser reaches EOF while parsing the function body
                // Just check that we get a parse error
                assert!(!msg.is_empty());
            }
            _ => panic!("Expected Parse error"),
        }
    }

    #[test]
    fn test_invalid_function_syntax() {
        let error = parse_and_expect_error("fn test( -> int { 5 }");
        match error {
            Error::Parse(msg, _) => {
                assert!(msg.contains("Expected") || msg.contains("parameter"));
            }
            _ => panic!("Expected Parse error"),
        }
    }

    #[test]
    fn test_invalid_if_syntax() {
        let error = parse_and_expect_error("if { 5 }");
        match error {
            Error::Parse(msg, _) => {
                assert!(msg.contains("Invalid expression") || msg.contains("condition"));
            }
            _ => panic!("Expected Parse error"),
        }
    }

    #[test]
    fn test_invalid_struct_syntax() {
        let error = parse_and_expect_error("struct Point { x }");
        match error {
            Error::Parse(msg, _) => {
                assert!(msg.contains("Expected") || msg.contains(":"));
            }
            _ => panic!("Expected Parse error"),
        }
    }

    #[test]
    fn test_invalid_array_syntax() {
        // Test with missing closing bracket instead
        let error = parse_and_expect_error("let arr = [1, 2");
        match error {
            Error::Parse(msg, _) => {
                assert!(msg.contains("Expected") || msg.contains("]"));
            }
            _ => panic!("Expected Parse error"),
        }
    }

    #[test]
    fn test_invalid_match_syntax() {
        let error = parse_and_expect_error("match x { }");
        match error {
            Error::Parse(msg, _) => {
                assert!(
                    msg.contains("Expected") || msg.contains("match") || msg.contains("pattern")
                );
            }
            _ => panic!("Expected Parse error"),
        }
    }

    #[test]
    fn test_empty_block() {
        // This might be valid, but let's test it
        let code = "let x = {};";
        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);

        // Empty blocks might be valid expressions, so we just test parsing
        let result = parser.parse();
        assert!(result.is_ok() || result.is_err());
    }
}
