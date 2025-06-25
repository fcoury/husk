#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    #[test]
    fn test_await_try_parsing() {
        let input = "let x = fetch(\"test\").await?;";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        if let Err(e) = &result {
            println!("Parse error: {}", e);
        }
        assert!(result.is_ok(), "Should parse .await? operator successfully");
    }

    #[test]
    fn test_await_parsing_still_works() {
        let input = "let x = fetch(\"test\").await;";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok(), "Should parse .await operator successfully");
    }
}
