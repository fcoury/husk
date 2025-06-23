#[cfg(test)]
mod tests {
    use crate::{
        lexer::Lexer,
        parser::Parser,
        transpiler::JsTranspiler,
    };

    #[test]
    fn test_error_mapping_helper_generation() {
        let input = r#"
extern fn parseJSON(s: string) -> Result<any, any>;

async fn test() -> Result<string, any> {
    let result = parseJSON("test").await?;
    Result::Ok("success")
}
"#;
        
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Should parse successfully");
        
        let mut transpiler = JsTranspiler::new();
        let js_output = transpiler.generate(&ast).expect("Should transpile successfully");
        
        // Check that error mapping helpers are included
        assert!(js_output.contains("function __husk_map_error(error)"));
        assert!(js_output.contains("function __husk_safe_call(fn, ...args)"));
        assert!(js_output.contains("function __husk_await_bridge(promise)"));
        
        // Check error type handling
        assert!(js_output.contains("error instanceof Error"));
        assert!(js_output.contains("error instanceof DOMException"));
        assert!(js_output.contains("typeof error === 'string'"));
        
        // Check that await bridge uses error mapping
        assert!(js_output.contains("return __husk_map_error(error)"));
    }
    
    #[test]
    fn test_error_mapping_comprehensive_cases() {
        let input = r#"
extern fn fetch(url: string) -> Promise<any>;

async fn test_all_error_types() -> Result<string, any> {
    let result = fetch("test").await?;
    Result::Ok("success")
}
"#;
        
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Should parse successfully");
        
        let mut transpiler = JsTranspiler::new();
        let js_output = transpiler.generate(&ast).expect("Should transpile successfully");
        
        // Check error payload structure for different error types
        assert!(js_output.contains("name: error.name"));
        assert!(js_output.contains("message: error.message"));
        assert!(js_output.contains("stack: error.stack || null"));
        assert!(js_output.contains("code: error.code")); // DOMException
        assert!(js_output.contains("value: error")); // Generic errors
    }
}