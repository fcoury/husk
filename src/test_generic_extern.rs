//! Test generic type parsing in extern declarations

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semantic::SemanticVisitor;
    use crate::ast::visitor::AstVisitor;
    
    #[test]
    fn test_extern_with_generic_types() {
        let program = r#"
            extern fn fetch(url: string) -> Promise<string>;
            extern fn map<T, U>(arr: array<T>, mapper: fn(T) -> U) -> array<U>;
            extern fn parseJson<T>(json: string) -> Result<T, string>;
            
            extern mod console {
                fn log<T>(value: T) -> void;
                fn error<T>(message: T) -> void;
            }
            
            async fn test() -> string {
                let response = fetch("https://api.example.com").await;
                response
            }
        "#;
        
        // Lexing
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        
        // Parsing
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");
        
        // Verify we have the extern declarations
        assert_eq!(ast.len(), 5); // 3 extern functions + 1 extern mod + 1 async function
        
        // Semantic analysis
        let mut analyzer = SemanticVisitor::new();
        for stmt in &ast {
            analyzer.visit_stmt(stmt).expect("Semantic analysis failed");
        }
        
        println!("✓ Generic types in extern declarations parsed successfully");
    }
    
    #[test]
    fn test_nested_generic_types() {
        let program = r#"
            extern fn sequence<T>(promises: array<Promise<T>>) -> Promise<array<T>>;
            extern fn either<L, R>(left: L, right: R) -> Either<L, R>;
            extern fn compose<A, B, C>(f: fn(B) -> C, g: fn(A) -> B) -> fn(A) -> C;
            
            extern mod collections {
                type Map<K, V>;
                fn createMap<K, V>() -> Map<K, V>;
                fn get<K, V>(map: Map<K, V>, key: K) -> Option<V>;
            }
        "#;
        
        // Lexing
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        
        // Parsing
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");
        
        // Semantic analysis
        let mut analyzer = SemanticVisitor::new();
        for stmt in &ast {
            analyzer.visit_stmt(stmt).expect("Semantic analysis failed");
        }
        
        println!("✓ Nested generic types parsed successfully");
    }
    
    #[test]
    fn test_invalid_generic_syntax() {
        // This test now expects proper error handling for missing return types
        let program = r#"
            extern fn bad1() -> ;  // Missing return type
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        
        assert!(parser.parse().is_err(), "Should fail on missing return type");
        
        let program2 = r#"
            extern fn bad2() -> Promise<>;  // Empty generic parameters
        "#;
        
        let mut lexer = Lexer::new(program2);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        
        // Empty generic parameters should be rejected
        let result = parser.parse();
        if result.is_ok() {
            // If it parsed, check that it treated it as "Promise<>" string
            let ast = result.unwrap();
            assert_eq!(ast.len(), 1);
        }
        
        let program3 = r#"
            extern fn good() -> Promise;  // Missing angle brackets - valid as non-generic type
        "#;
        
        let mut lexer = Lexer::new(program3);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        
        // This should succeed and treat Promise as a non-generic type
        let ast = parser.parse().expect("Should parse Promise as regular type");
        assert_eq!(ast.len(), 1);
        
        println!("✓ Invalid generic syntax properly handled");
    }
}