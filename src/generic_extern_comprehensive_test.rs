//! Comprehensive tests for generic type parsing in extern declarations

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::transpiler::JsTranspiler;
    use crate::semantic::SemanticVisitor;
    
    #[test]
    fn test_transpile_generic_externs() {
        let program = r#"
            extern fn fetch(url: string) -> Promise<string>;
            extern fn all<T>(promises: array<Promise<T>>) -> Promise<array<T>>;
            
            extern mod utils {
                fn map<T, U>(arr: array<T>, mapper: fn(T) -> U) -> array<U>;
                fn filter<T>(arr: array<T>, predicate: fn(T) -> bool) -> array<T>;
                type Result<T, E>;
                fn ok<T, E>(value: T) -> Result<T, E>;
                fn err<T, E>(error: E) -> Result<T, E>;
            }
            
            async fn fetchData() -> string {
                let url = "https://api.example.com";
                let data = fetch(url).await;
                data
            }
        "#;
        
        // Full pipeline
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");
        
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
        
        let mut transpiler = JsTranspiler::new();
        let js = transpiler.generate(&ast).expect("Transpilation failed");
        
        // Check that the JS output includes the async function
        assert!(js.contains("async function fetchData()"));
        assert!(js.contains("await fetch(url)"));
        
        // The transpiler should have processed the extern declarations 
        // (even if they don't appear in the output)
        
        println!("✓ Generic externs transpiled successfully");
    }
    
    #[test] 
    fn test_complex_nested_generics() {
        let program = r#"
            extern fn transform<A, B, C>(
                data: array<A>,
                mapper: fn(A) -> Promise<B>,
                reducer: fn(array<B>) -> C
            ) -> Promise<C>;
            
            extern mod collections {
                type Map<K, V>;
                type Set<T>;
                
                fn createMap<K, V>() -> Map<K, V>;
                fn createSet<T>() -> Set<T>;
                
                fn mapGet<K, V>(map: Map<K, V>, key: K) -> Option<V>;
                fn mapSet<K, V>(map: Map<K, V>, key: K, value: V) -> void;
                
                fn setAdd<T>(set: Set<T>, value: T) -> void;
                fn setHas<T>(set: Set<T>, value: T) -> bool;
            }
            
            extern mod types {
                type Either<L, R>;
                type Option<T>;
                type Result<T, E>;
            }
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse complex generics");
        
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast).expect("Semantic analysis failed");
        
        println!("✓ Complex nested generics parsed successfully");
    }
    
    #[test]
    fn test_generic_constraints_future() {
        // This test documents future syntax we might want to support
        // Currently, these should parse the constraint syntax as part of the type name
        let program = r#"
            extern mod future {
                // These parse but constraints aren't enforced yet
                type Comparable<T>;
                type Numeric<T>;
            }
        "#;
        
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();
        
        let mut parser = Parser::new(tokens);
        let _ast = parser.parse().expect("Should parse");
        
        println!("✓ Future generic constraint syntax parses (but not enforced)");
    }
}