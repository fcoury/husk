#[cfg(test)]
mod tests {
    use crate::{
        lexer::Lexer,
        parser::{Parser, Stmt},
    };

    #[test]
    fn test_generic_function_parsing() {
        let input = "fn map<T, U>(item: T, mapper: fn(T) -> U) -> U { mapper(item) }";
        
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Should parse generic function");
        
        assert_eq!(ast.len(), 1);
        if let Stmt::Function(name, generic_params, params, return_type, _body, _span) = &ast[0] {
            assert_eq!(name, "map");
            assert_eq!(generic_params, &vec!["T".to_string(), "U".to_string()]);
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].0, "item");
            assert_eq!(params[0].1, "T");
            assert_eq!(return_type, "U");
        } else {
            panic!("Expected Function statement");
        }
    }

    #[test]
    fn test_generic_struct_parsing() {
        let input = "struct Container<T> { value: T }";
        
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Should parse generic struct");
        
        assert_eq!(ast.len(), 1);
        if let Stmt::Struct(name, generic_params, fields, _span) = &ast[0] {
            assert_eq!(name, "Container");
            assert_eq!(generic_params, &vec!["T".to_string()]);
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].0, "value");
            assert_eq!(fields[0].1, "T");
        } else {
            panic!("Expected Struct statement");
        }
    }

    #[test]
    fn test_generic_enum_parsing() {
        let input = "enum Result<T, E> { Ok(T), Err(E) }";
        
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Should parse generic enum");
        
        assert_eq!(ast.len(), 1);
        if let Stmt::Enum(name, generic_params, variants, _span) = &ast[0] {
            assert_eq!(name, "Result");
            assert_eq!(generic_params, &vec!["T".to_string(), "E".to_string()]);
            assert_eq!(variants.len(), 2);
            assert_eq!(variants[0].0, "Ok");
            assert_eq!(variants[0].1, "T");
            assert_eq!(variants[1].0, "Err");
            assert_eq!(variants[1].1, "E");
        } else {
            panic!("Expected Enum statement");
        }
    }

    #[test]
    fn test_generic_async_function_parsing() {
        let input = "async fn fetch_data<T>() -> Promise<T> { }";
        
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Should parse generic async function");
        
        assert_eq!(ast.len(), 1);
        if let Stmt::AsyncFunction(name, generic_params, params, return_type, _body, _span) = &ast[0] {
            assert_eq!(name, "fetch_data");
            assert_eq!(generic_params, &vec!["T".to_string()]);
            assert_eq!(params.len(), 0);
            assert_eq!(return_type, "Promise<T>");
        } else {
            panic!("Expected AsyncFunction statement");
        }
    }

    #[test]
    fn test_no_generic_parameters() {
        let input = "fn regular_function(x: int) -> int { x }";
        
        let mut lexer = Lexer::new(input);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Should parse regular function");
        
        assert_eq!(ast.len(), 1);
        if let Stmt::Function(name, generic_params, params, return_type, _body, _span) = &ast[0] {
            assert_eq!(name, "regular_function");
            assert_eq!(generic_params, &Vec::<String>::new()); // Empty generic params
            assert_eq!(params.len(), 1);
            assert_eq!(return_type, "int");
        } else {
            panic!("Expected Function statement");
        }
    }
}