#[cfg(test)]
mod tests {
    use crate::parser::ExternItem;
    use crate::{Lexer, Parser};

    #[test]
    fn test_parse_extern_impl_with_self() {
        let code = r#"
extern mod console {
    type Logger;
    
    impl Logger {
        fn log(self, message: string);
        fn error(self, message: string);
        fn warn(self, message: string);
    }
}
"#;

        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        // Check that parsing succeeds with self parameters
        assert_eq!(ast.len(), 1);

        // Verify the extern mod structure
        match &ast[0] {
            crate::Stmt::ExternMod(name, items, _) => {
                assert_eq!(name, "console");
                assert_eq!(items.len(), 2); // type Logger + impl Logger

                // Check impl block
                if let ExternItem::Impl(type_name, methods) = &items[1] {
                    assert_eq!(type_name, "Logger");
                    assert_eq!(methods.len(), 3);

                    // All methods should have self as first parameter
                    for method in methods {
                        if let ExternItem::Function(_, _, _, params, _) = method {
                            assert!(!params.is_empty());
                            assert_eq!(params[0].0, "self");
                        }
                    }
                }
            }
            _ => panic!("Expected ExternMod statement"),
        }
    }

    #[test]
    fn test_parse_extern_impl_mixed_self_and_no_self() {
        let code = r#"
extern mod fs {
    type File;
    
    fn open(path: string) -> File;
    
    impl File {
        fn read(self) -> string;
        fn write(self, data: string);
        fn metadata(path: string) -> string;
    }
}
"#;

        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(ast.len(), 1);

        match &ast[0] {
            crate::Stmt::ExternMod(name, items, _) => {
                assert_eq!(name, "fs");
                assert_eq!(items.len(), 3); // type File + fn open + impl File

                // Check the impl block
                if let ExternItem::Impl(type_name, methods) = &items[2] {
                    assert_eq!(type_name, "File");
                    assert_eq!(methods.len(), 3);

                    // Check each method's parameters
                    if let ExternItem::Function(_, name, _, params, _) = &methods[0] {
                        assert_eq!(name, "read");
                        assert_eq!(params.len(), 1);
                        assert_eq!(params[0].0, "self");
                    }

                    if let ExternItem::Function(_, name, _, params, _) = &methods[1] {
                        assert_eq!(name, "write");
                        assert_eq!(params.len(), 2);
                        assert_eq!(params[0].0, "self");
                        assert_eq!(params[1].0, "data");
                    }

                    if let ExternItem::Function(_, name, _, params, _) = &methods[2] {
                        assert_eq!(name, "metadata");
                        assert_eq!(params.len(), 1);
                        assert_eq!(params[0].0, "path");
                        // This one doesn't have self
                    }
                }
            }
            _ => panic!("Expected ExternMod statement"),
        }
    }

    #[test]
    fn test_parse_extern_impl_self_with_multiple_params() {
        let code = r#"
extern mod express {
    type Application;
    type Request;
    type Response;
    
    impl Application {
        fn get(self, path: string, handler: fn(Request, Response));
        fn post(self, path: string, handler: fn(Request, Response));
        fn use_middleware(self, middleware: fn(Request, Response, fn()));
        fn listen(self, port: int, callback: fn());
    }
}
"#;

        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(ast.len(), 1);

        match &ast[0] {
            crate::Stmt::ExternMod(name, items, _) => {
                assert_eq!(name, "express");

                // Find the impl block
                for item in items {
                    if let ExternItem::Impl(type_name, methods) = item {
                        assert_eq!(type_name, "Application");
                        assert_eq!(methods.len(), 4);

                        // All methods should have self as first parameter
                        for method in methods {
                            if let ExternItem::Function(_, method_name, _, params, _) = method {
                                assert!(
                                    !params.is_empty(),
                                    "Method {} has no parameters",
                                    method_name
                                );
                                assert_eq!(
                                    params[0].0, "self",
                                    "Method {} doesn't have self as first parameter",
                                    method_name
                                );

                                // Check specific methods have correct number of params
                                match method_name.as_str() {
                                    "get" | "post" => assert_eq!(params.len(), 3),
                                    "use_middleware" => assert_eq!(params.len(), 2), // self + middleware param
                                    "listen" => assert_eq!(params.len(), 3),
                                    _ => {}
                                }
                            }
                        }
                    }
                }
            }
            _ => panic!("Expected ExternMod statement"),
        }
    }

    #[test]
    fn test_parse_extern_impl_chained_methods() {
        let code = r#"
extern mod builder {
    type QueryBuilder;
    
    impl QueryBuilder {
        fn select(self, columns: string) -> QueryBuilder;
        fn from(self, table: string) -> QueryBuilder;
        fn where_clause(self, condition: string) -> QueryBuilder;
        fn order_by(self, column: string) -> QueryBuilder;
        fn limit(self, count: int) -> QueryBuilder;
        fn build(self) -> string;
    }
}
"#;

        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        assert_eq!(ast.len(), 1);

        match &ast[0] {
            crate::Stmt::ExternMod(name, items, _) => {
                assert_eq!(name, "builder");

                // Find the impl block
                for item in items {
                    if let ExternItem::Impl(type_name, methods) = item {
                        assert_eq!(type_name, "QueryBuilder");
                        assert_eq!(methods.len(), 6);

                        // All methods should have self and return appropriate type
                        for method in methods {
                            if let ExternItem::Function(_, method_name, _, params, return_type) =
                                method
                            {
                                assert_eq!(params[0].0, "self");

                                // Check return types
                                match method_name.as_str() {
                                    "build" => assert_eq!(return_type, "string"),
                                    _ => assert_eq!(return_type, "QueryBuilder"),
                                }
                            }
                        }
                    }
                }
            }
            _ => panic!("Expected ExternMod statement"),
        }
    }
}
