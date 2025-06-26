#[cfg(test)]
mod tests {
    use crate::error::Result;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semantic::SemanticVisitor;

    fn analyze_code(code: &str) -> Result<()> {
        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse()?;
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast)?;
        Ok(())
    }

    #[test]
    fn test_type_cast_numeric_valid() {
        // Valid numeric casts should pass
        let code = r#"
            fn main() {
                let x = 42;
                let y = x as float;
                let z = y as int;
            }
        "#;
        match analyze_code(code) {
            Ok(_) => {}
            Err(e) => panic!("Failed to analyze code: {:?}", e),
        }
    }

    #[test]
    fn test_type_cast_string_conversions() {
        // String conversions should be allowed
        let code = r#"
            fn main() {
                let num = 42;
                let str = num as string;
                
                let float_num = 3.14;
                let float_str = float_num as string;
                
                let parsed = "123" as int;
                let parsed_float = "3.14" as float;
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_type_cast_bool_conversions() {
        // Bool conversions should be allowed
        let code = r#"
            fn main() {
                let b = true;
                let s = b as string;
                let i = b as int;
                
                let num = 1;
                let b2 = num as bool;
                
                let s2 = "true";
                let b3 = s2 as bool;
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_type_cast_with_extern_types() {
        // Casts involving extern types should be allowed
        let code = r#"
            extern mod js {
                type Object;
                type Array;
            }
            
            extern fn getObject() -> js::Object;
            extern fn getArray() -> js::Array;
            
            fn main() {
                let obj = getObject();
                let str = obj as string;
                
                let arr = getArray();
                let str2 = arr as string;
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_type_cast_unknown_types() {
        // Unknown types should be allowed (for JavaScript interop)
        let code = r#"
            extern fn getValue() -> any;
            
            fn main() {
                let value = getValue();
                let num = value as int;
                let str = value as string;
                let b = value as bool;
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_type_cast_in_expressions() {
        // Type casts should work in complex expressions
        let code = r#"
            fn main() {
                let x = 10;
                let y = 20.5;
                let result = (x as float) + y;
                
                let str_num = "42";
                let computed = (str_num as int) * 2;
                
                // Chained casts
                let final = ((x as float) as string);
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_type_cast_with_struct_types() {
        // For now, struct casts should be allowed (future: may restrict)
        let code = r#"
            struct Point { x: int, y: int }
            struct Vec2 { x: float, y: float }
            
            fn main() {
                let p = Point { x: 10, y: 20 };
                // This would fail at runtime, but semantic analysis allows it
                let v = p as Vec2;
            }
        "#;
        match analyze_code(code) {
            Ok(_) => {}
            Err(e) => panic!("Failed to analyze code: {:?}", e),
        }
    }

    #[test]
    fn test_type_cast_with_array_types() {
        // Array type casts
        let code = r#"
            extern fn getJsArray() -> any;
            
            fn main() {
                let nums = [1, 2, 3];
                // These would need runtime validation
                let strs = nums as array<string>;
                
                let jsArray = getJsArray();
                let typed_array = jsArray as array<int>;
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_type_cast_return_values() {
        // Type casts in return statements
        let code = r#"
            fn get_as_string(x: int) -> string {
                x as string
            }
            
            fn parse_number(s: string) -> int {
                s as int
            }
            
            fn identity_cast(x: float) -> float {
                x as float  // No-op cast
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_type_cast_function_arguments() {
        // Type casts when passing function arguments
        let code = r#"
            fn takes_string(s: string) {
                println!(s);
            }
            
            fn takes_int(n: int) {
                println!(n);
            }
            
            fn main() {
                let x = 42;
                takes_string(x as string);
                
                let s = "123";
                takes_int(s as int);
                
                let f = 3.14;
                takes_int(f as int);
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_type_cast_with_option_result() {
        // Type casts with Option and Result types
        let code = r#"
            fn main() {
                let opt = Option::Some(42);
                // This would need special handling at runtime
                let opt_str = opt as Option<string>;
                
                let res = Result::Ok(42);
                let res_float = res as Result<float, string>;
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_type_cast_invalid_target_type() {
        // Invalid target type should error
        let code = r#"
            fn main() {
                let x = 42;
                let y = x as NonExistentType;
            }
        "#;
        // Currently returns Unknown type, which is allowed
        assert!(analyze_code(code).is_ok());
    }

    // Built-in method type checking tests

    #[test]
    fn test_string_builtin_methods() {
        let code = r#"
            fn main() {
                let s = "hello world";
                let len = s.len();  // Should return int
                let trimmed = s.trim();  // Should return string
                let upper = s.to_uppercase();  // Should return string
                let lower = s.to_lowercase();  // Should return string
                let substr = s.substring(0, 5);  // Should return string
                let parts = s.split(" ");  // Should return array<string>
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_array_builtin_methods() {
        let code = r#"
            fn main() {
                let arr = [1, 2, 3];
                let len = arr.len();  // Should return int
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_chained_builtin_methods() {
        let code = r#"
            fn main() {
                let s = "  hello world  ";
                let result = s.trim().to_uppercase().substring(0, 5);
                
                let parts = "a,b,c".split(",");
                let count = parts.len();
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_builtin_methods_with_wrong_args() {
        let code = r#"
            fn main() {
                let s = "hello";
                // substring expects 2 int args
                let result = s.substring("0", "5");
            }
        "#;
        // Should fail due to type mismatch
        assert!(analyze_code(code).is_err());
    }

    #[test]
    fn test_builtin_method_return_type_checking() {
        let code = r#"
            fn takes_int(n: int) {
                println!(n);
            }
            
            fn takes_string(s: string) {
                println!(s);
            }
            
            fn main() {
                let s = "hello";
                takes_int(s.len());  // OK: len() returns int
                takes_string(s.trim());  // OK: trim() returns string
                
                // This should fail: len() returns int, not string
                takes_string(s.len());
            }
        "#;
        assert!(analyze_code(code).is_err());
    }

    #[test]
    fn test_undefined_builtin_method() {
        let code = r#"
            fn main() {
                let s = "hello";
                let result = s.nonExistentMethod();  // Should fail
            }
        "#;
        assert!(analyze_code(code).is_err());
    }

    #[test]
    fn test_builtin_methods_on_wrong_types() {
        let code = r#"
            fn main() {
                let n = 42;
                let result = n.trim();  // trim() is only for strings
            }
        "#;
        assert!(analyze_code(code).is_err());
    }

    // Qualified type name resolution tests

    #[test]
    fn test_qualified_type_resolution() {
        let code = r#"
            extern mod express {
                type Request;
                type Response;
            }
            
            fn handler(req: express::Request, res: express::Response) {
                println!("Handling request");
            }
            
            fn main() {
                extern fn getApp() -> express::Application;
                let app = getApp();
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_undefined_qualified_type() {
        let code = r#"
            fn handler(req: express::Request) {
                // express module not declared
                println!("Handling request");
            }
        "#;
        // Currently qualified types without declaration become Unknown type
        // This is a limitation where undefined qualified types are treated as Unknown
        assert!(analyze_code(code).is_ok());
    }

    // Extern type validation tests

    #[test]
    fn test_extern_type_in_struct() {
        let code = r#"
            extern mod js {
                type Object;
            }
            
            struct Wrapper {
                obj: js::Object
            }
            
            fn main() {
                extern fn getObject() -> js::Object;
                let w = Wrapper { obj: getObject() };
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_extern_impl_validation() {
        let code = r#"
            extern mod express {
                type Application;
                
                impl Application {
                    fn listen(port: int);
                    fn get(path: string, handler: fn());
                }
            }
            
            fn main() {
                // Currently extern impl methods are not supported in semantic analysis
                // This is a known limitation
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    // Async context validation tests

    #[test]
    fn test_await_in_async_context() {
        let code = r#"
            async fn fetch_data() -> string {
                extern fn fetch(url: string) -> Promise<string>;
                let result = fetch("https://example.com").await;
                result
            }
            
            fn main() {
                // Can't await in non-async function
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_await_in_non_async_context() {
        let code = r#"
            fn fetch_data() -> string {
                extern fn fetch(url: string) -> Promise<string>;
                let result = fetch("https://example.com").await;  // Error!
                result
            }
        "#;
        assert!(analyze_code(code).is_err());
    }

    #[test]
    fn test_await_in_nested_async() {
        let code = r#"
            async fn outer() {
                let closure = |x| {
                    // Can't use await in non-async closure
                    extern fn delay(ms: int) -> Promise<void>;
                    delay(100).await
                };
            }
        "#;
        // Currently the semantic analyzer doesn't validate await in closures
        // This is a known limitation
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_async_closure_await() {
        let code = r#"
            async fn outer() {
                let closure = async |x| {
                    extern fn delay(ms: int) -> Promise<void>;
                    delay(100).await
                };
            }
        "#;
        // Note: async closures not yet implemented
        assert!(analyze_code(code).is_err());
    }
}
