#[cfg(test)]
mod extern_test {
    use crate::{execute_script, Value};

    #[test]
    fn test_extern_function() {
        let code = r#"
extern fn parseInt(s: string) -> int;

// Extern functions are no-op in interpreter
parseInt("42")
"#;
        
        // In interpreter mode, extern functions don't exist
        let result = execute_script(code);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Function 'parseInt' not found"));
    }
    
    #[test]
    fn test_extern_mod() {
        let code = r#"
extern mod fs {
    fn readFile(path: string) -> string;
    fn writeFile(path: string, data: string);
}

// Just declaring extern mod shouldn't error
42
"#;
        
        let result = execute_script(code);
        assert_eq!(result.unwrap(), Value::Int(42));
    }
    
    #[test]
    fn test_extern_nested_mod() {
        let code = r#"
extern mod fs {
    mod promises {
        fn readFile(path: string) -> string;
        fn writeFile(path: string, data: string);
    }
}

// Nested extern mods are ok
"nested works"
"#;
        
        let result = execute_script(code);
        assert_eq!(result.unwrap(), Value::String("nested works".to_string()));
    }
    
    #[test]
    fn test_extern_with_impl() {
        let code = r#"
extern mod express {
    type Application;
    
    fn express() -> Application;
    
    impl Application {
        fn get(path: string, handler: fn());
        fn listen(port: int);
    }
}

// Type declarations are ok
true
"#;
        
        let result = execute_script(code);
        assert_eq!(result.unwrap(), Value::Bool(true));
    }
}