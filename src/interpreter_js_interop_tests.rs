#[cfg(test)]
mod tests {
    use crate::error::Result;
    use crate::{execute_script, Value};

    fn run_code(code: &str) -> Result<Value> {
        execute_script(code)
    }

    // Type casting runtime tests

    #[test]
    fn test_cast_int_to_float() {
        let code = r#"
            fn main() -> float {
                let x = 42;
                return x as float;
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::Float(f)) => assert_eq!(f, 42.0),
            other => panic!("Expected float 42.0, got {:?}", other),
        }
    }

    #[test]
    fn test_cast_float_to_int() {
        let code = r#"
            fn main() -> int {
                let x = 42.7;
                return x as int;  // Should truncate like Rust
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::Int(i)) => assert_eq!(i, 42),
            other => panic!("Expected int 42, got {:?}", other),
        }
    }

    #[test]
    fn test_cast_int_to_string() {
        let code = r#"
            fn main() -> string {
                let x = 42;
                return x as string;
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::String(s)) => assert_eq!(s, "42"),
            other => panic!("Expected string '42', got {:?}", other),
        }
    }

    #[test]
    fn test_cast_float_to_string() {
        let code = r#"
            fn main() -> string {
                let x = 42.5;
                return x as string;
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::String(s)) => assert_eq!(s, "42.5"),
            other => panic!("Expected string '42.5', got {:?}", other),
        }
    }

    #[test]
    fn test_cast_bool_to_string() {
        let code = r#"
            fn main() -> string {
                let b = true;
                return b as string;
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::String(s)) => assert_eq!(s, "true"),
            other => panic!("Expected string 'true', got {:?}", other),
        }
    }

    #[test]
    fn test_cast_string_to_int() {
        let code = r#"
            fn main() -> int {
                let s = "42";
                return s as int;
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::Int(i)) => assert_eq!(i, 42),
            other => panic!("Expected int 42, got {:?}", other),
        }
    }

    #[test]
    fn test_cast_string_to_float() {
        let code = r#"
            fn main() -> float {
                let s = "42.5";
                return s as float;
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::Float(f)) => assert_eq!(f, 42.5),
            other => panic!("Expected float 42.5, got {:?}", other),
        }
    }

    #[test]
    fn test_cast_invalid_string_to_int() {
        let code = r#"
            fn main() -> int {
                let s = "not a number";
                return s as int;
            }
            
            main()
        "#;

        // Should return an error for invalid cast
        assert!(run_code(code).is_err());
    }

    #[test]
    fn test_cast_bool_to_int() {
        let code = r#"
            fn test_bool_cast() -> int {
                let t = true;
                let f = false;
                let t_int = t as int;
                let f_int = f as int;
                if t_int == 1 {
                    if f_int == 0 {
                        return 1;
                    }
                    return 2;
                }
                return 3;
            }
            
            test_bool_cast()
        "#;

        match run_code(code) {
            Ok(Value::Int(i)) => assert_eq!(i, 1),
            other => panic!("Expected 1, got {:?}", other),
        }
    }

    #[test]
    fn test_cast_in_expression() {
        let code = r#"
            fn main() -> float {
                let x = 10;
                let y = 20.5;
                return (x as float) + y;
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::Float(f)) => assert_eq!(f, 30.5),
            other => panic!("Expected 30.5, got {:?}", other),
        }
    }

    #[test]
    fn test_chained_casts() {
        let code = r#"
            fn main() -> string {
                let x = 42;
                return ((x as float) as string);
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::String(s)) => assert_eq!(s, "42"),
            other => panic!("Expected string '42', got {:?}", other),
        }
    }

    #[test]
    fn test_cast_array_to_string() {
        let code = r#"
            fn main() -> string {
                let arr = [1, 2, 3];
                return arr as string;
            }
            
            main()
        "#;

        // Currently array to string cast is not supported in interpreter
        assert!(run_code(code).is_err());
    }

    // Built-in method runtime tests

    #[test]
    fn test_string_len() {
        let code = r#"
            fn main() -> int {
                let s = "hello";
                return s.len();
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::Int(i)) => assert_eq!(i, 5),
            other => panic!("Expected 5, got {:?}", other),
        }
    }

    #[test]
    fn test_string_len_utf8() {
        let code = r#"
            fn main() -> int {
                let s = "hello world";
                return s.len();  // Should count characters
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::Int(i)) => assert_eq!(i, 11),
            other => panic!("Expected 11, got {:?}", other),
        }
    }

    #[test]
    fn test_string_trim() {
        let code = r#"
            fn main() -> string {
                let s = "  hello world  ";
                return s.trim();
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::String(s)) => assert_eq!(s, "hello world"),
            other => panic!("Expected 'hello world', got {:?}", other),
        }
    }

    #[test]
    fn test_string_to_uppercase() {
        let code = r#"
            fn main() -> string {
                let s = "hello world";
                return s.toUpperCase();
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::String(s)) => assert_eq!(s, "HELLO WORLD"),
            other => panic!("Expected 'HELLO WORLD', got {:?}", other),
        }
    }

    #[test]
    fn test_string_to_lowercase() {
        let code = r#"
            fn main() -> string {
                let s = "HELLO WORLD";
                return s.toLowerCase();
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::String(s)) => assert_eq!(s, "hello world"),
            other => panic!("Expected 'hello world', got {:?}", other),
        }
    }

    #[test]
    fn test_string_substring() {
        let code = r#"
            fn main() -> string {
                let s = "hello world";
                return s.substring(0, 5);
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::String(s)) => assert_eq!(s, "hello"),
            other => panic!("Expected 'hello', got {:?}", other),
        }
    }

    #[test]
    fn test_string_split() {
        let code = r#"
            fn main() -> int {
                let s = "one,two,three";
                let parts = s.split(",");
                return parts.len();
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::Int(i)) => assert_eq!(i, 3),
            other => panic!("Expected 3, got {:?}", other),
        }
    }

    #[test]
    fn test_array_len() {
        let code = r#"
            fn main() -> int {
                let arr = [1, 2, 3, 4, 5];
                return arr.len();
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::Int(i)) => assert_eq!(i, 5),
            other => panic!("Expected 5, got {:?}", other),
        }
    }

    #[test]
    fn test_chained_methods() {
        let code = r#"
            fn main() -> string {
                let s = "  hello world  ";
                return s.trim().toUpperCase();
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::String(s)) => assert_eq!(s, "HELLO WORLD"),
            other => panic!("Expected 'HELLO WORLD', got {:?}", other),
        }
    }

    #[test]
    fn test_method_on_cast_result() {
        let code = r#"
            fn main() -> int {
                let n = 42;
                return (n as string).len();
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::Int(i)) => assert_eq!(i, 2),
            other => panic!("Expected 2, got {:?}", other),
        }
    }

    #[test]
    fn test_cast_method_result() {
        let code = r#"
            fn main() -> string {
                let s = "hello";
                return (s.len()) as string;
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::String(s)) => assert_eq!(s, "5"),
            other => panic!("Expected '5', got {:?}", other),
        }
    }

    #[test]
    fn test_string_substring_bounds() {
        let code = r#"
            fn main() -> string {
                let s = "hello";
                return s.substring(1, 4);
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::String(s)) => assert_eq!(s, "ell"),
            other => panic!("Expected 'ell', got {:?}", other),
        }
    }

    #[test]
    fn test_string_split_result() {
        let code = r#"
            fn main() -> string {
                let s = "a,b,c";
                let parts = s.split(",");
                if parts.len() == 3 {
                    return "success";
                }
                return "fail";
            }
            
            main()
        "#;

        match run_code(code) {
            Ok(Value::String(s)) => assert_eq!(s, "success"),
            other => panic!("Expected 'success', got {:?}", other),
        }
    }
}
