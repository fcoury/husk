#[cfg(test)]
mod tests {
    use crate::error::Result;
    use crate::semantic::SemanticVisitor;
    use crate::transpiler::JsTranspiler;
    use crate::{Lexer, Parser};

    fn transpile_code(code: &str) -> Result<String> {
        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse()?;

        // Run semantic analysis
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast)?;

        // Transpile to JavaScript
        let mut transpiler = JsTranspiler::new();
        transpiler.generate(&ast)
    }

    // Type casting transpilation tests

    #[test]
    fn test_transpile_int_to_float() {
        let code = r#"
            fn main() {
                let x = 42;
                let y = x as float;
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let x = 42;"));
        assert!(js.contains("let y = Number(x);"));
    }

    #[test]
    fn test_transpile_float_to_int() {
        let code = r#"
            fn main() {
                let x = 42.7;
                let y = x as int;
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let x = 42.7;"));
        assert!(js.contains("let y = Math.floor(Number(x));"));
    }

    #[test]
    fn test_transpile_int_to_string() {
        let code = r#"
            fn main() {
                let x = 42;
                let y = x as string;
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let x = 42;"));
        assert!(js.contains("let y = String(x);"));
    }

    #[test]
    fn test_transpile_bool_to_string() {
        let code = r#"
            fn main() {
                let b = true;
                let s = b as string;
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let b = true;"));
        assert!(js.contains("let s = String(b);"));
    }

    #[test]
    fn test_transpile_string_to_int() {
        let code = r#"
            fn main() {
                let s = "42";
                let n = s as int;
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let s = \"42\";"));
        assert!(js.contains("let n = Math.floor(Number(s));"));
    }

    #[test]
    fn test_transpile_bool_to_int() {
        let code = r#"
            fn main() {
                let b = true;
                let n = b as int;
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let b = true;"));
        assert!(js.contains("let n = Math.floor(Number(b));"));
    }

    #[test]
    fn test_transpile_cast_in_expression() {
        let code = r#"
            fn main() {
                let x = 10;
                let y = 20.5;
                let result = (x as float) + y;
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let result = (Number(x) + y);"));
    }

    #[test]
    fn test_transpile_chained_casts() {
        let code = r#"
            fn main() {
                let x = 42;
                let s = ((x as float) as string);
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let s = String(Number(x));"));
    }

    // Built-in method transpilation tests

    #[test]
    fn test_transpile_string_len() {
        let code = r#"
            fn main() {
                let s = "hello";
                let len = s.len();
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let s = \"hello\";"));
        assert!(js.contains("let len = s.length;"));
    }

    #[test]
    fn test_transpile_string_trim() {
        let code = r#"
            fn main() {
                let s = "  hello  ";
                let trimmed = s.trim();
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let trimmed = s.trim();"));
    }

    #[test]
    fn test_transpile_string_to_uppercase() {
        let code = r#"
            fn main() {
                let s = "hello";
                let upper = s.to_uppercase();
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let upper = s.toUpperCase();"));
    }

    #[test]
    fn test_transpile_string_to_lowercase() {
        let code = r#"
            fn main() {
                let s = "HELLO";
                let lower = s.to_lowercase();
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let lower = s.toLowerCase();"));
    }

    #[test]
    fn test_transpile_string_substring() {
        let code = r#"
            fn main() {
                let s = "hello world";
                let sub = s.substring(0, 5);
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let sub = s.substring(0, 5);"));
    }

    #[test]
    fn test_transpile_string_split() {
        let code = r#"
            fn main() {
                let s = "a,b,c";
                let parts = s.split(",");
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let parts = s.split(\",\");"));
    }

    #[test]
    fn test_transpile_array_len() {
        let code = r#"
            fn main() {
                let arr = [1, 2, 3];
                let len = arr.len();
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let arr = [1, 2, 3];"));
        assert!(js.contains("let len = arr.length;"));
    }

    #[test]
    fn test_transpile_chained_methods() {
        let code = r#"
            fn main() {
                let s = "  hello  ";
                let result = s.trim().to_uppercase();
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let result = s.trim().toUpperCase();"));
    }

    #[test]
    fn test_transpile_method_on_cast() {
        let code = r#"
            fn main() {
                let n = 42;
                let len = (n as string).len();
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let len = String(n).length;"));
    }

    // Module system transpilation tests

    #[test]
    fn test_transpile_import_local_module() {
        let code = r#"
            use local::utils::helper;
            
            fn main() {
                helper();
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("import { helper } from './utils/helper.js';"));
    }

    #[test]
    fn test_transpile_import_npm_package() {
        let code = r#"
            use express::express;
            
            fn main() {
                let app = express();
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("import { express } from 'express/express';"));
    }

    #[test]
    fn test_transpile_export_function() {
        let code = r#"
            pub fn greet(name: string) -> string {
                return format!("Hello, {}", name);
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("export function greet(name)"));
        assert!(js.contains("return `Hello, ${name}`;"));
    }

    #[test]
    fn test_transpile_multiple_imports() {
        let code = r#"
            use fs::readFile;
            use path::join;
            use local::utils::{helper1, helper2};
            
            fn main() {
                helper1();
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("import { readFile } from 'fs/readFile';"));
        assert!(js.contains("import { join } from 'path/join';"));
        assert!(js.contains("import { helper1, helper2 } from './utils.js';"));
    }

    #[test]
    fn test_transpile_async_function() {
        let code = r#"
            async fn fetchData() -> string {
                return "data";
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("async function fetchData()"));
        assert!(js.contains("return \"data\";"));
    }

    #[test]
    fn test_transpile_await_expression() {
        let code = r#"
            extern fn fetchData() -> Promise<string>;
            
            async fn main() {
                let data = fetchData().await;
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let data = await fetchData();"));
    }

    #[test]
    fn test_transpile_template_literal() {
        let code = r#"
            fn main() {
                let name = "world";
                let msg = format!("Hello, {}!", name);
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let msg = `Hello, ${name}!`;"));
    }

    #[test]
    fn test_transpile_format_macro() {
        let code = r#"
            fn main() {
                let x = 42;
                let s = format!("The answer is {}", x);
            }
        "#;

        let js = transpile_code(code).unwrap();
        assert!(js.contains("let s = `The answer is ${x}`;"));
    }
}
