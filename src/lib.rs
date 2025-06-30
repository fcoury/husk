mod ast;
mod builtin_methods;
mod config;
pub mod dts;
mod error;
mod error_tests;
mod integration_error_tests;
mod interpreter;
mod io;
mod lexer;
mod package_resolver;
mod parser;
mod parser_error_tests;
mod repl;
mod runtime_error_tests;
mod semantic;
mod semantic_error_tests;
mod span;
pub mod test_registry;
pub mod test_runner;
pub mod test_transpiler;
mod transpiler;
mod transpiler_error_tests;
mod types;

#[cfg(test)]
mod interpreter_modules_test;

#[cfg(test)]
mod extern_test;

#[cfg(test)]
mod extern_transpiler_test;

#[cfg(test)]
mod async_test;

#[cfg(test)]
mod promise_type_test;

#[cfg(test)]
mod test_generic_extern;

#[cfg(test)]
mod generic_extern_comprehensive_test;

#[cfg(test)]
mod closure_test;

#[cfg(test)]
mod format_test;

#[cfg(test)]
mod option_result_test;

#[cfg(test)]
mod try_operator_test;

#[cfg(test)]
mod await_try_test;

#[cfg(test)]
mod error_mapping_test;

#[cfg(test)]
mod generic_types_test;

#[cfg(test)]
mod config_test;

#[cfg(test)]
mod package_resolution_test;

#[cfg(test)]
mod semantic_js_interop_tests;

#[cfg(test)]
mod interpreter_js_interop_tests;

#[cfg(test)]
mod transpiler_js_interop_tests;

#[cfg(test)]
mod transpiler_target_modes_test;

pub use config::HuskConfig;
pub use error::{Error, Result};
pub use interpreter::{InterpreterVisitor, Value};
pub use lexer::Lexer;
pub use package_resolver::PackageResolver;
pub use parser::{Parser, Stmt};
pub use repl::repl;
pub use semantic::SemanticVisitor;
pub use test_registry::TestRegistry;
pub use test_runner::TestResult;
use transpiler::JsTranspiler;

pub fn execute_script(code: impl Into<String>) -> Result<Value> {
    let code = code.into();
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // Use visitor pattern for semantic analysis
    let mut analyzer = SemanticVisitor::new();
    analyzer.analyze(&ast)?;

    // Use visitor pattern for interpretation
    let mut interpreter = InterpreterVisitor::new();
    interpreter.interpret(&ast)
}

pub fn execute_script_with_context(
    code: impl Into<String>,
    current_file: Option<std::path::PathBuf>,
    project_root: Option<std::path::PathBuf>,
) -> Result<Value> {
    let code = code.into();
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // Use visitor pattern for semantic analysis
    let mut analyzer = SemanticVisitor::with_context(current_file.clone(), project_root.clone());
    analyzer.analyze(&ast)?;

    // Use visitor pattern for interpretation with context
    let mut interpreter = InterpreterVisitor::with_context(current_file, project_root);
    interpreter.interpret(&ast)
}

pub fn transpile_to_js(code: impl Into<String>) -> Result<String> {
    let code = code.into();
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // Use visitor pattern for semantic analysis
    let mut analyzer = SemanticVisitor::new();
    analyzer.analyze(&ast)?;

    let mut js_generator = JsTranspiler::new();
    // Pass type-only imports information to transpiler
    js_generator.set_type_only_imports(analyzer.get_type_only_imports().clone());
    // Pass extern functions mapping to transpiler
    js_generator.set_extern_functions(analyzer.get_extern_functions().clone());
    js_generator.generate(&ast)
}

pub fn transpile_to_js_with_packages(code: impl Into<String>) -> Result<String> {
    let code = code.into();
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // Use visitor pattern for semantic analysis with package resolution
    let mut analyzer = SemanticVisitor::with_package_resolver().unwrap_or_default();
    analyzer.analyze(&ast)?;

    // Use transpiler with package resolution
    let mut js_generator = match JsTranspiler::with_package_resolver() {
        Ok(transpiler) => transpiler,
        Err(_) => {
            // Fall back to basic transpiler if package resolution fails
            JsTranspiler::new()
        }
    };
    // Pass type-only imports information to transpiler
    js_generator.set_type_only_imports(analyzer.get_type_only_imports().clone());
    // Pass extern functions mapping to transpiler
    js_generator.set_extern_functions(analyzer.get_extern_functions().clone());
    js_generator.generate(&ast)
}

pub fn transpile_to_js_with_target(code: impl Into<String>, target: &str) -> Result<String> {
    let code = code.into();
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // Use visitor pattern for semantic analysis with package resolution
    let mut analyzer = SemanticVisitor::with_package_resolver().unwrap_or_default();
    analyzer.analyze(&ast)?;

    // Use transpiler with target configuration
    let mut js_generator = match JsTranspiler::with_target(target) {
        Ok(transpiler) => transpiler,
        Err(_) => {
            // Fall back to basic transpiler if target parsing fails
            JsTranspiler::new()
        }
    };
    // Pass type-only imports information to transpiler
    js_generator.set_type_only_imports(analyzer.get_type_only_imports().clone());
    // Pass extern functions mapping to transpiler
    js_generator.set_extern_functions(analyzer.get_extern_functions().clone());
    js_generator.transpile(&ast)
}

/// Transpile Husk code to JavaScript with target configuration and entry point validation
pub fn transpile_to_js_with_entry_validation(
    code: impl Into<String>,
    target: &str,
    is_main_entry: bool,
    current_file: Option<&std::path::Path>,
    project_root: Option<&std::path::Path>,
) -> Result<String> {
    let code = code.into();
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // Check for main function if this is the main entry point
    if is_main_entry {
        let has_main = ast.iter().any(|stmt| {
            matches!(
                stmt,
                crate::parser::Stmt::Function(_, _, name, _, _, _, _, _)
                | crate::parser::Stmt::AsyncFunction(_, _, name, _, _, _, _, _)
                if name == "main"
            )
        });

        if !has_main {
            return Err(Error::new_semantic(
                "Main entry point must contain a 'main' function".to_string(),
                crate::span::Span::default(),
            ));
        }
    }

    // Use visitor pattern for semantic analysis with package resolution and file context
    let mut analyzer = SemanticVisitor::with_package_resolver().unwrap_or_default();
    if let Some(file) = current_file {
        analyzer.set_current_file(file.to_path_buf());
    }
    if let Some(root) = project_root {
        analyzer.set_project_root(root.to_path_buf());
    }
    analyzer.analyze(&ast)?;

    // Use transpiler with target configuration
    let mut js_generator = match JsTranspiler::with_target(target) {
        Ok(transpiler) => transpiler,
        Err(_) => {
            // Fall back to basic transpiler if target parsing fails
            JsTranspiler::new()
        }
    };
    // Pass type-only imports information to transpiler
    js_generator.set_type_only_imports(analyzer.get_type_only_imports().clone());
    // Pass extern functions mapping to transpiler
    js_generator.set_extern_functions(analyzer.get_extern_functions().clone());
    js_generator.transpile(&ast)
}

#[cfg(test)]
mod lib_tests {
    use super::*;

    #[test]
    fn test_entry_validation_with_main() {
        let code = r#"
            fn helper() {
                println!("Helper function");
            }
            
            fn main() {
                println!("Hello from main!");
                helper();
            }
        "#;

        // Should succeed when main is present and is_main_entry is true
        let result = transpile_to_js_with_entry_validation(code, "node-esm", true, None, None);
        assert!(result.is_ok());
        let js = result.unwrap();
        assert!(js.contains("function main()"));
        assert!(js.contains("main();"));
    }

    #[test]
    fn test_entry_validation_without_main() {
        let code = r#"
            fn helper() {
                println!("Helper function");
            }
            
            fn another_function() {
                println!("Another function");
            }
        "#;

        // Should fail when main is missing and is_main_entry is true
        let result = transpile_to_js_with_entry_validation(code, "node-esm", true, None, None);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err
            .to_string()
            .contains("Main entry point must contain a 'main' function"));
    }

    #[test]
    fn test_entry_validation_non_entry_file() {
        let code = r#"
            fn helper() {
                println!("Helper function");
            }
            
            fn another_function() {
                println!("Another function");
            }
        "#;

        // Should succeed when main is missing but is_main_entry is false
        let result = transpile_to_js_with_entry_validation(code, "node-esm", false, None, None);
        assert!(result.is_ok());
        let js = result.unwrap();
        assert!(js.contains("function helper()"));
        assert!(js.contains("function another_function()"));
        assert!(!js.contains("main();"));
    }

    #[test]
    fn test_entry_validation_async_main() {
        let code = r#"
            async fn main() {
                println!("Hello from async main!");
            }
        "#;

        // Should succeed with async main when is_main_entry is true
        let result = transpile_to_js_with_entry_validation(code, "node-esm", true, None, None);
        assert!(result.is_ok());
        let js = result.unwrap();

        assert!(js.contains("async function main()"));
        assert!(js.contains("main();"));
    }

    #[test]
    fn test_entry_validation_with_different_targets() {
        let code = r#"
            fn main() {
                println!("Hello!");
            }
        "#;

        // Test with different targets
        for target in &["node-esm", "node-cjs", "browser", "deno", "bun"] {
            let result = transpile_to_js_with_entry_validation(code, target, true, None, None);
            assert!(result.is_ok(), "Failed for target: {}", target);
        }
    }
}
