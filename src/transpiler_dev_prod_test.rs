use crate::ast::visitor::AstVisitor;
use crate::config::{HuskConfig, TargetConfig};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::transpiler::{JsTranspiler, ModuleFormat, TargetInfo, TargetPlatform};
use std::collections::HashMap;

fn create_test_config_with_dev_mode(dev: bool) -> HuskConfig {
    let mut config = HuskConfig::default();

    // Create target with dev mode enabled/disabled
    let target = TargetConfig {
        platform: Some("browser".to_string()),
        format: Some("esm".to_string()),
        external: vec![],
        entry: None,
        globals: HashMap::new(),
        output: None,
        import_map: HashMap::new(),
        tree_shaking: true, // Enable tree shaking to test dev mode override
        dev,
    };
    config.targets.insert("browser".to_string(), target);

    config
}

#[test]
fn test_development_mode_debugging_comments() {
    let _config = create_test_config_with_dev_mode(true);

    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map: HashMap::new(),
        tree_shaking: true,
        dev: true,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: Function should have debug comment in dev mode
    let input = r#"
        fn calculateTotal(price: number) -> number {
            return price * 1.1;
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Should contain debug comment for function
    assert!(result.contains("/* Function: calculateTotal */"));
    assert!(result.contains("function calculateTotal(price)"));
}

#[test]
fn test_production_mode_no_debugging_comments() {
    let _config = create_test_config_with_dev_mode(false);

    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map: HashMap::new(),
        tree_shaking: true,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: Function should NOT have debug comment in production mode
    let input = r#"
        fn calculateTotal(price: number) -> number {
            return price * 1.1;
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Should NOT contain debug comments
    assert!(!result.contains("/* Function: calculateTotal */"));
    assert!(result.contains("function calculateTotal(price)"));
}

#[test]
fn test_development_mode_variable_comments() {
    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map: HashMap::new(),
        tree_shaking: true,
        dev: true,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: Variable should have debug comment in dev mode
    let input = r#"
        fn test() {
            let userName = "Alice";
            let age = 30;
            let isActive = true;
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Should contain debug comments for variables
    assert!(result.contains("/* Variable: userName */"));
    assert!(result.contains("/* Variable: age */"));
    assert!(result.contains("/* Variable: isActive */"));
}

#[test]
fn test_development_mode_runtime_type_assertions() {
    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map: HashMap::new(),
        tree_shaking: true,
        dev: true,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: Variable should have runtime type assertion in dev mode
    let input = r#"
        fn test() {
            let userName = "Alice";
            let age = 30;
            let isActive = true;
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Should contain runtime type assertions
    assert!(result.contains("if (typeof userName !== 'string')"));
    assert!(result.contains("if (typeof age !== 'number')"));
    assert!(result.contains("if (typeof isActive !== 'boolean')"));
}

#[test]
fn test_production_mode_no_runtime_assertions() {
    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map: HashMap::new(),
        tree_shaking: true,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: Variable should NOT have runtime assertions in production mode
    let input = r#"
        fn test() {
            let userName = "Alice";
            let age = 30;
            let isActive = true;
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Should NOT contain runtime type assertions
    assert!(!result.contains("if (typeof"));
    assert!(!result.contains("console.warn"));
}

#[test]
fn test_development_mode_disables_tree_shaking() {
    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map: HashMap::new(),
        tree_shaking: true, // Tree shaking enabled
        dev: true,          // But dev mode should disable it
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: Pure function calls should NOT get annotations in dev mode
    let input = r#"
        fn test() {
            let result = Math.max(1, 2);
            let sqrt_val = Math.sqrt(16);
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Should NOT contain pure annotations in dev mode (faster builds)
    assert!(!result.contains("/*#__PURE__*/"));
    assert!(result.contains("Math.max(1, 2)"));
    assert!(result.contains("Math.sqrt(16)"));
}

#[test]
fn test_production_mode_enables_tree_shaking() {
    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map: HashMap::new(),
        tree_shaking: true, // Tree shaking enabled
        dev: false,         // Production mode
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: Pure function calls should get annotations in production mode
    let input = r#"
        fn test() {
            let result = Math.max(1, 2);
            let sqrt_val = Math.sqrt(16);
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Should contain pure annotations in production mode
    assert!(result.contains("/*#__PURE__*/Math.max(1, 2)"));
    assert!(result.contains("/*#__PURE__*/Math.sqrt(16)"));
}

#[test]
fn test_no_target_info_defaults_to_production() {
    // Test without target info (should default to production behavior)
    let mut transpiler = JsTranspiler::new();
    // Don't set target_info - it will be None

    let input = r#"
        fn test() {
            let userName = "Alice";
            let result = Math.max(1, 2);
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Should behave like production mode (no debug comments, no assertions)
    assert!(!result.contains("/* Variable: userName */"));
    assert!(!result.contains("if (typeof"));
    assert!(!result.contains("/*#__PURE__*/")); // No tree shaking without target info
}
