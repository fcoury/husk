use crate::ast::visitor::AstVisitor;
use crate::config::{HuskConfig, TargetConfig};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::transpiler::{JsTranspiler, ModuleFormat, TargetInfo, TargetPlatform};
use std::collections::HashMap;

fn create_test_config_with_tree_shaking() -> HuskConfig {
    let mut config = HuskConfig::default();

    // Create target with tree shaking enabled
    let browser_target = TargetConfig {
        platform: Some("browser".to_string()),
        format: Some("esm".to_string()),
        external: vec![],
        entry: None,
        globals: HashMap::new(),
        output: None,
        import_map: HashMap::new(),
        tree_shaking: true,
        dev: false,
    };
    config.targets.insert("browser".to_string(), browser_target);

    // Create target with tree shaking disabled
    let node_target = TargetConfig {
        platform: Some("node".to_string()),
        format: Some("esm".to_string()),
        external: vec![],
        entry: None,
        globals: HashMap::new(),
        output: None,
        import_map: HashMap::new(),
        tree_shaking: false,
        dev: false,
    };
    config.targets.insert("node".to_string(), node_target);

    config
}

#[test]
fn test_tree_shaking_pure_annotations_enabled() {
    let _config = create_test_config_with_tree_shaking();

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

    // Test pure Math function calls
    let input = r#"
        fn test() {
            let result = Math.max(1, 2);
            let sqrt_val = Math.sqrt(16);
            let parsed = parseInt("42");
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Should contain pure annotations
    assert!(result.contains("/*#__PURE__*/Math.max(1, 2)"));
    assert!(result.contains("/*#__PURE__*/Math.sqrt(16)"));
    assert!(result.contains("/*#__PURE__*/parseInt(\"42\")"));
}

#[test]
fn test_tree_shaking_pure_annotations_disabled() {
    let _config = create_test_config_with_tree_shaking();

    let target_info = TargetInfo {
        platform: TargetPlatform::NodeJs,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map: HashMap::new(),
        tree_shaking: false,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test the same functions but with tree shaking disabled
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

    // Should NOT contain pure annotations
    assert!(!result.contains("/*#__PURE__*/"));
    assert!(result.contains("Math.max(1, 2)"));
    assert!(result.contains("Math.sqrt(16)"));
}

#[test]
fn test_constructor_calls_get_pure_annotation() {
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

    // Test constructor calls (capital first letter)
    let input = r#"
        fn test() {
            let date = Date();
            let array = Array(1, 2, 3);
            let obj = Object();
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Constructor calls should get pure annotations
    assert!(result.contains("/*#__PURE__*/Date()"));
    assert!(result.contains("/*#__PURE__*/Array(1, 2, 3)"));
    assert!(result.contains("/*#__PURE__*/Object()"));
}

#[test]
fn test_object_methods_get_pure_annotation() {
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

    // Test Object static methods
    let input = r#"
        fn test() {
            let keys = Object.keys(obj);
            let values = Object.values(obj);
            let created = Object.create(proto);
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Object methods should get pure annotations
    assert!(result.contains("/*#__PURE__*/Object.keys(obj)"));
    assert!(result.contains("/*#__PURE__*/Object.values(obj)"));
    assert!(result.contains("/*#__PURE__*/Object.create(proto)"));
}

#[test]
fn test_json_methods_get_pure_annotation() {
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

    // Test JSON methods
    let input = r#"
        fn test() {
            let parsed = JSON.parse(jsonString);
            let serialized = JSON.stringify(obj);
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // JSON methods should NOT get pure annotations (they can throw exceptions)
    assert!(!result.contains("/*#__PURE__*/JSON.parse"));
    assert!(!result.contains("/*#__PURE__*/JSON.stringify"));

    // But they should still be called normally
    assert!(result.contains("JSON.parse(jsonString)"));
    assert!(result.contains("JSON.stringify(obj)"));
}

#[test]
fn test_husk_utilities_get_pure_annotation() {
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

    // Test Husk-specific utilities
    let input = r#"
        fn test() {
            let safe = huskSafeCall("test");
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Husk utilities should get pure annotations
    assert!(result.contains("/*#__PURE__*/huskSafeCall(\"test\")"));
}

#[test]
fn test_non_pure_functions_no_annotation() {
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

    // Test functions that should NOT get pure annotations
    let input = r#"
        fn test() {
            console.log("test");
            document.getElementById("test");
            localStorage.setItem("key", "value");
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Non-pure functions should NOT get annotations
    assert!(!result.contains("/*#__PURE__*/console.log"));
    assert!(!result.contains("/*#__PURE__*/document.getElementById"));
    assert!(!result.contains("/*#__PURE__*/localStorage.setItem"));

    // But they should still be called normally
    assert!(result.contains("console.log(\"test\")"));
    assert!(result.contains("document.getElementById(\"test\")"));
    assert!(result.contains("localStorage.setItem(\"key\", \"value\")"));
}

#[test]
fn test_no_target_info_no_annotations() {
    // Test without target info (tree shaking disabled by default)
    let mut transpiler = JsTranspiler::new();
    // Don't set target_info - it will be None

    let input = r#"
        fn test() {
            let result = Math.max(1, 2);
        }
    "#;

    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();

    // Should NOT contain pure annotations when no target info
    assert!(!result.contains("/*#__PURE__*/"));
    assert!(result.contains("Math.max(1, 2)"));
}
