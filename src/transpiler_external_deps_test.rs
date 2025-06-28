use crate::ast::visitor::AstVisitor;
use crate::config::{HuskConfig, TargetConfig};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::transpiler::{JsTranspiler, ModuleFormat, TargetInfo, TargetPlatform};
use std::collections::HashMap;

fn create_test_config_with_external() -> HuskConfig {
    let mut config = HuskConfig::default();

    // Add browser target with axios as external
    let browser_target = TargetConfig {
        platform: None,
        format: None,
        external: vec!["axios".to_string(), "react".to_string()],
        entry: None,
        globals: HashMap::new(),
        output: None,
        import_map: HashMap::new(),
        tree_shaking: false,
        dev: false,
    };
    config.targets.insert("browser".to_string(), browser_target);

    // Add node target with different external deps
    let node_target = TargetConfig {
        platform: None,
        format: None,
        external: vec!["aws-sdk".to_string()],
        entry: None,
        globals: HashMap::new(),
        output: None,
        import_map: HashMap::new(),
        tree_shaking: false,
        dev: false,
    };
    config.targets.insert("node-esm".to_string(), node_target);

    config
}

#[test]
fn test_external_dependency_browser() {
    let _config = create_test_config_with_external();
    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec!["axios".to_string(), "react".to_string()],
        import_map: HashMap::new(),
        tree_shaking: false,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: axios should be imported directly without resolution
    let input = "use axios;";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();
    assert_eq!(result, "import 'axios'");
}

#[test]
fn test_external_dependency_with_named_imports() {
    let _config = create_test_config_with_external();
    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec!["axios".to_string()],
        import_map: HashMap::new(),
        tree_shaking: false,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: Named imports from external package
    let input = "use axios::{get, post};";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();
    assert_eq!(result, "import { get, post } from 'axios'");
}

#[test]
fn test_non_external_dependency() {
    let _config = create_test_config_with_external();
    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec!["axios".to_string()],
        import_map: HashMap::new(),
        tree_shaking: false,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: lodash is not external, so it should fall back to basic import
    // (without package resolver it will still generate basic import)
    let input = "use lodash;";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();
    assert_eq!(result, "import 'lodash'");
}

#[test]
fn test_external_dependency_different_targets() {
    // Test node target with aws-sdk as external
    let target_info = TargetInfo {
        platform: TargetPlatform::NodeJs,
        module_format: ModuleFormat::ESModule,
        external_deps: vec!["aws-sdk".to_string()],
        import_map: HashMap::new(),
        tree_shaking: false,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    let input = "use aws_sdk;";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();
    // External packages are imported directly without resolution
    // Note: aws_sdk stays as is (Husk naming convention) - not converted to aws-sdk
    assert_eq!(result, "import 'aws_sdk'");
}

#[test]
fn test_no_external_dependencies() {
    // Test with no external dependencies configured
    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map: HashMap::new(),
        tree_shaking: false,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    let input = "use axios;";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    // Without external deps, axios should still generate basic import
    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();
    assert_eq!(result, "import 'axios'");
}

#[test]
fn test_external_dependency_subpath() {
    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec!["react".to_string()],
        import_map: HashMap::new(),
        tree_shaking: false,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: Named imports from external package
    let input = "use react::{Component, useState};";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();
    // External packages with named imports work correctly
    assert_eq!(result, "import { Component, useState } from 'react'");
}
