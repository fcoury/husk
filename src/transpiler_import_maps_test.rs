use crate::ast::visitor::AstVisitor;
use crate::config::{HuskConfig, TargetConfig};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::transpiler::{JsTranspiler, ModuleFormat, TargetInfo, TargetPlatform};
use std::collections::HashMap;

fn create_test_config_with_import_map() -> HuskConfig {
    let mut config = HuskConfig::default();

    // Create import map for browser target
    let mut import_map = HashMap::new();
    import_map.insert("react".to_string(), "https://esm.sh/react@18".to_string());
    import_map.insert("lodash".to_string(), "/vendor/lodash.js".to_string());
    import_map.insert(
        "axios".to_string(),
        "https://cdn.skypack.dev/axios".to_string(),
    );

    let browser_target = TargetConfig {
        platform: Some("browser".to_string()),
        format: Some("esm".to_string()),
        external: vec![],
        entry: None,
        globals: HashMap::new(),
        output: None,
        import_map,
        tree_shaking: false,
        dev: false,
    };
    config.targets.insert("browser".to_string(), browser_target);

    // Create different import map for deno target
    let mut deno_import_map = HashMap::new();
    deno_import_map.insert(
        "express".to_string(),
        "https://deno.land/x/express@v1.0.0/mod.ts".to_string(),
    );
    deno_import_map.insert(
        "oak".to_string(),
        "https://deno.land/x/oak/mod.ts".to_string(),
    );

    let deno_target = TargetConfig {
        platform: Some("deno".to_string()),
        format: Some("esm".to_string()),
        external: vec![],
        entry: None,
        globals: HashMap::new(),
        output: None,
        import_map: deno_import_map,
        tree_shaking: false,
        dev: false,
    };
    config.targets.insert("deno".to_string(), deno_target);

    config
}

#[test]
fn test_import_map_basic_import() {
    let _config = create_test_config_with_import_map();

    // Create import map for testing
    let mut import_map = HashMap::new();
    import_map.insert("react".to_string(), "https://esm.sh/react@18".to_string());

    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map,
        tree_shaking: false,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: react should be imported from mapped URL
    let input = "use react;";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    println!("DEBUG: Testing import map with input: {}", input);
    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();
    println!("DEBUG: Result: {}", result);
    assert_eq!(result, "import 'https://esm.sh/react@18'");
}

#[test]
fn test_import_map_named_imports() {
    let _config = create_test_config_with_import_map();

    // Create import map for testing
    let mut import_map = HashMap::new();
    import_map.insert("react".to_string(), "https://esm.sh/react@18".to_string());

    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map,
        tree_shaking: false,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: Named imports from mapped package
    let input = "use react::{useState, useEffect};";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();
    assert_eq!(
        result,
        "import { useState, useEffect } from 'https://esm.sh/react@18'"
    );
}

#[test]
fn test_import_map_with_alias() {
    let _config = create_test_config_with_import_map();

    // Create import map for testing
    let mut import_map = HashMap::new();
    import_map.insert("lodash".to_string(), "/vendor/lodash.js".to_string());

    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map,
        tree_shaking: false,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: Named import with alias
    let input = "use lodash::{debounce as delay};";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();
    assert_eq!(
        result,
        "import { debounce as delay } from '/vendor/lodash.js'"
    );
}

#[test]
fn test_import_map_wildcard_import() {
    let _config = create_test_config_with_import_map();

    // Create import map for testing
    let mut import_map = HashMap::new();
    import_map.insert(
        "axios".to_string(),
        "https://cdn.skypack.dev/axios".to_string(),
    );

    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map,
        tree_shaking: false,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: Wildcard import from mapped package
    let input = "use axios::*;";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();
    assert_eq!(result, "import * from 'https://cdn.skypack.dev/axios'");
}

#[test]
fn test_import_map_unmapped_package_fallback() {
    let _config = create_test_config_with_import_map();

    // Create import map with only one mapping
    let mut import_map = HashMap::new();
    import_map.insert("react".to_string(), "https://esm.sh/react@18".to_string());

    let target_info = TargetInfo {
        platform: TargetPlatform::Browser,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map,
        tree_shaking: false,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: Package not in import map should fall back to normal resolution
    let input = "use express;";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();
    // Should fall back to basic import since no package resolver is set
    assert_eq!(result, "import 'express'");
}

#[test]
fn test_import_map_different_platforms() {
    let _config = create_test_config_with_import_map();

    // Test Deno platform with different import map
    let mut deno_import_map = HashMap::new();
    deno_import_map.insert(
        "express".to_string(),
        "https://deno.land/x/express@v1.0.0/mod.ts".to_string(),
    );

    let target_info = TargetInfo {
        platform: TargetPlatform::Deno,
        module_format: ModuleFormat::ESModule,
        external_deps: vec![],
        import_map: deno_import_map,
        tree_shaking: false,
        dev: false,
    };

    let mut transpiler = JsTranspiler::new();
    transpiler.target_info = Some(target_info);

    // Test: express should be imported from Deno-specific URL
    let input = "use express;";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();
    assert_eq!(result, "import 'https://deno.land/x/express@v1.0.0/mod.ts'");
}

#[test]
fn test_import_map_empty() {
    // Test with empty import map
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

    // Test: Should fall back to normal resolution with empty import map
    let input = "use react;";
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    let result =
        <JsTranspiler as AstVisitor<String>>::visit_stmt(&mut transpiler, &program[0]).unwrap();
    // Should fall back to basic import
    assert_eq!(result, "import 'react'");
}
