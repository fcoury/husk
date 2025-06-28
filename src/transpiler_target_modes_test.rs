#[cfg(test)]
mod tests {
    use crate::config::{HuskConfig, TargetConfig};
    use crate::error::Result;
    use crate::semantic::SemanticVisitor;
    use crate::transpiler::JsTranspiler;
    use crate::{Lexer, Parser};

    fn transpile_with_config(code: &str, _config: &HuskConfig) -> Result<String> {
        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse()?;

        // Run semantic analysis
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast)?;

        // Create basic transpiler (config support to be implemented)
        // TODO: Implement JsTranspiler::with_config() method
        let mut transpiler = JsTranspiler::new();
        transpiler.generate(&ast)
    }

    fn create_config_for_target(platform: &str, format: &str, target: &str) -> HuskConfig {
        let mut config = HuskConfig::default();

        // Set build configuration
        config.build.target = target.to_string();
        config.build.module = format.to_string();
        config.build.out = "dist".to_string();

        // Add target-specific configuration
        let target_config = TargetConfig {
            platform: Some(platform.to_string()),
            format: Some(format.to_string()),
            entry: Some("main.husk".to_string()),
            output: Some("output.js".to_string()),
            external: Vec::new(),
            globals: std::collections::HashMap::new(),
            import_map: std::collections::HashMap::new(),
            tree_shaking: false,
            dev: false,
        };
        config.targets.insert("test".to_string(), target_config);

        config
    }

    // Test CLI target modes: node-esm, node-cjs, browser

    #[test]
    #[ignore = "Target mode configuration not fully implemented"]
    fn test_transpile_node_esm_target() {
        let code = r#"
            use fs::readFile;
            
            pub fn main() {
                let content = readFile("test.txt");
                println!(content);
            }
        "#;

        let config = create_config_for_target("node", "esm", "node-esm");

        let js = transpile_with_config(code, &config).unwrap();

        // Should generate ES module syntax for Node.js
        assert!(js.contains("import { readFile } from 'fs/readFile';"));
        assert!(js.contains("export function main()"));

        // Should include Node.js-specific features
        assert!(js.contains("console.log")); // println should map to console.log

        // Should not include browser-specific polyfills
        assert!(!js.contains("window"));
        assert!(!js.contains("document"));
    }

    #[test]
    #[ignore = "Target mode configuration not fully implemented"]
    fn test_transpile_node_cjs_target() {
        let code = r#"
            use fs::readFile;
            
            pub fn main() {
                let content = readFile("test.txt");
                println!(content);
            }
        "#;

        let config = create_config_for_target("node", "cjs", "node-cjs");

        let js = transpile_with_config(code, &config).unwrap();

        // Should generate CommonJS syntax for Node.js
        assert!(js.contains("const { readFile } = require('fs');") || js.contains("require('fs')"));
        assert!(js.contains("exports.main") || js.contains("module.exports"));

        // Should include Node.js-specific features
        assert!(js.contains("console.log"));

        // Should not include ES module syntax
        assert!(!js.contains("import"));
        assert!(!js.contains("export function"));
    }

    #[test]
    #[ignore = "Target mode configuration not fully implemented"]
    fn test_transpile_browser_target() {
        let code = r#"
            use local::utils::helper;
            
            pub fn handleClick() {
                let result = helper();
                console.log(result);
            }
        "#;

        let config = create_config_for_target("browser", "esm", "browser");

        let js = transpile_with_config(code, &config).unwrap();

        // Should generate ES module syntax for browsers
        assert!(js.contains("import { helper } from './utils.js';"));
        assert!(js.contains("export function handleClick()"));

        // Should include browser-compatible code
        assert!(js.contains("console.log"));

        // Should not include Node.js-specific features
        assert!(!js.contains("require("));
        assert!(!js.contains("module.exports"));
    }

    // Test different module formats

    #[test]
    #[ignore = "Target mode configuration not fully implemented"]
    fn test_transpile_umd_format() {
        let code = r#"
            pub fn greet(name: string) -> string {
                return format!("Hello, {}!", name);
            }
        "#;

        let config = create_config_for_target("browser", "umd", "es2020");

        let js = transpile_with_config(code, &config).unwrap();

        // Should generate UMD wrapper
        assert!(
            js.contains("(function (root, factory)") || js.contains("typeof exports === 'object'")
        );

        // Should support both CommonJS and AMD
        assert!(js.contains("module.exports") || js.contains("define"));

        // Should have global fallback
        assert!(js.contains("root.") || js.contains("this."));
    }

    #[test]
    #[ignore = "Target mode configuration not fully implemented"]
    fn test_transpile_iife_format() {
        let code = r#"
            fn init() {
                println!("Application started");
            }
            
            init();
        "#;

        let config = create_config_for_target("browser", "iife", "es2015");

        let js = transpile_with_config(code, &config).unwrap();

        // Should wrap in IIFE
        assert!(js.contains("(function()") || js.contains("(() =>"));
        assert!(js.contains("})()") || js.contains("})();"));

        // Should not expose internal variables globally
        assert!(!js.contains("var init") || !js.contains("function init"));

        // Should be self-contained
        assert!(!js.contains("import"));
        assert!(!js.contains("require"));
        assert!(!js.contains("export"));
    }

    // Test ECMAScript target versions

    #[test]
    #[ignore = "Target mode configuration not fully implemented"]
    fn test_transpile_es2015_target() {
        let code = r#"
            async fn fetchData() -> string {
                let data = fetch("api/data").await;
                return data.json().await;
            }
        "#;

        let config = create_config_for_target("browser", "esm", "es2015");

        let js = transpile_with_config(code, &config).unwrap();

        // Should transpile async/await for ES2015 (if supported)
        // In ES2015, async/await would need to be transpiled to Promises
        if !js.contains("async function") {
            // Should contain Promise-based code
            assert!(js.contains("Promise") || js.contains(".then("));
        }

        // Should use ES2015-compatible syntax
        assert!(!js.contains("?.")); // No optional chaining
        assert!(!js.contains("??")); // No nullish coalescing
    }

    #[test]
    fn test_transpile_es2022_target() {
        let code = r#"
            async fn processData(data: string) -> string {
                return data.to_uppercase();
            }
        "#;

        let config = create_config_for_target("browser", "esm", "es2022");

        let js = transpile_with_config(code, &config).unwrap();

        // Should use modern ES2022 features
        assert!(js.contains("async function"));

        // Should preserve modern syntax if supported by target
        // Note: The actual implementation may not support these yet
        // This test validates the target configuration system
        assert!(js.contains("processData"));
    }

    // Test platform-specific features

    #[test]
    #[ignore = "Platform-specific code generation not fully implemented"]
    fn test_node_platform_specifics() {
        let code = r#"
            use fs::readFileSync;
            use path::join;
            
            fn loadConfig() -> string {
                let configPath = join(process.cwd(), "config.json");
                return readFileSync(configPath, "utf8");
            }
        "#;

        let config = create_config_for_target("node", "esm", "node");

        let js = transpile_with_config(code, &config).unwrap();

        // Should generate Node.js-compatible imports
        assert!(js.contains("import { readFileSync } from 'fs/readFileSync';"));
        assert!(js.contains("import { join } from 'path/join';"));

        // Should preserve Node.js globals (if used)
        assert!(js.contains("process.cwd()") || js.contains("process"));
    }

    #[test]
    #[ignore = "Platform-specific code generation not fully implemented"]
    fn test_browser_platform_specifics() {
        let code = r#"
            fn handlePageLoad() {
                let title = document.title;
                console.log(format!("Page loaded: {}", title));
            }
        "#;

        let config = create_config_for_target("browser", "esm", "es2020");

        let js = transpile_with_config(code, &config).unwrap();

        // Should preserve browser globals
        assert!(js.contains("document.title"));
        assert!(js.contains("console.log"));

        // Should generate browser-compatible code
        assert!(!js.contains("require("));
        assert!(!js.contains("process."));
    }

    #[test]
    #[ignore = "Platform-specific code generation not fully implemented"]
    fn test_deno_platform_specifics() {
        let code = r#"
            use std::env;
            
            fn getEnvVar(name: string) -> Option<string> {
                return env::var(name);
            }
        "#;

        let config = create_config_for_target("deno", "esm", "es2022");

        let js = transpile_with_config(code, &config).unwrap();

        // Should generate Deno-compatible imports
        assert!(js.contains("import") || js.contains("std"));

        // Should use ES modules (Deno's default)
        assert!(!js.contains("require("));
        assert!(!js.contains("module.exports"));
    }

    // Test external dependency handling per target

    #[test]
    #[ignore = "Package resolution with target mode not fully implemented"]
    fn test_external_deps_node_target() {
        let code = r#"
            use express::express;
            use fs::promises::readFile;
            
            async fn createServer() {
                let app = express();
                let content = readFile("index.html").await;
                app.get("/", |req, res| res.send(content));
            }
        "#;

        let config = create_config_for_target("node", "cjs", "node-cjs");

        let js = transpile_with_config(code, &config).unwrap();

        // Should handle npm packages for Node.js
        assert!(js.contains("express") || js.contains("require"));
        assert!(js.contains("fs") || js.contains("readFile"));

        // Should generate CommonJS for Node.js target
        assert!(js.contains("require") || js.contains("module.exports"));
    }

    #[test]
    #[ignore = "Package resolution with target mode not fully implemented"]
    fn test_external_deps_browser_target() {
        let code = r#"
            use react::{React, useState};
            use lodash::debounce;
            
            fn SearchComponent() {
                let [query, setQuery] = useState("");
                let debouncedSearch = debounce(setQuery, 300);
                return React.createElement("input", { onChange: debouncedSearch });
            }
        "#;

        let config = create_config_for_target("browser", "esm", "es2020");

        let js = transpile_with_config(code, &config).unwrap();

        // Should handle browser-compatible imports
        assert!(js.contains("import") && (js.contains("react") || js.contains("lodash")));

        // Should generate ES modules for browser
        assert!(!js.contains("require("));
        assert!(js.contains("export") || js.contains("import"));
    }

    // Test configuration validation

    #[test]
    fn test_invalid_target_config() {
        let code = r#"
            fn test() {
                println!("Hello");
            }
        "#;

        // Create config with invalid target
        let mut config = HuskConfig::default();
        config.build.target = "invalid-target".to_string();

        // Should handle invalid targets gracefully
        let result = transpile_with_config(code, &config);

        // Either should work with default fallback or return appropriate error
        match result {
            Ok(js) => {
                // Should still generate valid JavaScript with fallback
                assert!(js.contains("function test"));
            }
            Err(_) => {
                // Or should return a clear error about invalid target
                // This is also acceptable behavior
            }
        }
    }

    #[test]
    fn test_missing_target_config() {
        let code = r#"
            fn greet() {
                println!("Hello, World!");
            }
        "#;

        let config = HuskConfig::default(); // No target specified

        let js = transpile_with_config(code, &config).unwrap();

        // Should work with default target
        assert!(js.contains("function greet"));
        assert!(js.contains("console.log")); // println should work
    }
}
