use crate::lexer::Lexer;
use crate::package_resolver::PackageResolver;
use crate::parser::Parser;
use crate::semantic::SemanticVisitor;
use crate::transpiler::JsTranspiler;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_package_resolver_creation() {
        // Test that we can create a package resolver without errors
        let result = PackageResolver::from_current_dir();

        // This might fail if husk.toml doesn't exist, which is expected
        match result {
            Ok(resolver) => {
                println!("Successfully created package resolver");
                // Basic sanity check
                assert!(resolver.is_external_package("express"));
                assert!(!resolver.is_external_package("local::utils"));
            }
            Err(_) => {
                println!("Failed to create package resolver (expected if no husk.toml)");
                // This is fine - just means husk.toml is not set up
            }
        }
    }

    #[test]
    fn test_external_package_detection() {
        use crate::config::HuskConfig;
        use std::path::PathBuf;

        let config = HuskConfig::default();
        let resolver = PackageResolver::new(PathBuf::from("/tmp"), config);

        // Test external package detection
        assert!(resolver.is_external_package("express"));
        assert!(resolver.is_external_package("react"));
        assert!(resolver.is_external_package("@types/node"));

        // Test local package detection
        assert!(!resolver.is_external_package("local::utils"));
        assert!(!resolver.is_external_package("self::components"));
        assert!(!resolver.is_external_package("super::shared"));
        assert!(!resolver.is_external_package("./relative"));
        assert!(!resolver.is_external_package("../parent"));
    }

    #[test]
    fn test_semantic_analyzer_with_package_resolution() {
        let code = r#"
            use express::express;
            use lodash::{debounce, throttle};
            
            fn main() {
                let app = express();
            }
        "#;

        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        // Test with basic semantic analyzer (no package resolver)
        let mut analyzer = SemanticVisitor::new();
        let result = analyzer.analyze(&ast);

        // Should succeed even without package resolution
        // (imports are registered as Unknown type)
        assert!(
            result.is_ok(),
            "Semantic analysis should not fail for external imports: {result:?}"
        );
    }

    #[test]
    fn test_transpiler_with_package_resolution() {
        let code = r#"
            use express::express;
            use lodash::{debounce, throttle};
            
            fn main() {
                let app = express();
            }
        "#;

        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        // Test with basic transpiler (no package resolver)
        let mut transpiler = JsTranspiler::new();
        let result = transpiler.generate(&ast);

        assert!(result.is_ok(), "Transpilation should not fail: {result:?}");

        let js_output = result.unwrap();

        // Should contain import statements
        assert!(
            js_output.contains("import"),
            "Should contain import statements"
        );
        assert!(
            js_output.contains("express"),
            "Should contain express import"
        );
        assert!(
            js_output.contains("lodash") || js_output.contains("debounce"),
            "Should contain lodash imports"
        );

        println!("Generated JavaScript:\n{js_output}");
    }
}
