#[cfg(test)]
mod tests {
    use crate::config::*;
    use std::collections::HashMap;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_parse_minimal_husk_toml() {
        let toml_content = r#"
[package]
name = "minimal-app"
version = "0.1.0"
"#;

        let config = HuskConfig::from_str(toml_content).unwrap();

        assert_eq!(config.package.name, "minimal-app");
        assert_eq!(config.package.version, "0.1.0");
        assert!(config.package.description.is_none());
        assert!(config.dependencies.is_empty());
        assert_eq!(config.build.src, "src");
        assert_eq!(config.build.out, "dist");
    }

    #[test]
    fn test_parse_comprehensive_husk_toml() {
        let toml_content = r#"
[package]
name = "comprehensive-app"
version = "2.1.0"
description = "A comprehensive Husk application"
author = "Jane Developer <jane@example.com>"
license = "Apache-2.0"
repository = "https://github.com/jane/comprehensive-app"
homepage = "https://comprehensive-app.dev"
keywords = ["husk", "web", "api"]
main = "src/main.husk"

[[package.bin]]
name = "server"
path = "src/bin/server.husk"

[[package.bin]]
name = "worker"
path = "src/bin/worker.husk"

[dependencies]
express = "^4.18.2"
lodash = { version = "^4.17.21", optional = false }
utils = { path = "../shared-utils" }
experimental = { git = "https://github.com/user/experimental", tag = "v1.0.0" }

[dev-dependencies]
jest = "^29.5.0"
nodemon = "^2.0.22"

[build]
src = "source"
out = "build"
target = "es2022"
module = "cjs"
minify = false
source_map = true
watch = true

[targets.production]
platform = "node"
format = "cjs"
entry = "source/prod.husk"
output = "build/prod.js"
external = ["fs", "path"]

[targets.client]
platform = "browser"
format = "esm"
entry = "source/client.husk"
output = "build/client.js"
globals = { "lodash" = "_" }
"#;

        let config = HuskConfig::from_str(toml_content).unwrap();

        // Test package info
        assert_eq!(config.package.name, "comprehensive-app");
        assert_eq!(config.package.version, "2.1.0");
        assert_eq!(
            config.package.description,
            Some("A comprehensive Husk application".to_string())
        );
        assert_eq!(
            config.package.author,
            Some("Jane Developer <jane@example.com>".to_string())
        );
        assert_eq!(config.package.license, Some("Apache-2.0".to_string()));
        assert_eq!(
            config.package.repository,
            Some("https://github.com/jane/comprehensive-app".to_string())
        );
        assert_eq!(
            config.package.homepage,
            Some("https://comprehensive-app.dev".to_string())
        );
        assert_eq!(config.package.keywords, vec!["husk", "web", "api"]);
        assert_eq!(config.package.main, Some("src/main.husk".to_string()));
        assert_eq!(config.package.bin.len(), 2);
        assert_eq!(config.package.bin[0].name, "server");
        assert_eq!(config.package.bin[0].path, "src/bin/server.husk");

        // Test dependencies
        assert_eq!(config.dependencies.len(), 4);

        match config.dependencies.get("express").unwrap() {
            DependencySpec::Simple(version) => assert_eq!(version, "^4.18.2"),
            _ => panic!("Expected simple dependency"),
        }

        match config.dependencies.get("lodash").unwrap() {
            DependencySpec::Detailed {
                version: Some(v),
                optional,
                ..
            } => {
                assert_eq!(v, "^4.17.21");
                assert_eq!(*optional, false);
            }
            _ => panic!("Expected detailed dependency"),
        }

        match config.dependencies.get("utils").unwrap() {
            DependencySpec::Detailed { path: Some(p), .. } => {
                assert_eq!(p, "../shared-utils");
            }
            _ => panic!("Expected path dependency"),
        }

        match config.dependencies.get("experimental").unwrap() {
            DependencySpec::Detailed {
                git: Some(g),
                tag: Some(t),
                ..
            } => {
                assert_eq!(g, "https://github.com/user/experimental");
                assert_eq!(t, "v1.0.0");
            }
            _ => panic!("Expected git dependency"),
        }

        // Test dev dependencies
        assert_eq!(config.dev_dependencies.len(), 2);

        // Test build config
        assert_eq!(config.build.src, "source");
        assert_eq!(config.build.out, "build");
        assert_eq!(config.build.target, "es2022");
        assert_eq!(config.build.module, "cjs");
        assert_eq!(config.build.minify, false);
        assert_eq!(config.build.source_map, true);
        assert_eq!(config.build.watch, true);

        // Test targets
        assert_eq!(config.targets.len(), 2);

        let prod_target = config.targets.get("production").unwrap();
        assert_eq!(prod_target.platform, Some("node".to_string()));
        assert_eq!(prod_target.format, Some("cjs".to_string()));
        assert_eq!(prod_target.entry, Some("source/prod.husk".to_string()));
        assert_eq!(prod_target.output, Some("build/prod.js".to_string()));
        assert_eq!(prod_target.external, vec!["fs", "path"]);

        let client_target = config.targets.get("client").unwrap();
        assert_eq!(client_target.platform, Some("browser".to_string()));
        assert_eq!(client_target.format, Some("esm".to_string()));
        assert_eq!(client_target.globals.get("lodash"), Some(&"_".to_string()));
    }

    #[test]
    fn test_generate_package_json_from_config() {
        let config = HuskConfig {
            package: PackageConfig {
                name: "test-package".to_string(),
                version: "1.2.3".to_string(),
                description: Some("Test package description".to_string()),
                author: Some("Test Author <test@example.com>".to_string()),
                license: Some("MIT".to_string()),
                repository: Some("https://github.com/test/package".to_string()),
                homepage: Some("https://test-package.dev".to_string()),
                keywords: vec!["test".to_string(), "husk".to_string(), "config".to_string()],
                main: Some("src/index.husk".to_string()),
                bin: vec![],
            },
            dependencies: {
                let mut deps = HashMap::new();
                deps.insert(
                    "express".to_string(),
                    DependencySpec::Simple("^4.18.0".to_string()),
                );
                deps.insert(
                    "lodash".to_string(),
                    DependencySpec::Detailed {
                        version: Some("^4.17.21".to_string()),
                        path: None,
                        git: None,
                        branch: None,
                        tag: None,
                        optional: false,
                        features: vec![],
                    },
                );
                deps.insert(
                    "local-lib".to_string(),
                    DependencySpec::Detailed {
                        version: None,
                        path: Some("../local-lib".to_string()),
                        git: None,
                        branch: None,
                        tag: None,
                        optional: false,
                        features: vec![],
                    },
                );
                deps
            },
            dev_dependencies: {
                let mut deps = HashMap::new();
                deps.insert(
                    "jest".to_string(),
                    DependencySpec::Simple("^29.0.0".to_string()),
                );
                deps
            },
            build: BuildConfig {
                src: "src".to_string(),
                out: "build".to_string(),
                target: "es2020".to_string(),
                module: "esm".to_string(),
                minify: true,
                source_map: false,
                watch: false,
            },
            targets: HashMap::new(),
        };

        let package_json = config.generate_package_json().unwrap();

        // Parse the generated JSON to verify structure
        let parsed: serde_json::Value = serde_json::from_str(&package_json).unwrap();

        assert_eq!(parsed["name"], "test-package");
        assert_eq!(parsed["version"], "1.2.3");
        assert_eq!(parsed["description"], "Test package description");
        assert_eq!(parsed["author"], "Test Author <test@example.com>");
        assert_eq!(parsed["license"], "MIT");
        assert_eq!(parsed["type"], "module"); // ESM module
        assert_eq!(parsed["main"], "build/index.js");

        // Check repository object
        assert_eq!(parsed["repository"]["type"], "git");
        assert_eq!(
            parsed["repository"]["url"],
            "https://github.com/test/package"
        );

        // Check keywords array
        let keywords = parsed["keywords"].as_array().unwrap();
        assert_eq!(keywords.len(), 3);
        assert!(keywords.contains(&serde_json::Value::String("test".to_string())));
        assert!(keywords.contains(&serde_json::Value::String("husk".to_string())));

        // Check dependencies
        assert_eq!(parsed["dependencies"]["express"], "^4.18.0");
        assert_eq!(parsed["dependencies"]["lodash"], "^4.17.21");
        assert_eq!(parsed["dependencies"]["local-lib"], "file:../local-lib");

        // Check dev dependencies
        assert_eq!(parsed["devDependencies"]["jest"], "^29.0.0");

        // Check scripts
        assert_eq!(parsed["scripts"]["build"], "husk build");
        assert_eq!(parsed["scripts"]["dev"], "husk dev");
        assert_eq!(parsed["scripts"]["start"], "husk run");
    }

    #[test]
    fn test_config_validation() {
        // Test empty package name
        let mut config = HuskConfig::default();
        config.package.name = "".to_string();
        assert!(config.validate().is_err());

        // Test empty version
        config.package.name = "valid-name".to_string();
        config.package.version = "".to_string();
        assert!(config.validate().is_err());

        // Test invalid build target
        config.package.version = "1.0.0".to_string();
        config.build.target = "invalid-target".to_string();
        assert!(config.validate().is_err());

        // Test invalid module format
        config.build.target = "es2020".to_string();
        config.build.module = "invalid-module".to_string();
        assert!(config.validate().is_err());

        // Test valid config
        config.build.module = "esm".to_string();
        assert!(config.validate().is_ok());
    }

    #[test]
    fn test_find_config_file() {
        // Create a temporary directory structure
        let temp_dir = TempDir::new().unwrap();
        let project_dir = temp_dir.path().join("project");
        let src_dir = project_dir.join("src");
        fs::create_dir_all(&src_dir).unwrap();

        // Create husk.toml in project root
        let config_content = r#"
[package]
name = "find-test"
version = "1.0.0"
"#;
        let config_path = project_dir.join("husk.toml");
        fs::write(&config_path, config_content).unwrap();

        // Change to src directory and try to find config
        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(&src_dir).unwrap();

        // Should find config in parent directory
        let result = HuskConfig::find_and_load();

        // Restore original directory
        std::env::set_current_dir(original_dir).unwrap();

        assert!(result.is_ok());
        let (config, found_dir) = result.unwrap();
        assert_eq!(config.package.name, "find-test");
        // Use canonicalize to resolve symlinks for comparison
        assert_eq!(
            found_dir.canonicalize().unwrap(),
            project_dir.canonicalize().unwrap()
        );
    }

    #[test]
    fn test_get_all_dependencies() {
        let mut config = HuskConfig::default();

        config.dependencies.insert(
            "prod-dep".to_string(),
            DependencySpec::Simple("^1.0.0".to_string()),
        );
        config.dev_dependencies.insert(
            "dev-dep".to_string(),
            DependencySpec::Simple("^2.0.0".to_string()),
        );

        // Test production dependencies only
        let prod_deps = config.get_all_dependencies(false);
        assert_eq!(prod_deps.len(), 1);
        assert!(prod_deps.contains_key("prod-dep"));
        assert!(!prod_deps.contains_key("dev-dep"));

        // Test all dependencies
        let all_deps = config.get_all_dependencies(true);
        assert_eq!(all_deps.len(), 2);
        assert!(all_deps.contains_key("prod-dep"));
        assert!(all_deps.contains_key("dev-dep"));
    }

    #[test]
    fn test_get_main_entry() {
        let mut config = HuskConfig::default();

        // Test default main entry
        let main = config.get_main_entry();
        assert_eq!(main, "src/main.husk"); // Uses build.src not build.out

        // Test custom main entry
        config.package.main = Some("custom/entry.husk".to_string());
        let main = config.get_main_entry();
        assert_eq!(main, "custom/entry.husk");

        // Test with custom src directory
        config.package.main = None;
        config.build.src = "source".to_string();
        let main = config.get_main_entry();
        assert_eq!(main, "source/main.husk");
    }
}
