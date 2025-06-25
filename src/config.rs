use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::error::{Error, Result};

/// Husk project configuration parsed from husk.toml
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HuskConfig {
    pub package: PackageConfig,
    #[serde(default)]
    pub dependencies: HashMap<String, DependencySpec>,
    #[serde(default, rename = "dev-dependencies")]
    pub dev_dependencies: HashMap<String, DependencySpec>,
    #[serde(default)]
    pub build: BuildConfig,
    #[serde(default)]
    pub targets: HashMap<String, TargetConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageConfig {
    pub name: String,
    pub version: String,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub author: Option<String>,
    #[serde(default)]
    pub license: Option<String>,
    #[serde(default)]
    pub repository: Option<String>,
    #[serde(default)]
    pub homepage: Option<String>,
    #[serde(default)]
    pub keywords: Vec<String>,
    #[serde(default)]
    pub main: Option<String>,
    #[serde(default)]
    pub bin: Vec<BinaryConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BinaryConfig {
    pub name: String,
    pub path: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DependencySpec {
    Simple(String), // "^1.0.0"
    Detailed {
        version: Option<String>,
        path: Option<String>,
        git: Option<String>,
        branch: Option<String>,
        tag: Option<String>,
        #[serde(default)]
        optional: bool,
        #[serde(default)]
        features: Vec<String>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildConfig {
    #[serde(default = "default_src_dir")]
    pub src: String,
    #[serde(default = "default_out_dir")]
    pub out: String,
    #[serde(default)]
    pub target: String, // "es2020", "node", "browser"
    #[serde(default)]
    pub module: String, // "esm", "cjs", "umd"
    #[serde(default)]
    pub minify: bool,
    #[serde(default)]
    pub source_map: bool,
    #[serde(default)]
    pub watch: bool,
}

impl Default for BuildConfig {
    fn default() -> Self {
        Self {
            src: default_src_dir(),
            out: default_out_dir(),
            target: String::new(),
            module: String::new(),
            minify: false,
            source_map: false,
            watch: false,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TargetConfig {
    #[serde(default)]
    pub platform: Option<String>, // "node", "browser", "deno", "bun"
    #[serde(default)]
    pub format: Option<String>, // "esm", "cjs", "iife", "umd"
    #[serde(default)]
    pub entry: Option<String>,
    #[serde(default)]
    pub output: Option<String>,
    #[serde(default)]
    pub external: Vec<String>,
    #[serde(default)]
    pub globals: HashMap<String, String>,
}

fn default_src_dir() -> String {
    "src".to_string()
}

fn default_out_dir() -> String {
    "dist".to_string()
}

impl HuskConfig {
    /// Load configuration from husk.toml file
    pub fn load<P: AsRef<Path>>(path: P) -> Result<Self> {
        let content = fs::read_to_string(path)
            .map_err(|e| Error::new_config(format!("Failed to read husk.toml: {}", e)))?;

        Self::parse_from_str(&content)
    }

    /// Parse configuration from TOML string
    pub fn parse_from_str(content: &str) -> Result<Self> {
        toml::from_str(content)
            .map_err(|e| Error::new_config(format!("Invalid husk.toml format: {}", e)))
    }

    /// Find and load husk.toml from current directory or parent directories
    pub fn find_and_load() -> Result<(Self, PathBuf)> {
        let mut current_dir = std::env::current_dir()
            .map_err(|e| Error::new_config(format!("Failed to get current directory: {}", e)))?;

        loop {
            let config_path = current_dir.join("husk.toml");
            if config_path.exists() {
                let config = Self::load(&config_path)?;
                return Ok((config, current_dir));
            }

            if let Some(parent) = current_dir.parent() {
                current_dir = parent.to_path_buf();
            } else {
                return Err(Error::new_config(
                    "No husk.toml found in current directory or parent directories".to_string(),
                ));
            }
        }
    }

    /// Get the main entry point file path
    pub fn get_main_entry(&self) -> String {
        self.package
            .main
            .clone()
            .unwrap_or_else(|| format!("{}/main.husk", self.build.src))
    }

    /// Get all dependencies including dev dependencies (for development builds)
    pub fn get_all_dependencies(&self, include_dev: bool) -> HashMap<String, &DependencySpec> {
        let mut deps = HashMap::new();

        for (name, spec) in &self.dependencies {
            deps.insert(name.clone(), spec);
        }

        if include_dev {
            for (name, spec) in &self.dev_dependencies {
                deps.insert(name.clone(), spec);
            }
        }

        deps
    }

    /// Generate package.json content from husk.toml
    pub fn generate_package_json(&self) -> Result<String> {
        let mut pkg = serde_json::Map::new();

        // Basic package info
        pkg.insert(
            "name".to_string(),
            serde_json::Value::String(self.package.name.clone()),
        );
        pkg.insert(
            "version".to_string(),
            serde_json::Value::String(self.package.version.clone()),
        );

        if let Some(desc) = &self.package.description {
            pkg.insert(
                "description".to_string(),
                serde_json::Value::String(desc.clone()),
            );
        }

        if let Some(author) = &self.package.author {
            pkg.insert(
                "author".to_string(),
                serde_json::Value::String(author.clone()),
            );
        }

        if let Some(license) = &self.package.license {
            pkg.insert(
                "license".to_string(),
                serde_json::Value::String(license.clone()),
            );
        }

        if let Some(repo) = &self.package.repository {
            let mut repository = serde_json::Map::new();
            repository.insert(
                "type".to_string(),
                serde_json::Value::String("git".to_string()),
            );
            repository.insert("url".to_string(), serde_json::Value::String(repo.clone()));
            pkg.insert(
                "repository".to_string(),
                serde_json::Value::Object(repository),
            );
        }

        if let Some(homepage) = &self.package.homepage {
            pkg.insert(
                "homepage".to_string(),
                serde_json::Value::String(homepage.clone()),
            );
        }

        if !self.package.keywords.is_empty() {
            let keywords: Vec<serde_json::Value> = self
                .package
                .keywords
                .iter()
                .map(|k| serde_json::Value::String(k.clone()))
                .collect();
            pkg.insert("keywords".to_string(), serde_json::Value::Array(keywords));
        }

        // Entry point
        let main_file = format!("{}/index.js", self.build.out);
        pkg.insert("main".to_string(), serde_json::Value::String(main_file));

        // Module type
        if self.build.module == "esm" {
            pkg.insert(
                "type".to_string(),
                serde_json::Value::String("module".to_string()),
            );
        }

        // Dependencies
        if !self.dependencies.is_empty() {
            let mut deps = serde_json::Map::new();
            for (name, spec) in &self.dependencies {
                let version = match spec {
                    DependencySpec::Simple(v) => v.clone(),
                    DependencySpec::Detailed {
                        version: Some(v), ..
                    } => v.clone(),
                    DependencySpec::Detailed { git: Some(git), .. } => git.clone(),
                    DependencySpec::Detailed {
                        path: Some(path), ..
                    } => format!("file:{}", path),
                    _ => "latest".to_string(),
                };
                deps.insert(name.clone(), serde_json::Value::String(version));
            }
            pkg.insert("dependencies".to_string(), serde_json::Value::Object(deps));
        }

        // Dev dependencies
        if !self.dev_dependencies.is_empty() {
            let mut deps = serde_json::Map::new();
            for (name, spec) in &self.dev_dependencies {
                let version = match spec {
                    DependencySpec::Simple(v) => v.clone(),
                    DependencySpec::Detailed {
                        version: Some(v), ..
                    } => v.clone(),
                    DependencySpec::Detailed { git: Some(git), .. } => git.clone(),
                    DependencySpec::Detailed {
                        path: Some(path), ..
                    } => format!("file:{}", path),
                    _ => "latest".to_string(),
                };
                deps.insert(name.clone(), serde_json::Value::String(version));
            }
            pkg.insert(
                "devDependencies".to_string(),
                serde_json::Value::Object(deps),
            );
        }

        // Scripts
        let mut scripts = serde_json::Map::new();
        scripts.insert(
            "build".to_string(),
            serde_json::Value::String("husk build".to_string()),
        );
        scripts.insert(
            "dev".to_string(),
            serde_json::Value::String("husk dev".to_string()),
        );
        scripts.insert(
            "start".to_string(),
            serde_json::Value::String("husk run".to_string()),
        );
        pkg.insert("scripts".to_string(), serde_json::Value::Object(scripts));

        serde_json::to_string_pretty(&serde_json::Value::Object(pkg))
            .map_err(|e| Error::new_config(format!("Failed to generate package.json: {}", e)))
    }

    /// Validate the configuration
    pub fn validate(&self) -> Result<()> {
        // Validate package name
        if self.package.name.is_empty() {
            return Err(Error::new_config(
                "Package name cannot be empty".to_string(),
            ));
        }

        // Validate version format
        if self.package.version.is_empty() {
            return Err(Error::new_config(
                "Package version cannot be empty".to_string(),
            ));
        }

        // Validate build target
        if !self.build.target.is_empty() {
            let valid_targets = [
                "es2015", "es2016", "es2017", "es2018", "es2019", "es2020", "es2021", "es2022",
                "esnext", "node",
            ];
            if !valid_targets.contains(&self.build.target.as_str()) {
                return Err(Error::new_config(format!(
                    "Invalid build target '{}'. Valid targets: {}",
                    self.build.target,
                    valid_targets.join(", ")
                )));
            }
        }

        // Validate module format
        if !self.build.module.is_empty() {
            let valid_modules = ["esm", "cjs", "umd", "iife"];
            if !valid_modules.contains(&self.build.module.as_str()) {
                return Err(Error::new_config(format!(
                    "Invalid module format '{}'. Valid formats: {}",
                    self.build.module,
                    valid_modules.join(", ")
                )));
            }
        }

        Ok(())
    }
}

impl Default for HuskConfig {
    fn default() -> Self {
        Self {
            package: PackageConfig {
                name: "husk-project".to_string(),
                version: "0.1.0".to_string(),
                description: None,
                author: None,
                license: None,
                repository: None,
                homepage: None,
                keywords: Vec::new(),
                main: None,
                bin: Vec::new(),
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            build: BuildConfig::default(),
            targets: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_minimal_config() {
        let toml = r#"
[package]
name = "my-app"
version = "1.0.0"
"#;

        let config = HuskConfig::parse_from_str(toml).unwrap();
        assert_eq!(config.package.name, "my-app");
        assert_eq!(config.package.version, "1.0.0");
        assert!(config.dependencies.is_empty());
    }

    #[test]
    fn test_parse_full_config() {
        let toml = r#"
[package]
name = "my-app"
version = "1.0.0"
description = "A sample Husk application"
author = "Developer <dev@example.com>"
license = "MIT"
repository = "https://github.com/user/my-app"
keywords = ["husk", "javascript", "transpiler"]

[dependencies]
lodash = "^4.17.21"
express = { version = "^4.18.0", optional = false }

[dev-dependencies]
jest = "^29.0.0"

[build]
src = "src"
out = "dist"
target = "es2020"
module = "esm"
minify = true
source_map = true

[targets.node]
platform = "node"
format = "cjs"
entry = "src/server.husk"
output = "dist/server.js"

[targets.browser]
platform = "browser"
format = "esm"
entry = "src/client.husk"
output = "dist/bundle.js"
"#;

        let config = HuskConfig::parse_from_str(toml).unwrap();

        assert_eq!(config.package.name, "my-app");
        assert_eq!(
            config.package.description,
            Some("A sample Husk application".to_string())
        );
        assert_eq!(config.dependencies.len(), 2);
        assert_eq!(config.dev_dependencies.len(), 1);
        assert_eq!(config.build.target, "es2020");
        assert_eq!(config.build.module, "esm");
        assert!(config.build.minify);
        assert_eq!(config.targets.len(), 2);
    }

    #[test]
    fn test_generate_package_json() {
        let config = HuskConfig {
            package: PackageConfig {
                name: "test-app".to_string(),
                version: "1.2.3".to_string(),
                description: Some("Test description".to_string()),
                author: Some("Test Author".to_string()),
                license: Some("MIT".to_string()),
                repository: Some("https://github.com/test/app".to_string()),
                homepage: None,
                keywords: vec!["test".to_string(), "husk".to_string()],
                main: None,
                bin: Vec::new(),
            },
            dependencies: {
                let mut deps = HashMap::new();
                deps.insert(
                    "express".to_string(),
                    DependencySpec::Simple("^4.18.0".to_string()),
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
                minify: false,
                source_map: true,
                watch: false,
            },
            targets: HashMap::new(),
        };

        let package_json = config.generate_package_json().unwrap();
        assert!(package_json.contains("\"name\": \"test-app\""));
        assert!(package_json.contains("\"version\": \"1.2.3\""));
        assert!(package_json.contains("\"type\": \"module\""));
        assert!(package_json.contains("\"express\": \"^4.18.0\""));
        assert!(package_json.contains("\"jest\": \"^29.0.0\""));
    }

    #[test]
    fn test_validation() {
        let mut config = HuskConfig::default();
        config.package.name = "".to_string();

        assert!(config.validate().is_err());

        config.package.name = "valid-name".to_string();
        config.build.target = "invalid-target".to_string();

        assert!(config.validate().is_err());

        config.build.target = "es2020".to_string();
        assert!(config.validate().is_ok());
    }
}
