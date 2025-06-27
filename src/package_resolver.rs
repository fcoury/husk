use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::config::{DependencySpec, HuskConfig};
use crate::error::{Error, Result};

/// Handles npm package resolution and dependency management
pub struct PackageResolver {
    /// Project root directory
    project_root: PathBuf,
    /// Loaded husk.toml configuration
    config: HuskConfig,
    /// Cache of resolved package information
    package_cache: HashMap<String, ResolvedPackage>,
    /// Cache of package.json contents
    package_json_cache: HashMap<PathBuf, PackageJson>,
}

/// Information about a resolved npm package
#[derive(Debug, Clone)]
pub struct ResolvedPackage {
    /// Package name
    pub name: String,
    /// Resolved version
    pub version: String,
    /// Path to the package directory
    pub path: PathBuf,
    /// Main entry point file
    pub main: Option<String>,
    /// Module type (module, commonjs)
    pub module_type: ModuleType,
    /// Exported names from package.json
    pub exports: Vec<String>,
}

/// JavaScript module type
#[derive(Debug, Clone, PartialEq)]
pub enum ModuleType {
    CommonJS,
    ESModule,
    Umd,
    Unknown,
}

/// Simplified package.json structure
#[derive(Debug, Clone, serde::Deserialize)]
pub struct PackageJson {
    #[allow(dead_code)]
    pub name: Option<String>,
    pub version: Option<String>,
    pub main: Option<String>,
    pub module: Option<String>,
    #[serde(rename = "type")]
    pub module_type: Option<String>,
    pub exports: Option<serde_json::Value>,
    #[allow(dead_code)]
    pub dependencies: Option<HashMap<String, String>>,
    #[allow(dead_code)]
    #[serde(rename = "peerDependencies")]
    pub peer_dependencies: Option<HashMap<String, String>>,
}

impl PackageResolver {
    /// Create a new package resolver
    pub fn new(project_root: PathBuf, config: HuskConfig) -> Self {
        Self {
            project_root,
            config,
            package_cache: HashMap::new(),
            package_json_cache: HashMap::new(),
        }
    }

    /// Create resolver by finding husk.toml in current directory or parents
    pub fn from_current_dir() -> Result<Self> {
        let (config, project_root) = HuskConfig::find_and_load()?;
        Ok(Self::new(project_root, config))
    }

    /// Resolve a package import to its actual location and type information
    pub fn resolve_package(&mut self, package_name: &str) -> Result<ResolvedPackage> {
        // Check cache first
        if let Some(cached) = self.package_cache.get(package_name) {
            return Ok(cached.clone());
        }

        // Check if package is listed in husk.toml dependencies
        let has_dependency = self.config.dependencies.contains_key(package_name)
            || self.config.dev_dependencies.contains_key(package_name);

        if has_dependency {
            // Get the dependency spec after the borrow check
            let dep_spec = self
                .config
                .dependencies
                .get(package_name)
                .or_else(|| self.config.dev_dependencies.get(package_name))
                .unwrap()
                .clone();
            let resolved = self.resolve_dependency(package_name, &dep_spec)?;
            self.package_cache
                .insert(package_name.to_string(), resolved.clone());
            Ok(resolved)
        } else {
            Err(Error::new_config(format!(
                "Package '{package_name}' not found in dependencies. Add it to husk.toml [dependencies] or [dev-dependencies]"
            )))
        }
    }

    /// Resolve a specific dependency based on its specification
    fn resolve_dependency(&mut self, name: &str, spec: &DependencySpec) -> Result<ResolvedPackage> {
        match spec {
            DependencySpec::Simple(_version) => {
                // Standard npm package - resolve from node_modules
                self.resolve_npm_package(name)
            }
            DependencySpec::Detailed {
                version: Some(_version),
                path: None,
                git: None,
                ..
            } => {
                // Standard npm package with detailed version spec
                self.resolve_npm_package(name)
            }
            DependencySpec::Detailed {
                path: Some(local_path),
                ..
            } => {
                // Local path dependency
                self.resolve_local_package(name, local_path)
            }
            DependencySpec::Detailed {
                git: Some(_git_url),
                ..
            } => {
                // Git dependency - treat as npm package for now
                // In a full implementation, this would clone the repo
                self.resolve_npm_package(name)
            }
            _ => Err(Error::new_config(format!(
                "Invalid dependency specification for package '{name}'"
            ))),
        }
    }

    /// Resolve an npm package from node_modules
    fn resolve_npm_package(&mut self, name: &str) -> Result<ResolvedPackage> {
        let node_modules_path = self.project_root.join("node_modules");
        let package_path = node_modules_path.join(name);

        if !package_path.exists() {
            return Err(Error::new_config(format!(
                "Package '{name}' not found in node_modules. Run 'npm install' first."
            )));
        }

        let package_json_path = package_path.join("package.json");
        let package_json = self.load_package_json(&package_json_path)?;

        let main_file = package_json
            .main
            .clone()
            .or_else(|| package_json.module.clone())
            .unwrap_or_else(|| "index.js".to_string());

        let module_type = match package_json.module_type.as_deref() {
            Some("module") => ModuleType::ESModule,
            Some("commonjs") => ModuleType::CommonJS,
            _ => {
                // Guess based on file extension and main field
                if main_file.ends_with(".mjs") || package_json.module.is_some() {
                    ModuleType::ESModule
                } else {
                    ModuleType::CommonJS
                }
            }
        };

        // Extract exported names (simplified - in reality this would be more complex)
        let exports = self.extract_exports(&package_json);

        Ok(ResolvedPackage {
            name: name.to_string(),
            version: package_json
                .version
                .unwrap_or_else(|| "unknown".to_string()),
            path: package_path,
            main: Some(main_file),
            module_type,
            exports,
        })
    }

    /// Resolve a local path dependency
    fn resolve_local_package(
        &mut self,
        name: &str,
        relative_path: &str,
    ) -> Result<ResolvedPackage> {
        let package_path = self.project_root.join(relative_path);

        if !package_path.exists() {
            return Err(Error::new_config(format!(
                "Local dependency '{name}' not found at path: {relative_path}"
            )));
        }

        // Try to load package.json if it exists
        let package_json_path = package_path.join("package.json");
        let package_json = if package_json_path.exists() {
            Some(self.load_package_json(&package_json_path)?)
        } else {
            None
        };

        let main_file = package_json
            .as_ref()
            .and_then(|pkg| pkg.main.clone())
            .unwrap_or_else(|| "index.js".to_string());

        let module_type = package_json
            .as_ref()
            .and_then(|pkg| pkg.module_type.as_deref())
            .map(|t| match t {
                "module" => ModuleType::ESModule,
                "commonjs" => ModuleType::CommonJS,
                _ => ModuleType::Unknown,
            })
            .unwrap_or(ModuleType::Unknown);

        let exports = package_json
            .as_ref()
            .map(|pkg| self.extract_exports(pkg))
            .unwrap_or_default();

        Ok(ResolvedPackage {
            name: name.to_string(),
            version: package_json
                .as_ref()
                .and_then(|pkg| pkg.version.clone())
                .unwrap_or_else(|| "local".to_string()),
            path: package_path,
            main: Some(main_file),
            module_type,
            exports,
        })
    }

    /// Load and parse a package.json file
    fn load_package_json(&mut self, path: &Path) -> Result<PackageJson> {
        // Check cache first
        if let Some(cached) = self.package_json_cache.get(path) {
            return Ok(cached.clone());
        }

        let content = fs::read_to_string(path).map_err(|e| {
            Error::new_config(format!(
                "Failed to read package.json at {}: {}",
                path.display(),
                e
            ))
        })?;

        let package_json: PackageJson = serde_json::from_str(&content).map_err(|e| {
            Error::new_config(format!("Invalid package.json at {}: {}", path.display(), e))
        })?;

        self.package_json_cache
            .insert(path.to_path_buf(), package_json.clone());
        Ok(package_json)
    }

    /// Extract exported names from package.json (simplified)
    fn extract_exports(&self, package_json: &PackageJson) -> Vec<String> {
        let mut exports = Vec::new();

        // Add main export
        if package_json.main.is_some() || package_json.module.is_some() {
            exports.push("default".to_string());
        }

        // Process exports field (simplified)
        if let Some(exports_value) = &package_json.exports {
            match exports_value {
                serde_json::Value::String(_) => {
                    exports.push("default".to_string());
                }
                serde_json::Value::Object(map) => {
                    for key in map.keys() {
                        if key.starts_with('.') {
                            let export_name = if key == "." {
                                "default".to_string()
                            } else {
                                key.trim_start_matches("./").to_string()
                            };
                            exports.push(export_name);
                        }
                    }
                }
                _ => {}
            }
        }

        // If no exports found, assume default export
        if exports.is_empty() {
            exports.push("default".to_string());
        }

        exports
    }

    /// Check if a package import is external (npm package)
    pub fn is_external_package(&self, import_path: &str) -> bool {
        // External packages don't start with local::, self::, super::, or relative paths
        !import_path.starts_with("local::")
            && !import_path.starts_with("self::")
            && !import_path.starts_with("super::")
            && !import_path.starts_with("./")
            && !import_path.starts_with("../")
            && !import_path.starts_with("/")
    }

    /// Get the JavaScript import path for a resolved package
    pub fn get_import_path(&self, package: &ResolvedPackage, subpath: Option<&str>) -> String {
        match subpath {
            Some(sub) if !sub.is_empty() => format!("{}/{}", package.name, sub),
            _ => package.name.clone(),
        }
    }

    /// Generate appropriate import statement for transpiler
    pub fn generate_import_statement(
        &self,
        package: &ResolvedPackage,
        imports: &[String],
        subpath: Option<&str>,
    ) -> String {
        let import_path = self.get_import_path(package, subpath);

        match package.module_type {
            ModuleType::ESModule => {
                // Check if importing the package name itself (default import)
                if imports.len() == 1 && (imports[0] == package.name || imports[0] == "default") {
                    format!("import {} from \"{}\"", imports[0], import_path)
                } else if imports.contains(&"default".to_string()) {
                    let named_imports: Vec<_> =
                        imports.iter().filter(|&name| name != "default").collect();
                    if named_imports.is_empty() {
                        format!("import {} from \"{}\"", package.name, import_path)
                    } else {
                        format!(
                            "import {}, {{ {} }} from \"{}\"",
                            package.name,
                            named_imports
                                .iter()
                                .map(|s| s.as_str())
                                .collect::<Vec<_>>()
                                .join(", "),
                            import_path
                        )
                    }
                } else {
                    format!(
                        "import {{ {} }} from \"{}\"",
                        imports.join(", "),
                        import_path
                    )
                }
            }
            ModuleType::CommonJS => {
                if imports.len() == 1 && (imports[0] == package.name || imports[0] == "default") {
                    format!("const {} = require(\"{}\")", imports[0], import_path)
                } else {
                    format!(
                        "const {{ {} }} = require(\"{}\")",
                        imports.join(", "),
                        import_path
                    )
                }
            }
            _ => {
                // Fallback to CommonJS
                if imports.len() == 1 && imports[0] == package.name {
                    format!("const {} = require(\"{}\")", package.name, import_path)
                } else {
                    format!(
                        "const {{ {} }} = require(\"{}\")",
                        imports.join(", "),
                        import_path
                    )
                }
            }
        }
    }

    /// Get project dependencies for package.json generation
    pub fn get_project_dependencies(&self) -> &HuskConfig {
        &self.config
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_is_external_package() {
        let config = HuskConfig::default();
        let resolver = PackageResolver::new(PathBuf::from("/tmp"), config);

        // External packages
        assert!(resolver.is_external_package("express"));
        assert!(resolver.is_external_package("lodash"));
        assert!(resolver.is_external_package("@types/node"));

        // Local packages
        assert!(!resolver.is_external_package("local::utils"));
        assert!(!resolver.is_external_package("self::components"));
        assert!(!resolver.is_external_package("super::shared"));
        assert!(!resolver.is_external_package("./utils"));
        assert!(!resolver.is_external_package("../shared"));
        assert!(!resolver.is_external_package("/absolute/path"));
    }

    #[test]
    fn test_get_import_path() {
        let config = HuskConfig::default();
        let resolver = PackageResolver::new(PathBuf::from("/tmp"), config);

        let package = ResolvedPackage {
            name: "express".to_string(),
            version: "4.18.0".to_string(),
            path: PathBuf::from("/tmp/node_modules/express"),
            main: Some("index.js".to_string()),
            module_type: ModuleType::CommonJS,
            exports: vec!["default".to_string()],
        };

        assert_eq!(resolver.get_import_path(&package, None), "express");
        assert_eq!(
            resolver.get_import_path(&package, Some("router")),
            "express/router"
        );
        assert_eq!(resolver.get_import_path(&package, Some("")), "express");
    }

    #[test]
    fn test_generate_import_statement() {
        let config = HuskConfig::default();
        let resolver = PackageResolver::new(PathBuf::from("/tmp"), config);

        // ESM package with default import
        let esm_package = ResolvedPackage {
            name: "express".to_string(),
            version: "4.18.0".to_string(),
            path: PathBuf::from("/tmp/node_modules/express"),
            main: Some("index.js".to_string()),
            module_type: ModuleType::ESModule,
            exports: vec!["default".to_string()],
        };

        let stmt = resolver.generate_import_statement(&esm_package, &["default".to_string()], None);
        assert_eq!(stmt, "import express from \"express\";");

        // CommonJS package
        let cjs_package = ResolvedPackage {
            name: "lodash".to_string(),
            version: "4.17.21".to_string(),
            path: PathBuf::from("/tmp/node_modules/lodash"),
            main: Some("index.js".to_string()),
            module_type: ModuleType::CommonJS,
            exports: vec!["default".to_string()],
        };

        let stmt = resolver.generate_import_statement(&cjs_package, &["default".to_string()], None);
        assert_eq!(stmt, "const lodash = require(\"lodash\");");

        // Named imports
        let stmt = resolver.generate_import_statement(
            &esm_package,
            &["Router".to_string(), "json".to_string()],
            None,
        );
        assert_eq!(stmt, "import { Router, json } from \"express\";");
    }

    #[test]
    fn test_package_json_parsing() {
        let temp_dir = TempDir::new().unwrap();
        let package_json_path = temp_dir.path().join("package.json");

        let package_json_content = r#"{
            "name": "test-package",
            "version": "1.0.0",
            "main": "lib/index.js",
            "module": "es/index.js",
            "type": "module",
            "exports": {
                ".": "./lib/index.js",
                "./utils": "./lib/utils.js"
            }
        }"#;

        fs::write(&package_json_path, package_json_content).unwrap();

        let config = HuskConfig::default();
        let mut resolver = PackageResolver::new(temp_dir.path().to_path_buf(), config);

        let package_json = resolver.load_package_json(&package_json_path).unwrap();

        assert_eq!(package_json.name, Some("test-package".to_string()));
        assert_eq!(package_json.version, Some("1.0.0".to_string()));
        assert_eq!(package_json.main, Some("lib/index.js".to_string()));
        assert_eq!(package_json.module, Some("es/index.js".to_string()));
        assert_eq!(package_json.module_type, Some("module".to_string()));
    }
}
