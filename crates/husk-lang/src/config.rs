//! Husk project configuration (husk.toml) parsing and types.

use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Platform target for configuration.
/// Used in husk.toml to specify the target platform.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum ConfigPlatform {
    /// Server-side execution (Node.js)
    #[default]
    Node,
    /// Client-side execution (Browser)
    Browser,
    /// Auto-detect from package.json
    Auto,
}

/// Root configuration structure for husk.toml.
#[derive(Debug, Clone, Deserialize, Default)]
pub struct HuskConfig {
    /// Package metadata.
    pub package: Option<PackageConfig>,
    /// Build configuration.
    #[serde(default)]
    pub build: BuildConfig,
    /// DTS entries for TypeScript definition imports.
    #[serde(default)]
    pub dts: Vec<DtsEntry>,
    /// Global DTS options.
    #[serde(rename = "dts.options", alias = "dts_options")]
    pub dts_options: Option<DtsOptions>,
    /// Runtime configuration.
    pub run: Option<RunConfig>,
    /// Watch mode configuration.
    pub watch: Option<WatchConfig>,
}

/// Package metadata section.
#[derive(Debug, Clone, Deserialize)]
pub struct PackageConfig {
    /// Project name.
    pub name: String,
    /// Project version (semver).
    pub version: String,
    /// Project description.
    pub description: Option<String>,
    /// List of authors.
    pub authors: Option<Vec<String>>,
    /// License identifier.
    pub license: Option<String>,
    /// Repository URL.
    pub repository: Option<String>,
}

/// Build configuration section.
#[derive(Debug, Clone, Deserialize, Default)]
pub struct BuildConfig {
    /// Entry point file path.
    pub entry: Option<String>,
    /// Output directory.
    pub output: Option<String>,
    /// JavaScript target: "esm", "cjs", or "auto".
    pub target: Option<String>,
    /// Platform target: "node", "browser", or "auto".
    pub platform: Option<ConfigPlatform>,
    /// Library mode (don't call main()).
    pub lib: Option<bool>,
    /// Emit TypeScript declaration file.
    pub emit_dts: Option<bool>,
    /// Generate source maps.
    pub source_maps: Option<bool>,
    /// Inject stdlib prelude (Option/Result types).
    pub prelude: Option<bool>,
}

/// A single DTS dependency entry.
#[derive(Debug, Clone, Deserialize)]
pub struct DtsEntry {
    /// NPM package name.
    pub package: String,
    /// TypeScript types package (e.g., "@types/express").
    pub types: Option<String>,
    /// Version constraint (semver).
    pub version: Option<String>,
    /// Output path for generated .hk file.
    pub output: String,
    /// Types to include (filter).
    pub include: Option<Vec<String>>,
    /// Types to exclude (filter).
    pub exclude: Option<Vec<String>>,
    /// Union handling strategy: "enum", "jsvalue", or "auto".
    pub union_strategy: Option<String>,
    /// Generate builder patterns for interfaces with many optional props.
    pub generate_builders: Option<bool>,
    /// Minimum optional properties to trigger builder generation.
    pub builder_min_optional: Option<usize>,
    /// Expand utility types (Partial, Pick, etc.) inline.
    pub expand_utility_types: Option<bool>,
    /// Use `extern "js" const` for TypeScript constants instead of zero-arg functions.
    pub use_extern_const: Option<bool>,
    /// Follow import graph and include dependencies.
    pub follow_imports: Option<bool>,
    /// Maximum depth for import resolution.
    pub max_import_depth: Option<usize>,
    /// Base directory for resolving this package (overrides project root).
    pub base_dir: Option<String>,
    /// Explicit path to .d.ts file (overrides automatic discovery).
    pub types_path: Option<String>,
    /// Use generation gap pattern for this entry (overrides global setting).
    pub generation_gap: Option<bool>,
}

/// Global options for DTS management.
#[derive(Debug, Clone, Deserialize, Default)]
pub struct DtsOptions {
    /// Default output directory for generated .hk files.
    pub output_dir: Option<String>,
    /// Auto-update .hk files on build.
    pub auto_update: Option<bool>,
    /// Warning level: "all", "simplified", "none".
    pub warn_level: Option<String>,
    /// Generate diagnostic report (dts-report.md).
    pub generate_report: Option<bool>,
    /// Default union handling strategy: "enum", "jsvalue", or "auto".
    pub default_union_strategy: Option<String>,
    /// Default setting for builder generation.
    pub default_generate_builders: Option<bool>,
    /// Default setting for utility type expansion.
    pub default_expand_utility_types: Option<bool>,
    /// Default setting for using extern const.
    pub default_use_extern_const: Option<bool>,
    /// Node modules search paths.
    pub node_modules_paths: Option<Vec<String>>,
    /// Use generation gap pattern (*.gen.hk + *.hk wrapper).
    /// Default: true
    pub generation_gap: Option<bool>,
}

/// Runtime configuration for `huskc run`.
#[derive(Debug, Clone, Deserialize, Default)]
pub struct RunConfig {
    /// Additional arguments to pass to Node.js.
    pub node_args: Option<Vec<String>>,
    /// Environment variables.
    pub env: Option<HashMap<String, String>>,
}

/// Watch mode configuration.
#[derive(Debug, Clone, Deserialize, Default)]
pub struct WatchConfig {
    /// Debounce delay in milliseconds.
    pub debounce_ms: Option<u64>,
    /// Glob patterns to ignore.
    pub ignore: Option<Vec<String>>,
    /// Run the compiled output after each change.
    pub run: Option<bool>,
    /// Custom command to execute after compilation.
    pub exec: Option<String>,
}

impl HuskConfig {
    /// Load configuration from husk.toml in the current directory.
    /// Returns None if file doesn't exist.
    pub fn load() -> Option<Self> {
        Self::load_from_path(Path::new("husk.toml"))
    }

    /// Load configuration from a specific path.
    pub fn load_from_path(path: &Path) -> Option<Self> {
        if !path.exists() {
            return None;
        }
        let content = fs::read_to_string(path).ok()?;
        Self::parse(&content).ok()
    }

    /// Parse configuration from a TOML string.
    pub fn parse(content: &str) -> Result<Self, toml::de::Error> {
        toml::from_str(content)
    }

    /// Load configuration or return default if not found.
    pub fn load_or_default() -> Self {
        Self::load().unwrap_or_default()
    }
}

impl BuildConfig {
    /// Get the output directory, defaulting to "dist".
    pub fn output_dir(&self) -> &str {
        self.output.as_deref().unwrap_or("dist")
    }

    /// Get whether source maps should be generated (default: true).
    pub fn source_maps(&self) -> bool {
        self.source_maps.unwrap_or(true)
    }

    /// Get whether prelude should be injected (default: true).
    pub fn prelude(&self) -> bool {
        self.prelude.unwrap_or(true)
    }

    /// Get whether library mode is enabled (default: false).
    pub fn lib(&self) -> bool {
        self.lib.unwrap_or(false)
    }

    /// Get whether to emit .d.ts (default: false).
    pub fn emit_dts(&self) -> bool {
        self.emit_dts.unwrap_or(false)
    }

    /// Get the platform target (default: Auto).
    pub fn platform(&self) -> ConfigPlatform {
        self.platform.unwrap_or(ConfigPlatform::Auto)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_minimal_config() {
        let toml = r#"
[package]
name = "test-project"
version = "0.1.0"
"#;
        let config = HuskConfig::parse(toml).unwrap();
        assert_eq!(config.package.as_ref().unwrap().name, "test-project");
        assert_eq!(config.package.as_ref().unwrap().version, "0.1.0");
    }

    #[test]
    fn test_parse_build_config() {
        let toml = r#"
[build]
entry = "src/main.hk"
output = "out"
target = "esm"
lib = true
emit_dts = true
source_maps = false
prelude = false
"#;
        let config = HuskConfig::parse(toml).unwrap();
        assert_eq!(config.build.entry, Some("src/main.hk".to_string()));
        assert_eq!(config.build.output_dir(), "out");
        assert_eq!(config.build.target, Some("esm".to_string()));
        assert!(config.build.lib());
        assert!(config.build.emit_dts());
        assert!(!config.build.source_maps());
        assert!(!config.build.prelude());
    }

    #[test]
    fn test_parse_dts_entries() {
        let toml = r#"
[[dts]]
package = "express"
types = "@types/express"
version = "^5.0.0"
output = "src/extern/express.hk"

[[dts]]
package = "better-sqlite3"
types = "@types/better-sqlite3"
output = "src/extern/sqlite.hk"
include = ["Database", "Statement"]
"#;
        let config = HuskConfig::parse(toml).unwrap();
        assert_eq!(config.dts.len(), 2);

        let express = &config.dts[0];
        assert_eq!(express.package, "express");
        assert_eq!(express.types, Some("@types/express".to_string()));
        assert_eq!(express.version, Some("^5.0.0".to_string()));
        assert_eq!(express.output, "src/extern/express.hk");

        let sqlite = &config.dts[1];
        assert_eq!(sqlite.package, "better-sqlite3");
        assert_eq!(
            sqlite.include,
            Some(vec!["Database".to_string(), "Statement".to_string()])
        );
    }

    #[test]
    fn test_parse_run_config() {
        let toml = r#"
[run]
node_args = ["--enable-source-maps", "--experimental-modules"]
"#;
        let config = HuskConfig::parse(toml).unwrap();
        let run = config.run.unwrap();
        assert_eq!(
            run.node_args,
            Some(vec![
                "--enable-source-maps".to_string(),
                "--experimental-modules".to_string()
            ])
        );
    }

    #[test]
    fn test_parse_watch_config() {
        let toml = r#"
[watch]
debounce_ms = 300
ignore = ["dist/**", "node_modules/**"]
"#;
        let config = HuskConfig::parse(toml).unwrap();
        let watch = config.watch.unwrap();
        assert_eq!(watch.debounce_ms, Some(300));
        assert_eq!(
            watch.ignore,
            Some(vec!["dist/**".to_string(), "node_modules/**".to_string()])
        );
    }

    #[test]
    fn test_parse_empty_config() {
        let config = HuskConfig::parse("").unwrap();
        assert!(config.package.is_none());
        assert!(config.build.entry.is_none());
        assert!(config.dts.is_empty());
    }

    #[test]
    fn test_build_config_defaults() {
        let config = BuildConfig::default();
        assert_eq!(config.output_dir(), "dist");
        assert!(config.source_maps());
        assert!(config.prelude());
        assert!(!config.lib());
        assert!(!config.emit_dts());
        assert_eq!(config.platform(), ConfigPlatform::Auto);
    }

    #[test]
    fn test_platform_config() {
        // Test browser platform
        let toml = r#"
[build]
platform = "browser"
"#;
        let config = HuskConfig::parse(toml).unwrap();
        assert_eq!(config.build.platform(), ConfigPlatform::Browser);

        // Test node platform
        let toml = r#"
[build]
platform = "node"
"#;
        let config = HuskConfig::parse(toml).unwrap();
        assert_eq!(config.build.platform(), ConfigPlatform::Node);

        // Test auto platform
        let toml = r#"
[build]
platform = "auto"
"#;
        let config = HuskConfig::parse(toml).unwrap();
        assert_eq!(config.build.platform(), ConfigPlatform::Auto);

        // Test unset platform defaults to auto
        let toml = r#"
[build]
entry = "src/main.hk"
"#;
        let config = HuskConfig::parse(toml).unwrap();
        assert_eq!(config.build.platform(), ConfigPlatform::Auto);
    }

    #[test]
    fn test_full_config() {
        let toml = r#"
[package]
name = "my-app"
version = "1.0.0"
description = "My awesome app"
authors = ["Alice <alice@example.com>"]
license = "MIT"

[build]
entry = "src/main.hk"
output = "dist"
target = "auto"
source_maps = true

[[dts]]
package = "express"
types = "@types/express"
output = "src/extern/express.hk"

[run]
node_args = ["--enable-source-maps"]

[watch]
debounce_ms = 200
"#;
        let config = HuskConfig::parse(toml).unwrap();
        assert_eq!(config.package.as_ref().unwrap().name, "my-app");
        assert_eq!(config.build.entry, Some("src/main.hk".to_string()));
        assert_eq!(config.dts.len(), 1);
        assert!(config.run.is_some());
        assert!(config.watch.is_some());
    }

    #[test]
    fn test_generation_gap_config() {
        let toml = r#"
[dts_options]
generation_gap = false

[[dts]]
package = "express"
output = "src/extern/express.hk"

[[dts]]
package = "better-sqlite3"
output = "src/extern/sqlite.hk"
generation_gap = true
"#;
        let config = HuskConfig::parse(toml).unwrap();

        // Global default is false
        assert_eq!(
            config.dts_options.as_ref().unwrap().generation_gap,
            Some(false)
        );

        // First entry inherits global (no override)
        let express = &config.dts[0];
        assert!(express.generation_gap.is_none());

        // Second entry overrides global
        let sqlite = &config.dts[1];
        assert_eq!(sqlite.generation_gap, Some(true));
    }
}
