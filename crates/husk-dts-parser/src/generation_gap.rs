//! Generation Gap pattern for DTS code generation.
//!
//! This module implements the Generation Gap pattern where:
//! - `*.gen.hk` files are auto-generated and always overwritten
//! - `*.hk` wrapper files are created once and preserved across regeneration
//!
//! This allows users to customize bindings without losing changes on regeneration.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// Writer that implements the Generation Gap pattern.
pub struct GenerationGapWriter {
    /// Whether to use the generation gap pattern (true) or single file mode (false).
    enabled: bool,
}

impl GenerationGapWriter {
    /// Create a new writer with the generation gap pattern enabled or disabled.
    pub fn new(enabled: bool) -> Self {
        Self { enabled }
    }

    /// Write generated code to the output path.
    ///
    /// If generation gap is enabled:
    /// - Writes to `{name}.gen.hk` (always overwritten)
    /// - Creates `{name}.hk` wrapper if it doesn't exist
    ///
    /// If generation gap is disabled:
    /// - Writes directly to the specified path (legacy behavior)
    pub fn write(&self, output_path: &Path, generated_code: &str) -> io::Result<WriteResult> {
        // Ensure parent directory exists
        if let Some(parent) = output_path.parent() {
            if !parent.as_os_str().is_empty() {
                fs::create_dir_all(parent)?;
            }
        }

        if !self.enabled {
            // Legacy single-file mode
            fs::write(output_path, generated_code)?;
            return Ok(WriteResult {
                gen_path: output_path.to_path_buf(),
                wrapper_path: None,
                wrapper_created: false,
            });
        }

        // Generation gap mode
        let (gen_path, wrapper_path) = self.split_paths(output_path);

        // Always overwrite the .gen.hk file
        let gen_content = self.add_gen_header(generated_code);
        fs::write(&gen_path, gen_content)?;

        // Create wrapper only if it doesn't exist
        let wrapper_created = if !wrapper_path.exists() {
            let module_name = output_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("module");
            let wrapper_content = self.create_wrapper_template(module_name);
            fs::write(&wrapper_path, wrapper_content)?;
            true
        } else {
            false
        };

        Ok(WriteResult {
            gen_path,
            wrapper_path: Some(wrapper_path),
            wrapper_created,
        })
    }

    /// Split the output path into .gen.hk and .hk paths.
    fn split_paths(&self, output_path: &Path) -> (PathBuf, PathBuf) {
        let parent = output_path.parent().unwrap_or(Path::new("."));
        let stem = output_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output");

        // Remove .hk extension if present, then add .gen.hk
        let stem = stem.strip_suffix(".gen").unwrap_or(stem);

        let gen_path = parent.join(format!("{}.gen.hk", stem));
        let wrapper_path = parent.join(format!("{}.hk", stem));

        (gen_path, wrapper_path)
    }

    /// Add header comment to generated file.
    fn add_gen_header(&self, code: &str) -> String {
        format!(
            "// AUTO-GENERATED FILE - DO NOT EDIT\n\
             // Re-run `huskc dts update` to regenerate\n\
             // To customize, edit the wrapper file instead\n\n\
             {}",
            code
        )
    }

    /// Create a wrapper template that imports from the generated file.
    fn create_wrapper_template(&self, module_name: &str) -> String {
        format!(
            "// User customizations for {} bindings\n\
             // This file is preserved across regeneration\n\n\
             pub use {}.gen.*;\n\n\
             // Add your customizations below:\n",
            module_name, module_name
        )
    }
}

/// Result of writing files with the generation gap pattern.
#[derive(Debug)]
pub struct WriteResult {
    /// Path to the generated file (.gen.hk or direct output).
    pub gen_path: PathBuf,
    /// Path to the wrapper file (.hk), if generation gap is enabled.
    pub wrapper_path: Option<PathBuf>,
    /// Whether the wrapper was created (only true on first generation).
    pub wrapper_created: bool,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_legacy_mode() {
        let dir = tempdir().unwrap();
        let output_path = dir.path().join("express.hk");

        let writer = GenerationGapWriter::new(false);
        let result = writer.write(&output_path, "// generated code").unwrap();

        assert_eq!(result.gen_path, output_path);
        assert!(result.wrapper_path.is_none());
        assert!(!result.wrapper_created);
        assert!(output_path.exists());
    }

    #[test]
    fn test_generation_gap_creates_both_files() {
        let dir = tempdir().unwrap();
        let output_path = dir.path().join("express.hk");

        let writer = GenerationGapWriter::new(true);
        let result = writer.write(&output_path, "// generated code").unwrap();

        assert_eq!(result.gen_path, dir.path().join("express.gen.hk"));
        assert_eq!(result.wrapper_path, Some(dir.path().join("express.hk")));
        assert!(result.wrapper_created);

        // Both files should exist
        assert!(result.gen_path.exists());
        assert!(result.wrapper_path.as_ref().unwrap().exists());

        // Generated file should have header
        let gen_content = fs::read_to_string(&result.gen_path).unwrap();
        assert!(gen_content.contains("AUTO-GENERATED"));
        assert!(gen_content.contains("// generated code"));

        // Wrapper should have import
        let wrapper_content = fs::read_to_string(result.wrapper_path.unwrap()).unwrap();
        assert!(wrapper_content.contains("pub use express.gen.*"));
    }

    #[test]
    fn test_wrapper_not_overwritten() {
        let dir = tempdir().unwrap();
        let output_path = dir.path().join("express.hk");
        let wrapper_path = dir.path().join("express.hk");

        // Create custom wrapper first
        fs::write(&wrapper_path, "// my custom code").unwrap();

        let writer = GenerationGapWriter::new(true);
        let result = writer.write(&output_path, "// new generated code").unwrap();

        assert!(!result.wrapper_created);

        // Wrapper should still have custom code
        let wrapper_content = fs::read_to_string(&wrapper_path).unwrap();
        assert_eq!(wrapper_content, "// my custom code");

        // Generated file should be updated
        let gen_content = fs::read_to_string(&result.gen_path).unwrap();
        assert!(gen_content.contains("// new generated code"));
    }

    #[test]
    fn test_split_paths() {
        let writer = GenerationGapWriter::new(true);

        // Normal .hk file
        let (gen_path, wrapper_path) = writer.split_paths(Path::new("extern/express.hk"));
        assert_eq!(gen_path, PathBuf::from("extern/express.gen.hk"));
        assert_eq!(wrapper_path, PathBuf::from("extern/express.hk"));

        // Already has .gen.hk (shouldn't double)
        let (gen_path, wrapper_path) = writer.split_paths(Path::new("extern/express.gen.hk"));
        assert_eq!(gen_path, PathBuf::from("extern/express.gen.hk"));
        assert_eq!(wrapper_path, PathBuf::from("extern/express.hk"));
    }
}
