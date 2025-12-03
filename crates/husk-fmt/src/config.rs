//! Formatter configuration.

/// Configuration options for the Husk formatter.
#[derive(Debug, Clone)]
pub struct FormatConfig {
    /// Maximum line length before breaking.
    pub max_line_length: usize,
    /// Number of spaces per indentation level.
    pub indent_size: usize,
    /// Use tabs instead of spaces for indentation.
    pub use_tabs: bool,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            max_line_length: 100,
            indent_size: 4,
            use_tabs: false,
        }
    }
}

impl FormatConfig {
    /// Create a new config with custom line length.
    pub fn with_line_length(mut self, length: usize) -> Self {
        self.max_line_length = length;
        self
    }

    /// Create a new config with custom indent size.
    pub fn with_indent_size(mut self, size: usize) -> Self {
        self.indent_size = size;
        self
    }

    /// Create a new config using tabs for indentation.
    pub fn with_tabs(mut self) -> Self {
        self.use_tabs = true;
        self
    }

    /// Get the indentation string for one level.
    pub fn indent_str(&self) -> String {
        if self.use_tabs {
            "\t".to_string()
        } else {
            " ".repeat(self.indent_size)
        }
    }
}
