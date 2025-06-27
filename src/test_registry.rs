use crate::{parser::Attribute, span::Span};
use std::collections::HashMap;

/// Information about a discovered test function
#[derive(Debug, Clone)]
pub struct TestFunction {
    /// Full name of the test function (including module path if in a test module)
    pub name: String,
    /// Location of the test in the source file
    pub span: Span,
    /// Whether the test should be ignored
    pub ignore: bool,
    /// Whether the test is expected to panic
    pub should_panic: bool,
    /// Optional panic message to expect
    pub panic_message: Option<String>,
    /// Module path where the test is defined (e.g., "tests::unit")
    pub module_path: Vec<String>,
}

impl TestFunction {
    pub fn new(name: String, span: Span) -> Self {
        TestFunction {
            name,
            span,
            ignore: false,
            should_panic: false,
            panic_message: None,
            module_path: Vec::new(),
        }
    }

    /// Get the full qualified name of the test (module_path::name)
    pub fn qualified_name(&self) -> String {
        if self.module_path.is_empty() {
            self.name.clone()
        } else {
            format!("{}::{}", self.module_path.join("::"), self.name)
        }
    }

    /// Apply attributes to the test function
    pub fn apply_attributes(&mut self, attributes: &[Attribute]) {
        for attr in attributes {
            match attr.name.as_str() {
                "ignore" => self.ignore = true,
                "should_panic" => {
                    self.should_panic = true;
                    // Check if there's an expected panic message
                    if let Some(expected) = attr.args.first() {
                        self.panic_message = Some(expected.clone());
                    }
                }
                _ => {} // Other attributes like #[test] are already handled
            }
        }
    }
}

/// Registry that collects all test functions during semantic analysis
#[derive(Debug, Default)]
pub struct TestRegistry {
    /// All discovered test functions
    tests: Vec<TestFunction>,
    /// Current module path for test discovery
    current_module_path: Vec<String>,
    /// Whether we're currently in a #[cfg(test)] module
    in_test_module: bool,
    /// Map from file paths to their test functions (for IDE integration)
    tests_by_file: HashMap<String, Vec<usize>>,
}

impl TestRegistry {
    pub fn new() -> Self {
        TestRegistry::default()
    }

    /// Enter a module (updates the current module path)
    pub fn enter_module(&mut self, name: &str, attributes: &[Attribute]) {
        self.current_module_path.push(name.to_string());

        // Check if this is a test module
        for attr in attributes {
            if attr.name == "cfg" && attr.args.contains(&"test".to_string()) {
                self.in_test_module = true;
            }
        }
    }

    /// Exit a module (pops from the current module path)
    pub fn exit_module(&mut self) {
        self.current_module_path.pop();
        // Reset test module flag when exiting a module
        // TODO: This should be a stack to handle nested modules correctly
        if self.current_module_path.is_empty() {
            self.in_test_module = false;
        }
    }

    /// Register a test function
    pub fn register_test(
        &mut self,
        name: String,
        span: Span,
        attributes: &[Attribute],
    ) -> Option<usize> {
        // Check if this function has the #[test] attribute
        let is_test = attributes.iter().any(|attr| attr.name == "test");
        if !is_test {
            return None;
        }

        let mut test = TestFunction::new(name, span);
        test.module_path = self.current_module_path.clone();
        test.apply_attributes(attributes);

        // Add to registry
        let test_index = self.tests.len();
        self.tests.push(test);

        // TODO: Track by file when we add file information to Span

        Some(test_index)
    }

    /// Get all registered tests
    pub fn all_tests(&self) -> &[TestFunction] {
        &self.tests
    }

    /// Get tests for a specific file
    pub fn tests_in_file(&self, file: &str) -> Vec<&TestFunction> {
        self.tests_by_file
            .get(file)
            .map(|indices| {
                indices
                    .iter()
                    .filter_map(|&idx| self.tests.get(idx))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Filter tests by pattern (supports simple wildcards)
    pub fn filter_tests(&self, pattern: &str) -> Vec<&TestFunction> {
        let pattern = pattern.to_lowercase();
        self.tests
            .iter()
            .filter(|test| {
                let name = test.qualified_name().to_lowercase();
                if pattern.contains('*') {
                    // Simple wildcard matching
                    let parts: Vec<&str> = pattern.split('*').collect();
                    if parts.len() == 2 {
                        name.starts_with(parts[0]) && name.ends_with(parts[1])
                    } else {
                        name.contains(&pattern.replace('*', ""))
                    }
                } else {
                    name.contains(&pattern)
                }
            })
            .collect()
    }

    /// Get count of tests by status
    pub fn test_stats(&self) -> TestStats {
        let mut stats = TestStats::default();
        for test in &self.tests {
            stats.total += 1;
            if test.ignore {
                stats.ignored += 1;
            } else {
                stats.active += 1;
            }
        }
        stats
    }

    /// Check if we're currently in a test context (for conditional compilation)
    pub fn is_in_test_context(&self) -> bool {
        self.in_test_module
    }
}

/// Statistics about discovered tests
#[derive(Debug, Default)]
pub struct TestStats {
    pub total: usize,
    pub active: usize,
    pub ignored: usize,
}

impl TestStats {
    pub fn summary(&self) -> String {
        format!(
            "{} tests found ({} active, {} ignored)",
            self.total, self.active, self.ignored
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_qualified_name() {
        let mut test = TestFunction::new("test_example".to_string(), Span::default());
        assert_eq!(test.qualified_name(), "test_example");

        test.module_path = vec!["tests".to_string(), "unit".to_string()];
        assert_eq!(test.qualified_name(), "tests::unit::test_example");
    }

    #[test]
    fn test_filter_tests() {
        let mut registry = TestRegistry::new();

        // Add some test functions
        registry.tests.push(TestFunction::new(
            "test_addition".to_string(),
            Span::default(),
        ));
        registry.tests.push(TestFunction::new(
            "test_subtraction".to_string(),
            Span::default(),
        ));
        registry.tests.push(TestFunction::new(
            "integration_test".to_string(),
            Span::default(),
        ));

        // Test exact match
        let filtered = registry.filter_tests("addition");
        assert_eq!(filtered.len(), 1);
        assert_eq!(filtered[0].name, "test_addition");

        // Test wildcard
        let filtered = registry.filter_tests("test_*");
        assert_eq!(filtered.len(), 2);

        // Test partial match
        let filtered = registry.filter_tests("test");
        assert_eq!(filtered.len(), 3);
    }
}
