use crate::{
    ast::visitor::AstVisitor,
    error::{Error, Result},
    interpreter::InterpreterVisitor,
    parser::Stmt,
    test_registry::{TestFunction, TestRegistry},
    Value,
};
use std::time::{Duration, Instant};

/// Result of running a single test
#[derive(Debug)]
pub struct TestResult {
    /// Name of the test
    pub name: String,
    /// Whether the test passed
    pub passed: bool,
    /// Duration of the test
    pub duration: Duration,
    /// Captured output during test execution
    pub output: String,
    /// Error message if test failed
    pub error: Option<String>,
    /// Whether the test was ignored
    pub ignored: bool,
}

impl TestResult {
    fn passed(name: String, duration: Duration, output: String) -> Self {
        TestResult {
            name,
            passed: true,
            duration,
            output,
            error: None,
            ignored: false,
        }
    }

    fn failed(name: String, duration: Duration, output: String, error: String) -> Self {
        TestResult {
            name,
            passed: false,
            duration,
            output,
            error: Some(error),
            ignored: false,
        }
    }

    fn ignored(name: String) -> Self {
        TestResult {
            name,
            passed: true,
            duration: Duration::from_secs(0),
            output: String::new(),
            error: None,
            ignored: true,
        }
    }
}

/// Configuration for test execution
#[derive(Debug, Clone)]
pub struct TestConfig {
    /// Whether to show output from passing tests
    pub show_output: bool,
    /// Whether to capture output or let it go to stdout
    pub capture_output: bool,
    /// Whether to stop on first failure
    pub fail_fast: bool,
    /// Whether to run ignored tests
    pub include_ignored: bool,
    /// Filter pattern for test names
    pub filter: Option<String>,
    /// Number of threads to use (1 = sequential)
    pub test_threads: usize,
    /// Whether to show detailed timing
    pub show_timing: bool,
}

impl Default for TestConfig {
    fn default() -> Self {
        TestConfig {
            show_output: false,
            capture_output: true,
            fail_fast: false,
            include_ignored: false,
            filter: None,
            test_threads: 1,
            show_timing: false,
        }
    }
}

/// Test runner for interpreter mode
pub struct TestRunner {
    config: TestConfig,
    ast: Vec<Stmt>,
}

impl TestRunner {
    pub fn new(ast: Vec<Stmt>, config: TestConfig) -> Self {
        TestRunner { config, ast }
    }

    /// Run all tests and return results with source code for error formatting
    pub fn run_tests_with_source(
        &self,
        registry: &TestRegistry,
        source_code: &str,
    ) -> Vec<TestResult> {
        let mut results = Vec::new();

        // Get tests to run based on filter
        let tests: Vec<&TestFunction> = if let Some(filter) = &self.config.filter {
            registry.filter_tests(filter)
        } else {
            registry.all_tests().iter().collect()
        };

        // Print test plan
        let active_tests: Vec<_> = tests
            .iter()
            .filter(|t| !t.ignore || self.config.include_ignored)
            .collect();

        println!(
            "\nrunning {} test{}",
            active_tests.len(),
            if active_tests.len() == 1 { "" } else { "s" }
        );

        let start_time = Instant::now();

        // Run each test
        for test in tests {
            if test.ignore && !self.config.include_ignored {
                results.push(TestResult::ignored(test.qualified_name()));
                continue;
            }

            let result = self.run_single_test_with_source(test, source_code);
            let failed = !result.passed && !result.ignored;

            // Print test result
            self.print_test_result(&result);

            results.push(result);

            // Stop if fail_fast is enabled and test failed
            if failed && self.config.fail_fast {
                break;
            }
        }

        let total_duration = start_time.elapsed();

        // Print summary
        self.print_summary(&results, total_duration);

        results
    }

    /// Run all tests and return results
    pub fn run_tests(&self, registry: &TestRegistry) -> Vec<TestResult> {
        let mut results = Vec::new();

        // Get tests to run based on filter
        let tests: Vec<&TestFunction> = if let Some(filter) = &self.config.filter {
            registry.filter_tests(filter)
        } else {
            registry.all_tests().iter().collect()
        };

        // Print test plan
        let active_tests: Vec<_> = tests
            .iter()
            .filter(|t| !t.ignore || self.config.include_ignored)
            .collect();

        println!(
            "\nrunning {} test{}",
            active_tests.len(),
            if active_tests.len() == 1 { "" } else { "s" }
        );

        let start_time = Instant::now();

        // Run each test
        for test in tests {
            if test.ignore && !self.config.include_ignored {
                results.push(TestResult::ignored(test.qualified_name()));
                continue;
            }

            let result = self.run_single_test(test);
            let failed = !result.passed && !result.ignored;

            // Print test result
            self.print_test_result(&result);

            results.push(result);

            // Stop if fail_fast is enabled and test failed
            if failed && self.config.fail_fast {
                break;
            }
        }

        let total_duration = start_time.elapsed();

        // Print summary
        self.print_summary(&results, total_duration);

        results
    }

    /// Run a single test with source code for better error reporting
    fn run_single_test_with_source(&self, test: &TestFunction, source_code: &str) -> TestResult {
        let start = Instant::now();

        // TODO: Implement output capture when we have proper I/O handling
        let output = String::new();

        // Find the test function in the AST
        let test_stmt = self.find_test_function(&test.name, &test.module_path);

        match test_stmt {
            Some(stmt) => {
                // Create a new interpreter for this test
                let mut interpreter = InterpreterVisitor::new();

                // Initialize built-in functions (like println)
                // The interpreter should already have these initialized in new()

                // Execute any necessary setup (imports, type definitions, etc.)
                // For now, we'll run the whole program but only execute the test function
                match self.setup_test_environment(&mut interpreter, &test.module_path) {
                    Ok(_) => {
                        // Execute the test function
                        match self.execute_test_function(&mut interpreter, stmt, test) {
                            Ok(_) => {
                                let duration = start.elapsed();
                                TestResult::passed(test.qualified_name(), duration, output)
                            }
                            Err(e) => {
                                let duration = start.elapsed();
                                // Use pretty_print for better error formatting
                                let error_msg = e.pretty_print(source_code);

                                // Check if this was an expected panic
                                if test.should_panic {
                                    // If a panic message was specified, check if it matches
                                    if let Some(expected_msg) = &test.panic_message {
                                        if error_msg.contains(expected_msg) {
                                            TestResult::passed(
                                                test.qualified_name(),
                                                duration,
                                                output,
                                            )
                                        } else {
                                            TestResult::failed(
                                                test.qualified_name(),
                                                duration,
                                                output,
                                                format!(
                                                    "Expected panic message '{}', but got '{}'",
                                                    expected_msg, error_msg
                                                ),
                                            )
                                        }
                                    } else {
                                        // No specific message expected, any panic is fine
                                        TestResult::passed(test.qualified_name(), duration, output)
                                    }
                                } else {
                                    TestResult::failed(
                                        test.qualified_name(),
                                        duration,
                                        output,
                                        error_msg,
                                    )
                                }
                            }
                        }
                    }
                    Err(e) => {
                        let duration = start.elapsed();
                        TestResult::failed(
                            test.qualified_name(),
                            duration,
                            output,
                            format!("Setup failed: {}", e.pretty_print(source_code)),
                        )
                    }
                }
            }
            None => {
                let duration = start.elapsed();
                TestResult::failed(
                    test.qualified_name(),
                    duration,
                    output,
                    "Test function not found in AST".to_string(),
                )
            }
        }
    }

    /// Run a single test
    fn run_single_test(&self, test: &TestFunction) -> TestResult {
        let start = Instant::now();

        // TODO: Implement output capture when we have proper I/O handling
        let output = String::new();

        // Find the test function in the AST
        let test_stmt = self.find_test_function(&test.name, &test.module_path);

        match test_stmt {
            Some(stmt) => {
                // Create a new interpreter for this test
                let mut interpreter = InterpreterVisitor::new();

                // Initialize built-in functions (like println)
                // The interpreter should already have these initialized in new()

                // Execute any necessary setup (imports, type definitions, etc.)
                // For now, we'll run the whole program but only execute the test function
                match self.setup_test_environment(&mut interpreter, &test.module_path) {
                    Ok(_) => {
                        // Execute the test function
                        match self.execute_test_function(&mut interpreter, stmt, test) {
                            Ok(_) => {
                                let duration = start.elapsed();
                                TestResult::passed(test.qualified_name(), duration, output)
                            }
                            Err(e) => {
                                let duration = start.elapsed();
                                let error_msg = format!("{}", e);

                                // Check if this was an expected panic
                                if test.should_panic {
                                    // If a panic message was specified, check if it matches
                                    if let Some(expected_msg) = &test.panic_message {
                                        if error_msg.contains(expected_msg) {
                                            TestResult::passed(
                                                test.qualified_name(),
                                                duration,
                                                output,
                                            )
                                        } else {
                                            TestResult::failed(
                                                test.qualified_name(),
                                                duration,
                                                output,
                                                format!(
                                                    "Expected panic message '{}', but got '{}'",
                                                    expected_msg, error_msg
                                                ),
                                            )
                                        }
                                    } else {
                                        // No specific message expected, any panic is fine
                                        TestResult::passed(test.qualified_name(), duration, output)
                                    }
                                } else {
                                    TestResult::failed(
                                        test.qualified_name(),
                                        duration,
                                        output,
                                        error_msg,
                                    )
                                }
                            }
                        }
                    }
                    Err(e) => {
                        let duration = start.elapsed();
                        TestResult::failed(
                            test.qualified_name(),
                            duration,
                            output,
                            format!("Setup failed: {}", e),
                        )
                    }
                }
            }
            None => {
                let duration = start.elapsed();
                TestResult::failed(
                    test.qualified_name(),
                    duration,
                    output,
                    format!("Test function not found in AST"),
                )
            }
        }
    }

    /// Find a test function in the AST
    fn find_test_function(&self, name: &str, module_path: &[String]) -> Option<&Stmt> {
        // TODO: This is a simplified version. In a real implementation,
        // we would need to properly traverse the AST respecting module boundaries

        for stmt in &self.ast {
            match stmt {
                Stmt::Function(attrs, _, fn_name, _, _, _, _, _) => {
                    if fn_name == name && module_path.is_empty() {
                        // Check if it has the test attribute
                        if attrs.iter().any(|a| a.name == "test") {
                            return Some(stmt);
                        }
                    }
                }
                Stmt::Module(_attrs, mod_name, body, _) => {
                    // Check if this is the right module
                    if !module_path.is_empty() && module_path[0] == *mod_name {
                        // Recursively search in the module
                        // TODO: Handle nested modules properly
                        for mod_stmt in body {
                            if let Stmt::Function(attrs, _, fn_name, _, _, _, _, _) = mod_stmt {
                                if fn_name == name && attrs.iter().any(|a| a.name == "test") {
                                    return Some(mod_stmt);
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        None
    }

    /// Setup the test environment (imports, types, etc.)
    fn setup_test_environment(
        &self,
        interpreter: &mut InterpreterVisitor,
        module_path: &[String],
    ) -> Result<()> {
        // Execute all non-test statements to set up the environment
        // This includes struct/enum definitions, imports, etc.

        for stmt in &self.ast {
            match stmt {
                // Skip test functions during setup
                Stmt::Function(attrs, _, _, _, _, _, _, _)
                | Stmt::AsyncFunction(attrs, _, _, _, _, _, _, _) => {
                    if attrs.iter().any(|a| a.name == "test") {
                        continue;
                    }
                    interpreter.visit_stmt(stmt)?;
                }
                // Skip test modules during setup unless we're in that module
                Stmt::Module(attrs, name, body, _) => {
                    let is_test_module = attrs
                        .iter()
                        .any(|a| a.name == "cfg" && a.args.contains(&"test".to_string()));

                    if is_test_module {
                        // Only process if we're running a test in this module
                        if !module_path.is_empty() && module_path[0] == *name {
                            // Process module contents
                            for mod_stmt in body {
                                if !matches!(mod_stmt, Stmt::Function(attrs, _, _, _, _, _, _, _)
                                           if attrs.iter().any(|a| a.name == "test"))
                                {
                                    interpreter.visit_stmt(mod_stmt)?;
                                }
                            }
                        }
                    } else {
                        interpreter.visit_stmt(stmt)?;
                    }
                }
                // Execute all other statements
                _ => {
                    interpreter.visit_stmt(stmt)?;
                }
            }
        }

        Ok(())
    }

    /// Execute a test function
    fn execute_test_function(
        &self,
        interpreter: &mut InterpreterVisitor,
        stmt: &Stmt,
        test: &TestFunction,
    ) -> Result<Value> {
        match stmt {
            Stmt::Function(_, _, name, _, params, _, body, _) => {
                // Verify test function has no parameters
                if !params.is_empty() {
                    return Err(Error::new_test_error(
                        format!("Test function '{}' cannot have parameters", name),
                        test.span,
                    ));
                }

                // Execute the test function body
                // Create a new scope for the function
                let mut last_value = Value::Unit;
                for stmt in body {
                    last_value = interpreter.visit_stmt(stmt)?;
                }
                Ok(last_value)
            }
            _ => Err(Error::new_test_error(
                format!("Expected function statement for test '{}'", test.name),
                test.span,
            )),
        }
    }

    /// Print a single test result
    fn print_test_result(&self, result: &TestResult) {
        let status = if result.ignored {
            "ignored"
        } else if result.passed {
            "ok"
        } else {
            "FAILED"
        };

        let timing = if self.config.show_timing {
            format!(" ({:.3}s)", result.duration.as_secs_f64())
        } else {
            String::new()
        };

        println!("test {} ... {}{}", result.name, status, timing);

        // Show output if requested or if test failed
        if !result.output.is_empty() && (self.config.show_output || !result.passed) {
            println!("---- {} stdout ----", result.name);
            print!("{}", result.output);
            if !result.output.ends_with('\n') {
                println!();
            }
        }

        // Show error if test failed
        if let Some(error) = &result.error {
            println!("---- {} stderr ----", result.name);
            println!("{}", error);
            println!();
        }
    }

    /// Print test summary
    fn print_summary(&self, results: &[TestResult], total_duration: Duration) {
        let passed = results.iter().filter(|r| r.passed && !r.ignored).count();
        let failed = results.iter().filter(|r| !r.passed).count();
        let ignored = results.iter().filter(|r| r.ignored).count();

        println!();

        if failed > 0 {
            println!("failures:");
            println!();
            for result in results.iter().filter(|r| !r.passed) {
                println!("    {}", result.name);
            }
            println!();
        }

        let result_str = if failed > 0 { "FAILED" } else { "ok" };

        print!("test result: {}. ", result_str);
        print!("{} passed; ", passed);
        if failed > 0 {
            print!("{} failed; ", failed);
        }
        if ignored > 0 {
            print!("{} ignored; ", ignored);
        }
        println!(
            "0 measured; 0 filtered out; finished in {:.2}s",
            total_duration.as_secs_f64()
        );

        if failed > 0 {
            println!();
            std::process::exit(1);
        }
    }
}

/// Extension trait to add test error to Error type
trait TestError {
    fn new_test_error(msg: String, span: crate::span::Span) -> Self;
}

impl TestError for Error {
    fn new_test_error(msg: String, span: crate::span::Span) -> Self {
        // For now, use semantic error
        // TODO: Add a specific test error variant
        Error::new_semantic(msg, span)
    }
}
