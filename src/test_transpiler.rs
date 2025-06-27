use crate::{
    ast::visitor::AstVisitor,
    error::{Error, Result},
    parser::Stmt,
    test_registry::{TestFunction, TestRegistry},
    transpiler::JsTranspiler,
};

/// JavaScript test harness generator for transpiler mode
pub struct TestTranspiler {
    transpiler: JsTranspiler,
}

/// Configuration for test transpilation
#[derive(Debug, Clone)]
pub struct TestTranspileConfig {
    /// Whether to include Node.js compatibility
    pub node_compat: bool,
    /// Whether to generate ES modules
    pub es_modules: bool,
    /// Test runner framework to use (node, jest, mocha)
    pub runner: TestRunner,
    /// Whether to include timing information
    pub include_timing: bool,
}

#[derive(Debug, Clone)]
pub enum TestRunner {
    /// Built-in Node.js test runner
    Node,
    /// Jest framework
    Jest,
    /// Mocha framework
    Mocha,
    /// Custom standalone runner
    Standalone,
}

impl Default for TestTranspileConfig {
    fn default() -> Self {
        TestTranspileConfig {
            node_compat: true,
            es_modules: false,
            runner: TestRunner::Standalone,
            include_timing: true,
        }
    }
}

impl TestTranspiler {
    pub fn new() -> Self {
        TestTranspiler {
            transpiler: JsTranspiler::new(),
        }
    }

    /// Generate a complete JavaScript test harness
    pub fn generate_test_harness(
        &mut self,
        ast: &[Stmt],
        registry: &TestRegistry,
        config: &TestTranspileConfig,
    ) -> Result<String> {
        let mut output = String::new();

        // Add header comment
        output.push_str("// Generated test harness for Husk tests\n");
        output.push_str("// This file was automatically generated - do not edit\n\n");

        // Add runtime utilities and setup
        output.push_str(&self.generate_test_runtime(config)?);

        // Transpile the main code (excluding test functions)
        let main_code = self.transpile_main_code(ast)?;
        if !main_code.trim().is_empty() {
            output.push_str("// Main application code\n");
            output.push_str(&main_code);
            output.push_str("\n");
        }

        // Generate test functions
        output.push_str("// Test functions\n");
        for test in registry.all_tests() {
            output.push_str(&self.generate_test_function(ast, test, config)?);
        }

        // Generate test runner
        output.push_str("\n// Test runner\n");
        output.push_str(&self.generate_test_runner(registry, config)?);

        Ok(output)
    }

    /// Generate the JavaScript test runtime (assertions, utilities, etc.)
    fn generate_test_runtime(&self, config: &TestTranspileConfig) -> Result<String> {
        let mut runtime = String::new();

        // Add Node.js imports if needed
        if config.node_compat {
            match config.runner {
                TestRunner::Node => {
                    runtime.push_str("const { test, describe } = require('node:test');\n");
                    runtime.push_str("const assert = require('node:assert');\n\n");
                }
                TestRunner::Jest => {
                    runtime.push_str("// Jest globals (test, expect) are available\n\n");
                }
                TestRunner::Mocha => {
                    runtime.push_str("const assert = require('assert');\n");
                    runtime.push_str("// Mocha globals (describe, it) are available\n\n");
                }
                TestRunner::Standalone => {
                    // No imports needed for standalone
                }
            }
        }

        // Add basic Husk runtime functions
        runtime.push_str(&self.generate_husk_runtime()?);

        // Add test utilities
        runtime.push_str(&self.generate_test_utilities(config)?);

        Ok(runtime)
    }

    /// Generate basic Husk runtime functions needed by tests
    fn generate_husk_runtime(&self) -> Result<String> {
        Ok(r#"// Basic Husk runtime functions
function println(...args) { 
    console.log(...args); 
}

function panic(message) {
    throw new Error(message || "panic");
}

// Husk assertion functions
function assert(condition, message) {
    if (!condition) {
        throw new Error(message || "Assertion failed");
    }
}

function assert_eq(left, right, message) {
    if (left !== right) {
        const msg = message || `Assertion failed: expected ${JSON.stringify(right)}, got ${JSON.stringify(left)}`;
        throw new Error(msg);
    }
}

function assert_ne(left, right, message) {
    if (left === right) {
        const msg = message || `Assertion failed: expected values to be different, but both were ${JSON.stringify(left)}`;
        throw new Error(msg);
    }
}

"#.to_string())
    }

    /// Generate test utility functions
    fn generate_test_utilities(&self, config: &TestTranspileConfig) -> Result<String> {
        let mut utilities = String::new();

        match config.runner {
            TestRunner::Standalone => {
                utilities.push_str(&self.generate_standalone_utilities(config)?);
            }
            TestRunner::Node => {
                utilities.push_str(&self.generate_node_utilities(config)?);
            }
            TestRunner::Jest => {
                utilities.push_str(&self.generate_jest_utilities(config)?);
            }
            TestRunner::Mocha => {
                utilities.push_str(&self.generate_mocha_utilities(config)?);
            }
        }

        Ok(utilities)
    }

    /// Generate standalone test utilities
    fn generate_standalone_utilities(&self, config: &TestTranspileConfig) -> Result<String> {
        let timing_code = if config.include_timing {
            r#"
    const startTime = performance.now();
    try {
        await testFn();
        const duration = performance.now() - startTime;
        console.log(`✓ ${name} (${duration.toFixed(3)}ms)`);
        return { name, passed: true, duration, error: null };
    } catch (error) {
        const duration = performance.now() - startTime;
        if (shouldPanic) {
            console.log(`✓ ${name} (${duration.toFixed(3)}ms)`);
            return { name, passed: true, duration, error: null };
        } else {
            console.log(`✗ ${name}: ${error.message} (${duration.toFixed(3)}ms)`);
            return { name, passed: false, duration, error: error.message };
        }
    }"#
        } else {
            r#"
    try {
        await testFn();
        console.log(`✓ ${name}`);
        return { name, passed: true, duration: 0, error: null };
    } catch (error) {
        if (shouldPanic) {
            console.log(`✓ ${name}`);
            return { name, passed: true, duration: 0, error: null };
        } else {
            console.log(`✗ ${name}: ${error.message}`);
            return { name, passed: false, duration: 0, error: error.message };
        }
    }"#
        };

        Ok(format!(
            r#"// Standalone test utilities
const tests = [];

function registerTest(name, testFn, shouldPanic = false, ignore = false) {{
    tests.push({{ name, testFn, shouldPanic, ignore }});
}}

async function runTest(name, testFn, shouldPanic = false) {{{timing_code}
}}

async function runAllTests() {{
    console.log(`\nrunning ${{tests.length}} test${{tests.length === 1 ? '' : 's'}}`);
    
    const results = [];
    for (const {{ name, testFn, shouldPanic, ignore }} of tests) {{
        if (ignore) {{
            console.log(`test ${{name}} ... ignored`);
            results.push({{ name, passed: true, duration: 0, error: null, ignored: true }});
            continue;
        }}
        
        const result = await runTest(name, testFn, shouldPanic);
        results.push(result);
    }}
    
    const passed = results.filter(r => r.passed && !r.ignored).length;
    const failed = results.filter(r => !r.passed).length;
    const ignored = results.filter(r => r.ignored).length;
    
    console.log();
    if (failed > 0) {{
        console.log("failures:");
        console.log();
        for (const result of results.filter(r => !r.passed)) {{
            console.log(`    ${{result.name}}`);
        }}
        console.log();
    }}
    
    const resultStr = failed > 0 ? "FAILED" : "ok";
    const timingInfo = results.reduce((sum, r) => sum + r.duration, 0);
    console.log(`test result: ${{resultStr}}. ${{passed}} passed; ${{failed > 0 ? failed + ' failed; ' : ''}}${{ignored > 0 ? ignored + ' ignored; ' : ''}}0 measured; 0 filtered out; finished in ${{(timingInfo / 1000).toFixed(2)}}s`);
    
    if (failed > 0 && typeof process !== 'undefined') {{
        process.exit(1);
    }}
    
    return results;
}}

"#
        ))
    }

    /// Generate Node.js test utilities
    fn generate_node_utilities(&self, _config: &TestTranspileConfig) -> Result<String> {
        Ok(r#"// Node.js test utilities
function registerTest(name, testFn, shouldPanic = false, ignore = false) {
    const testOptions = {};
    if (ignore) {
        testOptions.skip = true;
    }
    
    test(name, testOptions, async () => {
        if (shouldPanic) {
            await assert.rejects(testFn, Error);
        } else {
            await testFn();
        }
    });
}

"#
        .to_string())
    }

    /// Generate Jest utilities
    fn generate_jest_utilities(&self, _config: &TestTranspileConfig) -> Result<String> {
        Ok(r#"// Jest test utilities
function registerTest(name, testFn, shouldPanic = false, ignore = false) {
    const testImpl = async () => {
        if (shouldPanic) {
            await expect(testFn()).rejects.toThrow();
        } else {
            await testFn();
        }
    };
    
    if (ignore) {
        test.skip(name, testImpl);
    } else {
        test(name, testImpl);
    }
}

"#
        .to_string())
    }

    /// Generate Mocha utilities
    fn generate_mocha_utilities(&self, _config: &TestTranspileConfig) -> Result<String> {
        Ok(r#"// Mocha test utilities
function registerTest(name, testFn, shouldPanic = false, ignore = false) {
    const testImpl = async () => {
        if (shouldPanic) {
            let threw = false;
            try {
                await testFn();
            } catch (e) {
                threw = true;
            }
            assert(threw, `Expected test to throw, but it didn't`);
        } else {
            await testFn();
        }
    };
    
    if (ignore) {
        it.skip(name, testImpl);
    } else {
        it(name, testImpl);
    }
}

"#
        .to_string())
    }

    /// Transpile main application code (excluding test functions)
    fn transpile_main_code(&mut self, ast: &[Stmt]) -> Result<String> {
        let mut main_stmts = Vec::new();

        for stmt in ast {
            match stmt {
                // Skip test functions
                Stmt::Function(attrs, _, _, _, _, _, _, _) => {
                    if attrs.iter().any(|a| a.name == "test") {
                        continue;
                    }
                    main_stmts.push(stmt.clone());
                }
                // Skip test modules
                Stmt::Module(attrs, _, _, _) => {
                    let is_test_module = attrs
                        .iter()
                        .any(|a| a.name == "cfg" && a.args.contains(&"test".to_string()));
                    if is_test_module {
                        continue;
                    }
                    main_stmts.push(stmt.clone());
                }
                // Include all other statements
                _ => {
                    main_stmts.push(stmt.clone());
                }
            }
        }

        self.transpiler.transpile(&main_stmts)
    }

    /// Generate a single test function in JavaScript
    fn generate_test_function(
        &mut self,
        ast: &[Stmt],
        test: &TestFunction,
        _config: &TestTranspileConfig,
    ) -> Result<String> {
        // Find the test function in the AST
        let test_stmt = self.find_test_function(ast, &test.name, &test.module_path)?;

        // Transpile the function body
        let function_code = match test_stmt {
            Stmt::Function(_, _, name, _, params, _, body, _) => {
                if !params.is_empty() {
                    return Err(Error::new_transpile(
                        format!("Test function '{}' cannot have parameters", name),
                        test.span,
                    ));
                }

                // Transpile function body
                let mut body_code = String::new();
                for stmt in body {
                    let stmt_code = self.transpiler.visit_stmt(stmt)?;
                    body_code.push_str("    ");
                    body_code.push_str(&stmt_code);
                    body_code.push('\n');
                }

                format!("async function {}() {{\n{}}}\n", name, body_code)
            }
            _ => {
                return Err(Error::new_transpile(
                    format!("Expected function statement for test '{}'", test.name),
                    test.span,
                ));
            }
        };

        // Generate test registration
        let registration = format!(
            "registerTest('{}', {}, {}, {});\n",
            test.qualified_name(),
            test.name,
            test.should_panic,
            test.ignore
        );

        Ok(format!("{}{}", function_code, registration))
    }

    /// Find a test function in the AST
    fn find_test_function<'a>(
        &self,
        ast: &'a [Stmt],
        name: &str,
        module_path: &[String],
    ) -> Result<&'a Stmt> {
        for stmt in ast {
            match stmt {
                Stmt::Function(attrs, _, fn_name, _, _, _, _, _) => {
                    if fn_name == name && module_path.is_empty() {
                        if attrs.iter().any(|a| a.name == "test") {
                            return Ok(stmt);
                        }
                    }
                }
                Stmt::Module(_attrs, mod_name, body, _) => {
                    if !module_path.is_empty() && module_path[0] == *mod_name {
                        for mod_stmt in body {
                            if let Stmt::Function(attrs, _, fn_name, _, _, _, _, _) = mod_stmt {
                                if fn_name == name && attrs.iter().any(|a| a.name == "test") {
                                    return Ok(mod_stmt);
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        Err(Error::new_transpile(
            format!("Test function '{}' not found in AST", name),
            crate::span::Span::default(),
        ))
    }

    /// Generate the test runner code
    fn generate_test_runner(
        &self,
        _registry: &TestRegistry,
        config: &TestTranspileConfig,
    ) -> Result<String> {
        match config.runner {
            TestRunner::Standalone => {
                Ok("// Run all tests\nrunAllTests().catch(console.error);\n".to_string())
            }
            TestRunner::Node => {
                Ok("// Tests are automatically run by Node.js test runner\n".to_string())
            }
            TestRunner::Jest => Ok("// Tests are automatically run by Jest\n".to_string()),
            TestRunner::Mocha => Ok(r#"// Mocha test suite
describe('Husk Tests', () => {
    // Test functions are registered above
});
"#
            .to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Span;

    #[test]
    fn test_generate_husk_runtime() {
        let transpiler = TestTranspiler::new();
        let runtime = transpiler.generate_husk_runtime().unwrap();

        assert!(runtime.contains("function println"));
        assert!(runtime.contains("function panic"));
        assert!(runtime.contains("function assert("));
        assert!(runtime.contains("function assert_eq"));
        assert!(runtime.contains("function assert_ne"));
    }

    #[test]
    fn test_generate_standalone_utilities() {
        let transpiler = TestTranspiler::new();
        let config = TestTranspileConfig::default();
        let utilities = transpiler.generate_standalone_utilities(&config).unwrap();

        assert!(utilities.contains("registerTest"));
        assert!(utilities.contains("runAllTests"));
        assert!(utilities.contains("performance.now()"));
    }

    #[test]
    fn test_test_function_qualified_name() {
        let mut test = TestFunction::new("test_example".to_string(), Span::default());
        assert_eq!(test.qualified_name(), "test_example");

        test.module_path = vec!["tests".to_string()];
        assert_eq!(test.qualified_name(), "tests::test_example");
    }
}
