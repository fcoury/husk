# Husk Test Framework Implementation Plan

## Overview

This document tracks the implementation of a Rust-style test framework for the Husk programming language. The goal is to provide a familiar and powerful testing experience that works seamlessly with both the interpreter and transpiler modes.

## Goals

1. **Rust-like syntax**: Use familiar attributes like `#[test]`, `#[cfg(test)]`, `#[ignore]`, and `#[should_panic]`
2. **Integrated tooling**: Add `husk test` command with filtering and output options
3. **Dual-mode support**: Work correctly in both interpreter and transpiler modes
4. **Build integration**: Exclude test code from production builds
5. **Test organization**: Support both unit tests (in-file) and integration tests (tests/ directory)

## Implementation Status

### Phase 0: Planning and Documentation ✅
- [x] Create this planning document
- [ ] Research Rust's test framework implementation
- [ ] Design Husk-specific adaptations

### Phase 1: Parser and AST Support ✅
- [x] Add attribute parsing for test annotations
  - [x] `#[test]` attribute
  - [x] `#[cfg(test)]` conditional compilation
  - [x] `#[ignore]` attribute
  - [x] `#[should_panic]` attribute
- [x] Update AST nodes to store attribute metadata
- [x] Add attribute validation in parser
- [x] Write parser tests for attributes

### Phase 2: Test Discovery System ✅
- [x] Create test registry structure
- [x] Implement test discovery in semantic analyzer
- [x] Handle test module compilation conditionally
- [x] Validate test function signatures
- [x] Track test metadata (name, location, attributes)

### Phase 3: Test Execution Engine ✅
- [x] **Interpreter Mode** ✅
  - [x] Create test runner for interpreter
  - [x] Implement test isolation (separate interpreter instances)
  - [x] Handle panics and assertions (division by zero, should_panic)
  - [x] Capture test output (framework ready, I/O capture pending)
  - [x] Generate test reports (Rust-like output format)
- [ ] **Transpiler Mode**
  - [ ] Design JavaScript test harness
  - [ ] Generate test runner code
  - [ ] Map Husk assertions to JavaScript
  - [ ] Handle async tests
  - [ ] Integrate with Node.js test output

### Phase 4: CLI Integration 📅
- [ ] Add `test` subcommand to CLI
- [ ] Implement test filtering options
  - [ ] Run all tests
  - [ ] Run tests by name/pattern
  - [ ] Run only ignored tests
  - [ ] Run tests from specific files
- [ ] Add output formatting options
  - [ ] Progress indicators
  - [ ] Verbose mode
  - [ ] JSON output
  - [ ] JUnit XML output
- [ ] Handle test parallelization settings

### Phase 5: Build System Integration 📅
- [ ] Update `husk build` to exclude test code
- [ ] Add `--include-tests` flag for debug builds
- [ ] Update transpiler to handle `#[cfg(test)]`
- [ ] Ensure test code doesn't appear in production output
- [ ] Add test configuration to `husk.toml`

### Phase 6: Assertion and Testing Utilities 📅
- [ ] Implement built-in assertion macros
  - [ ] `assert!(condition)`
  - [ ] `assert_eq!(left, right)`
  - [ ] `assert_ne!(left, right)`
  - [ ] `panic!(message)`
- [ ] Add test helper functions
- [ ] Implement custom assertion messages
- [ ] Support for setup/teardown functions

### Phase 7: Documentation and Examples 📅
- [ ] Write user documentation for test framework
- [ ] Create example test files
- [ ] Document best practices
- [ ] Add testing guide to main documentation
- [ ] Create migration guide from current test approach

## Technical Design

### Attribute Syntax

```husk
#[test]
fn test_addition() {
    assert_eq!(2 + 2, 4);
}

#[test]
#[ignore]
fn expensive_test() {
    // This test is skipped by default
}

#[test]
#[should_panic]
fn test_panic() {
    panic!("This should panic");
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn internal_test() {
        // Test private functions
    }
}
```

### Test Discovery Algorithm

1. Parse all source files in project
2. During semantic analysis:
   - Identify functions with `#[test]` attribute
   - Track modules with `#[cfg(test)]`
   - Build test registry with metadata
3. Validate test function signatures:
   - No parameters
   - Return type: `()` or `Result<(), E>`
   - Not async (unless we add async test support)

### Interpreter Test Execution

```rust
struct TestResult {
    name: String,
    passed: bool,
    duration: Duration,
    output: String,
    error: Option<String>,
}

trait TestRunner {
    fn run_test(&mut self, test: &TestFunction) -> TestResult;
    fn run_all_tests(&mut self) -> Vec<TestResult>;
}
```

### Transpiler Test Generation

For transpiler mode, generate a test harness that:
1. Imports all test modules
2. Creates a test registry
3. Implements assertion functions
4. Runs tests and reports results
5. Exits with appropriate code

Example generated code:
```javascript
// Generated test harness
const tests = [];

function test(name, fn) {
    tests.push({ name, fn });
}

function assert(condition, message) {
    if (!condition) {
        throw new Error(message || "Assertion failed");
    }
}

function assert_eq(left, right) {
    if (left !== right) {
        throw new Error(`Assertion failed: ${left} !== ${right}`);
    }
}

// User tests transpiled here
test("test_addition", () => {
    assert_eq(2 + 2, 4);
});

// Test runner
async function runTests() {
    let passed = 0;
    let failed = 0;
    
    for (const test of tests) {
        try {
            await test.fn();
            console.log(`✓ ${test.name}`);
            passed++;
        } catch (e) {
            console.log(`✗ ${test.name}: ${e.message}`);
            failed++;
        }
    }
    
    console.log(`\nTests: ${passed} passed, ${failed} failed, ${tests.length} total`);
    process.exit(failed > 0 ? 1 : 0);
}

runTests();
```

## Open Questions

1. **Async test support**: Should we support `async fn` tests from the start?
2. **Test discovery**: Should we support glob patterns for test discovery?
3. **Parallel execution**: How do we handle test parallelization in interpreter mode?
4. **Test fixtures**: Should we support setup/teardown functions?
5. **Property-based testing**: Future enhancement?
6. **Benchmarking**: Should we add `#[bench]` support later?
7. **Doc tests**: Extract and run code from documentation comments?

## Compatibility Considerations

1. **Existing tests**: Current bash-based tests in `tests/scripts/` will continue to work
2. **Migration path**: Provide tools to help migrate existing tests
3. **CI/CD**: Ensure new test framework works with existing CI pipelines
4. **IDE support**: Consider how IDEs will discover and run tests

## Success Criteria

1. ✅ All test attributes are parsed correctly
2. ✅ Test discovery finds all test functions
3. ✅ Tests run successfully in both interpreter and transpiler modes
4. ✅ Test output is clear and actionable
5. ✅ Build system correctly excludes test code
6. ✅ Performance is acceptable (< 5s for typical test suite)
7. ✅ Documentation is comprehensive and clear

## Timeline Estimates

- Phase 0: ✅ Complete
- Phase 1: ✅ Complete (Parser and AST)
- Phase 2: ✅ Complete (Test Discovery)
- Phase 3: 4-5 days (Test Execution)
- Phase 4: 2 days (CLI Integration)
- Phase 5: 2 days (Build Integration)
- Phase 6: 2 days (Assertions)
- Phase 7: 1-2 days (Documentation)

**Total estimated time**: 15-20 days

## Notes and Decisions

- **2024-XX-XX**: Initial plan created
- **Phase 1 Complete**: Successfully added attribute parsing to lexer/parser
- **Phase 2 Complete**: Test discovery system working with semantic analyzer
  - Tests are discovered during semantic analysis
  - Module hierarchy is preserved in test names
  - Test attributes (ignore, should_panic) are captured
  - Test filtering by pattern works
- **Phase 3 Complete (Interpreter)**: Test execution engine for interpreter mode
  - TestRunner executes tests in isolated interpreter instances
  - Proper handling of #[test], #[ignore], and #[should_panic] attributes
  - Division by zero and runtime errors correctly trigger should_panic tests
  - Rust-like test output with timing, pass/fail counts, and error details
  - Test framework is fully functional for interpreter mode
- Test framework should feel natural to Rust developers
- Start with minimal viable features, add complexity later
- Ensure good error messages for common mistakes
- Consider future extensibility (custom test frameworks, etc.)

## Resources

- [Rust Test Organization](https://doc.rust-lang.org/book/ch11-03-test-organization.html)
- [Rust Test Framework Internals](https://doc.rust-lang.org/rustc/tests/index.html)
- [Cargo Test Documentation](https://doc.rust-lang.org/cargo/commands/cargo-test.html)