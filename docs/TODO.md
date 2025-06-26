# Transpiler Target Modes TODO

## Overview

This document tracks the remaining work needed for the transpiler target modes feature. Several tests have been marked as `#[ignore]` due to incomplete implementation.

## Failing Tests Summary

As of the latest test run, 12 tests are currently ignored:

### 1. Import Path Resolution
- **Test**: `test_transpile_multiple_imports`
- **Issue**: Import path resolution needs to be updated to match new transpiler behavior
- **Details**: The test expects specific import formats for Node.js built-in modules and local imports, but the current transpiler generates different output

### 2. Target Mode Configuration (6 tests)
- **Tests**:
  - `test_transpile_browser_target`
  - `test_transpile_node_esm_target`
  - `test_transpile_node_cjs_target`
  - `test_transpile_es2015_target`
  - `test_transpile_iife_format`
  - `test_transpile_umd_format`
- **Issue**: Target mode configuration not fully implemented
- **Details**: The transpiler needs to support different output formats based on the target environment (browser, Node.js, Deno) and module system (ESM, CommonJS, IIFE, UMD)

### 3. Platform-Specific Code Generation (3 tests)
- **Tests**:
  - `test_node_platform_specifics`
  - `test_browser_platform_specifics`
  - `test_deno_platform_specifics`
- **Issue**: Platform-specific code generation not fully implemented
- **Details**: The transpiler should generate different code based on the target platform (e.g., using `process` in Node.js, `window` in browsers)

### 4. Package Resolution with Target Mode (2 tests)
- **Tests**:
  - `test_external_deps_node_target`
  - `test_external_deps_browser_target`
- **Issue**: Package resolution with target mode not fully implemented
- **Details**: The transpiler needs to handle external dependencies differently based on the target environment

## Implementation Tasks

### Task 1: Implement Target Mode Configuration
- [ ] Add support for different module formats (ESM, CommonJS, IIFE, UMD)
- [ ] Implement configuration parsing for target modes
- [ ] Update transpiler to use configuration when generating output

### Task 2: Platform-Specific Code Generation
- [ ] Detect platform-specific APIs in code
- [ ] Generate appropriate alternatives for each platform
- [ ] Add platform detection helpers

### Task 3: Fix Import Path Resolution
- [ ] Update import path generation to match test expectations
- [ ] Handle Node.js built-in modules correctly
- [ ] Support different import styles based on target

### Task 4: Package Resolution Enhancement
- [ ] Integrate package resolution with target mode
- [ ] Support browser-specific package resolutions (e.g., using CDN URLs)
- [ ] Handle Node.js-specific package features

## Testing Strategy

Once implementation is complete:
1. Remove `#[ignore]` attributes from tests
2. Run full test suite to ensure all tests pass
3. Add additional tests for edge cases
4. Test with real-world projects

## Related Files

- `/src/transpiler_target_modes_test.rs` - Contains the ignored tests
- `/src/transpiler_js_interop_tests.rs` - Contains import-related test
- `/src/transpiler.rs` - Main transpiler implementation
- `/src/package_resolver.rs` - Package resolution logic

## Notes

- The current transpiler works correctly for basic transpilation
- Target modes are an advanced feature for production use
- Consider implementing incrementally, starting with the most common use cases (Node.js ESM and browser ESM)