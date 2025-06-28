# Integration Tests for NPM Features

This directory contains integration tests for the npm improvement features implemented in Tasks 4.1-4.3.

## Test Overview

The `test_npm_features.sh` script creates real Husk projects using `husk new` and tests the following features from a black-box perspective:

### ✅ Passing Tests (6/8)

1. **Import Maps Feature** - Maps package names to custom URLs
   - ✅ Import maps correctly map packages to URLs
   - ✅ Tree shaking configuration respected in browser build

2. **Tree Shaking Feature** - Pure function annotations for dead code elimination
   - ✅ Tree shaking configuration respected: browser build succeeded  
   - ✅ Tree shaking disabled: no pure annotations in node-cjs build

3. **Development vs Production Mode** - Different optimizations per build type
   - ✅ Development mode: build succeeded and tree shaking disabled
   - ✅ Production mode: build succeeded with clean output

### ❌ Failing Tests (2/8)

4. **Combined Features** - All features working together
   - ❌ Combined features in dev mode: only 1/3 features working
   - ❌ Combined features in prod mode: only 1/2 features working

## Key Implementation Details

### Import Maps Configuration

Import maps must be configured using TOML table syntax:

```toml
[targets.browser]
tree_shaking = true
dev = false

[targets.browser.import_map]
react = "https://esm.sh/react@18"
lodash = "https://cdn.skypack.dev/lodash"
```

### Tree Shaking

Tree shaking adds `/*#__PURE__*/` annotations to known pure functions like:
- `Math.max`, `Math.min`, `Object.assign`, etc.
- Only works with JavaScript built-ins, not user-defined functions
- Automatically disabled in development mode

### Development vs Production Mode

- **Development Mode (`dev: true`)**:
  - Tree shaking disabled for faster builds
  - Debug comments and runtime type assertions (when implemented)
  
- **Production Mode (`dev: false`)**:
  - Tree shaking enabled (if configured)
  - Clean, optimized output

## Usage

```bash
# Run all integration tests
./test_npm_features.sh

# Debug specific features
./debug_import_maps.sh
```

## File Structure

- `test_npm_features.sh` - Main integration test suite
- `debug_import_maps.sh` - Debug script for import maps
- `README.md` - This documentation

## Test Results

Current status: **6/8 tests passing (75%)**

The core npm improvement features (import maps, tree shaking, dev/prod mode) are all working correctly individually. The combined features test may have configuration issues that need further investigation.