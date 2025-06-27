# NPM Support Improvement Tracker

This document tracks the implementation progress of npm support improvements in Husk.

## Overview

**Goal**: Fix and improve npm package support in Husk to make it production-ready.  
**Timeline**: 4 weeks (Phase 1 is the priority)  
**Status**: 🚧 In Progress

## Phase 1: Critical Import Issues (Week 1)

### Task 1.1: Make package.json Generation Default
**Status**: ✅ Completed  
**Priority**: HIGH  
**Description**: Change package.json generation from opt-in to default behavior

**Changes**:
- [x] Rename `--generate-package-json` flag to `--skip-package-json`
- [x] Generate package.json by default during build
- [x] Add smart detection to avoid unnecessary regeneration
- [x] Show helpful messages about npm install

**Files modified**:
- `src/main.rs` - Updated Build struct and build_command function
- `tests/build_command_test.rs` - Updated tests to reflect new behavior

---

### Task 1.2: Fix Import Path Generation
**Status**: ✅ Completed  
**Priority**: HIGH  
**Description**: Fix incorrect import paths (e.g., "express/express" → "express")

**Changes**:
- [x] Remove duplicate package names in import paths
- [x] Handle default exports correctly
- [x] Fix named import syntax
- [x] Distinguish between package imports and subpath imports
- [x] Remove double semicolons from import statements

**Files modified**:
- `src/transpiler.rs` - Updated visit_use() method to properly handle import names vs subpaths
- `src/package_resolver.rs` - Removed semicolons from generate_import_statement()

---

### Task 1.3: Consistent Module Format
**Status**: ✅ Completed  
**Priority**: HIGH  
**Description**: Ensure consistent use of either import or require (not mixed)

**Changes**:
- [x] Add module_format field to JsTranspiler
- [x] Detect target module format from config
- [x] Use consistent import/require based on target
- [x] Default to ESM for packages with "type": "module"
- [x] Update Node.js built-ins to use consistent format

**Files modified**:
- `src/transpiler.rs` - Added ModuleFormat enum and module_format field
- `src/package_resolver.rs` - Updated generate_import_statement to respect module format
- Fixed all Node.js built-in imports (fs, path, readline, fsPromises)

---

### Task 1.4: Fix Package Resolution
**Status**: ✅ Completed  
**Priority**: HIGH  
**Description**: Improve package.json parsing and export resolution

**Changes**:
- [x] Handle exports field properly
- [x] Support conditional exports
- [x] Resolve main entry correctly  
- [x] Special handling for CommonJS packages with named imports in ESM mode
- [x] Improved module type detection based on package.json fields

**Files modified**:
- `src/package_resolver.rs` - Added resolve_exports_entry() method for proper exports field resolution
- Added special import generation for CommonJS packages (import as default, then destructure)

---

## Phase 2: Enhanced Package Resolution (Week 2)

### Task 2.1: Implement Exports Field Resolution
**Status**: ✅ Completed  
**Priority**: MEDIUM  
**Description**: Enhanced package.json exports field resolution for subpath imports

**Changes**:
- [x] Enhanced resolve_exports_entry() to handle nested conditional exports
- [x] Added support for wildcard patterns in exports (e.g., "./foo/*")
- [x] Added resolve_package_subpath() method for subpath resolution
- [x] Integrated subpath resolution into transpiler's visit_use()
- [x] Support for conditional exports (import/require/default/node)

**Files modified**:
- `src/package_resolver.rs` - Enhanced exports field resolution with wildcard and conditional support
- `src/transpiler.rs` - Integrated subpath resolution in visit_use method  

### Task 2.2: Better Module Type Detection
**Status**: ✅ Completed  
**Priority**: MEDIUM  
**Description**: Improved module type detection with multiple heuristics

**Changes**:
- [x] Check package.json "type" field (highest priority)
- [x] Detect .mjs and .cjs file extensions
- [x] Handle dual packages (both main and module fields)
- [x] Analyze exports field for module type hints
- [x] File content analysis for ESM/CommonJS/UMD patterns
- [x] Smart selection of entry point based on build target

**Files modified**:
- `src/package_resolver.rs` - Added detect_module_type() method with comprehensive heuristics  

### Task 2.3: TypeScript Definitions Support
**Status**: ✅ Completed  
**Priority**: LOW  
**Description**: Add support for resolving TypeScript definitions

**Changes**:
- [x] Added types and typings fields to PackageJson struct
- [x] Added TypeScript info to ResolvedPackage (types field and has_types_package flag)
- [x] Implemented resolve_types_path() method to find .d.ts files
- [x] Added check_types_package_exists() to detect @types packages
- [x] Added resolve_package_types() for full TypeScript resolution
- [x] Fixed CommonJS import handling for single named imports
- [x] Updated all tests to include new TypeScript fields

**Files modified**:
- `src/package_resolver.rs` - Added TypeScript detection and resolution methods
- Fixed generate_import_statement to handle single named imports from CommonJS modules

---

## Phase 3: Build Target Integration (Week 3)

### Task 3.1: Wire Up Target Configuration
**Status**: ✅ Completed  
**Priority**: MEDIUM  
**Description**: Connect build targets to module resolution and transpilation

**Changes**:
- [x] Added TargetPlatform enum (NodeJs, Browser, Deno, Bun)
- [x] Added TargetInfo struct to hold platform, module format, and external deps
- [x] Created JsTranspiler::with_target() method for target-specific transpilation
- [x] Added transpile_to_js_with_target() function in lib.rs
- [x] Updated build command to use target-aware transpilation
- [x] Added parse_target() to parse target strings (node-esm, node-cjs, browser, etc.)
- [x] Platform-specific handling of Node.js built-ins (skip for browser)
- [x] Support for external dependencies configuration
- [x] Module format selection based on target (ESM vs CommonJS)

**Files modified**:
- `src/transpiler.rs` - Added target support and platform-specific logic
- `src/lib.rs` - Added transpile_to_js_with_target function
- `src/main.rs` - Updated build command to use target
- `src/package_resolver.rs` - Use select_entry_point method consistently

### Task 3.2: Platform-Specific Imports
**Status**: ✅ Completed  
**Priority**: MEDIUM  
**Description**: Implement platform-specific module resolution and imports

**Changes**:
- [x] Added browser field to PackageJson struct
- [x] Added target_platform field to PackageResolver
- [x] Implemented PackageResolver::with_target() constructor
- [x] Added browser field resolution in select_entry_point()
- [x] Created resolve_browser_mapping() for subpath browser field mappings
- [x] Implemented generate_browser_polyfill_import() for browser polyfills
- [x] Implemented generate_deno_builtin_import() for Deno node: prefix
- [x] Updated transpiler to use platform-specific import generation

**Files modified**:
- `src/package_resolver.rs` - Added browser field support and platform-aware resolution
- `src/transpiler.rs` - Added platform-specific import generation methods  

### Task 3.3: External Dependencies
**Status**: ✅ Completed  
**Priority**: LOW  
**Description**: Ensure external dependencies are not resolved through node_modules

**Changes**:
- [x] External dependency check already implemented in visit_use method
- [x] Packages listed in targets.{target}.external bypass package resolution
- [x] External packages use basic import generation
- [x] Comprehensive unit tests for external dependencies
- [x] Tested with different targets and configurations

**Files modified**:
- `src/transpiler.rs` - External dependency check at lines 1845-1851
- `src/transpiler_external_deps_test.rs` - New comprehensive test suite  

---

## Phase 4: Advanced Features (Week 4)

### Task 4.1: Import Maps Support
**Status**: ✅ Completed  
**Priority**: LOW  
**Description**: Implement import maps (Web Standard) for controlling module specifier resolution

**Changes**:
- [x] Added `import_map: HashMap<String, String>` field to `TargetConfig` in config.rs
- [x] Added `import_map` field to `TargetInfo` struct in transpiler.rs
- [x] Updated `parse_target_with_config` to extract import_map from configuration
- [x] Implemented import map resolution logic in `visit_use` method
- [x] Added `generate_import_map_import` method to handle mapped imports
- [x] Created comprehensive test suite for import maps functionality
- [x] Modified semantic analyzer to handle packages that might be resolved by import maps
- [x] Tested with real projects and multiple targets (browser, deno)

**Files modified**:
- `src/config.rs` - Added import_map field to TargetConfig
- `src/transpiler.rs` - Added import map resolution logic and generate_import_map_import method
- `src/semantic.rs` - Made semantic analyzer more lenient for import map packages
- `src/transpiler_import_maps_test.rs` - Comprehensive test suite
- Multiple test files updated to include import_map field

### Task 4.2: Tree Shaking Hints
**Status**: ✅ Completed  
**Priority**: LOW  
**Description**: Add pure function annotations for dead code elimination by bundlers

**Changes**:
- [x] Added `tree_shaking: bool` field to `TargetConfig` and `TargetInfo`
- [x] Implemented `should_add_pure_annotation` method with extensive list of pure functions
- [x] Added pure annotation support to `visit_function_call` method
- [x] Added tree shaking support to `visit_method_call` for method calls like `Math.max()`
- [x] Modified package.json generation to include `sideEffects: false` when tree shaking is enabled
- [x] Created comprehensive test suite for tree shaking functionality
- [x] Updated all test files with new `tree_shaking` field

**Files modified**:
- `src/transpiler.rs` - Added tree shaking logic and pure function annotation generation
- `src/config.rs` - Added tree_shaking field to TargetConfig and package.json generation
- `src/transpiler_tree_shaking_test.rs` - New comprehensive test suite with 8 tests
- Multiple test files updated to include tree_shaking field

### Task 4.3: Development vs Production Mode
**Status**: ⏳ Not Started  
**Priority**: LOW  

---

## Testing Progress

### Unit Tests
- [ ] Package resolution tests
- [ ] Import generation tests
- [ ] Module format detection tests

### Integration Tests
- [ ] Express.js package test
- [ ] Lodash package test
- [ ] Axios package test
- [ ] React package test
- [ ] Complex monorepo test

---

## Commits

### Phase 1 Commits
- [x] feat: make package.json generation default behavior
- [x] fix: correct import path generation for npm packages
- [x] feat: add consistent module format support
- [x] fix: improve package resolution and exports handling

---

## Notes

- Each task should be implemented, tested, and committed separately
- Run full test suite after each change
- Update this document after completing each task
- Create git commits with descriptive messages

---

Last Updated: 2024-12-27 (Task 4.2 completed)

## Implementation Details

### Task 1.1 Implementation Notes
- Renamed the CLI flag from `--generate-package-json` to `--skip-package-json`
- Package.json is now generated by default unless explicitly skipped
- Smart detection compares existing package.json dependencies to avoid unnecessary regeneration
- Shows helpful reminder to run `npm install` when node_modules doesn't exist
- All integration tests updated and passing

### Task 1.2 Implementation Notes
- Fixed visit_use() in transpiler.rs to properly distinguish between import names and subpaths
- For `use package::item;`, the second segment is the import name, not a subpath
- For `use package::subpath::item;`, middle segments are subpath, last is import name
- Fixed generate_import_statement() to handle default imports correctly
- Removed semicolons from import statement generation (transpiler adds them)
- Import paths now correctly generate as "express" instead of "express/express"

### Task 1.3 Implementation Notes
- Added ModuleFormat enum to track whether to use ESM or CommonJS
- Transpiler determines module format from husk.toml build.module setting
- Updated generate_import_statement() to take use_esm parameter
- All imports now consistently use the project's module format
- Fixed Node.js built-in modules (fs, path, readline) to use import statements when in ESM mode
- Note: CommonJS packages in ESM mode may require special handling (Task 1.4)

### Task 1.4 Implementation Notes
- Added resolve_exports_entry() method to properly handle package.json exports field
- Supports conditional exports (default, node, require, import)
- Improved module type detection - packages with exports field are often ESM
- Added special handling for CommonJS packages with named imports in ESM mode:
  - Generates `import __pkg from "lodash"; const { debounce } = __pkg;`
  - This avoids the "Named export not found" error for CommonJS modules
- Fixed generate_import_statement to use package name when importing "default"
- All package resolver tests passing
- Successfully tested with lodash (CommonJS) in ESM project

### Task 2.1 Implementation Notes
- Enhanced resolve_exports_entry() to handle nested conditional exports objects
- Added support for wildcard patterns using simple string matching and replacement
- Conditional exports resolution order: import → require → default → node
- Added resolve_package_subpath() public method for resolving subpath imports
- Integrated with transpiler to resolve subpaths through exports field when available
- Falls back to direct subpath if exports resolution fails
- Added comprehensive test for exports field resolution
- Successfully tested with axios subpath imports (./lib/adapters/http)

### Task 2.2 Implementation Notes
- Created detect_module_type() method with 8-step detection process:
  1. Explicit "type" field in package.json (highest priority)
  2. File extension detection (.mjs → ESM, .cjs → CommonJS)
  3. Dual package handling based on build target
  4. Exports field analysis for import/require conditions
  5. File content analysis with improved pattern matching
  6. Modern packages with only "module" field → ESM
  7. Packages with exports field → ESM (modern convention)
  8. Default to CommonJS for backward compatibility
- Added UMD detection through content analysis
- Improved entry point selection for dual packages
- Added comprehensive tests for all detection scenarios
- Better handling of edge cases and modern package patterns

### Task 2.3 Implementation Notes
- Added types and typings fields to PackageJson struct for TypeScript definitions
- Extended ResolvedPackage with types field and has_types_package boolean
- Implemented resolve_types_path() to check for .d.ts files in multiple locations:
  - Explicit types/typings field in package.json
  - Alongside main file (e.g., index.js → index.d.ts)
  - Root index.d.ts
  - types/index.d.ts directory
- Added check_types_package_exists() to detect @types packages
- Handles scoped packages correctly (@org/pkg → @types/org__pkg)
- Fixed bug in generate_import_statement where single named imports from CommonJS modules weren't handled correctly
- Removed the imports.len() > 1 condition to ensure all named imports get the CommonJS workaround
- All package resolver tests updated and passing

### Task 3.1 Implementation Notes
- Created TargetPlatform enum with NodeJs, Browser, Deno, and Bun variants
- Added TargetInfo struct containing platform, module_format, and external_deps
- Implemented JsTranspiler::with_target() that parses target strings like "node-esm", "browser"
- Added transpile_to_js_with_target() public function for target-aware transpilation
- Platform-specific features:
  - Browser target skips Node.js built-in modules (fs, path, readline)
  - Node.js built-in imports return comment "// Skipped Node.js built-in: {module}"
  - Module format automatically set based on target (CommonJS for node-cjs, ESM for others)
- External dependencies support:
  - Packages listed in targets.{target}.external are not resolved through node_modules
  - External packages use basic import generation without package resolution
- Updated build command to pass target through to transpiler
- select_entry_point method now used consistently in package resolver

### Task 3.2 Implementation Notes
- Browser field support added to PackageJson and used in module resolution
- Platform-specific import generation for Node.js built-ins:
  - Browser: Maps to polyfill packages (e.g., buffer → buffer, crypto → crypto-browserify)
  - Deno: Uses node: prefix (e.g., fs → node:fs)
  - Node.js/Bun: Standard imports
- Browser field resolution supports:
  - String value: Direct browser entry point
  - Object value: Path mappings and exclusions (false = exclude module)
- resolve_browser_mapping() handles subpath mappings for browser builds
- PackageResolver now takes optional target_platform for platform-aware resolution
- Successfully tested with different targets:
  - Browser: Uses polyfills for available modules, skips unsupported ones
  - Deno: Uses node: prefix for all Node.js built-ins
  - Node.js: Standard module imports

### Task 3.3 Implementation Notes
- External dependencies feature was already partially implemented in Task 3.1
- The check happens in visit_use() at the package resolution stage
- If a package name is in target_info.external_deps list, it bypasses package resolution
- External packages use generate_basic_import() which creates simple imports without node_modules resolution
- Added comprehensive test suite covering:
  - External packages with different targets
  - Named imports from external packages
  - Non-external packages (for comparison)
  - Empty external dependencies list
  - Different platforms (Browser, Node.js)
- External dependencies are useful for:
  - CDN-loaded libraries in browser builds
  - Peer dependencies that shouldn't be bundled
  - Platform-specific modules that are provided by the runtime

### Task 4.1 Implementation Notes
- Import maps follow the Web Standard for controlling module specifier resolution
- Added import_map field as HashMap<String, String> to support key-value mappings
- Import map resolution happens before package resolution but after external dependencies check
- Order of resolution in transpiler: Node.js built-ins → External deps → Import maps → Package resolver
- Semantic analyzer was made more lenient to allow packages that might be resolved by import maps
- Import maps work with all import styles: basic imports, named imports, wildcard imports, and aliases
- Different targets can have different import maps (e.g., browser uses CDN URLs, Deno uses deno.land URLs)
- Import maps are particularly useful for:
  - Mapping packages to CDN URLs for browser builds
  - Using Deno-specific module URLs for Deno builds  
  - Overriding specific package versions or implementations
  - Supporting ESM-only environments with custom module resolution
- Configuration format: `[targets.{target}.import_map]` in husk.toml
- Successfully tested with React, Lodash, Axios (browser) and Express, Oak (Deno)

### Task 4.2 Implementation Notes
- Tree shaking hints use `/*#__PURE__*/` comments to mark function calls as pure (no side effects)
- Pure annotations are only added when tree_shaking is enabled for the target in configuration
- Extensive list of known pure functions includes:
  - Math functions: abs, max, min, floor, ceil, round, sqrt, pow, sin, cos, tan, log, random
  - Object/Array creation: Object.create, Object.assign, Object.keys, Object.values, Object.entries, Array.from, Array.of
  - Type conversion: Number, String, Boolean, parseInt, parseFloat
  - JSON methods: JSON.parse, JSON.stringify  
  - Husk utilities: __format__, __husk_safe_call, huskSafeCall
  - Constructor calls (capital first letter) are considered pure by default
- Both function calls and method calls get tree shaking support
- Package.json generation includes `sideEffects: false` when any target has tree shaking enabled
- Comprehensive test suite covers:
  - Pure annotations enabled/disabled
  - Different function types (Math, Object, JSON, constructors, Husk utilities)
  - Non-pure functions that shouldn't get annotations
  - Behavior when no target info is available
- Tree shaking integration at target level allows different optimization settings per build target