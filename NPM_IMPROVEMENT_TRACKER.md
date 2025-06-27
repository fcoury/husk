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
**Status**: ⏳ Not Started  
**Priority**: LOW  

---

## Phase 3: Build Target Integration (Week 3)

### Task 3.1: Wire Up Target Configuration
**Status**: ⏳ Not Started  
**Priority**: MEDIUM  

### Task 3.2: Platform-Specific Imports
**Status**: ⏳ Not Started  
**Priority**: MEDIUM  

### Task 3.3: External Dependencies
**Status**: ⏳ Not Started  
**Priority**: LOW  

---

## Phase 4: Advanced Features (Week 4)

### Task 4.1: Import Maps Support
**Status**: ⏳ Not Started  
**Priority**: LOW  

### Task 4.2: Tree Shaking Hints
**Status**: ⏳ Not Started  
**Priority**: LOW  

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

Last Updated: 2024-12-27 (Task 2.2 completed)

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