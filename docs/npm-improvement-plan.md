# NPM Support Improvement Plan for Husk

## Current State Analysis

### What's Working
- ✅ Dependency declaration in `husk.toml`
- ✅ Package resolution from `node_modules/`
- ✅ Basic import statement generation
- ✅ Package.json generation from `husk.toml`
- ✅ Semantic analysis with package awareness

### Critical Issues to Fix

#### 1. **Mixed Import/Require Statements** (HIGH PRIORITY)
**Problem**: Generated JavaScript mixes `require()` and `import` statements, causing module loading errors.
```javascript
// Current problematic output:
const express = require("express/express");
import { axios } from "axios/axios";  // Can't mix with require
```

#### 2. **Incorrect Import Paths** (HIGH PRIORITY)
**Problem**: Import paths like `"express/express"` and `"axios/axios"` are incorrect.
```javascript
// Current: import { axios } from "axios/axios";
// Should be: import axios from "axios";
```

#### 3. **Module Format Detection** (MEDIUM PRIORITY)
**Problem**: Need better detection of package module types (ESM vs CommonJS).

#### 4. **Subpath Exports** (MEDIUM PRIORITY)
**Problem**: Modern packages use complex export maps that need proper resolution.

#### 5. **Build Target Support** (LOW PRIORITY)
**Problem**: The `--target` flag doesn't affect import generation.

## Implementation Plan

### Phase 1: Fix Critical Import Issues (Week 1)

#### Task 1.1: Make package.json Generation Default
**File**: `src/main.rs`
```rust
// Change the build command to generate package.json by default
struct Build {
    #[clap(long)]
    target: Option<String>,
    
    #[clap(long)]
    watch: bool,
    
    // Change from opt-in to opt-out
    #[clap(long)]
    skip_package_json: bool,  // NEW: renamed from generate_package_json
}
```

**Changes needed**:
1. Rename flag from `--generate-package-json` to `--skip-package-json`
2. Generate package.json by default unless skipped
3. Add smart detection to avoid regenerating if unchanged
4. Show informative message when package.json is created/updated

**Rationale**:
- Most JavaScript projects need package.json
- Reduces friction for new users
- Ensures npm install works out of the box
- Aligns with JavaScript ecosystem expectations

**Implementation Details**:
```rust
// In build_command()
fn build_command(cli: Build, _no_color: bool) -> anyhow::Result<()> {
    // ... existing code ...
    
    // Generate package.json by default (unless skipped)
    if !cli.skip_package_json {
        // Check if package.json exists and if it needs updating
        let should_generate = if let Ok(existing) = fs::read_to_string(&package_json_path) {
            let existing_json: serde_json::Value = serde_json::from_str(&existing)?;
            let new_json: serde_json::Value = serde_json::from_str(&package_json)?;
            
            // Only regenerate if dependencies changed
            existing_json.get("dependencies") != new_json.get("dependencies") ||
            existing_json.get("devDependencies") != new_json.get("devDependencies")
        } else {
            true // No existing package.json
        };
        
        if should_generate {
            fs::write(&package_json_path, package_json)?;
            println!("✓ Generated package.json");
            
            // Remind user to run npm install if needed
            if !Path::new("node_modules").exists() {
                println!("  → Run 'npm install' to install dependencies");
            }
        }
    }
}
```

#### Task 1.2: Fix Import Path Generation
**File**: `src/transpiler.rs`
```rust
// Fix the visit_use() method to generate correct import paths
// Current: "express/express" → Fixed: "express"
```

**Changes needed**:
1. Remove duplicate package names in import paths
2. Handle default exports correctly
3. Fix named import syntax

#### Task 1.3: Consistent Module Format
**File**: `src/transpiler.rs`
```rust
// Add module_format field to JsTranspiler
struct JsTranspiler {
    indent_level: usize,
    package_resolver: Option<PackageResolver>,
    method_registry: TranspilerMethodRegistry,
    module_format: ModuleFormat, // NEW
}
```

**Changes needed**:
1. Add configuration for output module format
2. Use consistent import/require based on target
3. Default to ESM for `type: "module"` packages

#### Task 1.4: Fix Package Resolution
**File**: `src/package_resolver.rs`
```rust
// Improve package.json parsing and export resolution
fn resolve_package_exports(&self, package_json: &PackageJson) -> PackageExports {
    // Handle "exports" field properly
    // Support conditional exports
    // Resolve main entry correctly
}
```

### Phase 2: Enhanced Package Resolution (Week 2)

#### Task 2.1: Implement Exports Field Resolution
**Files**: `src/package_resolver.rs`
```rust
// Support modern package.json exports field
enum ExportCondition {
    Import,    // ES modules
    Require,   // CommonJS
    Node,      // Node.js environment
    Browser,   // Browser environment
    Default,   // Fallback
}
```

#### Task 2.2: Better Module Type Detection
**File**: `src/package_resolver.rs`
```rust
impl PackageResolver {
    fn detect_module_type(&self, package_json: &PackageJson) -> ModuleType {
        // Check "type" field
        // Check file extensions (.mjs, .cjs)
        // Check "exports" field
        // Analyze actual file content if needed
    }
}
```

#### Task 2.3: TypeScript Definitions Support
**File**: `src/package_resolver.rs`
```rust
// Add support for @types packages
fn resolve_type_definitions(&self, package_name: &str) -> Option<TypeInfo> {
    // Check for @types/package
    // Parse .d.ts files for better type information
}
```

### Phase 3: Build Target Integration (Week 3)

#### Task 3.1: Wire Up Target Configuration
**Files**: `src/transpiler.rs`, `src/lib.rs`
```rust
// Pass build configuration to transpiler
pub fn transpile_to_js_with_config(
    code: &str, 
    config: &BuildConfig
) -> Result<String> {
    // Use config.target and config.module
}
```

#### Task 3.2: Platform-Specific Imports
**File**: `src/transpiler.rs`
```rust
enum ImportStrategy {
    NodeCommonJS,   // require()
    NodeESM,        // import
    Browser,        // import with bundler hints
    Deno,           // import with URLs
}
```

#### Task 3.3: External Dependencies
**File**: `src/transpiler.rs`
```rust
// Handle external dependencies per target
fn should_bundle_dependency(&self, package: &str, target: &Target) -> bool {
    // Check target.external list
    // Handle Node.js built-ins
}
```

### Phase 4: Advanced Features (Week 4)

#### Task 4.1: Import Maps Support
```rust
// Generate import maps for browsers
fn generate_import_map(&self) -> ImportMap {
    // Map package names to CDN URLs
    // Support local overrides
}
```

#### Task 4.2: Tree Shaking Hints
```rust
// Mark imports for tree shaking
fn mark_side_effect_free(&self, import: &Import) -> bool {
    // Check package.json sideEffects field
    // Analyze import usage
}
```

#### Task 4.3: Development vs Production Mode
```rust
// Different strategies for dev/prod
enum BuildMode {
    Development,  // Fast builds, source maps
    Production,   // Optimized, minified
}
```

## Testing Strategy

### Unit Tests
1. **Package Resolution Tests**
   - Test various package.json formats
   - Test export field resolution
   - Test module type detection

2. **Import Generation Tests**
   - Test ESM imports
   - Test CommonJS requires
   - Test mixed formats

3. **Integration Tests**
   - Real npm packages (express, lodash, axios)
   - Complex packages (React, Vue)
   - Monorepo packages

### Test Cases
```rust
#[test]
fn test_simple_default_import() {
    // use express::express;
    // → import express from "express";
}

#[test]
fn test_named_imports() {
    // use lodash::{debounce, throttle};
    // → import { debounce, throttle } from "lodash";
}

#[test]
fn test_commonjs_package() {
    // use old_package::func;
    // → const { func } = require("old_package");
}
```

## Migration Path

### For Existing Projects
1. Run `husk build --fix-imports` to update import statements
2. Regenerate package.json with new format
3. Test with both Node.js and bundlers

### Breaking Changes
- None expected - improvements should be backward compatible

## Success Metrics

1. **All test cases pass**: Including real-world npm packages
2. **Generated code runs**: Both in Node.js and browsers
3. **Performance**: No regression in build times
4. **Compatibility**: Works with major bundlers (webpack, vite, etc.)

## Implementation Order

1. **Week 1**: Fix critical import issues (Tasks 1.1-1.3)
   - Priority: Make basic npm packages work correctly
   - Goal: express, lodash, axios working
   - **Task 1.4**: Make package.json generation default

2. **Week 2**: Enhanced package resolution (Tasks 2.1-2.3)
   - Priority: Support modern npm packages
   - Goal: React, Vue, and complex packages working

3. **Week 3**: Build target integration (Tasks 3.1-3.3)
   - Priority: Platform-specific builds
   - Goal: Optimized output for each target

4. **Week 4**: Advanced features (Tasks 4.1-4.3)
   - Priority: Developer experience
   - Goal: Fast builds, better optimization

## Code Examples

### Fixed Import Generation
```rust
// In transpiler.rs
fn generate_import_statement(&self, use_stmt: &UseStmt) -> String {
    match &self.module_format {
        ModuleFormat::ESM => {
            match &use_stmt.items {
                UseItems::Single => {
                    format!("import {} from \"{}\";", 
                        use_stmt.alias.unwrap_or(package_name),
                        package_name)
                },
                UseItems::Named(items) => {
                    format!("import {{ {} }} from \"{}\";", 
                        items.join(", "), 
                        package_name)
                },
                _ => // wildcard imports
            }
        },
        ModuleFormat::CommonJS => {
            match &use_stmt.items {
                UseItems::Single => {
                    format!("const {} = require(\"{}\");", 
                        use_stmt.alias.unwrap_or(package_name),
                        package_name)
                },
                UseItems::Named(items) => {
                    format!("const {{ {} }} = require(\"{}\");", 
                        items.join(", "), 
                        package_name)
                },
                _ => // wildcard requires
            }
        }
    }
}
```

### Package.json Exports Resolution
```rust
// In package_resolver.rs
fn resolve_exports(&self, exports: &Value, conditions: &[String]) -> Option<String> {
    match exports {
        Value::String(path) => Some(path.clone()),
        Value::Object(map) => {
            // Check conditions in order
            for condition in conditions {
                if let Some(path) = map.get(condition) {
                    return self.resolve_exports(path, conditions);
                }
            }
            // Fall back to default
            map.get("default")
                .and_then(|v| self.resolve_exports(v, conditions))
        },
        _ => None
    }
}
```

## Deliverables

1. **Fixed transpiler** with correct import generation
2. **Enhanced package resolver** with modern npm support
3. **Comprehensive test suite** for npm packages
4. **Updated documentation** with examples
5. **Migration guide** for existing projects

This plan addresses all critical issues and provides a path to robust npm package support in Husk.