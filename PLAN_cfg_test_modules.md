# Plan: Support `#[cfg(test)] mod test { ... }` with Multiple Tests

## Overview

Add support for defining test functions inside a module annotated with `#[cfg(test)]`, following Rust's idiomatic test pattern:

### Key Design Decisions

1. **Module Flattening**: Inline modules are flattened to JavaScript top-level (not nested JS objects)
2. **Automatic Name Prefixing**: All items in modules get prefixed with sanitized module path (e.g., `test__helper`)
3. **Collision Detection**: Build-time validation ensures no name collisions after prefixing
4. **Recursive Cfg Filtering**: Cfg predicates evaluated at each nesting level independently
5. **MVP Scoping**: Cross-module calls (`other_mod::func()`) deferred; only same-module and top-level calls supported initially

These decisions prioritize correctness and simplicity for the MVP, with a clear path to full module system in future iterations.

### Example

```rust
#[cfg(test)]
mod test {
    use super::*;
    
    #[test]
    fn test_one() { ... }
    
    #[test]
    fn test_two() { ... }
}
```

This is more ergonomic than the current requirement of annotating each test function individually:

```rust
#[cfg(test)]
#[test]
fn test_one() { ... }

#[cfg(test)]
#[test]
fn test_two() { ... }
```

## Current Implementation Analysis

### 1. **AST Structure** (`husk-ast/src/lib.rs`)
- `File` contains `Vec<Item>`
- `Item` has:
  - `attributes: Vec<Attribute>` - can include `#[cfg(test)]`, `#[test]`, etc.
  - `kind: ItemKind` - function, struct, enum, trait, impl, extern, use, type alias
- **Current limitation**: No `Mod` variant in `ItemKind` for inline modules

### 2. **Cfg Filtering** (`husk-semantic/src/lib.rs`)
- `filter_items_by_cfg(file, flags)` - filters top-level items only (line 228)
- `item_passes_cfg(item, flags)` - checks if single item passes cfg (line 217)
- `evaluate_cfg(predicate, flags)` - evaluates CfgPredicate against flags (line 201)
- **Current limitation**: Only filters top-level items, doesn't recurse into nested structures

### 3. **Test Discovery** (`husk-lang/src/main.rs`)
- `discover_tests(file)` - finds test functions (line 1515)
- Iterates `file.items` looking for items with `#[test]` attribute
- **Current limitation**: Only looks at top-level items, doesn't recurse into modules

### 4. **Parser** (`husk-parser/src/lib.rs`)
- Parses various item types (fn, struct, enum, extern, use, trait, impl)
- **Current limitation**: No support for inline `mod name { ... }` syntax

## Implementation Plan

### Phase 1: AST & Core Parsing

#### 1.1. Extend AST (`husk-ast/src/lib.rs`)

**Add `Mod` variant to `ItemKind` enum:**

```rust
pub enum ItemKind {
    // ... existing variants ...
    
    /// Inline module: `mod name { items }`
    /// Used for organizing code and cfg-conditional compilation
    Mod {
        name: Ident,
        items: Vec<Item>,
    },
}
```

**Critical: Audit all subsystems for non-exhaustive ItemKind matches**

Adding a new variant to `ItemKind` requires updating every location that matches on it. The following subsystems MUST be audited and updated:

1. **husk-semantic/src/lib.rs**:
   - `Resolver::collect()` - Add `Mod` case to register module as a symbol if needed
   - `TypeChecker::build_type_env()` - Add `Mod` case to recursively process nested items
   - `TypeChecker::check_file()` - Add `Mod` case to recursively check nested items  
   - Symbol registration - Register module scope and track which items it exports
   - Cfg filtering - Already covered in Phase 2.1, but ensure recursion is complete
   
2. **husk-lang/src/load.rs**:
   - Any import resolution logic - Ensure inline modules are recognized
   - Module filtering/assembly - Apply cfg predicates to inline modules
   - Dependency tracking - If modules can have external deps in future
   
3. **husk-fmt/src/visitor.rs**:
   - The Rust compiler will catch missing patterns in exhaustive matches
   - Add `ItemKind::Mod` case with proper indentation and brace formatting
   - Preserve trivia (comments) around module declarations
   
4. **husk-lsp/src/**:
   - Document symbols - Include module items in symbol tree (fold/outline view)
   - Symbol navigation - "Go to definition" should work across module boundaries
   - Hover info - Show module name and contained symbols
   - Range/location mapping - Ensure spans include full module body
   
5. **husk-codegen-js/src/lib.rs**:
   - `lower_file_to_module()` or equivalent - Add `Mod` case for flattening (Phase 3)
   - Ensure correct behavior for both ESM and CJS targets
   - Handle any special cases for main/lib mode
   
6. **husk-parser/src/lib.rs**:
   - Already covered in Phase 1.2
   - Ensure synchronization/error recovery handles modules
   
**Verification checklist**:
- [ ] Run `cargo check` and fix any non-exhaustive match errors
- [ ] Search codebase for `ItemKind::` to find all match sites
- [ ] Add unit tests for each subsystem's module handling
- [ ] Add integration test that exercises all code paths

**Add helper methods to `Item`:**

```rust
impl Item {
    /// Returns true if this is a module item
    pub fn is_mod(&self) -> bool {
        matches!(self.kind, ItemKind::Mod { .. })
    }
    
    /// If this is a module, returns its name
    pub fn mod_name(&self) -> Option<&str> {
        match &self.kind {
            ItemKind::Mod { name, .. } => Some(&name.name),
            _ => None,
        }
    }
    
    /// If this is a module, returns its items
    pub fn mod_items(&self) -> Option<&[Item]> {
        match &self.kind {
            ItemKind::Mod { items, .. } => Some(items),
            _ => None,
        }
    }
}
```

**Update `SetFilePath` trait implementation:**

Add case for `Mod` in the `SetFilePath` implementation for `Item` to recursively set file paths on nested module items.

In `husk-ast/src/lib.rs`, locate the `impl SetFilePath for Item` block (around line 1100-1200) and add the `Mod` case:

```rust
impl SetFilePath for Item {
    fn set_file_path(&mut self, file: Arc<str>) {
        self.span.set_file_path(file.clone());
        for attr in &mut self.attributes {
            attr.span.set_file_path(file.clone());
            attr.name.set_file_path(file.clone());
        }
        match &mut self.kind {
            // ... existing cases for Fn, Struct, Enum, etc ...
            
            ItemKind::Mod { name, items } => {
                name.set_file_path(file.clone());
                for item in items {
                    item.set_file_path(file.clone());
                }
            }
            
            // ... rest of existing cases ...
        }
    }
}
```

This ensures that when loading multi-file projects, all nested module items correctly track their source file for error reporting.

#### 1.2. Extend Parser (`husk-parser/src/lib.rs`)

**Add module parsing:**

Locate the `parse_item()` function and add support for parsing `mod name { items }`:

```rust
// In parse_item() function, add case for "mod" keyword:

if self.check_keyword("mod") {
    return self.parse_mod_item(attributes, visibility);
}

// Add new function:
fn parse_mod_item(&mut self, attributes: Vec<Attribute>, visibility: Visibility) -> Result<Item, ParseError> {
    let start = self.current_pos();
    self.expect_keyword("mod")?;
    
    let name = self.parse_ident()?;
    
    self.expect(TokenKind::LBrace)?;
    
    let mut items = Vec::new();
    while !self.check(TokenKind::RBrace) && !self.is_at_end() {
        match self.parse_item() {
            Ok(item) => items.push(item),
            Err(err) => {
                self.errors.push(err);
                self.synchronize();
            }
        }
    }
    
    let end = self.expect(TokenKind::RBrace)?;
    
    Ok(Item {
        attributes,
        visibility,
        kind: ItemKind::Mod { name, items },
        span: Span::new(start, end),
    })
}
```

**Add tests:**

```rust
#[cfg(test)]
#[test]
fn parse_empty_module() {
    let src = "mod test {}";
    let result = parse_str(src);
    assert!(result.errors.is_empty());
    let file = result.file.unwrap();
    assert_eq!(file.items.len(), 1);
    assert!(file.items[0].is_mod());
}

#[cfg(test)]
#[test]
fn parse_module_with_functions() {
    let src = r#"
mod test {
    fn helper() -> i32 { 42 }
    fn another() { }
}
"#;
    let result = parse_str(src);
    assert!(result.errors.is_empty());
    let file = result.file.unwrap();
    assert_eq!(file.items.len(), 1);
    let mod_items = file.items[0].mod_items().unwrap();
    assert_eq!(mod_items.len(), 2);
}

#[cfg(test)]
#[test]
fn parse_cfg_test_module() {
    let src = r#"
#[cfg(test)]
mod test {
    #[test]
    fn test_one() { }
    
    #[test]
    fn test_two() { }
}
"#;
    let result = parse_str(src);
    assert!(result.errors.is_empty());
    let file = result.file.unwrap();
    assert_eq!(file.items.len(), 1);
    
    let item = &file.items[0];
    assert!(item.is_mod());
    assert!(item.cfg_predicate().is_some());
    
    let mod_items = item.mod_items().unwrap();
    assert_eq!(mod_items.len(), 2);
    assert!(mod_items[0].is_test());
    assert!(mod_items[1].is_test());
}
```

### Phase 2: Semantic Analysis

#### 2.1. Update Cfg Filtering (`husk-semantic/src/lib.rs`)

**Make `filter_items_by_cfg` recursive:**

```rust
pub fn filter_items_by_cfg(file: &File, flags: &HashSet<String>) -> File {
    File {
        items: file
            .items
            .iter()
            .filter_map(|item| filter_item_by_cfg(item, flags))
            .collect(),
    }
}

/// Filter a single item by cfg, recursively filtering module contents
fn filter_item_by_cfg(item: &Item, flags: &HashSet<String>) -> Option<Item> {
    // First check if this item itself passes cfg
    if !item_passes_cfg(item, flags) {
        return None;
    }
    
    // If it's a module, recursively filter its items
    if let ItemKind::Mod { name, items } = &item.kind {
        let filtered_items: Vec<Item> = items
            .iter()
            .filter_map(|nested_item| filter_item_by_cfg(nested_item, flags))
            .collect();
        
        // Return the module with filtered items
        Some(Item {
            attributes: item.attributes.clone(),
            visibility: item.visibility.clone(),
            kind: ItemKind::Mod {
                name: name.clone(),
                items: filtered_items,
            },
            span: item.span.clone(),
        })
    } else {
        // Not a module, return as-is
        Some(item.clone())
    }
}
```

**Add tests:**

```rust
#[cfg(test)]
#[test]
fn filter_cfg_test_module_included_in_test_mode() {
    let src = r#"
#[cfg(test)]
mod test {
    fn helper() { }
    
    #[test]
    fn test_one() { }
}
"#;
    let parsed = parse_str(src);
    let file = parsed.file.unwrap();
    
    // With test flag
    let mut flags = HashSet::new();
    flags.insert("test".to_string());
    let filtered = filter_items_by_cfg(&file, &flags);
    assert_eq!(filtered.items.len(), 1);
    let mod_items = filtered.items[0].mod_items().unwrap();
    assert_eq!(mod_items.len(), 2); // both helper and test_one
}

#[cfg(test)]
#[test]
fn filter_cfg_test_module_excluded_in_normal_mode() {
    let src = r#"
#[cfg(test)]
mod test {
    #[test]
    fn test_one() { }
}

fn main() { }
"#;
    let parsed = parse_str(src);
    let file = parsed.file.unwrap();
    
    // Without test flag
    let flags = HashSet::new();
    let filtered = filter_items_by_cfg(&file, &flags);
    assert_eq!(filtered.items.len(), 1); // only main
    assert!(!filtered.items[0].is_mod());
}

#[cfg(test)]
#[test]
fn filter_nested_cfg_in_module() {
    let src = r#"
mod outer {
    fn always_included() { }
    
    #[cfg(test)]
    fn test_only() { }
}
"#;
    let parsed = parse_str(src);
    let file = parsed.file.unwrap();
    
    // Without test flag
    let flags = HashSet::new();
    let filtered = filter_items_by_cfg(&file, &flags);
    let mod_items = filtered.items[0].mod_items().unwrap();
    assert_eq!(mod_items.len(), 1); // only always_included
    
    // With test flag
    let mut flags = HashSet::new();
    flags.insert("test".to_string());
    let filtered = filter_items_by_cfg(&file, &flags);
    let mod_items = filtered.items[0].mod_items().unwrap();
    assert_eq!(mod_items.len(), 2); // both functions
}
```

#### 2.2. Update Name Resolution for Module-Scoped Calls

When flattening modules during codegen, function calls need to be rewritten to use prefixed names. This requires updating the name resolution in semantic analysis:

**In `husk-semantic/src/lib.rs`:**

```rust
/// Context for tracking module scope during name resolution
struct NameResolutionContext {
    /// Current module path (e.g., ["outer", "inner"])
    module_path: Vec<String>,
    /// Map of (original_name, module_path) -> prefixed_name
    name_mapping: HashMap<(String, Vec<String>), String>,
}

impl NameResolutionContext {
    /// Generate prefixed name for a function in current module
    fn prefixed_name(&self, name: &str) -> String {
        if self.module_path.is_empty() {
            name.to_string()
        } else {
            format!("{}__{}",self.module_path.join("__"), name)
        }
    }
}
```

During type checking, when processing function calls inside modules:
1. Track the current module path
2. When a function is defined, register its prefixed name
3. When a function is called, resolve it to the prefixed name if in same module

This ensures calls like `helper()` inside `mod test` get rewritten to `test__helper()` in the generated JS.

**Note**: Full implementation of qualified paths like `other_module::function()` can be deferred to future work. For the MVP, functions can only call other functions within the same module or at top level.

#### 2.3. Update Test Discovery (`husk-lang/src/main.rs`)

**Make `discover_tests` recursive:**

```rust
fn discover_tests(file: &husk_ast::File) -> Vec<(String, bool, Option<String>)> {
    let mut tests = Vec::new();
    discover_tests_recursive(&file.items, &mut tests, None);
    tests
}

/// Recursively discover test functions in items, with optional module path prefix
fn discover_tests_recursive(
    items: &[husk_ast::Item],
    tests: &mut Vec<(String, bool, Option<String>)>,
    module_path: Option<&str>,
) {
    for item in items {
        match &item.kind {
            husk_ast::ItemKind::Fn { name, .. } if item.is_test() => {
                let test_name = if let Some(prefix) = module_path {
                    format!("{}::{}", prefix, name.name)
                } else {
                    name.name.clone()
                };
                let is_ignored = item.is_ignored();
                let expected_panic = item.expected_panic_message().map(|s| s.to_string());
                tests.push((test_name, is_ignored, expected_panic));
            }
            husk_ast::ItemKind::Mod { name, items } => {
                // Recurse into module
                let new_path = if let Some(prefix) = module_path {
                    format!("{}::{}", prefix, name.name)
                } else {
                    name.name.clone()
                };
                discover_tests_recursive(items, tests, Some(&new_path));
            }
            _ => {}
        }
    }
}
```

**Add tests:**

```rust
#[cfg(test)]
#[test]
fn discover_tests_in_module() {
    let src = r#"
#[cfg(test)]
mod test {
    #[test]
    fn test_one() { }
    
    #[test]
    fn test_two() { }
}
"#;
    let parsed = parse_str(src);
    let file = parsed.file.unwrap();
    
    let mut flags = HashSet::new();
    flags.insert("test".to_string());
    let filtered = husk_semantic::filter_items_by_cfg(&file, &flags);
    
    let tests = discover_tests(&filtered);
    assert_eq!(tests.len(), 2);
    assert!(tests.iter().any(|(name, _, _)| name == "test::test_one"));
    assert!(tests.iter().any(|(name, _, _)| name == "test::test_two"));
}

#[cfg(test)]
#[test]
fn discover_tests_in_nested_modules() {
    let src = r#"
mod outer {
    mod inner {
        #[test]
        fn test_nested() { }
    }
}
"#;
    let parsed = parse_str(src);
    let file = parsed.file.unwrap();
    
    let tests = discover_tests(&file);
    assert_eq!(tests.len(), 1);
    assert_eq!(tests[0].0, "outer::inner::test_nested");
}
```

### Phase 3: Codegen

#### 3.1. Update JS Codegen (`husk-codegen-js/src/lib.rs`)

**Handle Mod items by flattening with automatic name prefixing:**

We flatten module items to JavaScript top-level, using **automatic prefixing** to guarantee no name collisions.

**Name Collision Strategy (CANONICAL APPROACH):**

All functions and items inside modules are prefixed with a sanitized module path when flattened to JavaScript:

- Module path `foo::bar::baz` → sanitized to `foo__bar__baz__`
- Function `helper` in `test::utils` → JS name `test__utils__helper`
- Top-level functions (no module) → no prefix, keep original name

**Implementation:**

```rust
// In lower_file_to_module or similar function:

/// Track module path during recursive lowering
struct CodegenContext {
    module_path: Vec<String>,
    // ... other context ...
}

fn lower_items(
    items: &[Item], 
    module: &mut JsModule,
    context: &mut CodegenContext,
    /* ... */
) {
    for item in items {
        match &item.kind {
            ItemKind::Mod { name, items: nested_items } => {
                // Push module name onto path
                context.module_path.push(name.name.clone());
                
                // Recursively lower nested items with updated path
                lower_items(nested_items, module, context, /* ... */);
                
                // Pop module name after processing
                context.module_path.pop();
            }
            ItemKind::Fn { name, .. } => {
                // Generate prefixed name if inside a module
                let js_name = if context.module_path.is_empty() {
                    name.name.clone()
                } else {
                    format!(
                        "{}__{}",
                        context.module_path.join("__"),
                        name.name
                    )
                };
                
                // Lower function with prefixed name
                lower_function(item, js_name, module, context, /* ... */);
            }
            // ... other item kinds (struct, enum, etc.) get similar treatment ...
        }
    }
}

/// Sanitize module path component (replace invalid JS identifier chars)
fn sanitize_identifier(name: &str) -> String {
    name.chars()
        .map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' })
        .collect()
}
```

**Uniqueness validation:**

Add a runtime check during codegen to catch any remaining collisions and fail the build:

```rust
fn validate_no_collisions(module: &JsModule) -> Result<(), CodegenError> {
    let mut seen_names = HashSet::new();
    for item in &module.items {
        let name = item.name();
        if !seen_names.insert(name.to_string()) {
            return Err(CodegenError::NameCollision {
                name: name.to_string(),
                message: format!(
                    "Name collision after flattening modules: '{}' appears multiple times. \
                     This should not happen with automatic prefixing - please report as a bug.",
                    name
                ),
            });
        }
    }
    Ok(())
}
```

**Example transformations:**

```rust
// Input Husk:
#[cfg(test)]
mod tests {
    fn helper() -> i32 { 42 }
    
    #[test]
    fn test_something() {
        assert(helper() == 42);
    }
}

// Output JS (CJS):
function tests__helper() {
    return 42;
}

function tests__test_something() {
    const __husk_assert_1 = (tests__helper() === 42);
    if (!__husk_assert_1) throw new Error("Husk assertion failed");
}
```

```rust
// Input Husk with nested modules:
mod outer {
    mod inner {
        fn utility() { }
    }
    
    fn use_utility() {
        inner::utility();
    }
}

// Output JS:
function outer__inner__utility() { }

function outer__use_utility() {
    outer__inner__utility();
}
```

**Scope analysis note (optional future optimization):**

Helper functions used only within a module could be exempted from prefixing if we implement proper scope analysis. For now, we prefix everything for simplicity and correctness. An allowlist approach could be:

```rust
struct ScopeAnalysis {
    // Functions only called within their own module
    internal_only: HashSet<String>,
}

// Then in codegen:
if context.module_path.is_empty() || scope.internal_only.contains(&name.name) {
    // No prefix for top-level or internal-only functions
} else {
    // Apply prefix
}
```

This optimization can be added later without breaking changes.

**Add tests:**

```rust
#[cfg(test)]
#[test]
fn codegen_flattens_module_items_with_prefixing() {
    let src = r#"
mod test {
    fn helper() -> i32 { 42 }
    
    fn use_helper() -> i32 { helper() }
}
"#;
    let parsed = parse_str(src);
    let file = parsed.file.unwrap();
    let js = lower_file_to_js(&file, false, JsTarget::Cjs, &Default::default(), &Default::default(), &Default::default(), &Default::default());
    
    // Both functions should be at top level in JS with module prefix
    let src = js.to_source();
    assert!(src.contains("function test__helper()"));
    assert!(src.contains("function test__use_helper()"));
    // Calls should also use prefixed names
    assert!(src.contains("test__helper()"));
}

#[cfg(test)]
#[test]
fn codegen_top_level_functions_not_prefixed() {
    let src = r#"
fn top_level() -> i32 { 42 }

mod test {
    fn in_module() -> i32 { 10 }
}
"#;
    let parsed = parse_str(src);
    let file = parsed.file.unwrap();
    let js = lower_file_to_js(&file, false, JsTarget::Cjs, &Default::default(), &Default::default(), &Default::default(), &Default::default());
    
    let src = js.to_source();
    // Top-level function has no prefix
    assert!(src.contains("function top_level()"));
    // Module function has prefix
    assert!(src.contains("function test__in_module()"));
}

#[cfg(test)]
#[test]
fn codegen_nested_modules_use_full_path_prefix() {
    let src = r#"
mod outer {
    mod inner {
        fn deeply_nested() { }
    }
}
"#;
    let parsed = parse_str(src);
    let file = parsed.file.unwrap();
    let js = lower_file_to_js(&file, false, JsTarget::Cjs, &Default::default(), &Default::default(), &Default::default(), &Default::default());
    
    let src = js.to_source();
    assert!(src.contains("function outer__inner__deeply_nested()"));
}

#[cfg(test)]
#[test]
fn codegen_detects_name_collisions() {
    // This should not happen with proper prefixing, but test the validation
    let src = r#"
fn test__helper() { }  // Manually uses prefixed name

mod test {
    fn helper() { }  // Will become test__helper
}
"#;
    let parsed = parse_str(src);
    let file = parsed.file.unwrap();
    
    // This should either:
    // 1. Error during codegen due to collision detection, OR
    // 2. Succeed but emit warning about suspicious naming
    let result = std::panic::catch_unwind(|| {
        lower_file_to_js(&file, false, JsTarget::Cjs, &Default::default(), &Default::default(), &Default::default(), &Default::default())
    });
    
    // For now, just document this edge case - we may want to add validation
    // that prevents users from using names with double underscores
}

#[cfg(test)]
#[test]
fn codegen_test_module_with_cfg() {
    let src = r#"
#[cfg(test)]
mod tests {
    #[test]
    fn test_something() {
        assert(true);
    }
}
"#;
    let parsed = parse_str(src);
    let file = parsed.file.unwrap();
    
    // With test flag - module should be included
    let mut flags = HashSet::new();
    flags.insert("test".to_string());
    let filtered = filter_items_by_cfg(&file, &flags);
    let js = lower_file_to_js(&filtered, false, JsTarget::Cjs, &Default::default(), &Default::default(), &Default::default(), &Default::default());
    
    let src = js.to_source();
    assert!(src.contains("function tests__test_something()"));
    
    // Without test flag - module should be excluded
    let flags = HashSet::new();
    let filtered = filter_items_by_cfg(&file, &flags);
    let js = lower_file_to_js(&filtered, false, JsTarget::Cjs, &Default::default(), &Default::default(), &Default::default(), &Default::default());
    
    let src = js.to_source();
    assert!(!src.contains("test_something"));
}
```

### Phase 4: Formatter

#### 4.1. Update Formatter (`husk-fmt/src/lib.rs` and `husk-fmt/src/visitor.rs`)

**Add module formatting:**

```rust
// In visitor or formatting logic, add case for Mod:
fn format_item(item: &Item, indent: usize) -> String {
    // ... format attributes ...
    
    match &item.kind {
        ItemKind::Mod { name, items } => {
            let mut result = String::new();
            result.push_str(&format!("{}mod {} {{\n", " ".repeat(indent), name.name));
            
            for (i, nested_item) in items.iter().enumerate() {
                if i > 0 {
                    result.push('\n');
                }
                result.push_str(&format_item(nested_item, indent + 4));
            }
            
            result.push_str(&format!("{}}}\n", " ".repeat(indent)));
            result
        }
        // ... other item kinds ...
    }
}
```

**Add tests:**

```rust
#[cfg(test)]
#[test]
fn format_module() {
    let src = "mod test{fn foo(){}}";
    let expected = r#"mod test {
    fn foo() {}
}
"#;
    let formatted = format_str(src);
    assert_eq!(formatted, expected);
}
```

### Phase 5: LSP Support (Optional, can be deferred)

#### 5.1. Module Scoping

For now, since we're flattening modules during codegen, LSP support can treat module items as if they're at the top level. Future work could add proper module scoping for:
- "Go to definition" across module boundaries
- Proper scoping resolution for `use super::*`
- Hover info showing module path

### Phase 6: Integration Tests

Create comprehensive end-to-end tests in `examples/` or test suite:

**Example: `examples/test_modules/main.hk`:**

```rust
fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn multiply(a: i32, b: i32) -> i32 {
    a * b
}

#[cfg(test)]
mod tests {
    use super::*;  // Future: import parent scope
    
    // For now, functions from parent are in scope
    
    #[test]
    fn test_add() {
        assert(add(2, 3) == 5);
        assert(add(0, 0) == 0);
    }
    
    #[test]
    fn test_multiply() {
        assert(multiply(2, 3) == 6);
        assert(multiply(0, 5) == 0);
    }
    
    #[test]
    #[ignore]
    fn test_ignored() {
        assert(false);
    }
}

#[cfg(test)]
mod more_tests {
    #[test]
    fn test_another() {
        assert(true);
    }
}

fn main() {
    println("add(2, 3) = {}", add(2, 3));
}
```

**Run and verify:**

```bash
huskc test examples/test_modules/main.hk
# Should run: tests::test_add, tests::test_multiply, tests::test_another
# Should ignore: tests::test_ignored

huskc build examples/test_modules/main.hk
# Should only include add, multiply, main (no test modules)
```

## Implementation Order

1. **AST Extension** - Add `Mod` variant to `ItemKind`, update `SetFilePath` implementation
2. **Audit all ItemKind matches** - Search codebase for all pattern matches, add `Mod` cases
3. **Parser** - Add module parsing support with comprehensive tests
4. **Cfg Filtering** - Make recursive with tests for nested modules
5. **Name Resolution** - Track module context for function name prefixing
6. **Test Discovery** - Make recursive with tests, output full module path  
7. **Codegen** - Implement flattening with automatic name prefixing
8. **Codegen Validation** - Add collision detection and build-time checks
9. **Formatter** - Add module formatting with tests
10. **LSP Updates** - Add module symbols to document outline/navigation
11. **Integration Tests** - End-to-end examples with assertions on prefixed names
12. **Documentation** - Update syntax.md, add module examples

## Notes on `use super::*`

For the initial implementation, we can defer full `use super::*` support. Test functions can reference parent scope functions directly since we're flattening during codegen. Full module scoping with imports can be added in a future iteration.

## Compatibility

This change is **backward compatible**:
- Existing code with `#[cfg(test)]` on individual functions continues to work
- New code can use module syntax
- Both styles can coexist in the same file

## Edge Cases to Handle

1. **Empty modules**: `mod test {}` should parse but generate no code
2. **Nested modules**: `mod outer { mod inner { } }` should work recursively with full path prefixes
3. **Mixed cfg**: Module with `#[cfg(test)]` containing items with `#[cfg(debug)]` - both predicates evaluated independently
4. **Non-test modules**: Regular modules (without cfg) should also work for code organization
5. **Name collisions**: Prevented by automatic prefixing; manual use of `__` in names may still cause issues (document)
6. **Module visibility**: For now, module items inherit parent visibility rules
7. **User-defined prefixed names**: Names like `test__helper` at top level may collide with generated names (add lint warning)
8. **Cross-module calls**: For MVP, only same-module and top-level calls work; `other_mod::func()` deferred to future

## Future Enhancements

1. **Full module system**: Proper scoping with `use`, `pub(crate)`, etc.
2. **Nested namespaces**: Generate nested JS objects for module structure
3. **Module-level attributes**: Support more than just `#[cfg(...)]`
4. **File-based modules**: `mod name;` loading from `name.hk` file
5. **Use statements**: Full support for `use super::*`, `use crate::*`, etc.

## Success Criteria

- [ ] Can parse `mod name { items }` syntax
- [ ] Can apply `#[cfg(test)]` to modules
- [ ] All ItemKind match sites updated (verified with `cargo check`)
- [ ] SetFilePath recursively updates nested module items
- [ ] Cfg filtering works recursively for nested modules
- [ ] Test functions inside modules are discovered by `huskc test`
- [ ] Test modules are excluded from `huskc build`
- [ ] Test output shows module path (e.g., `test::test_one`)
- [ ] Generated JS uses prefixed names (e.g., `test__helper`)
- [ ] Function calls within modules resolve to prefixed names
- [ ] Collision detection catches duplicate names at build time
- [ ] Formatter handles module syntax with proper indentation
- [ ] LSP includes modules in document symbols/outline
- [ ] All existing tests pass
- [ ] New unit tests pass for parser, semantic, codegen
- [ ] Integration tests verify prefixed output
- [ ] Documentation updated with module examples

## Estimated Effort

- AST Extension & SetFilePath: 1 hour
- Audit & Update All ItemKind Matches: 2-3 hours
- Parser Implementation: 2-3 hours
- Semantic Analysis (cfg + name resolution): 3-4 hours  
- Codegen (flattening + prefixing + validation): 3-4 hours
- Formatter: 1-2 hours
- LSP Updates: 1-2 hours
- Unit Tests (parser, semantic, codegen): 3-4 hours
- Integration Tests: 2-3 hours
- Documentation: 1-2 hours

**Total: ~20-28 hours** for complete implementation with comprehensive tests, validation, and documentation.

Note: This is higher than initial estimate due to:
- Comprehensive subsystem audit requirements
- Name prefixing and collision detection implementation
- Additional test coverage for all edge cases
- LSP integration work
