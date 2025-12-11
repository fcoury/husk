# Plan: Support `#[cfg(test)] mod test { ... }` with Multiple Tests

## Overview

Add support for defining test functions inside a module annotated with `#[cfg(test)]`, following Rust's idiomatic test pattern:

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

#### 2.2. Update Test Discovery (`husk-lang/src/main.rs`)

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

**Handle Mod items by flattening them:**

In the codegen, when encountering a `Mod` item, we have two approaches:

**Option A: Flatten all module items to top level**

```rust
// In lower_file_to_module or similar function:
fn lower_items(items: &[Item], module: &mut JsModule, /* ... */) {
    for item in items {
        match &item.kind {
            ItemKind::Mod { items: nested_items, .. } => {
                // Recursively lower nested items as if they were at top level
                lower_items(nested_items, module, /* ... */);
            }
            ItemKind::Fn { .. } => {
                // lower function
            }
            // ... other item kinds ...
        }
    }
}
```

**Option B: Generate JavaScript module/namespace (more complex, maybe future work)**

For now, Option A (flattening) is simpler and sufficient for test modules.

**Note about name collisions:**

When flattening, we need to ensure no name collisions. For test functions, this is typically not an issue since:
1. Test functions are usually uniquely named
2. Helper functions in test modules are only used within tests
3. We could optionally prefix names with module path to avoid collisions

**Add tests:**

```rust
#[cfg(test)]
#[test]
fn codegen_flattens_module_items() {
    let src = r#"
mod test {
    fn helper() -> i32 { 42 }
    
    fn use_helper() -> i32 { helper() }
}
"#;
    let parsed = parse_str(src);
    let file = parsed.file.unwrap();
    let js = lower_file_to_js(&file, false, JsTarget::Cjs, &Default::default(), &Default::default(), &Default::default(), &Default::default());
    
    // Both functions should be at top level in JS
    let src = js.to_source();
    assert!(src.contains("function helper()"));
    assert!(src.contains("function use_helper()"));
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

1. **AST Extension** - Add `Mod` variant to `ItemKind`
2. **Parser** - Add module parsing support with tests
3. **Cfg Filtering** - Make recursive with tests
4. **Test Discovery** - Make recursive with tests  
5. **Codegen** - Flatten module items with tests
6. **Formatter** - Add module formatting with tests
7. **Integration Tests** - End-to-end example
8. **Documentation** - Update syntax.md and examples

## Notes on `use super::*`

For the initial implementation, we can defer full `use super::*` support. Test functions can reference parent scope functions directly since we're flattening during codegen. Full module scoping with imports can be added in a future iteration.

## Compatibility

This change is **backward compatible**:
- Existing code with `#[cfg(test)]` on individual functions continues to work
- New code can use module syntax
- Both styles can coexist in the same file

## Edge Cases to Handle

1. **Empty modules**: `mod test {}` should parse but generate no code
2. **Nested modules**: `mod outer { mod inner { } }` should work recursively
3. **Mixed cfg**: Module with `#[cfg(test)]` containing items with `#[cfg(debug)]`
4. **Non-test modules**: Regular modules (without cfg) should also work for code organization
5. **Name collisions**: When flattening, ensure unique names or document limitations
6. **Module visibility**: For now, module items inherit parent visibility rules

## Future Enhancements

1. **Full module system**: Proper scoping with `use`, `pub(crate)`, etc.
2. **Nested namespaces**: Generate nested JS objects for module structure
3. **Module-level attributes**: Support more than just `#[cfg(...)]`
4. **File-based modules**: `mod name;` loading from `name.hk` file
5. **Use statements**: Full support for `use super::*`, `use crate::*`, etc.

## Success Criteria

- [ ] Can parse `mod name { items }` syntax
- [ ] Can apply `#[cfg(test)]` to modules
- [ ] Test functions inside modules are discovered by `huskc test`
- [ ] Test modules are excluded from `huskc build`
- [ ] Test output shows module path (e.g., `test::test_one`)
- [ ] Formatter handles module syntax
- [ ] All existing tests pass
- [ ] New integration tests pass
- [ ] Documentation updated

## Estimated Effort

- AST & Parser: 2-3 hours
- Semantic Analysis: 2-3 hours  
- Codegen: 1-2 hours
- Formatter: 1 hour
- Testing & Integration: 2-3 hours
- Documentation: 1 hour

**Total: ~10-13 hours** for complete implementation with tests and documentation.
