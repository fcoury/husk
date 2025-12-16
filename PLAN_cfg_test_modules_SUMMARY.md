# Implementation Summary: `#[cfg(test)] mod test { ... }` Support

## Quick Reference

This document summarizes the key aspects of the full implementation plan. See `PLAN_cfg_test_modules.md` for complete details.

## Core Changes Required

### 1. AST (`husk-ast/src/lib.rs`)
- Add `ItemKind::Mod { name: Ident, items: Vec<Item> }` variant
- Update `SetFilePath for Item` to recursively process module items
- Add helper methods: `is_mod()`, `mod_name()`, `mod_items()`

### 2. Parser (`husk-parser/src/lib.rs`)
- Implement `parse_mod_item()` to parse `mod name { items }` syntax
- Add error recovery for module parsing

### 3. Semantic Analysis (`husk-semantic/src/lib.rs`)
- Make `filter_items_by_cfg()` recursive to filter nested module items
- Update `Resolver::collect()` to handle Mod items
- Update `TypeChecker::build_type_env()` and `check_file()` for modules
- Track module context for name prefixing during resolution

### 4. Test Discovery (`husk-lang/src/main.rs`)
- Make `discover_tests()` recursive
- Output full module path (e.g., `tests::test_one`)

### 5. Codegen (`husk-codegen-js/src/lib.rs`)
- Flatten module items to JS top-level
- **Automatic name prefixing**: `module__path__function`
- Add collision detection with build-time error
- Track module path context during lowering

### 6. Formatter (`husk-fmt/src/visitor.rs`)
- Add formatting for `Mod` items with proper indentation

### 7. LSP (`husk-lsp/src/`)
- Include modules in document symbols
- Support navigation to module items

## Name Prefixing Strategy (CANONICAL)

**Rule**: All items inside modules are prefixed with sanitized module path.

### Examples

| Husk Source | JavaScript Output |
|------------|-------------------|
| `mod test { fn helper() }` | `function test__helper()` |
| `fn top_level()` | `function top_level()` (no prefix) |
| `mod outer { mod inner { fn deep() } }` | `function outer__inner__deep()` |

### Sanitization
- Module path joined with `__`
- Invalid JS chars replaced with `_`
- Top-level items: no prefix

### Collision Detection
```rust
// Build fails if duplicate names exist after prefixing:
fn test__helper() { }  // User-defined

mod test {
    fn helper() { }  // Becomes test__helper - COLLISION!
}
```

## Subsystems Requiring Updates

All locations matching on `ItemKind` must add `Mod` case:

1. ✅ `husk-semantic/src/lib.rs` - Resolver, TypeChecker, cfg filtering
2. ✅ `husk-lang/src/load.rs` - Module loading/filtering
3. ✅ `husk-fmt/src/visitor.rs` - Formatting (compiler enforced)
4. ✅ `husk-lsp/src/` - Document symbols, navigation
5. ✅ `husk-codegen-js/src/lib.rs` - Flattening with prefixing
6. ✅ `husk-parser/src/lib.rs` - Parsing

**Verification**: Run `cargo check` - compiler catches non-exhaustive matches.

## Test Coverage Required

### Parser Tests
- Empty module: `mod test {}`
- Module with functions
- Module with attributes: `#[cfg(test)] mod test {}`
- Nested modules
- Error recovery

### Semantic Tests
- Cfg filtering includes test modules with `test` flag
- Cfg filtering excludes test modules without flag
- Recursive filtering of nested items
- Mixed cfg predicates at different levels

### Codegen Tests
- Functions in modules get prefixed
- Top-level functions not prefixed
- Nested modules use full path prefix
- Function calls within modules use prefixed names
- Collision detection triggers build error
- Test modules excluded in normal build
- Test modules included with test flag

### Integration Tests
- Full example with `#[cfg(test)] mod test { ... }`
- `huskc test` discovers and runs module tests
- `huskc build` excludes test modules
- Output shows module path in test names

## MVP Limitations (Deferred to Future)

1. **Cross-module calls**: Can't call `other_mod::function()` yet
2. **`use super::*`**: Not implemented; functions access parent scope directly
3. **Module visibility**: No `pub(crate)` or fine-grained visibility yet
4. **File-based modules**: `mod name;` loading from file not supported
5. **Namespace objects**: JS output is flattened, not nested objects

## Implementation Checklist

- [ ] Add `Mod` to `ItemKind` enum
- [ ] Update `SetFilePath for Item`
- [ ] Search codebase for all `ItemKind::` matches, add `Mod` cases
- [ ] Implement parser for modules
- [ ] Make cfg filtering recursive
- [ ] Add name prefixing context to semantic analysis
- [ ] Make test discovery recursive
- [ ] Implement codegen flattening with prefixing
- [ ] Add collision detection
- [ ] Update formatter
- [ ] Update LSP
- [ ] Write comprehensive tests for each subsystem
- [ ] Add integration tests
- [ ] Update documentation

## Estimated Timeline

**Total: 20-28 hours** for complete, production-ready implementation.

Key phases:
- Core implementation (AST, parser, semantic): 8-10 hours
- Codegen with prefixing: 3-4 hours
- Testing (unit + integration): 5-7 hours
- Tooling (formatter, LSP): 2-4 hours
- Documentation: 1-2 hours

## Success Metrics

✅ `cargo check` passes (all matches exhaustive)  
✅ Can parse `#[cfg(test)] mod test { #[test] fn foo() {} }`  
✅ `huskc test` discovers module tests  
✅ Test output shows `module::test_name`  
✅ Generated JS has `module__function` names  
✅ Build fails on name collisions  
✅ All existing tests pass  
✅ New tests pass  

## References

- Full plan: `PLAN_cfg_test_modules.md`
- Example usage: See Phase 6 in full plan
- Rust modules: https://doc.rust-lang.org/book/ch07-00-managing-growing-projects-with-packages-crates-and-modules.html
