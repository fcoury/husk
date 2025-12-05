# Plan: Move Hardcoded Methods to Husk Stdlib

## Status: ✅ COMPLETED

All phases have been implemented and all tests pass.

## Executive Summary

This plan outlines how to move the hardcoded string/array/number method definitions from `husk-semantic/src/lib.rs` to a Husk stdlib written in Husk itself, making the language more extensible and self-hosting.

## Current State Analysis

### Where Methods Are Hardcoded

**File**: `crates/husk-semantic/src/lib.rs` (lines 1822-1858)

```rust
Type::Primitive(PrimitiveType::String) => {
    match method_name.as_str() {
        "split" => return Type::Array(...),
        "trim" => return Type::Primitive(PrimitiveType::String),
        "len" => return Type::Primitive(PrimitiveType::I32),
        "charAt" | "char_at" => return Type::Primitive(PrimitiveType::String),
        "slice" => return Type::Primitive(PrimitiveType::String),
        "substring" => return Type::Primitive(PrimitiveType::String),
        "indexOf" | "lastIndexOf" => return Type::Primitive(PrimitiveType::I32),
        "startsWith" | "endsWith" | "includes" => return Type::Primitive(PrimitiveType::Bool),
        _ => {}
    }
}
```

Similar hardcoded methods exist for:
- **i32/f64**: `toString()`
- **Array**: `len()`, `push()`, `slice()`, `some()`, `every()`, `filter()`, `map()`, `reduce()`, `sort()`, `reverse()`, `join()`

### Existing Infrastructure

1. **Trait System**: Already works for `From<T>`, `Into<T>`, `TryFrom<T>`, `TryInto<T>`, `PartialEq`, `Eq`
2. **Stdlib Loading**: `core.hk` is embedded and loaded at compile time
3. **Attribute System**: `#[getter]`, `#[setter]`, `#[js_name]`, `#[cfg]`, `#[test]` already work
4. **Impl Block Resolution**: User-defined impl blocks are already resolved (lines 1880-1919)
5. **Extern Functions**: `extern "js"` declarations work for free functions and methods

### The Gap

**Why can't we just write `impl String { fn trim(&self) -> String { ... } }` in stdlib?**

1. **Primitives aren't defined in Husk**: `String`, `i32`, `i64`, `f64`, `bool` are built-in `Type::Primitive(...)` variants in the Rust compiler - they don't exist as Husk `struct` or `type` declarations.

2. **Impl lookup checks `self_ty_name`**: The impl block resolution (line 1892) matches against a string type name, but primitives don't have a "name" in the same way user types do.

3. **Method call resolution has priority**: Built-in methods are checked BEFORE impl blocks (line 1822), so even if we add impl blocks, they'd be shadowed.

---

## Proposed Solution: Inherent Impl Blocks for Primitives

The simplest approach: allow `impl` blocks directly on primitive type names without any special attributes. The compiler already knows what primitives are - we just need to teach it to accept impl blocks for them.

### Phase 1: Inherent Impls for Primitive Types

#### 1.1 Stdlib Syntax in `stdlib/core.hk`

```husk
// String methods - these declare the method signatures.
// All methods use snake_case (Husk convention).
// Codegen automatically converts to camelCase for JavaScript.
impl String {
    #[js_name = "length"]  // Special: property access, not method
    extern "js" fn len(&self) -> i32;

    extern "js" fn trim(&self) -> String;
    extern "js" fn split(&self, separator: String) -> [String];
    extern "js" fn char_at(&self, index: i32) -> String;      // -> charAt
    extern "js" fn slice(&self, start: i32, end: i32) -> String;
    extern "js" fn substring(&self, start: i32, end: i32) -> String;
    extern "js" fn index_of(&self, search: String) -> i32;    // -> indexOf
    extern "js" fn last_index_of(&self, search: String) -> i32; // -> lastIndexOf
    extern "js" fn starts_with(&self, prefix: String) -> bool;  // -> startsWith
    extern "js" fn ends_with(&self, suffix: String) -> bool;    // -> endsWith
    extern "js" fn includes(&self, search: String) -> bool;
    extern "js" fn to_upper_case(&self) -> String;            // -> toUpperCase
    extern "js" fn to_lower_case(&self) -> String;            // -> toLowerCase
}

impl i32 {
    extern "js" fn to_string(&self) -> String;  // -> toString
}

impl i64 {
    extern "js" fn to_string(&self) -> String;  // -> toString
}

impl f64 {
    extern "js" fn to_string(&self) -> String;  // -> toString
}

impl bool {
    extern "js" fn to_string(&self) -> String;  // -> toString
}
```

This is clean, familiar syntax - it looks just like regular impl blocks but for primitives. All names follow Rust/Husk snake_case conventions, and codegen handles the JavaScript camelCase conversion automatically.

#### 1.2 Why This Works

- **No special attributes to abuse**: Users can't accidentally break stdlib with `#[lang]` misuse
- **Extensible**: Users can add their own methods to primitives if desired (extension methods)
- **Familiar**: Same syntax as regular impl blocks
- **Simple**: Minimal compiler changes needed

### Phase 2: Compiler Changes

#### 2.1 Primitive Type Name Recognition

**In `husk-semantic/src/lib.rs`:**

Add a function to map primitive types to their canonical names:

```rust
fn primitive_type_name(ty: &Type) -> Option<&'static str> {
    match ty {
        Type::Primitive(PrimitiveType::String) => Some("String"),
        Type::Primitive(PrimitiveType::I32) => Some("i32"),
        Type::Primitive(PrimitiveType::I64) => Some("i64"),
        Type::Primitive(PrimitiveType::F64) => Some("f64"),
        Type::Primitive(PrimitiveType::Bool) => Some("bool"),
        Type::Primitive(PrimitiveType::Unit) => Some("()"),
        _ => None,
    }
}
```

#### 2.2 Modify Impl Block Processing

**In `build_type_env`** (around line 693):

When processing `impl String { ... }`, recognize that `String` refers to the primitive and register the impl block with `self_ty_name = "String"`.

Currently the code does:
```rust
let self_ty_name = type_expr_to_name(&impl_block.self_ty);
```

This already works - `type_expr_to_name` on a `TypeExpr::Path` with name "String" returns "String". No change needed here.

#### 2.3 Modify Method Resolution

**In `check_expr` for MethodCall** (lines 1880-1903):

Change the receiver type name extraction:

```rust
// BEFORE (line 1881-1884):
let receiver_type_name = match &receiver_ty {
    Type::Named { name, .. } => Some(name.clone()),
    _ => None,
};

// AFTER:
let receiver_type_name = match &receiver_ty {
    Type::Named { name, .. } => Some(name.clone()),
    Type::Primitive(p) => Some(primitive_type_name_str(p).to_string()),
    Type::Array(_) => Some("Array".to_string()),
    _ => None,
};
```

Where `primitive_type_name_str` is:
```rust
fn primitive_type_name_str(p: &PrimitiveType) -> &'static str {
    match p {
        PrimitiveType::String => "String",
        PrimitiveType::I32 => "i32",
        PrimitiveType::I64 => "i64",
        PrimitiveType::F64 => "f64",
        PrimitiveType::Bool => "bool",
        PrimitiveType::Unit => "()",
    }
}
```

#### 2.4 Remove Hardcoded Method Checks

Once impl-based resolution works, remove the hardcoded match statements (lines 1822-1858). The method lookup in impl blocks (lines 1886-1903) will handle everything.

### Phase 3: Generic Array Methods

Arrays need special handling because they're parameterized.

#### 3.1 Syntax

```husk
impl<T> [T] {
    extern "js" fn len(&self) -> i32;
    extern "js" fn push(&mut self, value: T);
    extern "js" fn slice(&self, start: i32, end: i32) -> [T];
    extern "js" fn join(&self, separator: String) -> String;
    extern "js" fn sort(&mut self) -> [T];
    extern "js" fn reverse(&mut self) -> [T];
}
```

#### 3.2 Parser Changes

1. Allow `impl<T> [T] { ... }` syntax
2. Parse `[T]` as a valid `self_ty` in impl blocks

#### 3.3 Semantic Analysis

1. When processing `impl<T> [T]`, register with a special marker like `self_ty_name = "[T]"` or `self_ty_name = "Array"`
2. During method lookup for `Type::Array(elem_ty)`, find impl blocks for `"Array"` or `"[T]"`
3. Substitute the actual element type for `T` in return types

---

## Implementation Steps

### Step 1: Add snake_to_camel for Method Calls (Small)

1. Modify `lower_expr` for `MethodCall` to apply `snake_to_camel` to method names
2. Add `js_name()` helper to `ImplMethod` AST node for explicit overrides
3. In codegen, check for `#[js_name]` attribute before applying automatic conversion
4. Add tests for snake_case → camelCase conversion

**Files**: `crates/husk-codegen-js/src/lib.rs`, `crates/husk-ast/src/lib.rs`
**Complexity**: Low

### Step 2: Primitive Type Name Mapping (Small)

1. Add `primitive_type_name_str(p: &PrimitiveType) -> &'static str` function
2. Modify `receiver_type_name` extraction to include primitives
3. Add tests verifying primitive impl blocks are found

**Files**: `crates/husk-semantic/src/lib.rs`
**Complexity**: Low

### Step 3: Add Primitive Impls to Stdlib (Small)

1. Add `impl String { ... }` with all string methods (snake_case) to `stdlib/core.hk`
2. Add `impl i32/i64/f64/bool { fn to_string... }` blocks
3. Verify stdlib still parses with existing test

**Files**: `stdlib/core.hk`
**Complexity**: Low

### Step 4: Remove Hardcoded Method Checks (Medium)

1. Comment out the hardcoded primitive method match statements
2. Run test suite to verify impl-based resolution works
3. Delete the commented code once verified

**Files**: `crates/husk-semantic/src/lib.rs`
**Complexity**: Medium (need careful testing)

### Step 5: Array Impl Block Support (Medium-High)

1. Extend parser to allow `impl<T> [T] { ... }`
2. Register array impls with appropriate type pattern
3. Implement type parameter substitution during method resolution
4. Add array method impls to stdlib
5. Remove hardcoded array method checks

**Files**: `crates/husk-parser/src/lib.rs`, `crates/husk-semantic/src/lib.rs`, `stdlib/core.hk`
**Complexity**: Medium-High

---

## What About User-Defined Primitive Impls?

With this approach, users CAN add their own methods to primitives:

```husk
// User code
impl String {
    fn shout(&self) -> String {
        self.toUpperCase() + "!"
    }
}
```

**Is this a problem?** No - it's actually a feature! This is "extension methods" and is useful. The stdlib methods are loaded first, so they take precedence. User methods can add new functionality but can't override stdlib methods (first-match-wins in impl lookup).

---

## Codegen Considerations: Snake Case to Camel Case

### Goal: Use snake_case in Husk, camelCase in JavaScript

All stdlib methods should follow Rust/Husk naming conventions (snake_case), but emit JavaScript-standard camelCase:

```husk
// Husk code (snake_case)
let idx = text.index_of("hello");
let upper = text.to_upper_case();
```

```javascript
// Generated JavaScript (camelCase)
let idx = text.indexOf("hello");
let upper = text.toUpperCase();
```

### Current State

| Feature | snake_to_camel conversion? |
|---------|---------------------------|
| `#[getter]` properties | ✅ Yes (automatic) |
| `extern "js"` methods | ❌ No (uses name as-is) |
| `#[js_name = "..."]` | ✅ Properties only |

### Required Change: Automatic snake_to_camel for Extern Methods

**In `husk-codegen-js/src/lib.rs`**, modify method call lowering (around line 1372):

```rust
// BEFORE:
let js_method_name = strip_variadic_suffix(&method.name);

// AFTER:
let js_method_name = snake_to_camel(&strip_variadic_suffix(&method.name));
```

This applies automatic conversion for ALL method calls. Since JavaScript methods are camelCase and Husk methods are snake_case, this is the right default.

### Special Cases: `#[js_name]` for Methods

For methods that don't follow the pattern (e.g., `len` → `length`), extend `#[js_name]` support to methods:

```husk
impl String {
    #[js_name = "length"]
    extern "js" fn len(&self) -> i32;
}
```

**Implementation:**
1. Add `js_name()` method to `ImplMethod` AST node (similar to `ExternProperty`)
2. In codegen, check for `#[js_name]` before applying `snake_to_camel`

```rust
// In lower_expr for MethodCall:
let js_method_name = method
    .js_name()
    .map(|s| s.to_string())
    .unwrap_or_else(|| snake_to_camel(&strip_variadic_suffix(&method.name)));
```

### Final Stdlib Syntax

```husk
impl String {
    // Automatic: len -> len (no change needed, but see below)
    #[js_name = "length"]  // Special: len -> length (property, not method)
    extern "js" fn len(&self) -> i32;

    // Automatic: trim -> trim (same in both)
    extern "js" fn trim(&self) -> String;

    // Automatic: index_of -> indexOf
    extern "js" fn index_of(&self, search: String) -> i32;

    // Automatic: last_index_of -> lastIndexOf
    extern "js" fn last_index_of(&self, search: String) -> i32;

    // Automatic: starts_with -> startsWith
    extern "js" fn starts_with(&self, prefix: String) -> bool;

    // Automatic: ends_with -> endsWith
    extern "js" fn ends_with(&self, suffix: String) -> bool;

    // Automatic: char_at -> charAt
    extern "js" fn char_at(&self, index: i32) -> String;

    // Automatic: to_upper_case -> toUpperCase
    extern "js" fn to_upper_case(&self) -> String;

    // Automatic: to_lower_case -> toLowerCase
    extern "js" fn to_lower_case(&self) -> String;
}

impl i32 {
    // Automatic: to_string -> toString
    extern "js" fn to_string(&self) -> String;
}
```

---

## Testing Strategy

1. **Unit Tests**: Primitive type name mapping
2. **Integration Tests**:
   - `"hello".trim()` works
   - `"hello".split(",")` returns `[String]`
   - `42.toString()` works
   - `[1, 2, 3].len()` works
3. **Regression Tests**: All existing examples still compile and run
4. **Stdlib Parse Test**: Already exists in `tests/stdlib.rs`

---

## Migration Path

1. **Phase A**: Add primitive name mapping + impl resolution (keeps hardcoded fallback)
2. **Phase B**: Add stdlib impl blocks (both paths work)
3. **Phase C**: Run full test suite, verify equivalent behavior
4. **Phase D**: Remove hardcoded fallback
5. **Phase E**: Add array impl support
6. **Phase F**: Document the stdlib extensibility

---

## Summary

The key insight is that we don't need special `#[lang]` attributes. We just need to:

1. **Recognize primitive type names** when looking up impl blocks
2. **Allow impl blocks for primitives** in the stdlib (and user code)
3. **Let the existing impl resolution machinery** do the work

This is simpler, safer, and more extensible than the lang-item approach.

---

## Implementation Complete

All phases have been implemented. Here's what was done:

### Files Modified

1. **`crates/husk-ast/src/lib.rs`**
   - Added `attributes` field to `ImplMethod` struct
   - Added `js_name()` helper method to `ImplMethod`

2. **`crates/husk-parser/src/lib.rs`**
   - Updated `parse_impl_method` to pass attributes to `ImplMethod`

3. **`crates/husk-codegen-js/src/lib.rs`**
   - Added `extern_methods` set to `PropertyAccessors` to track which methods are extern
   - Added collection of `#[js_name]` overrides
   - Updated method call lowering to:
     - Apply `snake_to_camel` only for extern "js" methods (user methods keep original names)
     - Use `#[js_name]` override when present

4. **`crates/husk-semantic/src/lib.rs`**
   - Added `primitive_type_name()` helper to map primitive types to string names
   - Added `substitute_type_param()` helper for generic type parameter substitution
   - Updated method lookup to include primitive types (`impl String`, `impl i32`, etc.)
   - Updated method lookup for arrays to use `[T]` pattern matching
   - Added type parameter substitution for array method return types
   - Removed hardcoded String, i32, f64 method checks
   - Simplified hardcoded array methods to only closure-taking ones (some, every, filter, map, reduce)

5. **`stdlib/core.hk`**
   - Added `impl String { ... }` with all string methods using snake_case names
   - Added `impl i32 { fn to_string() }`, `impl i64 { fn to_string() }`, `impl f64 { ... }`, `impl bool { ... }`
   - Added `impl<T> [T] { ... }` with array methods (len, push, slice, join, sort, reverse, etc.)
   - Used `#[js_name = "length"]` for `len()` method to emit correct JS

### Key Design Decisions

1. **Snake case only for extern methods**: The `snake_to_camel` conversion only applies to `extern "js"` methods. User-defined Husk methods keep their original names to avoid breaking user code (e.g., `p.x_coord()` stays as `p.x_coord()` in JS).

2. **Generic array impl blocks**: The `impl<T> [T] { ... }` syntax is now supported. Type parameter substitution is handled specially for array types to resolve return types like `[T]` to the actual element type.

3. **Closure methods remain hardcoded**: Methods like `map`, `filter`, `reduce`, `some`, `every` remain hardcoded in semantic analysis because they require closure parameter type inference that isn't fully supported yet.

### All Tests Pass

- All unit tests pass
- All integration tests pass
- Node.js execution tests pass
