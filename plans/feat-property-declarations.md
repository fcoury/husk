# First-Class Property Support in Husk

## Problem Statement

Husk currently uses **heuristics** to determine when a method call should be compiled to a JavaScript property access. This approach has several critical issues:

1. **Unpredictable behavior**: Users must understand the heuristic rules to predict output
2. **False positives/negatives**: Methods like `open()` or `run()` need special exclusion lists
3. **Maintenance burden**: `NON_GETTER_METHODS` list grows with each edge case discovered
4. **Unclear syntax**: `req.params()` in Husk compiling to `req.params` in JS is confusing
5. **No setter support for new properties**: Only `set_*` prefix methods are recognized
6. **No snake_case → camelCase conversion**: JS conventions differ from Rust conventions

### Current Implementation

**Heuristic-based detection** in `crates/husk-codegen-js/src/lib.rs`:
- Zero-arg extern methods with return types → property getters
- Methods starting with `set_` taking one arg → property setters
- Exclusion list: `["all", "toJsValue", "open", "run", "iterate", "bind", "pluck", "raw", "columns"]`

```husk
// Current syntax (confusing)
impl Request {
    extern "js" fn body(self) -> JsValue;     // Becomes req.body
    extern "js" fn params(self) -> JsValue;   // Becomes req.params
}

// Usage looks like method call but compiles to property access
let body = req.body();  // → req.body
```

## Proposed Solution

Introduce **true property declarations** as extern fields in impl blocks. Properties use field-like syntax and natural assignment/access, not function calls.

### Syntax: Extern Property Declarations

```husk
impl Request {
    // Read-only property
    #[getter]
    extern "js" body: JsValue;

    // Read-only property
    #[getter]
    extern "js" params: JsValue;
}

impl Element {
    // Read-write property
    #[getter]
    #[setter]
    extern "js" text_content: String;  // → textContent in JS

    // Read-write with explicit JS name
    #[getter]
    #[setter]
    #[js_name = "innerHTML"]
    extern "js" inner_html: String;
}

// Usage - natural property access syntax!
let body = req.body;                    // → req.body
let params = req.params;                // → req.params
element.text_content = "hello";         // → element.textContent = "hello"
let content = element.text_content;     // → element.textContent
```

### snake_case → camelCase Conversion

**Default behavior**: All extern property names are automatically converted from snake_case to camelCase:
- `text_content` → `textContent`
- `status_code` → `statusCode`
- `last_insert_rowid` → `lastInsertRowid`

**Override with `#[js_name]`**: When the JS name doesn't follow camelCase:
```husk
#[js_name = "innerHTML"]
extern "js" inner_html: String;

#[js_name = "XMLHttpRequest"]
extern "js" xml_http_request: JsValue;
```

### Places Requiring snake_case → camelCase Conversion

1. **Extern properties**: `extern "js" text_content: String` → `.textContent`
2. **Extern functions**: `extern "js" fn get_value()` → `getValue()`
3. **Extern methods in impl blocks**: `extern "js" fn do_something()` → `doSomething()`

**NOT converted** (stays as-is):
- Struct names (PascalCase preserved)
- Type names
- Explicit `#[js_name = "..."]` overrides

## Implementation Plan

### Phase 1: Attribute Infrastructure

**Files**: `crates/husk-ast/src/lib.rs`, `crates/husk-parser/src/lib.rs`

1. Add attribute support to AST:
```rust
pub struct Attribute {
    pub name: String,
    pub value: Option<String>,  // For #[js_name = "value"]
    pub span: Span,
}
```

2. Add `ExternProperty` to AST:
```rust
pub struct ExternProperty {
    pub name: Ident,
    pub ty: TypeExpr,
    pub has_getter: bool,
    pub has_setter: bool,
    pub js_name: Option<String>,  // From #[js_name = "..."]
    pub attributes: Vec<Attribute>,
    pub span: Span,
}
```

3. Extend `ImplItem` enum:
```rust
pub enum ImplItem {
    Method(ImplMethod),
    Property(ExternProperty),  // New variant
}
```

4. Update parser to recognize attributes and extern properties in impl blocks

### Phase 2: snake_case → camelCase Conversion

**File**: `crates/husk-codegen-js/src/lib.rs`

1. Add conversion utility:
```rust
fn snake_to_camel(s: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = false;
    for c in s.chars() {
        if c == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_ascii_uppercase());
            capitalize_next = false;
        } else {
            result.push(c);
        }
    }
    result
}
```

2. Apply conversion to:
   - All extern property names (unless `#[js_name]` present)
   - All extern function names (unless `#[js_name]` present)
   - Method calls on extern types

### Phase 3: Property Access Semantics

**Files**: `crates/husk-semantic/src/lib.rs`, `crates/husk-codegen-js/src/lib.rs`

1. Track extern properties per type in semantic analysis
2. Validate property access:
   - Read access requires `#[getter]` attribute
   - Write access requires `#[setter]` attribute
3. Generate property access in codegen:
   - Read: `obj.prop_name` → `obj.propName`
   - Write: `obj.prop_name = val` → `obj.propName = val`

### Phase 4: Remove Heuristics

**File**: `crates/husk-codegen-js/src/lib.rs`

1. **Remove** `PropertyAccessors` struct
2. **Remove** `NON_GETTER_METHODS` exclusion list
3. **Remove** heuristic-based getter/setter detection
4. **Keep** explicit property declarations only

## Detailed Implementation Tasks

### Task 1: Attribute AST Support
- [ ] Add `Attribute` struct to `husk-ast`
- [ ] Add `attributes` field to relevant AST nodes
- [ ] Update AST pretty-printing

### Task 2: Attribute Parser
- [ ] Add `#` token recognition
- [ ] Implement `parse_attribute()` - handles `#[name]` and `#[name = "value"]`
- [ ] Implement `parse_attributes()` - collects multiple attributes
- [ ] Add parser tests for attributes

### Task 3: Extern Property AST & Parser
- [ ] Add `ExternProperty` struct to `husk-ast`
- [ ] Add `ImplItem::Property` variant
- [ ] Implement `parse_extern_property()` - parses `extern "js" name: Type;`
- [ ] Integrate into `parse_impl_item()` after attribute parsing
- [ ] Add parser tests for extern properties

### Task 4: snake_case → camelCase
- [ ] Add `snake_to_camel()` utility function
- [ ] Apply to extern property codegen
- [ ] Apply to extern function codegen
- [ ] Apply to method call codegen for extern types
- [ ] Add tests for conversion edge cases

### Task 5: Property Semantic Analysis
- [ ] Track extern properties per type
- [ ] Validate getter/setter attributes on property access
- [ ] Emit errors for accessing write-only or read-only properties incorrectly

### Task 6: Property Code Generation
- [ ] Generate property read: `obj.prop` → `obj.jsProp`
- [ ] Generate property write: `obj.prop = val` → `obj.jsProp = val`
- [ ] Handle `#[js_name]` overrides

### Task 7: Remove Heuristics
- [ ] Remove `NON_GETTER_METHODS` constant
- [ ] Remove `PropertyAccessors` struct
- [ ] Remove heuristic detection loop
- [ ] Update all affected code paths

### Task 8: Update Examples
- [ ] Migrate `examples/express_sqlite/main.hk`
- [ ] Convert method-based properties to true properties
- [ ] Update any snake_case names

## Example Migration

### Before (current heuristic-based)
```husk
impl Request {
    extern "js" fn body(self) -> JsValue;
    extern "js" fn params(self) -> JsValue;
}

impl RunResult {
    extern "js" fn changes(self) -> i32;
    extern "js" fn lastInsertRowid(self) -> i32;  // Must use JS name
}

// Usage (confusing - looks like method call)
let body = req.body();
let id = result.lastInsertRowid();
```

### After (true property declarations)
```husk
impl Request {
    #[getter]
    extern "js" body: JsValue;

    #[getter]
    extern "js" params: JsValue;
}

impl RunResult {
    #[getter]
    extern "js" changes: i32;

    #[getter]
    extern "js" last_insert_rowid: i32;  // Rust naming, auto-converts
}

// Usage - natural property access!
let body = req.body;
let id = result.last_insert_rowid;  // → result.lastInsertRowid in JS
```

## Edge Cases

### 1. Read-Only Properties
```husk
impl Request {
    #[getter]
    extern "js" body: JsValue;
}

// Error: cannot assign to read-only property
req.body = something;  // Compile error!
```

### 2. Write-Only Properties (rare)
```husk
impl Element {
    #[setter]
    extern "js" text_content: String;
}

// Error: cannot read write-only property
let x = element.text_content;  // Compile error!
element.text_content = "hello";  // OK
```

### 3. Read-Write Properties
```husk
impl Element {
    #[getter]
    #[setter]
    extern "js" value: String;
}

let v = element.value;      // OK - read
element.value = "new";      // OK - write
```

### 4. Properties with Non-Standard JS Names
```husk
impl Element {
    #[getter]
    #[setter]
    #[js_name = "innerHTML"]
    extern "js" inner_html: String;
}
// → element.innerHTML in JS
```

### 5. Methods vs Properties Disambiguation
```husk
impl Database {
    // This is a METHOD - has parameters, called with ()
    extern "js" fn open(path: String) -> Database;

    // This is a PROPERTY - no parameters, accessed directly
    #[getter]
    extern "js" is_open: bool;
}

let db = Database::open("test.db");  // Method call → Database.open("test.db")
let open = db.is_open;               // Property access → db.isOpen
```

## Testing Strategy

1. **Parser tests**: Verify attribute and property parsing
2. **snake_to_camel tests**: Edge cases (leading underscores, double underscores, etc.)
3. **Semantic tests**: Verify getter/setter access validation
4. **Codegen tests**: Verify correct JS output
5. **Integration tests**: Full compile with properties
6. **Migration tests**: Existing examples work with new syntax

## Success Criteria

1. Properties declared with `extern "js" name: Type` syntax
2. Natural access: `obj.prop` for read, `obj.prop = val` for write
3. Automatic snake_case → camelCase conversion
4. `#[js_name]` allows exact name override
5. `#[getter]`/`#[setter]` control read/write access
6. Heuristic-based detection removed
7. All examples migrated and working
8. Clear error messages for property access violations

## References

- [wasm-bindgen getter/setter](https://rustwasm.github.io/wasm-bindgen/reference/attributes/on-rust-exports/getter-and-setter.html)
- [wasm-bindgen js_name](https://rustwasm.github.io/wasm-bindgen/reference/attributes/on-rust-exports/js_name.html)
- [Rust naming conventions](https://rust-lang.github.io/api-guidelines/naming.html)
