# feat: Implement Closures and Function Types in Husk

## Overview

Add first-class function types and closure expressions to the Husk programming language. This enables functional programming patterns like higher-order functions, callbacks, and lambda expressions, compiling to JavaScript arrow functions.

**Current State:** Husk already has `Type::Function { params: Vec<Type>, ret: Box<Type> }` in the type system but lacks closure expression syntax and the infrastructure to use functions as values.

## Problem Statement / Motivation

Husk currently supports only named function declarations. Users cannot:
- Pass functions as arguments to other functions
- Return functions from functions
- Create inline anonymous functions (closures)
- Use common functional patterns like `map`, `filter`, `reduce`

This severely limits the expressiveness of the language and makes many common patterns impossible to implement.

**Example of what's currently impossible:**
```rust
// Cannot do this today
let doubled = numbers.iter().map(|x| x * 2).collect();

// Must write verbose named functions instead
fn double(x: i32) -> i32 { x * 2 }
```

## Proposed Solution

Implement Rust-style closure syntax with type inference and JavaScript arrow function codegen:

```rust
// Basic closure
let add = |x, y| x + y;

// With type annotations
let multiply: fn(i32, i32) -> i32 = |x: i32, y: i32| -> i32 { x * y };

// Higher-order function
fn apply(f: fn(i32) -> i32, x: i32) -> i32 {
    f(x)
}

// Closure capturing variables
let factor = 2;
let scale = |x| x * factor;

// Returning closures
fn make_adder(n: i32) -> fn(i32) -> i32 {
    |x| x + n
}
```

## Technical Approach

### Architecture

The implementation spans all compiler phases:

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   Lexer     │ -> │   Parser    │ -> │  Semantic   │ -> │   Codegen   │
│             │    │             │    │  Analysis   │    │    (JS)     │
├─────────────┤    ├─────────────┤    ├─────────────┤    ├─────────────┤
│ Tokenize |  │    │ Parse       │    │ Type check  │    │ Emit arrow  │
│ Handle ||   │    │ closure     │    │ closures    │    │ functions   │
│ for empty   │    │ expressions │    │ Capture     │    │             │
│ params      │    │             │    │ analysis    │    │             │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘
```

### Implementation Phases

#### Phase 1: AST & Parser Foundation

**Tasks:**
- [ ] Add `ExprKind::Closure` variant to AST (`crates/husk-ast/src/lib.rs`)
- [ ] Add `ClosureParam` struct for optionally-typed parameters
- [ ] Implement `parse_closure_expr()` in parser (`crates/husk-parser/src/lib.rs`)
- [ ] Handle disambiguation of `|` (closure start vs bitwise OR)
- [ ] Support both expression body (`|x| x + 1`) and block body (`|x| { x + 1 }`)
- [ ] Parse function type syntax in type positions: `fn(T, U) -> V`

**Files to modify:**
- `crates/husk-ast/src/lib.rs:63-113` - Add to `ExprKind` enum
- `crates/husk-parser/src/lib.rs` - Add closure parsing

**AST Design:**
```rust
// crates/husk-ast/src/lib.rs

/// Closure parameter with optional type annotation
pub struct ClosureParam {
    pub name: Ident,
    pub ty: Option<TypeExpr>,
    pub span: Span,
}

// Add to ExprKind enum
pub enum ExprKind {
    // ... existing variants ...

    /// Closure expression: |params| body
    Closure {
        params: Vec<ClosureParam>,
        ret_type: Option<TypeExpr>,
        body: Box<Expr>,
    },
}
```

**Parser Strategy:**
```rust
// crates/husk-parser/src/lib.rs

fn parse_closure_expr(&mut self) -> Option<Expr> {
    let start = self.current_span();

    // Consume opening |
    self.expect(TokenKind::Pipe)?;

    // Parse parameters (empty for ||)
    let params = if self.check(TokenKind::Pipe) {
        vec![]  // || case
    } else {
        self.parse_closure_params()?
    };

    // Consume closing |
    self.expect(TokenKind::Pipe)?;

    // Optional return type annotation
    let ret_type = if self.check(TokenKind::Arrow) {
        self.advance();
        Some(self.parse_type_expr()?)
    } else {
        None
    };

    // Parse body (expression or block)
    let body = if self.check(TokenKind::LBrace) {
        self.parse_block_expr()?
    } else {
        self.parse_expr()?
    };

    Some(Expr {
        kind: ExprKind::Closure { params, ret_type, body: Box::new(body) },
        span: start.to(self.previous_span()),
    })
}
```

#### Phase 2: Type System Integration

**Tasks:**
- [ ] Add closure type checking in `check_expr()` (`crates/husk-semantic/src/lib.rs`)
- [ ] Implement type inference for closure parameters from context
- [ ] Add capture analysis to detect free variables
- [ ] Validate captured variables are in scope
- [ ] Support function types in struct fields and enum variants
- [ ] Parse and validate `fn(T) -> U` type syntax

**Files to modify:**
- `crates/husk-semantic/src/lib.rs:843-858` - Extend function type handling
- `crates/husk-types/src/lib.rs` - Already has `Type::Function`

**Type Checking Strategy:**
```rust
// crates/husk-semantic/src/lib.rs

fn check_closure(
    &mut self,
    params: &[ClosureParam],
    ret_type: Option<&TypeExpr>,
    body: &Expr,
    expected_type: Option<&Type>,
) -> Type {
    // 1. Determine parameter types
    let param_types: Vec<Type> = params.iter().enumerate().map(|(i, p)| {
        if let Some(ty) = &p.ty {
            self.resolve_type(ty)
        } else if let Some(Type::Function { params: expected_params, .. }) = expected_type {
            expected_params.get(i).cloned().unwrap_or(Type::Var(self.fresh_type_var()))
        } else {
            Type::Var(self.fresh_type_var())
        }
    }).collect();

    // 2. Create scope with parameters
    self.push_scope();
    for (param, ty) in params.iter().zip(&param_types) {
        self.define_local(&param.name.name, ty.clone());
    }

    // 3. Analyze captures (free variables)
    let captures = self.analyze_captures(body);
    for (name, ty) in &captures {
        // Validate captured variables exist in enclosing scope
        if self.lookup_outer(name).is_none() {
            self.error(format!("Cannot capture undefined variable: {}", name));
        }
    }

    // 4. Type check body
    let body_type = self.check_expr(body);

    // 5. Verify return type if annotated
    if let Some(ret_ty_expr) = ret_type {
        let ret_ty = self.resolve_type(ret_ty_expr);
        self.unify(&body_type, &ret_ty);
    }

    self.pop_scope();

    Type::Function {
        params: param_types,
        ret: Box::new(body_type),
    }
}
```

**Capture Analysis:**
```rust
fn analyze_captures(&self, expr: &Expr) -> Vec<(String, Type)> {
    let mut captures = Vec::new();
    let mut visitor = CaptureVisitor {
        local_bindings: HashSet::new(),
        captures: &mut captures,
        checker: self,
    };
    visitor.visit_expr(expr);
    captures
}

struct CaptureVisitor<'a> {
    local_bindings: HashSet<String>,
    captures: &'a mut Vec<(String, Type)>,
    checker: &'a TypeChecker,
}

impl CaptureVisitor<'_> {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if !self.local_bindings.contains(&name.name) {
                    // This is a free variable - capture it
                    if let Some(ty) = self.checker.lookup_outer(&name.name) {
                        self.captures.push((name.name.clone(), ty));
                    }
                }
            }
            ExprKind::Closure { params, body, .. } => {
                // Nested closure - params are local to inner closure
                let mut inner = CaptureVisitor {
                    local_bindings: params.iter().map(|p| p.name.name.clone()).collect(),
                    captures: self.captures,
                    checker: self.checker,
                };
                inner.visit_expr(body);
            }
            // ... handle other expression kinds
            _ => self.visit_children(expr),
        }
    }
}
```

#### Phase 3: JavaScript Code Generation

**Tasks:**
- [ ] Add closure codegen in `lower_expr()` (`crates/husk-codegen-js/src/lib.rs`)
- [ ] Emit arrow functions for closures
- [ ] Handle recursive closures with named function expressions
- [ ] Ensure proper `this` binding in method contexts
- [ ] Generate source maps for closures

**Files to modify:**
- `crates/husk-codegen-js/src/lib.rs:697-714` - Add closure case to `lower_expr`

**Codegen Strategy:**
```rust
// crates/husk-codegen-js/src/lib.rs

fn lower_closure(params: &[ClosureParam], body: &Expr) -> JsExpr {
    let js_params: Vec<String> = params.iter()
        .map(|p| p.name.name.clone())
        .collect();

    let js_body = lower_expr(body);

    JsExpr::Arrow {
        params: js_params,
        body: Box::new(js_body),
    }
}

// Add to JsExpr enum
pub enum JsExpr {
    // ... existing variants ...

    /// Arrow function: (params) => body
    Arrow {
        params: Vec<String>,
        body: Box<JsExpr>,
    },
}

// In emit_expr()
fn emit_expr(expr: &JsExpr, out: &mut String) {
    match expr {
        JsExpr::Arrow { params, body } => {
            if params.is_empty() {
                out.push_str("() => ");
            } else if params.len() == 1 {
                out.push_str(&params[0]);
                out.push_str(" => ");
            } else {
                out.push('(');
                out.push_str(&params.join(", "));
                out.push_str(") => ");
            }
            emit_expr(body, out);
        }
        // ... other cases
    }
}
```

**Recursive Closure Detection:**
```rust
fn is_recursive_closure(name: &str, body: &Expr) -> bool {
    struct RecursionChecker<'a> {
        name: &'a str,
        found: bool,
    }

    impl RecursionChecker<'_> {
        fn check(&mut self, expr: &Expr) {
            match &expr.kind {
                ExprKind::Ident(id) if id.name == self.name => {
                    self.found = true;
                }
                ExprKind::Closure { .. } => {
                    // Don't recurse into nested closures
                }
                _ => self.check_children(expr),
            }
        }
    }

    let mut checker = RecursionChecker { name, found: false };
    checker.check(body);
    checker.found
}

// Emit named function for recursive closures
fn lower_recursive_closure(name: &str, params: &[ClosureParam], body: &Expr) -> JsExpr {
    // Emit: (function name(params) { return body; })
    JsExpr::Paren(Box::new(JsExpr::NamedFunction {
        name: name.to_string(),
        params: params.iter().map(|p| p.name.name.clone()).collect(),
        body: vec![JsStmt::Return(lower_expr(body))],
    }))
}
```

#### Phase 4: Testing & Polish

**Tasks:**
- [ ] Add parser tests for closure syntax variations
- [ ] Add type checker tests for inference and captures
- [ ] Add codegen tests verifying JavaScript output
- [ ] Add integration tests with example programs
- [ ] Update error messages for closure-specific issues
- [ ] Add LSP support for closures (hover, completion)

**Test Files to Create:**
- `crates/husk-parser/tests/closures.rs`
- `crates/husk-semantic/tests/closures.rs`
- `crates/husk-codegen-js/tests/closures.rs`
- `examples/closures_basic.hk`
- `examples/closures_higher_order.hk`
- `examples/closures_capture.hk`

## Acceptance Criteria

### Functional Requirements

- [ ] Closure syntax `|params| body` parses correctly
- [ ] Empty parameter closures `|| expr` work
- [ ] Block body closures `|x| { stmt; expr }` work
- [ ] Type annotations on parameters work: `|x: i32| x + 1`
- [ ] Return type annotations work: `|x| -> i32 { x + 1 }`
- [ ] Function type syntax works: `fn(i32, i32) -> i32`
- [ ] Variables can hold closures: `let f = |x| x + 1;`
- [ ] Closures can be passed as function arguments
- [ ] Closures can be returned from functions
- [ ] Closures correctly capture enclosing scope variables
- [ ] Type inference works for closure parameters when context available
- [ ] Closures compile to JavaScript arrow functions
- [ ] Recursive closures compile to named function expressions

### Non-Functional Requirements

- [ ] Parser produces clear error messages for malformed closures
- [ ] Type errors for closures are understandable
- [ ] Generated JavaScript is readable and debuggable
- [ ] Source maps correctly point to closure locations
- [ ] No performance regression in compilation

### Quality Gates

- [ ] All existing tests pass
- [ ] New tests cover happy path and error cases
- [ ] Examples compile and run correctly
- [ ] Code reviewed for consistency with existing patterns

## Design Decisions

### Decision 1: Rust-style Pipe Syntax

**Chosen:** `|x, y| x + y`

**Alternatives Considered:**
- TypeScript/JavaScript: `(x, y) => x + y` - Too similar to existing expressions
- Swift: `{ x, y in x + y }` - Conflicts with block syntax

**Rationale:** Husk is Rust-flavored, so Rust syntax is expected by users.

### Decision 2: Immutable Captures Only (MVP)

**Chosen:** Closures capture variables immutably (read-only)

**Alternatives Considered:**
- Mutable captures with `mut` keyword
- Rust's `move` semantics

**Rationale:** Simplest implementation for MVP. Mutable captures can be added later. JavaScript's reference semantics mean captured variables can still be mutated, but Husk won't provide explicit mutable capture syntax initially.

### Decision 3: Arrow Functions for Codegen

**Chosen:** Always emit JavaScript arrow functions for closures

**Alternatives Considered:**
- Regular functions with `.bind(this)`
- Mix based on context

**Rationale:** Arrow functions capture lexical `this` which matches Husk's semantics. Simpler codegen.

### Decision 4: Type Inference from Context

**Chosen:** Infer closure parameter types from expected type when available

**Alternatives Considered:**
- Always require explicit type annotations
- Full Hindley-Milner inference

**Rationale:** Matches Rust's behavior. Explicit types required when context unavailable.

### Decision 5: Single Concrete Type per Closure

**Chosen:** Each closure has one concrete type (no polymorphic closures)

**Alternatives Considered:**
- Generic closures like `|x| x` being polymorphic

**Rationale:** Matches Rust semantics. Simpler type system. Users can use generic functions instead.

## Dependencies & Prerequisites

- **Required:** Existing `Type::Function` in type system (already present)
- **Required:** Block expressions (already implemented)
- **Required:** Type inference infrastructure (partially present)
- **Helpful:** Recent traits/impl implementation provides patterns to follow

## Risk Analysis & Mitigation

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Parser ambiguity with `\|` | Medium | High | Use lookahead to distinguish closure from bitwise OR |
| Type inference complexity | Medium | Medium | Start with explicit types, add inference incrementally |
| JavaScript `this` binding issues | Low | Medium | Always use arrow functions |
| Recursive closure detection | Low | Low | Conservative check, emit named functions when in doubt |
| Memory leaks from captures | Low | Low | Document implications; JavaScript GC handles most cases |

## Future Considerations

After MVP, consider:
- **Mutable captures:** `|mut x| { x += 1; }`
- **Move semantics:** `move |x| ...` for explicit ownership transfer
- **Fn traits:** `Fn`, `FnMut`, `FnOnce` for fine-grained control
- **Method references:** `obj.method` as closure value
- **Trailing closure syntax:** `foo(x) { |y| ... }`
- **Closure in pattern matching:** Match on function types

## References

### Internal References

- Type system: `crates/husk-types/src/lib.rs` - `Type::Function` already defined
- AST expressions: `crates/husk-ast/src/lib.rs:63-113` - `ExprKind` enum
- Function parsing: `crates/husk-parser/src/lib.rs:210-246`
- Function type checking: `crates/husk-semantic/src/lib.rs:401-443`
- Function codegen: `crates/husk-codegen-js/src/lib.rs:342-372`

### External References

- [Rust Closures - The Rust Book](https://doc.rust-lang.org/book/ch13-01-closures.html)
- [TypeScript Function Types](https://www.typescriptlang.org/docs/handbook/2/functions.html)
- [Swift Closures](https://docs.swift.org/swift-book/LanguageGuide/Closures.html)
- [JavaScript Arrow Functions - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions)

---

## Example Code

### Basic Closure Usage

```rust
// examples/closures_basic.hk

fn main() {
    // Simple closure
    let add = |x: i32, y: i32| x + y;
    println("add(2, 3) = {}", add(2, 3));

    // Closure with inferred types
    let double: fn(i32) -> i32 = |x| x * 2;
    println("double(5) = {}", double(5));

    // Empty parameter closure
    let get_value = || 42;
    println("value = {}", get_value());

    // Block body
    let complex = |x: i32| {
        let y = x * 2;
        y + 1
    };
    println("complex(3) = {}", complex(3));
}
```

### Higher-Order Functions

```rust
// examples/closures_higher_order.hk

fn apply(f: fn(i32) -> i32, x: i32) -> i32 {
    f(x)
}

fn compose(f: fn(i32) -> i32, g: fn(i32) -> i32) -> fn(i32) -> i32 {
    |x| f(g(x))
}

fn main() {
    let result = apply(|x| x * 2, 5);
    println("apply result: {}", result);  // 10

    let add_one = |x: i32| x + 1;
    let double = |x: i32| x * 2;
    let combined = compose(add_one, double);
    println("composed(3): {}", combined(3));  // 7 = (3 * 2) + 1
}
```

### Variable Capture

```rust
// examples/closures_capture.hk

fn make_counter(start: i32) -> fn() -> i32 {
    let count = start;
    || count
}

fn make_adder(n: i32) -> fn(i32) -> i32 {
    |x| x + n
}

fn main() {
    let counter = make_counter(10);
    println("counter: {}", counter());  // 10

    let add_five = make_adder(5);
    println("add_five(3): {}", add_five(3));  // 8

    // Nested capture
    let outer = 100;
    let nested = |x: i32| {
        let inner = |y: i32| x + y + outer;
        inner(10)
    };
    println("nested(5): {}", nested(5));  // 115
}
```

### Expected JavaScript Output

```javascript
// closures_basic.hk compiled

function main() {
    const add = (x, y) => x + y;
    console.log(`add(2, 3) = ${add(2, 3)}`);

    const double = x => x * 2;
    console.log(`double(5) = ${double(5)}`);

    const get_value = () => 42;
    console.log(`value = ${get_value()}`);

    const complex = x => {
        const y = x * 2;
        return y + 1;
    };
    console.log(`complex(3) = ${complex(3)}`);
}

main();
```
