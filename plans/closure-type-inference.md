# ✨ feat: Closure Parameter Type Inference

> Infer closure parameter types from call-site context using bidirectional type checking

## Overview

Implement type inference for closure parameters when the expected function type is known from context. This allows developers to write `|x| x + 1` instead of `|x: i32| x + 1` when passing closures to functions with known signatures.

**Current State:** Untyped closure parameters default to `Unit` type (placeholder)
**Goal:** Infer parameter types from expected function type at call sites

## Problem Statement / Motivation

Currently, Husk closures require explicit type annotations for all parameters:

```rust
// Today: explicit types required
fn map(x: i32, f: fn(i32) -> i32) -> i32 { f(x) }
map(42, |x: i32| x + 1)  // Verbose

// Goal: infer from context
map(42, |x| x + 1)  // x: i32 inferred from map's signature
```

This is a usability gap compared to Rust and TypeScript, which infer closure/arrow function parameter types from context.

## Proposed Solution

Implement **bidirectional type checking** that propagates expected types from call sites to closure expressions:

1. **Checking mode**: When expected type is known, propagate parameter types to closures
2. **Synthesis mode**: When no context available, require explicit annotations
3. **Validation**: When explicit annotations exist, validate against expected type

## Technical Approach

### Architecture

The implementation modifies the semantic analysis phase to pass expected types through the AST:

```
Call Expression
       │
       ▼ (extract fn signature)
  Expected Type: fn(i32) -> i32
       │
       ▼ (propagate to argument)
  Closure Expression
       │
       ▼ (extract param types)
  Infer x: i32
```

### Key Files to Modify

| File | Changes |
|------|---------|
| `crates/husk-semantic/src/lib.rs:842-1195` | Add `expected` parameter to `check_expr` |
| `crates/husk-semantic/src/lib.rs:1198-1251` | Update `check_closure_expr` to use expected type |
| `crates/husk-semantic/src/lib.rs:892-947` | Modify call expression to propagate param types |
| `crates/husk-types/src/lib.rs:16,33` | Potentially use existing `Type::Var` for unknowns |

### Implementation Phases

#### Phase 1: Add Expected Type Propagation

Add optional expected type parameter to type checking:

```rust
// crates/husk-semantic/src/lib.rs

/// Check expression with optional expected type for bidirectional inference
fn check_expr_with_expected(
    &mut self,
    expr: &Expr,
    expected: Option<&Type>
) -> Type {
    match &expr.kind {
        ExprKind::Closure { params, ret_type, body } => {
            self.check_closure_expr_with_expected(
                expr, params, ret_type.as_ref(), body, expected
            )
        }
        _ => self.check_expr(expr)  // Fall back for other expressions
    }
}
```

#### Phase 2: Update Closure Type Checking

Modify `check_closure_expr` to extract parameter types from expected type:

```rust
// crates/husk-semantic/src/lib.rs

fn check_closure_expr_with_expected(
    &mut self,
    expr: &Expr,
    params: &[ClosureParam],
    ret_type: Option<&TypeExpr>,
    body: &Expr,
    expected: Option<&Type>,
) -> Type {
    let mut param_types = Vec::new();
    let mut closure_locals = self.locals.clone();

    // Extract expected parameter types if available
    let expected_params = match expected {
        Some(Type::Function { params, .. }) => Some(params),
        _ => None,
    };

    for (i, param) in params.iter().enumerate() {
        let ty = if let Some(type_expr) = &param.ty {
            // Explicit annotation - use it
            let annotated = self.tcx.resolve_type_expr(type_expr, &[]);

            // Optionally validate against expected
            if let Some(expected_params) = expected_params {
                if let Some(expected_ty) = expected_params.get(i) {
                    if !self.types_compatible(expected_ty, &annotated) {
                        self.tcx.errors.push(SemanticError {
                            message: format!(
                                "closure parameter type `{}` does not match expected `{}`",
                                annotated, expected_ty
                            ),
                            span: param.name.span.clone(),
                        });
                    }
                }
            }
            annotated
        } else if let Some(expected_params) = expected_params {
            // No annotation - infer from expected type
            expected_params.get(i).cloned().unwrap_or_else(|| {
                self.tcx.errors.push(SemanticError {
                    message: format!(
                        "cannot infer type for closure parameter `{}`",
                        param.name.name
                    ),
                    span: param.name.span.clone(),
                });
                Type::Primitive(PrimitiveType::Unit)
            })
        } else {
            // No context - require annotation
            self.tcx.errors.push(SemanticError {
                message: format!(
                    "cannot infer type for closure parameter `{}`. \
                     Add type annotation: `|{}: Type|`",
                    param.name.name, param.name.name
                ),
                span: param.name.span.clone(),
            });
            Type::Primitive(PrimitiveType::Unit)
        };

        param_types.push(ty.clone());
        closure_locals.insert(param.name.name.clone(), ty);
    }

    // ... rest of closure type checking (return type, body)
}
```

#### Phase 3: Update Call Expression

Propagate expected types to closure arguments:

```rust
// crates/husk-semantic/src/lib.rs (around line 892)

ExprKind::Call { callee, args } => {
    let callee_ty = self.check_expr(callee);

    let (param_tys, ret_ty) = match callee_ty {
        Type::Function { params, ret } => (params, *ret),
        other => { /* existing error handling */ }
    };

    // Check arguments WITH expected types for closure inference
    for (i, arg) in args.iter().enumerate() {
        let expected = param_tys.get(i);
        let arg_ty = self.check_expr_with_expected(arg, expected);

        if let Some(expected) = expected {
            if !self.types_compatible(expected, &arg_ty) {
                self.tcx.errors.push(SemanticError {
                    message: format!(
                        "mismatched argument type at position {}: \
                         expected `{}`, found `{}`",
                        i, expected, arg_ty
                    ),
                    span: arg.span.clone(),
                });
            }
        }
    }

    ret_ty
}
```

#### Phase 4: Handle Variable Bindings with Type Annotations

Allow type annotations on let bindings to provide context:

```rust
// Support: let f: fn(i32) -> i32 = |x| x + 1;

ExprKind::Let { name, ty, value, .. } => {
    let expected = ty.as_ref().map(|t| self.tcx.resolve_type_expr(t, &[]));
    let value_ty = self.check_expr_with_expected(value, expected.as_ref());
    // ... rest of let handling
}
```

## Acceptance Criteria

### Functional Requirements

- [x] **Basic inference**: `map(42, |x| x + 1)` infers `x: i32` from function signature
- [x] **Multi-param**: `fold(|acc, x| acc + x)` infers both parameters
- [x] **Error on no context**: `let f = |x| x + 1` produces clear error with fix suggestion
- [ ] **Explicit validated**: `map(42, |x: String| ...)` errors if `String` doesn't match expected `i32`
- [ ] **Return type works**: `map(|x| x.to_string())` validates return type bidirectionally
- [ ] **Nested closures**: `outer(|x| |y| x + y)` infers both levels
- [x] **Variable binding context**: `let f: fn(i32) -> i32 = |x| x + 1` uses annotation

### Non-Functional Requirements

- [x] Single-pass inference (no backward propagation across statements)
- [x] Clear error messages with suggested fixes
- [x] No performance regression for existing code
- [x] Backward compatible (existing annotated closures work unchanged)

### Quality Gates

- [x] All existing tests pass
- [x] New tests cover each inference scenario
- [x] Error messages reviewed for clarity
- [ ] LSP hover shows inferred types (if applicable)

## Test Plan

### test_closure_inference.hk

```rust
// Basic inference from function signature
fn apply(x: i32, f: fn(i32) -> i32) -> i32 {
    f(x)
}

fn test_basic_inference() {
    let result = apply(5, |x| x + 1);  // x inferred as i32
    // assert result == 6
}

// Multi-parameter inference
fn combine(a: i32, b: i32, f: fn(i32, i32) -> i32) -> i32 {
    f(a, b)
}

fn test_multi_param() {
    let result = combine(3, 4, |x, y| x + y);  // Both inferred
    // assert result == 7
}

// Empty closure inference
fn run(f: fn() -> i32) -> i32 {
    f()
}

fn test_empty_closure() {
    let result = run(|| 42);  // No params to infer
    // assert result == 42
}

// Explicit annotations still work
fn test_explicit_annotations() {
    let result = apply(5, |x: i32| x + 1);  // Explicit, validated
    // assert result == 6
}

// Variable binding with type annotation
fn test_variable_binding_context() {
    let f: fn(i32) -> i32 = |x| x * 2;  // Inferred from binding type
    let result = f(5);
    // assert result == 10
}

fn main() {
    test_basic_inference();
    test_multi_param();
    test_empty_closure();
    test_explicit_annotations();
    test_variable_binding_context();
}
```

### test_closure_inference_errors.hk

```rust
// Error: no context available
fn test_no_context_error() {
    let f = |x| x + 1;  // ERROR: cannot infer type
}

// Error: type mismatch
fn apply(x: i32, f: fn(i32) -> i32) -> i32 { f(x) }

fn test_type_mismatch_error() {
    apply(5, |x: String| x.len());  // ERROR: String vs i32
}

// Error: wrong arity
fn test_arity_mismatch() {
    apply(5, |x, y| x + y);  // ERROR: expected 1 param, got 2
}
```

## Dependencies & Prerequisites

- [x] Closure syntax implemented (PR: fc98922)
- [x] Function types (`fn(T) -> U`) in type system
- [x] Basic type checking infrastructure
- [ ] No external dependencies required

## Risk Analysis & Mitigation

| Risk | Impact | Likelihood | Mitigation |
|------|--------|------------|------------|
| Breaks existing code | High | Low | Inference only adds capability, doesn't change annotated closures |
| Complex generic interaction | Medium | Medium | Start with non-generic functions, add generic support later |
| Performance regression | Low | Low | Single-pass inference, no constraint solving |
| Poor error messages | Medium | Medium | Prioritize error message quality in testing |

## Future Considerations

- **Generic function inference**: Unify closure types with generic type parameters
- **Trait bound propagation**: Infer trait bounds on closure parameters
- **Multi-pass inference**: Allow backward propagation from usage sites (complex, likely not needed)
- **Type variable support**: Use `Type::Var` for more sophisticated unification (already defined in husk-types)

## References

### Internal References

- Type checking: `crates/husk-semantic/src/lib.rs:842-1195`
- Current closure checking: `crates/husk-semantic/src/lib.rs:1198-1251`
- Call expression: `crates/husk-semantic/src/lib.rs:892-947`
- Type definitions: `crates/husk-types/src/lib.rs:18-34`
- Closure AST: `crates/husk-ast/src/lib.rs:113-118, 300-304`
- Planning docs: `plans/closures-and-function-types.md:156-221`

### External References

- [Bidirectional Type Checking Tutorial](https://davidchristiansen.dk/tutorials/bidirectional.pdf)
- [Rust Closure Inference](https://rustc-dev-guide.rust-lang.org/closure.html)
- [Local Type Inference (Pierce & Turner)](https://www.cis.upenn.edu/~bcpierce/papers/lti-toplas.pdf)
- [Reconstructing TypeScript](https://jaked.org/blog/2021-09-15-Reconstructing-TypeScript-part-1)

### Related Work

- Recent commit: `feat: implement closures and function types` (2524e5f)
- Roadmap item: Phase 2 closure inference task
- Plan.md: Section 4.1 closures and function types
