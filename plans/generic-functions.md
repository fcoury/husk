# ✨ feat: Add Generic Types to Functions

> Enable functions to have type parameters with bidirectional type inference

## Overview

Implement generic type parameters for functions, allowing developers to write reusable, type-safe code like `fn identity<T>(x: T) -> T`. The infrastructure is largely in place—parser, AST, and TypeScript generation already support generics. The main work is threading type parameters through semantic analysis.

**Current State:** Parser captures `<T, U>` on functions, but semantic analysis ignores them
**Goal:** Full generic function support with type inference at call sites

## Problem Statement / Motivation

Currently, Husk functions cannot be generic, limiting code reuse and expressiveness:

```rust
// Today: Must write separate functions for each type
fn identity_i32(x: i32) -> i32 { x }
fn identity_string(x: String) -> String { x }

// Goal: Single generic function
fn identity<T>(x: T) -> T { x }

let x = identity(42);        // T inferred as i32
let s = identity("hello");   // T inferred as String
```

This is a fundamental feature for:
- Higher-order functions (`map`, `filter`, `fold`)
- Generic data structure operations
- Type-safe utility functions
- Idiomatic Rust-style code

## Proposed Solution

Thread type parameters through semantic analysis using the existing infrastructure:

1. **Store generics in `FnDef`** - Add `type_params: Vec<String>` field
2. **Pass to type resolution** - Thread generic params through `resolve_type_expr()`
3. **Infer at call sites** - Use bidirectional type checking (already implemented for closures)
4. **Type erasure for JS** - No codegen changes needed

## Technical Approach

### Architecture

The implementation follows the existing pattern used for structs and enums:

```
Function Definition          Call Site
       │                          │
       ▼                          ▼
  Parse <T, U>              Infer T, U from args
       │                          │
       ▼                          ▼
  Store in FnDef             Unify with FnDef
       │                          │
       ▼                          ▼
  Check body with            Validate argument types
  T, U in scope              against instantiated sig
```

### Key Files to Modify

| File | Lines | Changes |
|------|-------|---------|
| `crates/husk-semantic/src/lib.rs:155-159` | Add `type_params` to `FnDef` struct |
| `crates/husk-semantic/src/lib.rs:279-290` | Store type_params in function registration |
| `crates/husk-semantic/src/lib.rs:385-400` | Extract type_params in `check_file()` |
| `crates/husk-semantic/src/lib.rs:402-444` | Accept and use type_params in `check_fn()` |
| `crates/husk-semantic/src/lib.rs:892-947` | Infer type args at call sites |

### Implementation Phases

#### Phase 1: Update FnDef Structure

```rust
// crates/husk-semantic/src/lib.rs:155-159

#[derive(Debug, Clone)]
struct FnDef {
    type_params: Vec<String>,  // ← ADD THIS
    params: Vec<Param>,
    ret_type: Option<TypeExpr>,
}
```

#### Phase 2: Store Type Parameters During Registration

```rust
// crates/husk-semantic/src/lib.rs:279-290

ItemKind::Fn {
    name,
    type_params,  // ← EXTRACT
    params,
    ret_type,
    ..
} => {
    let def = FnDef {
        type_params: type_params.iter().map(|id| id.name.clone()).collect(),
        params: params.clone(),
        ret_type: ret_type.clone(),
    };
    self.env.functions.insert(name.name.clone(), def);
}
```

#### Phase 3: Thread Through check_fn

```rust
// crates/husk-semantic/src/lib.rs:402-444

fn check_fn(
    &mut self,
    name: &Ident,
    type_params: &[Ident],  // ← ADD PARAMETER
    params: &[Param],
    ret_type_expr: Option<&TypeExpr>,
    body: &[Stmt],
    span: Span,
) {
    // Convert to Vec<String> for resolve_type_expr
    let generic_params: Vec<String> = type_params
        .iter()
        .map(|id| id.name.clone())
        .collect();

    let ret_ty = if let Some(ty_expr) = ret_type_expr {
        self.resolve_type_expr(ty_expr, &generic_params)  // ← PASS GENERICS
    } else {
        Type::Primitive(PrimitiveType::Unit)
    };

    let mut locals: HashMap<String, Type> = HashMap::new();

    for param in params {
        let ty = self.resolve_type_expr(&param.ty, &generic_params);  // ← PASS GENERICS
        if locals.insert(param.name.name.clone(), ty).is_some() {
            self.errors.push(SemanticError {
                message: format!(
                    "duplicate parameter name `{}` in function `{}`",
                    param.name.name, name.name
                ),
                span: param.name.span.clone(),
            });
        }
    }

    let mut ctx = FnContext {
        tcx: self,
        locals,
        ret_ty,
        in_loop: false,
    };

    for stmt in body {
        ctx.check_stmt(stmt);
    }
}
```

#### Phase 4: Type Inference at Call Sites

```rust
// crates/husk-semantic/src/lib.rs (around line 892-947)

ExprKind::Call { callee, args } => {
    // ... existing code ...

    // For generic functions, infer type arguments from call arguments
    if let Some(fn_def) = self.tcx.env.functions.get(&callee_name) {
        if !fn_def.type_params.is_empty() {
            // Build substitution map from argument types
            let mut substitutions: HashMap<String, Type> = HashMap::new();

            for (i, arg) in args.iter().enumerate() {
                let arg_ty = self.check_expr(arg);
                if let Some(param) = fn_def.params.get(i) {
                    // Unify param type with arg type, collecting substitutions
                    self.unify_and_substitute(
                        &param.ty,
                        &arg_ty,
                        &fn_def.type_params,
                        &mut substitutions,
                    );
                }
            }

            // Apply substitutions to return type
            return self.apply_substitutions(&ret_ty, &substitutions);
        }
    }

    ret_ty
}
```

#### Phase 5: Add Unification Helper

```rust
// crates/husk-semantic/src/lib.rs (new function)

/// Attempt to unify a type expression with a concrete type,
/// collecting substitutions for generic parameters.
fn unify_and_substitute(
    &mut self,
    type_expr: &TypeExpr,
    concrete: &Type,
    generic_params: &[String],
    substitutions: &mut HashMap<String, Type>,
) {
    match &type_expr.kind {
        TypeExprKind::Named(id) if generic_params.contains(&id.name) => {
            // This is a generic parameter - record the substitution
            if let Some(existing) = substitutions.get(&id.name) {
                // Already have a substitution - check compatibility
                if !self.types_compatible(existing, concrete) {
                    self.tcx.errors.push(SemanticError {
                        message: format!(
                            "conflicting types for generic parameter `{}`: \
                             `{:?}` vs `{:?}`",
                            id.name, existing, concrete
                        ),
                        span: type_expr.span.clone(),
                    });
                }
            } else {
                substitutions.insert(id.name.clone(), concrete.clone());
            }
        }
        TypeExprKind::Generic { name, args } => {
            // Recursively unify generic arguments
            if let Type::Named { args: concrete_args, .. } = concrete {
                for (type_arg, concrete_arg) in args.iter().zip(concrete_args.iter()) {
                    self.unify_and_substitute(
                        type_arg,
                        concrete_arg,
                        generic_params,
                        substitutions,
                    );
                }
            }
        }
        _ => {
            // Non-generic type - no substitution needed
        }
    }
}

/// Apply collected substitutions to a type.
fn apply_substitutions(
    &self,
    ty: &Type,
    substitutions: &HashMap<String, Type>,
) -> Type {
    match ty {
        Type::Named { name, args } if args.is_empty() => {
            // Could be a generic parameter
            if let Some(concrete) = substitutions.get(name) {
                concrete.clone()
            } else {
                ty.clone()
            }
        }
        Type::Named { name, args } => {
            // Apply substitutions to type arguments
            Type::Named {
                name: name.clone(),
                args: args.iter()
                    .map(|a| self.apply_substitutions(a, substitutions))
                    .collect(),
            }
        }
        Type::Function { params, ret } => {
            Type::Function {
                params: params.iter()
                    .map(|p| self.apply_substitutions(p, substitutions))
                    .collect(),
                ret: Box::new(self.apply_substitutions(ret, substitutions)),
            }
        }
        _ => ty.clone(),
    }
}
```

## Acceptance Criteria

### Functional Requirements

- [x] **Parser ready**: `fn foo<T, U>()` already parses correctly
- [x] **AST ready**: `ItemKind::Fn` has `type_params: Vec<Ident>`
- [x] **TypeScript ready**: `.d.ts` generation handles generics
- [x] **Semantic analysis**: Generic params stored in `FnDef`
- [x] **Type checking**: Generic params in scope during body check
- [x] **Call inference**: Type args inferred from arguments
- [x] **Error handling**: Clear errors for inference failures

### Non-Functional Requirements

- [x] No changes to code generation (type erasure)
- [x] All existing tests pass
- [x] New tests cover generic scenarios
- [x] Performance: no significant slowdown

### Quality Gates

- [x] All existing tests pass
- [x] New tests for each generic scenario
- [x] Error messages are clear and actionable
- [x] TypeScript .d.ts files correctly typed

## Test Plan

### test_generic_functions.hk

```rust
// Basic single-parameter generic
fn identity<T>(x: T) -> T {
    x
}

// Multiple type parameters
fn pair<T, U>(first: T, second: U) -> (T, U) {
    (first, second)
}

// Generic with generic type in signature
fn wrap<T>(value: T) -> Option<T> {
    Option::Some(value)
}

// Generic higher-order function
fn apply<T, U>(x: T, f: fn(T) -> U) -> U {
    f(x)
}

// Generic with closure inference
fn map_option<T, U>(opt: Option<T>, f: fn(T) -> U) -> Option<U> {
    match opt {
        Option::Some(value) => Option::Some(f(value)),
        Option::None => Option::None,
    }
}

fn main() {
    // Basic inference
    let x = identity(42);        // T = i32
    let s = identity("hello");   // T = String

    // Multi-param inference
    let p = pair(1, "two");      // T = i32, U = String

    // With Option
    let opt = wrap(100);         // Option<i32>

    // Higher-order with closure
    let doubled = apply(21, |x| x * 2);  // T = i32, U = i32

    // Nested generic usage
    let mapped = map_option(Option::Some(5), |x| x.to_string());
}
```

### test_generic_errors.hk

```rust
// Error: conflicting type arguments
fn identity<T>(x: T) -> T { x }

fn test_conflict() {
    // ERROR: T inferred as i32 from first arg, but second is String
    // identity(42, "hello");  // Arity error first
}

// Error: cannot infer without context
fn mystery<T>() -> T {
    // ERROR: Cannot determine T
}

fn test_no_inference() {
    let x = mystery();  // ERROR: cannot infer type for T
}

// Error: unused type parameter (optional - may be warning)
fn unused<T>(x: i32) -> i32 {
    x  // T is never used
}
```

## Dependencies & Prerequisites

- [x] Closure type inference implemented (provides bidirectional checking pattern)
- [x] Parser handles `<T, U>` syntax (lines 210-244, 898-914)
- [x] AST has `type_params` field (lines 308-315)
- [x] `resolve_type_expr()` accepts generic params (lines 446-470)
- [x] `resolve_named_type()` checks generic params (lines 472-490)
- [x] TypeScript generation handles generics (lines 1694-1732)

## Risk Analysis & Mitigation

| Risk | Impact | Likelihood | Mitigation |
|------|--------|------------|------------|
| Inference algorithm complexity | Medium | Medium | Start with simple unification; defer advanced cases |
| Type erasure limitations | Low | Low | Document limitations; matches TypeScript behavior |
| Trait bounds integration | Medium | Low | Defer bounds to Phase 2; unconstrained generics first |
| Parser conflicts at call sites | Medium | Low | MVP uses inference only; turbofish syntax later |

## Future Considerations

- **Explicit type arguments**: Add turbofish syntax `::<T>` for disambiguation
- **Trait bounds**: `fn foo<T: Display>(x: T)` requires trait system integration
- **Where clauses**: Complex bounds like `where T: Clone + Debug`
- **Higher-kinded types**: `fn foo<F<_>>(x: F<i32>)` - likely out of scope
- **Const generics**: `fn foo<const N: usize>()` - future enhancement

## References

### Internal References

- Parser type params: `crates/husk-parser/src/lib.rs:210-244, 898-914`
- AST ItemKind::Fn: `crates/husk-ast/src/lib.rs:308-315`
- FnDef struct: `crates/husk-semantic/src/lib.rs:155-159`
- Function registration: `crates/husk-semantic/src/lib.rs:279-290`
- check_fn(): `crates/husk-semantic/src/lib.rs:402-444`
- resolve_type_expr(): `crates/husk-semantic/src/lib.rs:446-470`
- resolve_named_type(): `crates/husk-semantic/src/lib.rs:472-490`
- TypeScript generation: `crates/husk-codegen-js/src/lib.rs:1694-1732`
- Design decisions: `decisions.md:68-71`
- Syntax spec: `syntax.md:38-51`
- Closure inference: `plans/closure-type-inference.md`

### External References

- [Rust Generic Functions](https://doc.rust-lang.org/book/ch10-01-syntax.html)
- [TypeScript Generics](https://www.typescriptlang.org/docs/handbook/2/generics.html)
- [Bidirectional Type Checking Tutorial](https://davidchristiansen.dk/tutorials/bidirectional.pdf)
- [Type Erasure vs Monomorphization](https://langdev.stackexchange.com/questions/902/how-to-choose-between-monomorphisation-and-type-erasure-for-generics)

### Related Work

- Recent commit: `feat: implement bidirectional type inference for closure parameters`
- Traits implementation: `plans/traits-in-husk.md`
- Roadmap: Phase 2 generics tasks
