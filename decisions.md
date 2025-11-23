# MVP Decisions

This document summarizes the key decisions for the initial MVP of the language: what is in scope, what is explicitly out of scope, and the condensed rationale behind those choices.

## 1. Language Name and File Extensions

**Decision:** The language is called **Husk**.

- Preferred file extensions:
  - `.hk` (short form)
  - `.husk` (long form)
- Compiler CLI:
  - Referred to as `huskc` in documentation and examples.

**Rationale:**

- The name “Husk” echoes “Rust” while conveying something lightweight and focused around a core.
- Supporting both `.hk` and `.husk` gives a short, ergonomic extension and a more descriptive one.
- A dedicated `huskc` CLI reinforces the identity of the toolchain.

## 2. High-Level Positioning

**Decision:** The MVP is a Rust-like, statically typed language that compiles to JavaScript (ES modules), focusing on:

- Algebraic data types (structs and enums) and exhaustive pattern matching.
- Parametric generics with type erasure at runtime.
- A simple, predictable type system and readable JS output.
- Manual but ergonomic interop with existing JS/TS libraries.

**Rationale (condensed):**

- Dropping linear/resource types and Drop semantics greatly simplifies the compiler and mental model.
- The language competes more with TypeScript/ReScript/ML-to-JS, but gets to a usable state much faster.
- Safety is primarily via types and enums/match, not ownership or resource protocols.

## 3. Linear / Resource Types and Drop

**Decision:** No linear types or resource types in the MVP. No `Drop`/destructor semantics in the MVP.

- All user-defined structs behave like normal JS/TS data: freely aliased, no ownership tracking.
- No special "must consume" guarantees enforced by the compiler.
- No RAII-style automatic clean-up; resources are managed via explicit APIs and conventions.

**Rationale:**

- JS is already memory-safe via garbage collection; linear types are about resource protocols, not memory safety.
- Enforcing linear discipline everywhere would:
  - Complicate the type system and control-flow analysis.
  - Make interop with JS/TS harder, since JS values are freely shared and aliased.
  - Add cognitive overhead for ordinary data that does not benefit strongly from linearity.
- Destructors in a JS backend are non-trivial to specify (async, closures, escaping values). They can be added later once the core language is stable.

**Implication:** The language will not statically guarantee correct resource usage in MVP. Correctness for resources (e.g., closing files) relies on API design and tests, not the type checker.

## 4. Types, Generics, and Enums

**In MVP:**

- Primitive types: `i32`, `bool`, `String`, `()`.
- Structs: record-style only (no tuple structs initially).
- Enums: unit, tuple-like, and struct-like variants, with generics:
  ```rust
  enum Result<T, E> {
      Ok(T),
      Err(E),
  }
  ```
- Generics:
  - On structs and enums: `struct Box<T> { value: T }`.
  - On functions: `fn id<T>(x: T) -> T`.
  - Implemented via type erasure at runtime (one JS implementation, type-only in the checker).
- Pattern matching:
  - `match` with exhaustive checking on enums.
  - Patterns for unit, tuple, and struct variants, plus `_` and simple bindings.

**Out of MVP:**

- Trait bounds and typeclasses/traits.
- Higher-kinded types, advanced generics features.

**Rationale:**

- Enums + generics + match are central to the language’s ergonomic and safety story.
- Type-erased generics are a good fit for JS and simpler than monomorphization.
- Traits and richer generic constraints can be layered on later without invalidating early design decisions.

## 5. Expressions and Method Syntax

**In MVP:**

- Literals: integer, string, `true`, `false`.
- Names and function calls: `foo(a, b)`.
- Field access: `obj.field`.
- **Method calls:** `obj.method(arg1, arg2)`:
  - For MVP, codegen treats this as a normal JS method call: `obj.method(arg1, arg2)`.
  - Full “methods on user-defined types” can be introduced later, possibly as sugar over free functions.
- Basic operators:
  - Arithmetic: `+`, `-`, `*`, `/`.
  - Comparisons: `==`, `!=`, `<`, `>`, `<=`, `>=`.
  - Logical: `&&`, `||`, `!`.

**Out of MVP:**

- Operator overloading.
- User-defined operators.

**Rationale:**

- Method-call syntax is key for ergonomic JS interop and mirrors common JS/TS usage (`array.push(x)`, etc.).
- Keeping operators built-in and non-overloadable in MVP keeps parsing and type checking simple.

## 6. Control Flow and Statements

**In MVP:**

- `let` / `let mut` bindings, with optional type annotations.
- Expression statements.
- `return expr;` and `return;`.
- Blocks `{ stmt* }`.
- Control flow:
  - `if` / `else`.
  - `while`.
  - `break` and `continue` within `while`.

**Out of MVP:**

- `for` loops, `loop`, `switch`/`match`-style sugar beyond the main `match`.
- `async`, `await`, generators.

**Rationale:**

- These constructs are enough to express typical algorithms and glue JS APIs.
- More advanced control flow can be added later without changing fundamentals.

## 7. Modules and Namespaces

**In MVP:**

- Each file is treated as a module.
- No explicit `mod` / `use` syntax yet.
- The compiler works at the “one-file-in, one (or few) JS files out” level initially.

**Out of MVP:**

- Language-level module/import syntax.
- Complex namespace and visibility rules.

**Rationale:**

- Simplifies parsing and name resolution in early phases.
- Real module system can be added once the core language and tooling are solid.

## 8. JavaScript / TypeScript Interop

**In MVP:**

- Manual `extern "js"` declarations for interop:
  ```rust
  extern "js" {
      fn parse_json(text: String) -> Result<Json, JsError>;
  }
  ```
- A special “dynamic” or JS-typed value (e.g., `JsValue` or similar) to represent opaque JS values where needed.
- Outbound `.d.ts` generation:
  - Emit `.d.ts` for compiled modules so TypeScript can consume them.

**Out of MVP:**

- Automatic `.d.ts` ingestion and conversion into language types.
- Deep integration with the TypeScript compiler or type system.

**Rationale:**

- Manual externs (C-header style) are enough to interop with existing JS libraries in MVP; only a subset of each library typically needs to be declared.
- Automatic `.d.ts` ingestion is a major feature that can be added later without changing surface syntax.

## 9. Runtime and Standard Library

**In MVP:**

- A small JS preamble/runtime:
  - Tagged union helpers for enums.
  - `Result` constructors (`Ok`, `Err`).
  - `panic` / assertion utilities.
- Minimal “core” library in the language:
  - Definitions of `Option`/`Result` (if not fully inlined into the runtime).

**Out of MVP:**

- Rich standard library (collections, IO, async facilities).
- Versioned and stable public std APIs.

**Rationale:**

- Keep the runtime small and focused, just enough to support enums, results, and panics.
- A richer standard library is important but can evolve independently after the core language and compiler are working.
