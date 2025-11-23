# Language Project Plan

This document describes the high-level design and phased implementation plan for **Husk**, a Rust-syntax language that compiles to JavaScript, with Rust-style enums, generics, and TypeScript interop.

The implementation will be written in Rust as a multi-crate workspace.

---

## 1. Goals and Non-Goals

### 1.1 Primary Goals

- Provide a Rust-like programming experience that compiles to modern JavaScript (ES modules).
- Support Rust-style enums and pattern matching, encoded as tagged unions in JavaScript.
- Integrate with the TypeScript ecosystem:
  - Emit `.d.ts` files for all compiled modules.
  - Eventually ingest `.d.ts` files and turn them into safe extern declarations.
- Keep the generated JavaScript readable and idiomatic.
- Maintain a small, well-scoped JavaScript runtime (“preamble”) that is stable over time.

### 1.2 Non-Goals (for early versions)

- Full Rust-bytes compatibility or full Rust language coverage.
- Native code generation or WASM backend.
- Full TypeScript type system coverage (e.g., conditional types, mapped types, etc.).
- A complete replacement for TypeScript; the goal is a niche language with strong safety guarantees, not a general-purpose JS superset.

---

## 2. High-Level Architecture

The compiler will be structured in classic frontend/middleend/backend phases.

### 2.1 Frontend

- **Lexer**: Converts source text into tokens with spans.
- **Parser**: Consumes tokens into an Abstract Syntax Tree (AST) for a Rust-like syntax.
- **Diagnostics**: Collects and reports syntax and early errors in a user-friendly way.

### 2.2 Middleend (Semantic Analysis)

- **Name Resolution**: Builds symbol tables and resolves variable, function, and type names.
- **Type Checking**:
  - Primitive types (`i32`, `bool`, `String`, `()`).
  - Structs and enums, including basic generics (e.g. `Option<T>`, `Result<T, E>`) with type erasure at runtime.
  - Function types and calls.
  - `match` exhaustiveness checks for enums.
- **Potential future IR (non-MVP)**:
  - Introduce a dedicated intermediate representation in a later phase if needed for optimizations or advanced analyses.

### 2.3 Backend

- **Typed AST to JS AST**:
  - Map primitives, structs, enums, and functions to JavaScript constructs.
  - Encode enums as tagged unions (`{ tag: "Variant", ... }`).
  - Map modules to ES modules (`import` / `export`).
- **JS Code Generation**:
  - Pretty-print an internal JS AST to source code.
  - Eventually emit basic sourcemaps to link JS back to source.
- **TypeScript Declaration Emission**:
  - Emit `.d.ts` files for each compiled module, covering exported symbols.

### 2.4 Runtime / Standard Library

- A tiny JavaScript preamble with:
  - Tagged union helpers.
  - `Result` type helpers (`Ok`, `Err`).
  - `panic` / assertion utilities.
- A small standard library in the language itself:
  - Option/result types (if not part of the core).
  - Resource-like abstractions (file handles, network handles) as examples.

### 2.5 Tooling

- **CLI** (`huskc`):
  - `huskc compile main.hk -o main.js`
  - `huskc check` (typecheck only).
  - Future: `huskc new`, `huskc fmt`, `huskc test`.
- **LSP (later)**:
  - Language server for IDE integration (hover, go-to-definition, diagnostics).

---

## 3. Rust Workspace Layout

The project will be implemented as a Rust workspace with multiple crates:

- `husk-ast`: Shared AST data structures, basic visitors (with room to add an IR later if needed).
- `husk-lexer`: Token definitions and lexing logic.
- `husk-parser`: Parsing from tokens to AST.
- `husk-types`: Type representations.
- `husk-semantic`: Name resolution, type checking, exhaustiveness analysis.
- `husk-codegen-js`: JS and `.d.ts` code generation.
- `husk-runtime-js`: Source and packaging for the JavaScript preamble.
- `husk-cli`: CLI binary entrypoint, wiring all stages together.
- `examples/`: Example programs.
- `tests/`: Integration and end-to-end tests.

This layout can be adjusted as the project evolves, but the separation of concerns should remain.

---

## 4. Language Design (MVP)

### 4.1 Syntax & Features (Initial Subset)

The initial language version will support:

- Modules and files (single module per file to start, no `mod`/`use` syntax yet).
- Function definitions:
  - `fn name<T1, T2>(args) -> Ret { ... }`
- Variable bindings:
  - Immutable: `let x = expr;`
  - Mutable: `let mut x = expr;`
- Control flow:
  - `if` / `else`
  - `while`
  - `break` and `continue` inside `while`
  - `return`
- Types:
  - Primitives: `i32`, `bool`, `String`, `()`.
  - Structs: `struct Name { field: Type, ... }` (record structs only).
  - Enums (including basic generics, e.g. `Option<T>`, `Result<T, E>`), with type erasure at runtime:
    ```rust
    enum Message<T> {
        Quit,
        Move { x: i32, y: i32 },
        Write(T),
    }
    ```
- Pattern matching:
  - `match` on enums with unit, tuple, and struct variants.
  - Exhaustive checking for closed enums.

#### Entry Point

- Conventionally, a Husk module can define:
  ```rust
  fn main() {
      // ...
  }
  ```
- For the MVP, when a zero-argument `main` function is present at the top level of a compiled file, the JavaScript backend will emit a call to `main();` at the end of the generated module so that running the JS file (e.g., under Node) immediately executes the program.

### 4.2 Mutability & Immutability

### 4.3 Mutability & Immutability

- Bindings are immutable by default (`let`), mutable with `let mut`.
- The compiler enforces binding mutability statically.
- In JS output, both immutable and mutable bindings may use `let` or `const` where safe, but mutability is enforced at compile time, not runtime.

### 4.4 Error Handling and Results

- Encourage `Result<T, E>` style error handling.
- Interop with JS/TS functions that may throw:
  - Require extern declarations to return `Result`.
  - Wrap underlying JS calls in `try/catch` and translate to `Ok`/`Err` at the boundary.

---

## 5. JavaScript Code Generation

### 5.1 Representation Strategy

- **Primitives**:
  - `i32` → JS `number`.
  - `bool` → JS `boolean`.
  - `String` → JS `string`.
  - `()` (unit) → JS `undefined` (this matches how `return;` is currently lowered).
- **Structs**:
  - Compile to plain JS objects whose fields mirror the struct fields:
    ```javascript
    const user = { name: "Alice", id: 1 };
    ```
  - Generic structs erase type parameters at runtime; all `Box<T>` instances are just plain objects.
- **Enums**:
  - Individual enum *values* compile to tagged-union objects with a `tag` field and payload fields:
    ```javascript
    // Conceptual shape for values of `enum Message<T> { Quit, Move { x: i32, y: i32 }, Write(T) }`
    { tag: "Quit" }
    { tag: "Move", x, y }
    { tag: "Write", value }
    ```
  - For convenience, codegen may also emit a namespace-like object of constructors:
    ```javascript
    const Message = {
      Quit: { tag: "Quit" },
      Move: (x, y) => ({ tag: "Move", x, y }),
      Write: (value) => ({ tag: "Write", value }),
    };
    ```
  - `Result<T, E>` uses the standard runtime constructors from `std_preamble.js`:
    - `Ok(value)` → `{ tag: "Ok", value }`
    - `Err(error)` → `{ tag: "Err", error }`
  - Generic enums also use type erasure; all instantiations share the same runtime representation.
- **Pattern Matching**:
  - Compile `match` expressions into `switch` or `if/else` chains on `value.tag`, with temporary variables binding payloads for each arm.
  - Where helpful, the `matchEnum(value, handlers)` helper from `std_preamble.js` can be used as a lowering target for simple matches.
- **Functions and Modules**:
  - Functions compile to ordinary JS functions. Higher-order functions are just values of function type.
  - Each Husk source file is compiled as an ES module:
    - Top-level items that are conceptually “public” become `export`ed bindings.
    - Future visibility modifiers (`pub`, etc.) will refine which items are exported.
  - Imported Husk or JS modules are referenced using ES `import` syntax in the generated JS.

### 5.2 Codegen Pipeline

- Typed AST → JS AST:
  - Translate expressions and statements to a JS-oriented representation.
- JS AST → Source:
  - Implement a pretty-printer for stable, readable JS.
- (Optional) Source Maps:
  - Track source positions through IR and JS AST to optionally generate source maps.

### 5.3 Runtime Bundling Strategy

- For the MVP, the JavaScript micro-runtime (`std_preamble.js`) is **inlined** at the top of every generated module:
  - `husk-codegen-js` exposes `JsModule::to_source_with_preamble()`, which prepends the preamble helpers to the emitted JS.
  - `huskc compile` uses this inlined form so compiled output is a single self-contained JS file.
- A separate, shared runtime file (e.g., as an import) can be introduced later if/when bundling and code-reuse become more important than single-file simplicity.

---

## 6. TypeScript Interop

### 6.1 Outbound: Generating `.d.ts` Files

- For each compiled module, generate a `.d.ts` with:
  - Exported function declarations and signatures.
  - Exported structs as TS interfaces.
  - Exported enums as discriminated unions that mirror the runtime tagged-union encoding
    `{ tag: "Variant", ... }` (including payload fields).
- Type mapping examples:
  - `i32` → `number`
  - `bool` → `boolean`
  - `String` → `string`

### 6.2 Inbound: Consuming `.d.ts` Files (Later Phase)

- Use TS compiler APIs or a Rust TS parser (e.g. via `swc`) to parse `.d.ts`.
- Translate a subset of TS declarations into language externs:
  - Functions with simple, non-generic signatures.
  - Interfaces and type aliases that correspond to simple structs/unions.
- Assumptions:
  - Imported types are non-linear, shared, and may be mutable.
  - Advanced TS features are initially ignored or approximated.

---

## 7. Runtime and Standard Library

### 7.1 Micro-Runtime (`std_preamble.js`)

- Provide small, reusable helpers:
  - `Ok` and `Err` constructors for `Result`-like values, encoded as tagged unions
    `{ tag: "Ok", value }` and `{ tag: "Err", error }`.
  - A simple `match` helper (`matchEnum`) for runtime pattern matching on tagged unions.
  - `panic` implementation (throwing an error or similar).
- Ensure the runtime is:
  - Small (goal: under ~1 KB minified).
  - Stable over time.
  - Backwards compatible across language versions when possible.

### 7.2 Standard Library (Language Side)

- Core types and utilities:
  - Option-like enums (`Option<T>`).
  - Result-like enums (`Result<T, E>`), unless modeled via runtime only.
- Interop wrappers:
  - Safe wrappers for selected JS/Node/Web APIs, returning `Result` where appropriate.

### 7.3 Runtime Diagnostics (Future)

- Improve runtime error reporting for `panic` and `matchEnum`:
  - Preserve or surface richer stack traces from the host JS engine.
  - Eventually integrate with optional source maps or other metadata so that runtime panics can be mapped back to Husk source locations.
- This is intentionally deferred until after the core language, codegen, and basic tooling are stable.

---

## 8. Tooling and Developer Experience

### 8.1 CLI (`lang-cli`)

- Commands:
  - `langc compile main.lang -o main.js`
  - `langc check main.lang` (no codegen).
  - (Later) `langc fmt`, `langc new`, `langc test`.
- Flags:
  - `--emit-dts` to control `.d.ts` emission (on by default).
  - `--emit-runtime` options for bundling or referencing the JS preamble.

### 8.2 LSP and Editor Integration (Later)

- Implement an LSP server in Rust:
  - Real-time diagnostics.
  - Go-to-definition and find references.
  - Hover information (types and documentation).

### 8.3 Formatter

- A simple, deterministic formatter for the language syntax.
- Eventually integrate it as `langc fmt` and/or LSP formatting.

---

## 9. Phased Roadmap Overview

The roadmap is tracked in more detail in `roadmap.md`. This section summarizes the phases.

### Phase 1 – Minimal Language Core

- Define the subset of Rust-like syntax for the MVP.
- Implement lexer and parser.
- Provide basic diagnostics.
- Implement a CLI that parses and reports syntax errors.

### Phase 2 – Type System & Generics

- Implement core type system and name resolution, including basic generics for structs, enums, and functions (type-erased at runtime).
- Implement type checking for primitives, structs, enums, function calls/returns, and `match` exhaustiveness.

### Phase 3 – JavaScript Backend & Runtime

- Design JS representation for all core types.
- Implement JS codegen directly from the typed AST.
- Implement `std_preamble.js` (micro-runtime) with enum/result helpers and panic utilities.
- Implement shim generation for `extern` functions to wrap JS calls in `try/catch` and return `Result` values.
- Generate runnable ES modules and verify end-to-end examples using Node and the micro-runtime.

### Phase 4 – TypeScript Interop

- Implement `.d.ts` emission.
- Support manual extern declarations with JS/TS interop.
- Later, ingest `.d.ts` to generate externs.

### Phase 5 – Standard Library & Ecosystem

- Design and implement a minimal standard library in the language.
- Refine and version the runtime and standard library APIs as the ecosystem grows.

### Phase 6 – Tooling & Developer Experience

- Enhance CLI commands.
- Add formatter and basic LSP support.

### Phase 7 – Hardening & Future Extensions

- Extend and refine the type system (richer generics support, traits/interfaces).
- Optimize performance and JS output (potentially introducing an IR for advanced optimizations).
- Improve diagnostics and developer experience.

---

## 10. Next Steps (Implementation Kickoff)

Immediate next actions:

1. Initialize the Rust workspace with the crates listed in section 3.
2. Implement the lexer and parser for the MVP syntax.
3. Add CLI commands (e.g., `huskc check`) to compile sources through the frontend.
4. Start building a small suite of example programs and tests to validate parsing and basic analysis.
