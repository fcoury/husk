# Project Roadmap

This roadmap tracks implementation progress across phases. Update this file as tasks are started and completed.

Use `[ ]` for pending items and `[x]` for completed items.

---

## Phase 1 – Minimal Language Core

- [x] Decide definitive MVP syntax subset.
- [x] Define token set and trivia handling.
- [x] Implement lexer (`husk-lexer`).
- [x] Implement parser (`husk-parser`) for:
  - [x] Expressions.
  - [x] Statements.
  - [x] Function items.
  - [x] Struct and enum definitions.
- [x] Implement AST data structures (`husk-ast`).
- [x] Implement basic diagnostics (errors with spans, pretty-printing).
- [x] Add CLI command to parse and report syntax errors (`husk-cli`).
- [x] Create example programs that parse without errors.

---

## Phase 2 – Type System & Generics

- [x] Define core type language and representation (`husk-types`).
- [x] Implement basic generics for structs, enums, and functions using type erasure (no monomorphization).
- [x] Implement symbol tables and name resolution (`husk-semantic`).
- [x] Implement type checking for:
  - [x] Primitive types.
  - [x] Structs (including generic structs).
  - [x] Generic enums (e.g. `Option<T>`, `Result<T, E>`).
  - [x] Function calls and returns.
  - [x] `match` exhaustiveness on enums.
- [x] Extend CLI with `check` mode that runs full semantic analysis.

---

## Phase 3 – JavaScript Backend & Runtime

- [x] Design JS representation for:
  - [x] Primitives.
  - [x] Structs.
  - [x] Enums (tagged unions).
  - [x] Functions and modules (ES modules).
- [x] Implement JS AST types and builders (`husk-codegen-js`).
  - [x] Basic expressions, functions, and statements.
  - [x] Object literals and property access for struct/enum representations.
- [x] Implement codegen from typed AST to JS AST and then to source.
- [x] Design contents and API surface of `std_preamble.js`.
- [x] Implement micro-runtime helpers:
  - [x] `Ok` / `Err` constructors.
  - [x] Optional `match` helper.
  - [x] `panic` / assertion utilities.
- [x] Implement shim generator for `extern` functions (auto-wrap calls in `try/catch` and return `Result`).
- [x] Decide bundling strategy for runtime (inline vs. separate file).
- [x] Integrate codegen and runtime into CLI (`huskc compile`).
- [x] Create small E2E examples that compile and run under Node using `std_preamble.js`.

---

## Phase 4 – TypeScript Interop

### 4.1 Outbound: `.d.ts` Emission

- [x] Define mapping from language types to TS types.
- [x] Implement `.d.ts` emitter for:
  - [x] Exported functions.
  - [x] Exported structs.
  - [x] Exported enums (discriminated unions).
- [x] Integrate `.d.ts` generation into CLI workflow.
- [x] Validate `.d.ts` by using compiled modules from a TS project.

### 4.2 Inbound: Consuming `.d.ts` (Later)

- [x] Select approach for parsing `.d.ts` (minimal custom Rust parser for a narrow subset).
- [x] Implement parser and translator from `.d.ts` to Husk `extern "js"` declarations.
- [x] Implement generator for language `extern` declarations from imported `.d.ts` (via `huskc import-dts`).
- [x] Decide on conservative assumptions for imported types (treated as ordinary, shared JS values in the MVP).
- [ ] Validate by importing a small subset of real-world libraries.

### 4.3 Minimal JS Library Interop (Express MVP)

- [ ] Design minimal Husk-facing Express surface:
  - [ ] Define opaque types for `ExpressApp`, `Request`, and `Response`.
  - [ ] Define `extern "js"` entrypoint for `express()` returning `ExpressApp`.
- [ ] Implement a Husk example that:
  - [ ] Calls `express()` to obtain an app instance.
  - [ ] Registers at least one route via `app.get("/path", handler)`.
  - [ ] Defines a handler function that uses `Request`/`Response` types and calls methods like `res.send(...)`.
- [ ] Extend Node integration tests to:
  - [ ] Compile the minimal Express example to JS.
  - [ ] Prepend a small stub `express()` implementation (no real `express` dependency).
  - [ ] Execute the resulting JS under Node and assert successful exit.

---

## Phase 5 – Standard Library & Ecosystem

- [ ] Implement standard library modules in the language:
  - [ ] Core types (Option, Result, etc., if not built-in).
- [ ] Ensure versioning and compatibility strategy for the runtime.

---

## Phase 6 – Tooling & Developer Experience

- [ ] Extend CLI:
  - [ ] `huskc fmt` (formatter).
  - [ ] `huskc new` (project template).
  - [ ] `huskc test` (test harness, if added).
- [ ] Implement or integrate a source formatter for the language.
- [ ] Implement an LSP server (basic version):
  - [ ] Real-time diagnostics.
  - [ ] Go-to-definition.
  - [ ] Hover type information.
- [ ] Add editor configuration and example setups (VS Code, etc.).

---

## Phase 7 – Hardening & Future Extensions

- [ ] Extend type system:
  - [ ] Richer generics support (constraints, trait bounds, etc.).
  - [ ] Trait-like abstractions or interfaces.
  - [ ] More advanced inference (where beneficial).
- [ ] Performance and optimization:
  - [ ] If an IR is introduced, IR-level optimizations (DCE, constant folding, inlining).
  - [ ] Output-level improvements for JS engines (e.g., predictable object shapes).
- [ ] Diagnostics and UX:
  - [ ] More precise, actionable error messages.
  - [ ] Improved runtime panic/match error reporting (better stack traces and, eventually, mapping back to Husk source).
  - [ ] Suggestions and quick fixes (e.g., in LSP).
- [ ] Ecosystem:
  - [ ] Package publishing story (e.g., npm packages).
  - [ ] CI templates and recommended workflows.
  - [ ] Real-world example projects and benchmarks.

---

## Meta

- [ ] Periodically revisit `plan.md` to align roadmap with evolving goals.
- [ ] Keep this roadmap up to date as tasks start and complete.
