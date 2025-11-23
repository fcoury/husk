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
  - [x] Generic enums (e.g. `Option<T>`, `Result<T, E>`). (_match expressions not yet parsed; exhaustiveness to be added when `match` lands in the parser._)
  - [x] Function calls and returns.
  - [ ] `match` exhaustiveness on enums.
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
- [ ] Implement codegen from typed AST to JS AST and then to source.
- [ ] Design contents and API surface of `std_preamble.js`.
- [ ] Implement micro-runtime helpers:
  - [ ] `Ok` / `Err` constructors.
  - [ ] Optional `match` helper.
  - [ ] `panic` / assertion utilities.
- [ ] Implement shim generator for `extern` functions (auto-wrap calls in `try/catch` and return `Result`).
- [ ] Decide bundling strategy for runtime (inline vs. separate file).
- [ ] Integrate codegen and runtime into CLI (`huskc compile`).
- [ ] Create small E2E examples that compile and run under Node using `std_preamble.js`.

---

## Phase 4 – TypeScript Interop

### 4.1 Outbound: `.d.ts` Emission

- [ ] Define mapping from language types to TS types.
- [ ] Implement `.d.ts` emitter for:
  - [ ] Exported functions.
  - [ ] Exported structs.
  - [ ] Exported enums (discriminated unions).
- [ ] Integrate `.d.ts` generation into CLI workflow.
- [ ] Validate `.d.ts` by using compiled modules from a TS project.

### 4.2 Inbound: Consuming `.d.ts` (Later)

- [ ] Select approach for parsing `.d.ts` (TS compiler API or Rust TS parser).
- [ ] Implement parser and translator from `.d.ts` to internal type representations.
- [ ] Implement generator for language `extern` declarations from imported `.d.ts`.
- [ ] Decide on conservative assumptions for imported types (non-linear, shared, mutable).
- [ ] Validate by importing a small subset of real-world libraries.

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
  - [ ] Suggestions and quick fixes (e.g., in LSP).
- [ ] Ecosystem:
  - [ ] Package publishing story (e.g., npm packages).
  - [ ] CI templates and recommended workflows.
  - [ ] Real-world example projects and benchmarks.

---

## Meta

- [ ] Periodically revisit `plan.md` to align roadmap with evolving goals.
- [ ] Keep this roadmap up to date as tasks start and complete.
