# Husk Compiler: Next Steps Plan

**Date**: November 29, 2025
**Status**: Planning Complete
**Detail Level**: A LOT (Comprehensive)

---

## Executive Summary

The Husk compiler is in a **remarkably complete MVP state** for its stated goals—a Rust-syntax language that compiles to JavaScript with TypeScript interop. Phases 1-4 are substantially complete (~6,164 lines of Rust across 8 crates), with working lexer, parser, type system, generics, exhaustiveness checking, JS codegen, and TypeScript declaration emission.

However, **critical gaps prevent real-world usage**:

1. **Stdlib exists but can't be imported** (`stdlib/core.hk` defines `Option<T>` and `Result<T, E>` but there's no prelude or module system to expose them)
2. **Integration test pipeline is broken** (`cargo test -p husk-cli --test node_examples` currently fails with `failed to resolve workspace root: No such file or directory`)
3. **ESM/CJS target is implicit** (single-file programs without imports still emit CommonJS; there is no `--target` flag to force ESM for Bun/Deno/browser)
4. **No multi-file compilation** (each file is isolated)
5. **No project scaffolding or tooling** (high friction for new users)

This plan prioritizes unblocking real applications by completing the module system and adding essential tooling.

---

## Current State Assessment

### What's Complete (Phases 1-5)

| Component | Status | Lines | Key Features |
|-----------|--------|-------|--------------|
| `husk-ast` | ✅ | 286 | Expressions, statements, items, patterns |
| `husk-lexer` | ✅ | 480 | UTF-8 tokenizer with spans |
| `husk-parser` | ✅ | 1,427 | Recursive descent, error reporting |
| `husk-types` | ✅ | 107 | Primitives, named types, generics |
| `husk-semantic` | ✅ | 1,551 | Type checking, exhaustiveness, name resolution |
| `husk-codegen-js` | ✅ | 1,327 | JS AST, pretty printer, .d.ts emission |
| `husk-runtime-js` | ✅ | 58 | Ok/Err/panic/matchEnum helpers |
| `husk-cli` | ✅ | 285 | parse/check/compile/import-dts commands |

### What's Broken or Incomplete

| Issue | Severity | Details |
|-------|----------|---------|
| Stdlib import | 🔴 CRITICAL | `stdlib/core.hk` exists but no prelude/import path for `Option`/`Result` |
| Integration tests | 🔴 CRITICAL | `cargo test -p husk-cli --test node_examples` fails: `failed to resolve workspace root (No such file or directory)` |
| ESM target selection | 🟡 HIGH | ESM exports only when imports exist; no `--target` flag, single-file programs stuck on CommonJS |
| Multi-file compilation | 🟡 HIGH | No `use` or project module graph |
| Project scaffolding | 🟠 MEDIUM | No `huskc new`, no templates |
| Testing story | 🟠 MEDIUM | No `huskc test`, no test framework design |
| LSP/Editor support | ⚪ LOW | Not started |

---

## Proposed Implementation Plan

### Phase A: Stabilize Single-File Tooling (Weeks 1-2)

**Goal**: Make Husk usable for single-file programs with stdlib and npm packages.

#### A.1 Unblock Integration Tests (workspace_root)

**Problem**: `cargo test -p husk-cli --test node_examples` currently fails with  
`failed to resolve workspace root: No such file or directory` because `workspace_root()` canonically resolves `crates/husk-cli/../../` and blows up when paths are symlinked or missing (reproduced 2025-11-29).

**Files to modify**:
- `crates/husk-cli/tests/node_examples.rs`

**Implementation**:
1. Replace `canonicalize()` with a tolerant search that walks `current_dir()` ancestors until it finds `Cargo.toml` (or use `dunce::canonicalize` as a fallback).
2. Keep the Node-availability guard untouched.
3. Add a targeted unit test for `workspace_root()` if feasible (pure function).

**Test**:
```bash
cargo test -p husk-cli --test node_examples -- --nocapture
```
Should pass from a fresh checkout (no symlink assumptions).

#### A.2 Implement Stdlib Prelude

**Problem**: `Option<T>` and `Result<T, E>` defined in `stdlib/core.hk` but can't be imported.

**Solution (MVP)**: Auto-inject stdlib types into every compilation unit.

**Files to modify**:
- `crates/husk-semantic/src/lib.rs` (add prelude injection)
- `crates/husk-cli/src/main.rs` (add `--no-prelude` flag for edge cases)

**Implementation**:
1. Parse `stdlib/core.hk` at compile time (embed via `include_str!`)
2. Inject parsed items into symbol table before user code
3. User definitions can shadow prelude (like Rust)

**Test**:
```husk
// examples/use_option.hk
fn main() {
    let x: Option<i32> = Some(42);
    match x {
        Some(val) => println(val),
        None => (),
    }
}
```

Should compile without explicit import.

#### A.3 Add Explicit Target Flag for ESM/CJS

**Problem**: ESM exports are only emitted when imports exist; import-less programs still produce CommonJS and there is no `--target` flag to force ESM for Bun/Deno/browser workflows.

**Solution**: Add `--target` flag with `esm` and `cjs` options; honor it regardless of whether imports are present.

**Files to modify**:
- `crates/husk-cli/src/main.rs` (add flag)
- `crates/husk-codegen-js/src/lib.rs` (import/exports conditional on target)

**ESM Output (target = esm)**:
```javascript
export function add(x, y) {
    return x + y;
}

export function main() {
    // ...
}
```

**CommonJS Output (target = cjs)**:
```javascript
function add(x, y) {
    return x + y;
}

function main() {
    // ...
}

module.exports = { add, main };
```

**Tests**:
- `cargo test -p husk-codegen-js generates_esm_imports_for_extern_mod_declarations`
- Compile a no-import sample (`fn main() {}`) with `--target esm` and run with `node --input-type=module`.

---

### Phase B: Multi-File Projects (Weeks 3-4)

**Goal**: Enable real project structure with multiple Husk files.

#### B.1 Design Module Syntax

**Proposed Syntax** (follow Rust conventions):
```husk
// src/types.hk
pub struct User {
    id: i32,
    name: String,
}

pub enum Status {
    Active,
    Inactive,
}

// src/utils.hk
use crate::types::{User, Status};

pub fn is_active(u: User) -> bool {
    match u.status {
        Status::Active => true,
        Status::Inactive => false,
    }
}

// src/main.hk
use crate::types::User;
use crate::utils::is_active;

fn main() {
    let user = User { id: 1, name: "Alice" };
    if is_active(user) {
        // ...
    }
}
```

**Key Decisions**:
- `crate::` refers to project root
- `use` brings items into scope
- `pub` exports items from module
- Each file is a module (like Rust)

#### B.2 Implement Module Resolution

**Files to create**:
- `crates/husk-modules/` (new crate)

**Components**:
1. **Module graph builder**: Discover all `.hk` files in project
2. **Import resolver**: Resolve `use crate::foo` to file paths
3. **Compilation ordering**: Topological sort for dependency order

**Algorithm**:
```
1. Start from entry file (e.g., src/main.hk)
2. Parse and collect all `use` declarations
3. Resolve each import to a file path
4. Parse dependent files recursively
5. Build dependency graph
6. Check for cycles (error if found)
7. Compile in topological order
8. Link into single output or multiple modules
```

#### B.3 Project Configuration

**Proposed `husk.toml`**:
```toml
[package]
name = "my-project"
version = "0.1.0"
entry = "src/main.hk"

[output]
target = "esm"          # esm | cjs
dir = "dist"

[dependencies]
# Future: Husk package registry
```

**Alternative**: Just use `package.json` with scripts:
```json
{
  "scripts": {
    "build": "huskc compile src/main.hk --lib -o dist/main.js",
    "start": "node dist/host.js"
  }
}
```

For MVP, prefer the `package.json` approach to avoid new tooling.

---

### Phase C: Developer Experience (Weeks 5-6)

**Goal**: Make Husk pleasant to use day-to-day.

#### C.1 Project Scaffolding (`huskc new`)

**Templates**:

1. **`cli-app`**: Simple command-line program
   ```
   my-cli/
   ├── src/
   │   └── main.hk
   ├── package.json
   └── README.md
   ```

2. **`web-server`**: Express-based HTTP server
   ```
   my-server/
   ├── src/
   │   ├── main.hk
   │   └── handlers.hk
   ├── host.js
   ├── package.json
   └── README.md
   ```

3. **`library`**: Reusable Husk library
   ```
   my-lib/
   ├── src/
   │   └── lib.hk
   ├── package.json
   └── README.md
   ```

**Command**:
```bash
huskc new my-project --template web-server
```

#### C.2 Error Message Improvements

**Current**: Basic error messages with spans.

**Improved**: Rust-style rich errors with context.

**Example**:
```
error[E0001]: non-exhaustive match
  --> src/main.hk:15:5
   |
15 |     match status {
   |     ^^^^^ patterns `Status::Pending` and `Status::Cancelled` not covered
   |
   = help: ensure all variants are covered, or add a wildcard pattern `_`
```

**Files to modify**:
- `crates/husk-semantic/src/lib.rs` (improve error types)
- Add `codespan-reporting` or similar crate for formatting

#### C.3 Watch Mode

**Command**:
```bash
huskc watch src/main.hk -o dist/main.js
```

**Implementation**:
- Use `notify` crate for file watching
- Recompile on `.hk` file changes
- Report errors inline

#### C.4 Testing Framework Design

**Proposed Syntax**:
```husk
// tests/math_test.hk
use crate::math::add;

#[test]
fn test_add() {
    assert_eq(add(2, 3), 5);
}

#[test]
fn test_add_negative() {
    assert_eq(add(-1, 1), 0);
}
```

**Command**:
```bash
huskc test
```

**Implementation**:
- Discover test files (`*_test.hk` or `tests/*.hk`)
- Generate test harness that calls each `#[test]` function
- Report pass/fail with source locations

---

### Phase D: Ecosystem Integration (Weeks 7-8)

**Goal**: Integrate with existing JavaScript tooling.

#### D.1 Improved .d.ts Import

**Current**: Minimal parser that handles basic function signatures.

**Enhanced**: Support for:
- Interfaces → Husk structs
- Classes → Husk structs + extern methods
- Generics (basic)
- Union types → Husk enums (with manual mapping)

**Example**:
```typescript
// express.d.ts
interface Request {
    params: Record<string, string>;
    query: Record<string, string>;
}

export function express(): Application;
```

Generates:
```husk
// express_externs.hk
struct Request {
    params: JsObject,
    query: JsObject,
}

extern "js" {
    mod express;
    fn express() -> Application;
}
```

#### D.2 Source Maps

**Implementation**:
- Track source positions through codegen
- Generate `.js.map` files alongside output
- Use `sourcemap` crate

**Benefit**: Stack traces point to Husk source, not generated JS.

#### D.3 npm Package Publishing Guide

**Documentation** for publishing Husk libraries to npm:
```
1. Compile with `huskc compile src/lib.hk --lib --emit-dts -o dist/index.js`
2. Configure package.json:
   {
     "main": "dist/index.js",
     "types": "dist/index.d.ts"
   }
3. npm publish
```

---

## Gap Analysis and Recommendations

### Critical Gaps (Block Real Usage)

| Gap | Impact | Recommended Fix | Priority |
|-----|--------|-----------------|----------|
| No stdlib import | Can't use Option/Result | Prelude injection in Phase A.2 | 🔴 P0 |
| Integration tests broken | CI/CD unreliable | Fix workspace_root in Phase A.1 | 🔴 P0 |
| ESM target implicit | ESM-only hosts blocked for import-less programs | Add target flag in Phase A.3 | 🟡 P1 |

### Important Gaps (Affect Usability)

| Gap | Impact | Recommended Fix | Priority |
|-----|--------|-----------------|----------|
| No multi-file projects | Can't build real apps | Module system in Phase B | 🟡 P1 |
| No project scaffolding | High friction for new users | `huskc new` in Phase C.1 | 🟡 P1 |
| Poor error messages | Hard to debug issues | Improve in Phase C.2 | 🟠 P2 |
| No testing story | Can't verify code | Design in Phase C.4 | 🟠 P2 |

### Nice-to-Have (Future Enhancement)

| Gap | Impact | Recommended Fix | Priority |
|-----|--------|-----------------|----------|
| No watch mode | Dev workflow friction | Add in Phase C.3 | ⚪ P3 |
| No source maps | Debugging harder | Add in Phase D.2 | ⚪ P3 |
| No LSP | No IDE support | Phase 6 (future) | ⚪ P4 |
| No formatter | Inconsistent code style | Phase 6 (future) | ⚪ P4 |

---

## New Ideas and Language Enhancements

### Idea 1: `println!` Macro

**Problem**: No easy way to print debug output.

**Solution**: Add a `println!` macro that compiles to `console.log`:

```husk
fn main() {
    let x = 42;
    println!("x = {}", x);  // Outputs: x = 42
}
```

**Implementation**: Macro expansion during parsing or semantic analysis.

### Idea 2: Async/Await (Post-MVP)

**Problem**: Many JS APIs are async (fetch, file I/O, etc.).

**Solution**: Add `async` functions and `await` expressions:

```husk
async fn fetch_data() -> Result<String, Error> {
    let response = await fetch("https://api.example.com/data")?;
    let text = await response.text()?;
    Ok(text)
}
```

**Compilation**: Async functions compile to JS async functions.

### Idea 3: Derive Macros (Post-MVP)

**Problem**: Boilerplate for common patterns (Debug, Clone, PartialEq).

**Solution**: Derive macros like Rust:

```husk
#[derive(Debug, Clone)]
struct User {
    id: i32,
    name: String,
}
```

### Idea 4: Linear Types for Resources

**Problem**: No way to ensure resources are properly cleaned up.

**Solution**: Opt-in linearity with `linear` keyword:

```husk
linear struct FileHandle {
    fd: i32,
}

fn process_file() {
    let file = open("data.txt")?;
    // file MUST be consumed before scope ends
    close(file);  // OK: file consumed
}

fn bad_process() {
    let file = open("data.txt")?;
    // ERROR: file not consumed, would leak!
}
```

### Idea 5: Pattern Matching in Let

**Problem**: Only `match` supports patterns.

**Solution**: Allow patterns in `let`:

```husk
let Some(value) = maybe_value else {
    return Err("No value");
};

let User { id, name } = get_user();
```

---

## Example Programs to Validate Implementation

### Example 1: Real Express Server (Phase A validation)

```husk
// examples/express_server.hk
extern "js" {
    mod express;
}

struct User {
    id: i32,
    name: String,
}

fn main() {
    let app = express();

    app.get("/", |req, res| {
        res.send("Hello from Husk!");
    });

    app.get("/users/:id", |req, res| {
        let id = req.params.get("id");
        match id {
            Some(id_str) => {
                res.json({ "id": id_str, "name": "Alice" });
            },
            None => {
                res.status(400).send("Missing id");
            }
        }
    });

    app.listen(3000, || {
        println!("Server running on port 3000");
    });
}
```

**What it tests**:
- Real npm module import (not globalThis wrapper)
- Closure callbacks
- Option handling from extern calls
- Method chaining on extern objects

### Example 2: Multi-File Library (Phase B validation)

```husk
// src/types.hk
pub enum Color {
    Red,
    Green,
    Blue,
    Custom(i32, i32, i32),
}

pub struct Theme {
    primary: Color,
    secondary: Color,
}

// src/utils.hk
use crate::types::{Color, Theme};

pub fn default_theme() -> Theme {
    Theme {
        primary: Color::Blue,
        secondary: Color::Custom(128, 128, 128),
    }
}

pub fn color_to_hex(color: Color) -> String {
    match color {
        Color::Red => "#FF0000",
        Color::Green => "#00FF00",
        Color::Blue => "#0000FF",
        Color::Custom(r, g, b) => format!("#{:02X}{:02X}{:02X}", r, g, b),
    }
}

// src/main.hk
use crate::types::Theme;
use crate::utils::{default_theme, color_to_hex};

fn main() {
    let theme = default_theme();
    println!("Primary: {}", color_to_hex(theme.primary));
    println!("Secondary: {}", color_to_hex(theme.secondary));
}
```

**What it tests**:
- Multi-file compilation
- Module imports with `use`
- Visibility with `pub`
- Cross-module type usage
- Generic enum variants

### Example 3: CLI Tool with Error Handling (Stdlib validation)

```husk
// examples/word_count.hk
extern "js" {
    mod fs;
}

fn main() -> Result<(), String> {
    let args = env_args();

    let path = match args.get(1) {
        Some(p) => p,
        None => return Err("Usage: word_count <file>"),
    };

    let content = fs.read_file_sync(path)
        .map_err(|e| format!("Failed to read {}: {}", path, e))?;

    let words = content.split_whitespace().count();
    let lines = content.lines().count();
    let chars = content.len();

    println!("{} words, {} lines, {} characters", words, lines, chars);
    Ok(())
}
```

**What it tests**:
- `Result` error handling with `?` operator
- `Option` handling from collections
- `.map_err()` for error transformation
- Extern function calls returning Result
- `main() -> Result<T, E>` pattern

### Example 4: Generic Data Structure (Type system validation)

```husk
// examples/stack.hk
enum StackNode<T> {
    Empty,
    Node { value: T, next: Box<StackNode<T>> },
}

struct Stack<T> {
    top: StackNode<T>,
    size: i32,
}

fn new_stack<T>() -> Stack<T> {
    Stack {
        top: StackNode::Empty,
        size: 0,
    }
}

fn push<T>(stack: Stack<T>, value: T) -> Stack<T> {
    Stack {
        top: StackNode::Node {
            value: value,
            next: Box::new(stack.top),
        },
        size: stack.size + 1,
    }
}

fn pop<T>(stack: Stack<T>) -> Option<(T, Stack<T>)> {
    match stack.top {
        StackNode::Empty => None,
        StackNode::Node { value, next } => Some((value, Stack {
            top: *next,
            size: stack.size - 1,
        })),
    }
}

fn main() {
    let stack = new_stack::<i32>();
    let stack = push(stack, 1);
    let stack = push(stack, 2);
    let stack = push(stack, 3);

    match pop(stack) {
        Some((val, rest)) => println!("Popped: {}", val),  // 3
        None => println!("Empty!"),
    }
}
```

**What it tests**:
- Generic structs and enums
- Recursive data structures
- Box heap allocation (if supported)
- Pattern matching on nested generics
- Type parameter inference

---

## Acceptance Criteria

### Phase A Complete When:

- [ ] `cargo test -p husk-cli --test node_examples` passes from a fresh checkout (workspace_root no longer panics)
- [ ] `Option<T>` and `Result<T, E>` are available without explicit import; `--no-prelude` opts out
- [ ] `huskc compile --target esm` emits runnable ESM even when the program has zero imports; `--target cjs` still works
- [ ] `extern "js" { mod express; }` lowers to `import` (esm) or `require` (cjs) per target, and the real Express example runs with the chosen target

### Phase B Complete When:

- [ ] Multi-file project compiles with `use crate::module::Item` imports
- [ ] `pub` visibility exports items from modules
- [ ] Circular import detection with helpful error message
- [ ] Example multi-file library compiles and runs correctly

### Phase C Complete When:

- [ ] `huskc new my-project --template web-server` creates working project
- [ ] Error messages include source context and suggestions
- [ ] `huskc watch` recompiles on file changes
- [ ] Test framework design documented (implementation optional)

### Phase D Complete When:

- [ ] Enhanced .d.ts importer handles interfaces and classes
- [ ] Source maps generated alongside JS output
- [ ] npm publishing guide documented with working example

---

## Success Metrics

### Usability Metrics

| Metric | Target | How to Measure |
|--------|--------|----------------|
| Time to first program | < 5 min | User testing with `huskc new` |
| Compile error resolution | < 30 sec avg | Track common error patterns |
| Real app build time | < 2 sec | Benchmark with example projects |

### Quality Metrics

| Metric | Target | How to Measure |
|--------|--------|----------------|
| Integration test pass rate | 100% | CI pipeline |
| Example programs working | 5+ real-world examples | Manual verification |
| Generated JS readability | Human-reviewable | Code review of output |

### Adoption Metrics

| Metric | Target (6 months) | How to Measure |
|--------|-------------------|----------------|
| GitHub stars | 500+ | GitHub insights |
| Example projects | 10+ community examples | Search GitHub |
| npm downloads (future) | 100+/week | npm stats |

---

## Dependencies & Risks

### Technical Dependencies

| Dependency | Required For | Risk | Mitigation |
|------------|--------------|------|------------|
| Node.js 18+ | Running compiled code | Low | Document requirement |
| TypeScript (optional) | Validating .d.ts | Low | Skip if not installed |
| npm packages | Interop testing | Medium | Use stubs for tests |

### Technical Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Module resolution complexity | Medium | High | Start with simple flat resolution |
| Breaking changes to JS runtime | Low | High | Version runtime, document compatibility |
| ESM/CJS interop issues | Medium | Medium | Test both thoroughly |
| Performance regression | Low | Medium | Add benchmarks early |

### Timeline Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Scope creep | High | Medium | Strict phase gates |
| Underestimated complexity | Medium | High | Time-box each phase |
| Competing priorities | Medium | Medium | Document minimum viable scope for each phase |

---

## Documentation Plan

### New Documentation Needed

1. **Getting Started Guide** (Phase A)
   - Installation
   - Hello World
   - Using stdlib types
   - First real program

2. **JavaScript Interop Guide** (Phase A)
   - Manual extern declarations
   - Importing .d.ts
   - The host pattern
   - Handling errors from JS

3. **Project Structure Guide** (Phase B)
   - Multi-file projects
   - Module syntax
   - Visibility rules
   - Build configuration

4. **API Reference** (Phase C)
   - Stdlib documentation
   - Built-in types
   - Compiler flags

5. **npm Publishing Guide** (Phase D)
   - Library compilation
   - TypeScript declarations
   - Package.json setup

### Existing Documentation Updates

- [ ] Update `README.md` with quick start
- [ ] Update `roadmap.md` with Phase A-D details
- [ ] Update `plan.md` to reflect current state
- [ ] Add `CONTRIBUTING.md` for contributors

---

## References & Research

### Internal References

- Architecture: `/Volumes/External/code-external/husk/plan.md`
- Decisions: `/Volumes/External/code-external/husk/decisions.md`
- Roadmap: `/Volumes/External/code-external/husk/roadmap.md`
- Syntax: `/Volumes/External/code-external/husk/syntax.md`
- Codegen: `/Volumes/External/code-external/husk/crates/husk-codegen-js/src/lib.rs`
- Semantic: `/Volumes/External/code-external/husk/crates/husk-semantic/src/lib.rs`
- CLI: `/Volumes/External/code-external/husk/crates/husk-cli/src/main.rs`

### External References

- [ReScript Compiler](https://github.com/rescript-lang/rescript-compiler) - Similar goals, production-ready
- [PureScript FFI](https://github.com/purescript/documentation/blob/master/language/FFI.md) - FFI patterns
- [SWC Parser](https://swc.rs/) - TypeScript parsing in Rust
- [Oxc Parser](https://oxc.rs/) - Fast JS parsing in Rust
- [Node.js ESM Docs](https://nodejs.org/api/esm.html) - ESM specification
- [bumpalo Arena](https://docs.rs/bumpalo) - Arena allocation for ASTs

### Related Issues/PRs

- Latest commit: `bd73d41` (npm module imports)
- Previous: `56079bc` (.d.ts validation)

---

## Summary

The Husk compiler has made impressive progress but needs focused work on:

1. **Immediate** (Phase A): Fix module codegen, add stdlib prelude, ESM output
2. **Short-term** (Phase B): Enable multi-file projects
3. **Medium-term** (Phase C): Developer experience improvements
4. **Long-term** (Phase D): Ecosystem integration

The core language and type system are solid. The remaining work is primarily about making Husk practical for real-world use. Completing Phase A alone would make Husk usable for building actual applications with npm packages.
