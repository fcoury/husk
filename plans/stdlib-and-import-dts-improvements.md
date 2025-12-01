# Stdlib JS Builtins & import-dts CLI Improvements

## Overview

Two improvements to reduce boilerplate in Husk programs that use JavaScript FFI:

1. **Auto-inject JS runtime type declarations** - Users shouldn't manually declare `JsValue`, `jsvalue_get()`, etc.
2. **Document & test `import-dts` command** - The command exists but lacks documentation and tests

## Problem Statement

Users currently write 50+ lines of extern declarations in every file that uses JavaScript interop:

```husk
// This is what users have to write TODAY in every file:
extern "js" {
    struct JsValue;
    struct JsObject;
    fn JsObject_new() -> JsObject;
    fn jsvalue_get(obj: JsValue, key: String) -> JsValue;
    fn jsvalue_isNull(obj: JsValue) -> bool;
    // ... 20+ more declarations
}

impl JsObject {
    extern "js" fn setString(self, key: String, value: String) -> JsObject;
    // ... more methods
}
```

**The irony**: These functions already exist in the JS runtime preamble (`crates/husk-runtime-js/src/lib.rs:209-301`). Users are just re-declaring what's already there.

## Key Discovery: `std/js/globals.hk` Already Exists!

The file `/Volumes/External/code-external/husk/std/js/globals.hk` already contains these declarations but **isn't being auto-injected**. The solution is simpler than creating a new file.

## Proposed Solution

### Phase 1: Auto-Inject `std/js/globals.hk` via Prelude System

**Files to modify:**

1. `crates/husk-semantic/src/lib.rs` - Add JS globals injection

**Implementation:**

```rust
// crates/husk-semantic/src/lib.rs

// Around line 119, add alongside existing prelude:
static JS_GLOBALS_SRC: &str = include_str!("../../../std/js/globals.hk");
static JS_GLOBALS_AST: OnceLock<File> = OnceLock::new();

fn js_globals_file() -> &'static File {
    JS_GLOBALS_AST.get_or_init(|| {
        let parsed = parse_str(JS_GLOBALS_SRC);
        if !parsed.errors.is_empty() {
            panic!("failed to parse JS globals: {:?}", parsed.errors);
        }
        parsed.file.expect("JS globals parse produced no AST")
    })
}

// Around line 99, in analyze_file_with_options():
if opts.prelude {
    checker.build_type_env(prelude_file());
    checker.build_type_env(js_globals_file());  // ADD THIS LINE
}
```

**Behavior:**
- When `--no-prelude` is used, BOTH `core.hk` (Option/Result) AND `globals.hk` (JsValue/JsObject) are skipped
- This matches Rust's behavior where `--no-std` disables everything

### Phase 2: Handle Duplicate Declarations Gracefully

When user manually declares a type that's in the prelude, the compiler should:

1. **Check if type already exists** in the type environment
2. **Skip the duplicate** silently (user declaration is redundant but not an error)
3. **Optionally warn** with `HUSKC_DEBUG=1`: "JsValue already available from prelude, declaration unnecessary"

**Files to modify:**

1. `crates/husk-semantic/src/checker.rs` - Add duplicate check in `build_type_env()`

### Phase 3: Document & Test `import-dts` Command

The command already exists at `crates/husk-cli/src/main.rs:92-103, 475-515`. It needs:

**Documentation** - Add to README or create `docs/cli-reference.md`:

```markdown
## huskc import-dts

Generate Husk bindings from TypeScript declaration files (.d.ts).

### Usage

```bash
# Basic: output to stdout
huskc import-dts node_modules/@types/express/index.d.ts

# Save to file
huskc import-dts node_modules/@types/express/index.d.ts -o express.hk

# Include npm module import
huskc import-dts node_modules/express/index.d.ts --module express -o express.hk
```

### Finding .d.ts Files

1. Check `node_modules/<package>/` - look at `package.json` "types" field
2. Check `node_modules/@types/<package>/index.d.ts`
3. Some packages include types directly (e.g., `express.d.ts` in package root)

### Examples

```bash
# Express.js
huskc import-dts node_modules/@types/express/index.d.ts -m express -o lib/express.hk

# better-sqlite3 (has inline types)
huskc import-dts node_modules/better-sqlite3/better-sqlite3.d.ts -m better-sqlite3 -o lib/sqlite.hk
```
```

**Tests** - Create `crates/husk-cli/tests/import_dts.rs`:

```rust
use std::fs;
use std::process::Command;
use tempfile::tempdir;

#[test]
fn import_dts_basic_function() {
    let dir = tempdir().unwrap();
    let dts = dir.path().join("test.d.ts");
    fs::write(&dts, "declare function greet(name: string): void;").unwrap();

    let output = Command::new("cargo")
        .args(["run", "-p", "husk-cli", "--", "import-dts"])
        .arg(&dts)
        .output()
        .unwrap();

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("fn greet(name: String)"));
}

#[test]
fn import_dts_with_module_flag() {
    let dir = tempdir().unwrap();
    let dts = dir.path().join("express.d.ts");
    fs::write(&dts, "declare function express(): Application;").unwrap();

    let out = dir.path().join("express.hk");
    let status = Command::new("cargo")
        .args(["run", "-p", "husk-cli", "--", "import-dts"])
        .arg(&dts)
        .args(["-o", out.to_str().unwrap()])
        .args(["-m", "express"])
        .status()
        .unwrap();

    assert!(status.success());
    let content = fs::read_to_string(&out).unwrap();
    assert!(content.contains("mod express;"));
}

#[test]
fn import_dts_file_not_found() {
    let output = Command::new("cargo")
        .args(["run", "-p", "husk-cli", "--", "import-dts", "nonexistent.d.ts"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Failed to read"));
}
```

### Phase 4: Update Example to Use Prelude (Validation)

Once Phase 1 is complete, update `examples/express_sqlite/main.hk` to remove manual declarations:

**Before (lines 11-103):** 90+ lines of extern declarations
**After:** Just the application code, types come from prelude

This serves as validation that the prelude injection works correctly.

## Acceptance Criteria

### Functional Requirements

- [ ] Programs can use `JsValue`, `JsObject`, `jsvalue_get()`, etc. without declaring them
- [ ] `--no-prelude` flag disables both core types AND JS globals
- [ ] Duplicate declarations (user + prelude) don't cause errors
- [ ] `huskc import-dts --help` shows useful examples
- [ ] All existing tests continue to pass

### Testing Requirements

- [ ] Unit test for prelude injection of JS globals
- [ ] Integration test: compile program using `JsValue` without declaring it
- [ ] Integration test: compile with `--no-prelude`, verify `JsValue` not available
- [ ] CLI tests for `import-dts` command (basic, with flags, error cases)

### Documentation Requirements

- [ ] Update README with note about auto-available JS types
- [ ] Document `import-dts` command usage and examples
- [ ] List all types/functions available from JS prelude

## Implementation Tasks

### Task 1: Add JS Globals to Prelude Injection
**File:** `crates/husk-semantic/src/lib.rs`
- Add `JS_GLOBALS_SRC` static with `include_str!`
- Add `JS_GLOBALS_AST` OnceLock
- Add `js_globals_file()` function
- Call `checker.build_type_env(js_globals_file())` in `analyze_file_with_options()`

### Task 2: Handle Duplicate Type Declarations
**File:** `crates/husk-semantic/src/checker.rs`
- In `build_type_env()`, check if type already exists before adding
- Skip silently or log debug message
- Ensure no error is thrown for duplicates

### Task 3: Add Prelude Tests
**File:** `tests/prelude_js_globals.rs` (new)
- Test: `JsValue` available without declaration
- Test: `jsvalue_get()` function callable
- Test: `--no-prelude` makes them unavailable
- Test: User declaration + prelude doesn't error

### Task 4: Add import-dts CLI Tests
**File:** `crates/husk-cli/tests/import_dts.rs` (new)
- Test: Basic .d.ts → stdout
- Test: With `-o` output file
- Test: With `-m` module flag
- Test: File not found error
- Test: Parse error handling

### Task 5: Document import-dts Command
**File:** `docs/cli-reference.md` or README section
- Usage examples
- How to find .d.ts files
- Common packages examples

### Task 6: Update express_sqlite Example
**File:** `examples/express_sqlite/main.hk`
- Remove lines 11-28 (JsValue/JsObject declarations)
- Remove corresponding impl blocks
- Verify it still compiles and runs

## Dependencies

- Phase 2 depends on Phase 1
- Phase 3 is independent (can be done in parallel)
- Phase 4 depends on Phase 1 (validation step)

## Risk Analysis

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Duplicate type handling breaks existing code | Low | High | Thorough testing, conservative behavior (skip silently) |
| `std/js/globals.hk` missing types that runtime has | Medium | Medium | Audit runtime preamble vs globals.hk before implementation |
| `--no-prelude` behavior change confuses users | Low | Low | Document in changelog, behavior is more consistent now |

## Future Considerations

### Package Resolution for import-dts

A future enhancement could add package name resolution:

```bash
huskc import-dts express  # Resolves from node_modules automatically
```

This would require:
1. Reading `package.json` to find types field
2. Walking up directory tree for node_modules
3. Checking `@types/*` packages as fallback

**Not in scope for this plan** - the file-path approach works and is explicit.

### Separate `--no-js-prelude` Flag

If users want Option/Result but NOT JsValue/JsObject, we could add:
- `--no-prelude` - disables everything
- `--no-js-prelude` - disables only JS globals

**Not in scope for this plan** - single flag is simpler, matches Rust's `#![no_std]`.

## References

### Internal Files
- Runtime preamble: `crates/husk-runtime-js/src/lib.rs:206-301`
- Existing prelude: `stdlib/core.hk`
- JS globals: `std/js/globals.hk`
- Semantic options: `crates/husk-semantic/src/lib.rs:80-130`
- import-dts impl: `crates/husk-cli/src/main.rs:475-515`
- DTS parser: `crates/husk-dts-parser/src/codegen.rs`

### User Pain Point
- Example with manual declarations: `examples/express_sqlite/main.hk:11-103`
- User question: `questions.txt`

### Patterns
- Rust prelude: https://doc.rust-lang.org/std/prelude/
- Clap subcommands: https://docs.rs/clap/latest/clap/
