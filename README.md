# Husk

Husk is an experimental, Rust-flavored language that compiles to modern JavaScript (ES modules). It focuses on:

- Rust-like syntax for functions, structs, and enums.
- Strong, static typing with enums and `match` (including exhaustiveness checks).
- A small JavaScript micro-runtime (`std_preamble.js`) providing `Ok`/`Err`, `panic`, and basic tagged-union helpers.
- A CLI compiler, `huskc`, implemented in Rust.

The implementation is organized as a Rust workspace with multiple crates (`husk-ast`, `husk-lexer`, `husk-parser`, `husk-types`, `husk-semantic`, `husk-codegen-js`, `husk-runtime-js`, and `husk-cli`).

## Building and Testing

From the workspace root:

- Format all crates:

  ```bash
  cargo fmt --all
  ```

- Run Clippy with warnings as errors:

  ```bash
  cargo clippy --all -- -D warnings
  ```

- Run all tests (unit + integration):

  ```bash
  cargo test --all
  ```

These commands are also expected to be run after each development task (see `AGENTS.md` for contributor guidance).

## Using the CLI

The CLI binary is called `huskc` and is built via Cargo.

- Parse-only (frontend):

  ```bash
  cargo run --bin huskc -- examples/hello.hk
  ```

- Type-check a file:

  ```bash
  cargo run --bin huskc -- check examples/hello.hk
  ```

- Compile a Husk source file to JavaScript (printed to stdout):

  ```bash
  cargo run --bin huskc -- compile examples/hello.hk
  ```

The compiled output includes the inlined JavaScript preamble (`std_preamble.js`) at the top of the file. The runtime helpers (`Ok`, `Err`, `panic`, `matchEnum`, etc.) are versioned via a `HUSK_RUNTIME_VERSION` constant inside the preamble (currently `0.1.0`).

### Useful flags

- `--target {esm,cjs}` (default `cjs`): choose export style explicitly. Use `--target esm` for Bun/Deno/browser or Node `--input-type=module`, even when your Husk file has no imports.
- `--no-prelude`: disable automatic injection of the stdlib prelude (`Option`, `Result`). Keep the default for normal usage; opt out only for experiments or custom core definitions.

### Quick start (ESM and CJS)

```bash
# CommonJS (default)
cargo run --quiet --bin huskc -- compile examples/hello.hk > target/hello.cjs.js
node target/hello.cjs.js

# ESM output (rename to .mjs so Node treats it as ESM)
cargo run --quiet --bin huskc -- compile --target esm examples/hello.hk > target/hello.mjs
node target/hello.mjs
```

### Binary vs. Library mode

By default, `huskc compile` generates a “binary-style” JS module:

- If a zero-argument `fn main()` is present, the compiled JS will:
  - Define `function main() { ... }`.
  - Emit a call to `main();` at the end of the file.
- It also exports all top-level functions via CommonJS:
  ```js
  module.exports = { main: main /*, other functions */ };
  ```

When you pass `--lib`:

- `huskc compile --lib ...` still generates the same functions and `module.exports` object.
- It **does not** auto-call `main()`, even if present.
- This is the recommended mode when you want to treat Husk code as a library and call its exports explicitly from a Node/Bun host:
  ```js
  const huskLib = require("./compiled_husk.js");
  huskLib.main(); // called explicitly by the host
  ```

## Running a Husk Program under Node

For convenience, there is a small script under `scripts/` that compiles a Husk source file and runs the resulting JavaScript with Node.

By convention, if your Husk module defines a zero-argument `fn main()`:

```husk
fn main() {
    // your program entry point
}
```

the generated JavaScript will include a call to `main();` at the end of the module, so running the compiled file under Node will automatically execute your program.

Prerequisites:

- Node.js installed and available on your `PATH`.
- Rust toolchain and Cargo installed.

Usage:

```bash
scripts/run_node_example.sh examples/node_simple.hk
```

This script:

1. Changes to the repository root.
2. Invokes:

   ```bash
   cargo run --quiet --bin huskc -- compile examples/node_simple.hk > target/husk-node-example.js
   ```

3. Runs the generated JS with:

   ```bash
   node target/husk-node-example.js
   ```

You can pass any `.hk` file path instead of `examples/node_simple.hk`, as long as it is a valid Husk program.

## Examples

The `examples/` directory contains:

- `examples/hello.hk` – a minimal arithmetic and `if` example.
- `examples/feature_match/simple_match.hk` – focused enum + `match` usage.
- `examples/integration/basic_program.hk` – a larger integration example combining enums, generics, and `match`.
- `examples/node_simple.hk` – a small program intended to be compiled and run under Node (good target for the `scripts/run_node_example.sh` script).
- `examples/interop_express_minimal.hk` – a minimal Express-style interop example used together with `examples/express_host.js` to demonstrate the Node host pattern.

All `.hk` examples are covered by the integration tests in `tests/examples.rs`, which ensure they parse, type-check, and lower to JS successfully.

To experiment with the Express-style example using a real `express` package:

1. Install Express in the workspace:

   ```bash
   npm install express
   ```

2. Compile the Husk example to JS:

   ```bash
   cargo run --bin huskc -- compile --lib examples/interop_express_minimal.hk \
     > target/interop_express_minimal.js
   ```

3. Run the Node host script:

   ```bash
   node examples/express_host.js
   ```

The host script creates an Express app, exposes a compatible `express()` factory on `globalThis`, requires the compiled Husk module (which runs `main()` and registers routes), and finally starts the HTTP server.
