<p align="center">
  <img src="assets/logo-small.png" alt="Husk Logo" />
</p>

<p align="center">
  <strong>A modern, Rust-inspired language that compiles to JavaScript with first-class developer experience</strong>
</p>

<p align="center">
  <!--
  <a href="https://github.com/fcoury/husk/actions">
    <img src="https://github.com/fcoury/husk/actions/workflows/rust.yml/badge.svg" alt="Build Status" />
  </a>
  -->
  <a href="https://crates.io/crates/husk-lang">
    <img src="https://img.shields.io/crates/v/husk-lang.svg" alt="Crates.io" />
  </a>
  <a href="https://opensource.org/licenses/MIT">
    <img src="https://img.shields.io/badge/License-MIT-yellow.svg" alt="License: MIT" />
  </a>
  <a href="https://husk-lang.org">
    <img src="https://img.shields.io/badge/Website-husk--lang.org-orange.svg" alt="Website" />
  </a>
</p>

Husk brings Rust's elegant syntax and safety to the JavaScript ecosystem. Write code with algebraic data types, pattern matching, and strong static typing, then seamlessly compile to clean, modern JavaScript that runs anywhere.

## Why Husk?

**🎯 Rust-like Syntax, JavaScript Runtime**

- Familiar syntax for Rust developers
- Compiles to clean, readable ES modules or CommonJS
- Interoperates seamlessly with npm packages
- Zero-cost abstractions over JavaScript

**⚡ Outstanding Developer Experience**

- **Language Server (LSP)** with real-time diagnostics, hover info, and go-to-definition
- **Code formatter** (`husk fmt`) for consistent style
- **Editor support** for VS Code and Neovim with syntax highlighting and intelligent features
- **TypeScript definitions** parser for npm package interop
- **Watch mode** for instant feedback during development

**🔒 Type Safety You Can Trust**

- Strong static typing with inference
- Exhaustive pattern matching on enums
- Compile-time guarantees, runtime confidence
- Result types (`Ok`/`Err`) for error handling

**🚀 Built for Real-World Development**

- Express.js integration examples
- SQLite database support
- npm package ecosystem access
- Source maps for debugging

## Quick Start

### Installation

```bash
# Install from source (requires Rust)
cargo install --path crates/husk-cli

# Or build locally
git clone https://github.com/fcoury/husk.git
cd husk
cargo build --release
```

### Your First Program

Create `hello.hk`:

```rust
fn greet(name: string) -> string {
    format!("Hello, {}!", name)
}

fn main() {
    let message = greet("World");
    println!("{}", message);
}
```

Compile and run:

```bash
# Compile to JavaScript
huskc compile hello.hk > hello.js

# Run with Node.js
node hello.js
```

Or use the watch mode for rapid development:

```bash
# Watch and run on changes
huskc watch --run hello.hk
```

## Developer Experience

### Language Server (LSP)

Husk includes a full-featured language server providing:

- **Real-time diagnostics** - See parse and semantic errors as you type
- **Hover information** - View type information and documentation
- **Go to definition** - Jump to function, struct, enum, and trait definitions
- **Document symbols** - Navigate your code structure with ease

Install for VS Code:

```bash
cd editors/vscode
npm install
npm run compile
code --install-extension .
```

### Code Formatting

Keep your code clean and consistent:

```bash
# Format a file
husk fmt src/main.hk

# Check without modifying
husk fmt --check src/

# Format entire project
husk fmt .
```

### Editor Support

**VS Code**: Full LSP integration with syntax highlighting, diagnostics, hover, and go-to-definition

- Install from `editors/vscode`

**Neovim**: Syntax highlighting, indentation, and filetype detection

- Install using your plugin manager: `'fcoury/husk.nvim'`

## Language Features

### Pattern Matching with Exhaustiveness Checking

```rust
enum Result<T, E> {
    Ok(T),
    Err(E),
}

fn divide(a: int, b: int) -> Result<int, string> {
    if b == 0 {
        Result::Err("Division by zero")
    } else {
        Result::Ok(a / b)
    }
}

fn main() {
    match divide(10, 2) {
        Result::Ok(value) => println!("Result: {}", value),
        Result::Err(err) => println!("Error: {}", err),
    }
    // Compiler ensures all cases are handled!
}
```

### Structs and Methods

```rust
struct User {
    name: string,
    email: string,
    age: int,
}

impl User {
    fn new(name: string, email: string, age: int) -> User {
        User { name, email, age }
    }

    fn greet(&self) -> string {
        format!("Hi, I'm {} and I'm {} years old", self.name, self.age)
    }
}
```

### Generics

```rust
fn first<T>(items: Array<T>) -> Option<T> {
    if items.length > 0 {
        Option::Some(items[0])
    } else {
        Option::None
    }
}
```

### Closures

```rust
fn main() {
    let numbers = [1, 2, 3, 4, 5];
    let doubled = numbers.map(|x| x * 2);
    println!("{:?}", doubled);
}
```

## JavaScript Interoperability

### Using npm Packages

Husk can call JavaScript libraries directly:

```rust
// express.hk
extern fn express() -> Express;
extern fn json() -> Middleware;

struct Express {
    extern fn get(&self, path: string, handler: fn(Request, Response) -> void) -> void;
    extern fn use(&self, middleware: Middleware) -> void;
    extern fn listen(&self, port: int, callback: fn() -> void) -> void;
}

fn main() {
    let app = express();
    app.use(json());

    app.get("/", |req, res| {
        res.send("Hello from Husk!");
    });

    app.listen(3000, || {
        println!("Server running on port 3000");
    });
}
```

### TypeScript Definitions Support

Automatically parse `.d.ts` files to use TypeScript-typed npm packages:

```bash
huskc dts-to-husk node_modules/@types/express/index.d.ts > express.hk
```

## Build System

### Project-based Builds

```bash
# Auto-detect entry point and build to dist/
huskc build

# Specify entry point
huskc build src/main.hk

# Build as library (no auto-call of main)
huskc build --lib

# Choose output format
huskc build --target esm  # ES modules
huskc build --target cjs  # CommonJS

# Generate TypeScript definitions
huskc build --emit-dts

# Generate source maps for debugging
huskc build --source-map
```

### Watch Mode

Automatically rebuild and run on file changes:

```bash
# Watch and rebuild
huskc watch src/main.hk

# Watch and run
huskc watch --run src/main.hk

# Watch and execute with Node
huskc watch --exec "node dist/main.js"
```

## CLI Reference

### Commands

- `huskc [file]` - Parse and check a Husk file
- `huskc check <file>` - Type-check a file
- `huskc compile <file>` - Compile to JavaScript (stdout)
- `huskc build [file]` - Build project to dist/
- `huskc watch <file>` - Watch for changes and rebuild
- `huskc run <file>` - Compile and execute with Node.js
- `huskc fmt <path>` - Format Husk source files
- `huskc dts-to-husk <file.d.ts>` - Convert TypeScript definitions

### Compilation Options

- `--target {esm,cjs}` - Output format (default: auto-detect from package.json)
- `--lib` - Library mode (don't auto-call main)
- `--no-prelude` - Disable stdlib prelude injection
- `--emit-dts` - Generate TypeScript definition files
- `--source-map` - Generate source maps
- `--output <dir>` - Output directory (default: dist)

### Formatter Options

- `--check` - Check if files are formatted without modifying
- `--config <file>` - Use custom formatter config

## Examples

The `examples/` directory showcases Husk's capabilities:

- **hello.hk** - Basic syntax and control flow
- **closures.hk** - Closure type inference
- **generic_functions.hk** - Generic functions with type parameters
- **format_strings.hk** - String interpolation
- **feature_match/** - Pattern matching with enums
- **express_sqlite/** - Full Express.js + SQLite web application
- **demo_npm/** - Multi-file project with imports
- **advent2025/** - Advent of Code solutions

Run any example:

```bash
huskc run examples/hello.hk
```

## Building from Source

### Prerequisites

- Rust 1.70+ and Cargo
- Node.js 16+ (for running compiled output)

### Build All Crates

```bash
# Format code
cargo fmt --all

# Run linter
cargo clippy --all -- -D warnings

# Run tests
cargo test --all

# Build release binary
cargo build --release
```

The compiler binary will be at `target/release/huskc`.

## Architecture

Husk is implemented as a Rust workspace with specialized crates:

| Crate             | Purpose                                          |
| ----------------- | ------------------------------------------------ |
| `husk-ast`        | Abstract syntax tree definitions                 |
| `husk-lexer`      | Tokenization and lexical analysis                |
| `husk-parser`     | Recursive descent parser                         |
| `husk-types`      | Type system and inference                        |
| `husk-semantic`   | Semantic analysis and type checking              |
| `husk-codegen-js` | JavaScript code generation                       |
| `husk-runtime-js` | JavaScript runtime helpers (Result, panic, etc.) |
| `husk-lsp`        | Language Server Protocol implementation          |
| `husk-fmt`        | Code formatter                                   |
| `husk-dts-parser` | TypeScript definition parser                     |
| `husk-cli`        | Command-line interface                           |

## JavaScript Runtime

Compiled Husk code includes a small runtime preamble providing:

- `Result<T, E>` with `Ok` and `Err` constructors
- `Option<T>` with `Some` and `None` constructors
- `panic(message)` for unrecoverable errors
- `matchEnum(value, variants)` for pattern matching
- Versioned runtime (currently 0.1.0) for compatibility

The runtime is inlined at compile-time, adding ~2KB to output.

## Contributing

We welcome contributions! Please see [AGENTS.md](AGENTS.md) for development guidelines.

## License

MIT License - see [LICENSE](LICENSE) for details.

## Acknowledgments

Husk draws inspiration from:

- **Rust** - Syntax, type system, and pattern matching
- **TypeScript** - JavaScript interop and tooling
- **OCaml** - ML-family type inference
- **Roc** - Approach to compiling functional languages to imperative targets
