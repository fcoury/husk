# Husk - Rust's syntax meets JavaScript's flexibility

<p align="center">
  <img src="assets/logo-small.png" alt="Husk Logo" />
</p>

<p align="center">
  <strong>Choose your own adventure: Run interpreted for rapid development or transpile to JavaScript for the entire npm ecosystem</strong>
</p>

<p align="center">
  <a href="https://github.com/fcoury/husk/actions">
    <img src="https://github.com/fcoury/husk/actions/workflows/rust.yml/badge.svg" alt="Build Status" />
  </a>
  <a href="https://opensource.org/licenses/MIT">
    <img src="https://img.shields.io/badge/License-MIT-yellow.svg" alt="License: MIT" />
  </a>
  <a href="https://husk-lang.org">
    <img src="https://img.shields.io/badge/Website-husk--lang.org-orange.svg" alt="Website" />
  </a>
</p>

<!--
[![Crates.io](https://img.shields.io/crates/v/husk.svg)](https://crates.io/crates/husk)
-->

Husk is a lightweight interpreted language that brings Rust's elegant syntax to the world of scripting and JavaScript development. Write type-safe, expressive code with the flexibility to run it instantly or compile it for production use with full access to npm's 1M+ packages.

## Why Husk?

### 🚀 Best of Both Worlds
Write scripts with Rust's clean, expressive syntax while leveraging JavaScript's massive ecosystem. No more choosing between elegant code and practical libraries.

### 🎯 Choose Your Path

#### **Interpreter Mode** - Quick & Iterative
Perfect for:
- Rapid prototyping
- Build scripts
- CLI tools  
- Learning and experimentation

```bash
# Zero configuration, instant feedback
husk run script.husk

# Interactive REPL for exploration
husk repl
```

#### **Transpiler Mode** - Production Ready
Perfect for:
- Web applications
- npm packages
- Node.js services
- Existing JavaScript projects

```bash
# Build once, run anywhere JavaScript runs
husk build

# Use with your favorite npm packages
npm install express
node dist/app.js
```

## Features

### Language Features
- **Rust-inspired syntax** - Familiar and expressive
- **Static typing with inference** - Catch errors early, write less boilerplate
- **Pattern matching** - Powerful control flow with match expressions
- **Option & Result types** - Built-in error handling the Rust way
- **Structs & Enums** - Model your data precisely
- **Module system** - Organize code across files
- **Async/await** - Modern asynchronous programming
- **Type casting** - Seamless conversions with `as`
- **Format macro** - String interpolation done right

### Developer Experience
- **Interactive REPL** - Test ideas instantly
- **Zero configuration** - Just write and run
- **Project build system** - Scale from scripts to applications
- **JavaScript interop** - Use any npm package
- **Fast iteration** - No compile times in interpreter mode
- **Cross-platform** - Runs wherever Node.js runs

## Quick Start

### Installation

```bash
# Install Husk (requires Rust/Cargo)
cargo install husk
```

### Your First Husk Program

```rust
// hello.husk
fn greet(name: string) -> string {
    format!("Hello, {}!", name)
}

fn main() {
    let message = greet("World");
    println(message);
}
```

Run it instantly:
```bash
husk run hello.husk
```

Or compile to JavaScript:
```bash
husk compile hello.husk | node
```

## Usage Modes

### REPL Mode

To start the Husk REPL, run:

```bash
husk repl
```

### Script Execution

To execute a Husk script file, use:

```bash
husk run path/to/your/script.hk
```

### Transpilation to JavaScript

To transpile a Husk script to JavaScript, use:

```bash
husk compile path/to/your/script.hk
```

This will output the transpiled JavaScript code to stdout. If you have node installed you can do:

```bash
husk compile path/to/your/script.hk | node
```

### Project Build System

Husk supports project-based development with `husk.toml` configuration files. To build an entire project:

```bash
husk build
```

Additional build options:

```bash
# Generate package.json from husk.toml
husk build --generate-package-json

# Specify target platform
husk build --target node-cjs
```

#### Project Structure

Create a `husk.toml` file in your project root:

```toml
[package]
name = "my-husk-app"
version = "0.1.0"
description = "My Husk application"
author = "Your Name"

[dependencies]
express = "^4.18.0"
lodash = "^4.17.21"

[build]
target = "node-esm"
src = "src"
out = "dist"
```

Place your Husk source files in the `src` directory. The build command will compile all `.husk` files and generate corresponding JavaScript files in the output directory.

## Real-World Example

Here's how Husk enables you to move seamlessly from rapid prototyping to production:

### 1. Start with the Interpreter (Rapid Development)

```rust
// calculator.husk - Quick prototype
fn calculate(operation: string, a: int, b: int) -> Result<int, string> {
    match operation {
        "add" => Ok(a + b),
        "subtract" => Ok(a - b),
        "multiply" => Ok(a * b),
        "divide" => {
            if b == 0 {
                Err("Division by zero")
            } else {
                Ok(a / b)
            }
        },
        _ => Err("Unknown operation")
    }
}

fn main() {
    let result = calculate("add", 10, 5);
    match result {
        Ok(value) => println(format!("Result: {}", value)),
        Err(msg) => println(format!("Error: {}", msg))
    }
}
```

```bash
# Test instantly during development
husk run calculator.husk
# Output: Result: 15
```

### 2. Transpile for Production (Access npm ecosystem)

```rust
// web-calculator.husk - Production version with Express
use external::express;

async fn create_server() -> Result<unit, string> {
    let app = express();
    
    app.get("/calculate/:op/:a/:b", |req, res| {
        let operation = req.params.op;
        let a = req.params.a as int;
        let b = req.params.b as int;
        
        match calculate(operation, a, b) {
            Ok(result) => res.json({"result": result}),
            Err(error) => res.status(400).json({"error": error})
        }
    });
    
    app.listen(3000);
    println("Server running on http://localhost:3000");
    Ok(())
}

async fn main() {
    match create_server().await {
        Ok(_) => {},
        Err(e) => println(format!("Server error: {}", e))
    }
}
```

```bash
# Build for production
husk build
npm install express
node dist/web-calculator.js
```

The same core logic, two different execution modes!

## Language Syntax

Here are some examples of Husk syntax:

### Variable Declaration

```rust
let x = 5;
let name = "Alice";
let is_true = true;
```

### Function Definition

```rust
fn add(x: int, y: int) -> int {
    x + y
}
```

### Struct Definition and Instantiation

```rust
struct Person {
    name: string,
    age: int,
}

let p = Person {
    name: "Bob",
    age: 30,
};
```

### Enum Definition and Pattern Matching

```rust
enum Option {
    Some(int),
    None,
}

let opt = Option::Some(5);

match opt {
    Option::Some(value) => println(value),
    Option::None => println("No value"),
}
```

### Arrays and Ranges

```rust
let arr = [1, 2, 3, 4, 5];
let slice = arr[1..3];

for i in 0..5 {
    println(i);
}
```

### Loops

```rust
// For loop
for x in [1, 2, 3, 4, 5] {
    println(x);
}

// While loop
let i = 0;
while i < 5 {
    println(i);
    i = i + 1;
}

// Infinite loop with break
loop {
    println("Hello");
    if some_condition {
        break;
    }
}
```

### Module System

Husk supports a module system for organizing code across multiple files:

```rust
// In utils.hk
pub fn log(message: string) {
    println(format!("[LOG] {}", message));
}

pub fn formatDate(date: string) -> string {
    format!("Date: {}", date)
}
```

```rust
// In main.husk
use local::utils::{log, formatDate};

fn main() {
    log("Application started");
    let today = formatDate("2023-12-25");
    println(today);
}
```

Module import prefixes:
- `local::` - Import from project root
- `self::` - Import from current directory
- `super::` - Import from parent directory

### Async/Await (Transpiler Only)

```rust
async fn fetchData() -> Result<string, string> {
    let response = fetch("https://api.example.com/data").await?;
    Ok(response.text().await?)
}

async fn main() {
    match fetchData().await {
        Ok(data) => println(data),
        Err(error) => println(format!("Error: {}", error)),
    }
}
```

### Built-in Types

Husk includes Option and Result types for safe error handling:

```rust
fn divide(a: int, b: int) -> Result<int, string> {
    if b == 0 {
        Err("Division by zero")
    } else {
        Ok(a / b)
    }
}

let result = divide(10, 2)?; // Error propagation operator
```

### Type Casting

Husk supports explicit type conversion using the `as` operator:

```rust
// Numeric conversions
let i = 42;
let f = i as float;  // 42.0

let f2 = 3.14;
let i2 = f2 as int;  // 3 (truncates decimal)

// String parsing
let s = "123";
let num = s as int;  // 123

// To string conversion
let n = 456;
let str = n as string;  // "456"

// Boolean conversion
let b = true as int;     // 1
let zero = 0 as bool;    // false
let nonzero = 5 as bool; // true
```

### Built-in Methods

Husk provides JavaScript-compatible methods for primitive types:

#### String Methods

```rust
let s = "  Hello World  ";
println(s.len());           // 15
println(s.trim());          // "Hello World"
println(s.toLowerCase());   // "  hello world  "
println(s.toUpperCase());   // "  HELLO WORLD  "

let text = "Hello World";
println(text.substring(0, 5));  // "Hello"
println(text.substring(6, 11)); // "World"

let csv = "apple,banana,orange";
let parts = csv.split(",");    // ["apple", "banana", "orange"]
```

#### Array Methods

```rust
let arr = [1, 2, 3, 4, 5];
println(arr.len());  // 5
```

## Development

To set up the development environment:

1. Clone the repository:

   ```bash
   git clone https://github.com/fcoury/husk.git
   cd husk
   ```

2. Build the project:

   ```bash
   cargo build
   ```

3. Run tests:
   ```bash
   cargo test
   ```

## Resources

- **🌐 Website**: [husk-lang.org](https://husk-lang.org)
- **📚 Documentation**: [husk-lang.org/docs](https://husk-lang.org/docs)  
- **🎮 Playground**: [husk-lang.org/playground](https://husk-lang.org/playground)
- **📖 Examples**: [husk-lang.org/examples](https://husk-lang.org/examples)

## Community

- **💬 Discussions**: [GitHub Discussions](https://github.com/fcoury/husk/discussions)
- **🐛 Issues**: [GitHub Issues](https://github.com/fcoury/husk/issues)
- **💡 Feature Requests**: [GitHub Issues](https://github.com/fcoury/husk/issues/new)

## Contributing

Husk is an open-source project that welcomes contributions from everyone! Whether you're:

- 🐛 Reporting bugs
- 💡 Suggesting features  
- 📝 Improving documentation
- 🔧 Writing code
- ✨ Sharing examples

We'd love your help making Husk better. Check out our [Contributing Guide](CONTRIBUTING.md) to get started.

## Development

To set up the development environment:

```bash
# Clone the repository
git clone https://github.com/fcoury/husk.git
cd husk

# Build the project
cargo build

# Run tests
cargo test

# Install locally for testing
cargo install --path .
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

<p align="center">
  <strong>Ready to choose your own adventure?</strong><br>
  <a href="https://husk-lang.org">Visit husk-lang.org</a> to get started!
</p>
