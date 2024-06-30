# Husk - Script Language Inspired by Rust

<p align="center">
  <img src="assets/logo-small.png" alt="AIPIM Icon" />
</p>

[![Build Status](https://github.com/fcoury/husk/actions/workflows/rust.yml/badge.svg)](https://github.com/fcoury/husk/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Crates.io](https://img.shields.io/crates/v/husk.svg)](https://crates.io/crates/husk)

Husk is a lightweight scripting language inspired by Rust, designed for simplicity and ease of use while maintaining some of Rust's powerful features.

## Features

- Rust-inspired syntax
- Static typing
- Support for basic data types: integers, floats, booleans, and strings
- Struct definitions and instantiation
- Function definitions and calls
- Control flow with if-else statements
- Basic arithmetic and comparison operations
- Interactive REPL (Read-Eval-Print Loop)
- Script execution from files

## Installation

To install Husk, you need to have Rust and Cargo installed on your system. Then, you can install Husk using:

```bash
cargo install husk
```

## Usage

### REPL Mode

To start the Husk REPL, run:

```bash
husk repl
```

### Script Execution

To execute a Husk script file, use:

```bash
husk run path/to/your/script.husk
```

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

### If-Else Statement

```rust
if x == 10 {
    println("x is ten");
} else {
    println("x is not ten");
}
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

## Contributing

Contributions to Husk are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
