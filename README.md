# Husk - Script Language Inspired by Rust

<p align="center">
  <img src="assets/logo-small.png" alt="AIPIM Icon" />
</p>

[![Build Status](https://github.com/fcoury/husk/actions/workflows/rust.yml/badge.svg)](https://github.com/fcoury/husk/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!--
[![Crates.io](https://img.shields.io/crates/v/husk.svg)](https://crates.io/crates/husk)
-->

Husk is a lightweight scripting language inspired by Rust, designed for simplicity and ease of use while maintaining some of Rust's powerful features.

## Features

- Rust-inspired syntax
- Static typing
- Support for basic data types: integers, floats, booleans, and strings
- Struct definitions and instantiation
- Function definitions and calls
- Control flow with if-else expressions and match expressions
- Return statements for early function returns
- Enums with associated values
- Arrays and ranges
- Loop constructs: for, while, and loop with break/continue
- Arithmetic operations: `+`, `-`, `*`, `/`, `%`
- Comparison operations: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical operations: `&&`, `||`, `!`
- Compound assignment: `+=`, `-=`, `*=`, `/=`, `%=`
- Interactive REPL (Read-Eval-Print Loop)
- Script execution from files
- Transpilation to JavaScript

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
