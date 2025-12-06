# Introduction

Husk is a modern programming language that brings Rust's elegant syntax and powerful type system to the JavaScript ecosystem. Write code with familiar Rust patterns and compile to clean, readable ES modules that run on Node.js.

## Why Husk?

- **Rust-inspired syntax** - Structs, enums, pattern matching, and Result types
- **Strong static typing** - Catch errors at compile time with algebraic data types
- **JavaScript output** - Generate clean ES modules with full npm ecosystem access
- **First-class tooling** - LSP support, VS Code extension, formatter, and watch mode

## Quick Example

```rust
struct Person {
    name: String,
    age: i32,
}

fn greet(person: Person) -> String {
    format!("Hello, {}! You are {} years old.", person.name, person.age)
}

fn main() {
    let alice = Person { name: "Alice".to_string(), age: 30 };
    println!("{}", greet(alice));
}
```

This compiles to readable JavaScript:

```javascript
const alice = { name: "Alice", age: 30 };
console.log(`Hello, ${alice.name}! You are ${alice.age} years old.`);
```

## Getting Started

Ready to try Husk? Head to the [Installation](./getting-started/installation.md) guide to get started.
