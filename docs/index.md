# Husk Programming Language Documentation

Welcome to the official documentation for Husk, a modern programming language that combines the expressiveness and safety of Rust with simplicity and ease of use.

## 🚀 Quick Navigation

### Getting Started
- **[Installation Guide](getting-started/installation.md)** - Install Husk on your system
- **[Hello World Tutorial](getting-started/hello-world.md)** - Write your first Husk program
- **[Quick Start Guide](getting-started/quickstart.md)** - Essential features in 15 minutes
- **[Editor Setup](getting-started/editor-setup.md)** - Configure your development environment

### Language Reference
- **[Language Features Overview](LANGUAGE_FEATURES.md)** - Complete language syntax and features
- **[Type System](language/types.md)** - Static typing and type inference
- **[Variables and Constants](language/variables.md)** - Declaration and scoping
- **[Functions](language/functions.md)** - Function definition and calling conventions
- **[Control Flow](language/control-flow.md)** - Conditionals and loops
- **[Pattern Matching](language/pattern-matching.md)** - Match expressions and destructuring
- **[Error Handling](language/error-handling.md)** - Option and Result types
- **[Closures](language_features/CLOSURES.md)** - First-class functions and closures
- **[Data Types](language/data-types.md)** - Built-in and custom types

### Standard Library
- **[Standard Library Reference](STANDARD_LIBRARY.md)** - Complete API reference
- **[String Operations](stdlib/strings.md)** - String manipulation and Unicode
- **[Array Operations](stdlib/arrays.md)** - Functional array methods
- **[File I/O](stdlib/file-io.md)** - Reading and writing files
- **[Console I/O](stdlib/console-io.md)** - Input and output operations
- **[Math Functions](stdlib/math.md)** - Mathematical operations

### Tools & Ecosystem
- **[Husk CLI](tools/cli.md)** - Command-line interface reference
- **[Build System](tools/build.md)** - Project structure and compilation
- **[Testing](tools/testing.md)** - Writing and running tests
- **[Debugging](tools/debugging.md)** - Debugging Husk programs

### Tutorials & Guides
- **[Building a CLI Application](tutorials/cli-app.md)** - Step-by-step CLI tutorial
- **[Data Processing](tutorials/data-processing.md)** - Working with data in Husk
- **[Migrating from Rust](tutorials/from-rust.md)** - Guide for Rust developers
- **[Migrating from JavaScript](tutorials/from-javascript.md)** - Guide for JS developers

### Advanced Topics
- **[JavaScript Transpilation](advanced/javascript-transpilation.md)** - Compiling to JavaScript
- **[Memory Management](advanced/memory.md)** - How Husk manages memory
- **[JavaScript Interop](advanced/js-interop.md)** - Working with JavaScript code
- **[Performance Optimization](advanced/performance.md)** - Writing efficient code
- **[Best Practices](advanced/best-practices.md)** - Idiomatic Husk patterns

## 📖 About Husk

Husk is a statically-typed, memory-safe programming language designed for:

- **Simplicity**: Easy to learn and use, with familiar syntax
- **Safety**: Memory safety without garbage collection
- **Performance**: Compiles to efficient JavaScript
- **Expressiveness**: Modern features like pattern matching and type inference
- **Interoperability**: Seamless JavaScript integration

### Key Features

- ✅ **Static typing** with local type inference
- ✅ **Pattern matching** for expressive control flow
- ✅ **First-class functions** and closures
- ✅ **Memory safety** without manual management
- ✅ **Option and Result types** for error handling
- ✅ **Functional programming** support
- ✅ **JavaScript transpilation** for web deployment

### Example

```rust
// A simple Husk program demonstrating key features
fn main() {
    let numbers = [1, 2, 3, 4, 5];
    
    // Functional programming with closures
    let evens = numbers.filter(|n| n % 2 == 0);
    let sum = evens.fold(0, |acc, n| acc + n);
    
    // Pattern matching
    match sum {
        0 => println!("No even numbers found"),
        n if n < 10 => println!("Sum of evens: {}", n),
        _ => println!("Large sum: {}", sum)
    }
    
    // Error handling with Result
    match read_config("app.toml") {
        Ok(config) => process_config(config),
        Err(e) => eprintln!("Config error: {}", e)
    }
}

// Type inference and Option handling
fn find_user(id: int) -> Option<User> {
    let users = get_users();
    users.find(|u| u.id == id)
}
```

## 🎯 Where to Start

1. **New to Husk?** Begin with the [Installation Guide](getting-started/installation.md) and [Hello World Tutorial](getting-started/hello-world.md)

2. **Experienced developer?** Jump to the [Quick Start Guide](getting-started/quickstart.md) or browse the [Language Features](LANGUAGE_FEATURES.md)

3. **Looking for specific functionality?** Check the [Standard Library Reference](STANDARD_LIBRARY.md)

4. **Ready to build?** Follow our [tutorials](tutorials/) for hands-on experience

## 📝 Documentation Status

| Section | Status | Description |
|---------|--------|-------------|
| Language Features | ✅ Complete | Core language documentation |
| Standard Library | ✅ Complete | Built-in functions and types |
| Getting Started | 🚧 In Progress | Installation and tutorials |
| Advanced Topics | 📋 Planned | Performance and interop guides |
| Tools & Ecosystem | 📋 Planned | Build and development tools |

## 🤝 Contributing

We welcome contributions to both Husk and its documentation! Ways to contribute:

- Report issues or suggest improvements
- Submit pull requests with fixes or new content
- Share your Husk projects and experiences
- Help others in the community

## 📚 Additional Resources

- [GitHub Repository](https://github.com/username/husk) - Source code and issue tracker
- [Community Forum](https://forum.husk-lang.org) - Discussion and support
- [Language Specification](spec/) - Formal language specification
- [Roadmap](roadmap.md) - Future development plans

## 🔄 Version

This documentation is for Husk version 0.1.0 (current development version).

---

*Last updated: 2025-06-27*