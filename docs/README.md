# Husk Documentation

Welcome to the official documentation for the Husk programming language. This guide provides comprehensive information about the language features, standard library, and development practices.

## 📚 Documentation Index

### Core Documentation

- **[Language Features](LANGUAGE_FEATURES.md)** - Complete guide to Husk language syntax and features
  - Type system and inference
  - Control flow and pattern matching
  - Functions and closures
  - Data types and structures
  - Memory management

- **[Standard Library](STANDARD_LIBRARY.md)** - Comprehensive standard library reference
  - String operations and Unicode handling
  - Array manipulation and functional programming
  - File I/O and directory operations
  - Console input/output
  - Type utilities (Option and Result)

### Additional Resources

More documentation sections will be added here as the language evolves, including:
- Getting Started Guide
- Tutorial and Examples
- Build System Documentation
- Module System Guide
- JavaScript Interop Guide
- Contributing Guidelines

## 🚀 Quick Start

Husk is a modern programming language that combines the safety and expressiveness of Rust with the simplicity needed for both learning and production use. It features:

- **Static typing** with type inference
- **Pattern matching** for expressive control flow
- **First-class functions** and closures
- **Memory safety** without manual management
- **JavaScript compilation** for web deployment

### Example Code

```husk
// Define a function with pattern matching
fn greet(name: string) -> string {
    match name {
        "World" => "Hello, World! 🌍",
        "" => "Hello, anonymous!",
        _ => format!("Hello, {}!", name)
    }
}

// Use higher-order functions
let numbers = [1, 2, 3, 4, 5];
let evens = numbers.filter(|n| n % 2 == 0);
let doubled = evens.map(|n| n * 2);

println!("Doubled evens: {:?}", doubled); // [4, 8]

// Safe file operations with Result handling
match read_file("config.json") {
    Ok(contents) => {
        let config = parse_json(contents);
        println!("Loaded configuration");
    },
    Err(e) => eprintln!("Failed to load config: {}", e)
}
```

## 📖 How to Use This Documentation

1. **New to Husk?** Start with the [Language Features](LANGUAGE_FEATURES.md) guide to understand the core concepts and syntax.

2. **Looking for specific functionality?** Check the [Standard Library](STANDARD_LIBRARY.md) reference for built-in functions and methods.

3. **Building a project?** Each documentation page includes practical examples and best practices.

## 🛠 Documentation Structure

The documentation is organized to help you find information quickly:

- **Overview sections** provide high-level understanding
- **Detailed references** include type signatures and parameters
- **Code examples** demonstrate practical usage
- **Implementation status** shows what's available now and what's coming
- **Cross-references** link related topics

## 🤝 Contributing

The Husk documentation is constantly evolving. If you find errors, unclear explanations, or missing information, please contribute by:

- Opening an issue on the Husk repository
- Submitting a pull request with improvements
- Sharing your feedback and suggestions

## 📝 License

Husk and its documentation are open source. See the repository for license details.

---

*This documentation index will expand as new guides and references are added. Check back regularly for updates!*