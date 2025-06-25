# Husk Language Features

This document provides an overview of the key language features implemented in Husk, with links to detailed documentation for each feature.

## Core Language Features

### Function Programming Features

- **[Closures](language_features/CLOSURES.md)** - Anonymous functions with environment capture
- **Higher-Order Functions** - Functions that take or return other functions
- **Pattern Matching** - Match expressions for control flow and destructuring

### Type System

- **Static Type Checking** - Compile-time type verification
- **Type Inference** - Automatic type deduction
- **Generic Types** - Parameterized types (planned)
- **Option and Result Types** - Safe error handling and null safety

### Control Flow

- **Match Expressions** - Pattern matching for control flow
- **If-Else Expressions** - Conditional expressions
- **Loop Constructs** - For loops, while loops, and iteration

### Data Types

- **Primitive Types** - Int, Float, Bool, String
- **Composite Types** - Arrays, Tuples, Structs
- **Enum Types** - Sum types with pattern matching
- **Range Types** - Numeric ranges for iteration

### Memory Management

- **Automatic Memory Management** - Garbage collection in interpreter mode
- **JavaScript Compilation** - Direct compilation to JavaScript for web deployment

### Standard Library

- **[String Methods](../STANDARD_LIBRARY_PLAN.md#string-methods)** - Comprehensive string manipulation
- **[Array Methods](../STANDARD_LIBRARY_PLAN.md#array-methods)** - Functional array operations
- **Built-in Functions** - Core utility functions

### Compilation Targets

- **Interpreter Mode** - Direct execution in Rust-based interpreter
- **JavaScript Transpilation** - Compilation to modern JavaScript for web platforms

## Implementation Status

### ✅ Fully Implemented
- Closures and higher-order functions
- Static type checking with inference
- Pattern matching with enums
- Core data types and operations
- String and array standard library
- JavaScript transpilation

### 🚧 In Progress
- Generic type system
- Advanced pattern matching features
- Extended standard library

### 📋 Planned
- Module system
- Async/await support
- Foreign function interface (FFI)
- Package management

## Getting Started

For detailed information about any specific feature, follow the links above to the dedicated documentation files.

## Examples

Quick examples of key language features:

```husk
// Closures and higher-order functions
let numbers = [1, 2, 3, 4, 5];
let doubled = numbers.map(|x| x * 2);
let evens = numbers.filter(|x| x % 2 == 0);

// Pattern matching
match some_value {
    Option::Some(x) => println("Got value: {}", x),
    Option::None => println("No value")
}

// Type-safe operations
let result: Result<int, string> = safe_operation();
match result {
    Result::Ok(value) => use_value(value),
    Result::Err(error) => handle_error(error)
}
```

For more detailed examples and explanations, see the individual feature documentation files.