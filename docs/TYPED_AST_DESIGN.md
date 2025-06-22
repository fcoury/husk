# Typed AST Design

## Overview

This document describes the typed AST implementation in Husk, which provides a clean separation between parsing and semantic analysis, allowing for proper disambiguation of syntactically similar but semantically different constructs.

## Problem Statement

The parser creates an AST node called `EnumVariantOrMethodCall` for expressions like:
- `Option::Some(5)` - enum variant construction
- `Point::new(3, 4)` - static method call

These have identical syntax (`Type::name(args)`) but completely different semantics and JavaScript transpilation:
- Enum variant: `new Option.Some(5)`
- Static method: `Point.new(3, 4)`

## Solution: Typed AST

After semantic analysis, we transform the untyped AST into a typed AST with distinct nodes for each semantic construct.

### Architecture

```
Source Code
    ↓
Parser → Untyped AST (with EnumVariantOrMethodCall)
    ↓
Semantic Analyzer → Type Information
    ↓
AST Transformer → Typed AST (with EnumVariant vs StaticMethodCall)
    ↓
Typed Transpiler → JavaScript
```

### Key Components

1. **Typed AST** (`src/typed_ast.rs`):
   - `TypedExpr::EnumVariant` - clearly an enum variant
   - `TypedExpr::StaticMethodCall` - clearly a static method
   - `TypedExpr::MethodCall` - instance method calls
   - Each node carries its type information

2. **AST Transformer** (`src/typed_ast.rs::AstTransformer`):
   - Takes untyped AST + type information from semantic analyzer
   - Produces typed AST with proper disambiguation
   - Uses semantic information to distinguish constructs

3. **Typed Transpiler** (`src/typed_transpiler.rs`):
   - Works with typed AST
   - No guessing or heuristics needed
   - Clean, straightforward transpilation

## Benefits

1. **Correctness**: No more guessing based on naming conventions
2. **Clarity**: Each AST node represents exactly one semantic construct
3. **Extensibility**: Easy to add new constructs without ambiguity
4. **Type Safety**: Type information is carried throughout compilation

## Implementation Status

### Completed
- Typed AST structure definition
- Basic AST transformer for expressions
- Typed transpiler for basic expressions
- Integration with semantic analyzer

### TODO
- Complete transformation for all statement types
- Integrate typed transpiler into main compilation pipeline
- Add support for pattern matching in typed AST
- Implement type-directed optimizations

## Example

```rust
// Husk code
enum Option {
    Some(int),
    None,
}

struct Point {
    x: int,
    y: int,
}

impl Point {
    fn new(x: int, y: int) -> Point {
        Point { x: x, y: y }
    }
}

let opt = Option::Some(42);  // EnumVariant in typed AST
let p = Point::new(3, 4);    // StaticMethodCall in typed AST
```

## Future Improvements

1. **Type Inference**: Use typed AST for better type inference
2. **Optimizations**: Type-directed optimizations
3. **Error Messages**: Better error messages with type context
4. **IDE Support**: Rich type information for IDE features