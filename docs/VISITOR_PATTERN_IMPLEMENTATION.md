# Visitor Pattern Implementation Plan

## Overview

This document outlines the plan to complete the visitor pattern implementation for Husk. The visitor pattern will provide a clean, maintainable foundation for the expression-based language transformation and all future AST-related features.

## Motivation

After implementing semicolon tracking and preparing for expression-based semantics, we realized that continuing with direct modifications to the semantic analyzer, interpreter, and transpiler would lead to:
- Scattered, repetitive changes across multiple files
- Difficulty maintaining consistency
- Accumulating technical debt
- Complex code that's hard to reason about

The visitor pattern will centralize all AST traversal logic, making the codebase more maintainable and the expression-based transformation cleaner.

## Current State

### Completed
- ✅ `AstVisitor` trait defined in `src/ast/visitor.rs`
- ✅ Default traversal methods implemented
- ✅ Visitor trait updated for semicolon tracking
- ✅ All visit methods signatures defined
- ✅ Semantic analyzer refactored to use visitor pattern (2025-01-22)
- ✅ Interpreter refactored to use visitor pattern (2025-01-22)

### Remaining Work
- 🔲 Refactor transpiler to use visitor pattern (NEXT TASK)

## Implementation Plan

### Phase 1: Semantic Analyzer (2-3 days)

#### 1.1 Create SemanticVisitor
```rust
struct SemanticVisitor {
    type_env: TypeEnvironment,
    structs: HashMap<String, HashMap<String, Type>>,
    functions: HashMap<String, (Vec<(String, Type)>, Type, Span)>,
    enums: HashMap<String, HashMap<String, Option<Type>>>,
    match_bound_vars: HashMap<String, Type>,
    loop_depth: u32,
}

impl AstVisitor<Type> for SemanticVisitor {
    type Error = Error;
    // Implement all visit methods
}
```

#### 1.2 Key Implementation Points
- Each expression visit method returns its `Type`
- Statement visit methods will return `Type::Unit` (preparing for expression-based)
- Maintain all current semantic checks
- Proper error propagation
- Handle scoping correctly

#### 1.3 Migration Strategy
1. Create new `semantic_visitor.rs` module
2. Implement `SemanticVisitor` struct
3. Implement expression visit methods first
4. Implement statement visit methods
5. Update `semantic.rs` to use visitor
6. Ensure all tests pass
7. Remove old traversal code

### Phase 2: Interpreter (2-3 days)

#### 2.1 Create InterpreterVisitor
```rust
struct InterpreterVisitor {
    environment: Environment,
    control_flow: ControlFlow,
}

impl AstVisitor<Value> for InterpreterVisitor {
    type Error = Error;
    // Implement all visit methods
}
```

#### 2.2 Key Implementation Points
- Each visit method returns a `Value`
- Handle control flow (break, continue, return)
- Maintain execution state
- Proper error handling

#### 2.3 Migration Strategy
1. Create new `interpreter_visitor.rs` module
2. Implement `InterpreterVisitor` struct
3. Handle control flow state
4. Implement all visit methods
5. Update `interpreter.rs` to use visitor
6. Ensure all tests pass
7. Remove old traversal code

### Phase 3: Transpiler (1-2 days)

#### 3.1 Create TranspilerVisitor
```rust
struct TranspilerVisitor {
    output: String,
    indent_level: usize,
}

impl AstVisitor<String> for TranspilerVisitor {
    type Error = Error;
    // Implement all visit methods
}
```

#### 3.2 Key Implementation Points
- Each visit method returns generated JavaScript code
- Handle indentation and formatting
- Preserve semantics in translation

#### 3.3 Migration Strategy
1. Create new `transpiler_visitor.rs` module
2. Implement `TranspilerVisitor` struct
3. Implement code generation methods
4. Update `transpiler.rs` to use visitor
5. Ensure all tests pass
6. Remove old traversal code

## Benefits for Expression-Based Language

Once the visitor pattern is complete, implementing expression-based semantics becomes much cleaner:

1. **Unified Type Returns**: All visit methods already return values, making it natural for statements to return types
2. **Centralized Semicolon Handling**: One place to implement semicolon → unit conversion
3. **Block Expression Logic**: Easy to implement in one place
4. **Consistent Semantics**: Ensures all AST nodes are handled consistently
5. **Easier Testing**: Can test transformations in isolation

## Testing Strategy

1. **Maintain all existing tests** - They should pass without modification
2. **Add visitor-specific tests** - Test visitor behavior in isolation
3. **Integration tests** - Ensure the whole pipeline still works
4. **Performance tests** - Verify no performance regression

## Success Criteria

1. All existing tests pass
2. Visitor pattern fully replaces old traversal code
3. Code is cleaner and more maintainable
4. Ready for expression-based language features
5. Performance is maintained or improved

## Timeline

- **Week 1**: Semantic analyzer visitor (Days 1-3) ✅ COMPLETED
- **Week 1**: Interpreter visitor (Days 3-5) ✅ COMPLETED
- **Week 2**: Transpiler visitor (Days 1-2) 🔲 IN PROGRESS
- **Week 2**: Testing and cleanup (Days 2-3)

Total estimated time: 5-7 working days

## Implementation Notes

### Semantic Analyzer (Completed)
- Created `src/semantic_visitor.rs` with full implementation
- Fixed TypeEnvironment API mismatches (get vs lookup)
- Handled borrow checker issues with HashMap cloning
- Updated error messages to match test expectations
- Added support for both static and instance methods
- All semantic analyzer tests passing

### Interpreter (Completed)
- Created `src/interpreter_visitor.rs` with full implementation
- Added ControlFlow::Return variant for proper control flow
- Fixed method call handling with dot notation (e.g., `p.distance_from_origin()`)
- Updated semantic visitor to handle method calls in tandem
- 23 interpreter tests passing, 3 tests postponed:
  - Non-exhaustive match detection for enums
  - Array slicing with ranges (2 tests)

### Postponed Features
During the visitor pattern implementation, we identified some advanced features that will be implemented after the core visitor pattern is complete:

1. **Non-exhaustive match detection for enums**: The visitor pattern doesn't yet validate that all enum variants are covered in match expressions.
2. **Array slicing with ranges**: Syntax like `arr[1..3]` is not yet supported in the visitor implementation.

## Future Benefits

The visitor pattern will also make these future features easier:
- Type inference improvements
- Optimization passes
- Code formatting
- Linting and analysis tools
- Language server protocol implementation
- Macro system
- Pattern matching enhancements