# Husk Expression-Based Language Design Plan

## Overview

This document outlines the plan to transform Husk into a fully expression-based language like Rust, where everything returns a value (even if it's unit `()`).

## Goals

1. **Everything is an expression** - All language constructs return values
2. **Semicolon semantics** - Semicolons convert expressions to unit-returning statements  
3. **Block expressions** - Blocks return the value of their last expression
4. **Consistent semantics** - Predictable behavior across all constructs

## Design Decisions

### AST Structure

We will take a **hybrid approach**:
- Keep `Expr` and `Stmt` types for now (less disruptive)
- Make all `Stmt` variants return values (usually `Type::Unit`)
- Eventually consider merging into single `Expr` type in Phase 2

### Core Language Changes

#### 1. Statement Return Values
All statements will return values:
- `let x = 5;` returns `()`
- `struct Foo { ... }` returns `()`  
- `x = 10;` returns `()`
- Expression statements return their value (with semicolon handling)

#### 2. Semicolon Semantics
```husk
// Without semicolon - returns value
let x = 5

// With semicolon - returns ()
let y = 5;

// In blocks
let result = {
    let temp = calculate();
    temp * 2  // No semicolon - this is returned
};
```

#### 3. Block Expressions
```husk
let value = {
    do_something();
    let x = 5;
    x + 1  // Last expression without semicolon is returned
};
```

#### 4. Control Flow Expressions

**If expressions:**
```husk
let max = if a > b { a } else { b };

// If without else must return unit
if condition {
    do_something();  // Must end with semicolon
}
// Equivalent to: if condition { do_something(); } else { () }
```

**Match expressions:**
```husk
let result = match value {
    Some(x) => x * 2,
    None => 0,
};
```

**Loop expressions:**
```husk
let result = loop {
    if condition {
        break 42;  // Break with value
    }
};

// While and for return ()
let () = while condition {
    do_something();
};
```

## Implementation Progress

### Completed Work

#### Semicolon Tracking (2024-12-29)
- Modified `Stmt::Expression(Expr)` to `Stmt::Expression(Expr, bool)`
- Updated `parse_expression_statement` to detect semicolons:
  - Sets boolean to `true` when semicolon is present
  - Sets boolean to `false` when no semicolon
- Updated all pattern matches across codebase:
  - `semantic.rs`: Both in `analyze_stmt` and function return type checking
  - `interpreter.rs`: In statement execution
  - `transpiler.rs`: In code generation
  - `ast/visitor.rs`: Updated trait and dispatch
- Fixed all test cases to include semicolon parameter
- All tests passing

This provides the foundation for implementing expression semantics where semicolons will convert expression values to unit type.

## Implementation Plan

### Prerequisites: Complete Visitor Pattern First (✅ COMPLETED - 2025-01-22)

After analyzing the complexity of expression-based transformations, we've decided to complete the visitor pattern implementation first. This will provide:
- Centralized AST traversal logic
- Easier implementation of consistent semantics
- Better separation of concerns
- Cleaner code for complex transformations

**Visitor Pattern Tasks:**
1. ✅ Refactor semantic analyzer to use visitor pattern
2. ✅ Refactor interpreter to use visitor pattern  
3. ✅ Refactor transpiler to use visitor pattern

The visitor pattern is now complete, providing a clean foundation for expression-based language implementation.

### Phase 1: Core Expression Semantics (2-3 weeks)

#### 1.1 Update AST and Parser ✅ (100% Complete)
- [x] Add `semicolon: bool` field to `Stmt::Expression` ✅
- [x] Update parser to track semicolons ✅
- [x] Ensure blocks can be used as expressions ✅
- [x] Update precedence rules for expression blocks ✅

#### 1.2 Update Type System (75% Complete)
- [x] Make all `Stmt` analysis return `Type` ✅
- [x] Implement block type inference (last expression type) ✅
- [x] Handle semicolon type conversion (expr → unit) ✅
- [ ] Update function return type inference

#### 1.3 Update Semantic Analyzer (40% Complete)
- [x] Refactor `analyze_stmt` to return `Type` ✅
- [x] Implement proper block typing ✅
- [ ] Check if/else branch type compatibility
- [ ] Validate match arm type consistency
- [ ] Handle loop break values

#### 1.4 Update Interpreter (75% Complete)
- [ ] Make all statement evaluation return `Value`
- [x] Implement block value returns ✅
- [x] Implement proper block scoping ✅
- [ ] Handle break with values
- [ ] Update Value enum if needed

#### 1.5 Update Transpiler (33% Complete)
- [x] Generate JS code that preserves semantics (for blocks) ✅
- [ ] Handle expression/statement differences between Husk and JS
- [ ] Ensure proper return value handling

### Phase 2: Language Refinements (1-2 weeks)

#### 2.1 Enhanced Control Flow
- [ ] Allow `break` with values in all loops
- [ ] Consider labeled breaks
- [ ] Implement `return` expression

#### 2.2 Pattern Improvements  
- [ ] If-let expressions
- [ ] While-let loops
- [ ] Guard clauses in match

#### 2.3 Error Handling
- [ ] Implement `?` operator for Result/Option
- [ ] Early return syntax

### Phase 3: AST Unification (Optional, 2-3 weeks)

#### 3.1 Merge Stmt and Expr
- [ ] Consider unifying statements and expressions into single AST node type
- [ ] Create unified `Expr` type
- [ ] Update all consumers
- [ ] Simplify visitor pattern

**Note**: This has been added to the todo list as medium priority for future consideration. Since everything now returns a type/value (even Unit), the distinction between statements and expressions is becoming artificial.

## Migration Strategy

### For Existing Code
1. Add semicolons to all statements that shouldn't return values
2. Update function bodies to remove explicit `return` where possible
3. Convert statement-based code to expression-based

### Compatibility Mode
- Consider a flag for "classic" mode during transition
- Provide migration tooling/linter

## Testing Strategy

### Unit Tests
- Semicolon semantics
- Block expressions  
- Control flow expressions
- Type inference

### Integration Tests
- Update all existing test files
- Add expression-based examples
- Cross-feature interactions

### Migration Tests
- Test old code with compatibility mode
- Verify migration tools

## Documentation Updates

1. Language reference
2. Tutorial updates
3. Migration guide
4. Example updates

## Risks and Mitigations

### Risk: Breaking Changes
**Mitigation**: 
- Clear migration guide
- Compatibility mode
- Gradual rollout

### Risk: Parser Complexity
**Mitigation**:
- Incremental implementation
- Extensive testing
- Clear precedence rules

### Risk: User Confusion  
**Mitigation**:
- Good error messages
- Clear documentation
- Examples and tutorials

## Success Metrics

1. All tests pass with new semantics
2. Clean expression-based example code
3. Performance unchanged or improved
4. User feedback positive

## Timeline

- **Week 1-2**: Core expression semantics
- **Week 3**: Interpreter and transpiler updates  
- **Week 4**: Testing and documentation
- **Week 5**: Refinements and polish
- **Week 6+**: Optional AST unification

## Open Questions

1. Should we require `else` branch for non-unit `if` expressions?
2. How do we handle top-level expressions in scripts?
3. Should we allow implicit returns in all blocks or just functions?
4. Do we want expression-oriented for loops like Rust's iterator chains?

## Decision Log

- **2024-12-29**: Decided to go with full Rust-like expression semantics
- **2024-12-29**: Chose hybrid AST approach for Phase 1
- **2024-12-29**: Completed semicolon tracking implementation
  - Successfully modified AST and parser
  - All tests passing with new semicolon boolean
  - Ready to proceed with block expressions
- **2025-01-22**: Completed block type inference implementation
  - Fixed block scoping in interpreter with proper push_scope/pop_scope
  - Block type inference now works with proper variable shadowing
  - All statement analysis methods confirmed to return Type correctly
  - Comprehensive test coverage: basic blocks, nested blocks, semicolon handling, scoping
- **2025-01-22**: Completed recursive function implementation
  - Fixed critical issue with function calls not finding themselves during recursion
  - Functions now properly stored in global environment for universal access
  - All recursive function tests pass (factorial, countdown, etc.)
- **2024-12-29**: Changed implementation order - Visitor pattern first
  - Analyzed trade-offs of direct modification vs visitor pattern
  - Determined visitor pattern will make expression-based changes cleaner
  - Pausing expression-based work to complete visitor pattern
  - This will provide better foundation for complex AST transformations