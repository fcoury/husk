# Return Statements Implementation Plan

## Overview
Add explicit `return` statements to Husk for early returns from functions, while maintaining the current expression-based semantics where functions can return the value of their last expression.

## Implementation Strategy

### Phase 1: Lexer & Parser (Foundation)
1. **Add Return Token to Lexer**
   - Add `Return` variant to `TokenKind` enum in `src/lexer.rs`
   - Add keyword recognition for "return" in lexer implementation
   - Update token display formatting

2. **Add Return Statement to AST**
   - Add `Return(Option<Expr>, Span)` variant to `Stmt` enum in `src/parser.rs`
   - `Option<Expr>` allows for both `return;` (Unit) and `return expr;`
   - Update all `PartialEq` implementations for `Stmt`
   - Update `Display` implementation for pretty printing

3. **Implement Return Parsing**
   - Add `parse_return_statement()` method to parser
   - Handle both `return;` and `return expression;` forms
   - Add return statement parsing to main statement parsing logic
   - Add proper error handling for malformed returns

### Phase 2: Semantic Analysis
1. **Update Semantic Analyzer**
   - Add `visit_return()` method to `AstVisitor` trait in `src/ast/visitor.rs`
   - Implement return statement analysis in `SemanticVisitor`
   - Type-check return expressions against function return type
   - Validate returns only occur within function bodies
   - Handle `return;` as returning Unit type

2. **Function Context Tracking**
   - Track current function context during analysis
   - Store expected return type for validation
   - Ensure returns are only used in appropriate contexts

### Phase 3: Interpreter Implementation
1. **Restore ControlFlow::Return**
   - Keep `ControlFlow::Return(Value)` variant in interpreter
   - Implement return statement execution in `InterpreterVisitor`
   - Set `self.control_flow = ControlFlow::Return(value)` when return is executed

2. **Function Execution Updates**
   - Update function call handling to detect return control flow
   - Early exit from function when return is encountered
   - Preserve existing expression-based return behavior

### Phase 4: Transpiler Support
1. **JavaScript Generation**
   - Add `visit_return()` to transpiler's `AstVisitor` implementation
   - Generate `return expr;` or `return;` JavaScript statements
   - Handle return statements in nested blocks correctly

### Phase 5: Testing & Validation
1. **Unit Tests**
   - Test return parsing (with and without expressions)
   - Test semantic analysis (type checking, context validation)
   - Test interpreter execution (early returns, nested functions)
   - Test transpiler output

2. **Integration Tests**
   - Create test files demonstrating various return scenarios:
     - Early returns in conditionals
     - Multiple return paths
     - Return with different types
     - Mixed explicit returns and expression returns

## Syntax Examples

```husk
// Early return with value
fn find_positive(arr: [int]) -> int {
    for item in arr {
        if item > 0 {
            return item;  // Early exit
        }
    }
    -1  // Expression-based fallback
}

// Early return without value (Unit)
fn validate_input(x: int) -> unit {
    if x < 0 {
        return;  // Early exit with Unit
    }
    println("Valid input: ", x);
}

// Mixed returns and expressions
fn complex_logic(x: int) -> string {
    if x == 0 {
        return "zero";
    }
    
    if x < 0 {
        "negative"  // Expression return
    } else {
        "positive"  // Expression return
    }
}
```

## Design Decisions

1. **Syntax**: Use `return expr;` and `return;` (consistent with Rust)
2. **Semantics**: Return statements immediately exit the function
3. **Type Safety**: Return expressions must match function return type
4. **Scope**: Returns only valid inside function bodies
5. **Compatibility**: Preserve existing expression-based returns

## Implementation Order
1. Lexer token support
2. Parser AST and parsing logic  
3. Semantic analysis and validation
4. Interpreter execution
5. Transpiler JavaScript generation
6. Comprehensive testing

## Estimated Effort
- **Lexer/Parser**: 2-3 hours
- **Semantic Analysis**: 3-4 hours  
- **Interpreter**: 2-3 hours
- **Transpiler**: 1-2 hours
- **Testing**: 3-4 hours
- **Total**: ~12-16 hours

This implementation will provide Husk with explicit return statements while maintaining its expression-based nature, giving developers the flexibility to use either style as appropriate.