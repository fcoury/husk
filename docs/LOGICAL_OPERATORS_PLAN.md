# Logical and Comparison Operators Implementation Plan

## Status: ✅ COMPLETED

## Overview
Add support for logical operators (`&&`, `||`) and the not-equals comparison operator (`!=`) to Husk.

## Operators to Implement

### Logical Operators
- `&&` (logical AND) - short-circuit evaluation
- `||` (logical OR) - short-circuit evaluation

### Comparison Operator
- `!=` (not equals)

## Implementation Steps

### 1. Lexer Updates
- Add `TokenKind::DblAmpersand` for `&&`
- Add `TokenKind::DblPipe` for `||`
- Add `TokenKind::BangEquals` for `!=`

### 2. Parser Updates
- Add new operators to `Operator` enum:
  - `And` for `&&`
  - `Or` for `||`
  - `NotEquals` for `!=`
- Update operator precedence (AND has higher precedence than OR)
- Update `parse_operator` method

### 3. Semantic Analyzer Updates
- `&&` and `||` require both operands to be `bool` type, return `bool`
- `!=` works like `==` - can compare any compatible types, returns `bool`

### 4. Interpreter Updates
- Implement short-circuit evaluation for `&&` and `||`
  - `&&`: if left is false, don't evaluate right, return false
  - `||`: if left is true, don't evaluate right, return true
- `!=` returns opposite of `==`

### 5. Transpiler Updates
- `&&` → `&&`
- `||` → `||`
- `!=` → `!==` (strict inequality in JavaScript)

## Short-Circuit Evaluation
This is crucial for logical operators:
```rust
// Should not crash even though arr[10] is out of bounds
let arr = [1, 2, 3];
if false && arr[10] > 0 {
    println("This won't execute");
}

// Should not crash even though x/0 would divide by zero
let x = 5;
if true || x/0 > 0 {
    println("This will execute");
}
```

## Operator Precedence (from highest to lowest)
1. Unary operators (`!`, `-`)
2. Multiplicative (`*`, `/`, `%`)
3. Additive (`+`, `-`)
4. Comparison (`<`, `>`, `<=`, `>=`, `==`, `!=`)
5. Logical AND (`&&`)
6. Logical OR (`||`)

## Test Cases
- Basic logical operations
- Short-circuit evaluation
- Operator precedence
- Not equals with different types
- Complex boolean expressions