# Unary Operators Implementation Plan

## Overview
Add support for unary operators in Husk:
- `-` (negation) for numeric values
- `!` (logical NOT) for boolean values

## Implementation Steps

### 1. Lexer Updates
- `!` token already exists as `TokenKind::Bang`
- `-` token already exists as `TokenKind::Minus`
- No lexer changes needed

### 2. Parser Updates
- Add `UnaryOp` enum with variants: `Neg`, `Not`
- Add `Expr::UnaryOp(UnaryOp, Box<Expr>, Span)` variant
- Update `parse_unary_expression` method in parser
- Handle operator precedence correctly

### 3. AST Visitor Updates
- Add `visit_unary_op` method to AstVisitor trait
- Parameters: `op: &UnaryOp`, `expr: &Expr`, `span: &Span`

### 4. Semantic Analyzer Updates
- Implement `visit_unary_op` in SemanticVisitor
- Type checking rules:
  - `-` operator: requires numeric type (int or float), returns same type
  - `!` operator: requires bool type, returns bool
- Add appropriate error messages for type mismatches

### 5. Interpreter Updates
- Implement `visit_unary_op` in InterpreterVisitor
- Execution logic:
  - `-`: negate numeric value
  - `!`: logical NOT for boolean

### 6. Transpiler Updates
- Implement `visit_unary_op` in JsTranspiler
- Generate JavaScript:
  - `-expr` → `-expr`
  - `!expr` → `!expr`

### 7. Test Coverage
- Parser tests for unary expressions
- Semantic analyzer tests for type checking
- Interpreter tests for execution
- Transpiler tests for code generation
- Integration tests for complex expressions

## Priority Considerations
- Start with parser implementation
- Ensure correct precedence (unary ops have high precedence)
- Handle edge cases like `--5` (double negation)