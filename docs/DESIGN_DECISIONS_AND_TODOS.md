# Husk Language Design Decisions and Implementation TODOs

This document tracks design decisions made during development and features that need to be implemented or improved.

## Design Decisions

### 1. If as Expression (Implemented)
- **Decision**: Treat `if` as an expression everywhere, not as a statement
- **Rationale**: Provides consistency and eliminates the artificial distinction between if statements and if expressions
- **Status**: ✅ Implemented
- **Notes**: Similar to Rust, Kotlin, and Scala

### 2. Parentheses in Expressions (Implemented)
- **Decision**: Support parentheses for grouping expressions
- **Rationale**: Essential for controlling operator precedence and expression clarity
- **Status**: ✅ Implemented

### 3. Pattern Variable Scoping (Implemented)
- **Decision**: Pattern variables in match expressions should be available in the match arm body without creating a new scope
- **Rationale**: Makes pattern matching more useful and intuitive
- **Status**: ✅ Implemented

## Implementation TODOs

### High Priority

#### 1. Logical Operators
- **Features**: `&&` (AND), `||` (OR), `!` (NOT)
- **Current Status**: Not implemented
- **Impact**: Basic boolean operations are not possible
- **Example**: `if x > 5 && x < 10 { ... }`

#### 2. Comparison Operators
- **Features**: `!=` (not equals)
- **Current Status**: Only `==` is supported
- **Impact**: Common comparisons require workarounds
- **Example**: `if x != 0 { ... }`

#### 3. Compound Assignment Operators
- **Features**: `+=` (add and assign), `-=` (subtract and assign)
- **Current Status**: Not implemented
- **Impact**: Common increment/decrement operations require verbose syntax
- **Example**: `x += 1` instead of `x = x + 1`
- **Notes**: Should also consider `*=`, `/=`, `%=` for completeness

#### 4. Unary Operators
- **Features**: `-` (negation), `!` (logical NOT)
- **Current Status**: Not implemented
- **Impact**: Cannot express negative numbers or boolean negation
- **Example**: `-1`, `!condition`
- **Workaround**: Currently tests use positive numbers like `999` instead of `-1`

#### 5. Method Call Syntax
- **Features**: Automatic `self` passing for method calls like `object.method()`
- **Current Status**: Not implemented
- **Impact**: Methods must be called as `Type::method(object)` instead of `object.method()`
- **Example**: Need to write `Rectangle::area(rect)` instead of `rect.area()`
- **Complexity**: Requires distinguishing between methods and fields in member access

### Medium Priority

#### 6. Mutable Variables
- **Features**: `mut` keyword for explicit mutability
- **Current Status**: All variables are mutable by default
- **Impact**: No way to enforce immutability
- **Design Question**: Should we make variables immutable by default (like Rust) or keep current behavior?

#### 7. Anonymous Functions / Closures
- **Features**: `fn(args) -> ret { body }` syntax for anonymous functions
- **Current Status**: Not implemented
- **Impact**: Cannot create higher-order functions or closures
- **Example**: `let add = fn(a: int, b: int) -> int { a + b };`

#### 8. String Concatenation
- **Features**: String concatenation with `+` operator
- **Current Status**: Not implemented
- **Impact**: No way to concatenate strings
- **Example**: `"hello" + " world"`

#### 9. Self Type in Methods
- **Features**: Proper handling of `self` type in method signatures
- **Current Status**: `self` type is not properly resolved to the struct type
- **Impact**: Method implementations don't work correctly
- **Example**: `fn area(self) -> int` expects type `self` but gets `Rectangle`
- **Notes**: Related to method call syntax - both need to be fixed together

### Low Priority

#### 10. If Expressions in Any Position
- **Current Status**: If expressions work but were initially limited in where they could appear
- **Notes**: This was partially addressed by making if always an expression

## Type System Improvements

### 1. Better Error Messages
- Improve type mismatch error messages with suggestions
- Show expected vs actual types more clearly

### 2. Type Inference
- Currently limited type inference
- Could improve to reduce type annotation requirements

## Parser Improvements

### 1. Better Error Recovery
- Current parser stops at first error
- Could implement error recovery to show multiple errors

### 2. Operator Precedence
- Currently uses basic precedence rules
- May need refinement for more complex expressions

## Standard Library

### 1. Basic Functions
- Need more built-in functions beyond `print` and `println`
- String manipulation functions
- Math functions
- Type conversion functions

## Testing Infrastructure

### 1. Integration Test Framework
- Current test runner is basic
- Could add better test organization and reporting

### 2. Error Testing
- Need systematic testing of error conditions
- Ensure good error messages for common mistakes

## Documentation

### 1. Language Reference
- Comprehensive documentation of all language features
- Examples for each feature
- Common patterns and idioms

### 2. Tutorial
- Step-by-step guide for new users
- Building increasingly complex programs

## Future Considerations

### 1. Module System
- How to organize larger programs
- Import/export mechanisms
- Namespace management

### 2. Error Handling
- Currently no structured error handling
- Consider Result/Option types or exceptions

### 3. Memory Management
- Currently using simple reference counting
- May need more sophisticated approach for complex programs

### 4. Concurrency
- No concurrency support currently
- Would need careful design to fit language philosophy

## Notes

This document should be updated as new decisions are made or new TODOs are discovered. Each item should include:
- Clear description of the feature/issue
- Current status and workarounds (if any)
- Impact on users
- Priority level
- Any design questions that need to be answered