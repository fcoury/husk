# Postponed Features During Visitor Pattern Implementation

This document tracks features that were postponed during the visitor pattern implementation. These features were working in the old implementation but need to be re-implemented with the visitor pattern approach.

## Features to Re-implement

### 1. Recursive Function Calls ✅ COMPLETED
**Status**: ✅ All tests passing
**Tests**: `factorial.hk`, `test_simple_factorial.husk`, `test_simple_recursion.husk`

**Description**: Functions can now call themselves recursively. Both the semantic analyzer and interpreter properly handle function definitions being in scope during analysis and execution.

**Solution Implemented**:
- **Semantic Analyzer**: Functions are registered before analyzing their body (semantic.rs:549-553)
- **Interpreter**: Functions are stored in the global environment for universal access (interpreter.rs:700)
- **Function Execution**: Closure environment merges with current function definitions to preserve access (interpreter.rs:334-342)

**Test Results**:
- `factorial(0) = 1` ✅
- `factorial(1) = 1` ✅  
- `factorial(3) = 6` ✅
- `factorial(5) = 120` ✅
- `count_down(3) = 0` ✅

### 2. Non-exhaustive Match Detection for Enums
**Status**: 1 test failing
**Test**: `test_enum_invalid_variant`
**Error**: "Non-exhaustive match: variant 'Option::Some' not covered"

**Description**: The semantic analyzer should detect when a match expression doesn't cover all variants of an enum. This validation ensures that all possible enum values are handled.

**Example**:
```rust
enum Option {
    None,
    Some(string),
}

let option = Option::Some("Hello");
match option {
    Option::None => println("None"),
    // Missing Option::Some case - should be detected
}
```

**Implementation Notes**: 
- Need to track all enum variants during semantic analysis
- During match expression analysis, verify all variants are covered
- Consider implementing in the semantic visitor's `visit_match` method

### 3. Array Slicing with Ranges
**Status**: 2 tests failing  
**Tests**: `test_range_of_array`, `test_until_end_range_of_array`, `range.hk`
**Error**: "Array index must be an integer, found range"

**Description**: Support for slicing arrays using range syntax like `arr[1..3]` or `arr[..5]`.

**Example**:
```rust
let arr = [1, 2, 3, 4, 5];
let slice = arr[1..3];    // Should return [2, 3]
let start = arr[2..];     // Should return [3, 4, 5]
let end = arr[..3];       // Should return [1, 2, 3]
```

**Implementation Notes**:
- Need to handle Range expressions in array indexing
- Update `visit_array_index` to support both integer indices and ranges
- Return a new array (slice) when range is used
- Handle edge cases: out of bounds, empty ranges, etc.

## Priority

1. ✅ **Recursive Function Calls** - COMPLETED: Core language feature now working
2. **Non-exhaustive Match Detection** - LOW priority: Safety feature but not blocking functionality  
3. **Array Slicing with Ranges** - LOW priority: Convenience feature, can work around with loops

## Implementation Plan

1. ✅ Complete visitor pattern for all three components (semantic analyzer ✅, interpreter ✅, transpiler ✅)
2. ✅ Fix recursive function calls (HIGH priority) - COMPLETED
3. Add remaining features back one at a time with proper tests
4. Ensure the visitor pattern makes these implementations cleaner than before

## Last Updated
2025-01-22 - Completed recursive function calls implementation. All factorial and recursion tests now pass.