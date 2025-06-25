# Husk Type System Design Decisions

This document captures the design decisions made for Husk's type system during the architecture refactoring.

## Decisions Made

### 1. Type Inference Level: Local Type Inference ✓

**Decision**: Implement local type inference within expressions and simple assignments.

**Rationale**:
- Simpler to implement and understand
- Provides good developer experience without complexity
- Faster compilation times
- Better error messages that are easier to trace
- Covers the most common use cases (let bindings, array literals, etc.)

**Examples**:
```husk
let x = 5;              // Infers x: int
let y = x + 10;         // Infers y: int  
let arr = [1, 2, 3];    // Infers arr: array<int>
let s = "hello";        // Infers s: string

// Functions still need type annotations
function add(a: int, b: int) -> int {
    a + b
}
```

**Future**: Can be extended to more sophisticated inference if needed.

### 2. Generic Types: Prepare Architecture, Implement Later ✓

**Decision**: Design the type system architecture to support generics but defer implementation.

**Rationale**:
- Keeps architecture flexible for future generics
- Avoids complexity in initial implementation
- No major refactoring needed when adding generics
- Can ship v1 sooner while keeping options open
- Learn from real-world usage before implementing

**Implementation approach**:
- Type enum includes `Generic` variant (already done)
- APIs designed to be extended for generics
- Type checker structure allows for future generic support
- Parser can be extended to parse generic syntax

**Future generic syntax** (not implemented yet):
```husk
// What we're preparing for
function identity<T>(x: T) -> T { x }
function map<T, U>(arr: array<T>, f: fn(T) -> U) -> array<U> { ... }

struct Box<T> {
    value: T
}
```

### 3. Type Aliases: Simple Type Aliases ✓

**Decision**: Support simple type aliases for better code documentation and maintainability.

**Rationale**:
- Improves code readability and self-documentation
- Makes refactoring easier (change underlying type in one place)
- Relatively simple to implement
- Good balance between feature value and complexity
- Allows domain-specific naming without type safety overhead

**Syntax and semantics**:
```husk
type UserId = int;
type Username = string;
type Score = float;
type UserList = array<User>;

struct User {
    id: UserId,       // More semantic than 'int'
    name: Username,   // Clear what this string represents
    score: Score
}

// Type aliases are transparent - no conversion needed
let id: UserId = 42;          // OK
let num: int = id;            // OK - UserId is just int
let sum = id + 10;            // OK - works like int
```

**Implementation notes**:
- Store aliases in semantic analyzer
- Resolve during type checking
- Display original names in error messages for clarity

### 4. Null/Option Types: Rust-style Option Type ✓

**Decision**: No null values in Husk. Use Option<T> for optional values.

**Rationale**:
- Eliminates null pointer errors at compile time
- Makes optional values explicit in the type system
- Aligns with Rust's safety philosophy
- Forces deliberate handling of missing values
- Clean JavaScript interop strategy available

**Syntax**:
```husk
// Cannot have null values
let name: string = "Alice";        // Must have a value
// let bad: string = null;         // COMPILE ERROR!

// Use Option for optional values
let maybe_name: Option<string> = Some("Bob");
let no_name: Option<string> = None;

// Must handle both cases
match maybe_name {
    Some(n) => println("Hello, " + n),
    None => println("No name provided")
}
```

**JavaScript Interoperability**:
```husk
// At JS boundary, automatic conversion:
// JS null/undefined → Husk None  
// JS value → Husk Some(value)
// Husk None → JS null
// Husk Some(value) → JS value

// External JS functions
extern function querySelector(selector: string) -> Option<Element>;

let button = querySelector("#submit");  // Might be None if not found
```

**Implementation plan**:
- Implement `Option<T>` enum in the standard library
- Add `Result<T, E>` type for error handling (implement together with Option)
- Helper methods: `map`, `unwrap_or`, `and_then`, etc.
- `if let` syntax for simpler matching

**Result Type for Error Handling**:
```husk
// Result type for explicit error handling
enum Result<T, E> {
    Ok(T),
    Err(E)
}

// Example usage
function divide(a: float, b: float) -> Result<float, string> {
    if b == 0.0 {
        Err("Division by zero")
    } else {
        Ok(a / b)
    }
}

let result = divide(10.0, 2.0);
match result {
    Ok(value) => println("Result: " + value),
    Err(msg) => println("Error: " + msg)
}
```

### 5. Type Coercion: No Automatic Coercion ✓

**Decision**: All type conversions must be explicit. No automatic coercion.

**Rationale**:
- Prevents subtle bugs from implicit conversions
- Makes developer intent clear
- Consistent with Rust's explicit philosophy
- Aligns with our Option/Result safety choices
- No surprising behavior or precision loss

**Examples**:
```husk
let x: int = 5;
let y: float = 3.14;

// Arithmetic operations require explicit conversion
// let z = x + y;              // ERROR: Type mismatch int + float
let z = x.to_float() + y;      // OK: 5.0 + 3.14 = 8.14
let w = x + y.to_int();        // OK: 5 + 3 = 8

// Assignment also requires explicit conversion
// let f: float = x;           // ERROR: Cannot assign int to float
let f: float = x.to_float();   // OK: Explicit conversion

// Function calls require exact types
function calculate(a: float, b: float) -> float { a * b }
// calculate(5, 3.14);         // ERROR: Expected float, got int
calculate(5.to_float(), 3.14); // OK
```

**Conversion methods to implement**:
- `int.to_float() -> float`
- `float.to_int() -> int` (truncates)
- `string.parse_int() -> Result<int, ParseError>`
- `string.parse_float() -> Result<float, ParseError>`
- `T.to_string() -> string` (for all types)

---

## Summary

All type system design decisions have been made:

1. **Type Inference**: Local type inference only ✓
2. **Generics**: Prepare architecture, implement later ✓
3. **Type Aliases**: Simple type aliases ✓
4. **Null/Option**: Rust-style Option and Result types ✓
5. **Type Coercion**: No automatic coercion ✓

These decisions create a consistent, safe, and explicit type system that aligns with Rust's philosophy while remaining practical for a scripting language.