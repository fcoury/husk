# Husk Language Limitations and Improvement Roadmap

This document captures the limitations discovered while building the Express.js Todo API demo application and proposes solutions for each issue.

## 1. Missing Built-in String Methods

### Issue
Strings don't have built-in methods like `.len()`, `.trim()`, `.substring()`, etc.

### Current Workaround
```husk
// Have to use extern functions
extern fn String_length(s: string) -> int;
let len = String_length(myString);
```

### Proposed Solution
Implement built-in methods for primitive types:

```husk
// String methods
impl string {
    fn len(self) -> int;
    fn trim(self) -> string;
    fn substring(self, start: int, end: int) -> string;
    fn split(self, delimiter: string) -> array<string>;
    fn contains(self, substr: string) -> bool;
    fn replace(self, from: string, to: string) -> string;
    fn to_upper(self) -> string;
    fn to_lower(self) -> string;
    fn starts_with(self, prefix: string) -> bool;
    fn ends_with(self, suffix: string) -> bool;
}
```

### Implementation Plan
1. Add `BuiltinMethod` variant to `Expr` enum
2. Extend parser to recognize method calls on primitives
3. Implement method resolution in semantic analyzer
4. Add built-in method implementations in interpreter
5. Generate appropriate JavaScript in transpiler

## 2. Missing Array Methods

### Issue
Arrays don't have methods like `.len()`, `.push()`, `.pop()`, `.map()`, etc.

### Current Workaround
```husk
// Cannot dynamically modify arrays
// Have to create new arrays instead
let newArray = [...oldArray, newItem];  // Spread not implemented either
```

### Proposed Solution
```husk
// Array methods
impl<T> array<T> {
    fn len(self) -> int;
    fn push(self, item: T) -> void;
    fn pop(self) -> Option<T>;
    fn get(self, index: int) -> Option<T>;
    fn set(self, index: int, value: T) -> void;
    fn map<U>(self, f: fn(T) -> U) -> array<U>;
    fn filter(self, f: fn(T) -> bool) -> array<T>;
    fn find(self, f: fn(T) -> bool) -> Option<T>;
    fn contains(self, item: T) -> bool;
    fn index_of(self, item: T) -> Option<int>;
    fn slice(self, start: int, end: int) -> array<T>;
}
```

### Implementation Plan
1. Add generic method support to type system
2. Implement mutable array operations
3. Add iterator protocol support
4. Generate efficient JavaScript array method calls

## 3. Limited Pattern Matching in Expressions

### Issue
Cannot use pattern matching to destructure in let bindings or function parameters.

### Current State
```husk
// This doesn't work
let CreateTodoDto { title, description } = dto;

// Have to do
let title = dto.title;
let description = dto.description;
```

### Proposed Solution
```husk
// Destructuring in let bindings
let Point { x, y } = point;
let [first, second, ...rest] = array;

// Destructuring in function parameters
fn process({ title, description }: CreateTodoDto) {
    // Use title and description directly
}
```

### Implementation Plan
1. Extend parser to support destructuring patterns
2. Add pattern matching to let bindings
3. Support destructuring in function parameters
4. Implement array destructuring

## 4. No Property Access Shorthand

### Issue
Cannot check if a property exists without explicit comparison.

### Current State
```husk
// This doesn't work
if body.title { ... }

// Have to do something else
```

### Proposed Solution
```husk
// Property existence checking
if let Some(title) = body.title { 
    // Use title
}

// Or with optional chaining
let title = body?.title;
```

### Implementation Plan
1. Implement `if let` syntax
2. Add optional chaining operator `?.`
3. Support null coalescing operator `??`

## 5. Limited Type System for External APIs

### Issue
Cannot properly type dynamic JavaScript objects and their properties.

### Current State
```husk
// Have to use 'any' type
impl Request {
    fn body: any;
    fn params: any;
}

// Cast manually (now implemented!)
let dto = req.body as CreateTodoDto;
```

### Proposed Solution
```husk
// Better extern type definitions
extern type Request {
    body: unknown;  // New unknown type
    params: Record<string, string>;
    query: Record<string, string>;
}

// Type guards
fn isCreateTodoDto(value: unknown) -> bool {
    // Type checking logic
}

// Safe casting with validation
let dto = req.body as? CreateTodoDto;  // Returns Option<CreateTodoDto>
```

### Implementation Plan
1. Add `unknown` type distinct from `any`
2. Implement `Record<K, V>` type
3. Add type guard functions
4. Implement safe casting operator `as?`

## 6. No Async/Await in Interpreter

### Issue
Async functions only work in transpiler mode, not in interpreter.

### Current State
```husk
// This only works when transpiled to JavaScript
async fn fetchData() -> Result<string, Error> {
    let response = fetch(url).await?;
    Ok(response.text().await?)
}
```

### Proposed Solution
Implement async runtime in the interpreter using Rust's async infrastructure.

### Implementation Plan
1. Add async runtime (tokio) to interpreter
2. Implement Future/Promise type in Husk
3. Add async function execution support
4. Implement .await operator in interpreter

## 7. Missing Module Features

### Issue
- Cannot re-export items
- No module aliasing
- No glob imports working properly

### Current State
```husk
// These don't work
pub use local::types::*;  // Glob re-export
use local::very::long::path as short;  // Module aliasing
```

### Proposed Solution
```husk
// Re-exports
pub use local::types::{Todo, CreateTodoDto};
pub use local::types::*;

// Module aliasing
use local::very::long::path as short;
use short::SomeType;

// Nested module definitions
pub mod api {
    pub mod v1 {
        pub fn handler() {}
    }
}
```

### Implementation Plan
1. Implement re-export syntax
2. Add module aliasing support
3. Fix glob import handling
4. Support nested module definitions

## 8. Limited Error Handling

### Issue
- No way to define custom error types with data
- No error chaining or context

### Current State
```husk
enum ApiError {
    NotFound(string),
    BadRequest(string),
}
// Cannot attach additional data or context
```

### Proposed Solution
```husk
// Error trait
trait Error {
    fn message(self) -> string;
    fn source(self) -> Option<Error>;
}

// Custom errors with structured data
struct ValidationError {
    field: string,
    message: string,
    code: string,
}

impl Error for ValidationError {
    fn message(self) -> string {
        format!("{}: {}", self.field, self.message)
    }
}

// Error chaining
result.map_err(|e| ApiError::BadRequest(e.message()))?;
```

### Implementation Plan
1. Implement trait system
2. Add Error trait
3. Support error chaining
4. Add context methods to Result

## 9. No Runtime Type Information

### Issue
Cannot check types at runtime for dynamic JavaScript values.

### Current State
```husk
// This doesn't work
if err is ApiError {
    let api_err = err as ApiError;
}
```

### Proposed Solution
```husk
// Type checking
if value is string {
    // value is known to be string here
}

if let Some(error) = value.as<ApiError>() {
    // error is ApiError
}

// Type guards
match typeof(value) {
    "string" => ...,
    "number" => ...,
    "object" => ...,
}
```

### Implementation Plan
1. Add `is` operator for type checking
2. Implement `typeof` function
3. Add runtime type information to values
4. Support safe downcasting

## 10. Missing Standard Library Functions

### Issue
No built-in functions for common operations.

### Current State
```husk
// Have to use extern for everything
extern fn Date() -> DateInstance;
extern fn parseInt(s: string) -> int;
extern fn parseFloat(s: string) -> float;
```

### Proposed Solution
```husk
// Built-in functions in std module
mod std {
    fn parse_int(s: string) -> Result<int, ParseError>;
    fn parse_float(s: string) -> Result<float, ParseError>;
    fn random() -> float;
    fn exit(code: int) -> void;
}

// Date/time handling
mod time {
    struct DateTime {
        // ...
    }
    
    impl DateTime {
        fn now() -> DateTime;
        fn format(self, fmt: string) -> string;
    }
}
```

### Implementation Plan
1. Create standard library modules
2. Implement common utility functions
3. Add date/time handling
4. Include file system operations

## 11. Limited Generic Support

### Issue
- No generic constraints
- No associated types
- Limited generic inference

### Current State
```husk
// This is all we have
fn map<T, U>(items: array<T>, f: fn(T) -> U) -> array<U>
```

### Proposed Solution
```husk
// Generic constraints
fn sort<T: Ord>(items: array<T>) -> array<T>

// Associated types
trait Iterator {
    type Item;
    fn next(self) -> Option<Self::Item>;
}

// Better inference
let result = vec.map(|x| x * 2);  // T and U inferred
```

### Implementation Plan
1. Add trait system
2. Implement generic constraints
3. Add associated types
4. Improve type inference

## 12. No Macro System

### Issue
Only `format!` is hard-coded, no way to define custom macros.

### Proposed Solution
```husk
// Macro definitions
macro_rules! vec {
    () => { [] };
    ($($x:expr),+) => { [$($x),+] };
}

// Usage
let v = vec![1, 2, 3];

// Procedural macros for frameworks
#[derive(Serialize, Deserialize)]
struct Todo {
    id: string,
    title: string,
}
```

### Implementation Plan
1. Design macro syntax
2. Implement macro parser
3. Add macro expansion phase
4. Support procedural macros

## Priority Order for Implementation

### High Priority (Needed for basic usability)
1. Built-in methods for strings and arrays
2. Property access and existence checking
3. Better extern type definitions
4. Runtime type checking

### Medium Priority (Improves developer experience)
5. Pattern matching in let bindings
6. Standard library functions
7. Module re-exports and aliasing
8. Error handling improvements

### Low Priority (Advanced features)
9. Async/await in interpreter
10. Full generic constraints
11. Macro system
12. Trait system

## Conclusion

While Husk has a solid foundation with its Rust-inspired syntax and JavaScript transpilation, it needs these improvements to be practical for real-world applications. The Express.js demo revealed that even simple web applications require many of these features.

The highest priority should be implementing built-in methods for primitive types and improving the type system for external JavaScript APIs, as these are fundamental for any JavaScript interop scenario.