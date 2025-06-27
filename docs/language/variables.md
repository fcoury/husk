# Variables and Constants

Variables and constants are fundamental building blocks in Husk. This guide covers declaration, scoping, mutability, and best practices for working with variables and constants.

## Table of Contents

- [Overview](#overview)
- [Variable Declaration](#variable-declaration)
- [Mutability](#mutability)
- [Constants](#constants)
- [Type Annotations](#type-annotations)
- [Scoping Rules](#scoping-rules)
- [Shadowing](#shadowing)
- [Destructuring](#destructuring)
- [Best Practices](#best-practices)
- [Common Patterns](#common-patterns)
- [Troubleshooting](#troubleshooting)
- [Related Topics](#related-topics)

## Overview

Husk provides:
- **Immutable by default** - Variables cannot be changed unless explicitly marked mutable
- **Type inference** - Types are inferred when possible
- **Lexical scoping** - Variables are scoped to blocks
- **Compile-time constants** - Values computed at compile time

### Key Principles

1. **Immutability is preferred** - Makes code easier to reason about
2. **Explicit is better than implicit** - Use type annotations when clarity is needed
3. **Minimize scope** - Declare variables in the smallest scope possible
4. **Meaningful names** - Variable names should express intent

## Variable Declaration

### Basic Syntax

```rust
// Declare an immutable variable
let x = 42;
let name = "Alice";
let is_ready = true;

// Variables must be initialized
let y;  // Error: uninitialized variable
```

### With Type Annotations

```rust
// Explicit type annotation
let count: int = 0;
let price: float = 19.99;
let message: string = "Hello";

// Type annotations are helpful when inference is ambiguous
let numbers: array<int> = [];  // Empty array needs type
```

### Multiple Declarations

```rust
// Declare multiple variables
let x = 1, y = 2, z = 3;  // Not supported in Husk

// Instead, use separate declarations
let x = 1;
let y = 2;
let z = 3;

// Or destructuring for related values
let (x, y, z) = (1, 2, 3);
```

## Mutability

### Mutable Variables

```rust
// Declare a mutable variable with 'mut'
let mut counter = 0;
counter = counter + 1;  // OK
counter = 10;          // OK

// Immutable by default
let value = 42;
value = 43;  // Error: cannot assign to immutable variable
```

### Mutability Rules

```rust
// Mutability is not part of the type
let mut x = 5;         // x is mutable
let y = x;             // y is immutable (gets value 5)
x = 10;                // OK
y = 20;                // Error: y is immutable

// Mutable references (if supported)
let mut data = [1, 2, 3];
let slice = &mut data;  // Mutable reference to data
```

### When to Use Mutability

```rust
// Good use of mutability: accumulation
let mut sum = 0;
for i in 1..=10 {
    sum = sum + i;
}

// Good use of mutability: state tracking
let mut is_running = true;
while is_running {
    // Process...
    if should_stop() {
        is_running = false;
    }
}

// Consider immutable alternatives
// Instead of:
let mut result = vec![];
for item in items {
    if item.is_valid() {
        result.push(item);
    }
}

// Prefer:
let result = items.filter(|item| item.is_valid()).collect();
```

## Constants

### Constant Declaration

```rust
// Constants use UPPER_SNAKE_CASE by convention
const MAX_SIZE: int = 100;
const PI: float = 3.14159;
const APP_NAME: string = "MyApp";

// Constants must have type annotations
const VALUE = 42;  // Error: missing type annotation
```

### Constant Rules

```rust
// Constants must be compile-time evaluable
const COMPUTED: int = 10 * 20;  // OK: compile-time computation
const DOUBLE_PI: float = PI * 2.0;  // OK: using other constants

// Cannot use runtime values
let x = 10;
const INVALID: int = x;  // Error: not a compile-time constant

// Constants are always immutable
const mut INVALID: int = 42;  // Error: constants cannot be mutable
```

### Constants vs Variables

```rust
// Use constants for:
// - Mathematical constants
const E: float = 2.71828;

// - Configuration values
const MAX_RETRIES: int = 3;
const TIMEOUT_SECONDS: int = 30;

// - Shared values across the application
const DEFAULT_PORT: int = 8080;

// Use variables for:
// - Values that might change
let current_users = 0;

// - Values computed at runtime
let timestamp = current_time();

// - Function parameters and local state
fn process(data: string) {
    let length = data.len();
}
```

## Type Annotations

### When to Annotate

```rust
// Type inference works well for literals
let x = 42;              // Inferred as int
let y = 3.14;            // Inferred as float
let name = "Alice";      // Inferred as string

// Annotations needed for ambiguous cases
let items: array<string> = [];  // Empty collection
let result: Result<int, string> = parse_number(input);

// Annotations for clarity
let timeout_ms: int = 5000;  // Clear unit of measurement
let is_authenticated: bool = check_auth();  // Clear intent
```

### Annotation Syntax

```rust
// Basic syntax
let variable: Type = value;

// Complex types
let numbers: array<int> = [1, 2, 3];
let lookup: HashMap<string, User> = HashMap::new();
let handler: fn(int) -> int = double;

// Multiple annotations in destructuring
let (x, y): (int, int) = get_coordinates();
```

## Scoping Rules

### Block Scope

```rust
// Variables are scoped to blocks
{
    let x = 42;
    println!("x is {}", x);  // OK
}
println!("x is {}", x);  // Error: x not in scope

// Function scope
fn example() {
    let y = 10;
    // y is only accessible within this function
}
```

### Nested Scopes

```rust
let x = 1;
{
    let y = 2;
    {
        let z = 3;
        println!("{} {} {}", x, y, z);  // All accessible
    }
    println!("{} {}", x, y);  // OK
    println!("{}", z);  // Error: z not in scope
}
```

### Loop Scopes

```rust
// Loop variables are scoped to the loop
for i in 0..10 {
    let squared = i * i;
    // i and squared are only available here
}
// i is not accessible here

// While loop scope
let mut counter = 0;
while counter < 10 {
    let temp = counter * 2;
    counter = counter + 1;
}
// temp is not accessible here, but counter is
```

## Shadowing

### Variable Shadowing

```rust
// Shadowing allows reusing names
let x = 5;
let x = "hello";  // Shadows previous x
println!("{}", x);  // Prints: hello

// Different from mutation
let mut y = 5;
y = 10;  // Mutation, same variable
let y = "world";  // Shadowing, new variable
```

### Shadowing with Type Changes

```rust
// Shadowing can change types
let value = "42";
let value: int = value.parse().unwrap();  // New variable with same name

// Useful for transformations
let input = read_line();
let input = input.trim();
let input: int = input.parse().unwrap_or(0);
```

### Scope and Shadowing

```rust
let x = 1;
{
    let x = 2;  // Shadows outer x
    println!("Inner x: {}", x);  // Prints: 2
}
println!("Outer x: {}", x);  // Prints: 1
```

## Destructuring

### Tuple Destructuring

```rust
// Destructure tuples into variables
let point = (10, 20);
let (x, y) = point;

// With type annotations
let (name, age): (string, int) = ("Alice", 30);

// Ignore values with _
let (first, _) = (1, 2);
```

### Array Destructuring

```rust
// Destructure arrays (fixed size)
let rgb = [255, 128, 0];
let [r, g, b] = rgb;

// Partial destructuring (if supported)
let numbers = [1, 2, 3, 4, 5];
let [first, second, ..rest] = numbers;
```

### Struct Destructuring

```rust
struct Point {
    x: int,
    y: int,
}

let p = Point { x: 10, y: 20 };

// Destructure struct fields
let Point { x, y } = p;

// Rename during destructuring
let Point { x: x_coord, y: y_coord } = p;
```

## Best Practices

### Naming Conventions

```rust
// Variables: snake_case
let user_name = "Alice";
let total_count = 42;

// Constants: UPPER_SNAKE_CASE
const MAX_BUFFER_SIZE: int = 1024;
const DEFAULT_TIMEOUT: int = 30;

// Type parameters: CamelCase
fn identity<T>(value: T) -> T { value }
```

### Initialization

```rust
// Initialize variables when declared
let count = 0;  // Good

// Avoid uninitialized variables
let count;      // Bad (if allowed)
count = 0;

// Use meaningful initial values
let name = "";  // Better than uninitialized
let items = []; // Clear empty state
```

### Scope Management

```rust
// Minimize variable scope
// Bad: wide scope
let mut result = 0;
for i in 0..10 {
    result = result + i;
}
use_result(result);

// Good: narrow scope
let result = {
    let mut sum = 0;
    for i in 0..10 {
        sum = sum + i;
    }
    sum
};
use_result(result);
```

### Mutability Guidelines

```rust
// Prefer immutability
// Bad: unnecessary mutability
let mut x = 5;
let mut y = 10;
let mut sum = x + y;

// Good: immutable
let x = 5;
let y = 10;
let sum = x + y;

// Use mutability only when needed
let mut counter = 0;
loop {
    counter = counter + 1;
    if counter > 10 { break; }
}
```

## Common Patterns

### Configuration Constants

```rust
// Group related constants
mod config {
    pub const SERVER_PORT: int = 8080;
    pub const MAX_CONNECTIONS: int = 100;
    pub const TIMEOUT_SECONDS: int = 30;
}

// Use constants in code
let server = Server::new(config::SERVER_PORT);
```

### Builder Pattern Variables

```rust
// Accumulate configuration
let mut builder = ServerBuilder::new();
builder = builder.port(8080);
builder = builder.max_connections(100);
let server = builder.build();

// Or with method chaining
let server = ServerBuilder::new()
    .port(8080)
    .max_connections(100)
    .build();
```

### State Tracking

```rust
// Track state with meaningful variable names
let mut is_running = true;
let mut error_count = 0;
let mut last_update = current_time();

while is_running {
    match process_item() {
        Ok(_) => last_update = current_time(),
        Err(_) => {
            error_count = error_count + 1;
            if error_count > MAX_ERRORS {
                is_running = false;
            }
        }
    }
}
```

## Troubleshooting

### Common Errors

**"cannot assign to immutable variable"**
```rust
let x = 5;
x = 10;  // Error

// Fix: Make it mutable
let mut x = 5;
x = 10;  // OK
```

**"use of uninitialized variable"**
```rust
let x;
println!("{}", x);  // Error

// Fix: Initialize before use
let x = 0;
println!("{}", x);  // OK
```

**"variable not found in scope"**
```rust
{
    let x = 42;
}
println!("{}", x);  // Error: x not in scope

// Fix: Declare in appropriate scope
let x = 42;
println!("{}", x);  // OK
```

### Performance Considerations

```rust
// Constants are inlined at compile time
const MULTIPLIER: int = 100;
let result = value * MULTIPLIER;  // No runtime lookup

// Avoid recreating immutable values
// Bad: creates new string each iteration
for i in 0..1000 {
    let prefix = "Item: ";  // Allocated 1000 times
}

// Good: create once
let prefix = "Item: ";
for i in 0..1000 {
    // Use prefix
}
```

## JavaScript Transpilation

When transpiled to JavaScript:

```rust
// Husk
let x = 42;
let mut y = 0;
const PI: float = 3.14159;
```

Becomes:
```javascript
// JavaScript
const x = 42;
let y = 0;
const PI = 3.14159;
```

Note that Husk's immutable `let` becomes JavaScript's `const`, while mutable variables use JavaScript's `let`.

## Related Topics

- [Type System](types.md) - Understanding Husk's types
- [Functions](functions.md) - Function parameters and returns
- [Pattern Matching](pattern-matching.md) - Destructuring in patterns
- [Scoping Rules](control-flow.md#scoping) - Block and control flow scoping
- [Best Practices](../advanced/best-practices.md) - General coding guidelines

---

*Variables and constants form the foundation of any Husk program. Master these concepts to write clear, efficient, and maintainable code.*