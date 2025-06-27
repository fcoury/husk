# Functions

Functions are the primary way to organize and reuse code in Husk. This guide covers function definition, parameters, return types, and advanced features like closures and higher-order functions.

## Table of Contents

- [Overview](#overview)
- [Function Definition](#function-definition)
- [Parameters](#parameters)
- [Return Types](#return-types)
- [Function Calls](#function-calls)
- [Methods](#methods)
- [Closures](#closures)
- [Higher-Order Functions](#higher-order-functions)
- [Recursion](#recursion)
- [Generic Functions](#generic-functions)
- [Function Overloading](#function-overloading)
- [Best Practices](#best-practices)
- [Common Patterns](#common-patterns)
- [JavaScript Transpilation](#javascript-transpilation)
- [Related Topics](#related-topics)

## Overview

Husk functions provide:
- **Type safety** - Parameters and returns are statically typed
- **First-class functions** - Functions can be passed as values
- **Closures** - Functions can capture their environment
- **Type inference** - Return types can often be inferred

### Key Concepts

1. **Functions are expressions** - They return the last expression
2. **No function overloading** - Each function has a unique name
3. **Pass by value** - Arguments are copied (or moved)
4. **Early returns** - Use `return` for early exit

## Function Definition

### Basic Syntax

```rust
// Simple function
fn greet() {
    println!("Hello, World!");
}

// Function with parameters
fn add(a: int, b: int) -> int {
    a + b  // Last expression is returned
}

// Function with explicit return
fn multiply(a: int, b: int) -> int {
    return a * b;
}

// No return value (unit type)
fn print_sum(a: int, b: int) {
    println!("Sum: {}", a + b);
    // Implicitly returns ()
}
```

### Function Signatures

```rust
// Full signature with all parts
fn function_name(param1: Type1, param2: Type2) -> ReturnType {
    // function body
}

// Multiple parameters
fn process_data(
    name: string,
    age: int,
    is_active: bool
) -> string {
    format!("{} ({}) - {}", name, age, if is_active { "Active" } else { "Inactive" })
}

// No parameters
fn get_timestamp() -> int {
    current_time_millis()
}
```

### Function Naming

```rust
// Use snake_case for function names
fn calculate_total() -> int { /* ... */ }
fn is_valid_email(email: string) -> bool { /* ... */ }
fn parse_config_file(path: string) -> Config { /* ... */ }

// Predicates often start with is_, has_, or can_
fn is_empty(s: string) -> bool { s.len() == 0 }
fn has_permission(user: User, action: string) -> bool { /* ... */ }
fn can_edit(document: Document) -> bool { /* ... */ }

// Conversion functions use to_ or from_
fn to_string(value: int) -> string { /* ... */ }
fn from_json(json: string) -> Result<Data, Error> { /* ... */ }
```

## Parameters

### Parameter Types

```rust
// Required parameters
fn greet(name: string) {
    println!("Hello, {}!", name);
}

// Multiple parameters
fn calculate(base: float, rate: float, time: int) -> float {
    base * rate * time as float
}

// Parameters must have type annotations
fn invalid(x) -> int { x }  // Error: missing type annotation
```

### Parameter Patterns

```rust
// Destructuring parameters
fn print_point((x, y): (int, int)) {
    println!("Point: ({}, {})", x, y);
}

// Struct destructuring
struct User { name: string, age: int }

fn greet_user(User { name, age }: User) {
    println!("Hello {}, age {}", name, age);
}

// Ignored parameters
fn ignore_second(first: int, _: int) -> int {
    first
}
```

### Default Parameters (Planned)

```rust
// Default parameters are planned but not yet implemented
fn connect(host: string = "localhost", port: int = 8080) -> Connection {
    // Implementation
}

// Current workaround: use Option
fn connect_with_defaults(host: Option<string>, port: Option<int>) -> Connection {
    let host = host.unwrap_or("localhost");
    let port = port.unwrap_or(8080);
    // Implementation
}
```

## Return Types

### Explicit Returns

```rust
// Explicit return type
fn double(x: int) -> int {
    x * 2
}

// Multiple return points
fn sign(x: int) -> int {
    if x > 0 {
        return 1;
    }
    if x < 0 {
        return -1;
    }
    0  // Last expression
}

// Early return pattern
fn divide(a: float, b: float) -> Option<float> {
    if b == 0.0 {
        return None;
    }
    Some(a / b)
}
```

### Implicit Returns

```rust
// Last expression is returned
fn add(a: int, b: int) -> int {
    a + b  // No semicolon, this is returned
}

// Be careful with semicolons
fn wrong() -> int {
    42;  // This returns () due to semicolon
}  // Error: expected int, found ()

// Correct version
fn correct() -> int {
    42  // No semicolon
}
```

### Unit Type Returns

```rust
// Functions that don't return a value return ()
fn print_message(msg: string) {
    println!("{}", msg);
    // Implicitly returns ()
}

// Explicit unit return
fn do_something() -> () {
    // Implementation
}

// Common pattern: Result with unit
fn save_file(path: string, data: string) -> Result<(), Error> {
    // Returns Ok(()) on success
}
```

## Function Calls

### Basic Calls

```rust
// Calling functions
let result = add(5, 3);
greet("Alice");

// Nested calls
let total = multiply(add(2, 3), 4);

// Using return values
let sum = calculate_sum([1, 2, 3, 4, 5]);
if sum > 10 {
    println!("Large sum: {}", sum);
}
```

### Function References

```rust
// Functions are first-class values
let op = add;  // Function reference
let result = op(10, 20);  // Call through reference

// Function type annotations
let operation: fn(int, int) -> int = multiply;

// Array of functions
let operations = [add, subtract, multiply, divide];
let result = operations[0](10, 5);
```

## Methods

### Associated Functions

```rust
struct Rectangle {
    width: float,
    height: float,
}

impl Rectangle {
    // Associated function (no self)
    fn new(width: float, height: float) -> Rectangle {
        Rectangle { width, height }
    }
    
    // Method (takes self)
    fn area(&self) -> float {
        self.width * self.height
    }
    
    // Mutable method
    fn double_size(&mut self) {
        self.width = self.width * 2.0;
        self.height = self.height * 2.0;
    }
    
    // Consuming method
    fn into_square(self) -> Square {
        Square { side: self.width }
    }
}

// Usage
let rect = Rectangle::new(10.0, 20.0);  // Associated function
let area = rect.area();  // Method call
```

### Method Receivers

```rust
impl MyType {
    // Immutable borrow
    fn read(&self) -> &Data {
        &self.data
    }
    
    // Mutable borrow
    fn modify(&mut self) {
        self.count = self.count + 1;
    }
    
    // Move ownership
    fn consume(self) -> NewType {
        NewType::from(self)
    }
}
```

## Closures

### Basic Closures

```rust
// Closure syntax
let add_one = |x| x + 1;
let result = add_one(5);  // 6

// With type annotations
let multiply: fn(int, int) -> int = |a, b| a * b;

// Multi-line closures
let complex = |x: int| {
    let doubled = x * 2;
    let squared = doubled * doubled;
    squared + 1
};
```

### Capturing Variables

```rust
// Closures can capture environment
let factor = 10;
let scale = |x| x * factor;

let result = scale(5);  // 50

// Mutable capture
let mut count = 0;
let mut increment = || {
    count = count + 1;
    count
};

println!("{}", increment());  // 1
println!("{}", increment());  // 2
```

For more details on closures, see the [Closures Guide](../language_features/CLOSURES.md).

## Higher-Order Functions

### Functions as Parameters

```rust
// Function that takes another function
fn apply_twice(f: fn(int) -> int, x: int) -> int {
    f(f(x))
}

let double = |x| x * 2;
let result = apply_twice(double, 5);  // 20

// Generic higher-order function
fn map_array<T, U>(arr: array<T>, f: fn(T) -> U) -> array<U> {
    let mut result = [];
    for item in arr {
        result.push(f(item));
    }
    result
}
```

### Functions Returning Functions

```rust
// Function that returns a closure
fn make_adder(n: int) -> fn(int) -> int {
    |x| x + n
}

let add_five = make_adder(5);
let result = add_five(10);  // 15

// Factory pattern
fn create_validator(min: int, max: int) -> fn(int) -> bool {
    |value| value >= min && value <= max
}

let is_valid_age = create_validator(0, 150);
```

### Common Higher-Order Functions

```rust
// Built-in higher-order functions
let numbers = [1, 2, 3, 4, 5];

// map
let doubled = numbers.map(|x| x * 2);  // [2, 4, 6, 8, 10]

// filter
let evens = numbers.filter(|x| x % 2 == 0);  // [2, 4]

// fold (reduce)
let sum = numbers.fold(0, |acc, x| acc + x);  // 15

// Chain operations
let result = numbers
    .filter(|x| x % 2 == 0)
    .map(|x| x * x)
    .fold(0, |acc, x| acc + x);  // 20 (4 + 16)
```

## Recursion

### Basic Recursion

```rust
// Factorial
fn factorial(n: int) -> int {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

// Fibonacci
fn fibonacci(n: int) -> int {
    match n {
        0 => 0,
        1 => 1,
        _ => fibonacci(n - 1) + fibonacci(n - 2)
    }
}
```

### Tail Recursion

```rust
// Tail-recursive factorial
fn factorial_tail(n: int) -> int {
    fn factorial_helper(n: int, acc: int) -> int {
        if n <= 1 {
            acc
        } else {
            factorial_helper(n - 1, n * acc)
        }
    }
    factorial_helper(n, 1)
}

// Tail-recursive sum
fn sum_list(numbers: array<int>) -> int {
    fn sum_helper(numbers: array<int>, index: int, acc: int) -> int {
        if index >= numbers.len() {
            acc
        } else {
            sum_helper(numbers, index + 1, acc + numbers[index])
        }
    }
    sum_helper(numbers, 0, 0)
}
```

### Mutual Recursion

```rust
// Functions can call each other
fn is_even(n: int) -> bool {
    if n == 0 {
        true
    } else {
        is_odd(n - 1)
    }
}

fn is_odd(n: int) -> bool {
    if n == 0 {
        false
    } else {
        is_even(n - 1)
    }
}
```

## Generic Functions

### Basic Generics

```rust
// Generic function
fn identity<T>(value: T) -> T {
    value
}

// Multiple type parameters
fn pair<A, B>(first: A, second: B) -> (A, B) {
    (first, second)
}

// Using generics
let x = identity(42);        // T inferred as int
let y = identity("hello");   // T inferred as string
let p = pair(10, "ten");     // (int, string)
```

### Constrained Generics (Planned)

```rust
// Type constraints are planned
fn sum<T: Numeric>(a: T, b: T) -> T {
    a + b
}

// Current workaround: separate implementations
fn sum_int(a: int, b: int) -> int { a + b }
fn sum_float(a: float, b: float) -> float { a + b }
```

## Function Overloading

Husk does not support function overloading. Each function must have a unique name.

```rust
// Not allowed: same name, different parameters
fn process(x: int) -> int { x * 2 }
fn process(x: string) -> string { x.upper() }  // Error!

// Instead, use different names
fn process_int(x: int) -> int { x * 2 }
fn process_string(x: string) -> string { x.upper() }

// Or use generics when appropriate
fn process<T>(x: T, processor: fn(T) -> T) -> T {
    processor(x)
}
```

## Best Practices

### Function Length

```rust
// Keep functions short and focused
// Bad: doing too much
fn process_user_data(data: string) -> Result<User, Error> {
    // Parse JSON
    // Validate fields
    // Check permissions
    // Save to database
    // Send email
    // Log activity
    // ... 100 lines later
}

// Good: single responsibility
fn parse_user(data: string) -> Result<UserData, ParseError> { /* ... */ }
fn validate_user(data: UserData) -> Result<User, ValidationError> { /* ... */ }
fn save_user(user: User) -> Result<(), DatabaseError> { /* ... */ }
```

### Parameter Count

```rust
// Avoid too many parameters
// Bad: too many parameters
fn create_user(
    name: string,
    email: string,
    age: int,
    address: string,
    phone: string,
    role: string
) -> User {
    // ...
}

// Good: use a struct
struct UserData {
    name: string,
    email: string,
    age: int,
    address: string,
    phone: string,
    role: string,
}

fn create_user(data: UserData) -> User {
    // ...
}
```

### Error Handling

```rust
// Use Result for fallible operations
fn parse_number(s: string) -> Result<int, ParseError> {
    // Implementation
}

// Propagate errors with ?
fn calculate(input: string) -> Result<int, Error> {
    let n = parse_number(input)?;
    let doubled = n * 2;
    Ok(doubled)
}
```

### Documentation

```rust
// Document public functions
/// Calculates the area of a circle given its radius.
/// 
/// # Arguments
/// * `radius` - The radius of the circle
/// 
/// # Returns
/// The area of the circle
/// 
/// # Example
/// ```
/// let area = calculate_circle_area(5.0);
/// assert_eq!(area, 78.53981633974483);
/// ```
fn calculate_circle_area(radius: float) -> float {
    PI * radius * radius
}
```

## Common Patterns

### Builder Functions

```rust
// Builder pattern for complex objects
fn build_server() -> ServerBuilder {
    ServerBuilder::new()
}

impl ServerBuilder {
    fn port(mut self, port: int) -> ServerBuilder {
        self.port = Some(port);
        self
    }
    
    fn host(mut self, host: string) -> ServerBuilder {
        self.host = Some(host);
        self
    }
    
    fn build(self) -> Result<Server, Error> {
        // Validate and construct
    }
}

// Usage
let server = build_server()
    .port(8080)
    .host("localhost")
    .build()?;
```

### Callback Pattern

```rust
// Callbacks for async operations
fn fetch_data(url: string, callback: fn(Result<Data, Error>)) {
    // Async implementation
}

// Usage
fetch_data("https://api.example.com", |result| {
    match result {
        Ok(data) => process_data(data),
        Err(e) => handle_error(e),
    }
});
```

### Factory Functions

```rust
// Factory functions create configured instances
fn create_default_logger() -> Logger {
    Logger::new()
        .level(LogLevel::Info)
        .output(LogOutput::Console)
        .format(LogFormat::Json)
}

fn create_test_database() -> Database {
    Database::memory()
        .schema(test_schema())
        .seed_data(test_data())
}
```

## JavaScript Transpilation

Husk functions transpile cleanly to JavaScript:

```rust
// Husk
fn add(a: int, b: int) -> int {
    a + b
}

fn greet(name: string) {
    println!("Hello, {}!", name);
}

let double = |x| x * 2;
```

Becomes:
```javascript
// JavaScript
function add(a, b) {
    return a + b;
}

function greet(name) {
    console.log(`Hello, ${name}!`);
}

const double = x => x * 2;
```

Methods transpile to class methods:
```rust
// Husk
impl Rectangle {
    fn area(&self) -> float {
        self.width * self.height
    }
}
```

```javascript
// JavaScript
class Rectangle {
    area() {
        return this.width * this.height;
    }
}
```

## Related Topics

- [Closures](../language_features/CLOSURES.md) - Deep dive into closures
- [Type System](types.md) - Function types and signatures
- [Error Handling](error-handling.md) - Result and Option in functions
- [Pattern Matching](pattern-matching.md) - Patterns in parameters
- [Generic Programming](generics.md) - Generic functions (planned)

---

*Functions are the building blocks of Husk programs. Master them to write modular, reusable, and maintainable code.*