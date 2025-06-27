# Type System

Husk features a static type system with local type inference, providing safety and expressiveness without verbosity. This guide covers Husk's type system in detail.

## Table of Contents

- [Overview](#overview)
- [Primitive Types](#primitive-types)
- [Type Inference](#type-inference)
- [Type Annotations](#type-annotations)
- [Composite Types](#composite-types)
- [Custom Types](#custom-types)
- [Option and Result Types](#option-and-result-types)
- [Type Aliases](#type-aliases)
- [Generics](#generics)
- [Type Conversion](#type-conversion)
- [Common Patterns](#common-patterns)
- [Best Practices](#best-practices)

## Overview

Husk's type system provides:

- **Static typing** - All types are known at compile time
- **Type inference** - Types are inferred when possible
- **Memory safety** - No null pointer exceptions
- **Zero-cost abstractions** - Type safety without runtime overhead

### Key Principles

1. **Explicit is better than implicit** - When in doubt, annotate types
2. **Fail at compile time** - Catch errors before runtime
3. **No implicit conversions** - All conversions must be explicit
4. **Null safety** - Use Option<T> instead of null

## Primitive Types

### Numeric Types

```husk
// Integers
let a: int = 42;              // 32-bit signed integer
let b: int = -100;            // Negative integers
let c: int = 1_000_000;       // Underscores for readability

// Floating point
let pi: float = 3.14159;      // 64-bit floating point
let e: float = 2.718;         
let big: float = 1.23e10;     // Scientific notation

// Type inference
let inferred_int = 42;        // Inferred as int
let inferred_float = 3.14;    // Inferred as float
```

### Boolean Type

```husk
let is_ready: bool = true;
let is_done: bool = false;

// From expressions
let is_positive = 42 > 0;     // true
let is_even = 10 % 2 == 0;    // true
```

### String Type

```husk
let name: string = "Alice";
let greeting: string = "Hello, World!";
let empty: string = "";

// String interpolation
let message = format!("Hello, {}!", name);

// Multi-line strings
let poem = "Roses are red,
Violets are blue,
Husk is awesome,
And so are you!";
```

### Character Type

```husk
let ch: char = 'A';
let emoji: char = '🎉';
let newline: char = '\n';

// Characters are Unicode scalar values
let unicode: char = '\u{1F600}'; // 😀
```

### Unit Type

```husk
// Unit type () represents "no value"
fn print_message(msg: string) -> () {
    println!(msg);
    // Implicitly returns ()
}

// Often omitted in function signatures
fn print_message2(msg: string) {
    println!(msg);
}
```

## Type Inference

Husk infers types based on usage:

```husk
// Basic inference
let x = 42;                    // Inferred as int
let y = 3.14;                  // Inferred as float
let name = "Alice";            // Inferred as string

// From function returns
fn get_count() -> int { 10 }
let count = get_count();       // Inferred as int

// From operations
let sum = 5 + 10;              // Inferred as int
let product = 2.5 * 4.0;       // Inferred as float

// Array inference
let numbers = [1, 2, 3];       // Inferred as array<int>
let names = ["Alice", "Bob"];  // Inferred as array<string>
```

### Inference Limitations

```husk
// Sometimes inference needs help
let empty_array: array<int> = [];  // Type annotation required

// Generic functions may need hints
fn identity<T>(x: T) -> T { x }
let result: int = identity(42);    // Type hint helps inference
```

## Type Annotations

### When to Use Annotations

```husk
// Function parameters always need annotations
fn add(a: int, b: int) -> int {
    a + b
}

// Variable annotations for clarity
let count: int = 0;
let rate: float = 0.05;

// Complex types benefit from annotations
let users: array<User> = [];
let lookup: HashMap<string, int> = HashMap::new();
```

### Annotation Syntax

```husk
// Basic syntax
let variable: Type = value;

// Function syntax
fn function(param: Type) -> ReturnType {
    // body
}

// Array types
let numbers: array<int> = [1, 2, 3];

// Tuple types
let point: (int, int) = (10, 20);

// Option types
let maybe: Option<string> = Some("value");
```

## Composite Types

### Arrays

```husk
// Array declaration
let numbers: array<int> = [1, 2, 3, 4, 5];
let strings: array<string> = ["hello", "world"];

// Empty arrays need type annotation
let empty: array<int> = [];

// Array operations preserve types
let doubled = numbers.map(|n| n * 2);  // array<int>
let lengths = strings.map(|s| s.len()); // array<int>

// Nested arrays
let matrix: array<array<int>> = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
];
```

### Tuples

```husk
// Tuple types
let point: (int, int) = (10, 20);
let person: (string, int, bool) = ("Alice", 30, true);

// Accessing tuple elements
let x = point.0;  // 10
let y = point.1;  // 20

// Destructuring
let (name, age, is_student) = person;

// Functions returning tuples
fn get_coordinates() -> (float, float) {
    (3.14, 2.71)
}

let (lat, lon) = get_coordinates();
```

### Slices

```husk
// Slices are views into arrays
let numbers = [1, 2, 3, 4, 5];
let slice: &[int] = &numbers[1..4];  // [2, 3, 4]

// String slices
let text = "Hello, World!";
let hello: &str = &text[0..5];  // "Hello"
```

## Custom Types

### Structs

```husk
// Define a struct
struct Point {
    x: float,
    y: float,
}

// Create instances
let origin = Point { x: 0.0, y: 0.0 };
let p1 = Point { x: 3.0, y: 4.0 };

// Access fields
let distance = (p1.x * p1.x + p1.y * p1.y).sqrt();

// Nested structs
struct Rectangle {
    top_left: Point,
    bottom_right: Point,
}

// Methods (when implemented)
impl Point {
    fn distance_from_origin(&self) -> float {
        (self.x * self.x + self.y * self.y).sqrt()
    }
}
```

### Enums

```husk
// Simple enum
enum Direction {
    North,
    South,
    East,
    West,
}

// Enum with data
enum Message {
    Quit,
    Move { x: int, y: int },
    Write(string),
    ChangeColor(int, int, int),
}

// Using enums
let dir = Direction::North;
let msg = Message::Write("Hello".to_string());

// Pattern matching enums
match msg {
    Message::Quit => println!("Quitting"),
    Message::Move { x, y } => println!("Moving to {}, {}", x, y),
    Message::Write(text) => println!("Writing: {}", text),
    Message::ChangeColor(r, g, b) => println!("RGB: {}, {}, {}", r, g, b),
}
```

## Option and Result Types

### Option<T>

```husk
// Option represents a value that might be absent
enum Option<T> {
    Some(T),
    None,
}

// Creating Options
let some_number: Option<int> = Some(42);
let no_number: Option<int> = None;

// Common patterns
fn find_user(id: int) -> Option<User> {
    // Returns Some(user) or None
}

// Using Option
match find_user(1) {
    Some(user) => println!("Found: {}", user.name),
    None => println!("User not found"),
}

// Option methods
let x = Some(5);
let y = x.map(|n| n * 2);        // Some(10)
let z = x.unwrap_or(0);          // 5
```

### Result<T, E>

```husk
// Result represents success or failure
enum Result<T, E> {
    Ok(T),
    Err(E),
}

// Creating Results
fn divide(a: float, b: float) -> Result<float, string> {
    if b == 0.0 {
        Err("Division by zero".to_string())
    } else {
        Ok(a / b)
    }
}

// Using Result
match divide(10.0, 2.0) {
    Ok(result) => println!("Result: {}", result),
    Err(error) => println!("Error: {}", error),
}

// Result methods
let result = divide(10.0, 2.0)
    .map(|x| x * 2.0)              // Ok(10.0)
    .unwrap_or(0.0);               // 10.0
```

## Type Aliases

```husk
// Create type aliases for clarity
type UserId = int;
type Username = string;
type Coordinate = (float, float);

// Use aliases like regular types
fn get_user(id: UserId) -> Option<Username> {
    // Implementation
}

// Complex type aliases
type Result<T> = Result<T, string>;  // Default error type
type UserMap = HashMap<UserId, User>;

// Aliases improve readability
let users: UserMap = HashMap::new();
let location: Coordinate = (10.5, 20.3);
```

## Generics

```husk
// Generic functions
fn identity<T>(value: T) -> T {
    value
}

// Multiple type parameters
fn pair<A, B>(first: A, second: B) -> (A, B) {
    (first, second)
}

// Generic structs
struct Container<T> {
    value: T,
}

// Implementing for generic types
impl<T> Container<T> {
    fn new(value: T) -> Container<T> {
        Container { value }
    }
    
    fn get(&self) -> &T {
        &self.value
    }
}

// Using generics
let int_container = Container::new(42);
let string_container = Container::new("Hello");
```

## Type Conversion

### Explicit Conversions

```husk
// Numeric conversions
let x: int = 42;
let y: float = x as float;        // 42.0

// String conversions
let num: int = 123;
let s: string = num.to_string();  // "123"

// Parsing strings
let s = "42";
let n: int = s.parse().unwrap();  // 42

// Custom conversions
impl From<int> for MyType {
    fn from(n: int) -> MyType {
        MyType { value: n }
    }
}
```

### Safe Conversions

```husk
// Use Result for fallible conversions
fn safe_divide(a: int, b: int) -> Result<float, string> {
    if b == 0 {
        Err("Division by zero")
    } else {
        Ok((a as float) / (b as float))
    }
}

// TryFrom for checked conversions
let big: int = 1_000_000;
let small: i16 = big.try_into()?;  // May fail
```

## Common Patterns

### Builder Pattern

```husk
struct Config {
    host: string,
    port: int,
    timeout: int,
}

struct ConfigBuilder {
    host: Option<string>,
    port: Option<int>,
    timeout: Option<int>,
}

impl ConfigBuilder {
    fn new() -> ConfigBuilder {
        ConfigBuilder {
            host: None,
            port: None,
            timeout: None,
        }
    }
    
    fn host(mut self, host: string) -> ConfigBuilder {
        self.host = Some(host);
        self
    }
    
    fn build(self) -> Result<Config, string> {
        // Validate and build
    }
}
```

### Newtype Pattern

```husk
// Wrap primitive types for type safety
struct Meters(float);
struct Feet(float);

impl Meters {
    fn to_feet(&self) -> Feet {
        Feet(self.0 * 3.28084)
    }
}

// Prevents mixing units
fn calculate_area(width: Meters, height: Meters) -> float {
    width.0 * height.0
}
```

## Best Practices

### 1. Use Type Annotations for Clarity

```husk
// Good: Clear intent
let max_retries: int = 3;
let timeout_seconds: float = 30.0;

// Less clear
let max_retries = 3;
let timeout = 30.0;
```

### 2. Leverage Type Inference

```husk
// Good: Let inference work
let result = calculate_something();

// Unnecessary annotation
let result: int = calculate_something();  // If return type is clear
```

### 3. Use Custom Types

```husk
// Good: Domain-specific types
struct Email(string);
struct UserId(int);

// Less safe
let email: string = "user@example.com";
let user_id: int = 123;
```

### 4. Prefer Option over Null

```husk
// Good: Explicit absence
fn find_user(id: int) -> Option<User> {
    // ...
}

// Never use null or sentinel values
fn find_user(id: int) -> User {
    // Returns a "null user" - bad!
}
```

### 5. Use Result for Errors

```husk
// Good: Explicit error handling
fn parse_config(data: string) -> Result<Config, ParseError> {
    // ...
}

// Avoid: Panic on error
fn parse_config(data: string) -> Config {
    // panic! on error - bad!
}
```

## Common Pitfalls

### Type Mismatch

```husk
// Error: Type mismatch
let x: int = 3.14;  // Error: expected int, found float

// Fix: Use correct type
let x: float = 3.14;
```

### Missing Annotations

```husk
// Error: Cannot infer type
let empty = [];  // Error: type annotations needed

// Fix: Add annotation
let empty: array<int> = [];
```

### Implicit Conversions

```husk
// Error: No implicit conversion
let x: int = 42;
let y: float = x;  // Error: mismatched types

// Fix: Explicit conversion
let y: float = x as float;
```

## Related Topics

- [Variables and Constants](variables.md) - Variable declarations
- [Functions](functions.md) - Function types and signatures
- [Pattern Matching](pattern-matching.md) - Type patterns
- [Error Handling](error-handling.md) - Option and Result usage
- [Generics](generics.md) - Generic programming (planned)

---

*Understanding Husk's type system is key to writing safe, efficient code. The compiler is your friend - let it help you catch errors early!*