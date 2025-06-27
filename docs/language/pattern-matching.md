# Pattern Matching

Pattern matching is one of Husk's most powerful features, allowing you to destructure data and control program flow based on the shape and content of values. This guide covers all aspects of pattern matching in Husk.

## Table of Contents

- [Overview](#overview)
- [Match Expressions](#match-expressions)
- [Pattern Types](#pattern-types)
  - [Literal Patterns](#literal-patterns)
  - [Variable Patterns](#variable-patterns)
  - [Wildcard Patterns](#wildcard-patterns)
  - [Tuple Patterns](#tuple-patterns)
  - [Array Patterns](#array-patterns)
  - [Struct Patterns](#struct-patterns)
  - [Enum Patterns](#enum-patterns)
  - [Range Patterns](#range-patterns)
- [Pattern Guards](#pattern-guards)
- [Pattern Binding](#pattern-binding)
- [Exhaustiveness](#exhaustiveness)
- [Let Patterns](#let-patterns)
- [Advanced Patterns](#advanced-patterns)
- [Best Practices](#best-practices)
- [Common Patterns](#common-patterns)
- [JavaScript Transpilation](#javascript-transpilation)
- [Related Topics](#related-topics)

## Overview

Pattern matching provides:
- **Destructuring** - Extract values from complex data structures
- **Control flow** - Branch based on data shape
- **Exhaustiveness checking** - Compiler ensures all cases are handled
- **Binding** - Bind matched values to variables

### Key Principles

1. **Patterns must be exhaustive** - Cover all possible cases
2. **First match wins** - Patterns are tested in order
3. **Bindings are immutable** - Pattern variables are immutable by default
4. **Refutability** - Some patterns always match, others may fail

## Match Expressions

### Basic Syntax

```rust
// Basic match expression
let result = match value {
    pattern1 => expression1,
    pattern2 => expression2,
    pattern3 => expression3,
    _ => default_expression,
};

// Match with blocks
let result = match value {
    0 => {
        println!("Zero");
        "none"
    },
    1 => {
        println!("One");
        "single"
    },
    n => {
        println!("Multiple: {}", n);
        "many"
    }
};
```

### Match Arms

```rust
// Single expression arms
match number {
    0 => "zero",
    1 => "one",
    _ => "other",
}

// Block arms for complex logic
match command {
    "start" => {
        initialize();
        run();
        "started"
    },
    "stop" => {
        cleanup();
        shutdown();
        "stopped"
    },
    _ => "unknown command",
}

// Multiple patterns per arm
match character {
    'a' | 'e' | 'i' | 'o' | 'u' => "vowel",
    'A' | 'E' | 'I' | 'O' | 'U' => "uppercase vowel",
    _ => "consonant",
}
```

## Pattern Types

### Literal Patterns

```rust
// Matching literals
match value {
    0 => "zero",
    1 => "one",
    42 => "the answer",
    _ => "something else",
}

// String literals
match name {
    "Alice" => "Hello, Alice!",
    "Bob" => "Hey Bob!",
    "" => "No name provided",
    _ => "Nice to meet you!",
}

// Boolean literals
match is_ready {
    true => start_process(),
    false => wait(),
}
```

### Variable Patterns

```rust
// Binding to variables
match value {
    0 => println!("Zero"),
    n => println!("Number: {}", n),  // n binds to the value
}

// Variable patterns always match
let point = (10, 20);
match point {
    (x, y) => println!("Point at ({}, {})", x, y),
}

// Naming for clarity
match result {
    Ok(data) => process(data),
    Err(error) => handle_error(error),
}
```

### Wildcard Patterns

```rust
// Underscore ignores the value
match value {
    0 => "zero",
    1 => "one",
    _ => "other",  // Matches anything
}

// Partial wildcards
let point = (10, 20, 30);
match point {
    (x, _, z) => println!("x: {}, z: {}", x, z),  // Ignores y
}

// Struct wildcards
struct User { name: string, age: int, email: string }
match user {
    User { name, .. } => println!("User: {}", name),  // Ignores other fields
}
```

### Tuple Patterns

```rust
// Match tuples
let pair = (1, "hello");
match pair {
    (0, _) => "first is zero",
    (1, "hello") => "specific match",
    (1, s) => format!("one and {}", s),
    (n, s) => format!("{} and {}", n, s),
}

// Nested tuples
let nested = ((1, 2), (3, 4));
match nested {
    ((a, b), (c, d)) => a + b + c + d,
}

// Different sizes
match get_tuple() {
    () => "unit",
    (a,) => format!("single: {}", a),
    (a, b) => format!("pair: {} {}", a, b),
    (a, b, c) => format!("triple: {} {} {}", a, b, c),
}
```

### Array Patterns

```rust
// Fixed-size array matching
let rgb = [255, 0, 0];
match rgb {
    [255, 0, 0] => "red",
    [0, 255, 0] => "green",
    [0, 0, 255] => "blue",
    [r, g, b] => format!("rgb({}, {}, {})", r, g, b),
}

// Array patterns with wildcards
let numbers = [1, 2, 3, 4, 5];
match numbers {
    [first, .., last] => println!("First: {}, Last: {}", first, last),
    [] => println!("Empty array"),
}

// Slice patterns (if supported)
match slice {
    [] => "empty",
    [single] => format!("one element: {}", single),
    [first, second] => format!("two elements: {} and {}", first, second),
    [first, .., last] => format!("multiple: {} to {}", first, last),
}
```

### Struct Patterns

```rust
struct Point { x: int, y: int }

// Match struct fields
let point = Point { x: 10, y: 20 };
match point {
    Point { x: 0, y: 0 } => "origin",
    Point { x: 0, y } => format!("on y-axis at {}", y),
    Point { x, y: 0 } => format!("on x-axis at {}", x),
    Point { x, y } => format!("at ({}, {})", x, y),
}

// Field renaming
match point {
    Point { x: x_coord, y: y_coord } => {
        println!("X: {}, Y: {}", x_coord, y_coord)
    }
}

// Struct patterns with ..
struct Person { name: string, age: int, city: string }
match person {
    Person { name, age, .. } => {
        println!("{} is {} years old", name, age)
    }
}
```

### Enum Patterns

```rust
enum Message {
    Quit,
    Move { x: int, y: int },
    Write(string),
    ChangeColor(int, int, int),
}

// Match enum variants
match message {
    Message::Quit => println!("Quit"),
    Message::Move { x, y } => println!("Move to ({}, {})", x, y),
    Message::Write(text) => println!("Text: {}", text),
    Message::ChangeColor(r, g, b) => {
        println!("Change color to rgb({}, {}, {})", r, g, b)
    }
}

// Option matching
match some_value {
    Some(x) => println!("Got value: {}", x),
    None => println!("No value"),
}

// Result matching
match operation() {
    Ok(result) => use_result(result),
    Err(error) => handle_error(error),
}
```

### Range Patterns

```rust
// Numeric ranges
match age {
    0..=12 => "child",
    13..=19 => "teenager",
    20..=64 => "adult",
    _ => "senior",
}

// Character ranges
match ch {
    'a'..='z' => "lowercase",
    'A'..='Z' => "uppercase",
    '0'..='9' => "digit",
    _ => "other",
}

// Exclusive ranges (if supported)
match score {
    0..50 => "fail",
    50..80 => "pass",
    80..100 => "excellent",
    _ => "invalid score",
}
```

## Pattern Guards

### Basic Guards

```rust
// Guards add additional conditions
match number {
    n if n < 0 => "negative",
    n if n == 0 => "zero",
    n if n % 2 == 0 => "positive even",
    n => "positive odd",
}

// Guards with bindings
match point {
    Point { x, y } if x == y => "on diagonal",
    Point { x, y } if x > y => "above diagonal",
    Point { x, y } => "below diagonal",
}

// Complex guards
match user {
    User { age, is_verified: true } if age >= 18 => {
        grant_full_access()
    },
    User { is_verified: true, .. } => {
        grant_limited_access()
    },
    _ => deny_access(),
}
```

### Guard Evaluation

```rust
// Guards are evaluated after pattern matching
let pair = (2, 4);
match pair {
    (x, y) if x + y == 6 => "sum is 6",
    (x, y) if x * y == 8 => "product is 8",  // This matches
    _ => "other",
}

// Guards can use external variables
let threshold = 100;
match value {
    n if n > threshold => "above threshold",
    n if n == threshold => "at threshold",
    _ => "below threshold",
}
```

## Pattern Binding

### @ Bindings

```rust
// Bind while matching
match age {
    teen @ 13..=19 => println!("Teenager of age {}", teen),
    adult @ 20..=64 => println!("Adult of age {}", adult),
    other => println!("Age {}", other),
}

// Binding with structs
match message {
    msg @ Message::Write(_) => {
        log_message(&msg);
        process_message(msg);
    },
    other => handle_other(other),
}

// Complex binding
match value {
    n @ 1..=10 if n % 2 == 0 => println!("Even number {} in range", n),
    n @ 1..=10 => println!("Odd number {} in range", n),
    n => println!("Number {} out of range", n),
}
```

### Nested Bindings

```rust
// Bind nested values
struct Container { value: Option<int> }

match container {
    Container { value: Some(n @ 1..=10) } => {
        println!("Container has small value: {}", n)
    },
    Container { value: Some(n) } => {
        println!("Container has value: {}", n)
    },
    Container { value: None } => {
        println!("Container is empty")
    },
}
```

## Exhaustiveness

### Exhaustive Matching

```rust
// Compiler ensures all cases are covered
enum Status {
    Active,
    Inactive,
    Pending,
}

// Must handle all variants
match status {
    Status::Active => "active",
    Status::Inactive => "inactive",
    Status::Pending => "pending",
    // No _ needed - all cases covered
}

// Non-exhaustive match is an error
match status {
    Status::Active => "active",
    Status::Inactive => "inactive",
    // Error: missing Status::Pending
}
```

### Wildcard for Exhaustiveness

```rust
// Use _ for remaining cases
match number {
    0 => "zero",
    1 => "one",
    2 => "two",
    _ => "many",  // Catches all other values
}

// Or use a variable
match number {
    0 => "zero",
    1 => "one",
    2 => "two",
    n => format!("number: {}", n),
}
```

## Let Patterns

### Destructuring with Let

```rust
// Pattern in let statements
let (x, y) = (10, 20);
let Point { x, y } = get_point();

// With type annotations
let (name, age): (string, int) = get_user_info();

// Ignoring values
let (first, _, third) = (1, 2, 3);

// Struct destructuring
let User { name, email, .. } = get_user();
```

### Refutable Patterns

```rust
// Some patterns can fail
let Some(value) = maybe_value;  // Error if None

// Use if let for refutable patterns
if let Some(value) = maybe_value {
    use_value(value);
}

// Or match
let value = match maybe_value {
    Some(v) => v,
    None => return,
};
```

### While Let

```rust
// Pattern matching in loops
while let Some(item) = iterator.next() {
    process(item);
}

// With more complex patterns
while let Ok(line) = read_line() {
    if line.is_empty() {
        break;
    }
    process_line(line);
}
```

## Advanced Patterns

### Or Patterns

```rust
// Multiple patterns with |
match value {
    0 | 1 | 2 => "small",
    3 | 4 | 5 => "medium",
    _ => "large",
}

// With bindings (if supported)
match message {
    Message::Write(s) | Message::Log(s) => {
        record_text(s);
    },
    _ => {},
}
```

### Nested Patterns

```rust
// Deep pattern matching
enum Container {
    Box(Option<Value>),
    List(array<Value>),
}

match container {
    Container::Box(Some(Value::Number(n))) => {
        println!("Boxed number: {}", n)
    },
    Container::Box(Some(Value::Text(s))) => {
        println!("Boxed text: {}", s)
    },
    Container::Box(None) => {
        println!("Empty box")
    },
    Container::List(items) => {
        println!("List with {} items", items.len())
    },
}
```

### Reference Patterns

```rust
// Matching references
let reference = &42;
match reference {
    &val => println!("Value: {}", val),
}

// Or dereference
match *reference {
    val => println!("Value: {}", val),
}

// With mutable references
let mut value = 42;
let reference = &mut value;
match reference {
    val => *val += 1,
}
```

## Best Practices

### Order Matters

```rust
// Put specific patterns first
match value {
    0 => "zero",           // Specific
    n if n < 0 => "negative",  // Less specific
    _ => "positive",       // Catch-all
}

// Wrong order (unreachable pattern)
match value {
    _ => "any",            // This catches everything!
    0 => "zero",           // Unreachable
}
```

### Use Guards Wisely

```rust
// Good: Clear and simple
match score {
    s if s >= 90 => "A",
    s if s >= 80 => "B",
    s if s >= 70 => "C",
    _ => "F",
}

// Avoid: Complex guards
match user {
    u if u.age >= 18 && u.is_verified && 
         u.country == "US" && u.score > 100 => {
        // Too complex
    },
    _ => {},
}

// Better: Extract to function
fn is_eligible(user: &User) -> bool {
    user.age >= 18 && 
    user.is_verified && 
    user.country == "US" && 
    user.score > 100
}

match user {
    u if is_eligible(&u) => grant_access(),
    _ => deny_access(),
}
```

### Avoid Deep Nesting

```rust
// Bad: Deeply nested patterns
match outer {
    Some(middle) => {
        match middle {
            Ok(inner) => {
                match inner {
                    Value::Number(n) => n,
                    _ => 0,
                }
            },
            Err(_) => 0,
        }
    },
    None => 0,
}

// Good: Flatten with multiple matches or methods
let result = outer
    .and_then(|middle| middle.ok())
    .and_then(|inner| match inner {
        Value::Number(n) => Some(n),
        _ => None,
    })
    .unwrap_or(0);
```

## Common Patterns

### Error Handling

```rust
// Pattern matching for error handling
fn process_data(input: string) -> Result<Data, Error> {
    let parsed = match parse(input) {
        Ok(data) => data,
        Err(e) => return Err(e),
    };
    
    let validated = match validate(parsed) {
        Ok(data) => data,
        Err(e) => return Err(e),
    };
    
    Ok(transform(validated))
}

// Or use ? operator
fn process_data(input: string) -> Result<Data, Error> {
    let parsed = parse(input)?;
    let validated = validate(parsed)?;
    Ok(transform(validated))
}
```

### State Machines

```rust
enum State {
    Init,
    Running { count: int },
    Paused { count: int, duration: int },
    Stopped,
}

impl State {
    fn transition(self, event: Event) -> State {
        match (self, event) {
            (State::Init, Event::Start) => {
                State::Running { count: 0 }
            },
            (State::Running { count }, Event::Pause) => {
                State::Paused { count, duration: 0 }
            },
            (State::Paused { count, .. }, Event::Resume) => {
                State::Running { count }
            },
            (state, _) => state,  // No transition
        }
    }
}
```

### Command Parsing

```rust
enum Command {
    Move { x: int, y: int },
    Attack { target: string },
    Defend,
    Quit,
}

fn parse_command(input: string) -> Option<Command> {
    let parts: array<string> = input.split(' ').collect();
    
    match parts.as_slice() {
        ["move", x, y] => {
            Some(Command::Move {
                x: x.parse().ok()?,
                y: y.parse().ok()?,
            })
        },
        ["attack", target] => {
            Some(Command::Attack {
                target: target.to_string(),
            })
        },
        ["defend"] => Some(Command::Defend),
        ["quit"] => Some(Command::Quit),
        _ => None,
    }
}
```

## JavaScript Transpilation

Pattern matching transpiles to JavaScript switch statements and conditionals:

```rust
// Husk
match value {
    0 => "zero",
    1 => "one",
    n => format!("number: {}", n),
}

// Destructuring
let (x, y) = get_point();
match point {
    Point { x: 0, y: 0 } => "origin",
    Point { x, y } => format!("({}, {})", x, y),
}
```

Becomes:
```javascript
// JavaScript
const result = (() => {
    switch (value) {
        case 0: return "zero";
        case 1: return "one";
        default: return `number: ${value}`;
    }
})();

// Destructuring
const [x, y] = get_point();
const result = (() => {
    if (point.x === 0 && point.y === 0) {
        return "origin";
    } else {
        return `(${point.x}, ${point.y})`;
    }
})();
```

## Related Topics

- [Control Flow](control-flow.md) - Match in control flow
- [Enums](data-types.md#enums) - Enum patterns
- [Error Handling](error-handling.md) - Option and Result patterns
- [Variables](variables.md) - Pattern binding in let
- [Functions](functions.md) - Patterns in parameters

---

*Pattern matching is the heart of expressive Husk code. Master it to write concise, safe, and elegant programs.*