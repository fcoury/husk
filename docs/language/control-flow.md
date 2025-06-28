# Control Flow

Control flow determines the order in which code is executed. Husk provides several constructs for controlling program flow, including conditionals, loops, and pattern matching.

## Table of Contents

- [Overview](#overview)
- [If Expressions](#if-expressions)
- [Match Expressions](#match-expressions)
- [Loops](#loops)
  - [For Loops](#for-loops)
  - [While Loops](#while-loops)
  - [Loop Expressions](#loop-expressions)
- [Break and Continue](#break-and-continue)
- [Early Returns](#early-returns)
- [Scoping](#scoping)
- [Control Flow and Types](#control-flow-and-types)
- [Best Practices](#best-practices)
- [Common Patterns](#common-patterns)
- [JavaScript Transpilation](#javascript-transpilation)
- [Related Topics](#related-topics)

## Overview

Husk's control flow features:
- **Expression-based** - Most control flow constructs are expressions that return values
- **Pattern matching** - Powerful matching with exhaustiveness checking
- **Type safety** - All branches must return compatible types
- **Structured** - No goto or unstructured jumps

### Key Principles

1. **Expressions over statements** - Control flow returns values
2. **Exhaustive handling** - Cover all cases in match expressions
3. **Explicit control** - No implicit fallthrough
4. **Block scoping** - Variables are scoped to their blocks

## If Expressions

### Basic If

```rust
// Simple if expression
let age = 18;
if age >= 18 {
    println!("You can vote!");
}

// If-else
let status = if age >= 18 {
    "adult"
} else {
    "minor"
};

// Multiple conditions
let category = if age < 13 {
    "child"
} else if age < 18 {
    "teenager"
} else if age < 65 {
    "adult"
} else {
    "senior"
};
```

### If as Expression

```rust
// If expressions return values
let max = if a > b { a } else { b };

// All branches must return the same type
let result = if condition {
    42          // int
} else {
    "hello"     // string - Error: type mismatch
};

// Use blocks for complex logic
let discount = if is_member {
    let base = 0.1;
    if years > 5 {
        base + 0.05
    } else {
        base
    }
} else {
    0.0
};
```

### Conditional Chains

```rust
// Chain conditions with logical operators
if temperature > 30 && humidity > 80 {
    println!("Hot and humid!");
}

if username == "admin" || has_admin_role {
    grant_access();
}

// Complex conditions
if (age >= 18 && has_id) || has_parent_permission {
    allow_entry();
}
```

### Guard Patterns

```rust
// Early return pattern
fn process(value: Option<int>) -> int {
    if value.is_none() {
        return 0;  // Early return
    }
    
    let v = value.unwrap();
    v * 2
}

// Nested if simplification
// Instead of:
if condition1 {
    if condition2 {
        if condition3 {
            do_something();
        }
    }
}

// Prefer:
if !condition1 {
    return;
}
if !condition2 {
    return;
}
if !condition3 {
    return;
}
do_something();
```

## Match Expressions

### Basic Matching

```rust
// Match on values
let day = "Monday";
let day_type = match day {
    "Saturday" => "weekend",
    "Sunday" => "weekend",
    _ => "weekday"
};

// Match on numbers
let description = match number {
    0 => "zero",
    1 => "one",
    2..=9 => "single digit",
    10..=99 => "double digit",
    _ => "large number"
};
```

### Pattern Matching

```rust
// Match with patterns
let point = (0, 5);
match point {
    (0, 0) => println!("origin"),
    (0, y) => println!("on y-axis at {}", y),
    (x, 0) => println!("on x-axis at {}", x),
    (x, y) => println!("at ({}, {})", x, y)
}

// Destructuring in match
struct Color { r: int, g: int, b: int }

let color = Color { r: 255, g: 0, b: 0 };
match color {
    Color { r: 255, g: 0, b: 0 } => println!("red"),
    Color { r: 0, g: 255, b: 0 } => println!("green"),
    Color { r: 0, g: 0, b: 255 } => println!("blue"),
    Color { r, g, b } => println!("rgb({}, {}, {})", r, g, b)
}
```

For more details on pattern matching, see the [Pattern Matching Guide](pattern-matching.md).

## Loops

### For Loops

```rust
// Range iteration
for i in 0..10 {
    println!("{}", i);  // Prints 0 through 9
}

// Inclusive range
for i in 1..=10 {
    println!("{}", i);  // Prints 1 through 10
}

// Array iteration
let numbers = [1, 2, 3, 4, 5];
for num in numbers {
    println!("{}", num);
}

// With index
for (index, value) in numbers.iter().enumerate() {
    println!("{}: {}", index, value);
}
```

### Iterator Methods

```rust
// Functional iteration
let numbers = [1, 2, 3, 4, 5];

// Map
let doubled: array<int> = numbers.iter()
    .map(|x| x * 2)
    .collect();

// Filter and map
let result: array<int> = numbers.iter()
    .filter(|x| x % 2 == 0)
    .map(|x| x * x)
    .collect();

// For-each
numbers.iter().for_each(|n| {
    println!("Number: {}", n);
});
```

### While Loops

```rust
// Basic while loop
let mut count = 0;
while count < 10 {
    println!("{}", count);
    count = count + 1;
}

// While with condition
let mut input = read_line();
while input != "quit" {
    process(input);
    input = read_line();
}

// Infinite loop with break
while true {
    let data = fetch_data();
    if data.is_empty() {
        break;
    }
    process(data);
}
```

### Loop Expressions

```rust
// Infinite loop
loop {
    let input = read_line();
    if input == "exit" {
        break;
    }
    process(input);
}

// Loop with return value
let result = loop {
    counter = counter + 1;
    if counter == 10 {
        break counter * 2;  // Returns 20
    }
};

// Retry pattern
let data = loop {
    match fetch_data() {
        Ok(data) => break data,
        Err(e) => {
            println!("Retrying: {}", e);
            sleep(1000);
        }
    }
};
```

## Break and Continue

### Break Statement

```rust
// Break from loop
for i in 0..100 {
    if i == 42 {
        break;  // Exit loop
    }
    println!("{}", i);
}

// Break with value (in loop expressions)
let result = loop {
    let value = compute();
    if value > 100 {
        break value;  // Return value from loop
    }
};

// Break from nested loops (with labels - if supported)
'outer: for i in 0..10 {
    for j in 0..10 {
        if i * j > 30 {
            break 'outer;
        }
    }
}
```

### Continue Statement

```rust
// Skip iteration
for i in 0..10 {
    if i % 2 == 0 {
        continue;  // Skip even numbers
    }
    println!("{}", i);  // Only prints odd numbers
}

// Continue in while loop
let mut count = 0;
while count < 100 {
    count = count + 1;
    if !is_valid(count) {
        continue;
    }
    process(count);
}
```

## Early Returns

### Return Statement

```rust
// Early return from function
fn find_first_negative(numbers: array<int>) -> Option<int> {
    for num in numbers {
        if num < 0 {
            return Some(num);  // Early return
        }
    }
    None  // Normal return
}

// Multiple return points
fn classify(value: int) -> string {
    if value < 0 {
        return "negative";
    }
    if value == 0 {
        return "zero";
    }
    if value < 100 {
        return "small";
    }
    "large"  // Final return
}
```

### Guard Clauses

```rust
// Guard clause pattern
fn process_user(user: Option<User>) -> Result<(), Error> {
    // Early returns for error cases
    if user.is_none() {
        return Err("No user provided");
    }
    
    let user = user.unwrap();
    if !user.is_active {
        return Err("User is not active");
    }
    
    if !user.has_permission("process") {
        return Err("User lacks permission");
    }
    
    // Main logic here
    do_processing(user);
    Ok(())
}
```

## Scoping

### Block Scope

```rust
// Variables are scoped to blocks
let x = 5;
{
    let y = 10;
    println!("{} {}", x, y);  // OK: both accessible
}
println!("{}", y);  // Error: y not in scope

// Blocks can return values
let result = {
    let a = 10;
    let b = 20;
    a + b  // Block returns 30
};
```

### Loop Scope

```rust
// Loop variables are scoped to the loop
for i in 0..10 {
    let square = i * i;
    // i and square only exist here
}
// i and square not accessible here

// While loop scope
while condition {
    let temp = calculate();
    // temp only exists in this iteration
}
```

### Conditional Scope

```rust
// Variables in if branches
if condition {
    let x = 10;
    // x only exists here
} else {
    let x = 20;  // Different x
    // This x only exists here
}

// Pattern matching scope
match value {
    Some(x) => {
        // x is only available in this branch
        println!("{}", x);
    },
    None => {
        // x is not available here
    }
}
```

## Control Flow and Types

### Type Consistency

```rust
// All branches must return the same type
let result = if condition {
    42          // int
} else {
    0           // int - OK
};

// Type mismatch error
let bad = if condition {
    42          // int
} else {
    "zero"      // string - Error!
};

// Match expressions must be exhaustive
let result = match option {
    Some(value) => value,
    None => 0,  // Must handle all cases
};
```

### Diverging Functions

```rust
// Functions that never return
fn panic_with_message(msg: string) -> ! {
    panic!(msg);
    // Never reaches here
}

// Useful in match arms
let value = match result {
    Ok(v) => v,
    Err(e) => panic_with_message(e),  // Never returns
};
```

## Best Practices

### Prefer Early Returns

```rust
// Bad: deeply nested
fn process(data: Option<Data>) -> Result<Output, Error> {
    if data.is_some() {
        let d = data.unwrap();
        if d.is_valid() {
            if d.has_permission() {
                Ok(d.process())
            } else {
                Err("No permission")
            }
        } else {
            Err("Invalid data")
        }
    } else {
        Err("No data")
    }
}

// Good: early returns
fn process(data: Option<Data>) -> Result<Output, Error> {
    let d = data.ok_or("No data")?;
    
    if !d.is_valid() {
        return Err("Invalid data");
    }
    
    if !d.has_permission() {
        return Err("No permission");
    }
    
    Ok(d.process())
}
```

### Use Match for Multiple Conditions

```rust
// Bad: multiple if-else
let category = if score >= 90 {
    "A"
} else if score >= 80 {
    "B"
} else if score >= 70 {
    "C"
} else if score >= 60 {
    "D"
} else {
    "F"
};

// Good: match with ranges
let category = match score {
    90..=100 => "A",
    80..=89 => "B",
    70..=79 => "C",
    60..=69 => "D",
    _ => "F"
};
```

### Avoid Complex Conditions

```rust
// Bad: complex boolean expression
if (user.is_active && user.age >= 18 && user.has_verified_email) ||
   (user.is_admin && !user.is_suspended) ||
   (user.has_special_permission && user.account_age > 365) {
    grant_access();
}

// Good: extract to function
fn can_access(user: &User) -> bool {
    if user.is_admin && !user.is_suspended {
        return true;
    }
    
    if user.has_special_permission && user.account_age > 365 {
        return true;
    }
    
    user.is_active && user.age >= 18 && user.has_verified_email
}

if can_access(&user) {
    grant_access();
}
```

## Common Patterns

### State Machines

```rust
enum State {
    Idle,
    Processing,
    Complete,
    Error(string),
}

let mut state = State::Idle;

loop {
    state = match state {
        State::Idle => {
            if has_work() {
                State::Processing
            } else {
                State::Idle
            }
        },
        State::Processing => {
            match do_work() {
                Ok(_) => State::Complete,
                Err(e) => State::Error(e),
            }
        },
        State::Complete => {
            cleanup();
            State::Idle
        },
        State::Error(msg) => {
            log_error(msg);
            State::Idle
        }
    };
    
    if should_stop() {
        break;
    }
}
```

### Retry Logic

```rust
fn retry_operation<T>(
    operation: fn() -> Result<T, Error>,
    max_attempts: int
) -> Result<T, Error> {
    let mut attempts = 0;
    
    loop {
        attempts = attempts + 1;
        
        match operation() {
            Ok(result) => return Ok(result),
            Err(e) if attempts >= max_attempts => return Err(e),
            Err(_) => {
                sleep(1000 * attempts);  // Exponential backoff
                continue;
            }
        }
    }
}
```

### Pipeline Pattern

```rust
// Process data through a pipeline
let result = input
    .validate()
    .and_then(|data| transform(data))
    .and_then(|data| enrich(data))
    .map(|data| format(data))
    .unwrap_or_else(|e| {
        log_error(e);
        default_output()
    });
```

## JavaScript Transpilation

Husk control flow transpiles naturally to JavaScript:

```rust
// Husk
let status = if age >= 18 { "adult" } else { "minor" };

for i in 0..10 {
    println!("{}", i);
}

match color {
    "red" => 0xFF0000,
    "green" => 0x00FF00,
    "blue" => 0x0000FF,
    _ => 0x000000,
}
```

Becomes:
```javascript
// JavaScript
const status = age >= 18 ? "adult" : "minor";

for (let i = 0; i < 10; i++) {
    console.log(i);
}

const colorValue = (() => {
    switch (color) {
        case "red": return 0xFF0000;
        case "green": return 0x00FF00;
        case "blue": return 0x0000FF;
        default: return 0x000000;
    }
})();
```

## Related Topics

- [Pattern Matching](pattern-matching.md) - Advanced matching patterns
- [Functions](functions.md) - Control flow in functions
- [Error Handling](error-handling.md) - Control flow with Result/Option
- [Variables](variables.md) - Variable scoping
- [Best Practices](../advanced/best-practices.md) - Control flow guidelines

---

*Effective use of control flow constructs leads to clear, maintainable code. Master these patterns to write expressive Husk programs.*