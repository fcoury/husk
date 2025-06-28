# Quick Start Guide

Learn the essential features of Husk in 15 minutes. This guide assumes you have Husk [installed](installation.md) and covers the most important concepts to get you productive quickly.

## Table of Contents

- [Variables and Types](#variables-and-types)
- [Functions](#functions)
- [Control Flow](#control-flow)
- [Data Structures](#data-structures)
- [Error Handling](#error-handling)
- [Pattern Matching](#pattern-matching)
- [Standard Library Highlights](#standard-library-highlights)
- [Building a Small Program](#building-a-small-program)
- [Next Steps](#next-steps)

## Variables and Types

### Basic Variables

```rust
// Immutable by default
let name = "Alice";           // string (inferred)
let age = 30;                 // int (inferred)
let pi = 3.14159;            // float (inferred)
let is_student = false;       // bool (inferred)

// Explicit type annotations
let count: int = 42;
let price: float = 19.99;

// Mutable variables
let mut score = 0;
score = score + 10;          // OK - mutable
```

### Type Inference

Husk infers types when possible:

```rust
let numbers = [1, 2, 3, 4, 5];        // array<int>
let doubled = numbers.map(|n| n * 2); // array<int>
let message = format!("Count: {}", numbers.len()); // string
```

## Functions

### Basic Functions

```rust
// Simple function
fn add(a: int, b: int) -> int {
    a + b  // implicit return
}

// With explicit return
fn multiply(a: int, b: int) -> int {
    return a * b;
}

// No return value
fn greet(name: string) {
    println!("Hello, {}!", name);
}

// Function with default parameter (planned feature)
fn power(base: int, exp: int = 2) -> int {
    base.pow(exp)
}
```

### Higher-Order Functions

```rust
// Functions as parameters
fn apply_twice(f: fn(int) -> int, x: int) -> int {
    f(f(x))
}

fn double(n: int) -> int {
    n * 2
}

let result = apply_twice(double, 5); // 20

// Closures
let add_one = |x| x + 1;
let numbers = [1, 2, 3].map(add_one); // [2, 3, 4]
```

## Control Flow

### If Expressions

```rust
// If as expression
let status = if age >= 18 { "adult" } else { "minor" };

// Traditional if statement
if score > 100 {
    println!("High score!");
} else if score > 50 {
    println!("Good job!");
} else {
    println!("Keep trying!");
}
```

### Loops

```rust
// For loop
for i in 0..5 {
    println!("Count: {}", i);
}

// For each
let items = ["apple", "banana", "cherry"];
for item in items {
    println!("Fruit: {}", item);
}

// While loop
let mut n = 0;
while n < 10 {
    n = n + 1;
}

// Loop with break
let mut counter = 0;
loop {
    counter = counter + 1;
    if counter > 5 {
        break;
    }
}
```

## Data Structures

### Arrays

```rust
// Array literal
let fruits = ["apple", "banana", "cherry"];

// Array methods
let numbers = [1, 2, 3, 4, 5];
let evens = numbers.filter(|n| n % 2 == 0);    // [2, 4]
let doubled = numbers.map(|n| n * 2);          // [2, 4, 6, 8, 10]
let sum = numbers.fold(0, |acc, n| acc + n);   // 15

// Array indexing
let first = fruits[0];  // "apple"
let last = fruits[fruits.len() - 1]; // "cherry"
```

### Tuples

```rust
// Tuple creation
let point = (10, 20);
let person = ("Alice", 30, true);

// Destructuring
let (x, y) = point;
let (name, age, is_student) = person;

// Accessing elements
let x_coord = point.0;
let y_coord = point.1;
```

### Custom Types (Structs)

```rust
// Define a struct
struct User {
    name: string,
    email: string,
    age: int,
}

// Create instance
let user = User {
    name: "Alice",
    email: "alice@example.com",
    age: 30,
};

// Access fields
println!("Name: {}", user.name);

// Methods (planned feature)
impl User {
    fn is_adult(&self) -> bool {
        self.age >= 18
    }
}
```

## Error Handling

### Option Type

```rust
// Option represents a value that might be absent
fn find_user(id: int) -> Option<User> {
    if id == 1 {
        Some(User { name: "Alice", email: "alice@example.com", age: 30 })
    } else {
        None
    }
}

// Using Option
match find_user(1) {
    Some(user) => println!("Found: {}", user.name),
    None => println!("User not found"),
}

// Option methods
let maybe_user = find_user(1);
if maybe_user.is_some() {
    let user = maybe_user.unwrap();
    println!("User: {}", user.name);
}
```

### Result Type

```rust
// Result represents success or error
fn divide(a: float, b: float) -> Result<float, string> {
    if b == 0.0 {
        Err("Division by zero")
    } else {
        Ok(a / b)
    }
}

// Using Result
match divide(10.0, 2.0) {
    Ok(result) => println!("Result: {}", result),
    Err(error) => println!("Error: {}", error),
}

// Chaining operations
let result = divide(10.0, 2.0)
    .map(|x| x * 2.0)
    .unwrap_or(0.0);
```

## Pattern Matching

### Match Expressions

```rust
// Basic pattern matching
let day = "Monday";
let day_type = match day {
    "Saturday" | "Sunday" => "weekend",
    _ => "weekday"
};

// Matching with values
let score = 85;
let grade = match score {
    90..=100 => "A",
    80..=89 => "B",
    70..=79 => "C",
    60..=69 => "D",
    _ => "F"
};

// Destructuring in patterns
let point = (3, 4);
match point {
    (0, 0) => println!("Origin"),
    (x, 0) => println!("On x-axis at {}", x),
    (0, y) => println!("On y-axis at {}", y),
    (x, y) => println!("Point at ({}, {})", x, y),
}
```

## Standard Library Highlights

### String Operations

```rust
let text = "Hello, World!";

// String methods
let upper = text.to_upper();        // "HELLO, WORLD!"
let lower = text.to_lower();        // "hello, world!"
let trimmed = "  hello  ".trim();   // "hello"
let parts = text.split(", ");       // ["Hello", "World!"]

// String formatting
let name = "Alice";
let age = 30;
let message = format!("{} is {} years old", name, age);
```

### File I/O

```rust
// Reading a file
match read_file("config.txt") {
    Ok(contents) => println!("File contents: {}", contents),
    Err(error) => println!("Error reading file: {}", error),
}

// Writing a file
let data = "Hello, Husk!";
match write_file("output.txt", data) {
    Ok(()) => println!("File written successfully"),
    Err(error) => println!("Error writing file: {}", error),
}
```

### Console I/O

```rust
// Output
println!("Hello, World!");          // with newline
print!("Enter your name: ");        // without newline

// Input
let name = read_line();
println!("Hello, {}!", name);

// Formatted output
let pi = 3.14159;
println!("Pi is approximately {:.2}", pi); // "Pi is approximately 3.14"
```

## Building a Small Program

Let's combine what we've learned into a simple todo list program:

```rust
// Save as todo.hk
struct Todo {
    id: int,
    task: string,
    completed: bool,
}

fn main() {
    let mut todos: array<Todo> = [];
    let mut next_id = 1;
    
    loop {
        println!("\n=== Todo List ===");
        print_todos(&todos);
        
        println!("\n1. Add todo");
        println!("2. Complete todo");
        println!("3. Exit");
        print!("Choice: ");
        
        let choice = read_line();
        
        match choice {
            "1" => {
                print!("Task: ");
                let task = read_line();
                todos.push(Todo {
                    id: next_id,
                    task: task,
                    completed: false,
                });
                next_id = next_id + 1;
            },
            "2" => {
                print!("Todo ID: ");
                let id = read_line().parse::<int>().unwrap_or(0);
                for mut todo in todos {
                    if todo.id == id {
                        todo.completed = true;
                    }
                }
            },
            "3" => break,
            _ => println!("Invalid choice"),
        }
    }
}

fn print_todos(todos: &array<Todo>) {
    if todos.is_empty() {
        println!("No todos yet!");
        return;
    }
    
    for todo in todos {
        let status = if todo.completed { "✓" } else { " " };
        println!("[{}] {} - {}", status, todo.id, todo.task);
    }
}
```

### Running the Program

```bash
# Run directly
husk run todo.hk

# Or compile to JavaScript
husk build todo.hk --target js
node todo.js
```

The program works identically whether run in the Husk interpreter or compiled to JavaScript!

## Key Takeaways

You've learned:

✅ **Variables**: Immutable by default, type inference  
✅ **Functions**: Parameters, return types, closures  
✅ **Control Flow**: if, loops, pattern matching  
✅ **Data Structures**: Arrays, tuples, structs  
✅ **Error Handling**: Option and Result types  
✅ **Standard Library**: Strings, I/O, array methods  

## Next Steps

Ready to dive deeper? Explore:

1. **[Type System](../language/types.md)** - Advanced type features
2. **[Pattern Matching](../language/pattern-matching.md)** - Advanced patterns
3. **[Error Handling](../language/error-handling.md)** - Best practices
4. **[Standard Library](../STANDARD_LIBRARY.md)** - Full API reference
5. **[Tutorials](../tutorials/)** - Build real applications

## Quick Reference Card

```rust
// Variables
let x = 42;              // immutable
let mut y = 0;           // mutable

// Functions  
fn add(a: int, b: int) -> int { a + b }

// Control Flow
if x > 0 { } else { }
for i in 0..10 { }
while condition { }

// Arrays
let arr = [1, 2, 3];
let filtered = arr.filter(|x| x > 1);

// Error Handling
match result {
    Ok(value) => { },
    Err(error) => { },
}

// Pattern Matching
match value {
    0 => "zero",
    1..=10 => "small",
    _ => "large",
}
```

---

*Congratulations! You now know enough Husk to start building programs. Happy coding! 🚀*