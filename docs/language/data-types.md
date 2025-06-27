# Data Types

Husk provides a rich set of built-in data types and the ability to define custom types. This comprehensive reference covers all data types available in Husk.

## Table of Contents

- [Overview](#overview)
- [Primitive Types](#primitive-types)
  - [Numeric Types](#numeric-types)
  - [Boolean Type](#boolean-type)
  - [Character Type](#character-type)
  - [Unit Type](#unit-type)
- [String Type](#string-type)
- [Compound Types](#compound-types)
  - [Arrays](#arrays)
  - [Tuples](#tuples)
  - [Slices](#slices)
- [Custom Types](#custom-types)
  - [Structs](#structs)
  - [Tuple Structs](#tuple-structs)
  - [Unit Structs](#unit-structs)
  - [Enums](#enums)
- [Type Aliases](#type-aliases)
- [References](#references)
- [Special Types](#special-types)
  - [Option Type](#option-type)
  - [Result Type](#result-type)
  - [Never Type](#never-type)
- [Type Conversions](#type-conversions)
- [Memory Layout](#memory-layout)
- [JavaScript Representation](#javascript-representation)
- [Related Topics](#related-topics)

## Overview

Husk's type system features:
- **Static typing** - All types known at compile time
- **Type inference** - Types can be inferred from context
- **No implicit conversions** - Explicit conversions required
- **Memory safety** - No undefined behavior

### Type Categories

1. **Primitive types** - Built into the language
2. **Compound types** - Composed of other types
3. **Custom types** - User-defined structures
4. **Special types** - Option, Result, and others

## Primitive Types

### Numeric Types

#### Integer Type (int)

```rust
// 32-bit signed integer
let a: int = 42;
let b: int = -100;
let c: int = 0;

// Integer literals
let decimal = 1_000_000;      // Underscores for readability
let hex = 0xFF;               // Hexadecimal
let octal = 0o77;             // Octal
let binary = 0b1111_0000;     // Binary

// Integer operations
let sum = 10 + 20;           // 30
let difference = 100 - 50;    // 50
let product = 5 * 6;          // 30
let quotient = 20 / 4;        // 5
let remainder = 20 % 3;       // 2

// Integer methods
let abs_value = (-42).abs();  // 42
let power = 2.pow(10);        // 1024
```

#### Floating-Point Type (float)

```rust
// 64-bit floating point (IEEE-754 double)
let pi: float = 3.14159;
let e: float = 2.71828;
let negative: float = -273.15;

// Float literals
let regular = 3.14;
let scientific = 1.23e10;     // 1.23 × 10^10
let small = 1.23e-10;        // 1.23 × 10^-10

// Float operations
let sum = 1.5 + 2.3;         // 3.8
let product = 2.5 * 4.0;     // 10.0
let division = 10.0 / 3.0;   // 3.333...

// Float methods
let absolute = (-3.14).abs(); // 3.14
let square_root = 16.0.sqrt(); // 4.0
let sine = 0.0.sin();        // 0.0
```

### Boolean Type

```rust
// Boolean values: true or false
let is_ready: bool = true;
let is_finished: bool = false;

// Boolean operations
let and_result = true && false;    // false
let or_result = true || false;     // true
let not_result = !true;            // false

// From comparisons
let is_greater = 10 > 5;           // true
let is_equal = 42 == 42;           // true
let is_not_equal = "a" != "b";     // true

// In conditions
if is_ready && !is_finished {
    process();
}
```

### Character Type

```rust
// Unicode scalar value
let letter: char = 'A';
let emoji: char = '😊';
let unicode: char = '\u{1F600}';  // 😀
let escape: char = '\n';          // Newline

// Character literals must use single quotes
let ch = 'x';    // OK
let bad = "x";   // Error: this is a string

// Character methods
let is_alphabetic = 'A'.is_alphabetic();  // true
let is_numeric = '7'.is_numeric();        // true
let to_lowercase = 'A'.to_lowercase();    // 'a'
let to_uppercase = 'a'.to_uppercase();    // 'A'
```

### Unit Type

```rust
// Unit type has exactly one value: ()
let unit: () = ();

// Functions that don't return a value return unit
fn print_message(msg: string) -> () {
    println!(msg);
    // Implicitly returns ()
}

// Often omitted in function signatures
fn do_something() {  // Returns ()
    // Implementation
}

// Used in generic contexts
let empty_result: Result<(), Error> = Ok(());
```

## String Type

```rust
// UTF-8 encoded string
let greeting: string = "Hello, World!";
let empty: string = "";
let unicode: string = "Hello, 世界! 🌍";

// String literals
let single_line = "This is a string";
let multi_line = "This is a
multi-line
string";

// Escape sequences
let escaped = "Line 1\nLine 2\tTabbed";
let quote = "She said \"Hello\"";
let backslash = "C:\\Windows\\System32";

// String concatenation
let hello = "Hello";
let world = "World";
let combined = hello + ", " + world + "!";

// String methods
let length = greeting.len();              // Character count
let uppercase = greeting.to_upper();      // "HELLO, WORLD!"
let lowercase = greeting.to_lower();      // "hello, world!"
let trimmed = "  hello  ".trim();        // "hello"
let contains = greeting.contains("World"); // true

// String slicing
let slice = &greeting[0..5];             // "Hello"

// String formatting
let name = "Alice";
let age = 30;
let formatted = format!("{} is {} years old", name, age);
```

## Compound Types

### Arrays

```rust
// Fixed-size, homogeneous collection
let numbers: array<int> = [1, 2, 3, 4, 5];
let strings: array<string> = ["a", "b", "c"];

// Array initialization
let zeros: array<int> = [0; 5];  // [0, 0, 0, 0, 0]

// Accessing elements
let first = numbers[0];          // 1
let last = numbers[numbers.len() - 1];  // 5

// Array methods
let length = numbers.len();      // 5
let is_empty = numbers.is_empty(); // false

// Functional operations
let doubled = numbers.map(|x| x * 2);     // [2, 4, 6, 8, 10]
let evens = numbers.filter(|x| x % 2 == 0); // [2, 4]
let sum = numbers.fold(0, |acc, x| acc + x); // 15

// Iteration
for num in numbers {
    println!("{}", num);
}

// With index
for (i, num) in numbers.iter().enumerate() {
    println!("{}: {}", i, num);
}
```

### Tuples

```rust
// Fixed-size, heterogeneous collection
let pair: (int, string) = (42, "answer");
let triple: (int, float, bool) = (1, 3.14, true);
let unit: () = ();  // Empty tuple (unit type)

// Accessing elements
let first = pair.0;   // 42
let second = pair.1;  // "answer"

// Destructuring
let (x, y) = pair;
let (a, b, c) = triple;

// Tuple patterns
match pair {
    (0, _) => println!("First is zero"),
    (_, "answer") => println!("Second is answer"),
    (n, s) => println!("{}: {}", n, s),
}

// Nested tuples
let nested = ((1, 2), (3, 4));
let ((a, b), (c, d)) = nested;

// Function returning tuple
fn min_max(array: array<int>) -> (int, int) {
    (array.min(), array.max())
}

let (min, max) = min_max([3, 1, 4, 1, 5]);
```

### Slices

```rust
// View into contiguous sequence
let array = [1, 2, 3, 4, 5];
let slice: &[int] = &array[1..4];  // [2, 3, 4]

// String slices
let text = "Hello, World!";
let hello: &str = &text[0..5];     // "Hello"
let world: &str = &text[7..12];    // "World"

// Slice operations
let first = slice.first();          // Some(2)
let last = slice.last();           // Some(4)
let length = slice.len();          // 3

// Slice patterns
match slice {
    [] => println!("Empty"),
    [single] => println!("One: {}", single),
    [first, .., last] => println!("Many: {} to {}", first, last),
}
```

## Custom Types

### Structs

```rust
// Named fields
struct Person {
    name: string,
    age: int,
    email: string,
}

// Creating instances
let alice = Person {
    name: "Alice".to_string(),
    age: 30,
    email: "alice@example.com".to_string(),
};

// Field access
println!("Name: {}", alice.name);
println!("Age: {}", alice.age);

// Mutable structs
let mut bob = Person {
    name: "Bob".to_string(),
    age: 25,
    email: "bob@example.com".to_string(),
};
bob.age = 26;

// Struct update syntax
let alice_older = Person {
    age: 31,
    ..alice  // Copy other fields from alice
};

// Methods
impl Person {
    // Associated function (constructor)
    fn new(name: string, age: int, email: string) -> Person {
        Person { name, age, email }
    }
    
    // Method
    fn greet(&self) -> string {
        format!("Hi, I'm {}!", self.name)
    }
    
    // Mutable method
    fn have_birthday(&mut self) {
        self.age += 1;
    }
}

// Using methods
let mut person = Person::new("Carol".to_string(), 28, "carol@example.com".to_string());
println!("{}", person.greet());
person.have_birthday();
```

### Tuple Structs

```rust
// Unnamed fields
struct Point(float, float);
struct Color(int, int, int);

// Creating instances
let origin = Point(0.0, 0.0);
let red = Color(255, 0, 0);

// Accessing fields
let x = origin.0;
let y = origin.1;

// Destructuring
let Point(x, y) = origin;
let Color(r, g, b) = red;

// Methods work the same
impl Point {
    fn distance_from_origin(&self) -> float {
        (self.0 * self.0 + self.1 * self.1).sqrt()
    }
}

// Newtype pattern
struct Meters(float);
struct Feet(float);

impl Meters {
    fn to_feet(&self) -> Feet {
        Feet(self.0 * 3.28084)
    }
}
```

### Unit Structs

```rust
// No fields
struct Empty;
struct Marker;

// Creating instances
let empty = Empty;
let marker = Marker;

// Used as markers or in generic contexts
trait MyTrait {
    fn do_something(&self);
}

impl MyTrait for Marker {
    fn do_something(&self) {
        println!("Marker doing something");
    }
}
```

### Enums

```rust
// Simple enum (like C enum)
enum Direction {
    North,
    South,
    East,
    West,
}

// Using simple enum
let dir = Direction::North;
match dir {
    Direction::North => println!("Going north"),
    Direction::South => println!("Going south"),
    Direction::East => println!("Going east"),
    Direction::West => println!("Going west"),
}

// Enum with data
enum Message {
    Quit,                       // Unit variant
    Move { x: int, y: int },   // Struct variant
    Write(string),             // Tuple variant
    ChangeColor(int, int, int), // Multiple values
}

// Creating enum instances
let msg1 = Message::Quit;
let msg2 = Message::Move { x: 10, y: 20 };
let msg3 = Message::Write("Hello".to_string());
let msg4 = Message::ChangeColor(255, 0, 0);

// Pattern matching enums
match msg2 {
    Message::Quit => quit(),
    Message::Move { x, y } => move_to(x, y),
    Message::Write(text) => print(text),
    Message::ChangeColor(r, g, b) => set_color(r, g, b),
}

// Enums with methods
impl Message {
    fn is_quit(&self) -> bool {
        match self {
            Message::Quit => true,
            _ => false,
        }
    }
}

// Generic enums
enum Option<T> {
    Some(T),
    None,
}

enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

## Type Aliases

```rust
// Create alternative names for types
type UserId = int;
type Username = string;
type Point2D = (float, float);

// Use aliases like original types
let id: UserId = 42;
let name: Username = "alice".to_string();
let point: Point2D = (10.0, 20.0);

// Complex type aliases
type ResultString = Result<string, string>;
type UserMap = HashMap<UserId, User>;
type Callback = fn(Event) -> ();

// Generic type aliases
type Pair<T> = (T, T);
type Optional<T> = Option<T>;

// Improves readability
fn get_user(id: UserId) -> Option<Username> {
    // Implementation
}

// Instead of
fn get_user_verbose(id: int) -> Option<string> {
    // Implementation
}
```

## References

```rust
// Immutable references
let value = 42;
let reference: &int = &value;
let dereferenced = *reference;  // 42

// Mutable references
let mut value = 42;
let mut_ref: &mut int = &mut value;
*mut_ref = 100;  // value is now 100

// References in functions
fn print_value(x: &int) {
    println!("Value: {}", x);
}

fn increment(x: &mut int) {
    *x += 1;
}

let mut num = 10;
print_value(&num);     // Immutable borrow
increment(&mut num);   // Mutable borrow

// Reference rules
// 1. Many immutable references OR one mutable reference
// 2. References must be valid
// 3. No reference outlives its data
```

## Special Types

### Option Type

```rust
// Represents optional values
enum Option<T> {
    Some(T),
    None,
}

// Creating Options
let some_number: Option<int> = Some(42);
let no_number: Option<int> = None;

// Using Options
match some_number {
    Some(n) => println!("Number: {}", n),
    None => println!("No number"),
}

// Option methods
let doubled = some_number.map(|n| n * 2);
let value = some_number.unwrap_or(0);
```

### Result Type

```rust
// Represents success or failure
enum Result<T, E> {
    Ok(T),
    Err(E),
}

// Creating Results
let success: Result<int, string> = Ok(42);
let failure: Result<int, string> = Err("Error".to_string());

// Using Results
match success {
    Ok(value) => println!("Success: {}", value),
    Err(error) => println!("Error: {}", error),
}

// Result methods
let doubled = success.map(|n| n * 2);
let value = success.unwrap_or(0);
```

### Never Type

```rust
// Type that has no values (!)
fn diverging() -> ! {
    panic!("This function never returns");
}

// Used in match arms that never complete
let value = match option {
    Some(v) => v,
    None => panic!("No value!"),  // Returns !
};

// Infinite loops
fn server_loop() -> ! {
    loop {
        handle_request();
        // Never breaks
    }
}
```

## Type Conversions

### Explicit Conversions

```rust
// Numeric conversions with 'as'
let integer: int = 42;
let float: float = integer as float;  // 42.0

let big: float = 1000.0;
let small: int = big as int;  // 1000

// String conversions
let num_string = 42.to_string();  // "42"
let parsed: int = "42".parse().unwrap();  // 42

// Between custom types
impl From<int> for MyType {
    fn from(value: int) -> MyType {
        MyType { value }
    }
}

let my_value = MyType::from(42);
let my_value: MyType = 42.into();  // Same thing
```

### Type Coercion

```rust
// Limited automatic coercion
let array: array<int> = [1, 2, 3];
let slice: &[int] = &array;  // array to slice

// Deref coercion
let string = String::from("hello");
let str_slice: &str = &string;  // String to &str

// No numeric coercion
let x: int = 42;
let y: float = x;  // Error! Must use: x as float
```

## Memory Layout

### Size and Alignment

```rust
// Primitive sizes
// int: 4 bytes
// float: 8 bytes  
// bool: 1 byte
// char: 4 bytes (Unicode scalar)

// Compound types
// Arrays: element_size * length
// Tuples: sum of elements + padding
// Structs: sum of fields + padding

// Memory optimization
struct Optimized {
    a: int,    // 4 bytes
    b: int,    // 4 bytes  
    c: bool,   // 1 byte
    // Total: 9 bytes + padding
}

struct Padded {
    a: bool,   // 1 byte + 3 padding
    b: int,    // 4 bytes
    c: bool,   // 1 byte + 3 padding
    // Total: 12 bytes
}
```

## JavaScript Representation

How Husk types map to JavaScript:

```rust
// Husk primitives
let int_val: int = 42;
let float_val: float = 3.14;
let bool_val: bool = true;
let char_val: char = 'A';
let string_val: string = "Hello";
```

```javascript
// JavaScript equivalents
const int_val = 42;
const float_val = 3.14;
const bool_val = true;
const char_val = 'A';
const string_val = "Hello";
```

```rust
// Husk compounds
let array = [1, 2, 3];
let tuple = (1, "hello");
struct Point { x: int, y: int }
let point = Point { x: 10, y: 20 };
```

```javascript
// JavaScript equivalents
const array = [1, 2, 3];
const tuple = [1, "hello"];
class Point {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
}
const point = new Point(10, 20);
```

```rust
// Husk enums
enum Status { Active, Inactive }
let status = Status::Active;

// Option
let some = Some(42);
let none: Option<int> = None;
```

```javascript
// JavaScript equivalents
const Status = { Active: 'Active', Inactive: 'Inactive' };
const status = Status.Active;

// Option
const some = 42;
const none = null;
```

## Related Topics

- [Type System](types.md) - Type system overview
- [Variables](variables.md) - Variable declarations
- [Pattern Matching](pattern-matching.md) - Destructuring types
- [Functions](functions.md) - Function parameter and return types
- [Error Handling](error-handling.md) - Option and Result usage
- [Memory Management](../advanced/memory.md) - Memory details

---

*Understanding Husk's data types is fundamental to writing safe and efficient programs. Each type has specific use cases and performance characteristics.*