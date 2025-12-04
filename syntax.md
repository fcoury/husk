# MVP Syntax Overview

This document summarizes the concrete surface syntax supported in the Husk MVP. It is intentionally small but sufficient to write useful programs and interop with JS.

- Source files typically use the `.hk` (short) or `.husk` (long) extensions.

## 1. Files and Items

- A source file consists of a sequence of **items**.
- Items (MVP):
  - Function definitions: `fn`
  - Struct definitions: `struct`
  - Enum definitions: `enum`
  - Trait definitions: `trait`
  - Impl blocks: `impl`
  - Type aliases: `type`
  - Extern JS declarations: `extern "js" { ... }`

Example:

```rust
fn main() {
    println("hello");
}

struct User {
    name: String,
    id: i32,
}

enum Result<T, E> {
    Ok(T),
    Err(E),
}

type UserId = i32;
```

## 2. Functions

Syntax:

```rust
fn name<T1, T2>(param1: Type1, param2: Type2) -> ReturnType {
    // body
}
```

- Generics `<T1, T2>` are optional.
- Parameter types are required in MVP.
- Return type:
  - `-> Ret` required for now (except `-> ()` is implied if omitted or we choose to allow omission).
- Function bodies are blocks `{ ... }`.

## 3. Structs

Record structs only (no tuple or unit structs in MVP):

```rust
struct Point {
    x: i32,
    y: i32,
}

struct Box<T> {
    value: T,
}
```

## 4. Enums

Enums support unit, tuple-like, and struct-like variants, with generics:

```rust
enum Message<T> {
    Quit,
    Write(T),
    Move { x: i32, y: i32 },
}
```

## 5. Type Aliases

Simple aliases:

```rust
type UserId = i32;
type MaybeString = Option<String>;
```

## 6. Extern JS Declarations

Extern blocks declare functions provided by JS:

```rust
extern "js" {
    fn parse_json(text: String) -> Result<Json, JsError>;
}
```

- Semantics of imports/binding to actual JS functions are handled by the compiler/codegen.

## 7. Statements

Inside function bodies and blocks:

- Variable bindings:
  ```rust
  let x = expr;
  let mut y: i32 = expr;
  ```
- Expression statements:
  ```rust
  expr;
  ```
- Return statements:
  ```rust
  return expr;
  return;
  ```
- Blocks:
  ```rust
  {
      // zero or more statements
  }
  ```
- Control flow:
  ```rust
  if condition {
      // ...
  } else {
      // ...
  }

  while condition {
      // ...
      break;
      continue;
  }

  loop {
      // infinite loop, exit with break or return
      if condition {
          break;
      }
  }
  ```

## 8. Expressions

### 8.1 Primary Expressions

- Literals:
  - Integer: `0`, `1`, `42`
  - String: `"hello"`
  - Boolean: `true`, `false`
- Name reference:
  ```rust
  x
  ```
- Parenthesized:
  ```rust
  (expr)
  ```

### 8.2 Field Access and Method Call

- Field access:
  ```rust
  expr.field
  ```

- Function call:
  ```rust
  func(arg1, arg2)
  ```

- Method call:
  ```rust
  expr.method(arg1, arg2)
  ```

### 8.3 Operators (MVP)

Basic built-in operators:

- Arithmetic:
  ```rust
  a + b
  a - b
  a * b
  a / b
  ```

- Comparisons:
  ```rust
  a == b
  a != b
  a < b
  a > b
  a <= b
  a >= b
  ```

- Logical:
  ```rust
  !a
  a && b
  a || b
  ```

Operator precedence and associativity follow a reasonable, Rust-like ordering (details can be refined as the parser is implemented).

## 9. Types

Type forms in MVP:

- Named types:
  ```rust
  i32
  bool
  String
  MyType
  ```

- Generic type applications:
  ```rust
  Option<T>
  Result<T, E>
  Box<String>
  ```

- Unit type:
  ```rust
  ()
  ```

No references (`&T`) or lifetimes in MVP. See section 11 for trait bounds.

## 10. Pattern Matching

`match` expressions:

```rust
match expr {
    Pattern1 => expr_or_block,
    Pattern2 => expr_or_block,
}
```

Supported patterns:

- Wildcard:
  ```rust
  _
  ```

- Binding:
  ```rust
  x
  ```

- Enum unit variant:
  ```rust
  Message::Quit
  ```

- Enum tuple variant:
  ```rust
  Message::Write(text)
  ```

- Enum struct variant:
  ```rust
  Message::Move { x, y }
  ```

Pattern guards (`if` in patterns) and or-patterns (`A | B`) are out of MVP.

## 11. Traits and Trait Bounds

### 11.1 Trait Definitions

Traits define shared behavior that types can implement:

```rust
trait Greet {
    fn greet(&self) -> String;
}

// Trait with default implementation
trait Debug {
    fn debug(&self) -> String {
        "[object]"
    }
}
```

### 11.2 Supertraits

Traits can require other traits as prerequisites using the supertrait syntax:

```rust
trait PartialEq {}
trait Eq: PartialEq {}  // Eq requires PartialEq
```

When implementing a trait with supertraits, all supertraits must also be implemented:

```rust
struct Point { x: i32, y: i32 }

impl PartialEq for Point {}  // Must implement PartialEq first
impl Eq for Point {}         // Now Eq can be implemented
```

### 11.3 Impl Blocks

Implement methods for a type (inherent impl) or implement a trait for a type:

```rust
// Inherent impl - methods belong to the type itself
impl Point {
    fn new(x: i32, y: i32) -> Point {
        Point { x, y }
    }

    fn distance(&self) -> f64 {
        // ...
    }
}

// Trait impl - implement trait for a type
impl Greet for Point {
    fn greet(&self) -> String {
        "Hello from Point!"
    }
}
```

### 11.4 Trait Bounds on Functions

Generic functions can require type parameters to implement specific traits:

```rust
fn compare<T: PartialEq>(a: T, b: T) -> bool {
    a == b
}

// Multiple bounds with + syntax (not yet supported)
// fn print_and_compare<T: PartialEq + Debug>(a: T, b: T) { ... }
```

The compiler enforces trait bounds at call sites:

```rust
compare(1, 2);      // OK: i32 implements PartialEq
compare(p1, p2);    // Error if Point doesn't implement PartialEq
```

## 12. Standard Library Traits

### 12.1 PartialEq

The `PartialEq` trait enables equality comparisons (`==` and `!=`):

```rust
trait PartialEq {}
```

Types implementing `PartialEq` can be compared for equality, but the comparison
may not be reflexive (e.g., `NaN != NaN` for floating point).

**Primitive implementations:**
- `i32` - implements PartialEq
- `f64` - implements PartialEq (but not Eq due to NaN)
- `bool` - implements PartialEq
- `String` - implements PartialEq

### 12.2 Eq

The `Eq` trait guarantees a full equivalence relation:

```rust
trait Eq: PartialEq {}
```

Types implementing `Eq` guarantee that equality is:
- **Reflexive:** `a == a` is always true
- **Symmetric:** `a == b` implies `b == a`
- **Transitive:** `a == b` and `b == c` implies `a == c`

**Primitive implementations:**
- `i32` - implements Eq
- `bool` - implements Eq
- `String` - implements Eq
- `f64` - does NOT implement Eq (because `NaN != NaN`)

### 12.3 Implementing Traits for Custom Types

```rust
struct Point { x: i32, y: i32 }

impl PartialEq for Point {}
impl Eq for Point {}  // Requires PartialEq to be implemented first
```

## 13. Testing and Assertions

Husk provides built-in assertion functions for testing:

### 13.1 assert

Asserts that a condition is true. Panics if the condition is false:

```rust
fn test_positive() {
    let x = 5;
    assert(x > 0);
}
```

### 13.2 assert_msg

Asserts with a custom error message:

```rust
fn test_bounds() {
    let x = 10;
    assert_msg(x < 100, "x must be less than 100");
}
```

### 13.3 assert_eq

Asserts that two values are equal. Panics with a detailed message showing both values if they differ:

```rust
fn test_addition() {
    let result = 2 + 2;
    assert_eq(result, 4);
}

fn test_string() {
    let s = "hello";
    assert_eq(s, "hello");
}
```

### 13.4 assert_ne

Asserts that two values are NOT equal. Panics if they are equal:

```rust
fn test_different() {
    let a = 1;
    let b = 2;
    assert_ne(a, b);
}
```

### 13.5 Writing Tests

Tests are functions marked with the `#[test]` attribute:

```rust
#[test]
fn test_point_creation() {
    let p = Point { x: 10, y: 20 };
    assert_eq(p.x, 10);
    assert_eq(p.y, 20);
}

#[test]
fn test_equality() {
    let p1 = Point { x: 1, y: 2 };
    let p2 = Point { x: 1, y: 2 };
    assert_eq(p1, p2);
}
```

Run tests with `husk test` or `husk test path/to/file.hk`.
