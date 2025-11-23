# MVP Syntax Overview

This document summarizes the concrete surface syntax supported in the Husk MVP. It is intentionally small but sufficient to write useful programs and interop with JS.

- Source files typically use the `.hk` (short) or `.husk` (long) extensions.

## 1. Files and Items

- A source file consists of a sequence of **items**.
- Items (MVP):
  - Function definitions: `fn`
  - Struct definitions: `struct`
  - Enum definitions: `enum`
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

No references (`&T`), lifetimes, or trait bounds in MVP.

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
