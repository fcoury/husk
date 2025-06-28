# String Operations

Husk provides a comprehensive set of string manipulation functions and methods. This guide covers all string operations available in the standard library.

## Table of Contents

- [Overview](#overview)
- [String Creation](#string-creation)
- [Basic Operations](#basic-operations)
- [String Methods](#string-methods)
  - [Inspection Methods](#inspection-methods)
  - [Transformation Methods](#transformation-methods)
  - [Search Methods](#search-methods)
  - [Splitting and Joining](#splitting-and-joining)
- [String Formatting](#string-formatting)
- [Unicode Support](#unicode-support)
- [String Parsing](#string-parsing)
- [Memory and Performance](#memory-and-performance)
- [Common Patterns](#common-patterns)
- [JavaScript Interop](#javascript-interop)
- [Related Topics](#related-topics)

## Overview

Husk strings are:
- **UTF-8 encoded** - Full Unicode support
- **Immutable** - String operations return new strings
- **Efficient** - Optimized for common operations
- **Safe** - No buffer overflows or invalid UTF-8

### Key Concepts

1. **String vs &str** - Owned strings vs string slices
2. **Zero-copy operations** - Many operations avoid copying
3. **Unicode correctness** - Operations handle Unicode properly
4. **Builder pattern** - Efficient string construction

## String Creation

### String Literals

```rust
// Simple string literal
let simple = "Hello, World!";

// Multi-line string
let multi = "Line 1
Line 2
Line 3";

// Raw strings (no escapes)
let raw = r"C:\Windows\System32";
let raw_multi = r#"
    She said "Hello"
    He said "Goodbye"
"#;

// String with escapes
let escaped = "Tab:\tNewline:\nQuote:\"";

// Unicode in strings
let unicode = "Hello, 世界! 🌍";
```

### String Construction

```rust
// From string literal
let s1 = String::from("Hello");
let s2 = "Hello".to_string();

// Empty string
let empty = String::new();
let also_empty = "".to_string();

// From characters
let from_char = 'A'.to_string();
let from_chars = ['H', 'e', 'l', 'l', 'o'].iter().collect::<String>();

// String concatenation
let hello = "Hello";
let world = "World";
let greeting = hello + ", " + world + "!";

// Using format!
let name = "Alice";
let age = 30;
let message = format!("{} is {} years old", name, age);

// String builder pattern
let mut builder = String::new();
builder.push_str("Hello");
builder.push(' ');
builder.push_str("World");
let result = builder; // "Hello World"
```

## Basic Operations

### Length and Size

```rust
let text = "Hello, 世界!";

// Character count (Unicode-aware)
let char_count = text.len();        // 9 characters
let is_empty = text.is_empty();    // false

// Byte length
let byte_count = text.as_bytes().len(); // 14 bytes (UTF-8)

// Checking emptiness
if !text.is_empty() {
    process(text);
}
```

### Concatenation

```rust
// Using + operator
let s1 = "Hello";
let s2 = "World";
let combined = s1 + " " + s2;

// Using format!
let combined = format!("{} {}", s1, s2);

// Building strings efficiently
let mut result = String::with_capacity(100);
result.push_str("Hello");
result.push(' ');
result.push_str("World");

// Joining collections
let words = ["Hello", "World", "from", "Husk"];
let sentence = words.join(" ");  // "Hello World from Husk"
```

### Comparison

```rust
// Equality
let equal = "hello" == "hello";     // true
let not_equal = "hello" != "HELLO"; // true

// Ordering
let less = "apple" < "banana";       // true
let greater = "zebra" > "apple";     // true

// Case-insensitive comparison
let s1 = "Hello";
let s2 = "HELLO";
let equal_ignore_case = s1.to_lower() == s2.to_lower(); // true

// Starts/ends with
let starts = "Hello, World".starts_with("Hello");  // true
let ends = "Hello, World".ends_with("World");      // true
```

## String Methods

### Inspection Methods

```rust
let text = "Hello, World!";

// Length and emptiness
let len = text.len();                    // 13
let empty = text.is_empty();             // false

// Character inspection
let starts = text.starts_with("Hello");  // true
let ends = text.ends_with("!");          // true
let contains = text.contains("World");   // true

// Character at index (if supported)
let first_char = text.chars().nth(0);    // Some('H')
let last_char = text.chars().last();     // Some('!')

// Counting occurrences
let count = text.matches('l').count();   // 3

// Check character properties
let is_ascii = text.is_ascii();          // false (if unicode)
let all_alphabetic = text.chars().all(|c| c.is_alphabetic());
```

### Transformation Methods

```rust
let text = "  Hello, World!  ";

// Case conversion
let upper = text.to_upper();      // "  HELLO, WORLD!  "
let lower = text.to_lower();      // "  hello, world!  "

// Trimming whitespace
let trimmed = text.trim();        // "Hello, World!"
let trim_start = text.trim_start(); // "Hello, World!  "
let trim_end = text.trim_end();   // "  Hello, World!"

// Replace operations
let replaced = text.replace("World", "Husk");  // "  Hello, Husk!  "
let replaced_first = text.replacen("l", "L", 1); // "  HeLlo, World!  "

// Remove characters
let no_spaces = text.replace(" ", "");  // "Hello,World!"

// Repeat string
let repeated = "Ha".repeat(3);    // "HaHaHa"

// Reverse string
let reversed: String = text.chars().rev().collect();
```

### Search Methods

```rust
let text = "Hello, wonderful World!";

// Find substring
let pos = text.find("World");      // Some(17)
let not_found = text.find("xyz");  // None

// Find from end
let last_pos = text.rfind("o");    // Some(18)

// Find with predicate
let first_digit = text.find(|c: char| c.is_numeric());

// Get substring indices
if let Some(start) = text.find("wonderful") {
    let end = start + "wonderful".len();
    let substr = &text[start..end];  // "wonderful"
}

// Check multiple patterns
let has_greeting = text.starts_with("Hello") || text.starts_with("Hi");

// Count matches
let o_count = text.matches('o').count();  // 2
let word_count = text.split_whitespace().count(); // 3
```

### Splitting and Joining

```rust
// Split by delimiter
let data = "apple,banana,cherry";
let fruits: Vec<&str> = data.split(',').collect();
// ["apple", "banana", "cherry"]

// Split by whitespace
let words: Vec<&str> = "Hello World from Husk".split_whitespace().collect();
// ["Hello", "World", "from", "Husk"]

// Split with limit
let parts: Vec<&str> = "a-b-c-d".splitn(3, '-').collect();
// ["a", "b", "c-d"]

// Split lines
let lines: Vec<&str> = "Line 1\nLine 2\nLine 3".lines().collect();

// Join strings
let words = vec!["Hello", "World"];
let sentence = words.join(" ");     // "Hello World"
let csv = words.join(",");          // "Hello,World"

// Custom separator
let parts = ["2024", "03", "15"];
let date = parts.join("-");         // "2024-03-15"
```

## String Formatting

### Format Macro

```rust
// Basic formatting
let name = "Alice";
let age = 30;
let message = format!("Name: {}, Age: {}", name, age);

// Positional arguments
let formatted = format!("{1} is {0} years old", age, name);

// Named arguments
let formatted = format!("{name} is {age} years old", name=name, age=age);

// Formatting specifiers
let pi = 3.14159;
let formatted = format!("Pi: {:.2}", pi);        // "Pi: 3.14"
let hex = format!("Hex: {:x}", 255);            // "Hex: ff"
let binary = format!("Binary: {:b}", 42);       // "Binary: 101010"

// Padding and alignment
let padded = format!("{:>10}", "test");         // "      test"
let left = format!("{:<10}", "test");           // "test      "
let center = format!("{:^10}", "test");         // "   test   "

// Zero padding
let number = format!("{:05}", 42);              // "00042"
```

### String Interpolation

```rust
// Using format!
let user = "Bob";
let action = "logged in";
let log = format!("{} has {}", user, action);

// Multi-line formatting
let report = format!(
    "User: {}
Status: {}
Time: {}",
    user, status, time
);

// Building complex strings
let html = format!(
    r#"<div class="user">
    <h1>{}</h1>
    <p>Age: {}</p>
</div>"#,
    name, age
);
```

## Unicode Support

### Unicode Operations

```rust
// Working with Unicode
let text = "Hello, 世界! 🌍";

// Character iteration
for ch in text.chars() {
    println!("Character: {}", ch);
}

// Character count vs byte count
let char_count = text.chars().count();     // 11
let byte_count = text.len();               // 17

// Unicode normalization (if supported)
let s1 = "café";  // é as single character
let s2 = "café";  // e + combining acute accent
let normalized1 = s1.nfc().collect::<String>();
let normalized2 = s2.nfc().collect::<String>();

// Character boundaries
let slice = &text[0..5];   // "Hello" - OK
// let bad = &text[0..8];   // Error! Not on char boundary

// Safe slicing
if text.is_char_boundary(8) {
    let slice = &text[0..8];
}
```

### Character Classification

```rust
let ch = '9';

// Character properties
let is_numeric = ch.is_numeric();        // true
let is_alphabetic = ch.is_alphabetic();  // false
let is_alphanumeric = ch.is_alphanumeric(); // true
let is_whitespace = ch.is_whitespace();  // false
let is_uppercase = ch.is_uppercase();    // false
let is_lowercase = ch.is_lowercase();    // false

// Character conversion
let upper = 'a'.to_uppercase();          // 'A'
let lower = 'A'.to_lowercase();          // 'a'

// Unicode categories
let is_emoji = '😀'.is_alphabetic();     // false
let is_chinese = '中'.is_alphabetic();   // true
```

## String Parsing

### Parsing to Numbers

```rust
// Parse to integer
let num_str = "42";
let num: Result<int, _> = num_str.parse();
let num = num_str.parse::<int>().unwrap_or(0);

// Parse to float
let float_str = "3.14";
let float: float = float_str.parse().unwrap();

// Parse with error handling
match "123".parse::<int>() {
    Ok(n) => println!("Number: {}", n),
    Err(e) => println!("Parse error: {}", e),
}

// Parse multiple values
let coords = "10,20";
let parts: Vec<&str> = coords.split(',').collect();
let x: int = parts[0].parse().unwrap();
let y: int = parts[1].parse().unwrap();
```

### Custom Parsing

```rust
// Parse boolean
fn parse_bool(s: &str) -> Option<bool> {
    match s.to_lower().as_str() {
        "true" | "yes" | "1" => Some(true),
        "false" | "no" | "0" => Some(false),
        _ => None,
    }
}

// Parse key-value pairs
fn parse_config(s: &str) -> HashMap<String, String> {
    s.lines()
        .filter_map(|line| {
            let parts: Vec<&str> = line.splitn(2, '=').collect();
            if parts.len() == 2 {
                Some((parts[0].trim().to_string(), parts[1].trim().to_string()))
            } else {
                None
            }
        })
        .collect()
}

// Parse CSV line
fn parse_csv_line(line: &str) -> Vec<String> {
    line.split(',')
        .map(|s| s.trim().to_string())
        .collect()
}
```

## Memory and Performance

### Efficient String Building

```rust
// Pre-allocate capacity
let mut s = String::with_capacity(100);
s.push_str("Hello");
s.push_str(" World");

// Avoid repeated allocations
// Bad
let mut s = String::new();
for i in 0..1000 {
    s = s + &i.to_string();  // Allocates new string each time
}

// Good
let mut s = String::new();
for i in 0..1000 {
    s.push_str(&i.to_string());  // Modifies in place
}

// Best
let s: String = (0..1000)
    .map(|i| i.to_string())
    .collect::<Vec<_>>()
    .join("");
```

### String Reuse

```rust
// Reuse string buffer
let mut buffer = String::new();
for line in lines {
    buffer.clear();
    buffer.push_str(prefix);
    buffer.push_str(line);
    process(&buffer);
}

// Avoid unnecessary clones
// Bad
fn process(s: String) { }  // Takes ownership
let data = "Hello".to_string();
process(data.clone());     // Unnecessary clone
process(data);            // Error: moved

// Good
fn process(s: &str) { }    // Borrows
let data = "Hello".to_string();
process(&data);           // Borrow
process(&data);           // Can reuse
```

## Common Patterns

### String Validation

```rust
// Email validation (simple)
fn is_valid_email(email: &str) -> bool {
    email.contains('@') && email.contains('.')
}

// URL validation
fn is_valid_url(url: &str) -> bool {
    url.starts_with("http://") || url.starts_with("https://")
}

// Custom validation
fn is_valid_username(username: &str) -> bool {
    username.len() >= 3 &&
    username.len() <= 20 &&
    username.chars().all(|c| c.is_alphanumeric() || c == '_')
}
```

### String Templates

```rust
// Simple template replacement
fn render_template(template: &str, values: &HashMap<String, String>) -> String {
    let mut result = template.to_string();
    for (key, value) in values {
        let placeholder = format!("{{{{{}}}}}", key);
        result = result.replace(&placeholder, value);
    }
    result
}

// Usage
let template = "Hello, {{name}}! You have {{count}} messages.";
let mut values = HashMap::new();
values.insert("name".to_string(), "Alice".to_string());
values.insert("count".to_string(), "5".to_string());
let rendered = render_template(template, &values);
// "Hello, Alice! You have 5 messages."
```

### String Sanitization

```rust
// Remove dangerous characters
fn sanitize_html(input: &str) -> String {
    input
        .replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace("\"", "&quot;")
        .replace("'", "&#x27;")
}

// Sanitize filename
fn sanitize_filename(filename: &str) -> String {
    filename
        .chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '.' || c == '-' || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect()
}
```

## JavaScript Interop

When transpiled to JavaScript:

```rust
// Husk string operations
let s = "Hello, World!";
let upper = s.to_upper();
let parts = s.split(", ");
let replaced = s.replace("World", "Husk");
```

Becomes:
```javascript
// JavaScript equivalents
const s = "Hello, World!";
const upper = s.toUpperCase();
const parts = s.split(", ");
const replaced = s.replace("World", "Husk");
```

### Method Mappings

| Husk Method | JavaScript Equivalent |
|-------------|---------------------|
| `to_upper()` | `toUpperCase()` |
| `to_lower()` | `toLowerCase()` |
| `trim()` | `trim()` |
| `split()` | `split()` |
| `replace()` | `replace()` |
| `starts_with()` | `startsWith()` |
| `ends_with()` | `endsWith()` |
| `contains()` | `includes()` |
| `len()` | `length` |

## Related Topics

- [Data Types](../language/data-types.md#string-type) - String type details
- [Unicode](../advanced/unicode.md) - Unicode handling
- [Regular Expressions](regex.md) - Pattern matching (planned)
- [File I/O](file-io.md) - Reading/writing strings
- [Performance](../advanced/performance.md) - String optimization

---

*Strings are fundamental to most programs. Husk's string operations provide safety, performance, and Unicode correctness.*