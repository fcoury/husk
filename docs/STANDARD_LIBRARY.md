# Husk Standard Library

This document provides comprehensive documentation for the Husk Standard Library, which offers essential functionality for common programming tasks while maintaining compatibility with both interpreter and transpiler modes.

## Overview

The Husk Standard Library follows Rust-inspired patterns while adapting to Husk's simplified type system and JavaScript compilation target. All standard library functions work identically in both interpreter and transpiler modes.

## Core Modules

### [String Operations](#string-operations)
Comprehensive string manipulation with Unicode support

### [Array Operations](#array-operations)
Functional array operations and transformations

### [IO Operations](#io-operations)
File system and console input/output

### [Type Utilities](#type-utilities)
Option and Result types for safe error handling

## String Operations

Husk provides a unified `string` type with extensive built-in methods. All string operations handle Unicode correctly.

### String Methods

#### Length and Inspection

- **`len() -> int`** - Returns the length of the string
  ```rust
  let s = "hello";
  println!(s.len()); // 5
  ```

- **`is_empty() -> bool`** - Checks if the string is empty
  ```rust
  let s = "";
  println!(s.is_empty()); // true
  ```

- **`chars() -> array<string>`** - Returns an array of single-character strings (Unicode-aware)
  ```rust
  let s = "hello👋";
  let chars = s.chars(); // ["h", "e", "l", "l", "o", "👋"]
  ```

- **`bytes() -> array<int>`** - Returns UTF-8 bytes as an array of integers
  ```rust
  let s = "hello";
  let bytes = s.bytes(); // [104, 101, 108, 108, 111]
  ```

#### Whitespace Handling

- **`trim() -> string`** - Removes whitespace from both ends
  ```rust
  let s = "  hello  ";
  println!(s.trim()); // "hello"
  ```

- **`trim_start() -> string`** - Removes leading whitespace
  ```rust
  let s = "  hello";
  println!(s.trim_start()); // "hello"
  ```

- **`trim_end() -> string`** - Removes trailing whitespace
  ```rust
  let s = "hello  ";
  println!(s.trim_end()); // "hello"
  ```

#### Pattern Searching

- **`contains(pattern: string) -> bool`** - Checks if string contains pattern
  ```rust
  let s = "hello world";
  println!(s.contains("world")); // true
  ```

- **`starts_with(prefix: string) -> bool`** - Checks if string starts with prefix
  ```rust
  let s = "hello world";
  println!(s.starts_with("hello")); // true
  ```

- **`ends_with(suffix: string) -> bool`** - Checks if string ends with suffix
  ```rust
  let s = "hello world";
  println!(s.ends_with("world")); // true
  ```

- **`find(pattern: string) -> Option<int>`** - Finds first occurrence of pattern
  ```rust
  let s = "hello world";
  match s.find("world") {
      Some(pos) => println!("Found at {}", pos), // Found at 6
      None => println!("Not found")
  }
  ```

- **`rfind(pattern: string) -> Option<int>`** - Finds last occurrence of pattern
  ```rust
  let s = "hello hello";
  match s.rfind("hello") {
      Some(pos) => println!("Last at {}", pos), // Last at 6
      None => println!("Not found")
  }
  ```

#### String Splitting

- **`split(delimiter: string) -> array<string>`** - Splits string by delimiter
  ```rust
  let s = "a,b,c";
  let parts = s.split(","); // ["a", "b", "c"]
  ```

- **`splitn(n: int, delimiter: string) -> array<string>`** - Splits with limit
  ```rust
  let s = "a,b,c,d";
  let parts = s.splitn(2, ","); // ["a", "b,c,d"]
  ```

- **`split_once(delimiter: string) -> Option<(string, string)>`** - Splits at first occurrence
  ```rust
  let s = "key=value=extra";
  match s.split_once("=") {
      Some((k, v)) => println!("{}: {}", k, v), // key: value=extra
      None => println!("No delimiter")
  }
  ```

- **`lines() -> array<string>`** - Splits by line breaks
  ```rust
  let s = "line1\nline2\nline3";
  let lines = s.lines(); // ["line1", "line2", "line3"]
  ```

#### Case Conversion

- **`to_lowercase() -> string`** - Converts to lowercase
  ```rust
  let s = "HELLO World";
  println!(s.to_lowercase()); // "hello world"
  ```

- **`to_uppercase() -> string`** - Converts to uppercase
  ```rust
  let s = "hello WORLD";
  println!(s.to_uppercase()); // "HELLO WORLD"
  ```

#### String Manipulation

- **`replace(from: string, to: string) -> string`** - Replaces all occurrences
  ```rust
  let s = "hello world";
  println!(s.replace("o", "0")); // "hell0 w0rld"
  ```

- **`substring(start: int, end: int) -> string`** - Extracts substring
  ```rust
  let s = "hello world";
  println!(s.substring(0, 5)); // "hello"
  ```

- **`slice(start: int, end: int) -> string`** - Slices string (supports negative indices)
  ```rust
  let s = "hello world";
  println!(s.slice(-5, -1)); // "worl"
  ```

- **`char_at(index: int) -> Option<string>`** - Gets character at index
  ```rust
  let s = "hello";
  match s.char_at(1) {
      Some(c) => println!("Char: {}", c), // Char: e
      None => println!("Out of bounds")
  }
  ```

- **`repeat(n: int) -> string`** - Repeats string n times
  ```rust
  let s = "ha";
  println!(s.repeat(3)); // "hahaha"
  ```

## Array Operations

Arrays in Husk are primitive types with extensive built-in methods for functional programming.

### Array Methods

#### Length and Access

- **`len() -> int`** - Returns array length
  ```rust
  let arr = [1, 2, 3];
  println!(arr.len()); // 3
  ```

- **`is_empty() -> bool`** - Checks if array is empty
  ```rust
  let arr = [];
  println!(arr.is_empty()); // true
  ```

- **`first() -> Option<T>`** - Gets first element
  ```rust
  let arr = [1, 2, 3];
  match arr.first() {
      Some(x) => println!("First: {}", x), // First: 1
      None => println!("Empty array")
  }
  ```

- **`last() -> Option<T>`** - Gets last element
  ```rust
  let arr = [1, 2, 3];
  match arr.last() {
      Some(x) => println!("Last: {}", x), // Last: 3
      None => println!("Empty array")
  }
  ```

- **`get(index: int) -> Option<T>`** - Gets element at index safely
  ```rust
  let arr = [1, 2, 3];
  match arr.get(1) {
      Some(x) => println!("Value: {}", x), // Value: 2
      None => println!("Out of bounds")
  }
  ```

#### Array Transformation

- **`slice(start: int, end: int) -> array<T>`** - Extracts sub-array
  ```rust
  let arr = [1, 2, 3, 4, 5];
  let sub = arr.slice(1, 4); // [2, 3, 4]
  ```

- **`concat(other: array<T>) -> array<T>`** - Concatenates arrays
  ```rust
  let arr1 = [1, 2];
  let arr2 = [3, 4];
  let combined = arr1.concat(arr2); // [1, 2, 3, 4]
  ```

- **`reverse() -> array<T>`** - Returns reversed array
  ```rust
  let arr = [1, 2, 3];
  let rev = arr.reverse(); // [3, 2, 1]
  ```

- **`sort() -> array<T>`** - Sorts array (requires Ord trait)
  ```rust
  let arr = [3, 1, 4, 1, 5];
  let sorted = arr.sort(); // [1, 1, 3, 4, 5]
  ```

#### Functional Operations

- **`map<U>(f: fn(T) -> U) -> array<U>`** - Transforms each element
  ```rust
  let numbers = [1, 2, 3];
  let doubled = numbers.map(|x| x * 2); // [2, 4, 6]
  ```

- **`filter(f: fn(T) -> bool) -> array<T>`** - Filters elements
  ```rust
  let numbers = [1, 2, 3, 4, 5];
  let evens = numbers.filter(|x| x % 2 == 0); // [2, 4]
  ```

- **`find(f: fn(T) -> bool) -> Option<T>`** - Finds first matching element
  ```rust
  let numbers = [1, 2, 3, 4];
  match numbers.find(|x| x > 2) {
      Some(x) => println!("Found: {}", x), // Found: 3
      None => println!("Not found")
  }
  ```

- **`position(f: fn(T) -> bool) -> Option<int>`** - Finds index of first match
  ```rust
  let numbers = [10, 20, 30, 40];
  match numbers.position(|x| x == 30) {
      Some(i) => println!("At index: {}", i), // At index: 2
      None => println!("Not found")
  }
  ```

#### Array Modification

- **`push(values...: T) -> array<T>`** - Returns new array with elements appended
  ```rust
  let arr = [1, 2];
  let new_arr = arr.push(3, 4); // [1, 2, 3, 4]
  ```

- **`pop() -> (array<T>, Option<T>)`** - Returns tuple of new array and popped element
  ```rust
  let arr = [1, 2, 3];
  let (new_arr, popped) = arr.pop(); // ([1, 2], Some(3))
  ```

#### Utility Methods

- **`join(separator: string) -> string`** - Joins elements into string
  ```rust
  let words = ["hello", "world"];
  println!(words.join(" ")); // "hello world"
  ```

- **`contains(value: T) -> bool`** - Checks if array contains value
  ```rust
  let arr = [1, 2, 3];
  println!(arr.contains(2)); // true
  ```

## IO Operations

Husk provides comprehensive I/O operations for file system and console interaction. All I/O operations return `Result` types for safe error handling.

### File Operations

#### Reading Files

- **`read_file(path: string) -> Result<string, Error>`** - Reads entire file as string
  ```rust
  match read_file("config.txt") {
      Ok(contents) => println!("File: {}", contents),
      Err(e) => println!("Error: {}", e)
  }
  ```

- **`read_file_bytes(path: string) -> Result<array<int>, Error>`** - Reads file as bytes
  ```rust
  match read_file_bytes("image.png") {
      Ok(bytes) => println!("Size: {} bytes", bytes.len()),
      Err(e) => println!("Error: {}", e)
  }
  ```

- **`read_lines(path: string) -> Result<array<string>, Error>`** - Reads file as lines
  ```rust
  match read_lines("data.txt") {
      Ok(lines) => {
          for line in lines {
              println!(line);
          }
      },
      Err(e) => println!("Error: {}", e)
  }
  ```

#### Writing Files

- **`write_file(path: string, contents: string) -> Result<(), Error>`** - Writes string to file
  ```rust
  match write_file("output.txt", "Hello, World!") {
      Ok(()) => println!("File written"),
      Err(e) => println!("Error: {}", e)
  }
  ```

- **`write_file_bytes(path: string, data: array<int>) -> Result<(), Error>`** - Writes bytes
  ```rust
  let data = [72, 101, 108, 108, 111]; // "Hello" in ASCII
  match write_file_bytes("binary.dat", data) {
      Ok(()) => println!("Binary file written"),
      Err(e) => println!("Error: {}", e)
  }
  ```

- **`append_file(path: string, contents: string) -> Result<(), Error>`** - Appends to file
  ```rust
  match append_file("log.txt", "New log entry\n") {
      Ok(()) => println!("Appended to log"),
      Err(e) => println!("Error: {}", e)
  }
  ```

#### Path Operations

- **`exists(path: string) -> bool`** - Checks if path exists
  ```rust
  if exists("config.json") {
      println!("Config file found");
  }
  ```

- **`is_file(path: string) -> bool`** - Checks if path is a file
  ```rust
  if is_file("document.txt") {
      println!("It's a file");
  }
  ```

- **`is_dir(path: string) -> bool`** - Checks if path is a directory
  ```rust
  if is_dir("src") {
      println!("Source directory exists");
  }
  ```

### Directory Operations

- **`create_dir(path: string) -> Result<(), Error>`** - Creates a directory
  ```rust
  match create_dir("output") {
      Ok(()) => println!("Directory created"),
      Err(e) => println!("Error: {}", e)
  }
  ```

- **`create_dir_all(path: string) -> Result<(), Error>`** - Creates directory and parents
  ```rust
  match create_dir_all("output/data/cache") {
      Ok(()) => println!("All directories created"),
      Err(e) => println!("Error: {}", e)
  }
  ```

- **`remove_dir(path: string) -> Result<(), Error>`** - Removes empty directory
  ```rust
  match remove_dir("temp") {
      Ok(()) => println!("Directory removed"),
      Err(e) => println!("Error: {}", e)
  }
  ```

- **`remove_dir_all(path: string) -> Result<(), Error>`** - Removes directory recursively
  ```rust
  match remove_dir_all("old_project") {
      Ok(()) => println!("Directory tree removed"),
      Err(e) => println!("Error: {}", e)
  }
  ```

- **`read_dir(path: string) -> Result<array<DirEntry>, Error>`** - Lists directory contents
  ```rust
  match read_dir(".") {
      Ok(entries) => {
          for entry in entries {
              println!("{} - {}", entry.name, 
                  if entry.is_dir { "directory" } else { "file" });
          }
      },
      Err(e) => println!("Error: {}", e)
  }
  ```

### Console Operations

- **`read_line() -> Result<string, Error>`** - Reads line from stdin
  ```rust
  println!("Enter your name:");
  match read_line() {
      Ok(name) => println!("Hello, {}!", name.trim()),
      Err(e) => println!("Error reading input: {}", e)
  }
  ```

- **`eprint(message: string) -> int`** - Prints to stderr without newline
  ```rust
  let bytes = eprint("Error: ");
  eprintln("Something went wrong");
  ```

- **`eprintln(message: string)`** - Prints to stderr with newline
  ```rust
  eprintln("Warning: This is a warning message");
  ```

### Async File Operations (Transpiler Only)

When compiling to JavaScript, async versions of file operations are available:

- **`read_file_async(path: string) -> Promise<Result<string, Error>>`**
- **`write_file_async(path: string, contents: string) -> Promise<Result<(), Error>>`**
- **`append_file_async(path: string, contents: string) -> Promise<Result<(), Error>>`**

Example:
```rust
async fn load_config() -> Result<Config, Error> {
    let contents = await read_file_async("config.json")?;
    Ok(parse_config(contents))
}
```

## Type Utilities

### Option Type

The `Option<T>` type represents an optional value:

```rust
enum Option<T> {
    Some(T),
    None
}
```

Used by methods that may not return a value:
```rust
let arr = [1, 2, 3];
match arr.first() {
    Some(x) => println!("First element: {}", x),
    None => println!("Array is empty")
}
```

### Result Type

The `Result<T, E>` type represents either success or failure:

```rust
enum Result<T, E> {
    Ok(T),
    Err(E)
}
```

Used by operations that can fail:
```rust
match read_file("data.txt") {
    Ok(contents) => process_data(contents),
    Err(error) => println!("Failed to read file: {}", error)
}
```

## Built-in Functions and Macros

### Printing

- **`print!(format, args...)`** - Prints formatted text without newline
  ```rust
  print!("Hello, {}!", "world");
  ```

- **`println!(format, args...)`** - Prints formatted text with newline
  ```rust
  println!("The answer is {}", 42);
  ```

- **`format!(format, args...) -> string`** - Returns formatted string
  ```rust
  let msg = format!("Score: {}/{}", 8, 10);
  ```

### Assertions

- **`assert!(condition)`** - Panics if condition is false
  ```rust
  assert!(x > 0);
  ```

- **`assert_eq!(left, right)`** - Panics if values are not equal
  ```rust
  assert_eq!(result, expected);
  ```

## Implementation Status

### ✅ Fully Implemented
- All string methods with Unicode support
- Basic array operations (pending closure support for map/filter)
- File I/O operations (sync and async)
- Directory operations
- Console I/O
- Print macros with formatting

### 🚧 In Progress
- Array functional methods (map, filter) - blocked on closure support
- Iterator support for lazy evaluation

### 📋 Planned
- Vec<T> type with mutable operations
- HashMap and HashSet collections
- Path manipulation utilities
- JSON parsing and serialization
- Regular expressions
- Time and date utilities

## JavaScript Translation

All standard library functions compile to efficient JavaScript:

### String Methods
- Direct mapping to JavaScript string methods
- Unicode handled via `Array.from()` for character iteration
- Option types converted to JavaScript pattern matching

### Array Methods
- Immutable operations return new arrays
- Functional methods use JavaScript's built-in array methods
- Safe bounds checking with Option returns

### I/O Operations
- File operations use Node.js `fs` module
- Async operations use Promises in transpiled code
- Result types handle JavaScript exceptions

## Best Practices

1. **Error Handling**: Always handle `Result` types from I/O operations
2. **Unicode Safety**: Use `chars()` method for character iteration
3. **Immutability**: String and array methods return new values
4. **Type Safety**: Use `Option` returns for safe element access
5. **Performance**: Prefer built-in methods over manual loops

## Examples

### Reading and Processing a File
```rust
fn process_config() -> Result<(), Error> {
    let contents = read_file("config.txt")?;
    let lines = contents.lines();
    
    for line in lines {
        if !line.is_empty() && !line.starts_with("#") {
            let parts = line.split("=");
            if parts.len() == 2 {
                println!("Config: {} = {}", parts[0].trim(), parts[1].trim());
            }
        }
    }
    
    Ok(())
}
```

### String Processing
```rust
fn normalize_name(name: string) -> string {
    name.trim()
        .to_lowercase()
        .replace(" ", "_")
        .replace("-", "_")
}
```

### Array Transformation
```rust
let numbers = [1, 2, 3, 4, 5];
let result = numbers
    .map(|x| x * 2)
    .filter(|x| x > 5)
    .join(", ");
println!("Result: {}", result); // Result: 6, 8, 10
```

## See Also

- [Language Features](LANGUAGE_FEATURES.md) - Core language documentation
- [Standard Library Plan](STANDARD_LIBRARY_PLAN.md) - Implementation details and roadmap