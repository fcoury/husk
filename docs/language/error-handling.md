# Error Handling

Husk provides robust error handling through the `Option` and `Result` types, eliminating null pointer exceptions and encouraging explicit error handling. This guide covers error handling patterns and best practices.

## Table of Contents

- [Overview](#overview)
- [Option Type](#option-type)
  - [Creating Options](#creating-options)
  - [Pattern Matching Options](#pattern-matching-options)
  - [Option Methods](#option-methods)
  - [Option Combinators](#option-combinators)
- [Result Type](#result-type)
  - [Creating Results](#creating-results)
  - [Pattern Matching Results](#pattern-matching-results)
  - [Result Methods](#result-methods)
  - [Result Combinators](#result-combinators)
- [The ? Operator](#the--operator)
- [Error Propagation](#error-propagation)
- [Custom Error Types](#custom-error-types)
- [Error Conversion](#error-conversion)
- [Best Practices](#best-practices)
- [Common Patterns](#common-patterns)
- [Panic and Recovery](#panic-and-recovery)
- [JavaScript Transpilation](#javascript-transpilation)
- [Related Topics](#related-topics)

## Overview

Husk's error handling philosophy:
- **No null** - Use `Option<T>` for values that might be absent
- **Explicit errors** - Use `Result<T, E>` for operations that can fail
- **Compiler-enforced** - Must handle all error cases
- **Composable** - Chain operations with combinators

### Key Types

```rust
// Option: Some value or None
enum Option<T> {
    Some(T),
    None,
}

// Result: Success or Error
enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

## Option Type

### Creating Options

```rust
// Direct construction
let some_value: Option<int> = Some(42);
let no_value: Option<int> = None;

// From functions that might fail
let parsed: Option<int> = "42".parse().ok();
let first: Option<&str> = ["a", "b", "c"].first();

// Conditional creation
let opt = if condition {
    Some("value")
} else {
    None
};

// From nullable operations
let divided = if divisor != 0 {
    Some(dividend / divisor)
} else {
    None
};
```

### Pattern Matching Options

```rust
// Basic pattern matching
match maybe_value {
    Some(value) => println!("Got: {}", value),
    None => println!("No value"),
}

// With binding
let message = match maybe_name {
    Some(name) => format!("Hello, {}!", name),
    None => "Hello, stranger!".to_string(),
};

// In function returns
fn get_user_name(id: int) -> string {
    match find_user(id) {
        Some(user) => user.name,
        None => "Unknown".to_string(),
    }
}

// If let for single pattern
if let Some(value) = maybe_value {
    use_value(value);
}

// While let for loops
while let Some(item) = iterator.next() {
    process(item);
}
```

### Option Methods

```rust
// Check if value exists
let has_value = option.is_some();
let is_empty = option.is_none();

// Extract value (dangerous!)
let value = some_value.unwrap();  // Panics if None
let value = none_value.unwrap();   // PANIC!

// Safe extraction with default
let value = option.unwrap_or(0);  // Returns 0 if None
let value = option.unwrap_or_else(|| compute_default());

// Transform the inner value
let doubled = Some(21).map(|x| x * 2);  // Some(42)
let doubled = None.map(|x| x * 2);      // None

// Filter based on predicate
let even = Some(4).filter(|x| x % 2 == 0);  // Some(4)
let even = Some(3).filter(|x| x % 2 == 0);  // None

// Get reference to inner value
let ref_value = option.as_ref();  // Option<&T>
let mut_ref = option.as_mut();    // Option<&mut T>
```

### Option Combinators

```rust
// Chain operations
let result = Some(2)
    .map(|x| x + 1)      // Some(3)
    .map(|x| x * 2)      // Some(6)
    .filter(|x| x > 5);  // Some(6)

// Flat map (and_then)
fn parse_int(s: &str) -> Option<int> {
    s.parse().ok()
}

let result = Some("42")
    .and_then(parse_int)     // Some(42)
    .map(|x| x * 2);         // Some(84)

// Or combinator
let value = None.or(Some(42));             // Some(42)
let value = Some(1).or(Some(2));           // Some(1)
let value = None.or_else(|| Some(compute())); // Lazy evaluation

// Zip options
let x = Some(1);
let y = Some(2);
let sum = x.zip(y).map(|(a, b)| a + b);  // Some(3)
```

## Result Type

### Creating Results

```rust
// Direct construction
let success: Result<int, string> = Ok(42);
let failure: Result<int, string> = Err("error".to_string());

// From fallible operations
fn divide(a: float, b: float) -> Result<float, string> {
    if b == 0.0 {
        Err("Division by zero".to_string())
    } else {
        Ok(a / b)
    }
}

// From parsing
fn parse_config(data: string) -> Result<Config, ParseError> {
    // Parsing logic that might fail
}

// Using Result in functions
fn read_number() -> Result<int, string> {
    let input = read_line();
    input.parse()
        .map_err(|_| "Invalid number".to_string())
}
```

### Pattern Matching Results

```rust
// Basic pattern matching
match operation() {
    Ok(value) => println!("Success: {}", value),
    Err(error) => println!("Error: {}", error),
}

// Early return pattern
fn process() -> Result<Output, Error> {
    let data = match load_data() {
        Ok(d) => d,
        Err(e) => return Err(e),
    };
    
    let parsed = match parse(data) {
        Ok(p) => p,
        Err(e) => return Err(e),
    };
    
    Ok(transform(parsed))
}

// If let for success case only
if let Ok(value) = result {
    use_value(value);
}

// Match with different error handling
match file_operation() {
    Ok(content) => process(content),
    Err(IoError::NotFound) => create_default(),
    Err(IoError::PermissionDenied) => request_permission(),
    Err(e) => log_error(e),
}
```

### Result Methods

```rust
// Check status
let is_success = result.is_ok();
let is_failure = result.is_err();

// Extract values (dangerous!)
let value = result.unwrap();      // Panics on Err
let error = result.unwrap_err();  // Panics on Ok

// Safe extraction with defaults
let value = result.unwrap_or(default);
let value = result.unwrap_or_else(|e| recover_from(e));
let value = result.unwrap_or_default();  // If T: Default

// Transform success value
let doubled = Ok(21).map(|x| x * 2);  // Ok(42)
let doubled = Err("e").map(|x| x * 2); // Err("e")

// Transform error value
let result = Err(5).map_err(|e| e * 2);  // Err(10)

// Extract as Option
let option = result.ok();  // Result<T, E> -> Option<T>
let error_opt = result.err();  // Result<T, E> -> Option<E>
```

### Result Combinators

```rust
// Chain operations
let result = Ok(2)
    .map(|x| x + 1)              // Ok(3)
    .map(|x| x * 2)              // Ok(6)
    .and_then(|x| divide(x, 2)); // Ok(3)

// Flat map (and_then)
fn step1(input: int) -> Result<int, Error> {
    Ok(input * 2)
}

fn step2(input: int) -> Result<int, Error> {
    if input > 10 {
        Ok(input)
    } else {
        Err(Error::TooSmall)
    }
}

let result = Ok(4)
    .and_then(step1)  // Ok(8)
    .and_then(step2); // Err(TooSmall)

// Or combinator for fallback
let result = Err("first").or(Ok(42));     // Ok(42)
let result = Ok(1).or(Ok(2));             // Ok(1)
let result = Err("e").or_else(|_| Ok(0)); // Ok(0)

// Collect Results
let results: array<Result<int, Error>> = vec![Ok(1), Ok(2), Ok(3)];
let collected: Result<array<int>, Error> = results.into_iter().collect();
// Ok([1, 2, 3])

let results = vec![Ok(1), Err("e"), Ok(3)];
let collected: Result<array<int>, &str> = results.into_iter().collect();
// Err("e") - fails fast
```

## The ? Operator

### Basic Usage

```rust
// Manual error propagation
fn read_username() -> Result<string, io::Error> {
    let mut file = match File::open("username.txt") {
        Ok(f) => f,
        Err(e) => return Err(e),
    };
    
    let mut username = string::new();
    match file.read_to_string(&mut username) {
        Ok(_) => Ok(username),
        Err(e) => Err(e),
    }
}

// With ? operator
fn read_username() -> Result<string, io::Error> {
    let mut file = File::open("username.txt")?;
    let mut username = string::new();
    file.read_to_string(&mut username)?;
    Ok(username)
}

// Chaining with ?
fn process_data() -> Result<Output, Error> {
    let raw = read_file("data.txt")?;
    let parsed = parse_data(raw)?;
    let validated = validate(parsed)?;
    Ok(transform(validated))
}
```

### ? with Option

```rust
// ? works with Option too
fn get_user_age(name: &str) -> Option<int> {
    let user = find_user(name)?;
    let profile = user.profile?;
    Some(profile.age)
}

// Early return on None
fn process_optional(data: Option<Data>) -> Option<Output> {
    let validated = validate(data?)?;
    Some(transform(validated))
}
```

## Error Propagation

### Function Signatures

```rust
// Propagating the same error type
fn load_and_parse() -> Result<Data, io::Error> {
    let content = read_file("data.json")?;
    let parsed = parse_json(content)?;  // Must return io::Error
    Ok(parsed)
}

// Converting error types
fn load_config() -> Result<Config, ConfigError> {
    let content = read_file("config.toml")
        .map_err(|e| ConfigError::Io(e))?;
    
    let config = parse_toml(content)
        .map_err(|e| ConfigError::Parse(e))?;
    
    Ok(config)
}
```

### Error Chains

```rust
// Building error context
fn process_user_data(id: int) -> Result<ProcessedData, Error> {
    let user = find_user(id)
        .ok_or_else(|| Error::UserNotFound(id))?;
    
    let data = load_user_data(user.id)
        .map_err(|e| Error::DataLoad(user.id, e))?;
    
    let processed = process(data)
        .map_err(|e| Error::Processing(user.id, e))?;
    
    Ok(processed)
}
```

## Custom Error Types

### Simple Error Enums

```rust
enum AppError {
    NotFound,
    InvalidInput(string),
    NetworkError(string),
    DatabaseError(string),
}

// Using custom errors
fn fetch_user(id: int) -> Result<User, AppError> {
    if id < 0 {
        return Err(AppError::InvalidInput("ID must be positive".to_string()));
    }
    
    let user = database.get(id)
        .map_err(|e| AppError::DatabaseError(e.to_string()))?;
    
    Ok(user)
}
```

### Error Structs

```rust
struct DetailedError {
    code: int,
    message: string,
    context: HashMap<string, string>,
}

impl DetailedError {
    fn new(code: int, message: string) -> DetailedError {
        DetailedError {
            code,
            message,
            context: HashMap::new(),
        }
    }
    
    fn with_context(mut self, key: string, value: string) -> DetailedError {
        self.context.insert(key, value);
        self
    }
}
```

### Nested Errors

```rust
enum ParseError {
    InvalidFormat(string),
    MissingField(string),
    ValidationError(ValidationError),
}

enum ValidationError {
    TooLong { max: int, actual: int },
    InvalidCharacter(char),
    OutOfRange { min: int, max: int, value: int },
}

// Composing errors
fn parse_and_validate(input: string) -> Result<Data, ParseError> {
    let parsed = parse(input)?;
    
    validate(parsed)
        .map_err(|e| ParseError::ValidationError(e))?;
    
    Ok(parsed)
}
```

## Error Conversion

### From Trait

```rust
// Implement From for automatic conversion
impl From<io::Error> for AppError {
    fn from(error: io::Error) -> AppError {
        AppError::IoError(error.to_string())
    }
}

impl From<ParseError> for AppError {
    fn from(error: ParseError) -> AppError {
        AppError::ParseError(error.to_string())
    }
}

// Now ? automatically converts
fn load_data() -> Result<Data, AppError> {
    let content = read_file("data.txt")?;  // io::Error -> AppError
    let parsed = parse(content)?;          // ParseError -> AppError
    Ok(parsed)
}
```

### Manual Conversion

```rust
// Convert with map_err
fn process() -> Result<Output, MyError> {
    let data = external_api()
        .map_err(|e| MyError::External(e.to_string()))?;
    
    let result = compute(data)
        .map_err(|e| MyError::Computation(e))?;
    
    Ok(result)
}

// Convert Option to Result
fn get_required_config(key: &str) -> Result<string, Error> {
    config.get(key)
        .ok_or_else(|| Error::MissingConfig(key.to_string()))
}
```

## Best Practices

### Use Appropriate Types

```rust
// Use Option for values that might not exist
fn find_user(id: int) -> Option<User> {
    database.get(id)
}

// Use Result for operations that can fail
fn save_user(user: User) -> Result<(), SaveError> {
    database.save(user)
}

// Don't use Result when Option suffices
// Bad
fn get_first() -> Result<Item, string> {
    items.first().ok_or("empty".to_string())
}

// Good
fn get_first() -> Option<Item> {
    items.first()
}
```

### Error Messages

```rust
// Provide context in errors
// Bad
Err("failed".to_string())

// Good
Err(format!("Failed to parse user ID: {}", input))

// Better: structured errors
Err(ParseError::InvalidUserId { 
    input: input.to_string(),
    reason: "must be positive integer".to_string()
})
```

### Early Returns

```rust
// Use ? and early returns to avoid nesting
// Bad
fn complex_operation() -> Result<Output, Error> {
    match step1() {
        Ok(r1) => {
            match step2(r1) {
                Ok(r2) => {
                    match step3(r2) {
                        Ok(r3) => Ok(r3),
                        Err(e) => Err(e),
                    }
                },
                Err(e) => Err(e),
            }
        },
        Err(e) => Err(e),
    }
}

// Good
fn complex_operation() -> Result<Output, Error> {
    let r1 = step1()?;
    let r2 = step2(r1)?;
    let r3 = step3(r2)?;
    Ok(r3)
}
```

## Common Patterns

### Fallback Chain

```rust
// Try multiple sources
fn get_config_value(key: &str) -> Option<string> {
    env_var(key)
        .or_else(|| config_file.get(key))
        .or_else(|| default_config.get(key))
}

// With Results
fn connect() -> Result<Connection, Error> {
    connect_primary()
        .or_else(|_| connect_backup())
        .or_else(|_| connect_failover())
}
```

### Collecting Results

```rust
// Process all items, fail on first error
fn process_all(items: array<Item>) -> Result<array<Output>, Error> {
    items.iter()
        .map(|item| process_item(item))
        .collect()  // Result<array<Output>, Error>
}

// Collect all results, separate successes and failures
fn process_batch(items: array<Item>) -> (array<Output>, array<Error>) {
    let results: array<Result<Output, Error>> = items.iter()
        .map(|item| process_item(item))
        .collect();
    
    let mut successes = vec![];
    let mut failures = vec![];
    
    for result in results {
        match result {
            Ok(output) => successes.push(output),
            Err(error) => failures.push(error),
        }
    }
    
    (successes, failures)
}
```

### Validation Pattern

```rust
struct Validator<T> {
    value: T,
    errors: array<string>,
}

impl<T> Validator<T> {
    fn new(value: T) -> Validator<T> {
        Validator { value, errors: vec![] }
    }
    
    fn validate<F>(mut self, f: F, error: &str) -> Validator<T>
    where F: Fn(&T) -> bool {
        if !f(&self.value) {
            self.errors.push(error.to_string());
        }
        self
    }
    
    fn finish(self) -> Result<T, array<string>> {
        if self.errors.is_empty() {
            Ok(self.value)
        } else {
            Err(self.errors)
        }
    }
}

// Usage
let user = Validator::new(user_input)
    .validate(|u| u.age >= 18, "Must be 18 or older")
    .validate(|u| u.email.contains('@'), "Invalid email")
    .validate(|u| u.username.len() >= 3, "Username too short")
    .finish()?;
```

## Panic and Recovery

### When to Panic

```rust
// Panic for unrecoverable errors
fn initialize() {
    let config = load_config()
        .expect("Failed to load configuration");
    
    let database = connect_database(&config)
        .expect("Failed to connect to database");
}

// Panic for programmer errors
fn divide_by(n: int, d: int) -> int {
    assert!(d != 0, "Division by zero");
    n / d
}

// Use Result for recoverable errors
fn parse_user_input(input: string) -> Result<Command, ParseError> {
    // Don't panic on bad user input
}
```

### Assertions

```rust
// Debug assertions (removed in release)
debug_assert!(index < array.len());

// Always-on assertions
assert!(divisor != 0, "Divisor must not be zero");

// Assert equality
assert_eq!(computed, expected);
assert_ne!(value, forbidden_value);
```

## JavaScript Transpilation

Husk's error handling transpiles to JavaScript patterns:

```rust
// Husk
match result {
    Ok(value) => process(value),
    Err(error) => handle_error(error),
}

// Option handling
let value = option.unwrap_or(0);

// Result chaining
let output = input
    .parse()
    .map(|n| n * 2)
    .unwrap_or(0);
```

Becomes:
```javascript
// JavaScript
if (result.ok !== undefined) {
    process(result.ok);
} else {
    handle_error(result.err);
}

// Option handling
const value = option !== null ? option : 0;

// Result chaining
const output = (() => {
    const parsed = parse(input);
    if (parsed.ok !== undefined) {
        return parsed.ok * 2;
    }
    return 0;
})();
```

## Related Topics

- [Pattern Matching](pattern-matching.md) - Matching Option and Result
- [Functions](functions.md) - Error handling in functions
- [Type System](types.md) - Option and Result types
- [Best Practices](../advanced/best-practices.md) - Error handling guidelines
- [Control Flow](control-flow.md) - Error handling flow

---

*Effective error handling is crucial for robust applications. Husk's Option and Result types make errors explicit and composable.*