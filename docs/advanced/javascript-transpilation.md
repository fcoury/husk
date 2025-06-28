# JavaScript Transpilation

Husk compiles to JavaScript, enabling deployment to web browsers, Node.js, and any JavaScript runtime. This guide covers the transpilation process, interoperability features, and best practices for JavaScript integration.

## Table of Contents

- [Overview](#overview)
- [Basic Transpilation](#basic-transpilation)
- [Generated JavaScript](#generated-javascript)
- [Type Mappings](#type-mappings)
- [JavaScript Interop](#javascript-interop)
- [Module System](#module-system)
- [Runtime Features](#runtime-features)
- [Optimization](#optimization)
- [Debugging](#debugging)
- [Deployment](#deployment)
- [Best Practices](#best-practices)
- [Common Patterns](#common-patterns)

## Overview

Husk's transpilation to JavaScript provides:

- **Zero-overhead abstractions** - Husk types compile to efficient JavaScript
- **Readable output** - Generated code is human-readable and debuggable
- **Full ecosystem access** - Use any JavaScript library seamlessly
- **Modern JavaScript** - Targets ES2020+ with optional polyfills
- **Source maps** - Debug Husk code directly in browser DevTools

### Key Benefits

1. **Web deployment** - Run Husk applications in browsers
2. **Node.js compatibility** - Build server applications
3. **NPM integration** - Publish and consume packages
4. **Gradual migration** - Mix Husk and JavaScript code
5. **Performance** - Optimized JavaScript output

## Basic Transpilation

### Command Line Usage

```bash
# Transpile a single file
husk build main.hk --target js

# Transpile with options
husk build main.hk --target js --output dist/app.js

# Transpile entire project
husk build --target js --output-dir dist/

# Watch mode for development
husk build --target js --watch
```

### Build Configuration

Create a `Husk.toml` file for project settings:

```toml
[build]
target = "js"
output-dir = "dist"
source-maps = true

[js]
module-type = "esm"  # "esm" or "commonjs"
target-version = "es2020"
minify = true
bundle = true
```

## Generated JavaScript

### Simple Example

Husk code:
```rust
fn greet(name: string) -> string {
    format!("Hello, {}!", name)
}

fn main() {
    let message = greet("World");
    println!(message);
}
```

Generated JavaScript:
```javascript
function greet(name) {
    return `Hello, ${name}!`;
}

function main() {
    const message = greet("World");
    console.log(message);
}

main();
```

### Complex Example

Husk code:
```rust
struct User {
    name: string,
    age: int,
}

impl User {
    fn new(name: string, age: int) -> User {
        User { name, age }
    }
    
    fn is_adult(&self) -> bool {
        self.age >= 18
    }
}

fn main() {
    let users = [
        User::new("Alice", 25),
        User::new("Bob", 17),
    ];
    
    let adults = users.filter(|u| u.is_adult());
    for user in adults {
        println!("{} is an adult", user.name);
    }
}
```

Generated JavaScript:
```javascript
class User {
    constructor(name, age) {
        this.name = name;
        this.age = age;
    }
    
    static new(name, age) {
        return new User(name, age);
    }
    
    is_adult() {
        return this.age >= 18;
    }
}

function main() {
    const users = [
        User.new("Alice", 25),
        User.new("Bob", 17),
    ];
    
    const adults = users.filter(u => u.is_adult());
    for (const user of adults) {
        console.log(`${user.name} is an adult`);
    }
}

main();
```

## Type Mappings

### Primitive Types

| Husk Type | JavaScript Type | Notes |
|-----------|----------------|-------|
| `int` | `number` | 32-bit integers |
| `float` | `number` | 64-bit floats |
| `bool` | `boolean` | `true`/`false` |
| `string` | `string` | UTF-8 strings |
| `char` | `string` | Single character |
| `()` | `undefined` | Unit type |

### Composite Types

| Husk Type | JavaScript Type | Example |
|-----------|----------------|---------|
| `array<T>` | `Array<T>` | `[1, 2, 3]` |
| `(T, U)` | `[T, U]` | Tuple as array |
| `Option<T>` | `T \| null` | `null` for `None` |
| `Result<T, E>` | `{ ok: T } \| { err: E }` | Tagged union |
| `struct` | `class` | ES6 classes |
| `enum` | Tagged objects | Pattern matching |

### Option Type Transpilation

```rust
// Husk
fn find_user(id: int) -> Option<User> {
    if id == 1 {
        Some(User::new("Alice", 25))
    } else {
        None
    }
}

let user = find_user(1);
match user {
    Some(u) => println!("Found: {}", u.name),
    None => println!("Not found"),
}
```

```javascript
// JavaScript
function find_user(id) {
    if (id === 1) {
        return User.new("Alice", 25);
    } else {
        return null;
    }
}

const user = find_user(1);
if (user !== null) {
    console.log(`Found: ${user.name}`);
} else {
    console.log("Not found");
}
```

### Result Type Transpilation

```rust
// Husk
fn divide(a: float, b: float) -> Result<float, string> {
    if b == 0.0 {
        Err("Division by zero")
    } else {
        Ok(a / b)
    }
}
```

```javascript
// JavaScript
function divide(a, b) {
    if (b === 0.0) {
        return { err: "Division by zero" };
    } else {
        return { ok: a / b };
    }
}
```

## JavaScript Interop

### Importing JavaScript

```rust
// Import JavaScript modules
extern "js" {
    // Import functions
    fn fetch(url: string) -> Promise<Response>;
    
    // Import classes
    type Date;
    fn Date::now() -> float;
    
    // Import values
    static console: Console;
}

// Use imported JavaScript
fn main() {
    let timestamp = Date::now();
    console.log("Current time:", timestamp);
}
```

### Exporting to JavaScript

```rust
// Mark functions for export
#[export]
pub fn calculate_tax(amount: float, rate: float) -> float {
    amount * rate
}

// Export classes
#[export]
pub struct Calculator {
    precision: int,
}

#[export]
impl Calculator {
    pub fn new(precision: int) -> Calculator {
        Calculator { precision }
    }
    
    pub fn round(&self, value: float) -> float {
        // Implementation
    }
}
```

Generated JavaScript module:
```javascript
export function calculate_tax(amount, rate) {
    return amount * rate;
}

export class Calculator {
    constructor(precision) {
        this.precision = precision;
    }
    
    static new(precision) {
        return new Calculator(precision);
    }
    
    round(value) {
        // Implementation
    }
}
```

### Using NPM Packages

```rust
// Import from node_modules
extern "npm" {
    // Import lodash
    mod lodash {
        fn debounce<T>(func: fn(T), wait: int) -> fn(T);
        fn chunk<T>(array: array<T>, size: int) -> array<array<T>>;
    }
    
    // Import React
    mod react {
        type ReactElement;
        fn createElement(tag: string, props: Object, children: array<ReactElement>) -> ReactElement;
    }
}

fn main() {
    let numbers = [1, 2, 3, 4, 5, 6];
    let chunks = lodash::chunk(numbers, 2);
    // [[1, 2], [3, 4], [5, 6]]
}
```

## Module System

### ES Modules

```rust
// math.hk
pub fn add(a: int, b: int) -> int {
    a + b
}

pub fn multiply(a: int, b: int) -> int {
    a * b
}
```

Transpiles to:
```javascript
// math.js
export function add(a, b) {
    return a + b;
}

export function multiply(a, b) {
    return a * b;
}
```

### CommonJS

With `module-type = "commonjs"`:
```javascript
// math.js
function add(a, b) {
    return a + b;
}

function multiply(a, b) {
    return a * b;
}

module.exports = { add, multiply };
```

## Runtime Features

### Async/Await

```rust
// Husk async syntax (planned)
async fn fetch_user(id: int) -> Result<User, string> {
    let response = await fetch(format!("/api/users/{}", id))?;
    let data = await response.json()?;
    Ok(User::from_json(data))
}
```

Transpiles to:
```javascript
async function fetch_user(id) {
    try {
        const response = await fetch(`/api/users/${id}`);
        const data = await response.json();
        return { ok: User.from_json(data) };
    } catch (e) {
        return { err: e.toString() };
    }
}
```

### Memory Management

Husk's ownership system helps generate efficient JavaScript:

```rust
fn process_data(data: Vec<int>) -> Vec<int> {
    // Ownership transferred, no cloning needed
    data.into_iter()
        .map(|x| x * 2)
        .collect()
}
```

```javascript
function process_data(data) {
    // Direct array manipulation, no copying
    return data.map(x => x * 2);
}
```

## Optimization

### Dead Code Elimination

Unused functions and types are automatically removed:

```rust
// Only used functions are included in output
fn used_function() { }
fn unused_function() { }  // Removed

fn main() {
    used_function();
}
```

### Constant Folding

```rust
const PI: float = 3.14159;
const TAU: float = PI * 2.0;  // Computed at compile time

fn area(radius: float) -> float {
    PI * radius * radius
}
```

```javascript
// Constants are inlined
function area(radius) {
    return 3.14159 * radius * radius;
}
```

### Bundle Size Optimization

```toml
[js.optimization]
tree-shaking = true
minify = true
compress = true
mangle = true
```

## Debugging

### Source Maps

Enable source maps for debugging:

```bash
husk build --target js --source-maps
```

In browser DevTools, you'll see Husk source code:
- Set breakpoints in `.hk` files
- Step through Husk code
- Inspect Husk variables

### Debug Builds

```bash
# Development build with debug info
husk build --target js --debug

# Production build with optimizations
husk build --target js --release
```

### Console Integration

```rust
// Debug prints work in browser console
println!("Debug: {}", value);        // console.log
eprintln!("Error: {}", error);       // console.error

// Conditional compilation
#[cfg(debug)]
println!("Debug mode only");
```

## Deployment

### Browser Deployment

```html
<!DOCTYPE html>
<html>
<head>
    <script type="module" src="app.js"></script>
</head>
<body>
    <!-- Your Husk app runs here -->
</body>
</html>
```

### Node.js Deployment

```json
// package.json
{
  "name": "husk-app",
  "version": "1.0.0",
  "type": "module",
  "main": "dist/main.js",
  "scripts": {
    "build": "husk build --target js --output-dir dist",
    "start": "node dist/main.js"
  }
}
```

### CDN Deployment

```bash
# Build for CDN
husk build --target js --format umd --output husk-lib.min.js
```

## Best Practices

### 1. Type Annotations for Interop

```rust
// Explicit types help transpilation
extern "js" {
    fn parseInt(s: string, radix: int) -> int;
}

// Type aliases for clarity
type DOMElement = JsObject;
type EventHandler = fn(Event) -> ();
```

### 2. Error Handling

```rust
// Husk Result maps well to try/catch
fn safe_parse(json: string) -> Result<Data, string> {
    // Transpiler generates appropriate try/catch
}

// Use Option for nullable values
fn find_element(id: string) -> Option<DOMElement> {
    // Returns null in JavaScript when None
}
```

### 3. Performance Considerations

```rust
// Avoid excessive allocations
let result = data
    .iter()
    .filter(|x| x > 0)
    .map(|x| x * 2)
    .collect();  // Single allocation

// Use references when possible
fn process(data: &[int]) -> int {
    // No copying needed
}
```

### 4. Module Organization

```
src/
  lib.hk          # Library exports
  main.hk         # Application entry
  utils/
    math.hk       # Utility modules
    string.hk
dist/
  lib.js          # Transpiled library
  main.js         # Transpiled app
```

## Common Patterns

### Web Application

```rust
// DOM manipulation
extern "js" {
    fn document() -> Document;
    type Document;
    impl Document {
        fn getElementById(&self, id: string) -> Option<Element>;
        fn createElement(&self, tag: string) -> Element;
    }
}

fn main() {
    let doc = document();
    match doc.getElementById("app") {
        Some(app) => {
            let button = doc.createElement("button");
            button.textContent = "Click me!";
            app.appendChild(button);
        },
        None => eprintln!("App element not found"),
    }
}
```

### Node.js Server

```rust
extern "js" {
    mod express {
        type App;
        fn express() -> App;
        impl App {
            fn get(&self, path: string, handler: fn(Request, Response));
            fn listen(&self, port: int);
        }
    }
}

fn main() {
    let app = express::express();
    
    app.get("/", |req, res| {
        res.send("Hello from Husk!");
    });
    
    app.listen(3000);
    println!("Server running on http://localhost:3000");
}
```

### React Component

```rust
extern "js" {
    mod react {
        fn useState<T>(initial: T) -> (T, fn(T));
    }
}

#[component]
fn Counter() -> ReactElement {
    let (count, setCount) = react::useState(0);
    
    jsx! {
        <div>
            <p>Count: {count}</p>
            <button onClick={|| setCount(count + 1)}>
                Increment
            </button>
        </div>
    }
}
```

## Troubleshooting

### Common Issues

**"Cannot find module"**
- Ensure NPM packages are installed
- Check import paths match JavaScript conventions
- Verify `node_modules` is in the correct location

**Type mismatches**
- JavaScript numbers are always floats
- Arrays may contain mixed types in JavaScript
- Use type assertions when needed

**Performance issues**
- Enable optimizations with `--release`
- Use Chrome DevTools profiler
- Check for unnecessary allocations

### Migration Tips

When migrating JavaScript to Husk:
1. Start with type definitions for existing code
2. Gradually convert modules
3. Use `extern "js"` for gradual migration
4. Keep build outputs separate
5. Test both Husk and JavaScript versions

## Related Topics

- [JavaScript Interop](js-interop.md) - Detailed interop guide
- [Build System](../tools/build.md) - Build configuration
- [Module System](../language/modules.md) - Husk modules
- [Type System](../language/types.md) - Understanding types
- [Performance](performance.md) - Optimization guide

---

*Husk's JavaScript transpilation enables you to write safe, expressive code while leveraging the entire JavaScript ecosystem.*