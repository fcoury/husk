# Local Module System Example

This example demonstrates Husk's local module system and JavaScript interop features.

## Project Structure

```
local-modules/
├── main.husk           # Entry point
├── utils/
│   └── logger.husk     # Logging utilities
├── math/
│   └── calculator.husk # Mathematical operations
└── data/
    └── user.husk       # User data structures
```

## Features Demonstrated

### Module System
- Local module imports using `local::` prefix
- Self-relative imports using `self::` prefix
- Public exports with `pub` keyword
- Structured module organization

### JavaScript Interop Features
- **Type Casting**: Using `as` operator for explicit type conversions
- **Built-in Methods**: String methods like `.trim()`, `.toUpperCase()`, `.len()`
- **Template Literals**: String formatting with `format!` macro
- **Enum Support**: Pattern matching with enum variants
- **Struct Implementation**: Methods and associated functions
- **Option Types**: Handling nullable values with `Option<T>`

### Code Examples

#### Type Casting
```husk
let score_str = "95";
let score = score_str as int;  // String to integer conversion
```

#### Built-in String Methods
```husk
let message = "  Welcome to Husk Module System!  ";
let clean_message = message.trim().toUpperCase();
```

#### Module Imports
```husk
use local::utils::logger::{Logger, LogLevel};
use local::math::calculator::{Calculator, Operation};
use local::data::user::{User, UserProfile};
```

#### Struct and Enum Usage
```husk
let logger = Logger::new(LogLevel::Info);
let calc = Calculator::new();
let result = calc.calculate(Operation::Add, 10, 5);
```

## Running the Example

To transpile this example to JavaScript:

```bash
husk build local-modules/main.husk --target browser
```

The transpiled JavaScript will include:
- ES6 module imports/exports
- Proper type conversions
- Built-in method implementations
- Error handling for type casts

## JavaScript Output

The Husk code will be transpiled to modern JavaScript with:
- Type-safe conversions using `Number()`, `String()`, `Boolean()`
- Built-in method implementations for strings and arrays
- Module imports using ES6 `import`/`export` syntax
- Proper error handling for invalid operations

## Educational Value

This example showcases:
1. **Module Organization**: How to structure a multi-file Husk project
2. **JavaScript Integration**: Seamless interop with JavaScript ecosystems
3. **Type Safety**: Compile-time type checking with runtime conversions
4. **Modern Syntax**: Clean, readable code that transpiles to efficient JavaScript
5. **Best Practices**: Proper error handling and code organization