# Extern Declarations in Husk

Extern declarations allow Husk programs to interface with JavaScript libraries and APIs, particularly object-oriented ones. This feature enables seamless integration with the JavaScript ecosystem while maintaining type safety.

## Overview

Extern declarations provide a way to:
- Declare types and functions that exist in JavaScript
- Define method signatures for object-oriented APIs
- Maintain type safety when calling external code
- Support both functional and object-oriented JavaScript patterns

## Basic Syntax

### Extern Functions

Simple extern functions can be declared at the top level:

```husk
extern fn parseInt(s: string) -> int;
extern fn parseFloat(s: string) -> float;
extern fn setTimeout(callback: fn(), delay: int);
```

### Extern Modules

For organizing related functionality, use extern modules:

```husk
extern mod console {
    fn log(message: string);
    fn error(message: string);
    fn warn(message: string);
}
```

### Extern Types

Declare opaque types that represent JavaScript objects:

```husk
extern mod express {
    type Application;
    type Request;
    type Response;
}
```

## Object-Oriented API Support

### Implementing Methods on Extern Types

The most powerful feature of extern declarations is the ability to define methods on extern types using `impl` blocks:

```husk
extern mod express {
    type Application;
    type Request;
    type Response;
    
    impl Application {
        fn get(self, path: string, handler: fn(Request, Response));
        fn post(self, path: string, handler: fn(Request, Response));
        fn listen(self, port: int, callback: fn());
    }
    
    impl Response {
        fn send(self, data: string);
        fn json(self, data: any);
        fn status(self, code: int) -> Response;
    }
}
```

### The `self` Parameter

Methods in extern impl blocks can use `self` as the first parameter to indicate instance methods:

- `self` represents the object instance the method is called on
- `self` doesn't require a type annotation in extern declarations
- Methods without `self` are treated as static methods

## Complete Example: Express.js Integration

Here's a complete example showing how to use Express.js with Husk:

```husk
// Import the express default export
use external::express::default;

// Declare the express function and types
extern fn express() -> express::Application;

extern mod express {
    type Application;
    type Request;
    type Response;
    
    impl Application {
        fn get(self, path: string, handler: fn(Request, Response));
        fn post(self, path: string, handler: fn(Request, Response));
        fn put(self, path: string, handler: fn(Request, Response));
        fn delete(self, path: string, handler: fn(Request, Response));
        fn use_middleware(self, middleware: fn(Request, Response, fn()));
        fn listen(self, port: int, callback: fn());
    }
    
    impl Request {
        fn param(self, name: string) -> string;
        fn query(self, name: string) -> string;
        fn body(self) -> any;
    }
    
    impl Response {
        fn send(self, data: string);
        fn json(self, data: any);
        fn status(self, code: int) -> Response;
        fn redirect(self, url: string);
    }
}

fn main() {
    let app = express();
    
    // Define routes using method calls
    app.get("/", |req: express::Request, res: express::Response| {
        res.send("Hello, World!");
    });
    
    app.post("/api/users", |req: express::Request, res: express::Response| {
        res.json({ message: "User created", data: req.body() });
    });
    
    app.get("/users/:id", |req: express::Request, res: express::Response| {
        let user_id = req.param("id");
        res.json({ id: user_id, name: "John Doe" });
    });
    
    // Start the server
    app.listen(3000, || {
        println("Server running on http://localhost:3000");
    });
}
```

## Advanced Patterns

### Method Chaining

Extern types can support method chaining by returning `self`:

```husk
extern mod builder {
    type QueryBuilder;
    
    impl QueryBuilder {
        fn select(self, columns: string) -> QueryBuilder;
        fn from(self, table: string) -> QueryBuilder;
        fn where_clause(self, condition: string) -> QueryBuilder;
        fn order_by(self, column: string) -> QueryBuilder;
        fn limit(self, count: int) -> QueryBuilder;
        fn build(self) -> string;
    }
}

fn main() {
    let query = create_query()
        .select("name, email")
        .from("users")
        .where_clause("active = true")
        .order_by("created_at DESC")
        .limit(10)
        .build();
}
```

### Mixed Static and Instance Methods

Extern types can have both static methods (without `self`) and instance methods:

```husk
extern mod database {
    type Connection;
    
    // Static function to create a connection
    fn connect(url: string) -> Connection;
    
    impl Connection {
        // Instance methods with self
        fn query(self, sql: string) -> any;
        fn close(self);
        
        // Static methods without self
        fn escape_string(value: string) -> string;
    }
}
```

### Generic Types (Future Enhancement)

While not yet implemented, future versions will support generic extern types:

```husk
extern mod promise {
    type Promise<T>;
    
    impl Promise<T> {
        fn then<U>(self, callback: fn(T) -> U) -> Promise<U>;
        fn catch(self, handler: fn(any) -> T) -> Promise<T>;
    }
}
```

## Type Safety

Extern declarations maintain type safety at compile time:

1. **Parameter Checking**: The compiler verifies that method calls have the correct number and types of arguments
2. **Return Type Validation**: Return types are checked when extern functions are used in expressions
3. **Self Parameter**: The `self` parameter is automatically handled and doesn't count toward the argument count when calling methods

## Limitations

Current limitations of extern declarations:

1. **No Runtime Implementation**: Extern functions must exist in the JavaScript environment
2. **No Generic Types**: Generic parameters are not yet supported
3. **Limited Type Mapping**: Complex JavaScript types may need to be represented as `any`
4. **No Property Access**: Direct property access on extern types is not supported; use methods instead

## Best Practices

1. **Group Related Functionality**: Use extern modules to organize related types and functions
2. **Document External Dependencies**: Clearly document which npm packages or browser APIs are required
3. **Type Safety First**: Prefer specific types over `any` when possible
4. **Method Naming**: Follow Husk naming conventions (snake_case) even for JavaScript APIs
5. **Error Handling**: Consider wrapping extern calls that might throw in Result types

## Integration with Build System

When using extern declarations, ensure your `husk.toml` includes the necessary dependencies:

```toml
[dependencies]
express = "^5.0.0"

[build]
target = "node-esm"
```

The build system will:
- Generate appropriate import statements in the transpiled JavaScript
- Include type definitions for better IDE support
- Ensure dependencies are available at runtime

## Future Enhancements

Planned improvements to extern declarations include:

1. **Generic Type Parameters**: Support for `Promise<T>`, `Array<T>`, etc.
2. **Property Declarations**: Direct property access syntax
3. **TypeScript Integration**: Automatic generation from `.d.ts` files
4. **Async/Await**: Native support for Promise-based APIs
5. **Operator Overloading**: Support for JavaScript operators on extern types