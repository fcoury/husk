# Extern Declaration Limitations

This document describes the current limitations with extern declarations in Husk and the workarounds needed.

## Current State

Husk supports extern declarations for foreign functions, types, and modules:

```rust
// Function declarations
extern fn parseInt(s: string) -> int;

// Type declarations
extern type XMLHttpRequest;

// Module declarations with types and functions
extern mod fs {
    fn readFile(path: string) -> string;
    fn writeFile(path: string, data: string);
}
```

## The Self Parameter Limitation

When declaring methods in extern impl blocks, Husk currently does not support the `self` parameter:

```rust
// This SHOULD work but doesn't:
extern mod express {
    type Application;
    
    impl Application {
        fn get(self, path: string, handler: fn(Request, Response) -> void);
        //     ^^^^ Error: Expected type annotation, found 'self'
    }
}

// Current workaround (doesn't work properly):
extern mod express {
    type Application;
    
    impl Application {
        fn get(path: string, handler: fn(Request, Response) -> void);
        // Missing self parameter causes semantic analyzer issues
    }
}
```

## The Problem

1. **Parser Issue**: The parser doesn't recognize `self` as a valid parameter in extern method declarations
2. **Semantic Analyzer Issue**: When calling methods on objects, the semantic analyzer automatically prepends the object as the first argument (like `self`), but extern methods are registered without accounting for this

This makes it impossible to properly declare object-oriented APIs from npm packages like Express, React, etc.

## Impact

Without proper self parameter support, we cannot:
- Declare methods that operate on instances
- Type-check method calls on external objects
- Provide proper IntelliSense/autocomplete for external APIs

## Proposed Solution

1. Modify the parser to accept `self` as a special first parameter in extern impl methods
2. Update the semantic analyzer to correctly match method calls with extern declarations that have self
3. Ensure the transpiler doesn't emit `self` in the generated JavaScript

## Workaround

Currently, the only workaround is to:
1. Import external packages using `use external::package::default`
2. Accept that method calls on imported objects will fail semantic analysis
3. Rely on the transpiler generating correct JavaScript despite semantic errors

This is not ideal as it breaks the type safety that Husk aims to provide.

## Example

Here's what we want to support:

```rust
extern mod express {
    type Application;
    type Request;
    type Response;
    
    impl Application {
        fn get(self, path: string, handler: fn(Request, Response) -> void);
        fn post(self, path: string, handler: fn(Request, Response) -> void);
        fn listen(self, port: int, callback: fn() -> void);
    }
    
    impl Response {
        fn send(self, data: string);
        fn json(self, data: any);
        fn status(self, code: int) -> Response;
    }
}

use external::express::default;

fn main() {
    let app = express();
    
    app.get("/", |req: express::Request, res: express::Response| {
        res.send("Hello, World!");
    });
    
    app.listen(3000, || {
        println("Server is running on http://localhost:3000");
    });
}
```

This would provide full type safety for external npm packages while generating correct JavaScript imports and method calls.