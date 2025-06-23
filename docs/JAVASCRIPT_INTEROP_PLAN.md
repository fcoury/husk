# JavaScript Interop Implementation Plan for Husk

## Overview

This document outlines the comprehensive plan for implementing JavaScript interoperability in Husk, enabling developers to:
1. Use Node.js packages and APIs
2. Build browser-compatible applications
3. Leverage the JavaScript ecosystem while maintaining Husk's type safety

## Goals

- **Seamless Integration**: Import and use JavaScript/TypeScript libraries naturally
- **Type Safety**: Maintain Husk's type checking for external APIs
- **Multiple Targets**: Support Node.js, browser, and bundler outputs
- **Performance**: Generate efficient, idiomatic JavaScript
- **Developer Experience**: Familiar syntax for JavaScript developers

## Language Extensions

### 1. Use/Import System

#### Syntax
```husk
// External npm packages
use express::express;
use lodash::{debounce, throttle};
use react::{React, useState, useEffect};
use fs::promises::{readFile, writeFile};
use @mui::material::{Button, TextField};

// Local imports from project root
use local::components::Header;
use local::models::User;
use local::services::database::connect;

// Parent directory imports
use super::shared::constants;
use super::super::utils::logger;

// Current directory siblings (explicit with self::)
use self::auth::login;
use self::validation::rules;

// Aliasing
use fs::promises::readFile as read_file;
use local::utils::formatters::dateFormat as formatDate;

// Type-only imports
use type express::{Request, Response};
```

#### Import Resolution Rules
- **No prefix** (e.g., `express::`) - External npm package
- **`local::`** - Import from project root
- **`super::`** - Import from parent directory(ies)
- **`self::`** - Import from current directory

#### Parser Changes
- Extend existing `TokenKind::Use` support
- Enhance `Stmt::Use` variant to handle different import sources:
  ```rust
  Use {
      prefix: UsePrefix,           // local, super, self, or none (external)
      path: Vec<String>,           // Module path segments
      imports: UseImports,         // What to import
      type_only: bool,
      span: Span,
  }
  
  enum UsePrefix {
      None,                        // External package
      Local,                       // local:: prefix
      Super(usize),                // super:: (count for multiple)
      Self,                        // self:: prefix
  }
  
  enum UseImports {
      All,                         // use module::*;
      Single(String),              // use module::item;
      Named(Vec<(String, Option<String>)>), // use module::{a, b as c};
  }
  ```

### 2. Async/Await Support

#### Syntax
```husk
async fn fetchUser(id: int) -> Result<User, Error> {
    let response = await fetch(`/api/users/${id}`);
    let data = await response.json();
    Ok(User::from_json(data))
}
```

#### AST Changes
- Add `async` modifier to function declarations
- Add `Expr::Await(Box<Expr>, Span)` variant
- Add `TypeAnnotation::Promise(Box<TypeAnnotation>)` for Promise types

### 3. Closure/Lambda Syntax

#### Syntax
```husk
// Single parameter
let double = |x| x * 2;

// Multiple parameters
let add = |a, b| a + b;

// With type annotations
let greet = |name: string| -> string { `Hello, ${name}!` };

// Async closures
let fetchData = async |url| await fetch(url);
```

#### AST Changes
- Add `Expr::Closure` variant:
  ```rust
  Closure {
      params: Vec<(String, Option<TypeAnnotation>)>,
      body: Box<Expr>,
      return_type: Option<TypeAnnotation>,
      is_async: bool,
      span: Span,
  }
  ```

### 4. Template Literals

#### Syntax
```husk
let name = "World";
let greeting = `Hello, ${name}!`;
let multiline = `
    This is a
    multiline template
    with ${1 + 1} expressions
`;
```

### 5. Spread Operator

#### Syntax
```husk
// Array spread
let arr1 = [1, 2, 3];
let arr2 = [...arr1, 4, 5];

// Struct spread
let user = User { name: "Alice", age: 30 };
let updated = User { ...user, age: 31 };
```

### 6. JSX Support (Optional)

#### Syntax
```husk
fn Button(props: ButtonProps) -> JSX {
    <button 
        className={props.className}
        onClick={props.onClick}>
        {props.children}
    </button>
}
```

## Type System Extensions

### 1. External Type Declarations

#### File Structure
```
project/
├── src/
│   └── main.hk
├── types/
│   ├── node.d.hk      # Node.js type definitions
│   ├── react.d.hk     # React type definitions
│   └── express.d.hk   # Express type definitions
└── husk.toml
```

#### Declaration Syntax
```husk
// types/node.d.hk
extern mod fs {
    mod promises {
        async fn readFile(path: string, encoding?: string) -> string;
        async fn writeFile(path: string, data: string) -> void;
    }
}

// types/express.d.hk
extern mod express {
    type Application;
    type Request;
    type Response;
    
    fn express() -> Application;
    
    impl Application {
        fn get(path: string, handler: fn(Request, Response) -> void);
        fn listen(port: int, callback?: fn() -> void);
    }
}
```

### 2. Generic Type Parameters

```husk
struct Container<T> {
    value: T,
}

fn map<T, U>(items: Vec<T>, f: fn(T) -> U) -> Vec<U> {
    let result = Vec::new();
    for item in items {
        result.push(f(item));
    }
    result
}
```

### 3. Union Types (for JS interop)

```husk
// For JavaScript APIs that return multiple types
type StringOrNumber = string | int;
type Nullable<T> = T | null;
```

## Transpiler Enhancements

### 1. Module System

#### CommonJS Output (--target=node-cjs)
```javascript
// Input: use express from "express";
const express = require("express");

// Input: export fn hello() { ... }
exports.hello = function hello() { ... };
```

#### ES Modules Output (--target=node-esm, --target=browser)
```javascript
// Input: use express from "express";
import express from "express";

// Input: export fn hello() { ... }
export function hello() { ... }
```

### 2. Async/Await Transformation

```javascript
// Husk: async fn getData() -> string { await fetch(url) }
async function getData() {
    return await fetch(url);
}
```

### 3. Type Stripping

Remove type annotations during transpilation:
```husk
// Input
fn add(a: int, b: int) -> int { a + b }

// Output
function add(a, b) { return a + b; }
```

### 4. Source Maps

Generate source maps for debugging:
- Line-to-line mappings
- Variable name preservation
- Stack trace translation

## Build System Integration

### 1. Configuration File (husk.toml)

```toml
[project]
name = "my-app"
version = "0.1.0"

[dependencies]
express = "^4.18.0"
react = "^18.2.0"

[build]
target = "node-esm"  # node-cjs, node-esm, browser, bundler
outdir = "./dist"
sourcemap = true

[types]
paths = ["./types", "./node_modules/@types"]
```

### 2. Package.json Generation

Automatically generate package.json from husk.toml:
```json
{
  "name": "my-app",
  "version": "0.1.0",
  "type": "module",
  "dependencies": {
    "express": "^4.18.0",
    "react": "^18.2.0"
  },
  "scripts": {
    "build": "husk build",
    "dev": "husk watch"
  }
}
```

### 3. TypeScript Declaration Output

Generate .d.ts files for Husk modules:
```typescript
// From: export fn add(a: int, b: int) -> int
export declare function add(a: number, b: number): number;
```

## Implementation Phases

### Phase 1: Core Language Features (Weeks 1-3)
1. Import/export syntax parsing ✅
2. Basic transpiler module system support ✅
3. External type declaration parsing
4. Simple Node.js API usage

**Completed:**
- Added Use and Pub tokens to lexer
- Implemented use statement parsing with local::, self::, super:: prefixes
- Added pub keyword parsing for function/struct/enum declarations
- Implemented transpiler conversion of use statements to JavaScript imports
- Added interpreter error handling for external packages
- Implemented module loading and caching infrastructure in interpreter
- Added path resolution for local::, self::, super:: prefixes
- Created module cache to avoid reloading modules
- Added tests for module loading error cases

### Phase 2: Async Programming (Weeks 4-5)
1. Async/await syntax and semantics
2. Promise type integration
3. Error handling for async functions

### Phase 3: Advanced Features (Weeks 6-8)
1. Closure/lambda syntax
2. Template literals
3. Spread operator
4. Generic type parameters

### Phase 4: Ecosystem Integration (Weeks 9-10)
1. Build system (husk.toml)
2. Package.json generation
3. npm package resolution
4. TypeScript declaration generation

### Phase 5: Optimizations (Weeks 11-12)
1. Source map generation
2. Tree shaking support
3. Bundler integration
4. Performance optimizations

### Phase 6: JSX Support (Optional, Weeks 13-14)
1. JSX parsing
2. JSX transformation
3. React type definitions

## Testing Strategy

### Unit Tests
- Parser tests for new syntax
- Type checker tests for external types
- Transpiler output validation

### Integration Tests
- Node.js API usage examples
- npm package imports
- Build system workflows

### Example Applications

#### 1. Node.js Express Server

```husk
// src/server.hk
use express::express;
use dotenv::dotenv;
use local::routes::api;
use local::middleware::auth::verifyToken;
use local::config::database;

async fn main() {
    dotenv::config();
    let app = express();
    
    app.use(express::json());
    app.use("/api", api::router);
    
    app.listen(3000, || {
        println("Server running on port 3000");
    });
}
```

```husk
// src/routes/api.hk
use express::{Router, Request, Response};
use local::controllers::UserController;
use super::middleware::auth::verifyToken;

pub fn router() -> Router {
    let router = Router::new();
    
    router.get("/users", verifyToken, UserController::list);
    router.post("/users", verifyToken, UserController::create);
    router.get("/users/:id", verifyToken, UserController::get);
    
    router
}
```

```husk
// src/controllers/UserController.hk
use express::{Request, Response};
use local::models::User;
use local::services::database::db;

impl UserController {
    async fn list(req: Request, res: Response) {
        let users = await User::find_all();
        res.json(users);
    }
    
    async fn create(req: Request, res: Response) {
        let user_data = req.body;
        let user = await User::create(user_data);
        res.status(201).json(user);
    }
    
    async fn get(req: Request, res: Response) {
        let id = req.params.id;
        match await User::find_by_id(id) {
            Some(user) => res.json(user),
            None => res.status(404).json({ error: "User not found" }),
        }
    }
}
```

#### 2. React Todo Application

```husk
// src/App.hk
use react::{React, useState, useEffect};
use local::components::{TodoList, AddTodo};
use local::types::Todo;
use local::services::api::todoService;

fn App() -> JSX {
    let [todos, setTodos] = useState<Vec<Todo>>([]);
    let [loading, setLoading] = useState(true);
    
    useEffect(|| {
        async {
            let data = await todoService::fetchAll();
            setTodos(data);
            setLoading(false);
        }
    }, []);
    
    fn addTodo(text: string) {
        let newTodo = Todo {
            id: Date::now(),
            text: text,
            completed: false,
        };
        setTodos([...todos, newTodo]);
    }
    
    fn toggleTodo(id: int) {
        setTodos(todos.map(|todo| {
            if todo.id == id {
                Todo { ...todo, completed: !todo.completed }
            } else {
                todo
            }
        }));
    }
    
    <div className="app">
        <h1>Husk Todo App</h1>
        <AddTodo onAdd={addTodo} />
        {loading ? (
            <p>Loading...</p>
        ) : (
            <TodoList todos={todos} onToggle={toggleTodo} />
        )}
    </div>
}
```

```husk
// src/components/TodoList.hk
use react::React;
use local::types::Todo;
use self::TodoItem;

struct TodoListProps {
    todos: Vec<Todo>,
    onToggle: fn(int),
}

fn TodoList(props: TodoListProps) -> JSX {
    <ul className="todo-list">
        {props.todos.map(|todo| 
            <TodoItem 
                key={todo.id} 
                todo={todo} 
                onToggle={props.onToggle} 
            />
        )}
    </ul>
}
```

#### 3. CLI Tool Example

```husk
// src/cli.hk
use commander::program;
use fs::promises::{readFile, writeFile};
use path::path;
use chalk::chalk;
use local::utils::config::Config;

async fn main() {
    program
        .version("1.0.0")
        .description("Husk CLI tool example");
    
    program
        .command("init")
        .description("Initialize a new project")
        .action(async || {
            println(chalk::blue("Initializing new Husk project..."));
            
            let config = Config::default();
            let config_path = path::join(process::cwd(), "husk.toml");
            
            await writeFile(config_path, config.to_toml());
            println(chalk::green("✓ Created husk.toml"));
        });
    
    program
        .command("build")
        .description("Build the project")
        .option("-w, --watch", "Watch for changes")
        .action(async |options| {
            let config = await Config::load();
            
            if options.watch {
                println(chalk::yellow("Watching for changes..."));
                // Watch implementation
            } else {
                println(chalk::blue("Building project..."));
                // Build implementation
            }
        });
    
    program.parse(process::argv);
}
```

## Migration Guide

### For JavaScript Developers
1. Import syntax uses `::` instead of `/` or `.`
2. Local imports require explicit prefixes (`local::`, `self::`, `super::`)
3. Type annotations are optional but recommended
4. Async/await works as expected
5. Can gradually adopt Husk's stronger type system

### For Husk Users
1. External packages use bare `package::item` syntax
2. Local imports use `local::` from project root
3. Current directory imports use `self::`
4. Async functions for I/O operations
5. External type declarations in `.d.hk` files
6. Build configuration via husk.toml

## Future Considerations

### Potential Extensions
1. Decorator syntax for frameworks
2. Reflection API for runtime type information
3. WebAssembly compilation target
4. Native Node.js addon support

### Performance Optimizations
1. Incremental compilation
2. Parallel transpilation
3. Caching of type definitions
4. Hot module reloading

## Conclusion

This JavaScript interop system will position Husk as a powerful alternative to TypeScript, offering:
- Better type safety through Husk's type system
- Seamless JavaScript ecosystem integration
- Modern syntax and features
- Excellent developer experience

The phased implementation approach ensures each feature is thoroughly tested before moving to the next, resulting in a robust and reliable system.