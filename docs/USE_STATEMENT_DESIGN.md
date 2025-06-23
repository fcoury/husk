# Use Statement Design for Husk

## Overview

The `use` statement in Husk needs to handle two distinct execution modes:
1. **Interpreter mode**: Direct execution of Husk code
2. **Transpiler mode**: Converting to JavaScript for Node.js/browser execution

## Design Considerations

### Interpreter Mode Behavior

#### Local Imports (`local::`, `self::`, `super::`)
- Load and parse the referenced `.hk` file
- Execute it in the current interpreter context
- Make exported symbols available in the current scope
- Cache parsed modules to avoid re-parsing

Example:
```husk
use local::utils::math;  // Loads src/utils/math.hk
let result = math::add(2, 3);
```

#### External Package Imports (no prefix)
Options:

**Option 1: Not Supported in Interpreter**
- External packages only work in transpiled mode
- Interpreter throws error: "External packages require transpilation"
- Simplest to implement, clear separation of concerns

**Option 2: JavaScript Bridge**
- Embed a JavaScript runtime (V8/QuickJS)
- Load npm packages through the JS bridge
- Marshal data between Husk and JavaScript
- Complex but enables full interop

**Option 3: On-the-fly Transpilation**
- Transpile imports to JS when encountered
- Execute JS code and capture exports
- Convert JS values back to Husk values
- Performance overhead but transparent to user

### Transpiler Mode Behavior

#### Local Imports
```husk
// Input
use local::utils::math;
use self::helper;
use super::shared;

// Output (ES Modules)
import * as math from "./utils/math.js";
import * as helper from "./helper.js";
import * as shared from "../shared.js";

// Output (CommonJS)
const math = require("./utils/math.js");
const helper = require("./helper.js");
const shared = require("../shared.js");
```

#### External Package Imports
```husk
// Input
use express::express;
use react::{React, useState};

// Output (ES Modules)
import express from "express";
import React, { useState } from "react";

// Output (CommonJS)
const express = require("express");
const { React, useState } = require("react");
```

## Module Resolution Algorithm

### For Interpreter

1. Parse the use statement to extract prefix and path
2. Based on prefix:
   - `local::` → resolve from project root
   - `self::` → resolve from current file's directory
   - `super::` → resolve from parent directory
   - No prefix → handle based on chosen strategy (error/bridge/transpile)
3. For local files:
   - Check cache for already-loaded module
   - If not cached, read and parse the file
   - Execute top-level statements
   - Extract exported symbols
   - Cache the module
4. Return module exports to importing scope

### For Transpiler

1. Parse the use statement
2. Based on prefix:
   - `local::` → convert to relative path from project root
   - `self::` → convert to relative path from current file
   - `super::` → convert to parent directory path
   - No prefix → keep as external package name
3. Generate appropriate import/require statement based on target
4. Track all imports for bundler optimization

## Type System Integration

### Type Declaration Files (.d.hk)

For external packages, type information comes from `.d.hk` files:

```husk
// types/express.d.hk
extern mod express {
    type Application;
    type Request;
    type Response;
    
    fn express() -> Application;
    
    impl Application {
        fn listen(port: int, callback: fn() -> void);
        fn get(path: string, handler: fn(Request, Response) -> void);
    }
}
```

### Type Resolution Process

1. When encountering `use express::express`:
   - Look for `types/express.d.hk` or `node_modules/@types/express.d.hk`
   - Parse type declarations
   - Make types available for type checking
   - No runtime execution needed for type-only information

## Export System

Husk modules need an export mechanism:

```husk
// math.hk
pub fn add(a: int, b: int) -> int {
    a + b
}

pub struct Calculator {
    value: int,
}

pub impl Calculator {
    // ...
}
```

The `pub` keyword marks items as exported. Without `pub`, items are module-private.

## Implementation Strategy

### Phase 1: Interpreter Support for Local Modules
1. Add `use` token to lexer ✅
2. Parse use statements with prefixes ✅
3. Implement module loading for local files
4. Add module caching
5. Handle `pub` keyword for exports ✅

**Completed:**
- Lexer now recognizes `use` and `pub` tokens
- Parser handles all use statement forms with local::, self::, super:: prefixes
- Parser recognizes pub modifier on functions, structs, and enums
- Transpiler converts use statements to appropriate JavaScript imports
- Interpreter shows clear error message for external packages
- Module loading infrastructure implemented:
  - Module struct with path and exports tracking
  - Path resolution for all prefix types (local::, self::, super::)
  - Module caching to avoid reloading
  - Context tracking (current file and project root)
  - Tests for error cases
- Export collection tracking (all top-level items currently exported)
- Semantic analyzer support for imported names (Unknown type)
- Successful module imports for functions, structs, and enums

**Current Limitations:**
- All top-level items are exported regardless of `pub` keyword
- The AST doesn't track visibility modifiers, requiring significant refactoring to fix
- Imported items have Unknown type in semantic analysis

### Phase 2: Type Declaration Support
1. Parse `.d.hk` files
2. Build type environment from declarations
3. Type check against external types

### Phase 3: Transpiler Support
1. Convert use statements to imports
2. Handle different module systems
3. Resolve paths correctly

### Phase 4: External Package Support (Choose Strategy)
1. Implement chosen strategy for interpreter
2. Add npm package resolution
3. Handle type definitions

## Recommendation

For the initial implementation, I recommend **Option 1** (external packages not supported in interpreter). This allows us to:

1. Focus on getting local modules working correctly
2. Implement a clean transpilation story
3. Add external package support later if needed
4. Keep the interpreter simple and fast

The interpreter would primarily be for:
- Development and testing of pure Husk code
- REPL exploration
- Quick scripts without external dependencies

While transpilation would be for:
- Production applications
- Using npm packages
- Browser deployment
- Full JavaScript ecosystem access

## Example: Complete Module System

```husk
// src/models/user.hk
pub struct User {
    id: int,
    name: string,
}

pub fn create_user(name: string) -> User {
    User { id: generate_id(), name }
}

fn generate_id() -> int {  // Private function
    // ...
}
```

```husk
// src/main.hk
use local::models::user::{User, create_user};
use express::express;  // Only works when transpiled

fn main() {
    let user = create_user("Alice");  // Works in interpreter
    
    // This block only works when transpiled
    let app = express();
    app.get("/user", |req, res| {
        res.json(user);
    });
}
```

When run in interpreter:
- Local imports work fine
- Express import throws: "External package 'express' requires transpilation to JavaScript"

When transpiled:
- Both imports work
- Generates proper JavaScript with all imports resolved