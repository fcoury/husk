# Module System

Husk provides a flexible module system that supports both local project organization and external npm package integration.

## Import Syntax

The `use` statement is used to import modules and their contents:

```husk
use module_path::item;
use module_path::{item1, item2};
use module_path::*;
```

## Import Prefixes

Husk uses prefixes to distinguish between different types of imports:

### External Packages (No Prefix)

```husk
use express;
use lodash::{debounce, throttle};
use fs::readFile;
```

- Imports npm packages or Node.js built-in modules
- Requires packages to be declared in `husk.toml` dependencies
- Only works in transpiler mode (generates JavaScript `import`/`require`)
- In interpreter mode, shows error: "External packages require transpilation"

### Local Imports (`local::`)

```husk
use local::utils::logger::{Logger, LogLevel};
use local::models::User;
use local::config::*;
```

- Resolves from the **project root** directory (where `husk.toml` is located)
- Allows importing any module from anywhere in the project
- Transpiles to relative paths in JavaScript (e.g., `./utils/logger.js`)

### Current Directory (`self::`)

```husk
use self::helper;
use self::types::Config;
use self::components::*;
```

- Resolves from the **current file's directory**
- Used for importing sibling modules
- Equivalent to `./` in JavaScript imports

### Parent Directory (`super::`)

```husk
use super::shared;
use super::super::utils;  // Two levels up
use super::common::constants;
```

- Resolves from **parent directories**
- Can be chained to go multiple levels up
- Transpiles to `../` paths in JavaScript

## Special Import Syntax

### Default Exports

```husk
use express::default;  // Import default export
use external::express::default;  // Explicit external default
```

### Node.js Built-ins

```husk
use fs;
use fs::promises;  // Submodule
use path::join;    // Specific function
```

Node.js built-in modules receive special handling during transpilation.

## File Resolution

When resolving local modules, Husk:
1. First tries files with `.hk` extension
2. Then tries files with `.husk` extension
3. Returns an error if neither is found

Example:
- `use local::utils::math` looks for:
  - `src/utils/math.hk`
  - `src/utils/math.husk`

## Package Management

External packages must be declared in `husk.toml`:

```toml
[dependencies]
express = "4.18.0"
lodash = "4.17.21"

[dev-dependencies]
jest = "29.0.0"
```

The PackageResolver ensures:
- Packages are declared before use
- Correct versions are resolved from `node_modules`
- TypeScript definitions are found (if available)
- Module types (CommonJS/ESM) are detected

## Module Exports

Currently, all top-level items in a module are exported. The `pub` keyword is recognized by the parser but not yet enforced:

```husk
// All of these are currently exported
pub fn public_function() { }
fn private_function() { }  // Also exported (for now)

pub struct User {
    name: string,
}
```

## Examples

### Project Structure
```
my-project/
├── husk.toml
├── src/
│   ├── main.husk
│   ├── utils/
│   │   ├── logger.husk
│   │   └── math.husk
│   └── models/
│       └── user.husk
```

### main.husk
```husk
use local::utils::logger::{Logger, LogLevel};
use local::models::user::User;
use express;  // External package

fn main() {
    let logger = Logger::new(LogLevel::Info);
    let user = User::new("Alice");
    
    // External packages only work when transpiled
    let app = express();
}
```

### utils/logger.husk
```husk
use self::config;  // Sibling module
use super::models::user::User;  // Parent directory

pub enum LogLevel {
    Debug,
    Info,
    Warn,
    Error,
}

pub struct Logger {
    level: LogLevel,
}
```

## Error Handling

Common import errors:
- "Package 'X' not found in dependencies" - Add to `husk.toml`
- "Failed to read module 'X'" - Module file doesn't exist
- "External packages require transpilation" - Can't use npm packages in interpreter
- "Too many super:: levels" - Trying to go above filesystem root

## Implementation Notes

- Module resolution happens during semantic analysis
- Each module is cached after first load
- Circular dependencies are not yet detected
- The transpiler converts all imports to JavaScript-compatible paths