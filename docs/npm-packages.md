# Using npm Packages in Husk

Husk provides comprehensive support for using npm packages in your Husk code through its `husk.toml` configuration file and package resolution system.

## Configuration in husk.toml

### Basic Dependency Declaration

Add npm packages to your `husk.toml` file under the `[dependencies]` section:

```toml
[dependencies]
# Simple version specification
lodash = "^4.17.21"
express = "^4.18.0"

# Detailed dependency specification
axios = { version = "^1.4.0", optional = false }
moment = { version = "^2.29.0", features = ["timezone"] }

# Git dependency
my-utils = { git = "https://github.com/user/my-utils", branch = "main" }

# Local path dependency
shared = { path = "../shared-lib" }

[dev-dependencies]
jest = "^29.0.0"
typescript = "^5.0.0"
eslint = "^8.0.0"
```

### Dependency Specification Types

1. **Simple**: `package = "version"`
2. **Detailed**: Full object with optional fields:
   - `version`: Version constraint
   - `path`: Local file path
   - `git`: Git repository URL
   - `branch`/`tag`: Git reference
   - `optional`: Whether dependency is optional
   - `features`: Enable specific package features

## Using Packages in Husk Code

### Import Syntax

Husk uses Rust-like `use` statements to import npm packages:

```husk
// Import the main export
use express::express;

// Import named exports
use lodash::{debounce, throttle, capitalize};

// Import with alias
use axios::{axios as httpClient};

// Import all exports (use with caution)
use utils::*;
```

### Supported Import Patterns

- **External packages**: `use package_name::item`
- **Local modules**: `use local::module::item`
- **Relative imports**: `use self::sibling` or `use super::parent`

### Example Usage

```husk
// File: src/main.husk
use express::express;
use lodash::{debounce, capitalize};
use axios::axios;

fn main() {
    // Create an Express app
    let app = express();
    
    // Use lodash utilities
    let message = capitalize("hello from husk!");
    println!("Message: {}", message);
    
    // Create a debounced function
    let debounced_log = debounce(|| {
        println!("Debounced log executed");
    }, 1000);
    
    println!("Express app created successfully!");
}
```

## Package Resolution System

### How It Works

1. **Configuration Loading**: Husk reads `husk.toml` to find declared dependencies
2. **Package Discovery**: Looks for packages in `node_modules/`
3. **Type Resolution**: Determines if package is CommonJS, ESM, or UMD
4. **Import Generation**: Creates appropriate JavaScript import statements

### Package Types Supported

- **CommonJS**: Generates `const pkg = require("package")`
- **ES Modules**: Generates `import { item } from "package"`
- **Mixed**: Handles packages with both CommonJS and ESM exports

### Generated JavaScript

Husk transpiles import statements to appropriate JavaScript:

```javascript
// Husk: use express::express;
const express = require("express");

// Husk: use lodash::{debounce, capitalize};
const { debounce, capitalize } = require("lodash");

// Husk: use axios::axios; (ESM package)
import { axios } from "axios";
```

## Workflow

### 1. Define Dependencies

Edit your `husk.toml`:

```toml
[dependencies]
express = "^4.18.0"
lodash = "^4.17.21"
```

### 2. Generate package.json

```bash
husk build --generate-package-json
```

This creates a `package.json` with:
- Dependencies from `husk.toml`
- Appropriate module type (`"type": "module"` for ESM)
- Build scripts

### 3. Install Dependencies

```bash
npm install
```

### 4. Build Project

```bash
husk build
```

## Current Status and Limitations

### What Works

✅ **Dependency Declaration**: Full support for various dependency types in `husk.toml`  
✅ **Package Resolution**: Resolves packages from `node_modules/`  
✅ **Import Generation**: Converts Husk `use` statements to JavaScript imports  
✅ **CommonJS Support**: Handles CommonJS packages correctly  
✅ **Named Imports**: Supports importing specific functions/objects  
✅ **Local Dependencies**: Supports path-based dependencies  

### Current Limitations

⚠️ **Mixed Import Types**: Generated code may mix `require()` and `import` statements  
⚠️ **Subpath Resolution**: Some complex package exports may not resolve correctly  
⚠️ **Type Information**: Limited TypeScript definition support  
⚠️ **Build Targets**: Package resolution doesn't vary by target platform  

### Error Handling

Common errors and solutions:

**"Package 'xxx' not found in dependencies"**
- Add the package to `[dependencies]` in `husk.toml`

**"Package 'xxx' not found in node_modules"**
- Run `npm install` after updating `husk.toml`

**"Package subpath './xxx' is not defined"**
- Check the package's `package.json` exports field
- Use the correct import path according to package documentation

## Advanced Configuration

### Build Targets

Configure different import strategies for different platforms:

```toml
[targets.node]
platform = "node"
format = "cjs"
external = ["fs", "path", "os"]

[targets.browser]
platform = "browser"
format = "esm"
external = []
```

### External Dependencies

Mark packages as external to avoid bundling:

```toml
[targets.browser]
external = ["react", "react-dom"]
```

## Best Practices

1. **Pin Versions**: Use specific version ranges in production
2. **Separate Dev Dependencies**: Use `[dev-dependencies]` for build tools
3. **Check Compatibility**: Verify npm packages work with your target platform
4. **Generate Package.json**: Always generate `package.json` for npm compatibility
5. **Test Imports**: Verify generated JavaScript runs correctly

## Examples

### Express Web Server

```husk
use express::{express, json, urlencoded};

fn main() {
    let app = express();
    
    app.use(json());
    app.use(urlencoded({ extended: true }));
    
    app.get("/", |req, res| {
        res.send("Hello from Husk!");
    });
    
    app.listen(3000, || {
        println!("Server running on port 3000");
    });
}
```

### Utility Functions with Lodash

```husk
use lodash::{debounce, throttle, map, filter};

fn process_data(data: Array<i32>) -> Array<i32> {
    data
        |> filter(|x| x > 0)
        |> map(|x| x * 2)
}

fn setup_event_handlers() {
    let debouncedSave = debounce(save_document, 500);
    let throttledScroll = throttle(handle_scroll, 100);
}
```

### HTTP Client with Axios

```husk
use axios::axios;

async fn fetch_user(id: i32) -> Result<User, String> {
    let response = await axios.get(`/api/users/${id}`)?;
    Ok(response.data)
}
```

This system provides a robust foundation for integrating npm packages into Husk projects while maintaining type safety and clear dependency management.