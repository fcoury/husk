# Husk npm Demo

A comprehensive demonstration of Husk language features integrated with real npm packages.

## Features Demonstrated

- **Multi-file modules**: `use crate::types::User`
- **Structs**: `User` with multiple fields
- **Enums**: `ValidationError` with multiple variants
- **Pattern matching**: On `Result` and custom enums
- **Extern FFI**: Declaring and calling JavaScript functions
- **String operations**: Concatenation with `+`

## npm Packages Used

- **chalk** (v4.x): Terminal colors and styling
- **nanoid** (v3.x): Unique ID generation
- **validator** (v13.x): String validation (email, alpha, length)

## Quick Start

```bash
# From the repository root

# 1. Install npm dependencies
cd examples/demo_npm
npm install
cd ../..

# 2. Build the Husk compiler (if not already built)
cargo build

# 3. Compile the Husk code to JavaScript
cargo run --bin huskc -- compile --lib examples/demo_npm/main.hk > examples/demo_npm/main.js

# 4. Run the demo
node examples/demo_npm/host.js
```

## Expected Output

```
=== Husk npm Demo ===

Creating user: Alice, alice@example.com
User created successfully!
  ID:    V1StGXR8_Z5jdHi6B-myT
  Name:  Alice
  Email: alice@example.com

Creating user: Bob, not-an-email
Error: Invalid email address

Creating user: Alice123, alice@test.com
Error: Name must contain only letters

Creating user: (empty), test@test.com
Error: Field cannot be empty

=== Demo Complete ===
```

(IDs will vary as they are randomly generated)

## File Structure

```
demo_npm/
├── main.hk       # Main application with validation logic
├── types.hk      # Type definitions (User, ValidationError)
├── host.js       # Node.js host that wires npm packages
├── main.js       # Compiled output (generated)
├── package.json  # npm dependencies
└── README.md     # This file
```

## How It Works

1. **types.hk** defines the data types used by the application
2. **main.hk** imports types and declares extern functions for npm packages
3. **host.js** loads the npm packages and exposes them via `globalThis`
4. The Husk compiler generates JavaScript that calls these `globalThis` functions
5. **host.js** loads the compiled code and runs `main()`

## Architecture Notes

This demo uses the "host pattern" where a JavaScript file:
1. Requires npm packages
2. Exposes functions via `globalThis`
3. Loads and runs the compiled Husk code

This pattern is used because Husk's direct module import syntax (`extern "js" { mod name; }`)
generates imports but the current runtime still needs the host to wire things up properly
for complex package APIs.

Future versions of Husk may simplify this to just `node main.js`.
