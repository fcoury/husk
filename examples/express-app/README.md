# Express App Example

This example demonstrates how to import npm packages with default exports in Husk.

## Default Export Syntax

Husk now supports importing default exports from npm packages using the `::default` syntax:

```husk
// Import the default export from express
use external::express::default;

// This generates: import express from 'express';
```

## Current Limitations

While the import syntax works and generates correct JavaScript, there are limitations with Husk's type system:

1. **Method calls on external packages**: The semantic analyzer treats imported npm packages as having an "Unknown" type, and doesn't allow method calls on Unknown types. This means code like `express().get("/", handler)` will fail semantic analysis.

2. **Extern declarations**: While Husk supports `extern mod` for declaring external types, it currently doesn't support the `self` parameter in method declarations. This prevents proper type declarations for object-oriented APIs. See [EXTERN_LIMITATIONS.md](./EXTERN_LIMITATIONS.md) for details.

## Workarounds

Currently, there are two approaches:

### Option 1: Accept the limitation
```husk
use external::express::default;

fn main() {
    // This works in the transpiler but fails semantic analysis
    let app = express();
    // Cannot call methods on unknown type
}
```

### Option 2: Use extern declarations (partial support)
```husk
extern mod express {
    type Application;
    // Methods cannot use 'self' parameter currently
}
```

## Example Code

See `src/main.husk` for a working example that demonstrates:
- How to import a package with a default export
- The current limitations with method calls and extern declarations

## Future Improvements

To fully support external npm packages, Husk needs:

1. **Self parameter support**: Allow `self` in extern method declarations
2. **Type definitions**: A way to declare external module types (similar to TypeScript's `.d.ts` files)
3. **Relaxed type checking**: Allow method calls on Unknown types for external packages
4. **Type inference**: Infer types from usage or package metadata

## Building

```bash
husk build
```

This will transpile all Husk files to JavaScript in the `dist/` directory. Note that you may see semantic errors due to the limitations described above, but the transpiler will still generate correct JavaScript code.