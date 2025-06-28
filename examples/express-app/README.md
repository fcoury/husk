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

While the import syntax works and generates correct JavaScript, there's currently a limitation with Husk's semantic analyzer:

1. **Method calls on external packages**: The semantic analyzer treats imported npm packages as having an "Unknown" type, and doesn't allow method calls on Unknown types. This means code like `express().get("/", handler)` will fail semantic analysis.

2. **Workaround**: The transpiler still generates correct JavaScript, but you'll get a semantic error during the build process if you try to call methods on external packages.

## Example Code

See `src/main.husk` for a working example that demonstrates:
- How to import a package with a default export
- The current limitation with method calls

## Future Improvements

To fully support external npm packages, Husk would need one of the following:

1. **Type definitions**: A way to declare external module types (similar to TypeScript's `.d.ts` files)
2. **Relaxed type checking**: Allow method calls on Unknown types for external packages
3. **Type inference**: Infer types from usage or package metadata

## Building

```bash
husk build
```

This will transpile all Husk files to JavaScript in the `dist/` directory.