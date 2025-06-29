# TypeScript .d.ts to Husk Extern Converter Plan

## Overview

This document outlines the plan for implementing a new command in Husk that converts TypeScript declaration files (.d.ts) to Husk extern declarations.

## Recommended TypeScript Parser

**SWC (swc_ecma_parser)** - Chosen for:
- Mature, well-maintained, and battle-tested
- Used by major projects (Next.js, Deno)
- Excellent TypeScript/.d.ts support
- Fast performance
- Good documentation

Alternative: **oxc_parser** for maximum performance

## TypeScript to Husk Mapping Rules

### Basic Conversions
- `declare function name(): Type` → `extern fn name() -> Type`
- `interface Name` → `extern mod { type Name; }`
- Interface methods → `impl Name { fn method(...) }`
- `this` return type → No explicit return (uses `self`)
- Optional parameters → Removed or made required
- Generic types → Simplified to concrete types

### Type Mappings
- `string` → `string`
- `number` → `i32` or `f64`
- `boolean` → `bool`
- `any` → `any`
- `void` → no return type
- `T[]` → simplified representation
- Complex objects → extern types

## CLI Command Design

```bash
# Basic usage
husk dts2extern <input.d.ts> [options]

# Options
--output, -o <file>     # Output file (default: stdout)
--module, -m <name>     # Module name (default: inferred from filename)
--simplify              # Simplify complex types to 'any'
--filter <pattern>      # Only convert matching exports
```

## Architecture

```
src/
├── commands/
│   └── dts2extern.rs     # New CLI command
├── dts/
│   ├── mod.rs           # Module root
│   ├── parser.rs        # SWC parser wrapper
│   ├── converter.rs     # AST to Husk conversion
│   └── types.rs         # Type mapping utilities
```

## Implementation Steps

1. **Add SWC dependency** to Cargo.toml
   ```toml
   swc_ecma_parser = "0.143"
   swc_ecma_ast = "0.112"
   swc_common = "0.33"
   ```

2. **Create command structure** in `commands/dts2extern.rs`
   - Parse CLI arguments
   - Load .d.ts file
   - Call converter
   - Output results

3. **Implement parser wrapper** (`dts/parser.rs`)
   - Initialize SWC parser for TypeScript
   - Parse .d.ts file into AST
   - Handle parse errors

4. **Build AST visitor** (`dts/converter.rs`)
   - Traverse TypeScript AST
   - Extract declarations
   - Build Husk extern AST

5. **Create type conversion** (`dts/types.rs`)
   - Map TypeScript types to Husk
   - Handle complex type simplification

6. **Add CLI integration** in `main.rs`
   - Register new command
   - Handle command routing

## Example Conversion

### Input (Express .d.ts simplified):
```typescript
declare function express(): Express;

interface Express extends Application {
    request: Request;
    response: Response;
}

interface Application {
    listen(port: number, callback?: () => void): Server;
    get(path: string, ...handlers: RequestHandler[]): this;
    post(path: string, ...handlers: RequestHandler[]): this;
    use(...handlers: RequestHandler[]): this;
}

interface Request {
    body: any;
    params: any;
    query: any;
    method: string;
    path: string;
}

interface Response {
    send(body?: any): this;
    json(obj: any): this;
    status(code: number): this;
    redirect(url: string): void;
}
```

### Output (Husk extern):
```husk
extern fn express() -> express::Express;

extern mod express {
    type Express;
    type Application;
    type Server;
    type Request;
    type Response;
    type RequestHandler;
    
    impl Express {
        fn request(self) -> Request;
        fn response(self) -> Response;
    }
    
    impl Application {
        fn listen(self, port: i32) -> Server;
        fn get(self, path: string, handler: RequestHandler);
        fn post(self, path: string, handler: RequestHandler);
        fn use(self, handler: RequestHandler);
    }
    
    impl Request {
        fn body(self) -> any;
        fn params(self) -> any;
        fn query(self) -> any;
        fn method(self) -> string;
        fn path(self) -> string;
    }
    
    impl Response {
        fn send(self, body: any);
        fn json(self, obj: any);
        fn status(self, code: i32);
        fn redirect(self, url: string);
    }
}
```

## Challenges & Solutions

1. **Complex Generics**
   - Solution: Simplify to `any` or most common concrete type
   - Future: Add generic support to Husk

2. **Function Overloads**
   - Solution: Generate multiple extern functions with suffixes
   - Or: Take the most general signature

3. **Namespace Nesting**
   - Solution: Flatten into module hierarchy
   - Use dot notation for deeply nested types

4. **Optional Parameters**
   - Solution: Generate versions without optional params
   - Or: Make all parameters required

5. **Union Types**
   - Solution: Default to `any`
   - Future: Add union type support to Husk

6. **Variadic Parameters**
   - Solution: Convert to single array parameter
   - Or: Generate fixed-arity versions

## Testing Strategy

1. Unit tests for each conversion type
2. Integration tests with real .d.ts files
3. Test popular libraries: Express, React, Node.js core
4. Verify generated code compiles in Husk

## Future Enhancements

1. Support for generic type parameters
2. Preserve JSDoc comments
3. Handle module augmentation
4. Support for conditional types
5. Interactive mode for handling ambiguities