# Missing Features in TypeScript .d.ts to Husk Converter

Based on analysis of the official Express .d.ts file, here are the missing features that need to be implemented:

## 1. Import Statement Handling

**Current**: Imports are detected but ignored
**Need**: Process imports to understand external type references

```typescript
import * as core from "express-serve-static-core";
import * as bodyParser from "body-parser";
```

**Options**:
- Follow imports and parse referenced .d.ts files
- Allow user to provide type mappings for common imports
- Generate stub types for imported references

## 2. Namespace Declarations

**Current**: Namespaces are detected but their contents aren't processed
**Need**: Process namespace body and generate appropriate Husk modules

```typescript
declare namespace e {
    var json: typeof bodyParser.json;
    export function Router(options?: RouterOptions): core.Router;
}
```

## 3. Extended Interfaces

**Current**: Interface extensions are ignored
**Need**: Either inline inherited members or maintain inheritance chain

```typescript
interface Application extends core.Application {}
interface Request<P = core.ParamsDictionary> extends core.Request<P> {}
```

## 4. Generic Type Parameters

**Current**: Generics are completely ignored
**Need**: Basic generic support or simplification strategy

```typescript
interface Request<
    P = core.ParamsDictionary,
    ResBody = any,
    ReqBody = any
> extends core.Request<P, ResBody, ReqBody> {}
```

## 5. Type References with Qualifiers

**Current**: Only simple type references work
**Need**: Handle qualified type references like `core.Express`

```typescript
declare function e(): core.Express;
var static: serveStatic.RequestHandlerConstructor<Response>;
```

## 6. Typeof Type Operators

**Current**: Not handled
**Need**: Convert or simplify typeof expressions

```typescript
var json: typeof bodyParser.json;
```

## 7. Variable Declarations in Namespaces

**Current**: Var declarations are ignored
**Need**: Convert to appropriate Husk constructs

```typescript
declare namespace e {
    var application: Application;
    var request: Request;
}
```

## 8. Export Statements

**Current**: Export modifiers are ignored
**Need**: Track exported vs internal declarations

```typescript
export function Router(options?: RouterOptions): core.Router;
```

## 9. Optional Parameters and Properties

**Current**: Optional modifiers are stripped
**Need**: Strategy for handling optionality

```typescript
interface RouterOptions {
    caseSensitive?: boolean | undefined;
}
```

## 10. Union Types

**Current**: Simplified to 'any'
**Need**: Better union type handling

```typescript
caseSensitive?: boolean | undefined;
```

## Implementation Priority

### High Priority (Required for basic Express support)
1. Import statement handling - at least stub generation
2. Namespace body processing
3. Type references with module qualifiers

### Medium Priority
4. Extended interfaces - inline inherited members
5. Export tracking
6. Variable declarations in namespaces

### Low Priority (Can be simplified)
7. Generic type parameters
8. Typeof operators
9. Complex union types
10. Optional parameter handling

## Recommended Approach

1. **Phase 1**: Add namespace body processing and variable declarations
2. **Phase 2**: Implement basic import handling with stub generation
3. **Phase 3**: Handle qualified type references (module.Type)
4. **Phase 4**: Add interface extension support