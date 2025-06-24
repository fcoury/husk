# Husk Project TODO List

This document tracks the current development tasks for the Husk programming language, organized by priority and completion status.

## High Priority Tasks (Completed ✅)

### Core Language Features
- ✅ Add Use and Pub tokens to lexer
- ✅ Parse use statements with local::, self::, super:: prefixes
- ✅ Parse pub keyword for exports
- ✅ Convert use statements to JS imports in transpiler
- ✅ Implement extern keyword for external type declarations
- ✅ Add Extern token to lexer
- ✅ Parse extern fn and extern mod declarations
- ✅ Add AST nodes for extern declarations
- ✅ Type check extern declarations

### Async/Await Support
- ✅ Add async/await syntax parsing (async functions, await expressions)
- ✅ Add Async token to lexer
- ✅ Parse async fn declarations
- ✅ Implement .await postfix syntax
- ✅ Add AsyncFunction AST node
- ✅ Add Expr::Await variant
- ✅ Update visitor trait for async/await
- ✅ Type check async functions and await expressions
- ✅ Return error for async in interpreter mode
- ✅ Implement async/await transformation in transpiler
- ✅ Add TypeAnnotation::Promise for Promise type integration

### Module System
- ✅ Implement module loading and caching for local files in interpreter
- ✅ Implement path resolution for local::, self::, super::
- ✅ Collect and track pub exports from modules

### Type System Enhancements
- ✅ Add match expression support (in addition to match statement)
- ✅ Add basic generic type parsing for extern declarations (Promise<T>)
- ✅ Implement Option and Result as built-in types
- ✅ Add ? operator for Result error propagation
- ✅ Wrap JS Promise.catch errors in Result::Err automatically
- ✅ Implement generic type parameters (Container<T>, fn map<T,U>)
- ✅ Fix all compilation errors from generic type parameter changes
- ✅ Allow using extern types in function signatures
- ✅ Implement type casting (as operator)
- ✅ Add support for extern type declarations syntax (extern type Buffer;)
- ✅ Fix unit type parsing in async function return types (Result<(), string>)
- ✅ Implement struct destructuring in match patterns with field renaming
- ✅ Add support for struct-like enum variants in enum definitions
- ✅ Add support for struct-like enum variant construction (Command::Process { ... })

### Built-in Methods
- ✅ Implement built-in methods for strings (.len(), .trim(), etc)
- ✅ Extend parser to allow method calls on primitive types
- ✅ Add built-in method signatures to type system
- ✅ Implement string methods in interpreter
- ✅ Implement string methods in transpiler
- ✅ Implement built-in methods for arrays (.len(), .push(), etc)

### Build System
- ✅ Implement build command for project compilation

### Testing Infrastructure
- ✅ Write comprehensive parser tests for new syntax features
- ✅ Test module loading and caching in interpreter
- ✅ Add semantic tests for type casting validation
- ✅ Add interpreter tests for type casting runtime behavior
- ✅ Add transpiler tests for type casting JS output
- ✅ Add semantic tests for built-in method type checking
- ✅ Add interpreter tests for built-in method runtime
- ✅ Add semantic tests for qualified type name resolution
- ✅ Add semantic tests for extern type validation
- ✅ Add semantic tests for async context validation
- ✅ Add transpiler tests for ES6 module generation
- ✅ Create dedicated semantic_js_interop_tests.rs module
- ✅ Create dedicated transpiler_js_interop_tests.rs module
- ✅ Create dedicated interpreter_js_interop_tests.rs module
- ✅ Validate transpiler output for all target modes

### Documentation
- ✅ Update documentation with implementation progress
- ✅ Update documentation with module loading progress
- ✅ Update documentation with export collection and commit progress
- ✅ Update documentation with build system implementation

### String Processing
- ✅ Implement template literal parsing and transpilation
- ✅ Implement format! macro for string formatting

### Examples
- ✅ Create example using local module system

## Medium Priority Tasks

### In Progress 🚧
- 🚧 Fix pattern parsing to correctly handle struct patterns vs expression parsing
  - Fixed match block parsing to properly consume braces
  - Investigating issue where `match variable` fails but `match literal` works with enum patterns
- 🚧 Build example CLI tool using Node.js APIs

### Pending 📋
- 📋 Show clear error for external packages in interpreter mode
- 📋 Add Expr::Closure variant and parse lambda/closure syntax
- 📋 Add error mapping helper for JS exceptions
- 📋 Fix lexer UTF-8 character handling for emojis
- 📋 Support typed global variables
- 📋 Implement runtime type checking (is operator)
- 📋 Add property existence checking
- 📋 **Add else if syntax support to parser** (discovered during testing)
- 📋 Add transpiler tests for built-in method JS output
- 📋 Add integration tests for multi-file module projects
- 📋 Add error case tests for invalid type casts
- 📋 Add error case tests for undefined built-in methods
- 📋 Add error case tests for circular module dependencies
- 📋 Create Express web server example in Husk

## Low Priority Tasks

### Future Enhancements 🔮
- 🔮 Future: Add async support to interpreter mode
- 🔮 Add spread operator for arrays and structs
- 🔮 Generate TypeScript .d.ts declarations from Husk modules
- 🔮 Implement source map generation for debugging
- 🔮 Add tree shaking support for dead code elimination
- 🔮 Create webpack/vite plugin for bundler integration
- 🔮 Add JSX syntax parsing (optional feature)
- 🔮 Implement JSX to JavaScript transformation
- 🔮 Create React type definitions (react.d.hk)
- 🔮 Build React todo app example (if JSX implemented)

## Discovered Issues

### Language Syntax Limitations
1. **else if syntax**: Currently requires nested if statements, discovered during example testing
   - Current workaround: Use nested if-else blocks
   - Priority: Medium (affects code readability)
   - Location: Parser needs enhancement to support `else if` chains

### Module System Limitations
1. **Local module imports**: Full module system not working in interpreter mode
   - Affects: Complex examples with multiple file imports
   - Workaround: Test individual features in simple examples
   - Priority: Medium (affects comprehensive testing)

## Testing Status

### Validated Features ✅
- Type casting with `as` operator (int, float, string, bool)
- String built-in methods (.len(), .trim(), .toUpperCase(), .toLowerCase())
- Array built-in methods (.len())
- Template literals with format! macro
- Nested if statements (workaround for else if)
- Basic transpiler output for all core JavaScript interop features

### Testing Approach
- Simple examples for individual feature validation
- Comprehensive test suites for each component (semantic, interpreter, transpiler)
- Target mode validation for different JavaScript environments
- Integration testing with realistic multi-file examples

## Progress Summary

**Completed**: 70+ tasks across core language features, type system, build tools, and testing infrastructure

**In Progress**: CLI tool example with Node.js API integration

**Next Priority**: Complete remaining medium-priority tasks and address discovered language syntax limitations

---

*Last Updated: 2024-06-24*
*Generated from active todo list during struct destructuring and enum variant implementation*