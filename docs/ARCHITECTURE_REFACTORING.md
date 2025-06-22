# Husk Architecture Refactoring Plan

## Executive Summary

This document outlines a comprehensive plan to refactor the Husk language implementation to address critical architectural issues and prepare it for production use. The refactoring is organized into 5 phases, each building upon the previous one, with clear deliverables and success metrics.

### Key Issues Identified

1. **Type System**: String-based type representation causing errors and limiting extensibility
2. **Performance**: O(n) lexer operations and excessive environment cloning
3. **Maintainability**: Tight coupling, missing abstractions, and code duplication
4. **Robustness**: No error recovery, limited error context, and poor diagnostics
5. **Extensibility**: Hard-coded features, no plugin system, and difficult to add backends

## Phase 1: Core Infrastructure (Estimated: 4-6 weeks)

### Goals
Establish fundamental abstractions and patterns that will support all future development.

### 1.1 Type System Overhaul

**Current Issue**: Types are represented as strings throughout the codebase
```rust
// Current (BAD)
if var_type == "int" { ... }
```

**Solution**: Implement a proper type system
```rust
// New type system
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Int,
    Float,
    Bool,
    String,
    Array(Box<Type>),
    Function { params: Vec<Type>, return_type: Box<Type> },
    Struct { name: String, fields: HashMap<String, Type> },
    Enum { name: String, variants: HashMap<String, Option<Type>> },
    Generic { name: String, constraints: Vec<TypeConstraint> },
}

pub struct TypeChecker {
    environment: TypeEnvironment,
}

impl TypeChecker {
    pub fn infer(&mut self, expr: &Expr) -> Result<Type, TypeError> {
        // Type inference logic
    }
    
    pub fn unify(&self, t1: &Type, t2: &Type) -> Result<Type, TypeError> {
        // Type unification for inference
    }
}
```

**Tasks**:
- [ ] Create `src/types/mod.rs` with Type enum and related types
- [ ] Implement TypeEnvironment for managing type bindings
- [ ] Create TypeChecker with inference and checking methods
- [ ] Update AST nodes to carry type information
- [ ] Migrate all string-based type checks to use new system

### 1.2 AST Visitor Pattern

**Current Issue**: Manual AST traversal in multiple places with duplicated logic

**Solution**: Implement visitor pattern
```rust
// Visitor trait
pub trait AstVisitor<T> {
    type Error;
    
    fn visit_expr(&mut self, expr: &Expr) -> Result<T, Self::Error> {
        match expr {
            Expr::Int(n) => self.visit_int(*n),
            Expr::BinaryOp { left, op, right } => {
                self.visit_binary_op(left, op, right)
            }
            // ... other variants
        }
    }
    
    fn visit_int(&mut self, value: i64) -> Result<T, Self::Error>;
    fn visit_binary_op(&mut self, left: &Expr, op: &BinaryOp, right: &Expr) -> Result<T, Self::Error>;
    // ... other visit methods
}

// Example implementation
struct TypeInferenceVisitor { /* ... */ }
impl AstVisitor<Type> for TypeInferenceVisitor { /* ... */ }
```

**Tasks**:
- [ ] Create `src/ast/visitor.rs` with visitor traits
- [ ] Implement default traversal methods
- [ ] Refactor semantic analyzer to use visitor pattern
- [ ] Refactor interpreter to use visitor pattern
- [ ] Refactor transpiler to use visitor pattern

### 1.3 Lexer Performance Optimization

**Current Issue**: O(n) character access with `chars().nth()`

**Solution**: Byte-based lexing with proper Unicode handling
```rust
pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    char_indices: Vec<(usize, char)>, // Pre-computed for efficiency
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let bytes = input.as_bytes().to_vec();
        let char_indices = input.char_indices().collect();
        Self { input: bytes, position: 0, char_indices }
    }
    
    fn current_char(&self) -> Option<char> {
        self.char_indices.get(self.position).map(|(_, ch)| *ch)
    }
}
```

**Tasks**:
- [ ] Rewrite lexer to use byte-based approach
- [ ] Add proper Unicode support with char boundary tracking
- [ ] Implement zero-copy token creation where possible
- [ ] Add lexer benchmarks to track performance
- [ ] Optimize keyword recognition with perfect hashing

### Testing & Validation
- Unit tests for each new component
- Property-based tests for type system invariants
- Performance benchmarks showing improvement
- All existing tests must pass

## Phase 2: Robustness & Reliability (Estimated: 3-4 weeks)

### 2.1 Error Architecture Redesign

**Current Issue**: Errors require spans, no recovery, poor diagnostics

**Solution**: Structured error system with recovery
```rust
#[derive(Debug)]
pub enum Diagnostic {
    Error(Error),
    Warning(Warning),
    Note(Note),
}

pub struct Error {
    code: ErrorCode,
    message: String,
    span: Option<Span>, // Now optional
    suggestions: Vec<Suggestion>,
    notes: Vec<Note>,
}

pub struct ParseError {
    expected: Vec<TokenType>,
    found: Token,
    recovery_point: usize,
}

// Parser with error recovery
impl Parser {
    fn parse_with_recovery(&mut self) -> (Option<AST>, Vec<Diagnostic>) {
        let mut diagnostics = vec![];
        // Implement panic mode recovery
    }
}
```

**Tasks**:
- [ ] Design new error type hierarchy
- [ ] Implement error recovery in parser
- [ ] Add structured suggestions system
- [ ] Create error code registry
- [ ] Implement diagnostic formatting with colors

### 2.2 Environment Abstraction

**Current Issue**: Environment cloning, no proper scoping

**Solution**: Efficient environment with proper abstractions
```rust
pub struct Environment<T> {
    scopes: Vec<Scope<T>>,
    global: Scope<T>,
}

pub struct Scope<T> {
    bindings: HashMap<String, T>,
    parent: Option<ScopeId>,
}

impl<T: Clone> Environment<T> {
    pub fn push_scope(&mut self) -> ScopeGuard {
        // Returns RAII guard for automatic scope cleanup
    }
    
    pub fn lookup(&self, name: &str) -> Option<&T> {
        // Efficient lookup through scope chain
    }
}
```

**Tasks**:
- [ ] Create environment abstraction module
- [ ] Implement efficient scope management
- [ ] Add RAII scope guards
- [ ] Migrate interpreter to use new environment
- [ ] Add environment visualization for debugging

### 2.3 Parser Architecture Improvements

**Current Issue**: Massive match statements, manual precedence handling

**Solution**: Table-driven parsing with precedence climbing
```rust
pub struct PrecedenceTable {
    operators: HashMap<TokenType, (Precedence, Associativity)>,
}

impl Parser {
    fn parse_expression(&mut self, min_precedence: Precedence) -> Result<Expr, ParseError> {
        // Precedence climbing algorithm
    }
}
```

**Tasks**:
- [ ] Implement precedence climbing algorithm
- [ ] Create operator precedence table
- [ ] Refactor expression parsing
- [ ] Add better error messages for precedence errors

## Phase 3: Performance & Optimization (Estimated: 4-5 weeks)

### 3.1 Intermediate Representation (IR)

**Current Issue**: Direct AST interpretation/transpilation limits optimization

**Solution**: Lower AST to optimizable IR
```rust
pub enum IR {
    Block(Vec<IRStatement>),
    Assign { target: IRValue, value: IRExpr },
    If { condition: IRExpr, then_block: Box<IR>, else_block: Option<Box<IR>> },
    // ... other IR nodes
}

pub struct IRBuilder {
    pub fn lower_ast(ast: &AST) -> Result<IR, LoweringError> {
        // AST -> IR transformation
    }
}
```

**Tasks**:
- [ ] Design IR representation
- [ ] Implement AST to IR lowering
- [ ] Create IR validator
- [ ] Update interpreter to execute IR
- [ ] Update transpiler to work with IR

### 3.2 Optimization Passes

**Solution**: Implement common optimizations
```rust
pub trait OptimizationPass {
    fn optimize(&mut self, ir: IR) -> Result<IR, OptimizationError>;
}

pub struct ConstantFolding;
pub struct DeadCodeElimination;
pub struct InlineExpansion;
```

**Tasks**:
- [ ] Implement constant folding
- [ ] Implement dead code elimination
- [ ] Implement function inlining
- [ ] Create optimization pipeline
- [ ] Add optimization level flags

### 3.3 Memory Management

**Solution**: Efficient value representation
```rust
pub enum Value {
    // Use small-value optimization
    Int(i64),
    Float(f64),
    Bool(bool),
    // Heap-allocated values use Rc
    String(Rc<String>),
    Array(Rc<RefCell<Vec<Value>>>),
    // ... other values
}
```

**Tasks**:
- [ ] Implement small-value optimization
- [ ] Add reference counting for heap values
- [ ] Implement value pooling for common values
- [ ] Add memory profiling tools

## Phase 4: Language Features (Estimated: 6-8 weeks)

### 4.1 Module System

**Solution**: Proper module and import system
```rust
// In Husk code
mod math {
    pub fn sqrt(x: float) -> float { /* ... */ }
}

use math::sqrt;
```

**Tasks**:
- [ ] Design module syntax and semantics
- [ ] Implement module resolution
- [ ] Add import/export mechanisms
- [ ] Create standard library modules
- [ ] Implement visibility rules

### 4.2 Advanced Type Features

**Tasks**:
- [ ] Add Option and Result types
- [ ] Implement basic generics
- [ ] Add type aliases
- [ ] Implement traits/interfaces
- [ ] Add pattern exhaustiveness checking

### 4.3 Standard Library Expansion

**Tasks**:
- [ ] Create collections module (Vec, HashMap)
- [ ] Add string manipulation functions
- [ ] Implement file I/O module
- [ ] Add math module
- [ ] Create testing framework

## Phase 5: Tooling & Ecosystem (Estimated: 8-10 weeks)

### 5.1 Language Server Protocol (LSP)

**Tasks**:
- [ ] Implement LSP server
- [ ] Add go-to-definition
- [ ] Implement auto-completion
- [ ] Add real-time error checking
- [ ] Create VS Code extension

### 5.2 Package Manager

**Tasks**:
- [ ] Design package format
- [ ] Implement dependency resolution
- [ ] Create package registry
- [ ] Add version management
- [ ] Implement build tool

### 5.3 Debugging Support

**Tasks**:
- [ ] Add source map generation
- [ ] Implement debug adapter protocol
- [ ] Add breakpoint support
- [ ] Create stack trace formatting
- [ ] Add REPL debugging commands

## Success Metrics

### Performance
- Lexer: 10x faster on large files
- Parser: 5x faster with error recovery
- Interpreter: 3x faster with IR and optimizations
- Memory usage: 50% reduction

### Code Quality
- 90%+ test coverage
- All components have unit tests
- Property-based tests for critical paths
- Zero string-based type comparisons

### Developer Experience
- Error messages with suggestions
- LSP support in major editors
- Comprehensive documentation
- Active community

## Migration Strategy

1. **Parallel Development**: New components developed alongside old ones
2. **Feature Flags**: Toggle between old and new implementations
3. **Gradual Migration**: One component at a time
4. **Compatibility Layer**: Maintain API compatibility during transition
5. **Extensive Testing**: Each migration step fully tested

## Timeline Overview

- **Phase 1**: Weeks 1-6 (Core Infrastructure)
- **Phase 2**: Weeks 7-10 (Robustness)
- **Phase 3**: Weeks 11-15 (Performance)
- **Phase 4**: Weeks 16-23 (Features)
- **Phase 5**: Weeks 24-33 (Tooling)

Total estimated time: 8-9 months for complete refactoring

## Getting Started

To begin the refactoring:

1. Create a `refactoring` branch
2. Set up feature flags for gradual migration
3. Start with Phase 1.1 (Type System)
4. Ensure all tests pass after each change
5. Document design decisions as you go

## Open Questions

1. Should we maintain backward compatibility during refactoring?
2. What's the priority order for optimization passes?
3. Should we consider WASM as a compilation target?
4. How much of the standard library should be written in Husk vs Rust?
5. What's the strategy for community involvement?

## References

- [Crafting Interpreters](https://craftinginterpreters.com/)
- [Engineering a Compiler](https://www.elsevier.com/books/engineering-a-compiler/cooper/978-0-12-088478-0)
- [Rust Compiler Development Guide](https://rustc-dev-guide.rust-lang.org/)
- [Language Server Protocol Specification](https://microsoft.github.io/language-server-protocol/)