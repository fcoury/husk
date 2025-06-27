# Built-in Methods Implementation Plan

## Overview

This document outlines the implementation strategy for adding built-in methods to primitive types (strings and arrays) in Husk, ensuring they work correctly in both interpreter and transpiler modes.

## Core Design Principles

1. **JavaScript Parity**: Method names and behaviors should match JavaScript where possible
2. **Dual Mode Support**: Every method must work in both interpreter and transpiler
3. **Type Safety**: Methods should be properly type-checked at compile time
4. **Minimal Breaking Changes**: Extend existing systems rather than replacing them

## Implementation Strategy

### 1. Parser Modifications

Currently, the parser only allows method calls on struct instances. We need to extend it to recognize methods on primitives.

#### Current State
```rust
// In parse_postfix_expression
if self.current_token().kind == TokenKind::Dot {
    // Only works if expr is Identifier (struct instance)
}
```

#### Proposed Change
```rust
// In parse_postfix_expression
if self.current_token().kind == TokenKind::Dot {
    self.advance(); // consume '.'
    let method_name = self.parse_identifier()?;
    
    if self.current_token().kind == TokenKind::LParen {
        // Method call
        let args = self.parse_call_arguments()?;
        expr = Expr::MethodCall(Box::new(expr), method_name, args, span);
    }
    // Continue with property access...
}
```

### 2. Type System Extensions

Add built-in method signatures to the semantic analyzer.

```rust
// In semantic.rs
struct BuiltinMethod {
    name: &'static str,
    param_types: Vec<Type>,
    return_type: Type,
}

const STRING_METHODS: &[BuiltinMethod] = &[
    BuiltinMethod { name: "len", param_types: vec![], return_type: Type::Int },
    BuiltinMethod { name: "trim", param_types: vec![], return_type: Type::String },
    BuiltinMethod { name: "substring", param_types: vec![Type::Int, Type::Int], return_type: Type::String },
    // ... more methods
];

const ARRAY_METHODS: &[BuiltinMethod] = &[
    BuiltinMethod { name: "len", param_types: vec![], return_type: Type::Int },
    BuiltinMethod { name: "push", param_types: vec![Type::Generic("T")], return_type: Type::Void },
    // ... more methods
];
```

### 3. Semantic Analyzer Updates

Modify the type checker to handle method calls on primitives:

```rust
// In visit_method_call
fn visit_method_call(&mut self, object: &Expr, method: &str, args: &[Expr], span: &Span) -> Result<Type> {
    let object_type = self.visit_expr(object)?;
    
    match &object_type {
        Type::String => {
            // Check string methods
            if let Some(method_info) = STRING_METHODS.iter().find(|m| m.name == method) {
                // Validate arguments and return type
                return Ok(method_info.return_type.clone());
            }
        }
        Type::Array(elem_type) => {
            // Check array methods
            if let Some(method_info) = ARRAY_METHODS.iter().find(|m| m.name == method) {
                // Handle generic type substitution
                return Ok(method_info.return_type.clone());
            }
        }
        Type::Struct(name) => {
            // Existing struct method handling
        }
        _ => {
            return Err(Error::new_semantic(
                format!("{} is not a struct instance or built-in type with methods", object_name),
                *span,
            ));
        }
    }
}
```

### 4. Interpreter Implementation

Add built-in method execution:

```rust
// In interpreter.rs
fn visit_method_call(&mut self, object: &Expr, method: &str, args: &[Expr], _span: &Span) -> Result<Value> {
    let obj_value = self.visit_expr(object)?;
    
    match &obj_value {
        Value::String(s) => {
            match method {
                "len" => Ok(Value::Int(s.len() as i64)),
                "trim" => Ok(Value::String(s.trim().to_string())),
                "substring" => {
                    let start = self.get_int_arg(&args[0])?;
                    let end = self.get_int_arg(&args[1])?;
                    Ok(Value::String(s.chars().skip(start as usize).take((end - start) as usize).collect()))
                }
                "split" => {
                    let delimiter = self.get_string_arg(&args[0])?;
                    let parts: Vec<Value> = s.split(&delimiter)
                        .map(|s| Value::String(s.to_string()))
                        .collect();
                    Ok(Value::Array(parts))
                }
                // ... more string methods
                _ => Err(Error::new_runtime(format!("Unknown string method: {}", method), *_span))
            }
        }
        Value::Array(arr) => {
            match method {
                "len" => Ok(Value::Int(arr.len() as i64)),
                "push" => {
                    // Note: This requires making arrays mutable
                    let value = self.visit_expr(&args[0])?;
                    arr.push(value);
                    Ok(Value::Void)
                }
                "pop" => {
                    if let Some(value) = arr.pop() {
                        Ok(Value::Some(Box::new(value)))
                    } else {
                        Ok(Value::None)
                    }
                }
                // ... more array methods
                _ => Err(Error::new_runtime(format!("Unknown array method: {}", method), *_span))
            }
        }
        // Existing struct method handling...
    }
}
```

### 5. Transpiler Implementation

Generate appropriate JavaScript method calls:

```rust
// In transpiler.rs
fn visit_method_call(&mut self, object: &Expr, method: &str, args: &[Expr], _span: &Span) -> Result<String> {
    let obj_str = self.visit_expr(object)?;
    
    // Check if it's a built-in method that needs special handling
    match method {
        // String methods
        "len" if is_string_type(object) => {
            // JavaScript uses .length property, not .len() method
            Ok(format!("{}.length", obj_str))
        }
        "substring" => {
            let args_str = self.generate_args(args)?;
            Ok(format!("{}.substring({})", obj_str, args_str))
        }
        
        // Array methods that need special handling
        "len" if is_array_type(object) => {
            Ok(format!("{}.length", obj_str))
        }
        "push" => {
            let args_str = self.generate_args(args)?;
            Ok(format!("void ({}.push({}))", obj_str, args_str))
        }
        
        // Direct method mapping
        _ => {
            let args_str = self.generate_args(args)?;
            Ok(format!("{}.{}({})", obj_str, method, args_str))
        }
    }
}
```

## Minimum JavaScript Parity Methods

### String Methods (Phase 1)
```rust
impl string {
    fn len() -> int;               // JS: .length (property)
    fn charAt(index: int) -> string;
    fn substring(start: int, end: int) -> string;
    fn indexOf(search: string) -> int;
    fn split(delimiter: string) -> array<string>;
    fn trim() -> string;
    fn toLowerCase() -> string;
    fn toUpperCase() -> string;
    fn replace(from: string, to: string) -> string;
    fn startsWith(prefix: string) -> bool;
    fn endsWith(suffix: string) -> bool;
    fn includes(substr: string) -> bool;
}
```

### Array Methods (Phase 1)
```rust
impl<T> array<T> {
    fn len() -> int;               // JS: .length (property)
    fn push(item: T) -> void;
    fn pop() -> Option<T>;
    fn shift() -> Option<T>;
    fn unshift(item: T) -> void;
    fn indexOf(item: T) -> int;
    fn includes(item: T) -> bool;
    fn slice(start: int, end: int) -> array<T>;
    fn concat(other: array<T>) -> array<T>;
    fn join(separator: string) -> string;
    
    // Higher-order methods (Phase 2)
    fn map<U>(f: fn(T) -> U) -> array<U>;
    fn filter(f: fn(T) -> bool) -> array<T>;
    fn reduce<U>(f: fn(U, T) -> U, initial: U) -> U;
    fn find(f: fn(T) -> bool) -> Option<T>;
    fn findIndex(f: fn(T) -> bool) -> int;
    fn every(f: fn(T) -> bool) -> bool;
    fn some(f: fn(T) -> bool) -> bool;
}
```

## Implementation Phases

### Phase 1: Core Methods (1-2 weeks)
1. Parser changes to allow method calls on primitives
2. Basic string methods (len, substring, split, trim)
3. Basic array methods (len, push, pop, indexOf)
4. Type checking for built-in methods
5. Interpreter implementation
6. Transpiler implementation

### Phase 2: Extended Methods (1 week)
1. Additional string methods (replace, includes, etc.)
2. Array higher-order functions (map, filter, reduce)
3. Method chaining support
4. Performance optimizations

### Phase 3: Advanced Features (1 week)
1. Property access for .length
2. Mutability handling for array methods
3. Generic type inference for array methods
4. Error handling for out-of-bounds access

## Testing Strategy

### Unit Tests
```rust
// String method tests
fn test_string_methods() {
    let s = "  hello world  ";
    assert_eq(s.len(), 15);
    assert_eq(s.trim(), "hello world");
    assert_eq(s.substring(2, 7), "hello");
    
    let parts = "a,b,c".split(",");
    assert_eq(parts.len(), 3);
    assert_eq(parts[0], "a");
}

// Array method tests
fn test_array_methods() {
    let arr = [1, 2, 3];
    assert_eq(arr.len(), 3);
    
    arr.push(4);
    assert_eq(arr.len(), 4);
    
    let doubled = arr.map(|x| x * 2);
    assert_eq(doubled, [2, 4, 6, 8]);
}
```

### Integration Tests
- Test method calls in complex expressions
- Test method chaining
- Test generic type inference
- Test error cases

## Backwards Compatibility

- No breaking changes to existing code
- Method calls on primitives are new syntax
- Existing struct methods continue to work

## Alternative Considerations

### 1. Module-based Approach
Instead of methods, use module functions:
```rust
use std::string;
let len = string::len(myStr);
```
**Pros**: Simpler implementation
**Cons**: Less JavaScript-like, verbose

### 2. Extension Methods
Allow defining methods on existing types:
```rust
impl string {
    fn custom_method(self) -> string {
        // ...
    }
}
```
**Pros**: Extensible
**Cons**: More complex implementation

### 3. Macro-based Solution
Use macros to generate method calls:
```rust
let len = len!(myStr);
```
**Pros**: No parser changes
**Cons**: Not JavaScript-like, requires macro system

## Recommendation

Implement built-in methods directly in the language (Option 1) because:
1. Most JavaScript-like syntax
2. Best developer experience
3. Type safety at compile time
4. Clear implementation path
5. Works in both interpreter and transpiler

## Next Steps

1. Create proof-of-concept with string.len() method
2. Extend parser to handle method calls on primitives
3. Add type checking for built-in methods
4. Implement in interpreter
5. Implement in transpiler
6. Add comprehensive tests
7. Extend to full method set