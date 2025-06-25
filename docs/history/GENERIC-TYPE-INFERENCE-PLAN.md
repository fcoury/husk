# Full Generic Type Inference for Nested Patterns - Implementation Plan

## Overview

This document outlines the implementation plan for adding full generic type inference support for nested patterns in Husk. This is a significant undertaking that requires a major overhaul of the type system to properly handle complex nested patterns like `Ok(Some(Config { host, .. }))`.

## Current State Analysis

### Type System Limitations

The current type system in `src/types/mod.rs` has several limitations:

1. **Generic Type Placeholder**: The `Type::Generic` variant exists but has no real implementation:
   ```rust
   Generic {
       name: String,
       constraints: Vec<TypeConstraint>,
   }
   ```
   The `TypeConstraint` enum is empty and marked as a placeholder.

2. **Type::Unknown Overuse**: Currently used as a catch-all for type inference, leading to:
   - Loss of type information in nested contexts
   - Inability to properly track generic type parameters
   - No type parameter substitution mechanism

3. **Hard-coded Generic Handling**: Option and Result types are handled specially in `from_string`:
   ```rust
   // Handle generic Option<T>
   s if s.starts_with("Option<") && s.ends_with(">") => {
       // For now, just return the base Option type
       // TODO: Track generic type parameters
       Some(Type::Enum { 
           name: "Option".to_string(), 
           variants: {
               let mut v = HashMap::new();
               v.insert("Some".to_string(), Some(Type::Unknown));
               v.insert("None".to_string(), None);
               v
           }
       })
   }
   ```

### Pattern Matching Limitations

The semantic analyzer's pattern handling has several gaps:

1. **No Type Parameter Tracking**: When matching nested patterns, type parameters aren't propagated
2. **Limited Pattern Type Inference**: Patterns don't contribute to type inference
3. **No Unification Algorithm**: Missing a proper type unification system

## Implementation Requirements

### 1. Enhanced Type System

#### Generic Type Representation
```rust
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // ... existing variants ...
    
    // Enhanced generic type
    Generic {
        name: String,
        id: TypeVarId,  // Unique identifier for type variables
        constraints: Vec<TypeConstraint>,
    },
    
    // Parameterized types
    Parameterized {
        base: Box<Type>,  // e.g., Option, Result
        args: Vec<Type>,  // e.g., [T], [T, E]
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeConstraint {
    Trait(String),  // e.g., Display, Debug
    Equals(Type),   // Type equality constraint
    SubtypeOf(Type), // Subtype constraint
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVarId(u32);
```

#### Type Variable Generation
```rust
pub struct TypeVarGenerator {
    next_id: u32,
}

impl TypeVarGenerator {
    pub fn new() -> Self {
        TypeVarGenerator { next_id: 0 }
    }
    
    pub fn fresh(&mut self) -> TypeVarId {
        let id = TypeVarId(self.next_id);
        self.next_id += 1;
        id
    }
}
```

### 2. Unification Algorithm

Implement a proper unification algorithm to handle type inference:

```rust
pub struct TypeUnifier {
    substitutions: HashMap<TypeVarId, Type>,
}

impl TypeUnifier {
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<Type, UnificationError> {
        match (t1, t2) {
            // Exact match
            (a, b) if a == b => Ok(a.clone()),
            
            // Generic type variable
            (Type::Generic { id, .. }, other) | (other, Type::Generic { id, .. }) => {
                self.bind_type_var(*id, other.clone())
            }
            
            // Parameterized types
            (Type::Parameterized { base: b1, args: a1 }, 
             Type::Parameterized { base: b2, args: a2 }) => {
                let unified_base = self.unify(b1, b2)?;
                let unified_args = a1.iter().zip(a2.iter())
                    .map(|(arg1, arg2)| self.unify(arg1, arg2))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Parameterized {
                    base: Box::new(unified_base),
                    args: unified_args,
                })
            }
            
            // ... other cases ...
        }
    }
    
    fn bind_type_var(&mut self, id: TypeVarId, ty: Type) -> Result<Type, UnificationError> {
        // Check for occurs check (prevent infinite types)
        if self.occurs_check(id, &ty) {
            return Err(UnificationError::OccursCheck);
        }
        
        self.substitutions.insert(id, ty.clone());
        Ok(ty)
    }
}
```

### 3. Pattern Type Inference

Enhance pattern analysis to contribute to type inference:

```rust
impl SemanticAnalyzer {
    fn infer_pattern_type(
        &mut self,
        pattern: &Expr,
        expected_type: Option<&Type>,
        unifier: &mut TypeUnifier,
    ) -> Result<Type, String> {
        match pattern {
            Expr::StructPattern(name, fields, _) => {
                // Get the struct type (potentially parameterized)
                let struct_type = self.get_type(name)?;
                
                // If we have an expected type, unify with it
                if let Some(expected) = expected_type {
                    unifier.unify(&struct_type, expected)?;
                }
                
                // Infer types for nested patterns
                for (field_name, pattern) in fields {
                    if field_name == ".." {
                        continue;  // Rest pattern
                    }
                    
                    let field_type = self.get_field_type(&struct_type, field_name)?;
                    if let Some(pattern) = pattern {
                        self.infer_pattern_type(pattern, Some(&field_type), unifier)?;
                    }
                }
                
                Ok(struct_type)
            }
            
            Expr::EnumPattern(variant, inner_pattern, _) => {
                // Handle enum patterns with potential generic parameters
                let enum_type = self.get_enum_type_from_variant(variant)?;
                
                if let Some(inner) = inner_pattern {
                    let variant_type = self.get_variant_payload_type(&enum_type, variant)?;
                    self.infer_pattern_type(inner, Some(&variant_type), unifier)?;
                }
                
                Ok(enum_type)
            }
            
            // ... other pattern types ...
        }
    }
}
```

### 4. Nested Pattern Support

Implement proper handling for deeply nested patterns:

```rust
// Example: Ok(Some(Config { host, port, .. }))
fn analyze_nested_pattern(&mut self, pattern: &Expr) -> Result<TypeBindings, String> {
    let mut bindings = TypeBindings::new();
    let mut unifier = TypeUnifier::new();
    
    // Start with fresh type variables for Result<T, E>
    let t_var = self.type_var_gen.fresh();
    let e_var = self.type_var_gen.fresh();
    let result_type = Type::Parameterized {
        base: Box::new(Type::Enum { name: "Result".to_string(), .. }),
        args: vec![
            Type::Generic { id: t_var, .. },
            Type::Generic { id: e_var, .. },
        ],
    };
    
    // Analyze the pattern and collect constraints
    self.analyze_pattern_with_type(pattern, &result_type, &mut unifier)?;
    
    // Apply substitutions to get concrete types
    let substitutions = unifier.solve()?;
    bindings.apply_substitutions(&substitutions);
    
    Ok(bindings)
}
```

## Implementation Steps

### Phase 1: Type System Foundation (2-3 weeks)
1. Implement `TypeVarId` and `TypeVarGenerator`
2. Enhance `Type` enum with `Parameterized` variant
3. Implement basic `TypeConstraint` variants
4. Update type parsing to handle generic syntax

### Phase 2: Unification Algorithm (2-3 weeks)
1. Implement basic unification algorithm
2. Add occurs check for preventing infinite types
3. Implement substitution application
4. Add comprehensive tests for unification

### Phase 3: Pattern Type Inference (3-4 weeks)
1. Enhance semantic analyzer's pattern handling
2. Implement pattern type inference
3. Connect patterns to type unification
4. Handle nested patterns recursively

### Phase 4: Integration (2-3 weeks)
1. Update all pattern matching code paths
2. Ensure interpreter handles generic types
3. Update transpiler for generic type information
4. Comprehensive testing of nested patterns

### Phase 5: Advanced Features (2-3 weeks)
1. Type parameter constraints
2. Improved error messages for type mismatches
3. Generic function type inference
4. Performance optimization

## Testing Strategy

### Unit Tests
- Type unification tests
- Pattern type inference tests
- Substitution application tests

### Integration Tests
```husk
// Test nested Option patterns
let data: Option<Option<int>> = Some(Some(42));
match data {
    Some(Some(x)) => println(format!("Value: {}", x)),
    Some(None) => println("Inner None"),
    None => println("Outer None"),
}

// Test Result with struct patterns
let result: Result<Config, String> = Ok(Config { host: "localhost", port: 8080 });
match result {
    Ok(Config { host, port, .. }) => {
        println(format!("Connected to {}:{}", host, port));
    },
    Err(msg) => println(format!("Error: {}", msg)),
}

// Test deeply nested patterns
let complex: Result<Option<Vec<Point>>, Error> = get_points();
match complex {
    Ok(Some(points)) => {
        for Point { x, y } in points {
            println(format!("Point: ({}, {})", x, y));
        }
    },
    Ok(None) => println("No points"),
    Err(e) => println(format!("Error: {}", e)),
}
```

## Challenges and Considerations

### 1. Backward Compatibility
- Ensure existing code continues to work
- Gradual migration path from Type::Unknown

### 2. Performance Impact
- Unification can be expensive for complex types
- Need efficient substitution application
- Consider caching resolved types

### 3. Error Messages
- Generic type errors can be confusing
- Need clear, actionable error messages
- Show type inference steps when helpful

### 4. Incremental Implementation
- Can implement in stages
- Start with Option/Result, expand to user types
- Each phase should be independently useful

## Alternative Approaches

### 1. Constraint-Based Type Inference
Instead of unification, use a constraint solver approach similar to Rust's trait system.

### 2. Gradual Typing
Allow mixing of fully typed and dynamically typed code, inferring types where possible.

### 3. Limited Generics
Support only specific generic types (Option, Result, Vec) without full parametric polymorphism.

## Conclusion

Implementing full generic type inference for nested patterns is a significant undertaking that will greatly enhance Husk's type system. While complex, it can be implemented incrementally, with each phase providing immediate value. The key is to build a solid foundation with the type system enhancements and unification algorithm before tackling the more complex pattern inference scenarios.