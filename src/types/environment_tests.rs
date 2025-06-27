#[cfg(test)]
mod tests {
    use super::super::{Type, TypeEnvironment};
    use std::collections::HashMap;

    #[test]
    fn test_basic_operations() {
        let mut env = TypeEnvironment::new();

        // Define and lookup
        env.define("x".to_string(), Type::Int);
        assert_eq!(env.lookup("x"), Some(&Type::Int));

        // Non-existent variable
        assert_eq!(env.lookup("y"), None);

        // Redefine in same scope
        env.define("x".to_string(), Type::Float);
        assert_eq!(env.lookup("x"), Some(&Type::Float));
    }

    #[test]
    fn test_nested_scopes() {
        let mut env = TypeEnvironment::new();

        // Global scope
        env.define("global".to_string(), Type::Int);
        env.define("x".to_string(), Type::Bool);

        // Enter scope 1
        env.push_scope();
        env.define("scope1".to_string(), Type::String);
        env.define("x".to_string(), Type::Float); // Shadow global x

        assert_eq!(env.lookup("global"), Some(&Type::Int));
        assert_eq!(env.lookup("scope1"), Some(&Type::String));
        assert_eq!(env.lookup("x"), Some(&Type::Float)); // Gets shadowed version

        // Enter scope 2
        env.push_scope();
        env.define("scope2".to_string(), Type::Unit);
        env.define("x".to_string(), Type::Range); // Shadow again

        assert_eq!(env.lookup("global"), Some(&Type::Int));
        assert_eq!(env.lookup("scope1"), Some(&Type::String));
        assert_eq!(env.lookup("scope2"), Some(&Type::Unit));
        assert_eq!(env.lookup("x"), Some(&Type::Range)); // Gets most recent shadow

        // Exit scope 2
        env.pop_scope();
        assert_eq!(env.lookup("scope2"), None);
        assert_eq!(env.lookup("x"), Some(&Type::Float)); // Back to scope 1 version

        // Exit scope 1
        env.pop_scope();
        assert_eq!(env.lookup("scope1"), None);
        assert_eq!(env.lookup("x"), Some(&Type::Bool)); // Back to global version
    }

    #[test]
    fn test_scope_guard() {
        let mut env = TypeEnvironment::new();
        env.define("outer".to_string(), Type::Int);

        // Test with manual scope management since we can't borrow mutably twice
        env.push_scope();
        env.define("inner".to_string(), Type::Bool);
        assert_eq!(env.lookup("inner"), Some(&Type::Bool));
        assert_eq!(env.lookup("outer"), Some(&Type::Int));
        env.pop_scope();

        assert_eq!(env.lookup("inner"), None);
        assert_eq!(env.lookup("outer"), Some(&Type::Int));
    }

    #[test]
    fn test_is_defined_in_current_scope() {
        let mut env = TypeEnvironment::new();

        env.define("x".to_string(), Type::Int);
        assert!(env.is_defined_in_current_scope("x"));
        assert!(!env.is_defined_in_current_scope("y"));

        env.push_scope();
        assert!(!env.is_defined_in_current_scope("x")); // x is in parent scope

        env.define("x".to_string(), Type::Float);
        assert!(env.is_defined_in_current_scope("x")); // Now x is in current scope
    }

    #[test]
    fn test_pop_scope_protection() {
        let mut env = TypeEnvironment::new();

        // Should not be able to pop the global scope
        env.pop_scope();
        env.pop_scope();
        env.pop_scope();

        // Should still have at least one scope
        env.define("x".to_string(), Type::Int);
        assert_eq!(env.lookup("x"), Some(&Type::Int));
    }

    #[test]
    fn test_complex_type_environment() {
        let mut env = TypeEnvironment::new();

        // Define a struct type
        let point_type = Type::Struct {
            name: "Point".to_string(),
            fields: vec![("x".to_string(), Type::Int), ("y".to_string(), Type::Int)],
        };
        env.define("Point".to_string(), point_type.clone());

        // Define a function type
        let make_point_type = Type::Function {
            params: vec![Type::Int, Type::Int],
            return_type: Box::new(point_type.clone()),
        };
        env.define("make_point".to_string(), make_point_type);

        // Define an enum type
        let mut variants = HashMap::new();
        variants.insert("Some".to_string(), vec![point_type.clone()]);
        variants.insert("None".to_string(), vec![]);
        let option_point_type = Type::Enum {
            name: "OptionPoint".to_string(),
            variants,
        };
        env.define("OptionPoint".to_string(), option_point_type);

        // Verify all types are accessible
        assert!(matches!(env.lookup("Point"), Some(Type::Struct { .. })));
        assert!(matches!(
            env.lookup("make_point"),
            Some(Type::Function { .. })
        ));
        assert!(matches!(env.lookup("OptionPoint"), Some(Type::Enum { .. })));
    }

    #[test]
    fn test_multiple_scope_guards() {
        let mut env = TypeEnvironment::new();
        env.define("level0".to_string(), Type::Int);

        // Test with manual scope management
        env.push_scope();
        env.define("level1".to_string(), Type::Bool);

        env.push_scope();
        env.define("level2".to_string(), Type::String);

        assert_eq!(env.lookup("level0"), Some(&Type::Int));
        assert_eq!(env.lookup("level1"), Some(&Type::Bool));
        assert_eq!(env.lookup("level2"), Some(&Type::String));

        env.pop_scope();
        assert_eq!(env.lookup("level2"), None);
        assert_eq!(env.lookup("level1"), Some(&Type::Bool));

        env.pop_scope();
        assert_eq!(env.lookup("level1"), None);
        assert_eq!(env.lookup("level0"), Some(&Type::Int));
    }

    #[test]
    fn test_shadow_and_restore() {
        let mut env = TypeEnvironment::new();

        // Define x as Int
        env.define("x".to_string(), Type::Int);
        assert_eq!(env.lookup("x"), Some(&Type::Int));

        env.push_scope();
        // Shadow x as Float
        env.define("x".to_string(), Type::Float);
        assert_eq!(env.lookup("x"), Some(&Type::Float));

        env.push_scope();
        // Shadow x as Bool
        env.define("x".to_string(), Type::Bool);
        assert_eq!(env.lookup("x"), Some(&Type::Bool));

        env.push_scope();
        // Don't define x in this scope
        assert_eq!(env.lookup("x"), Some(&Type::Bool)); // Gets from parent

        env.pop_scope();
        assert_eq!(env.lookup("x"), Some(&Type::Bool));

        env.pop_scope();
        assert_eq!(env.lookup("x"), Some(&Type::Float));

        env.pop_scope();
        assert_eq!(env.lookup("x"), Some(&Type::Int));
    }

    #[test]
    fn test_array_types_in_environment() {
        let mut env = TypeEnvironment::new();

        // Array of primitives
        env.define("int_array".to_string(), Type::Array(Box::new(Type::Int)));
        env.define(
            "string_array".to_string(),
            Type::Array(Box::new(Type::String)),
        );

        // Nested array
        let nested = Type::Array(Box::new(Type::Array(Box::new(Type::Bool))));
        env.define("matrix".to_string(), nested);

        // Array of struct
        let point_type = Type::Struct {
            name: "Point".to_string(),
            fields: vec![],
        };
        env.define("points".to_string(), Type::Array(Box::new(point_type)));

        assert!(matches!(
            env.lookup("int_array"),
            Some(Type::Array(inner)) if **inner == Type::Int
        ));

        assert!(matches!(
            env.lookup("matrix"),
            Some(Type::Array(inner)) if matches!(**inner, Type::Array(_))
        ));
    }

    #[test]
    fn test_function_types_in_environment() {
        let mut env = TypeEnvironment::new();

        // Simple function
        let add_fn = Type::Function {
            params: vec![Type::Int, Type::Int],
            return_type: Box::new(Type::Int),
        };
        env.define("add".to_string(), add_fn);

        // Higher-order function (function returning function)
        let curry_add = Type::Function {
            params: vec![Type::Int],
            return_type: Box::new(Type::Function {
                params: vec![Type::Int],
                return_type: Box::new(Type::Int),
            }),
        };
        env.define("curry_add".to_string(), curry_add);

        // Function taking array
        let sum_array = Type::Function {
            params: vec![Type::Array(Box::new(Type::Int))],
            return_type: Box::new(Type::Int),
        };
        env.define("sum_array".to_string(), sum_array);

        assert!(matches!(env.lookup("add"), Some(Type::Function { .. })));
        assert!(matches!(
            env.lookup("curry_add"),
            Some(Type::Function { return_type, .. }) if matches!(**return_type, Type::Function { .. })
        ));
    }

    #[test]
    fn test_empty_environment() {
        let env = TypeEnvironment::new();

        // Should have no bindings initially
        assert_eq!(env.lookup("anything"), None);
        assert!(!env.is_defined_in_current_scope("anything"));
    }

    #[test]
    fn test_large_environment() {
        let mut env = TypeEnvironment::new();

        // Add many bindings
        for i in 0..100 {
            env.define(format!("var{}", i), Type::Int);
        }

        // All should be accessible
        for i in 0..100 {
            assert_eq!(env.lookup(&format!("var{}", i)), Some(&Type::Int));
        }

        // Non-existent should still return None
        assert_eq!(env.lookup("var100"), None);
    }
}
