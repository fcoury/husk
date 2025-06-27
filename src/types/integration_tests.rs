#[cfg(test)]
mod tests {
    use super::super::*;
    use std::collections::HashMap;

    #[test]
    fn test_type_from_string_round_trip() {
        let types = vec![
            Type::Int,
            Type::Float,
            Type::Bool,
            Type::String,
            Type::Unit,
            Type::Range,
            Type::Array(Box::new(Type::Int)),
            Type::Array(Box::new(Type::Array(Box::new(Type::Bool)))),
        ];

        for ty in types {
            let string_repr = ty.to_string();
            if let Some(parsed) = Type::from_string(&string_repr) {
                // For primitive and array types, round trip should work
                match &ty {
                    Type::Struct { .. }
                    | Type::Enum { .. }
                    | Type::Function { .. }
                    | Type::Generic { .. } => {
                        // These don't round-trip perfectly
                    }
                    _ => {
                        assert_eq!(ty, parsed, "Round trip failed for {}", string_repr);
                    }
                }
            }
        }
    }

    #[test]
    fn test_complex_type_scenarios() {
        // Test a scenario with nested types
        let mut env = TypeEnvironment::new();

        // Array of structs
        let point_type = Type::Struct {
            name: "Point".to_string(),
            fields: vec![("x".to_string(), Type::Int), ("y".to_string(), Type::Int)],
        };
        let points_array = Type::Array(Box::new(point_type.clone()));
        env.define("points".to_string(), points_array);

        // Function returning enum
        let mut option_variants = HashMap::new();
        option_variants.insert("Some".to_string(), vec![Type::Int]);
        option_variants.insert("None".to_string(), vec![]);
        let option_type = Type::Enum {
            name: "Option".to_string(),
            variants: option_variants,
        };
        let find_fn = Type::Function {
            params: vec![Type::Array(Box::new(Type::Int)), Type::Int],
            return_type: Box::new(option_type.clone()),
        };
        env.define("find".to_string(), find_fn);

        // Struct containing function
        let handler_type = Type::Struct {
            name: "Handler".to_string(),
            fields: vec![(
                "callback".to_string(),
                Type::Function {
                    params: vec![Type::Int],
                    return_type: Box::new(Type::Unit),
                },
            )],
        };
        env.define("Handler".to_string(), handler_type);

        // Verify all types are correctly stored
        assert!(matches!(env.lookup("points"), Some(Type::Array(_))));
        assert!(matches!(env.lookup("find"), Some(Type::Function { .. })));
        assert!(matches!(env.lookup("Handler"), Some(Type::Struct { .. })));
    }

    #[test]
    fn test_type_compatibility() {
        // Test various type compatibility scenarios
        let int_array = Type::Array(Box::new(Type::Int));
        let float_array = Type::Array(Box::new(Type::Float));
        let int_array2 = Type::Array(Box::new(Type::Int));

        // Same types should be compatible
        assert!(int_array.is_assignable_to(&int_array2));

        // Different element types should not be compatible
        assert!(!int_array.is_assignable_to(&float_array));

        // Test with unknown
        let unknown_array = Type::Array(Box::new(Type::Unknown));
        assert!(unknown_array.is_assignable_to(&int_array));
        assert!(int_array.is_assignable_to(&unknown_array));
    }

    #[test]
    fn test_type_environment_with_complex_types() {
        let mut env = TypeEnvironment::new();

        // Define a complex generic type (for future use)
        let generic_list = Type::Generic {
            name: "List<T>".to_string(),
            constraints: vec![],
        };
        env.define("GenericList".to_string(), generic_list);

        // Define a function with multiple parameters
        let complex_fn = Type::Function {
            params: vec![Type::Int, Type::Array(Box::new(Type::String)), Type::Bool],
            return_type: Box::new(Type::Unit),
        };
        env.define("process".to_string(), complex_fn);

        // Nested enums
        let mut inner_variants = HashMap::new();
        inner_variants.insert("Ok".to_string(), vec![Type::Int]);
        inner_variants.insert("Err".to_string(), vec![Type::String]);
        let result_type = Type::Enum {
            name: "Result".to_string(),
            variants: inner_variants,
        };

        let mut outer_variants = HashMap::new();
        outer_variants.insert("Success".to_string(), vec![result_type.clone()]);
        outer_variants.insert("Pending".to_string(), vec![]);
        let status_type = Type::Enum {
            name: "Status".to_string(),
            variants: outer_variants,
        };
        env.define("Status".to_string(), status_type);

        // Verify lookups
        assert!(matches!(
            env.lookup("GenericList"),
            Some(Type::Generic { .. })
        ));
        assert!(
            matches!(env.lookup("process"), Some(Type::Function { params, .. }) if params.len() == 3)
        );
        assert!(matches!(env.lookup("Status"), Some(Type::Enum { .. })));
    }

    #[test]
    fn test_type_display_formatting() {
        // Test that complex types display correctly
        let mut variants = HashMap::new();
        variants.insert("Some".to_string(), vec![Type::Array(Box::new(Type::Int))]);
        variants.insert("None".to_string(), vec![]);

        let complex_enum = Type::Enum {
            name: "OptionIntArray".to_string(),
            variants,
        };
        assert_eq!(complex_enum.to_string(), "OptionIntArray");

        // Complex function type
        let curry_fn = Type::Function {
            params: vec![Type::Int],
            return_type: Box::new(Type::Function {
                params: vec![Type::Int],
                return_type: Box::new(Type::Int),
            }),
        };
        assert_eq!(curry_fn.to_string(), "fn(int) -> fn(int) -> int");

        // Deeply nested arrays
        let nested = Type::Array(Box::new(Type::Array(Box::new(Type::Array(Box::new(
            Type::String,
        ))))));
        assert_eq!(nested.to_string(), "array<array<array<string>>>");
    }

    #[test]
    fn test_assignability_edge_cases() {
        // Test assignability with structs
        let point1 = Type::Struct {
            name: "Point".to_string(),
            fields: vec![("x".to_string(), Type::Int)],
        };
        let point2 = Type::Struct {
            name: "Point".to_string(),
            fields: vec![("x".to_string(), Type::Int)],
        };
        let point3 = Type::Struct {
            name: "Point".to_string(),
            fields: vec![("x".to_string(), Type::Float)],
        };

        assert!(point1.is_assignable_to(&point2)); // Same struct
        assert!(!point1.is_assignable_to(&point3)); // Different field types

        // Test with functions
        let fn1 = Type::Function {
            params: vec![Type::Int],
            return_type: Box::new(Type::Int),
        };
        let fn2 = Type::Function {
            params: vec![Type::Int],
            return_type: Box::new(Type::Int),
        };

        assert!(fn1.is_assignable_to(&fn2));

        // Unknown in nested positions
        let array_unknown = Type::Array(Box::new(Type::Unknown));
        let array_int = Type::Array(Box::new(Type::Int));
        let array_array_unknown = Type::Array(Box::new(array_unknown.clone()));
        let array_array_int = Type::Array(Box::new(array_int.clone()));

        assert!(array_array_unknown.is_assignable_to(&array_array_int));
        assert!(array_array_int.is_assignable_to(&array_array_unknown));
    }

    #[test]
    fn test_type_environment_with_shadowing() {
        let mut env = TypeEnvironment::new();

        // Test complex shadowing scenarios
        env.define("value".to_string(), Type::Int);

        env.push_scope();
        env.define("value".to_string(), Type::Float);

        env.push_scope();
        env.define("value".to_string(), Type::Array(Box::new(Type::String)));

        assert!(matches!(env.lookup("value"), Some(Type::Array(_))));

        env.pop_scope();
        assert_eq!(env.lookup("value"), Some(&Type::Float));

        env.pop_scope();
        assert_eq!(env.lookup("value"), Some(&Type::Int));
    }

    #[test]
    fn test_struct_with_self_reference() {
        // Test a struct that contains itself (through array/option)
        let node_fields = vec![
            ("value".to_string(), Type::Int),
            (
                "next".to_string(),
                Type::Array(Box::new(Type::Struct {
                    name: "Node".to_string(),
                    fields: vec![], // Simplified for this test
                })),
            ),
        ];

        let node_type = Type::Struct {
            name: "Node".to_string(),
            fields: node_fields,
        };

        assert_eq!(node_type.to_string(), "Node");
        assert!(matches!(&node_type, Type::Struct { fields, .. } if fields.len() == 2));
    }
}
