#[cfg(test)]
mod tests {
    use super::super::*;
    use std::collections::HashMap;

    #[test]
    fn test_primitive_types_equality() {
        assert_eq!(Type::Int, Type::Int);
        assert_eq!(Type::Float, Type::Float);
        assert_eq!(Type::Bool, Type::Bool);
        assert_eq!(Type::String, Type::String);
        assert_eq!(Type::Unit, Type::Unit);

        assert_ne!(Type::Int, Type::Float);
        assert_ne!(Type::Bool, Type::String);
    }

    #[test]
    fn test_array_type_equality() {
        let int_array = Type::Array(Box::new(Type::Int));
        let float_array = Type::Array(Box::new(Type::Float));
        let nested_array = Type::Array(Box::new(Type::Array(Box::new(Type::Int))));

        assert_eq!(int_array, Type::Array(Box::new(Type::Int)));
        assert_ne!(int_array, float_array);
        assert_ne!(int_array, nested_array);
    }

    #[test]
    fn test_struct_type() {
        let point_type = Type::Struct {
            name: "Point".to_string(),
            fields: vec![("x".to_string(), Type::Int), ("y".to_string(), Type::Int)],
        };

        let point_type2 = Type::Struct {
            name: "Point".to_string(),
            fields: vec![("x".to_string(), Type::Int), ("y".to_string(), Type::Int)],
        };

        let rect_type = Type::Struct {
            name: "Rectangle".to_string(),
            fields: vec![
                ("width".to_string(), Type::Int),
                ("height".to_string(), Type::Int),
            ],
        };

        assert_eq!(point_type, point_type2);
        assert_ne!(point_type, rect_type);
    }

    #[test]
    fn test_enum_type() {
        let mut variants1 = HashMap::new();
        variants1.insert("Red".to_string(), None);
        variants1.insert("Green".to_string(), None);
        variants1.insert("Blue".to_string(), None);

        let color_type = Type::Enum {
            name: "Color".to_string(),
            variants: variants1.clone(),
        };

        let color_type2 = Type::Enum {
            name: "Color".to_string(),
            variants: variants1,
        };

        let mut variants2 = HashMap::new();
        variants2.insert("Some".to_string(), Some(Type::Int));
        variants2.insert("None".to_string(), None);

        let option_type = Type::Enum {
            name: "Option".to_string(),
            variants: variants2,
        };

        assert_eq!(color_type, color_type2);
        assert_ne!(color_type, option_type);
    }

    #[test]
    fn test_function_type() {
        let add_type = Type::Function {
            params: vec![Type::Int, Type::Int],
            return_type: Box::new(Type::Int),
        };

        let add_type2 = Type::Function {
            params: vec![Type::Int, Type::Int],
            return_type: Box::new(Type::Int),
        };

        let multiply_float = Type::Function {
            params: vec![Type::Float, Type::Float],
            return_type: Box::new(Type::Float),
        };

        assert_eq!(add_type, add_type2);
        assert_ne!(add_type, multiply_float);
    }

    #[test]
    fn test_is_assignable_to() {
        // Exact match
        assert!(Type::Int.is_assignable_to(&Type::Int));
        assert!(Type::Float.is_assignable_to(&Type::Float));

        // Different types
        assert!(!Type::Int.is_assignable_to(&Type::Float));
        assert!(!Type::Bool.is_assignable_to(&Type::String));

        // Unknown type
        assert!(Type::Unknown.is_assignable_to(&Type::Int));
        assert!(Type::Int.is_assignable_to(&Type::Unknown));
        assert!(Type::Unknown.is_assignable_to(&Type::Unknown));

        // Array covariance
        let int_array = Type::Array(Box::new(Type::Int));
        let float_array = Type::Array(Box::new(Type::Float));
        let unknown_array = Type::Array(Box::new(Type::Unknown));

        assert!(int_array.is_assignable_to(&int_array));
        assert!(!int_array.is_assignable_to(&float_array));
        assert!(unknown_array.is_assignable_to(&int_array));
        assert!(int_array.is_assignable_to(&unknown_array));
    }

    #[test]
    fn test_type_to_string() {
        // Primitive types
        assert_eq!(Type::Unit.to_string(), "unit");
        assert_eq!(Type::Int.to_string(), "int");
        assert_eq!(Type::Float.to_string(), "float");
        assert_eq!(Type::Bool.to_string(), "bool");
        assert_eq!(Type::String.to_string(), "string");
        assert_eq!(Type::Range.to_string(), "range");
        assert_eq!(Type::Unknown.to_string(), "?");

        // Array type
        assert_eq!(Type::Array(Box::new(Type::Int)).to_string(), "array<int>");
        assert_eq!(
            Type::Array(Box::new(Type::Array(Box::new(Type::Bool)))).to_string(),
            "array<array<bool>>"
        );

        // Struct type
        let point_type = Type::Struct {
            name: "Point".to_string(),
            fields: vec![],
        };
        assert_eq!(point_type.to_string(), "Point");

        // Enum type
        let color_type = Type::Enum {
            name: "Color".to_string(),
            variants: HashMap::new(),
        };
        assert_eq!(color_type.to_string(), "Color");

        // Function type
        let func_type = Type::Function {
            params: vec![Type::Int, Type::Bool],
            return_type: Box::new(Type::String),
        };
        assert_eq!(func_type.to_string(), "fn(int, bool) -> string");

        // Generic type
        let generic_type = Type::Generic {
            name: "T".to_string(),
            constraints: vec![],
        };
        assert_eq!(generic_type.to_string(), "T");
    }

    #[test]
    fn test_type_from_string() {
        // Primitive types
        assert_eq!(Type::from_string("unit"), Some(Type::Unit));
        assert_eq!(Type::from_string("void"), Some(Type::Unit));
        assert_eq!(Type::from_string("int"), Some(Type::Int));
        assert_eq!(Type::from_string("float"), Some(Type::Float));
        assert_eq!(Type::from_string("bool"), Some(Type::Bool));
        assert_eq!(Type::from_string("string"), Some(Type::String));
        assert_eq!(Type::from_string("range"), Some(Type::Range));

        // Array types
        assert_eq!(
            Type::from_string("array<int>"),
            Some(Type::Array(Box::new(Type::Int)))
        );
        assert_eq!(
            Type::from_string("array<array<bool>>"),
            Some(Type::Array(Box::new(Type::Array(Box::new(Type::Bool)))))
        );

        // User-defined types (treated as structs for now)
        match Type::from_string("Point") {
            Some(Type::Struct { name, fields }) => {
                assert_eq!(name, "Point");
                assert!(fields.is_empty());
            }
            _ => panic!("Expected struct type"),
        }
    }

    #[test]
    fn test_complex_types() {
        // Function returning array
        let func_type = Type::Function {
            params: vec![Type::Int],
            return_type: Box::new(Type::Array(Box::new(Type::String))),
        };
        assert_eq!(func_type.to_string(), "fn(int) -> array<string>");

        // Struct with function field
        let struct_with_func = Type::Struct {
            name: "Handler".to_string(),
            fields: vec![(
                "callback".to_string(),
                Type::Function {
                    params: vec![Type::Int],
                    return_type: Box::new(Type::Unit),
                },
            )],
        };
        assert_eq!(struct_with_func.to_string(), "Handler");

        // Enum with associated data
        let mut variants = HashMap::new();
        variants.insert("Some".to_string(), Some(Type::Array(Box::new(Type::Int))));
        variants.insert("None".to_string(), None);

        let option_array = Type::Enum {
            name: "OptionArray".to_string(),
            variants,
        };
        assert_eq!(option_array.to_string(), "OptionArray");
    }

    #[test]
    fn test_nested_array_assignability() {
        let int_array = Type::Array(Box::new(Type::Int));
        let int_array_array = Type::Array(Box::new(int_array.clone()));
        let float_array_array = Type::Array(Box::new(Type::Array(Box::new(Type::Float))));

        assert!(int_array_array.is_assignable_to(&int_array_array));
        assert!(!int_array_array.is_assignable_to(&float_array_array));

        // Test with unknown
        let unknown_array_array = Type::Array(Box::new(Type::Array(Box::new(Type::Unknown))));
        assert!(unknown_array_array.is_assignable_to(&int_array_array));
        assert!(int_array_array.is_assignable_to(&unknown_array_array));
    }

    #[test]
    fn test_function_type_equality() {
        let f1 = Type::Function {
            params: vec![Type::Int, Type::Int],
            return_type: Box::new(Type::Int),
        };

        let f2 = Type::Function {
            params: vec![Type::Int, Type::Int],
            return_type: Box::new(Type::Int),
        };

        let f3 = Type::Function {
            params: vec![Type::Int, Type::Float],
            return_type: Box::new(Type::Int),
        };

        let f4 = Type::Function {
            params: vec![Type::Int, Type::Int],
            return_type: Box::new(Type::Float),
        };

        let f5 = Type::Function {
            params: vec![Type::Int],
            return_type: Box::new(Type::Int),
        };

        assert_eq!(f1, f2);
        assert_ne!(f1, f3); // Different parameter types
        assert_ne!(f1, f4); // Different return type
        assert_ne!(f1, f5); // Different number of parameters
    }

    #[test]
    fn test_struct_field_order() {
        let point1 = Type::Struct {
            name: "Point".to_string(),
            fields: vec![("x".to_string(), Type::Int), ("y".to_string(), Type::Int)],
        };

        let point2 = Type::Struct {
            name: "Point".to_string(),
            fields: vec![("y".to_string(), Type::Int), ("x".to_string(), Type::Int)],
        };

        // Field order matters for equality
        assert_ne!(point1, point2);
    }

    #[test]
    fn test_edge_cases() {
        // Empty struct
        let empty_struct = Type::Struct {
            name: "Empty".to_string(),
            fields: vec![],
        };
        assert_eq!(empty_struct.to_string(), "Empty");

        // Empty enum
        let empty_enum = Type::Enum {
            name: "Never".to_string(),
            variants: HashMap::new(),
        };
        assert_eq!(empty_enum.to_string(), "Never");

        // Function with no parameters
        let no_param_func = Type::Function {
            params: vec![],
            return_type: Box::new(Type::Unit),
        };
        assert_eq!(no_param_func.to_string(), "fn() -> unit");

        // Deeply nested array
        let deep_array = Type::Array(Box::new(Type::Array(Box::new(Type::Array(Box::new(
            Type::Int,
        ))))));
        assert_eq!(deep_array.to_string(), "array<array<array<int>>>");
    }
}
