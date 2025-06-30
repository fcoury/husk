mod environment;
mod environment_tests;
mod integration_tests;
mod type_tests;

pub use environment::TypeEnvironment;

use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Primitive types
    Unit,
    Int,
    Float,
    Bool,
    String,

    // Composite types
    Array(Box<Type>),
    Range,
    Tuple(Vec<Type>),

    // User-defined types
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
    },
    Enum {
        name: String,
        variants: HashMap<String, Vec<Type>>,
    },

    // Function type
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },

    // Promise type for async operations
    Promise(Box<Type>),

    // For future use: Generic types
    Generic {
        name: String,
        constraints: Vec<TypeConstraint>,
    },

    // Unknown type (for inference)
    Unknown,
    
    // Any type (accepts and can be assigned to anything)
    Any,
}

// Placeholder for future implementation
#[derive(Debug, Clone, PartialEq)]
pub enum TypeConstraint {
    // Will be implemented when we add generics
}

impl Type {
    /// Returns true if this type can be assigned to the target type
    pub fn is_assignable_to(&self, target: &Type) -> bool {
        match (self, target) {
            // Exact match
            (a, b) if a == b => true,

            // Unknown can be assigned to anything (for inference)
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            
            // Any can be assigned to anything and anything can be assigned to Any
            (Type::Any, _) | (_, Type::Any) => true,

            // Array covariance
            (Type::Array(a), Type::Array(b)) => a.is_assignable_to(b),

            // Tuple type checking
            (Type::Tuple(types1), Type::Tuple(types2)) => {
                if types1.len() != types2.len() {
                    return false;
                }
                types1
                    .iter()
                    .zip(types2.iter())
                    .all(|(t1, t2)| t1.is_assignable_to(t2))
            }

            // Promise covariance
            (Type::Promise(a), Type::Promise(b)) => a.is_assignable_to(b),

            // Function types - check parameters and return type
            (
                Type::Function {
                    params: params1,
                    return_type: ret1,
                },
                Type::Function {
                    params: params2,
                    return_type: ret2,
                },
            ) => {
                // Check same number of parameters
                if params1.len() != params2.len() {
                    return false;
                }

                // Check each parameter is assignable
                for (p1, p2) in params1.iter().zip(params2.iter()) {
                    if !p1.is_assignable_to(p2) {
                        return false;
                    }
                }

                // Check return type is assignable
                ret1.is_assignable_to(ret2)
            }

            // Struct types are compatible if they have the same name and field structure
            // Special case: if either struct has empty fields, only compare names (for forward declarations)
            (
                Type::Struct {
                    name: name1,
                    fields: fields1,
                },
                Type::Struct {
                    name: name2,
                    fields: fields2,
                },
            ) => {
                if name1 != name2 {
                    false
                } else if fields1.is_empty() || fields2.is_empty() {
                    // If either has empty fields, it's a forward declaration - only check name
                    true
                } else {
                    // Both have fields - check they match
                    fields1 == fields2
                }
            }

            // Enum types are compatible if they have the same name
            // We compare by name only to handle built-in generic enums like Option<T> and Result<T,E>
            (Type::Enum { name: name1, .. }, Type::Enum { name: name2, .. }) => name1 == name2,

            // TODO: Add more rules as needed (e.g., int to float coercion)
            _ => false,
        }
    }

    /// Creates a Type from a string representation (used during migration)
    pub fn from_string(s: &str) -> Option<Type> {
        match s {
            "unit" | "void" => Some(Type::Unit),
            "int" | "i32" | "i64" | "isize" => Some(Type::Int),
            "float" | "f32" | "f64" => Some(Type::Float),
            "bool" => Some(Type::Bool),
            "string" => Some(Type::String),
            "range" => Some(Type::Range),
            "any" => Some(Type::Any),
            s if s.starts_with("array<") && s.ends_with(">") => {
                let inner = &s[6..s.len() - 1];
                Type::from_string(inner).map(|t| Type::Array(Box::new(t)))
            }
            s if s.starts_with("Vec<") && s.ends_with(">") => {
                // Vec<T> is an alias for array<T>
                let inner = &s[4..s.len() - 1];
                Type::from_string(inner).map(|t| Type::Array(Box::new(t)))
            }
            s if s.starts_with("Promise<") && s.ends_with(">") => {
                let inner = &s[8..s.len() - 1];
                Type::from_string(inner).map(|t| Type::Promise(Box::new(t)))
            }
            s if s.starts_with("fn(") && s.contains(") ->") => {
                // Parse function type: fn(int, string) -> bool
                let paren_end = s.find(") ->").unwrap();
                let params_str = &s[3..paren_end];
                let return_str = &s[paren_end + 4..].trim();

                // Parse parameters
                let mut params = Vec::new();
                if !params_str.is_empty() {
                    for param_str in params_str.split(',') {
                        let param_type = Type::from_string(param_str.trim())?;
                        params.push(param_type);
                    }
                }

                // Parse return type
                let return_type = Type::from_string(return_str)?;

                Some(Type::Function {
                    params,
                    return_type: Box::new(return_type),
                })
            }
            // Special handling for built-in enums
            "Option" => Some(Type::Enum {
                name: "Option".to_string(),
                variants: {
                    let mut v = HashMap::new();
                    v.insert("Some".to_string(), vec![Type::Unknown]);
                    v.insert("None".to_string(), vec![]);
                    v
                },
            }),
            "Result" => Some(Type::Enum {
                name: "Result".to_string(),
                variants: {
                    let mut v = HashMap::new();
                    v.insert("Ok".to_string(), vec![Type::Unknown]);
                    v.insert("Err".to_string(), vec![Type::Unknown]);
                    v
                },
            }),
            // Handle generic Option<T>
            s if s.starts_with("Option<") && s.ends_with(">") => {
                // For now, just return the base Option type
                // TODO: Track generic type parameters
                Some(Type::Enum {
                    name: "Option".to_string(),
                    variants: {
                        let mut v = HashMap::new();
                        v.insert("Some".to_string(), vec![Type::Unknown]);
                        v.insert("None".to_string(), vec![]);
                        v
                    },
                })
            }
            // Handle generic Result<T, E>
            s if s.starts_with("Result<") && s.ends_with(">") => {
                // For now, just return the base Result type
                // TODO: Track generic type parameters
                Some(Type::Enum {
                    name: "Result".to_string(),
                    variants: {
                        let mut v = HashMap::new();
                        v.insert("Ok".to_string(), vec![Type::Unknown]);
                        v.insert("Err".to_string(), vec![Type::Unknown]);
                        v
                    },
                })
            }
            // For now, treat any other string as a struct/enum name
            _ => Some(Type::Struct {
                name: s.to_string(),
                fields: vec![], // Will be filled in by semantic analyzer
            }),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "string"),
            Type::Array(inner) => write!(f, "array<{}>", inner),
            Type::Range => write!(f, "range"),
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::Struct { name, .. } => write!(f, "{}", name),
            Type::Enum { name, .. } => write!(f, "{}", name),
            Type::Function {
                params,
                return_type,
            } => {
                write!(f, "fn(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, ") -> {}", return_type)
            }
            Type::Promise(inner) => write!(f, "Promise<{}>", inner),
            Type::Generic { name, .. } => write!(f, "{}", name),
            Type::Unknown => write!(f, "?"),
            Type::Any => write!(f, "any"),
        }
    }
}
