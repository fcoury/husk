mod environment;
mod environment_tests;
mod integration_tests;
mod type_tests;

pub use environment::TypeEnvironment;

use std::collections::HashMap;

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

    // User-defined types
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
    },
    Enum {
        name: String,
        variants: HashMap<String, Option<Type>>,
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
            
            // Array covariance
            (Type::Array(a), Type::Array(b)) => a.is_assignable_to(b),
            
            // Promise covariance
            (Type::Promise(a), Type::Promise(b)) => a.is_assignable_to(b),
            
            // Function types - check parameters and return type
            (Type::Function { params: params1, return_type: ret1 }, 
             Type::Function { params: params2, return_type: ret2 }) => {
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
            },
            
            // Struct types are compatible if they have the same name and field structure
            (Type::Struct { name: name1, fields: fields1 }, Type::Struct { name: name2, fields: fields2 }) => {
                name1 == name2 && fields1 == fields2
            },
            
            // Enum types are compatible if they have the same name
            // We compare by name only to handle built-in generic enums like Option<T> and Result<T,E>
            (Type::Enum { name: name1, .. }, Type::Enum { name: name2, .. }) => {
                name1 == name2
            },
            
            // TODO: Add more rules as needed (e.g., int to float coercion)
            _ => false,
        }
    }
    
    /// Returns a human-readable string representation of the type
    pub fn to_string(&self) -> String {
        match self {
            Type::Unit => "unit".to_string(),
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "string".to_string(),
            Type::Array(inner) => format!("array<{}>", inner.to_string()),
            Type::Range => "range".to_string(),
            Type::Struct { name, .. } => name.clone(),
            Type::Enum { name, .. } => name.clone(),
            Type::Function { params, return_type } => {
                let param_str = params.iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn({}) -> {}", param_str, return_type.to_string())
            },
            Type::Promise(inner) => format!("Promise<{}>", inner.to_string()),
            Type::Generic { name, .. } => name.clone(),
            Type::Unknown => "?".to_string(),
        }
    }
    
    /// Creates a Type from a string representation (used during migration)
    pub fn from_string(s: &str) -> Option<Type> {
        match s {
            "unit" | "void" => Some(Type::Unit),
            "int" => Some(Type::Int),
            "float" => Some(Type::Float),
            "bool" => Some(Type::Bool),
            "string" => Some(Type::String),
            "range" => Some(Type::Range),
            s if s.starts_with("array<") && s.ends_with(">") => {
                let inner = &s[6..s.len()-1];
                Type::from_string(inner).map(|t| Type::Array(Box::new(t)))
            },
            s if s.starts_with("Promise<") && s.ends_with(">") => {
                let inner = &s[8..s.len()-1];
                Type::from_string(inner).map(|t| Type::Promise(Box::new(t)))
            },
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
            },
            // Special handling for built-in enums
            "Option" => Some(Type::Enum { 
                name: "Option".to_string(), 
                variants: {
                    let mut v = HashMap::new();
                    v.insert("Some".to_string(), Some(Type::Unknown));
                    v.insert("None".to_string(), None);
                    v
                }
            }),
            "Result" => Some(Type::Enum { 
                name: "Result".to_string(), 
                variants: {
                    let mut v = HashMap::new();
                    v.insert("Ok".to_string(), Some(Type::Unknown));
                    v.insert("Err".to_string(), Some(Type::Unknown));
                    v
                }
            }),
            // For now, treat any other string as a struct/enum name
            _ => Some(Type::Struct { 
                name: s.to_string(), 
                fields: vec![] // Will be filled in by semantic analyzer
            }),
        }
    }
}
