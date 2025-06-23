pub mod visitor;
mod visitor_tests;
mod visitor_integration_tests;

use std::cell::RefCell;
use std::rc::Rc;

/// Type annotation that can be attached to AST nodes
#[derive(Debug, Clone)]
pub enum TypeAnnotation {
    /// Node represents an enum type
    Enum(String),
    /// Node represents a struct type  
    Struct(String),
    /// Node represents a function
    Function,
    /// Node represents a static method
    StaticMethod(String), // Contains the struct/enum name
    /// Node represents an instance method
    InstanceMethod(String), // Contains the struct name
    /// No type information available
    None,
}

/// Wrapper for shared mutable type annotations
pub type TypeAnnotationRef = Rc<RefCell<TypeAnnotation>>;

impl TypeAnnotation {
    pub fn new() -> TypeAnnotationRef {
        Rc::new(RefCell::new(TypeAnnotation::None))
    }
    
    pub fn enum_type(name: String) -> TypeAnnotationRef {
        Rc::new(RefCell::new(TypeAnnotation::Enum(name)))
    }
    
    pub fn struct_type(name: String) -> TypeAnnotationRef {
        Rc::new(RefCell::new(TypeAnnotation::Struct(name)))
    }
    
    pub fn static_method(type_name: String) -> TypeAnnotationRef {
        Rc::new(RefCell::new(TypeAnnotation::StaticMethod(type_name)))
    }
}