use std::collections::HashMap;
use super::Type;

/// Manages type bindings with lexical scoping
#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    scopes: Vec<HashMap<String, Type>>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()], // Start with global scope
        }
    }
    
    /// Enter a new scope
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    
    /// Exit the current scope
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }
    
    /// Define a type binding in the current scope
    pub fn define(&mut self, name: String, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }
    
    /// Look up a type binding, searching from innermost to outermost scope
    pub fn lookup(&self, name: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }
    
    /// Check if a name is defined in the current scope only
    #[allow(dead_code)]
    pub fn is_defined_in_current_scope(&self, name: &str) -> bool {
        self.scopes
            .last()
            .map(|scope| scope.contains_key(name))
            .unwrap_or(false)
    }
}

/// RAII guard for automatic scope cleanup
pub struct ScopeGuard<'a> {
    env: &'a mut TypeEnvironment,
}

impl<'a> ScopeGuard<'a> {
    #[allow(dead_code)]
    pub fn new(env: &'a mut TypeEnvironment) -> Self {
        env.push_scope();
        Self { env }
    }
}

impl<'a> Drop for ScopeGuard<'a> {
    fn drop(&mut self) {
        self.env.pop_scope();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scoping() {
        let mut env = TypeEnvironment::new();
        
        // Define in global scope
        env.define("x".to_string(), Type::Int);
        assert_eq!(env.lookup("x"), Some(&Type::Int));
        
        // Enter new scope
        env.push_scope();
        
        // Shadow x in inner scope
        env.define("x".to_string(), Type::Float);
        assert_eq!(env.lookup("x"), Some(&Type::Float));
        
        // Define y in inner scope
        env.define("y".to_string(), Type::Bool);
        assert_eq!(env.lookup("y"), Some(&Type::Bool));
        
        // Exit scope
        env.pop_scope();
        
        // Back to global scope
        assert_eq!(env.lookup("x"), Some(&Type::Int));
        assert_eq!(env.lookup("y"), None); // y is no longer in scope
    }
}