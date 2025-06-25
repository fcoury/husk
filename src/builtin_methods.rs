use std::collections::HashMap;
use crate::{
    interpreter::Value,
    error::{Error, Result},
    span::Span,
};

/// Type for built-in method implementations in interpreter
pub type MethodImpl = fn(&Value, &[Value], &Span) -> Result<Value>;

/// Type for transpiler code generation
pub type TranspilerImpl = fn(&str, &[String]) -> String;

/// Registry for built-in methods on primitive types
pub struct MethodRegistry {
    string_methods: HashMap<&'static str, MethodImpl>,
    array_methods: HashMap<&'static str, MethodImpl>,
}

/// Registry for transpiler method implementations
pub struct TranspilerMethodRegistry {
    string_methods: HashMap<&'static str, TranspilerImpl>,
    array_methods: HashMap<&'static str, TranspilerImpl>,
}

impl MethodRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            string_methods: HashMap::new(),
            array_methods: HashMap::new(),
        };
        
        registry.register_string_methods();
        registry.register_array_methods();
        
        registry
    }
    
    fn register_string_methods(&mut self) {
        self.string_methods.insert("len", string_len);
        self.string_methods.insert("trim", string_trim);
        self.string_methods.insert("chars", string_chars);
        self.string_methods.insert("substring", string_substring);
        self.string_methods.insert("split", string_split);
        self.string_methods.insert("toLowerCase", string_to_lowercase);
        self.string_methods.insert("toUpperCase", string_to_uppercase);
        self.string_methods.insert("contains", string_contains);
        self.string_methods.insert("starts_with", string_starts_with);
        self.string_methods.insert("ends_with", string_ends_with);
        self.string_methods.insert("replace", string_replace);
    }
    
    fn register_array_methods(&mut self) {
        self.array_methods.insert("len", array_len);
        self.array_methods.insert("first", array_first);
        self.array_methods.insert("last", array_last);
        self.array_methods.insert("get", array_get);
        self.array_methods.insert("slice", array_slice);
        self.array_methods.insert("concat", array_concat);
        self.array_methods.insert("join", array_join);
        self.array_methods.insert("contains", array_contains);
        self.array_methods.insert("map", array_map);
        self.array_methods.insert("filter", array_filter);
    }
    
    pub fn get_string_method(&self, name: &str) -> Option<&MethodImpl> {
        self.string_methods.get(name)
    }
    
    pub fn get_array_method(&self, name: &str) -> Option<&MethodImpl> {
        self.array_methods.get(name)
    }
}

// String method implementations

fn string_len(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        Ok(Value::Int(s.len() as i64))
    } else {
        Err(Error::new_runtime("len() called on non-string", *_span))
    }
}

fn string_trim(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        Ok(Value::String(s.trim().to_string()))
    } else {
        Err(Error::new_runtime("trim() called on non-string", *_span))
    }
}

fn string_chars(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        // Convert string to array of single-character strings
        // Using chars() properly handles Unicode
        let chars: Vec<Value> = s.chars()
            .map(|c| Value::String(c.to_string()))
            .collect();
        Ok(Value::Array(chars))
    } else {
        Err(Error::new_runtime("chars() called on non-string", *_span))
    }
}

fn string_substring(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.len() != 2 {
            return Err(Error::new_runtime(
                "substring() requires exactly 2 arguments", 
                *span
            ));
        }
        
        let start = match &args[0] {
            Value::Int(i) => *i as usize,
            _ => return Err(Error::new_runtime(
                "substring start must be an integer", 
                *span
            )),
        };
        
        let end = match &args[1] {
            Value::Int(i) => *i as usize,
            _ => return Err(Error::new_runtime(
                "substring end must be an integer", 
                *span
            )),
        };
        
        // Handle UTF-8 properly
        let chars: Vec<char> = s.chars().collect();
        let result: String = chars
            .iter()
            .skip(start)
            .take(end.saturating_sub(start))
            .collect();
        Ok(Value::String(result))
    } else {
        Err(Error::new_runtime("substring() called on non-string", *span))
    }
}

fn string_split(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.is_empty() {
            return Err(Error::new_runtime(
                "split() requires a delimiter argument", 
                *span
            ));
        }
        
        let delimiter = match &args[0] {
            Value::String(d) => d,
            _ => return Err(Error::new_runtime(
                "split delimiter must be a string", 
                *span
            )),
        };
        
        let parts: Vec<Value> = s
            .split(delimiter)
            .map(|part| Value::String(part.to_string()))
            .collect();
        Ok(Value::Array(parts))
    } else {
        Err(Error::new_runtime("split() called on non-string", *span))
    }
}

fn string_to_lowercase(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        Ok(Value::String(s.to_lowercase()))
    } else {
        Err(Error::new_runtime("toLowerCase() called on non-string", *_span))
    }
}

fn string_to_uppercase(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        Ok(Value::String(s.to_uppercase()))
    } else {
        Err(Error::new_runtime("toUpperCase() called on non-string", *_span))
    }
}

fn string_contains(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.is_empty() {
            return Err(Error::new_runtime(
                "contains() requires a pattern argument", 
                *span
            ));
        }
        
        let pattern = match &args[0] {
            Value::String(p) => p,
            _ => return Err(Error::new_runtime(
                "contains pattern must be a string", 
                *span
            )),
        };
        
        Ok(Value::Bool(s.contains(pattern)))
    } else {
        Err(Error::new_runtime("contains() called on non-string", *span))
    }
}

fn string_starts_with(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.is_empty() {
            return Err(Error::new_runtime(
                "starts_with() requires a prefix argument", 
                *span
            ));
        }
        
        let prefix = match &args[0] {
            Value::String(p) => p,
            _ => return Err(Error::new_runtime(
                "starts_with prefix must be a string", 
                *span
            )),
        };
        
        Ok(Value::Bool(s.starts_with(prefix)))
    } else {
        Err(Error::new_runtime("starts_with() called on non-string", *span))
    }
}

fn string_ends_with(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.is_empty() {
            return Err(Error::new_runtime(
                "ends_with() requires a suffix argument", 
                *span
            ));
        }
        
        let suffix = match &args[0] {
            Value::String(p) => p,
            _ => return Err(Error::new_runtime(
                "ends_with suffix must be a string", 
                *span
            )),
        };
        
        Ok(Value::Bool(s.ends_with(suffix)))
    } else {
        Err(Error::new_runtime("ends_with() called on non-string", *span))
    }
}

fn string_replace(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.len() != 2 {
            return Err(Error::new_runtime(
                "replace() requires exactly 2 arguments", 
                *span
            ));
        }
        
        let from = match &args[0] {
            Value::String(f) => f,
            _ => return Err(Error::new_runtime(
                "replace 'from' must be a string", 
                *span
            )),
        };
        
        let to = match &args[1] {
            Value::String(t) => t,
            _ => return Err(Error::new_runtime(
                "replace 'to' must be a string", 
                *span
            )),
        };
        
        Ok(Value::String(s.replace(from, to)))
    } else {
        Err(Error::new_runtime("replace() called on non-string", *span))
    }
}

// Array method implementations

fn array_len(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        Ok(Value::Int(arr.len() as i64))
    } else {
        Err(Error::new_runtime("len() called on non-array", *_span))
    }
}

fn array_first(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        if let Some(first) = arr.first() {
            Ok(first.clone())
        } else {
            // Return None for empty array - would need Option type
            Ok(Value::Unit)
        }
    } else {
        Err(Error::new_runtime("first() called on non-array", *_span))
    }
}

fn array_last(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        if let Some(last) = arr.last() {
            Ok(last.clone())
        } else {
            // Return None for empty array - would need Option type
            Ok(Value::Unit)
        }
    } else {
        Err(Error::new_runtime("last() called on non-array", *_span))
    }
}

fn array_get(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        if args.is_empty() {
            return Err(Error::new_runtime(
                "get() requires an index argument", 
                *span
            ));
        }
        
        let index = match &args[0] {
            Value::Int(i) => *i as usize,
            _ => return Err(Error::new_runtime(
                "get index must be an integer", 
                *span
            )),
        };
        
        if let Some(elem) = arr.get(index) {
            Ok(elem.clone())
        } else {
            // Return None for out of bounds - would need Option type
            Ok(Value::Unit)
        }
    } else {
        Err(Error::new_runtime("get() called on non-array", *span))
    }
}

fn array_slice(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        if args.len() != 2 {
            return Err(Error::new_runtime(
                "slice() requires exactly 2 arguments", 
                *span
            ));
        }
        
        let start = match &args[0] {
            Value::Int(i) => *i as usize,
            _ => return Err(Error::new_runtime(
                "slice start must be an integer", 
                *span
            )),
        };
        
        let end = match &args[1] {
            Value::Int(i) => *i as usize,
            _ => return Err(Error::new_runtime(
                "slice end must be an integer", 
                *span
            )),
        };
        
        let slice: Vec<Value> = arr.iter()
            .skip(start)
            .take(end.saturating_sub(start))
            .cloned()
            .collect();
        Ok(Value::Array(slice))
    } else {
        Err(Error::new_runtime("slice() called on non-array", *span))
    }
}

fn array_concat(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::Array(arr1) = value {
        if args.is_empty() {
            return Err(Error::new_runtime(
                "concat() requires an array argument", 
                *span
            ));
        }
        
        if let Value::Array(arr2) = &args[0] {
            let mut result = arr1.clone();
            result.extend(arr2.clone());
            Ok(Value::Array(result))
        } else {
            Err(Error::new_runtime(
                "concat argument must be an array", 
                *span
            ))
        }
    } else {
        Err(Error::new_runtime("concat() called on non-array", *span))
    }
}

fn array_join(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        if args.is_empty() {
            return Err(Error::new_runtime(
                "join() requires a separator argument", 
                *span
            ));
        }
        
        let separator = match &args[0] {
            Value::String(s) => s,
            _ => return Err(Error::new_runtime(
                "join separator must be a string", 
                *span
            )),
        };
        
        let strings: Vec<String> = arr.iter()
            .map(|v| v.to_string())
            .collect();
        Ok(Value::String(strings.join(separator)))
    } else {
        Err(Error::new_runtime("join() called on non-array", *span))
    }
}

fn array_contains(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        if args.is_empty() {
            return Err(Error::new_runtime(
                "contains() requires a value argument", 
                *span
            ));
        }
        
        Ok(Value::Bool(arr.contains(&args[0])))
    } else {
        Err(Error::new_runtime("contains() called on non-array", *span))
    }
}

// TODO: Implement these with proper closure/function value support
fn array_map(_value: &Value, _args: &[Value], span: &Span) -> Result<Value> {
    Err(Error::new_runtime(
        "map() not yet implemented - requires closure support", 
        *span
    ))
}

fn array_filter(_value: &Value, _args: &[Value], span: &Span) -> Result<Value> {
    Err(Error::new_runtime(
        "filter() not yet implemented - requires closure support", 
        *span
    ))
}

// Transpiler method registry implementation

impl TranspilerMethodRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            string_methods: HashMap::new(),
            array_methods: HashMap::new(),
        };
        
        registry.register_string_methods();
        registry.register_array_methods();
        
        registry
    }
    
    fn register_string_methods(&mut self) {
        self.string_methods.insert("len", transpile_string_len);
        self.string_methods.insert("trim", transpile_string_trim);
        self.string_methods.insert("chars", transpile_string_chars);
        self.string_methods.insert("substring", transpile_string_substring);
        self.string_methods.insert("split", transpile_string_split);
        self.string_methods.insert("toLowerCase", transpile_string_to_lowercase);
        self.string_methods.insert("toUpperCase", transpile_string_to_uppercase);
        self.string_methods.insert("contains", transpile_string_contains);
        self.string_methods.insert("starts_with", transpile_string_starts_with);
        self.string_methods.insert("ends_with", transpile_string_ends_with);
        self.string_methods.insert("replace", transpile_string_replace);
    }
    
    fn register_array_methods(&mut self) {
        self.array_methods.insert("len", transpile_array_len);
        self.array_methods.insert("first", transpile_array_first);
        self.array_methods.insert("last", transpile_array_last);
        self.array_methods.insert("get", transpile_array_get);
        self.array_methods.insert("slice", transpile_array_slice);
        self.array_methods.insert("concat", transpile_array_concat);
        self.array_methods.insert("join", transpile_array_join);
        self.array_methods.insert("contains", transpile_array_contains);
        self.array_methods.insert("push", transpile_array_push);
        self.array_methods.insert("pop", transpile_array_pop);
        // map and filter need closure support
    }
    
    pub fn get_string_method(&self, name: &str) -> Option<&TranspilerImpl> {
        self.string_methods.get(name)
    }
    
    pub fn get_array_method(&self, name: &str) -> Option<&TranspilerImpl> {
        self.array_methods.get(name)
    }
}

// Transpiler implementations for string methods

fn transpile_string_len(obj: &str, _args: &[String]) -> String {
    format!("{}.length", obj)
}

fn transpile_string_trim(obj: &str, _args: &[String]) -> String {
    format!("{}.trim()", obj)
}

fn transpile_string_chars(obj: &str, _args: &[String]) -> String {
    // Array.from() properly handles Unicode surrogate pairs
    format!("Array.from({})", obj)
}

fn transpile_string_substring(obj: &str, args: &[String]) -> String {
    format!("{}.substring({})", obj, args.join(", "))
}

fn transpile_string_split(obj: &str, args: &[String]) -> String {
    format!("{}.split({})", obj, args.join(", "))
}

fn transpile_string_to_lowercase(obj: &str, _args: &[String]) -> String {
    format!("{}.toLowerCase()", obj)
}

fn transpile_string_to_uppercase(obj: &str, _args: &[String]) -> String {
    format!("{}.toUpperCase()", obj)
}

fn transpile_string_contains(obj: &str, args: &[String]) -> String {
    format!("{}.includes({})", obj, args.join(", "))
}

fn transpile_string_starts_with(obj: &str, args: &[String]) -> String {
    format!("{}.startsWith({})", obj, args.join(", "))
}

fn transpile_string_ends_with(obj: &str, args: &[String]) -> String {
    format!("{}.endsWith({})", obj, args.join(", "))
}

fn transpile_string_replace(obj: &str, args: &[String]) -> String {
    format!("{}.replaceAll({})", obj, args.join(", "))
}

// Transpiler implementations for array methods

fn transpile_array_len(obj: &str, _args: &[String]) -> String {
    format!("{}.length", obj)
}

fn transpile_array_first(obj: &str, _args: &[String]) -> String {
    // Should return Option<T> but for now just returns the element
    format!("{}[0]", obj)
}

fn transpile_array_last(obj: &str, _args: &[String]) -> String {
    // Should return Option<T> but for now just returns the element
    format!("{}[{}.length - 1]", obj, obj)
}

fn transpile_array_get(obj: &str, args: &[String]) -> String {
    // Should return Option<T> but for now just returns the element
    format!("{}[{}]", obj, args[0])
}

fn transpile_array_slice(obj: &str, args: &[String]) -> String {
    format!("{}.slice({})", obj, args.join(", "))
}

fn transpile_array_concat(obj: &str, args: &[String]) -> String {
    format!("{}.concat({})", obj, args.join(", "))
}

fn transpile_array_join(obj: &str, args: &[String]) -> String {
    format!("{}.join({})", obj, args.join(", "))
}

fn transpile_array_contains(obj: &str, args: &[String]) -> String {
    format!("{}.includes({})", obj, args.join(", "))
}

fn transpile_array_push(obj: &str, args: &[String]) -> String {
    // In JS, push returns the new length, but in Husk it returns void
    format!("void ({}.push({}))", obj, args.join(", "))
}

fn transpile_array_pop(obj: &str, _args: &[String]) -> String {
    // Should return Option<T> but for now just returns the element
    format!("{}.pop()", obj)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_string_chars() {
        let s = Value::String("hello".to_string());
        let result = string_chars(&s, &[], &Span::default()).unwrap();
        
        if let Value::Array(chars) = result {
            assert_eq!(chars.len(), 5);
            assert_eq!(chars[0], Value::String("h".to_string()));
            assert_eq!(chars[4], Value::String("o".to_string()));
        } else {
            panic!("Expected array result");
        }
    }
    
    #[test]
    fn test_string_chars_unicode() {
        let s = Value::String("👋🌍".to_string());
        let result = string_chars(&s, &[], &Span::default()).unwrap();
        
        if let Value::Array(chars) = result {
            assert_eq!(chars.len(), 2);
            assert_eq!(chars[0], Value::String("👋".to_string()));
            assert_eq!(chars[1], Value::String("🌍".to_string()));
        } else {
            panic!("Expected array result");
        }
    }
}