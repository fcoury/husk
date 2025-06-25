use crate::{
    error::{Error, Result},
    interpreter::Value,
    span::Span,
    types::Type,
};
use std::collections::HashMap;

/// Type for built-in method implementations in interpreter
pub type MethodImpl = fn(&Value, &[Value], &Span) -> Result<Value>;

/// Type for transpiler code generation
pub type TranspilerImpl = fn(&str, &[String]) -> String;

/// Method signature for type checking
#[derive(Clone, Debug)]
pub struct MethodSignature {
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

/// Registry for built-in methods on primitive types
pub struct MethodRegistry {
    string_methods: HashMap<&'static str, MethodImpl>,
    array_methods: HashMap<&'static str, MethodImpl>,
}

/// Registry for method signatures used by semantic analyzer
pub struct MethodSignatureRegistry {
    string_methods: HashMap<&'static str, MethodSignature>,
    array_methods: HashMap<&'static str, MethodSignature>,
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
        self.string_methods.insert("is_empty", string_is_empty);
        self.string_methods.insert("trim", string_trim);
        self.string_methods.insert("chars", string_chars);
        self.string_methods.insert("substring", string_substring);
        self.string_methods.insert("split", string_split);
        self.string_methods
            .insert("to_lowercase", string_to_lowercase);
        self.string_methods
            .insert("to_uppercase", string_to_uppercase);
        self.string_methods.insert("contains", string_contains);
        self.string_methods
            .insert("starts_with", string_starts_with);
        self.string_methods.insert("ends_with", string_ends_with);
        self.string_methods.insert("replace", string_replace);
        self.string_methods.insert("find", string_find);
        self.string_methods.insert("rfind", string_rfind);
        self.string_methods.insert("bytes", string_bytes);
        self.string_methods.insert("trim_start", string_trim_start);
        self.string_methods.insert("trim_end", string_trim_end);
    }

    fn register_array_methods(&mut self) {
        self.array_methods.insert("len", array_len);
        self.array_methods.insert("is_empty", array_is_empty);
        self.array_methods.insert("first", array_first);
        self.array_methods.insert("last", array_last);
        self.array_methods.insert("get", array_get);
        self.array_methods.insert("slice", array_slice);
        self.array_methods.insert("concat", array_concat);
        self.array_methods.insert("join", array_join);
        self.array_methods.insert("contains", array_contains);
        self.array_methods.insert("reverse", array_reverse);
        self.array_methods.insert("push", array_push);
        self.array_methods.insert("pop", array_pop);
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

fn string_is_empty(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        Ok(Value::Bool(s.is_empty()))
    } else {
        Err(Error::new_runtime(
            "is_empty() called on non-string",
            *_span,
        ))
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
        let chars: Vec<Value> = s.chars().map(|c| Value::String(c.to_string())).collect();
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
                *span,
            ));
        }

        let start = match &args[0] {
            Value::Int(i) => *i as usize,
            _ => {
                return Err(Error::new_runtime(
                    "substring start must be an integer",
                    *span,
                ))
            }
        };

        let end = match &args[1] {
            Value::Int(i) => *i as usize,
            _ => {
                return Err(Error::new_runtime(
                    "substring end must be an integer",
                    *span,
                ))
            }
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
        Err(Error::new_runtime(
            "substring() called on non-string",
            *span,
        ))
    }
}

fn string_split(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.is_empty() {
            return Err(Error::new_runtime(
                "split() requires a delimiter argument",
                *span,
            ));
        }

        let delimiter = match &args[0] {
            Value::String(d) => d,
            _ => {
                return Err(Error::new_runtime(
                    "split delimiter must be a string",
                    *span,
                ))
            }
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
        Err(Error::new_runtime(
            "toLowerCase() called on non-string",
            *_span,
        ))
    }
}

fn string_to_uppercase(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        Ok(Value::String(s.to_uppercase()))
    } else {
        Err(Error::new_runtime(
            "toUpperCase() called on non-string",
            *_span,
        ))
    }
}

fn string_contains(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.is_empty() {
            return Err(Error::new_runtime(
                "contains() requires a pattern argument",
                *span,
            ));
        }

        let pattern = match &args[0] {
            Value::String(p) => p,
            _ => {
                return Err(Error::new_runtime(
                    "contains pattern must be a string",
                    *span,
                ))
            }
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
                *span,
            ));
        }

        let prefix = match &args[0] {
            Value::String(p) => p,
            _ => {
                return Err(Error::new_runtime(
                    "starts_with prefix must be a string",
                    *span,
                ))
            }
        };

        Ok(Value::Bool(s.starts_with(prefix)))
    } else {
        Err(Error::new_runtime(
            "starts_with() called on non-string",
            *span,
        ))
    }
}

fn string_ends_with(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.is_empty() {
            return Err(Error::new_runtime(
                "ends_with() requires a suffix argument",
                *span,
            ));
        }

        let suffix = match &args[0] {
            Value::String(p) => p,
            _ => {
                return Err(Error::new_runtime(
                    "ends_with suffix must be a string",
                    *span,
                ))
            }
        };

        Ok(Value::Bool(s.ends_with(suffix)))
    } else {
        Err(Error::new_runtime(
            "ends_with() called on non-string",
            *span,
        ))
    }
}

fn string_replace(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.len() != 2 {
            return Err(Error::new_runtime(
                "replace() requires exactly 2 arguments",
                *span,
            ));
        }

        let from = match &args[0] {
            Value::String(f) => f,
            _ => return Err(Error::new_runtime("replace 'from' must be a string", *span)),
        };

        let to = match &args[1] {
            Value::String(t) => t,
            _ => return Err(Error::new_runtime("replace 'to' must be a string", *span)),
        };

        Ok(Value::String(s.replace(from, to)))
    } else {
        Err(Error::new_runtime("replace() called on non-string", *span))
    }
}

fn string_find(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.len() != 1 {
            return Err(Error::new_runtime(
                "find() requires exactly 1 argument",
                *span,
            ));
        }

        let search = match &args[0] {
            Value::String(search_str) => search_str,
            _ => return Err(Error::new_runtime("find() pattern must be a string", *span)),
        };

        // Convert to char indices for proper Unicode handling
        let chars: Vec<char> = s.chars().collect();
        let search_chars: Vec<char> = search.chars().collect();

        if search_chars.is_empty() {
            return Ok(make_some(Value::Int(0)));
        }

        // Search from the beginning
        for i in 0..chars.len() {
            if i + search_chars.len() > chars.len() {
                break;
            }

            let mut matches = true;
            for j in 0..search_chars.len() {
                if chars[i + j] != search_chars[j] {
                    matches = false;
                    break;
                }
            }

            if matches {
                return Ok(make_some(Value::Int(i as i64)));
            }
        }

        Ok(make_none())
    } else {
        Err(Error::new_runtime("find() called on non-string", *span))
    }
}

fn string_rfind(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.len() != 1 {
            return Err(Error::new_runtime(
                "rfind() requires exactly 1 argument",
                *span,
            ));
        }

        let search = match &args[0] {
            Value::String(search_str) => search_str,
            _ => {
                return Err(Error::new_runtime(
                    "rfind() pattern must be a string",
                    *span,
                ))
            }
        };

        // Convert to char indices for proper Unicode handling
        let chars: Vec<char> = s.chars().collect();
        let search_chars: Vec<char> = search.chars().collect();

        if search_chars.is_empty() {
            return Ok(make_some(Value::Int(chars.len() as i64)));
        }

        // Search backwards from the end
        let end_pos = chars.len().saturating_sub(search_chars.len());

        for i in (0..=end_pos).rev() {
            let mut matches = true;
            for j in 0..search_chars.len() {
                if chars[i + j] != search_chars[j] {
                    matches = false;
                    break;
                }
            }

            if matches {
                return Ok(make_some(Value::Int(i as i64)));
            }
        }

        Ok(make_none())
    } else {
        Err(Error::new_runtime("rfind() called on non-string", *span))
    }
}

fn string_bytes(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        // Convert string to UTF-8 bytes
        let bytes: Vec<Value> = s.bytes().map(|b| Value::Int(b as i64)).collect();
        Ok(Value::Array(bytes))
    } else {
        Err(Error::new_runtime("bytes() called on non-string", *_span))
    }
}

fn string_trim_start(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        Ok(Value::String(s.trim_start().to_string()))
    } else {
        Err(Error::new_runtime(
            "trim_start() called on non-string",
            *_span,
        ))
    }
}

fn string_trim_end(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        Ok(Value::String(s.trim_end().to_string()))
    } else {
        Err(Error::new_runtime(
            "trim_end() called on non-string",
            *_span,
        ))
    }
}

// Helper functions for Option types

fn make_some(value: Value) -> Value {
    Value::EnumVariant(
        "Option".to_string(),
        "Some".to_string(),
        Some(Box::new(value)),
    )
}

fn make_none() -> Value {
    Value::EnumVariant("Option".to_string(), "None".to_string(), None)
}

// Array method implementations

fn array_len(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        Ok(Value::Int(arr.len() as i64))
    } else {
        Err(Error::new_runtime("len() called on non-array", *_span))
    }
}

fn array_is_empty(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        Ok(Value::Bool(arr.is_empty()))
    } else {
        Err(Error::new_runtime("is_empty() called on non-array", *_span))
    }
}

fn array_first(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        if let Some(first) = arr.first() {
            Ok(make_some(first.clone()))
        } else {
            Ok(make_none())
        }
    } else {
        Err(Error::new_runtime("first() called on non-array", *_span))
    }
}

fn array_last(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        if let Some(last) = arr.last() {
            Ok(make_some(last.clone()))
        } else {
            Ok(make_none())
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
                *span,
            ));
        }

        let index = match &args[0] {
            Value::Int(i) => *i as usize,
            _ => return Err(Error::new_runtime("get index must be an integer", *span)),
        };

        if let Some(elem) = arr.get(index) {
            Ok(make_some(elem.clone()))
        } else {
            Ok(make_none())
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
                *span,
            ));
        }

        let start = match &args[0] {
            Value::Int(i) => *i as usize,
            _ => return Err(Error::new_runtime("slice start must be an integer", *span)),
        };

        let end = match &args[1] {
            Value::Int(i) => *i as usize,
            _ => return Err(Error::new_runtime("slice end must be an integer", *span)),
        };

        let slice: Vec<Value> = arr
            .iter()
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
                *span,
            ));
        }

        if let Value::Array(arr2) = &args[0] {
            let mut result = arr1.clone();
            result.extend(arr2.clone());
            Ok(Value::Array(result))
        } else {
            Err(Error::new_runtime(
                "concat argument must be an array",
                *span,
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
                *span,
            ));
        }

        let separator = match &args[0] {
            Value::String(s) => s,
            _ => return Err(Error::new_runtime("join separator must be a string", *span)),
        };

        let strings: Vec<String> = arr.iter().map(|v| v.to_string()).collect();
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
                *span,
            ));
        }

        Ok(Value::Bool(arr.contains(&args[0])))
    } else {
        Err(Error::new_runtime("contains() called on non-array", *span))
    }
}

fn array_reverse(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        let mut reversed = arr.clone();
        reversed.reverse();
        Ok(Value::Array(reversed))
    } else {
        Err(Error::new_runtime("reverse() called on non-array", *_span))
    }
}

fn array_push(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        if args.is_empty() {
            return Err(Error::new_runtime(
                "push() requires at least one argument",
                *span,
            ));
        }

        let mut new_arr = arr.clone();
        for arg in args {
            new_arr.push(arg.clone());
        }
        Ok(Value::Array(new_arr))
    } else {
        Err(Error::new_runtime("push() called on non-array", *span))
    }
}

fn array_pop(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        if arr.is_empty() {
            Ok(make_none())
        } else {
            let mut new_arr = arr.clone();
            let popped = new_arr.pop().unwrap();
            // Return tuple of (new_array, popped_value)
            Ok(Value::Tuple(vec![Value::Array(new_arr), make_some(popped)]))
        }
    } else {
        Err(Error::new_runtime("pop() called on non-array", *_span))
    }
}

// TODO: Implement these with proper closure/function value support
fn array_map(_value: &Value, _args: &[Value], span: &Span) -> Result<Value> {
    Err(Error::new_runtime(
        "map() not yet implemented - requires closure support",
        *span,
    ))
}

fn array_filter(_value: &Value, _args: &[Value], span: &Span) -> Result<Value> {
    Err(Error::new_runtime(
        "filter() not yet implemented - requires closure support",
        *span,
    ))
}

// Method signature registry implementation

impl MethodSignatureRegistry {
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
        // No parameters methods
        self.string_methods.insert(
            "len",
            MethodSignature {
                param_types: vec![],
                return_type: Type::Int,
            },
        );
        self.string_methods.insert(
            "is_empty",
            MethodSignature {
                param_types: vec![],
                return_type: Type::Bool,
            },
        );
        self.string_methods.insert(
            "trim",
            MethodSignature {
                param_types: vec![],
                return_type: Type::String,
            },
        );
        self.string_methods.insert(
            "chars",
            MethodSignature {
                param_types: vec![],
                return_type: Type::Array(Box::new(Type::String)),
            },
        );
        self.string_methods.insert(
            "bytes",
            MethodSignature {
                param_types: vec![],
                return_type: Type::Array(Box::new(Type::Int)), // Using Int to represent u8
            },
        );
        self.string_methods.insert(
            "trim_start",
            MethodSignature {
                param_types: vec![],
                return_type: Type::String,
            },
        );
        self.string_methods.insert(
            "trim_end",
            MethodSignature {
                param_types: vec![],
                return_type: Type::String,
            },
        );
        self.string_methods.insert(
            "to_lowercase",
            MethodSignature {
                param_types: vec![],
                return_type: Type::String,
            },
        );
        self.string_methods.insert(
            "to_uppercase",
            MethodSignature {
                param_types: vec![],
                return_type: Type::String,
            },
        );

        // Single string parameter methods
        self.string_methods.insert(
            "split",
            MethodSignature {
                param_types: vec![Type::String],
                return_type: Type::Array(Box::new(Type::String)),
            },
        );
        self.string_methods.insert(
            "contains",
            MethodSignature {
                param_types: vec![Type::String],
                return_type: Type::Bool,
            },
        );
        self.string_methods.insert(
            "starts_with",
            MethodSignature {
                param_types: vec![Type::String],
                return_type: Type::Bool,
            },
        );
        self.string_methods.insert(
            "ends_with",
            MethodSignature {
                param_types: vec![Type::String],
                return_type: Type::Bool,
            },
        );

        // Two int parameters
        self.string_methods.insert(
            "substring",
            MethodSignature {
                param_types: vec![Type::Int, Type::Int],
                return_type: Type::String,
            },
        );

        // Two string parameters
        self.string_methods.insert(
            "replace",
            MethodSignature {
                param_types: vec![Type::String, Type::String],
                return_type: Type::String,
            },
        );

        // Methods with optional second parameter (find, rfind)
        self.string_methods.insert(
            "find",
            MethodSignature {
                param_types: vec![Type::String], // Optional second Int parameter
                return_type: Type::Unknown,      // Should be Option<usize>
            },
        );
        self.string_methods.insert(
            "rfind",
            MethodSignature {
                param_types: vec![Type::String], // Optional second Int parameter
                return_type: Type::Unknown,      // Should be Option<usize>
            },
        );
    }

    fn register_array_methods(&mut self) {
        // No parameters methods
        self.array_methods.insert(
            "len",
            MethodSignature {
                param_types: vec![],
                return_type: Type::Int,
            },
        );
        self.array_methods.insert(
            "is_empty",
            MethodSignature {
                param_types: vec![],
                return_type: Type::Bool,
            },
        );
        self.array_methods.insert(
            "first",
            MethodSignature {
                param_types: vec![],
                return_type: Type::Unknown, // Should be Option<T>
            },
        );
        self.array_methods.insert(
            "last",
            MethodSignature {
                param_types: vec![],
                return_type: Type::Unknown, // Should be Option<T>
            },
        );
        self.array_methods.insert(
            "reverse",
            MethodSignature {
                param_types: vec![],
                return_type: Type::Unknown, // Should be Array<T>
            },
        );
        self.array_methods.insert(
            "pop",
            MethodSignature {
                param_types: vec![],
                return_type: Type::Unknown, // Should be (Array<T>, Option<T>)
            },
        );

        // Single parameter methods
        self.array_methods.insert(
            "get",
            MethodSignature {
                param_types: vec![Type::Int],
                return_type: Type::Unknown, // Should be Option<T>
            },
        );
        self.array_methods.insert(
            "contains",
            MethodSignature {
                param_types: vec![Type::Unknown], // Should be T
                return_type: Type::Bool,
            },
        );
        self.array_methods.insert(
            "join",
            MethodSignature {
                param_types: vec![Type::String],
                return_type: Type::String,
            },
        );

        // Two int parameters
        self.array_methods.insert(
            "slice",
            MethodSignature {
                param_types: vec![Type::Int, Type::Int],
                return_type: Type::Unknown, // Should be Array<T>
            },
        );

        // Variable parameters
        self.array_methods.insert(
            "push",
            MethodSignature {
                param_types: vec![Type::Unknown], // Should be T, varargs
                return_type: Type::Unknown,       // Should be Array<T>
            },
        );
        self.array_methods.insert(
            "concat",
            MethodSignature {
                param_types: vec![Type::Unknown], // Should be Array<T>
                return_type: Type::Unknown,       // Should be Array<T>
            },
        );
    }

    pub fn get_string_method(&self, name: &str) -> Option<&MethodSignature> {
        self.string_methods.get(name)
    }

    pub fn get_array_method(&self, name: &str) -> Option<&MethodSignature> {
        self.array_methods.get(name)
    }
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
        self.string_methods
            .insert("is_empty", transpile_string_is_empty);
        self.string_methods.insert("trim", transpile_string_trim);
        self.string_methods.insert("chars", transpile_string_chars);
        self.string_methods.insert("bytes", transpile_string_bytes);
        self.string_methods
            .insert("substring", transpile_string_substring);
        self.string_methods.insert("split", transpile_string_split);
        self.string_methods
            .insert("to_lowercase", transpile_string_to_lowercase);
        self.string_methods
            .insert("to_uppercase", transpile_string_to_uppercase);
        self.string_methods
            .insert("contains", transpile_string_contains);
        self.string_methods
            .insert("starts_with", transpile_string_starts_with);
        self.string_methods
            .insert("ends_with", transpile_string_ends_with);
        self.string_methods
            .insert("replace", transpile_string_replace);
        self.string_methods.insert("find", transpile_string_find);
        self.string_methods.insert("rfind", transpile_string_rfind);
        self.string_methods
            .insert("trim_start", transpile_string_trim_start);
        self.string_methods
            .insert("trim_end", transpile_string_trim_end);
    }

    fn register_array_methods(&mut self) {
        self.array_methods.insert("len", transpile_array_len);
        self.array_methods
            .insert("is_empty", transpile_array_is_empty);
        self.array_methods.insert("first", transpile_array_first);
        self.array_methods.insert("last", transpile_array_last);
        self.array_methods.insert("get", transpile_array_get);
        self.array_methods.insert("slice", transpile_array_slice);
        self.array_methods.insert("concat", transpile_array_concat);
        self.array_methods.insert("join", transpile_array_join);
        self.array_methods
            .insert("contains", transpile_array_contains);
        self.array_methods
            .insert("reverse", transpile_array_reverse);
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

fn transpile_string_is_empty(obj: &str, _args: &[String]) -> String {
    format!("({}.length === 0)", obj)
}

fn transpile_string_trim(obj: &str, _args: &[String]) -> String {
    format!("{}.trim()", obj)
}

fn transpile_string_chars(obj: &str, _args: &[String]) -> String {
    // Array.from() properly handles Unicode surrogate pairs
    format!("Array.from({})", obj)
}

fn transpile_string_bytes(obj: &str, _args: &[String]) -> String {
    // Use TextEncoder to get UTF-8 bytes, then convert to regular array
    format!("Array.from(new TextEncoder().encode({}))", obj)
}

fn transpile_string_trim_start(obj: &str, _args: &[String]) -> String {
    format!("{}.trimStart()", obj)
}

fn transpile_string_trim_end(obj: &str, _args: &[String]) -> String {
    format!("{}.trimEnd()", obj)
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

fn transpile_string_find(obj: &str, args: &[String]) -> String {
    // Convert JS indexOf (-1 for not found) to Option<usize>
    format!(
        "(function() {{ const idx = {}.indexOf({}); return idx >= 0 ? {{ type: 'Some', value: idx }} : {{ type: 'None' }}; }})()",
        obj, args.join(", ")
    )
}

fn transpile_string_rfind(obj: &str, args: &[String]) -> String {
    // Convert JS lastIndexOf (-1 for not found) to Option<usize>
    format!(
        "(function() {{ const idx = {}.lastIndexOf({}); return idx >= 0 ? {{ type: 'Some', value: idx }} : {{ type: 'None' }}; }})()",
        obj, args.join(", ")
    )
}

// Transpiler implementations for array methods

fn transpile_array_len(obj: &str, _args: &[String]) -> String {
    format!("{}.length", obj)
}

fn transpile_array_is_empty(obj: &str, _args: &[String]) -> String {
    format!("{}.length === 0", obj)
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

fn transpile_array_reverse(obj: &str, _args: &[String]) -> String {
    // Returns a new reversed array (doesn't mutate original)
    format!("[...{}].reverse()", obj)
}

fn transpile_array_push(obj: &str, args: &[String]) -> String {
    // Returns a new array with pushed elements (doesn't mutate original)
    format!("[...{}, {}]", obj, args.join(", "))
}

fn transpile_array_pop(obj: &str, _args: &[String]) -> String {
    // Should return tuple of (new_array, Option<popped>) but for now just returns the element
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
