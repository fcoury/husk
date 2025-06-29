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
        self.string_methods.insert("splitn", string_splitn);
        self.string_methods.insert("split_once", string_split_once);
        self.string_methods.insert("lines", string_lines);
        self.string_methods.insert("slice", string_slice);
        self.string_methods.insert("char_at", string_char_at);
        self.string_methods.insert("repeat", string_repeat);
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
        self.array_methods.insert("sort", array_sort);
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

fn string_splitn(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.len() != 2 {
            return Err(Error::new_runtime(
                "splitn() requires exactly 2 arguments",
                *span,
            ));
        }

        let n = match &args[0] {
            Value::Int(i) => {
                if *i < 0 {
                    return Err(Error::new_runtime(
                        "splitn() count must be non-negative",
                        *span,
                    ));
                }
                *i as usize
            }
            _ => {
                return Err(Error::new_runtime(
                    "splitn() count must be an integer",
                    *span,
                ))
            }
        };

        let separator = match &args[1] {
            Value::String(sep) => sep,
            _ => {
                return Err(Error::new_runtime(
                    "splitn() separator must be a string",
                    *span,
                ))
            }
        };

        let parts: Vec<Value> = s
            .splitn(n, separator)
            .map(|s| Value::String(s.to_string()))
            .collect();

        Ok(Value::Array(parts))
    } else {
        Err(Error::new_runtime("splitn() called on non-string", *span))
    }
}

fn string_split_once(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.len() != 1 {
            return Err(Error::new_runtime(
                "split_once() requires exactly 1 argument",
                *span,
            ));
        }

        let separator = match &args[0] {
            Value::String(sep) => sep,
            _ => {
                return Err(Error::new_runtime(
                    "split_once() separator must be a string",
                    *span,
                ))
            }
        };

        if let Some((before, after)) = s.split_once(separator) {
            // Create a tuple with two string values
            let tuple = Value::Tuple(vec![
                Value::String(before.to_string()),
                Value::String(after.to_string()),
            ]);
            Ok(make_some(tuple))
        } else {
            Ok(make_none())
        }
    } else {
        Err(Error::new_runtime(
            "split_once() called on non-string",
            *span,
        ))
    }
}

fn string_lines(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        // Split by newlines, handling different line ending styles
        let lines: Vec<Value> = s
            .lines()
            .map(|line| Value::String(line.to_string()))
            .collect();
        Ok(Value::Array(lines))
    } else {
        Err(Error::new_runtime("lines() called on non-string", *_span))
    }
}

fn string_slice(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.len() != 2 {
            return Err(Error::new_runtime(
                "slice() requires exactly 2 arguments",
                *span,
            ));
        }

        let len = s.chars().count() as i64;

        // Get start index, handling negative indices
        let start = match &args[0] {
            Value::Int(i) => {
                if *i < 0 {
                    // Negative index counts from end
                    (len + i).max(0) as usize
                } else {
                    (*i as usize).min(len as usize)
                }
            }
            _ => {
                return Err(Error::new_runtime(
                    "slice() start must be an integer",
                    *span,
                ))
            }
        };

        // Get end index, handling negative indices
        let end = match &args[1] {
            Value::Int(i) => {
                if *i < 0 {
                    // Negative index counts from end
                    (len + i).max(0) as usize
                } else {
                    (*i as usize).min(len as usize)
                }
            }
            _ => return Err(Error::new_runtime("slice() end must be an integer", *span)),
        };

        // If start is greater than end, return empty string
        if start >= end {
            return Ok(Value::String(String::new()));
        }

        // Convert to char indices and extract substring
        let chars: Vec<char> = s.chars().collect();
        let result: String = chars[start..end].iter().collect();
        Ok(Value::String(result))
    } else {
        Err(Error::new_runtime("slice() called on non-string", *span))
    }
}

fn string_char_at(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.len() != 1 {
            return Err(Error::new_runtime(
                "char_at() requires exactly 1 argument",
                *span,
            ));
        }

        let index = match &args[0] {
            Value::Int(i) => *i,
            _ => {
                return Err(Error::new_runtime(
                    "char_at() index must be an integer",
                    *span,
                ))
            }
        };

        let chars: Vec<char> = s.chars().collect();
        let len = chars.len() as i64;

        // Handle negative indices
        let actual_index = if index < 0 { len + index } else { index };

        // Check bounds
        if actual_index < 0 || actual_index >= len {
            Ok(make_none())
        } else {
            let ch = chars[actual_index as usize];
            Ok(make_some(Value::String(ch.to_string())))
        }
    } else {
        Err(Error::new_runtime("char_at() called on non-string", *span))
    }
}

fn string_repeat(value: &Value, args: &[Value], span: &Span) -> Result<Value> {
    if let Value::String(s) = value {
        if args.len() != 1 {
            return Err(Error::new_runtime(
                "repeat() requires exactly 1 argument",
                *span,
            ));
        }

        let count = match &args[0] {
            Value::Int(n) => {
                if *n < 0 {
                    return Err(Error::new_runtime(
                        "repeat() count cannot be negative",
                        *span,
                    ));
                }
                *n as usize
            }
            _ => {
                return Err(Error::new_runtime(
                    "repeat() count must be an integer",
                    *span,
                ))
            }
        };

        Ok(Value::String(s.repeat(count)))
    } else {
        Err(Error::new_runtime("repeat() called on non-string", *span))
    }
}

// Helper functions for Option types

fn make_some(value: Value) -> Value {
    Value::EnumVariant("Option".to_string(), "Some".to_string(), vec![value])
}

fn make_none() -> Value {
    Value::EnumVariant("Option".to_string(), "None".to_string(), vec![])
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
// Note: array_map and array_filter are implemented directly in the interpreter
// to access closure calling functionality. See InterpreterVisitor::array_map()
// and InterpreterVisitor::array_filter() in interpreter.rs

fn array_map(_value: &Value, _args: &[Value], span: &Span) -> Result<Value> {
    // This should never be called since map() is handled directly in the interpreter
    Err(Error::new_runtime(
        "map() should be handled by interpreter, not method registry",
        *span,
    ))
}

fn array_filter(_value: &Value, _args: &[Value], span: &Span) -> Result<Value> {
    // This should never be called since filter() is handled directly in the interpreter
    Err(Error::new_runtime(
        "filter() should be handled by interpreter, not method registry",
        *span,
    ))
}

fn array_sort(value: &Value, _args: &[Value], _span: &Span) -> Result<Value> {
    if let Value::Array(arr) = value {
        let mut sorted_arr = arr.clone();

        // Sort the array based on the type of elements
        sorted_arr.sort_by(|a, b| {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => a.cmp(b),
                (Value::Float(a), Value::Float(b)) => {
                    a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
                }
                (Value::String(a), Value::String(b)) => a.cmp(b),
                (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
                // For mixed types, convert to string and compare
                _ => format!("{a:?}").cmp(&format!("{b:?}")),
            }
        });

        Ok(Value::Array(sorted_arr))
    } else {
        Err(Error::new_runtime("sort() called on non-array", *_span))
    }
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
            "splitn",
            MethodSignature {
                param_types: vec![Type::Int, Type::String],
                return_type: Type::Array(Box::new(Type::String)),
            },
        );
        self.string_methods.insert(
            "split_once",
            MethodSignature {
                param_types: vec![Type::String],
                return_type: Type::Unknown, // Should be Option<(String, String)>
            },
        );
        self.string_methods.insert(
            "lines",
            MethodSignature {
                param_types: vec![],
                return_type: Type::Array(Box::new(Type::String)),
            },
        );
        self.string_methods.insert(
            "slice",
            MethodSignature {
                param_types: vec![Type::Int, Type::Int],
                return_type: Type::String,
            },
        );
        self.string_methods.insert(
            "char_at",
            MethodSignature {
                param_types: vec![Type::Int],
                return_type: Type::Unknown, // Option<String> - no Option type yet
            },
        );
        self.string_methods.insert(
            "repeat",
            MethodSignature {
                param_types: vec![Type::Int],
                return_type: Type::String,
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
        self.array_methods.insert(
            "sort",
            MethodSignature {
                param_types: vec![],
                return_type: Type::Unknown, // Should be Array<T>
            },
        );
        self.array_methods.insert(
            "map",
            MethodSignature {
                param_types: vec![Type::Unknown], // Should be fn(T) -> U
                return_type: Type::Unknown,       // Should be Array<U>
            },
        );
        self.array_methods.insert(
            "filter",
            MethodSignature {
                param_types: vec![Type::Unknown], // Should be fn(T) -> bool
                return_type: Type::Unknown,       // Should be Array<T>
            },
        );
        self.array_methods.insert(
            "find",
            MethodSignature {
                param_types: vec![Type::Unknown], // Should be fn(T) -> bool
                return_type: Type::Unknown,       // Should be Option<T>
            },
        );
        self.array_methods.insert(
            "position",
            MethodSignature {
                param_types: vec![Type::Unknown], // Should be fn(T) -> bool
                return_type: Type::Unknown,       // Should be Option<usize>
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
        self.string_methods
            .insert("splitn", transpile_string_splitn);
        self.string_methods
            .insert("split_once", transpile_string_split_once);
        self.string_methods.insert("lines", transpile_string_lines);
        self.string_methods.insert("slice", transpile_string_slice);
        self.string_methods
            .insert("char_at", transpile_string_char_at);
        self.string_methods
            .insert("repeat", transpile_string_repeat);
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
        self.array_methods.insert("sort", transpile_array_sort);
        self.array_methods.insert("map", transpile_array_map);
        self.array_methods.insert("filter", transpile_array_filter);
        self.array_methods.insert("find", transpile_array_find);
        self.array_methods
            .insert("position", transpile_array_position);
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
    format!("{obj}.length")
}

fn transpile_string_is_empty(obj: &str, _args: &[String]) -> String {
    format!("({obj}.length === 0)")
}

fn transpile_string_trim(obj: &str, _args: &[String]) -> String {
    format!("{obj}.trim()")
}

fn transpile_string_chars(obj: &str, _args: &[String]) -> String {
    // Array.from() properly handles Unicode surrogate pairs
    format!("Array.from({obj})")
}

fn transpile_string_bytes(obj: &str, _args: &[String]) -> String {
    // Use TextEncoder to get UTF-8 bytes, then convert to regular array
    format!("Array.from(new TextEncoder().encode({obj}))")
}

fn transpile_string_trim_start(obj: &str, _args: &[String]) -> String {
    format!("{obj}.trimStart()")
}

fn transpile_string_trim_end(obj: &str, _args: &[String]) -> String {
    format!("{obj}.trimEnd()")
}

fn transpile_string_splitn(obj: &str, args: &[String]) -> String {
    // JavaScript doesn't have a direct splitn equivalent, so we need to implement it
    // args[0] is the limit, args[1] is the separator
    if args.len() >= 2 {
        format!(
            "(function() {{ const limit = {}; if (limit <= 0) return []; if (limit === 1) return [{}]; const parts = {}.split({}); if (parts.length <= limit) return parts; return parts.slice(0, limit - 1).concat([parts.slice(limit - 1).join({})]); }})()",
            &args[0], obj, obj, &args[1], &args[1]
        )
    } else {
        // Fallback to regular split if args are incorrect
        format!("{obj}.split('')")
    }
}

fn transpile_string_split_once(obj: &str, args: &[String]) -> String {
    // JavaScript doesn't have split_once, so we need to implement it
    if !args.is_empty() {
        format!(
            "(function() {{ const idx = {}.indexOf({}); return idx === -1 ? {{ type: 'None' }} : {{ type: 'Some', value: {{ '0': {}.substring(0, idx), '1': {}.substring(idx + {}.length) }} }}; }})()",
            obj, &args[0], obj, obj, &args[0]
        )
    } else {
        // Fallback if no separator provided
        "{ type: 'None' }".to_string()
    }
}

fn transpile_string_lines(obj: &str, _args: &[String]) -> String {
    // Split by newline - handles \n, \r\n properly
    // Note: Rust's lines() removes the line terminators and doesn't include final empty line
    format!("{obj}.split(/\\r?\\n/).filter((line, i, arr) => i < arr.length - 1 || line !== '')")
}

fn transpile_string_slice(obj: &str, args: &[String]) -> String {
    // JavaScript's slice handles negative indices natively and works with UTF-16 code units
    // For proper Unicode support, we need to convert to array and back
    if args.len() >= 2 {
        format!(
            "Array.from({}).slice({}, {}).join('')",
            obj, &args[0], &args[1]
        )
    } else {
        format!("{obj}.slice()")
    }
}

fn transpile_string_char_at(obj: &str, args: &[String]) -> String {
    // JavaScript doesn't have a direct char_at that returns Option, so we implement it
    // Array.from() handles Unicode properly
    if !args.is_empty() {
        format!(
            "(function() {{ const chars = Array.from({}); const idx = {}; const len = chars.length; const actualIdx = idx < 0 ? len + idx : idx; return actualIdx >= 0 && actualIdx < len ? {{ type: 'Some', value: chars[actualIdx] }} : {{ type: 'None' }}; }})()",
            obj, &args[0]
        )
    } else {
        "{ type: 'None' }".to_string()
    }
}

fn transpile_string_repeat(obj: &str, args: &[String]) -> String {
    // JavaScript String.repeat() method
    if !args.is_empty() {
        format!("{}.repeat({})", obj, &args[0])
    } else {
        format!("{obj}.repeat(0)")
    }
}

fn transpile_string_substring(obj: &str, args: &[String]) -> String {
    format!("{}.substring({})", obj, args.join(", "))
}

fn transpile_string_split(obj: &str, args: &[String]) -> String {
    format!("{}.split({})", obj, args.join(", "))
}

fn transpile_string_to_lowercase(obj: &str, _args: &[String]) -> String {
    format!("{obj}.toLowerCase()")
}

fn transpile_string_to_uppercase(obj: &str, _args: &[String]) -> String {
    format!("{obj}.toUpperCase()")
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
    format!("{obj}.length")
}

fn transpile_array_is_empty(obj: &str, _args: &[String]) -> String {
    format!("{obj}.length === 0")
}

fn transpile_array_first(obj: &str, _args: &[String]) -> String {
    // Should return Option<T> but for now just returns the element
    format!("{obj}[0]")
}

fn transpile_array_last(obj: &str, _args: &[String]) -> String {
    // Should return Option<T> but for now just returns the element
    format!("{obj}[{obj}.length - 1]")
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
    format!("[...{obj}].reverse()")
}

fn transpile_array_push(obj: &str, args: &[String]) -> String {
    // Returns a new array with pushed elements (doesn't mutate original)
    format!("[...{}, {}]", obj, args.join(", "))
}

fn transpile_array_pop(obj: &str, _args: &[String]) -> String {
    // In Husk, arrays are immutable, so we need to clone before popping
    // Should return tuple of (new_array, Option<popped>) but for now just returns the element
    format!("[...{obj}].pop()")
}

fn transpile_array_sort(obj: &str, _args: &[String]) -> String {
    // JavaScript Array.sort() with custom comparison for mixed types
    format!(
        "[...{obj}].sort((a, b) => {{
            if (typeof a === typeof b) {{
                if (typeof a === 'number') return a - b;
                if (typeof a === 'string') return a.localeCompare(b);
                if (typeof a === 'boolean') return a - b;
                return String(a).localeCompare(String(b));
            }}
            return String(a).localeCompare(String(b));
        }})"
    )
}

fn transpile_array_map(obj: &str, args: &[String]) -> String {
    // JavaScript Array.map() with closure support
    if !args.is_empty() {
        format!("{}.map({})", obj, &args[0])
    } else {
        format!("{obj}.map(x => x)") // Identity map as fallback
    }
}

fn transpile_array_filter(obj: &str, args: &[String]) -> String {
    // JavaScript Array.filter() with closure support
    if !args.is_empty() {
        format!("{}.filter({})", obj, &args[0])
    } else {
        format!("{obj}.filter(x => true)") // No-op filter as fallback
    }
}

fn transpile_array_find(obj: &str, args: &[String]) -> String {
    // JavaScript Array.find() returns undefined instead of Option, so we wrap it
    if !args.is_empty() {
        format!(
            "(function() {{ const result = {}.find({}); return result !== undefined ? {{ type: 'Some', value: result }} : {{ type: 'None' }}; }})()",
            obj, &args[0]
        )
    } else {
        "{ type: 'None' }".to_string() // No predicate means no match
    }
}

fn transpile_array_position(obj: &str, args: &[String]) -> String {
    // JavaScript Array.findIndex() returns -1 instead of Option, so we wrap it
    if !args.is_empty() {
        format!(
            "(function() {{ const result = {}.findIndex({}); return result !== -1 ? {{ type: 'Some', value: result }} : {{ type: 'None' }}; }})()",
            obj, &args[0]
        )
    } else {
        "{ type: 'None' }".to_string() // No predicate means no match
    }
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
