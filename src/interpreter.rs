use std::collections::HashMap;
use std::fmt;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use indexmap::IndexMap;

use crate::{
    ast::visitor::AstVisitor,
    builtin_methods::MethodRegistry,
    error::{Error, Result},
    io as husk_io,
    lexer::Lexer,
    parser::{Expr, ExternItem, Operator, Parser, Stmt, UnaryOp, UseItems, UsePath, UsePrefix},
    span::Span,
};

#[derive(Debug, PartialEq)]
pub enum ControlFlow {
    Normal,
    Break,
    Continue,
    #[allow(dead_code)]
    Return(Value),
}

#[derive(Clone, Debug)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Array(Vec<Value>),
    Tuple(Vec<Value>),
    Range(Option<i64>, Option<i64>, bool),
    Function(Function),
    Struct(String, IndexMap<String, String>),
    Enum(String, IndexMap<String, String>),
    StructInstance(String, IndexMap<String, Value>),
    EnumVariant(String, String, Option<Box<Value>>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Array(elements) => {
                let elements_str: Vec<String> = elements.iter().map(|e| e.to_string()).collect();
                write!(f, "[{}]", elements_str.join(", "))
            }
            Value::Tuple(elements) => {
                let elements_str: Vec<String> = elements.iter().map(|e| e.to_string()).collect();
                write!(f, "({})", elements_str.join(", "))
            }
            Value::Range(start, end, inclusive) => {
                let start_str = start
                    .as_ref()
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "None".to_string());
                let end_str = end
                    .as_ref()
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "None".to_string());
                write!(
                    f,
                    "{}..{}{}",
                    start_str,
                    end_str,
                    if *inclusive { "=" } else { "" }
                )
            }
            Value::Function(_) => write!(f, "function"),
            Value::Struct(name, _) => write!(f, "struct {}", name),
            Value::StructInstance(name, fields) => {
                let mut field_strings = Vec::new();
                for (name, value) in fields {
                    field_strings.push(format!("{}: {:?}", name, value));
                }
                write!(f, "struct {} {{{}}}", name, field_strings.join(", "))
            }
            Value::Enum(name, _) => write!(f, "enum {}", name),
            Value::EnumVariant(name, variant, value) => match value {
                Some(value) => write!(f, "{}::{}({})", name, variant, value),
                None => write!(f, "{}::{}", name, variant),
            },
        }
    }
}

impl Value {
    pub fn type_str(&self) -> String {
        match self {
            Value::Unit => "void".to_string(),
            Value::Int(_) => "int".to_string(),
            Value::Float(_) => "float".to_string(),
            Value::Bool(_) => "bool".to_string(),
            Value::String(_) => "string".to_string(),
            Value::Array(_) => "array".to_string(),
            Value::Tuple(_) => "tuple".to_string(),
            Value::Range(_, _, _) => "range".to_string(),
            Value::Function(_) => "function".to_string(),
            Value::Struct(name, _) => format!("struct {name}"),
            Value::StructInstance(name, _) => format!("struct instance {name}"),
            Value::Enum(name, _) => format!("enum {name}"),
            Value::EnumVariant(name, variant, value) => match value {
                Some(value) => format!("{name}::{variant}({value})"),
                None => format!("{name}::{variant}"),
            },
        }
    }

    pub fn as_mut_instance(&mut self) -> Option<&mut IndexMap<String, Value>> {
        match self {
            Value::StructInstance(_, fields) => Some(fields),
            _ => None,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Unit, Value::Unit) => true,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Function(_), Value::Function(_)) => false, // Functions are not comparable
            (Value::Struct(a, _), Value::Struct(b, _)) => a == b,
            (Value::StructInstance(a_name, a_values), Value::StructInstance(b_name, b_values)) => {
                a_name == b_name && a_values == b_values
            }
            (
                Value::Range(a_start, a_end, a_inclusive),
                Value::Range(b_start, b_end, b_inclusive),
            ) => a_start == b_start && a_end == b_end && a_inclusive == b_inclusive,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Function {
    UserDefined(
        String,
        Vec<(String, String)>,
        Vec<Stmt>,
        IndexMap<String, Value>,
    ),
    BuiltIn(fn(&[Value]) -> Result<Value>),
    Closure {
        params: Vec<String>,
        body: Expr,
        captured_env: IndexMap<String, Value>,
        span: Span,
    },
}

// Note: print and println are now implemented as macros, not functions
// These functions are kept commented for reference
/*
pub fn stdlib_print(args: &[Value]) -> Result<Value> {
    for arg in args {
        print!("{}", arg);
    }
    io::stdout()
        .flush()
        .map_err(|e| Error::new_runtime(format!("IO error: {}", e), Span::default()))?;
    Ok(Value::Int(0)) // print returns 0 on success
}

pub fn stdlib_println(args: &[Value]) -> Result<Value> {
    for arg in args {
        match arg {
            Value::EnumVariant(_, _, Some(value)) => print!("{}", value),
            _ => print!("{}", arg),
        }
    }
    println!();
    Ok(Value::Unit)
}
*/

pub fn stdlib_format(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::new_runtime(
            "format! requires at least one argument (the format string)".to_string(),
            Span::default(),
        ));
    }

    // First argument must be a string
    let format_str = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "format! first argument must be a string".to_string(),
                Span::default(),
            ))
        }
    };

    let mut result = String::new();
    let mut arg_index = 1;
    let mut chars = format_str.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '{' {
            if chars.peek() == Some(&'{') {
                // Escaped {{
                chars.next();
                result.push('{');
            } else if chars.peek() == Some(&'}') {
                // Placeholder {}
                chars.next();
                if arg_index >= args.len() {
                    return Err(Error::new_runtime(
                        format!("format! expects {} arguments after format string, but {} were provided",
                               arg_index - 1, args.len() - 1),
                        Span::default(),
                    ));
                }
                result.push_str(&args[arg_index].to_string());
                arg_index += 1;
            } else {
                result.push(ch);
            }
        } else if ch == '}' {
            if chars.peek() == Some(&'}') {
                // Escaped }}
                chars.next();
                result.push('}');
            } else {
                result.push(ch);
            }
        } else {
            result.push(ch);
        }
    }

    Ok(Value::String(result))
}

// IO function wrappers
pub fn stdlib_read_file(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "read_file requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "read_file path must be a string",
                Span::default(),
            ))
        }
    };

    husk_io::read_file(path, &Span::default())
}

pub fn stdlib_read_file_bytes(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "read_file_bytes requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "read_file_bytes path must be a string",
                Span::default(),
            ))
        }
    };

    husk_io::read_file_bytes(path, &Span::default())
}

pub fn stdlib_read_lines(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "read_lines requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "read_lines path must be a string",
                Span::default(),
            ))
        }
    };

    husk_io::read_lines(path, &Span::default())
}

pub fn stdlib_write_file(args: &[Value]) -> Result<Value> {
    if args.len() != 2 {
        return Err(Error::new_runtime(
            "write_file requires exactly 2 arguments (path, contents)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "write_file path must be a string",
                Span::default(),
            ))
        }
    };

    let contents = match &args[1] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "write_file contents must be a string",
                Span::default(),
            ))
        }
    };

    husk_io::write_file(path, contents, &Span::default())
}

pub fn stdlib_write_file_bytes(args: &[Value]) -> Result<Value> {
    if args.len() != 2 {
        return Err(Error::new_runtime(
            "write_file_bytes requires exactly 2 arguments (path, data)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "write_file_bytes path must be a string",
                Span::default(),
            ))
        }
    };

    let data = match &args[1] {
        Value::Array(arr) => arr,
        _ => {
            return Err(Error::new_runtime(
                "write_file_bytes data must be an array",
                Span::default(),
            ))
        }
    };

    husk_io::write_file_bytes(path, data, &Span::default())
}

pub fn stdlib_append_file(args: &[Value]) -> Result<Value> {
    if args.len() != 2 {
        return Err(Error::new_runtime(
            "append_file requires exactly 2 arguments (path, contents)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "append_file path must be a string",
                Span::default(),
            ))
        }
    };

    let contents = match &args[1] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "append_file contents must be a string",
                Span::default(),
            ))
        }
    };

    husk_io::append_file(path, contents, &Span::default())
}

pub fn stdlib_exists(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "exists requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "exists path must be a string",
                Span::default(),
            ))
        }
    };

    Ok(husk_io::exists(path))
}

pub fn stdlib_is_file(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "is_file requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "is_file path must be a string",
                Span::default(),
            ))
        }
    };

    Ok(husk_io::is_file(path))
}

pub fn stdlib_is_dir(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "is_dir requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "is_dir path must be a string",
                Span::default(),
            ))
        }
    };

    Ok(husk_io::is_dir(path))
}

pub fn stdlib_create_dir(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "create_dir requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "create_dir path must be a string",
                Span::default(),
            ))
        }
    };

    // Convert Rust Result to Husk Result enum variant
    match husk_io::create_dir(path, &Span::default()) {
        Ok(value) => Ok(Value::EnumVariant(
            "Result".to_string(),
            "Ok".to_string(),
            Some(Box::new(value)),
        )),
        Err(error) => Ok(Value::EnumVariant(
            "Result".to_string(),
            "Err".to_string(),
            Some(Box::new(Value::String(error.to_string()))),
        )),
    }
}

pub fn stdlib_create_dir_all(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "create_dir_all requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "create_dir_all path must be a string",
                Span::default(),
            ))
        }
    };

    // Convert Rust Result to Husk Result enum variant
    match husk_io::create_dir_all(path, &Span::default()) {
        Ok(value) => Ok(Value::EnumVariant(
            "Result".to_string(),
            "Ok".to_string(),
            Some(Box::new(value)),
        )),
        Err(error) => Ok(Value::EnumVariant(
            "Result".to_string(),
            "Err".to_string(),
            Some(Box::new(Value::String(error.to_string()))),
        )),
    }
}

pub fn stdlib_remove_dir(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "remove_dir requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "remove_dir path must be a string",
                Span::default(),
            ))
        }
    };

    // Convert Rust Result to Husk Result enum variant
    match husk_io::remove_dir(path, &Span::default()) {
        Ok(value) => Ok(Value::EnumVariant(
            "Result".to_string(),
            "Ok".to_string(),
            Some(Box::new(value)),
        )),
        Err(error) => Ok(Value::EnumVariant(
            "Result".to_string(),
            "Err".to_string(),
            Some(Box::new(Value::String(error.to_string()))),
        )),
    }
}

pub fn stdlib_remove_dir_all(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "remove_dir_all requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "remove_dir_all path must be a string",
                Span::default(),
            ))
        }
    };

    // Convert Rust Result to Husk Result enum variant
    match husk_io::remove_dir_all(path, &Span::default()) {
        Ok(value) => Ok(Value::EnumVariant(
            "Result".to_string(),
            "Ok".to_string(),
            Some(Box::new(value)),
        )),
        Err(error) => Ok(Value::EnumVariant(
            "Result".to_string(),
            "Err".to_string(),
            Some(Box::new(Value::String(error.to_string()))),
        )),
    }
}

pub fn stdlib_read_dir(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "read_dir requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "read_dir path must be a string",
                Span::default(),
            ))
        }
    };

    // Convert Rust Result to Husk Result enum variant
    match husk_io::read_dir(path, &Span::default()) {
        Ok(value) => Ok(Value::EnumVariant(
            "Result".to_string(),
            "Ok".to_string(),
            Some(Box::new(value)),
        )),
        Err(error) => Ok(Value::EnumVariant(
            "Result".to_string(),
            "Err".to_string(),
            Some(Box::new(Value::String(error.to_string()))),
        )),
    }
}

// Console IO function wrappers
pub fn stdlib_read_line(args: &[Value]) -> Result<Value> {
    if !args.is_empty() {
        return Err(Error::new_runtime(
            "read_line requires no arguments",
            Span::default(),
        ));
    }

    // Convert Rust Result to Husk Result enum variant
    match husk_io::read_line(&Span::default()) {
        Ok(value) => Ok(Value::EnumVariant(
            "Result".to_string(),
            "Ok".to_string(),
            Some(Box::new(value)),
        )),
        Err(error) => Ok(Value::EnumVariant(
            "Result".to_string(),
            "Err".to_string(),
            Some(Box::new(Value::String(error.to_string()))),
        )),
    }
}

// Async file operation wrappers
pub fn stdlib_read_file_async(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "read_file_async requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "read_file_async path must be a string",
                Span::default(),
            ))
        }
    };

    husk_io::read_file_async(path, &Span::default())
}

pub fn stdlib_read_file_bytes_async(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "read_file_bytes_async requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "read_file_bytes_async path must be a string",
                Span::default(),
            ))
        }
    };

    husk_io::read_file_bytes_async(path, &Span::default())
}

pub fn stdlib_read_lines_async(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(Error::new_runtime(
            "read_lines_async requires exactly 1 argument (path)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "read_lines_async path must be a string",
                Span::default(),
            ))
        }
    };

    husk_io::read_lines_async(path, &Span::default())
}

pub fn stdlib_write_file_async(args: &[Value]) -> Result<Value> {
    if args.len() != 2 {
        return Err(Error::new_runtime(
            "write_file_async requires exactly 2 arguments (path, contents)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "write_file_async path must be a string",
                Span::default(),
            ))
        }
    };

    let contents = match &args[1] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "write_file_async contents must be a string",
                Span::default(),
            ))
        }
    };

    husk_io::write_file_async(path, contents, &Span::default())
}

pub fn stdlib_write_file_bytes_async(args: &[Value]) -> Result<Value> {
    if args.len() != 2 {
        return Err(Error::new_runtime(
            "write_file_bytes_async requires exactly 2 arguments (path, data)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "write_file_bytes_async path must be a string",
                Span::default(),
            ))
        }
    };

    let data = match &args[1] {
        Value::Array(arr) => arr,
        _ => {
            return Err(Error::new_runtime(
                "write_file_bytes_async data must be an array",
                Span::default(),
            ))
        }
    };

    husk_io::write_file_bytes_async(path, data, &Span::default())
}

pub fn stdlib_append_file_async(args: &[Value]) -> Result<Value> {
    if args.len() != 2 {
        return Err(Error::new_runtime(
            "append_file_async requires exactly 2 arguments (path, contents)",
            Span::default(),
        ));
    }

    let path = match &args[0] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "append_file_async path must be a string",
                Span::default(),
            ))
        }
    };

    let contents = match &args[1] {
        Value::String(s) => s,
        _ => {
            return Err(Error::new_runtime(
                "append_file_async contents must be a string",
                Span::default(),
            ))
        }
    };

    husk_io::append_file_async(path, contents, &Span::default())
}

/// Represents a loaded module with its exports
#[derive(Clone, Debug)]
pub struct Module {
    pub _path: PathBuf,
    pub exports: IndexMap<String, Value>,
}

/// Interpreter implemented using the visitor pattern
pub struct InterpreterVisitor {
    environment: IndexMap<String, Value>,
    global_environment: IndexMap<String, Value>,
    control_flow: ControlFlow,
    module_cache: HashMap<PathBuf, Module>,
    current_file: Option<PathBuf>,
    project_root: Option<PathBuf>,
    // Track exports for the current module being executed
    current_exports: IndexMap<String, Value>,
    // Track whether we're at the top level (for export collection)
    is_top_level: bool,
    // Track the variable name of 'self' in method contexts
    self_var_name: Option<String>,
    // Registry for built-in methods
    method_registry: MethodRegistry,
}

impl Default for InterpreterVisitor {
    fn default() -> Self {
        Self::new()
    }
}

impl InterpreterVisitor {
    pub fn new() -> Self {
        let mut visitor = InterpreterVisitor {
            environment: IndexMap::new(),
            global_environment: IndexMap::new(),
            control_flow: ControlFlow::Normal,
            module_cache: HashMap::new(),
            current_file: None,
            project_root: None,
            current_exports: IndexMap::new(),
            is_top_level: true,
            self_var_name: None,
            method_registry: MethodRegistry::new(),
        };
        visitor.init_standard_library();
        visitor
    }

    pub fn with_context(current_file: Option<PathBuf>, project_root: Option<PathBuf>) -> Self {
        let mut visitor = InterpreterVisitor {
            environment: IndexMap::new(),
            global_environment: IndexMap::new(),
            control_flow: ControlFlow::Normal,
            module_cache: HashMap::new(),
            current_file,
            project_root,
            current_exports: IndexMap::new(),
            is_top_level: true,
            self_var_name: None,
            method_registry: MethodRegistry::new(),
        };
        visitor.init_standard_library();
        visitor
    }

    fn init_standard_library(&mut self) {
        // Note: print and println are now macros, not functions
        // Only format! remains as a function for backward compatibility
        self.global_environment.insert(
            "format!".to_string(),
            Value::Function(Function::BuiltIn(stdlib_format)),
        );

        // Register built-in Option enum
        let mut option_variants = IndexMap::new();
        option_variants.insert("Some".to_string(), "T".to_string());
        option_variants.insert("None".to_string(), "".to_string());
        self.global_environment.insert(
            "Option".to_string(),
            Value::Enum("Option".to_string(), option_variants),
        );

        // Register built-in Result enum
        let mut result_variants = IndexMap::new();
        result_variants.insert("Ok".to_string(), "T".to_string());
        result_variants.insert("Err".to_string(), "E".to_string());
        self.global_environment.insert(
            "Result".to_string(),
            Value::Enum("Result".to_string(), result_variants),
        );

        // Register IO functions
        self.global_environment.insert(
            "read_file".to_string(),
            Value::Function(Function::BuiltIn(stdlib_read_file)),
        );
        self.global_environment.insert(
            "read_file_bytes".to_string(),
            Value::Function(Function::BuiltIn(stdlib_read_file_bytes)),
        );
        self.global_environment.insert(
            "read_lines".to_string(),
            Value::Function(Function::BuiltIn(stdlib_read_lines)),
        );
        self.global_environment.insert(
            "write_file".to_string(),
            Value::Function(Function::BuiltIn(stdlib_write_file)),
        );
        self.global_environment.insert(
            "write_file_bytes".to_string(),
            Value::Function(Function::BuiltIn(stdlib_write_file_bytes)),
        );
        self.global_environment.insert(
            "append_file".to_string(),
            Value::Function(Function::BuiltIn(stdlib_append_file)),
        );
        self.global_environment.insert(
            "exists".to_string(),
            Value::Function(Function::BuiltIn(stdlib_exists)),
        );
        self.global_environment.insert(
            "is_file".to_string(),
            Value::Function(Function::BuiltIn(stdlib_is_file)),
        );
        self.global_environment.insert(
            "is_dir".to_string(),
            Value::Function(Function::BuiltIn(stdlib_is_dir)),
        );
        self.global_environment.insert(
            "create_dir".to_string(),
            Value::Function(Function::BuiltIn(stdlib_create_dir)),
        );
        self.global_environment.insert(
            "create_dir_all".to_string(),
            Value::Function(Function::BuiltIn(stdlib_create_dir_all)),
        );
        self.global_environment.insert(
            "remove_dir".to_string(),
            Value::Function(Function::BuiltIn(stdlib_remove_dir)),
        );
        self.global_environment.insert(
            "remove_dir_all".to_string(),
            Value::Function(Function::BuiltIn(stdlib_remove_dir_all)),
        );
        self.global_environment.insert(
            "read_dir".to_string(),
            Value::Function(Function::BuiltIn(stdlib_read_dir)),
        );

        // Register console IO functions
        self.global_environment.insert(
            "read_line".to_string(),
            Value::Function(Function::BuiltIn(stdlib_read_line)),
        );

        // Register async file operation functions
        self.global_environment.insert(
            "read_file_async".to_string(),
            Value::Function(Function::BuiltIn(stdlib_read_file_async)),
        );
        self.global_environment.insert(
            "read_file_bytes_async".to_string(),
            Value::Function(Function::BuiltIn(stdlib_read_file_bytes_async)),
        );
        self.global_environment.insert(
            "read_lines_async".to_string(),
            Value::Function(Function::BuiltIn(stdlib_read_lines_async)),
        );
        self.global_environment.insert(
            "write_file_async".to_string(),
            Value::Function(Function::BuiltIn(stdlib_write_file_async)),
        );
        self.global_environment.insert(
            "write_file_bytes_async".to_string(),
            Value::Function(Function::BuiltIn(stdlib_write_file_bytes_async)),
        );
        self.global_environment.insert(
            "append_file_async".to_string(),
            Value::Function(Function::BuiltIn(stdlib_append_file_async)),
        );
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<Value> {
        // First pass: collect all type and function definitions for forward declarations
        self.collect_definitions(stmts)?;

        // Second pass: execute all statements
        let mut last_value = Value::Unit;
        for stmt in stmts {
            last_value = self.visit_stmt(stmt)?;

            // Check for early return
            match &self.control_flow {
                ControlFlow::Return(value) => return Ok(value.clone()),
                ControlFlow::Break | ControlFlow::Continue => {
                    return Err(Error::new_runtime(
                        "Break or continue outside of loop".to_string(),
                        Span::default(),
                    ));
                }
                ControlFlow::Normal => {}
            }
        }
        Ok(last_value)
    }

    /// First pass: collect all type and function definitions without executing them
    /// This enables forward declarations for both types and functions
    fn collect_definitions(&mut self, stmts: &[Stmt]) -> Result<()> {
        // First collect all type definitions (enums and structs)
        for stmt in stmts {
            match stmt {
                Stmt::Enum(name, generic_params, variants, span) => {
                    // Process enum definition immediately
                    self.visit_enum(name, generic_params, variants, span)?;
                }
                Stmt::Struct(name, generic_params, fields, span) => {
                    // Process struct definition immediately
                    self.visit_struct(name, generic_params, fields, span)?;
                }
                _ => {} // Skip other statements in this pass
            }
        }

        // Then collect all function definitions
        for stmt in stmts {
            match stmt {
                Stmt::Function(_, name, _generic_params, params, _return_type, body, _span) => {
                    // For recursive functions, we need to create a closure that includes the function itself
                    // Step 1: Create closure that includes current environment
                    let mut closure = self.global_environment.clone();

                    // Step 2: Create a temporary function with current closure
                    let temp_func = Value::Function(Function::UserDefined(
                        name.to_string(),
                        params.to_vec(),
                        body.to_vec(),
                        closure.clone(),
                    ));

                    // Step 3: Add this function to the closure for recursion
                    closure.insert(name.to_string(), temp_func);

                    // Step 4: Create the final function with closure that includes itself
                    let final_func = Value::Function(Function::UserDefined(
                        name.to_string(),
                        params.to_vec(),
                        body.to_vec(),
                        closure,
                    ));

                    // Step 5: Register the function in the global environment so it's always accessible
                    self.global_environment
                        .insert(name.to_string(), final_func.clone());

                    // Step 6: If we're at the top level, add to exports
                    if self.is_top_level {
                        self.current_exports.insert(name.to_string(), final_func);
                    }
                }
                Stmt::Impl(struct_name, methods, _span) => {
                    // Process methods in impl blocks
                    for method in methods {
                        if let Stmt::Function(
                            _,
                            method_name,
                            _generic_params,
                            params,
                            _return_type,
                            body,
                            _method_span,
                        ) = method
                        {
                            // Create full method name with struct prefix
                            let full_method_name = format!("{}::{}", struct_name, method_name);

                            // Create closure for the method
                            let mut closure = self.global_environment.clone();

                            // Create temporary function
                            let temp_func = Value::Function(Function::UserDefined(
                                full_method_name.to_string(),
                                params.to_vec(),
                                body.to_vec(),
                                closure.clone(),
                            ));

                            // Add function to its own closure for recursion
                            closure.insert(full_method_name.to_string(), temp_func.clone());

                            // Create final function with updated closure
                            let final_func = Value::Function(Function::UserDefined(
                                full_method_name.to_string(),
                                params.to_vec(),
                                body.to_vec(),
                                closure,
                            ));

                            // Register the method in global environment
                            self.global_environment
                                .insert(full_method_name.to_string(), final_func.clone());

                            // If we're at the top level, add to exports
                            if self.is_top_level {
                                self.current_exports.insert(full_method_name, final_func);
                            }
                        }
                    }
                }
                _ => {} // Skip other statements
            }
        }
        Ok(())
    }

    /// Visit a list of statements without creating a new scope
    /// Used for if-else blocks that should share the same scope as their parent
    fn visit_statements_no_scope(&mut self, stmts: &[Stmt]) -> Result<Value> {
        let mut result_value = Value::Unit;

        // Visit all statements
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;

            match stmt {
                // If the last statement is an expression without semicolon,
                // return that expression's value
                Stmt::Expression(expr, false) if is_last => {
                    result_value = self.visit_expr(expr)?;
                }
                // Otherwise, just visit the statement normally
                _ => {
                    self.visit_stmt(stmt)?;
                }
            }

            // Check for control flow changes
            match self.control_flow {
                ControlFlow::Break | ControlFlow::Continue | ControlFlow::Return(_) => {
                    return Ok(Value::Unit);
                }
                ControlFlow::Normal => {}
            }
        }

        Ok(result_value)
    }

    /// Get a variable from the environment
    fn get_var(&self, name: &str) -> Option<Value> {
        self.environment
            .get(name)
            .or_else(|| self.global_environment.get(name))
            .cloned()
    }

    /// Set a variable in the environment
    fn set_var(&mut self, name: String, value: Value) {
        self.environment.insert(name, value);
    }

    /// Push a new scope
    fn push_scope(&mut self) -> IndexMap<String, Value> {
        std::mem::take(&mut self.environment)
    }

    /// Pop a scope
    fn pop_scope(&mut self, old_env: IndexMap<String, Value>) {
        self.environment = old_env;
    }

    /// Resolve a module path based on the use prefix
    fn resolve_module_path(&self, path: &UsePath, span: &Span) -> Result<PathBuf> {
        let base_path = path.segments.join("/");

        let base_dir = match path.prefix {
            UsePrefix::Local => {
                // local:: - from project root
                match &self.project_root {
                    Some(root) => root.clone(),
                    None => {
                        return Err(Error::new_runtime(
                            "Cannot use local:: imports without project root context".to_string(),
                            *span,
                        ))
                    }
                }
            }
            UsePrefix::Self_ => {
                // self:: - from current file's directory
                match &self.current_file {
                    Some(current) => {
                        let parent = current.parent().ok_or_else(|| {
                            Error::new_runtime(
                                "Current file has no parent directory".to_string(),
                                *span,
                            )
                        })?;
                        parent.to_path_buf()
                    }
                    None => {
                        return Err(Error::new_runtime(
                            "Cannot use self:: imports without current file context".to_string(),
                            *span,
                        ))
                    }
                }
            }
            UsePrefix::Super(count) => {
                // super:: - from parent directory
                match &self.current_file {
                    Some(current) => {
                        let mut parent = current.parent().ok_or_else(|| {
                            Error::new_runtime(
                                "Current file has no parent directory".to_string(),
                                *span,
                            )
                        })?;

                        // Go up 'count' directories
                        for _ in 0..count {
                            parent = parent.parent().ok_or_else(|| {
                                Error::new_runtime(
                                    "Too many super:: levels, reached filesystem root".to_string(),
                                    *span,
                                )
                            })?;
                        }

                        parent.to_path_buf()
                    }
                    None => {
                        return Err(Error::new_runtime(
                            "Cannot use super:: imports without current file context".to_string(),
                            *span,
                        ))
                    }
                }
            }
            UsePrefix::None => {
                // Should have been caught earlier
                return Err(Error::new_runtime(
                    "External packages not supported in interpreter".to_string(),
                    *span,
                ));
            }
        };

        // Try both .hk and .husk extensions
        let hk_path = base_dir.join(&base_path).with_extension("hk");
        let husk_path = base_dir.join(&base_path).with_extension("husk");

        if hk_path.exists() {
            Ok(hk_path)
        } else if husk_path.exists() {
            Ok(husk_path)
        } else {
            // Return the .hk path for the error message
            Ok(hk_path)
        }
    }

    /// Load a module from disk or cache
    fn load_module(&mut self, module_path: &Path, span: &Span) -> Result<Module> {
        // Check cache first
        if let Some(module) = self.module_cache.get(module_path) {
            return Ok(module.clone());
        }

        // Read the file
        let contents = std::fs::read_to_string(module_path).map_err(|e| {
            Error::new_runtime(
                format!("Failed to read module '{}': {}", module_path.display(), e),
                *span,
            )
        })?;

        // Parse the module
        let mut lexer = Lexer::new(contents);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse().map_err(|e| {
            Error::new_runtime(
                format!("Failed to parse module '{}': {}", module_path.display(), e),
                *span,
            )
        })?;

        // Create a new interpreter for the module with its context
        let mut module_interpreter = InterpreterVisitor::with_context(
            Some(module_path.to_path_buf()),
            self.project_root.clone(),
        );

        // Execute the module
        module_interpreter.interpret(&stmts)?;

        // Collect the exports from the module interpreter
        let exports = module_interpreter.current_exports;

        let module = Module {
            _path: module_path.to_path_buf(),
            exports,
        };

        // Cache the module
        self.module_cache
            .insert(module_path.to_path_buf(), module.clone());

        Ok(module)
    }

    /// Evaluate a binary operation
    fn evaluate_binary_op(
        _self: &Self,
        left: Value,
        op: &Operator,
        right: Value,
        span: Span,
    ) -> Result<Value> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => match op {
                Operator::Plus => Ok(Value::Int(a + b)),
                Operator::Minus => Ok(Value::Int(a - b)),
                Operator::Multiply => Ok(Value::Int(a * b)),
                Operator::Divide => {
                    if b == 0 {
                        Err(Error::new_runtime("Division by zero".to_string(), span))
                    } else {
                        Ok(Value::Int(a / b))
                    }
                }
                Operator::Modulo => {
                    if b == 0 {
                        Err(Error::new_runtime("Division by zero".to_string(), span))
                    } else {
                        Ok(Value::Int(a % b))
                    }
                }
                Operator::Equals => Ok(Value::Bool(a == b)),
                Operator::NotEquals => Ok(Value::Bool(a != b)),
                Operator::LessThan => Ok(Value::Bool(a < b)),
                Operator::GreaterThan => Ok(Value::Bool(a > b)),
                Operator::LessThanEquals => Ok(Value::Bool(a <= b)),
                Operator::GreaterThanEquals => Ok(Value::Bool(a >= b)),
                Operator::And | Operator::Or => Err(Error::new_runtime(
                    "Logical operators require boolean operands".to_string(),
                    span,
                )),
            },
            (Value::Float(a), Value::Float(b)) => match op {
                Operator::Plus => Ok(Value::Float(a + b)),
                Operator::Minus => Ok(Value::Float(a - b)),
                Operator::Multiply => Ok(Value::Float(a * b)),
                Operator::Divide => {
                    if b == 0.0 {
                        Err(Error::new_runtime("Division by zero".to_string(), span))
                    } else {
                        Ok(Value::Float(a / b))
                    }
                }
                Operator::Modulo => Ok(Value::Float(a % b)),
                Operator::Equals => Ok(Value::Bool(a == b)),
                Operator::NotEquals => Ok(Value::Bool(a != b)),
                Operator::LessThan => Ok(Value::Bool(a < b)),
                Operator::GreaterThan => Ok(Value::Bool(a > b)),
                Operator::LessThanEquals => Ok(Value::Bool(a <= b)),
                Operator::GreaterThanEquals => Ok(Value::Bool(a >= b)),
                Operator::And | Operator::Or => Err(Error::new_runtime(
                    "Logical operators require boolean operands".to_string(),
                    span,
                )),
            },
            (Value::Int(a), Value::Float(b)) => {
                Self::evaluate_binary_op(_self, Value::Float(a as f64), op, Value::Float(b), span)
            }
            (Value::Float(a), Value::Int(b)) => {
                Self::evaluate_binary_op(_self, Value::Float(a), op, Value::Float(b as f64), span)
            }
            (Value::Bool(a), Value::Bool(b)) => match op {
                Operator::Equals => Ok(Value::Bool(a == b)),
                Operator::NotEquals => Ok(Value::Bool(a != b)),
                Operator::And => Ok(Value::Bool(a && b)),
                Operator::Or => Ok(Value::Bool(a || b)),
                _ => Err(Error::new_runtime(
                    format!("Invalid operation {:?} for boolean values", op),
                    span,
                )),
            },
            (Value::String(a), Value::String(b)) => match op {
                Operator::Equals => Ok(Value::Bool(a == b)),
                Operator::NotEquals => Ok(Value::Bool(a != b)),
                _ => Err(Error::new_runtime(
                    format!("Invalid operation {:?} for string values", op),
                    span,
                )),
            },
            (
                Value::EnumVariant(type1, variant1, data1),
                Value::EnumVariant(type2, variant2, data2),
            ) => match op {
                Operator::Equals => Ok(Value::Bool(
                    type1 == type2 && variant1 == variant2 && data1 == data2,
                )),
                Operator::NotEquals => Ok(Value::Bool(
                    !(type1 == type2 && variant1 == variant2 && data1 == data2),
                )),
                _ => Err(Error::new_runtime(
                    format!("Invalid operation {:?} for enum values", op),
                    span,
                )),
            },
            _ => Err(Error::new_runtime(
                format!("Type mismatch for binary operation {:?}", op),
                span,
            )),
        }
    }

    /// Execute a function call
    fn execute_function_call(
        &mut self,
        func: Value,
        args: Vec<Value>,
        span: Span,
    ) -> Result<Value> {
        match func {
            Value::Function(Function::BuiltIn(func)) => func(&args),
            Value::Function(Function::UserDefined(_, params, body, closure)) => {
                if args.len() != params.len() {
                    return Err(Error::new_runtime(
                        format!("Expected {} arguments, got {}", params.len(), args.len()),
                        span,
                    ));
                }

                // Save current environment and use closure
                let saved_env = self.push_scope();
                // Instead of completely replacing environment, merge closure with current functions
                let mut new_env = closure.clone();
                // Preserve function definitions from current environment
                for (name, value) in &self.environment {
                    if let Value::Function(_) = value {
                        new_env.insert(name.clone(), value.clone());
                    }
                }
                self.environment = new_env;

                // Bind arguments
                let mut has_self_param = false;
                for ((name, _), value) in params.iter().zip(args.iter()) {
                    if name == "self" {
                        has_self_param = true;
                    }
                    self.set_var(name.clone(), value.clone());
                }

                // Execute function body
                let mut result = Value::Unit;
                for stmt in &body {
                    result = self.visit_stmt(stmt)?;

                    // Check for return
                    if let ControlFlow::Return(value) = &self.control_flow {
                        result = value.clone();
                        self.control_flow = ControlFlow::Normal;
                        break;
                    }
                }

                // If this method had a self parameter and we have a self_var_name,
                // update the original variable with the potentially modified self
                if has_self_param {
                    if let Some(self_var_name) = self.self_var_name.clone() {
                        if let Some(modified_self) = self.environment.get("self").cloned() {
                            // We need to update this in the parent scope after we restore
                            self.pop_scope(saved_env);
                            self.set_var(self_var_name, modified_self);
                            return Ok(result);
                        }
                    }
                }

                // Restore environment
                self.pop_scope(saved_env);
                Ok(result)
            }
            Value::Function(Function::Closure {
                params,
                body,
                captured_env,
                ..
            }) => {
                if args.len() != params.len() {
                    return Err(Error::new_runtime(
                        format!("Expected {} arguments, got {}", params.len(), args.len()),
                        span,
                    ));
                }

                // Save current environment and use captured environment
                let saved_env = self.push_scope();

                // Set up closure environment
                let mut new_env = captured_env.clone();
                // Preserve function definitions from current environment
                for (name, value) in &self.environment {
                    if let Value::Function(_) = value {
                        new_env.insert(name.clone(), value.clone());
                    }
                }
                self.environment = new_env;

                // Bind arguments
                for (param, value) in params.iter().zip(args.iter()) {
                    self.set_var(param.clone(), value.clone());
                }

                // Execute closure body (expression)
                let result = self.visit_expr(&body)?;

                // Restore environment
                self.pop_scope(saved_env);
                Ok(result)
            }
            _ => Err(Error::new_runtime(
                "Expected function value".to_string(),
                span,
            )),
        }
    }
}

impl AstVisitor<Value> for InterpreterVisitor {
    type Error = Error;

    // ===== Expression visit methods =====

    fn visit_int(&mut self, value: i64, _span: &Span) -> Result<Value> {
        Ok(Value::Int(value))
    }

    fn visit_float(&mut self, value: f64, _span: &Span) -> Result<Value> {
        Ok(Value::Float(value))
    }

    fn visit_bool(&mut self, value: bool, _span: &Span) -> Result<Value> {
        Ok(Value::Bool(value))
    }

    fn visit_unit(&mut self, _span: &Span) -> Result<Value> {
        Ok(Value::Unit)
    }

    fn visit_string(&mut self, value: &str, _span: &Span) -> Result<Value> {
        Ok(Value::String(value.to_string()))
    }

    fn visit_identifier(&mut self, name: &str, span: &Span) -> Result<Value> {
        self.get_var(name)
            .ok_or_else(|| Error::new_runtime(format!("Undefined variable: {}", name), *span))
    }

    fn visit_array(&mut self, elements: &[Expr], _span: &Span) -> Result<Value> {
        let mut values = Vec::new();
        for element in elements {
            values.push(self.visit_expr(element)?);
        }
        Ok(Value::Array(values))
    }

    fn visit_tuple(&mut self, elements: &[Expr], _span: &Span) -> Result<Value> {
        let mut values = Vec::new();
        for element in elements {
            values.push(self.visit_expr(element)?);
        }
        Ok(Value::Tuple(values))
    }

    fn visit_array_index(&mut self, array: &Expr, index: &Expr, span: &Span) -> Result<Value> {
        let array_value = self.visit_expr(array)?;
        let index_value = self.visit_expr(index)?;

        match (&array_value, &index_value) {
            (Value::Array(elements), Value::Int(i)) => {
                if *i < 0 || *i >= elements.len() as i64 {
                    return Err(Error::new_runtime(
                        format!("Array index out of bounds: {}", i),
                        *span,
                    ));
                }
                Ok(elements[*i as usize].clone())
            }
            (Value::Array(elements), Value::Range(start, end, inclusive)) => {
                // Array slicing with range
                let len = elements.len() as i64;

                // Calculate start index (default to 0 if None)
                let start_idx = start.unwrap_or(0).max(0).min(len) as usize;

                // Calculate end index
                let end_idx = match end {
                    Some(e) => {
                        let mut end_val = *e;
                        if *inclusive && end_val < len {
                            end_val += 1;
                        }
                        end_val.max(0).min(len) as usize
                    }
                    None => len as usize,
                };

                // Return slice (empty array if invalid range)
                if start_idx <= end_idx {
                    Ok(Value::Array(elements[start_idx..end_idx].to_vec()))
                } else {
                    Ok(Value::Array(vec![]))
                }
            }
            (Value::Array(_), _) => Err(Error::new_runtime(
                format!(
                    "Array index must be an integer or range, found {:?}",
                    index_value
                ),
                *span,
            )),
            _ => Err(Error::new_runtime(
                "Cannot index non-array value".to_string(),
                *span,
            )),
        }
    }

    fn visit_range(
        &mut self,
        start: Option<&Expr>,
        end: Option<&Expr>,
        inclusive: bool,
        _span: &Span,
    ) -> Result<Value> {
        let start_val = match start {
            Some(expr) => match self.visit_expr(expr)? {
                Value::Int(n) => Some(n),
                _ => {
                    return Err(Error::new_runtime(
                        "Range start must be an integer".to_string(),
                        expr.span(),
                    ))
                }
            },
            None => None,
        };

        let end_val = match end {
            Some(expr) => match self.visit_expr(expr)? {
                Value::Int(n) => Some(n),
                _ => {
                    return Err(Error::new_runtime(
                        "Range end must be an integer".to_string(),
                        expr.span(),
                    ))
                }
            },
            None => None,
        };

        Ok(Value::Range(start_val, end_val, inclusive))
    }

    fn visit_binary_op(
        &mut self,
        left: &Expr,
        op: &Operator,
        right: &Expr,
        span: &Span,
    ) -> Result<Value> {
        // Handle short-circuit evaluation for logical operators
        match op {
            Operator::And => {
                let left_val = self.visit_expr(left)?;
                match left_val {
                    Value::Bool(false) => Ok(Value::Bool(false)), // Short-circuit: false && _ = false
                    Value::Bool(true) => {
                        let right_val = self.visit_expr(right)?;
                        match right_val {
                            Value::Bool(b) => Ok(Value::Bool(b)),
                            _ => Err(Error::new_runtime(
                                "Right operand of && must be bool".to_string(),
                                *span,
                            )),
                        }
                    }
                    _ => Err(Error::new_runtime(
                        "Left operand of && must be bool".to_string(),
                        *span,
                    )),
                }
            }
            Operator::Or => {
                let left_val = self.visit_expr(left)?;
                match left_val {
                    Value::Bool(true) => Ok(Value::Bool(true)), // Short-circuit: true || _ = true
                    Value::Bool(false) => {
                        let right_val = self.visit_expr(right)?;
                        match right_val {
                            Value::Bool(b) => Ok(Value::Bool(b)),
                            _ => Err(Error::new_runtime(
                                "Right operand of || must be bool".to_string(),
                                *span,
                            )),
                        }
                    }
                    _ => Err(Error::new_runtime(
                        "Left operand of || must be bool".to_string(),
                        *span,
                    )),
                }
            }
            _ => {
                // Regular evaluation for other operators
                let left_val = self.visit_expr(left)?;
                let right_val = self.visit_expr(right)?;
                Self::evaluate_binary_op(self, left_val, op, right_val, *span)
            }
        }
    }

    fn visit_unary_op(&mut self, op: &UnaryOp, expr: &Expr, span: &Span) -> Result<Value> {
        let value = self.visit_expr(expr)?;

        match op {
            UnaryOp::Neg => match value {
                Value::Int(n) => Ok(Value::Int(-n)),
                Value::Float(f) => Ok(Value::Float(-f)),
                _ => Err(Error::new_runtime(
                    format!("Cannot negate non-numeric value: {:?}", value),
                    *span,
                )),
            },
            UnaryOp::Not => match value {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                _ => Err(Error::new_runtime(
                    format!("Cannot apply logical NOT to non-boolean value: {:?}", value),
                    *span,
                )),
            },
        }
    }

    fn visit_assign(&mut self, left: &Expr, right: &Expr, span: &Span) -> Result<Value> {
        let value = self.visit_expr(right)?;

        match left {
            Expr::Identifier(name, _) => {
                self.set_var(name.clone(), value);
                Ok(Value::Unit)
            }
            Expr::MemberAccess(object, field, _) => {
                // For struct field assignment: obj.field = value
                // Handle similarly to compound assignment

                if let Expr::Identifier(var_name, _) = &**object {
                    // Get the actual variable (handling self)
                    let actual_var_name = if var_name == "self" {
                        "self" // Use self directly within the method scope
                    } else {
                        var_name.as_str()
                    };

                    // Get the struct instance
                    let struct_val = self.get_var(actual_var_name).ok_or_else(|| {
                        Error::new_runtime(format!("Undefined variable: {}", var_name), *span)
                    })?;

                    match struct_val {
                        Value::StructInstance(struct_name, mut fields) => {
                            // Update field
                            fields.insert(field.clone(), value);

                            // Update the struct in the environment
                            self.set_var(
                                actual_var_name.to_string(),
                                Value::StructInstance(struct_name, fields),
                            );
                            Ok(Value::Unit)
                        }
                        _ => Err(Error::new_runtime(
                            "Cannot assign to field of non-struct value".to_string(),
                            *span,
                        )),
                    }
                } else {
                    // For complex expressions, can't update
                    Err(Error::new_runtime(
                        "Cannot assign to field of temporary value".to_string(),
                        *span,
                    ))
                }
            }
            Expr::ArrayIndex(array_expr, index_expr, _) => {
                let index = match self.visit_expr(index_expr)? {
                    Value::Int(i) => i as usize,
                    _ => {
                        return Err(Error::new_runtime(
                            "Array index must be an integer".to_string(),
                            *span,
                        ))
                    }
                };

                match self.visit_expr(array_expr)? {
                    Value::Array(mut elements) => {
                        if index >= elements.len() {
                            return Err(Error::new_runtime(
                                format!("Array index out of bounds: {}", index),
                                *span,
                            ));
                        }
                        elements[index] = value;
                        // Need to update the array in the environment
                        if let Expr::Identifier(var_name, _) = &**array_expr {
                            self.set_var(var_name.clone(), Value::Array(elements));
                        }
                        Ok(Value::Unit)
                    }
                    _ => Err(Error::new_runtime(
                        "Cannot index non-array value".to_string(),
                        *span,
                    )),
                }
            }
            _ => Err(Error::new_runtime(
                "Invalid assignment target".to_string(),
                *span,
            )),
        }
    }

    fn visit_compound_assign(
        &mut self,
        left: &Expr,
        op: &Operator,
        right: &Expr,
        span: &Span,
    ) -> Result<Value> {
        match left {
            Expr::Identifier(name, _) => {
                // Simple variable compound assignment (existing logic)
                let left_val = self.get_var(name).ok_or_else(|| {
                    Error::new_runtime(format!("Undefined variable: {}", name), *span)
                })?;
                let right_val = self.visit_expr(right)?;
                let result = Self::evaluate_binary_op(self, left_val, op, right_val, *span)?;
                self.set_var(name.clone(), result);
                Ok(Value::Unit)
            }
            Expr::MemberAccess(object, field, _) => {
                // For struct field compound assignment: obj.field += value
                // We need to handle it differently to ensure we update the actual struct

                // First, get the object and check if it's an identifier
                if let Expr::Identifier(var_name, _) = &**object {
                    // Get the actual variable (handling self redirection)
                    let actual_var_name = if var_name == "self" {
                        "self" // Use self directly within the method scope
                    } else {
                        var_name.as_str()
                    };

                    // Get the struct instance
                    let struct_val = self.get_var(actual_var_name).ok_or_else(|| {
                        Error::new_runtime(format!("Undefined variable: {}", var_name), *span)
                    })?;

                    match struct_val {
                        Value::StructInstance(struct_name, mut fields) => {
                            // Get current field value
                            let current_value = fields.get(field).cloned().ok_or_else(|| {
                                Error::new_runtime(
                                    format!("Field '{}' not found in struct", field),
                                    *span,
                                )
                            })?;

                            // Compute new value
                            let right_val = self.visit_expr(right)?;
                            let result = Self::evaluate_binary_op(
                                self,
                                current_value,
                                op,
                                right_val,
                                *span,
                            )?;

                            // Update field
                            fields.insert(field.clone(), result);

                            // Update the struct in the environment
                            self.set_var(
                                actual_var_name.to_string(),
                                Value::StructInstance(struct_name, fields),
                            );
                            Ok(Value::Unit)
                        }
                        _ => Err(Error::new_runtime(
                            "Cannot assign to field of non-struct value".to_string(),
                            *span,
                        )),
                    }
                } else {
                    // For complex expressions like foo().field += value, evaluate normally
                    let current_value = self.visit_expr(left)?;
                    let right_val = self.visit_expr(right)?;
                    let _result =
                        Self::evaluate_binary_op(self, current_value, op, right_val, *span)?;

                    // This case can't actually update the original, so just return
                    Err(Error::new_runtime(
                        "Cannot perform compound assignment on temporary value".to_string(),
                        *span,
                    ))
                }
            }
            Expr::ArrayIndex(array_expr, index_expr, _) => {
                // Array element compound assignment: arr[i] += value
                let current_value = self.visit_expr(left)?; // Get current element value
                let right_val = self.visit_expr(right)?;
                let result = Self::evaluate_binary_op(self, current_value, op, right_val, *span)?;

                // Update the array element using assignment logic
                let index = match self.visit_expr(index_expr)? {
                    Value::Int(i) => i as usize,
                    _ => {
                        return Err(Error::new_runtime(
                            "Array index must be an integer".to_string(),
                            *span,
                        ))
                    }
                };

                match self.visit_expr(array_expr)? {
                    Value::Array(mut elements) => {
                        if index >= elements.len() {
                            return Err(Error::new_runtime(
                                format!("Array index out of bounds: {}", index),
                                *span,
                            ));
                        }
                        elements[index] = result;
                        // Need to update the array in the environment
                        if let Expr::Identifier(var_name, _) = &**array_expr {
                            self.set_var(var_name.clone(), Value::Array(elements));
                        }
                        Ok(Value::Unit)
                    }
                    _ => Err(Error::new_runtime(
                        "Cannot index non-array value".to_string(),
                        *span,
                    )),
                }
            }
            _ => Err(Error::new_runtime(
                "Invalid compound assignment target".to_string(),
                *span,
            )),
        }
    }

    fn visit_function_call(&mut self, name: &str, args: &[Expr], span: &Span) -> Result<Value> {
        // Handle method calls with dot notation
        let (func_name, arg_values) = if name.contains('.') {
            let (target_var, method_name) = name.split_once('.').unwrap();

            // Get the target struct instance
            let target_value = self.get_var(target_var).ok_or_else(|| {
                Error::new_runtime(format!("Undefined variable: {}", target_var), *span)
            })?;

            // Extract the struct type name
            let struct_name = match &target_value {
                Value::StructInstance(name, _) => name.clone(),
                _ => {
                    return Err(Error::new_runtime(
                        format!("{} is not a struct instance", target_var),
                        *span,
                    ))
                }
            };

            // Build the full method name
            let full_method_name = format!("{}::{}", struct_name, method_name);

            // Evaluate arguments and prepend 'self'
            let mut args_with_self = vec![target_value];
            for arg in args {
                args_with_self.push(self.visit_expr(arg)?);
            }

            (full_method_name, args_with_self)
        } else {
            // Regular function call
            let mut values = Vec::new();
            for arg in args {
                values.push(self.visit_expr(arg)?);
            }
            (name.to_string(), values)
        };

        let func = self.get_var(&func_name).ok_or_else(|| {
            Error::new_runtime(format!("Function '{}' not found", func_name), *span)
        })?;

        self.execute_function_call(func, arg_values, *span)
    }

    fn visit_struct_init(
        &mut self,
        name: &str,
        fields: &[(String, Expr)],
        span: &Span,
    ) -> Result<Value> {
        // Check if struct exists
        let _struct_def = self
            .get_var(name)
            .ok_or_else(|| Error::new_runtime(format!("Undefined struct: {}", name), *span))?;

        let mut field_values = IndexMap::new();
        for (field_name, field_expr) in fields {
            let value = self.visit_expr(field_expr)?;
            field_values.insert(field_name.clone(), value);
        }

        Ok(Value::StructInstance(name.to_string(), field_values))
    }

    fn visit_member_access(&mut self, object: &Expr, field: &str, span: &Span) -> Result<Value> {
        let obj_value = self.visit_expr(object)?;

        match obj_value {
            Value::StructInstance(_, fields) => fields
                .get(field)
                .cloned()
                .ok_or_else(|| Error::new_runtime(format!("Field '{}' not found", field), *span)),
            _ => {
                // Check if it's a method call (will be handled by function call)
                if let Expr::Identifier(struct_name, _) = object {
                    let method_name = format!("{}::{}", struct_name, field);
                    self.get_var(&method_name).ok_or_else(|| {
                        Error::new_runtime(format!("Field or method '{}' not found", field), *span)
                    })
                } else {
                    Err(Error::new_runtime(
                        format!("Cannot access field '{}' on non-struct value", field),
                        *span,
                    ))
                }
            }
        }
    }

    fn visit_enum_variant_or_method_call(
        &mut self,
        target: &Expr,
        call: &str,
        args: &[Expr],
        span: &Span,
    ) -> Result<Value> {
        if let Expr::Identifier(type_name, _) = target {
            // Check if it's an enum variant
            if let Some(Value::Enum(enum_name, _)) = self.get_var(type_name) {
                if enum_name == *type_name {
                    let data = if args.is_empty() {
                        None
                    } else if args.len() == 1 {
                        Some(Box::new(self.visit_expr(&args[0])?))
                    } else {
                        return Err(Error::new_runtime(
                            format!("Enum variant takes at most 1 argument, got {}", args.len()),
                            *span,
                        ));
                    };
                    return Ok(Value::EnumVariant(
                        type_name.clone(),
                        call.to_string(),
                        data,
                    ));
                }
            }

            // Check if it's a method call
            let method_name = format!("{}::{}", type_name, call);
            if let Some(func) = self.get_var(&method_name) {
                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(self.visit_expr(arg)?);
                }
                return self.execute_function_call(func, arg_values, *span);
            }
        }

        Err(Error::new_runtime(
            format!("Unknown enum variant or method: {}::{}", target, call),
            *span,
        ))
    }

    // ===== Statement visit methods =====

    fn visit_let(&mut self, name: &str, expr: &Expr, _span: &Span) -> Result<Value> {
        let value = self.visit_expr(expr)?;
        self.set_var(name.to_string(), value);
        Ok(Value::Unit)
    }

    fn visit_function(
        &mut self,
        _name: &str,
        _generic_params: &[String],
        _params: &[(String, String)],
        _return_type: &str,
        _body: &[Stmt],
        _span: &Span,
    ) -> Result<Value> {
        // Function already registered in collect_function_definitions during first pass
        // No need to re-process here
        Ok(Value::Unit)
    }

    fn visit_struct(
        &mut self,
        name: &str,
        _generic_params: &[String],
        fields: &[(String, String)],
        _span: &Span,
    ) -> Result<Value> {
        let mut field_map = IndexMap::new();
        for (field_name, field_type) in fields {
            field_map.insert(field_name.clone(), field_type.clone());
        }
        let struct_value = Value::Struct(name.to_string(), field_map);
        // Always set structs in global environment so they're accessible from functions
        self.global_environment
            .insert(name.to_string(), struct_value.clone());

        // If we're at the top level, add to exports
        // TODO: This should only export items marked with 'pub' once we track visibility in AST
        if self.is_top_level {
            self.current_exports.insert(name.to_string(), struct_value);
        }

        Ok(Value::Unit)
    }

    fn visit_enum(
        &mut self,
        name: &str,
        _generic_params: &[String],
        variants: &[crate::parser::EnumVariant],
        _span: &Span,
    ) -> Result<Value> {
        let mut variant_map = IndexMap::new();
        for variant in variants {
            match variant {
                crate::parser::EnumVariant::Unit(name) => {
                    variant_map.insert(name.clone(), "unit".to_string());
                }
                crate::parser::EnumVariant::Tuple(name, type_name) => {
                    variant_map.insert(name.clone(), type_name.clone());
                }
                crate::parser::EnumVariant::Struct(name, _fields) => {
                    // For struct variants, we'll store them as a special struct type
                    variant_map.insert(name.clone(), format!("struct_{}", name));
                }
            }
        }
        let enum_value = Value::Enum(name.to_string(), variant_map);
        // Always set enums in global environment so they're accessible from functions
        self.global_environment
            .insert(name.to_string(), enum_value.clone());

        // If we're at the top level, add to exports
        // TODO: This should only export items marked with 'pub' once we track visibility in AST
        if self.is_top_level {
            self.current_exports.insert(name.to_string(), enum_value);
        }

        Ok(Value::Unit)
    }

    fn visit_impl(&mut self, struct_name: &str, methods: &[Stmt], span: &Span) -> Result<Value> {
        // Verify struct exists
        match self.get_var(struct_name) {
            Some(Value::Struct(_, _)) => {}
            _ => {
                return Err(Error::new_runtime(
                    format!("Undefined struct: {}", struct_name),
                    *span,
                ))
            }
        }

        // Register methods
        for method in methods {
            if let Stmt::Function(_, method_name, _, params, _, body, _) = method {
                let func = Value::Function(Function::UserDefined(
                    method_name.clone(),
                    params.clone(),
                    body.clone(),
                    self.environment.clone(),
                ));

                let full_name = format!("{}::{}", struct_name, method_name);
                self.set_var(full_name, func);
            }
        }

        Ok(Value::Unit)
    }

    fn visit_match(
        &mut self,
        expr: &Expr,
        arms: &[(Expr, Vec<Stmt>)],
        span: &Span,
    ) -> Result<Value> {
        let match_value = self.visit_expr(expr)?;

        for (pattern, body) in arms {
            // Simple pattern matching for now
            let matches = match pattern {
                Expr::Identifier(name, _) if name == "_" => true, // Wildcard
                Expr::EnumVariantOrMethodCall {
                    target, call, args, ..
                } => {
                    if let (
                        Expr::Identifier(enum_name, _),
                        Value::EnumVariant(val_enum, val_variant, val_data),
                    ) = (&**target, &match_value)
                    {
                        if enum_name == val_enum && call == val_variant {
                            // Bind pattern variables if any
                            if !args.is_empty() && args.len() == 1 {
                                if let (Expr::Identifier(var_name, _), Some(data)) =
                                    (&args[0], val_data)
                                {
                                    self.set_var(var_name.clone(), *data.clone());
                                }
                            }
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
                Expr::StructPattern(pattern_name, fields, _) => {
                    if let Value::StructInstance(struct_name, struct_fields) = &match_value {
                        if pattern_name == struct_name {
                            // Check if all pattern fields exist in struct and bind variables
                            let mut all_fields_match = true;
                            for (field_name, var_name) in fields {
                                if field_name == ".." {
                                    // Rest pattern - skip remaining fields
                                    continue;
                                }
                                if let Some(field_value) = struct_fields.get(field_name) {
                                    // Bind the field value to the variable name (or field name if no variable)
                                    let bind_name = var_name.as_ref().unwrap_or(field_name);
                                    self.set_var(bind_name.clone(), field_value.clone());
                                } else {
                                    all_fields_match = false;
                                    break;
                                }
                            }
                            all_fields_match
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
                _ => {
                    // Direct value comparison
                    let pattern_val = self.visit_expr(pattern)?;
                    match (&match_value, &pattern_val) {
                        (Value::Int(a), Value::Int(b)) => a == b,
                        (Value::Bool(a), Value::Bool(b)) => a == b,
                        (Value::String(a), Value::String(b)) => a == b,
                        _ => false,
                    }
                }
            };

            if matches {
                // Use visit_statements_no_scope to keep pattern variables in scope
                return self.visit_statements_no_scope(body);
            }
        }

        Err(Error::new_runtime(
            "Non-exhaustive match".to_string(),
            *span,
        ))
    }

    fn visit_for_loop(
        &mut self,
        pattern: &Expr,
        iterable: &Expr,
        body: &[Stmt],
        span: &Span,
    ) -> Result<Value> {
        let iter_value = self.visit_expr(iterable)?;

        match iter_value {
            Value::Array(elements) => {
                for element in elements {
                    self.bind_pattern(pattern, element)?;

                    for stmt in body {
                        self.visit_stmt(stmt)?;

                        match self.control_flow {
                            ControlFlow::Break => {
                                self.control_flow = ControlFlow::Normal;
                                return Ok(Value::Unit);
                            }
                            ControlFlow::Continue => {
                                self.control_flow = ControlFlow::Normal;
                                break;
                            }
                            ControlFlow::Return(_) => return Ok(Value::Unit),
                            ControlFlow::Normal => {}
                        }
                    }
                }
            }
            Value::Range(start, end, inclusive) => {
                // For ranges, we can only bind to simple identifiers
                let var_name = match pattern {
                    Expr::Identifier(name, _) => name,
                    _ => {
                        return Err(Error::new_runtime(
                            "Range-based for loops only support simple identifiers".to_string(),
                            *span,
                        ))
                    }
                };

                let start = start.unwrap_or(0);
                let end = end.ok_or_else(|| {
                    Error::new_runtime("Cannot iterate over unbounded range".to_string(), *span)
                })?;

                let range: Box<dyn Iterator<Item = i64>> = if inclusive {
                    Box::new(start..=end)
                } else {
                    Box::new(start..end)
                };

                for i in range {
                    self.set_var(var_name.clone(), Value::Int(i));

                    for stmt in body {
                        self.visit_stmt(stmt)?;

                        match self.control_flow {
                            ControlFlow::Break => {
                                self.control_flow = ControlFlow::Normal;
                                return Ok(Value::Unit);
                            }
                            ControlFlow::Continue => {
                                self.control_flow = ControlFlow::Normal;
                                break;
                            }
                            ControlFlow::Return(_) => return Ok(Value::Unit),
                            ControlFlow::Normal => {}
                        }
                    }
                }
            }
            _ => {
                return Err(Error::new_runtime(
                    format!("Cannot iterate over {:?}", iter_value),
                    *span,
                ))
            }
        }

        Ok(Value::Unit)
    }

    fn visit_while(&mut self, condition: &Expr, body: &[Stmt], _span: &Span) -> Result<Value> {
        loop {
            let condition_val = self.visit_expr(condition)?;

            match condition_val {
                Value::Bool(true) => {
                    for stmt in body {
                        self.visit_stmt(stmt)?;

                        match self.control_flow {
                            ControlFlow::Break => {
                                self.control_flow = ControlFlow::Normal;
                                return Ok(Value::Unit);
                            }
                            ControlFlow::Continue => {
                                self.control_flow = ControlFlow::Normal;
                                break;
                            }
                            ControlFlow::Return(_) => return Ok(Value::Unit),
                            ControlFlow::Normal => {}
                        }
                    }
                }
                Value::Bool(false) => break,
                _ => {
                    return Err(Error::new_runtime(
                        "While condition must be a boolean".to_string(),
                        condition.span(),
                    ))
                }
            }
        }

        Ok(Value::Unit)
    }

    fn visit_loop(&mut self, body: &[Stmt], _span: &Span) -> Result<Value> {
        loop {
            for stmt in body {
                self.visit_stmt(stmt)?;

                match self.control_flow {
                    ControlFlow::Break => {
                        self.control_flow = ControlFlow::Normal;
                        return Ok(Value::Unit);
                    }
                    ControlFlow::Continue => {
                        self.control_flow = ControlFlow::Normal;
                        break;
                    }
                    ControlFlow::Return(_) => return Ok(Value::Unit),
                    ControlFlow::Normal => {}
                }
            }
        }
    }

    fn visit_break(&mut self, _span: &Span) -> Result<Value> {
        self.control_flow = ControlFlow::Break;
        Ok(Value::Unit)
    }

    fn visit_continue(&mut self, _span: &Span) -> Result<Value> {
        self.control_flow = ControlFlow::Continue;
        Ok(Value::Unit)
    }

    fn visit_return(&mut self, expr: Option<&Expr>, _span: &Span) -> Result<Value> {
        let return_value = match expr {
            Some(return_expr) => self.visit_expr(return_expr)?,
            None => Value::Unit,
        };
        self.control_flow = ControlFlow::Return(return_value.clone());
        Ok(return_value)
    }

    fn visit_expression_stmt(&mut self, expr: &Expr, has_semicolon: bool) -> Result<Value> {
        let expr_value = self.visit_expr(expr)?;

        // If the expression has a semicolon, it's converted to a statement that returns Unit
        // If no semicolon, it returns the expression's value (for block expressions)
        if has_semicolon {
            Ok(Value::Unit)
        } else {
            Ok(expr_value)
        }
    }

    fn visit_extern_function(
        &mut self,
        _name: &str,
        _generic_params: &[String],
        _params: &[(String, String)],
        _return_type: &str,
        _span: &Span,
    ) -> Result<Value> {
        // Extern declarations are no-op in interpreter mode
        // They're only used for type checking external APIs
        Ok(Value::Unit)
    }

    fn visit_extern_mod(
        &mut self,
        _name: &str,
        _items: &[ExternItem],
        _span: &Span,
    ) -> Result<Value> {
        // Extern declarations are no-op in interpreter mode
        // They're only used for type checking external APIs
        Ok(Value::Unit)
    }

    fn visit_extern_type(
        &mut self,
        _name: &str,
        _generic_params: &[String],
        _span: &Span,
    ) -> Result<Value> {
        // Extern type declarations are no-op in interpreter mode
        // They're only used for type checking external APIs
        Ok(Value::Unit)
    }

    fn visit_async_function(
        &mut self,
        _name: &str,
        _generic_params: &[String],
        _params: &[(String, String)],
        _return_type: &str,
        _body: &[Stmt],
        _span: &Span,
    ) -> Result<Value> {
        Err(Error::new_runtime(
            "Async functions are not supported in interpreter mode. Use 'husk transpile' to generate JavaScript.",
            *_span,
        ))
    }

    fn visit_match_expr(
        &mut self,
        expr: &Expr,
        arms: &[(Expr, Vec<Stmt>)],
        span: &Span,
    ) -> Result<Value> {
        // Use the same implementation as visit_match
        self.visit_match(expr, arms, span)
    }

    fn visit_await(&mut self, _expr: &Expr, span: &Span) -> Result<Value> {
        Err(Error::new_runtime(
            ".await is not supported in interpreter mode. Use 'husk transpile' to generate JavaScript.",
            *span,
        ))
    }

    fn visit_try(&mut self, expr: &Expr, span: &Span) -> Result<Value> {
        // Evaluate the expression
        let result = self.visit_expr(expr)?;

        // Check if it's a Result type and unwrap it
        match result {
            Value::EnumVariant(enum_name, variant, value) if enum_name == "Result" && variant == "Ok" => {
                if let Some(val) = value {
                    Ok(*val)
                } else {
                    Ok(Value::Unit)
                }
            }
            Value::EnumVariant(enum_name, variant, _) if enum_name == "Result" && variant == "Err" => {
                Err(Error::new_runtime(
                    "? operator encountered Result::Err variant - error propagation would return early",
                    *span,
                ))
            }
            _ => Err(Error::new_runtime(
                "? operator can only be used on Result types",
                *span,
            )),
        }
    }

    fn visit_await_try(&mut self, _expr: &Expr, span: &Span) -> Result<Value> {
        Err(Error::new_runtime(
            ".await? is not supported in interpreter mode. Use 'husk transpile' to generate JavaScript.",
            *span,
        ))
    }

    fn visit_closure(
        &mut self,
        params: &[(String, Option<String>)],
        _ret_type: &Option<String>,
        body: &Expr,
        span: &Span,
    ) -> Result<Value> {
        // Create a closure value that captures the current environment
        let closure_params = params
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>();

        // Clone the body expression for storage
        let closure_body = body.clone();

        // Capture the current environment (simplified - just clone the whole thing)
        let captured_env = self.environment.clone();

        Ok(Value::Function(Function::Closure {
            params: closure_params,
            body: closure_body,
            captured_env,
            span: *span,
        }))
    }

    fn visit_use(&mut self, path: &UsePath, items: &UseItems, span: &Span) -> Result<Value> {
        // Check if it's an external package
        if path.prefix == UsePrefix::None {
            return Err(Error::new_runtime(
                format!(
                    "External package '{}' cannot be used in interpreter mode. \
                    Use the transpiler to convert to JavaScript for external package support.",
                    path.segments.join("::")
                ),
                *span,
            ));
        }

        // Resolve the module path
        let module_path = self.resolve_module_path(path, span)?;

        // Load the module
        let module = self.load_module(&module_path, span)?;

        // Import the requested items
        match items {
            UseItems::All => {
                // Import all exports from the module
                for (name, value) in &module.exports {
                    self.set_var(name.clone(), value.clone());
                }
            }
            UseItems::Single => {
                // Import the module itself as a namespace
                // For now, create a placeholder value
                // TODO: Implement proper module namespace handling
                let module_name = path
                    .segments
                    .last()
                    .ok_or_else(|| Error::new_runtime("Empty module path".to_string(), *span))?;
                // For now, just set a unit value as placeholder
                self.set_var(module_name.clone(), Value::Unit);
            }
            UseItems::Named(imports) => {
                // Import specific named items
                for (import_name, alias) in imports {
                    let value = module.exports.get(import_name).ok_or_else(|| {
                        Error::new_runtime(
                            format!(
                                "Module '{}' does not export '{}'",
                                module_path.display(),
                                import_name
                            ),
                            *span,
                        )
                    })?;

                    let local_name = alias.as_ref().unwrap_or(import_name);
                    self.set_var(local_name.clone(), value.clone());
                }
            }
        }

        Ok(Value::Unit)
    }

    fn visit_block(&mut self, stmts: &[Stmt], _span: &Span) -> Result<Value> {
        // Implement proper scoping for blocks
        let saved_env = self.push_scope();

        // Blocks are not top-level
        let was_top_level = self.is_top_level;
        self.is_top_level = false;

        let mut block_value = Value::Unit;

        // Execute all statements in the block
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;

            match stmt {
                // If the last statement is an expression without semicolon,
                // the block evaluates to that expression's value
                Stmt::Expression(expr, false) if is_last => {
                    block_value = self.visit_expr(expr)?;
                }
                // Otherwise, just execute the statement normally
                _ => {
                    self.visit_stmt(stmt)?;

                    // Check for control flow changes
                    match self.control_flow {
                        ControlFlow::Break | ControlFlow::Continue | ControlFlow::Return(_) => {
                            // If we hit a break/continue/return, stop executing and bubble up
                            self.pop_scope(saved_env);
                            self.is_top_level = was_top_level;
                            return Ok(Value::Unit);
                        }
                        ControlFlow::Normal => {}
                    }
                }
            }
        }

        // Restore the previous scope and top-level flag
        self.pop_scope(saved_env);
        self.is_top_level = was_top_level;

        Ok(block_value)
    }

    fn visit_if_expr(
        &mut self,
        condition: &Expr,
        then_block: &[Stmt],
        else_block: &[Stmt],
        _span: &Span,
    ) -> Result<Value> {
        let condition_val = self.visit_expr(condition)?;

        match condition_val {
            Value::Bool(true) => {
                // Execute then block without creating new scope
                self.visit_statements_no_scope(then_block)
            }
            Value::Bool(false) => {
                if else_block.is_empty() {
                    Ok(Value::Unit)
                } else {
                    // Execute else block without creating new scope
                    self.visit_statements_no_scope(else_block)
                }
            }
            _ => Err(Error::new_runtime(
                "If condition must be a boolean".to_string(),
                condition.span(),
            )),
        }
    }

    fn visit_method_call(
        &mut self,
        object: &Expr,
        method: &str,
        args: &[Expr],
        _span: &Span,
    ) -> Result<Value> {
        let obj_value = self.visit_expr(object)?;

        match &obj_value {
            Value::String(_) => {
                // Evaluate arguments first to avoid borrowing issues
                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(self.visit_expr(arg)?);
                }

                // Look up the method in the registry
                if let Some(method_impl) = self.method_registry.get_string_method(method) {
                    // Call the method implementation
                    method_impl(&obj_value, &arg_values, _span)
                } else {
                    Err(Error::new_runtime(
                        format!("Unknown string method: {}", method),
                        *_span,
                    ))
                }
            }
            Value::Array(_) => {
                // Special handling for closure methods that need interpreter context
                match method {
                    "map" => self.array_map(&obj_value, args, _span),
                    "filter" => self.array_filter(&obj_value, args, _span),
                    "find" => self.array_find(&obj_value, args, _span),
                    "position" => self.array_position(&obj_value, args, _span),
                    _ => {
                        // Evaluate arguments first to avoid borrowing issues
                        let mut arg_values = Vec::new();
                        for arg in args {
                            arg_values.push(self.visit_expr(arg)?);
                        }

                        // Look up the method in the registry
                        if let Some(method_impl) = self.method_registry.get_array_method(method) {
                            // Call the method implementation
                            method_impl(&obj_value, &arg_values, _span)
                        } else {
                            Err(Error::new_runtime(
                                format!("Unknown array method: {}", method),
                                *_span,
                            ))
                        }
                    }
                }
            }
            Value::Struct(struct_name, _) => {
                // Look up the method directly
                let method_name = format!("{}::{}", struct_name, method);
                if let Some(func) = self.get_var(&method_name) {
                    // Evaluate all arguments, prepending self as the first one
                    let mut arg_values = vec![obj_value.clone()];
                    for arg in args {
                        arg_values.push(self.visit_expr(arg)?);
                    }
                    self.execute_function_call(func, arg_values, *_span)
                } else {
                    Err(Error::new_runtime(
                        format!("Unknown method: {}", method_name),
                        *_span,
                    ))
                }
            }
            Value::StructInstance(name, fields) => {
                // Check if the method is a field access
                if let Some(field_value) = fields.get(method) {
                    return Ok(field_value.clone());
                }

                // Look up the method directly
                let method_name = format!("{}::{}", name, method);
                if let Some(func) = self.get_var(&method_name) {
                    // Extract the variable name if the object is a simple identifier
                    let var_name = if let Expr::Identifier(var_name, _) = object {
                        Some(var_name.clone())
                    } else {
                        None
                    };

                    // Save the current self_var_name and set the new one
                    let saved_self_var = self.self_var_name.clone();
                    self.self_var_name = var_name;

                    // Evaluate all arguments, prepending self as the first one
                    let mut arg_values = vec![obj_value.clone()];
                    for arg in args {
                        arg_values.push(self.visit_expr(arg)?);
                    }
                    let result = self.execute_function_call(func, arg_values, *_span);

                    // Restore the previous self_var_name
                    self.self_var_name = saved_self_var;
                    result
                } else {
                    Err(Error::new_runtime(
                        format!("Unknown method: {}", method_name),
                        *_span,
                    ))
                }
            }
            typ => {
                Err(Error::new_runtime(
                    format!(
                        "Cannot call method '{}' on {:?}: unexpected type {:?}",
                        method, obj_value, typ
                    ),
                    _span.clone(),
                ))
                // panic!("Method calls on non-string/array/struct types are not supported yet");
            }
        }
    }

    fn visit_cast(&mut self, expr: &Expr, target_type: &str, span: &Span) -> Result<Value> {
        let value = self.visit_expr(expr)?;

        // Parse the target type to determine the cast
        match target_type {
            "int" => match value {
                Value::Int(i) => Ok(Value::Int(i)), // No-op
                Value::Float(f) => Ok(Value::Int(f as i64)),
                Value::String(s) => match s.parse::<i64>() {
                    Ok(i) => Ok(Value::Int(i)),
                    Err(_) => Err(Error::new_runtime(
                        format!("Cannot parse '{}' as int", s),
                        *span,
                    )),
                },
                Value::Bool(b) => Ok(Value::Int(if b { 1 } else { 0 })),
                _ => Err(Error::new_runtime(
                    format!("Cannot cast {:?} to int", value),
                    *span,
                )),
            },
            "float" => match value {
                Value::Float(f) => Ok(Value::Float(f)), // No-op
                Value::Int(i) => Ok(Value::Float(i as f64)),
                Value::String(s) => match s.parse::<f64>() {
                    Ok(f) => Ok(Value::Float(f)),
                    Err(_) => Err(Error::new_runtime(
                        format!("Cannot parse '{}' as float", s),
                        *span,
                    )),
                },
                _ => Err(Error::new_runtime(
                    format!("Cannot cast {:?} to float", value),
                    *span,
                )),
            },
            "string" => match value {
                Value::String(s) => Ok(Value::String(s)), // No-op
                Value::Int(i) => Ok(Value::String(i.to_string())),
                Value::Float(f) => Ok(Value::String(f.to_string())),
                Value::Bool(b) => Ok(Value::String(b.to_string())),
                _ => Err(Error::new_runtime(
                    format!("Cannot cast {:?} to string", value),
                    *span,
                )),
            },
            "bool" => match value {
                Value::Bool(b) => Ok(Value::Bool(b)), // No-op
                Value::Int(i) => Ok(Value::Bool(i != 0)),
                Value::String(s) => Ok(Value::Bool(!s.is_empty())),
                _ => Err(Error::new_runtime(
                    format!("Cannot cast {:?} to bool", value),
                    *span,
                )),
            },
            _ => {
                // For custom types, we can't perform runtime casts
                // Just return the value as-is and let the type system handle it
                Ok(value)
            }
        }
    }

    fn visit_struct_pattern(
        &mut self,
        _variant: &str,
        _fields: &[(String, Option<String>)],
        span: &Span,
    ) -> Result<Value> {
        // Struct patterns are used for pattern matching in match expressions
        // In the interpreter, this should not be directly evaluated as an expression
        // It should only be used in the context of pattern matching
        Err(Error::new_runtime(
            "Struct patterns can only be used in match expressions".to_string(),
            *span,
        ))
    }

    fn visit_object_literal(&mut self, _fields: &[(String, Expr)], span: &Span) -> Result<Value> {
        // Object literals are primarily for JavaScript interop
        // In the interpreter, we don't have a direct object type
        // For now, we'll return an error suggesting to use the transpiler
        Err(Error::new_runtime(
            "Object literals are only supported in transpiler mode for JavaScript interop. Use structs for data structures in interpreter mode.".to_string(),
            *span,
        ))
    }

    fn visit_macro_call(&mut self, name: &str, args: &[Expr], span: &Span) -> Result<Value> {
        match name {
            "print" => {
                // print! macro with format string support
                if args.is_empty() {
                    return Err(Error::new_runtime(
                        "print! requires at least one argument".to_string(),
                        *span,
                    ));
                }

                // Evaluate all arguments
                let mut values = Vec::new();
                for arg in args {
                    values.push(self.visit_expr(arg)?);
                }

                // Use format logic if first arg is a string
                if let Value::String(format_str) = &values[0] {
                    let formatted = self.format_string(format_str, &values[1..])?;
                    print!("{}", formatted);
                } else {
                    // If not a string, just print the value
                    print!("{}", values[0]);
                }

                io::stdout()
                    .flush()
                    .map_err(|e| Error::new_runtime(format!("IO error: {}", e), *span))?;
                Ok(Value::Int(0)) // print! returns 0 on success
            }
            "println" => {
                // println! macro with format string support
                if args.is_empty() {
                    // println! with no args just prints a newline
                    println!();
                    return Ok(Value::Unit);
                }

                // Evaluate all arguments
                let mut values = Vec::new();
                for arg in args {
                    values.push(self.visit_expr(arg)?);
                }

                // Use format logic if first arg is a string
                if let Value::String(format_str) = &values[0] {
                    let formatted = self.format_string(format_str, &values[1..])?;
                    println!("{}", formatted);
                } else {
                    // If not a string, just print the value
                    match &values[0] {
                        Value::EnumVariant(_, _, Some(value)) => println!("{}", value),
                        _ => println!("{}", values[0]),
                    }
                }

                Ok(Value::Unit)
            }
            "format" => {
                // format! macro
                if args.is_empty() {
                    return Err(Error::new_runtime(
                        "format! requires at least one argument (the format string)".to_string(),
                        *span,
                    ));
                }

                // Evaluate all arguments
                let mut values = Vec::new();
                for arg in args {
                    values.push(self.visit_expr(arg)?);
                }

                // First argument must be a string
                if let Value::String(format_str) = &values[0] {
                    let formatted = self.format_string(format_str, &values[1..])?;
                    Ok(Value::String(formatted))
                } else {
                    Err(Error::new_runtime(
                        "format! first argument must be a string".to_string(),
                        *span,
                    ))
                }
            }
            "eprint" => {
                // eprint! macro with format string support
                if args.is_empty() {
                    return Err(Error::new_runtime(
                        "eprint! requires at least one argument".to_string(),
                        *span,
                    ));
                }

                // Evaluate all arguments
                let mut values = Vec::new();
                for arg in args {
                    values.push(self.visit_expr(arg)?);
                }

                // Use format logic if first arg is a string
                if let Value::String(format_str) = &values[0] {
                    let formatted = self.format_string(format_str, &values[1..])?;
                    eprint!("{}", formatted);
                } else {
                    // If not a string, just print the value
                    eprint!("{}", values[0]);
                }

                io::stderr()
                    .flush()
                    .map_err(|e| Error::new_runtime(format!("IO error: {}", e), *span))?;
                Ok(Value::Int(0)) // eprint! returns 0 on success
            }
            "eprintln" => {
                // eprintln! macro with format string support
                if args.is_empty() {
                    // eprintln! with no args just prints a newline to stderr
                    eprintln!();
                    return Ok(Value::Unit);
                }

                // Evaluate all arguments
                let mut values = Vec::new();
                for arg in args {
                    values.push(self.visit_expr(arg)?);
                }

                // Use format logic if first arg is a string
                if let Value::String(format_str) = &values[0] {
                    let formatted = self.format_string(format_str, &values[1..])?;
                    eprintln!("{}", formatted);
                } else {
                    // If not a string, just print the value
                    match &values[0] {
                        Value::EnumVariant(_, _, Some(value)) => eprintln!("{}", value),
                        _ => eprintln!("{}", values[0]),
                    }
                }

                Ok(Value::Unit)
            }
            _ => Err(Error::new_runtime(
                format!("Unknown macro: {}!", name),
                *span,
            )),
        }
    }
}

impl InterpreterVisitor {
    fn format_string(&self, format_str: &str, args: &[Value]) -> Result<String> {
        let mut result = String::new();
        let mut arg_index = 0;
        let mut chars = format_str.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '{' {
                if chars.peek() == Some(&'{') {
                    // Escaped {{
                    chars.next();
                    result.push('{');
                } else if chars.peek() == Some(&'}') {
                    // Placeholder {}
                    chars.next();
                    if arg_index >= args.len() {
                        return Err(Error::new_runtime(
                            format!("Not enough arguments for format string: expected at least {}, got {}",
                                   arg_index + 1, args.len()),
                            Span::default(),
                        ));
                    }
                    result.push_str(&args[arg_index].to_string());
                    arg_index += 1;
                } else {
                    result.push(ch);
                }
            } else if ch == '}' {
                if chars.peek() == Some(&'}') {
                    // Escaped }}
                    chars.next();
                    result.push('}');
                } else {
                    result.push(ch);
                }
            } else {
                result.push(ch);
            }
        }

        Ok(result)
    }

    fn bind_pattern(&mut self, pattern: &Expr, value: Value) -> Result<()> {
        match pattern {
            Expr::Identifier(name, _) => {
                self.set_var(name.clone(), value);
                Ok(())
            }
            Expr::Tuple(elements, span) => match value {
                Value::Tuple(values) => {
                    if elements.len() != values.len() {
                        return Err(Error::new_runtime(
                            format!(
                                "Tuple pattern has {} elements but value has {} elements",
                                elements.len(),
                                values.len()
                            ),
                            *span,
                        ));
                    }

                    for (elem_pattern, elem_value) in elements.iter().zip(values.into_iter()) {
                        self.bind_pattern(elem_pattern, elem_value)?;
                    }
                    Ok(())
                }
                _ => Err(Error::new_runtime(
                    "Cannot destructure non-tuple value in pattern".to_string(),
                    *span,
                )),
            },
            _ => Err(Error::new_runtime(
                "Only identifiers and tuples are supported in for loop patterns".to_string(),
                pattern.span(),
            )),
        }
    }

    /// Implements array.map() method with closure support
    fn array_map(&mut self, array_value: &Value, args: &[Expr], span: &Span) -> Result<Value> {
        if let Value::Array(elements) = array_value {
            if args.len() != 1 {
                return Err(Error::new_runtime(
                    "map() requires exactly 1 argument (closure)",
                    *span,
                ));
            }

            // Evaluate the closure argument
            let closure = self.visit_expr(&args[0])?;
            let mut results = Vec::new();

            // Apply the closure to each element
            for element in elements {
                let result = self.call_closure(&closure, &[element.clone()], span)?;
                results.push(result);
            }

            Ok(Value::Array(results))
        } else {
            Err(Error::new_runtime("map() called on non-array", *span))
        }
    }

    /// Implements array.filter() method with closure support
    fn array_filter(&mut self, array_value: &Value, args: &[Expr], span: &Span) -> Result<Value> {
        if let Value::Array(elements) = array_value {
            if args.len() != 1 {
                return Err(Error::new_runtime(
                    "filter() requires exactly 1 argument (closure)",
                    *span,
                ));
            }

            // Evaluate the closure argument
            let closure = self.visit_expr(&args[0])?;
            let mut results = Vec::new();

            // Apply the closure to each element and keep those that return true
            for element in elements {
                let result = self.call_closure(&closure, &[element.clone()], span)?;
                match result {
                    Value::Bool(true) => results.push(element.clone()),
                    Value::Bool(false) => {} // Skip this element
                    _ => {
                        return Err(Error::new_runtime(
                            "filter() closure must return a boolean",
                            *span,
                        ))
                    }
                }
            }

            Ok(Value::Array(results))
        } else {
            Err(Error::new_runtime("filter() called on non-array", *span))
        }
    }

    /// Implements array.find() method with closure support
    fn array_find(&mut self, array_value: &Value, args: &[Expr], span: &Span) -> Result<Value> {
        if let Value::Array(elements) = array_value {
            if args.len() != 1 {
                return Err(Error::new_runtime(
                    "find() requires exactly 1 argument (closure)",
                    *span,
                ));
            }

            // Evaluate the closure argument
            let closure = self.visit_expr(&args[0])?;

            // Apply the closure to each element and return the first match
            for element in elements {
                let result = self.call_closure(&closure, &[element.clone()], span)?;
                match result {
                    Value::Bool(true) => {
                        // Found a match, return Some(element)
                        return Ok(Value::EnumVariant(
                            "Option".to_string(),
                            "Some".to_string(),
                            Some(Box::new(element.clone())),
                        ));
                    }
                    Value::Bool(false) => {} // Continue searching
                    _ => {
                        return Err(Error::new_runtime(
                            "find() closure must return a boolean",
                            *span,
                        ))
                    }
                }
            }

            // No match found, return None
            Ok(Value::EnumVariant(
                "Option".to_string(),
                "None".to_string(),
                None,
            ))
        } else {
            Err(Error::new_runtime("find() called on non-array", *span))
        }
    }

    /// Implements array.position() method with closure support
    fn array_position(&mut self, array_value: &Value, args: &[Expr], span: &Span) -> Result<Value> {
        if let Value::Array(elements) = array_value {
            if args.len() != 1 {
                return Err(Error::new_runtime(
                    "position() requires exactly 1 argument (closure)",
                    *span,
                ));
            }

            // Evaluate the closure argument
            let closure = self.visit_expr(&args[0])?;

            // Apply the closure to each element and return the index of the first match
            for (index, element) in elements.iter().enumerate() {
                let result = self.call_closure(&closure, &[element.clone()], span)?;
                match result {
                    Value::Bool(true) => {
                        // Found a match, return Some(index)
                        return Ok(Value::EnumVariant(
                            "Option".to_string(),
                            "Some".to_string(),
                            Some(Box::new(Value::Int(index as i64))),
                        ));
                    }
                    Value::Bool(false) => {} // Continue searching
                    _ => {
                        return Err(Error::new_runtime(
                            "position() closure must return a boolean",
                            *span,
                        ))
                    }
                }
            }

            // No match found, return None
            Ok(Value::EnumVariant(
                "Option".to_string(),
                "None".to_string(),
                None,
            ))
        } else {
            Err(Error::new_runtime("position() called on non-array", *span))
        }
    }

    /// Helper method to call a closure with arguments
    fn call_closure(&mut self, closure: &Value, args: &[Value], span: &Span) -> Result<Value> {
        match closure {
            Value::Function(Function::Closure {
                params,
                body,
                captured_env,
                ..
            }) => {
                if args.len() != params.len() {
                    return Err(Error::new_runtime(
                        format!("Expected {} arguments, got {}", params.len(), args.len()),
                        *span,
                    ));
                }

                // Save current environment and use captured environment
                let saved_env = self.push_scope();

                // Set up closure environment
                let mut new_env = captured_env.clone();
                // Preserve function definitions from current environment
                for (name, value) in &self.environment {
                    if let Value::Function(_) = value {
                        new_env.insert(name.clone(), value.clone());
                    }
                }
                self.environment = new_env;

                // Bind arguments
                for (param, value) in params.iter().zip(args.iter()) {
                    self.set_var(param.clone(), value.clone());
                }

                // Execute closure body (expression)
                let result = self.visit_expr(&body)?;

                // Restore environment
                self.pop_scope(saved_env);
                Ok(result)
            }
            _ => Err(Error::new_runtime(
                "Expected a closure (function) argument",
                *span,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semantic::SemanticVisitor;

    fn run_test(input: &str) -> Result<Value> {
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse()?;

        let mut semantic = SemanticVisitor::new();
        semantic.analyze(&stmts)?;

        let mut interpreter = InterpreterVisitor::new();
        interpreter.interpret(&stmts)
    }

    #[test]
    fn test_external_package_error() {
        let result = run_test("use express::express;");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("External package"));
    }

    #[test]
    fn test_module_loading_without_context() {
        // Test that local:: imports fail without project root context
        let result = run_test("use local::utils::math;");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string()
                .contains("Cannot use local:: imports without project root context")
                || err.to_string().contains("Failed to read module")
        );

        // Test that self:: imports fail without current file context
        let result = run_test("use self::helper;");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string()
                .contains("Cannot use self:: imports without current file context")
                || err.to_string().contains("Failed to read module")
        );
    }

    // Arithmetic operations tests
    #[test]
    fn test_integer_arithmetic() {
        assert_eq!(run_test("2 + 3").unwrap(), Value::Int(5));
        assert_eq!(run_test("10 - 4").unwrap(), Value::Int(6));
        assert_eq!(run_test("3 * 4").unwrap(), Value::Int(12));
        assert_eq!(run_test("15 / 3").unwrap(), Value::Int(5));
        assert_eq!(run_test("17 % 5").unwrap(), Value::Int(2));
    }

    #[test]
    fn test_float_arithmetic() {
        assert_eq!(run_test("2.5 + 3.5").unwrap(), Value::Float(6.0));
        assert_eq!(run_test("10.0 - 4.5").unwrap(), Value::Float(5.5));
        assert_eq!(run_test("3.0 * 4.0").unwrap(), Value::Float(12.0));
        assert_eq!(run_test("15.0 / 4.0").unwrap(), Value::Float(3.75));
    }

    #[test]
    fn test_mixed_arithmetic() {
        assert_eq!(run_test("2 + 3.5").unwrap(), Value::Float(5.5));
        assert_eq!(run_test("10.0 - 4").unwrap(), Value::Float(6.0));
    }

    // Comparison operations tests
    #[test]
    fn test_numeric_comparisons() {
        assert_eq!(run_test("5 > 3").unwrap(), Value::Bool(true));
        assert_eq!(run_test("2 < 7").unwrap(), Value::Bool(true));
        assert_eq!(run_test("4 >= 4").unwrap(), Value::Bool(true));
        assert_eq!(run_test("6 <= 5").unwrap(), Value::Bool(false));
        assert_eq!(run_test("3 == 3").unwrap(), Value::Bool(true));
        // Note: != operator is not currently supported
    }

    #[test]
    fn test_string_operations() {
        // Note: String concatenation with + is not currently supported
        assert_eq!(run_test("\"abc\" == \"abc\"").unwrap(), Value::Bool(true));
        assert_eq!(run_test("\"abc\" == \"def\"").unwrap(), Value::Bool(false));
    }

    // Note: Logical operators (&&, ||, !) are not currently supported in Husk

    // Control flow tests
    #[test]
    fn test_if_else_expression() {
        assert_eq!(
            run_test("if true { 5 } else { 10 }").unwrap(),
            Value::Int(5)
        );
        assert_eq!(
            run_test("if false { 5 } else { 10 }").unwrap(),
            Value::Int(10)
        );
        assert_eq!(
            run_test("if 5 > 3 { \"yes\" } else { \"no\" }").unwrap(),
            Value::String("yes".to_string())
        );
    }

    #[test]
    fn test_nested_if_else() {
        let program = r#"
            let result = if 5 > 3 {
                if 2 < 4 {
                    "both true"
                } else {
                    "first true"
                }
            } else {
                "first false"
            };
            result
        "#;
        assert_eq!(
            run_test(program).unwrap(),
            Value::String("both true".to_string())
        );
    }

    // Loop tests
    #[test]
    fn test_for_loop() {
        let program = r#"
            let sum = 0;
            for i in 1..4 {
                sum = sum + i;
            }
            sum
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(6)); // 1 + 2 + 3
    }

    #[test]
    fn test_for_loop_inclusive() {
        let program = r#"
            let sum = 0;
            for i in 1..=3 {
                sum = sum + i;
            }
            sum
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(6)); // 1 + 2 + 3
    }

    #[test]
    fn test_while_loop() {
        let program = r#"
            let count = 0;
            let sum = 0;
            while count < 3 {
                sum = sum + count;
                count = count + 1;
            }
            sum
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(3)); // 0 + 1 + 2
    }

    #[test]
    fn test_break_in_loop() {
        let program = r#"
            let sum = 0;
            for i in 1..10 {
                if i > 3 {
                    break;
                }
                sum = sum + i;
            }
            sum
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(6)); // 1 + 2 + 3
    }

    #[test]
    fn test_continue_in_loop() {
        let program = r#"
            let sum = 0;
            for i in 1..=5 {
                if i == 3 {
                    continue;
                }
                sum = sum + i;
            }
            sum
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(12)); // 1 + 2 + 4 + 5
    }

    // Function tests
    #[test]
    fn test_function_definition_and_call() {
        let program = r#"
            fn add(a: int, b: int) -> int {
                a + b
            }
            add(3, 4)
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(7));
    }

    #[test]
    fn test_recursive_function() {
        let program = r#"
            fn factorial(n: int) -> int {
                if n <= 1 {
                    1
                } else {
                    n * factorial(n - 1)
                }
            }
            factorial(5)
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(120));
    }

    #[test]
    fn test_function_with_return() {
        let program = r#"
            fn early_return(x: int) -> int {
                if x > 10 {
                    return 10;
                }
                x * 2
            }
            early_return(15)
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(10));
    }

    // Variable tests
    #[test]
    fn test_variable_binding() {
        let program = r#"
            let x = 5;
            let y = x + 3;
            y
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(8));
    }

    #[test]
    fn test_variable_reassignment() {
        let program = r#"
            let x = 5;
            x = x + 3;
            x
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(8));
    }

    #[test]
    fn test_variable_shadowing() {
        let program = r#"
            let x = 5;
            let x = "hello";
            x
        "#;
        assert_eq!(
            run_test(program).unwrap(),
            Value::String("hello".to_string())
        );
    }

    // Array tests
    #[test]
    fn test_array_creation_and_access() {
        let program = r#"
            let arr = [1, 2, 3, 4];
            arr[2]
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(3));
    }

    #[test]
    fn test_array_slicing() {
        let program = r#"
            let arr = [1, 2, 3, 4, 5];
            let slice = arr[1..4];
            slice[1]
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(3));
    }

    #[test]
    fn test_array_bounds_error() {
        let program = r#"
            let arr = [1, 2, 3];
            arr[10]
        "#;
        assert!(run_test(program).is_err());
    }

    // Struct tests
    #[test]
    fn test_struct_creation_and_access() {
        let program = r#"
            struct Point {
                x: int,
                y: int,
            }
            let p = Point { x: 10, y: 20 };
            p.x + p.y
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(30));
    }

    // TODO: Fix self type handling in semantic analyzer
    // #[test]
    // fn test_struct_method() {
    //     let program = r#"
    //         struct Rectangle {
    //             width: int,
    //             height: int,
    //         }
    //         impl Rectangle {
    //             fn area(self) -> int {
    //                 self.width * self.height
    //             }
    //         }
    //         let rect = Rectangle { width: 5, height: 3 };
    //         Rectangle::area(rect)
    //     "#;
    //     assert_eq!(run_test(program).unwrap(), Value::Int(15));
    // }

    // Enum tests
    #[test]
    fn test_enum_variant_creation() {
        let program = r#"
            enum Option {
                Some(int),
                None,
            }
            let x = Option::Some(42);
            match x {
                Option::Some(n) => n,
                Option::None => 0,
            }
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(42));
    }

    #[test]
    fn test_enum_pattern_matching() {
        let program = r#"
            enum Result {
                Ok(int),
                Err(string),
            }
            let result = Result::Err("error");
            match result {
                Result::Ok(n) => n,
                Result::Err(_) => 999,
            }
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(999));
    }

    // Block expression tests
    #[test]
    fn test_block_expression_value() {
        let program = r#"
            let x = {
                let a = 5;
                let b = 3;
                a + b
            };
            x
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(8));
    }

    #[test]
    fn test_block_with_statements() {
        let program = r#"
            let x = {
                let a = 5;
                let b = 3;
                a + b;
                10
            };
            x
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(10));
    }

    // Range tests
    #[test]
    fn test_range_creation() {
        assert!(matches!(
            run_test("1..5").unwrap(),
            Value::Range(Some(1), Some(5), false)
        ));
        assert!(matches!(
            run_test("1..=5").unwrap(),
            Value::Range(Some(1), Some(5), true)
        ));
    }

    // Error handling tests
    #[test]
    fn test_undefined_variable() {
        assert!(run_test("undefined_var").is_err());
    }

    #[test]
    fn test_type_mismatch_in_operation() {
        assert!(run_test("5 + \"hello\"").is_err());
    }

    #[test]
    fn test_division_by_zero() {
        assert!(run_test("5 / 0").is_err());
    }

    // Print macro tests
    #[test]
    fn test_print_function() {
        // print! returns Int(0), println! returns Unit
        assert_eq!(run_test("print!(\"hello\")").unwrap(), Value::Int(0));
        assert_eq!(run_test("println!(\"world\")").unwrap(), Value::Unit);
    }

    // Complex expression tests
    #[test]
    fn test_complex_expression() {
        // Test if expressions as the final expression in a program
        let program = r#"
            let x = 5;
            if x > 3 {
                (x * 2) + 1
            } else {
                (x + 10) + 1
            }
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(11)); // (5 * 2) + 1
    }

    // Note: Anonymous functions are not currently supported in Husk

    // Unary operator tests
    #[test]
    fn test_unary_negation() {
        // Integer negation
        assert_eq!(run_test("-5").unwrap(), Value::Int(-5));
        assert_eq!(run_test("-(-10)").unwrap(), Value::Int(10));

        // Float negation
        assert_eq!(run_test("-3.1").unwrap(), Value::Float(-3.1));
        assert_eq!(run_test("-(-2.5)").unwrap(), Value::Float(2.5));

        // Double negation
        assert_eq!(run_test("--5").unwrap(), Value::Int(5));
    }

    #[test]
    fn test_unary_not() {
        // Boolean NOT
        assert_eq!(run_test("!true").unwrap(), Value::Bool(false));
        assert_eq!(run_test("!false").unwrap(), Value::Bool(true));

        // Double NOT
        assert_eq!(run_test("!!true").unwrap(), Value::Bool(true));
        assert_eq!(run_test("!!false").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_unary_in_expressions() {
        // Negation in arithmetic
        assert_eq!(run_test("5 + -3").unwrap(), Value::Int(2));
        assert_eq!(run_test("-5 * 2").unwrap(), Value::Int(-10));
        assert_eq!(run_test("10 / -2").unwrap(), Value::Int(-5));

        // NOT in boolean expressions
        assert_eq!(run_test("!true == false").unwrap(), Value::Bool(true));
        assert_eq!(run_test("!(5 > 3)").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_unary_with_variables() {
        let program = r#"
            let x = 10;
            let y = -x;
            y
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(-10));

        let program = r#"
            let flag = true;
            let opposite = !flag;
            opposite
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_unary_errors() {
        // Cannot negate non-numeric - caught by semantic analyzer
        let err = run_test("-true").unwrap_err();
        assert!(err
            .to_string()
            .contains("Unary negation requires numeric type"));

        // Cannot NOT non-boolean - caught by semantic analyzer
        let err = run_test("!5").unwrap_err();
        assert!(err.to_string().contains("Logical NOT requires bool type"));
    }

    #[test]
    fn test_compound_assignment() {
        // Integer compound assignment
        let program = r#"
            let x = 10;
            x += 5;
            x
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(15));

        let program = r#"
            let x = 20;
            x -= 8;
            x
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(12));

        let program = r#"
            let x = 5;
            x *= 3;
            x
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(15));

        let program = r#"
            let x = 20;
            x /= 4;
            x
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(5));

        let program = r#"
            let x = 17;
            x %= 5;
            x
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Int(2));

        // Float compound assignment
        let program = r#"
            let y = 10.0;
            y += 2.5;
            y
        "#;
        assert_eq!(run_test(program).unwrap(), Value::Float(12.5));
    }

    #[test]
    fn test_compound_assignment_complex_targets() {
        // Array element compound assignment now supported
        let program = r#"
            let arr = [1, 2, 3];
            arr[0] += 5;
            arr[0]  // Return the modified value
        "#;
        let result = run_test(program).unwrap();
        assert_eq!(result, Value::Int(6)); // 1 + 5 = 6

        // Struct field compound assignment now supported
        let program = r#"
            struct Point { x: int, y: int }
            let p = Point { x: 10, y: 20 };
            p.x += 5;
            p.x  // Return the modified value
        "#;
        let result = run_test(program).unwrap();
        assert_eq!(result, Value::Int(15)); // 10 + 5 = 15

        // Test that the operations actually work (no errors thrown)
        let program = r#"
            struct Point { x: int, y: int }
            let arr = [5, 10];
            let p = Point { x: 2, y: 4 };

            arr[0] *= 3;    // Should not error
            arr[1] /= 2;    // Should not error
            p.x += 8;       // Should not error
            p.y -= 1;       // Should not error

            42  // Return something to indicate success
        "#;
        let result = run_test(program).unwrap();
        assert_eq!(result, Value::Int(42)); // Just verify no errors occurred
    }
}
