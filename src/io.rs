use crate::{
    error::{Error, Result},
    interpreter::Value,
    span::Span,
};
use std::fs;
use std::path::Path;

/// Read entire file contents as a string
pub fn read_file(path: &str, span: &Span) -> Result<Value> {
    match fs::read_to_string(path) {
        Ok(contents) => Ok(Value::String(contents)),
        Err(e) => {
            let error_msg = match e.kind() {
                std::io::ErrorKind::NotFound => format!("File not found: {}", path),
                std::io::ErrorKind::PermissionDenied => format!("Permission denied: {}", path),
                _ => format!("IO error reading {}: {}", path, e),
            };
            Err(Error::new_runtime(&error_msg, *span))
        }
    }
}

/// Read file contents as raw bytes
pub fn read_file_bytes(path: &str, span: &Span) -> Result<Value> {
    match fs::read(path) {
        Ok(bytes) => {
            // Convert bytes to array of integers (since Husk doesn't have u8 type)
            let values: Vec<Value> = bytes.into_iter().map(|b| Value::Int(b as i64)).collect();
            Ok(Value::Array(values))
        }
        Err(e) => {
            let error_msg = match e.kind() {
                std::io::ErrorKind::NotFound => format!("File not found: {}", path),
                std::io::ErrorKind::PermissionDenied => format!("Permission denied: {}", path),
                _ => format!("IO error reading {}: {}", path, e),
            };
            Err(Error::new_runtime(&error_msg, *span))
        }
    }
}

/// Read file as an array of lines
pub fn read_lines(path: &str, span: &Span) -> Result<Value> {
    match fs::read_to_string(path) {
        Ok(contents) => {
            let lines: Vec<Value> = contents
                .lines()
                .map(|line| Value::String(line.to_string()))
                .collect();
            Ok(Value::Array(lines))
        }
        Err(e) => {
            let error_msg = match e.kind() {
                std::io::ErrorKind::NotFound => format!("File not found: {}", path),
                std::io::ErrorKind::PermissionDenied => format!("Permission denied: {}", path),
                _ => format!("IO error reading {}: {}", path, e),
            };
            Err(Error::new_runtime(&error_msg, *span))
        }
    }
}

/// Write string contents to a file (overwrites if exists)
pub fn write_file(path: &str, contents: &str, span: &Span) -> Result<Value> {
    match fs::write(path, contents) {
        Ok(()) => Ok(Value::Unit),
        Err(e) => {
            let error_msg = match e.kind() {
                std::io::ErrorKind::PermissionDenied => format!("Permission denied: {}", path),
                std::io::ErrorKind::InvalidInput => format!("Invalid path: {}", path),
                _ => format!("IO error writing {}: {}", path, e),
            };
            Err(Error::new_runtime(&error_msg, *span))
        }
    }
}

/// Write raw bytes to a file
pub fn write_file_bytes(path: &str, data: &[Value], span: &Span) -> Result<Value> {
    // Convert Value array to bytes
    let mut bytes = Vec::with_capacity(data.len());
    for (i, val) in data.iter().enumerate() {
        match val {
            Value::Int(n) => {
                if *n < 0 || *n > 255 {
                    return Err(Error::new_runtime(
                        &format!("Byte value out of range at index {}: {}", i, n),
                        *span,
                    ));
                }
                bytes.push(*n as u8);
            }
            _ => {
                return Err(Error::new_runtime(
                    &format!("Expected integer at index {}, got {:?}", i, val),
                    *span,
                ))
            }
        }
    }

    match fs::write(path, bytes) {
        Ok(()) => Ok(Value::Unit),
        Err(e) => {
            let error_msg = match e.kind() {
                std::io::ErrorKind::PermissionDenied => format!("Permission denied: {}", path),
                std::io::ErrorKind::InvalidInput => format!("Invalid path: {}", path),
                _ => format!("IO error writing {}: {}", path, e),
            };
            Err(Error::new_runtime(&error_msg, *span))
        }
    }
}

/// Append string to an existing file
pub fn append_file(path: &str, contents: &str, span: &Span) -> Result<Value> {
    use std::fs::OpenOptions;
    use std::io::Write;

    match OpenOptions::new().create(true).append(true).open(path) {
        Ok(mut file) => match file.write_all(contents.as_bytes()) {
            Ok(()) => Ok(Value::Unit),
            Err(e) => Err(Error::new_runtime(
                &format!("IO error appending to {}: {}", path, e),
                *span,
            )),
        },
        Err(e) => {
            let error_msg = match e.kind() {
                std::io::ErrorKind::PermissionDenied => format!("Permission denied: {}", path),
                std::io::ErrorKind::InvalidInput => format!("Invalid path: {}", path),
                _ => format!("IO error opening {}: {}", path, e),
            };
            Err(Error::new_runtime(&error_msg, *span))
        }
    }
}

/// Check if a path exists
pub fn exists(path: &str) -> Value {
    Value::Bool(Path::new(path).exists())
}

/// Check if path is a file
pub fn is_file(path: &str) -> Value {
    Value::Bool(Path::new(path).is_file())
}

/// Check if path is a directory
pub fn is_dir(path: &str) -> Value {
    Value::Bool(Path::new(path).is_dir())
}

/// Read a line from stdin
pub fn read_line(span: &Span) -> Result<Value> {
    use std::io::{self, BufRead};

    let stdin = io::stdin();
    let mut line = String::new();

    match stdin.lock().read_line(&mut line) {
        Ok(_) => {
            // Remove the trailing newline if present
            if line.ends_with('\n') {
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
            }
            Ok(Value::String(line))
        }
        Err(e) => {
            let error_msg = match e.kind() {
                std::io::ErrorKind::UnexpectedEof => "Unexpected end of input".to_string(),
                std::io::ErrorKind::Interrupted => "Input interrupted".to_string(),
                _ => format!("IO error reading from stdin: {}", e),
            };
            Err(Error::new_runtime(&error_msg, *span))
        }
    }
}

/// Print formatted string to stderr (returns number of bytes written)
pub fn eprint(message: &str) -> Value {
    use std::io::{self, Write};

    match io::stderr().write(message.as_bytes()) {
        Ok(n) => Value::Int(n as i64),
        Err(_) => Value::Int(0),
    }
}

/// Print formatted string to stderr with newline
pub fn eprintln(message: &str) -> Value {
    use std::io::{self, Write};

    let full_message = format!("{}\n", message);
    match io::stderr().write(full_message.as_bytes()) {
        Ok(_) => Value::Unit,
        Err(_) => Value::Unit,
    }
}

/// Create a directory (fails if exists)
pub fn create_dir(path: &str, span: &Span) -> Result<Value> {
    match fs::create_dir(path) {
        Ok(()) => Ok(Value::Unit),
        Err(e) => {
            let error_msg = match e.kind() {
                std::io::ErrorKind::AlreadyExists => format!("Directory already exists: {}", path),
                std::io::ErrorKind::PermissionDenied => format!("Permission denied: {}", path),
                std::io::ErrorKind::NotFound => format!("Parent directory not found: {}", path),
                _ => format!("IO error creating directory {}: {}", path, e),
            };
            Err(Error::new_runtime(&error_msg, *span))
        }
    }
}

/// Create directory and all parent directories
pub fn create_dir_all(path: &str, span: &Span) -> Result<Value> {
    match fs::create_dir_all(path) {
        Ok(()) => Ok(Value::Unit),
        Err(e) => {
            let error_msg = match e.kind() {
                std::io::ErrorKind::PermissionDenied => format!("Permission denied: {}", path),
                std::io::ErrorKind::InvalidInput => format!("Invalid path: {}", path),
                _ => format!("IO error creating directories {}: {}", path, e),
            };
            Err(Error::new_runtime(&error_msg, *span))
        }
    }
}

/// Remove an empty directory
pub fn remove_dir(path: &str, span: &Span) -> Result<Value> {
    match fs::remove_dir(path) {
        Ok(()) => Ok(Value::Unit),
        Err(e) => {
            let error_msg = match e.kind() {
                std::io::ErrorKind::NotFound => format!("Directory not found: {}", path),
                std::io::ErrorKind::PermissionDenied => format!("Permission denied: {}", path),
                _ => {
                    // Check if directory is not empty
                    if Path::new(path).is_dir() {
                        match fs::read_dir(path) {
                            Ok(mut entries) => {
                                if entries.next().is_some() {
                                    format!("Directory not empty: {}", path)
                                } else {
                                    format!("IO error removing directory {}: {}", path, e)
                                }
                            }
                            _ => format!("IO error removing directory {}: {}", path, e),
                        }
                    } else {
                        format!("IO error removing directory {}: {}", path, e)
                    }
                }
            };
            Err(Error::new_runtime(&error_msg, *span))
        }
    }
}

/// Remove directory and all contents recursively
pub fn remove_dir_all(path: &str, span: &Span) -> Result<Value> {
    match fs::remove_dir_all(path) {
        Ok(()) => Ok(Value::Unit),
        Err(e) => {
            let error_msg = match e.kind() {
                std::io::ErrorKind::NotFound => format!("Directory not found: {}", path),
                std::io::ErrorKind::PermissionDenied => format!("Permission denied: {}", path),
                _ => format!("IO error removing directory {}: {}", path, e),
            };
            Err(Error::new_runtime(&error_msg, *span))
        }
    }
}

// Async file operations - these are only used in the transpiler
// The interpreter doesn't support async operations yet

/// Async read entire file contents as a string
pub fn read_file_async(_path: &str, _span: &Span) -> Result<Value> {
    // This function is only for semantic analysis in the interpreter
    // The actual async behavior is handled by the transpiler
    Err(Error::new_runtime(
        "Async operations are not supported in the interpreter mode",
        Span::default(),
    ))
}

/// Async read file contents as raw bytes
pub fn read_file_bytes_async(_path: &str, _span: &Span) -> Result<Value> {
    Err(Error::new_runtime(
        "Async operations are not supported in the interpreter mode",
        Span::default(),
    ))
}

/// Async read file as an array of lines
pub fn read_lines_async(_path: &str, _span: &Span) -> Result<Value> {
    Err(Error::new_runtime(
        "Async operations are not supported in the interpreter mode",
        Span::default(),
    ))
}

/// Async write string contents to a file
pub fn write_file_async(_path: &str, _contents: &str, _span: &Span) -> Result<Value> {
    Err(Error::new_runtime(
        "Async operations are not supported in the interpreter mode",
        Span::default(),
    ))
}

/// Async write raw bytes to a file
pub fn write_file_bytes_async(_path: &str, _data: &[Value], _span: &Span) -> Result<Value> {
    Err(Error::new_runtime(
        "Async operations are not supported in the interpreter mode",
        Span::default(),
    ))
}

/// Async append string to an existing file
pub fn append_file_async(_path: &str, _contents: &str, _span: &Span) -> Result<Value> {
    Err(Error::new_runtime(
        "Async operations are not supported in the interpreter mode",
        Span::default(),
    ))
}

/// List directory contents
pub fn read_dir(path: &str, span: &Span) -> Result<Value> {
    match fs::read_dir(path) {
        Ok(entries) => {
            let mut result = Vec::new();

            for entry in entries {
                match entry {
                    Ok(entry) => {
                        let file_name = entry.file_name().to_string_lossy().to_string();
                        let is_file = entry.file_type().map(|t| t.is_file()).unwrap_or(false);
                        let is_dir = entry.file_type().map(|t| t.is_dir()).unwrap_or(false);

                        // Create a struct-like value for DirEntry
                        let mut fields = indexmap::IndexMap::new();
                        fields.insert("name".to_string(), Value::String(file_name));
                        fields.insert("is_file".to_string(), Value::Bool(is_file));
                        fields.insert("is_dir".to_string(), Value::Bool(is_dir));

                        result.push(Value::StructInstance("DirEntry".to_string(), fields));
                    }
                    Err(e) => {
                        return Err(Error::new_runtime(
                            &format!("Error reading directory entry: {}", e),
                            *span,
                        ))
                    }
                }
            }

            Ok(Value::Array(result))
        }
        Err(e) => {
            let error_msg = match e.kind() {
                std::io::ErrorKind::NotFound => format!("Directory not found: {}", path),
                std::io::ErrorKind::PermissionDenied => format!("Permission denied: {}", path),
                std::io::ErrorKind::NotADirectory => format!("Not a directory: {}", path),
                _ => format!("IO error reading directory {}: {}", path, e),
            };
            Err(Error::new_runtime(&error_msg, *span))
        }
    }
}
