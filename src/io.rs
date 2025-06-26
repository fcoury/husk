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
