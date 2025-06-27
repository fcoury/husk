# Husk IO Operations Plan

This document outlines the planned IO operations for Husk, providing essential file system and console interaction capabilities.

## Overview

IO operations are fundamental for practical programming. This plan introduces a set of IO functions that will allow Husk programs to interact with the file system and console, supporting both synchronous and asynchronous operations.

## Design Principles

1. **Safety First**: All operations return `Result<T, Error>` types
2. **UTF-8 by Default**: String operations assume UTF-8 encoding
3. **Async-Ready**: Core operations have async variants for non-blocking IO
4. **Cross-Platform**: Works consistently across different operating systems
5. **Transpiler Support**: Maps to Node.js fs module or browser APIs appropriately

## Planned IO Operations

### 1. File Reading Operations

#### `read_file(path: string) -> Result<string, Error>`
Read entire file contents as a string.
```rust
let contents = read_file("config.json")?;
println(contents);
```

#### `read_file_bytes(path: string) -> Result<array<u8>, Error>`
Read file contents as raw bytes.
```rust
let data = read_file_bytes("image.png")?;
```

#### `read_lines(path: string) -> Result<array<string>, Error>`
Read file as an array of lines.
```rust
let lines = read_lines("data.txt")?;
for line in lines {
    process_line(line);
}
```

### 2. File Writing Operations

#### `write_file(path: string, contents: string) -> Result<(), Error>`
Write string contents to a file (overwrites if exists).
```rust
write_file("output.txt", "Hello, World!")?;
```

#### `write_file_bytes(path: string, data: array<u8>) -> Result<(), Error>`
Write raw bytes to a file.
```rust
write_file_bytes("data.bin", bytes)?;
```

#### `append_file(path: string, contents: string) -> Result<(), Error>`
Append string to an existing file.
```rust
append_file("log.txt", "New log entry\n")?;
```

### 3. Directory Operations

#### `create_dir(path: string) -> Result<(), Error>`
Create a directory (fails if exists).
```rust
create_dir("output")?;
```

#### `create_dir_all(path: string) -> Result<(), Error>`
Create directory and all parent directories.
```rust
create_dir_all("output/images/thumbnails")?;
```

#### `remove_dir(path: string) -> Result<(), Error>`
Remove an empty directory.
```rust
remove_dir("temp")?;
```

#### `remove_dir_all(path: string) -> Result<(), Error>`
Remove directory and all contents recursively.
```rust
remove_dir_all("old_project")?;
```

#### `read_dir(path: string) -> Result<array<DirEntry>, Error>`
List directory contents.
```rust
let entries = read_dir(".")?;
for entry in entries {
    println(entry.name);
}
```

### 4. Path Operations

#### `exists(path: string) -> bool`
Check if a path exists.
```rust
if exists("config.json") {
    let config = read_file("config.json")?;
}
```

#### `is_file(path: string) -> bool`
Check if path is a file.

#### `is_dir(path: string) -> bool`
Check if path is a directory.

#### `join_path(parts: array<string>) -> string`
Join path segments with proper separators.
```rust
let config_path = join_path(["home", "user", ".config", "app.json"]);
```

### 5. Console Operations

#### `read_line() -> Result<string, Error>`
Read a line from stdin.
```rust
print("Enter your name: ");
let name = read_line()?;
println("Hello, " + name + "!");
```

#### `print(message: string)`
Print without newline (already exists as println with newline).

#### `eprint(message: string)` / `eprintln(message: string)`
Print to stderr.
```rust
eprintln("Error: File not found");
```

### 6. File Metadata

#### `file_size(path: string) -> Result<int, Error>`
Get file size in bytes.

#### `file_modified(path: string) -> Result<DateTime, Error>`
Get last modification time.

### 7. Async Variants (Future Enhancement)

All file operations will have async variants:
- `read_file_async(path: string) -> Promise<Result<string, Error>>`
- `write_file_async(path: string, contents: string) -> Promise<Result<(), Error>>`
- etc.

```rust
async fn process_files() -> Result<(), Error> {
    let contents = read_file_async("input.txt").await?;
    let processed = transform(contents);
    write_file_async("output.txt", processed).await?;
    Ok(())
}
```

## Implementation Strategy

### Phase 1: Core File Operations
1. Implement `read_file`, `write_file`, `exists`
2. Add proper error handling with descriptive messages
3. Create comprehensive tests

### Phase 2: Extended File Operations  
1. Add `read_lines`, `append_file`, `read_file_bytes`, `write_file_bytes`
2. Implement directory operations
3. Add path utilities

### Phase 3: Console IO
1. Implement `read_line` for interactive programs
2. Add stderr output functions
3. Consider formatted output options

### Phase 4: Async Support
1. Add async variants of all file operations
2. Ensure proper Promise/Future integration
3. Support for concurrent IO operations

## Transpiler Considerations

### Node.js Target
- Map to `fs` module functions
- Use `fs.promises` for async operations
- Handle path separators correctly

### Browser Target
- Use File API where available
- Provide polyfills or graceful degradation
- Consider localStorage for simple persistence

## Error Types

```rust
enum IOError {
    FileNotFound(String),
    PermissionDenied(String),
    AlreadyExists(String),
    NotADirectory(String),
    DirectoryNotEmpty(String),
    InvalidPath(String),
    IOError(String),  // Generic IO error with message
}
```

## Example Programs

### Simple File Copy
```rust
fn copy_file(source: string, dest: string) -> Result<(), Error> {
    let contents = read_file(source)?;
    write_file(dest, contents)?;
    Ok(())
}
```

### Directory Listing
```rust
fn list_files(dir: string) -> Result<(), Error> {
    let entries = read_dir(dir)?;
    for entry in entries {
        if entry.is_file {
            println("📄 " + entry.name);
        } else {
            println("📁 " + entry.name);
        }
    }
    Ok(())
}
```

### Interactive Program
```rust
fn main() -> Result<(), Error> {
    print("What's your name? ");
    let name = read_line()?.trim();
    
    let greeting = "Hello, " + name + "!";
    println(greeting);
    
    // Save to file
    write_file("greeting.txt", greeting)?;
    println("Greeting saved to greeting.txt");
    
    Ok(())
}
```

## Testing Strategy

1. **Unit Tests**: Test each operation with various inputs
2. **Integration Tests**: Test file operations with actual filesystem
3. **Error Tests**: Verify proper error handling
4. **Cross-Platform Tests**: Ensure consistency across OS
5. **Transpiler Tests**: Verify JavaScript output works correctly

## Priority Order

1. **High Priority**: Basic file read/write, exists check
2. **Medium Priority**: Directory operations, console input
3. **Low Priority**: Async variants, advanced metadata

This plan provides Husk with essential IO capabilities while maintaining safety and cross-platform compatibility.