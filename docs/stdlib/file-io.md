# File I/O

File Input/Output operations are essential for most applications. Husk provides safe, efficient file handling with comprehensive error handling. This guide covers all file operations in the standard library.

## Table of Contents

- [Overview](#overview)
- [Reading Files](#reading-files)
  - [Reading Text Files](#reading-text-files)
  - [Reading Binary Files](#reading-binary-files)
  - [Reading Line by Line](#reading-line-by-line)
- [Writing Files](#writing-files)
  - [Writing Text Files](#writing-text-files)
  - [Writing Binary Files](#writing-binary-files)
  - [Appending to Files](#appending-to-files)
- [File Operations](#file-operations)
  - [File Metadata](#file-metadata)
  - [File Management](#file-management)
  - [File Permissions](#file-permissions)
- [Directory Operations](#directory-operations)
- [Path Operations](#path-operations)
- [Error Handling](#error-handling)
- [Async File I/O](#async-file-io)
- [Common Patterns](#common-patterns)
- [Performance Tips](#performance-tips)
- [JavaScript/Node.js Interop](#javascriptnodejs-interop)
- [Related Topics](#related-topics)

## Overview

Husk's file I/O features:
- **Safe by default** - All operations return Result types
- **UTF-8 text support** - Handles Unicode correctly
- **Binary file support** - Raw byte operations
- **Cross-platform** - Works on all supported platforms
- **Efficient buffering** - Optimized for performance

### Key Principles

1. **Explicit error handling** - All I/O can fail
2. **Resource management** - Files are automatically closed
3. **Path abstraction** - Platform-independent paths
4. **Buffered I/O** - Efficient reading and writing

## Reading Files

### Reading Text Files

```rust
// Read entire file to string
fn read_file_simple() -> Result<(), Error> {
    let contents = read_file("hello.txt")?;
    println!("File contents: {}", contents);
    Ok(())
}

// Read with error handling
match read_file("config.json") {
    Ok(contents) => {
        println!("Successfully read {} bytes", contents.len());
        process_config(contents);
    },
    Err(e) => {
        eprintln!("Failed to read file: {}", e);
    }
}

// Read to string with explicit encoding
let contents = read_file_with_encoding("data.txt", Encoding::UTF8)?;

// Read and parse
let json_data = read_file("data.json")?;
let parsed: MyData = json::parse(&json_data)?;
```

### Reading Binary Files

```rust
// Read binary file to bytes
let bytes = read_file_bytes("image.png")?;
println!("Read {} bytes", bytes.len());

// Read with limited size
let max_size = 1024 * 1024; // 1MB
let bytes = read_file_bytes_limited("large.bin", max_size)?;

// Process binary data
let header = &bytes[0..4];
if header == b"PNG\x89" {
    println!("PNG image detected");
}

// Read into buffer
let mut buffer = [0u8; 1024];
let bytes_read = read_file_into_buffer("data.bin", &mut buffer)?;
```

### Reading Line by Line

```rust
// Read lines as iterator
let file = File::open("lines.txt")?;
for line in file.lines() {
    let line = line?; // Handle potential error
    println!("Line: {}", line);
}

// Process lines with line numbers
let file = File::open("source.rs")?;
for (num, line) in file.lines().enumerate() {
    let line = line?;
    println!("{:4}: {}", num + 1, line);
}

// Filter and collect lines
let comments: Vec<String> = File::open("code.rs")?
    .lines()
    .filter_map(Result::ok)
    .filter(|line| line.trim().starts_with("//"))
    .collect();

// Read lines into array
let lines = read_lines("todo.txt")?;
for line in lines {
    process_todo_item(&line);
}
```

## Writing Files

### Writing Text Files

```rust
// Write string to file
write_file("output.txt", "Hello, World!")?;

// Write with formatting
let data = format!("Name: {}\nAge: {}\n", name, age);
write_file("person.txt", &data)?;

// Write lines
let lines = vec!["Line 1", "Line 2", "Line 3"];
write_lines("lines.txt", &lines)?;

// Atomic write (write to temp, then rename)
write_file_atomic("important.dat", &critical_data)?;

// Write with specific encoding
write_file_with_encoding("utf16.txt", &text, Encoding::UTF16)?;
```

### Writing Binary Files

```rust
// Write bytes
let bytes = vec![0xFF, 0xFE, 0xFD];
write_file_bytes("binary.dat", &bytes)?;

// Write struct as bytes
let data = MyStruct { x: 10, y: 20 };
let bytes = data.to_bytes();
write_file_bytes("struct.dat", &bytes)?;

// Write with buffer
let mut file = File::create("large.bin")?;
let mut writer = BufWriter::new(file);
for chunk in data_chunks {
    writer.write_all(chunk)?;
}
writer.flush()?;
```

### Appending to Files

```rust
// Append text
append_file("log.txt", "New log entry\n")?;

// Append with timestamp
let entry = format!("[{}] {}\n", timestamp(), message);
append_file("app.log", &entry)?;

// Append multiple lines
let new_items = vec!["Item 4", "Item 5"];
append_lines("list.txt", &new_items)?;

// Open file for appending
let mut file = File::options()
    .append(true)
    .create(true)
    .open("data.txt")?;
file.write_all(b"Appended data\n")?;
```

## File Operations

### File Metadata

```rust
// Check if file exists
if file_exists("config.json") {
    load_config();
}

// Get file size
let size = file_size("download.zip")?;
println!("File size: {} bytes", size);

// Get file metadata
let metadata = file_metadata("document.pdf")?;
println!("Size: {} bytes", metadata.size);
println!("Modified: {:?}", metadata.modified);
println!("Created: {:?}", metadata.created);
println!("Is directory: {}", metadata.is_dir);
println!("Is file: {}", metadata.is_file);
println!("Permissions: {:?}", metadata.permissions);

// Check file type
if is_file("data.txt") {
    println!("It's a regular file");
}
if is_directory("src") {
    println!("It's a directory");
}
```

### File Management

```rust
// Copy file
copy_file("source.txt", "destination.txt")?;

// Move/rename file
move_file("old_name.txt", "new_name.txt")?;
rename_file("temp.txt", "final.txt")?;

// Delete file
delete_file("unwanted.txt")?;

// Safe delete (only if exists)
if file_exists("maybe.txt") {
    delete_file("maybe.txt")?;
}

// Create hard link
hard_link("original.txt", "link.txt")?;

// Create symbolic link
symlink("target.txt", "symlink.txt")?;

// Create temporary file
let temp_file = create_temp_file()?;
write_file(&temp_file.path(), "temporary data")?;
// temp_file is automatically deleted when dropped
```

### File Permissions

```rust
// Get permissions
let perms = file_permissions("script.sh")?;
println!("Readable: {}", perms.is_readable());
println!("Writable: {}", perms.is_writable());
println!("Executable: {}", perms.is_executable());

// Set permissions (Unix-like systems)
set_permissions("script.sh", 0o755)?; // rwxr-xr-x

// Make file executable
make_executable("script.sh")?;

// Make file read-only
make_readonly("important.dat")?;

// Remove read-only
remove_readonly("editable.txt")?;
```

## Directory Operations

### Reading Directories

```rust
// List directory contents
let entries = read_dir("src")?;
for entry in entries {
    println!("Entry: {}", entry.name);
}

// List with full paths
let paths = list_dir_paths("src")?;
for path in paths {
    println!("Path: {}", path);
}

// Recursive directory walk
walk_dir("src", |path| {
    if path.ends_with(".rs") {
        println!("Rust file: {}", path);
    }
    Ok(())
})?;

// Find files matching pattern
let rust_files = find_files("src", "*.rs")?;
let test_files = find_files(".", "*_test.hk")?;

// Get directory tree
let tree = directory_tree("project")?;
print_tree(&tree);
```

### Creating Directories

```rust
// Create single directory
create_dir("output")?;

// Create directory path (recursive)
create_dir_all("path/to/nested/dir")?;

// Create with permissions
create_dir_with_perms("secure", 0o700)?;

// Create temporary directory
let temp_dir = create_temp_dir()?;
let temp_path = temp_dir.path();
// Use temp_dir...
// Automatically cleaned up when dropped
```

### Directory Management

```rust
// Remove empty directory
remove_dir("empty_dir")?;

// Remove directory and contents
remove_dir_all("old_project")?;

// Copy directory recursively
copy_dir_all("source_dir", "dest_dir")?;

// Move directory
move_dir("old_location", "new_location")?;

// Check if directory is empty
if is_dir_empty("downloads")? {
    println!("Directory is empty");
}

// Get directory size
let size = dir_size("node_modules")?;
println!("Total size: {} bytes", size);
```

## Path Operations

### Path Manipulation

```rust
// Join paths
let path = path_join("home", "user", "documents");
// Result: "home/user/documents" (Unix) or "home\\user\\documents" (Windows)

// Get components
let full_path = "/home/user/file.txt";
let dir = path_dir(full_path);        // "/home/user"
let filename = path_filename(full_path); // "file.txt"
let stem = path_stem(full_path);      // "file"
let ext = path_extension(full_path);  // "txt"

// Change extension
let new_path = change_extension("doc.txt", "md");
// Result: "doc.md"

// Make absolute
let abs_path = absolute_path("../file.txt")?;

// Make relative
let rel_path = relative_path("/home/user/file.txt", "/home")?;
// Result: "user/file.txt"

// Normalize path (remove . and ..)
let normalized = normalize_path("./foo/../bar/./baz");
// Result: "bar/baz"
```

### Path Validation

```rust
// Check if path is absolute
if is_absolute_path("/home/user") {
    println!("Absolute path");
}

// Check if path is relative
if is_relative_path("../file.txt") {
    println!("Relative path");
}

// Validate path
if is_valid_path(user_input) {
    process_path(user_input);
} else {
    eprintln!("Invalid path");
}

// Check path exists
if path_exists("config.toml") {
    load_config();
}

// Get path type
match path_type("unknown")? {
    PathType::File => println!("It's a file"),
    PathType::Directory => println!("It's a directory"),
    PathType::Symlink => println!("It's a symlink"),
}
```

## Error Handling

### Common Errors

```rust
// Handle specific errors
match read_file("config.json") {
    Ok(content) => process_config(content),
    Err(e) => match e.kind() {
        ErrorKind::NotFound => {
            println!("Config not found, using defaults");
            use_default_config();
        },
        ErrorKind::PermissionDenied => {
            eprintln!("Permission denied reading config");
        },
        _ => eprintln!("Error reading config: {}", e),
    }
}

// Provide context
let content = read_file("data.txt")
    .map_err(|e| format!("Failed to read data.txt: {}", e))?;

// Chain operations with error propagation
fn process_file(path: &str) -> Result<Data, Error> {
    let content = read_file(path)?;
    let parsed = parse_content(&content)?;
    let validated = validate_data(parsed)?;
    Ok(validated)
}
```

### Error Recovery

```rust
// Fallback on error
let config = read_file("config.json")
    .or_else(|_| read_file("config.default.json"))
    .unwrap_or_else(|_| DEFAULT_CONFIG.to_string());

// Retry on failure
fn read_with_retry(path: &str, max_attempts: int) -> Result<String, Error> {
    for attempt in 0..max_attempts {
        match read_file(path) {
            Ok(content) => return Ok(content),
            Err(e) if attempt < max_attempts - 1 => {
                thread::sleep(Duration::from_millis(100 * (attempt + 1)));
                continue;
            },
            Err(e) => return Err(e),
        }
    }
    unreachable!()
}

// Create parent directories if needed
fn safe_write_file(path: &str, content: &str) -> Result<(), Error> {
    if let Some(parent) = path_dir(path) {
        create_dir_all(parent)?;
    }
    write_file(path, content)
}
```

## Async File I/O

### Async Reading

```rust
// Async file read
async fn read_async() -> Result<String, Error> {
    let content = async_read_file("large.txt").await?;
    Ok(content)
}

// Concurrent file reads
async fn read_multiple() -> Result<Vec<String>, Error> {
    let files = vec!["file1.txt", "file2.txt", "file3.txt"];
    let futures = files.iter()
        .map(|f| async_read_file(f))
        .collect::<Vec<_>>();
    
    let results = join_all(futures).await;
    results.into_iter().collect()
}

// Async line reading
async fn process_lines_async(path: &str) -> Result<(), Error> {
    let file = AsyncFile::open(path).await?;
    let mut lines = file.lines();
    
    while let Some(line) = lines.next().await {
        let line = line?;
        process_line_async(&line).await?;
    }
    Ok(())
}
```

### Async Writing

```rust
// Async file write
async fn write_async(path: &str, content: &str) -> Result<(), Error> {
    async_write_file(path, content).await?;
    Ok(())
}

// Async append
async fn log_async(message: &str) -> Result<(), Error> {
    let entry = format!("[{}] {}\n", timestamp(), message);
    async_append_file("app.log", &entry).await?;
    Ok(())
}

// Buffered async write
async fn write_large_async(path: &str, data: &[u8]) -> Result<(), Error> {
    let file = AsyncFile::create(path).await?;
    let mut writer = AsyncBufWriter::new(file);
    
    for chunk in data.chunks(8192) {
        writer.write_all(chunk).await?;
    }
    writer.flush().await?;
    Ok(())
}
```

## Common Patterns

### Configuration Files

```rust
// Load configuration with fallbacks
fn load_config() -> Result<Config, Error> {
    // Try user config first
    if let Ok(content) = read_file("~/.myapp/config.toml") {
        return Config::from_toml(&content);
    }
    
    // Try system config
    if let Ok(content) = read_file("/etc/myapp/config.toml") {
        return Config::from_toml(&content);
    }
    
    // Use default
    Ok(Config::default())
}

// Save configuration
fn save_config(config: &Config) -> Result<(), Error> {
    let path = "~/.myapp/config.toml";
    let content = config.to_toml()?;
    
    // Ensure directory exists
    create_dir_all(path_dir(path)?)?;
    
    // Write atomically
    write_file_atomic(path, &content)?;
    Ok(())
}
```

### Log Files

```rust
// Rotating log files
struct Logger {
    path: String,
    max_size: usize,
    max_files: usize,
}

impl Logger {
    fn log(&mut self, message: &str) -> Result<(), Error> {
        // Check file size
        if file_size(&self.path)? > self.max_size {
            self.rotate()?;
        }
        
        // Append log entry
        let entry = format!("[{}] {}\n", timestamp(), message);
        append_file(&self.path, &entry)?;
        Ok(())
    }
    
    fn rotate(&mut self) -> Result<(), Error> {
        // Rotate existing logs
        for i in (1..self.max_files).rev() {
            let old = format!("{}.{}", self.path, i);
            let new = format!("{}.{}", self.path, i + 1);
            if file_exists(&old) {
                rename_file(&old, &new)?;
            }
        }
        
        // Move current to .1
        rename_file(&self.path, &format!("{}.1", self.path))?;
        Ok(())
    }
}
```

### File Processing

```rust
// Process large file in chunks
fn process_large_file(path: &str) -> Result<(), Error> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut buffer = vec![0; 8192];
    
    loop {
        let bytes_read = reader.read(&mut buffer)?;
        if bytes_read == 0 {
            break;
        }
        
        process_chunk(&buffer[..bytes_read])?;
    }
    Ok(())
}

// Transform file
fn transform_file(input: &str, output: &str) -> Result<(), Error> {
    let reader = BufReader::new(File::open(input)?);
    let writer = BufWriter::new(File::create(output)?);
    
    for line in reader.lines() {
        let line = line?;
        let transformed = transform_line(&line);
        writeln!(writer, "{}", transformed)?;
    }
    
    writer.flush()?;
    Ok(())
}
```

## Performance Tips

### Buffering

```rust
// Use buffered I/O for better performance
// Bad: Many small reads
let mut file = File::open("data.txt")?;
let mut byte = [0];
while file.read(&mut byte)? > 0 {
    process_byte(byte[0]);
}

// Good: Buffered reading
let reader = BufReader::new(File::open("data.txt")?);
for byte in reader.bytes() {
    process_byte(byte?);
}

// Buffer size tuning
let reader = BufReader::with_capacity(64 * 1024, file);
```

### Memory Mapping

```rust
// Memory map large files
let file = File::open("large.dat")?;
let mmap = unsafe { Mmap::map(&file)? };

// Access as byte slice
let data = &mmap[..];
process_bytes(data);

// Memory mapped writing
let mut file = File::create("output.dat")?;
file.set_len(size)?;
let mut mmap = unsafe { MmapMut::map_mut(&file)? };
mmap.copy_from_slice(&data);
```

### Parallel Processing

```rust
// Process multiple files in parallel
fn process_files_parallel(paths: Vec<String>) -> Result<Vec<Result<Data, Error>>, Error> {
    use rayon::prelude::*;
    
    paths.par_iter()
        .map(|path| {
            let content = read_file(path)?;
            process_content(&content)
        })
        .collect()
}

// Parallel directory processing
fn process_directory_parallel(dir: &str) -> Result<(), Error> {
    let entries: Vec<_> = read_dir(dir)?.collect();
    
    entries.par_iter()
        .try_for_each(|entry| {
            if entry.is_file {
                process_file(&entry.path)?;
            }
            Ok(())
        })
}
```

## JavaScript/Node.js Interop

When transpiled to JavaScript, file operations use Node.js fs module:

```rust
// Husk
let content = read_file("data.txt")?;
write_file("output.txt", &content)?;
```

Becomes:
```javascript
// JavaScript (Node.js)
const fs = require('fs').promises;

const content = await fs.readFile("data.txt", 'utf8');
await fs.writeFile("output.txt", content);
```

### API Mappings

| Husk Function | Node.js Equivalent |
|---------------|-------------------|
| `read_file()` | `fs.readFile()` |
| `write_file()` | `fs.writeFile()` |
| `append_file()` | `fs.appendFile()` |
| `delete_file()` | `fs.unlink()` |
| `create_dir()` | `fs.mkdir()` |
| `read_dir()` | `fs.readdir()` |
| `file_exists()` | `fs.access()` |
| `file_metadata()` | `fs.stat()` |

## Related Topics

- [Error Handling](../language/error-handling.md) - Result and Option types
- [Async Programming](../advanced/async.md) - Async I/O patterns
- [Console I/O](console-io.md) - Terminal input/output
- [Serialization](../advanced/serialization.md) - JSON, TOML, etc.
- [Performance](../advanced/performance.md) - I/O optimization

---

*File I/O is a critical part of most applications. Husk provides safe, efficient, and cross-platform file operations with excellent error handling.*