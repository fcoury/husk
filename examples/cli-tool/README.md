# Husk CLI Tool Example

A comprehensive command-line file processing tool built with Husk, demonstrating seamless Node.js API integration and JavaScript interop features.

## Features

### Node.js API Integration
- **File System Operations**: `fs.promises` for async file I/O
- **Path Manipulation**: `path` module for cross-platform path handling
- **Process Integration**: Command-line argument parsing with `process.argv`
- **Error Handling**: Proper Node.js error mapping and user-friendly messages

### CLI Functionality
- **File Processing**: Transform and minify files with various options
- **Directory Analysis**: Analyze directory structure and file statistics
- **File Watching**: Monitor directories for changes (demonstration)
- **Multiple Output Formats**: Support for different file transformations

### JavaScript Interop Features
- **Async/Await**: Native async programming with Promise integration
- **Type Casting**: Safe type conversions with runtime validation
- **String Methods**: Built-in string manipulation and formatting
- **Error Propagation**: Result types with `?` operator for error handling
- **Extern Types**: Integration with Node.js built-in types like `Buffer` and `Stats`

## Project Structure

```
cli-tool/
├── main.husk           # Entry point and main CLI logic
├── cli.husk           # Command parsing and validation
├── file_processor.husk # File processing and transformation logic
├── utils.husk         # Utilities, logging, and helper functions
├── package.json       # Node.js package configuration
└── README.md          # This file
```

## Installation & Usage

### Building the CLI Tool

```bash
# Build for Node.js ES modules
npm run build

# Build for Node.js CommonJS
npm run build:cjs

# Development mode with file watching
npm run dev
```

### Running Commands

```bash
# Process files from source to destination
husk-cli process ./src ./dist --minify --filter "*.js"

# Analyze directory structure
husk-cli analyze ./project

# Watch directory for changes
husk-cli watch ./src --verbose

# Show help
husk-cli help
```

### Command Options

#### Process Command
```bash
husk-cli process <input> <output> [OPTIONS]

OPTIONS:
  --minify              Minify output files
  --transform <type>    Apply transformation (js, css, html, json)
  --filter <pattern>    Filter files by pattern (*.js, *.css)
  --verbose, -v         Enable verbose output
```

#### Analyze Command
```bash
husk-cli analyze <path>
```
Provides detailed statistics about:
- Total files and directories
- File type distribution
- Total size calculation
- Directory structure analysis

#### Watch Command
```bash
husk-cli watch <directory>
```
Monitors directory for file system changes (demonstration implementation).

## Code Examples

### Async File Operations
```husk
async fn process_file(input: string, output: string) -> Result<(), string> {
    let content = readFile(input, "utf8").await?;
    let processed = transform_content(content)?;
    writeFile(output, processed, "utf8").await?;
    Ok(())
}
```

### Node.js API Usage
```husk
use fs::promises::{readFile, writeFile, readdir, stat};
use path::{join, extname, basename, dirname};
use process::Process;

// Access command line arguments
let args = Process.argv;

// File system operations with error handling
let files = readdir(directory).await?;
for file in files {
    let file_path = join(directory, file);
    let stats = stat(file_path).await?;
    
    if stats.isFile() {
        process_single_file(file_path).await?;
    }
}
```

### Type Casting and Validation
```husk
fn parse_size(size_str: string) -> Result<int, string> {
    match size_str as int {
        Ok(size) => {
            if size > 0 {
                Ok(size)
            } else {
                Err("Size must be positive")
            }
        },
        Err(_) => Err("Invalid size format")
    }
}
```

### Error Handling with Result Types
```husk
async fn safe_file_operation(path: string) -> Result<string, string> {
    let exists = check_file_exists(path.clone()).await?;
    if !exists {
        return Err(format!("File not found: {}", path));
    }
    
    let content = readFile(path, "utf8").await?;
    Ok(content.trim())
}
```

## Generated JavaScript

The Husk code transpiles to clean, modern JavaScript:

```javascript
// Async/await is preserved
async function processFile(input, output) {
    try {
        const content = await readFile(input, 'utf8');
        const processed = transformContent(content);
        await writeFile(output, processed, 'utf8');
        return { type: 'Ok', value: undefined };
    } catch (error) {
        return { type: 'Err', value: error.message };
    }
}

// String methods are mapped to native JavaScript
const length = content.len();  // -> content.length
const trimmed = content.trim(); // -> content.trim()
const upper = content.toUpperCase(); // -> content.toUpperCase()

// Type casting with proper error handling
const number = Number(stringValue);
if (isNaN(number)) {
    throw new Error('Invalid number format');
}
```

## Target Configurations

### Node.js ES Modules (Default)
```bash
husk build main.husk --target node-esm
```
Generates ES module syntax with `import`/`export`.

### Node.js CommonJS
```bash
husk build main.husk --target node-cjs
```
Generates CommonJS syntax with `require`/`module.exports`.

## Educational Value

This example demonstrates:

1. **Real-world CLI Development**: Complete command-line application structure
2. **Node.js Integration**: Seamless use of Node.js APIs and ecosystem
3. **Async Programming**: Modern async/await patterns with error handling
4. **Type Safety**: Compile-time type checking with runtime conversions
5. **Error Handling**: Robust error propagation using Result types
6. **Module Organization**: Clean separation of concerns across modules
7. **Build Configuration**: Multiple target environments and output formats

## Next Steps

- Add more file transformation types
- Implement actual file watching with `fs.watch`
- Add configuration file support
- Integrate with popular CLI frameworks
- Add unit tests and integration tests
- Publish as npm package