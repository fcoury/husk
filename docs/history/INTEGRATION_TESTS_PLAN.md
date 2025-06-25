# Husk JavaScript Interop Integration Tests Plan

This document outlines comprehensive integration tests for all JavaScript interop features implemented in Husk, ensuring production readiness and real-world compatibility.

## Core Language Features Integration Tests

### 1. **Multi-File Module System**
```rust
// Test: examples/multi_file_project/
// - main.husk imports from utils.husk and models.husk
// - Cross-file type usage and method calls
// - Local module path resolution (local::utils)
```

**Test Coverage:**
- Local module imports with `use local::`
- Cross-file type definitions and usage
- Method resolution across module boundaries
- Module caching and circular dependency detection

### 2. **Node.js API Integration** 
```rust
// Test: examples/node_integration/
// - File system operations (fs::readFile, writeFile)
// - Path manipulation (path::join, dirname) 
// - Process handling (process::argv, exit)
// - Async/await with Node.js APIs
```

**Test Coverage:**
- External module imports (`use fs::promises`)
- Extern type declarations (`extern type Buffer`)
- Async function transpilation and execution
- Promise-based API integration

### 3. **Pattern Matching Complex Scenarios**
```rust
// Test: Nested enum patterns with local imports
// - Result<Command::Process { input, output }, ErrorType>
// - Mixed tuple and struct destructuring
// - Rest patterns in real-world data structures
```

**Test Coverage:**
- Nested pattern matching with generics
- Mixed pattern types in single match expression
- Complex enum variant destructuring

## New Feature Integration Tests

### 4. **Shorthand Field Syntax**
```rust
// Test: Real-world API construction patterns
fn create_config(host: string, port: int, debug: bool) -> Config {
    Config { host, port, debug }  // shorthand
}

// Test: Mixed shorthand and regular syntax
let config = DatabaseConfig { 
    host,           // shorthand
    port: 5432,     // regular
    pool_size       // shorthand
};

// Test: Enum variants with shorthand
let req = Request::Post { url, data, headers };
```

**Test Coverage:**
- Pure shorthand field initialization
- Mixed shorthand and explicit field syntax
- Shorthand in enum variant construction
- Nested struct initialization with shorthand
- JavaScript object construction validation

### 5. **Rest Patterns in Real APIs**
```rust
// Test: HTTP request parsing
match request {
    HttpRequest { method, url, .. } => handle_request(method, url),
    ApiRequest { endpoint, auth, .. } => handle_api(endpoint, auth),
}

// Test: Configuration extraction
match app_config {
    Config { database_url, redis_url, .. } => setup_services(database_url, redis_url),
}
```

**Test Coverage:**
- Rest patterns in struct destructuring
- Rest patterns in enum variant destructuring
- Multiple rest patterns in single match
- Rest patterns with local module types
- JavaScript destructuring generation

### 6. **Tuple Destructuring in Data Processing**
```rust
// Test: Coordinate processing
let points = [(0, 0), (10, 20), (30, 40)];
for (x, y) in points {
    transform_point(x, y);
}

// Test: Key-value pair iteration  
let entries = [("name", "Alice"), ("age", "25"), ("city", "NYC")];
for (key, value) in entries {
    set_property(key, value);
}

// Test: Nested tuple destructuring
let data = [((1, 2), (3, 4)), ((5, 6), (7, 8))];
for ((a, b), (c, d)) in data {
    process_quad(a, b, c, d);
}
```

**Test Coverage:**
- Simple tuple destructuring in for loops
- Nested tuple destructuring
- Mixed data types in tuples
- Array destructuring JavaScript generation
- Performance with large datasets

## Real-World Scenario Tests

### 7. **CLI Application End-to-End**
```rust
// Test: Complete CLI workflow
// - Argument parsing with shorthand syntax
// - File processing with rest patterns  
// - Result handling with implicit variants
// - Node.js API integration throughout
```

**Test Coverage:**
- Command-line argument parsing
- File system operations with error handling
- Multi-file project compilation
- Executable JavaScript generation
- Node.js runtime compatibility

### 8. **Web Server Simulation**
```rust
// Test: Express-like server patterns
enum Route {
    Get { path: string, handler: string },
    Post { path: string, handler: string, middleware: Vec<string> },
}

match route {
    Route::Get { path, handler, .. } => handle_get(path, handler),
    Route::Post { path, handler, .. } => handle_post(path, handler),
}
```

**Test Coverage:**
- HTTP routing patterns
- Middleware handling with rest patterns
- Request/response object construction
- Async handler functions

### 9. **Database Integration Patterns**
```rust
// Test: ORM-like patterns with all features
struct User { id, name, email, created_at }; // shorthand fields

match query_result {
    Ok(User { id, name, .. }) => format!("User {}: {}", id, name),
    Err(DatabaseError { code, .. }) => handle_error(code),
}

for (user_id, profile_data) in user_profiles {
    update_user(user_id, profile_data);
}
```

**Test Coverage:**
- Database result processing
- Error handling patterns
- Batch data operations
- ORM-style object mapping

### 10. **Type System Integration**
```rust
// Test: Generic types with new features  
enum ApiResponse<T> {
    Success { data: T, metadata: Map<string, string> },
    Error { code: int, message: string },
}

match response {
    ApiResponse::Success { data, .. } => process_data(data),
    ApiResponse::Error { code, message, .. } => log_error(code, message),
}
```

**Test Coverage:**
- Generic type instantiation
- Generic pattern matching
- Type inference across module boundaries
- Complex generic constraints

## JavaScript Output Validation Tests

### 11. **Transpilation Correctness**
```rust
// Test: Generated JavaScript is valid and executable
// - Shorthand fields become proper object construction
// - Rest patterns are handled correctly in destructuring
// - Tuple destructuring generates array destructuring
// - All patterns work with Node.js runtime
```

**Test Coverage:**
- JavaScript syntax validation
- Runtime execution verification
- Memory usage optimization
- Browser compatibility (if applicable)

### 12. **Performance Integration**
```rust
// Test: Large-scale data processing
// - 10,000+ item arrays with tuple destructuring
// - Complex nested patterns with rest operators
// - Memory usage validation with Node.js
```

**Test Coverage:**
- Large dataset processing
- Memory leak detection
- Performance benchmarking
- Garbage collection efficiency

## Error Handling Integration

### 13. **Comprehensive Error Scenarios**
```rust
// Test: Error propagation through all features
async fn process_files() -> Result<(), string> {
    for (input_path, output_path) in file_pairs {
        let content = readFile(input_path).await?;
        match parse_config(content) {
            Ok(Config { host, port, .. }) => {
                let result = connect_server(host, port).await?;
                writeFile(output_path, result).await?;
            }
            Err(error) => return Err(format!("Parse error: {}", error)),
        }
    }
    Ok(())
}
```

**Test Coverage:**
- Error propagation with `?` operator
- Result/Option handling in complex flows
- Async error handling
- Custom error types with pattern matching

## Test Implementation Strategy

### Phase 1: Critical Feature Tests (Immediate)
1. Shorthand field syntax semantic/interpreter/transpiler tests
2. Rest patterns semantic/interpreter/transpiler tests  
3. Tuple destructuring semantic/interpreter/transpiler tests
4. Multi-file module integration tests

### Phase 2: Real-World Scenario Tests (Short-term)
1. CLI application end-to-end tests
2. Node.js API integration tests
3. Complex pattern matching scenarios
4. JavaScript output validation

### Phase 3: Performance and Edge Cases (Long-term)
1. Large-scale data processing tests
2. Memory usage and performance benchmarks
3. Browser compatibility tests
4. Error handling edge cases

## Test Structure

```
tests/
├── integration/
│   ├── shorthand_fields/
│   ├── rest_patterns/
│   ├── tuple_destructuring/
│   ├── multi_file_modules/
│   ├── nodejs_integration/
│   ├── real_world_scenarios/
│   └── performance/
├── examples/
│   ├── cli_tool/
│   ├── web_server/
│   ├── database_orm/
│   └── data_processing/
└── benchmarks/
    ├── large_datasets/
    ├── memory_usage/
    └── transpilation_speed/
```

## Success Criteria

- ✅ All integration tests pass in both interpreter and transpiler modes
- ✅ Generated JavaScript executes correctly in Node.js environment
- ✅ Memory usage remains within acceptable bounds for large datasets
- ✅ Transpilation time is reasonable for real-world projects
- ✅ Error messages are clear and helpful for debugging
- ✅ All features work together seamlessly in complex scenarios

---

*This plan ensures comprehensive validation of all JavaScript interop features for production readiness.*