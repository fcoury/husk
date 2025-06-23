# Compound Assignment Operator Limitations

## Current Status
Compound assignment operators (`+=`, `-=`, `*=`, `/=`, `%=`) are implemented but with limitations on what can be used as the left-hand side (assignment target).

## Working Cases
```rust
// Simple variable assignment
let x = 10;
x += 5;  // ✅ Works

// With different types
let y = 10.5;
y *= 2.0;  // ✅ Works

// In loops
let sum = 0;
for i in 1..10 {
    sum += i;  // ✅ Works
}
```

## Non-Working Cases

### 1. Array Element Assignment
```rust
let arr = [1, 2, 3];
arr[0] += 10;  // ❌ Error: Invalid compound assignment target
```
**Workaround:**
```rust
arr[0] = arr[0] + 10;  // Use regular assignment
```

### 2. Struct Field Assignment
```rust
struct Point {
    x: int,
    y: int,
}

let p = Point { x: 10, y: 20 };
p.x += 5;  // ❌ Error: Invalid compound assignment target
```
**Workaround:**
```rust
p.x = p.x + 5;  // Use regular assignment
```

### 3. Nested Struct Fields
```rust
struct Rectangle {
    top_left: Point,
    width: int,
    height: int,
}

let rect = Rectangle { 
    top_left: Point { x: 0, y: 0 }, 
    width: 100, 
    height: 50 
};
rect.top_left.x += 10;  // ❌ Error: Invalid compound assignment target
```
**Workaround:**
```rust
rect.top_left.x = rect.top_left.x + 10;  // Use regular assignment
```

### 4. Dereferenced Values (Future Feature)
When pointers/references are added to the language:
```rust
let x = 10;
let ptr = &mut x;
*ptr += 5;  // ❌ Would not work with current implementation
```

## Technical Reason
The current implementation in `interpreter.rs` only handles `Expr::Identifier` as the left-hand side:
```rust
fn visit_compound_assign(&mut self, left: &Expr, op: &Operator, right: &Expr, span: &Span) -> Result<Value> {
    match left {
        Expr::Identifier(name, _) => {
            // Implementation for simple variables
        }
        _ => Err(Error::new_runtime(
            "Invalid compound assignment target".to_string(),
            *span,
        )),
    }
}
```

## Implementation Requirements
To support these cases, we would need to:

1. **Array Elements**: Handle `Expr::ArrayIndex` in compound assignment
2. **Struct Fields**: Handle `Expr::MemberAccess` in compound assignment
3. **Complex Lvalues**: Implement a general lvalue evaluation system that can:
   - Get the current value
   - Set the new value
   - Handle nested access patterns

## Priority
This is considered a low-priority enhancement because:
1. The workaround (using regular assignment) is straightforward
2. Simple variable compound assignment covers the most common use cases
3. The implementation would require significant changes to the assignment system