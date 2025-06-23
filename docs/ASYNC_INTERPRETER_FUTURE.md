# Future: Async Support in Interpreter Mode

## Overview

Currently, async/await is only supported in transpiler mode, leveraging JavaScript's native async runtime. This document outlines potential approaches for adding async support to Husk's interpreter mode in the future.

## Current State

- Async/await syntax is parsed and type-checked
- Transpiler converts to JavaScript async/await
- Interpreter returns error for async functions
- Clear mode-specific documentation

## Future Implementation Options

### Option 1: Simple Blocking Async (Recommended for MVP)

Convert async operations to synchronous blocking calls:

```rust
// In interpreter
async fn fetch(url) -> Response {
    // Actually performs blocking HTTP request
    blocking::get(url)
}
```

**Pros:**
- Simple to implement
- Works for basic use cases
- No event loop needed

**Cons:**
- Different behavior than JavaScript
- No true concurrency
- Can't handle complex async patterns

### Option 2: Tokio-based Runtime

Integrate Rust's Tokio async runtime:

```rust
struct InterpreterRuntime {
    tokio_runtime: tokio::runtime::Runtime,
}

impl Value {
    Promise(tokio::task::JoinHandle<Value>),
}
```

**Pros:**
- Real async execution
- Proper concurrency
- More similar to JavaScript

**Cons:**
- Complex integration
- Still different from JS event loop
- Heavy dependency

### Option 3: Custom Event Loop

Implement a JavaScript-like event loop:

```rust
struct EventLoop {
    ready_queue: VecDeque<Task>,
    waiting_tasks: HashMap<TaskId, WaitingTask>,
    timers: BinaryHeap<Timer>,
}
```

**Pros:**
- Closest to JavaScript semantics
- Full control over behavior
- Educational value

**Cons:**
- Most complex option
- Significant development effort
- Maintenance burden

## Recommended Approach

Start with Option 1 (blocking async) for MVP, with clear documentation about behavioral differences. This provides:

1. Basic async syntax support
2. Simple mental model
3. Path to upgrade later
4. Acceptable for scripting use cases

## Implementation Considerations

### 1. Promise Type in Interpreter

```rust
enum Value {
    // ... existing variants
    Promise(PromiseState),
}

enum PromiseState {
    // For blocking implementation
    Resolved(Box<Value>),
    Rejected(String),
}
```

### 2. Async Function Context

```rust
struct AsyncContext {
    is_async: bool,
    // For blocking: nothing special needed
    // For real async: would need suspension points
}
```

### 3. Built-in Async Functions

```rust
// Interpreter built-ins
fn sleep(ms: i64) -> Promise<()> {
    thread::sleep(Duration::from_millis(ms as u64));
    Promise::resolved(Value::Unit)
}

fn fetch(url: String) -> Promise<Response> {
    match blocking_http_get(&url) {
        Ok(response) => Promise::resolved(response),
        Err(e) => Promise::rejected(e.to_string()),
    }
}
```

## Migration Path

1. **Phase 1**: Document async as transpiler-only
2. **Phase 2**: Add blocking async to interpreter
3. **Phase 3**: Evaluate need for true async
4. **Phase 4**: Potentially upgrade to Tokio/custom runtime

## Testing Strategy

When implementing interpreter async:

1. **Behavior Tests**: Ensure blocking behavior is documented
2. **Compatibility Tests**: Verify same results as transpiled code
3. **Performance Tests**: Monitor blocking impact
4. **Error Tests**: Clear messages about limitations

## User Communication

When interpreter async is added:

```husk
// In interpreter mode:
// WARNING: Async functions run synchronously in interpreter mode.
// For true async behavior, use transpiler mode.

async fn example() {
    // This blocks in interpreter, but is async in transpiled JS
    let data = fetch(url).await;
}
```

## Decision Log

- **2024-01-XX**: Decided on transpiler-only async for initial implementation
- **Future**: Evaluate user needs for interpreter async support
- **Future**: Choose implementation approach based on use cases

## References

- [Tokio Runtime](https://tokio.rs/)
- [JavaScript Event Loop](https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop)
- [Rust Async Book](https://rust-lang.github.io/async-book/)