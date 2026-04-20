# Types

Husk uses static types for source checking and erases most type information in
the generated JavaScript.

## Primitive Types

Core primitive types are:

- `i32`
- `i64`
- `f64`
- `bool`
- `String`
- `()`

Integer and float types are checked by Husk. JavaScript output uses native
numbers where possible and runtime helpers where the compiler needs special
handling.

## Structs and Enums

Structs are nominal record types:

```husk
struct User {
    id: String,
    name: String,
}
```

Enums support unit and payload variants:

```husk
enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

Enums compile to tagged JavaScript objects and can be checked exhaustively in
`match` expressions.

## Generics

Generic types and functions are type-checked but erased at runtime:

```husk
fn first<T>(items: [T]) -> Option<T> {
    if items.len() > 0 {
        Some(items[0])
    } else {
        None
    }
}
```

All instantiations share the same JavaScript implementation.

## JavaScript Types

The standard JavaScript interop types are opaque wrappers:

- `JsValue`
- `JsArray<T>`
- `JsPromise<T>`
- `JsFn`
- `JsObject`

Use these for imported JS values that do not map cleanly to Husk. Prefer
wrapping them behind named Husk functions or structs before using them in app
logic.

## TypeScript Imports

The d.ts importer maps TypeScript declarations into the closest Husk shape it
can represent. Simple functions, interfaces, classes, arrays, promises, and
literal unions can often become typed declarations. Complex unions,
intersections, conditional types, `any`, and `unknown` may become `JsValue`.

This is intentional: imported JavaScript values remain shared and mutable, and
Husk does not try to emulate the full TypeScript type system.
