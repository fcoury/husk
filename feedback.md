## Enhancing Error Messages with Line Numbers or Stack Traces

In our micro-runtime methods:

```js
fcoury@m3pro ~/c/langdev on  master [⇡] 🦀 v1.91.1
> $ cargo run -q -- compile examples/feature_match/simple_match.hk
// Husk standard preamble (runtime helpers)
function Ok(value) {
    return { tag: "Ok", value };
}

function Err(error) {
    return { tag: "Err", error };
}

function panic(message) {
    throw new Error("[Husk panic] " + String(message));
}

function matchEnum(value, handlers) {
    var tag = value && value.tag;
    var handler = handlers[tag];
    if (typeof handler !== "function") {
        throw new Error("[Husk match] non-exhaustive handlers for tag " + String(tag));
    }
    return handler(value);
}

function describe(color) {
    (color.tag == "Red" ? 1 : 2);
}
```

Would it be possible to have either a line number or stacktrace information included in the error messages to help with debugging?

## Entry point definition

Also, is there a way to define an entry point for the compiled code? For example, in Rust we have the `main` function as the entry point. How would we specify that in Husk?
