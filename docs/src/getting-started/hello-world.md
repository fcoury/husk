# Hello World

Let's write your first Husk program.

## Create a File

Create a file called `hello.hk`:

```rust
fn main() {
    println!("Hello, world!");
}
```

## Compile and Run

Compile the file to JavaScript:

```bash
husk build hello.hk
```

This creates `hello.js`. Run it with Node.js:

```bash
node hello.js
```

You should see:

```
Hello, world!
```

## Watch Mode

For rapid development, use watch mode to automatically recompile on changes:

```bash
husk watch hello.hk
```

## Next Steps

Now that you have Husk running, learn about the [Basic Syntax](../guide/basic-syntax.md).
