# Hello World Tutorial

Welcome to your first Husk program! This tutorial will guide you through creating, understanding, and running a simple "Hello World" application.

## Table of Contents

- [Your First Program](#your-first-program)
- [Understanding the Code](#understanding-the-code)
- [Running the Program](#running-the-program)
- [Expanding Hello World](#expanding-hello-world)
- [Common Variations](#common-variations)
- [What You've Learned](#what-youve-learned)
- [Next Steps](#next-steps)

## Your First Program

Let's start with the traditional "Hello World" program. Create a new file called `hello.hk` (or `hello.husk`):

```rust
fn main() {
    println!("Hello, World!");
}
```

That's it! You've written your first Husk program.

## Understanding the Code

Let's break down each part of this program:

### The `main` Function

```rust
fn main() {
    // Your code here
}
```

- `fn` - This keyword declares a function
- `main` - The name of the function. Every Husk program must have a `main` function as the entry point
- `()` - Empty parentheses indicate the function takes no parameters
- `{ }` - Curly braces contain the function body

### The `println!` Macro

```rust
println!("Hello, World!");
```

- `println!` - A macro that prints text to the console followed by a newline
- `"Hello, World!"` - A string literal (text enclosed in double quotes)
- `;` - Semicolon ends the statement

## Running the Program

### Method 1: Direct Execution

Run your program directly with the Husk interpreter:

```bash
husk run hello.hk
```

Output:
```
Hello, World!
```

### Method 2: Compile and Run

Husk can transpile your code to JavaScript, making it runnable anywhere JavaScript runs:

```bash
# Compile to JavaScript
husk build hello.hk --target js

# Run the generated JavaScript
node hello.js
```

The generated `hello.js` will contain:
```javascript
function main() {
    console.log("Hello, World!");
}

main();
```

This JavaScript can run in Node.js, browsers, or any JavaScript environment!

### Method 3: Interactive Mode

Try the code in Husk's REPL:

```bash
husk repl
> println!("Hello, World!");
Hello, World!
```

## Expanding Hello World

Let's explore variations to learn more Husk features:

### 1. Using Variables

```rust
fn main() {
    let greeting = "Hello, World!";
    println!(greeting);
}
```

Key concepts:
- `let` declares a variable
- Variables are immutable by default
- Type inference determines `greeting` is a `string`

### 2. String Formatting

```rust
fn main() {
    let name = "Husk";
    let version = 0.1;
    println!("Welcome to {} v{}!", name, version);
}
```

Output:
```
Welcome to Husk v0.1!
```

Key concepts:
- `{}` are placeholders for values
- Multiple values can be formatted

### 3. User Input

```rust
fn main() {
    print!("What's your name? ");
    let name = read_line();
    println!("Hello, {}!", name);
}
```

Key concepts:
- `print!` doesn't add a newline
- `read_line()` reads user input
- String interpolation with user data

### 4. Function Calls

```rust
fn greet(name: string) {
    println!("Hello, {}!", name);
}

fn main() {
    greet("World");
    greet("Husk");
}
```

Output:
```
Hello, World!
Hello, Husk!
```

Key concepts:
- Functions can take parameters
- Type annotations (`: string`) specify parameter types
- Functions can be called multiple times

### 5. Return Values

```rust
fn create_greeting(name: string) -> string {
    format!("Hello, {}!", name)
}

fn main() {
    let message = create_greeting("World");
    println!(message);
}
```

Key concepts:
- `-> string` specifies the return type
- `format!` creates a formatted string without printing
- Last expression is returned (no `return` keyword needed)

## Common Variations

### Multiple Greetings

```rust
fn main() {
    let greetings = ["Hello", "Hola", "Bonjour", "Hallo"];
    let target = "World";
    
    for greeting in greetings {
        println!("{}, {}!", greeting, target);
    }
}
```

Output:
```
Hello, World!
Hola, World!
Bonjour, World!
Hallo, World!
```

### Conditional Greetings

```rust
fn main() {
    let hour = 14; // 2 PM
    
    let greeting = if hour < 12 {
        "Good morning"
    } else if hour < 18 {
        "Good afternoon"
    } else {
        "Good evening"
    };
    
    println!("{}, World!", greeting);
}
```

### Pattern Matching

```rust
fn get_greeting(language: string) -> string {
    match language {
        "English" => "Hello",
        "Spanish" => "Hola",
        "French" => "Bonjour",
        _ => "Hello"  // default case
    }
}

fn main() {
    let greeting = get_greeting("Spanish");
    println!("{}, World!", greeting);
}
```

## What You've Learned

In this tutorial, you've learned:

✅ How to create a basic Husk program  
✅ The structure of a `main` function  
✅ How to use `println!` for output  
✅ Variable declaration with `let`  
✅ String formatting and interpolation  
✅ Function definition and calling  
✅ Basic control flow (loops and conditionals)  
✅ Pattern matching with `match`  

## Troubleshooting

### Common Issues

**"command not found: husk"**
- Make sure Husk is [installed](installation.md) and in your PATH

**"cannot find main function"**
- Every Husk program needs a `main` function as the entry point

**Syntax errors**
- Check for missing semicolons, parentheses, or curly braces
- Ensure strings are properly quoted

## Next Steps

Now that you've written your first Husk program:

1. **[Quick Start Guide](quickstart.md)** - Learn more language features
2. **[Variables and Types](../language/variables.md)** - Understand Husk's type system
3. **[Functions](../language/functions.md)** - Deep dive into functions
4. **[Control Flow](../language/control-flow.md)** - Master conditionals and loops

## Practice Exercises

Try these modifications to solidify your understanding:

1. **Personalized Greeting**: Modify the program to ask for the user's name and age, then print a personalized message
2. **Multiple Languages**: Create a program that can say "Hello, World!" in 5 different languages
3. **Time-based Greeting**: Write a program that greets differently based on the time of day
4. **Formatted Output**: Create a nicely formatted welcome message with borders and centered text

---

*Congratulations on writing your first Husk program! 🎉*