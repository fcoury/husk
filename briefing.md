<user>
I have a dream of creating a programming language that takes the syntax of Rust and compiles to JavaScript. Here's the features I'd like to have:

- Support for Rust enums and linear types
- Interop with TypeScript type definitions

Help me understand how this could be done and ask questions if you need to refine the requirements.
</user>

<agent>
This is an ambitious and fascinating project. Effectively, you are aiming to build a language that offers **Rust’s correctness guarantees** (via ownership and pattern matching) but targets the **JavaScript ecosystem** (ubiquity and libraries).

There are existing languages in this space (like ReScript or Grain), but none strictly enforce **linear types** (affine types) in the way Rust does. Adding linear types to a garbage-collected target is a unique value proposition—not for memory safety, but for **resource safety** and **state correctness**.

Here is a high-level architectural breakdown of how to build this, followed by specific strategies for your three requested features.

---

### 1. The High-Level Architecture

Since you are compiling to JavaScript, you are building a **Source-to-Source Compiler (Transpiler)**.

1.  **Lexer/Parser:**
    - You need a grammar that matches Rust.
    - _Tooling:_ You can use tools like **Tree-sitter** (if you want a ready-made Rust grammar) or parser generators like **ANTLR** or **LALRPOP** (if you are writing the compiler in Rust).
    - _Output:_ Abstract Syntax Tree (AST).
2.  **Semantic Analysis (The "Brain"):**
    - This is where your language differs from TypeScript. You need a custom type checker that implements **Borrow Checking** and **Linear Typing**.
    - It needs to track variable usage counts to ensure values marked "linear" are consumed exactly once.
3.  **Codegen (The Backend):**
    - Convert your AST into a JavaScript AST (using a library like `estree` or `swc`) and print it to a `.js` file.
    - Simultaneously generate `.d.ts` files for the TypeScript ecosystem to consume your code.

---

### 2. Implementing the Key Features

#### A. Rust Enums (Algebraic Data Types)

JavaScript does not have Enums with payloads. You must model these as **Tagged Unions** (objects with a literal string identifier).

**Input (Your Language):**

```rust
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
}

let msg = Message::Move { x: 10, y: 20 };
```

**Compilation Strategy:**
Compile variants into plain JS objects with a `tag` (or `kind`) property.

**Output (JavaScript):**

```javascript
const Message = {
  Quit: { tag: "Quit" },
  Move: (x, y) => ({ tag: "Move", x, y }),
  Write: (content) => ({ tag: "Write", content }),
};

let msg = Message.Move(10, 20);
```

**Pattern Matching:**
You will need to compile `match` expressions into `switch` statements or `if/else` chains based on the `tag` property.

#### B. Linear Types (Ownership in a GC Environment)

This is your "killer feature." In Rust, ownership prevents memory leaks. In a JavaScript environment (which has a Garbage Collector), ownership prevents **logic errors** (like using a file handle after closing it, or modifying an object while another part of the code reads it).

**How to implement:**
This is purely a **Compile-Time** check. It does not affect the generated JavaScript code.

1.  **Symbol Table with State:** Your compiler's symbol table must track the _state_ of a variable (Unused, Active, Moved/Consumed).
2.  **The Rules:**
    - If a variable is passed to a function that takes it by value, mark the variable as `Moved` in the current scope.
    - If the user tries to reference a `Moved` variable, throw a compiler error.
    - If a linear variable goes out of scope without being "consumed" (e.g., by a destructor or specific function), throw a compiler error.

**Example Benefit:**

```rust
// Compile Error! 'file' was moved into close()
let file = open_file("data.txt");
close(file);
read(file); // Compiler stops you here. TypeScript would allow this runtime crash.
```

#### C. Interop with TypeScript Type Definitions

This is the hardest part technically. You want to be able to `import` a TypeScript library and have your compiler understand the types.

**Strategy:**

1.  **The Output Side (Easy):** Your compiler should generate `.d.ts` files automatically based on your internal types so TS users can use your library.
2.  **The Input Side (Hard):** You need a way to read `.d.ts` files and map them to your internal type system.
    - _Approach:_ You essentially need to embed a subset of the TypeScript compiler (or use a library like `swc` or the TS Compiler API) to parse `.d.ts` files.
    - _The Mismatch:_ TypeScript types are structural and mutable by default. Your language is nominal (likely) and linear.
    - _Solution:_ Treat all imported TypeScript types as **`Unrestricted` (Non-linear)** and **`RefCell` (Interior Mutability)** types by default. You cannot enforce linearity on external JS objects unless you create "wrapper bindings" that explicitly tell your compiler to treat them as linear.

---

### 3. Refinement Questions

To verify the scope and design, I have a few questions for you:

1.  **Strictness of Linear Types:**
    In Rust, _everything_ follows move semantics by default (unless it implements `Copy`). In JavaScript, everything is a shared reference.
    - _Question:_ Do you want your language to behave like Rust (move by default) or like JS (share by default, opt-in linearity)? Move-by-default is safer but makes interoperability with standard JS libraries annoying (you'd have to clone/borrow constantly).

2.  **Mutation Model:**
    Rust uses `mut`. JavaScript objects are always mutable.
    - _Question:_ Will your language enforce immutability by default? If so, when you compile to JS, will you use `Object.freeze()` (runtime cost) or just trust the compiler checks (zero-cost)?

3.  **The "Panic" vs "Result" dilemma:**
    Rust avoids exceptions in favor of `Result<T, E>`. JavaScript relies heavily on `try/catch`.
    - _Question:_ When your language imports a TS function that might throw, how do you want to handle it? Should the compiler force the user to wrap external calls in a `Result` type?

4.  **Runtime:**
    - _Question:_ Do you plan to include a small runtime library (for things like the Enum helpers or `Result` types) or do you want "Zero-Runtime" output where the code is pure idiomatic JS?

### Recommended Starting Point

If I were building this, I would start with this stack:

1.  **Language Implementation:** Rust (ironically, writing a compiler for a Rust-like language in Rust is the easiest path).
2.  **Parser:** `chumsky` (a great Rust parser combinator library).
3.  **Backend:** Generate TypeScript source code directly, then let `tsc` handle the final emit to JS. This solves the `.d.ts` generation problem immediately.

Would you like to see a pseudo-code example of how the **Linear Type Checker** logic would look?
</agent>

<user>
> Strictness of Linear Types:
> In Rust, everything follows move semantics by default (unless it implements Copy). In JavaScript, everything is a shared reference.
> Question: Do you want your language to behave like Rust (move by default) or like JS (share by default, opt-in linearity)? Move-by-default is safer but makes interoperability with standard JS libraries annoying (you'd have to clone/borrow constantly).
I think we can use share by default and opt-in like suggested.

> Mutation Model:
> Rust uses mut. JavaScript objects are always mutable.
> Question: Will your language enforce immutability by default? If so, when you compile to JS, will you use Object.freeze() (runtime cost) or just trust the compiler checks (zero-cost)?
> Initially zero-cost, but we can consider Object.freeze().

> The "Panic" vs "Result" dilemma:
> Rust avoids exceptions in favor of Result<T, E>. JavaScript relies heavily on try/catch.
> Question: When your language imports a TS function that might throw, how do you want to handle it? Should the compiler force the user to wrap external calls in a Result type?
> I would prefer the compiler to force the user to wrap external calls in a Result type for better error handling.

> Runtime:
> Question: Do you plan to include a small runtime library (for things like the Enum helpers or Result types) or do you want "Zero-Runtime" output where the code is pure idiomatic JS?
> What is your suggestion here?
> </user>

<agent>
This is a very practical direction. By choosing **"Share by default, Opt-in Linearity"** and **"Zero-cost Immutability,"** you are effectively designing a language that feels like TypeScript but has a "superpower" mode (linear types) for critical resource management, which is a sweet spot for adoption.

Here is how we can implement these specific design choices:

### 1. Linearity: The "Opt-In" Implementation

Since normal variables behave like JavaScript (shared references), we need a specific syntax to tell the compiler, "This specific type is special; track its ownership."

**Proposed Syntax:**
Use a keyword like `resource` or `linear` before a struct definition.

```rust
// 1. Normal Data (Shared by default, like JS/TS)
struct User {
    name: String,
    id: i32,
}

// 2. Linear Data (Opt-in Linearity)
// The compiler tracks this. It CANNOT be dropped silently.
// It MUST be consumed exactly once.
resource FileHandle {
    descriptor: i32,
}

fn process() {
    let u = User { name: "Alice", ... };
    let u2 = u; // OK! Both u and u2 point to the same object (Shared)

    let f = FileHandle { descriptor: 1 };
    let f2 = f; // OK! Ownership moved to f2. 'f' is now dead.

    // Error: 'f' used after move
    // print(f);

    // Error: Scope ended but 'f2' was not consumed/closed.
}
```

**Compiler Logic:**

- **Type Checker:** Checks if a type is marked `resource`.
- **If `resource`:** Apply "Move Semantics" (invalidate previous variable on assignment) and "Must Consume" checks (error if it leaves scope alive).
- **If normal:** Apply normal JS reference semantics.

---

### 2. Immutability: Zero-Cost (Compiler Checked)

You chose zero-cost. This means the output JavaScript will look like standard mutable code, but your compiler will yell at the developer if they try to mutate something they shouldn't.

**Source Code:**

```rust
let x = 10;
// x = 20; // Compiler Error: Re-assignment of immutable variable

let mut y = 10;
y = 20; // OK
```

**Generated JavaScript:**

```javascript
let x = 10;
// The compiler simply didn't output the reassignment code because it failed analysis.

let y = 10;
y = 20;
```

_Note: We use `let` for everything in JS output (or `const` where analysis proves it never changes) to be safe._

---

### 3. The "Panic" vs "Result" Interop Strategy

You want the compiler to **force** wrapping external calls. This implies your compiler needs to generate "Wrapper Thunks" (helper functions) around imported TypeScript functions.

**Scenario:**
You import a TS function `JSON.parse(text)`. In JS, this throws if text is invalid. In your language, you want `Result<T, Error>`.

**Your Language Declaration (Interop):**

```rust
// You define that this external function exists
extern "js" fn parse_json(text: String) -> Result<JsonObject, JsError>;
```

**The Compiler's Job:**
When the compiler sees you calling `parse_json`, it generates a `try/catch` block automatically in the output.

**Generated JavaScript:**

```javascript
function parse_json(text) {
  try {
    const result = JSON.parse(text);
    // Use runtime helper to wrap success
    return Runtime.Ok(result);
  } catch (e) {
    // Use runtime helper to wrap failure
    return Runtime.Err(e);
  }
}
```

_Now, the user in your language is forced to `match` on the Result to get the value. They cannot ignore the potential error._

---

### 4. Runtime Recommendation: "The Preamble Pattern"

For your question about the runtime: **I strongly suggest a "Micro-Runtime" (or Preamble).**

Do not go "Zero-Runtime" (pure inlining) for Enums and Results. It makes the generated JS file huge and unreadable because you have to repeat the object structure definitions everywhere.

Instead, create a tiny JavaScript file (e.g., `std_preamble.js`, < 1KB) that your compiler automatically imports or pastes at the top of the output.

**What goes in the Micro-Runtime?**

1.  **Enum Constructors:** Simple helpers to create tagged unions.
2.  **Result Type:** A standard implementation of `Ok` and `Err`.
3.  **Panic Handler:** A function to handle strict crashes if necessary.

**Example `std_preamble.js`:**

```javascript
export const Ok = (val) => ({ tag: "Ok", val });
export const Err = (err) => ({ tag: "Err", err });
export const match = (obj, cases) => cases[obj.tag](obj.val || obj);
```

**Why this is better:**
Your generated code becomes clean and idiomatic:

```javascript
import { Ok, Err } from "./std_preamble.js";

// Your compiler output looks like this:
function calculate(x) {
  if (x > 0) return Ok(x * 2);
  return Err("Negative number");
}
```

---

### 5. Next Steps & Refined Roadmap

Based on your answers, here is the refined roadmap for the "MVP" (Minimum Viable Product):

1.  **The Grammar:** Define Rust syntax, but add a `resource` (or `linear`) keyword for structs.
2.  **The Type System (Hybrid):**
    - Implement a standard type checker.
    - Add a flag to types: `is_linear`.
    - If `is_linear` is true, run the "Affine Logic" (ensure usage count == 1).
3.  **The Importer:** Write a tool that reads a TypeScript `.d.ts` file and converts functions like `fn(a) -> b` into `fn(a) -> Result<b, JsError>`.
4.  **The Codegen:** Target JavaScript (ES6 modules) and include the `std_preamble.js` import at the top.

**One Specific Question to move forward:**
To handle the TypeScript interoperability, do you want to:
A. **Manually define** the external types in your language (like writing C headers)?
B. **Automatically ingest** `.d.ts` files? (This is _much_ harder to build but easier to use).

I recommend starting with **A (Manual definitions)** to get the language working first. Do you agree?
</agent>
