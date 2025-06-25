# Husk Standard Library Implementation Plan

## Overview

This document outlines the plan for implementing a comprehensive standard library for the Husk language. The standard library will provide essential functionality for common programming tasks while maintaining compatibility with both the interpreter and transpiler modes.

## Design Principles

1. **Rust-Inspired API**: Follow Rust's standard library patterns for familiarity and consistency
2. **Dual-Mode Compatibility**: All standard library functions must work identically in both interpreter and transpiler modes
3. **JavaScript Translation**: Provide efficient mappings to JavaScript APIs with documented differences
4. **Type Safety**: Full type checking at compile time with proper error messages
5. **Zero-Cost Abstractions**: Transpiled code should be as efficient as hand-written JavaScript
6. **Ownership Semantics**: Respect Rust's borrowing principles while adapting to Husk's model
7. **Modularity**: Organized into logical modules that can be imported as needed

## Architecture

### Core Structure

```
std/
├── prelude.husk      # Automatically imported types and functions
├── string.husk       # String operations (unified, no str/String distinction)
├── array.husk        # Primitive array operations
├── vec.husk          # Vec<T> - growable arrays (wraps arrays with Rust-like API)
├── iter.husk         # Iterator traits and adapters (Rust: std::iter)
├── option.husk       # Option<T> methods (Rust: std::option)
├── result.husk       # Result<T,E> methods (Rust: std::result)
├── io.husk           # Input/output traits and types (Rust: std::io)
├── fs.husk           # File system operations (Rust: std::fs)
├── path.husk         # Path manipulation (Rust: std::path)
├── env.husk          # Environment interaction (Rust: std::env)
├── process.husk      # Process management (Rust: std::process)
├── collections/      # Data structures (Rust: std::collections)
│   ├── hash_map.husk
│   ├── hash_set.husk
│   ├── btree_map.husk
│   └── vec_deque.husk
└── fmt.husk          # Formatting utilities (Rust: std::fmt)
```

### Implementation Strategy

1. **Hybrid Approach**: Combine native Husk implementations with extern declarations
2. **Built-in Methods**: Extend primitive types with methods (string, array, etc.)
3. **Module System**: Use Husk's module system for organization
4. **Conditional Compilation**: Different implementations for interpreter vs transpiler where needed

## Core Modules

### 1. Prelude (Auto-imported)

```husk
// Core types already built-in: Option<T>, Result<T, E>

// Essential macros and functions (matching Rust's prelude)
fn panic(message: string) -> !;
fn assert(condition: bool) -> void;
fn assert_eq<T: Eq>(left: T, right: T) -> void;
fn debug_assert(condition: bool) -> void;
fn unreachable() -> !;
fn unimplemented() -> !;
fn todo() -> !;

// I/O (matching Rust's print!/println!)
fn print(args: fmt::Arguments) -> void;
fn println(args: fmt::Arguments) -> void;
fn eprint(args: fmt::Arguments) -> void;
fn eprintln(args: fmt::Arguments) -> void;

// Type conversions (matching Rust's From/Into traits)
trait From<T> {
    fn from(value: T) -> Self;
}

trait Into<T> {
    fn into(self) -> T;
}

// JavaScript Translation Notes:
// - panic() → throw new Error()
// - assert() → if (!condition) throw new Error()
// - print/println → console.log()
// - eprint/eprintln → console.error()
```

### 2. String Module

```husk
// String type - unified string type (no str/String distinction)
impl string {
    // Length and emptiness
    fn len(self) -> usize;                            // JS: .length
    fn is_empty(self) -> bool;                        // JS: .length === 0
    fn chars(self) -> array<string>;                  // JS: Array.from(str) - returns single-char strings ✅ IMPLEMENTED
    fn bytes(self) -> array<u8>;                      // JS: new TextEncoder().encode(str)
    
    // Whitespace handling
    fn trim(self) -> string;                          // JS: .trim() ✅ IMPLEMENTED
    fn trim_start(self) -> string;                    // JS: .trimStart()
    fn trim_end(self) -> string;                      // JS: .trimEnd()
    fn trim_matches(self, pat: string) -> string;     // JS: custom implementation
    
    // Pattern searching
    fn contains(self, pat: string) -> bool;           // JS: .includes() ✅ IMPLEMENTED
    fn starts_with(self, pat: string) -> bool;        // JS: .startsWith() ✅ IMPLEMENTED
    fn ends_with(self, pat: string) -> bool;          // JS: .endsWith() ✅ IMPLEMENTED
    fn find(self, pat: string) -> Option<usize>;      // JS: .indexOf() with -1 check ✅ IMPLEMENTED
    fn rfind(self, pat: string) -> Option<usize>;     // JS: .lastIndexOf() with -1 check ✅ IMPLEMENTED
    
    // Splitting
    fn split(self, pat: string) -> array<string>;     // JS: .split() ✅ IMPLEMENTED
    fn splitn(self, n: usize, pat: string) -> array<string>; // JS: .split() with limit
    fn split_once(self, pat: string) -> Option<(string, string)>; // JS: custom
    fn lines(self) -> array<string>;                  // JS: .split('\n')
    
    // Case conversion
    fn to_lowercase(self) -> string;                  // JS: .toLowerCase() ✅ IMPLEMENTED
    fn to_uppercase(self) -> string;                  // JS: .toUpperCase() ✅ IMPLEMENTED
    fn to_ascii_lowercase(self) -> string;            // JS: custom ASCII-only
    fn to_ascii_uppercase(self) -> string;            // JS: custom ASCII-only
    
    // Replacement
    fn replace(self, from: string, to: string) -> string;    // JS: .replaceAll() ✅ IMPLEMENTED
    fn replacen(self, from: string, to: string, n: usize) -> string; // JS: custom
    
    // Substring operations
    fn substring(self, start: usize, end: usize) -> string;  // JS: .substring() ✅ IMPLEMENTED
    fn slice(self, start: isize, end: isize) -> string;      // JS: .slice()
    
    // Character access
    fn char_at(self, idx: usize) -> Option<string>;   // JS: charAt() returns single-char string
    fn get(self, range: Range<usize>) -> Option<string>; // JS: substring with bounds check
    
    // Concatenation and repetition
    fn concat(self, other: string) -> string;         // JS: + operator
    fn repeat(self, n: usize) -> string;              // JS: .repeat()
    
    // Parsing (via FromStr trait)
    fn parse<T: FromStr>(self) -> Result<T, T::Err>;  // JS: parseInt/parseFloat/custom
}

// Module-level functions
mod string {
    // Constructors
    fn new() -> string;                               // JS: ''
    fn from_chars(chars: array<string>) -> string;    // JS: chars.join('') - from single-char strings
    fn from_bytes(bytes: array<u8>) -> Result<string, Error>; // JS: TextDecoder
    
    // Joining
    fn join(strings: array<string>, sep: string) -> string; // JS: array.join(sep)
}

// JavaScript Translation Notes:
// 1. Husk has only one string type (like JS), no borrowing
//    → All methods take 'self' by value, return new strings
// 2. UTF-8 vs UTF-16 handling
//    → Need proper char iteration with Array.from()
// 3. No capacity management needed
//    → Methods like with_capacity become no-ops
// 4. All operations are immutable
//    → No &mut self methods, always return new strings
```

### 3. Array and Vec Modules

```husk
// Array methods - built-in primitive array type
impl<T> array<T> {
    // Length and emptiness
    fn len(self) -> usize;                            // JS: .length
    fn is_empty(self) -> bool;                        // JS: .length === 0
    
    // Element access
    fn first(self) -> Option<T>;                      // JS: [0] with undefined check
    fn last(self) -> Option<T>;                       // JS: [length-1] with check
    fn get(self, idx: usize) -> Option<T>;            // JS: bounds-checked access
    
    // Searching
    fn contains(self, x: T) -> bool where T: Eq;      // JS: .includes()
    fn find(self, f: fn(T) -> bool) -> Option<T>;     // JS: .find()
    fn position(self, f: fn(T) -> bool) -> Option<usize>; // JS: .findIndex()
    
    // Functional operations (immutable)
    fn map<U>(self, f: fn(T) -> U) -> array<U>;       // JS: .map()
    fn filter(self, f: fn(T) -> bool) -> array<T>;    // JS: .filter()
    fn fold<B>(self, init: B, f: fn(B, T) -> B) -> B; // JS: .reduce()
    fn all(self, f: fn(T) -> bool) -> bool;           // JS: .every()
    fn any(self, f: fn(T) -> bool) -> bool;           // JS: .some()
    
    // Slicing and concatenation
    fn slice(self, start: usize, end: usize) -> array<T>; // JS: .slice()
    fn concat(self, other: array<T>) -> array<T>;     // JS: .concat()
    fn join(self, sep: string) -> string where T: ToString; // JS: .join()
    
    // Iteration
    fn iter(self) -> Iter<T>;                         // JS: Symbol.iterator
    fn enumerate(self) -> array<(usize, T)>;          // JS: .map((v, i) => [i, v])
}

// Vec<T> - growable array with Rust-like API
struct Vec<T> {
    inner: array<T>,
}

impl<T> Vec<T> {
    // Constructors
    fn new() -> Vec<T>;                               // JS: {inner: []}
    fn with_capacity(capacity: usize) -> Vec<T>;      // JS: {inner: []} (no pre-alloc)
    fn from_array(arr: array<T>) -> Vec<T>;           // JS: {inner: [...arr]}
    
    // Length and capacity
    fn len(self) -> usize;                            // JS: this.inner.length
    fn is_empty(self) -> bool;                        // JS: this.inner.length === 0
    fn capacity(self) -> usize;                       // JS: this.inner.length (no capacity)
    
    // Mutation methods (Rust-like API)
    fn push(mut self, value: T);                      // JS: this.inner.push(value)
    fn pop(mut self) -> Option<T>;                    // JS: this.inner.pop() with check
    fn insert(mut self, idx: usize, value: T);        // JS: this.inner.splice(idx, 0, value)
    fn remove(mut self, idx: usize) -> T;             // JS: this.inner.splice(idx, 1)[0]
    fn swap_remove(mut self, idx: usize) -> T;        // JS: O(1) removal via swap
    fn clear(mut self);                               // JS: this.inner = []
    fn truncate(mut self, len: usize);                // JS: this.inner.length = len
    fn retain(mut self, f: fn(T) -> bool);            // JS: this.inner = this.inner.filter(f)
    
    // Growing and shrinking
    fn reserve(mut self, additional: usize);          // JS: no-op
    fn shrink_to_fit(mut self);                       // JS: no-op
    fn resize(mut self, new_len: usize, value: T);    // JS: custom resize
    
    // Conversion
    fn into_array(self) -> array<T>;                  // JS: this.inner
    fn as_array(self) -> array<T>;                    // JS: this.inner (borrowed view)
    
    // Iteration
    fn iter(self) -> Iter<T>;                         // JS: this.inner[Symbol.iterator]()
    
    // Element access (delegates to array)
    fn get(self, idx: usize) -> Option<T>;            // JS: bounds check on inner
    fn first(self) -> Option<T>;                      // JS: this.inner[0]
    fn last(self) -> Option<T>;                       // JS: this.inner[length-1]
}

// Module-level functions
mod vec {
    fn from_elem<T>(elem: T, n: usize) -> Vec<T>;     // JS: Array(n).fill(elem)
    fn from_fn<T>(n: usize, f: fn(usize) -> T) -> Vec<T>; // JS: Array.from({length: n}, f)
}

// JavaScript Translation Notes:
// 1. Arrays are primitive, immutable operations return new arrays
// 2. Vec<T> wraps array for mutable, Rust-like API
// 3. No real capacity management in JS
// 4. Iterator methods return actual iterators (using JS generators)
// 5. Bounds checking returns Option<T> for safety
```

### 4. Iterator Module (Rust: std::iter)

```husk
// Iterator trait - core of Rust's iteration (Rust: Iterator)
trait Iterator {
    type Item;
    
    // Required method (Rust: Iterator::next)
    fn next(&mut self) -> Option<Self::Item>;
    
    // Adapters - lazy transformation (Rust: Iterator methods)
    fn map<B, F>(self, f: F) -> Map<Self, F>         // JS: .map()
        where F: FnMut(Self::Item) -> B;
    
    fn filter<P>(self, p: P) -> Filter<Self, P>      // JS: .filter()
        where P: FnMut(&Self::Item) -> bool;
    
    fn filter_map<B, F>(self, f: F) -> FilterMap<Self, F>  // JS: .map().filter()
        where F: FnMut(Self::Item) -> Option<B>;
    
    fn flat_map<U, F>(self, f: F) -> FlatMap<Self, U, F>   // JS: .flatMap()
        where F: FnMut(Self::Item) -> U, U: IntoIterator;
    
    fn flatten(self) -> Flatten<Self>                 // JS: .flat()
        where Self::Item: IntoIterator;
    
    fn take(self, n: usize) -> Take<Self>;           // JS: .slice(0, n)
    fn skip(self, n: usize) -> Skip<Self>;           // JS: .slice(n)
    fn take_while<P>(self, p: P) -> TakeWhile<Self, P>     // JS: custom
        where P: FnMut(&Self::Item) -> bool;
    
    fn skip_while<P>(self, p: P) -> SkipWhile<Self, P>     // JS: custom
        where P: FnMut(&Self::Item) -> bool;
    
    fn peekable(self) -> Peekable<Self>;             // JS: custom lookahead
    fn cycle(self) -> Cycle<Self>;                   // JS: custom infinite repeat
    fn enumerate(self) -> Enumerate<Self>;           // JS: .entries() or map
    fn zip<U>(self, other: U) -> Zip<Self, U::IntoIter>    // JS: custom zip
        where U: IntoIterator;
    
    fn chain<U>(self, other: U) -> Chain<Self, U::IntoIter> // JS: concat
        where U: IntoIterator<Item = Self::Item>;
    
    // Consumers - force evaluation (Rust: Iterator methods)
    fn collect<B>(self) -> B                         // JS: [...iter] or Array.from()
        where B: FromIterator<Self::Item>;
    
    fn count(self) -> usize;                         // JS: .length after array
    fn last(self) -> Option<Self::Item>;             // JS: arr[arr.length-1]
    fn nth(&mut self, n: usize) -> Option<Self::Item>; // JS: arr[n]
    
    fn fold<B, F>(self, init: B, f: F) -> B          // JS: .reduce()
        where F: FnMut(B, Self::Item) -> B;
    
    fn reduce<F>(self, f: F) -> Option<Self::Item>   // JS: .reduce() no init
        where F: FnMut(Self::Item, Self::Item) -> Self::Item;
    
    fn all<F>(&mut self, f: F) -> bool               // JS: .every()
        where F: FnMut(Self::Item) -> bool;
    
    fn any<F>(&mut self, f: F) -> bool               // JS: .some()
        where F: FnMut(Self::Item) -> bool;
    
    fn find<P>(&mut self, p: P) -> Option<Self::Item> // JS: .find()
        where P: FnMut(&Self::Item) -> bool;
    
    fn position<P>(&mut self, p: P) -> Option<usize> // JS: .findIndex()
        where P: FnMut(Self::Item) -> bool;
    
    fn max(self) -> Option<Self::Item>               // JS: Math.max(...arr)
        where Self::Item: Ord;
    
    fn min(self) -> Option<Self::Item>               // JS: Math.min(...arr)
        where Self::Item: Ord;
    
    fn sum<S>(self) -> S                             // JS: .reduce((a,b) => a+b, 0)
        where S: Sum<Self::Item>;
    
    fn product<P>(self) -> P                         // JS: .reduce((a,b) => a*b, 1)
        where P: Product<Self::Item>;
}

// Common iterator types
struct Range<T> { start: T, end: T }                 // JS: for loop or Array.from
struct RangeInclusive<T> { start: T, end: T }        // JS: for loop with <=
struct Repeat<T> { value: T }                        // JS: Array(n).fill(value)
struct Once<T> { value: Option<T> }                  // JS: [value]

// JavaScript Translation Challenges:
// 1. Rust iterators are lazy, JS array methods are eager
//    → Need to implement lazy evaluation wrappers
// 2. Rust's zero-cost abstractions via inlining
//    → JS function call overhead for each operation
// 3. Rust's move semantics in iterator chains
//    → JS copies/clones values through the chain
// 4. Rust's infinite iterators (cycle, repeat)
//    → JS needs careful implementation to avoid hanging
// 5. Rust's iterator fusion optimizations
//    → JS engines may not optimize method chains as well
```

### 5. IO Module (Rust: std::io)

```husk
mod io {
    // Error type for I/O operations (Rust: std::io::Error)
    struct Error {
        kind: ErrorKind,
        message: String,
    }
    
    enum ErrorKind {
        NotFound,
        PermissionDenied,
        ConnectionRefused,
        ConnectionReset,
        Interrupted,
        InvalidData,
        TimedOut,
        WriteZero,
        UnexpectedEof,
        Other,
    }
    
    // Core traits (Rust: std::io traits)
    trait Read {
        fn read(&mut self, buf: &mut [u8]) -> Result<usize, Error>;
        fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize, Error>;
        fn read_to_string(&mut self, buf: &mut String) -> Result<usize, Error>;
        fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), Error>;
    }
    
    trait Write {
        fn write(&mut self, buf: &[u8]) -> Result<usize, Error>;
        fn write_all(&mut self, buf: &[u8]) -> Result<(), Error>;
        fn flush(&mut self) -> Result<(), Error>;
        fn write_fmt(&mut self, fmt: fmt::Arguments) -> Result<(), Error>;
    }
    
    trait BufRead: Read {
        fn fill_buf(&mut self) -> Result<&[u8], Error>;
        fn consume(&mut self, amt: usize);
        fn read_line(&mut self, buf: &mut String) -> Result<usize, Error>;
        fn lines(self) -> Lines<Self>;
    }
    
    // Standard streams (Rust: std::io::{stdin, stdout, stderr})
    fn stdin() -> Stdin;                             // JS: process.stdin
    fn stdout() -> Stdout;                           // JS: process.stdout
    fn stderr() -> Stderr;                           // JS: process.stderr
    
    struct Stdin;  // Handle to standard input
    struct Stdout; // Handle to standard output
    struct Stderr; // Handle to standard error
    
    impl Read for Stdin {
        // Platform-specific stdin reading
    }
    
    impl Write for Stdout {
        // Platform-specific stdout writing
    }
    
    impl Write for Stderr {
        // Platform-specific stderr writing
    }
    
    // Buffered wrappers (Rust: std::io::BufReader/BufWriter)
    struct BufReader<R: Read> {
        inner: R,
        buffer: Vec<u8>,
    }
    
    struct BufWriter<W: Write> {
        inner: W,
        buffer: Vec<u8>,
    }
    
    // Cursor for in-memory I/O (Rust: std::io::Cursor)
    struct Cursor<T> {
        inner: T,
        pos: usize,
    }
    
    // Utility functions (Rust: std::io functions)
    fn copy<R: Read, W: Write>(reader: &mut R, writer: &mut W) -> Result<usize, Error>;
    fn empty() -> Empty;  // Reader that always returns EOF
    fn repeat(byte: u8) -> Repeat;  // Reader that infinitely repeats a byte
    fn sink() -> Sink;  // Writer that discards all data
}

// JavaScript Translation Challenges:
// 1. Rust's trait-based I/O vs JS's callback/promise-based
//    → Need to wrap Node.js streams or use sync APIs
// 2. Rust's error handling with Result<T, Error>
//    → JS uses exceptions or error callbacks
// 3. Buffered I/O in Rust for performance
//    → JS streams handle buffering internally
// 4. Rust's sync I/O by default, async via tokio
//    → JS I/O is inherently async, need special handling
// 5. Binary data handling: Rust uses [u8]
//    → JS uses various types (Buffer, Uint8Array, strings)
```

### 6. File System Module (Rust: std::fs)

```husk
mod fs {
    use io::{Read, Write, Error};
    
    // File handle (Rust: std::fs::File)
    struct File {
        // Platform-specific file handle
    }
    
    impl File {
        // Opening files (Rust: File methods)
        fn open<P: AsRef<Path>>(path: P) -> Result<File, Error>;     // JS: fs.openSync()
        fn create<P: AsRef<Path>>(path: P) -> Result<File, Error>;   // JS: fs.openSync('w')
        fn options() -> OpenOptions;                                   // JS: custom builder
        
        // Metadata (Rust: File methods)
        fn metadata(&self) -> Result<Metadata, Error>;               // JS: fs.fstatSync()
        fn sync_all(&self) -> Result<(), Error>;                     // JS: fs.fsyncSync()
        fn sync_data(&self) -> Result<(), Error>;                    // JS: fs.fdatasyncSync()
        fn set_len(&self, size: u64) -> Result<(), Error>;          // JS: fs.ftruncateSync()
        fn set_permissions(&self, perm: Permissions) -> Result<(), Error>; // JS: fs.fchmodSync()
    }
    
    impl Read for File { /* ... */ }                                 // JS: fs.readSync()
    impl Write for File { /* ... */ }                               // JS: fs.writeSync()
    
    // Builder for file options (Rust: std::fs::OpenOptions)
    struct OpenOptions {
        read: bool,
        write: bool,
        append: bool,
        truncate: bool,
        create: bool,
        create_new: bool,
    }
    
    // Directory operations (Rust: std::fs functions)
    fn create_dir<P: AsRef<Path>>(path: P) -> Result<(), Error>;    // JS: fs.mkdirSync()
    fn create_dir_all<P: AsRef<Path>>(path: P) -> Result<(), Error>; // JS: fs.mkdirSync({recursive: true})
    fn remove_dir<P: AsRef<Path>>(path: P) -> Result<(), Error>;    // JS: fs.rmdirSync()
    fn remove_dir_all<P: AsRef<Path>>(path: P) -> Result<(), Error>; // JS: fs.rmSync({recursive: true})
    fn read_dir<P: AsRef<Path>>(path: P) -> Result<ReadDir, Error>; // JS: fs.readdirSync()
    
    // File operations (Rust: std::fs functions)
    fn remove_file<P: AsRef<Path>>(path: P) -> Result<(), Error>;   // JS: fs.unlinkSync()
    fn copy<P: AsRef<Path>>(from: P, to: P) -> Result<u64, Error>;  // JS: fs.copyFileSync()
    fn rename<P: AsRef<Path>>(from: P, to: P) -> Result<(), Error>; // JS: fs.renameSync()
    fn hard_link<P: AsRef<Path>>(src: P, dst: P) -> Result<(), Error>; // JS: fs.linkSync()
    
    // Convenience functions (Rust: std::fs functions)
    fn read<P: AsRef<Path>>(path: P) -> Result<Vec<u8>, Error>;     // JS: fs.readFileSync()
    fn read_to_string<P: AsRef<Path>>(path: P) -> Result<String, Error>; // JS: fs.readFileSync('utf8')
    fn write<P: AsRef<Path>>(path: P, contents: &[u8]) -> Result<(), Error>; // JS: fs.writeFileSync()
    
    // Metadata (Rust: std::fs functions)
    fn metadata<P: AsRef<Path>>(path: P) -> Result<Metadata, Error>; // JS: fs.statSync()
    fn symlink_metadata<P: AsRef<Path>>(path: P) -> Result<Metadata, Error>; // JS: fs.lstatSync()
    fn canonicalize<P: AsRef<Path>>(path: P) -> Result<PathBuf, Error>; // JS: fs.realpathSync()
    fn read_link<P: AsRef<Path>>(path: P) -> Result<PathBuf, Error>; // JS: fs.readlinkSync()
    
    // Directory iterator (Rust: std::fs::ReadDir)
    struct ReadDir {
        // Iterator over directory entries
    }
    
    struct DirEntry {
        // Single directory entry
    }
    
    impl DirEntry {
        fn path(&self) -> PathBuf;                                   // JS: full path
        fn file_name(&self) -> OsString;                            // JS: basename
        fn metadata(&self) -> Result<Metadata, Error>;              // JS: fs.statSync()
        fn file_type(&self) -> Result<FileType, Error>;             // JS: from stats
    }
    
    // File metadata (Rust: std::fs::Metadata)
    struct Metadata {
        // File/directory metadata
    }
    
    impl Metadata {
        fn file_type(&self) -> FileType;                            // JS: stats.isFile() etc
        fn is_dir(&self) -> bool;                                   // JS: stats.isDirectory()
        fn is_file(&self) -> bool;                                  // JS: stats.isFile()
        fn is_symlink(&self) -> bool;                               // JS: stats.isSymbolicLink()
        fn len(&self) -> u64;                                       // JS: stats.size
        fn permissions(&self) -> Permissions;                       // JS: stats.mode
        fn modified(&self) -> Result<SystemTime, Error>;            // JS: stats.mtime
        fn accessed(&self) -> Result<SystemTime, Error>;            // JS: stats.atime
        fn created(&self) -> Result<SystemTime, Error>;             // JS: stats.birthtime
    }
    
    // File type (Rust: std::fs::FileType)
    struct FileType {
        // Type of file system object
    }
    
    // Permissions (Rust: std::fs::Permissions)
    struct Permissions {
        // Unix-style permissions
    }
}

// JavaScript Translation Challenges:
// 1. Rust's sync I/O by default vs JS async-first
//    → Use sync versions of Node.js fs methods
// 2. Rust's Path/PathBuf vs JS strings
//    → Need path normalization and validation
// 3. Rust's fine-grained permissions vs JS limited
//    → Map Unix permissions to JS available APIs
// 4. Rust's iterator pattern for ReadDir
//    → JS returns arrays, need to wrap for iteration
// 5. Error handling differences
//    → Convert JS exceptions to Result types
```

## Key Differences: Rust vs Husk vs JavaScript

### Type Unification
- **Rust**: Distinguishes &str/String, &[T]/Vec<T> for ownership
- **Husk**: Unified string type, primitive array type + Vec<T> wrapper
- **JS Translation**: Direct mapping to JS string and array primitives

### Ownership and Mutability
- **Rust**: Strict ownership with borrowing rules (&T, &mut T)
- **Husk**: Simplified model - immutable by default, explicit mut for modification
- **JS Translation**: All values are effectively owned, mutations modify in-place

### Iterator Design
- **Rust**: Lazy evaluation, zero-cost abstractions
- **Husk**: Implement lazy iterators with adapter pattern
- **JS Translation**: Use generator functions for lazy evaluation

### Error Handling
- **Rust**: Result<T, E> and Option<T> for explicit error handling
- **Husk**: Keep Result/Option pattern from Rust
- **JS Translation**: Convert to/from JS undefined/null at boundaries

### Memory Management
- **Rust**: Explicit capacity management (Vec capacity, String capacity)
- **Husk**: Expose capacity APIs for compatibility but no-op in implementation
- **JS Translation**: Ignore capacity hints, let JS engine optimize

### Method Semantics
- **Rust**: Methods take self, &self, or &mut self
- **Husk**: Methods take self by value (immutable) or mut self (mutable)
- **JS Translation**: All methods operate on values directly

## Implementation Details

### 1. Built-in Method Resolution

The semantic analyzer will be extended to recognize method calls on primitive types:

```rust
// In semantic analyzer
match expr_type {
    Type::String => resolve_string_method(method_name, args),
    Type::Array(_) => resolve_array_method(method_name, args),
    Type::Int | Type::Float => resolve_numeric_method(method_name, args),
    // ... other types
}
```

### 2. Interpreter Implementation

Built-in methods will be implemented directly in the interpreter:

```rust
// In interpreter
match (value, method_name) {
    (Value::String(s), "len") => Ok(Value::Int(s.len() as i64)),
    (Value::String(s), "trim") => Ok(Value::String(s.trim().to_string())),
    // ... other methods
}
```

### 3. Transpiler Implementation

The transpiler will map Husk methods to JavaScript equivalents:

```rust
// In transpiler
match (expr_type, method_name) {
    // String methods (unified type)
    (Type::String, "len") => format!("{}.length", obj_code),
    (Type::String, "is_empty") => format!("({}.length === 0)", obj_code),
    (Type::String, "chars") => format!("Array.from({})", obj_code),
    (Type::String, "trim") => format!("{}.trim()", obj_code),
    (Type::String, "trim_start") => format!("{}.trimStart()", obj_code),
    (Type::String, "contains") => format!("{}.includes({})", obj_code, args[0]),
    (Type::String, "find") => {
        // Convert JS indexOf to Option<usize>
        format!("({{const i = {}.indexOf({}); i >= 0 ? {{tag: 'Some', value: i}} : {{tag: 'None'}}}})", 
                obj_code, args[0])
    }
    (Type::String, "split") => format!("{}.split({})", obj_code, args[0]),
    (Type::String, "replace") => format!("{}.replaceAll({}, {})", obj_code, args[0], args[1]),
    
    // Array methods (primitive type)
    (Type::Array(_), "len") => format!("{}.length", obj_code),
    (Type::Array(_), "is_empty") => format!("({}.length === 0)", obj_code),
    (Type::Array(_), "first") => {
        format!("({}[0] !== undefined ? {{tag: 'Some', value: {}[0]}} : {{tag: 'None'}})",
                obj_code, obj_code)
    }
    (Type::Array(_), "map") => format!("{}.map({})", obj_code, args[0]),
    (Type::Array(_), "filter") => format!("{}.filter({})", obj_code, args[0]),
    (Type::Array(_), "find") => {
        format!("({{const v = {}.find({}); v !== undefined ? {{tag: 'Some', value: v}} : {{tag: 'None'}}}})",
                obj_code, args[0])
    }
    
    // Vec<T> methods (mutable operations)
    (Type::UserType("Vec", _), "push") => format!("{}.inner.push({})", obj_code, args[0]),
    (Type::UserType("Vec", _), "pop") => {
        format!("({{const v = {}.inner.pop(); v !== undefined ? {{tag: 'Some', value: v}} : {{tag: 'None'}}}})",
                obj_code)
    }
    (Type::UserType("Vec", _), "len") => format!("{}.inner.length", obj_code),
    
    // Iterator methods (requires runtime support)
    (Type::Iterator(_), "map") => format!("__husk_iter_map({}, {})", obj_code, args[0]),
    (Type::Iterator(_), "filter") => format!("__husk_iter_filter({}, {})", obj_code, args[0]),
    (Type::Iterator(_), "collect") => format!("__husk_iter_collect({})", obj_code),
    
    // ... other mappings
}
```

### 4. Module Loading

Standard library modules will be resolved from a special path:

```rust
// In package resolver
if module_path.starts_with("std::") {
    load_stdlib_module(module_path)
} else {
    load_user_module(module_path)
}
```

## Testing Strategy

1. **Unit Tests**: Test each function/method individually
2. **Integration Tests**: Test module interactions
3. **Cross-Mode Tests**: Ensure interpreter and transpiler produce identical results
4. **Performance Tests**: Benchmark critical operations
5. **Documentation Tests**: Ensure all examples in documentation work

## Implementation Phases

### Phase 1: Core Foundation (Week 1-2)
- Extend parser to support method calls on primitives
- Implement basic string methods (len, trim, split, etc.)
- Implement basic array methods (len, push, pop, etc.)
- Set up module loading infrastructure

### Phase 2: Essential Modules (Week 3-4)
- Complete string and array modules
- Implement math module
- Add prelude with print/println functions
- Create comprehensive test suite

### Phase 3: I/O and File System (Week 5)
- Implement io module for console I/O
- Add fs module for file operations
- Handle platform-specific differences

### Phase 4: Advanced Features (Week 6)
- JSON parsing and serialization
- Regular expressions
- Time and date utilities
- Random number generation

### Phase 5: Collections (Week 7)
- HashMap implementation
- HashSet implementation
- Queue and other data structures

### Phase 6: Polish and Documentation (Week 8)
- Performance optimization
- Comprehensive documentation
- Example programs
- Tutorial materials

## Detailed Method Implementation: `chars()`

### Overview
The `chars()` method converts a string into an array of single-character strings, handling Unicode properly. Since Husk doesn't have a separate char type (unlike Rust), we represent characters as single-character strings, which aligns well with JavaScript's string model.

### Design Decision: No Char Type
Unlike Rust which has a dedicated `char` type for Unicode scalar values, Husk uses single-character strings to represent characters. This simplifies the type system and aligns naturally with JavaScript where there's no distinction between characters and strings.

### JavaScript Unicode Challenges
JavaScript strings use UTF-16 encoding internally, which presents several challenges:
1. Surrogate pairs: Characters outside the Basic Multilingual Plane (e.g., emojis) are represented as two 16-bit code units
2. `string.split('')` or indexing can break surrogate pairs
3. `Array.from()` or spread operator `[...str]` properly handle Unicode

### Implementation Design

```husk
// In std/string.husk
impl string {
    // Returns an array of single-character strings, properly handling Unicode
    fn chars(self) -> array<string> {
        // Implementation will differ between interpreter and transpiler
    }
}
```

### Interpreter Implementation

```rust
// In interpreter.rs
fn builtin_string_chars(s: &str) -> Value {
    // Rust's chars() already handles Unicode correctly
    // Convert each char to a single-character string
    let chars: Vec<Value> = s.chars()
        .map(|c| Value::String(c.to_string()))
        .collect();
    Value::Array(chars)
}
```

### Transpiler Implementation

```rust
// In transpiler.rs
(Type::String, "chars") => {
    // Use Array.from() which properly handles Unicode surrogate pairs
    format!("Array.from({})", obj_code)
}
```

### JavaScript Runtime Behavior

```javascript
// Example of the difference:
const str = "Hello 👋 World";

// Wrong - breaks surrogate pairs:
str.split('') // ['H','e','l','l','o',' ','�','�',' ','W','o','r','l','d']

// Correct - handles Unicode properly:
Array.from(str) // ['H','e','l','l','o',' ','👋',' ','W','o','r','l','d']
[...str]        // Same result - each element is a single-char string
```

### Type System Considerations

1. **No Character Type**: Characters are represented as single-character strings
2. **Array Return**: Returns `array<string>` where each string has length 1 (when properly counted with Unicode awareness)
3. **Memory**: Arrays are eagerly evaluated, unlike Rust's lazy iterator

### Test Cases

```husk
// Basic ASCII
assert_eq("hello".chars(), ["h", "e", "l", "l", "o"]);

// Unicode
assert_eq("café".chars(), ["c", "a", "f", "é"]);

// Emojis (surrogate pairs in JS)
assert_eq("👋🌍".chars(), ["👋", "🌍"]);

// Mixed scripts
assert_eq("Hello世界".chars(), ["H", "e", "l", "l", "o", "世", "界"]);

// Empty string
assert_eq("".chars(), []);

// Verify each element is a single character
let chars = "test👋".chars();
assert_eq(chars.len(), 5);
assert_eq(chars[4], "👋");
```

### Implementation Steps

1. **Semantic Analysis**
   ```rust
   // Register chars() method on string type
   const STRING_METHODS: &[BuiltinMethod] = &[
       BuiltinMethod { 
           name: "chars", 
           param_types: vec![], 
           return_type: Type::Array(Box::new(Type::String)) // array<string>
       },
       // ...
   ];
   ```

2. **Interpreter Implementation**
   ```rust
   // In visit_method_call for string methods
   match method_name {
       "chars" => {
           if let Value::String(s) = obj_value {
               let chars: Vec<Value> = s.chars()
                   .map(|c| Value::String(c.to_string()))
                   .collect();
               Ok(Value::Array(chars))
           } else {
               Err(Error::new_runtime("chars() called on non-string", span))
           }
       }
       // ... other methods
   }
   ```

3. **Transpiler Implementation**
   ```rust
   // In transpiler method call handling
   (Type::String, "chars") => {
       // Array.from handles Unicode correctly
       format!("Array.from({})", obj_code)
   }
   ```

### Benefits of This Approach

1. **Simplicity**: No need for a separate char type in the type system
2. **JavaScript Alignment**: Natural mapping to JavaScript's string model
3. **Unicode Safety**: Array.from() handles surrogate pairs correctly
4. **Consistency**: All string operations work with the string type

### Alternative Designs Considered

1. **Dedicated Char Type**: Would require additional type system complexity
2. **Return Iterator**: More Rust-like but requires full iterator implementation
3. **Code Point Numbers**: Could return array<int> of Unicode code points, but less intuitive

### Recommendation
Implement as specified: `chars(self) -> array<string>` returning an eagerly-evaluated array of single-character strings. This provides the functionality of Rust's chars() method while keeping the type system simple and JavaScript-friendly.

## Open Questions

1. **Mutability Model**: Follow Rust's explicit mutability with `&mut self` methods?
2. **Iterator Implementation**: Use generator functions or custom iterator objects in JS?
3. **Async Support**: Add async versions of I/O operations or keep sync-only?
4. **Platform Differences**: Abstract over Node.js/Browser differences or target Node.js only?
5. **Runtime Library**: How much runtime support code for JS (iterators, Option/Result)?
6. **Path Handling**: Implement full Path/PathBuf or use strings like JS?
7. **Trait Bounds**: How to enforce trait bounds (Ord, Eq) in transpiled JS?

## JavaScript Runtime Support Library

To properly support Rust-style APIs in JavaScript, we'll need a runtime support library:

```javascript
// husk-runtime.js

// Iterator support
class HuskIterator {
    constructor(next) {
        this.next = next;
    }
    
    map(f) {
        return new HuskIterator(() => {
            const item = this.next();
            return item.tag === 'Some' 
                ? { tag: 'Some', value: f(item.value) }
                : { tag: 'None' };
        });
    }
    
    filter(p) {
        return new HuskIterator(() => {
            while (true) {
                const item = this.next();
                if (item.tag === 'None') return item;
                if (p(item.value)) return item;
            }
        });
    }
    
    collect() {
        const result = [];
        while (true) {
            const item = this.next();
            if (item.tag === 'None') break;
            result.push(item.value);
        }
        return result;
    }
}

// Result/Option helpers
function unwrap_or(option, default_value) {
    return option.tag === 'Some' ? option.value : default_value;
}

function is_some(option) {
    return option.tag === 'Some';
}

function is_none(option) {
    return option.tag === 'None';
}

// String slice emulation
class StrSlice {
    constructor(string, start, end) {
        this.string = string;
        this.start = start;
        this.end = end;
    }
    
    toString() {
        return this.string.substring(this.start, this.end);
    }
}

// Ensure proper Unicode handling for string operations
function string_chars(str) {
    // Array.from handles surrogate pairs correctly
    // Returns array of single-character strings
    return Array.from(str);
}

// Helper to check if a string is a single character (Unicode-aware)
function is_single_char(value) {
    return typeof value === 'string' && Array.from(value).length === 1;
}

// Export for use in transpiled code
window.__husk_runtime = {
    HuskIterator,
    unwrap_or,
    is_some,
    is_none,
    StrSlice,
    string_chars,
    is_single_char
};
```

## Success Criteria

1. All standard library functions work identically in interpreter and transpiler
2. Rust-like API that feels familiar to Rust developers
3. Efficient JavaScript output with minimal runtime overhead
4. Proper handling of Rust patterns (Option, Result, iterators)
5. Clear documentation of differences between Rust and Husk stdlib
6. Comprehensive test suite covering all edge cases

## Implementation Status

### String Methods (Interpreter & Transpiler)
- [x] `len()` - Returns string length
- [x] `trim()` - Removes whitespace from both ends
- [x] `chars()` - Returns array of single-character strings (Unicode-aware)
- [x] `substring(start, end)` - Extract substring by indices
- [x] `split(delimiter)` - Split string by delimiter
- [x] `to_lowercase()` - Convert to lowercase (renamed from toLowerCase)
- [x] `to_uppercase()` - Convert to uppercase (renamed from toUpperCase)
- [x] `contains(pattern)` - Check if string contains pattern
- [x] `starts_with(prefix)` - Check if string starts with prefix
- [x] `ends_with(suffix)` - Check if string ends with suffix
- [x] `replace(from, to)` - Replace all occurrences
- [x] `find(pattern)` - Find first occurrence (returns Option<usize>)
- [x] `rfind(pattern)` - Find last occurrence (returns Option<usize>)
- [ ] `is_empty()` - Check if string is empty
- [ ] `bytes()` - Get UTF-8 bytes
- [ ] `trim_start()` - Remove leading whitespace
- [ ] `trim_end()` - Remove trailing whitespace
- [ ] Other methods...

### Array Methods (Interpreter & Transpiler)
- [x] `len()` - Returns array length
- [x] `is_empty()` - Check if array is empty
- [x] `first()` - Get first element (returns Option<T>)
- [x] `last()` - Get last element (returns Option<T>)
- [x] `get(index)` - Get element at index (returns Option<T>)
- [x] `slice(start, end)` - Extract sub-array
- [x] `concat(other)` - Concatenate arrays
- [x] `join(separator)` - Join elements into string
- [x] `contains(value)` - Check if array contains value
- [x] `reverse()` - Returns new reversed array
- [x] `push(...values)` - Returns new array with pushed elements
- [x] `pop()` - Returns tuple of (new_array, Option<popped_value>)
- [ ] `map()` - Needs closure support
- [ ] `filter()` - Needs closure support
- [ ] Other methods...

### Known Issues
1. **Closure Support**: Functional methods like map() and filter() are blocked on closure implementation.

### Next Steps
1. Continue implementing remaining string methods (is_empty, bytes, trim_start, trim_end, etc.)
2. Continue implementing remaining array methods
3. Implement Vec<T> type with mutable operations
4. Add iterator support for lazy evaluation
5. Implement other core modules (io, fs, path, etc.)