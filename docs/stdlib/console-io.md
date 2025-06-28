# Console I/O

Console Input/Output operations allow interaction with the terminal/command line. Husk provides safe, cross-platform console operations with support for colored output, formatted printing, and user input. This guide covers all console I/O functions in the standard library.

## Table of Contents

- [Overview](#overview)
- [Output Operations](#output-operations)
  - [Basic Printing](#basic-printing)
  - [Formatted Output](#formatted-output)
  - [Error Output](#error-output)
  - [Colored Output](#colored-output)
- [Input Operations](#input-operations)
  - [Reading Lines](#reading-lines)
  - [Reading Characters](#reading-characters)
  - [Parsing Input](#parsing-input)
  - [Input Validation](#input-validation)
- [Interactive Programs](#interactive-programs)
- [Terminal Control](#terminal-control)
- [Progress Indicators](#progress-indicators)
- [Error Handling](#error-handling)
- [Performance Considerations](#performance-considerations)
- [Common Patterns](#common-patterns)
- [JavaScript/Node.js Interop](#javascriptnodejs-interop)
- [Related Topics](#related-topics)

## Overview

Husk's console I/O features:
- **Cross-platform** - Works on all supported platforms
- **UTF-8 support** - Full Unicode text output
- **Buffered I/O** - Efficient for bulk operations
- **Error handling** - Safe operations with Result types
- **Formatting** - Rich text formatting capabilities

### Key Principles

1. **Separation of output streams** - stdout vs stderr
2. **Buffering control** - Manual flush when needed
3. **Error propagation** - I/O operations can fail
4. **Unicode support** - Proper handling of international text

## Output Operations

### Basic Printing

```rust
// Print to stdout
print("Hello, World!");
print("Line 1");
print("Line 2");
// Output: Hello, World!Line 1Line 2

// Print with newline
println("Hello, World!");
println("Line 1");
println("Line 2");
// Output:
// Hello, World!
// Line 1
// Line 2

// Print multiple values
println("Name:", name, "Age:", age);

// Print empty line
println();

// Print without trailing newline
print("Enter name: ");
flush_stdout(); // Ensure prompt appears
let name = read_line()?;
```

### Formatted Output

```rust
// Using format strings
let name = "Alice";
let age = 30;
let score = 95.5;

// Positional arguments
println("Hello, {}!", name);
println("{} is {} years old", name, age);
println("Score: {:.1}%", score); // One decimal place

// Named arguments
println("{name} scored {score:.2} points", name=name, score=score);

// Multiple formatting options
println("Binary: {:b}, Hex: {:x}, Octal: {:o}", 42, 42, 42);
// Output: Binary: 101010, Hex: 2a, Octal: 52

// Padding and alignment
println("{:>10}", "right");     // "     right"
println("{:<10}", "left");      // "left      "
println("{:^10}", "center");    // "  center  "
println("{:*^10}", "center");   // "**center**"

// Number formatting
println("{:05}", 42);           // "00042"
println("{:+}", 42);            // "+42"
println("{:.3}", 3.14159);      // "3.142"
println("{:e}", 1000.0);        // "1e3"

// Debug formatting
let data = vec![1, 2, 3];
println("{:?}", data);          // Debug output
println("{:#?}", data);         // Pretty debug output

// Custom formatting
struct Point { x: int, y: int }
println("Point: ({}, {})", point.x, point.y);
```

### Error Output

```rust
// Print to stderr
eprintln("Error: File not found");
eprintln("Warning: {}", warning_message);

// Error with context
fn report_error(error: &Error) {
    eprintln("Error occurred in {}: {}", 
             error.location(), error.message());
}

// Error codes
fn exit_with_error(code: int, message: &str) -> ! {
    eprintln("Fatal error: {}", message);
    exit(code);
}

// Conditional error output
if debug_mode {
    eprintln("Debug: Processing item {}", id);
}

// Structured error reporting
struct ErrorReport {
    level: string,
    component: string,
    message: string,
    timestamp: string,
}

impl ErrorReport {
    fn print(&self) {
        eprintln("[{}] {} - {}: {}", 
                self.timestamp, self.level, 
                self.component, self.message);
    }
}
```

### Colored Output

```rust
// ANSI color codes
println("{}Red text{}", ansi::RED, ansi::RESET);
println("{}Green text{}", ansi::GREEN, ansi::RESET);
println("{}Blue text{}", ansi::BLUE, ansi::RESET);

// Background colors
println("{}White on red{}", ansi::BG_RED, ansi::RESET);

// Text styles
println("{}Bold text{}", ansi::BOLD, ansi::RESET);
println("{}Italic text{}", ansi::ITALIC, ansi::RESET);
println("{}Underlined{}", ansi::UNDERLINE, ansi::RESET);

// Combined styles
println("{}{}Bold red text{}", ansi::BOLD, ansi::RED, ansi::RESET);

// Color helpers
fn print_success(message: &str) {
    println("{}✓{} {}", ansi::GREEN, ansi::RESET, message);
}

fn print_error(message: &str) {
    eprintln("{}✗{} {}", ansi::RED, ansi::RESET, message);
}

fn print_warning(message: &str) {
    println("{}⚠{} {}", ansi::YELLOW, ansi::RESET, message);
}

// Color detection
if is_terminal() && supports_color() {
    println("{}Colored output enabled{}", ansi::GREEN, ansi::RESET);
} else {
    println("Plain text output");
}

// Color palette
const COLORS = [
    ansi::RED,
    ansi::GREEN,
    ansi::BLUE,
    ansi::YELLOW,
    ansi::MAGENTA,
    ansi::CYAN,
];

for (i, item) in items.iter().enumerate() {
    let color = COLORS[i % COLORS.len()];
    println("{}{}{}", color, item, ansi::RESET);
}
```

## Input Operations

### Reading Lines

```rust
// Read single line
print("Enter your name: ");
flush_stdout();
let name = read_line()?;
println("Hello, {}!", name.trim());

// Read line with prompt
fn prompt(message: &str) -> Result<string, Error> {
    print("{}", message);
    flush_stdout();
    read_line()
}

let age_str = prompt("Enter your age: ")?;

// Read multiple lines
println("Enter your message (empty line to finish):");
let mut lines = vec![];
loop {
    let line = read_line()?;
    if line.trim().is_empty() {
        break;
    }
    lines.push(line);
}
let message = lines.join("");

// Read all input
let input = read_all_input()?;
println("You entered {} characters", input.len());

// Read with timeout (if supported)
match read_line_with_timeout(Duration::from_secs(5)) {
    Ok(line) => println("Input: {}", line),
    Err(TimeoutError) => println("No input received"),
    Err(e) => eprintln("Error: {}", e),
}
```

### Reading Characters

```rust
// Read single character
print("Press any key: ");
flush_stdout();
let ch = read_char()?;
println("You pressed: {}", ch);

// Read character without echo
let password_char = read_char_no_echo()?;

// Read arrow keys and special keys
loop {
    let key = read_key()?;
    match key {
        Key::Arrow(Direction::Up) => println("Up arrow"),
        Key::Arrow(Direction::Down) => println("Down arrow"),
        Key::Enter => break,
        Key::Escape => return,
        Key::Char(ch) => println("Character: {}", ch),
        Key::Ctrl(ch) => println("Ctrl+{}", ch),
    }
}

// Character input validation
fn read_yes_no() -> Result<bool, Error> {
    loop {
        print("Continue? (y/n): ");
        flush_stdout();
        
        match read_char()?.to_lowercase() {
            'y' => return Ok(true),
            'n' => return Ok(false),
            _ => println("Please enter 'y' or 'n'"),
        }
    }
}
```

### Parsing Input

```rust
// Parse number input
fn read_number<T: FromStr>() -> Result<T, Error> {
    loop {
        print("Enter a number: ");
        flush_stdout();
        
        let input = read_line()?;
        match input.trim().parse() {
            Ok(num) => return Ok(num),
            Err(_) => println("Invalid number, try again"),
        }
    }
}

let age: int = read_number()?;
let score: float = read_number()?;

// Parse with validation
fn read_positive_int() -> Result<int, Error> {
    loop {
        let num: int = read_number()?;
        if num > 0 {
            return Ok(num);
        }
        println("Number must be positive");
    }
}

// Parse structured input
fn read_point() -> Result<Point, Error> {
    println("Enter coordinates:");
    let x = prompt("X: ")?.trim().parse()?;
    let y = prompt("Y: ")?.trim().parse()?;
    Ok(Point { x, y })
}

// Parse multiple values
fn read_list() -> Result<Vec<int>, Error> {
    print("Enter numbers separated by spaces: ");
    flush_stdout();
    
    let input = read_line()?;
    let numbers = input.trim()
        .split_whitespace()
        .map(|s| s.parse())
        .collect::<Result<Vec<int>, _>>()?;
    
    Ok(numbers)
}
```

### Input Validation

```rust
// Email validation
fn read_email() -> Result<string, Error> {
    loop {
        let email = prompt("Enter email: ")?;
        let email = email.trim();
        
        if email.contains('@') && email.contains('.') {
            return Ok(email.to_string());
        }
        
        println("Invalid email format");
    }
}

// Range validation
fn read_in_range(min: int, max: int) -> Result<int, Error> {
    loop {
        let num = read_number()?;
        if num >= min && num <= max {
            return Ok(num);
        }
        println("Number must be between {} and {}", min, max);
    }
}

// Password input
fn read_password() -> Result<string, Error> {
    print("Enter password: ");
    flush_stdout();
    
    let mut password = String::new();
    loop {
        let ch = read_char_no_echo()?;
        match ch {
            '\n' | '\r' => break,
            '\x08' | '\x7f' => { // Backspace
                if !password.is_empty() {
                    password.pop();
                    print("\x08 \x08"); // Erase character
                    flush_stdout();
                }
            },
            ch if ch.is_control() => {}, // Ignore other control chars
            ch => {
                password.push(ch);
                print("*");
                flush_stdout();
            }
        }
    }
    println(); // New line
    Ok(password)
}

// Choice validation
fn read_choice(options: &[&str]) -> Result<usize, Error> {
    println("Choose an option:");
    for (i, option) in options.iter().enumerate() {
        println("{}. {}", i + 1, option);
    }
    
    loop {
        let choice = read_number::<usize>()?;
        if choice >= 1 && choice <= options.len() {
            return Ok(choice - 1);
        }
        println("Invalid choice, try again");
    }
}
```

## Interactive Programs

### Menus

```rust
enum MenuChoice {
    NewFile,
    OpenFile,
    SaveFile,
    Exit,
}

fn show_menu() -> Result<MenuChoice, Error> {
    println("=== File Manager ===");
    println("1. New File");
    println("2. Open File");
    println("3. Save File");
    println("4. Exit");
    println();
    
    let choice = read_choice(&["New File", "Open File", "Save File", "Exit"])?;
    
    match choice {
        0 => Ok(MenuChoice::NewFile),
        1 => Ok(MenuChoice::OpenFile),
        2 => Ok(MenuChoice::SaveFile),
        3 => Ok(MenuChoice::Exit),
        _ => unreachable!(),
    }
}

// Main program loop
fn main() -> Result<(), Error> {
    loop {
        match show_menu()? {
            MenuChoice::NewFile => new_file()?,
            MenuChoice::OpenFile => open_file()?,
            MenuChoice::SaveFile => save_file()?,
            MenuChoice::Exit => break,
        }
    }
    Ok(())
}
```

### Wizards and Forms

```rust
struct UserProfile {
    name: string,
    email: string,
    age: int,
    country: string,
}

fn create_user_profile() -> Result<UserProfile, Error> {
    println("=== User Profile Setup ===");
    println();
    
    let name = prompt("Full name: ")?;
    let email = read_email()?;
    let age = read_in_range(13, 120)?;
    let country = prompt("Country: ")?;
    
    let profile = UserProfile {
        name: name.trim().to_string(),
        email,
        age,
        country: country.trim().to_string(),
    };
    
    // Confirmation
    println();
    println("Profile Summary:");
    println("Name: {}", profile.name);
    println("Email: {}", profile.email);
    println("Age: {}", profile.age);
    println("Country: {}", profile.country);
    println();
    
    if read_yes_no()? {
        Ok(profile)
    } else {
        Err(Error::UserCancelled)
    }
}
```

### Command Line Interface

```rust
fn cli_interface() -> Result<(), Error> {
    println("Welcome to Husk CLI. Type 'help' for commands.");
    
    loop {
        print("> ");
        flush_stdout();
        
        let input = read_line()?;
        let parts: Vec<&str> = input.trim().split_whitespace().collect();
        
        if parts.is_empty() {
            continue;
        }
        
        match parts[0] {
            "help" => show_help(),
            "exit" | "quit" => break,
            "list" => list_files()?,
            "create" => {
                if parts.len() > 1 {
                    create_file(parts[1])?;
                } else {
                    println("Usage: create <filename>");
                }
            },
            "delete" => {
                if parts.len() > 1 {
                    delete_file(parts[1])?;
                } else {
                    println("Usage: delete <filename>");
                }
            },
            cmd => println("Unknown command: {}. Type 'help' for available commands.", cmd),
        }
    }
    
    Ok(())
}

fn show_help() {
    println("Available commands:");
    println("  help     - Show this help");
    println("  list     - List files");
    println("  create   - Create a file");
    println("  delete   - Delete a file");
    println("  exit     - Exit the program");
}
```

## Terminal Control

### Screen Control

```rust
// Clear screen
fn clear_screen() {
    print("\x1b[2J\x1b[H");
    flush_stdout();
}

// Move cursor
fn move_cursor(row: int, col: int) {
    print("\x1b[{};{}H", row, col);
    flush_stdout();
}

// Hide/show cursor
fn hide_cursor() {
    print("\x1b[?25l");
    flush_stdout();
}

fn show_cursor() {
    print("\x1b[?25h");
    flush_stdout();
}

// Save/restore cursor position
fn save_cursor() {
    print("\x1b[s");
    flush_stdout();
}

fn restore_cursor() {
    print("\x1b[u");
    flush_stdout();
}

// Get terminal size
fn get_terminal_size() -> Result<(int, int), Error> {
    // Implementation depends on platform
    // Returns (width, height)
}
```

### Alternative Screen

```rust
// Enter alternative screen buffer
fn enter_alt_screen() {
    print("\x1b[?1049h");
    flush_stdout();
}

// Exit alternative screen buffer
fn exit_alt_screen() {
    print("\x1b[?1049l");
    flush_stdout();
}

// Full-screen application pattern
fn run_fullscreen_app() -> Result<(), Error> {
    enter_alt_screen();
    hide_cursor();
    
    // Ensure cleanup on exit
    let _guard = scopeguard::guard((), |_| {
        show_cursor();
        exit_alt_screen();
    });
    
    // Application logic
    clear_screen();
    move_cursor(1, 1);
    println("Full-screen application");
    
    // Event loop
    loop {
        let key = read_key()?;
        match key {
            Key::Char('q') | Key::Escape => break,
            Key::Char(ch) => {
                println("You pressed: {}", ch);
            },
            _ => {},
        }
    }
    
    Ok(())
}
```

## Progress Indicators

### Progress Bars

```rust
struct ProgressBar {
    total: int,
    current: int,
    width: int,
    label: string,
}

impl ProgressBar {
    fn new(total: int, label: string) -> ProgressBar {
        ProgressBar {
            total,
            current: 0,
            width: 50,
            label,
        }
    }
    
    fn update(&mut self, current: int) {
        self.current = current;
        self.draw();
    }
    
    fn increment(&mut self) {
        self.current += 1;
        self.draw();
    }
    
    fn draw(&self) {
        let percentage = (self.current as float / self.total as float * 100.0) as int;
        let filled = (self.current as float / self.total as float * self.width as float) as int;
        let empty = self.width - filled;
        
        print("\r{}: [{}{}] {}% ({}/{})",
              self.label,
              "=".repeat(filled),
              " ".repeat(empty),
              percentage,
              self.current,
              self.total);
        flush_stdout();
    }
    
    fn finish(&self) {
        println(); // New line after completion
    }
}

// Usage
fn process_items(items: &[Item]) -> Result<(), Error> {
    let mut progress = ProgressBar::new(items.len() as int, "Processing".to_string());
    
    for (i, item) in items.iter().enumerate() {
        process_item(item)?;
        progress.update(i as int + 1);
        thread::sleep(Duration::from_millis(100)); // Simulate work
    }
    
    progress.finish();
    println("All items processed successfully!");
    Ok(())
}
```

### Spinners

```rust
struct Spinner {
    frames: array<&'static str>,
    current: int,
    message: string,
}

impl Spinner {
    fn new(message: string) -> Spinner {
        Spinner {
            frames: ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"],
            current: 0,
            message,
        }
    }
    
    fn tick(&mut self) {
        let frame = self.frames[self.current % self.frames.len()];
        print("\r{} {}", frame, self.message);
        flush_stdout();
        self.current += 1;
    }
    
    fn finish(&self, final_message: &str) {
        print("\r✓ {}\n", final_message);
        flush_stdout();
    }
}

// Usage with background task
fn long_running_task() -> Result<(), Error> {
    let mut spinner = Spinner::new("Loading data...".to_string());
    
    // Simulate work with periodic updates
    for _ in 0..50 {
        spinner.tick();
        thread::sleep(Duration::from_millis(100));
        // Do actual work here
    }
    
    spinner.finish("Data loaded successfully");
    Ok(())
}
```

## Error Handling

### I/O Error Types

```rust
enum ConsoleError {
    InputError(string),
    OutputError(string),
    TerminalNotSupported,
    Interrupted,
    EndOfInput,
}

// Handling input errors
fn safe_read_line() -> Result<string, ConsoleError> {
    read_line()
        .map_err(|e| ConsoleError::InputError(e.to_string()))
}

// Handling output errors
fn safe_println(message: &str) -> Result<(), ConsoleError> {
    println("{}", message)
        .map_err(|e| ConsoleError::OutputError(e.to_string()))
}

// Graceful error recovery
fn robust_input(prompt: &str) -> Result<string, ConsoleError> {
    loop {
        print("{}", prompt);
        flush_stdout()?;
        
        match read_line() {
            Ok(line) => return Ok(line),
            Err(e) if e.kind() == ErrorKind::Interrupted => {
                println("\nInput interrupted. Try again.");
                continue;
            },
            Err(e) => return Err(ConsoleError::InputError(e.to_string())),
        }
    }
}
```

### Signal Handling

```rust
// Handle Ctrl+C gracefully
fn setup_signal_handlers() {
    signal::register(Signal::Interrupt, || {
        println("\nReceived interrupt signal. Cleaning up...");
        cleanup();
        exit(130);
    });
}

fn interactive_loop() -> Result<(), Error> {
    setup_signal_handlers();
    
    loop {
        match read_line() {
            Ok(line) => process_line(&line)?,
            Err(e) if e.kind() == ErrorKind::Interrupted => {
                println("\nUse Ctrl+D or 'exit' to quit.");
                continue;
            },
            Err(e) => return Err(e.into()),
        }
    }
}
```

## Performance Considerations

### Buffering

```rust
// Efficient bulk output
fn print_report(lines: &[string]) {
    // Buffer output for better performance
    let mut buffer = String::new();
    for line in lines {
        buffer.push_str(line);
        buffer.push('\n');
    }
    print("{}", buffer);
    flush_stdout().unwrap();
}

// Manual buffering control
fn interactive_output() {
    print("Processing");
    for i in 0..10 {
        print(".");
        flush_stdout().unwrap(); // Force immediate output
        thread::sleep(Duration::from_millis(100));
    }
    println(" Done!");
}

// Avoid excessive flushing
// Bad: flushes after every character
for ch in "Hello".chars() {
    print("{}", ch);
    flush_stdout().unwrap();
}

// Good: buffer and flush once
print("Hello");
flush_stdout().unwrap();
```

### Batch Operations

```rust
// Batch input reading
fn read_multiple_lines(count: int) -> Result<Vec<string>, Error> {
    let mut lines = Vec::with_capacity(count as usize);
    for i in 0..count {
        let line = prompt(&format!("Line {}: ", i + 1))?;
        lines.push(line);
    }
    Ok(lines)
}

// Efficient table printing
fn print_table(data: &[Vec<string>]) {
    // Calculate column widths first
    let mut widths = vec![0; data[0].len()];
    for row in data {
        for (i, cell) in row.iter().enumerate() {
            widths[i] = widths[i].max(cell.len());
        }
    }
    
    // Print with consistent formatting
    for row in data {
        for (i, cell) in row.iter().enumerate() {
            print("{:width$} ", cell, width = widths[i]);
        }
        println();
    }
}
```

## Common Patterns

### Configuration Input

```rust
fn read_config() -> Result<AppConfig, Error> {
    println("=== Application Configuration ===");
    
    let mut config = AppConfig::default();
    
    // Server settings
    println("\nServer Settings:");
    config.host = prompt("Host [localhost]: ")?
        .trim().to_string()
        .or_else(|| "localhost".to_string());
    
    config.port = prompt("Port [8080]: ")?
        .trim().parse()
        .unwrap_or(8080);
    
    // Database settings
    println("\nDatabase Settings:");
    config.db_url = prompt("Database URL: ")?
        .trim().to_string();
    
    // Optional settings
    if read_yes_no_default("Enable logging?", true)? {
        config.log_level = read_choice(&["ERROR", "WARN", "INFO", "DEBUG"])?;
    }
    
    Ok(config)
}

fn read_yes_no_default(prompt: &str, default: bool) -> Result<bool, Error> {
    let default_str = if default { "Y/n" } else { "y/N" };
    let input = prompt(&format!("{} [{}]: ", prompt, default_str))?;
    
    match input.trim().to_lowercase().as_str() {
        "y" | "yes" => Ok(true),
        "n" | "no" => Ok(false),
        "" => Ok(default),
        _ => {
            println("Please enter 'y' or 'n'");
            read_yes_no_default(prompt, default)
        }
    }
}
```

### Data Entry Forms

```rust
struct Contact {
    name: string,
    email: Option<string>,
    phone: Option<string>,
    address: Address,
}

struct Address {
    street: string,
    city: string,
    state: string,
    zip: string,
}

fn create_contact() -> Result<Contact, Error> {
    println("=== New Contact ===");
    
    let name = read_required("Name")?;
    let email = read_optional("Email")?;
    let phone = read_optional("Phone")?;
    
    println("\nAddress:");
    let address = Address {
        street: read_required("Street")?;
        city: read_required("City")?;
        state: read_required("State")?;
        zip: read_required("ZIP")?;
    };
    
    Ok(Contact { name, email, phone, address })
}

fn read_required(field: &str) -> Result<string, Error> {
    loop {
        let value = prompt(&format!("{}: ", field))?;
        let value = value.trim();
        if !value.is_empty() {
            return Ok(value.to_string());
        }
        println("{} is required", field);
    }
}

fn read_optional(field: &str) -> Result<Option<string>, Error> {
    let value = prompt(&format!("{} (optional): ", field))?;
    let value = value.trim();
    if value.is_empty() {
        Ok(None)
    } else {
        Ok(Some(value.to_string()))
    }
}
```

### Game Input

```rust
fn game_loop() -> Result<(), Error> {
    let mut game = Game::new();
    
    println("Welcome to the game! Use WASD to move, Q to quit.");
    game.draw();
    
    loop {
        let key = read_key()?;
        
        let action = match key {
            Key::Char('w') | Key::Arrow(Direction::Up) => Action::MoveUp,
            Key::Char('s') | Key::Arrow(Direction::Down) => Action::MoveDown,
            Key::Char('a') | Key::Arrow(Direction::Left) => Action::MoveLeft,
            Key::Char('d') | Key::Arrow(Direction::Right) => Action::MoveRight,
            Key::Char(' ') => Action::Select,
            Key::Char('q') | Key::Escape => break,
            _ => continue,
        };
        
        if game.process_action(action) {
            game.draw();
        }
        
        if game.is_over() {
            println("Game Over! Final score: {}", game.score());
            break;
        }
    }
    
    Ok(())
}
```

## JavaScript/Node.js Interop

When transpiled to JavaScript, console operations map to Node.js equivalents:

```rust
// Husk
println("Hello, World!");
eprintln("Error message");
let input = read_line()?;
```

Becomes:
```javascript
// JavaScript (Node.js)
console.log("Hello, World!");
console.error("Error message");

const readline = require('readline');
const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

const input = await new Promise(resolve => {
    rl.question('', resolve);
});
rl.close();
```

### API Mappings

| Husk Function | Node.js Equivalent |
|---------------|-------------------|
| `println()` | `console.log()` |
| `eprintln()` | `console.error()` |
| `print()` | `process.stdout.write()` |
| `read_line()` | `readline.question()` |
| `flush_stdout()` | `process.stdout.flush()` |
| `read_char()` | `process.stdin.read(1)` |

### Platform-specific Features

```rust
// Terminal size detection
if is_node_js() {
    let (width, height) = (process.stdout.columns, process.stdout.rows);
} else {
    let (width, height) = get_terminal_size()?;
}

// Color support detection
if supports_color() {
    use_ansi_colors();
} else {
    use_plain_text();
}
```

## Related Topics

- [File I/O](file-io.md) - Reading and writing files
- [String Operations](strings.md) - String manipulation
- [Error Handling](../language/error-handling.md) - Result and Option types
- [JavaScript Transpilation](../advanced/javascript-transpilation.md) - Console I/O in JS
- [Performance](../advanced/performance.md) - I/O optimization

---

*Console I/O is essential for user interaction. Husk provides safe, cross-platform operations with rich formatting and error handling capabilities.*