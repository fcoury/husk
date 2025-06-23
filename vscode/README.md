# Husk Language Support for VSCode

Syntax highlighting and language support for the Husk programming language.

## Installation

### From VSIX file

1. Download the `.vsix` file from the releases
2. In VSCode, open the Command Palette (`Cmd+Shift+P` or `Ctrl+Shift+P`)
3. Run "Extensions: Install from VSIX..."
4. Select the downloaded `.vsix` file

### From Source

1. Clone this repository
2. Copy the `vscode` directory to your VSCode extensions folder:
   - **Windows**: `%USERPROFILE%\.vscode\extensions\husk-language`
   - **macOS/Linux**: `~/.vscode/extensions/husk-language`
3. Restart VSCode

### Building from Source

To create a `.vsix` package:

```bash
cd vscode
npm install -g vsce
vsce package
```

## Features

- Syntax highlighting for:
  - Keywords (`let`, `fn`, `struct`, `enum`, `if`, `match`, `for`, `while`, `use`, etc.)
  - Types (`int`, `float`, `bool`, `string`)
  - Comments (line and block)
  - Strings and numbers
  - Functions and built-in functions
  - Operators
  - Use statements with `local::` support
- File association for `.husk` and `.hk` files
- Bracket matching and auto-closing
- Comment toggling (`Cmd+/` or `Ctrl+/`)
- Code folding support

## Usage

The extension will automatically activate for files with `.husk` or `.hk` extensions.

## Language Configuration

The extension provides:
- Auto-closing pairs for brackets, parentheses, and quotes
- Indentation rules for blocks and match expressions
- Comment shortcuts
- Folding markers