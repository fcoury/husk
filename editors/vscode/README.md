# Husk Language Support for VS Code

This extension provides language support for the [Husk programming language](https://github.com/husk-lang/husk).

## Features

- **Syntax Highlighting**: Full TextMate grammar for `.hk` and `.husk` files
- **Real-time Diagnostics**: Parse and semantic errors displayed as you type
- **Hover Information**: Shows identifier information on hover
- **Go to Definition**: Jump to function, struct, enum, and trait definitions
- **Document Symbols**: File outline showing all defined items

## Requirements

The extension includes a bundled Husk Language Server. Alternatively, you can:

1. Install husk-lsp manually:
   ```bash
   cargo install --path crates/husk-lsp
   ```

2. Configure the path in settings if needed:
   - Open VS Code Settings
   - Search for "Husk"
   - Set `husk.serverPath` to your husk-lsp binary location

## Extension Settings

| Setting | Description | Default |
|---------|-------------|---------|
| `husk.serverPath` | Path to husk-lsp binary | Bundled server |
| `husk.trace.server` | LSP communication logging level | `off` |

## Development

### Building the Extension

```bash
cd editors/vscode
npm install
npm run compile
```

### Building the LSP Server

```bash
cargo build --release -p husk-lsp
cp target/release/husk-lsp editors/vscode/server/
```

### Packaging

```bash
npm run package
```

## Known Issues

- Cross-file symbol resolution not yet implemented
- Completion not yet available

## Release Notes

### 0.1.0

Initial release:
- Basic syntax highlighting
- Diagnostics from parse and semantic errors
- Hover and go-to-definition support
- Document symbols
