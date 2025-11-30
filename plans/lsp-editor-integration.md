# feat: Add LSP and Editor Integration to Husk

**Date**: 2025-11-30
**Status**: Draft
**Category**: Enhancement - Tooling

---

## Overview

Add Language Server Protocol (LSP) support and editor integration to the Husk programming language. This enables IDE features like real-time diagnostics, hover information, go-to-definition, and code completion for developers using Husk.

The implementation creates a standalone LSP server (`husk-lsp`) that works with any LSP-compatible editor, with specific integrations for **VSCode** (primary) and **Neovim** (secondary via documentation).

---

## Problem Statement / Motivation

Currently, Husk developers lack IDE tooling support:

1. **No real-time error feedback** - Errors only appear after running `huskc build`
2. **No code navigation** - Cannot jump to definitions or find references
3. **No type information on hover** - Must read source files to understand types
4. **No auto-completion** - Manual typing for all keywords, identifiers, and imports
5. **Limited adoption** - Modern developers expect IDE tooling; absence is a barrier

Husk already has a solid foundation for LSP support:
- Parser with error recovery and span tracking (`crates/husk-parser/src/lib.rs:36-46`)
- AST with position information on all nodes (`crates/husk-ast/src/lib.rs:6-15`)
- Semantic analysis with symbol resolution (`crates/husk-semantic/src/lib.rs:44-64`)
- Diagnostic infrastructure using codespan-reporting (`crates/husk-cli/src/diagnostic.rs`)

---

## Proposed Solution

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Editor (Client)                         │
│  ┌──────────────┐  ┌──────────────┐  ┌────────────────────┐ │
│  │   VSCode     │  │   Neovim     │  │  Other LSP Editors │ │
│  │  Extension   │  │  lspconfig   │  │    (Emacs, etc)    │ │
│  └──────┬───────┘  └──────┬───────┘  └─────────┬──────────┘ │
└─────────┼─────────────────┼────────────────────┼────────────┘
          │                 │                    │
          │  JSON-RPC over stdio                 │
          └─────────────────┼────────────────────┘
                            │
┌───────────────────────────┴─────────────────────────────────┐
│                    husk-lsp (Server)                         │
│  ┌──────────────────────────────────────────────────────┐   │
│  │              tower-lsp Framework                      │   │
│  │  - Initialize/Shutdown lifecycle                     │   │
│  │  - Document synchronization (incremental)            │   │
│  │  - Request/notification handling                     │   │
│  └──────────────────────────────────────────────────────┘   │
│                            │                                 │
│  ┌─────────────┬───────────┼────────────┬────────────────┐  │
│  │ husk-lexer  │ husk-parser  │ husk-semantic │ husk-ast │  │
│  │ (tokenize)  │ (parse+spans)│ (symbols+types)│ (nodes) │  │
│  └─────────────┴─────────────┴───────────────┴───────────┘  │
└──────────────────────────────────────────────────────────────┘
```

### Key Design Decisions

1. **tower-lsp for Rust LSP framework** - Most popular, async-first, well-maintained
2. **Standalone LSP server** - Works with any editor, not editor-specific
3. **Reuse existing compiler crates** - Lexer, parser, semantic analyzer shared
4. **Incremental text synchronization** - Only sync changes, not entire documents
5. **VSCode as primary target** - Best UX with extension bundling; Neovim via config docs

### Editor Strategy: VSCode First, Then Neovim

**Research Finding**: Since we're building a standalone LSP server, supporting multiple editors requires minimal extra work:

| Component | Effort | Notes |
|-----------|--------|-------|
| LSP Server (husk-lsp) | 90% | Core implementation |
| VSCode Extension | 5% | Thin wrapper that launches server |
| Neovim Documentation | 5% | Config example for nvim-lspconfig |

**VSCode Advantages**:
- Most popular editor (requires extension for best UX)
- Bundling LSP binary with extension simplifies installation
- Better onboarding experience for new users

**Neovim Advantages**:
- Native LSP client since v0.5 (no extension needed)
- Just needs `husk-lsp` in PATH + config snippet
- Already well-supported by users who can read docs

**Recommendation**: Build LSP server first, then VSCode extension (bundled binary), then Neovim config docs.

---

## Technical Approach

### Phase 1: Foundation - LSP Server with Diagnostics

**Goal**: Get real-time error feedback working in editors

**New Crate**: `crates/husk-lsp/`

```
crates/husk-lsp/
├── Cargo.toml
├── src/
│   ├── main.rs           # Entry point, stdio server
│   ├── backend.rs        # LanguageServer trait implementation
│   ├── document.rs       # Document state management
│   └── diagnostics.rs    # Convert parse/semantic errors to LSP
```

**Key Files to Create**:

1. **`crates/husk-lsp/Cargo.toml`**
```toml
[package]
name = "husk-lsp"
version = "0.1.0"
edition = "2024"

[[bin]]
name = "husk-lsp"
path = "src/main.rs"

[dependencies]
tower-lsp = "0.20"
tokio = { version = "1", features = ["full"] }
serde_json = "1.0"

# Reuse existing Husk crates
husk-lexer = { path = "../husk-lexer" }
husk-parser = { path = "../husk-parser" }
husk-semantic = { path = "../husk-semantic" }
husk-ast = { path = "../husk-ast" }
husk-types = { path = "../husk-types" }
```

2. **`crates/husk-lsp/src/main.rs`**
```rust
use tower_lsp::{LspService, Server};
mod backend;

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(backend::HuskBackend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
```

3. **`crates/husk-lsp/src/backend.rs`**
```rust
use tower_lsp::{Client, LanguageServer};
use tower_lsp::lsp_types::*;

pub struct HuskBackend {
    client: Client,
    documents: /* ... */,
}

#[tower_lsp::async_trait]
impl LanguageServer for HuskBackend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> { /* ... */ }
    async fn initialized(&self, _: InitializedParams) { /* ... */ }
    async fn shutdown(&self) -> Result<()> { Ok(()) }
    async fn did_open(&self, params: DidOpenTextDocumentParams) { /* parse + diagnostics */ }
    async fn did_change(&self, params: DidChangeTextDocumentParams) { /* reparse + diagnostics */ }
    async fn did_close(&self, params: DidCloseTextDocumentParams) { /* cleanup */ }
}
```

**Implementation Tasks**:

- [ ] Create `crates/husk-lsp/` crate structure
- [ ] Add `husk-lsp` to workspace members in `/Volumes/External/code-external/husk/Cargo.toml`
- [ ] Implement basic `LanguageServer` trait with initialization
- [ ] Implement document state management (store open files)
- [ ] Create span-to-LSP-position converter (byte offset → line:column)
- [ ] Integrate Husk parser for on-change parsing
- [ ] Convert `ParseError` to LSP `Diagnostic` format
- [ ] Integrate semantic analyzer
- [ ] Convert `SemanticError` to LSP `Diagnostic` format
- [ ] Publish diagnostics on document open/change
- [ ] Test with manual stdio invocation

**Acceptance Criteria**:
- [ ] `cargo build -p husk-lsp` produces working binary
- [ ] Server responds to `initialize` request correctly
- [ ] Opening `.hk` file shows parse errors as diagnostics
- [ ] Editing file updates diagnostics in real-time
- [ ] Closing file clears diagnostics

---

### Phase 2: VSCode Extension

**Goal**: Easy installation and setup for VSCode users

**Directory**: `editors/vscode/`

```
editors/vscode/
├── package.json          # Extension manifest
├── tsconfig.json         # TypeScript config
├── src/
│   └── extension.ts      # Language client setup
├── syntaxes/
│   └── husk.tmLanguage.json  # TextMate grammar
├── language-configuration.json  # Brackets, comments, etc.
└── server/               # Pre-built husk-lsp binary
```

**Key Files**:

1. **`editors/vscode/package.json`**
```json
{
  "name": "husk-vscode",
  "displayName": "Husk Language Support",
  "version": "0.1.0",
  "engines": { "vscode": "^1.75.0" },
  "categories": ["Programming Languages"],
  "activationEvents": ["onLanguage:husk"],
  "main": "./dist/extension.js",
  "contributes": {
    "languages": [{
      "id": "husk",
      "aliases": ["Husk"],
      "extensions": [".hk", ".husk"],
      "configuration": "./language-configuration.json"
    }],
    "grammars": [{
      "language": "husk",
      "scopeName": "source.husk",
      "path": "./syntaxes/husk.tmLanguage.json"
    }]
  },
  "dependencies": {
    "vscode-languageclient": "^9.0.1"
  }
}
```

2. **`editors/vscode/src/extension.ts`**
```typescript
import * as path from 'path';
import { ExtensionContext } from 'vscode';
import { LanguageClient, ServerOptions } from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    const serverPath = path.join(context.extensionPath, 'server', 'husk-lsp');
    const serverOptions: ServerOptions = { run: { command: serverPath }, debug: { command: serverPath } };
    const clientOptions = { documentSelector: [{ scheme: 'file', language: 'husk' }] };

    client = new LanguageClient('husk', 'Husk Language Server', serverOptions, clientOptions);
    client.start();
}

export function deactivate() { return client?.stop(); }
```

**Implementation Tasks**:

- [ ] Create `editors/vscode/` directory structure
- [ ] Write `package.json` with language registration
- [ ] Write `language-configuration.json` (brackets, comments)
- [ ] Create TextMate grammar for syntax highlighting
- [ ] Write TypeScript extension to launch LSP
- [ ] Set up build script to copy `husk-lsp` binary
- [ ] Test extension in VSCode

**Acceptance Criteria**:
- [ ] Installing extension enables `.hk` file recognition
- [ ] Syntax highlighting works for Husk code
- [ ] LSP server starts automatically on file open
- [ ] Diagnostics appear in Problems panel
- [ ] Red squiggles under errors in editor

---

### Phase 3: Enhanced LSP Features

**Goal**: Code navigation and information display

**Features** (priority order):

1. **Hover** - Show type information
2. **Go to Definition** - Navigate to symbol declarations
3. **Document Symbols** - Outline view of file
4. **Completion** - Keyword and identifier suggestions

**Implementation Tasks**:

- [ ] Implement `textDocument/hover` - return type info from semantic analysis
- [ ] Implement `textDocument/definition` - use symbol table for lookup
- [ ] Implement `textDocument/documentSymbol` - extract from AST items
- [ ] Implement `textDocument/completion` - keywords + scope symbols
- [ ] Add capability declarations in `initialize` response

**Acceptance Criteria**:
- [ ] Hovering over variable shows its type
- [ ] Hovering over function shows signature
- [ ] Ctrl+Click on symbol jumps to definition
- [ ] Outline view shows functions/structs/enums
- [ ] Typing shows completion suggestions

---

### Phase 4: Neovim Support

**Goal**: Document configuration for Neovim users

**Files**:

1. **`editors/neovim/README.md`**
```markdown
# Husk Language Support for Neovim

## Requirements
- Neovim 0.11+ (or 0.5+ with nvim-lspconfig)
- `husk-lsp` binary in your PATH

## Installation

1. Install husk-lsp:
   ```bash
   cargo install --path crates/husk-lsp
   ```

2. Add to your Neovim config:

### Neovim 0.11+ (Modern)
```lua
-- ~/.config/nvim/after/lsp/husk_lsp.lua
vim.lsp.config('husk_lsp', {
  cmd = { 'husk-lsp' },
  filetypes = { 'husk' },
  root_markers = { '.git' },
})
vim.lsp.enable('husk_lsp')
```

### With nvim-lspconfig
```lua
local configs = require('lspconfig.configs')
if not configs.husk_lsp then
  configs.husk_lsp = {
    default_config = {
      cmd = { 'husk-lsp' },
      filetypes = { 'husk' },
      root_dir = require('lspconfig.util').root_pattern('.git'),
    },
  }
end
require('lspconfig').husk_lsp.setup({})
```

3. Add filetype detection:
```lua
-- ~/.config/nvim/ftdetect/husk.lua
vim.filetype.add({ extension = { hk = 'husk' } })
```
```

**Implementation Tasks**:

- [ ] Create `editors/neovim/README.md` with setup instructions
- [ ] Test with actual Neovim installation
- [ ] Document known limitations

**Acceptance Criteria**:
- [ ] Following docs enables LSP in Neovim
- [ ] Diagnostics appear in Neovim
- [ ] Basic navigation features work

---

## Success Metrics

1. **Functionality**: All Phase 1-3 features working in VSCode
2. **Performance**: Diagnostics appear within 200ms of edit
3. **Reliability**: No crashes on malformed code (error recovery works)
4. **Adoption**: Extension installable from VSCode marketplace

---

## Dependencies & Risks

### Dependencies
- **tower-lsp** crate for LSP framework
- **tokio** for async runtime
- **vscode-languageclient** npm package for extension

### Risks

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Parser error recovery inadequate | Medium | High | Test with many malformed code samples |
| Performance issues on large files | Medium | Medium | Implement incremental parsing if needed |
| Binary distribution complexity | High | Medium | Start with bundled, consider download later |
| Cross-platform binary builds | Medium | Medium | Use GitHub Actions matrix builds |

---

## Future Considerations

After initial implementation, consider:

1. **Semantic Highlighting** - Enhanced syntax coloring based on symbol types
2. **Find References** - Show all usages of a symbol
3. **Rename Symbol** - Refactor across files
4. **Code Actions** - Auto-import, quick fixes
5. **Formatting** - Auto-format Husk code
6. **Workspace Symbols** - Search symbols across project
7. **Inlay Hints** - Show inferred types inline

---

## References & Research

### Internal References
- Parser implementation: `crates/husk-parser/src/lib.rs:36-46`
- AST span tracking: `crates/husk-ast/src/lib.rs:6-15`
- Symbol resolution: `crates/husk-semantic/src/lib.rs:44-64`
- Diagnostic system: `crates/husk-cli/src/diagnostic.rs`
- Existing roadmap: `roadmap.md:128-132` (Phase 6 mentions LSP)

### External References
- [tower-lsp GitHub](https://github.com/ebkalderon/tower-lsp)
- [LSP 3.17 Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
- [VSCode LSP Extension Guide](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide)
- [Neovim LSP Documentation](https://neovim.io/doc/user/lsp.html)
- [rust-lsp-lab (Educational Example)](https://github.com/mjul/rust-lsp-lab)
- [Resilient LL Parsing Tutorial](https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html)

### Related Work
- Husk roadmap Phase 6: `[ ] LSP implementation`
- No existing PRs for LSP

---

## Implementation Checklist

### Phase 1: LSP Server with Diagnostics
- [ ] Create `crates/husk-lsp/` crate
- [ ] Add to workspace in `Cargo.toml`
- [ ] Implement `LanguageServer` trait basics
- [ ] Document state management
- [ ] Span-to-position conversion utility
- [ ] Parse error → LSP diagnostic conversion
- [ ] Semantic error → LSP diagnostic conversion
- [ ] Test with manual stdio

### Phase 2: VSCode Extension
- [ ] Create `editors/vscode/` structure
- [ ] `package.json` manifest
- [ ] `language-configuration.json`
- [ ] TextMate grammar (`husk.tmLanguage.json`)
- [ ] TypeScript extension code
- [ ] Build script for bundling
- [ ] Test in VSCode

### Phase 3: Enhanced Features
- [ ] Hover provider
- [ ] Go-to-definition provider
- [ ] Document symbols provider
- [ ] Completion provider
- [ ] Capability declarations

### Phase 4: Neovim Support
- [ ] Setup documentation
- [ ] Test with Neovim
- [ ] Document limitations

---

## MVP Scope

**Minimum for v0.1.0**:
- LSP server with diagnostics (parse + semantic errors)
- VSCode extension with syntax highlighting
- Basic hover showing type information
- Go-to-definition for local symbols

**Not in v0.1.0**:
- Cross-file symbol resolution
- Completion
- Find references
- Rename
- Formatting
- Neovim (documentation-only, not tested)
