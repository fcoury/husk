# Husk Language Support for Neovim

This guide explains how to set up the Husk Language Server (husk-lsp) with Neovim for IDE features like diagnostics, hover, and go-to-definition.

## Requirements

- Neovim 0.8+ (0.11+ recommended for modern LSP config)
- `husk-lsp` binary in your PATH

## Installing husk-lsp

### From Source

```bash
# Clone the Husk repository
git clone https://github.com/husk-lang/husk.git
cd husk

# Build and install the LSP server
cargo install --path crates/husk-lsp
```

### Verify Installation

```bash
husk-lsp --help
# Should show version info
```

## Configuration

### Neovim 0.11+ (Modern Method)

Create the following files in your Neovim configuration:

**1. Filetype Detection**

Create `~/.config/nvim/ftdetect/husk.lua`:

```lua
vim.filetype.add({
    extension = {
        hk = 'husk',
        husk = 'husk',
    },
})
```

**2. LSP Configuration**

Create `~/.config/nvim/after/lsp/husk_lsp.lua`:

```lua
vim.lsp.config('husk_lsp', {
    cmd = { 'husk-lsp' },
    filetypes = { 'husk' },
    root_markers = { '.git', 'Cargo.toml' },
    settings = {
        husk = {
            -- Future settings go here
        }
    },
})

vim.lsp.enable('husk_lsp')
```

### With nvim-lspconfig (Legacy Method)

If you're using nvim-lspconfig, add this to your configuration:

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

-- Define the Husk LSP server
if not configs.husk_lsp then
    configs.husk_lsp = {
        default_config = {
            cmd = { 'husk-lsp' },
            filetypes = { 'husk' },
            root_dir = lspconfig.util.root_pattern('.git', 'Cargo.toml'),
            settings = {},
        },
    }
end

-- Set up the server
lspconfig.husk_lsp.setup({
    on_attach = function(client, bufnr)
        -- Add your on_attach configuration here
        -- Example: Enable completion
        vim.bo[bufnr].omnifunc = 'v:lua.vim.lsp.omnifunc'

        -- Keymaps
        local opts = { buffer = bufnr }
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
        vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
    end,
})
```

### Filetype Detection (for nvim-lspconfig)

Add this to your init.lua:

```lua
vim.filetype.add({
    extension = {
        hk = 'husk',
        husk = 'husk',
    },
})
```

## Syntax Highlighting (Optional)

For basic syntax highlighting, create `~/.config/nvim/after/syntax/husk.vim`:

```vim
" Husk syntax highlighting

if exists("b:current_syntax")
    finish
endif

" Keywords
syn keyword huskKeyword fn let mut struct enum type trait impl
syn keyword huskKeyword if else while match return break continue for
syn keyword huskKeyword pub use mod extern as
syn keyword huskBoolean true false
syn keyword huskSelf self Self

" Types
syn keyword huskType i32 i64 u32 u64 bool String str

" Comments
syn region huskComment start="//" end="$"

" Strings
syn region huskString start='"' end='"' contains=huskEscape
syn match huskEscape contained "\\."

" Numbers
syn match huskNumber "\<[0-9]\+\>"

" Function definitions
syn match huskFunction "\<fn\s\+\zs[a-zA-Z_][a-zA-Z0-9_]*\ze\s*("

" Type names (PascalCase)
syn match huskTypeName "\<[A-Z][a-zA-Z0-9_]*\>"

" Highlighting
hi def link huskKeyword Keyword
hi def link huskBoolean Boolean
hi def link huskSelf Keyword
hi def link huskType Type
hi def link huskComment Comment
hi def link huskString String
hi def link huskEscape SpecialChar
hi def link huskNumber Number
hi def link huskFunction Function
hi def link huskTypeName Type

let b:current_syntax = "husk"
```

## Features

The Husk LSP currently supports:

| Feature | Status | Description |
|---------|--------|-------------|
| Diagnostics | Working | Real-time error and warning display |
| Hover | Basic | Shows identifier name |
| Go to Definition | Basic | Jump to function/struct/enum definitions |
| Document Symbols | Working | File outline with functions, structs, enums |

## Troubleshooting

### LSP Not Starting

1. Verify husk-lsp is installed:
   ```bash
   which husk-lsp
   ```

2. Check Neovim logs:
   ```vim
   :LspLog
   ```

3. Verify LSP is attached:
   ```vim
   :LspInfo
   ```

### No Diagnostics Showing

1. Ensure the file has `.hk` extension
2. Check that filetype is correctly detected:
   ```vim
   :set filetype?
   ```
   Should show `filetype=husk`

### Health Check

Run the built-in health check:
```vim
:checkhealth vim.lsp
```

## Known Limitations

- Cross-file symbol resolution is not yet implemented
- Completion is not yet available
- Rename refactoring is not yet available

## Getting Help

- [Husk GitHub Repository](https://github.com/husk-lang/husk)
- [Open an Issue](https://github.com/husk-lang/husk/issues)
