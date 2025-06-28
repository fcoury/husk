# Editor Setup

Configure your favorite editor for an optimal Husk development experience. This guide covers setup instructions for popular editors and IDEs.

## Table of Contents

- [Visual Studio Code](#visual-studio-code)
- [Neovim](#neovim)
- [Sublime Text](#sublime-text)
- [IntelliJ IDEA](#intellij-idea)
- [Emacs](#emacs)
- [Language Server Protocol](#language-server-protocol)
- [Syntax Highlighting](#syntax-highlighting)
- [Troubleshooting](#troubleshooting)

## Visual Studio Code

VSCode offers the best Husk development experience with full language server support.

### Installation

1. **Install the Husk Extension**
   - Open VSCode
   - Go to Extensions (Ctrl+Shift+X / Cmd+Shift+X)
   - Search for "Husk Language Support"
   - Click Install

2. **Alternative: Manual Installation**
   ```bash
   code --install-extension husk-lang.husk-vscode
   ```

### Features

- ✅ Syntax highlighting
- ✅ Code completion
- ✅ Error diagnostics
- ✅ Go to definition
- ✅ Hover documentation
- ✅ Code formatting
- ✅ Refactoring support

### Configuration

Add to your `settings.json`:

```json
{
  "husk.lsp.enabled": true,
  "husk.formatting.enable": true,
  "husk.formatting.onSave": true,
  "husk.linting.enable": true,
  "husk.completion.autoImport": true,
  "[husk]": {
    "editor.defaultFormatter": "husk-lang.husk-vscode",
    "editor.formatOnSave": true,
    "editor.tabSize": 4,
    "editor.insertSpaces": true
  }
}
```

### Recommended Extensions

- **Error Lens**: Inline error messages
- **Bracket Pair Colorizer**: Better bracket matching
- **GitLens**: Git integration

## Neovim

Neovim provides excellent Husk support through LSP integration.

### Using nvim-lspconfig

1. **Install the Husk LSP**
   ```bash
   cargo install husk-lsp
   ```

2. **Configure LSP**
   
   Add to your `init.lua`:
   ```lua
   local lspconfig = require('lspconfig')
   
   -- Husk LSP configuration
   lspconfig.husk_lsp.setup{
     cmd = {"husk-lsp"},
     filetypes = {"husk", "hk"},
     root_dir = lspconfig.util.root_pattern("Husk.toml", ".git"),
     settings = {
       husk = {
         checkOnSave = {
           command = "check"
         },
         completion = {
           autoImport = true
         }
       }
     }
   }
   ```

3. **Install Syntax Highlighting**
   
   Using Treesitter:
   ```lua
   require'nvim-treesitter.configs'.setup {
     ensure_installed = { "husk" },
     highlight = {
       enable = true,
     },
   }
   ```

### Using CoC.nvim

1. **Install CoC**
   ```vim
   Plug 'neoclide/coc.nvim', {'branch': 'release'}
   ```

2. **Configure for Husk**
   
   Add to `coc-settings.json`:
   ```json
   {
     "languageserver": {
       "husk": {
         "command": "husk-lsp",
         "filetypes": ["husk", "hk"],
         "rootPatterns": ["Husk.toml", ".git"]
       }
     }
   }
   ```

### Key Mappings

```vim
" Go to definition
nnoremap <silent> gd <Plug>(coc-definition)
" Show hover documentation
nnoremap <silent> K :call CocActionAsync('doHover')<CR>
" Rename symbol
nmap <leader>rn <Plug>(coc-rename)
" Format code
nmap <leader>f :call CocAction('format')<CR>
```

## Sublime Text

Sublime Text supports Husk through a dedicated package.

### Installation

1. **Via Package Control**
   - Open Command Palette (Ctrl+Shift+P / Cmd+Shift+P)
   - Type "Package Control: Install Package"
   - Search for "Husk"
   - Press Enter to install

2. **Manual Installation**
   ```bash
   cd ~/Library/Application\ Support/Sublime\ Text/Packages
   git clone https://github.com/husk-lang/sublime-husk Husk
   ```

### Configuration

Create `Husk.sublime-settings`:

```json
{
  "tab_size": 4,
  "translate_tabs_to_spaces": true,
  "rulers": [100],
  "ensure_newline_at_eof_on_save": true,
  "trim_trailing_white_space_on_save": true
}
```

### LSP Support

Install LSP package and configure:

```json
{
  "clients": {
    "husk-lsp": {
      "enabled": true,
      "command": ["husk-lsp"],
      "selector": "source.husk",
      "settings": {}
    }
  }
}
```

## IntelliJ IDEA

IntelliJ IDEA supports Husk through a plugin.

### Installation

1. **From Marketplace**
   - Go to Settings → Plugins
   - Search for "Husk Language"
   - Install and restart

2. **Configure SDK**
   - Go to Project Structure (Ctrl+Alt+Shift+S)
   - Add new SDK → Husk SDK
   - Point to your Husk installation

### Features

- Syntax highlighting
- Code completion
- Refactoring
- Debugging support
- Project templates

## Emacs

Emacs supports Husk through husk-mode and LSP.

### Installation

1. **Install husk-mode**
   ```elisp
   (use-package husk-mode
     :ensure t
     :mode (("\\.husk\\'" . husk-mode)
            ("\\.hk\\'" . husk-mode)))
   ```

2. **Configure LSP**
   ```elisp
   (use-package lsp-mode
     :ensure t
     :hook (husk-mode . lsp)
     :commands lsp)
   
   (use-package lsp-ui
     :ensure t
     :commands lsp-ui-mode)
   
   ;; Register Husk LSP
   (add-to-list 'lsp-language-id-configuration '(husk-mode . "husk"))
   (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "husk-lsp")
                     :major-modes '(husk-mode)
                     :server-id 'husk-lsp))
   ```

### Key Bindings

```elisp
(define-key husk-mode-map (kbd "C-c C-f") 'husk-format-buffer)
(define-key husk-mode-map (kbd "C-c C-r") 'husk-run)
(define-key husk-mode-map (kbd "C-c C-t") 'husk-test)
```

## Language Server Protocol

The Husk Language Server provides IDE features for any LSP-compatible editor.

### Installation

```bash
cargo install husk-lsp
```

### Capabilities

- **Text Document Synchronization**
- **Completion** - Context-aware code completion
- **Hover** - Type information and documentation
- **Signature Help** - Function parameter hints
- **Go to Definition** - Navigate to declarations
- **Find References** - Find all usages
- **Document Symbols** - File outline
- **Workspace Symbols** - Project-wide search
- **Code Actions** - Quick fixes and refactorings
- **Formatting** - Code formatting
- **Rename** - Rename symbols project-wide

### Generic LSP Configuration

```json
{
  "command": ["husk-lsp"],
  "filetypes": ["husk", "hk"],
  "rootPatterns": ["Husk.toml", ".git"],
  "initializationOptions": {
    "checkOnSave": true,
    "autoImport": true
  }
}
```

## Syntax Highlighting

### TextMate Grammar

For editors supporting TextMate grammars, install:

```bash
curl -o husk.tmLanguage.json \
  https://raw.githubusercontent.com/husk-lang/husk-textmate/main/syntaxes/husk.tmLanguage.json
```

### Tree-sitter Grammar

For Tree-sitter support:

```bash
git clone https://github.com/husk-lang/tree-sitter-husk
cd tree-sitter-husk
npm install
npm run build
```

### Highlighting Groups

Key syntax groups for theme authors:

- `keyword` - fn, let, mut, if, else, match
- `type` - int, string, bool, float
- `function` - Function names
- `string` - String literals
- `number` - Numeric literals
- `comment` - Comments
- `operator` - +, -, *, /, etc.

## Troubleshooting

### Common Issues

#### LSP Not Starting

**Check installation:**
```bash
which husk-lsp
husk-lsp --version
```

**View logs:**
- VSCode: Output → Husk Language Server
- Neovim: `:LspLog`
- Sublime: View → Show Console

#### No Syntax Highlighting

1. Verify file extension is `.husk` or `.hk`
2. Check language mode is set to "Husk"
3. Reinstall syntax package

#### Slow Performance

1. Exclude large directories in project settings
2. Disable unused LSP features
3. Increase LSP server memory limit

### Debug Commands

Test LSP connection:
```bash
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | husk-lsp
```

## Editor-Specific Tips

### VSCode

- Use workspace settings for project-specific configuration
- Install "Husk Snippets" for code templates
- Enable bracket pair colorization

### Neovim

- Use `vim.lsp.set_log_level("debug")` for troubleshooting
- Configure Telescope for fuzzy finding Husk symbols
- Set up DAP for debugging support

### Sublime Text

- Use project-specific settings for team consistency
- Configure build systems for quick compilation
- Install SublimeLinter-husk for additional linting

## Contributing

Help improve editor support:

1. Report issues to editor-specific repositories
2. Contribute syntax highlighting improvements
3. Add snippets and templates
4. Improve documentation

## Resources

- [Husk LSP Repository](https://github.com/husk-lang/husk-lsp)
- [VSCode Extension](https://github.com/husk-lang/vscode-husk)
- [Tree-sitter Grammar](https://github.com/husk-lang/tree-sitter-husk)
- [TextMate Grammar](https://github.com/husk-lang/husk-textmate)

---

*For editor-specific issues, please file bugs in the respective extension repositories.*