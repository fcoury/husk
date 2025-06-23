# Husk Language Support for Neovim

Syntax highlighting for the Husk programming language in Neovim.

## Installation

### Using a Plugin Manager

#### vim-plug

```vim
Plug 'fcoury/husk-highlight', { 'rtp': 'nvim' }
```

#### packer.nvim

```lua
use { 'fcoury/husk-highlight', rtp = 'nvim' }
```

#### lazy.nvim

```lua
{
  'fcoury/husk-highlight',
  config = function()
    vim.opt.runtimepath:append(vim.fn.stdpath('data') .. '/lazy/husk-highlight/nvim')
  end
}
```

### Manual Installation

1. Clone this repository
2. Copy the contents of the `nvim` directory to your Neovim configuration:

```bash
cp -r nvim/* ~/.config/nvim/
```

Or if you prefer to keep it separate:

```bash
mkdir -p ~/.local/share/nvim/site/pack/husk/start/husk-vim
cp -r nvim/* ~/.local/share/nvim/site/pack/husk/start/husk-vim/
```

## Features

- Syntax highlighting for:
  - Keywords (`let`, `fn`, `struct`, `enum`, `if`, `match`, `for`, `while`, `use`, etc.)
  - Types (`int`, `float`, `bool`, `string`)
  - Comments (line and block)
  - Strings and numbers
  - Functions and methods
  - Operators
  - Use statements with `local::` support
- File type detection for `.husk` and `.hk` files
- Automatic indentation
- Comment formatting support

## Usage

The plugin will automatically activate for files with `.husk` or `.hk` extensions.

