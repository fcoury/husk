# Installation Guide

This guide will help you install Husk on your system and verify that everything is working correctly.

## Table of Contents

- [System Requirements](#system-requirements)
- [Installation Methods](#installation-methods)
  - [From Source](#from-source)
  - [Using Cargo](#using-cargo)
  - [Pre-built Binaries](#pre-built-binaries)
- [Verifying Installation](#verifying-installation)
- [Updating Husk](#updating-husk)
- [Troubleshooting](#troubleshooting)
- [Next Steps](#next-steps)

## System Requirements

Husk requires:
- **Operating System**: Linux, macOS, or Windows
- **Rust**: Version 1.70 or higher (for building from source)
- **Node.js**: Version 16 or higher (for JavaScript transpilation)
- **Git**: For cloning the repository

### Supported Platforms

| Platform | Architecture | Status |
|----------|-------------|---------|
| Linux | x86_64 | ✅ Fully supported |
| macOS | x86_64, ARM64 | ✅ Fully supported |
| Windows | x86_64 | ✅ Fully supported |

## Installation Methods

### From Source

Building from source gives you the latest development version and is currently the primary installation method.

#### 1. Install Prerequisites

**Install Rust:**
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env
```

**Install Node.js:**
- **Ubuntu/Debian**: `sudo apt install nodejs npm`
- **macOS**: `brew install node`
- **Windows**: Download from [nodejs.org](https://nodejs.org)

#### 2. Clone the Repository

```bash
git clone https://github.com/username/husk.git
cd husk
```

#### 3. Build and Install

```bash
# Build the compiler
cargo build --release

# Install to cargo bin directory
cargo install --path .
```

The `husk` command will now be available in your terminal.

### Using Cargo (Coming Soon)

Once Husk is published to crates.io, you'll be able to install it directly:

```bash
cargo install husk
```

### Pre-built Binaries (Coming Soon)

Pre-built binaries will be available for major platforms:

```bash
# Linux/macOS
curl -sSL https://husk-lang.org/install.sh | bash

# Windows
irm https://husk-lang.org/install.ps1 | iex
```

## Verifying Installation

After installation, verify that Husk is working correctly:

### 1. Check Version

```bash
husk --version
```

Expected output:
```
husk 0.1.0
```

### 2. Run a Simple Program

Create a test file `hello.hk`:

```rust
fn main() {
    println!("Hello from Husk!");
}
```

Run it:

```bash
husk run hello.hk
```

Expected output:
```
Hello from Husk!
```

### 3. Check JavaScript Transpilation

Husk can transpile your code to JavaScript for web and Node.js deployment:

```bash
husk build hello.hk --target js
```

This creates `hello.js` that you can run with Node.js:

```bash
node hello.js
```

Expected output:
```
Hello from Husk!
```

For more details on JavaScript transpilation, see the [JavaScript Transpilation Guide](../advanced/javascript-transpilation.md).

## Environment Setup

### Setting PATH

If `husk` is not found after installation, add Cargo's bin directory to your PATH:

**Linux/macOS:**
```bash
echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

**Windows (PowerShell):**
```powershell
$env:Path += ";$env:USERPROFILE\.cargo\bin"
[Environment]::SetEnvironmentVariable("Path", $env:Path, [EnvironmentVariableTarget]::User)
```

### Editor Integration

For the best development experience, install Husk support for your editor:

- **VSCode**: Install the "Husk Language Support" extension
- **Neovim**: Add the Husk LSP configuration
- **Sublime Text**: Install the Husk syntax package

See [Editor Setup](editor-setup.md) for detailed configuration instructions.

## Updating Husk

### From Source

```bash
cd /path/to/husk
git pull origin main
cargo install --path . --force
```

### Using Cargo

```bash
cargo install husk --force
```

## Troubleshooting

### Common Issues

#### "command not found: husk"

**Solution**: Ensure `~/.cargo/bin` is in your PATH:
```bash
echo $PATH | grep cargo
```

If not present, follow the [Setting PATH](#setting-path) instructions.

#### Build Errors

**"error: linker 'cc' not found"**

Install build essentials:
- **Ubuntu/Debian**: `sudo apt install build-essential`
- **macOS**: `xcode-select --install`
- **Windows**: Install Visual Studio Build Tools

#### Node.js Not Found

**Solution**: Ensure Node.js is installed and in PATH:
```bash
node --version
npm --version
```

### Platform-Specific Issues

#### macOS

If you encounter SSL certificate errors:
```bash
export RUST_BACKTRACE=1
cargo build --release
```

#### Windows

For long path issues, enable long path support:
```powershell
New-ItemProperty -Path "HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem" -Name "LongPathsEnabled" -Value 1 -PropertyType DWORD -Force
```

### Getting Help

If you encounter issues not covered here:

1. Check the [GitHub Issues](https://github.com/username/husk/issues)
2. Ask on the [Community Forum](https://forum.husk-lang.org)
3. Join the [Discord Server](https://discord.gg/husk)

## Next Steps

Now that you have Husk installed:

1. **[Hello World Tutorial](hello-world.md)** - Write your first Husk program
2. **[Quick Start Guide](quickstart.md)** - Learn essential features
3. **[Language Features](../LANGUAGE_FEATURES.md)** - Explore the language
4. **[Standard Library](../STANDARD_LIBRARY.md)** - Discover built-in functionality

## Version Compatibility

| Husk Version | Rust Version | Node.js Version |
|--------------|--------------|-----------------|
| 0.1.x | 1.70+ | 16+ |
| 0.2.x (planned) | 1.75+ | 18+ |

---

*If you encounter any issues with this installation guide, please report them on our [issue tracker](https://github.com/username/husk/issues).*