# Husk CLI

The Husk Command Line Interface (CLI) is the primary tool for compiling, running, and managing Husk projects. This comprehensive guide covers all CLI commands, options, and workflows.

## Table of Contents

- [Overview](#overview)
- [Installation](#installation)
- [Basic Usage](#basic-usage)
- [Project Commands](#project-commands)
  - [New Project](#new-project)
  - [Build](#build)
  - [Run](#run)
  - [Test](#test)
  - [Check](#check)
- [Compilation Options](#compilation-options)
- [Target Platforms](#target-platforms)
- [Package Management](#package-management)
- [Development Tools](#development-tools)
- [Configuration](#configuration)
- [Environment Variables](#environment-variables)
- [Debugging Commands](#debugging-commands)
- [Performance Analysis](#performance-analysis)
- [Plugin System](#plugin-system)
- [CI/CD Integration](#cicd-integration)
- [Troubleshooting](#troubleshooting)
- [Related Topics](#related-topics)

## Overview

The Husk CLI provides:
- **Project management** - Create, build, and run projects
- **Cross-compilation** - Target multiple platforms
- **Package management** - Dependency handling
- **Development tools** - Testing, linting, formatting
- **Performance analysis** - Profiling and optimization

### Key Features

1. **Fast compilation** - Incremental builds and caching
2. **Multiple targets** - Native, WebAssembly, JavaScript
3. **Integrated tooling** - Built-in formatter, linter, tester
4. **Package ecosystem** - Dependency management
5. **Cross-platform** - Works on Windows, macOS, Linux

## Installation

### System Requirements

```bash
# Minimum requirements
- 64-bit processor
- 4GB RAM (8GB recommended)
- 2GB disk space
- Internet connection for packages

# Supported platforms
- Windows 10+ (x64)
- macOS 10.15+ (x64, ARM64)
- Linux (x64, ARM64)
- FreeBSD (x64)
```

### Install Methods

```bash
# Install via installer script (recommended)
curl -sSf https://husk-lang.org/install.sh | sh

# Install via package manager
# Windows (Chocolatey)
choco install husk

# macOS (Homebrew)
brew install husk

# Linux (Snap)
sudo snap install husk

# Arch Linux (AUR)
yay -S husk

# Manual installation
# Download from https://github.com/husk-lang/husk/releases
# Extract and add to PATH
```

### Verify Installation

```bash
# Check version
husk --version

# Show installation info
husk --info

# Test compilation
echo 'fn main() { println("Hello, World!"); }' > hello.hk
husk run hello.hk
```

## Basic Usage

### Command Structure

```bash
# General syntax
husk [COMMAND] [OPTIONS] [ARGUMENTS]

# Get help
husk --help
husk COMMAND --help

# Common patterns
husk build                    # Build current project
husk run src/main.hk         # Run specific file
husk test                    # Run tests
husk new my-project          # Create new project
husk add package-name        # Add dependency
```

### File Extensions

```bash
# Husk source files
*.hk       # Primary extension
*.husk     # Alternative extension

# Project files
Husk.toml  # Project configuration
.huskrc    # Local configuration
```

## Project Commands

### New Project

Create new Husk projects with various templates:

```bash
# Basic application
husk new my-app
husk new my-app --template app

# Library project
husk new my-lib --template lib

# Web application
husk new my-web --template web

# CLI application
husk new my-cli --template cli

# Game project
husk new my-game --template game

# Custom template
husk new my-project --template https://github.com/user/template.git

# Initialize in existing directory
cd existing-dir
husk init
husk init --template lib
```

### Build

Compile Husk projects:

```bash
# Build current project
husk build

# Build specific file
husk build src/main.hk

# Build with release optimizations
husk build --release

# Build for specific target
husk build --target wasm32
husk build --target js
husk build --target native

# Build with custom output
husk build --output ./dist/app
husk build --output-dir ./build

# Incremental build (default)
husk build --incremental

# Clean build (rebuild everything)
husk build --clean

# Parallel build
husk build --jobs 4
husk build -j $(nproc)

# Verbose output
husk build --verbose
husk build -v

# Watch mode (rebuild on changes)
husk build --watch
husk build -w

# Build documentation
husk build --docs
```

### Run

Execute Husk programs:

```bash
# Run current project
husk run

# Run specific file
husk run src/main.hk
husk run examples/hello.hk

# Run with arguments
husk run -- arg1 arg2 arg3
husk run src/main.hk -- --config config.toml

# Run in release mode
husk run --release

# Run with environment variables
husk run --env DEBUG=1 --env LOG_LEVEL=info

# Run with different target
husk run --target js
husk run --target wasm32

# Run and capture output
husk run > output.txt 2>&1

# Run with profiling
husk run --profile
husk run --profile cpu
husk run --profile memory

# Run in background
husk run --daemon
husk run --background
```

### Test

Run test suites:

```bash
# Run all tests
husk test

# Run specific test file
husk test tests/unit_tests.hk

# Run tests matching pattern
husk test --filter "math_*"
husk test --filter "integration"

# Run tests in parallel
husk test --parallel
husk test --jobs 4

# Run tests with coverage
husk test --coverage
husk test --coverage --output-dir coverage/

# Run tests in specific mode
husk test --release
husk test --debug

# Run benchmarks
husk test --bench
husk test --bench --filter "sort_*"

# Run tests with timeout
husk test --timeout 30s

# Continuous testing (watch mode)
husk test --watch

# Generate test report
husk test --report junit
husk test --report html --output test-report.html

# Run doctests
husk test --doc
```

### Check

Validate code without building:

```bash
# Check syntax and types
husk check

# Check specific files
husk check src/lib.hk
husk check src/**/*.hk

# Check with all warnings
husk check --warnings all

# Check formatting
husk check --format

# Check style guidelines
husk check --style

# Check documentation
husk check --docs

# Check for unused code
husk check --unused

# Check dependencies
husk check --deps

# Fix issues automatically
husk check --fix
husk check --format --fix
```

## Compilation Options

### Optimization Levels

```bash
# Debug mode (default)
husk build --debug
husk build -g

# Release mode (optimized)
husk build --release
husk build -O

# Optimization levels
husk build -O0  # No optimization
husk build -O1  # Basic optimization
husk build -O2  # Standard optimization
husk build -O3  # Aggressive optimization
husk build -Os  # Size optimization
husk build -Oz  # Maximum size optimization

# Link-time optimization
husk build --lto
husk build --lto thin
husk build --lto full
```

### Debug Information

```bash
# Include debug symbols
husk build --debug-info
husk build -g

# Debug info level
husk build --debug-info 0  # None
husk build --debug-info 1  # Line tables only
husk build --debug-info 2  # Full debug info

# Strip debug symbols
husk build --strip
husk build --strip-debug
husk build --strip-all
```

### Code Generation

```bash
# Target CPU
husk build --target-cpu native
husk build --target-cpu x86-64
husk build --target-cpu arm64

# Target features
husk build --target-feature +avx2
husk build --target-feature +sse4.2,+popcnt

# Code model
husk build --code-model small
husk build --code-model medium
husk build --code-model large

# Position independent code
husk build --pic
husk build --no-pic

# Panic strategy
husk build --panic abort
husk build --panic unwind
```

## Target Platforms

### Native Targets

```bash
# Current platform (default)
husk build --target native

# Specific architectures
husk build --target x86_64-unknown-linux-gnu
husk build --target aarch64-unknown-linux-gnu
husk build --target x86_64-pc-windows-msvc
husk build --target x86_64-apple-darwin
husk build --target aarch64-apple-darwin

# Cross-compilation
husk build --target arm-unknown-linux-gnueabihf
husk build --target riscv64gc-unknown-linux-gnu

# List available targets
husk targets list
husk targets list --installed
```

### WebAssembly Targets

```bash
# WebAssembly System Interface
husk build --target wasm32-wasi

# Browser WebAssembly
husk build --target wasm32-unknown-unknown

# WebAssembly with specific features
husk build --target wasm32-wasi --features bulk-memory
husk build --target wasm32-wasi --features simd

# Optimize for size (important for WASM)
husk build --target wasm32-wasi -Oz --strip
```

### JavaScript Targets

```bash
# Node.js target
husk build --target js-node

# Browser target
husk build --target js-browser

# ES module output
husk build --target js-esm

# CommonJS output
husk build --target js-cjs

# TypeScript declarations
husk build --target js --emit-types
```

## Package Management

### Adding Dependencies

```bash
# Add package from registry
husk add serde
husk add http --version "^1.0"

# Add development dependency
husk add test-utils --dev

# Add build dependency
husk add build-scripts --build

# Add optional dependency
husk add graphics --optional

# Add from Git repository
husk add my-package --git https://github.com/user/repo.git
husk add my-package --git https://github.com/user/repo.git --branch main
husk add my-package --git https://github.com/user/repo.git --tag v1.0.0

# Add local dependency
husk add my-lib --path ../my-lib

# Add with features
husk add database --features "postgresql,migrations"
```

### Managing Dependencies

```bash
# Update dependencies
husk update
husk update package-name

# Remove dependencies
husk remove package-name
husk rm package-name

# List dependencies
husk list
husk list --tree
husk list --installed

# Show dependency information
husk info package-name
husk show package-name

# Check for outdated packages
husk outdated

# Audit dependencies for security
husk audit
husk audit --fix
```

### Package Registry

```bash
# Search packages
husk search keyword
husk search --limit 20 web

# Show package details
husk info package-name
husk info package-name --version 1.0.0

# Login to registry
husk login
husk login --registry custom-registry

# Publish package
husk publish
husk publish --dry-run
husk publish --registry custom-registry

# Yank/unyank versions
husk yank package-name --version 1.0.0
husk unyank package-name --version 1.0.0
```

## Development Tools

### Code Formatting

```bash
# Format current project
husk fmt

# Format specific files
husk fmt src/main.hk
husk fmt src/**/*.hk

# Check formatting without changing
husk fmt --check

# Format and show diff
husk fmt --diff

# Format configuration
husk fmt --config .huskfmt.toml

# Custom formatting options
husk fmt --line-width 100
husk fmt --tab-size 4
husk fmt --use-tabs
```

### Linting

```bash
# Lint current project
husk lint

# Lint specific files
husk lint src/lib.hk

# Lint with specific rules
husk lint --rules performance,style
husk lint --rules all

# Fix linting issues
husk lint --fix

# Lint configuration
husk lint --config .husklint.toml

# Show lint rule documentation
husk lint --explain RULE_NAME
```

### Documentation

```bash
# Generate documentation
husk doc

# Generate and serve locally
husk doc --serve
husk doc --serve --port 8080

# Generate for specific target
husk doc --target wasm32

# Include private items
husk doc --document-private-items

# Open in browser
husk doc --open

# Generate external documentation
husk doc --extern-deps

# Custom documentation
husk doc --output-dir ./docs
husk doc --theme dark
```

## Configuration

### Project Configuration (Husk.toml)

```toml
[package]
name = "my-project"
version = "0.1.0"
description = "A Husk project"
authors = ["Your Name <your.email@example.com>"]
license = "MIT"
homepage = "https://example.com"
repository = "https://github.com/user/repo"
documentation = "https://docs.example.com"
readme = "README.md"
keywords = ["cli", "utility"]
categories = ["command-line-utilities"]

[dependencies]
serde = "1.0"
http = { version = "2.0", optional = true }
local-lib = { path = "../local-lib" }

[dev-dependencies]
test-utils = "0.1"

[build-dependencies]
build-script = "1.0"

[features]
default = ["std"]
std = []
web = ["http"]

[profile.dev]
debug = true
optimize = false

[profile.release]
debug = false
optimize = true
lto = true

[target.wasm32-wasi]
panic = "abort"

[target.js]
emit-types = true
```

### Global Configuration (.huskrc)

```toml
[build]
default-target = "native"
jobs = 4
incremental = true

[registry]
default = "https://packages.husk-lang.org"
token = "your-token-here"

[alias]
b = "build"
r = "run"
t = "test"
c = "check"

[editor]
command = "code"
args = ["-g", "{file}:{line}:{column}"]

[formatter]
line-width = 100
tab-size = 4
use-tabs = false

[linter]
rules = ["all"]
max-warnings = 0
```

### Environment-specific Configuration

```bash
# Development environment
export HUSK_ENV=development
export HUSK_LOG_LEVEL=debug

# Production environment
export HUSK_ENV=production
export HUSK_LOG_LEVEL=error

# Testing environment
export HUSK_ENV=test
export HUSK_PARALLEL_TESTS=true
```

## Environment Variables

### Build Environment

```bash
# Compilation settings
export HUSK_TARGET=wasm32-wasi
export HUSK_OPTIMIZE=true
export HUSK_DEBUG=false
export HUSK_LTO=true

# Parallel compilation
export HUSK_JOBS=8
export HUSK_INCREMENTAL=true

# Output settings
export HUSK_OUTPUT_DIR=./build
export HUSK_EMIT_ASM=true
export HUSK_EMIT_LLVM=true
```

### Runtime Environment

```bash
# Logging and debugging
export HUSK_LOG=debug
export HUSK_BACKTRACE=1
export HUSK_PANIC=abort

# Memory settings
export HUSK_STACK_SIZE=8388608  # 8MB
export HUSK_HEAP_SIZE=67108864  # 64MB

# Performance
export HUSK_PROFILE=cpu
export HUSK_TRACE=true
```

### Registry Settings

```bash
# Package registry
export HUSK_REGISTRY_URL=https://packages.husk-lang.org
export HUSK_REGISTRY_TOKEN=your-token
export HUSK_OFFLINE=false

# Cache settings
export HUSK_CACHE_DIR=$HOME/.husk/cache
export HUSK_PACKAGE_CACHE_SIZE=1GB
export HUSK_CLEAR_CACHE=false
```

## Debugging Commands

### Debug Information

```bash
# Show compilation process
husk build --verbose --timing

# Dump intermediate representations
husk build --emit ast
husk build --emit hir
husk build --emit mir
husk build --emit llvm-ir
husk build --emit asm

# Save intermediate files
husk build --save-temps

# Show linking information
husk build --verbose-linker

# Memory usage during compilation
husk build --memory-profile
```

### Runtime Debugging

```bash
# Run with debugger
husk debug
husk debug --args "arg1 arg2"

# Generate core dump
husk run --core-dump

# Memory debugging
husk run --valgrind
husk run --sanitize address
husk run --sanitize memory
husk run --sanitize thread

# Performance profiling
husk run --profile cpu
husk run --profile memory
husk run --profile io
```

### Diagnostics

```bash
# Show system information
husk --info
husk --version --verbose

# Dependency tree
husk tree
husk tree --duplicates
husk tree --format json

# Check project health
husk doctor
husk doctor --fix

# Verify installation
husk self check
husk self update
```

## Performance Analysis

### Profiling

```bash
# CPU profiling
husk profile cpu
husk profile cpu --duration 30s
husk profile cpu --output profile.svg

# Memory profiling
husk profile memory
husk profile memory --track-allocations

# I/O profiling
husk profile io
husk profile io --trace-syscalls

# Combined profiling
husk profile all --output-dir ./profiles
```

### Benchmarking

```bash
# Run benchmarks
husk bench
husk bench --filter "sort_*"

# Benchmark comparison
husk bench --baseline main
husk bench --compare-with baseline.json

# Custom benchmark duration
husk bench --duration 10s
husk bench --iterations 1000

# Benchmark output formats
husk bench --output json
husk bench --output html --output-file report.html
```

### Build Performance

```bash
# Compilation timing
husk build --timing
husk build --timing --output timing.html

# Incremental build analysis
husk build --incremental --explain

# Parallel build efficiency
husk build --jobs 1 --timing
husk build --jobs $(nproc) --timing

# Cache analysis
husk cache stats
husk cache clean
husk cache size
```

## Plugin System

### Managing Plugins

```bash
# List installed plugins
husk plugin list

# Install plugin
husk plugin install husk-formatter
husk plugin install --git https://github.com/user/plugin.git

# Update plugins
husk plugin update
husk plugin update husk-formatter

# Remove plugin
husk plugin remove husk-formatter

# Plugin information
husk plugin info husk-formatter
```

### Using Plugins

```bash
# Run plugin command
husk my-plugin command args

# Plugin with options
husk formatter --style=compact
husk linter --strict --fix

# Chain plugin commands
husk fmt && husk lint && husk test
```

### Developing Plugins

```bash
# Create plugin template
husk plugin new my-plugin
husk plugin new my-plugin --template advanced

# Test plugin locally
husk plugin link ./my-plugin
husk plugin test ./my-plugin

# Publish plugin
husk plugin publish
husk plugin publish --dry-run
```

## CI/CD Integration

### GitHub Actions

```yaml
name: CI
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: husk-lang/setup-husk@v1
        with:
          husk-version: latest
      - run: husk test
      - run: husk build --release

  cross-platform:
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        target: [native, wasm32-wasi, js]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v3
      - uses: husk-lang/setup-husk@v1
      - run: husk build --target ${{ matrix.target }}
```

### Docker Integration

```dockerfile
# Dockerfile
FROM husk:latest AS builder
WORKDIR /app
COPY . .
RUN husk build --release --target native

FROM debian:bullseye-slim
COPY --from=builder /app/target/release/app /usr/local/bin/
CMD ["app"]
```

### Build Scripts

```bash
#!/bin/bash
# build.sh

set -e

echo "Building Husk project..."

# Install dependencies
husk update

# Run tests
husk test --coverage

# Build for multiple targets
husk build --target native --release
husk build --target wasm32-wasi --release
husk build --target js --release

# Generate documentation
husk doc --output-dir ./dist/docs

# Package releases
tar -czf app-linux.tar.gz target/native/release/app
zip -r app-web.zip target/wasm32/release/ target/js/release/

echo "Build complete!"
```

## Troubleshooting

### Common Issues

```bash
# Compilation errors
husk check --explain E0001  # Explain error code
husk build --verbose        # Detailed error messages

# Dependency issues
husk clean                  # Clean build cache
husk update                 # Update dependencies
husk tree --duplicates      # Check for conflicts

# Performance issues
husk build --profile        # Profile compilation
husk build --jobs 1         # Disable parallel build
husk cache clean            # Clear cache

# Platform issues
husk targets list           # Check available targets
husk --info                 # Show system information
husk doctor                 # Diagnose problems
```

### Debug Commands

```bash
# Verbose output
husk -v build
husk --verbose test

# Environment debugging
husk env
husk config list

# Cache debugging
husk cache info
husk cache verify
husk cache clean --all

# Network debugging
husk --offline update       # Test offline mode
husk registry ping          # Test registry connection
```

### Getting Help

```bash
# Command help
husk --help
husk build --help
husk test --help

# Error explanations
husk explain E0001
husk explain --list

# Documentation
husk doc --open
husk doc std

# Community resources
husk community
husk discuss
husk report-bug
```

## Related Topics

- [Build System](build-system.md) - Advanced build configuration
- [Testing](testing.md) - Testing framework and tools
- [Debugging](debugging.md) - Debugging techniques
- [JavaScript Transpilation](../advanced/javascript-transpilation.md) - JS target details
- [Performance](../advanced/performance.md) - Optimization techniques

---

*The Husk CLI is your gateway to the Husk ecosystem. Master these commands to become productive with Husk development.*