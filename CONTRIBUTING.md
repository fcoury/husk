# Contributing to Husk

Thank you for your interest in contributing to Husk! We're excited to have you join our community of developers working to make programming more accessible and enjoyable.

Husk thrives on community contributions, whether you're reporting bugs, suggesting features, improving documentation, or writing code. This guide will help you get started.

## 🤝 Code of Conduct

We are committed to providing a welcoming and inclusive environment for everyone. Please be respectful, constructive, and kind in all interactions.

## 🎯 Ways to Contribute

### 🐛 Bug Reports
Found something that doesn't work as expected? Help us fix it!

- **Search existing issues** first to avoid duplicates
- **Use the bug report template** when creating new issues
- **Include reproduction steps** and your environment details
- **Add the `bug` label** to help us categorize

### 💡 Feature Requests
Have an idea to make Husk better? We'd love to hear it!

- **Check existing discussions** to see if it's already being considered
- **Describe the use case** and why it would be valuable
- **Consider the scope** - start with smaller, focused features
- **Add the `enhancement` label**

### 📝 Documentation
Good documentation makes Husk accessible to everyone.

- **Fix typos and improve clarity** in existing docs
- **Add examples** to illustrate language features
- **Update the website** with new content or improvements
- **Write tutorials** for common use cases

### 🔧 Code Contributions
Ready to dive into the codebase? Here's how to get started.

## 🚀 Getting Started

### Prerequisites
- **Rust 1.70+** and Cargo
- **Git** for version control
- **Node.js** (optional, for website development)

### Development Setup

1. **Fork the repository**
   ```bash
   # Click the "Fork" button on GitHub, then clone your fork
   git clone https://github.com/your-username/husk.git
   cd husk
   ```

2. **Set up the development environment**
   ```bash
   # Build the project
   cargo build
   
   # Run tests to ensure everything works
   cargo test
   
   # Install locally for testing
   cargo install --path .
   ```

3. **Create a feature branch**
   ```bash
   git checkout -b feature/your-feature-name
   # or
   git checkout -b fix/bug-description
   ```

### Making Changes

1. **Write your code**
   - Follow existing code style and patterns
   - Add tests for new functionality
   - Update documentation as needed

2. **Test your changes**
   ```bash
   # Run the full test suite
   cargo test
   
   # Test specific functionality
   cargo test test_name
   
   # Run integration tests
   cargo test --test integration_tests
   ```

3. **Test with real examples**
   ```bash
   # Test interpreter mode
   husk run examples/simple-test.husk
   
   # Test transpiler mode
   husk compile examples/simple-test.husk | node
   ```

## 📋 Contribution Guidelines

### Code Style
- **Follow Rust conventions** using `cargo fmt`
- **Use meaningful names** for variables, functions, and types
- **Write clear comments** for complex logic
- **Keep functions focused** and single-purpose

### Commit Messages
We use [Conventional Commits](https://www.conventionalcommits.org/) for clear, consistent commit history:

```
type(scope): description

feat(parser): add support for match expressions
fix(transpiler): resolve async/await generation bug
docs(readme): update installation instructions
test(interpreter): add tests for error handling
```

**Types:**
- `feat`: New features
- `fix`: Bug fixes
- `docs`: Documentation changes
- `test`: Test additions or modifications
- `refactor`: Code restructuring without behavior changes
- `perf`: Performance improvements
- `chore`: Maintenance tasks

### Pull Request Process

1. **Create a focused PR**
   - One feature or fix per PR
   - Clear title and description
   - Reference related issues

2. **Fill out the PR template**
   - Describe what changed and why
   - Include testing instructions
   - Note any breaking changes

3. **Ensure CI passes**
   - All tests must pass
   - Code must compile without warnings
   - Follow the style guidelines

4. **Respond to feedback**
   - Address review comments promptly
   - Ask questions if something is unclear
   - Make requested changes

## 🏗️ Architecture Overview

Understanding Husk's architecture helps you contribute more effectively:

### Core Components

```
src/
├── lexer/          # Tokenizes Husk source code
├── parser/         # Builds AST from tokens
├── interpreter/    # Executes Husk code directly
├── transpiler/     # Converts Husk AST to JavaScript
├── types/          # Type system and inference
├── stdlib/         # Built-in functions and types
└── cli/           # Command-line interface
```

### Key Concepts

- **Dual Execution**: Husk can run interpreted or transpiled
- **Type Inference**: Static typing with minimal annotations
- **JavaScript Interop**: Seamless integration with npm packages
- **Error Handling**: Built-in Result and Option types

## 🎯 Specific Contribution Areas

### 🔤 Language Features
- Pattern matching improvements
- New built-in types or methods
- Syntax enhancements
- Error message improvements

### 🏃 Performance
- Interpreter optimizations
- Transpiler output improvements
- Memory usage reductions
- Startup time optimizations

### 🌐 JavaScript Integration
- npm package compatibility
- Browser API support
- Node.js feature integration
- Build tool improvements

### 🧪 Testing
- Unit test coverage
- Integration test scenarios
- Performance benchmarks
- Error condition testing

### 📚 Documentation & Examples
- Language feature examples
- Real-world use cases
- Tutorial content
- API documentation

## 🐛 Debugging & Development Tips

### Useful Commands
```bash
# Run with debug output
RUST_LOG=debug husk run script.husk

# Profile performance
cargo build --release
time target/release/husk run large-script.husk

# Check for common issues
cargo clippy

# Generate documentation
cargo doc --open
```

### Testing Strategies
- **Unit tests**: Test individual components in isolation
- **Integration tests**: Test complete workflows
- **Example scripts**: Maintain a collection of working examples
- **Performance tests**: Ensure changes don't regress performance

## 📞 Getting Help

Stuck on something? Here's how to get help:

- **💬 GitHub Discussions**: Ask questions and discuss ideas
- **🐛 GitHub Issues**: Report bugs and request features  
- **📖 Documentation**: Check existing docs and examples
- **💻 Code Review**: Learn from PR feedback and discussions

## 🎉 Recognition

We appreciate all contributions! Contributors are recognized in:
- **README acknowledgments** for significant contributions
- **Release notes** for features and fixes
- **Community highlights** on the website
- **Contributor list** maintained in the repository

## 📝 License

By contributing to Husk, you agree that your contributions will be licensed under the MIT License, the same as the project.

---

## 🚀 Ready to Contribute?

1. **Choose an issue** labeled `good first issue` or `help wanted`
2. **Fork the repository** and create your branch
3. **Make your changes** following these guidelines
4. **Submit a pull request** and engage with the review process

We're here to help make your first contribution successful. Don't hesitate to ask questions - we're all learning together!

**Happy coding!** 🦀✨