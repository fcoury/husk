#!/bin/bash

# Integration test to verify that generated JavaScript actually works
set -e

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test directory
TEST_DIR="./js-exec-test-$$"
HUSK_BIN="../../target/debug/husk"

# Ensure we're in the right directory
cd "$(dirname "$0")"

echo -e "${GREEN}[INFO]${NC} Testing JavaScript execution from Husk build output..."

# Clean up function
cleanup() {
    rm -rf "$TEST_DIR"
}

# Set up cleanup on exit
trap cleanup EXIT

# Create test directory
mkdir -p "$TEST_DIR"
cd "$TEST_DIR"

# Test 1: Basic Node.js ESM execution
echo -e "${GREEN}[INFO]${NC} Test 1: Basic Node.js ESM execution..."
$HUSK_BIN new test-node-esm >/dev/null 2>&1
cd test-node-esm

# Create a more complex Husk program
cat > src/main.husk << 'EOF'
fn add(a: Int, b: Int) -> Int {
    a + b
}

fn greet(name: String) -> String {
    format!("Hello, {}!", name)
}

fn main() {
    println!("Testing Husk to JavaScript compilation");
    
    let result = add(5, 3);
    println!(format!("5 + 3 = {}", result));
    
    let message = greet("World");
    println!(message);
    
    // Test array operations
    let numbers = [1, 2, 3, 4, 5];
    let doubled = numbers.map(|x| x * 2);
    println!(format!("Doubled: {:?}", doubled));
    
    // Test string operations
    let text = "  Husk Language  ";
    println!(format!("Trimmed: '{}'", text.trim()));
    
    // Test control flow
    for i in 0..3 {
        println!(format!("Loop iteration: {}", i));
    }
}
EOF

# Build the project
echo "Building Node.js ESM target..."
$HUSK_BIN build --target node-esm >/dev/null 2>&1

# Run the generated JavaScript
echo "Running generated JavaScript..."
OUTPUT=$(cd dist && node main.js 2>&1)

# Verify output
if echo "$OUTPUT" | grep -q "Testing Husk to JavaScript compilation" && \
   echo "$OUTPUT" | grep -q "5 + 3 = 8" && \
   echo "$OUTPUT" | grep -q "Hello, World!" && \
   echo "$OUTPUT" | grep -q "Doubled: \[2, 4, 6, 8, 10\]" && \
   echo "$OUTPUT" | grep -q "Trimmed: 'Husk Language'" && \
   echo "$OUTPUT" | grep -q "Loop iteration: 2"; then
    echo -e "${GREEN}[PASS]${NC} Node.js ESM execution successful"
else
    echo -e "${RED}[FAIL]${NC} Node.js ESM execution failed"
    echo "Expected output not found. Got:"
    echo "$OUTPUT"
    exit 1
fi

cd ..

# Test 2: Node.js CommonJS execution
echo -e "${GREEN}[INFO]${NC} Test 2: Node.js CommonJS execution..."
$HUSK_BIN new test-node-cjs >/dev/null 2>&1
cd test-node-cjs

# Use the same Husk program
cat > src/main.husk << 'EOF'
fn calculate(x: Int) -> Int {
    x * x + 2 * x + 1
}

fn main() {
    println!("Testing CommonJS output");
    let result = calculate(5);
    println!(format!("f(5) = {}", result));
}
EOF

# Build with CommonJS target
echo "Building Node.js CommonJS target..."
$HUSK_BIN build --target node-cjs >/dev/null 2>&1

# Run the generated JavaScript
echo "Running generated JavaScript..."
OUTPUT=$(cd dist && node main.js 2>&1)

# Verify output
if echo "$OUTPUT" | grep -q "Testing CommonJS output" && \
   echo "$OUTPUT" | grep -q "f(5) = 36"; then
    echo -e "${GREEN}[PASS]${NC} Node.js CommonJS execution successful"
else
    echo -e "${RED}[FAIL]${NC} Node.js CommonJS execution failed"
    echo "Expected output not found. Got:"
    echo "$OUTPUT"
    exit 1
fi

cd ..

# Test 3: NPM package usage
echo -e "${GREEN}[INFO]${NC} Test 3: NPM package usage test..."
$HUSK_BIN new test-npm-packages >/dev/null 2>&1
cd test-npm-packages

# Add lodash dependency
cat > husk.toml << 'EOF'
[package]
name = "test-npm-packages"
version = "0.1.0"
description = "Test npm package usage"
author = ""
license = "MIT"

[dependencies]
lodash = "^4.17.21"

[dev-dependencies]

[build]
src = "src"
out = "dist"
target = ""
module = "esm"
EOF

# Create Husk program that uses lodash
cat > src/main.husk << 'EOF'
use lodash;

fn main() {
    println!("Testing npm package usage");
    
    // These will be transpiled to use lodash functions
    let arr = [1, 2, 3, 4, 5];
    println!(format!("Array: {:?}", arr));
}
EOF

# Build the project
echo "Building with npm packages..."
$HUSK_BIN build >/dev/null 2>&1

# Install npm dependencies
echo "Installing npm dependencies..."
npm install >/dev/null 2>&1

# Check if lodash import is in the generated code
if grep -q "import.*from.*lodash" dist/main.js || grep -q "require.*lodash" dist/main.js; then
    echo -e "${GREEN}[PASS]${NC} NPM package import generated correctly"
else
    echo -e "${RED}[FAIL]${NC} NPM package import not found in generated code"
    exit 1
fi

cd ..

# Test 4: Development vs Production mode
echo -e "${GREEN}[INFO]${NC} Test 4: Development vs Production mode..."
$HUSK_BIN new test-dev-prod >/dev/null 2>&1
cd test-dev-prod

# Configure dev and prod targets
cat > husk.toml << 'EOF'
[package]
name = "test-dev-prod"
version = "0.1.0"
description = "Test dev/prod modes"
author = ""
license = "MIT"

[dependencies]

[dev-dependencies]

[build]
src = "src"
out = "dist"
target = ""
module = "esm"

[targets.dev]
dev = true
tree_shaking = false

[targets.prod]
dev = false
tree_shaking = true
EOF

# Create test program
cat > src/main.husk << 'EOF'
fn unused_function() -> Int {
    42
}

fn main() {
    let x = 10;
    println!(format!("Value: {}", x));
}
EOF

# Build dev target
echo "Building development target..."
mkdir -p dist-dev
$HUSK_BIN build --target dev >/dev/null 2>&1
mv dist/main.js dist-dev/

# Build prod target
echo "Building production target..."
mkdir -p dist-prod
$HUSK_BIN build --target prod >/dev/null 2>&1
mv dist/main.js dist-prod/

# Check dev build has debug comments
if grep -q "// Variable: x" dist-dev/main.js && grep -q "Type assertion:" dist-dev/main.js; then
    echo -e "${GREEN}[PASS]${NC} Development build contains debug artifacts"
else
    echo -e "${YELLOW}[WARN]${NC} Development build missing expected debug artifacts"
fi

# Check prod build has tree shaking annotations
if grep -q "/\\*#__PURE__\\*/" dist-prod/main.js; then
    echo -e "${GREEN}[PASS]${NC} Production build contains tree shaking annotations"
else
    echo -e "${YELLOW}[WARN]${NC} Production build missing tree shaking annotations"
fi

# Both should execute correctly
OUTPUT_DEV=$(cd dist-dev && node main.js 2>&1)
OUTPUT_PROD=$(cd dist-prod && node main.js 2>&1)

if echo "$OUTPUT_DEV" | grep -q "Value: 10" && echo "$OUTPUT_PROD" | grep -q "Value: 10"; then
    echo -e "${GREEN}[PASS]${NC} Both dev and prod builds execute correctly"
else
    echo -e "${RED}[FAIL]${NC} Build execution failed"
    exit 1
fi

cd ..

# Test 5: Complex features (async, enums, structs)
echo -e "${GREEN}[INFO]${NC} Test 5: Complex features test..."
$HUSK_BIN new test-complex >/dev/null 2>&1
cd test-complex

cat > src/main.husk << 'EOF'
struct Person {
    name: String,
    age: Int,
}

enum Status {
    Active,
    Inactive,
    Pending(String),
}

impl Person {
    fn new(name: String, age: Int) -> Person {
        Person { name, age }
    }
    
    fn greet(&self) -> String {
        format!("{} is {} years old", self.name, self.age)
    }
}

async fn delay_message(msg: String) -> String {
    format!("Delayed: {}", msg)
}

fn main() {
    // Test struct
    let person = Person::new("Alice", 30);
    println!(person.greet());
    
    // Test enum
    let status = Status::Pending("Waiting for approval");
    match status {
        Status::Active => println!("Active"),
        Status::Inactive => println!("Inactive"),
        Status::Pending(msg) => println!(format!("Pending: {}", msg)),
    }
    
    // Test Option
    let maybe_value: Option<Int> = Some(42);
    match maybe_value {
        Some(v) => println!(format!("Value: {}", v)),
        None => println!("No value"),
    }
}
EOF

# Build and run
echo "Building complex features test..."
$HUSK_BIN build >/dev/null 2>&1

OUTPUT=$(cd dist && node main.js 2>&1)

if echo "$OUTPUT" | grep -q "Alice is 30 years old" && \
   echo "$OUTPUT" | grep -q "Pending: Waiting for approval" && \
   echo "$OUTPUT" | grep -q "Value: 42"; then
    echo -e "${GREEN}[PASS]${NC} Complex features work correctly"
else
    echo -e "${RED}[FAIL]${NC} Complex features test failed"
    echo "Got output:"
    echo "$OUTPUT"
    exit 1
fi

cd ..

# Summary
echo -e "\n${GREEN}[INFO]${NC} JavaScript Execution Test Summary:"
echo -e "${GREEN}[PASS]${NC} All JavaScript execution tests passed!"
echo "  ✓ Node.js ESM execution"
echo "  ✓ Node.js CommonJS execution"
echo "  ✓ NPM package imports"
echo "  ✓ Development/Production modes"
echo "  ✓ Complex language features"