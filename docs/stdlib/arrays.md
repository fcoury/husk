# Array Operations

Arrays are fundamental collections in Husk, providing efficient indexed access and a rich set of functional operations. This guide covers all array operations in the standard library.

## Table of Contents

- [Overview](#overview)
- [Array Creation](#array-creation)
- [Basic Operations](#basic-operations)
- [Functional Methods](#functional-methods)
  - [Transformation](#transformation)
  - [Filtering](#filtering)
  - [Reduction](#reduction)
  - [Searching](#searching)
- [Array Manipulation](#array-manipulation)
- [Sorting and Ordering](#sorting-and-ordering)
- [Array Slicing](#array-slicing)
- [Iteration](#iteration)
- [Multi-dimensional Arrays](#multi-dimensional-arrays)
- [Performance Considerations](#performance-considerations)
- [Common Patterns](#common-patterns)
- [JavaScript Interop](#javascript-interop)
- [Related Topics](#related-topics)

## Overview

Husk arrays are:
- **Fixed-size** - Size determined at creation
- **Homogeneous** - All elements have the same type
- **Zero-indexed** - First element at index 0
- **Memory-efficient** - Contiguous memory layout

### Key Features

1. **Functional operations** - map, filter, fold, etc.
2. **Type safety** - Compile-time type checking
3. **Bounds checking** - Safe indexing
4. **Iterator support** - Efficient iteration

## Array Creation

### Array Literals

```rust
// Array literal
let numbers = [1, 2, 3, 4, 5];
let strings = ["hello", "world"];
let booleans = [true, false, true];

// Empty array (needs type annotation)
let empty: array<int> = [];

// Array with repeated values
let zeros = [0; 10];        // [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
let trues = [true; 5];      // [true, true, true, true, true]

// Type inference
let inferred = [1, 2, 3];   // array<int> inferred
```

### Array Construction

```rust
// From iterator
let range: array<int> = (0..10).collect();
let squares: array<int> = (0..5).map(|x| x * x).collect();

// From vector (dynamic array)
let vec = vec![1, 2, 3];
let arr: array<int> = vec.into();

// Builder pattern
let arr = array::builder()
    .push(1)
    .push(2)
    .push(3)
    .build();

// With capacity
let mut arr = Array::with_capacity(100);
for i in 0..100 {
    arr.push(i);
}
```

## Basic Operations

### Length and Access

```rust
let arr = [10, 20, 30, 40, 50];

// Length
let len = arr.len();            // 5
let is_empty = arr.is_empty();  // false

// Indexing
let first = arr[0];             // 10
let last = arr[arr.len() - 1];  // 50

// Safe indexing
let maybe_element = arr.get(2);  // Some(30)
let out_of_bounds = arr.get(10); // None

// First and last
let first = arr.first();         // Some(10)
let last = arr.last();          // Some(50)

// Bounds checking
if index < arr.len() {
    let element = arr[index];
}
```

### Comparison

```rust
let arr1 = [1, 2, 3];
let arr2 = [1, 2, 3];
let arr3 = [1, 2, 4];

// Equality
let equal = arr1 == arr2;        // true
let not_equal = arr1 != arr3;    // true

// Lexicographic ordering
let less = arr1 < arr3;          // true
let greater = arr3 > arr1;       // true

// Element-wise comparison
let all_equal = arr1.iter()
    .zip(arr2.iter())
    .all(|(a, b)| a == b);       // true
```

### Copying and Cloning

```rust
let original = [1, 2, 3, 4, 5];

// Clone entire array
let cloned = original.clone();

// Copy to another array
let mut destination = [0; 5];
destination.copy_from_slice(&original);

// Partial copy
let mut partial = [0; 3];
partial.copy_from_slice(&original[0..3]);
```

## Functional Methods

### Transformation

```rust
let numbers = [1, 2, 3, 4, 5];

// Map - transform each element
let doubled = numbers.map(|x| x * 2);
// [2, 4, 6, 8, 10]

let strings = numbers.map(|x| x.to_string());
// ["1", "2", "3", "4", "5"]

// Map with index
let indexed = numbers.enumerate()
    .map(|(i, x)| format!("{}: {}", i, x));
// ["0: 1", "1: 2", "2: 3", "3: 4", "4: 5"]

// Flat map
let nested = [[1, 2], [3, 4], [5, 6]];
let flattened = nested.flat_map(|arr| arr.to_vec());
// [1, 2, 3, 4, 5, 6]

// Map in place (if mutable)
let mut nums = [1, 2, 3];
nums.iter_mut().for_each(|x| *x *= 2);
// nums is now [2, 4, 6]
```

### Filtering

```rust
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

// Filter - keep elements matching predicate
let evens = numbers.filter(|x| x % 2 == 0);
// [2, 4, 6, 8, 10]

let large = numbers.filter(|x| x > 5);
// [6, 7, 8, 9, 10]

// Filter with index
let odd_indices = numbers.enumerate()
    .filter(|(i, _)| i % 2 == 1)
    .map(|(_, x)| x);
// [2, 4, 6, 8, 10]

// Partition - split into two arrays
let (evens, odds) = numbers.partition(|x| x % 2 == 0);
// evens: [2, 4, 6, 8, 10]
// odds: [1, 3, 5, 7, 9]

// Take while condition is true
let prefix = numbers.take_while(|x| x < 5);
// [1, 2, 3, 4]

// Skip while condition is true
let suffix = numbers.skip_while(|x| x < 5);
// [5, 6, 7, 8, 9, 10]
```

### Reduction

```rust
let numbers = [1, 2, 3, 4, 5];

// Fold (reduce) - combine all elements
let sum = numbers.fold(0, |acc, x| acc + x);
// 15

let product = numbers.fold(1, |acc, x| acc * x);
// 120

// Fold from right
let concatenated = ["a", "b", "c"].fold_right("", |x, acc| x + acc);
// "abc"

// Scan - like fold but keeps intermediate results
let running_sum = numbers.scan(0, |acc, x| acc + x);
// [1, 3, 6, 10, 15]

// Reduce (no initial value)
let max = numbers.reduce(|a, b| if a > b { a } else { b });
// Some(5)

// Sum and product shortcuts
let sum = numbers.sum();        // 15
let product = numbers.product(); // 120
```

### Searching

```rust
let numbers = [1, 2, 3, 4, 5];

// Find first matching element
let found = numbers.find(|x| x > 3);
// Some(4)

// Find with index
let position = numbers.position(|x| x == 3);
// Some(2)

// Check if any/all match
let has_even = numbers.any(|x| x % 2 == 0);
// true

let all_positive = numbers.all(|x| x > 0);
// true

// Count matching elements
let even_count = numbers.filter(|x| x % 2 == 0).count();
// 2

// Binary search (on sorted arrays)
let sorted = [1, 3, 5, 7, 9];
let index = sorted.binary_search(&5);
// Ok(2)

// Contains
let has_three = numbers.contains(&3);
// true
```

## Array Manipulation

### Adding Elements

```rust
// Arrays are fixed-size, so we often convert to Vec
let arr = [1, 2, 3];
let mut vec = arr.to_vec();

// Push elements
vec.push(4);
vec.push(5);
// vec is now [1, 2, 3, 4, 5]

// Insert at position
vec.insert(2, 10);
// vec is now [1, 2, 10, 3, 4, 5]

// Extend with another collection
vec.extend([6, 7, 8]);
// vec is now [1, 2, 10, 3, 4, 5, 6, 7, 8]

// Convert back to array (if size known)
let new_arr: [int; 5] = vec[0..5].try_into().unwrap();
```

### Removing Elements

```rust
let arr = [1, 2, 3, 4, 5];
let mut vec = arr.to_vec();

// Remove by index
let removed = vec.remove(2);  // Returns 3
// vec is now [1, 2, 4, 5]

// Remove last element
let last = vec.pop();  // Some(5)
// vec is now [1, 2, 4]

// Remove first occurrence
vec.remove_item(&2);
// vec is now [1, 4]

// Retain only matching elements
vec.retain(|x| x % 2 == 0);
// vec is now [4]

// Clear all elements
vec.clear();
// vec is now []
```

### Array Reversal and Rotation

```rust
let mut arr = [1, 2, 3, 4, 5];

// Reverse in place
arr.reverse();
// arr is now [5, 4, 3, 2, 1]

// Create reversed copy
let original = [1, 2, 3, 4, 5];
let reversed: array<int> = original.iter().rev().collect();

// Rotate left
arr.rotate_left(2);
// [3, 4, 5, 1, 2]

// Rotate right
arr.rotate_right(2);
// [4, 5, 1, 2, 3]

// Swap elements
arr.swap(0, 4);
// Swaps first and last elements
```

## Sorting and Ordering

### Basic Sorting

```rust
let mut numbers = [3, 1, 4, 1, 5, 9, 2, 6];

// Sort in ascending order
numbers.sort();
// [1, 1, 2, 3, 4, 5, 6, 9]

// Sort in descending order
numbers.sort_by(|a, b| b.cmp(a));
// [9, 6, 5, 4, 3, 2, 1, 1]

// Sort with custom comparison
let mut words = ["apple", "Banana", "cherry"];
words.sort_by(|a, b| a.to_lower().cmp(&b.to_lower()));
// Case-insensitive sort

// Stable sort (preserves order of equal elements)
numbers.sort_stable();

// Check if sorted
let is_sorted = numbers.is_sorted();
```

### Custom Sorting

```rust
struct Person {
    name: string,
    age: int,
}

let mut people = [
    Person { name: "Alice", age: 30 },
    Person { name: "Bob", age: 25 },
    Person { name: "Charlie", age: 35 },
];

// Sort by age
people.sort_by_key(|p| p.age);

// Sort by multiple criteria
people.sort_by(|a, b| {
    match a.age.cmp(&b.age) {
        Ordering::Equal => a.name.cmp(&b.name),
        other => other,
    }
});

// Get sorted indices
let indices = (0..people.len())
    .sorted_by_key(|&i| people[i].age)
    .collect();
```

## Array Slicing

### Creating Slices

```rust
let arr = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

// Full slice
let full = &arr[..];           // [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

// Range slice
let middle = &arr[3..7];        // [3, 4, 5, 6]

// From start
let first_five = &arr[..5];     // [0, 1, 2, 3, 4]

// To end
let last_five = &arr[5..];      // [5, 6, 7, 8, 9]

// Single element slice
let one = &arr[4..5];           // [4]

// Empty slice
let empty = &arr[5..5];         // []
```

### Slice Operations

```rust
let arr = [1, 2, 3, 4, 5];
let slice = &arr[1..4];  // [2, 3, 4]

// Slice methods work like array methods
let sum = slice.sum();          // 9
let doubled = slice.map(|x| x * 2);  // [4, 6, 8]

// Split at index
let (left, right) = arr.split_at(3);
// left: [1, 2, 3], right: [4, 5]

// Split by predicate
let parts = arr.split(|x| x == 3);
// Iterator over [[1, 2], [4, 5]]

// Chunks
let chunks = arr.chunks(2);
// [[1, 2], [3, 4], [5]]

// Windows
let windows = arr.windows(3);
// [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
```

## Iteration

### Basic Iteration

```rust
let arr = [1, 2, 3, 4, 5];

// For loop
for element in arr {
    println!("{}", element);
}

// With index
for (index, element) in arr.iter().enumerate() {
    println!("{}: {}", index, element);
}

// Mutable iteration
let mut arr = [1, 2, 3];
for element in arr.iter_mut() {
    *element *= 2;
}

// Reverse iteration
for element in arr.iter().rev() {
    println!("{}", element);
}
```

### Advanced Iteration

```rust
// Step by
let arr = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
let evens = arr.iter().step_by(2);
// [0, 2, 4, 6, 8]

// Zip arrays
let nums = [1, 2, 3];
let letters = ['a', 'b', 'c'];
let zipped: array<(int, char)> = nums.iter()
    .zip(letters.iter())
    .collect();
// [(1, 'a'), (2, 'b'), (3, 'c')]

// Chain arrays
let arr1 = [1, 2, 3];
let arr2 = [4, 5, 6];
let chained = arr1.iter().chain(arr2.iter());
// [1, 2, 3, 4, 5, 6]

// Cycle (infinite iterator)
let cycled = [1, 2, 3].iter().cycle().take(10);
// [1, 2, 3, 1, 2, 3, 1, 2, 3, 1]
```

## Multi-dimensional Arrays

### 2D Arrays

```rust
// 2D array (array of arrays)
let matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9],
];

// Access element
let element = matrix[1][2];  // 6

// Row operations
let row_sum = matrix[0].sum();  // 6

// Column operations
let col_sum = (0..3)
    .map(|row| matrix[row][0])
    .sum();  // 12

// Flatten 2D to 1D
let flat: array<int> = matrix.iter()
    .flat_map(|row| row.iter())
    .collect();
// [1, 2, 3, 4, 5, 6, 7, 8, 9]

// Transpose
fn transpose<T>(matrix: array<array<T>>) -> array<array<T>> {
    let rows = matrix.len();
    let cols = matrix[0].len();
    
    (0..cols).map(|col| {
        (0..rows).map(|row| matrix[row][col]).collect()
    }).collect()
}
```

### 3D and Higher

```rust
// 3D array
let cube = [
    [[1, 2], [3, 4]],
    [[5, 6], [7, 8]],
];

// Access element
let element = cube[1][0][1];  // 6

// Iterate all elements
for plane in cube {
    for row in plane {
        for element in row {
            process(element);
        }
    }
}
```

## Performance Considerations

### Memory Layout

```rust
// Arrays have contiguous memory layout
// Good cache locality for iteration
let arr = [1, 2, 3, 4, 5];
let sum = arr.iter().sum();  // Fast sequential access

// Avoid unnecessary allocations
// Bad: creates new array each time
fn bad_double(arr: array<int>) -> array<int> {
    arr.map(|x| x * 2)
}

// Good: modifies in place
fn good_double(arr: &mut array<int>) {
    for x in arr.iter_mut() {
        *x *= 2;
    }
}
```

### Efficient Operations

```rust
// Use iterators for chaining operations
// Good: single pass
let result = numbers
    .iter()
    .filter(|x| x % 2 == 0)
    .map(|x| x * x)
    .sum();

// Bad: multiple passes
let evens = numbers.filter(|x| x % 2 == 0);
let squared = evens.map(|x| x * x);
let sum = squared.sum();

// Pre-allocate when size is known
let mut result = Vec::with_capacity(numbers.len());
for n in numbers {
    if n % 2 == 0 {
        result.push(n * n);
    }
}
```

## Common Patterns

### Array Algorithms

```rust
// Find min/max
fn min_max(arr: &array<int>) -> Option<(int, int)> {
    arr.iter().fold(None, |acc, &x| {
        match acc {
            None => Some((x, x)),
            Some((min, max)) => Some((min.min(x), max.max(x))),
        }
    })
}

// Unique elements
fn unique<T: Eq>(arr: &array<T>) -> array<T> {
    let mut result = vec![];
    for item in arr {
        if !result.contains(item) {
            result.push(item.clone());
        }
    }
    result
}

// Frequency count
fn frequency<T: Eq + Hash>(arr: &array<T>) -> HashMap<T, int> {
    let mut counts = HashMap::new();
    for item in arr {
        *counts.entry(item.clone()).or_insert(0) += 1;
    }
    counts
}
```

### Sliding Window

```rust
// Fixed window sum
fn window_sums(arr: &array<int>, window_size: usize) -> array<int> {
    arr.windows(window_size)
        .map(|window| window.sum())
        .collect()
}

// Variable window - find subarray with sum
fn find_sum_subarray(arr: &array<int>, target: int) -> Option<(usize, usize)> {
    let mut start = 0;
    let mut sum = 0;
    
    for end in 0..arr.len() {
        sum += arr[end];
        
        while sum > target && start <= end {
            sum -= arr[start];
            start += 1;
        }
        
        if sum == target {
            return Some((start, end));
        }
    }
    None
}
```

### Array Transformations

```rust
// Rotate array
fn rotate<T: Clone>(arr: &array<T>, k: int) -> array<T> {
    let n = arr.len();
    let k = k % n as int;
    let k = if k < 0 { n as int + k } else { k } as usize;
    
    let mut result = vec![];
    result.extend_from_slice(&arr[n-k..]);
    result.extend_from_slice(&arr[..n-k]);
    result
}

// Shuffle array
fn shuffle<T: Clone>(arr: &array<T>) -> array<T> {
    let mut result = arr.to_vec();
    let mut rng = rand::thread_rng();
    result.shuffle(&mut rng);
    result
}
```

## JavaScript Interop

Husk arrays map directly to JavaScript arrays:

```rust
// Husk
let arr = [1, 2, 3, 4, 5];
let doubled = arr.map(|x| x * 2);
let sum = arr.reduce(|a, b| a + b);
let filtered = arr.filter(|x| x > 2);
```

Becomes:
```javascript
// JavaScript
const arr = [1, 2, 3, 4, 5];
const doubled = arr.map(x => x * 2);
const sum = arr.reduce((a, b) => a + b);
const filtered = arr.filter(x => x > 2);
```

### Method Mappings

| Husk Method | JavaScript Equivalent |
|-------------|---------------------|
| `map()` | `map()` |
| `filter()` | `filter()` |
| `fold()` | `reduce()` |
| `find()` | `find()` |
| `any()` | `some()` |
| `all()` | `every()` |
| `contains()` | `includes()` |
| `position()` | `indexOf()` |
| `len()` | `length` |

## Related Topics

- [Data Types](../language/data-types.md#arrays) - Array type details
- [Iterators](../advanced/iterators.md) - Iterator patterns
- [Vec Type](collections/vec.md) - Dynamic arrays
- [Slices](../language/data-types.md#slices) - Array views
- [Performance](../advanced/performance.md) - Array optimization

---

*Arrays are the workhorses of data manipulation in Husk. Master their functional operations to write clean, efficient code.*