# Math Functions

Husk provides a comprehensive set of mathematical functions and constants for numerical computations. This guide covers all mathematical operations available in the standard library.

## Table of Contents

- [Overview](#overview)
- [Mathematical Constants](#mathematical-constants)
- [Basic Arithmetic](#basic-arithmetic)
- [Power and Root Functions](#power-and-root-functions)
- [Trigonometric Functions](#trigonometric-functions)
- [Exponential and Logarithmic](#exponential-and-logarithmic)
- [Rounding and Truncation](#rounding-and-truncation)
- [Comparison Functions](#comparison-functions)
- [Statistical Functions](#statistical-functions)
- [Random Number Generation](#random-number-generation)
- [Number Conversion](#number-conversion)
- [Precision and Limits](#precision-and-limits)
- [Complex Numbers](#complex-numbers)
- [Mathematical Utilities](#mathematical-utilities)
- [Performance Considerations](#performance-considerations)
- [JavaScript/Node.js Interop](#javascriptnodejs-interop)
- [Related Topics](#related-topics)

## Overview

Husk's mathematical functions:
- **IEEE-754 compliant** - Standard floating-point operations
- **Cross-platform** - Consistent behavior across platforms  
- **Type-safe** - Compile-time type checking
- **Performance optimized** - Efficient implementations

### Key Features

1. **Complete math library** - All common mathematical functions
2. **Constant accuracy** - High-precision mathematical constants
3. **Error handling** - Safe operations with Result types
4. **Generic functions** - Work with multiple numeric types

## Mathematical Constants

### Common Constants

```rust
// Fundamental constants
const PI: float = 3.141592653589793;
const E: float = 2.718281828459045;
const TAU: float = 6.283185307179586;  // 2 * PI

// Mathematical constants
const PHI: float = 1.618033988749895;  // Golden ratio
const SQRT_2: float = 1.4142135623730951;
const SQRT_3: float = 1.7320508075688772;
const LN_2: float = 0.6931471805599453;
const LN_10: float = 2.302585092994046;
const LOG2_E: float = 1.4426950408889634;
const LOG10_E: float = 0.4342944819032518;

// Usage examples
let circumference = 2.0 * PI * radius;
let area = PI * radius * radius;
let exponential = E.powf(x);
let golden_ratio = PHI;

// Derived constants
const PI_2: float = PI / 2.0;     // π/2
const PI_4: float = PI / 4.0;     // π/4
const TWO_PI: float = 2.0 * PI;   // 2π
const SQRT_PI: float = 1.7724538509055159;
const INV_PI: float = 1.0 / PI;   // 1/π
```

### Special Values

```rust
// IEEE-754 special values
const INFINITY: float = float::INFINITY;
const NEG_INFINITY: float = float::NEG_INFINITY;
const NAN: float = float::NAN;

// Limits
const MAX: float = float::MAX;
const MIN: float = float::MIN;
const MIN_POSITIVE: float = float::MIN_POSITIVE;
const EPSILON: float = float::EPSILON;

// Testing special values
fn handle_special(x: float) -> string {
    if x.is_nan() {
        "Not a number".to_string()
    } else if x.is_infinite() {
        if x.is_sign_positive() {
            "Positive infinity".to_string()
        } else {
            "Negative infinity".to_string()
        }
    } else if x.is_finite() {
        format!("Finite: {}", x)
    } else {
        "Unknown".to_string()
    }
}
```

## Basic Arithmetic

### Absolute Value and Sign

```rust
// Absolute value
let abs_positive = abs(5.0);    // 5.0
let abs_negative = abs(-5.0);   // 5.0
let abs_zero = abs(0.0);        // 0.0

// Sign operations
let sign_pos = signum(5.0);     // 1.0
let sign_neg = signum(-5.0);    // -1.0
let sign_zero = signum(0.0);    // 0.0

// Copy sign (magnitude of first, sign of second)
let result = copysign(5.0, -1.0);  // -5.0
let result2 = copysign(-3.0, 2.0); // 3.0

// Integer absolute value
let int_abs = (-42).abs();      // 42

// Safe absolute value for integers (prevents overflow)
fn safe_abs(x: int) -> Option<int> {
    if x == int::MIN {
        None  // Would overflow
    } else {
        Some(x.abs())
    }
}
```

### Min/Max Operations

```rust
// Binary min/max
let smaller = min(5.0, 3.0);    // 3.0
let larger = max(5.0, 3.0);     // 5.0

// Multiple values
let minimum = min(min(a, b), min(c, d));
let maximum = max(max(a, b), max(c, d));

// Array min/max
fn array_min(arr: &[float]) -> Option<float> {
    arr.iter().min_by(|a, b| a.partial_cmp(b).unwrap())
}

fn array_max(arr: &[float]) -> Option<float> {
    arr.iter().max_by(|a, b| a.partial_cmp(b).unwrap())
}

// Clamping (constrain to range)
fn clamp(value: float, min_val: float, max_val: float) -> float {
    max(min_val, min(value, max_val))
}

let clamped = clamp(15.0, 0.0, 10.0);  // 10.0
let in_range = clamp(5.0, 0.0, 10.0);  // 5.0
```

### Remainder and Modulo

```rust
// Floating-point remainder (IEEE remainder)
let rem1 = remainder(7.0, 3.0);    // 1.0
let rem2 = remainder(-7.0, 3.0);   // -1.0
let rem3 = remainder(7.0, -3.0);   // 1.0

// Modulo operation (always positive result)
fn modulo(a: float, b: float) -> float {
    ((a % b) + b) % b
}

let mod1 = modulo(7.0, 3.0);    // 1.0
let mod2 = modulo(-7.0, 3.0);   // 2.0
let mod3 = modulo(7.0, -3.0);   // 1.0

// Integer division and remainder
let quotient = 17 / 5;          // 3
let remainder = 17 % 5;         // 2

// Euclidean division
fn div_euclid(a: int, b: int) -> (int, int) {
    let q = a / b;
    let r = a % b;
    if r < 0 {
        if b > 0 {
            (q - 1, r + b)
        } else {
            (q + 1, r - b)
        }
    } else {
        (q, r)
    }
}
```

## Power and Root Functions

### Exponentiation

```rust
// Power function
let square = pow(5.0, 2.0);      // 25.0
let cube = pow(2.0, 3.0);        // 8.0
let fractional = pow(8.0, 1.0/3.0); // 2.0 (cube root)

// Integer power (more efficient)
let int_power = powi(2.0, 10);   // 1024.0

// Square
let squared = 5.0 * 5.0;         // 25.0
let sq_func = square(5.0);       // 25.0 (if available)

// Exponential base e
let exp_result = exp(1.0);       // e ≈ 2.718
let exp_2 = exp(2.0);            // e² ≈ 7.389

// Exponential base 2
let exp2_result = exp2(3.0);     // 2³ = 8.0
let exp2_half = exp2(0.5);       // √2 ≈ 1.414

// Exponential minus 1 (more accurate for small values)
let expm1_small = expm1(0.001);  // More accurate than exp(0.001) - 1
```

### Root Functions

```rust
// Square root
let sqrt_result = sqrt(16.0);    // 4.0
let sqrt_2 = sqrt(2.0);          // ≈ 1.414

// Cube root
let cbrt_result = cbrt(8.0);     // 2.0
let cbrt_neg = cbrt(-8.0);       // -2.0

// nth root
fn nth_root(x: float, n: float) -> float {
    if n == 0.0 {
        NAN
    } else if x < 0.0 && n % 2.0 == 0.0 {
        NAN  // Even root of negative number
    } else {
        x.abs().powf(1.0 / n) * x.signum()
    }
}

let fifth_root = nth_root(32.0, 5.0);  // 2.0

// Hypot (hypotenuse) - √(x² + y²) with overflow protection
let distance = hypot(3.0, 4.0);  // 5.0
let vector_len = hypot(x, y);

// 3D distance
fn hypot3(x: float, y: float, z: float) -> float {
    sqrt(x*x + y*y + z*z)
}
```

## Trigonometric Functions

### Basic Trigonometric Functions

```rust
// Sine, cosine, tangent (input in radians)
let sin_result = sin(PI / 2.0);  // 1.0
let cos_result = cos(0.0);       // 1.0
let tan_result = tan(PI / 4.0);  // 1.0

// Degrees to radians conversion
fn degrees_to_radians(degrees: float) -> float {
    degrees * PI / 180.0
}

fn radians_to_degrees(radians: float) -> float {
    radians * 180.0 / PI
}

// Trigonometric functions with degree input
fn sin_deg(degrees: float) -> float {
    sin(degrees_to_radians(degrees))
}

let sin_30_deg = sin_deg(30.0);  // 0.5
let cos_60_deg = cos(degrees_to_radians(60.0));  // 0.5

// Common angles
let angles = [
    (0.0, "0°"),
    (PI / 6.0, "30°"),
    (PI / 4.0, "45°"),
    (PI / 3.0, "60°"),
    (PI / 2.0, "90°"),
];

for (angle, name) in angles {
    println!("{}: sin={:.3}, cos={:.3}, tan={:.3}", 
             name, sin(angle), cos(angle), tan(angle));
}
```

### Inverse Trigonometric Functions

```rust
// Arcsine, arccosine, arctangent (result in radians)
let asin_result = asin(0.5);     // PI/6 ≈ 0.524
let acos_result = acos(0.5);     // PI/3 ≈ 1.047
let atan_result = atan(1.0);     // PI/4 ≈ 0.785

// Two-argument arctangent (handles all quadrants)
let atan2_result = atan2(1.0, 1.0);    // PI/4 (first quadrant)
let atan2_neg = atan2(-1.0, 1.0);      // -PI/4 (fourth quadrant)
let atan2_second = atan2(1.0, -1.0);   // 3*PI/4 (second quadrant)

// Convert to degrees
fn atan2_degrees(y: float, x: float) -> float {
    radians_to_degrees(atan2(y, x))
}

// Angle between vectors
fn angle_between_vectors(x1: float, y1: float, x2: float, y2: float) -> float {
    atan2(x1 * y2 - y1 * x2, x1 * x2 + y1 * y2)
}
```

### Hyperbolic Functions

```rust
// Hyperbolic sine, cosine, tangent
let sinh_result = sinh(1.0);     // (e¹ - e⁻¹)/2 ≈ 1.175
let cosh_result = cosh(0.0);     // (e⁰ + e⁻⁰)/2 = 1.0
let tanh_result = tanh(0.0);     // 0.0

// Inverse hyperbolic functions
let asinh_result = asinh(1.175); // ≈ 1.0
let acosh_result = acosh(1.0);   // 0.0
let atanh_result = atanh(0.0);   // 0.0

// Hyperbolic identities
fn verify_hyperbolic_identity(x: float) -> bool {
    let sinh_x = sinh(x);
    let cosh_x = cosh(x);
    let tanh_x = tanh(x);
    
    // cosh² - sinh² = 1
    let identity1 = abs(cosh_x * cosh_x - sinh_x * sinh_x - 1.0) < EPSILON;
    
    // tanh = sinh/cosh
    let identity2 = abs(tanh_x - sinh_x / cosh_x) < EPSILON;
    
    identity1 && identity2
}
```

## Exponential and Logarithmic

### Logarithmic Functions

```rust
// Natural logarithm (base e)
let ln_result = ln(E);           // 1.0
let ln_10 = ln(10.0);            // ≈ 2.303

// Logarithm base 2
let log2_result = log2(8.0);     // 3.0
let log2_1024 = log2(1024.0);    // 10.0

// Logarithm base 10
let log10_result = log10(100.0); // 2.0
let log10_1000 = log10(1000.0);  // 3.0

// Logarithm of any base
fn log_base(x: float, base: float) -> float {
    ln(x) / ln(base)
}

let log_3_27 = log_base(27.0, 3.0);  // 3.0

// Log(1 + x) - more accurate for small x
let ln1p_small = ln1p(0.001);    // More accurate than ln(1.001)

// Log base 2 of (1 + x)
let log2_1p = log2(1.0 + x);     // Can use ln1p for better accuracy
```

### Exponential Functions

```rust
// Exponential functions
let exp_1 = exp(1.0);            // e
let exp_0 = exp(0.0);            // 1.0

// Exponential base 2
let exp2_3 = exp2(3.0);          // 8.0
let exp2_10 = exp2(10.0);        // 1024.0

// Exponential minus 1 (for small values)
let expm1_0 = expm1(0.0);        // 0.0
let expm1_small = expm1(0.001);  // ≈ 0.001

// Compound interest calculation
fn compound_interest(principal: float, rate: float, time: float) -> float {
    principal * exp(rate * time)
}

// Continuous compound interest
fn continuous_compound(principal: float, rate: float, time: float, n: float) -> float {
    principal * (1.0 + rate / n).powf(n * time)
}
```

## Rounding and Truncation

### Basic Rounding

```rust
// Round to nearest integer
let rounded = round(3.7);        // 4.0
let rounded_neg = round(-3.7);   // -4.0
let rounded_half = round(3.5);   // 4.0 (rounds away from zero)

// Floor (round down)
let floored = floor(3.7);        // 3.0
let floored_neg = floor(-3.7);   // -4.0

// Ceiling (round up)  
let ceiled = ceil(3.2);          // 4.0
let ceiled_neg = ceil(-3.2);     // -3.0

// Truncate (round toward zero)
let truncated = trunc(3.7);      // 3.0
let truncated_neg = trunc(-3.7); // -3.0

// Fractional part
let fract_pos = fract(3.7);      // 0.7
let fract_neg = fract(-3.7);     // -0.7
```

### Precision Rounding

```rust
// Round to decimal places
fn round_to_places(x: float, places: int) -> float {
    let factor = 10.0_f64.powi(places);
    round(x * factor) / factor
}

let rounded_2dp = round_to_places(3.14159, 2);  // 3.14
let rounded_4dp = round_to_places(PI, 4);       // 3.1416

// Round to significant figures
fn round_to_significant(x: float, figures: int) -> float {
    if x == 0.0 {
        return 0.0;
    }
    
    let magnitude = floor(log10(abs(x)));
    let factor = 10.0_f64.powi(figures - 1 - magnitude as int);
    round(x * factor) / factor
}

let sig_figs = round_to_significant(123.456, 3);  // 123.0

// Round to nearest multiple
fn round_to_multiple(x: float, multiple: float) -> float {
    round(x / multiple) * multiple
}

let rounded_5 = round_to_multiple(23.0, 5.0);     // 25.0
let rounded_quarter = round_to_multiple(3.7, 0.25); // 3.75
```

## Comparison Functions

### Floating Point Comparison

```rust
// Exact equality (rarely useful for floats)
let exact_equal = (a == b);

// Approximate equality
fn approx_equal(a: float, b: float, epsilon: float) -> bool {
    abs(a - b) < epsilon
}

let almost_equal = approx_equal(0.1 + 0.2, 0.3, EPSILON);

// Relative comparison
fn relative_equal(a: float, b: float, tolerance: float) -> bool {
    if a == b {
        return true;  // Handle infinities and exact matches
    }
    
    let diff = abs(a - b);
    let largest = max(abs(a), abs(b));
    diff <= tolerance * largest
}

// ULP (Units in the Last Place) comparison
fn ulp_equal(a: float, b: float, max_ulps: int) -> bool {
    // Implementation depends on bit representation
    // More complex but most accurate for floating point
}

// Safe comparison functions
fn safe_min(a: float, b: float) -> float {
    if a.is_nan() || b.is_nan() {
        NAN
    } else if a <= b {
        a
    } else {
        b
    }
}

fn safe_max(a: float, b: float) -> float {
    if a.is_nan() || b.is_nan() {
        NAN
    } else if a >= b {
        a
    } else {
        b
    }
}
```

### Ordering and Sorting

```rust
// Partial ordering for floats (handles NaN)
fn compare_floats(a: float, b: float) -> Option<Ordering> {
    a.partial_cmp(&b)
}

// Total ordering (NaN sorted to end)
fn total_cmp_floats(a: float, b: float) -> Ordering {
    match (a.is_nan(), b.is_nan()) {
        (true, true) => Ordering::Equal,
        (true, false) => Ordering::Greater,
        (false, true) => Ordering::Less,
        (false, false) => a.partial_cmp(&b).unwrap(),
    }
}

// Sort array of floats
fn sort_floats(arr: &mut [float]) {
    arr.sort_by(|a, b| total_cmp_floats(*a, *b));
}

// Find median (requires sorted array)
fn median(sorted_arr: &[float]) -> float {
    let len = sorted_arr.len();
    if len % 2 == 0 {
        (sorted_arr[len / 2 - 1] + sorted_arr[len / 2]) / 2.0
    } else {
        sorted_arr[len / 2]
    }
}
```

## Statistical Functions

### Basic Statistics

```rust
// Sum
fn sum(values: &[float]) -> float {
    values.iter().sum()
}

// Mean (average)
fn mean(values: &[float]) -> Option<float> {
    if values.is_empty() {
        None
    } else {
        Some(sum(values) / values.len() as float)
    }
}

// Variance
fn variance(values: &[float]) -> Option<float> {
    let mean_val = mean(values)?;
    let sum_squares = values.iter()
        .map(|x| (x - mean_val).powi(2))
        .sum::<float>();
    Some(sum_squares / values.len() as float)
}

// Standard deviation
fn std_dev(values: &[float]) -> Option<float> {
    variance(values).map(|v| v.sqrt())
}

// Sample variance (Bessel's correction)
fn sample_variance(values: &[float]) -> Option<float> {
    if values.len() <= 1 {
        None
    } else {
        let mean_val = mean(values)?;
        let sum_squares = values.iter()
            .map(|x| (x - mean_val).powi(2))
            .sum::<float>();
        Some(sum_squares / (values.len() - 1) as float)
    }
}

// Sample standard deviation
fn sample_std_dev(values: &[float]) -> Option<float> {
    sample_variance(values).map(|v| v.sqrt())
}
```

### Advanced Statistics

```rust
// Geometric mean
fn geometric_mean(values: &[float]) -> Option<float> {
    if values.is_empty() || values.iter().any(|&x| x <= 0.0) {
        None
    } else {
        let log_sum = values.iter().map(|x| ln(*x)).sum::<float>();
        Some(exp(log_sum / values.len() as float))
    }
}

// Harmonic mean
fn harmonic_mean(values: &[float]) -> Option<float> {
    if values.is_empty() || values.iter().any(|&x| x == 0.0) {
        None
    } else {
        let reciprocal_sum = values.iter().map(|x| 1.0 / x).sum::<float>();
        Some(values.len() as float / reciprocal_sum)
    }
}

// Root mean square
fn rms(values: &[float]) -> Option<float> {
    if values.is_empty() {
        None
    } else {
        let sum_squares = values.iter().map(|x| x * x).sum::<float>();
        Some((sum_squares / values.len() as float).sqrt())
    }
}

// Percentile
fn percentile(sorted_values: &[float], p: float) -> Option<float> {
    if sorted_values.is_empty() || p < 0.0 || p > 100.0 {
        None
    } else {
        let index = (p / 100.0) * (sorted_values.len() - 1) as float;
        let lower = index.floor() as usize;
        let upper = index.ceil() as usize;
        
        if lower == upper {
            Some(sorted_values[lower])
        } else {
            let weight = index - lower as float;
            Some(sorted_values[lower] * (1.0 - weight) + 
                 sorted_values[upper] * weight)
        }
    }
}

// Correlation coefficient
fn correlation(x: &[float], y: &[float]) -> Option<float> {
    if x.len() != y.len() || x.is_empty() {
        return None;
    }
    
    let mean_x = mean(x)?;
    let mean_y = mean(y)?;
    
    let numerator = x.iter().zip(y.iter())
        .map(|(xi, yi)| (xi - mean_x) * (yi - mean_y))
        .sum::<float>();
    
    let sum_sq_x = x.iter().map(|xi| (xi - mean_x).powi(2)).sum::<float>();
    let sum_sq_y = y.iter().map(|yi| (yi - mean_y).powi(2)).sum::<float>();
    
    let denominator = (sum_sq_x * sum_sq_y).sqrt();
    
    if denominator == 0.0 {
        None
    } else {
        Some(numerator / denominator)
    }
}
```

## Random Number Generation

### Basic Random Functions

```rust
// Random float in [0, 1)
let random_float = random();

// Random in range [min, max)
fn random_range(min: float, max: float) -> float {
    min + random() * (max - min)
}

// Random integer in range [min, max]
fn random_int_range(min: int, max: int) -> int {
    min + (random() * (max - min + 1) as float) as int
}

// Random boolean
fn random_bool() -> bool {
    random() < 0.5
}

// Random boolean with probability
fn random_bool_prob(probability: float) -> bool {
    random() < probability
}

// Random choice from array
fn random_choice<T: Clone>(items: &[T]) -> Option<T> {
    if items.is_empty() {
        None
    } else {
        let index = random_int_range(0, items.len() as int - 1) as usize;
        Some(items[index].clone())
    }
}
```

### Distribution Functions

```rust
// Normal (Gaussian) distribution using Box-Muller transform
fn random_normal(mean: float, std_dev: float) -> float {
    static mut CACHED: Option<float> = None;
    static mut HAS_CACHED: bool = false;
    
    unsafe {
        if HAS_CACHED {
            HAS_CACHED = false;
            return mean + CACHED.unwrap() * std_dev;
        }
        
        let u1 = random();
        let u2 = random();
        
        let mag = std_dev * (-2.0 * ln(u1)).sqrt();
        let z0 = mag * cos(TAU * u2);
        let z1 = mag * sin(TAU * u2);
        
        CACHED = Some(z1);
        HAS_CACHED = true;
        
        mean + z0
    }
}

// Exponential distribution
fn random_exponential(lambda: float) -> float {
    -ln(1.0 - random()) / lambda
}

// Uniform distribution in range
fn random_uniform(min: float, max: float) -> float {
    min + random() * (max - min)
}

// Discrete uniform (integer) distribution
fn random_discrete_uniform(min: int, max: int) -> int {
    min + (random() * (max - min + 1) as float).floor() as int
}

// Poisson distribution (approximation for small lambda)
fn random_poisson(lambda: float) -> int {
    let l = exp(-lambda);
    let mut k = 0;
    let mut p = 1.0;
    
    while p > l {
        k += 1;
        p *= random();
    }
    
    k - 1
}
```

### Seeded Random Numbers

```rust
struct Random {
    state: u64,
}

impl Random {
    fn new(seed: u64) -> Random {
        Random { state: seed }
    }
    
    fn next(&mut self) -> float {
        // Linear congruential generator
        self.state = (self.state.wrapping_mul(1103515245).wrapping_add(12345)) & 0x7fffffff;
        self.state as float / 0x7fffffff as float
    }
    
    fn next_range(&mut self, min: float, max: float) -> float {
        min + self.next() * (max - min)
    }
    
    fn next_int(&mut self, min: int, max: int) -> int {
        min + (self.next() * (max - min + 1) as float) as int
    }
}

// Usage
let mut rng = Random::new(12345);
let value1 = rng.next();
let value2 = rng.next_range(1.0, 10.0);
let dice_roll = rng.next_int(1, 6);
```

## Number Conversion

### Type Conversions

```rust
// Float to integer (truncation)
let float_val = 3.7;
let int_val = float_val as int;     // 3

// Safe float to integer conversion
fn float_to_int_safe(f: float) -> Option<int> {
    if f >= int::MIN as float && f <= int::MAX as float && f.is_finite() {
        Some(f as int)
    } else {
        None
    }
}

// Rounding conversions
let rounded_int = round(3.7) as int;    // 4
let floored_int = floor(3.7) as int;    // 3
let ceiled_int = ceil(3.2) as int;      // 4

// Integer to float (always safe)
let int_to_float = 42 as float;         // 42.0

// String conversions
let float_from_str = "3.14".parse::<float>().unwrap();
let int_from_str = "42".parse::<int>().unwrap();

// Safe parsing
fn parse_float_safe(s: &str) -> Option<float> {
    s.parse().ok()
}

fn parse_int_safe(s: &str) -> Option<int> {
    s.parse().ok()
}
```

### Base Conversions

```rust
// Integer to different bases
fn to_binary(n: int) -> string {
    format!("{:b}", n)
}

fn to_hex(n: int) -> string {
    format!("{:x}", n)
}

fn to_octal(n: int) -> string {
    format!("{:o}", n)
}

// Parse from different bases
fn from_binary(s: &str) -> Option<int> {
    int::from_str_radix(s, 2).ok()
}

fn from_hex(s: &str) -> Option<int> {
    int::from_str_radix(s, 16).ok()
}

fn from_octal(s: &str) -> Option<int> {
    int::from_str_radix(s, 8).ok()
}

// Arbitrary base conversion
fn to_base(mut n: int, base: int) -> string {
    if n == 0 {
        return "0".to_string();
    }
    
    let digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let mut result = String::new();
    
    while n > 0 {
        let digit = (n % base) as usize;
        result.insert(0, digits.chars().nth(digit).unwrap());
        n /= base;
    }
    
    result
}

fn from_base(s: &str, base: int) -> Option<int> {
    int::from_str_radix(s, base as u32).ok()
}
```

## Precision and Limits

### Floating Point Precision

```rust
// Machine epsilon (smallest difference)
const MACHINE_EPSILON: float = float::EPSILON;

// Test for near-zero
fn is_near_zero(x: float, tolerance: float) -> bool {
    abs(x) < tolerance
}

// Relative error calculation
fn relative_error(computed: float, exact: float) -> float {
    if exact == 0.0 {
        abs(computed)
    } else {
        abs(computed - exact) / abs(exact)
    }
}

// ULP (Unit in the Last Place) difference
fn ulp_distance(a: float, b: float) -> u64 {
    // Implementation requires bit manipulation
    // Measures floating-point "steps" between numbers
}

// Check for representable number
fn is_representable(x: float) -> bool {
    x.is_finite() && !x.is_nan()
}

// Next representable number
fn next_float(x: float) -> float {
    // Implementation requires bit manipulation
    // Returns the next larger representable float
}

fn prev_float(x: float) -> float {
    // Returns the next smaller representable float
}
```

### Numeric Limits

```rust
// Integer limits
const INT_MIN: int = int::MIN;
const INT_MAX: int = int::MAX;

// Float limits
const FLOAT_MIN: float = float::MIN;
const FLOAT_MAX: float = float::MAX;
const FLOAT_MIN_POSITIVE: float = float::MIN_POSITIVE;

// Safe arithmetic operations
fn safe_add(a: int, b: int) -> Option<int> {
    a.checked_add(b)
}

fn safe_multiply(a: int, b: int) -> Option<int> {
    a.checked_mul(b)
}

fn safe_subtract(a: int, b: int) -> Option<int> {
    a.checked_sub(b)
}

fn safe_divide(a: int, b: int) -> Option<int> {
    a.checked_div(b)
}

// Overflow detection
fn would_overflow_add(a: int, b: int) -> bool {
    safe_add(a, b).is_none()
}

// Saturating arithmetic (clamps at limits)
fn saturating_add(a: int, b: int) -> int {
    a.saturating_add(b)
}
```

## Complex Numbers

### Complex Number Operations

```rust
struct Complex {
    real: float,
    imag: float,
}

impl Complex {
    fn new(real: float, imag: float) -> Complex {
        Complex { real, imag }
    }
    
    fn magnitude(&self) -> float {
        hypot(self.real, self.imag)
    }
    
    fn phase(&self) -> float {
        atan2(self.imag, self.real)
    }
    
    fn conjugate(&self) -> Complex {
        Complex::new(self.real, -self.imag)
    }
    
    fn add(&self, other: &Complex) -> Complex {
        Complex::new(self.real + other.real, self.imag + other.imag)
    }
    
    fn multiply(&self, other: &Complex) -> Complex {
        Complex::new(
            self.real * other.real - self.imag * other.imag,
            self.real * other.imag + self.imag * other.real
        )
    }
    
    fn divide(&self, other: &Complex) -> Option<Complex> {
        let denom = other.real * other.real + other.imag * other.imag;
        if denom == 0.0 {
            None
        } else {
            Some(Complex::new(
                (self.real * other.real + self.imag * other.imag) / denom,
                (self.imag * other.real - self.real * other.imag) / denom
            ))
        }
    }
    
    fn exp(&self) -> Complex {
        let r = exp(self.real);
        Complex::new(r * cos(self.imag), r * sin(self.imag))
    }
    
    fn ln(&self) -> Complex {
        Complex::new(ln(self.magnitude()), self.phase())
    }
    
    fn pow(&self, exponent: &Complex) -> Complex {
        // z^w = exp(w * ln(z))
        let ln_z = self.ln();
        let product = exponent.multiply(&ln_z);
        product.exp()
    }
}

// Polar form constructor
fn complex_polar(magnitude: float, phase: float) -> Complex {
    Complex::new(magnitude * cos(phase), magnitude * sin(phase))
}

// Common complex numbers
const I: Complex = Complex { real: 0.0, imag: 1.0 };
const ONE: Complex = Complex { real: 1.0, imag: 0.0 };
const ZERO: Complex = Complex { real: 0.0, imag: 0.0 };
```

### Complex Functions

```rust
// Complex trigonometric functions
impl Complex {
    fn sin(&self) -> Complex {
        // sin(z) = (e^(iz) - e^(-iz)) / (2i)
        let iz = Complex::new(-self.imag, self.real);
        let exp_iz = iz.exp();
        let exp_neg_iz = iz.conjugate().exp();
        
        let numerator = exp_iz.add(&exp_neg_iz.conjugate());
        Complex::new(numerator.imag / 2.0, -numerator.real / 2.0)
    }
    
    fn cos(&self) -> Complex {
        // cos(z) = (e^(iz) + e^(-iz)) / 2
        let iz = Complex::new(-self.imag, self.real);
        let exp_iz = iz.exp();
        let exp_neg_iz = iz.conjugate().exp();
        
        let sum = exp_iz.add(&exp_neg_iz);
        Complex::new(sum.real / 2.0, sum.imag / 2.0)
    }
    
    fn tan(&self) -> Complex {
        let sin_z = self.sin();
        let cos_z = self.cos();
        sin_z.divide(&cos_z).unwrap_or(Complex::new(NAN, NAN))
    }
    
    fn sinh(&self) -> Complex {
        // sinh(z) = (e^z - e^(-z)) / 2
        let exp_z = self.exp();
        let exp_neg_z = self.conjugate().exp();
        
        let diff = exp_z.add(&exp_neg_z.conjugate());
        Complex::new(diff.real / 2.0, diff.imag / 2.0)
    }
    
    fn cosh(&self) -> Complex {
        // cosh(z) = (e^z + e^(-z)) / 2
        let exp_z = self.exp();
        let exp_neg_z = self.conjugate().exp();
        
        let sum = exp_z.add(&exp_neg_z);
        Complex::new(sum.real / 2.0, sum.imag / 2.0)
    }
}
```

## Mathematical Utilities

### Numerical Methods

```rust
// Newton's method for finding roots
fn newton_raphson<F, G>(f: F, df: G, initial: float, tolerance: float, max_iterations: int) -> Option<float>
where
    F: Fn(float) -> float,
    G: Fn(float) -> float,
{
    let mut x = initial;
    
    for _ in 0..max_iterations {
        let fx = f(x);
        let dfx = df(x);
        
        if abs(fx) < tolerance {
            return Some(x);
        }
        
        if abs(dfx) < EPSILON {
            return None; // Derivative too small
        }
        
        x = x - fx / dfx;
    }
    
    None // Did not converge
}

// Numerical integration (Simpson's rule)
fn simpson_integration<F>(f: F, a: float, b: float, n: int) -> float
where
    F: Fn(float) -> float,
{
    let h = (b - a) / n as float;
    let mut sum = f(a) + f(b);
    
    for i in 1..n {
        let x = a + i as float * h;
        let coefficient = if i % 2 == 0 { 2.0 } else { 4.0 };
        sum += coefficient * f(x);
    }
    
    sum * h / 3.0
}

// Numerical differentiation
fn numerical_derivative<F>(f: F, x: float, h: float) -> float
where
    F: Fn(float) -> float,
{
    (f(x + h) - f(x - h)) / (2.0 * h)
}

// Factorial (with overflow protection)
fn factorial(n: int) -> Option<int> {
    if n < 0 {
        return None;
    }
    
    let mut result = 1;
    for i in 2..=n {
        result = result.checked_mul(i)?;
    }
    Some(result)
}

// Factorial using float (for large numbers)
fn factorial_float(n: int) -> float {
    if n < 0 {
        return NAN;
    }
    
    let mut result = 1.0;
    for i in 2..=n {
        result *= i as float;
    }
    result
}

// Stirling's approximation for factorial
fn stirling_factorial(n: int) -> float {
    if n <= 0 {
        return 1.0;
    }
    
    let n_f = n as float;
    sqrt(2.0 * PI * n_f) * (n_f / E).powf(n_f)
}
```

### Combinatorics

```rust
// Binomial coefficient "n choose k"
fn binomial_coefficient(n: int, k: int) -> Option<int> {
    if k > n || k < 0 {
        return Some(0);
    }
    
    if k == 0 || k == n {
        return Some(1);
    }
    
    // Use symmetry: C(n,k) = C(n,n-k)
    let k = if k > n - k { n - k } else { k };
    
    let mut result = 1;
    for i in 0..k {
        result = result.checked_mul(n - i)?;
        result = result.checked_div(i + 1)?;
    }
    
    Some(result)
}

// Binomial coefficient using floats
fn binomial_coefficient_float(n: int, k: int) -> float {
    if k > n || k < 0 {
        return 0.0;
    }
    
    if k == 0 || k == n {
        return 1.0;
    }
    
    // Use log gamma function for large numbers
    exp(ln_gamma(n + 1) - ln_gamma(k + 1) - ln_gamma(n - k + 1))
}

// Gamma function (extends factorial to real numbers)
fn gamma(x: float) -> float {
    // Stirling's approximation for large x
    if x > 12.0 {
        return sqrt(2.0 * PI / x) * (x / E).powf(x);
    }
    
    // Use recurrence relation: Γ(x+1) = x * Γ(x)
    if x < 1.0 {
        return gamma(x + 1.0) / x;
    }
    
    // Polynomial approximation for 1 <= x <= 2
    // (Simplified version)
    1.0 // Actual implementation would use more sophisticated approximation
}

// Log gamma function (more numerically stable)
fn ln_gamma(x: float) -> float {
    // Stirling's approximation
    if x > 12.0 {
        return (x - 0.5) * ln(x) - x + 0.5 * ln(2.0 * PI);
    }
    
    // Use recurrence for small x
    if x < 1.0 {
        return ln_gamma(x + 1.0) - ln(x);
    }
    
    // Polynomial approximation
    0.0 // Actual implementation would use series expansion
}

// Permutations P(n,k) = n!/(n-k)!
fn permutations(n: int, k: int) -> Option<int> {
    if k > n || k < 0 {
        return Some(0);
    }
    
    let mut result = 1;
    for i in 0..k {
        result = result.checked_mul(n - i)?;
    }
    
    Some(result)
}
```

## Performance Considerations

### Optimization Tips

```rust
// Use integer arithmetic when possible
// Bad: slow floating point
fn slow_average(a: int, b: int) -> float {
    (a as float + b as float) / 2.0
}

// Good: fast integer arithmetic
fn fast_average(a: int, b: int) -> int {
    a / 2 + b / 2 + (a % 2 + b % 2) / 2
}

// Avoid repeated expensive operations
// Bad: recalculates expensive function
fn compute_series_bad(x: float, n: int) -> float {
    let mut sum = 0.0;
    for i in 1..=n {
        sum += sin(x * i as float);  // Expensive sin() call
    }
    sum
}

// Good: minimize expensive operations
fn compute_series_good(x: float, n: int) -> float {
    let mut sum = 0.0;
    let sin_x = sin(x);
    let cos_x = cos(x);
    
    let mut sin_ix = 0.0;
    let mut cos_ix = 1.0;
    
    for _ in 1..=n {
        // Use trigonometric identities instead of calling sin()
        let new_sin = sin_ix * cos_x + cos_ix * sin_x;
        let new_cos = cos_ix * cos_x - sin_ix * sin_x;
        sin_ix = new_sin;
        cos_ix = new_cos;
        sum += sin_ix;
    }
    sum
}

// Use appropriate precision
// When float precision is sufficient, avoid extra precision
fn efficient_calculation(data: &[float]) -> float {
    // Use f32 if precision allows, it's faster
    data.iter().map(|&x| x.sqrt()).sum()
}

// Cache expensive calculations
struct MathCache {
    sqrt_cache: HashMap<int, float>,
    sin_cache: HashMap<int, float>,
}

impl MathCache {
    fn cached_sqrt(&mut self, x: int) -> float {
        *self.sqrt_cache.entry(x).or_insert_with(|| (x as float).sqrt())
    }
    
    fn cached_sin(&mut self, angle_degrees: int) -> float {
        *self.sin_cache.entry(angle_degrees)
            .or_insert_with(|| sin(degrees_to_radians(angle_degrees as float)))
    }
}
```

### SIMD and Vectorization

```rust
// Process arrays in chunks for better cache performance
fn sum_large_array(data: &[float]) -> float {
    const CHUNK_SIZE: usize = 1024;
    
    data.chunks(CHUNK_SIZE)
        .map(|chunk| chunk.iter().sum::<float>())
        .sum()
}

// Parallel processing for independent calculations
fn parallel_sqrt(data: &[float]) -> Vec<float> {
    use rayon::prelude::*;
    
    data.par_iter()
        .map(|&x| x.sqrt())
        .collect()
}

// Fused multiply-add when available
fn fma_calculation(a: float, b: float, c: float) -> float {
    // a * b + c computed with higher precision
    a.mul_add(b, c)  // If available
}
```

## JavaScript/Node.js Interop

When transpiled to JavaScript, math functions map to JavaScript's Math object:

```rust
// Husk
let result = sqrt(16.0);
let angle = sin(PI / 2.0);
let power = pow(2.0, 8.0);
let random_val = random();
```

Becomes:
```javascript
// JavaScript
const result = Math.sqrt(16.0);
const angle = Math.sin(Math.PI / 2.0);
const power = Math.pow(2.0, 8.0);
const random_val = Math.random();
```

### API Mappings

| Husk Function | JavaScript Equivalent |
|---------------|----------------------|
| `sqrt()` | `Math.sqrt()` |
| `sin()` | `Math.sin()` |
| `cos()` | `Math.cos()` |
| `tan()` | `Math.tan()` |
| `asin()` | `Math.asin()` |
| `acos()` | `Math.acos()` |
| `atan()` | `Math.atan()` |
| `atan2()` | `Math.atan2()` |
| `exp()` | `Math.exp()` |
| `ln()` | `Math.log()` |
| `log10()` | `Math.log10()` |
| `log2()` | `Math.log2()` |
| `pow()` | `Math.pow()` |
| `abs()` | `Math.abs()` |
| `floor()` | `Math.floor()` |
| `ceil()` | `Math.ceil()` |
| `round()` | `Math.round()` |
| `min()` | `Math.min()` |
| `max()` | `Math.max()` |
| `random()` | `Math.random()` |
| `PI` | `Math.PI` |
| `E` | `Math.E` |

### Precision Differences

```rust
// Husk uses 64-bit floats consistently
// JavaScript uses 64-bit floats (IEEE-754 double precision)
// Generally compatible, but watch for:

// Very large integers
let large_int = 9007199254740992;  // 2^53, may lose precision

// Very small differences
let diff = 0.1 + 0.2 - 0.3;  // Not exactly zero in both languages

// Special values
let nan = NAN;
let inf = INFINITY;
let neg_inf = NEG_INFINITY;
```

## Related Topics

- [Data Types](../language/data-types.md#numeric-types) - Numeric type details
- [Operators](../language/operators.md) - Arithmetic operators
- [Arrays](arrays.md) - Array mathematical operations
- [Error Handling](../language/error-handling.md) - Safe mathematical operations
- [Performance](../advanced/performance.md) - Mathematical optimization

---

*Mathematical functions are essential for scientific computing, graphics, statistics, and many other applications. Husk provides a complete, efficient, and safe mathematical library.*