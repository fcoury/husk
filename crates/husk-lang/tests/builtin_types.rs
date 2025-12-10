//! Tests for built-in types: Map<K, V>, Set<T>, Range, and their methods.

use std::collections::HashSet;

use husk_codegen_js::{JsTarget, lower_file_to_js};
use husk_parser::parse_str;
use husk_semantic::SemanticOptions;

/// Helper to parse, analyze, and generate JS from Husk source code.
fn compile_to_js(source: &str) -> String {
    let result = parse_str(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let file = result.file.expect("no file parsed");
    let sem = husk_semantic::analyze_file_with_options(
        &file,
        SemanticOptions {
            prelude: true,
            cfg_flags: HashSet::new(),
        },
    );

    assert!(
        sem.symbols.errors.is_empty() && sem.type_errors.is_empty(),
        "semantic errors: symbols={:?}, types={:?}",
        sem.symbols.errors,
        sem.type_errors
    );

    let module = lower_file_to_js(
        &file,
        true,
        JsTarget::Cjs,
        &sem.name_resolution,
        &sem.type_resolution,
        &sem.variant_calls,
        &sem.variant_patterns,
    );
    module.to_source()
}

/// Helper to check that source parses and type-checks without errors.
fn assert_type_checks(source: &str) {
    let result = parse_str(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let file = result.file.expect("no file parsed");
    let sem = husk_semantic::analyze_file_with_options(
        &file,
        SemanticOptions {
            prelude: true,
            cfg_flags: HashSet::new(),
        },
    );

    assert!(
        sem.symbols.errors.is_empty() && sem.type_errors.is_empty(),
        "semantic errors: symbols={:?}, types={:?}",
        sem.symbols.errors,
        sem.type_errors
    );
}

// =============================================================================
// Map<K, V> Tests
// =============================================================================

#[test]
fn map_new_generates_helper_call() {
    let source = r#"
fn main() {
    let m: Map<String, i32> = Map::new();
}
"#;
    let js = compile_to_js(source);
    assert!(
        js.contains("__husk_map_new()"),
        "Expected Map::new() to generate __husk_map_new(). Got:\n{}",
        js
    );
}

#[test]
fn map_methods_type_check() {
    let source = r#"
fn main() {
    let scores: Map<String, i32> = Map::new();
    scores.set("Alice", 100);
    let alice: Option<i32> = scores.get("Alice");
    let has: bool = scores.has("Alice");
    let deleted: bool = scores.delete("Alice");
    scores.clear();
}
"#;
    assert_type_checks(source);
}

#[test]
fn map_len_generates_size_property() {
    let source = r#"
fn main() {
    let m: Map<String, i32> = Map::new();
    let size = m.len();
}
"#;
    let js = compile_to_js(source);
    // Map.len() should become .size (property access, not method call)
    assert!(
        js.contains("m.size"),
        "Expected Map.len() to generate .size property access. Got:\n{}",
        js
    );
    assert!(
        !js.contains("m.size()"),
        "Expected .size as property, not method call. Got:\n{}",
        js
    );
}

#[test]
fn map_keys_generates_helper_call() {
    let source = r#"
fn main() {
    let m: Map<String, i32> = Map::new();
    let keys: [String] = m.keys();
}
"#;
    let js = compile_to_js(source);
    assert!(
        js.contains("__husk_map_keys(m)"),
        "Expected Map.keys() to generate __husk_map_keys(m). Got:\n{}",
        js
    );
}

#[test]
fn map_values_generates_helper_call() {
    let source = r#"
fn main() {
    let m: Map<String, i32> = Map::new();
    let values: [i32] = m.values();
}
"#;
    let js = compile_to_js(source);
    assert!(
        js.contains("__husk_map_values(m)"),
        "Expected Map.values() to generate __husk_map_values(m). Got:\n{}",
        js
    );
}

#[test]
fn map_keys_and_values_return_correct_types() {
    let source = r#"
fn main() {
    let m: Map<String, i32> = Map::new();
    let keys: [String] = m.keys();
    let values: [i32] = m.values();

    // These should work since keys is [String] and values is [i32]
    let keys_len: i32 = keys.len();
    let values_len: i32 = values.len();
}
"#;
    assert_type_checks(source);
}

// =============================================================================
// Set<T> Tests
// =============================================================================

#[test]
fn set_len_generates_size_property() {
    let source = r#"
fn main() {
    let s: Set<i32> = Set::new();
    let size = s.len();
}
"#;
    let js = compile_to_js(source);
    // Set.len() should become .size (property access, not method call)
    assert!(
        js.contains("s.size"),
        "Expected Set.len() to generate .size property access. Got:\n{}",
        js
    );
    assert!(
        !js.contains("s.size()"),
        "Expected .size as property, not method call. Got:\n{}",
        js
    );
}

#[test]
fn set_values_generates_helper_call() {
    let source = r#"
fn main() {
    let s: Set<i32> = Set::new();
    let values: [i32] = s.values();
}
"#;
    let js = compile_to_js(source);
    assert!(
        js.contains("__husk_set_values(s)"),
        "Expected Set.values() to generate __husk_set_values(s). Got:\n{}",
        js
    );
}

// =============================================================================
// String and Array .len() Tests
// =============================================================================

#[test]
fn string_len_generates_length_property() {
    let source = r#"
fn main() {
    let s = "hello";
    let len = s.len();
}
"#;
    let js = compile_to_js(source);
    // String.len() should become .length (property access, not method call)
    assert!(
        js.contains("s.length"),
        "Expected String.len() to generate .length property access. Got:\n{}",
        js
    );
    assert!(
        !js.contains("s.length()"),
        "Expected .length as property, not method call. Got:\n{}",
        js
    );
}

#[test]
fn array_len_generates_length_property() {
    let source = r#"
fn main() {
    let arr = [1, 2, 3];
    let len = arr.len();
}
"#;
    let js = compile_to_js(source);
    // Array.len() should become .length (property access, not method call)
    assert!(
        js.contains("arr.length"),
        "Expected Array.len() to generate .length property access. Got:\n{}",
        js
    );
    assert!(
        !js.contains("arr.length()"),
        "Expected .length as property, not method call. Got:\n{}",
        js
    );
}

#[test]
fn array_from_map_keys_has_length_property() {
    // Regression test: arrays returned from Map.keys() should have .length as property
    let source = r#"
fn main() {
    let m: Map<String, i32> = Map::new();
    let keys = m.keys();
    let count = keys.len();
}
"#;
    let js = compile_to_js(source);
    assert!(
        js.contains("keys.length"),
        "Expected keys.len() to generate .length property access. Got:\n{}",
        js
    );
    assert!(
        !js.contains("keys.length()"),
        "Expected .length as property, not method call. Got:\n{}",
        js
    );
}

// =============================================================================
// Range Field Access Tests
// =============================================================================

#[test]
fn range_start_field_type_checks() {
    let source = r#"
fn main() {
    let r = 1..10;
    let start: i32 = r.start;
}
"#;
    assert_type_checks(source);
}

#[test]
fn range_end_field_type_checks() {
    let source = r#"
fn main() {
    let r = 1..10;
    let end: i32 = r.end;
}
"#;
    assert_type_checks(source);
}

#[test]
fn range_fields_can_be_compared() {
    let source = r#"
fn main() {
    let r = 5..10;
    if r.end > r.start {
        println("valid range");
    }
}
"#;
    assert_type_checks(source);
}

#[test]
fn range_fields_can_be_compared_with_variables() {
    let source = r#"
fn main() {
    let r = 5..10;
    let max = 8;
    let min = 3;

    if r.end > max {
        println("exceeds max");
    }
    if r.start < min {
        println("below min");
    }
}
"#;
    assert_type_checks(source);
}

#[test]
fn range_fields_generate_property_access() {
    let source = r#"
fn main() {
    let r = 1..10;
    let s = r.start;
    let e = r.end;
}
"#;
    let js = compile_to_js(source);
    assert!(
        js.contains("r.start"),
        "Expected range.start to generate property access. Got:\n{}",
        js
    );
    assert!(
        js.contains("r.end"),
        "Expected range.end to generate property access. Got:\n{}",
        js
    );
}

#[test]
fn inclusive_range_fields_type_check() {
    let source = r#"
fn main() {
    let r = 1..=10;
    let start: i32 = r.start;
    let end: i32 = r.end;
}
"#;
    assert_type_checks(source);
}

#[test]
fn inclusive_flag_type_checks() {
    let source = r#"
fn main() {
    let r = 1..=10;
    let closed: bool = r.inclusive;
}
"#;
    assert_type_checks(source);
}

#[test]
fn range_object_includes_inclusive_flag_in_js() {
    let source = r#"
fn main() {
    let a = 1..5;
    let b = 1..=5;
}
"#;
    let js = compile_to_js(source);
    assert!(
        js.contains("inclusive: false"),
        "exclusive range should emit inclusive: false. Got:\n{}",
        js
    );
    assert!(
        js.contains("inclusive: true"),
        "inclusive range should emit inclusive: true. Got:\n{}",
        js
    );
}

// =============================================================================
// If expressions
// =============================================================================

#[test]
fn if_expression_type_checks() {
    let source = r#"
fn main() {
    let x: i32 = if true { 1 } else { 2 };
    let y: String = if false { "a" } else { "b" };
}
"#;
    assert_type_checks(source);
}

#[test]
fn if_expression_codegen_uses_ternary() {
    let source = r#"
fn main() {
    let x = if 1 < 2 { 10 } else { 20 };
}
"#;
    let js = compile_to_js(source);
    // If-expressions should generate a ternary operator pattern
    assert!(
        js.contains("1 < 2 ?") || js.contains("(1 < 2) ?"),
        "expected ternary operator in JS output, got:\n{}",
        js
    );
}

// =============================================================================
// Type-specific getter lookup (regression tests)
// =============================================================================

#[test]
fn different_types_use_correct_len_property() {
    // Regression test: Each type should use its own #[js_name] mapping
    // Map -> .size, Set -> .size, String -> .length, Array -> .length
    let source = r#"
fn main() {
    let map: Map<String, i32> = Map::new();
    let set: Set<i32> = Set::new();
    let str = "hello";
    let arr = [1, 2, 3];

    let map_len = map.len();
    let set_len = set.len();
    let str_len = str.len();
    let arr_len = arr.len();
}
"#;
    let js = compile_to_js(source);

    // Map and Set should use .size
    assert!(
        js.contains("map.size"),
        "Expected Map.len() to use .size. Got:\n{}",
        js
    );
    assert!(
        js.contains("set.size"),
        "Expected Set.len() to use .size. Got:\n{}",
        js
    );

    // String and Array should use .length
    assert!(
        js.contains("str.length"),
        "Expected String.len() to use .length. Got:\n{}",
        js
    );
    assert!(
        js.contains("arr.length"),
        "Expected Array.len() to use .length. Got:\n{}",
        js
    );
}

// =============================================================================
// i64/BigInt Tests (Regression tests for large integer support)
// =============================================================================

#[test]
fn large_integer_literal_types_as_i64() {
    // Regression test: Integer literals outside i32 range should be typed as i64
    let source = r#"
fn main() {
    let big: i64 = 68170613195522;
    let also_big: i64 = 9999999999999;
}
"#;
    assert_type_checks(source);
}

#[test]
fn large_integer_literal_generates_bigint() {
    // Regression test: Large integers should generate BigInt in JS
    let source = r#"
fn main() {
    let big = 68170613195522;
}
"#;
    let js = compile_to_js(source);
    assert!(
        js.contains("68170613195522n"),
        "Expected large integer to generate BigInt literal (with 'n' suffix). Got:\n{}",
        js
    );
}

#[test]
fn small_integer_does_not_generate_bigint() {
    // Regression test: Small integers should remain as regular JS numbers
    let source = r#"
fn main() {
    let small = 42;
}
"#;
    let js = compile_to_js(source);
    assert!(
        js.contains("42") && !js.contains("42n"),
        "Expected small integer to remain as regular number (no 'n' suffix). Got:\n{}",
        js
    );
}

#[test]
fn negative_large_integer_generates_bigint() {
    // Regression test: Large negative integers should also generate BigInt
    // Note: We need to use subtraction since unary - only works with i32
    let source = r#"
fn main() {
    let big_neg: i64 = (0 as i64) - 68170613195522;
}
"#;
    let js = compile_to_js(source);
    assert!(
        js.contains("68170613195522n"),
        "Expected large integer in subtraction to generate BigInt literal. Got:\n{}",
        js
    );
}

#[test]
fn i64_range_type_checks() {
    // Regression test: Range with i64 bounds should produce Range<i64>
    let source = r#"
fn main() {
    let r: Range<i64> = 68170613195522..=69237165933781;
    let start: i64 = r.start;
    let end: i64 = r.end;
}
"#;
    assert_type_checks(source);
}

#[test]
fn i64_range_generates_bigint_bounds() {
    // Regression test: Range with large integers should generate BigInt bounds
    let source = r#"
fn main() {
    let r = 68170613195522..=69237165933781;
}
"#;
    let js = compile_to_js(source);
    assert!(
        js.contains("68170613195522n"),
        "Expected range start to be BigInt. Got:\n{}",
        js
    );
    assert!(
        js.contains("69237165933781n"),
        "Expected range end to be BigInt. Got:\n{}",
        js
    );
}

#[test]
fn i64_range_equality_type_checks() {
    // Regression test: Range<i64> should support PartialEq
    let source = r#"
fn main() {
    let r1: Range<i64> = 68170613195522..=69237165933781;
    let r2: Range<i64> = 68170613195522..=69237165933781;
    let eq: bool = r1 == r2;
}
"#;
    assert_type_checks(source);
}

#[test]
fn parse_long_type_checks() {
    // Regression test: parse_long should return i64
    let source = r#"
fn main() {
    let n: i64 = parse_long("12345678901234");
}
"#;
    assert_type_checks(source);
}

#[test]
fn parse_long_generates_bigint_call() {
    // Regression test: parse_long should generate BigInt() call in JS
    let source = r#"
fn main() {
    let n = parse_long("12345678901234");
}
"#;
    let js = compile_to_js(source);
    assert!(
        js.contains("BigInt("),
        "Expected parse_long to generate BigInt() call. Got:\n{}",
        js
    );
}

#[test]
fn i32_range_still_works() {
    // Ensure i32 ranges still work correctly
    let source = r#"
fn main() {
    let r: Range<i32> = 1..=10;
    let start: i32 = r.start;
    let end: i32 = r.end;
}
"#;
    assert_type_checks(source);
}

#[test]
fn i32_range_does_not_generate_bigint() {
    // Ensure small range values don't generate BigInt
    let source = r#"
fn main() {
    let r = 1..=10;
}
"#;
    let js = compile_to_js(source);
    assert!(
        !js.contains("1n") && !js.contains("10n"),
        "Expected i32 range to not generate BigInt. Got:\n{}",
        js
    );
}

#[test]
fn mixed_i64_arithmetic_type_checks() {
    // Regression test: i64 arithmetic should type check
    // Note: Small literals like 1000 are i32, so we need to cast them
    let source = r#"
fn main() {
    let a: i64 = 68170613195522;
    let b: i64 = 1000 as i64;
    let sum: i64 = a + b;
    let diff: i64 = a - b;
}
"#;
    assert_type_checks(source);
}

#[test]
fn i64_comparison_type_checks() {
    // Regression test: i64 comparisons should type check
    let source = r#"
fn main() {
    let a: i64 = 68170613195522;
    let b: i64 = 69237165933781;
    let lt: bool = a < b;
    let gt: bool = a > b;
    let eq: bool = a == b;
}
"#;
    assert_type_checks(source);
}

#[test]
fn i64_cast_type_checks() {
    // Regression test: Casting to i64 should work
    let source = r#"
fn main() {
    let small: i32 = 42;
    let big: i64 = small as i64;
    let one: i64 = 1 as i64;
}
"#;
    assert_type_checks(source);
}

#[test]
fn range_i64_in_function_param_type_checks() {
    // Regression test: Functions accepting Range<i64> should type check
    let source = r#"
fn process_range(r: Range<i64>) -> i64 {
    r.end - r.start
}

fn main() {
    let r = 68170613195522..=69237165933781;
    let len = process_range(r);
}
"#;
    assert_type_checks(source);
}

#[test]
fn range_i64_array_type_checks() {
    // Regression test: Arrays of Range<i64> should type check
    let source = r#"
fn main() {
    let ranges: [Range<i64>] = [];
    ranges.push(68170613195522..=69237165933781);
    ranges.push(100000000000..=200000000000);
}
"#;
    assert_type_checks(source);
}

// =============================================================================
// Nested If Expression Tests (Regression tests for if/else in closures/blocks)
// =============================================================================

#[test]
fn nested_if_in_closure_type_checks() {
    // Regression test: Nested if/else in closure body should type check correctly
    let source = r#"
fn main() {
    let arr = [3, 1, 2];
    arr.sort(|a, b| if a < b {
        -1
    } else {
        if a > b {
            1
        } else {
            0
        }
    });
}
"#;
    assert_type_checks(source);
}

#[test]
fn nested_if_in_block_type_checks() {
    // Regression test: Nested if/else as last statement in block should return its type
    let source = r#"
fn test(x: i32) -> i32 {
    if x > 0 {
        1
    } else {
        if x < 0 {
            -1
        } else {
            0
        }
    }
}

fn main() {
    let result = test(5);
}
"#;
    assert_type_checks(source);
}

#[test]
fn if_else_chain_in_closure_type_checks() {
    // Regression test: Multiple else-if chains should type check in closures
    let source = r#"
fn main() {
    let compare = |a: i32, b: i32| if a < b {
        -1
    } else {
        if a > b {
            1
        } else {
            0
        }
    };
    let result: i32 = compare(1, 2);
}
"#;
    assert_type_checks(source);
}

#[test]
fn nested_if_generates_correct_js() {
    // Regression test: Nested if should generate proper ternary operators
    let source = r#"
fn main() {
    let arr = [3, 1, 2];
    arr.sort(|a, b| if a < b { -1 } else { if a > b { 1 } else { 0 } });
}
"#;
    let js = compile_to_js(source);
    // Should contain ternary operators for both if expressions
    assert!(
        js.contains("?") && js.contains(":"),
        "Expected nested ternary operators in JS output. Got:\n{}",
        js
    );
}
