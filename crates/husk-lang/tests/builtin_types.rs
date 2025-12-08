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
    let alice: i32 = scores.get("Alice");
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
