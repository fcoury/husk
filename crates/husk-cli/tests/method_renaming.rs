//! Regression tests for method name handling in codegen.

use std::collections::HashSet;

use husk_codegen_js::{lower_file_to_js, JsTarget};
use husk_parser::parse_str;
use husk_semantic::SemanticOptions;

/// Regression test: User-defined methods with names matching stdlib extern methods
/// should NOT be renamed. For example, a user's `MyList.len()` should stay as `.len()`
/// and not be converted to `.length` (which is the JS name for `String.len()`).
#[test]
fn user_defined_method_not_renamed_to_stdlib_js_name() {
    let source = r#"
struct MyList {
    count: i32,
}

impl MyList {
    fn len(&self) -> i32 {
        self.count
    }

    fn index_of(&self, value: i32) -> i32 {
        0
    }
}

fn main() {
    let list = MyList { count: 42 };
    let length = list.len();
    let idx = list.index_of(5);
}
"#;

    let result = parse_str(source);
    assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);
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
    );
    let js = module.to_source();

    // User's MyList.len() should NOT become .length (that's for String.len())
    assert!(
        js.contains("list.len()"),
        "Expected 'list.len()' to stay as 'len', not be renamed to 'length'. Got:\n{}",
        js
    );

    // User's MyList.index_of() should NOT become .indexOf (that's for String/Array.index_of())
    assert!(
        js.contains("list.index_of("),
        "Expected 'list.index_of()' to stay as 'index_of', not be renamed to 'indexOf'. Got:\n{}",
        js
    );

    // The method definition should also use the original name
    assert!(
        js.contains("MyList.prototype.len = function()"),
        "Expected method definition to use 'len'. Got:\n{}",
        js
    );
}

/// Test that user-defined methods with snake_case names that DON'T match stdlib
/// extern methods also stay unchanged.
#[test]
fn user_defined_snake_case_method_unchanged() {
    let source = r#"
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn x_coord(&self) -> i32 {
        self.x
    }

    fn get_distance_from_origin(&self) -> i32 {
        self.x + self.y
    }
}

fn main() {
    let p = Point { x: 10, y: 20 };
    let x = p.x_coord();
    let dist = p.get_distance_from_origin();
}
"#;

    let result = parse_str(source);
    assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);
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
    );
    let js = module.to_source();

    // User methods should keep their snake_case names
    assert!(
        js.contains("p.x_coord()"),
        "Expected 'p.x_coord()' to stay unchanged. Got:\n{}",
        js
    );

    assert!(
        js.contains("p.get_distance_from_origin()"),
        "Expected 'p.get_distance_from_origin()' to stay unchanged. Got:\n{}",
        js
    );

    // Method definitions should also use original names
    assert!(
        js.contains("Point.prototype.x_coord = function()"),
        "Expected method definition to use 'x_coord'. Got:\n{}",
        js
    );

    assert!(
        js.contains("Point.prototype.get_distance_from_origin = function()"),
        "Expected method definition to use 'get_distance_from_origin'. Got:\n{}",
        js
    );
}
