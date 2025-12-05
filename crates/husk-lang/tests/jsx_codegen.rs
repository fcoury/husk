//! JSX Codegen Integration Tests
//!
//! These tests verify that Husk's JSX parsing and code generation produce
//! correct JavaScript output that follows React jsx-runtime semantics.

use husk_codegen_js::{lower_file_to_js, JsTarget};
use husk_parser::parse_str;
use husk_semantic::analyze_file;
use std::collections::HashSet;

/// Helper to compile Husk source to JavaScript
fn compile_to_js(source: &str) -> String {
    let result = parse_str(source);
    assert!(
        result.errors.is_empty(),
        "Parse errors: {:?}",
        result.errors
    );

    let file = result.file.unwrap();
    let sem = analyze_file(&file);

    // Filter by cfg attributes (none specified, so keeps all items)
    let filtered_file = husk_semantic::filter_items_by_cfg(&file, &HashSet::new());

    let module = lower_file_to_js(
        &filtered_file,
        false, // not bin mode
        JsTarget::Esm,
        &sem.name_resolution,
        &sem.type_resolution,
        &sem.variant_calls,
        &sem.variant_patterns,
    );

    module.to_source()
}

// ============================================================================
// Basic JSX Element Tests
// ============================================================================

#[test]
fn jsx_simple_element() {
    let src = r#"
fn render() -> JsValue {
    <div />
}
"#;
    let js = compile_to_js(src);
    assert!(js.contains("_jsx("), "should use _jsx function");
    assert!(js.contains("\"div\""), "should have div tag as string");
}

#[test]
fn jsx_element_with_string_attribute() {
    let src = r#"
fn render() -> JsValue {
    <div class="container" />
}
"#;
    let js = compile_to_js(src);
    assert!(js.contains("class:"), "should have class property");
    assert!(
        js.contains("\"container\""),
        "should have container value"
    );
}

#[test]
fn jsx_element_with_multiple_attributes() {
    let src = r#"
fn render() -> JsValue {
    <div id="main" class="app" />
}
"#;
    let js = compile_to_js(src);
    assert!(js.contains("id:"), "should have id property");
    assert!(js.contains("class:"), "should have class property");
}

#[test]
fn jsx_boolean_attribute() {
    let src = r#"
fn render() -> JsValue {
    <input disabled />
}
"#;
    let js = compile_to_js(src);
    assert!(js.contains("disabled:"), "should have disabled property");
    assert!(js.contains("true"), "boolean attr should be true");
}

// ============================================================================
// JSX Children Tests
// ============================================================================

#[test]
fn jsx_single_child_uses_jsx() {
    let src = r#"
fn render() -> JsValue {
    <div>Hello</div>
}
"#;
    let js = compile_to_js(src);
    assert!(js.contains("_jsx("), "single child should use _jsx");
    assert!(
        !js.contains("_jsxs("),
        "single child should NOT use _jsxs"
    );
    assert!(js.contains("children:"), "should have children property");
}

#[test]
fn jsx_multiple_children_uses_jsxs() {
    let src = r#"
fn render() -> JsValue {
    <div>
        <span>A</span>
        <span>B</span>
    </div>
}
"#;
    let js = compile_to_js(src);
    // The outer div should use _jsxs because it has multiple children
    assert!(js.contains("_jsxs("), "multiple children should use _jsxs");
}

#[test]
fn jsx_text_preserves_spacing() {
    let src = r#"
fn render() -> JsValue {
    <div>Hello, world</div>
}
"#;
    let js = compile_to_js(src);
    // Text should preserve exact spacing (not "Hello , world")
    assert!(
        js.contains("\"Hello, world\""),
        "text should preserve comma spacing: {}",
        js
    );
}

#[test]
fn jsx_text_with_colon_preserves_spacing() {
    let src = r#"
fn render() -> JsValue {
    <div>Error: 42</div>
}
"#;
    let js = compile_to_js(src);
    assert!(
        js.contains("\"Error: 42\""),
        "text should preserve colon spacing: {}",
        js
    );
}

#[test]
fn jsx_numeric_text_content() {
    let src = r#"
fn render() -> JsValue {
    <div>42</div>
}
"#;
    let js = compile_to_js(src);
    assert!(js.contains("\"42\""), "numeric content should work: {}", js);
}

#[test]
fn jsx_keyword_text_content() {
    let src = r#"
fn render() -> JsValue {
    <div>true</div>
}
"#;
    let js = compile_to_js(src);
    // The word "true" as text, not the boolean
    assert!(
        js.contains("\"true\""),
        "keyword as text should work: {}",
        js
    );
}

#[test]
fn jsx_text_with_operators() {
    // JSX text should handle operators like &&, ||, ?, etc.
    let src = r#"
fn render() -> JsValue {
    <div>A && B</div>
}
"#;
    let js = compile_to_js(src);
    assert!(
        js.contains("\"A && B\""),
        "text with && operator should work: {}",
        js
    );
}

#[test]
fn jsx_text_with_pipe() {
    // Test || operator in JSX text (was previously not handled with whitelist approach)
    let src = r#"
fn render() -> JsValue {
    <div>A || B</div>
}
"#;
    let js = compile_to_js(src);
    assert!(
        js.contains("\"A || B\""),
        "text with || operator should work: {}",
        js
    );
}

#[test]
fn jsx_text_with_comparison() {
    let src = r#"
fn render() -> JsValue {
    <div>x > y</div>
}
"#;
    let js = compile_to_js(src);
    // Note: > is a JSX structural token for closing tags, but inside text it should work
    // Actually, this is tricky - the lexer will see `>` as Gt which could end tags
    // This test verifies the exclusion-based approach handles this correctly
    assert!(
        js.contains("\"x > y\"") || js.contains("x > y"),
        "text with > should work: {}",
        js
    );
}

// ============================================================================
// JSX Fragment Tests
// ============================================================================

#[test]
fn jsx_empty_fragment() {
    let src = r#"
fn render() -> JsValue {
    <></>
}
"#;
    let js = compile_to_js(src);
    assert!(js.contains("Fragment"), "should use Fragment for <>...</>");
}

#[test]
fn jsx_fragment_with_children() {
    let src = r#"
fn render() -> JsValue {
    <>
        <div />
        <span />
    </>
}
"#;
    let js = compile_to_js(src);
    assert!(js.contains("Fragment"), "should use Fragment");
    assert!(
        js.contains("_jsxs("),
        "fragment with multiple children uses _jsxs"
    );
}

#[test]
fn jsx_fragment_with_single_child() {
    // Single-child fragments should use _jsx (not _jsxs) and preserve the Fragment
    let src = r#"
fn render() -> JsValue {
    <><span /></>
}
"#;
    let js = compile_to_js(src);
    assert!(
        js.contains("Fragment"),
        "single-child fragment should preserve Fragment"
    );
    assert!(
        js.contains("_jsx(Fragment"),
        "single-child fragment uses _jsx, not _jsxs"
    );
}

#[test]
fn jsx_nested_fragment() {
    // Nested fragments should be preserved, not flattened away
    // This is important for React reconciliation and DevTools
    let src = r#"
fn render() -> JsValue {
    <div><><span /></></div>
}
"#;
    let js = compile_to_js(src);
    // The output should have a nested Fragment inside the div
    assert!(
        js.contains("Fragment"),
        "nested fragment should be preserved in output"
    );
}

// ============================================================================
// JSX Component Tests
// ============================================================================

#[test]
fn jsx_component_reference() {
    let src = r#"
fn MyComponent() -> JsValue {
    <div />
}

fn render() -> JsValue {
    <MyComponent />
}
"#;
    let js = compile_to_js(src);
    // Component should be referenced as identifier, not string
    assert!(
        js.contains("_jsx(MyComponent"),
        "component should be identifier: {}",
        js
    );
}

#[test]
fn jsx_member_expression_component() {
    let src = r#"
fn render(Foo: JsValue) -> JsValue {
    <Foo.Bar />
}
"#;
    let js = compile_to_js(src);
    assert!(
        js.contains("Foo.Bar"),
        "member expression component: {}",
        js
    );
}

// ============================================================================
// JSX Spread Attribute Tests - Critical for Correct Semantics
// ============================================================================

#[test]
fn jsx_spread_then_static_ordering() {
    let src = r#"
fn render(props: JsValue) -> JsValue {
    <div {...props} class="override" />
}
"#;
    let js = compile_to_js(src);
    assert!(
        js.contains("Object.assign"),
        "spread should use Object.assign: {}",
        js
    );

    // The static prop should come AFTER the spread in the args
    // Object.assign({}, props, { class: "override" })
    // Search for "props" after Object.assign to avoid matching the function parameter
    let after_assign = js.find("Object.assign").expect("should have Object.assign");
    let props_pos = js[after_assign..]
        .find("props")
        .expect("should have props in Object.assign") + after_assign;
    let class_pos = js.find("class:").expect("should have class:");
    assert!(
        props_pos < class_pos,
        "spread should come before static prop for correct override semantics: {}",
        js
    );
}

#[test]
fn jsx_static_then_spread_ordering() {
    let src = r#"
fn render(props: JsValue) -> JsValue {
    <div class="base" {...props} />
}
"#;
    let js = compile_to_js(src);
    assert!(js.contains("Object.assign"), "should use Object.assign");

    // Object.assign({}, { class: "base" }, props)
    let class_pos = js.find("class:").expect("should have class:");
    // Find "props" that comes after the first occurrence (which is in function params)
    let after_assign = js.find("Object.assign").unwrap();
    let props_in_assign = js[after_assign..].find("props").unwrap() + after_assign;
    assert!(
        class_pos < props_in_assign,
        "static prop should come before spread: {}",
        js
    );
}

#[test]
fn jsx_interleaved_static_spread_static() {
    let src = r#"
fn render(b: JsValue) -> JsValue {
    <div a="1" {...b} c="2" />
}
"#;
    let js = compile_to_js(src);
    assert!(js.contains("Object.assign"), "should use Object.assign");

    // Object.assign({}, { a: "1" }, b, { c: "2" })
    // Order should be: a, then b, then c
    let a_pos = js.find("a:").expect("should have a:");
    let after_assign = js.find("Object.assign").unwrap();
    let b_in_assign = js[after_assign..].find(", b,").unwrap() + after_assign;
    let c_pos = js.find("c:").expect("should have c:");

    assert!(a_pos < b_in_assign, "a should come before b");
    assert!(b_in_assign < c_pos, "b should come before c");
}

#[test]
fn jsx_no_spread_simple_object() {
    let src = r#"
fn render() -> JsValue {
    <div a="1" b="2" />
}
"#;
    let js = compile_to_js(src);
    // Without spreads, should NOT use Object.assign
    assert!(
        !js.contains("Object.assign"),
        "no spread should not use Object.assign: {}",
        js
    );
}

// ============================================================================
// JSX Expression Children Tests
// ============================================================================

#[test]
fn jsx_expression_child() {
    let src = r#"
fn render(value: String) -> JsValue {
    <div>{value}</div>
}
"#;
    let js = compile_to_js(src);
    assert!(js.contains("children:"), "should have children");
    assert!(js.contains("value"), "should include the expression");
}

#[test]
fn jsx_map_in_children() {
    let src = r#"
fn render(items: [String]) -> JsValue {
    <ul>
        {items.map(|item: String| <li>{item}</li>)}
    </ul>
}
"#;
    let js = compile_to_js(src);
    assert!(js.contains(".map("), "should have map call");
    assert!(js.contains("_jsx(\"li\""), "should create li elements");
}

// ============================================================================
// JSX Closing Tag Validation Tests
// ============================================================================

#[test]
fn jsx_mismatched_closing_tag_error() {
    let src = r#"
fn render() -> JsValue {
    <div></span>
}
"#;
    let result = parse_str(src);
    assert!(
        !result.errors.is_empty(),
        "should have error for mismatched tags"
    );
    assert!(
        result.errors[0].message.contains("does not match"),
        "error should mention mismatch: {:?}",
        result.errors[0].message
    );
}

#[test]
fn jsx_member_closing_tag_mismatch_error() {
    let src = r#"
fn render() -> JsValue {
    <Foo.Bar></Foo.Baz>
}
"#;
    let result = parse_str(src);
    assert!(
        !result.errors.is_empty(),
        "should have error for mismatched member tags"
    );
}

#[test]
fn jsx_member_closing_tag_match_success() {
    let src = r#"
fn render() -> JsValue {
    <Foo.Bar>content</Foo.Bar>
}
"#;
    let result = parse_str(src);
    assert!(
        result.errors.is_empty(),
        "matching member tags should succeed: {:?}",
        result.errors
    );
}
