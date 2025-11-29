use std::fs;
use std::path::PathBuf;
use std::process::Command;

use glob::glob;
use husk_cli::load::{assemble_root, load_graph};
use husk_codegen_js::{JsTarget, lower_file_to_js};
use husk_semantic::analyze_file;

fn workspace_root() -> PathBuf {
    // Walk up from the current directory, returning the outermost Cargo.toml (workspace root).
    let mut dir = std::env::current_dir().expect("failed to read current dir");
    let mut found: Option<PathBuf> = None;
    loop {
        if dir.join("Cargo.toml").exists() {
            found = Some(dir.clone());
        }
        if !dir.pop() {
            break;
        }
    }
    found.expect("failed to resolve workspace root from current dir")
}

fn husk_example_files() -> Vec<PathBuf> {
    let root = workspace_root();
    let pattern = format!("{}/examples/**/*.hk", root.display());
    glob(&pattern)
        .expect("invalid glob pattern")
        .filter_map(Result::ok)
        .collect()
}

fn has_node() -> bool {
    match Command::new("node").arg("--version").output() {
        Ok(output) => output.status.success(),
        Err(_) => false,
    }
}

#[test]
fn workspace_root_finds_cargo_toml() {
    let root = workspace_root();
    assert!(
        root.join("Cargo.toml").exists(),
        "workspace root {:?} missing Cargo.toml",
        root
    );
}

#[test]
fn examples_execute_with_node_when_available() {
    if !has_node() {
        eprintln!("[husk] `node` not found on PATH; skipping Node execution tests");
        return;
    }

    let root = workspace_root();
    let out_dir = root.join("target").join("node-examples");
    fs::create_dir_all(&out_dir).expect("failed to create node-examples directory");

    for path in husk_example_files() {
        let graph = load_graph(&path).unwrap_or_else(|e| panic!("{e}"));
        let file = assemble_root(&graph).unwrap_or_else(|e| panic!("{e}"));
        let sem = analyze_file(&file);
        assert!(
            sem.symbols.errors.is_empty() && sem.type_errors.is_empty(),
            "semantic errors in {}: symbols={:?}, types={:?}",
            path.display(),
            sem.symbols.errors,
            sem.type_errors
        );

        // Lower to JS with preamble (bin mode: auto-call main when present).
        let module = lower_file_to_js(&file, true, JsTarget::Cjs);
        let mut js = module.to_source_with_preamble();

        // For the minimal Express interop example, prepend a tiny stub `express`
        // implementation so that the generated code can run without depending
        // on the real `express` package.
        let file_stem_str = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("");

        let is_express_minimal = file_stem_str == "interop_express_minimal";
        let is_demo_npm = path.to_str().map(|s| s.contains("demo_npm")).unwrap_or(false);

        if is_express_minimal {
            let stub = r#"
// Stub globalThis.express for integration tests: returns an object with a no-op `get`.
globalThis.express = function () {
    return {
        get: function (_path, _handler) {
            // no-op: we don't actually start a server in tests
        },
    };
};
"#;
            let mut combined = String::new();
            combined.push_str(stub.trim_start());
            combined.push('\n');
            combined.push_str(&js);
            js = combined;
        }

        if is_demo_npm {
            let stub = r#"
// Stub npm package functions for demo_npm integration tests
globalThis.nanoid = function () { return "test-id-12345"; };
globalThis.is_email = function (s) { return s.includes("@"); };
globalThis.is_alpha = function (s) { return /^[A-Za-z]+$/.test(s); };
globalThis.is_length = function (s, min, max) { return s.length >= min && s.length <= max; };
globalThis.chalk_green = function (s) { return s; };
globalThis.chalk_red = function (s) { return s; };
globalThis.chalk_blue = function (s) { return s; };
globalThis.chalk_gray = function (s) { return s; };
globalThis.chalk_bold = function (s) { return s; };
globalThis.println = function (s) { console.log(s); };
"#;
            let mut combined = String::new();
            combined.push_str(stub.trim_start());
            combined.push('\n');
            combined.push_str(&js);
            js = combined;
        }

        let file_stem = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("example");
        let js_path = out_dir.join(format!("{file_stem}.js"));
        fs::write(&js_path, js).unwrap_or_else(|e| {
            panic!("failed to write JS for {}: {e}", path.display());
        });

        // Execute with Node; any non-zero exit code is considered a failure.
        let status = Command::new("node")
            .arg(&js_path)
            .status()
            .unwrap_or_else(|e| panic!("failed to run node for {}: {e}", js_path.display()));

        assert!(
            status.success(),
            "Node execution failed for {} (status: {status})",
            js_path.display()
        );
    }
}
