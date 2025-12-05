use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

use glob::glob;
use husk_lang::load::{assemble_root, load_graph};
use husk_codegen_js::{JsTarget, lower_file_to_js};
use husk_semantic::{analyze_file, filter_items_by_cfg};

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
        // Skip demo_npm files - they require actual npm packages installed
        .filter(|p| !p.to_string_lossy().contains("demo_npm"))
        // Skip bindings directories - they're library modules, not runnable examples
        .filter(|p| !p.to_string_lossy().contains("bindings"))
        // Skip express_sqlite directory - requires npm packages (express, better-sqlite3)
        .filter(|p| !p.to_string_lossy().contains("express_sqlite"))
        // Skip advent2025 directory - requires external input files
        .filter(|p| !p.to_string_lossy().contains("advent2025"))
        // Skip array_range_test - uses unimplemented features (array indexing, ranges)
        .filter(|p| !p.to_string_lossy().contains("array_range_test"))
        // Skip jsx_demo - requires React runtime (react/jsx-runtime)
        .filter(|p| !p.to_string_lossy().contains("jsx_demo"))
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

        // Filter out #[cfg(test)] items before codegen (not running in test mode)
        let filtered_file = filter_items_by_cfg(&file, &HashSet::new());

        // Lower to JS with preamble (bin mode: auto-call main when present).
        let module = lower_file_to_js(&filtered_file, true, JsTarget::Cjs, &sem.name_resolution, &sem.type_resolution, &sem.variant_calls, &sem.variant_patterns);
        let mut js = module.to_source_with_preamble();

        // For the minimal Express interop example, prepend a tiny stub `express`
        // implementation so that the generated code can run without depending
        // on the real `express` package.
        let file_stem_str = path.file_stem().and_then(|s| s.to_str()).unwrap_or("");

        let is_express_minimal = file_stem_str == "interop_express_minimal";

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
