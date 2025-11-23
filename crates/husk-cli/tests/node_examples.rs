use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use glob::glob;
use husk_codegen_js::lower_file_to_js;
use husk_parser::parse_str;
use husk_semantic::analyze_file;

fn workspace_root() -> PathBuf {
    // `CARGO_MANIFEST_DIR` points to `crates/husk-cli`.
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .canonicalize()
        .expect("failed to resolve workspace root")
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
fn examples_execute_with_node_when_available() {
    if !has_node() {
        eprintln!("[husk] `node` not found on PATH; skipping Node execution tests");
        return;
    }

    let root = workspace_root();
    let out_dir = root.join("target").join("node-examples");
    fs::create_dir_all(&out_dir).expect("failed to create node-examples directory");

    for path in husk_example_files() {
        let src = fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));

        // Frontend + semantics must succeed.
        let parse = parse_str(&src);
        assert!(
            parse.errors.is_empty(),
            "parse errors in {}: {:?}",
            path.display(),
            parse.errors
        );
        let file = parse.file.expect("parser produced no AST");
        let sem = analyze_file(&file);
        assert!(
            sem.symbols.errors.is_empty() && sem.type_errors.is_empty(),
            "semantic errors in {}: symbols={:?}, types={:?}",
            path.display(),
            sem.symbols.errors,
            sem.type_errors
        );

        // Lower to JS with preamble and write to a per-example JS file.
        let module = lower_file_to_js(&file);
        let js = module.to_source_with_preamble();

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
