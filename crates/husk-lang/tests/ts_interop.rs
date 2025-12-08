use std::fs;
use std::path::PathBuf;
use std::process::Command;

use husk_codegen_js::{JsTarget, file_to_dts, lower_file_to_js};
use husk_lang::load::{assemble_root, load_graph};
use husk_semantic::analyze_file;

fn workspace_root() -> PathBuf {
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

fn has_tsc() -> bool {
    match Command::new("tsc").arg("--version").output() {
        Ok(output) => output.status.success(),
        Err(_) => false,
    }
}

#[test]
fn typescript_can_typecheck_generated_dts_when_available() {
    if !has_tsc() {
        eprintln!("[husk] `tsc` not found on PATH; skipping TS interop test");
        return;
    }

    let root = workspace_root();
    let hk_path = root.join("examples").join("hello.hk");

    let graph = load_graph(&hk_path).unwrap_or_else(|e| panic!("{e}"));
    let file = assemble_root(&graph).unwrap_or_else(|e| panic!("{e}"));

    let sem = analyze_file(&file);
    assert!(
        sem.symbols.errors.is_empty() && sem.type_errors.is_empty(),
        "semantic errors in {}: symbols={:?}, types={:?}",
        hk_path.display(),
        sem.symbols.errors,
        sem.type_errors
    );

    let out_dir = root.join("target").join("ts-interop");
    fs::create_dir_all(&out_dir).expect("failed to create ts-interop directory");

    // Emit JS + preamble and .d.ts side by side.
    let module = lower_file_to_js(
        &file,
        false,
        JsTarget::Esm,
        &sem.name_resolution,
        &sem.type_resolution,
        &sem.variant_calls,
        &sem.variant_patterns,
    );
    let js = module.to_source_with_preamble();
    let js_path = out_dir.join("hello.js");
    fs::write(&js_path, js)
        .unwrap_or_else(|e| panic!("failed to write JS to {}: {e}", js_path.display()));

    let dts = file_to_dts(&file);
    let dts_path = out_dir.join("hello.d.ts");
    fs::write(&dts_path, dts)
        .unwrap_or_else(|e| panic!("failed to write d.ts to {}: {e}", dts_path.display()));

    // Small TS file that imports the Husk module and uses the declared types.
    let main_ts = r#"
import { add } from "./hello";

const result: number = add(1, 2);
console.log(result);
"#;
    let main_ts_path = out_dir.join("main.ts");
    fs::write(&main_ts_path, main_ts.trim_start()).unwrap_or_else(|e| {
        panic!(
            "failed to write TS entry point {}: {e}",
            main_ts_path.display()
        )
    });

    // Run TypeScript compiler in the out dir with strict checking and no output.
    let status = Command::new("tsc")
        .arg("--strict")
        .arg("--noEmit")
        .arg("main.ts")
        .current_dir(&out_dir)
        .status()
        .unwrap_or_else(|e| panic!("failed to run tsc in {}: {e}", out_dir.display()));

    assert!(
        status.success(),
        "tsc type-checking failed in {} (status: {status})",
        out_dir.display()
    );
}
