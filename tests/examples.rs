use std::fs;
use std::path::Path;

use glob::glob;

use husk_ast;
use husk_codegen_js::{lower_file_to_js, JsTarget};
use husk_parser::parse_str;
use husk_semantic::analyze_file;
use husk_cli::load::{assemble_root, load_graph};
use std::collections::HashSet;

fn husk_files(pattern: &str) -> Vec<String> {
    glob(pattern)
        .expect("invalid glob pattern")
        .filter_map(Result::ok)
        .map(|p| p.to_string_lossy().into_owned())
        .collect()
}

#[test]
fn examples_parse_and_typecheck() {
    for path in husk_files("examples/**/*.hk") {
        let graph = load_graph(Path::new(&path)).unwrap_or_else(|e| panic!("{e}"));
        let file = assemble_root(&graph).unwrap_or_else(|e| panic!("{e}"));
        let sem = analyze_file(&file);
        assert!(
            sem.symbols.errors.is_empty() && sem.type_errors.is_empty(),
            "semantic errors in {}: symbols={:?}, types={:?}",
            path,
            sem.symbols.errors,
            sem.type_errors
        );
    }
}

#[test]
fn examples_codegen() {
    for path in husk_files("examples/**/*.hk") {
        let file = match load_graph(Path::new(&path)).and_then(|g| assemble_root(&g)) {
            Ok(f) => f,
            Err(_) => continue,
        };
        let sem = analyze_file(&file);
        if sem.symbols.errors.is_empty() && sem.type_errors.is_empty() {
            let module = lower_file_to_js(&file, true, JsTarget::Cjs);
            let _js = module.to_source_with_preamble();
        }
    }
}
