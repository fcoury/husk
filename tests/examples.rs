use std::fs;
use std::path::Path;

use glob::glob;

use husk_codegen_js::{lower_file_to_js, JsTarget};
use husk_parser::parse_str;
use husk_semantic::analyze_file;

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
        let src = fs::read_to_string(&path).unwrap_or_else(|e| {
            panic!("failed to read {}: {e}", path);
        });
        let parse = parse_str(&src);
        assert!(
            parse.errors.is_empty(),
            "parse errors in {}: {:?}",
            path,
            parse.errors
        );
        let file = parse.file.expect("parser produced no AST");
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
        let src = fs::read_to_string(&path).unwrap_or_else(|e| {
            panic!("failed to read {}: {e}", path);
        });
        let parse = parse_str(&src);
        if !parse.errors.is_empty() {
            continue;
        }
        let file = parse.file.expect("parser produced no AST");
        let sem = analyze_file(&file);
        if sem.symbols.errors.is_empty() && sem.type_errors.is_empty() {
            let module = lower_file_to_js(&file, true, JsTarget::Cjs);
            let _js = module.to_source_with_preamble();
        }
    }
}
