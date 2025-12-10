use std::fs;
use std::path::Path;

use husk_parser::parse_str;
use husk_semantic::analyze_file;

#[test]
fn iterator_example_parses_and_typechecks() {
    let path = Path::new("examples/iterators.hk");
    let src = fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));

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
}

#[test]
fn new_iterator_methods_parse_and_typecheck() {
    let path = Path::new("examples/new_iterators.hk");
    let src = fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));

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
}

