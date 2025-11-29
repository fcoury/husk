use std::fs;
use std::io::Write;
use std::path::Path;

use husk_ast;
use husk_cli::load::{assemble_root, load_graph};
use tempfile::tempdir;

fn write(p: &Path, content: &str) {
    let mut f = fs::File::create(p).unwrap();
    f.write_all(content.as_bytes()).unwrap();
}

#[test]
fn loads_pub_items_and_rejects_private() {
    let dir = tempdir().unwrap();
    let root = dir.path();

    write(
        &root.join("main.hk"),
        "use crate::util::add;
fn main() { let _ = add(1,2); }
",
    );
    write(
        &root.join("util.hk"),
        "fn hidden() {}
pub fn add(a: i32, b: i32) -> i32 { a + b }
",
    );

    let graph = load_graph(&root.join("main.hk")).unwrap();
    let file = assemble_root(&graph).unwrap();
    assert!(
        file.items.iter().any(
            |it| matches!(&it.kind, husk_ast::ItemKind::Fn { name, .. } if name.name == "add")
        )
    );
    assert!(!file.items.iter().any(
        |it| matches!(&it.kind, husk_ast::ItemKind::Fn { name, .. } if name.name == "hidden")
    ));
}

#[test]
fn errors_when_importing_private() {
    let dir = tempdir().unwrap();
    let root = dir.path();

    write(&root.join("main.hk"), "use crate::util::hidden;");
    write(&root.join("util.hk"), "fn hidden() {}");

    let graph = load_graph(&root.join("main.hk")).unwrap();
    let err = assemble_root(&graph).unwrap_err();
    assert!(err.to_string().contains("hidden"));
}

#[test]
fn detects_missing_module() {
    let dir = tempdir().unwrap();
    let root = dir.path();
    write(&root.join("main.hk"), "use crate::missing::foo;");

    let err = load_graph(&root.join("main.hk")).unwrap_err();
    assert!(err.to_string().contains("missing"));
}

#[test]
fn detects_cycle_between_modules() {
    let dir = tempdir().unwrap();
    let root = dir.path();
    write(&root.join("main.hk"), "use crate::a::Foo;");
    write(&root.join("a.hk"), "use crate::b::Foo; pub struct Foo {}");
    write(&root.join("b.hk"), "use crate::a::Foo; pub struct Foo {}");

    let err = load_graph(&root.join("main.hk")).unwrap_err();
    assert!(err.to_string().contains("cycle"));
}

#[test]
fn detects_conflicting_imports() {
    let dir = tempdir().unwrap();
    let root = dir.path();
    write(
        &root.join("main.hk"),
        "use crate::foo::Thing; use crate::bar::Thing;",
    );
    write(&root.join("foo.hk"), "pub struct Thing { }\n");
    write(&root.join("bar.hk"), "pub struct Thing { }\n");

    let graph = load_graph(&root.join("main.hk")).unwrap();
    let err = assemble_root(&graph).unwrap_err();
    assert!(err.to_string().contains("conflicting import"));
}

#[test]
fn detects_unknown_item_in_module() {
    let dir = tempdir().unwrap();
    let root = dir.path();
    write(&root.join("main.hk"), "use crate::util::Missing;");
    write(&root.join("util.hk"), "pub struct Present {}\n");

    let graph = load_graph(&root.join("main.hk")).unwrap();
    let err = assemble_root(&graph).unwrap_err();
    assert!(err.to_string().contains("Missing"));
}

#[test]
fn loads_nested_module_path() {
    let dir = tempdir().unwrap();
    let root = dir.path();
    write(
        &root.join("main.hk"),
        "use crate::foo::bar::Thing; fn main() { let _x = Thing {}; }",
    );
    fs::create_dir_all(root.join("foo")).unwrap();
    write(&root.join("foo/bar.hk"), "pub struct Thing {}\n");

    let graph = load_graph(&root.join("main.hk")).unwrap();
    let file = assemble_root(&graph).unwrap();
    assert!(file.items.iter().any(
        |it| matches!(&it.kind, husk_ast::ItemKind::Struct { name, .. } if name.name == "Thing")
    ));
}
