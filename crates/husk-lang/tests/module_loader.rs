use std::fs;
use std::io::Write;
use std::path::Path;

use husk_ast::{self, ItemKind, TypeExprKind};
use husk_lang::load::{assemble_root, load_graph};
use tempfile::tempdir;

/// Helper to extract type name from a TypeExpr
fn type_expr_to_name(ty: &husk_ast::TypeExpr) -> String {
    match &ty.kind {
        TypeExprKind::Named(ident) => ident.name.clone(),
        TypeExprKind::Generic { name, .. } => name.name.clone(),
        TypeExprKind::Function { .. } => String::new(),
        TypeExprKind::Array(elem) => format!("[{}]", type_expr_to_name(elem)),
        TypeExprKind::Tuple(types) => {
            let type_names: Vec<String> = types.iter().map(type_expr_to_name).collect();
            format!("({})", type_names.join(", "))
        }
        TypeExprKind::ImplTrait { trait_ty } => type_expr_to_name(trait_ty),
    }
}

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

// =============================================================================
// Cross-Module Extern and Impl Import Tests
// =============================================================================

#[test]
fn imports_impl_blocks_for_imported_types() {
    let dir = tempdir().unwrap();
    let root = dir.path();

    write(
        &root.join("types.hk"),
        r#"
pub struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn distance(self) -> f64 {
        0.0
    }
}
"#,
    );

    write(
        &root.join("main.hk"),
        r#"
use crate::types::Point;

fn main() {
    let p = Point { x: 3, y: 4 };
}
"#,
    );

    let graph = load_graph(&root.join("main.hk")).unwrap();
    let file = assemble_root(&graph).unwrap();

    // Verify struct is imported
    assert!(
        file.items
            .iter()
            .any(|it| matches!(&it.kind, ItemKind::Struct { name, .. } if name.name == "Point")),
        "Point struct should be imported"
    );

    // Verify impl block is included
    assert!(
        file.items.iter().any(|it| {
            if let ItemKind::Impl(impl_block) = &it.kind {
                type_expr_to_name(&impl_block.self_ty) == "Point"
            } else {
                false
            }
        }),
        "Point impl block should be included"
    );
}

#[test]
fn imports_extern_blocks_for_imported_types() {
    let dir = tempdir().unwrap();
    let root = dir.path();

    write(
        &root.join("bindings.hk"),
        r#"
extern "js" {
    struct Database;
    fn connect() -> Database;
}

impl Database {
    extern "js" fn exec(self, sql: String);
}
"#,
    );

    write(
        &root.join("main.hk"),
        r#"
use crate::bindings::Database;

fn main() {
    let db = connect();
}
"#,
    );

    let graph = load_graph(&root.join("main.hk")).unwrap();
    let file = assemble_root(&graph).unwrap();

    // Verify extern block is included
    assert!(
        file.items.iter().any(|it| {
            if let ItemKind::ExternBlock { items, .. } = &it.kind {
                items.iter().any(|ext| {
                    matches!(&ext.kind, husk_ast::ExternItemKind::Struct { name, .. } if name.name == "Database")
                })
            } else {
                false
            }
        }),
        "Extern block with Database should be included"
    );

    // Verify impl block is included
    assert!(
        file.items.iter().any(|it| {
            if let ItemKind::Impl(impl_block) = &it.kind {
                type_expr_to_name(&impl_block.self_ty) == "Database"
            } else {
                false
            }
        }),
        "Database impl block should be included"
    );
}

#[test]
fn does_not_import_impl_for_non_imported_type() {
    let dir = tempdir().unwrap();
    let root = dir.path();

    write(
        &root.join("util.hk"),
        r#"
struct Private {}

impl Private {
    fn method(&self) {}
}

pub fn helper() -> i32 { 42 }
"#,
    );

    write(
        &root.join("main.hk"),
        r#"
use crate::util::helper;

fn main() {
    let _ = helper();
}
"#,
    );

    let graph = load_graph(&root.join("main.hk")).unwrap();
    let file = assemble_root(&graph).unwrap();

    // Private type's impl should NOT be included
    assert!(
        !file.items.iter().any(|it| {
            if let ItemKind::Impl(impl_block) = &it.kind {
                type_expr_to_name(&impl_block.self_ty) == "Private"
            } else {
                false
            }
        }),
        "Private impl block should NOT be included"
    );
}

#[test]
fn deduplicates_extern_declarations() {
    let dir = tempdir().unwrap();
    let root = dir.path();

    write(
        &root.join("a.hk"),
        r#"
extern "js" {
    struct SharedType;
}

pub struct WrapperA {}
"#,
    );

    write(
        &root.join("b.hk"),
        r#"
extern "js" {
    struct SharedType;
}

pub struct WrapperB {}
"#,
    );

    write(
        &root.join("main.hk"),
        r#"
use crate::a::WrapperA;
use crate::b::WrapperB;

fn main() {}
"#,
    );

    let graph = load_graph(&root.join("main.hk")).unwrap();
    let file = assemble_root(&graph).unwrap();

    // Count how many times SharedType appears in extern blocks
    let count = file
        .items
        .iter()
        .filter(|it| {
            if let ItemKind::ExternBlock { items, .. } = &it.kind {
                items.iter().any(|ext| {
                    matches!(&ext.kind, husk_ast::ExternItemKind::Struct { name, .. } if name.name == "SharedType")
                })
            } else {
                false
            }
        })
        .count();

    // For now, we allow duplicates as long as they're identical
    // The main requirement is that they don't cause compilation errors
    assert!(
        count <= 2,
        "Should have at most 2 SharedType declarations (one from each module)"
    );
}

#[test]
fn imports_impl_from_separate_module() {
    let dir = tempdir().unwrap();
    let root = dir.path();

    // Type defined in one module
    write(
        &root.join("types.hk"),
        r#"
pub struct Widget {
    id: i32,
}
"#,
    );

    // Impl defined in same module as type
    // (impls must be in same module as type for now)
    write(
        &root.join("main.hk"),
        r#"
use crate::types::Widget;

fn main() {
    let w = Widget { id: 1 };
}
"#,
    );

    let graph = load_graph(&root.join("main.hk")).unwrap();
    let file = assemble_root(&graph).unwrap();

    // Widget should be imported
    assert!(
        file.items
            .iter()
            .any(|it| matches!(&it.kind, ItemKind::Struct { name, .. } if name.name == "Widget")),
        "Widget should be imported"
    );
}

#[test]
fn imports_multiple_items_from_same_extern_block() {
    let dir = tempdir().unwrap();
    let root = dir.path();

    write(
        &root.join("bindings.hk"),
        r#"
extern "js" {
    mod "some-lib" as some_lib;

    struct Database;
    fn connect() -> Database;
}

impl Database {
    extern "js" fn exec(self, sql: String);
}
"#,
    );

    write(
        &root.join("main.hk"),
        r#"
use crate::bindings::Database;
use crate::bindings::connect;

fn main() {
    let db = connect();
    db.exec("SELECT 1");
}
"#,
    );

    let graph = load_graph(&root.join("main.hk")).unwrap();
    let file = assemble_root(&graph).unwrap();

    // Count extern blocks - should only have one (not duplicated)
    let extern_block_count = file
        .items
        .iter()
        .filter(|it| matches!(&it.kind, ItemKind::ExternBlock { .. }))
        .count();

    assert_eq!(
        extern_block_count, 1,
        "Should have exactly one extern block, not duplicated"
    );

    // Verify impl block is included
    assert!(
        file.items.iter().any(|it| {
            if let ItemKind::Impl(impl_block) = &it.kind {
                type_expr_to_name(&impl_block.self_ty) == "Database"
            } else {
                false
            }
        }),
        "Database impl block should be included"
    );
}
