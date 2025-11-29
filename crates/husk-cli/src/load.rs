use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use husk_ast::{File, Ident, Item, ItemKind};
use husk_parser::parse_str;

/// Load an entry Husk file plus any `use crate::...` dependencies into a single flattened `File`.
///
/// Current limitations (MVP):
/// - Only handles `use crate::foo::...;` and assumes `foo` maps to `foo.hk` relative to the entry dir.
/// - Ignores visibility (`pub`) and nested module structures; items are flattened into one namespace.
pub fn load_with_deps(entry: &Path) -> Result<File, String> {
    let entry = entry
        .canonicalize()
        .map_err(|e| format!("failed to resolve {}: {e}", entry.display()))?;
    let root = entry
        .parent()
        .map(Path::to_path_buf)
        .ok_or_else(|| format!("entry path {} has no parent", entry.display()))?;

    let mut visited = HashSet::new();
    let mut items = Vec::new();
    load_recursive(&entry, &root, &mut visited, &mut items)?;
    Ok(File { items })
}

fn load_recursive(
    path: &Path,
    root: &Path,
    visited: &mut HashSet<PathBuf>,
    out_items: &mut Vec<Item>,
) -> Result<(), String> {
    let canon = path
        .canonicalize()
        .map_err(|e| format!("failed to resolve {}: {e}", path.display()))?;
    if !visited.insert(canon.clone()) {
        return Ok(());
    }

    let src = fs::read_to_string(&canon)
        .map_err(|e| format!("failed to read {}: {e}", canon.display()))?;
    let parsed = parse_str(&src);
    if !parsed.errors.is_empty() {
        return Err(format!("parse errors in {}: {:?}", canon.display(), parsed.errors));
    }
    let file = parsed
        .file
        .ok_or_else(|| format!("parser produced no AST for {}", canon.display()))?;

    // Recurse into dependencies first
    for dep in use_dependencies(&file) {
        if dep.is_empty() {
            continue;
        }
        let module = &dep[0];
        let dep_path = root.join(format!("{}.hk", module.name));
        if !dep_path.exists() {
            return Err(format!(
                "use references crate::{} but {} does not exist",
                module.name,
                dep_path.display()
            ));
        }
        load_recursive(&dep_path, root, visited, out_items)?;
    }

    // Append all non-use items from this file.
    for item in file.items.into_iter() {
        if matches!(item.kind, ItemKind::Use { .. }) {
            continue;
        }
        out_items.push(item);
    }

    Ok(())
}

fn use_dependencies(file: &File) -> Vec<Vec<Ident>> {
    let mut deps = Vec::new();
    for item in &file.items {
        if let ItemKind::Use { path } = &item.kind {
            if path.first().map(|id| id.name.as_str()) == Some("crate") && path.len() >= 2 {
                // skip the final segment (item); first segment after crate is the module file
                deps.push(path[1..path.len() - 0].to_vec());
            }
        }
    }
    deps
}
