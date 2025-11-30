use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use husk_ast::{File, Ident, Item, ItemKind, Visibility};
use husk_parser::parse_str;

#[derive(Debug, Clone)]
pub struct Module {
    pub file: File,
}

#[derive(Debug)]
pub struct ModuleGraph {
    pub modules: HashMap<Vec<String>, Module>,
}

#[derive(Debug, thiserror::Error)]
pub enum LoadError {
    #[error("failed to resolve {0}: {1}")]
    Io(String, String),
    #[error("parse errors in {0}: {1:?}")]
    Parse(String, Vec<husk_parser::ParseError>),
    #[error("cycle detected involving {0}")]
    Cycle(String),
    #[error("missing module {0}")]
    Missing(String),
}

/// Build a module graph starting from `entry` (a .hk file).
/// Supports `use crate::a::b::Item;` where modules map to `a.hk`, `a/b.hk`, etc.
pub fn load_graph(entry: &Path) -> Result<ModuleGraph, LoadError> {
    let entry_abs = canonical(entry)?;
    let root = entry_abs
        .parent()
        .ok_or_else(|| LoadError::Io(entry.display().to_string(), "no parent".into()))?
        .to_path_buf();

    let mut modules = HashMap::new();
    let mut visiting = HashSet::new();
    let mut order = Vec::new();

    dfs_load(
        &entry_abs,
        &root,
        vec!["crate".into()],
        &mut modules,
        &mut visiting,
        &mut order,
    )?;

    Ok(ModuleGraph { modules })
}

fn dfs_load(
    path: &Path,
    root: &Path,
    module_path: Vec<String>,
    modules: &mut HashMap<Vec<String>, Module>,
    visiting: &mut HashSet<Vec<String>>,
    order: &mut Vec<Vec<String>>,
) -> Result<(), LoadError> {
    if modules.contains_key(&module_path) {
        return Ok(());
    }
    if !visiting.insert(module_path.clone()) {
        return Err(LoadError::Cycle(module_path.join("::")));
    }

    let src = fs::read_to_string(path)
        .map_err(|e| LoadError::Io(path.display().to_string(), e.to_string()))?;
    let parsed = parse_str(&src);
    if !parsed.errors.is_empty() {
        return Err(LoadError::Parse(path.display().to_string(), parsed.errors));
    }
    let file = parsed
        .file
        .ok_or_else(|| LoadError::Parse(path.display().to_string(), Vec::new()))?;

    // Recurse into module deps discovered via `use crate::foo::...`
    for dep_mod_path in discover_module_paths(&file) {
        let dep_fs_path = module_path_to_file(root, &dep_mod_path)
            .ok_or_else(|| LoadError::Missing(dep_mod_path.join("::")))?;
        if !dep_fs_path.exists() {
            return Err(LoadError::Missing(dep_mod_path.join("::")));
        }
        dfs_load(&dep_fs_path, root, dep_mod_path, modules, visiting, order)?;
    }

    modules.insert(module_path.clone(), Module { file: file.clone() });
    order.push(module_path.clone());
    visiting.remove(&module_path);
    Ok(())
}

fn canonical(path: &Path) -> Result<PathBuf, LoadError> {
    path.canonicalize()
        .map_err(|e| LoadError::Io(path.display().to_string(), e.to_string()))
}

fn module_path_to_file(root: &Path, mod_path: &[String]) -> Option<PathBuf> {
    if mod_path.is_empty() {
        return None;
    }
    if mod_path[0] != "crate" {
        return None;
    }
    if mod_path.len() == 1 {
        return None;
    }
    let rel_parts = &mod_path[1..];
    let mut p = root.to_path_buf();
    for (i, part) in rel_parts.iter().enumerate() {
        if i + 1 == rel_parts.len() {
            p.push(format!("{part}.hk"));
        } else {
            p.push(part);
        }
    }
    Some(p)
}

fn discover_module_paths(file: &File) -> Vec<Vec<String>> {
    let mut mods = Vec::new();
    for item in &file.items {
        if let ItemKind::Use { path } = &item.kind {
            if path.first().map(|i| i.name.as_str()) == Some("crate") && path.len() >= 2 {
                let module_seg = path[1].name.clone();
                let mut mod_path = vec!["crate".to_string(), module_seg];
                if path.len() > 2 {
                    mod_path.extend(path[2..path.len() - 1].iter().map(|i| i.name.clone()));
                }
                mods.push(mod_path);
            }
        }
    }
    mods
}

/// Assemble a single `File` with resolved `use` items applied (namespace-aware).
/// - Keeps the root module’s items
/// - For `use crate::foo::Bar;`, injects a synthetic alias Item for Bar into root
/// - Respects `pub`: only imports public items from other modules
pub fn assemble_root(graph: &ModuleGraph) -> Result<File, LoadError> {
    let root_mod = graph
        .modules
        .get(&vec!["crate".into()])
        .ok_or_else(|| LoadError::Missing("crate".into()))?;
    let mut items = Vec::new();
    let mut seen_names = HashSet::new();

    for item in root_mod.file.items.iter() {
        match &item.kind {
            ItemKind::Use { path } => {
                if path.len() < 3 {
                    continue;
                }
                let module_path = module_path_from_use(path);
                let module = graph
                    .modules
                    .get(&module_path)
                    .ok_or_else(|| LoadError::Missing(module_path.join("::")))?;
                let item_name = path.last().unwrap().name.clone();
                let export = find_pub_item(&module.file, &item_name).ok_or_else(|| {
                    LoadError::Missing(format!(
                        "`{}` not found or not public in `{}`",
                        item_name,
                        module_path.join("::")
                    ))
                })?;
                if !seen_names.insert(item_name.clone()) {
                    return Err(LoadError::Missing(format!(
                        "conflicting import `{}` in root module",
                        item_name
                    )));
                }
                items.push(export.clone());
            }
            _ => {
                if let Some(name) = top_level_name(item) {
                    if seen_names.insert(name.clone()) {
                        items.push(item.clone());
                    } else {
                        return Err(LoadError::Missing(format!(
                            "duplicate top-level definition `{}`",
                            name
                        )));
                    }
                } else {
                    items.push(item.clone());
                }
            }
        }
    }

    Ok(File { items })
}

fn module_path_from_use(path: &[Ident]) -> Vec<String> {
    let mut out = Vec::new();
    for seg in &path[..path.len() - 1] {
        out.push(seg.name.clone());
    }
    out
}

fn find_pub_item<'a>(file: &'a File, name: &str) -> Option<&'a Item> {
    file.items
        .iter()
        .find(|it| it.visibility == Visibility::Public && matches_name(it, name))
}

fn matches_name(item: &Item, name: &str) -> bool {
    match &item.kind {
        ItemKind::Fn { name: n, .. } => n.name == name,
        ItemKind::Struct { name: n, .. } => n.name == name,
        ItemKind::Enum { name: n, .. } => n.name == name,
        ItemKind::TypeAlias { name: n, .. } => n.name == name,
        ItemKind::Trait(trait_def) => trait_def.name.name == name,
        ItemKind::Impl(_) => false, // Impl blocks don't have names
        ItemKind::ExternBlock { .. } => false,
        ItemKind::Use { .. } => false,
    }
}

fn top_level_name(item: &Item) -> Option<String> {
    match &item.kind {
        ItemKind::Fn { name, .. } => Some(name.name.clone()),
        ItemKind::Struct { name, .. } => Some(name.name.clone()),
        ItemKind::Enum { name, .. } => Some(name.name.clone()),
        ItemKind::TypeAlias { name, .. } => Some(name.name.clone()),
        _ => None,
    }
}
