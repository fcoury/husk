use std::collections::{HashMap, HashSet};
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use husk_ast::{ExternItemKind, File, Ident, Item, ItemKind, TypeExpr, TypeExprKind, Visibility};
use husk_parser::parse_str;

/// Trait for providing file content during module loading.
/// This allows the LSP to provide in-memory content for unsaved files.
pub trait ContentProvider {
    fn read_file(&self, path: &Path) -> Result<String, io::Error>;
}

/// Default implementation that reads from the filesystem.
pub struct FileSystemProvider;

impl ContentProvider for FileSystemProvider {
    fn read_file(&self, path: &Path) -> Result<String, io::Error> {
        fs::read_to_string(path)
    }
}

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
    #[error("parse errors in {path}")]
    Parse {
        path: String,
        source_code: String,
        errors: Vec<husk_parser::ParseError>,
    },
    #[error("cycle detected involving {0}")]
    Cycle(String),
    #[error("missing module {0}")]
    Missing(String),
}

/// Build a module graph starting from `entry` (a .hk file).
/// Supports `use crate::a::b::Item;` where modules map to `a.hk`, `a/b.hk`, etc.
pub fn load_graph(entry: &Path) -> Result<ModuleGraph, LoadError> {
    load_graph_with_provider(entry, &FileSystemProvider)
}

/// Build a module graph with a custom content provider.
/// This allows the LSP to provide in-memory content for unsaved files.
pub fn load_graph_with_provider<P: ContentProvider>(
    entry: &Path,
    provider: &P,
) -> Result<ModuleGraph, LoadError> {
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
        provider,
    )?;

    Ok(ModuleGraph { modules })
}

fn dfs_load<P: ContentProvider>(
    path: &Path,
    root: &Path,
    module_path: Vec<String>,
    modules: &mut HashMap<Vec<String>, Module>,
    visiting: &mut HashSet<Vec<String>>,
    order: &mut Vec<Vec<String>>,
    provider: &P,
) -> Result<(), LoadError> {
    if modules.contains_key(&module_path) {
        return Ok(());
    }
    if !visiting.insert(module_path.clone()) {
        return Err(LoadError::Cycle(module_path.join("::")));
    }

    let src = provider
        .read_file(path)
        .map_err(|e| LoadError::Io(path.display().to_string(), e.to_string()))?;
    let parsed = parse_str(&src);
    if !parsed.errors.is_empty() {
        return Err(LoadError::Parse {
            path: path.display().to_string(),
            source_code: src.clone(),
            errors: parsed.errors,
        });
    }
    let file = parsed.file.ok_or_else(|| LoadError::Parse {
        path: path.display().to_string(),
        source_code: src.clone(),
        errors: Vec::new(),
    })?;

    // Recurse into module deps discovered via `use crate::foo::...`
    for dep_mod_path in discover_module_paths(&file) {
        let dep_fs_path = module_path_to_file(root, &dep_mod_path)
            .ok_or_else(|| LoadError::Missing(dep_mod_path.join("::")))?;
        if !dep_fs_path.exists() {
            return Err(LoadError::Missing(dep_mod_path.join("::")));
        }
        dfs_load(&dep_fs_path, root, dep_mod_path, modules, visiting, order, provider)?;
    }

    modules.insert(
        module_path.clone(),
        Module {
            file: file.clone(),
        },
    );
    order.push(module_path.clone());
    visiting.remove(&module_path);
    Ok(())
}

fn canonical(path: &Path) -> Result<PathBuf, LoadError> {
    path.canonicalize()
        .map_err(|e| LoadError::Io(path.display().to_string(), e.to_string()))
}

/// Convert a module path like ["crate", "express"] to a file path like "express.hk"
pub fn module_path_to_file(root: &Path, mod_path: &[String]) -> Option<PathBuf> {
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
        if let ItemKind::Use { path, .. } = &item.kind {
            if path.first().map(|i| i.name.as_str()) == Some("crate") && path.len() >= 2 {
                let module_seg = path[1].name.clone();
                let mut mod_path = vec!["crate".to_string(), module_seg];
                // The last segment is always an item (type, function, or enum for variant imports),
                // so we exclude it. E.g., `use crate::foo::Result::{Ok, Err}` depends on module
                // `crate::foo` (foo.hk), not `crate::foo::Result` (foo/Result.hk).
                let path_end = path.len() - 1;
                if path.len() > 2 {
                    mod_path.extend(path[2..path_end].iter().map(|i| i.name.clone()));
                }
                mods.push(mod_path);
            }
        }
    }
    mods
}

/// Assemble a single `File` with resolved `use` items applied (namespace-aware).
/// - Keeps the root module's items
/// - For `use crate::foo::Bar;`, injects a synthetic alias Item for Bar into root
/// - Respects `pub`: only imports public items from other modules
/// - Automatically includes impl blocks for imported types
/// - Automatically includes extern blocks that declare imported types
pub fn assemble_root(graph: &ModuleGraph) -> Result<File, LoadError> {
    let root_mod = graph
        .modules
        .get(&vec!["crate".into()])
        .ok_or_else(|| LoadError::Missing("crate".into()))?;
    let mut items = Vec::new();
    let mut seen_names = HashSet::new();
    let mut imported_types = HashSet::new(); // Track imported type names
    let mut included_extern_blocks = HashSet::new(); // Track which extern blocks we've added

    // Phase 1: Process use statements and root items
    for item in root_mod.file.items.iter() {
        match &item.kind {
            ItemKind::Use { path, kind } => {
                match kind {
                    husk_ast::UseKind::Item => {
                        // Regular item import: `use crate::foo::Bar;`
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

                        // Track if this is a type (for impl block inclusion)
                        match &export.kind {
                            ItemKind::Struct { name, .. } | ItemKind::Enum { name, .. } => {
                                imported_types.insert(name.name.clone());
                            }
                            ItemKind::ExternBlock { items: ext_items, .. } => {
                                // If we're importing from an extern block, track the type
                                for ext in ext_items {
                                    if let ExternItemKind::Struct { name, .. } = &ext.kind {
                                        if name.name == item_name {
                                            imported_types.insert(name.name.clone());
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }

                        if !seen_names.insert(item_name.clone()) {
                            return Err(LoadError::Missing(format!(
                                "conflicting import `{}` in root module",
                                item_name
                            )));
                        }

                        // For extern blocks, avoid adding the same block multiple times
                        // (e.g., when importing multiple items from the same extern block)
                        if let ItemKind::ExternBlock { items: ext_items, .. } = &export.kind {
                            // Check if all structs in this block have already been included
                            let all_structs_included = ext_items.iter().all(|ext| {
                                if let ExternItemKind::Struct { name, .. } = &ext.kind {
                                    included_extern_blocks.contains(&name.name)
                                } else {
                                    true
                                }
                            });

                            if !all_structs_included {
                                // Mark all structs as included
                                for ext in ext_items {
                                    if let ExternItemKind::Struct { name, .. } = &ext.kind {
                                        included_extern_blocks.insert(name.name.clone());
                                    }
                                }
                                items.push(export.clone());
                            }
                        } else {
                            items.push(export.clone());
                        }
                    }
                    husk_ast::UseKind::Glob | husk_ast::UseKind::Variants(_) => {
                        // Variant imports are handled in semantic analysis, not here
                        // We just need to pass them through to the assembled file
                        // The use statement itself is preserved for semantic analysis
                        items.push(item.clone());
                    }
                }
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

    // Phase 2: Include extern blocks and impl blocks for imported types from all modules
    for (module_path, module) in &graph.modules {
        // Skip root module (already processed)
        if module_path == &vec!["crate".to_string()] {
            continue;
        }

        for item in &module.file.items {
            match &item.kind {
                // Include extern blocks that declare imported types
                ItemKind::ExternBlock { items: ext_items, .. } => {
                    let has_imported_type = ext_items.iter().any(|ext| {
                        if let ExternItemKind::Struct { name, .. } = &ext.kind {
                            imported_types.contains(&name.name)
                        } else {
                            false
                        }
                    });

                    if has_imported_type {
                        // Track extern structs to avoid duplicates
                        let mut should_include = false;
                        for ext in ext_items {
                            if let ExternItemKind::Struct { name, .. } = &ext.kind {
                                if !included_extern_blocks.contains(&name.name) {
                                    included_extern_blocks.insert(name.name.clone());
                                    should_include = true;
                                }
                            }
                        }
                        // Only include if we haven't already included these structs
                        if should_include {
                            // Check if this exact block is already in items (from find_pub_item)
                            let already_included = items.iter().any(|existing| {
                                if let ItemKind::ExternBlock {
                                    items: existing_items,
                                    ..
                                } = &existing.kind
                                {
                                    // Same items means same block
                                    existing_items.len() == ext_items.len()
                                        && existing_items.iter().zip(ext_items.iter()).all(
                                            |(a, b)| match (&a.kind, &b.kind) {
                                                (
                                                    ExternItemKind::Struct { name: n1, .. },
                                                    ExternItemKind::Struct { name: n2, .. },
                                                ) => n1.name == n2.name,
                                                _ => false,
                                            },
                                        )
                                } else {
                                    false
                                }
                            });

                            if !already_included {
                                items.push(item.clone());
                            }
                        }
                    }
                }
                // Include impl blocks for imported types
                ItemKind::Impl(impl_block) => {
                    let self_ty_name = type_expr_to_name(&impl_block.self_ty);
                    if imported_types.contains(&self_ty_name) {
                        items.push(item.clone());
                    }
                }
                _ => {}
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
    // First, check regular public items
    if let Some(item) = file
        .items
        .iter()
        .find(|it| it.visibility == Visibility::Public && matches_name(it, name))
    {
        return Some(item);
    }

    // Then, check extern block items (they don't have explicit visibility on individual items)
    for item in &file.items {
        if let ItemKind::ExternBlock { items: ext_items, .. } = &item.kind {
            for ext in ext_items {
                if matches_extern_item_name(&ext.kind, name) {
                    // Return the extern block containing this item
                    return Some(item);
                }
            }
        }
    }

    None
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

/// Extract the type name from a TypeExpr (for impl block self_ty).
fn type_expr_to_name(ty: &TypeExpr) -> String {
    match &ty.kind {
        TypeExprKind::Named(ident) => ident.name.clone(),
        TypeExprKind::Generic { name, .. } => name.name.clone(),
        TypeExprKind::Function { .. } => String::new(),
        TypeExprKind::Array(elem) => format!("[{}]", type_expr_to_name(elem)),
        TypeExprKind::Tuple(types) => {
            let type_names: Vec<String> = types.iter().map(type_expr_to_name).collect();
            format!("({})", type_names.join(", "))
        }
    }
}

/// Check if an extern item matches the given name.
fn matches_extern_item_name(kind: &ExternItemKind, name: &str) -> bool {
    match kind {
        ExternItemKind::Struct { name: n, .. } => n.name == name,
        ExternItemKind::Fn { name: n, .. } => n.name == name,
        ExternItemKind::Mod { binding, .. } => binding.name == name,
        ExternItemKind::Static { name: n, .. } => n.name == name,
        ExternItemKind::Const { name: n, .. } => n.name == name,
    }
}
