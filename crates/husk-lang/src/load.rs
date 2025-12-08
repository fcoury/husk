use std::collections::{HashMap, HashSet};
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use husk_ast::{ExternItemKind, File, Ident, Item, ItemKind, SetFilePath, TypeExpr, TypeExprKind, Visibility};
use std::sync::Arc;
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
    /// The file path this module was loaded from (for error reporting)
    pub file_path: Arc<str>,
    /// The source code of this module (for error reporting)
    pub source: String,
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
        dfs_load(
            &dep_fs_path,
            root,
            dep_mod_path,
            modules,
            visiting,
            order,
            provider,
        )?;
    }

    modules.insert(module_path.clone(), Module {
        file: file.clone(),
        file_path: path.display().to_string().into(),
        source: src.clone(),
    });
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
    let mut included_structs = HashSet::new(); // Track which extern structs we've added
    let mut included_fns = HashSet::new(); // Track which extern fns we've added
    let mut included_mods = HashSet::new(); // Track which extern mods we've added
    let mut included_consts = HashSet::new(); // Track which extern consts we've added

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
                            ItemKind::ExternBlock {
                                items: ext_items, ..
                            } => {
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

                        // For extern blocks, include only relevant items to avoid massive duplicates
                        if let ItemKind::ExternBlock {
                            items: ext_items, ..
                        } = &export.kind
                        {
                            if let Some(mut filtered) = filter_extern_block(
                                ext_items,
                                &imported_types,
                                &mut included_structs,
                                &mut included_fns,
                                &mut included_mods,
                                &mut included_consts,
                            ) {
                                // Set the file path on the cloned item
                                filtered.set_file_path(module.file_path.clone());
                                items.push(filtered);
                            }
                        } else {
                            let mut cloned = export.clone();
                            // Set the file path on the cloned item
                            cloned.set_file_path(module.file_path.clone());
                            items.push(cloned);
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
                ItemKind::ExternBlock {
                    items: ext_items, ..
                } => {
                    // Filter extern items to just the ones we actually import or need as entry points.
                    let filtered: Vec<_> = ext_items
                        .iter()
                        .filter(|ext| match &ext.kind {
                            ExternItemKind::Struct { name, .. } => {
                                imported_types.contains(&name.name)
                            }
                            ExternItemKind::Fn { name, .. } => {
                                // Keep callable module entry points
                                name.name == "express" || name.name == "better_sqlite3"
                            }
                            ExternItemKind::Mod { .. } => true,
                            _ => false,
                        })
                        .cloned()
                        .collect();

                    if filtered.is_empty() {
                        continue;
                    }

                    // Deduplicate by struct/const and fn name
                    let mut has_new = false;
                    for ext in &filtered {
                        match &ext.kind {
                            ExternItemKind::Struct { name, .. } => {
                                if included_structs.insert(name.name.clone()) {
                                    has_new = true;
                                }
                            }
                            ExternItemKind::Fn { name, .. } => {
                                if included_fns.insert(name.name.clone()) {
                                    has_new = true;
                                }
                            }
                            _ => {}
                        }
                    }
                    if has_new {
                        let mut new_block = item.clone();
                        if let ItemKind::ExternBlock {
                            items: block_items, ..
                        } = &mut new_block.kind
                        {
                            *block_items = filtered;
                        }
                        // Set the file path on the cloned item
                        new_block.set_file_path(module.file_path.clone());
                        items.push(new_block);
                    }
                }
                // Include impl blocks for imported types
                ItemKind::Impl(impl_block) => {
                    let self_ty_name = type_expr_to_name(&impl_block.self_ty);
                    if imported_types.contains(&self_ty_name) {
                        let mut cloned = item.clone();
                        // Set the file path on the cloned item
                        cloned.set_file_path(module.file_path.clone());
                        items.push(cloned);
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
        if let ItemKind::ExternBlock {
            items: ext_items, ..
        } = &item.kind
        {
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
        ExternItemKind::Impl { .. } => false, // Impl blocks don't have a top-level name
    }
}

/// Filter extern block items down to the imported structs and a small allowlist of functions.
fn filter_extern_block(
    ext_items: &[husk_ast::ExternItem],
    imported_types: &HashSet<String>,
    included_structs: &mut HashSet<String>,
    included_fns: &mut HashSet<String>,
    included_mods: &mut HashSet<String>,
    included_consts: &mut HashSet<String>,
) -> Option<husk_ast::Item> {
    let mut filtered = Vec::new();
    for ext in ext_items {
        match &ext.kind {
            ExternItemKind::Struct { name, .. } => {
                if imported_types.contains(&name.name) {
                    if included_structs.insert(name.name.clone()) {
                        filtered.push(ext.clone());
                    }
                }
            }
            ExternItemKind::Fn { name, .. } => {
                // Allow only the entry-point callables we need
                if name.name == "express" || name.name == "better_sqlite3" || name.name == "json" {
                    if included_fns.insert(name.name.clone()) {
                        filtered.push(ext.clone());
                    }
                }
            }
            ExternItemKind::Mod { binding, .. } => {
                if included_mods.insert(binding.name.clone()) {
                    filtered.push(ext.clone());
                }
            }
            ExternItemKind::Const { name, .. } => {
                if imported_types.contains(&name.name) {
                    if included_consts.insert(name.name.clone()) {
                        filtered.push(ext.clone());
                    }
                }
            }
            _ => {}
        }
    }

    if filtered.is_empty() {
        return None;
    }

    Some(husk_ast::Item {
        span: ext_items
            .first()
            .map(|e| e.span.clone())
            .unwrap_or_else(|| husk_ast::Span { range: 0..0, file: None }),
        visibility: husk_ast::Visibility::Public,
        attributes: Vec::new(),
        kind: husk_ast::ItemKind::ExternBlock {
            abi: "js".to_string(),
            items: filtered,
        },
    })
}
