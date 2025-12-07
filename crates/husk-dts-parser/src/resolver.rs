//! Very small resolver scaffold to classify modules (export = callable vs standard).
//!
//! This is intentionally minimal and single-file to avoid changing the existing
//! importer pipeline; it inspects a parsed `DtsFile` and returns metadata used by
//! codegen.

use std::collections::HashMap;
use std::path::PathBuf;

use crate::ast::*;

/// Resolved metadata for a single .d.ts file.
#[derive(Debug, Default, Clone)]
pub struct ResolvedModule {
    /// Export assignment target, if present: `export = name;`
    pub export_equals: Option<String>,
    /// Quick lookup of declarations by name (including namespaces/modules).
    pub symbols: HashMap<String, DeclKind>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclKind {
    Function,
    Class,
    Namespace,
    Module,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModuleKind {
    Standard,
    Callable,
    Constructable,
}

impl ResolvedModule {
    /// Classify the module based on `export =` target.
    pub fn module_kind(&self) -> ModuleKind {
        if let Some(target) = &self.export_equals {
            match self.symbols.get(target) {
                Some(DeclKind::Function) => ModuleKind::Callable,
                Some(DeclKind::Class) => ModuleKind::Constructable,
                Some(DeclKind::Namespace) => ModuleKind::Standard,
                _ => ModuleKind::Standard,
            }
        } else {
            ModuleKind::Standard
        }
    }
}

/// Resolve a single parsed .d.ts file into lightweight metadata.
pub fn resolve_module(file: &DtsFile, path: &PathBuf) -> ResolvedModule {
    let mut resolved = ResolvedModule::default();

    for item in &file.items {
        match item {
            DtsItem::Function(f) => {
                resolved.symbols.insert(f.name.clone(), DeclKind::Function);
            }
            DtsItem::Class(c) => {
                resolved.symbols.insert(c.name.clone(), DeclKind::Class);
            }
            DtsItem::Interface(i) => {
                resolved
                    .symbols
                    .entry(i.name.clone())
                    .or_insert(DeclKind::Namespace);
            }
            DtsItem::Namespace(ns) => {
                resolved
                    .symbols
                    .insert(ns.name.clone(), DeclKind::Namespace);
                for item in &ns.items {
                    add_symbols(item, &mut resolved.symbols, Some(ns.name.clone()));
                }
            }
            DtsItem::Module(m) => {
                resolved.symbols.insert(m.name.clone(), DeclKind::Module);
                for item in &m.items {
                    add_symbols(item, &mut resolved.symbols, Some(m.name.clone()));
                }
            }
            DtsItem::Export(DtsExport::Equals(name)) => {
                resolved.export_equals = Some(name.clone());
            }
            _ => {}
        }
    }

    // Keep path in case we want to extend graph logic later
    let _ = path;
    resolved
}

fn add_symbols(item: &DtsItem, symbols: &mut HashMap<String, DeclKind>, _scope: Option<String>) {
    match item {
        DtsItem::Function(f) => {
            symbols.insert(f.name.clone(), DeclKind::Function);
        }
        DtsItem::Class(c) => {
            symbols.insert(c.name.clone(), DeclKind::Class);
        }
        DtsItem::Interface(i) => {
            symbols.entry(i.name.clone()).or_insert(DeclKind::Namespace);
        }
        DtsItem::Namespace(ns) => {
            symbols.insert(ns.name.clone(), DeclKind::Namespace);
            for item in &ns.items {
                add_symbols(item, symbols, Some(ns.name.clone()));
            }
        }
        DtsItem::Module(m) => {
            symbols.insert(m.name.clone(), DeclKind::Module);
            for item in &m.items {
                add_symbols(item, symbols, Some(m.name.clone()));
            }
        }
        _ => {}
    }
}
