//! Multi-file resolver for TypeScript declaration files.
//!
//! This module handles:
//! - Resolving import statements to file paths
//! - Walking the import graph to collect all declarations
//! - Handling `/// <reference path="..." />` directives
//! - Node modules resolution for package imports
//!
//! ## TODO: Export Assignment Linking
//!
//! Currently the resolver collects files but does not link `export =` assignments
//! to their corresponding imports. For example:
//!
//! ```typescript
//! // File A: export = MyNamespace
//! // File B: import foo = require('A')
//! ```
//!
//! File B expects `foo` to *be* `MyNamespace`, but we don't yet perform this
//! identity linking. This needs to be implemented for full CommonJS/UMD support.

use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};

use oxc_allocator::Allocator;
use oxc_ast::ast::*;
use oxc_parser::Parser;
use oxc_span::SourceType;

use crate::ast::{DtsFile, DtsItem};
use crate::oxc_parser::parse_with_oxc;

/// Result of resolving and parsing a complete .d.ts module graph.
#[derive(Debug, Clone)]
pub struct ResolvedModule {
    /// The entry point file path
    pub entry_path: PathBuf,
    /// All resolved files, keyed by their absolute path
    pub files: HashMap<PathBuf, DtsFile>,
    /// Import graph: file -> list of files it imports
    pub import_graph: HashMap<PathBuf, Vec<PathBuf>>,
    /// Errors encountered during resolution
    pub errors: Vec<ResolveError>,
}

impl ResolvedModule {
    /// Get all items from all files in dependency order (leaf dependencies first).
    pub fn all_items(&self) -> Vec<&DtsItem> {
        let mut order = self.topological_order();
        order.reverse(); // Leaf dependencies first

        order
            .iter()
            .flat_map(|path| self.files.get(path).map(|f| f.items.iter()).unwrap_or_default())
            .collect()
    }

    /// Get files in topological order (dependencies before dependents).
    fn topological_order(&self) -> Vec<PathBuf> {
        let mut result = Vec::new();
        let mut visited = HashSet::new();
        let mut temp_visited = HashSet::new();

        for path in self.files.keys() {
            self.topo_visit(path, &mut visited, &mut temp_visited, &mut result);
        }

        result
    }

    fn topo_visit(
        &self,
        path: &Path,
        visited: &mut HashSet<PathBuf>,
        temp_visited: &mut HashSet<PathBuf>,
        result: &mut Vec<PathBuf>,
    ) {
        if visited.contains(path) {
            return;
        }
        if temp_visited.contains(path) {
            // Cycle detected, skip
            return;
        }

        temp_visited.insert(path.to_path_buf());

        if let Some(deps) = self.import_graph.get(path) {
            for dep in deps {
                self.topo_visit(dep, visited, temp_visited, result);
            }
        }

        temp_visited.remove(path);
        visited.insert(path.to_path_buf());
        result.push(path.to_path_buf());
    }
}

/// Error during module resolution.
#[derive(Debug, Clone)]
pub enum ResolveError {
    /// File not found
    FileNotFound { path: PathBuf, from: Option<PathBuf> },
    /// Parse error in file
    ParseError { path: PathBuf, error: String },
    /// IO error reading file
    IoError { path: PathBuf, message: String },
    /// Circular import detected
    CircularImport { path: PathBuf, cycle: Vec<PathBuf> },
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolveError::FileNotFound { path, from } => {
                if let Some(from_path) = from {
                    write!(f, "File not found: {:?} (imported from {:?})", path, from_path)
                } else {
                    write!(f, "File not found: {:?}", path)
                }
            }
            ResolveError::ParseError { path, error } => {
                write!(f, "Parse error in {:?}: {}", path, error)
            }
            ResolveError::IoError { path, message } => {
                write!(f, "IO error reading {:?}: {}", path, message)
            }
            ResolveError::CircularImport { path, cycle } => {
                write!(f, "Circular import at {:?}: {:?}", path, cycle)
            }
        }
    }
}

impl std::error::Error for ResolveError {}

/// Options for module resolution.
#[derive(Debug, Clone, Default)]
pub struct ResolveOptions {
    /// Base directory for relative imports
    pub base_dir: Option<PathBuf>,
    /// Additional paths to search for modules (like node_modules)
    pub module_paths: Vec<PathBuf>,
    /// Whether to follow triple-slash references
    pub follow_references: bool,
    /// Maximum depth for import resolution (to prevent infinite loops)
    pub max_depth: Option<usize>,
}

/// Resolver for TypeScript declaration files.
pub struct Resolver {
    options: ResolveOptions,
    /// Cache of parsed files
    file_cache: HashMap<PathBuf, DtsFile>,
    /// Track files currently being resolved (for cycle detection)
    #[allow(dead_code)]
    resolving: HashSet<PathBuf>,
}

impl Resolver {
    /// Create a new resolver with the given options.
    pub fn new(options: ResolveOptions) -> Self {
        Self {
            options,
            file_cache: HashMap::new(),
            resolving: HashSet::new(),
        }
    }

    /// Resolve a module starting from the given entry point.
    pub fn resolve(&mut self, entry_path: &Path) -> ResolvedModule {
        let entry_path = self.normalize_path(entry_path);
        let mut module = ResolvedModule {
            entry_path: entry_path.clone(),
            files: HashMap::new(),
            import_graph: HashMap::new(),
            errors: Vec::new(),
        };

        let mut queue: VecDeque<(PathBuf, Option<PathBuf>, usize)> = VecDeque::new();
        queue.push_back((entry_path, None, 0));

        let mut processed = HashSet::new();

        while let Some((path, _from, depth)) = queue.pop_front() {
            if processed.contains(&path) {
                continue;
            }

            // Check max depth
            if let Some(max_depth) = self.options.max_depth {
                if depth > max_depth {
                    continue;
                }
            }

            processed.insert(path.clone());

            // Read and parse the file
            match self.parse_file(&path) {
                Ok((file, imports, references)) => {
                    // Record the imports
                    let mut deps = Vec::new();

                    // Resolve imports
                    for import_spec in imports {
                        if let Some(resolved) = self.resolve_import(&import_spec, &path) {
                            deps.push(resolved.clone());
                            if !processed.contains(&resolved) {
                                queue.push_back((resolved, Some(path.clone()), depth + 1));
                            }
                        }
                    }

                    // Resolve triple-slash references
                    if self.options.follow_references {
                        for ref_path in references {
                            if let Some(resolved) = self.resolve_reference(&ref_path, &path) {
                                deps.push(resolved.clone());
                                if !processed.contains(&resolved) {
                                    queue.push_back((resolved, Some(path.clone()), depth + 1));
                                }
                            }
                        }
                    }

                    module.import_graph.insert(path.clone(), deps);
                    module.files.insert(path, file);
                }
                Err(e) => {
                    module.errors.push(e);
                }
            }
        }

        module
    }

    /// Parse a file and extract its imports and references.
    fn parse_file(
        &mut self,
        path: &Path,
    ) -> Result<(DtsFile, Vec<String>, Vec<String>), ResolveError> {
        // Check cache first
        if let Some(file) = self.file_cache.get(path) {
            // Re-extract imports (TODO: cache these too)
            let content = std::fs::read_to_string(path).map_err(|e| ResolveError::IoError {
                path: path.to_path_buf(),
                message: e.to_string(),
            })?;
            let imports = self.extract_imports(&content);
            let references = self.extract_references(&content);
            return Ok((file.clone(), imports, references));
        }

        // Read the file
        let content = std::fs::read_to_string(path).map_err(|e| ResolveError::IoError {
            path: path.to_path_buf(),
            message: e.to_string(),
        })?;

        // Parse with Oxc
        let file = parse_with_oxc(&content, path.to_str().unwrap_or("file.d.ts")).map_err(|e| {
            ResolveError::ParseError {
                path: path.to_path_buf(),
                error: e.to_string(),
            }
        })?;

        // Extract imports and references
        let imports = self.extract_imports(&content);
        let references = self.extract_references(&content);

        // Cache the parsed file
        self.file_cache.insert(path.to_path_buf(), file.clone());

        Ok((file, imports, references))
    }

    /// Extract import specifiers from source code using AST-based parsing.
    ///
    /// This handles:
    /// - `import { Foo } from "module"`
    /// - `import * as Foo from "module"`
    /// - `import "module"` (side-effect imports)
    /// - `export { Foo } from "module"` (re-exports)
    /// - `export * from "module"` (star re-exports)
    /// - `import("module")` in type positions (dynamic imports)
    fn extract_imports(&self, content: &str) -> Vec<String> {
        let allocator = Allocator::default();
        let source_type = SourceType::d_ts();

        let parser_return = Parser::new(&allocator, content, source_type).parse();

        // If parsing fails, return empty - we'll get errors during actual parsing
        if parser_return.panicked {
            return Vec::new();
        }

        let mut imports = Vec::new();
        let mut seen = HashSet::new();

        // Walk through all statements in the program
        for stmt in &parser_return.program.body {
            self.extract_imports_from_statement(stmt, &mut imports, &mut seen);
        }

        imports
    }

    /// Extract import sources from a statement.
    fn extract_imports_from_statement(
        &self,
        stmt: &Statement<'_>,
        imports: &mut Vec<String>,
        seen: &mut HashSet<String>,
    ) {
        match stmt {
            // import ... from "module" or import "module"
            Statement::ImportDeclaration(decl) => {
                let source = decl.source.value.to_string();
                if seen.insert(source.clone()) {
                    imports.push(source);
                }
            }

            // export * from "module"
            Statement::ExportAllDeclaration(decl) => {
                let source = decl.source.value.to_string();
                if seen.insert(source.clone()) {
                    imports.push(source);
                }
            }

            // export { ... } from "module"
            Statement::ExportNamedDeclaration(decl) => {
                if let Some(source) = &decl.source {
                    let source = source.value.to_string();
                    if seen.insert(source.clone()) {
                        imports.push(source);
                    }
                }
            }

            // Handle type aliases and other declarations that may contain import() types
            Statement::TSTypeAliasDeclaration(decl) => {
                self.extract_imports_from_type(&decl.type_annotation, imports, seen);
            }

            Statement::TSInterfaceDeclaration(decl) => {
                // Check extends clauses
                for heritage in &decl.extends {
                    self.extract_imports_from_expression(&heritage.expression, imports, seen);
                }
                // Check interface body members
                for member in &decl.body.body {
                    self.extract_imports_from_ts_signature(member, imports, seen);
                }
            }

            Statement::VariableDeclaration(decl) => {
                for declarator in &decl.declarations {
                    if let Some(annotation) = &declarator.id.type_annotation {
                        self.extract_imports_from_type(&annotation.type_annotation, imports, seen);
                    }
                }
            }

            Statement::FunctionDeclaration(decl) => {
                // Check parameter types
                for param in &decl.params.items {
                    if let Some(annotation) = &param.pattern.type_annotation {
                        self.extract_imports_from_type(&annotation.type_annotation, imports, seen);
                    }
                }
                // Check return type
                if let Some(ret) = &decl.return_type {
                    self.extract_imports_from_type(&ret.type_annotation, imports, seen);
                }
            }

            Statement::ClassDeclaration(decl) => {
                // Check super type arguments
                if let Some(type_args) = &decl.super_type_arguments {
                    for param in &type_args.params {
                        self.extract_imports_from_type(param, imports, seen);
                    }
                }
                // Check implements
                for impl_clause in &decl.implements {
                    if let Some(type_args) = &impl_clause.type_arguments {
                        for param in &type_args.params {
                            self.extract_imports_from_type(param, imports, seen);
                        }
                    }
                }
            }

            Statement::TSModuleDeclaration(decl) => {
                if let Some(body) = &decl.body {
                    self.extract_imports_from_module_body(body, imports, seen);
                }
            }

            _ => {}
        }
    }

    /// Extract imports from a module body.
    fn extract_imports_from_module_body(
        &self,
        body: &TSModuleDeclarationBody<'_>,
        imports: &mut Vec<String>,
        seen: &mut HashSet<String>,
    ) {
        match body {
            TSModuleDeclarationBody::TSModuleDeclaration(nested) => {
                if let Some(nested_body) = &nested.body {
                    self.extract_imports_from_module_body(nested_body, imports, seen);
                }
            }
            TSModuleDeclarationBody::TSModuleBlock(block) => {
                for stmt in &block.body {
                    self.extract_imports_from_statement(stmt, imports, seen);
                }
            }
        }
    }

    /// Extract imports from a type annotation (looking for import() types).
    fn extract_imports_from_type(
        &self,
        ty: &TSType<'_>,
        imports: &mut Vec<String>,
        seen: &mut HashSet<String>,
    ) {
        match ty {
            // import("module").Type - argument is a TSType, extract string literal
            TSType::TSImportType(import_type) => {
                if let Some(source) = self.extract_string_from_type(&import_type.argument) {
                    if seen.insert(source.clone()) {
                        imports.push(source);
                    }
                }
                // Also check type arguments
                if let Some(type_args) = &import_type.type_arguments {
                    for param in &type_args.params {
                        self.extract_imports_from_type(param, imports, seen);
                    }
                }
            }

            TSType::TSTypeReference(type_ref) => {
                if let Some(type_args) = &type_ref.type_arguments {
                    for param in &type_args.params {
                        self.extract_imports_from_type(param, imports, seen);
                    }
                }
            }

            TSType::TSUnionType(union) => {
                for ty in &union.types {
                    self.extract_imports_from_type(ty, imports, seen);
                }
            }

            TSType::TSIntersectionType(intersection) => {
                for ty in &intersection.types {
                    self.extract_imports_from_type(ty, imports, seen);
                }
            }

            TSType::TSArrayType(array) => {
                self.extract_imports_from_type(&array.element_type, imports, seen);
            }

            TSType::TSTupleType(tuple) => {
                for elem in &tuple.element_types {
                    self.extract_imports_from_tuple_element(elem, imports, seen);
                }
            }

            TSType::TSFunctionType(func) => {
                for param in &func.params.items {
                    if let Some(annotation) = &param.pattern.type_annotation {
                        self.extract_imports_from_type(&annotation.type_annotation, imports, seen);
                    }
                }
                // return_type is not optional in TSFunctionType
                self.extract_imports_from_type(&func.return_type.type_annotation, imports, seen);
            }

            TSType::TSConditionalType(cond) => {
                self.extract_imports_from_type(&cond.check_type, imports, seen);
                self.extract_imports_from_type(&cond.extends_type, imports, seen);
                self.extract_imports_from_type(&cond.true_type, imports, seen);
                self.extract_imports_from_type(&cond.false_type, imports, seen);
            }

            TSType::TSMappedType(mapped) => {
                if let Some(ty) = &mapped.type_annotation {
                    self.extract_imports_from_type(ty, imports, seen);
                }
            }

            TSType::TSIndexedAccessType(indexed) => {
                self.extract_imports_from_type(&indexed.object_type, imports, seen);
                self.extract_imports_from_type(&indexed.index_type, imports, seen);
            }

            TSType::TSTypeLiteral(literal) => {
                for member in &literal.members {
                    self.extract_imports_from_ts_signature(member, imports, seen);
                }
            }

            TSType::TSTypeQuery(query) => {
                // Check if the query references an import
                if let TSTypeQueryExprName::TSImportType(import_type) = &query.expr_name {
                    if let Some(source) = self.extract_string_from_type(&import_type.argument) {
                        if seen.insert(source.clone()) {
                            imports.push(source);
                        }
                    }
                }
            }

            TSType::TSParenthesizedType(paren) => {
                self.extract_imports_from_type(&paren.type_annotation, imports, seen);
            }

            TSType::TSTypeOperatorType(op) => {
                self.extract_imports_from_type(&op.type_annotation, imports, seen);
            }

            _ => {}
        }
    }

    /// Extract a string from a TSType (for import type arguments).
    fn extract_string_from_type(&self, ty: &TSType<'_>) -> Option<String> {
        if let TSType::TSLiteralType(lit) = ty {
            if let TSLiteral::StringLiteral(s) = &lit.literal {
                return Some(s.value.to_string());
            }
        }
        None
    }

    /// Extract imports from a tuple element.
    fn extract_imports_from_tuple_element(
        &self,
        elem: &TSTupleElement<'_>,
        imports: &mut Vec<String>,
        seen: &mut HashSet<String>,
    ) {
        match elem {
            TSTupleElement::TSOptionalType(opt) => {
                self.extract_imports_from_type(&opt.type_annotation, imports, seen);
            }
            TSTupleElement::TSRestType(rest) => {
                self.extract_imports_from_type(&rest.type_annotation, imports, seen);
            }
            // TSTupleElement inherits from TSType, so other variants are TSType variants
            _ => {
                // The element itself is a type - try to match it as TSType
                // Since TSTupleElement inherits TSType variants, we check for TSImportType etc.
                if let TSTupleElement::TSImportType(import_type) = elem {
                    if let Some(source) = self.extract_string_from_type(&import_type.argument) {
                        if seen.insert(source.clone()) {
                            imports.push(source);
                        }
                    }
                }
                // For other inherited TSType variants, recursively handle via TSType
                // by extracting the inner type if it's a named tuple member
                if let TSTupleElement::TSNamedTupleMember(member) = elem {
                    self.extract_imports_from_tuple_element(&member.element_type, imports, seen);
                }
            }
        }
    }

    /// Extract imports from a TypeScript signature.
    fn extract_imports_from_ts_signature(
        &self,
        sig: &TSSignature<'_>,
        imports: &mut Vec<String>,
        seen: &mut HashSet<String>,
    ) {
        match sig {
            TSSignature::TSPropertySignature(prop) => {
                if let Some(annotation) = &prop.type_annotation {
                    self.extract_imports_from_type(&annotation.type_annotation, imports, seen);
                }
            }
            TSSignature::TSMethodSignature(method) => {
                for param in &method.params.items {
                    if let Some(annotation) = &param.pattern.type_annotation {
                        self.extract_imports_from_type(&annotation.type_annotation, imports, seen);
                    }
                }
                if let Some(ret) = &method.return_type {
                    self.extract_imports_from_type(&ret.type_annotation, imports, seen);
                }
            }
            TSSignature::TSCallSignatureDeclaration(call) => {
                for param in &call.params.items {
                    if let Some(annotation) = &param.pattern.type_annotation {
                        self.extract_imports_from_type(&annotation.type_annotation, imports, seen);
                    }
                }
                if let Some(ret) = &call.return_type {
                    self.extract_imports_from_type(&ret.type_annotation, imports, seen);
                }
            }
            TSSignature::TSConstructSignatureDeclaration(ctor) => {
                for param in &ctor.params.items {
                    if let Some(annotation) = &param.pattern.type_annotation {
                        self.extract_imports_from_type(&annotation.type_annotation, imports, seen);
                    }
                }
                if let Some(ret) = &ctor.return_type {
                    self.extract_imports_from_type(&ret.type_annotation, imports, seen);
                }
            }
            TSSignature::TSIndexSignature(index) => {
                // type_annotation is not optional in TSIndexSignature
                self.extract_imports_from_type(&index.type_annotation.type_annotation, imports, seen);
            }
        }
    }

    /// Extract imports from an expression (for extends clauses).
    fn extract_imports_from_expression(
        &self,
        _expr: &Expression<'_>,
        _imports: &mut Vec<String>,
        _seen: &mut HashSet<String>,
    ) {
        // Most expressions in extends clauses are identifiers, not imports
        // But this could be extended to handle more complex cases
    }

    /// Extract triple-slash reference paths.
    fn extract_references(&self, content: &str) -> Vec<String> {
        let mut references = Vec::new();

        for line in content.lines() {
            let trimmed = line.trim();

            // /// <reference path="..." />
            if trimmed.starts_with("/// <reference path=") {
                if let Some(path) = extract_reference_path(trimmed) {
                    references.push(path);
                }
            }
            // /// <reference types="..." /> (for @types packages)
            else if trimmed.starts_with("/// <reference types=") {
                if let Some(types) = extract_reference_types(trimmed) {
                    // Convert to @types package reference
                    references.push(format!("@types/{}", types));
                }
            }
        }

        references
    }

    /// Resolve an import specifier to a file path.
    fn resolve_import(&self, specifier: &str, from_file: &Path) -> Option<PathBuf> {
        // Relative import
        if specifier.starts_with("./") || specifier.starts_with("../") {
            let from_dir = from_file.parent()?;
            let resolved = from_dir.join(specifier);
            return self.resolve_with_extensions(&resolved);
        }

        // Absolute import (package)
        self.resolve_package(specifier, from_file)
    }

    /// Resolve a triple-slash reference path.
    fn resolve_reference(&self, ref_path: &str, from_file: &Path) -> Option<PathBuf> {
        // @types package reference
        if ref_path.starts_with("@types/") {
            return self.resolve_package(ref_path, from_file);
        }

        // Relative path reference
        let from_dir = from_file.parent()?;
        let resolved = from_dir.join(ref_path);
        self.resolve_with_extensions(&resolved)
    }

    /// Resolve a package import to a file path.
    fn resolve_package(&self, package: &str, from_file: &Path) -> Option<PathBuf> {
        // Try node_modules resolution
        let mut search_dir = from_file.parent();

        while let Some(dir) = search_dir {
            let node_modules = dir.join("node_modules");
            if node_modules.exists() {
                if let Some(resolved) = self.resolve_in_node_modules(&node_modules, package) {
                    return Some(resolved);
                }
            }
            search_dir = dir.parent();
        }

        // Try configured module paths
        for module_path in &self.options.module_paths {
            if let Some(resolved) = self.resolve_in_node_modules(module_path, package) {
                return Some(resolved);
            }
        }

        None
    }

    /// Resolve a package within a node_modules directory.
    fn resolve_in_node_modules(&self, node_modules: &Path, package: &str) -> Option<PathBuf> {
        let package_dir = node_modules.join(package);

        if !package_dir.exists() {
            return None;
        }

        // Try package.json "types" or "typings" field
        let package_json = package_dir.join("package.json");
        if package_json.exists() {
            if let Some(types_path) = self.read_types_field(&package_json) {
                let resolved = package_dir.join(&types_path);
                if resolved.exists() {
                    return Some(resolved);
                }
            }
        }

        // Try index.d.ts
        let index_dts = package_dir.join("index.d.ts");
        if index_dts.exists() {
            return Some(index_dts);
        }

        // Try package_name.d.ts (for scoped packages, use the package name)
        let basename = package.split('/').last().unwrap_or(package);
        let named_dts = package_dir.join(format!("{}.d.ts", basename));
        if named_dts.exists() {
            return Some(named_dts);
        }

        None
    }

    /// Read the "types" or "typings" field from package.json.
    fn read_types_field(&self, package_json: &Path) -> Option<String> {
        let content = std::fs::read_to_string(package_json).ok()?;

        // Simple JSON parsing for "types" or "typings" field
        // This is a simplified implementation; a real one would use serde_json
        for line in content.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("\"types\"") || trimmed.starts_with("\"typings\"") {
                if let Some(colon_idx) = trimmed.find(':') {
                    let value_part = trimmed[colon_idx + 1..].trim();
                    return extract_quoted_string(value_part);
                }
            }
        }

        None
    }

    /// Try to resolve a path with TypeScript extensions.
    fn resolve_with_extensions(&self, path: &Path) -> Option<PathBuf> {
        // If path already exists as-is
        if path.exists() {
            return Some(path.to_path_buf());
        }

        // Try with .d.ts extension
        let with_dts = path.with_extension("d.ts");
        if with_dts.exists() {
            return Some(with_dts);
        }

        // Try with .ts extension
        let with_ts = path.with_extension("ts");
        if with_ts.exists() {
            return Some(with_ts);
        }

        // Try as directory with index.d.ts
        if path.is_dir() {
            let index_dts = path.join("index.d.ts");
            if index_dts.exists() {
                return Some(index_dts);
            }
        }

        // Path doesn't have extension - try adding /index.d.ts
        let index_dts = path.join("index.d.ts");
        if index_dts.exists() {
            return Some(index_dts);
        }

        None
    }

    /// Normalize a path to an absolute path.
    fn normalize_path(&self, path: &Path) -> PathBuf {
        if path.is_absolute() {
            path.to_path_buf()
        } else if let Some(base_dir) = &self.options.base_dir {
            base_dir.join(path)
        } else {
            std::env::current_dir()
                .map(|cwd| cwd.join(path))
                .unwrap_or_else(|_| path.to_path_buf())
        }
    }
}

/// Extract a quoted string (single or double quotes).
fn extract_quoted_string(s: &str) -> Option<String> {
    let trimmed = s.trim();

    if trimmed.starts_with('"') {
        let end = trimmed[1..].find('"')?;
        return Some(trimmed[1..=end].to_string());
    }

    if trimmed.starts_with('\'') {
        let end = trimmed[1..].find('\'')?;
        return Some(trimmed[1..=end].to_string());
    }

    None
}

/// Extract path from a triple-slash reference.
fn extract_reference_path(line: &str) -> Option<String> {
    // /// <reference path="..." />
    let start = line.find("path=\"")? + 6;
    let rest = &line[start..];
    let end = rest.find('"')?;
    Some(rest[..end].to_string())
}

/// Extract types from a triple-slash reference.
fn extract_reference_types(line: &str) -> Option<String> {
    // /// <reference types="..." />
    let start = line.find("types=\"")? + 7;
    let rest = &line[start..];
    let end = rest.find('"')?;
    Some(rest[..end].to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_imports() {
        let resolver = Resolver::new(ResolveOptions::default());

        let content = r#"
            import { Foo } from "./foo";
            import * as Bar from "../bar";
            import "side-effect";
            export { Baz } from "@types/baz";
            type X = import("./dynamic").Type;
        "#;

        let imports = resolver.extract_imports(content);
        assert_eq!(imports.len(), 5);
        assert!(imports.contains(&"./foo".to_string()));
        assert!(imports.contains(&"../bar".to_string()));
        assert!(imports.contains(&"side-effect".to_string()));
        assert!(imports.contains(&"@types/baz".to_string()));
        assert!(imports.contains(&"./dynamic".to_string()));
    }

    #[test]
    fn test_extract_references() {
        let resolver = Resolver::new(ResolveOptions::default());

        let content = r#"
            /// <reference path="./types.d.ts" />
            /// <reference types="node" />
        "#;

        let references = resolver.extract_references(content);
        assert_eq!(references.len(), 2);
        assert!(references.contains(&"./types.d.ts".to_string()));
        assert!(references.contains(&"@types/node".to_string()));
    }

    #[test]
    fn test_extract_quoted_string() {
        assert_eq!(
            extract_quoted_string("\"hello\""),
            Some("hello".to_string())
        );
        assert_eq!(
            extract_quoted_string("'world'"),
            Some("world".to_string())
        );
        assert_eq!(extract_quoted_string("no quotes"), None);
    }
}
