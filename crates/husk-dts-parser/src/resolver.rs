//! Multi-file resolver for TypeScript declaration files.
//!
//! This module handles:
//! - Resolving import statements to file paths
//! - Walking the import graph to collect all declarations
//! - Handling `/// <reference path="..." />` directives
//! - Node modules resolution for package imports
//! - Detecting `export =` assignments for CommonJS module identity
//!
//! ## Export Assignment Linking
//!
//! For CommonJS modules that use `export =`, this module tracks the exported
//! identifier and determines the module's identity (function, class, hybrid,
//! or namespace). This allows the codegen to emit appropriate bindings:
//!
//! ```typescript
//! // Function module (like express):
//! declare function e(): void;
//! export = e;  // -> ModuleIdentity::Function
//!
//! // Hybrid module (function + namespace):
//! function req(url: string): void;
//! namespace req { interface Options { x: number; } }
//! export = req;  // -> ModuleIdentity::Hybrid
//! ```
//!
//! Use `ResolvedModule::get_module_identity()` to determine how to generate
//! bindings for a resolved module.

use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};

use oxc_allocator::Allocator;
use oxc_ast::ast::*;
use oxc_parser::Parser;
use oxc_span::SourceType;
use serde_json::Value;

use crate::ast::{DtsExport, DtsFile, DtsItem};
use crate::oxc_parser::parse_with_oxc;

/// The identity/type of a CommonJS module based on its `export =` statement.
///
/// TypeScript's `export =` can export different kinds of things:
/// - A function (callable module like `express`)
/// - A class (class module like `request`)
/// - A namespace with merged function/class (hybrid like `request` with options)
/// - Just a namespace (standard ES module style)
#[derive(Debug, Clone, PartialEq)]
pub enum ModuleIdentity {
    /// Standard ES module - just a container of named exports
    StandardModule,
    /// Module exports a function: `export = myFunc`
    Function {
        /// The name of the exported function in the file
        name: String,
    },
    /// Module exports a class: `export = MyClass`
    Class {
        /// The name of the exported class in the file
        name: String,
    },
    /// Module exports a function/class that also has a merged namespace
    /// Example: `function req(...); namespace req { ... }; export = req;`
    Hybrid {
        /// The name of the callable (function or class)
        name: String,
        /// Whether the callable is a function (true) or class (false)
        is_function: bool,
    },
    /// Module exports a namespace or other non-callable: `export = MyNamespace`
    Namespace {
        /// The name of the exported namespace
        name: String,
    },
}

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
    /// Map of FilePath -> The identifier it exports via `export =`
    /// Example: "node_modules/@types/express/index.d.ts" -> "e"
    pub export_assignments: HashMap<PathBuf, String>,
}

impl ResolvedModule {
    /// Get all items from all files in dependency order (leaf dependencies first).
    ///
    /// Note: This method does not surface circular import errors. Use
    /// `all_items_with_cycle_errors` if you need to detect cycles.
    pub fn all_items(&self) -> Vec<&DtsItem> {
        let (order, _errors) = self.topological_order();
        self.items_from_order(order)
    }

    /// Get all items from all files in dependency order, also returning any cycle errors.
    ///
    /// Returns `(items, cycle_errors)` where `cycle_errors` contains any circular
    /// import errors detected during topological sorting.
    pub fn all_items_with_cycle_errors(&self) -> (Vec<&DtsItem>, Vec<ResolveError>) {
        let (order, errors) = self.topological_order();
        (self.items_from_order(order), errors)
    }

    /// Get items only from the entry file (not dependencies).
    ///
    /// This is used when `entry_file_only` mode is enabled. Dependencies are
    /// still available via `all_items()` for type resolution, but only entry
    /// file items are emitted in the output.
    pub fn entry_file_items(&self) -> Vec<&DtsItem> {
        self.files
            .get(&self.entry_path)
            .map(|f| f.items.iter().collect())
            .unwrap_or_default()
    }

    /// Get items from dependency files only (excludes entry file).
    ///
    /// This is useful for collecting type information from dependencies
    /// without including their items in the output.
    pub fn dependency_items(&self) -> Vec<&DtsItem> {
        self.files
            .iter()
            .filter(|(path, _)| *path != &self.entry_path)
            .flat_map(|(_, file)| file.items.iter())
            .collect()
    }

    /// Helper to extract items from an ordered list of paths.
    fn items_from_order(&self, mut order: Vec<PathBuf>) -> Vec<&DtsItem> {
        order.reverse(); // Leaf dependencies first

        order
            .iter()
            .flat_map(|path| {
                self.files
                    .get(path)
                    .map(|f| f.items.iter())
                    .unwrap_or_default()
            })
            .collect()
    }

    /// Get files in topological order (dependencies before dependents).
    ///
    /// Returns both the ordered paths and any circular import errors detected.
    fn topological_order(&self) -> (Vec<PathBuf>, Vec<ResolveError>) {
        let mut result = Vec::new();
        let mut errors = Vec::new();
        let mut visited = HashSet::new();
        let mut temp_visited = Vec::new(); // Use Vec to preserve order for cycle detection

        for path in self.files.keys() {
            self.topo_visit(
                path,
                &mut visited,
                &mut temp_visited,
                &mut result,
                &mut errors,
            );
        }

        (result, errors)
    }

    fn topo_visit(
        &self,
        path: &Path,
        visited: &mut HashSet<PathBuf>,
        temp_visited: &mut Vec<PathBuf>,
        result: &mut Vec<PathBuf>,
        errors: &mut Vec<ResolveError>,
    ) {
        if visited.contains(path) {
            return;
        }

        // Check for cycle: if path is in temp_visited, we have a circular import
        if let Some(cycle_start_idx) = temp_visited.iter().position(|p| p == path) {
            // Collect the cycle: from the first occurrence of path to the end, plus path again
            let mut cycle: Vec<PathBuf> = temp_visited[cycle_start_idx..].to_vec();
            cycle.push(path.to_path_buf()); // Complete the cycle back to the starting node

            errors.push(ResolveError::CircularImport {
                path: path.to_path_buf(),
                cycle,
            });

            // Just record the error and return - don't modify visited or result here.
            // The original call that first added this path to temp_visited will handle
            // adding it to visited and result when it unwinds normally.
            return;
        }

        temp_visited.push(path.to_path_buf());

        if let Some(deps) = self.import_graph.get(path) {
            for dep in deps {
                self.topo_visit(dep, visited, temp_visited, result, errors);
            }
        }

        temp_visited.pop();
        visited.insert(path.to_path_buf());
        result.push(path.to_path_buf());
    }

    /// Get the module identity for a given file path.
    ///
    /// This determines what kind of module it is based on `export =`:
    /// - Function (callable module like express)
    /// - Class (class module)
    /// - Hybrid (function/class with merged namespace)
    /// - Namespace (just a namespace export)
    /// - StandardModule (no export =, just named exports)
    pub fn get_module_identity(&self, module_path: &Path) -> ModuleIdentity {
        // Check if this file has an export assignment
        let Some(exported_id) = self.export_assignments.get(module_path) else {
            return ModuleIdentity::StandardModule;
        };

        // Look up what the exported identifier actually IS in that file
        let Some(file) = self.files.get(module_path) else {
            return ModuleIdentity::StandardModule;
        };

        self.lookup_symbol_identity(file, exported_id)
    }

    /// Look up a symbol in a file and determine its identity.
    ///
    /// This checks if the symbol is a function, class, namespace, or a hybrid
    /// (function/class with merged namespace).
    fn lookup_symbol_identity(&self, file: &DtsFile, symbol_name: &str) -> ModuleIdentity {
        let mut has_function = false;
        let mut has_class = false;
        let mut has_namespace = false;
        let mut has_constructor_var = false;

        for item in &file.items {
            match item {
                DtsItem::Function(f) if f.name == symbol_name => {
                    has_function = true;
                }
                DtsItem::Class(c) if c.name == symbol_name => {
                    has_class = true;
                }
                DtsItem::Namespace(ns) if ns.name == symbol_name => {
                    has_namespace = true;
                }
                DtsItem::Interface(iface) if iface.name == symbol_name => {
                    // An interface by itself isn't callable, but might be merged with function/class
                    // We don't set any flag here, just note it exists
                }
                DtsItem::Variable(v) if v.name == symbol_name => {
                    // Check if the variable type indicates a constructor
                    // Common patterns: `const X: XConstructor` or `const X: typeof SomeClass`
                    let type_name = format!("{:?}", v.ty);
                    if type_name.contains("Constructor") || type_name.contains("typeof") {
                        has_constructor_var = true;
                    }
                }
                _ => {}
            }
        }

        // Determine the identity based on what we found
        match (has_function, has_class, has_namespace, has_constructor_var) {
            // Function + namespace = Hybrid (like express)
            (true, false, true, _) => ModuleIdentity::Hybrid {
                name: symbol_name.to_string(),
                is_function: true,
            },
            // Class + namespace = Hybrid (like request with Options)
            (false, true, true, _) => ModuleIdentity::Hybrid {
                name: symbol_name.to_string(),
                is_function: false,
            },
            // Constructor var + namespace = Class module (like better-sqlite3)
            // The const is the constructor, namespace provides types
            (false, false, true, true) => ModuleIdentity::Class {
                name: symbol_name.to_string(),
            },
            // Just function
            (true, false, false, _) => ModuleIdentity::Function {
                name: symbol_name.to_string(),
            },
            // Just class
            (false, true, false, _) => ModuleIdentity::Class {
                name: symbol_name.to_string(),
            },
            // Constructor var without namespace
            (false, false, false, true) => ModuleIdentity::Class {
                name: symbol_name.to_string(),
            },
            // Just namespace (or nothing found that's callable)
            (false, false, true, false) => ModuleIdentity::Namespace {
                name: symbol_name.to_string(),
            },
            // Both function and class with same name (unusual, treat as function)
            (true, true, _, _) => ModuleIdentity::Hybrid {
                name: symbol_name.to_string(),
                is_function: true,
            },
            // Nothing found - might be a variable or type alias
            (false, false, false, false) => {
                // Check for variable declarations that might be callable
                for item in &file.items {
                    if let DtsItem::Variable(v) = item {
                        if v.name == symbol_name {
                            // A variable could be a function or object, treat as namespace
                            return ModuleIdentity::Namespace {
                                name: symbol_name.to_string(),
                            };
                        }
                    }
                }
                // Default to standard module if we can't find the symbol
                ModuleIdentity::StandardModule
            }
        }
    }
}

/// Error during module resolution.
#[derive(Debug, Clone)]
pub enum ResolveError {
    /// File not found
    FileNotFound {
        path: PathBuf,
        from: Option<PathBuf>,
    },
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
                    write!(
                        f,
                        "File not found: {:?} (imported from {:?})",
                        path, from_path
                    )
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
    /// Cache of parsed files with their imports and references
    file_cache: HashMap<PathBuf, CachedFile>,
}

/// Cached file with its parsed content and extracted imports/references.
struct CachedFile {
    file: DtsFile,
    imports: Vec<String>,
    references: Vec<String>,
}

impl Resolver {
    /// Create a new resolver with the given options.
    pub fn new(options: ResolveOptions) -> Self {
        Self {
            options,
            file_cache: HashMap::new(),
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
            export_assignments: HashMap::new(),
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

        // Populate export assignments by scanning all resolved files for `export =`
        for (path, file) in &module.files {
            for item in &file.items {
                if let DtsItem::Export(DtsExport::Equals(name)) = item {
                    module.export_assignments.insert(path.clone(), name.clone());
                    break; // A file can only have one export assignment
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
        // Check cache first - now includes imports/references
        if let Some(cached) = self.file_cache.get(path) {
            return Ok((
                cached.file.clone(),
                cached.imports.clone(),
                cached.references.clone(),
            ));
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

        // Cache the parsed file with imports/references
        self.file_cache.insert(
            path.to_path_buf(),
            CachedFile {
                file: file.clone(),
                imports: imports.clone(),
                references: references.clone(),
            },
        );

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
                self.extract_imports_from_type(
                    &index.type_annotation.type_annotation,
                    imports,
                    seen,
                );
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

    /// Read the "types" or "typings" field from a package.json file.
    ///
    /// Uses serde_json for robust parsing that handles escaped quotes,
    /// minified JSON, and other edge cases correctly.
    fn read_types_field(&self, package_json: &Path) -> Option<String> {
        let content = std::fs::read_to_string(package_json).ok()?;
        let json: Value = serde_json::from_str(&content).ok()?;

        // Try "types" first, then fall back to "typings"
        json.get("types")
            .or_else(|| json.get("typings"))
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
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
    fn test_module_identity_function() {
        use crate::ast::{DtsFunction, DtsType, Primitive};

        // Create a file with a function and export = e
        let file = DtsFile {
            items: vec![
                DtsItem::Function(DtsFunction {
                    name: "e".to_string(),
                    type_params: vec![],
                    params: vec![],
                    return_type: Some(DtsType::Primitive(Primitive::Void)),
                    this_param: None,
                }),
                DtsItem::Export(DtsExport::Equals("e".to_string())),
            ],
        };

        let mut module = ResolvedModule {
            entry_path: PathBuf::from("/test.d.ts"),
            files: HashMap::new(),
            import_graph: HashMap::new(),
            errors: vec![],
            export_assignments: HashMap::new(),
        };
        module.files.insert(PathBuf::from("/test.d.ts"), file);
        module
            .export_assignments
            .insert(PathBuf::from("/test.d.ts"), "e".to_string());

        let identity = module.get_module_identity(Path::new("/test.d.ts"));
        assert_eq!(
            identity,
            ModuleIdentity::Function {
                name: "e".to_string()
            }
        );
    }

    #[test]
    fn test_module_identity_class() {
        use crate::ast::DtsClass;

        // Create a file with a class and export = MyClass
        let file = DtsFile {
            items: vec![
                DtsItem::Class(DtsClass {
                    name: "MyClass".to_string(),
                    type_params: vec![],
                    extends: None,
                    implements: vec![],
                    members: vec![],
                }),
                DtsItem::Export(DtsExport::Equals("MyClass".to_string())),
            ],
        };

        let mut module = ResolvedModule {
            entry_path: PathBuf::from("/test.d.ts"),
            files: HashMap::new(),
            import_graph: HashMap::new(),
            errors: vec![],
            export_assignments: HashMap::new(),
        };
        module.files.insert(PathBuf::from("/test.d.ts"), file);
        module
            .export_assignments
            .insert(PathBuf::from("/test.d.ts"), "MyClass".to_string());

        let identity = module.get_module_identity(Path::new("/test.d.ts"));
        assert_eq!(
            identity,
            ModuleIdentity::Class {
                name: "MyClass".to_string()
            }
        );
    }

    #[test]
    fn test_module_identity_hybrid() {
        use crate::ast::{DtsFunction, DtsNamespace, DtsType, Primitive};

        // Create a file with a function + namespace with same name (hybrid like express)
        let file = DtsFile {
            items: vec![
                DtsItem::Function(DtsFunction {
                    name: "req".to_string(),
                    type_params: vec![],
                    params: vec![],
                    return_type: Some(DtsType::Primitive(Primitive::Void)),
                    this_param: None,
                }),
                DtsItem::Namespace(DtsNamespace {
                    name: "req".to_string(),
                    items: vec![],
                }),
                DtsItem::Export(DtsExport::Equals("req".to_string())),
            ],
        };

        let mut module = ResolvedModule {
            entry_path: PathBuf::from("/test.d.ts"),
            files: HashMap::new(),
            import_graph: HashMap::new(),
            errors: vec![],
            export_assignments: HashMap::new(),
        };
        module.files.insert(PathBuf::from("/test.d.ts"), file);
        module
            .export_assignments
            .insert(PathBuf::from("/test.d.ts"), "req".to_string());

        let identity = module.get_module_identity(Path::new("/test.d.ts"));
        assert_eq!(
            identity,
            ModuleIdentity::Hybrid {
                name: "req".to_string(),
                is_function: true
            }
        );
    }

    #[test]
    fn test_module_identity_namespace() {
        use crate::ast::DtsNamespace;

        // Create a file with only a namespace and export = ns
        let file = DtsFile {
            items: vec![
                DtsItem::Namespace(DtsNamespace {
                    name: "ns".to_string(),
                    items: vec![],
                }),
                DtsItem::Export(DtsExport::Equals("ns".to_string())),
            ],
        };

        let mut module = ResolvedModule {
            entry_path: PathBuf::from("/test.d.ts"),
            files: HashMap::new(),
            import_graph: HashMap::new(),
            errors: vec![],
            export_assignments: HashMap::new(),
        };
        module.files.insert(PathBuf::from("/test.d.ts"), file);
        module
            .export_assignments
            .insert(PathBuf::from("/test.d.ts"), "ns".to_string());

        let identity = module.get_module_identity(Path::new("/test.d.ts"));
        assert_eq!(
            identity,
            ModuleIdentity::Namespace {
                name: "ns".to_string()
            }
        );
    }

    #[test]
    fn test_module_identity_standard_module() {
        use crate::ast::{DtsFunction, DtsType, Primitive};

        // Create a file WITHOUT export = (standard ES module)
        let file = DtsFile {
            items: vec![DtsItem::Function(DtsFunction {
                name: "foo".to_string(),
                type_params: vec![],
                params: vec![],
                return_type: Some(DtsType::Primitive(Primitive::Void)),
                this_param: None,
            })],
        };

        let mut module = ResolvedModule {
            entry_path: PathBuf::from("/test.d.ts"),
            files: HashMap::new(),
            import_graph: HashMap::new(),
            errors: vec![],
            export_assignments: HashMap::new(),
        };
        module.files.insert(PathBuf::from("/test.d.ts"), file);
        // No export_assignment for this file

        let identity = module.get_module_identity(Path::new("/test.d.ts"));
        assert_eq!(identity, ModuleIdentity::StandardModule);
    }

    #[test]
    fn test_circular_import_detection() {
        // Create a cycle: A -> B -> C -> A
        let file_a = DtsFile { items: vec![] };
        let file_b = DtsFile { items: vec![] };
        let file_c = DtsFile { items: vec![] };

        let mut module = ResolvedModule {
            entry_path: PathBuf::from("/a.d.ts"),
            files: HashMap::new(),
            import_graph: HashMap::new(),
            errors: vec![],
            export_assignments: HashMap::new(),
        };

        module.files.insert(PathBuf::from("/a.d.ts"), file_a);
        module.files.insert(PathBuf::from("/b.d.ts"), file_b);
        module.files.insert(PathBuf::from("/c.d.ts"), file_c);

        // Create import graph: A -> B -> C -> A
        module
            .import_graph
            .insert(PathBuf::from("/a.d.ts"), vec![PathBuf::from("/b.d.ts")]);
        module
            .import_graph
            .insert(PathBuf::from("/b.d.ts"), vec![PathBuf::from("/c.d.ts")]);
        module
            .import_graph
            .insert(PathBuf::from("/c.d.ts"), vec![PathBuf::from("/a.d.ts")]);

        // Get items with cycle errors
        let (_items, errors) = module.all_items_with_cycle_errors();

        // Should have detected a circular import
        assert!(!errors.is_empty(), "Expected circular import error");

        // Check that the error contains the cycle
        if let ResolveError::CircularImport { path, cycle } = &errors[0] {
            // The cycle should contain the path that was being visited when the cycle was detected
            assert!(cycle.len() >= 2, "Cycle should have at least 2 nodes");
            // The cycle should end with the same path it starts with
            assert_eq!(cycle.first(), cycle.last(), "Cycle should be closed");
            // The path should be in the cycle
            assert!(cycle.contains(path), "Path should be in the cycle");
        } else {
            panic!("Expected CircularImport error, got {:?}", errors[0]);
        }
    }

    #[test]
    fn test_no_cycle_no_error() {
        // Create a DAG: A -> B -> C (no cycle)
        let file_a = DtsFile { items: vec![] };
        let file_b = DtsFile { items: vec![] };
        let file_c = DtsFile { items: vec![] };

        let mut module = ResolvedModule {
            entry_path: PathBuf::from("/a.d.ts"),
            files: HashMap::new(),
            import_graph: HashMap::new(),
            errors: vec![],
            export_assignments: HashMap::new(),
        };

        module.files.insert(PathBuf::from("/a.d.ts"), file_a);
        module.files.insert(PathBuf::from("/b.d.ts"), file_b);
        module.files.insert(PathBuf::from("/c.d.ts"), file_c);

        // Create import graph: A -> B -> C (no cycle)
        module
            .import_graph
            .insert(PathBuf::from("/a.d.ts"), vec![PathBuf::from("/b.d.ts")]);
        module
            .import_graph
            .insert(PathBuf::from("/b.d.ts"), vec![PathBuf::from("/c.d.ts")]);

        // Get items with cycle errors
        let (_items, errors) = module.all_items_with_cycle_errors();

        // Should have no errors
        assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
    }
}
