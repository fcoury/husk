//! Generate Husk `extern "js"` code from parsed .d.ts AST.

use crate::ast::*;
use crate::builder::{self, BuilderConfig};
use crate::diagnostics::CodegenMetrics;
use crate::resolver::{ModuleIdentity, ResolvedModule};
use crate::unions::{self, analyze_union};
use crate::utility_types::{expand_type, ExpansionContext, TypeRegistry};
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

/// Options for code generation.
#[derive(Debug, Clone, Default)]
pub struct CodegenOptions {
    /// Module name for `mod` declaration in extern block.
    pub module_name: Option<String>,
    /// Include verbose warnings as comments.
    pub verbose: bool,
    /// Module identity from `export =` analysis.
    /// This affects how the module is generated:
    /// - StandardModule: Generate as normal extern module
    /// - Function: Generate a top-level callable function
    /// - Class: Generate a struct with constructor
    /// - Hybrid: Generate both function and module contents
    /// - Namespace: Generate as namespace export
    pub module_identity: Option<ModuleIdentity>,
    /// Strategy for handling union types.
    pub union_strategy: UnionStrategy,
    /// Whether to generate builder patterns for interfaces with many optional properties.
    pub generate_builders: bool,
    /// Minimum number of optional properties to trigger builder generation.
    pub builder_min_optional: usize,
    /// Whether to expand utility types (Partial, Pick, Omit, etc.) inline.
    pub expand_utility_types: bool,
    /// Whether to emit `extern "js" const` instead of zero-arg functions for TypeScript constants.
    /// When true: `declare const VERSION: string;` → `extern "js" const VERSION: String;`
    /// When false (default): `declare const VERSION: string;` → `extern "js" fn VERSION() -> String;`
    pub use_extern_const: bool,
}

/// Strategy for handling TypeScript union types in generated code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum UnionStrategy {
    /// Automatically choose based on union structure (default).
    /// - String literals → enum
    /// - Nullable → Option<T>
    /// - Small unions → enum (if < 5 variants)
    /// - Large/complex unions → JsValue
    #[default]
    Auto,
    /// Always generate enums for unions (may fail for complex types).
    Enum,
    /// Always fall back to JsValue for unions.
    JsValue,
}

/// Warnings generated during code generation.
#[derive(Debug, Clone)]
pub struct Warning {
    pub message: String,
    pub kind: WarningKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WarningKind {
    Unsupported,
    Simplified,
    Skipped,
}

/// Result of code generation.
#[derive(Debug, Clone)]
pub struct CodegenResult {
    pub code: String,
    pub warnings: Vec<Warning>,
    /// Metrics collected during code generation.
    pub metrics: CodegenMetrics,
}

/// Generate Husk code from a parsed .d.ts file.
pub fn generate(file: &DtsFile, options: &CodegenOptions) -> CodegenResult {
    let mut codegen = Codegen::new(options);
    codegen.generate_file(file);
    CodegenResult {
        code: codegen.output,
        warnings: codegen.warnings,
        metrics: codegen.metrics,
    }
}

/// Generate Husk code from a resolved module graph.
///
/// This processes all files in the resolved module in dependency order,
/// generating code for types from imported files as well as the entry file.
/// Types are deduplicated by name, and dependencies are processed before
/// dependents to ensure proper type resolution.
pub fn generate_from_module(
    resolved: &ResolvedModule,
    options: &CodegenOptions,
) -> CodegenResult {
    let mut codegen = Codegen::new(options);

    // Get items in dependency order (leaf dependencies first)
    let (items, cycle_errors) = resolved.all_items_with_cycle_errors();

    // Report any circular import warnings
    for err in cycle_errors {
        codegen.warn(
            WarningKind::Simplified,
            format!("Circular import detected: {}", err),
        );
    }

    // Pre-pass: collect all struct names from all files first
    for item in &items {
        codegen.collect_struct_names(item);
    }

    // First pass: collect all declarations from all files
    for item in items {
        codegen.collect_item(item);
    }

    // Merge overloaded functions and methods
    codegen.merge_function_overloads();
    codegen.merge_method_overloads();

    // Resolve interface inheritance (copy parent methods to children)
    codegen.resolve_interface_inheritance();

    // Second pass: generate code
    codegen.emit_header();
    codegen.emit_extern_block();
    codegen.emit_untagged_enums();
    codegen.emit_impl_blocks();
    codegen.emit_builders();
    codegen.emit_warnings();

    CodegenResult {
        code: codegen.output,
        warnings: codegen.warnings,
        metrics: codegen.metrics,
    }
}

struct Codegen<'a> {
    options: &'a CodegenOptions,
    output: String,
    warnings: Vec<Warning>,
    /// Metrics collected during code generation.
    metrics: CodegenMetrics,
    /// Collected struct names and their type parameters (from interfaces and classes).
    /// Key: struct name, Value: list of type parameter names.
    structs: HashMap<String, Vec<String>>,
    /// Collected functions (before overload merging).
    functions: Vec<GeneratedFn>,
    /// Collected extern consts (from TypeScript `declare const`).
    consts: Vec<GeneratedConst>,
    /// Collected untagged enums (from TypeScript union types in type aliases).
    enums: Vec<GeneratedEnum>,
    /// Collected impl blocks: type name -> methods (before overload merging).
    impls: HashMap<String, Vec<GeneratedMethod>>,
    /// Collected properties: type name -> properties with #[getter] syntax.
    properties: HashMap<String, Vec<GeneratedProperty>>,
    /// Type aliases (for reference, not always generated).
    type_aliases: HashMap<String, DtsType>,
    /// Known generic types (struct-level type parameters).
    known_generics: HashSet<String>,
    /// Method-level type parameters (for the current method being processed).
    method_type_params: HashSet<String>,
    /// Whether we're currently processing an interface or class (vs a function).
    in_struct_context: bool,
    /// The name of the current struct/interface being processed (for `this` type).
    current_type_name: Option<String>,
    /// Collected interfaces for builder generation (only stored when generate_builders is enabled).
    interfaces: Vec<DtsInterface>,
    /// Type registry for utility type expansion (only populated when expand_utility_types is enabled).
    type_registry: TypeRegistry,
    /// Tracks which type aliases are currently being expanded to detect cycles.
    expanding_aliases: HashSet<String>,
    /// Tracks interface inheritance: child interface name -> list of parent interface names.
    /// Used to copy methods from parent interfaces to child impl blocks.
    interface_extends: HashMap<String, Vec<String>>,
}

struct GeneratedFn {
    name: String,
    type_params: Vec<String>,
    params: Vec<(String, String)>,
    return_type: Option<String>,
    comment: Option<String>,
    /// If Some, the function has a `this` parameter binding that requires #[this] attribute
    this_param: Option<String>,
}

struct GeneratedMethod {
    name: String,
    type_params: Vec<String>,
    params: Vec<(String, String)>,
    return_type: Option<String>,
    is_static: bool,
    comment: Option<String>,
    /// If Some, the method has a `this` parameter binding that requires #[this] attribute
    this_param: Option<String>,
}

/// A generated property with #[getter] attribute.
struct GeneratedProperty {
    name: String,
    ty: String,
    is_readonly: bool,
    is_static: bool,
}

/// A generated extern const declaration.
struct GeneratedConst {
    name: String,
    ty: String,
}

/// A generated untagged enum for TypeScript union types.
struct GeneratedEnum {
    name: String,
    /// Each variant is (variant_name, Option<inner_type>).
    /// For example: `Str(String)` would be ("Str", Some("String"))
    /// Unit variants have None for the inner type.
    variants: Vec<(String, Option<String>)>,
}

impl<'a> Codegen<'a> {
    fn new(options: &'a CodegenOptions) -> Self {
        Self {
            options,
            output: String::new(),
            warnings: Vec::new(),
            metrics: CodegenMetrics::new(),
            structs: HashMap::new(),
            functions: Vec::new(),
            consts: Vec::new(),
            enums: Vec::new(),
            impls: HashMap::new(),
            properties: HashMap::new(),
            type_aliases: HashMap::new(),
            known_generics: HashSet::new(),
            method_type_params: HashSet::new(),
            in_struct_context: false,
            current_type_name: None,
            interfaces: Vec::new(),
            type_registry: TypeRegistry::new(),
            expanding_aliases: HashSet::new(),
            interface_extends: HashMap::new(),
        }
    }

    fn warn(&mut self, kind: WarningKind, message: impl Into<String>) {
        self.warnings.push(Warning {
            message: message.into(),
            kind,
        });
    }

    fn generate_file(&mut self, file: &DtsFile) {
        // Pre-pass: collect all struct names first so type mapping knows what's defined
        for item in &file.items {
            self.collect_struct_names(item);
        }

        // First pass: collect all declarations with proper type mapping
        for item in &file.items {
            self.collect_item(item);
        }

        // Merge overloaded functions and methods
        self.merge_function_overloads();
        self.merge_method_overloads();

        // Resolve interface inheritance (copy parent methods to children)
        self.resolve_interface_inheritance();

        // Second pass: generate code
        self.emit_header();
        self.emit_extern_block();
        self.emit_untagged_enums();
        self.emit_impl_blocks();
        self.emit_builders();
        self.emit_warnings();
    }

    /// Merge overloaded top-level functions into single signatures.
    fn merge_function_overloads(&mut self) {
        let mut merged: HashMap<String, Vec<GeneratedFn>> = HashMap::new();

        // Group functions by name
        for f in std::mem::take(&mut self.functions) {
            merged.entry(f.name.clone()).or_default().push(f);
        }

        // Merge each group
        for (name, overloads) in merged {
            if overloads.len() == 1 {
                self.functions.push(overloads.into_iter().next().unwrap());
            } else {
                if let Some(merged_fn) = self.merge_fn_overloads(&name, overloads) {
                    self.functions.push(merged_fn);
                }
            }
        }
    }

    /// Merge overloaded methods in impl blocks into single signatures.
    fn merge_method_overloads(&mut self) {
        // Process each impl block
        let type_names: Vec<String> = self.impls.keys().cloned().collect();
        for type_name in type_names {
            let methods = self.impls.remove(&type_name).unwrap_or_default();
            // Group by (name, is_static) to keep static and instance methods separate
            let mut merged_groups: HashMap<(String, bool), Vec<GeneratedMethod>> = HashMap::new();

            // Group methods by name AND is_static flag
            for m in methods {
                merged_groups
                    .entry((m.name.clone(), m.is_static))
                    .or_default()
                    .push(m);
            }

            // Merge each group
            let mut final_methods = Vec::new();
            for ((name, _is_static), overloads) in merged_groups {
                if overloads.len() == 1 {
                    final_methods.push(overloads.into_iter().next().unwrap());
                } else {
                    if let Some(merged_method) = self.merge_method_group(&name, overloads) {
                        final_methods.push(merged_method);
                    }
                }
            }

            self.impls.insert(type_name, final_methods);
        }
    }

    /// Resolve interface inheritance by copying parent methods/properties to child impl blocks.
    ///
    /// This is called after all interfaces are collected and methods are merged,
    /// but before emitting the code. It handles the `extends` clause by copying
    /// methods and properties from parent interfaces to child interfaces.
    fn resolve_interface_inheritance(&mut self) {
        // Build a list of (child, parents) pairs to process
        // We need to clone to avoid borrow issues
        let inheritance: Vec<(String, Vec<String>)> = self
            .interface_extends
            .iter()
            .map(|(child, parents)| (child.clone(), parents.clone()))
            .collect();

        // Process each child interface
        for (child_name, parent_names) in inheritance {
            // Collect methods from all parents (including transitive)
            let mut inherited_methods = Vec::new();
            let mut inherited_properties = Vec::new();
            let mut visited = HashSet::new();

            self.collect_inherited_members(
                &parent_names,
                &mut inherited_methods,
                &mut inherited_properties,
                &mut visited,
            );

            // Add inherited methods to child's impl block
            if !inherited_methods.is_empty() {
                let child_methods = self.impls.entry(child_name.clone()).or_default();
                // Add inherited methods that aren't already defined on the child
                for method in inherited_methods {
                    if !child_methods.iter().any(|m| m.name == method.name) {
                        child_methods.push(method);
                    }
                }
            }

            // Add inherited properties to child
            if !inherited_properties.is_empty() {
                let child_props = self.properties.entry(child_name.clone()).or_default();
                // Add inherited properties that aren't already defined on the child
                for prop in inherited_properties {
                    if !child_props.iter().any(|p| p.name == prop.name) {
                        child_props.push(prop);
                    }
                }
            }
        }
    }

    /// Recursively collect methods and properties from parent interfaces.
    fn collect_inherited_members(
        &self,
        parent_names: &[String],
        methods: &mut Vec<GeneratedMethod>,
        properties: &mut Vec<GeneratedProperty>,
        visited: &mut HashSet<String>,
    ) {
        for parent_name in parent_names {
            // Avoid infinite loops in case of circular inheritance
            if visited.contains(parent_name) {
                continue;
            }
            visited.insert(parent_name.clone());

            // Get parent's methods
            if let Some(parent_methods) = self.impls.get(parent_name) {
                for method in parent_methods {
                    // Clone the method for the child
                    methods.push(GeneratedMethod {
                        name: method.name.clone(),
                        type_params: method.type_params.clone(),
                        params: method.params.clone(),
                        return_type: method.return_type.clone(),
                        is_static: method.is_static,
                        comment: Some(format!("inherited from {}", parent_name)),
                        this_param: method.this_param.clone(),
                    });
                }
            }

            // Get parent's properties
            if let Some(parent_props) = self.properties.get(parent_name) {
                for prop in parent_props {
                    properties.push(GeneratedProperty {
                        name: prop.name.clone(),
                        ty: prop.ty.clone(),
                        is_readonly: prop.is_readonly,
                        is_static: prop.is_static,
                    });
                }
            }

            // Recursively get grandparent's members
            if let Some(grandparents) = self.interface_extends.get(parent_name) {
                self.collect_inherited_members(grandparents, methods, properties, visited);
            }
        }
    }

    /// Merge a group of function overloads into a single signature.
    fn merge_fn_overloads(
        &mut self,
        name: &str,
        mut overloads: Vec<GeneratedFn>,
    ) -> Option<GeneratedFn> {
        if overloads.is_empty() {
            return None;
        }

        // Sort by param count descending - take the one with most params as base
        overloads.sort_by(|a, b| b.params.len().cmp(&a.params.len()));
        let base = overloads.remove(0);

        // Find which params are optional (present in base but not in all overloads)
        let min_params = overloads
            .iter()
            .map(|o| o.params.len())
            .min()
            .unwrap_or(base.params.len());

        let merged_params: Vec<(String, String)> = base
            .params
            .into_iter()
            .enumerate()
            .map(|(i, (name, ty))| {
                if i >= min_params {
                    // This param doesn't exist in shorter overloads, make it optional
                    let optional_ty = if ty.starts_with("Option<") {
                        ty
                    } else {
                        format!("Option<{}>", ty)
                    };
                    (name, optional_ty)
                } else {
                    (name, ty)
                }
            })
            .collect();

        if !overloads.is_empty() {
            self.warn(
                WarningKind::Simplified,
                format!(
                    "Merged {} overloads of function `{}`",
                    overloads.len() + 1,
                    name
                ),
            );
        }

        Some(GeneratedFn {
            name: base.name,
            type_params: base.type_params,
            params: merged_params,
            return_type: base.return_type,
            comment: Some(format!("merged from {} overloads", overloads.len() + 1)),
            this_param: base.this_param,
        })
    }

    /// Merge a group of method overloads into a single signature.
    fn merge_method_group(
        &mut self,
        name: &str,
        mut overloads: Vec<GeneratedMethod>,
    ) -> Option<GeneratedMethod> {
        if overloads.is_empty() {
            return None;
        }

        // Sort by param count descending - take the one with most params as base
        overloads.sort_by(|a, b| b.params.len().cmp(&a.params.len()));
        let base = overloads.remove(0);

        // Find which params are optional (present in base but not in all overloads)
        let min_params = overloads
            .iter()
            .map(|o| o.params.len())
            .min()
            .unwrap_or(base.params.len());

        let merged_params: Vec<(String, String)> = base
            .params
            .into_iter()
            .enumerate()
            .map(|(i, (name, ty))| {
                if i >= min_params {
                    // This param doesn't exist in shorter overloads, make it optional
                    let optional_ty = if ty.starts_with("Option<") {
                        ty
                    } else {
                        format!("Option<{}>", ty)
                    };
                    (name, optional_ty)
                } else {
                    (name, ty)
                }
            })
            .collect();

        if !overloads.is_empty() {
            self.warn(
                WarningKind::Simplified,
                format!(
                    "Merged {} overloads of method `{}`",
                    overloads.len() + 1,
                    name
                ),
            );
        }

        Some(GeneratedMethod {
            name: base.name,
            type_params: base.type_params,
            params: merged_params,
            return_type: base.return_type,
            is_static: base.is_static,
            comment: Some(format!("merged from {} overloads", overloads.len() + 1)),
            this_param: base.this_param,
        })
    }

    /// Pre-pass: collect struct names from interfaces and classes so type mapping knows what's defined.
    fn collect_struct_names(&mut self, item: &DtsItem) {
        match item {
            DtsItem::Interface(i) => {
                let type_params: Vec<String> = i.type_params.iter().map(|p| p.name.clone()).collect();
                self.structs.insert(i.name.clone(), type_params);
            }
            DtsItem::Class(c) => {
                let type_params: Vec<String> = c.type_params.iter().map(|p| p.name.clone()).collect();
                self.structs.insert(c.name.clone(), type_params);
            }
            DtsItem::Namespace(ns) => {
                for item in &ns.items {
                    self.collect_struct_names(item);
                }
            }
            DtsItem::Module(m) => {
                for item in &m.items {
                    self.collect_struct_names(item);
                }
            }
            _ => {} // Other items don't define structs
        }
    }

    fn collect_item(&mut self, item: &DtsItem) {
        match item {
            DtsItem::Function(f) => self.collect_function(f),
            DtsItem::Interface(i) => self.collect_interface(i),
            DtsItem::Class(c) => self.collect_class(c),
            DtsItem::TypeAlias(t) => self.collect_type_alias(t),
            DtsItem::Variable(v) => self.collect_variable(v),
            DtsItem::Namespace(ns) => {
                for item in &ns.items {
                    self.collect_item(item);
                }
            }
            DtsItem::Module(m) => {
                for item in &m.items {
                    self.collect_item(item);
                }
            }
            DtsItem::Export(_) => {} // Skip exports
        }
    }

    fn collect_function(&mut self, f: &DtsFunction) {
        // NOTE: Top-level extern functions don't support type parameters in the parser,
        // so we don't add them to known_generics. This means type params will be mapped
        // to JsValue, which is correct for JavaScript interop.

        let mut comment = None;

        // Check for constrained type params - add a note if any exist
        if !f.type_params.is_empty() {
            let param_names: Vec<&str> = f.type_params.iter().map(|p| p.name.as_str()).collect();
            comment = Some(format!("type params simplified: {}", param_names.join(", ")));
        }

        let params: Vec<(String, String)> = f
            .params
            .iter()
            .map(|p| {
                let ty = self.map_param_type(&p.ty, p.optional);
                (escape_keyword(&p.name), ty)
            })
            .collect();

        let return_type = f.return_type.as_ref().and_then(|ty| {
            let mapped = self.map_type(ty);
            if mapped == "()" { None } else { Some(mapped) }
        });

        self.functions.push(GeneratedFn {
            name: escape_keyword(&f.name),
            type_params: Vec::new(), // Don't emit type params for extern block functions
            params,
            return_type,
            comment,
            this_param: None, // Top-level functions don't have this binding
        });

        self.known_generics.clear();
    }

    fn collect_interface(&mut self, i: &DtsInterface) {
        let type_params: Vec<String> = i.type_params.iter().map(|p| p.name.clone()).collect();
        self.structs.insert(i.name.clone(), type_params);
        self.in_struct_context = true;
        self.current_type_name = Some(i.name.clone());

        // Record interface inheritance for later resolution
        if !i.extends.is_empty() {
            let parent_names: Vec<String> = i
                .extends
                .iter()
                .filter_map(|ty| {
                    // Extract the base name from the extends type
                    // e.g., `core.Express` -> `Express`, `Parent<T>` -> `Parent`
                    match ty {
                        DtsType::Named { name, .. } => {
                            // Handle qualified names like `core.Express` -> just `Express`
                            Some(name.split('.').last().unwrap_or(name).to_string())
                        }
                        _ => None,
                    }
                })
                .collect();
            if !parent_names.is_empty() {
                self.interface_extends
                    .insert(i.name.clone(), parent_names);
            }
        }

        // Store interface for builder generation if enabled
        if self.options.generate_builders {
            self.interfaces.push(i.clone());
        }

        // Register in type registry for utility type expansion (only for non-generic interfaces)
        if self.options.expand_utility_types && i.type_params.is_empty() {
            let object_type = interface_to_object_type(i);
            self.type_registry.register(i.name.clone(), object_type);
        }

        let type_params: Vec<String> = i.type_params.iter().map(|p| p.name.clone()).collect();
        self.known_generics = type_params.iter().cloned().collect();

        let mut methods = Vec::new();

        for member in &i.members {
            match member {
                InterfaceMember::Method(m) => {
                    // Track method-only type params (not in struct-level params)
                    let method_only_params: HashSet<String> =
                        m.type_params.iter().map(|p| p.name.clone()).collect();

                    // Only keep struct-level type params for generated method
                    // Method-only type params will be simplified to JsValue
                    let method_type_params: Vec<String> = type_params.clone();

                    // Save old state and set up for type mapping
                    let old_generics = self.known_generics.clone();
                    let old_method_params =
                        std::mem::replace(&mut self.method_type_params, method_only_params.clone());

                    // Add method type params to known generics for mapping
                    for tp in &m.type_params {
                        self.known_generics.insert(tp.name.clone());
                    }

                    let params: Vec<(String, String)> = m
                        .params
                        .iter()
                        .map(|p| {
                            let ty = self.map_param_type(&p.ty, p.optional);
                            (escape_keyword(&p.name), ty)
                        })
                        .collect();

                    let return_type = m.return_type.as_ref().and_then(|ty| {
                        let mapped = self.map_type(ty);
                        if mapped == "()" { None } else { Some(mapped) }
                    });

                    // Map this_param type if present
                    let this_param = m.this_param.as_ref().map(|tp| self.map_type(tp));

                    methods.push(GeneratedMethod {
                        name: escape_keyword(&m.name),
                        type_params: method_type_params,
                        params,
                        return_type,
                        is_static: false,
                        comment: None,
                        this_param,
                    });

                    self.known_generics = old_generics;
                    self.method_type_params = old_method_params;
                }
                InterfaceMember::Property(p) => {
                    // Check if this is a function-typed property (callback)
                    // Function-typed properties are treated as methods for better ergonomics
                    let is_function_property = matches!(p.ty, DtsType::Function(_));

                    if is_function_property {
                        // Generate as method for function-typed properties
                        let mapped_type = self.map_type(&p.ty);
                        let return_type = if p.optional {
                            format!("Option<{}>", mapped_type)
                        } else {
                            mapped_type
                        };

                        methods.push(GeneratedMethod {
                            name: escape_keyword(&p.name),
                            type_params: Vec::new(),
                            params: Vec::new(),
                            return_type: Some(return_type),
                            is_static: false,
                            comment: Some("function property".to_string()),
                            this_param: None,
                        });
                    } else {
                        // Generate as #[getter] property for non-function properties
                        let mapped_type = self.map_type(&p.ty);
                        let final_type = if p.optional {
                            format!("Option<{}>", mapped_type)
                        } else {
                            mapped_type
                        };

                        self.properties.entry(i.name.clone()).or_default().push(
                            GeneratedProperty {
                                name: escape_keyword(&p.name),
                                ty: final_type,
                                is_readonly: p.readonly,
                                is_static: false,
                            },
                        );
                    }
                }
                InterfaceMember::CallSignature(_) => {
                    self.warn(
                        WarningKind::Skipped,
                        format!("Call signature on {} skipped", i.name),
                    );
                }
                InterfaceMember::ConstructSignature(ctor) => {
                    // Constructor interfaces (like DatabaseConstructor) should generate
                    // a static new_ method that returns the constructed type
                    let params: Vec<(String, String)> = ctor
                        .params
                        .iter()
                        .map(|p| {
                            let ty = self.map_param_type(&p.ty, p.optional);
                            (escape_keyword(&p.name), ty)
                        })
                        .collect();

                    // Use the return type from the construct signature, or fall back to
                    // the interface name without "Constructor" suffix
                    let return_type = if let Some(ref ret_ty) = ctor.return_type {
                        Some(self.map_type(ret_ty))
                    } else {
                        // Try to infer return type from interface name
                        // e.g., DatabaseConstructor -> Database
                        let name = &i.name;
                        if name.ends_with("Constructor") {
                            Some(name.trim_end_matches("Constructor").to_string())
                        } else {
                            Some(name.clone())
                        }
                    };

                    methods.push(GeneratedMethod {
                        name: "new_".to_string(),
                        type_params: type_params.clone(),
                        params,
                        return_type,
                        is_static: true,
                        comment: Some("constructor".to_string()),
                        this_param: None,
                    });
                }
                InterfaceMember::IndexSignature(_) => {
                    self.warn(
                        WarningKind::Skipped,
                        format!("Index signature on {} skipped", i.name),
                    );
                }
            }
        }

        if !methods.is_empty() {
            self.impls.insert(i.name.clone(), methods);
        }

        self.known_generics.clear();
        self.in_struct_context = false;
        self.current_type_name = None;
    }

    fn collect_class(&mut self, c: &DtsClass) {
        let type_params: Vec<String> = c.type_params.iter().map(|p| p.name.clone()).collect();
        self.structs.insert(c.name.clone(), type_params.clone());
        self.in_struct_context = true;
        self.current_type_name = Some(c.name.clone());

        self.known_generics = type_params.iter().cloned().collect();

        let mut methods = Vec::new();

        for member in &c.members {
            match member {
                ClassMember::Method(m) => {
                    // Skip private methods
                    if m.visibility == Visibility::Private {
                        continue;
                    }

                    let method_type_params: Vec<String> = type_params
                        .iter()
                        .chain(m.type_params.iter().map(|p| &p.name))
                        .cloned()
                        .collect();

                    let old_generics = self.known_generics.clone();
                    for tp in &m.type_params {
                        self.known_generics.insert(tp.name.clone());
                    }

                    let params: Vec<(String, String)> = m
                        .params
                        .iter()
                        .map(|p| {
                            let ty = self.map_param_type(&p.ty, p.optional);
                            (escape_keyword(&p.name), ty)
                        })
                        .collect();

                    let return_type = m.return_type.as_ref().and_then(|ty| {
                        let mapped = self.map_type(ty);
                        if mapped == "()" { None } else { Some(mapped) }
                    });

                    // Map this_param type if present
                    let this_param = m.this_param.as_ref().map(|tp| self.map_type(tp));

                    methods.push(GeneratedMethod {
                        name: escape_keyword(&m.name),
                        type_params: method_type_params,
                        params,
                        return_type,
                        is_static: m.is_static,
                        comment: None,
                        this_param,
                    });

                    self.known_generics = old_generics;
                }
                ClassMember::Constructor(ctor) => {
                    // Generate a `new` static method
                    let params: Vec<(String, String)> = ctor
                        .params
                        .iter()
                        .map(|p| {
                            let ty = self.map_param_type(&p.ty, p.optional);
                            (escape_keyword(&p.name), ty)
                        })
                        .collect();

                    methods.push(GeneratedMethod {
                        name: "new_".to_string(), // Escape keyword
                        type_params: type_params.clone(),
                        params,
                        return_type: Some(c.name.clone()),
                        is_static: true,
                        comment: None,
                        this_param: None, // Constructors don't have this binding
                    });
                }
                ClassMember::Property(p) => {
                    // Skip private properties
                    if p.visibility == Visibility::Private {
                        continue;
                    }

                    // Generate getter method for property access
                    let mapped_type =
                        p.ty.as_ref()
                            .map(|t| self.map_type(t))
                            .unwrap_or_else(|| "JsValue".to_string());

                    let return_type = if p.optional {
                        format!("Option<{}>", mapped_type)
                    } else {
                        mapped_type.clone()
                    };

                    methods.push(GeneratedMethod {
                        name: escape_keyword(&p.name),
                        type_params: Vec::new(),
                        params: Vec::new(),
                        return_type: Some(return_type),
                        is_static: p.is_static,
                        comment: Some(if p.is_static {
                            if p.readonly {
                                "static property (readonly)".to_string()
                            } else {
                                "static property getter".to_string()
                            }
                        } else if p.readonly {
                            "property (readonly)".to_string()
                        } else {
                            "property getter".to_string()
                        }),
                        this_param: None, // Property getters don't have this binding
                    });

                    // Generate setter method for non-readonly properties
                    if !p.readonly {
                        let setter_param_type = if p.optional {
                            format!("Option<{}>", mapped_type)
                        } else {
                            mapped_type
                        };

                        methods.push(GeneratedMethod {
                            name: format!("set_{}", escape_keyword(&p.name)),
                            type_params: Vec::new(),
                            params: vec![("value".to_string(), setter_param_type)],
                            return_type: None,
                            is_static: p.is_static,
                            comment: Some(if p.is_static {
                                "static property setter".to_string()
                            } else {
                                "property setter".to_string()
                            }),
                            this_param: None, // Property setters don't have this binding
                        });
                    }
                }
                ClassMember::IndexSignature(_) => {
                    self.warn(
                        WarningKind::Skipped,
                        format!("Index signature on {} skipped", c.name),
                    );
                }
            }
        }

        if !methods.is_empty() {
            self.impls.insert(c.name.clone(), methods);
        }

        self.known_generics.clear();
        self.in_struct_context = false;
        self.current_type_name = None;
    }

    fn collect_type_alias(&mut self, t: &DtsTypeAlias) {
        self.type_aliases.insert(t.name.clone(), t.ty.clone());

        // Register in type registry for utility type expansion (only for non-generic type aliases)
        if self.options.expand_utility_types && t.type_params.is_empty() {
            self.type_registry.register(t.name.clone(), t.ty.clone());
        }

        // Check if we should generate an untagged enum for this type alias
        // We generate enums for union types that are suitable (non-generic, reasonable size)
        if t.type_params.is_empty() {
            if let Some(generated_enum) = self.try_generate_enum_from_type_alias(t) {
                self.enums.push(generated_enum);
            }
        }
    }

    /// Try to generate an untagged enum from a type alias.
    /// Returns Some(GeneratedEnum) if successful, None if the type is not suitable.
    fn try_generate_enum_from_type_alias(&mut self, t: &DtsTypeAlias) -> Option<GeneratedEnum> {
        // Only handle union types
        let types = match &t.ty {
            DtsType::Union(types) => types,
            _ => return None,
        };

        // Filter out null and undefined - these become Option<...> wrappers instead
        let non_null_types: Vec<&DtsType> = types
            .iter()
            .filter(|ty| {
                !matches!(
                    ty,
                    DtsType::Primitive(Primitive::Null) | DtsType::Primitive(Primitive::Undefined)
                )
            })
            .collect();

        // Skip if this would be better as Option<T> (single non-null type)
        if non_null_types.len() < 2 {
            return None;
        }

        // Skip large unions - they're better as JsValue
        if non_null_types.len() > 10 {
            return None;
        }

        // Try to create variants from each type
        let mut variants = Vec::new();
        for ty in non_null_types {
            match self.type_to_variant(ty) {
                Some(variant) => variants.push(variant),
                None => return None, // If any type can't be converted, fall back
            }
        }

        Some(GeneratedEnum {
            name: t.name.clone(),
            variants,
        })
    }

    /// Convert a DtsType to an enum variant (name, optional inner type).
    fn type_to_variant(&mut self, ty: &DtsType) -> Option<(String, Option<String>)> {
        match ty {
            // String literal -> unit variant with PascalCase name
            DtsType::StringLiteral(s) => {
                let variant_name = string_to_variant_name(s);
                Some((variant_name, None))
            }
            // Named type -> variant wrapping that type
            DtsType::Named { name, type_args } => {
                // Use the simple name (last segment) for the variant name and inner type
                // This matches how map_named_type handles qualified names
                let simple_name = name.split('.').last().unwrap_or(name);
                let variant_name = simple_name.to_string();
                let inner_type = if type_args.is_empty() {
                    simple_name.to_string()
                } else {
                    let args: Vec<String> = type_args.iter().map(|t| self.map_type(t)).collect();
                    format!("{}<{}>", simple_name, args.join(", "))
                };
                Some((variant_name, Some(inner_type)))
            }
            // Primitive types -> variant wrapping the mapped type
            DtsType::Primitive(p) => {
                let variant_name = primitive_to_variant_name(p);
                let inner_type = self.map_type(ty);
                Some((variant_name, Some(inner_type)))
            }
            // Array type -> "Array" variant
            DtsType::Array(inner) => {
                let inner_type = self.map_type(inner);
                Some(("Array".to_string(), Some(format!("Vec<{}>", inner_type))))
            }
            // Number literal -> unit variant
            DtsType::NumberLiteral(n) => {
                let variant_name = format!("N{}", n.replace(['-', '.'], "_"));
                Some((variant_name, None))
            }
            // Boolean literal -> unit variant
            DtsType::BooleanLiteral(b) => {
                let variant_name = if *b { "True" } else { "False" };
                Some((variant_name.to_string(), None))
            }
            // Other types are too complex for enum generation
            _ => None,
        }
    }

    fn collect_variable(&mut self, v: &DtsVariable) {
        let ty = self.map_type(&v.ty);

        if self.options.use_extern_const && v.is_const {
            // New behavior: emit extern const only for actual const bindings
            self.consts.push(GeneratedConst {
                name: escape_keyword(&v.name),
                ty,
            });
            self.metrics.extern_consts += 1;
        } else {
            // Legacy behavior or non-const: generate as a zero-arg function
            self.functions.push(GeneratedFn {
                name: escape_keyword(&v.name),
                type_params: Vec::new(),
                params: Vec::new(),
                return_type: Some(ty),
                comment: Some(if v.is_const { "constant" } else { "variable" }.to_string()),
                this_param: None,
            });
            self.metrics.legacy_const_functions += 1;
        }
    }

    fn map_type(&mut self, ty: &DtsType) -> String {
        match ty {
            DtsType::Primitive(p) => self.map_primitive(p),
            DtsType::Named { name, type_args } => self.map_named_type(name, type_args),
            DtsType::StringLiteral(_) => {
                self.warn(
                    WarningKind::Simplified,
                    "String literal type mapped to String",
                );
                "String".to_string()
            }
            DtsType::NumberLiteral(_) => {
                self.warn(WarningKind::Simplified, "Number literal type mapped to f64");
                "f64".to_string()
            }
            DtsType::BooleanLiteral(_) => "bool".to_string(),
            DtsType::Union(types) => self.map_union_type(types),
            DtsType::Intersection(types) => {
                // For intersections, try to pick the most meaningful type
                // Usually it's the first non-primitive
                for t in types {
                    let mapped = self.map_type(t);
                    if mapped != "JsValue" {
                        return mapped;
                    }
                }
                self.warn(
                    WarningKind::Simplified,
                    "Intersection type mapped to JsValue",
                );
                "JsValue".to_string()
            }
            DtsType::Function(f) => self.map_function_type(f),
            DtsType::Object(_) => {
                self.warn(
                    WarningKind::Simplified,
                    "Object literal type mapped to JsValue",
                );
                "JsValue".to_string()
            }
            DtsType::Array(inner) => {
                let inner_mapped = self.map_type(inner);
                format!("JsArray<{}>", inner_mapped)
            }
            DtsType::Tuple(elements) => {
                // Map to tuple type if Husk supports it, otherwise JsValue
                if elements.len() <= 4 {
                    let mapped: Vec<String> =
                        elements.iter().map(|e| self.map_type(&e.ty)).collect();
                    format!("({})", mapped.join(", "))
                } else {
                    self.warn(WarningKind::Simplified, "Large tuple mapped to JsValue");
                    "JsValue".to_string()
                }
            }
            DtsType::TypeOf(_) => {
                self.warn(WarningKind::Unsupported, "typeof type not supported");
                "JsValue".to_string()
            }
            DtsType::KeyOf(target) => {
                // Try to extract known keys from the target type
                if let Some(keys) = self.extract_known_keys(target) {
                    self.metrics.record_keyof_resolved();
                    if keys.is_empty() {
                        // No keys found - fall back to String
                        self.warn(
                            WarningKind::Simplified,
                            "keyof type with no known keys mapped to String",
                        );
                        "String".to_string()
                    } else if keys.len() == 1 {
                        // Single key - just use the literal value as a String type
                        "String".to_string()
                    } else {
                        // Multiple keys - create a union of string literals
                        // Since Husk doesn't have literal types, we map to String
                        // but warn about the loss of precision
                        self.warn(
                            WarningKind::Simplified,
                            format!(
                                "keyof type with {} known keys ({}...) mapped to String",
                                keys.len(),
                                keys.iter().take(3).cloned().collect::<Vec<_>>().join(", ")
                            ),
                        );
                        "String".to_string()
                    }
                } else {
                    // Cannot resolve keys - fall back to String (better than JsValue)
                    self.metrics.record_keyof_fallback();
                    self.warn(
                        WarningKind::Simplified,
                        "keyof type of unknown target mapped to String",
                    );
                    "String".to_string()
                }
            }
            DtsType::IndexAccess { object, index } => {
                // Try to resolve the index access type
                if let Some(resolved_type) = self.resolve_index_access(object, index) {
                    self.metrics.record_index_access_resolved();
                    self.map_type(&resolved_type)
                } else {
                    // Cannot resolve - fall back to JsValue
                    self.metrics.record_index_access_fallback();
                    self.warn(
                        WarningKind::Simplified,
                        "Index access type could not be resolved, mapped to JsValue",
                    );
                    "JsValue".to_string()
                }
            }
            DtsType::Conditional {
                true_type,
                false_type,
                ..
            } => {
                // Try to find common ground between true and false branches
                // If both branches have the same base type, use it
                if let Some(common) = self.extract_common_type(true_type, false_type) {
                    self.warn(
                        WarningKind::Simplified,
                        "Conditional type simplified to common base type",
                    );
                    common
                } else {
                    // Fall back to true_type as a reasonable default
                    // (the true branch is often the more common/expected case)
                    let mapped = self.map_type(true_type);
                    if mapped != "JsValue" {
                        self.warn(
                            WarningKind::Simplified,
                            "Conditional type simplified to true branch",
                        );
                        mapped
                    } else {
                        self.warn(WarningKind::Unsupported, "Conditional type not supported");
                        "JsValue".to_string()
                    }
                }
            }
            DtsType::Mapped { .. } => {
                self.warn(WarningKind::Unsupported, "Mapped type not supported");
                "JsValue".to_string()
            }
            DtsType::Infer(_) => {
                self.warn(WarningKind::Unsupported, "infer type not supported");
                "JsValue".to_string()
            }
            DtsType::TemplateLiteral(_) => {
                self.warn(
                    WarningKind::Simplified,
                    "Template literal type mapped to String",
                );
                "String".to_string()
            }
            DtsType::Parenthesized(inner) => self.map_type(inner),
            DtsType::This => {
                // `this` type - use the current type name
                self.current_type_name
                    .clone()
                    .unwrap_or_else(|| "JsValue".to_string())
            }
        }
    }

    fn map_primitive(&self, p: &Primitive) -> String {
        match p {
            Primitive::String => "String".to_string(),
            Primitive::Number => "f64".to_string(),
            Primitive::Boolean => "bool".to_string(),
            Primitive::Void => "()".to_string(),
            Primitive::Null => "()".to_string(),
            Primitive::Undefined => "()".to_string(),
            Primitive::Any => "JsValue".to_string(),
            Primitive::Unknown => "JsValue".to_string(),
            Primitive::Never => "()".to_string(), // Husk doesn't have never type
            Primitive::Object => "JsValue".to_string(),
            Primitive::Symbol => "JsValue".to_string(),
            Primitive::BigInt => "i64".to_string(),
        }
    }

    fn map_named_type(&mut self, name: &str, type_args: &[DtsType]) -> String {
        // Handle qualified names: use the last segment as the type name
        // e.g., "Database.RunResult" -> "RunResult"
        let simple_name = name.split('.').last().unwrap_or(name);

        // Check if it's a method-only type parameter - these get simplified to JsValue
        if self.method_type_params.contains(simple_name) {
            self.warn(
                WarningKind::Simplified,
                format!(
                    "Method-only type parameter `{}` mapped to JsValue",
                    simple_name
                ),
            );
            return "JsValue".to_string();
        }

        // Check if it's a known generic type parameter (struct-level or function-level)
        if self.known_generics.contains(simple_name) {
            // Preserve the type parameter - extern structs now support generics
            return simple_name.to_string();
        }

        // Map well-known types using simple name
        match simple_name {
            "Array" | "ReadonlyArray" => {
                if let Some(inner) = type_args.first() {
                    let inner_mapped = self.map_type(inner);
                    return format!("JsArray<{}>", inner_mapped);
                }
                "JsArray<JsValue>".to_string()
            }
            "Promise" => {
                if let Some(inner) = type_args.first() {
                    let inner_mapped = self.map_type(inner);
                    return format!("JsPromise<{}>", inner_mapped);
                }
                "JsPromise<JsValue>".to_string()
            }
            "Map" | "WeakMap" => "JsValue".to_string(),
            "Set" | "WeakSet" => "JsValue".to_string(),
            "Date" => "JsValue".to_string(),
            "RegExp" => "JsValue".to_string(),
            "Error" => "JsValue".to_string(),
            "Function" => "JsFn".to_string(),
            "Partial" | "Required" | "Readonly" | "Pick" | "Omit" | "Record" | "Exclude"
            | "Extract" | "NonNullable" | "ReturnType" | "Parameters" | "InstanceType" => {
                // TypeScript utility types - try expansion if enabled
                if self.options.expand_utility_types {
                    // Build the DtsType::Named to pass to expand_type
                    let named_type = DtsType::Named {
                        name: simple_name.to_string(),
                        type_args: type_args.to_vec(),
                    };
                    let ctx = ExpansionContext::new();
                    let expanded = expand_type(&named_type, &self.type_registry, &ctx);

                    // If expansion produced a different type (not the same Named type), map it
                    if expanded != named_type {
                        self.metrics.record_utility_expanded();
                        return self.map_type(&expanded);
                    }
                }

                // Fall back to JsValue if expansion disabled or didn't help
                self.metrics.record_utility_failed();
                self.warn(
                    WarningKind::Unsupported,
                    format!("Utility type {} not supported", simple_name),
                );
                "JsValue".to_string()
            }
            _ => {
                // User-defined type - use simple name (last segment of qualified name)
                // Check if this is a type alias we know about
                if let Some(alias_ty) = self.type_aliases.get(simple_name).cloned() {
                    // Detect cycles in type alias expansion
                    if self.expanding_aliases.contains(simple_name) {
                        self.warn(
                            WarningKind::Simplified,
                            format!("Cyclic type alias `{}` mapped to JsValue", simple_name),
                        );
                        return "JsValue".to_string();
                    }
                    // Mark this alias as being expanded
                    self.expanding_aliases.insert(simple_name.to_string());
                    let result = self.map_type(&alias_ty);
                    self.expanding_aliases.remove(simple_name);
                    return result;
                }

                // Check if this type is defined in the current file
                let is_known_struct = self.structs.contains_key(simple_name);

                if type_args.is_empty() {
                    if is_known_struct {
                        simple_name.to_string()
                    } else {
                        // Unknown type - simplify to JsValue
                        self.warn(
                            WarningKind::Simplified,
                            format!("Unknown type `{}` mapped to JsValue", simple_name),
                        );
                        "JsValue".to_string()
                    }
                } else {
                    // Check if any type args use method-only type parameters
                    let has_method_only_param = type_args.iter().any(|arg| {
                        if let DtsType::Named { name, type_args } = arg {
                            let arg_simple = name.split('.').last().unwrap_or(name);
                            self.method_type_params.contains(arg_simple)
                                || type_args
                                    .iter()
                                    .any(|inner| self.type_uses_method_param(inner))
                        } else {
                            self.type_uses_method_param(arg)
                        }
                    });

                    if has_method_only_param {
                        // Simplify the entire generic type to JsValue
                        self.warn(
                            WarningKind::Simplified,
                            format!("Generic type `{}<...>` with method-only params simplified to JsValue", simple_name),
                        );
                        return "JsValue".to_string();
                    }

                    // If the base type is unknown, simplify to JsValue
                    if !is_known_struct {
                        self.warn(
                            WarningKind::Simplified,
                            format!(
                                "Unknown generic type `{}<...>` mapped to JsValue",
                                simple_name
                            ),
                        );
                        return "JsValue".to_string();
                    }

                    let args: Vec<String> = type_args.iter().map(|a| self.map_type(a)).collect();
                    format!("{}<{}>", simple_name, args.join(", "))
                }
            }
        }
    }

    /// Check if a type uses any method-only type parameters.
    fn type_uses_method_param(&self, ty: &DtsType) -> bool {
        match ty {
            DtsType::Named { name, type_args } => {
                let simple_name = name.split('.').last().unwrap_or(name);
                self.method_type_params.contains(simple_name)
                    || type_args
                        .iter()
                        .any(|inner| self.type_uses_method_param(inner))
            }
            DtsType::Array(inner) => self.type_uses_method_param(inner),
            DtsType::Tuple(elements) => elements.iter().any(|e| self.type_uses_method_param(&e.ty)),
            DtsType::Union(types) | DtsType::Intersection(types) => {
                types.iter().any(|t| self.type_uses_method_param(t))
            }
            DtsType::Function(f) => {
                f.params.iter().any(|p| self.type_uses_method_param(&p.ty))
                    || self.type_uses_method_param(&f.return_type)
            }
            DtsType::Conditional {
                check,
                extends,
                true_type,
                false_type,
            } => {
                self.type_uses_method_param(check)
                    || self.type_uses_method_param(extends)
                    || self.type_uses_method_param(true_type)
                    || self.type_uses_method_param(false_type)
            }
            DtsType::IndexAccess { object, index } => {
                self.type_uses_method_param(object) || self.type_uses_method_param(index)
            }
            DtsType::KeyOf(inner) => self.type_uses_method_param(inner),
            DtsType::Parenthesized(inner) => self.type_uses_method_param(inner),
            _ => false,
        }
    }

    /// Extract a common base type from two types (for conditional type simplification).
    ///
    /// Returns Some(type_string) if both types have the same base type name,
    /// None if they are fundamentally different.
    fn extract_common_type(&mut self, true_type: &DtsType, false_type: &DtsType) -> Option<String> {
        match (true_type, false_type) {
            // Both are references to the same type name
            (
                DtsType::Named {
                    name: name1,
                    type_args: args1,
                },
                DtsType::Named {
                    name: name2,
                    type_args: args2,
                },
            ) => {
                let simple1 = name1.split('.').last().unwrap_or(name1);
                let simple2 = name2.split('.').last().unwrap_or(name2);
                if simple1 == simple2 {
                    // Same base type - use it with merged type args if possible
                    if args1.is_empty() && args2.is_empty() {
                        Some(simple1.to_string())
                    } else {
                        // Use the type args from the true branch
                        let args: Vec<String> = args1.iter().map(|a| self.map_type(a)).collect();
                        if args.is_empty() {
                            Some(simple1.to_string())
                        } else {
                            Some(format!("{}<{}>", simple1, args.join(", ")))
                        }
                    }
                } else {
                    None
                }
            }
            // Both are arrays
            (DtsType::Array(inner1), DtsType::Array(inner2)) => {
                self.extract_common_type(inner1, inner2)
                    .map(|common| format!("JsArray<{}>", common))
            }
            // Both are the same primitive
            (DtsType::Primitive(p1), DtsType::Primitive(p2)) if p1 == p2 => {
                Some(self.map_type(true_type))
            }
            // Otherwise no common type
            _ => None,
        }
    }

    /// Extract known property keys from a type for keyof resolution.
    ///
    /// Returns `Some(keys)` if the type's keys can be determined,
    /// `None` if the type cannot be resolved.
    fn extract_known_keys(&self, ty: &DtsType) -> Option<Vec<String>> {
        match ty {
            // Object literal type: { a: T, b: U } -> ["a", "b"]
            DtsType::Object(members) => {
                let keys: Vec<String> = members
                    .iter()
                    .filter_map(|m| match m {
                        ObjectMember::Property { name, .. } => Some(name.clone()),
                        ObjectMember::Method { name, .. } => Some(name.clone()),
                        // These don't have named keys
                        ObjectMember::IndexSignature { .. }
                        | ObjectMember::CallSignature(_)
                        | ObjectMember::ConstructSignature(_) => None,
                    })
                    .collect();
                Some(keys)
            }
            // Named type: look up in type registry
            DtsType::Named { name, .. } => {
                if let Some(resolved) = self.type_registry.get(name) {
                    self.extract_known_keys(resolved)
                } else {
                    // Type not in registry - cannot resolve keys
                    None
                }
            }
            // Parenthesized type: unwrap
            DtsType::Parenthesized(inner) => self.extract_known_keys(inner),
            // Other types - cannot extract keys
            _ => None,
        }
    }

    /// Get the type of a property in a named type, for index access resolution.
    fn get_property_type(&self, type_name: &str, property_name: &str) -> Option<DtsType> {
        if let Some(ty) = self.type_registry.get(type_name) {
            self.get_property_type_from_object(ty, property_name)
        } else {
            None
        }
    }

    /// Get the type of a property from an object type.
    fn get_property_type_from_object(&self, ty: &DtsType, property_name: &str) -> Option<DtsType> {
        match ty {
            DtsType::Object(members) => {
                for member in members {
                    match member {
                        ObjectMember::Property { name, ty, .. } if name == property_name => {
                            return Some(ty.clone());
                        }
                        ObjectMember::Method { name, return_type, .. } if name == property_name => {
                            // For methods, return the return type (or void)
                            return Some(
                                return_type
                                    .clone()
                                    .unwrap_or(DtsType::Primitive(Primitive::Void)),
                            );
                        }
                        _ => {}
                    }
                }
                None
            }
            DtsType::Named { name, .. } => {
                // Try to resolve the named type
                if let Some(resolved) = self.type_registry.get(name) {
                    self.get_property_type_from_object(resolved, property_name)
                } else {
                    None
                }
            }
            DtsType::Parenthesized(inner) => self.get_property_type_from_object(inner, property_name),
            _ => None,
        }
    }

    /// Resolve an index access type `T[K]` to its concrete type.
    ///
    /// Handles common patterns:
    /// - `T["propertyName"]` - literal string key lookup
    /// - `T[number]` - array element access
    /// - `T[K]` where K is a string literal type
    fn resolve_index_access(&self, object: &DtsType, index: &DtsType) -> Option<DtsType> {
        match (object, index) {
            // T["propertyName"] - string literal key lookup
            (DtsType::Named { name, .. }, DtsType::StringLiteral(key)) => {
                self.get_property_type(name, key)
            }
            // { a: T, b: U }["a"] - object literal with string literal key
            (DtsType::Object(_), DtsType::StringLiteral(key)) => {
                self.get_property_type_from_object(object, key)
            }
            // T[number] - array element type
            (DtsType::Array(elem), DtsType::Primitive(Primitive::Number)) => {
                Some((**elem).clone())
            }
            // ReadonlyArray<T>[number]
            (
                DtsType::Named { name, type_args },
                DtsType::Primitive(Primitive::Number),
            ) if name == "Array" || name == "ReadonlyArray" => {
                type_args.first().cloned()
            }
            // Tuple[N] - tuple element access with number literal
            // For now, we don't handle this as we'd need NumberLiteral type
            // (T) - parenthesized, unwrap
            (DtsType::Parenthesized(inner), _) => self.resolve_index_access(inner, index),
            (_, DtsType::Parenthesized(inner)) => self.resolve_index_access(object, inner),
            // Cannot resolve
            _ => None,
        }
    }

    fn map_union_type(&mut self, types: &[DtsType]) -> String {
        // Use the union analyzer to determine the best strategy
        let analysis = analyze_union(types);

        match analysis {
            unions::UnionStrategy::Nullable { inner } => {
                // T | null | undefined → Option<T>
                self.metrics.record_union_option();
                let mapped = self.map_type(&inner);
                format!("Option<{}>", mapped)
            }

            unions::UnionStrategy::Boolean => {
                // true | false → bool
                self.metrics.record_union_bool();
                "bool".to_string()
            }

            unions::UnionStrategy::StringEnum { variants } => {
                // String literal union - depends on config strategy
                match self.options.union_strategy {
                    UnionStrategy::JsValue => {
                        self.metrics.record_union_jsvalue();
                        self.warn(
                            WarningKind::Simplified,
                            format!(
                                "String literal union ({} variants) mapped to JsValue per config",
                                variants.len()
                            ),
                        );
                        "JsValue".to_string()
                    }
                    UnionStrategy::Enum | UnionStrategy::Auto => {
                        // Map to String for now - Husk doesn't have inline enum types
                        // A more advanced implementation could generate named enums
                        self.metrics.record_union_string();
                        self.warn(
                            WarningKind::Simplified,
                            format!(
                                "String literal union ({} variants: {}...) mapped to String",
                                variants.len(),
                                variants.iter().take(3).cloned().collect::<Vec<_>>().join(", ")
                            ),
                        );
                        "String".to_string()
                    }
                }
            }

            unions::UnionStrategy::NumberEnum { variants } => {
                // Number literal union
                match self.options.union_strategy {
                    UnionStrategy::JsValue => {
                        self.metrics.record_union_jsvalue();
                        self.warn(
                            WarningKind::Simplified,
                            format!(
                                "Number literal union ({} variants) mapped to JsValue per config",
                                variants.len()
                            ),
                        );
                        "JsValue".to_string()
                    }
                    UnionStrategy::Enum | UnionStrategy::Auto => {
                        // Map to f64 for now
                        self.metrics.record_union_number();
                        self.warn(
                            WarningKind::Simplified,
                            format!(
                                "Number literal union ({} variants) mapped to f64",
                                variants.len()
                            ),
                        );
                        "f64".to_string()
                    }
                }
            }

            unions::UnionStrategy::Discriminated { discriminant, variants } => {
                // Discriminated/tagged union
                self.metrics.record_union_jsvalue();
                match self.options.union_strategy {
                    UnionStrategy::JsValue => {
                        self.warn(
                            WarningKind::Simplified,
                            format!(
                                "Discriminated union (tag: {}, {} variants) mapped to JsValue per config",
                                discriminant,
                                variants.len()
                            ),
                        );
                        "JsValue".to_string()
                    }
                    UnionStrategy::Enum | UnionStrategy::Auto => {
                        // For discriminated unions, we could generate a Husk enum
                        // but this requires generating a type definition, not just a type reference
                        // For now, warn and use JsValue
                        self.warn(
                            WarningKind::Simplified,
                            format!(
                                "Discriminated union (tag: {}, {} variants) mapped to JsValue - enum generation pending",
                                discriminant,
                                variants.len()
                            ),
                        );
                        "JsValue".to_string()
                    }
                }
            }

            unions::UnionStrategy::TypeEnum { variants } => {
                // Union of named types like Foo | Bar
                self.metrics.record_union_jsvalue();
                match self.options.union_strategy {
                    UnionStrategy::JsValue => {
                        self.warn(
                            WarningKind::Simplified,
                            format!(
                                "Type union ({} variants) mapped to JsValue per config",
                                variants.len()
                            ),
                        );
                        "JsValue".to_string()
                    }
                    UnionStrategy::Enum => {
                        // User explicitly wants enums - warn about limitation
                        self.warn(
                            WarningKind::Simplified,
                            format!(
                                "Type union ({} variants: {}...) mapped to JsValue - inline enum generation not yet supported",
                                variants.len(),
                                variants.iter().take(3).cloned().collect::<Vec<_>>().join(", ")
                            ),
                        );
                        "JsValue".to_string()
                    }
                    UnionStrategy::Auto => {
                        // For Auto with small unions, we could try to be smarter
                        if variants.len() <= 4 {
                            self.warn(
                                WarningKind::Simplified,
                                format!(
                                    "Type union ({}) mapped to JsValue - enum generation pending",
                                    variants.join(" | ")
                                ),
                            );
                        } else {
                            self.warn(
                                WarningKind::Simplified,
                                format!(
                                    "Large type union ({} variants) mapped to JsValue",
                                    variants.len()
                                ),
                            );
                        }
                        "JsValue".to_string()
                    }
                }
            }

            unions::UnionStrategy::Overloaded { .. } => {
                // Function overloads - handled at function level, not type level
                self.metrics.record_union_jsvalue();
                self.warn(
                    WarningKind::Simplified,
                    "Function union/overload mapped to JsValue",
                );
                "JsValue".to_string()
            }

            unions::UnionStrategy::JsValue => {
                // Analysis determined JsValue is best
                self.metrics.record_union_jsvalue();
                self.warn(
                    WarningKind::Simplified,
                    "Mixed type union mapped to JsValue",
                );
                "JsValue".to_string()
            }

            unions::UnionStrategy::Passthrough(ref remaining) => {
                // Could not categorize - fall back based on config
                self.metrics.record_union_jsvalue();
                match self.options.union_strategy {
                    UnionStrategy::Enum => {
                        self.warn(
                            WarningKind::Simplified,
                            format!(
                                "Complex union ({} variants) could not generate enum, using JsValue",
                                remaining.len()
                            ),
                        );
                        "JsValue".to_string()
                    }
                    UnionStrategy::Auto | UnionStrategy::JsValue => {
                        self.warn(
                            WarningKind::Simplified,
                            format!("Complex union ({} variants) mapped to JsValue", remaining.len()),
                        );
                        "JsValue".to_string()
                    }
                }
            }
        }
    }

    fn map_function_type(&mut self, f: &FunctionType) -> String {
        // Check if we can map to Husk function type
        let can_map = f.this_param.is_none() && f.type_params.is_empty();

        if !can_map {
            // Fall back to JsFn
            return "JsFn".to_string();
        }

        let param_types: Vec<String> = f.params.iter().map(|p| self.map_type(&p.ty)).collect();
        let ret_type = self.map_type(&f.return_type);

        if param_types.is_empty() {
            format!("fn() -> {}", ret_type)
        } else {
            format!("fn({}) -> {}", param_types.join(", "), ret_type)
        }
    }

    fn map_param_type(&mut self, ty: &DtsType, optional: bool) -> String {
        let mapped = self.map_type(ty);
        if optional {
            format!("Option<{}>", mapped)
        } else {
            mapped
        }
    }

    fn emit_header(&mut self) {
        writeln!(self.output, "// Auto-generated from .d.ts file").unwrap();
        writeln!(
            self.output,
            "// Some types may be simplified - see warnings below"
        )
        .unwrap();
        writeln!(self.output).unwrap();
    }

    fn emit_extern_block(&mut self) {
        writeln!(self.output, "extern \"js\" {{").unwrap();

        // Determine module alias for code generation
        let mod_alias = self
            .options
            .module_name
            .as_ref()
            .map(|n| {
                if is_valid_identifier(n) {
                    n.clone()
                } else {
                    derive_binding_from_package(n)
                }
            })
            .unwrap_or_else(|| "module".to_string());

        // Handle module identity for CommonJS `export =` patterns
        match &self.options.module_identity {
            Some(ModuleIdentity::Function { name }) => {
                // For callable modules (like express), emit a comment and
                // use the module as a function entry point
                writeln!(
                    self.output,
                    "    // Module is callable: `export = {}`",
                    name
                )
                .unwrap();
                self.emit_module_import();
                self.emit_callable_module_function(&mod_alias, name);
            }
            Some(ModuleIdentity::Class { name }) => {
                // For class modules, the struct IS the module export
                // We generate a module-level callable that constructs the class
                writeln!(
                    self.output,
                    "    // Module exports class: `export = {}`",
                    name
                )
                .unwrap();
                self.emit_module_import();
                self.emit_class_module_callable(&mod_alias, name);
                self.emit_standard_structs_and_functions();
            }
            Some(ModuleIdentity::Hybrid { name, is_function }) => {
                // For hybrid modules, emit BOTH the callable and the namespace items
                writeln!(
                    self.output,
                    "    // Hybrid module: {} + namespace merged",
                    if *is_function { "function" } else { "class" }
                )
                .unwrap();
                self.emit_module_import();
                // Emit the callable entry point
                self.emit_callable_module_function(&mod_alias, name);
                writeln!(self.output).unwrap();
                // Emit the namespace contents (structs, other functions)
                self.emit_standard_structs_and_functions();
            }
            Some(ModuleIdentity::Namespace { name }) => {
                // Namespace export - treat as standard module but note the export
                writeln!(
                    self.output,
                    "    // Module exports namespace: `export = {}`",
                    name
                )
                .unwrap();
                self.emit_module_import();
                self.emit_standard_structs_and_functions();
            }
            Some(ModuleIdentity::StandardModule) | None => {
                // Standard ES module - just named exports
                self.emit_module_import();
                self.emit_standard_structs_and_functions();
            }
        }

        writeln!(self.output, "}}").unwrap();
    }

    /// Emit the module import statement.
    fn emit_module_import(&mut self) {
        if let Some(mod_name) = &self.options.module_name {
            if is_valid_identifier(mod_name) {
                writeln!(self.output, "    mod {};", mod_name).unwrap();
            } else {
                let alias = derive_binding_from_package(mod_name);
                writeln!(self.output, "    mod \"{}\" as {};", mod_name, alias).unwrap();
            }
            writeln!(self.output).unwrap();
        }
    }

    /// Emit a callable module function (for `export = func` patterns).
    ///
    /// This looks for the exported function in our collected functions, emits
    /// it as the module's callable entry point, and removes it from the list
    /// to prevent duplicate emission in `emit_standard_structs_and_functions`.
    fn emit_callable_module_function(&mut self, mod_alias: &str, exported_name: &str) {
        // Find and remove the exported function from our collected functions
        let idx = self.functions.iter().position(|f| f.name == exported_name);
        if let Some(idx) = idx {
            let f = self.functions.remove(idx);

            writeln!(
                self.output,
                "    // {} is the callable entry point",
                exported_name
            )
            .unwrap();

            if let Some(comment) = &f.comment {
                writeln!(self.output, "    // {}", comment).unwrap();
            }

            // Emit the function with the module alias as the binding name
            // Use #[js_name] attribute to map to the original JS function name
            writeln!(self.output, "    #[js_name = \"{}\"]", exported_name).unwrap();
            write!(self.output, "    fn {}", mod_alias).unwrap();

            if !f.type_params.is_empty() {
                write!(self.output, "<{}>", f.type_params.join(", ")).unwrap();
            }

            write!(self.output, "(").unwrap();
            for (i, (name, ty)) in f.params.iter().enumerate() {
                if i > 0 {
                    write!(self.output, ", ").unwrap();
                }
                write!(self.output, "{}: {}", name, ty).unwrap();
            }
            write!(self.output, ")").unwrap();

            if let Some(ret) = &f.return_type {
                write!(self.output, " -> {}", ret).unwrap();
            }

            writeln!(self.output, ";").unwrap();
        }
    }

    /// Emit a callable for a class module (for `export = ClassName` patterns).
    ///
    /// This looks for the constructor (named `new_`) in the class's impl block and
    /// generates a module-level callable that constructs the class.
    ///
    /// Also checks for `{ClassName}Constructor` pattern which is common in TypeScript
    /// definitions where the constructor interface is separate from the class.
    fn emit_class_module_callable(&mut self, mod_alias: &str, class_name: &str) {
        // Look for the constructor in the class's impl methods
        // Also check {ClassName}Constructor pattern (common in .d.ts files)
        // We need to check BOTH because Database may exist without new_ while
        // DatabaseConstructor has the new_ static method
        let constructor_name = format!("{}Constructor", class_name);

        // Find the constructor method - check both class and constructor interface
        let ctor = self
            .impls
            .get(class_name)
            .and_then(|methods| methods.iter().find(|m| m.name == "new_" && m.is_static))
            .or_else(|| {
                self.impls
                    .get(&constructor_name)
                    .and_then(|methods| methods.iter().find(|m| m.name == "new_" && m.is_static))
            });

        if let Some(ctor) = ctor {
            writeln!(
                self.output,
                "    // {} constructor as module callable",
                class_name
            )
            .unwrap();

            // Emit the callable with the module alias
            // Use #[js_name] attribute to map to the class constructor
            writeln!(self.output, "    #[js_name = \"{}\"]", class_name).unwrap();
            write!(self.output, "    fn {}", mod_alias).unwrap();

            write!(self.output, "(").unwrap();
            for (i, (name, ty)) in ctor.params.iter().enumerate() {
                if i > 0 {
                    write!(self.output, ", ").unwrap();
                }
                write!(self.output, "{}: {}", name, ty).unwrap();
            }
            write!(self.output, ")").unwrap();

            if let Some(ret) = &ctor.return_type {
                write!(self.output, " -> {}", ret).unwrap();
            }

            writeln!(self.output, ";").unwrap();
            writeln!(self.output).unwrap();
        }
    }

    /// Emit the standard structs and functions (for non-callable modules).
    fn emit_standard_structs_and_functions(&mut self) {
        // Structs
        let mut sorted_structs: Vec<(&String, &Vec<String>)> = self.structs.iter().collect();
        sorted_structs.sort_by_key(|(name, _)| *name);
        for (name, type_params) in sorted_structs {
            // Skip JS built-in types that would shadow globals
            if is_js_builtin(name) {
                continue;
            }

            if type_params.is_empty() {
                writeln!(self.output, "    struct {};", name).unwrap();
            } else {
                writeln!(self.output, "    struct {}<{}>;", name, type_params.join(", ")).unwrap();
            }
        }

        if !self.structs.is_empty() && !self.functions.is_empty() {
            writeln!(self.output).unwrap();
        }

        // Functions
        for f in &self.functions {
            // Skip JS built-in functions that would shadow globals
            if is_js_builtin(&f.name) {
                continue;
            }

            // Skip functions that share a name with a struct (class constructors)
            // In TypeScript, `declare class Foo { }` generates both a type and a value,
            // but in Husk we only need the struct - use `new_()` methods for construction.
            if self.structs.contains_key(&f.name) {
                continue;
            }

            if let Some(comment) = &f.comment {
                writeln!(self.output, "    // {}", comment).unwrap();
            }

            write!(self.output, "    fn {}", f.name).unwrap();

            if !f.type_params.is_empty() {
                write!(self.output, "<{}>", f.type_params.join(", ")).unwrap();
            }

            write!(self.output, "(").unwrap();

            // Track if we need comma before regular params
            let mut need_comma = false;

            // Add #[this] parameter if this function has explicit this binding
            if let Some(this_ty) = &f.this_param {
                write!(self.output, "#[this] this_arg: {}", this_ty).unwrap();
                need_comma = true;
            }

            for (i, (name, ty)) in f.params.iter().enumerate() {
                if i > 0 || need_comma {
                    write!(self.output, ", ").unwrap();
                }
                write!(self.output, "{}: {}", name, ty).unwrap();
            }
            write!(self.output, ")").unwrap();

            if let Some(ret) = &f.return_type {
                write!(self.output, " -> {}", ret).unwrap();
            }

            writeln!(self.output, ";").unwrap();
        }

        // Consts
        if !self.consts.is_empty() {
            if !self.functions.is_empty() {
                writeln!(self.output).unwrap();
            }
            for c in &self.consts {
                writeln!(self.output, "    const {}: {};", c.name, c.ty).unwrap();
            }
        }
    }

    /// Emit untagged enums for TypeScript union types.
    fn emit_untagged_enums(&mut self) {
        if self.enums.is_empty() {
            return;
        }

        writeln!(self.output).unwrap();

        // Deduplicate enums by name (same union type may be defined in multiple files)
        let mut emitted_names: HashSet<&str> = HashSet::new();

        for e in &self.enums {
            // Skip duplicates
            if !emitted_names.insert(&e.name) {
                continue;
            }

            writeln!(self.output, "#[untagged]").unwrap();
            writeln!(self.output, "enum {} {{", e.name).unwrap();

            for (variant_name, inner_type) in &e.variants {
                match inner_type {
                    Some(ty) => {
                        writeln!(self.output, "    {}({}),", variant_name, ty).unwrap();
                    }
                    None => {
                        writeln!(self.output, "    {},", variant_name).unwrap();
                    }
                }
            }

            writeln!(self.output, "}}").unwrap();
            writeln!(self.output).unwrap();
        }
    }

    fn emit_impl_blocks(&mut self) {
        // Collect all type names that have either methods or properties
        let mut all_type_names: HashSet<&String> = HashSet::new();
        all_type_names.extend(self.impls.keys());
        all_type_names.extend(self.properties.keys());

        let mut sorted_types: Vec<&String> = all_type_names.into_iter().collect();
        sorted_types.sort();

        for type_name in sorted_types {
            // Substitute struct-level generic parameters with JsValue because extern impl parsing
            // does not support generics and leaving the raw parameter names creates unbound types.
            let generic_params: Vec<String> = self
                .structs
                .get(type_name)
                .cloned()
                .unwrap_or_default();
            let substitute_generics = |ty: &str| -> String {
                if generic_params.is_empty() {
                    return ty.to_string();
                }

                let is_ident_char = |c: char| c.is_ascii_alphanumeric() || c == '_';
                let mut out = String::new();
                let mut current = String::new();

                for ch in ty.chars() {
                    if is_ident_char(ch) {
                        current.push(ch);
                    } else {
                        if !current.is_empty() {
                            if generic_params.iter().any(|gp| gp == &current) {
                                out.push_str("JsValue");
                            } else {
                                out.push_str(&current);
                            }
                            current.clear();
                        }
                        out.push(ch);
                    }
                }

                if !current.is_empty() {
                    if generic_params.iter().any(|gp| gp == &current) {
                        out.push_str("JsValue");
                    } else {
                        out.push_str(&current);
                    }
                }

                out
            };

            let methods = self
                .impls
                .get(type_name)
                .map(|v| v.as_slice())
                .unwrap_or(&[]);
            let properties = self
                .properties
                .get(type_name)
                .map(|v| v.as_slice())
                .unwrap_or(&[]);

            if methods.is_empty() && properties.is_empty() {
                continue;
            }

            // Skip impl blocks for JS built-in types
            if is_js_builtin(type_name) {
                continue;
            }

            writeln!(self.output).unwrap();
            // Emit impl block without type parameters
            // The semantic analyzer matches impl blocks by base type name only,
            // so `impl Statement` will apply to `Statement<JsValue, JsValue>`
            writeln!(self.output, "impl {} {{", type_name).unwrap();

            // Emit properties with appropriate attributes
            for p in properties {
                // Always add getter
                writeln!(self.output, "    #[getter]").unwrap();

                // Add setter for writable (non-readonly) properties
                if !p.is_readonly {
                    writeln!(self.output, "    #[setter]").unwrap();
                }

                // Static properties need special handling
                if p.is_static {
                    writeln!(
                        self.output,
                        "    // TODO: static property - may need manual adjustment"
                    )
                    .unwrap();
                }

                writeln!(
                    self.output,
                    "    extern \"js\" {}: {};",
                    p.name,
                    substitute_generics(&p.ty)
                )
                .unwrap();
            }

            // Add blank line between properties and methods if both exist
            if !properties.is_empty() && !methods.is_empty() {
                writeln!(self.output).unwrap();
            }

            // Emit methods
            for m in methods {
                if let Some(comment) = &m.comment {
                    writeln!(self.output, "    // {}", comment).unwrap();
                }

                write!(self.output, "    extern \"js\" fn {}", m.name).unwrap();

                // NOTE: Type params are not output because the extern impl parser doesn't support them.
                // Method type params get mapped to JsValue during type mapping.

                write!(self.output, "(").unwrap();

                // Track if we need a comma before regular params
                let mut need_comma = false;

                // Add self for non-static methods
                if !m.is_static {
                    write!(self.output, "self").unwrap();
                    need_comma = true;
                }

                // Add #[this] parameter if this method has explicit this binding
                if let Some(this_ty) = &m.this_param {
                    if need_comma {
                        write!(self.output, ", ").unwrap();
                    }
                    write!(self.output, "#[this] this_arg: {}", this_ty).unwrap();
                    need_comma = true;
                }

                for (i, (name, ty)) in m.params.iter().enumerate() {
                    if i > 0 || need_comma {
                        write!(self.output, ", ").unwrap();
                    }
                    write!(self.output, "{}: {}", name, substitute_generics(ty)).unwrap();
                }
                write!(self.output, ")").unwrap();

                if let Some(ret) = &m.return_type {
                    write!(self.output, " -> {}", substitute_generics(ret)).unwrap();
                }

                writeln!(self.output, ";").unwrap();
            }

            writeln!(self.output, "}}").unwrap();
        }
    }

    /// Emit builder patterns for interfaces with many optional properties.
    fn emit_builders(&mut self) {
        if !self.options.generate_builders || self.interfaces.is_empty() {
            return;
        }

        let config = BuilderConfig {
            min_optional_props: self.options.builder_min_optional,
            ..Default::default()
        };

        let mut builders_generated = false;

        for iface in &self.interfaces {
            if builder::should_generate_builder(iface, &config) {
                self.metrics.record_builder_generated();
                let generated = builder::generate_builder(iface, &config);

                if !builders_generated {
                    writeln!(self.output).unwrap();
                    writeln!(self.output, "// Builder patterns for interfaces with optional properties").unwrap();
                    builders_generated = true;
                }

                writeln!(self.output).unwrap();
                write!(self.output, "{}", generated.code).unwrap();
            } else {
                self.metrics.record_builder_skipped();
            }
        }
    }

    fn emit_warnings(&mut self) {
        if self.warnings.is_empty() || !self.options.verbose {
            return;
        }

        writeln!(self.output).unwrap();
        writeln!(self.output, "// WARNINGS:").unwrap();
        for w in &self.warnings {
            let prefix = match w.kind {
                WarningKind::Unsupported => "UNSUPPORTED",
                WarningKind::Simplified => "SIMPLIFIED",
                WarningKind::Skipped => "SKIPPED",
            };
            writeln!(self.output, "// {}: {}", prefix, w.message).unwrap();
        }
    }
}

/// Check if a string is a JavaScript built-in that should NOT be generated as a function.
/// These would shadow Node.js module system globals and cause runtime errors.
///
/// Note: We only filter the module system builtins and singleton globals that break
/// when shadowed. Web APIs like fetch/setTimeout are legitimate exports.
pub fn is_js_builtin(name: &str) -> bool {
    matches!(
        name,
        // Node.js CommonJS module system globals - shadowing these breaks require()
        "require"
            | "module"
            | "exports"
            | "__dirname"
            | "__filename"
            // The global object itself - shadowing these is nonsensical
            | "globalThis"
            | "global"
            // Singleton globals that would break if shadowed
            | "console"
    )
}

/// Check if a string is a Husk reserved keyword or a TypeScript keyword
/// that might conflict when used as an identifier.
pub fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        // Husk keywords
        "as" | "pub"
            | "use"
            | "fn"
            | "let"
            | "mod"
            | "mut"
            | "struct"
            | "enum"
            | "type"
            | "extern"
            | "if"
            | "else"
            | "while"
            | "match"
            | "return"
            | "true"
            | "false"
            | "break"
            | "continue"
            | "trait"
            | "impl"
            | "for"
            | "Self"
            // TypeScript keywords that might conflict as property/method names
            | "static"
            | "readonly"
            | "const"
            | "var"
            | "function"
            | "class"
            | "interface"
            | "public"
            | "private"
            | "protected"
            | "abstract"
            | "async"
            | "await"
            | "new"
            | "ref"
            | "global"
            | "js"
            | "void"
            | "null"
            | "default"
            | "export"
            | "import"
            | "extends"
            | "implements"
            | "typeof"
            | "instanceof"
            | "in"
    )
}

/// Sanitize an identifier by replacing invalid characters with underscores.
/// Handles dots, colons, hyphens, slashes that appear in TypeScript identifiers.
/// Returns (sanitized_name, needs_js_name_attr) where needs_js_name_attr is true
/// if the original name differs from the sanitized version.
fn sanitize_identifier(name: &str) -> (String, bool) {
    // Check if sanitization is needed
    let needs_sanitization = name.contains(|c: char| {
        matches!(c, '.' | ':' | '-' | '/')
    }) || name.starts_with(':');

    if !needs_sanitization {
        return (name.to_string(), false);
    }

    let mut result = String::with_capacity(name.len());
    let mut prev_was_separator = false;

    for (i, c) in name.chars().enumerate() {
        match c {
            '.' | ':' | '-' | '/' => {
                // Skip leading separators for colon (HTTP/2 pseudo-headers)
                if i == 0 && c == ':' {
                    prev_was_separator = true;
                    continue;
                }
                // Avoid double underscores
                if !prev_was_separator && !result.is_empty() {
                    result.push('_');
                }
                prev_was_separator = true;
            }
            _ => {
                result.push(c);
                prev_was_separator = false;
            }
        }
    }

    // Handle edge case: name was only special chars
    if result.is_empty() {
        result.push_str("value");
    }

    (result, true)
}

/// Escape a name if it's a reserved keyword by appending an underscore.
/// Also sanitizes identifiers with special characters (dots, colons, hyphens).
pub fn escape_keyword(name: &str) -> String {
    let (sanitized, _) = sanitize_identifier(name);
    if is_keyword(&sanitized) {
        format!("{}_", sanitized)
    } else {
        sanitized
    }
}

/// Convert a string literal to a valid PascalCase variant name.
fn string_to_variant_name(s: &str) -> String {
    // Handle empty strings
    if s.is_empty() {
        return "Empty".to_string();
    }

    // Split by non-alphanumeric characters and convert to PascalCase
    let mut result = String::new();
    let mut capitalize_next = true;

    for c in s.chars() {
        if c.is_alphanumeric() {
            if capitalize_next {
                result.push(c.to_ascii_uppercase());
                capitalize_next = false;
            } else {
                result.push(c);
            }
        } else {
            // Non-alphanumeric character - skip it but capitalize next
            capitalize_next = true;
        }
    }

    // Ensure it doesn't start with a digit
    if result.starts_with(|c: char| c.is_ascii_digit()) {
        result.insert(0, 'N');
    }

    // Ensure we have a valid identifier
    if result.is_empty() {
        "Value".to_string()
    } else {
        result
    }
}

/// Convert a primitive type to a PascalCase variant name.
fn primitive_to_variant_name(p: &Primitive) -> String {
    match p {
        Primitive::String => "Str".to_string(),
        Primitive::Number => "Num".to_string(),
        Primitive::Boolean => "Bool".to_string(),
        Primitive::Void => "Void".to_string(),
        Primitive::Null => "Null".to_string(),
        Primitive::Undefined => "Undefined".to_string(),
        Primitive::Any => "Any".to_string(),
        Primitive::Unknown => "Unknown".to_string(),
        Primitive::Never => "Never".to_string(),
        Primitive::Object => "Obj".to_string(),
        Primitive::Symbol => "Symbol".to_string(),
        Primitive::BigInt => "BigInt".to_string(),
    }
}

/// Convert a DtsInterface to a DtsType::Object for type registry.
///
/// This is used for utility type expansion - the interface is converted to an
/// object literal type so utility types like Partial<MyInterface> can be expanded.
fn interface_to_object_type(iface: &DtsInterface) -> DtsType {
    let members: Vec<ObjectMember> = iface
        .members
        .iter()
        .filter_map(|m| match m {
            InterfaceMember::Property(p) => Some(ObjectMember::Property {
                name: p.name.clone(),
                ty: p.ty.clone(),
                optional: p.optional,
                readonly: p.readonly,
            }),
            InterfaceMember::Method(m) => Some(ObjectMember::Method {
                name: m.name.clone(),
                type_params: m.type_params.clone(),
                params: m.params.clone(),
                return_type: m.return_type.clone(),
                optional: m.optional,
                this_param: m.this_param.clone(),
            }),
            InterfaceMember::IndexSignature(sig) => {
                Some(ObjectMember::IndexSignature(sig.clone()))
            }
            InterfaceMember::CallSignature(sig) => Some(ObjectMember::CallSignature(sig.clone())),
            InterfaceMember::ConstructSignature(sig) => {
                Some(ObjectMember::ConstructSignature(sig.clone()))
            }
        })
        .collect();

    DtsType::Object(members)
}

/// Convert a DtsType to a Husk type string.
///
/// This is a simplified conversion for use in builder patterns and union type display.
/// For full code generation, use the Codegen struct which handles more complex cases.
pub fn type_to_husk_string(ty: &DtsType) -> String {
    match ty {
        DtsType::Primitive(p) => primitive_to_husk_string(p),
        DtsType::Named { name, type_args } => {
            if type_args.is_empty() {
                name.clone()
            } else {
                let args: Vec<_> = type_args.iter().map(type_to_husk_string).collect();
                format!("{}<{}>", name, args.join(", "))
            }
        }
        DtsType::Array(inner) => format!("JsArray<{}>", type_to_husk_string(inner)),
        DtsType::StringLiteral(s) => format!("\"{}\"", s),
        DtsType::NumberLiteral(n) => n.clone(),
        DtsType::BooleanLiteral(b) => b.to_string(),
        DtsType::Union(types) => {
            // Check for Option pattern: T | null, T | undefined, or T | null | undefined
            let non_null: Vec<_> = types
                .iter()
                .filter(|t| {
                    !matches!(
                        t,
                        DtsType::Primitive(Primitive::Null)
                            | DtsType::Primitive(Primitive::Undefined)
                    )
                })
                .collect();
            // If we filtered out at least one null/undefined and have exactly one remaining type
            if non_null.len() == 1 && non_null.len() < types.len() {
                return format!("Option<{}>", type_to_husk_string(non_null[0]));
            }
            // Fall back to showing the union types
            let strs: Vec<_> = types.iter().map(type_to_husk_string).collect();
            strs.join(" | ")
        }
        DtsType::Function(_) => "JsFn".to_string(),
        DtsType::Object(_) => "JsObject".to_string(),
        DtsType::Tuple(elements) => {
            let types: Vec<_> = elements.iter().map(|e| type_to_husk_string(&e.ty)).collect();
            format!("({})", types.join(", "))
        }
        _ => "JsValue".to_string(),
    }
}

/// Convert a TypeScript primitive to Husk type name.
fn primitive_to_husk_string(p: &Primitive) -> String {
    match p {
        Primitive::String => "String".to_string(),
        Primitive::Number => "f64".to_string(),
        Primitive::Boolean => "bool".to_string(),
        Primitive::Void => "()".to_string(),
        Primitive::Null | Primitive::Undefined => "()".to_string(),
        Primitive::Any | Primitive::Unknown => "JsValue".to_string(),
        Primitive::Never => "()".to_string(), // Husk doesn't have never type
        Primitive::Object => "JsObject".to_string(),
        Primitive::Symbol => "JsValue".to_string(),
        Primitive::BigInt => "i64".to_string(),
    }
}

/// Check if a string is a valid Husk identifier.
fn is_valid_identifier(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }
    let mut chars = name.chars();
    let first = chars.next().unwrap();
    if !first.is_ascii_alphabetic() && first != '_' {
        return false;
    }
    for ch in chars {
        if !ch.is_ascii_alphanumeric() && ch != '_' {
            return false;
        }
    }
    !is_keyword(name)
}

/// Derive a valid identifier from a package name.
fn derive_binding_from_package(package: &str) -> String {
    // Strip @scope/ prefix
    let name = if let Some(idx) = package.rfind('/') {
        &package[idx + 1..]
    } else {
        package
    };

    // Replace hyphens with underscores
    name.replace('-', "_")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_generate_simple_function() {
        let src = "declare function add(a: number, b: number): number;";
        let file = parse(src).unwrap();
        let result = generate(&file, &CodegenOptions::default());

        assert!(result.code.contains("fn add(a: f64, b: f64) -> f64;"));
    }

    #[test]
    fn test_generate_interface_with_methods() {
        let src = r#"
            interface Server {
                listen(port: number): void;
                close(): void;
            }
        "#;
        let file = parse(src).unwrap();
        let result = generate(&file, &CodegenOptions::default());

        assert!(result.code.contains("struct Server;"));
        assert!(result.code.contains("impl Server {"));
        assert!(
            result
                .code
                .contains("extern \"js\" fn listen(self, port: f64);")
        );
        assert!(result.code.contains("extern \"js\" fn close(self);"));
    }

    #[test]
    fn test_generate_generic_function() {
        // Generic type parameters on extern functions are simplified to JsValue
        // because the Husk parser doesn't support type params on extern block functions.
        let src = "declare function identity<T>(x: T): T;";
        let file = parse(src).unwrap();
        let result = generate(&file, &CodegenOptions::default());

        assert!(result.code.contains("fn identity(x: JsValue) -> JsValue;"));
    }

    #[test]
    fn test_generate_optional_params() {
        let src = "declare function foo(a: string, b?: number): void;";
        let file = parse(src).unwrap();
        let result = generate(&file, &CodegenOptions::default());

        assert!(result.code.contains("fn foo(a: String, b: Option<f64>);"));
    }

    #[test]
    fn test_generate_callback_function() {
        let src = "declare function setTimeout(callback: () => void, ms: number): number;";
        let file = parse(src).unwrap();
        let result = generate(&file, &CodegenOptions::default());

        assert!(
            result
                .code
                .contains("fn setTimeout(callback: fn() -> (), ms: f64) -> f64;")
        );
    }

    #[test]
    fn test_generate_nullable_type() {
        let src = r#"
            interface User {}
            declare function find(id: string): User | null;
        "#;
        let file = parse(src).unwrap();
        let result = generate(&file, &CodegenOptions::default());

        assert!(result.code.contains("fn find(id: String) -> Option<User>;"));
    }

    #[test]
    fn test_generate_array_type() {
        let src = "declare function getItems(): string[];";
        let file = parse(src).unwrap();
        let result = generate(&file, &CodegenOptions::default());

        assert!(result.code.contains("fn getItems() -> JsArray<String>;"));
    }

    #[test]
    fn test_generate_promise_type() {
        let src = r#"
            interface Response {}
            declare function fetch(url: string): Promise<Response>;
        "#;
        let file = parse(src).unwrap();
        let result = generate(&file, &CodegenOptions::default());

        assert!(
            result
                .code
                .contains("fn fetch(url: String) -> JsPromise<Response>;")
        );
    }

    #[test]
    fn test_module_import() {
        let src = "declare function init(): void;";
        let file = parse(src).unwrap();
        let result = generate(
            &file,
            &CodegenOptions {
                module_name: Some("express".to_string()),
                ..Default::default()
            },
        );

        assert!(result.code.contains("mod express;"));
    }

    #[test]
    fn test_escaped_keywords() {
        let src = "declare function type(mod: string): void;";
        let file = parse(src).unwrap();
        let result = generate(&file, &CodegenOptions::default());

        assert!(result.code.contains("fn type_(mod_: String);"));
    }

    #[test]
    fn test_codegen_function_module_identity() {
        // Test that Function module identity emits callable entry point
        let src = "declare function e(): void;";
        let file = parse(src).unwrap();
        let result = generate(
            &file,
            &CodegenOptions {
                module_name: Some("express".to_string()),
                module_identity: Some(ModuleIdentity::Function {
                    name: "e".to_string(),
                }),
                ..Default::default()
            },
        );

        assert!(result.code.contains("// Module is callable: `export = e`"));
        assert!(result.code.contains("mod express;"));
        assert!(result.code.contains("// e is the callable entry point"));
        assert!(result.code.contains("#[js_name = \"e\"]"));
        assert!(result.code.contains("fn express()"));
    }

    #[test]
    fn test_codegen_class_module_identity() {
        // Test that Class module identity generates struct with constructor
        // Note: The legacy parser treats "constructor" as a method named "constructor"
        // rather than ClassMember::Constructor. The key thing is the module identity
        // comment and the struct/impl being generated correctly.
        let src = r#"
            declare class MyClass {
                doSomething(): void;
            }
        "#;
        let file = parse(src).unwrap();
        let result = generate(
            &file,
            &CodegenOptions {
                module_name: Some("my-class".to_string()),
                module_identity: Some(ModuleIdentity::Class {
                    name: "MyClass".to_string(),
                }),
                ..Default::default()
            },
        );

        assert!(result.code.contains("// Module exports class: `export = MyClass`"));
        assert!(result.code.contains("mod \"my-class\" as my_class;"));
        assert!(result.code.contains("struct MyClass;"));
        assert!(result.code.contains("impl MyClass"));
        assert!(result.code.contains("extern \"js\" fn doSomething(self);"));
    }

    #[test]
    fn test_codegen_hybrid_module_identity() {
        // Test that Hybrid module identity emits both callable and namespace items
        let src = r#"
            declare function req(url: string): void;
            interface Options {
                timeout: number;
            }
        "#;
        let file = parse(src).unwrap();
        let result = generate(
            &file,
            &CodegenOptions {
                module_name: Some("request".to_string()),
                module_identity: Some(ModuleIdentity::Hybrid {
                    name: "req".to_string(),
                    is_function: true,
                }),
                ..Default::default()
            },
        );

        assert!(result.code.contains("// Hybrid module: function + namespace merged"));
        assert!(result.code.contains("mod request;"));
        // Should have the callable entry point with js_name attribute
        assert!(result.code.contains("#[js_name = \"req\"]"));
        assert!(result.code.contains("fn request(url: String)"));
        // Should also have the namespace items (the interface struct)
        assert!(result.code.contains("struct Options;"));
    }

    #[test]
    fn test_codegen_namespace_module_identity() {
        // Test that Namespace module identity is noted in comments
        let src = r#"
            interface Foo { x: number; }
            interface Bar { y: string; }
        "#;
        let file = parse(src).unwrap();
        let result = generate(
            &file,
            &CodegenOptions {
                module_name: Some("mylib".to_string()),
                module_identity: Some(ModuleIdentity::Namespace {
                    name: "ns".to_string(),
                }),
                ..Default::default()
            },
        );

        assert!(result.code.contains("// Module exports namespace: `export = ns`"));
        assert!(result.code.contains("mod mylib;"));
        assert!(result.code.contains("struct Bar;"));
        assert!(result.code.contains("struct Foo;"));
    }

    #[test]
    fn test_codegen_standard_module_identity() {
        // Test that StandardModule / None produces normal output
        let src = "declare function hello(): void;";
        let file = parse(src).unwrap();

        // With explicit StandardModule
        let result = generate(
            &file,
            &CodegenOptions {
                module_name: Some("mymod".to_string()),
                module_identity: Some(ModuleIdentity::StandardModule),
                ..Default::default()
            },
        );

        assert!(result.code.contains("mod mymod;"));
        assert!(result.code.contains("fn hello();"));
        // Should NOT have any module identity comments
        assert!(!result.code.contains("Module is callable"));
        assert!(!result.code.contains("Module exports"));
    }

    #[test]
    fn test_generate_const_legacy_mode() {
        // Default: use_extern_const = false, generates zero-arg functions
        let src = "declare const VERSION: string;";
        let file = parse(src).unwrap();
        let result = generate(&file, &CodegenOptions::default());

        // Should generate as a function (legacy behavior)
        assert!(result.code.contains("fn VERSION() -> String;"));
        assert!(!result.code.contains("const VERSION:"));
        assert_eq!(result.metrics.legacy_const_functions, 1);
        assert_eq!(result.metrics.extern_consts, 0);
    }

    #[test]
    fn test_generate_const_new_mode() {
        // With use_extern_const = true, generates extern const
        let src = "declare const VERSION: string;";
        let file = parse(src).unwrap();
        let result = generate(
            &file,
            &CodegenOptions {
                use_extern_const: true,
                ..Default::default()
            },
        );

        // Should generate as extern const (new behavior)
        assert!(result.code.contains("const VERSION: String;"));
        assert!(!result.code.contains("fn VERSION()"));
        assert_eq!(result.metrics.extern_consts, 1);
        assert_eq!(result.metrics.legacy_const_functions, 0);
    }

    #[test]
    fn test_generate_multiple_consts() {
        let src = r#"
            declare const API_URL: string;
            declare const MAX_RETRIES: number;
            declare function fetch(): void;
        "#;
        let file = parse(src).unwrap();
        let result = generate(
            &file,
            &CodegenOptions {
                use_extern_const: true,
                ..Default::default()
            },
        );

        assert!(result.code.contains("const API_URL: String;"));
        assert!(result.code.contains("const MAX_RETRIES: f64;"));
        assert!(result.code.contains("fn fetch();"));
        assert_eq!(result.metrics.extern_consts, 2);
    }

    #[test]
    fn test_generate_method_with_this_param() {
        use crate::oxc_parser::parse_with_oxc;

        // TypeScript method signature with explicit this parameter
        // Define Element so it's recognized as a struct, not a JsValue
        let src = r#"
            interface Element {}
            interface EventTarget {
                addEventListener(this: Element, type: string, callback: () => void): void;
            }
        "#;
        let file = parse_with_oxc(src, "test.d.ts").unwrap();
        let result = generate(&file, &CodegenOptions::default());

        // Should emit #[this] attribute on first parameter
        assert!(
            result.code.contains("#[this] this_arg: Element"),
            "Expected #[this] this_arg: Element in output, got:\n{}",
            result.code
        );
    }

    #[test]
    fn test_generate_function_type_with_this_param() {
        use crate::oxc_parser::parse_with_oxc;

        // Function type annotation with this parameter
        let src = r#"
            type Handler = (this: Window, event: Event) => void;
        "#;
        let file = parse_with_oxc(src, "test.d.ts").unwrap();
        let result = generate(&file, &CodegenOptions::default());

        // Function types with this_param should include the this binding info
        // (though this may generate as JsFn since function types are complex)
        println!("Generated code:\n{}", result.code);
        // This test is informational - we want to ensure it doesn't crash
    }

    #[test]
    fn test_generate_untagged_enum_from_union_type() {
        use crate::oxc_parser::parse_with_oxc;

        // TypeScript union type alias that should become an untagged enum
        let src = r#"
            type StringOrNumber = string | number;
        "#;
        let file = parse_with_oxc(src, "test.d.ts").unwrap();
        let result = generate(&file, &CodegenOptions::default());

        println!("Generated code:\n{}", result.code);

        // Should generate an untagged enum
        assert!(
            result.code.contains("#[untagged]"),
            "Expected #[untagged] attribute in output"
        );
        assert!(
            result.code.contains("enum StringOrNumber"),
            "Expected enum StringOrNumber in output"
        );
        assert!(
            result.code.contains("Str(String)"),
            "Expected Str(String) variant in output"
        );
        assert!(
            result.code.contains("Num(f64)"),
            "Expected Num(f64) variant in output"
        );
    }

    #[test]
    fn test_generate_untagged_enum_from_string_literals() {
        use crate::oxc_parser::parse_with_oxc;

        // TypeScript string literal union type
        let src = r#"
            type HttpMethod = "GET" | "POST" | "PUT" | "DELETE";
        "#;
        let file = parse_with_oxc(src, "test.d.ts").unwrap();
        let result = generate(&file, &CodegenOptions::default());

        println!("Generated code:\n{}", result.code);

        // Should generate an untagged enum with unit variants
        assert!(
            result.code.contains("#[untagged]"),
            "Expected #[untagged] attribute in output"
        );
        assert!(
            result.code.contains("enum HttpMethod"),
            "Expected enum HttpMethod in output"
        );
        // Variants should be PascalCase
        assert!(
            result.code.contains("GET,") || result.code.contains("Get,"),
            "Expected GET or Get variant in output"
        );
        assert!(
            result.code.contains("POST,") || result.code.contains("Post,"),
            "Expected POST or Post variant in output"
        );
    }

    #[test]
    fn test_interface_inheritance_should_include_parent_methods() {
        // Issue: When interface A extends B, A's impl block should include B's methods
        // Currently the codegen doesn't follow the extends clause
        use crate::oxc_parser::parse_with_oxc;

        let src = r#"
            interface Parent {
                parentMethod(): void;
                parentProp: string;
            }
            interface Child extends Parent {
                childMethod(): void;
            }
        "#;

        let file = parse_with_oxc(src, "test.d.ts").unwrap();
        let result = generate(&file, &CodegenOptions::default());

        println!("Generated code:\n{}", result.code);

        // Child should have its own method
        assert!(
            result.code.contains("impl Child {"),
            "Expected impl Child block"
        );
        assert!(
            result.code.contains("fn childMethod(self)"),
            "Expected childMethod on Child"
        );

        // FIXME: Child should ALSO have parent's methods
        // This currently fails because we don't follow extends
        // Extract just the Child impl block (up to the closing brace)
        let child_impl = result
            .code
            .split("impl Child {")
            .nth(1)
            .and_then(|s| s.split("\n}\n").next())
            .unwrap_or("");

        println!("Child impl block contents:\n{}", child_impl);

        assert!(
            child_impl.contains("parentMethod"),
            "Expected parentMethod to be included in Child's impl block (inherited from Parent). \
             Child impl only contains: {}",
            child_impl
        );
    }

    #[test]
    fn test_module_callable_uses_module_alias() {
        // Issue: When `export = e`, the callable should use #[js_name = "e"] fn express()
        // not just `fn e()`
        use crate::resolver::ModuleIdentity;

        let src = r#"
            declare function e(): Express;
            interface Express {}
        "#;
        let file = parse(src).unwrap();

        let options = CodegenOptions {
            module_name: Some("express".to_string()),
            module_identity: Some(ModuleIdentity::Function {
                name: "e".to_string(),
            }),
            ..Default::default()
        };

        let result = generate(&file, &options);

        println!("Generated code:\n{}", result.code);

        // Should use js_name attribute to map to the original function name
        assert!(
            result.code.contains("#[js_name = \"e\"]"),
            "Expected #[js_name = \"e\"] attribute"
        );
        assert!(
            result.code.contains("fn express()"),
            "Expected fn express() as the callable"
        );
        // Should NOT have a separate fn e() declaration
        assert!(
            !result.code.contains("fn e()"),
            "Should not have duplicate fn e() declaration"
        );
    }

    #[test]
    fn test_class_module_generates_constructor_callable() {
        // Issue: For modules like better-sqlite3 that export a class,
        // we should generate a callable that constructs the class
        use crate::oxc_parser::parse_with_oxc;
        use crate::resolver::ModuleIdentity;

        let src = r#"
            declare class Database {
                constructor(filename: string, options?: Options);
                prepare(sql: string): Statement;
                close(): void;
            }
            interface Options {
                readonly?: boolean;
            }
            interface Statement {
                run(): void;
            }
        "#;
        // Use Oxc parser which properly handles class constructors
        let file = parse_with_oxc(src, "test.d.ts").unwrap();

        let options = CodegenOptions {
            module_name: Some("better_sqlite3".to_string()),
            module_identity: Some(ModuleIdentity::Class {
                name: "Database".to_string(),
            }),
            ..Default::default()
        };

        let result = generate(&file, &options);

        println!("Generated code:\n{}", result.code);

        // Should have a callable constructor using the module name with js_name attribute
        assert!(
            result.code.contains("#[js_name = \"Database\"]"),
            "Expected #[js_name = \"Database\"] attribute"
        );
        assert!(
            result.code.contains("fn better_sqlite3("),
            "Expected fn better_sqlite3() callable"
        );
    }
}
