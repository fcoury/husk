//! Generate Husk `extern "js"` code from parsed .d.ts AST.

use crate::ast::*;
use crate::builder::{self, BuilderConfig};
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
}

/// Generate Husk code from a parsed .d.ts file.
pub fn generate(file: &DtsFile, options: &CodegenOptions) -> CodegenResult {
    let mut codegen = Codegen::new(options);
    codegen.generate_file(file);
    CodegenResult {
        code: codegen.output,
        warnings: codegen.warnings,
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

    // Second pass: generate code
    codegen.emit_header();
    codegen.emit_extern_block();
    codegen.emit_impl_blocks();
    codegen.emit_builders();
    codegen.emit_warnings();

    CodegenResult {
        code: codegen.output,
        warnings: codegen.warnings,
    }
}

struct Codegen<'a> {
    options: &'a CodegenOptions,
    output: String,
    warnings: Vec<Warning>,
    /// Collected struct names (from interfaces and classes).
    structs: HashSet<String>,
    /// Collected functions (before overload merging).
    functions: Vec<GeneratedFn>,
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
}

struct GeneratedFn {
    name: String,
    type_params: Vec<String>,
    params: Vec<(String, String)>,
    return_type: Option<String>,
    comment: Option<String>,
}

struct GeneratedMethod {
    name: String,
    type_params: Vec<String>,
    params: Vec<(String, String)>,
    return_type: Option<String>,
    is_static: bool,
    comment: Option<String>,
}

/// A generated property with #[getter] attribute.
struct GeneratedProperty {
    name: String,
    ty: String,
    is_readonly: bool,
    is_static: bool,
}

impl<'a> Codegen<'a> {
    fn new(options: &'a CodegenOptions) -> Self {
        Self {
            options,
            output: String::new(),
            warnings: Vec::new(),
            structs: HashSet::new(),
            functions: Vec::new(),
            impls: HashMap::new(),
            properties: HashMap::new(),
            type_aliases: HashMap::new(),
            known_generics: HashSet::new(),
            method_type_params: HashSet::new(),
            in_struct_context: false,
            current_type_name: None,
            interfaces: Vec::new(),
            type_registry: TypeRegistry::new(),
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

        // Second pass: generate code
        self.emit_header();
        self.emit_extern_block();
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

        if overloads.len() > 0 {
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

        if overloads.len() > 0 {
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
        })
    }

    /// Pre-pass: collect struct names from interfaces and classes so type mapping knows what's defined.
    fn collect_struct_names(&mut self, item: &DtsItem) {
        match item {
            DtsItem::Interface(i) => {
                self.structs.insert(i.name.clone());
            }
            DtsItem::Class(c) => {
                self.structs.insert(c.name.clone());
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
        let type_params: Vec<String> = f.type_params.iter().map(|p| p.name.clone()).collect();

        // Set up known generics for type mapping
        self.known_generics = type_params.iter().cloned().collect();

        let mut comment = None;

        // Check for constrained type params
        for tp in &f.type_params {
            if tp.constraint.is_some() {
                comment = Some(format!("NOTE: constraint on `{}` not enforced", tp.name));
            }
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
            type_params,
            params,
            return_type,
            comment,
        });

        self.known_generics.clear();
    }

    fn collect_interface(&mut self, i: &DtsInterface) {
        self.structs.insert(i.name.clone());
        self.in_struct_context = true;
        self.current_type_name = Some(i.name.clone());

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

                    methods.push(GeneratedMethod {
                        name: escape_keyword(&m.name),
                        type_params: method_type_params,
                        params,
                        return_type,
                        is_static: false,
                        comment: None,
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
                InterfaceMember::ConstructSignature(_) => {
                    self.warn(
                        WarningKind::Skipped,
                        format!("Construct signature on {} skipped", i.name),
                    );
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
        self.structs.insert(c.name.clone());
        self.in_struct_context = true;
        self.current_type_name = Some(c.name.clone());

        let type_params: Vec<String> = c.type_params.iter().map(|p| p.name.clone()).collect();
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

                    methods.push(GeneratedMethod {
                        name: escape_keyword(&m.name),
                        type_params: method_type_params,
                        params,
                        return_type,
                        is_static: m.is_static,
                        comment: None,
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
                        name: "new".to_string(),
                        type_params: type_params.clone(),
                        params,
                        return_type: Some(c.name.clone()),
                        is_static: true,
                        comment: None,
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
    }

    fn collect_variable(&mut self, v: &DtsVariable) {
        // Generate as a function that returns the value
        // This is a simplification - ideally we'd have extern consts
        let ty = self.map_type(&v.ty);
        self.functions.push(GeneratedFn {
            name: escape_keyword(&v.name),
            type_params: Vec::new(),
            params: Vec::new(),
            return_type: Some(ty),
            comment: Some("constant".to_string()),
        });
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
                    self.map_type(&resolved_type)
                } else {
                    // Cannot resolve - fall back to JsValue
                    self.warn(
                        WarningKind::Simplified,
                        "Index access type could not be resolved, mapped to JsValue",
                    );
                    "JsValue".to_string()
                }
            }
            DtsType::Conditional { .. } => {
                self.warn(WarningKind::Unsupported, "Conditional type not supported");
                "JsValue".to_string()
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

        // Check if it's a known generic type parameter
        if self.known_generics.contains(simple_name) {
            // If we're NOT in struct/interface context, this is a function-level generic
            // and can be preserved in the generated code
            if !self.in_struct_context {
                return simple_name.to_string();
            }
            // Otherwise, it's a struct-level generic used in a method,
            // and since Husk extern structs don't support generics, simplify to JsValue
            self.warn(
                WarningKind::Simplified,
                format!(
                    "Struct-level type parameter `{}` mapped to JsValue",
                    simple_name
                ),
            );
            return "JsValue".to_string();
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
                        return self.map_type(&expanded);
                    }
                }

                // Fall back to JsValue if expansion disabled or didn't help
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
                    // Resolve type alias to its underlying type
                    return self.map_type(&alias_ty);
                }

                // Check if this type is defined in the current file
                let is_known_struct = self.structs.contains(simple_name);

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
                let mapped = self.map_type(&inner);
                format!("Option<{}>", mapped)
            }

            unions::UnionStrategy::Boolean => {
                // true | false → bool
                "bool".to_string()
            }

            unions::UnionStrategy::StringEnum { variants } => {
                // String literal union - depends on config strategy
                match self.options.union_strategy {
                    UnionStrategy::JsValue => {
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
                self.warn(
                    WarningKind::Simplified,
                    "Function union/overload mapped to JsValue",
                );
                "JsValue".to_string()
            }

            unions::UnionStrategy::JsValue => {
                // Analysis determined JsValue is best
                self.warn(
                    WarningKind::Simplified,
                    "Mixed type union mapped to JsValue",
                );
                "JsValue".to_string()
            }

            unions::UnionStrategy::Passthrough(ref remaining) => {
                // Could not categorize - fall back based on config
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
                writeln!(
                    self.output,
                    "    // Module exports class: `export = {}`",
                    name
                )
                .unwrap();
                self.emit_module_import();
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
    /// This looks for the exported function in our collected functions and emits
    /// it as the module's callable entry point.
    fn emit_callable_module_function(&mut self, mod_alias: &str, exported_name: &str) {
        // Find the exported function in our collected functions
        if let Some(f) = self.functions.iter().find(|f| f.name == exported_name) {
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
            // This makes `mod_alias(...)` the callable form
            write!(self.output, "    fn \"{}\" as {}", exported_name, mod_alias).unwrap();

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

    /// Emit the standard structs and functions (for non-callable modules).
    fn emit_standard_structs_and_functions(&mut self) {
        // Structs
        let mut sorted_structs: Vec<&String> = self.structs.iter().collect();
        sorted_structs.sort();
        for name in sorted_structs {
            writeln!(self.output, "    struct {};", name).unwrap();
        }

        if !self.structs.is_empty() && !self.functions.is_empty() {
            writeln!(self.output).unwrap();
        }

        // Functions
        for f in &self.functions {
            if let Some(comment) = &f.comment {
                writeln!(self.output, "    // {}", comment).unwrap();
            }

            write!(self.output, "    fn {}", f.name).unwrap();

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

    fn emit_impl_blocks(&mut self) {
        // Collect all type names that have either methods or properties
        let mut all_type_names: HashSet<&String> = HashSet::new();
        all_type_names.extend(self.impls.keys());
        all_type_names.extend(self.properties.keys());

        let mut sorted_types: Vec<&String> = all_type_names.into_iter().collect();
        sorted_types.sort();

        for type_name in sorted_types {
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

            writeln!(self.output).unwrap();
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

                writeln!(self.output, "    extern \"js\" {}: {};", p.name, p.ty).unwrap();
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

                // Type params (excluding the struct's own params for non-static methods)
                if !m.type_params.is_empty() && m.is_static {
                    write!(self.output, "<{}>", m.type_params.join(", ")).unwrap();
                }

                write!(self.output, "(").unwrap();

                // Add self for non-static methods
                if !m.is_static {
                    write!(self.output, "self").unwrap();
                    if !m.params.is_empty() {
                        write!(self.output, ", ").unwrap();
                    }
                }

                for (i, (name, ty)) in m.params.iter().enumerate() {
                    if i > 0 {
                        write!(self.output, ", ").unwrap();
                    }
                    write!(self.output, "{}: {}", name, ty).unwrap();
                }
                write!(self.output, ")").unwrap();

                if let Some(ret) = &m.return_type {
                    write!(self.output, " -> {}", ret).unwrap();
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
                let generated = builder::generate_builder(iface, &config);

                if !builders_generated {
                    writeln!(self.output).unwrap();
                    writeln!(self.output, "// Builder patterns for interfaces with optional properties").unwrap();
                    builders_generated = true;
                }

                writeln!(self.output).unwrap();
                write!(self.output, "{}", generated.code).unwrap();
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

/// Escape a name if it's a reserved keyword by appending an underscore.
pub fn escape_keyword(name: &str) -> String {
    if is_keyword(name) {
        format!("{}_", name)
    } else {
        name.to_string()
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
        let src = "declare function identity<T>(x: T): T;";
        let file = parse(src).unwrap();
        let result = generate(&file, &CodegenOptions::default());

        assert!(result.code.contains("fn identity<T>(x: T) -> T;"));
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
        assert!(result.code.contains("fn \"e\" as express();"));
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
        // Should have the callable entry point
        assert!(result.code.contains("fn \"req\" as request(url: String);"));
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
}
