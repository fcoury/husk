//! Generate Husk `extern "js"` code from parsed .d.ts AST.

use crate::ast::*;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

/// Options for code generation.
#[derive(Debug, Clone, Default)]
pub struct CodegenOptions {
    /// Module name for `mod` declaration in extern block.
    pub module_name: Option<String>,
    /// Include verbose warnings as comments.
    pub verbose: bool,
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

struct Codegen<'a> {
    options: &'a CodegenOptions,
    output: String,
    warnings: Vec<Warning>,
    /// Collected struct names (from interfaces and classes).
    structs: HashSet<String>,
    /// Collected functions.
    functions: Vec<GeneratedFn>,
    /// Collected impl blocks: type name -> methods.
    impls: HashMap<String, Vec<GeneratedMethod>>,
    /// Type aliases (for reference, not always generated).
    type_aliases: HashMap<String, DtsType>,
    /// Known generic types.
    known_generics: HashSet<String>,
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

impl<'a> Codegen<'a> {
    fn new(options: &'a CodegenOptions) -> Self {
        Self {
            options,
            output: String::new(),
            warnings: Vec::new(),
            structs: HashSet::new(),
            functions: Vec::new(),
            impls: HashMap::new(),
            type_aliases: HashMap::new(),
            known_generics: HashSet::new(),
        }
    }

    fn warn(&mut self, kind: WarningKind, message: impl Into<String>) {
        self.warnings.push(Warning {
            message: message.into(),
            kind,
        });
    }

    fn generate_file(&mut self, file: &DtsFile) {
        // First pass: collect all declarations
        for item in &file.items {
            self.collect_item(item);
        }

        // Second pass: generate code
        self.emit_header();
        self.emit_extern_block();
        self.emit_impl_blocks();
        self.emit_warnings();
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
                comment = Some(format!(
                    "NOTE: constraint on `{}` not enforced",
                    tp.name
                ));
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
            if mapped == "()" {
                None
            } else {
                Some(mapped)
            }
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

        let type_params: Vec<String> = i.type_params.iter().map(|p| p.name.clone()).collect();
        self.known_generics = type_params.iter().cloned().collect();

        let mut methods = Vec::new();

        for member in &i.members {
            match member {
                InterfaceMember::Method(m) => {
                    let method_type_params: Vec<String> = type_params
                        .iter()
                        .chain(m.type_params.iter().map(|p| &p.name))
                        .cloned()
                        .collect();

                    // Temporarily add method type params to known generics
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
                        if mapped == "()" {
                            None
                        } else {
                            Some(mapped)
                        }
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
                }
                InterfaceMember::Property(p) => {
                    let mapped_type = self.map_type(&p.ty);

                    // Generate getter method for property access
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
                        is_static: false,
                        comment: if p.readonly {
                            Some("property (readonly)".to_string())
                        } else {
                            Some("property getter".to_string())
                        },
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
                            is_static: false,
                            comment: Some("property setter".to_string()),
                        });
                    }
                }
                InterfaceMember::CallSignature(_) => {
                    self.warn(WarningKind::Skipped, format!("Call signature on {} skipped", i.name));
                }
                InterfaceMember::ConstructSignature(_) => {
                    self.warn(WarningKind::Skipped, format!("Construct signature on {} skipped", i.name));
                }
                InterfaceMember::IndexSignature(_) => {
                    self.warn(WarningKind::Skipped, format!("Index signature on {} skipped", i.name));
                }
            }
        }

        if !methods.is_empty() {
            self.impls.insert(i.name.clone(), methods);
        }

        self.known_generics.clear();
    }

    fn collect_class(&mut self, c: &DtsClass) {
        self.structs.insert(c.name.clone());

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
                        if mapped == "()" {
                            None
                        } else {
                            Some(mapped)
                        }
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
                    let mapped_type = p
                        .ty
                        .as_ref()
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
                    self.warn(WarningKind::Skipped, format!("Index signature on {} skipped", c.name));
                }
            }
        }

        if !methods.is_empty() {
            self.impls.insert(c.name.clone(), methods);
        }

        self.known_generics.clear();
    }

    fn collect_type_alias(&mut self, t: &DtsTypeAlias) {
        self.type_aliases.insert(t.name.clone(), t.ty.clone());
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
                self.warn(WarningKind::Simplified, "String literal type mapped to String");
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
                self.warn(WarningKind::Simplified, "Intersection type mapped to JsValue");
                "JsValue".to_string()
            }
            DtsType::Function(f) => self.map_function_type(f),
            DtsType::Object(_) => {
                self.warn(WarningKind::Simplified, "Object literal type mapped to JsValue");
                "JsValue".to_string()
            }
            DtsType::Array(inner) => {
                let inner_mapped = self.map_type(inner);
                format!("JsArray<{}>", inner_mapped)
            }
            DtsType::Tuple(elements) => {
                // Map to tuple type if Husk supports it, otherwise JsValue
                if elements.len() <= 4 {
                    let mapped: Vec<String> = elements.iter().map(|e| self.map_type(&e.ty)).collect();
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
            DtsType::KeyOf(_) => {
                self.warn(WarningKind::Unsupported, "keyof type not supported");
                "JsValue".to_string()
            }
            DtsType::IndexAccess { .. } => {
                self.warn(WarningKind::Unsupported, "Index access type not supported");
                "JsValue".to_string()
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
                self.warn(WarningKind::Simplified, "Template literal type mapped to String");
                "String".to_string()
            }
            DtsType::Parenthesized(inner) => self.map_type(inner),
            DtsType::This => {
                // `this` type - use Self if available
                "Self".to_string()
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
        // Check if it's a known generic
        if self.known_generics.contains(name) {
            return name.to_string();
        }

        // Map well-known types
        match name {
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
                // TypeScript utility types - map to JsValue
                self.warn(
                    WarningKind::Unsupported,
                    format!("Utility type {} not supported", name),
                );
                "JsValue".to_string()
            }
            _ => {
                // User-defined type - keep as-is
                if type_args.is_empty() {
                    name.to_string()
                } else {
                    let args: Vec<String> = type_args.iter().map(|a| self.map_type(a)).collect();
                    format!("{}<{}>", name, args.join(", "))
                }
            }
        }
    }

    fn map_union_type(&mut self, types: &[DtsType]) -> String {
        // Check for nullable pattern: T | null or T | undefined
        let non_null: Vec<&DtsType> = types
            .iter()
            .filter(|t| {
                !matches!(
                    t,
                    DtsType::Primitive(Primitive::Null) | DtsType::Primitive(Primitive::Undefined)
                )
            })
            .collect();

        if non_null.len() == 1 && non_null.len() < types.len() {
            // This is T | null or T | undefined -> Option<T>
            let inner = self.map_type(non_null[0]);
            return format!("Option<{}>", inner);
        }

        // Check for boolean union
        if types.len() == 2 {
            let has_true = types.iter().any(|t| matches!(t, DtsType::BooleanLiteral(true)));
            let has_false = types.iter().any(|t| matches!(t, DtsType::BooleanLiteral(false)));
            if has_true && has_false {
                return "bool".to_string();
            }
        }

        // General union - if all are string literals, use String
        if types.iter().all(|t| matches!(t, DtsType::StringLiteral(_))) {
            self.warn(WarningKind::Simplified, "String literal union mapped to String");
            return "String".to_string();
        }

        // If all are number literals, use f64
        if types.iter().all(|t| matches!(t, DtsType::NumberLiteral(_))) {
            self.warn(WarningKind::Simplified, "Number literal union mapped to f64");
            return "f64".to_string();
        }

        // Complex union - fallback to JsValue
        self.warn(WarningKind::Simplified, "Complex union type mapped to JsValue");
        "JsValue".to_string()
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
        writeln!(self.output, "// Some types may be simplified - see warnings below").unwrap();
        writeln!(self.output).unwrap();
    }

    fn emit_extern_block(&mut self) {
        writeln!(self.output, "extern \"js\" {{").unwrap();

        // Module import
        if let Some(mod_name) = &self.options.module_name {
            if is_valid_identifier(mod_name) {
                writeln!(self.output, "    mod {};", mod_name).unwrap();
            } else {
                let alias = derive_binding_from_package(mod_name);
                writeln!(self.output, "    mod \"{}\" as {};", mod_name, alias).unwrap();
            }
            writeln!(self.output).unwrap();
        }

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

        writeln!(self.output, "}}").unwrap();
    }

    fn emit_impl_blocks(&mut self) {
        let mut sorted_impls: Vec<(&String, &Vec<GeneratedMethod>)> = self.impls.iter().collect();
        sorted_impls.sort_by_key(|(k, _)| *k);

        for (type_name, methods) in sorted_impls {
            if methods.is_empty() {
                continue;
            }

            writeln!(self.output).unwrap();
            writeln!(self.output, "impl {} {{", type_name).unwrap();

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
fn is_keyword(name: &str) -> bool {
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
fn escape_keyword(name: &str) -> String {
    if is_keyword(name) {
        format!("{}_", name)
    } else {
        name.to_string()
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
        assert!(result.code.contains("extern \"js\" fn listen(self, port: f64);"));
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

        assert!(result.code.contains("fn setTimeout(callback: fn() -> (), ms: f64) -> f64;"));
    }

    #[test]
    fn test_generate_nullable_type() {
        let src = "declare function find(id: string): User | null;";
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
        let src = "declare function fetch(url: string): Promise<Response>;";
        let file = parse(src).unwrap();
        let result = generate(&file, &CodegenOptions::default());

        assert!(result.code.contains("fn fetch(url: String) -> JsPromise<Response>;"));
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
}
