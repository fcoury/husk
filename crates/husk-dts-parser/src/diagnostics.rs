//! Diagnostic report generator for DTS parsing.
//!
//! Generates a markdown report (`dts-report.md`) that documents:
//! - Successfully converted types (summarized in metrics tables)
//! - Types that required special handling (unions, utility types, generics)
//! - Types that couldn't be converted (blocked features)
//! - Warnings and suggestions
//!
//! The "Details by Category" section lists only special handling cases and issues,
//! while successful conversions are summarized via the metrics tables to avoid noise.

use std::collections::HashMap;
use std::fmt::Write;

use crate::ast::{ClassMember, DtsFile, DtsItem, DtsType};
use crate::builder::DEFAULT_BUILDER_OPTIONAL_THRESHOLD;
use crate::unions::{UnionStrategy, analyze_union};

/// Severity level for diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    /// Informational message.
    Info,
    /// Warning - type was converted with potential issues.
    Warning,
    /// Error - type could not be converted.
    Error,
}

impl Severity {
    fn emoji(&self) -> &'static str {
        match self {
            Severity::Info => "ℹ️",
            Severity::Warning => "⚠️",
            Severity::Error => "❌",
        }
    }

    #[allow(dead_code)]
    fn label(&self) -> &'static str {
        match self {
            Severity::Info => "Info",
            Severity::Warning => "Warning",
            Severity::Error => "Error",
        }
    }
}

/// A single diagnostic entry.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// Severity level.
    pub severity: Severity,
    /// The type or declaration name.
    pub name: String,
    /// Category of the diagnostic.
    pub category: DiagnosticCategory,
    /// Human-readable message.
    pub message: String,
    /// Suggested fix or workaround.
    pub suggestion: Option<String>,
}

/// Categories for diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticCategory {
    /// Type successfully converted.
    Success,
    /// Union type handling.
    UnionType,
    /// Utility type expansion.
    UtilityType,
    /// Generic type handling.
    GenericType,
    /// Blocked feature (requires language enhancement).
    BlockedFeature,
    /// Import resolution.
    ImportResolution,
    /// Unsupported syntax.
    UnsupportedSyntax,
    /// Builder pattern generation.
    BuilderGeneration,
}

impl DiagnosticCategory {
    fn label(&self) -> &'static str {
        match self {
            DiagnosticCategory::Success => "Success",
            DiagnosticCategory::UnionType => "Union Types",
            DiagnosticCategory::UtilityType => "Utility Types",
            DiagnosticCategory::GenericType => "Generic Types",
            DiagnosticCategory::BlockedFeature => "Blocked Features",
            DiagnosticCategory::ImportResolution => "Import Resolution",
            DiagnosticCategory::UnsupportedSyntax => "Unsupported Syntax",
            DiagnosticCategory::BuilderGeneration => "Builder Generation",
        }
    }
}

/// Collector for diagnostics during DTS processing.
#[derive(Debug, Default)]
pub struct DiagnosticCollector {
    diagnostics: Vec<Diagnostic>,
    /// Count of each type of conversion.
    stats: ConversionStats,
}

/// Statistics about the conversion.
#[derive(Debug, Default)]
pub struct ConversionStats {
    pub total_items: usize,
    pub interfaces: usize,
    pub type_aliases: usize,
    pub functions: usize,
    pub classes: usize,
    pub variables: usize,
    pub namespaces: usize,
    pub modules: usize,
    pub unions_to_option: usize,
    pub unions_to_enum: usize,
    pub unions_to_jsvalue: usize,
    pub utility_types_expanded: usize,
    pub builders_generated: usize,
    pub blocked_features: usize,
}

impl DiagnosticCollector {
    /// Create a new collector.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a diagnostic.
    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Add an info diagnostic.
    pub fn info(&mut self, name: &str, category: DiagnosticCategory, message: &str) {
        self.add(Diagnostic {
            severity: Severity::Info,
            name: name.to_string(),
            category,
            message: message.to_string(),
            suggestion: None,
        });
    }

    /// Add a warning diagnostic.
    pub fn warning(
        &mut self,
        name: &str,
        category: DiagnosticCategory,
        message: &str,
        suggestion: Option<&str>,
    ) {
        self.add(Diagnostic {
            severity: Severity::Warning,
            name: name.to_string(),
            category,
            message: message.to_string(),
            suggestion: suggestion.map(String::from),
        });
    }

    /// Add an error diagnostic.
    pub fn error(
        &mut self,
        name: &str,
        category: DiagnosticCategory,
        message: &str,
        suggestion: Option<&str>,
    ) {
        self.add(Diagnostic {
            severity: Severity::Error,
            name: name.to_string(),
            category,
            message: message.to_string(),
            suggestion: suggestion.map(String::from),
        });
    }

    /// Process a DtsFile and collect diagnostics.
    pub fn process_file(&mut self, file: &DtsFile, filename: &str) {
        self.info(
            filename,
            DiagnosticCategory::Success,
            "File parsed successfully",
        );

        for item in &file.items {
            self.process_item(item);
        }
    }

    /// Process a single item.
    fn process_item(&mut self, item: &DtsItem) {
        self.stats.total_items += 1;

        match item {
            DtsItem::Interface(iface) => {
                self.stats.interfaces += 1;
                self.info(
                    &iface.name,
                    DiagnosticCategory::Success,
                    &format!("Interface with {} members", iface.members.len()),
                );

                // Check for potential builder generation
                let optional_count = iface
                    .members
                    .iter()
                    .filter(|m| matches!(m, crate::ast::InterfaceMember::Property(p) if p.optional))
                    .count();
                if optional_count >= DEFAULT_BUILDER_OPTIONAL_THRESHOLD {
                    self.info(
                        &iface.name,
                        DiagnosticCategory::BuilderGeneration,
                        &format!(
                            "Interface has {} optional properties - builder pattern recommended",
                            optional_count
                        ),
                    );
                    self.stats.builders_generated += 1;
                }

                // Check member types for unions
                for member in &iface.members {
                    if let crate::ast::InterfaceMember::Property(prop) = member {
                        self.analyze_type(&prop.ty, &format!("{}.{}", iface.name, prop.name));
                    }
                }
            }

            DtsItem::TypeAlias(alias) => {
                self.stats.type_aliases += 1;
                self.analyze_type(&alias.ty, &alias.name);

                // Check for utility types (diagnostic only, counting is done in analyze_type)
                if let DtsType::Named { name, .. } = &alias.ty {
                    if is_utility_type(name) {
                        self.info(
                            &alias.name,
                            DiagnosticCategory::UtilityType,
                            &format!("Utility type {} will be expanded", name),
                        );
                    }
                }
            }

            DtsItem::Function(func) => {
                self.stats.functions += 1;
                self.info(
                    &func.name,
                    DiagnosticCategory::Success,
                    &format!("Function with {} parameters", func.params.len()),
                );

                // Check parameter types
                for param in &func.params {
                    self.analyze_type(&param.ty, &format!("{}::{}", func.name, param.name));
                }

                // Check return type
                if let Some(ref ret_type) = func.return_type {
                    self.analyze_type(ret_type, &format!("{}::return", func.name));
                }
            }

            DtsItem::Class(class) => {
                self.stats.classes += 1;
                self.info(
                    &class.name,
                    DiagnosticCategory::Success,
                    &format!("Class with {} members", class.members.len()),
                );

                // Check extends clause
                if let Some(ref extends) = class.extends {
                    self.analyze_type(extends, &format!("{}::extends", class.name));
                }

                // Check implements clauses
                for (i, impl_ty) in class.implements.iter().enumerate() {
                    self.analyze_type(impl_ty, &format!("{}::implements[{}]", class.name, i));
                }

                // Check member types
                for member in &class.members {
                    match member {
                        ClassMember::Property(prop) => {
                            if let Some(ref ty) = prop.ty {
                                self.analyze_type(ty, &format!("{}::{}", class.name, prop.name));
                            }
                        }
                        ClassMember::Method(method) => {
                            // Check parameter types
                            for param in &method.params {
                                self.analyze_type(
                                    &param.ty,
                                    &format!("{}::{}::{}", class.name, method.name, param.name),
                                );
                            }
                            // Check return type
                            if let Some(ref ret_type) = method.return_type {
                                self.analyze_type(
                                    ret_type,
                                    &format!("{}::{}::return", class.name, method.name),
                                );
                            }
                        }
                        ClassMember::Constructor(ctor) => {
                            // Check constructor parameter types
                            for param in &ctor.params {
                                self.analyze_type(
                                    &param.ty,
                                    &format!("{}::constructor::{}", class.name, param.name),
                                );
                            }
                            // Check return type (unusual for constructors but possible)
                            if let Some(ref ret_type) = ctor.return_type {
                                self.analyze_type(
                                    ret_type,
                                    &format!("{}::constructor::return", class.name),
                                );
                            }
                        }
                        ClassMember::IndexSignature(idx) => {
                            self.analyze_type(
                                &idx.key_type,
                                &format!("{}::[index]::key", class.name),
                            );
                            self.analyze_type(
                                &idx.value_type,
                                &format!("{}::[index]::value", class.name),
                            );
                        }
                    }
                }
            }

            DtsItem::Variable(var) => {
                self.stats.variables += 1;
                self.analyze_type(&var.ty, &var.name);
            }

            DtsItem::Namespace(ns) => {
                self.stats.namespaces += 1;
                for item in &ns.items {
                    self.process_item(item);
                }
            }

            DtsItem::Module(module) => {
                self.stats.modules += 1;
                if module.is_ambient {
                    self.info(
                        &module.name,
                        DiagnosticCategory::ImportResolution,
                        "Ambient module declaration",
                    );
                }
                for item in &module.items {
                    self.process_item(item);
                }
            }

            DtsItem::Export(_) => {
                // Exports are handled separately
            }
        }
    }

    /// Analyze a type for potential issues.
    fn analyze_type(&mut self, ty: &DtsType, context: &str) {
        match ty {
            DtsType::Union(types) => {
                let strategy = analyze_union(types);
                match strategy {
                    UnionStrategy::Nullable { .. } => {
                        self.stats.unions_to_option += 1;
                        self.info(
                            context,
                            DiagnosticCategory::UnionType,
                            "Nullable union → Option<T>",
                        );
                    }
                    UnionStrategy::StringEnum { variants } => {
                        self.stats.unions_to_enum += 1;
                        self.info(
                            context,
                            DiagnosticCategory::UnionType,
                            &format!(
                                "String literal union → enum with {} variants",
                                variants.len()
                            ),
                        );
                    }
                    UnionStrategy::NumberEnum { variants } => {
                        self.stats.unions_to_enum += 1;
                        self.info(
                            context,
                            DiagnosticCategory::UnionType,
                            &format!(
                                "Number literal union → enum with {} variants",
                                variants.len()
                            ),
                        );
                    }
                    UnionStrategy::Boolean => {
                        self.info(
                            context,
                            DiagnosticCategory::UnionType,
                            "Boolean literal union → bool",
                        );
                    }
                    UnionStrategy::Discriminated {
                        discriminant,
                        variants,
                    } => {
                        self.stats.unions_to_enum += 1;
                        self.info(
                            context,
                            DiagnosticCategory::UnionType,
                            &format!(
                                "Discriminated union (tag: {}) → enum with {} variants",
                                discriminant,
                                variants.len()
                            ),
                        );
                    }
                    UnionStrategy::JsValue => {
                        self.stats.unions_to_jsvalue += 1;
                        self.warning(
                            context,
                            DiagnosticCategory::UnionType,
                            "Mixed union → JsValue (type safety reduced)",
                            Some("Consider using more specific types or overloads"),
                        );
                    }
                    UnionStrategy::Overloaded { signatures } => {
                        self.info(
                            context,
                            DiagnosticCategory::UnionType,
                            &format!("Function union → {} overloads", signatures.len()),
                        );
                    }
                    UnionStrategy::TypeEnum { variants } => {
                        self.stats.unions_to_enum += 1;
                        self.info(
                            context,
                            DiagnosticCategory::UnionType,
                            &format!("Type union → enum with {} variants", variants.len()),
                        );
                    }
                    UnionStrategy::Passthrough(_) => {
                        self.warning(
                            context,
                            DiagnosticCategory::UnionType,
                            "Complex union kept as-is",
                            Some("May require manual type definition"),
                        );
                    }
                }
            }

            DtsType::Named { name, type_args } => {
                if is_utility_type(name) {
                    self.stats.utility_types_expanded += 1;
                }

                // Check for generic trait bounds usage (keyof pattern in type args)
                // If detected, report it here and skip recursing into type_args to avoid
                // duplicate diagnostics from the KeyOf branch
                if !type_args.is_empty() && has_keyof_pattern(ty) {
                    self.stats.blocked_features += 1;
                    self.error(
                        context,
                        DiagnosticCategory::BlockedFeature,
                        "keyof pattern requires generic trait bounds",
                        Some("Blocked on Husk language enhancement for generic trait bound resolution"),
                    );
                    // Short-circuit: don't recurse into type_args since we've already
                    // reported the keyof issue at this level
                } else {
                    // Recursively check type args (no keyof pattern detected)
                    for arg in type_args {
                        self.analyze_type(arg, context);
                    }
                }
            }

            DtsType::KeyOf(_) => {
                self.stats.blocked_features += 1;
                self.warning(
                    context,
                    DiagnosticCategory::BlockedFeature,
                    "keyof type operator requires generic trait bounds",
                    Some("Will be converted to string until language support is added"),
                );
            }

            DtsType::Conditional { .. } => {
                self.warning(
                    context,
                    DiagnosticCategory::UnsupportedSyntax,
                    "Conditional type may lose precision",
                    Some("Consider simplifying or using explicit type definitions"),
                );
            }

            DtsType::Mapped { .. } => {
                self.warning(
                    context,
                    DiagnosticCategory::UnsupportedSyntax,
                    "Mapped type will be expanded inline",
                    Some("May result in verbose generated code"),
                );
            }

            DtsType::Array(inner) => {
                self.analyze_type(inner, context);
            }

            DtsType::Tuple(elements) => {
                for elem in elements {
                    self.analyze_type(&elem.ty, context);
                }
            }

            DtsType::Function(func) => {
                for param in &func.params {
                    self.analyze_type(&param.ty, context);
                }
                self.analyze_type(&func.return_type, context);
            }

            DtsType::Object(members) => {
                for member in members {
                    if let crate::ast::ObjectMember::Property { ty, .. } = member {
                        self.analyze_type(ty, context);
                    }
                }
            }

            _ => {}
        }
    }

    /// Get statistics.
    pub fn stats(&self) -> &ConversionStats {
        &self.stats
    }

    /// Generate the markdown report.
    pub fn generate_report(&self, title: &str) -> String {
        let mut report = String::new();

        // Header
        writeln!(report, "# {}", title).unwrap();
        writeln!(report).unwrap();
        writeln!(report, "Generated by husk-dts-parser").unwrap();
        writeln!(report).unwrap();

        // Summary
        writeln!(report, "## Summary").unwrap();
        writeln!(report).unwrap();
        writeln!(report, "| Metric | Count |").unwrap();
        writeln!(report, "|--------|-------|").unwrap();
        writeln!(report, "| Total Items | {} |", self.stats.total_items).unwrap();
        writeln!(report, "| Interfaces | {} |", self.stats.interfaces).unwrap();
        writeln!(report, "| Type Aliases | {} |", self.stats.type_aliases).unwrap();
        writeln!(report, "| Functions | {} |", self.stats.functions).unwrap();
        writeln!(report, "| Classes | {} |", self.stats.classes).unwrap();
        writeln!(report, "| Variables | {} |", self.stats.variables).unwrap();
        writeln!(report, "| Namespaces | {} |", self.stats.namespaces).unwrap();
        writeln!(report, "| Modules | {} |", self.stats.modules).unwrap();
        writeln!(report).unwrap();

        // Union handling
        writeln!(report, "## Union Type Handling").unwrap();
        writeln!(report).unwrap();
        writeln!(report, "| Strategy | Count |").unwrap();
        writeln!(report, "|----------|-------|").unwrap();
        writeln!(
            report,
            "| Converted to Option | {} |",
            self.stats.unions_to_option
        )
        .unwrap();
        writeln!(
            report,
            "| Converted to Enum | {} |",
            self.stats.unions_to_enum
        )
        .unwrap();
        writeln!(
            report,
            "| Fallback to JsValue | {} |",
            self.stats.unions_to_jsvalue
        )
        .unwrap();
        writeln!(report).unwrap();

        // Other stats
        writeln!(report, "## Other Metrics").unwrap();
        writeln!(report).unwrap();
        writeln!(
            report,
            "- Utility types expanded: {}",
            self.stats.utility_types_expanded
        )
        .unwrap();
        writeln!(
            report,
            "- Builders generated: {}",
            self.stats.builders_generated
        )
        .unwrap();
        writeln!(
            report,
            "- Blocked features: {}",
            self.stats.blocked_features
        )
        .unwrap();
        writeln!(report).unwrap();

        // Diagnostics by category
        let mut by_category: HashMap<DiagnosticCategory, Vec<&Diagnostic>> = HashMap::new();
        for diag in &self.diagnostics {
            by_category.entry(diag.category).or_default().push(diag);
        }

        // Errors first
        let errors: Vec<_> = self
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();

        if !errors.is_empty() {
            writeln!(report, "## Errors").unwrap();
            writeln!(report).unwrap();
            for diag in errors {
                writeln!(
                    report,
                    "- {} **{}**: {}",
                    diag.severity.emoji(),
                    diag.name,
                    diag.message
                )
                .unwrap();
                if let Some(ref suggestion) = diag.suggestion {
                    writeln!(report, "  - *Suggestion*: {}", suggestion).unwrap();
                }
            }
            writeln!(report).unwrap();
        }

        // Warnings
        let warnings: Vec<_> = self
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .collect();

        if !warnings.is_empty() {
            writeln!(report, "## Warnings").unwrap();
            writeln!(report).unwrap();
            for diag in warnings {
                writeln!(
                    report,
                    "- {} **{}**: {}",
                    diag.severity.emoji(),
                    diag.name,
                    diag.message
                )
                .unwrap();
                if let Some(ref suggestion) = diag.suggestion {
                    writeln!(report, "  - *Suggestion*: {}", suggestion).unwrap();
                }
            }
            writeln!(report).unwrap();
        }

        // Detailed breakdown by category
        writeln!(report, "## Details by Category").unwrap();
        writeln!(report).unwrap();

        for category in [
            DiagnosticCategory::UnionType,
            DiagnosticCategory::UtilityType,
            DiagnosticCategory::GenericType,
            DiagnosticCategory::BlockedFeature,
            DiagnosticCategory::BuilderGeneration,
            DiagnosticCategory::ImportResolution,
            DiagnosticCategory::UnsupportedSyntax,
        ] {
            if let Some(diags) = by_category.get(&category) {
                writeln!(report, "### {}", category.label()).unwrap();
                writeln!(report).unwrap();
                for diag in diags.iter().take(20) {
                    // Limit to 20 per category
                    writeln!(
                        report,
                        "- {} **{}**: {}",
                        diag.severity.emoji(),
                        diag.name,
                        diag.message
                    )
                    .unwrap();
                }
                if diags.len() > 20 {
                    writeln!(report, "- ... and {} more", diags.len() - 20).unwrap();
                }
                writeln!(report).unwrap();
            }
        }

        report
    }
}

/// Check if a type name is a utility type.
fn is_utility_type(name: &str) -> bool {
    matches!(
        name,
        "Partial"
            | "Required"
            | "Readonly"
            | "Pick"
            | "Omit"
            | "Record"
            | "Exclude"
            | "Extract"
            | "NonNullable"
            | "ReturnType"
            | "Parameters"
            | "ConstructorParameters"
            | "InstanceType"
            | "ThisParameterType"
            | "OmitThisParameter"
            | "Awaited"
    )
}

/// Check if a type uses keyof pattern (which is blocked).
fn has_keyof_pattern(ty: &DtsType) -> bool {
    match ty {
        DtsType::KeyOf(_) => true,
        DtsType::Named { type_args, .. } => type_args.iter().any(has_keyof_pattern),
        DtsType::Union(types) | DtsType::Intersection(types) => types.iter().any(has_keyof_pattern),
        DtsType::Array(inner) => has_keyof_pattern(inner),
        DtsType::IndexAccess { object, index } => {
            has_keyof_pattern(object) || has_keyof_pattern(index)
        }
        _ => false,
    }
}

// =============================================================================
// CodegenMetrics - Actual results from code generation
// =============================================================================

/// Metrics collected during actual code generation.
///
/// Unlike `ConversionStats` which analyzes potential conversions,
/// `CodegenMetrics` tracks what actually happened during codegen.
#[derive(Debug, Clone, Default)]
pub struct CodegenMetrics {
    /// Total number of types processed.
    pub types_total: usize,
    /// Types successfully mapped to Husk types.
    pub types_mapped: usize,
    /// Types that degraded to JsValue/Any.
    pub types_degraded: usize,

    // Union strategy breakdown
    /// Unions converted to Option<T>.
    pub unions_as_option: usize,
    /// Unions converted to String (string literal unions).
    pub unions_as_string: usize,
    /// Unions converted to f64 (number literal unions).
    pub unions_as_number: usize,
    /// Unions converted to bool.
    pub unions_as_bool: usize,
    /// Unions that fell back to JsValue.
    pub unions_as_jsvalue: usize,

    // Utility type handling
    /// Utility types successfully expanded.
    pub utility_types_expanded: usize,
    /// Utility types that failed to expand.
    pub utility_types_failed: usize,

    // Builder generation
    /// Builders generated for interfaces.
    pub builders_generated: usize,
    /// Interfaces that skipped builder generation.
    pub builders_skipped: usize,

    // Advanced type handling
    /// keyof types resolved.
    pub keyof_resolved: usize,
    /// keyof types that fell back.
    pub keyof_fallback: usize,
    /// Index access types resolved.
    pub index_access_resolved: usize,
    /// Index access types that fell back.
    pub index_access_fallback: usize,

    // Const handling
    /// Extern consts generated (new behavior).
    pub extern_consts: usize,
    /// Legacy const functions generated (old behavior).
    pub legacy_const_functions: usize,

    /// Detailed warnings from codegen.
    pub warnings: Vec<CodegenWarning>,
}

/// A warning from code generation with context.
#[derive(Debug, Clone)]
pub struct CodegenWarning {
    /// The item name (function, interface, etc.).
    pub item: String,
    /// Warning message.
    pub message: String,
    /// Optional suggestion for fixing.
    pub suggestion: Option<String>,
}

impl CodegenMetrics {
    /// Create a new empty metrics collector.
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a type being mapped.
    pub fn record_type_mapped(&mut self) {
        self.types_total += 1;
        self.types_mapped += 1;
    }

    /// Record a type degrading to JsValue.
    pub fn record_type_degraded(&mut self) {
        self.types_total += 1;
        self.types_degraded += 1;
    }

    /// Record a union converted to Option.
    pub fn record_union_option(&mut self) {
        self.unions_as_option += 1;
    }

    /// Record a union converted to String.
    pub fn record_union_string(&mut self) {
        self.unions_as_string += 1;
    }

    /// Record a union converted to f64.
    pub fn record_union_number(&mut self) {
        self.unions_as_number += 1;
    }

    /// Record a union converted to bool.
    pub fn record_union_bool(&mut self) {
        self.unions_as_bool += 1;
    }

    /// Record a union falling back to JsValue.
    pub fn record_union_jsvalue(&mut self) {
        self.unions_as_jsvalue += 1;
    }

    /// Record a utility type being expanded.
    pub fn record_utility_expanded(&mut self) {
        self.utility_types_expanded += 1;
    }

    /// Record a utility type expansion failure.
    pub fn record_utility_failed(&mut self) {
        self.utility_types_failed += 1;
    }

    /// Record a builder being generated.
    pub fn record_builder_generated(&mut self) {
        self.builders_generated += 1;
    }

    /// Record a builder being skipped.
    pub fn record_builder_skipped(&mut self) {
        self.builders_skipped += 1;
    }

    /// Record a keyof type being resolved.
    pub fn record_keyof_resolved(&mut self) {
        self.keyof_resolved += 1;
    }

    /// Record a keyof type falling back.
    pub fn record_keyof_fallback(&mut self) {
        self.keyof_fallback += 1;
    }

    /// Record an index access type being resolved.
    pub fn record_index_access_resolved(&mut self) {
        self.index_access_resolved += 1;
    }

    /// Record an index access type falling back.
    pub fn record_index_access_fallback(&mut self) {
        self.index_access_fallback += 1;
    }

    /// Add a warning.
    pub fn add_warning(&mut self, item: &str, message: &str, suggestion: Option<&str>) {
        self.warnings.push(CodegenWarning {
            item: item.to_string(),
            message: message.to_string(),
            suggestion: suggestion.map(String::from),
        });
    }

    /// Aggregate another metrics instance into this one.
    pub fn aggregate(&mut self, other: &CodegenMetrics) {
        self.types_total += other.types_total;
        self.types_mapped += other.types_mapped;
        self.types_degraded += other.types_degraded;

        self.unions_as_option += other.unions_as_option;
        self.unions_as_string += other.unions_as_string;
        self.unions_as_number += other.unions_as_number;
        self.unions_as_bool += other.unions_as_bool;
        self.unions_as_jsvalue += other.unions_as_jsvalue;

        self.utility_types_expanded += other.utility_types_expanded;
        self.utility_types_failed += other.utility_types_failed;

        self.builders_generated += other.builders_generated;
        self.builders_skipped += other.builders_skipped;

        self.keyof_resolved += other.keyof_resolved;
        self.keyof_fallback += other.keyof_fallback;
        self.index_access_resolved += other.index_access_resolved;
        self.index_access_fallback += other.index_access_fallback;

        self.extern_consts += other.extern_consts;
        self.legacy_const_functions += other.legacy_const_functions;

        // Note: warnings are not aggregated to avoid duplication
    }

    /// Calculate the percentage of types successfully mapped.
    pub fn mapped_percentage(&self) -> f64 {
        if self.types_total == 0 {
            100.0
        } else {
            (self.types_mapped as f64 / self.types_total as f64) * 100.0
        }
    }

    /// Calculate the percentage of types that degraded.
    pub fn degraded_percentage(&self) -> f64 {
        if self.types_total == 0 {
            0.0
        } else {
            (self.types_degraded as f64 / self.types_total as f64) * 100.0
        }
    }

    /// Total number of unions processed.
    pub fn unions_total(&self) -> usize {
        self.unions_as_option
            + self.unions_as_string
            + self.unions_as_number
            + self.unions_as_bool
            + self.unions_as_jsvalue
    }

    /// Generate a markdown report from actual codegen results.
    pub fn to_markdown(&self, title: &str) -> String {
        let mut report = String::new();

        writeln!(report, "# {}", title).unwrap();
        writeln!(report).unwrap();
        writeln!(
            report,
            "Generated by husk-dts-parser (actual codegen results)"
        )
        .unwrap();
        writeln!(report).unwrap();

        // Summary
        writeln!(report, "## Summary").unwrap();
        writeln!(report).unwrap();
        writeln!(
            report,
            "- **Types mapped**: {}/{} ({:.1}%)",
            self.types_mapped,
            self.types_total,
            self.mapped_percentage()
        )
        .unwrap();
        writeln!(
            report,
            "- **Types degraded**: {} ({:.1}%)",
            self.types_degraded,
            self.degraded_percentage()
        )
        .unwrap();
        writeln!(report).unwrap();

        // Union handling
        if self.unions_total() > 0 {
            writeln!(report, "## Union Type Handling").unwrap();
            writeln!(report).unwrap();
            writeln!(report, "| Strategy | Count |").unwrap();
            writeln!(report, "|----------|-------|").unwrap();
            writeln!(report, "| Option<T> | {} |", self.unions_as_option).unwrap();
            writeln!(report, "| String (literals) | {} |", self.unions_as_string).unwrap();
            writeln!(
                report,
                "| f64 (number literals) | {} |",
                self.unions_as_number
            )
            .unwrap();
            writeln!(report, "| bool | {} |", self.unions_as_bool).unwrap();
            writeln!(
                report,
                "| JsValue (fallback) | {} |",
                self.unions_as_jsvalue
            )
            .unwrap();
            writeln!(report).unwrap();
        }

        // Utility types
        if self.utility_types_expanded > 0 || self.utility_types_failed > 0 {
            writeln!(report, "## Utility Types").unwrap();
            writeln!(report).unwrap();
            writeln!(report, "- Expanded: {}", self.utility_types_expanded).unwrap();
            writeln!(report, "- Failed: {}", self.utility_types_failed).unwrap();
            writeln!(report).unwrap();
        }

        // Builders
        if self.builders_generated > 0 || self.builders_skipped > 0 {
            writeln!(report, "## Builder Generation").unwrap();
            writeln!(report).unwrap();
            writeln!(report, "- Generated: {}", self.builders_generated).unwrap();
            writeln!(report, "- Skipped: {}", self.builders_skipped).unwrap();
            writeln!(report).unwrap();
        }

        // Advanced type handling
        if self.keyof_resolved > 0
            || self.keyof_fallback > 0
            || self.index_access_resolved > 0
            || self.index_access_fallback > 0
        {
            writeln!(report, "## Advanced Type Handling").unwrap();
            writeln!(report).unwrap();
            writeln!(report, "| Type | Resolved | Fallback |").unwrap();
            writeln!(report, "|------|----------|----------|").unwrap();
            writeln!(
                report,
                "| keyof | {} | {} |",
                self.keyof_resolved, self.keyof_fallback
            )
            .unwrap();
            writeln!(
                report,
                "| Index Access | {} | {} |",
                self.index_access_resolved, self.index_access_fallback
            )
            .unwrap();
            writeln!(report).unwrap();
        }

        // Const handling
        if self.extern_consts > 0 || self.legacy_const_functions > 0 {
            writeln!(report, "## Constant Handling").unwrap();
            writeln!(report).unwrap();
            writeln!(report, "- Extern consts: {}", self.extern_consts).unwrap();
            writeln!(
                report,
                "- Legacy const functions: {}",
                self.legacy_const_functions
            )
            .unwrap();
            writeln!(report).unwrap();
        }

        // Warnings
        if !self.warnings.is_empty() {
            writeln!(report, "## Warnings").unwrap();
            writeln!(report).unwrap();
            for (i, warning) in self.warnings.iter().take(50).enumerate() {
                writeln!(
                    report,
                    "{}. **{}**: {}",
                    i + 1,
                    warning.item,
                    warning.message
                )
                .unwrap();
                if let Some(ref suggestion) = warning.suggestion {
                    writeln!(report, "   - *Suggestion*: {}", suggestion).unwrap();
                }
            }
            if self.warnings.len() > 50 {
                writeln!(
                    report,
                    "\n... and {} more warnings",
                    self.warnings.len() - 50
                )
                .unwrap();
            }
            writeln!(report).unwrap();
        }

        report
    }

    /// Generate a JSON report from actual codegen results.
    pub fn to_json(&self) -> String {
        // Manual JSON generation to avoid adding serde dependency
        let mut json = String::new();
        writeln!(json, "{{").unwrap();
        writeln!(json, "  \"summary\": {{").unwrap();
        writeln!(json, "    \"types_total\": {},", self.types_total).unwrap();
        writeln!(json, "    \"types_mapped\": {},", self.types_mapped).unwrap();
        writeln!(json, "    \"types_degraded\": {},", self.types_degraded).unwrap();
        writeln!(
            json,
            "    \"mapped_percentage\": {:.1}",
            self.mapped_percentage()
        )
        .unwrap();
        writeln!(json, "  }},").unwrap();

        writeln!(json, "  \"unions\": {{").unwrap();
        writeln!(json, "    \"total\": {},", self.unions_total()).unwrap();
        writeln!(json, "    \"as_option\": {},", self.unions_as_option).unwrap();
        writeln!(json, "    \"as_string\": {},", self.unions_as_string).unwrap();
        writeln!(json, "    \"as_number\": {},", self.unions_as_number).unwrap();
        writeln!(json, "    \"as_bool\": {},", self.unions_as_bool).unwrap();
        writeln!(json, "    \"as_jsvalue\": {}", self.unions_as_jsvalue).unwrap();
        writeln!(json, "  }},").unwrap();

        writeln!(json, "  \"utility_types\": {{").unwrap();
        writeln!(json, "    \"expanded\": {},", self.utility_types_expanded).unwrap();
        writeln!(json, "    \"failed\": {}", self.utility_types_failed).unwrap();
        writeln!(json, "  }},").unwrap();

        writeln!(json, "  \"builders\": {{").unwrap();
        writeln!(json, "    \"generated\": {},", self.builders_generated).unwrap();
        writeln!(json, "    \"skipped\": {}", self.builders_skipped).unwrap();
        writeln!(json, "  }},").unwrap();

        writeln!(json, "  \"advanced_types\": {{").unwrap();
        writeln!(json, "    \"keyof_resolved\": {},", self.keyof_resolved).unwrap();
        writeln!(json, "    \"keyof_fallback\": {},", self.keyof_fallback).unwrap();
        writeln!(
            json,
            "    \"index_access_resolved\": {},",
            self.index_access_resolved
        )
        .unwrap();
        writeln!(
            json,
            "    \"index_access_fallback\": {}",
            self.index_access_fallback
        )
        .unwrap();
        writeln!(json, "  }},").unwrap();

        writeln!(json, "  \"constants\": {{").unwrap();
        writeln!(json, "    \"extern_consts\": {},", self.extern_consts).unwrap();
        writeln!(
            json,
            "    \"legacy_const_functions\": {}",
            self.legacy_const_functions
        )
        .unwrap();
        writeln!(json, "  }},").unwrap();

        writeln!(json, "  \"warnings_count\": {}", self.warnings.len()).unwrap();
        writeln!(json, "}}").unwrap();

        json
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{DtsFunction, Param, Primitive};

    #[test]
    fn test_collector_basic() {
        let mut collector = DiagnosticCollector::new();

        let file = DtsFile {
            items: vec![DtsItem::Function(DtsFunction {
                name: "add".to_string(),
                type_params: vec![],
                params: vec![
                    Param {
                        name: "a".to_string(),
                        ty: DtsType::Primitive(Primitive::Number),
                        optional: false,
                        rest: false,
                    },
                    Param {
                        name: "b".to_string(),
                        ty: DtsType::Primitive(Primitive::Number),
                        optional: false,
                        rest: false,
                    },
                ],
                return_type: Some(DtsType::Primitive(Primitive::Number)),
                this_param: None,
            })],
        };

        collector.process_file(&file, "test.d.ts");

        assert_eq!(collector.stats().functions, 1);
        assert_eq!(collector.stats().total_items, 1);
    }

    #[test]
    fn test_report_generation() {
        let mut collector = DiagnosticCollector::new();
        // Use a warning since info diagnostics with Success category aren't rendered
        collector.warning(
            "TestType",
            DiagnosticCategory::UnionType,
            "Test warning message",
            None,
        );
        collector.warning(
            "WarnType",
            DiagnosticCategory::UnionType,
            "Warning message",
            Some("Fix this"),
        );

        let report = collector.generate_report("Test Report");

        assert!(report.contains("# Test Report"));
        assert!(report.contains("TestType"));
        assert!(report.contains("WarnType"));
        assert!(report.contains("Fix this"));
    }

    #[test]
    fn test_codegen_metrics_basic() {
        let mut metrics = CodegenMetrics::new();

        // Record some types
        metrics.record_type_mapped();
        metrics.record_type_mapped();
        metrics.record_type_degraded();

        assert_eq!(metrics.types_total, 3);
        assert_eq!(metrics.types_mapped, 2);
        assert_eq!(metrics.types_degraded, 1);
        assert!((metrics.mapped_percentage() - 66.6).abs() < 1.0);
    }

    #[test]
    fn test_codegen_metrics_unions() {
        let mut metrics = CodegenMetrics::new();

        metrics.record_union_option();
        metrics.record_union_option();
        metrics.record_union_string();
        metrics.record_union_jsvalue();

        assert_eq!(metrics.unions_total(), 4);
        assert_eq!(metrics.unions_as_option, 2);
        assert_eq!(metrics.unions_as_string, 1);
        assert_eq!(metrics.unions_as_jsvalue, 1);
    }

    #[test]
    fn test_codegen_metrics_aggregate() {
        let mut metrics1 = CodegenMetrics::new();
        metrics1.record_type_mapped();
        metrics1.record_union_option();

        let mut metrics2 = CodegenMetrics::new();
        metrics2.record_type_mapped();
        metrics2.record_type_degraded();
        metrics2.record_union_jsvalue();

        metrics1.aggregate(&metrics2);

        assert_eq!(metrics1.types_total, 3);
        assert_eq!(metrics1.types_mapped, 2);
        assert_eq!(metrics1.types_degraded, 1);
        assert_eq!(metrics1.unions_as_option, 1);
        assert_eq!(metrics1.unions_as_jsvalue, 1);
    }

    #[test]
    fn test_codegen_metrics_to_markdown() {
        let mut metrics = CodegenMetrics::new();
        metrics.record_type_mapped();
        metrics.record_union_option();
        metrics.record_utility_expanded();

        let report = metrics.to_markdown("Test Report");

        assert!(report.contains("# Test Report"));
        assert!(report.contains("Types mapped"));
        assert!(report.contains("Option<T>"));
        assert!(report.contains("Utility Types"));
    }

    #[test]
    fn test_codegen_metrics_to_json() {
        let mut metrics = CodegenMetrics::new();
        metrics.record_type_mapped();
        metrics.record_type_degraded();

        let json = metrics.to_json();

        assert!(json.contains("\"types_total\": 2"));
        assert!(json.contains("\"types_mapped\": 1"));
        assert!(json.contains("\"types_degraded\": 1"));
    }
}
