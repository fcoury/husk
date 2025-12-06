//! Diagnostic report generator for DTS parsing.
//!
//! Generates a markdown report (`dts-report.md`) that documents:
//! - Successfully converted types
//! - Types that required special handling (unions, utility types)
//! - Types that couldn't be converted (blocked features)
//! - Warnings and suggestions

use std::collections::HashMap;
use std::fmt::Write;

use crate::ast::{DtsFile, DtsItem, DtsType};
use crate::unions::{analyze_union, UnionStrategy};

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
                    .filter(|m| {
                        matches!(m, crate::ast::InterfaceMember::Property(p) if p.optional)
                    })
                    .count();
                if optional_count >= 3 {
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
                        self.info(context, DiagnosticCategory::UnionType, "Boolean literal union → bool");
                    }
                    UnionStrategy::Discriminated { discriminant, variants } => {
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
                            &format!(
                                "Function union → {} overloads",
                                signatures.len()
                            ),
                        );
                    }
                    UnionStrategy::TypeEnum { variants } => {
                        self.stats.unions_to_enum += 1;
                        self.info(
                            context,
                            DiagnosticCategory::UnionType,
                            &format!(
                                "Type union → enum with {} variants",
                                variants.len()
                            ),
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

                // Check for generic trait bounds usage
                if !type_args.is_empty() && has_keyof_pattern(ty) {
                    self.stats.blocked_features += 1;
                    self.error(
                        context,
                        DiagnosticCategory::BlockedFeature,
                        "keyof pattern requires generic trait bounds",
                        Some("Blocked on Husk language enhancement for generic trait bound resolution"),
                    );
                }

                // Recursively check type args
                for arg in type_args {
                    self.analyze_type(arg, context);
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
            | "Awaited"
    )
}

/// Check if a type uses keyof pattern (which is blocked).
fn has_keyof_pattern(ty: &DtsType) -> bool {
    match ty {
        DtsType::KeyOf(_) => true,
        DtsType::Named { type_args, .. } => type_args.iter().any(has_keyof_pattern),
        DtsType::Union(types) | DtsType::Intersection(types) => {
            types.iter().any(has_keyof_pattern)
        }
        DtsType::Array(inner) => has_keyof_pattern(inner),
        DtsType::IndexAccess { object, index } => {
            has_keyof_pattern(object) || has_keyof_pattern(index)
        }
        _ => false,
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
            items: vec![
                DtsItem::Function(DtsFunction {
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
                }),
            ],
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
}
