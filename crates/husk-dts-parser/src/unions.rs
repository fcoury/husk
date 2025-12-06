//! Union type strategies for TypeScript to Husk conversion.
//!
//! TypeScript unions like `string | number | null` need different representation
//! strategies in Husk depending on the context:
//!
//! 1. **Nullable types** (`T | null | undefined`) → `Option<T>`
//! 2. **String literal unions** (`"GET" | "POST"`) → Husk enum with variants
//! 3. **Discriminated unions** (tagged unions) → Husk enum with data
//! 4. **Mixed primitives** (`string | number`) → JsValue or separate overloads
//! 5. **Boolean literal unions** (`true | false`) → bool

use std::collections::HashSet;

use crate::ast::{DtsType, Primitive};
use crate::codegen::type_to_husk_string;

/// Strategy for representing a TypeScript union in Husk.
#[derive(Debug, Clone, PartialEq)]
pub enum UnionStrategy {
    /// The union is nullable: `T | null | undefined` → `Option<T>`
    Nullable {
        /// The non-null type
        inner: Box<DtsType>,
    },

    /// String literal union: `"GET" | "POST"` → enum
    StringEnum {
        /// Variant names derived from the string literals
        variants: Vec<String>,
    },

    /// Number literal union: `1 | 2 | 3` → enum with numeric values
    NumberEnum {
        /// Pairs of (variant_name, numeric_value)
        variants: Vec<(String, String)>,
    },

    /// Boolean literal union: `true | false` → bool
    Boolean,

    /// Discriminated union with a tag field
    Discriminated {
        /// The name of the discriminant field
        discriminant: String,
        /// Variants with their tag value and associated type
        variants: Vec<DiscriminatedVariant>,
    },

    /// Mixed type union that can't be nicely represented
    /// Falls back to JsValue
    JsValue,

    /// Function overloads: union of function types
    /// Each function signature becomes a separate overload
    Overloaded {
        signatures: Vec<Box<DtsType>>,
    },

    /// Simple union of named types that can become an enum
    TypeEnum {
        /// Type names that become variants
        variants: Vec<String>,
    },

    /// Keep as-is (for complex cases we don't handle)
    Passthrough(Vec<DtsType>),
}

/// A variant in a discriminated union.
#[derive(Debug, Clone, PartialEq)]
pub struct DiscriminatedVariant {
    /// The tag value (string literal from the discriminant field)
    pub tag: String,
    /// The variant name (derived from tag, PascalCase)
    pub name: String,
    /// The type for this variant (without the discriminant)
    pub ty: DtsType,
}

/// Analyze a union type and determine the best strategy for Husk representation.
pub fn analyze_union(types: &[DtsType]) -> UnionStrategy {
    // First, check if it's nullable
    if let Some(strategy) = check_nullable(types) {
        return strategy;
    }

    // Check for boolean literal union
    if let Some(strategy) = check_boolean_union(types) {
        return strategy;
    }

    // Check for string literal union
    if let Some(strategy) = check_string_enum(types) {
        return strategy;
    }

    // Check for number literal union
    if let Some(strategy) = check_number_enum(types) {
        return strategy;
    }

    // Check for discriminated union
    if let Some(strategy) = check_discriminated_union(types) {
        return strategy;
    }

    // Check for function overloads
    if let Some(strategy) = check_function_overloads(types) {
        return strategy;
    }

    // Check for simple type enum
    if let Some(strategy) = check_type_enum(types) {
        return strategy;
    }

    // Default: passthrough or JsValue
    if should_use_jsvalue(types) {
        UnionStrategy::JsValue
    } else {
        UnionStrategy::Passthrough(types.to_vec())
    }
}

/// Check if the union is nullable (contains null/undefined and exactly one other type).
///
/// Note: Only simple nullable unions like `T | null` or `T | undefined` are handled.
/// Complex unions like `string | number | null` fall through to other strategies
/// because they cannot be cleanly represented as `Option<T>`.
fn check_nullable(types: &[DtsType]) -> Option<UnionStrategy> {
    let mut non_null_types = Vec::new();
    let mut has_null = false;
    let mut has_undefined = false;

    for ty in types {
        match ty {
            DtsType::Primitive(Primitive::Null) => has_null = true,
            DtsType::Primitive(Primitive::Undefined) => has_undefined = true,
            _ => non_null_types.push(ty.clone()),
        }
    }

    // Only consider it nullable if there's exactly one non-null type
    if (has_null || has_undefined) && non_null_types.len() == 1 {
        return Some(UnionStrategy::Nullable {
            inner: Box::new(non_null_types.into_iter().next().unwrap()),
        });
    }

    None
}

/// Check if it's a `true | false` → bool.
fn check_boolean_union(types: &[DtsType]) -> Option<UnionStrategy> {
    if types.len() != 2 {
        return None;
    }

    let has_true = types.iter().any(|t| matches!(t, DtsType::BooleanLiteral(true)));
    let has_false = types
        .iter()
        .any(|t| matches!(t, DtsType::BooleanLiteral(false)));

    if has_true && has_false {
        Some(UnionStrategy::Boolean)
    } else {
        None
    }
}

/// Check if all types are string literals → enum.
fn check_string_enum(types: &[DtsType]) -> Option<UnionStrategy> {
    let mut variants = Vec::new();
    let mut seen = HashSet::new();

    for ty in types {
        match ty {
            DtsType::StringLiteral(s) => {
                // Only add if not already seen (preserves order, deduplicates)
                if seen.insert(s.clone()) {
                    variants.push(s.clone());
                }
            }
            _ => return None,
        }
    }

    if !variants.is_empty() {
        Some(UnionStrategy::StringEnum { variants })
    } else {
        None
    }
}

/// Check if all types are number literals → enum.
fn check_number_enum(types: &[DtsType]) -> Option<UnionStrategy> {
    let mut variants = Vec::new();

    for ty in types {
        match ty {
            DtsType::NumberLiteral(n) => {
                // Generate variant name from number (e.g., "1" -> "N1", "-1" -> "NNeg1")
                let name = number_to_variant_name(n);
                variants.push((name, n.clone()));
            }
            _ => return None,
        }
    }

    if !variants.is_empty() {
        Some(UnionStrategy::NumberEnum { variants })
    } else {
        None
    }
}

/// Convert a number string to a valid variant name.
///
/// Produces valid Rust identifiers by:
/// - Using "Neg" prefix for negative numbers
/// - Replacing non-alphanumeric characters (like '.') with '_'
/// - Collapsing consecutive underscores
/// - Trimming leading/trailing underscores
/// - Prefixing with 'N' if the result would start with a digit
///
/// Examples:
/// - "1" -> "N1"
/// - "-1" -> "Neg1"
/// - "1.5" -> "N1_5"
/// - "-1.5" -> "Neg1_5"
/// - "0.25" -> "N0_25"
fn number_to_variant_name(n: &str) -> String {
    // Check for negative prefix
    let (is_negative, num_part) = if let Some(stripped) = n.strip_prefix('-') {
        (true, stripped)
    } else {
        (false, n)
    };

    // Replace non-alphanumeric characters with underscores
    let sanitized: String = num_part
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect();

    // Collapse consecutive underscores
    let mut collapsed = String::new();
    let mut prev_underscore = false;
    for c in sanitized.chars() {
        if c == '_' {
            if !prev_underscore {
                collapsed.push(c);
            }
            prev_underscore = true;
        } else {
            collapsed.push(c);
            prev_underscore = false;
        }
    }

    // Trim leading and trailing underscores
    let trimmed = collapsed.trim_matches('_');

    // Build the final name
    if is_negative {
        format!("Neg{}", trimmed)
    } else {
        // Prefix with 'N' since variant names can't start with a digit
        format!("N{}", trimmed)
    }
}

/// Check for discriminated unions (all types are objects with a common tag field).
fn check_discriminated_union(types: &[DtsType]) -> Option<UnionStrategy> {
    // All types must be object types
    let objects: Vec<_> = types
        .iter()
        .filter_map(|t| {
            if let DtsType::Object(members) = t {
                Some(members)
            } else if let DtsType::Named { .. } = t {
                // Can't analyze named types without more context
                None
            } else {
                None
            }
        })
        .collect();

    if objects.len() != types.len() || objects.is_empty() {
        return None;
    }

    // Find a common discriminant field (string literal type in all objects)
    let discriminant = find_discriminant(&objects)?;

    // Extract variants
    let mut variants = Vec::new();
    for (i, members) in objects.iter().enumerate() {
        // Find the discriminant value
        let tag = find_discriminant_value(members, &discriminant)?;
        let name = string_to_pascal_case(&tag);

        // Clone the original type (we'll use it as-is for now)
        variants.push(DiscriminatedVariant {
            tag,
            name,
            ty: types[i].clone(),
        });
    }

    Some(UnionStrategy::Discriminated {
        discriminant,
        variants,
    })
}

/// Find a field that exists in all objects with a string literal type.
///
/// Uses a curated list of common discriminant field names rather than detecting
/// any field with unique values. This approach is more conservative and avoids
/// false positives from fields that happen to have unique string values but
/// aren't intended as discriminants (e.g., `id`, `name`).
fn find_discriminant(objects: &[&Vec<crate::ast::ObjectMember>]) -> Option<String> {
    use crate::ast::ObjectMember;

    if objects.is_empty() {
        return None;
    }

    // Common discriminant field names used in tagged union patterns
    let candidates = ["type", "kind", "tag", "_tag", "status", "action"];

    for candidate in &candidates {
        let candidate_str = candidate.to_string();
        let all_have_literal = objects.iter().all(|members| {
            members.iter().any(|m| match m {
                ObjectMember::Property { name, ty, .. } => {
                    name == *candidate && matches!(ty, DtsType::StringLiteral(_))
                }
                _ => false,
            })
        });

        if all_have_literal {
            return Some(candidate_str);
        }
    }

    None
}

/// Find the string literal value of a discriminant field.
fn find_discriminant_value(
    members: &[crate::ast::ObjectMember],
    discriminant: &str,
) -> Option<String> {
    use crate::ast::ObjectMember;

    for member in members {
        if let ObjectMember::Property { name, ty, .. } = member {
            if name == discriminant {
                if let DtsType::StringLiteral(value) = ty {
                    return Some(value.clone());
                }
            }
        }
    }

    None
}

/// Convert a string to PascalCase for enum variant names.
fn string_to_pascal_case(s: &str) -> String {
    s.split(|c: char| !c.is_alphanumeric())
        .filter(|part| !part.is_empty())
        .map(|part| {
            let mut chars = part.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_uppercase().chain(chars).collect(),
            }
        })
        .collect()
}

/// Check if all types are function types → overloads.
fn check_function_overloads(types: &[DtsType]) -> Option<UnionStrategy> {
    let all_functions = types.iter().all(|t| matches!(t, DtsType::Function(_)));

    if all_functions && types.len() > 1 {
        Some(UnionStrategy::Overloaded {
            signatures: types
                .iter()
                .map(|t| Box::new(t.clone()))
                .collect(),
        })
    } else {
        None
    }
}

/// Check if all types are named types that could become enum variants.
fn check_type_enum(types: &[DtsType]) -> Option<UnionStrategy> {
    let mut variants = Vec::new();
    let mut seen = HashSet::new();

    for ty in types {
        match ty {
            DtsType::Named { name, type_args } if type_args.is_empty() => {
                // Only add if not already seen (preserves order, deduplicates)
                if seen.insert(name.clone()) {
                    variants.push(name.clone());
                }
            }
            _ => return None,
        }
    }

    if variants.len() >= 2 {
        Some(UnionStrategy::TypeEnum { variants })
    } else {
        None
    }
}

/// Check if we should use JsValue for this union.
fn should_use_jsvalue(types: &[DtsType]) -> bool {
    // Use JsValue for unions of mixed primitive types
    let primitives: Vec<_> = types
        .iter()
        .filter_map(|t| {
            if let DtsType::Primitive(p) = t {
                Some(p)
            } else {
                None
            }
        })
        .collect();

    if primitives.len() >= 2 {
        // Multiple different primitives → JsValue
        return true;
    }

    // Union of primitive and complex type
    let has_primitive = types.iter().any(|t| matches!(t, DtsType::Primitive(_)));
    let has_complex = types.iter().any(|t| {
        matches!(
            t,
            DtsType::Named { .. }
                | DtsType::Object(_)
                | DtsType::Array(_)
                | DtsType::Function(_)
        )
    });

    has_primitive && has_complex
}

/// Generate Husk code for a union based on its strategy.
pub fn generate_union_code(strategy: &UnionStrategy, union_name: Option<&str>) -> String {
    match strategy {
        UnionStrategy::Nullable { inner } => {
            // Return Option<inner_type>
            format!("Option<{}>", type_to_husk_string(inner))
        }

        UnionStrategy::StringEnum { variants } => {
            // Generate enum definition
            let name = union_name.unwrap_or("StringUnion");
            let variant_defs: Vec<_> = variants
                .iter()
                .map(|v| format!("    {},", string_to_pascal_case(v)))
                .collect();
            format!("enum {} {{\n{}\n}}", name, variant_defs.join("\n"))
        }

        UnionStrategy::NumberEnum { variants } => {
            let name = union_name.unwrap_or("NumberUnion");
            let variant_defs: Vec<_> = variants
                .iter()
                .map(|(var_name, _val)| format!("    {},", var_name))
                .collect();
            format!("enum {} {{\n{}\n}}", name, variant_defs.join("\n"))
        }

        UnionStrategy::Boolean => "bool".to_string(),

        UnionStrategy::Discriminated { variants, .. } => {
            let name = union_name.unwrap_or("TaggedUnion");
            let variant_defs: Vec<_> = variants
                .iter()
                .map(|v| format!("    {}({}),", v.name, type_to_husk_string(&v.ty)))
                .collect();
            format!("enum {} {{\n{}\n}}", name, variant_defs.join("\n"))
        }

        UnionStrategy::JsValue => "JsValue".to_string(),

        UnionStrategy::Overloaded { .. } => {
            // Overloads are handled at the function level, not type level
            "/* function overloads */".to_string()
        }

        UnionStrategy::TypeEnum { variants } => {
            let name = union_name.unwrap_or("TypeUnion");
            let variant_defs: Vec<_> = variants
                .iter()
                .map(|v| format!("    {}({}),", v, v))
                .collect();
            format!("enum {} {{\n{}\n}}", name, variant_defs.join("\n"))
        }

        UnionStrategy::Passthrough(types) => {
            // Just show the types as a comment
            let type_strs: Vec<_> = types.iter().map(type_to_husk_string).collect();
            format!("/* {} */", type_strs.join(" | "))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nullable_union() {
        let types = vec![
            DtsType::Primitive(Primitive::String),
            DtsType::Primitive(Primitive::Null),
        ];
        let strategy = analyze_union(&types);
        assert!(matches!(strategy, UnionStrategy::Nullable { .. }));
    }

    #[test]
    fn test_boolean_union() {
        let types = vec![DtsType::BooleanLiteral(true), DtsType::BooleanLiteral(false)];
        let strategy = analyze_union(&types);
        assert!(matches!(strategy, UnionStrategy::Boolean));
    }

    #[test]
    fn test_string_enum() {
        let types = vec![
            DtsType::StringLiteral("GET".to_string()),
            DtsType::StringLiteral("POST".to_string()),
            DtsType::StringLiteral("PUT".to_string()),
        ];
        let strategy = analyze_union(&types);
        if let UnionStrategy::StringEnum { variants } = strategy {
            assert_eq!(variants.len(), 3);
            assert!(variants.contains(&"GET".to_string()));
        } else {
            panic!("expected StringEnum");
        }
    }

    #[test]
    fn test_string_enum_deduplicates() {
        // Test that duplicate string literals are deduplicated while preserving order
        let types = vec![
            DtsType::StringLiteral("GET".to_string()),
            DtsType::StringLiteral("POST".to_string()),
            DtsType::StringLiteral("GET".to_string()), // duplicate
            DtsType::StringLiteral("PUT".to_string()),
            DtsType::StringLiteral("POST".to_string()), // duplicate
        ];
        let strategy = analyze_union(&types);
        if let UnionStrategy::StringEnum { variants } = strategy {
            // Should only have 3 unique variants
            assert_eq!(variants.len(), 3);
            // Order should be preserved (first occurrence)
            assert_eq!(variants[0], "GET");
            assert_eq!(variants[1], "POST");
            assert_eq!(variants[2], "PUT");
        } else {
            panic!("expected StringEnum");
        }
    }

    #[test]
    fn test_type_enum_deduplicates() {
        // Test that duplicate named types are deduplicated while preserving order
        let types = vec![
            DtsType::Named {
                name: "Foo".to_string(),
                type_args: vec![],
            },
            DtsType::Named {
                name: "Bar".to_string(),
                type_args: vec![],
            },
            DtsType::Named {
                name: "Foo".to_string(), // duplicate
                type_args: vec![],
            },
            DtsType::Named {
                name: "Baz".to_string(),
                type_args: vec![],
            },
            DtsType::Named {
                name: "Bar".to_string(), // duplicate
                type_args: vec![],
            },
        ];
        let strategy = analyze_union(&types);
        if let UnionStrategy::TypeEnum { variants } = strategy {
            // Should only have 3 unique variants
            assert_eq!(variants.len(), 3);
            // Order should be preserved (first occurrence)
            assert_eq!(variants[0], "Foo");
            assert_eq!(variants[1], "Bar");
            assert_eq!(variants[2], "Baz");
        } else {
            panic!("expected TypeEnum, got {:?}", strategy);
        }
    }

    #[test]
    fn test_mixed_primitives_use_jsvalue() {
        let types = vec![
            DtsType::Primitive(Primitive::String),
            DtsType::Primitive(Primitive::Number),
        ];
        let strategy = analyze_union(&types);
        assert!(matches!(strategy, UnionStrategy::JsValue));
    }

    #[test]
    fn test_string_to_pascal_case() {
        assert_eq!(string_to_pascal_case("hello-world"), "HelloWorld");
        assert_eq!(string_to_pascal_case("GET"), "GET");
        assert_eq!(string_to_pascal_case("some_thing"), "SomeThing");
    }

    #[test]
    fn test_number_to_variant_name() {
        // Simple integers
        assert_eq!(number_to_variant_name("1"), "N1");
        assert_eq!(number_to_variant_name("42"), "N42");

        // Negative numbers
        assert_eq!(number_to_variant_name("-1"), "Neg1");
        assert_eq!(number_to_variant_name("-42"), "Neg42");

        // Floating point numbers (decimal point becomes underscore)
        assert_eq!(number_to_variant_name("1.5"), "N1_5");
        assert_eq!(number_to_variant_name("-1.5"), "Neg1_5");
        assert_eq!(number_to_variant_name("3.14159"), "N3_14159");

        // Scientific notation
        assert_eq!(number_to_variant_name("1e10"), "N1e10");
        assert_eq!(number_to_variant_name("1.5e-3"), "N1_5e_3");

        // Edge cases
        assert_eq!(number_to_variant_name("0"), "N0");
        assert_eq!(number_to_variant_name("-0"), "Neg0");
    }
}
