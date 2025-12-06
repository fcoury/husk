//! Builder pattern generator for TypeScript interfaces.
//!
//! For TypeScript interfaces with many optional properties, we generate
//! a builder pattern in Husk that allows fluent construction:
//!
//! ```husk
//! let opts = RequestOptionsBuilder::new()
//!     .method("POST")
//!     .headers(my_headers)
//!     .build();
//! ```
//!
//! This is more ergonomic than constructing objects with many Optional fields.

use crate::ast::{DtsInterface, DtsType, InterfaceMember, Primitive, TypeParam};

/// Configuration for builder generation.
#[derive(Debug, Clone)]
pub struct BuilderConfig {
    /// Minimum number of optional properties to generate a builder
    pub min_optional_props: usize,
    /// Whether to generate a `new()` constructor with required properties
    pub generate_new: bool,
    /// Whether to generate a `build()` method that returns the final struct
    pub generate_build: bool,
    /// Suffix for builder struct name (e.g., "Builder" → "RequestOptionsBuilder")
    pub builder_suffix: String,
}

impl Default for BuilderConfig {
    fn default() -> Self {
        Self {
            min_optional_props: 3,
            generate_new: true,
            generate_build: true,
            builder_suffix: "Builder".to_string(),
        }
    }
}

/// A generated builder for an interface.
#[derive(Debug, Clone)]
pub struct GeneratedBuilder {
    /// Original interface name
    pub interface_name: String,
    /// Builder struct name
    pub builder_name: String,
    /// Type parameters from the interface
    pub type_params: Vec<TypeParam>,
    /// Required properties (must be provided in `new()`)
    pub required_props: Vec<BuilderProperty>,
    /// Optional properties (can be set via builder methods)
    pub optional_props: Vec<BuilderProperty>,
    /// Generated Husk code for the builder
    pub code: String,
}

/// A property in the builder.
#[derive(Debug, Clone)]
pub struct BuilderProperty {
    /// Property name
    pub name: String,
    /// Property type
    pub ty: DtsType,
    /// Whether this property is readonly
    pub readonly: bool,
}

/// Determine if an interface should have a builder generated.
pub fn should_generate_builder(interface: &DtsInterface, config: &BuilderConfig) -> bool {
    let optional_count = interface
        .members
        .iter()
        .filter(|m| matches!(m, InterfaceMember::Property(p) if p.optional))
        .count();

    optional_count >= config.min_optional_props
}

/// Generate a builder for an interface.
pub fn generate_builder(interface: &DtsInterface, config: &BuilderConfig) -> GeneratedBuilder {
    let builder_name = format!("{}{}", interface.name, config.builder_suffix);

    let (required_props, optional_props): (Vec<_>, Vec<_>) = interface
        .members
        .iter()
        .filter_map(|m| {
            if let InterfaceMember::Property(prop) = m {
                Some((
                    BuilderProperty {
                        name: prop.name.clone(),
                        ty: prop.ty.clone(),
                        readonly: prop.readonly,
                    },
                    prop.optional,
                ))
            } else {
                None
            }
        })
        .partition(|(_, optional)| !*optional);

    let required_props: Vec<_> = required_props.into_iter().map(|(p, _)| p).collect();
    let optional_props: Vec<_> = optional_props.into_iter().map(|(p, _)| p).collect();

    let code = generate_builder_code(
        &interface.name,
        &builder_name,
        &interface.type_params,
        &required_props,
        &optional_props,
        config,
    );

    GeneratedBuilder {
        interface_name: interface.name.clone(),
        builder_name,
        type_params: interface.type_params.clone(),
        required_props,
        optional_props,
        code,
    }
}

/// Generate the Husk code for a builder.
fn generate_builder_code(
    interface_name: &str,
    builder_name: &str,
    type_params: &[TypeParam],
    required_props: &[BuilderProperty],
    optional_props: &[BuilderProperty],
    config: &BuilderConfig,
) -> String {
    let mut code = String::new();

    // Type parameter string
    let type_params_str = if type_params.is_empty() {
        String::new()
    } else {
        let params: Vec<_> = type_params
            .iter()
            .map(|tp| {
                if let Some(constraint) = &tp.constraint {
                    format!("{}: {}", tp.name, type_to_husk_string(constraint))
                } else {
                    tp.name.clone()
                }
            })
            .collect();
        format!("<{}>", params.join(", "))
    };

    let type_args_str = if type_params.is_empty() {
        String::new()
    } else {
        let args: Vec<_> = type_params.iter().map(|tp| tp.name.clone()).collect();
        format!("<{}>", args.join(", "))
    };

    // Builder struct definition
    code.push_str(&format!(
        "/// Builder for `{}`.\n",
        interface_name
    ));
    code.push_str(&format!(
        "struct {}{} {{\n",
        builder_name, type_params_str
    ));

    // Required properties (stored directly)
    for prop in required_props {
        code.push_str(&format!(
            "    {}: {},\n",
            prop.name,
            type_to_husk_string(&prop.ty)
        ));
    }

    // Optional properties (wrapped in Option)
    for prop in optional_props {
        code.push_str(&format!(
            "    {}: Option<{}>,\n",
            prop.name,
            type_to_husk_string(&prop.ty)
        ));
    }

    code.push_str("}\n\n");

    // Impl block
    code.push_str(&format!(
        "impl{} {}{} {{\n",
        type_params_str, builder_name, type_args_str
    ));

    // new() constructor
    if config.generate_new {
        let params: Vec<_> = required_props
            .iter()
            .map(|p| format!("{}: {}", p.name, type_to_husk_string(&p.ty)))
            .collect();

        code.push_str(&format!(
            "    /// Create a new builder{}.\n",
            if required_props.is_empty() {
                ""
            } else {
                " with required properties"
            }
        ));
        code.push_str(&format!(
            "    fn new({}) -> Self {{\n",
            params.join(", ")
        ));
        code.push_str("        Self {\n");

        for prop in required_props {
            code.push_str(&format!("            {},\n", prop.name));
        }
        for prop in optional_props {
            code.push_str(&format!("            {}: None,\n", prop.name));
        }

        code.push_str("        }\n");
        code.push_str("    }\n\n");
    }

    // Builder methods for optional properties
    for prop in optional_props {
        let method_name = to_snake_case(&prop.name);
        code.push_str(&format!(
            "    /// Set the `{}` property.\n",
            prop.name
        ));
        code.push_str(&format!(
            "    fn {}(mut self, value: {}) -> Self {{\n",
            method_name,
            type_to_husk_string(&prop.ty)
        ));
        code.push_str(&format!(
            "        self.{} = Some(value);\n",
            prop.name
        ));
        code.push_str("        self\n");
        code.push_str("    }\n\n");
    }

    // build() method
    if config.generate_build {
        code.push_str(&format!(
            "    /// Build the final `{}`.\n",
            interface_name
        ));
        code.push_str(&format!(
            "    fn build(self) -> {}{} {{\n",
            interface_name, type_args_str
        ));
        code.push_str(&format!("        {}{} {{\n", interface_name, type_args_str));

        for prop in required_props {
            code.push_str(&format!("            {}: self.{},\n", prop.name, prop.name));
        }
        for prop in optional_props {
            code.push_str(&format!("            {}: self.{},\n", prop.name, prop.name));
        }

        code.push_str("        }\n");
        code.push_str("    }\n");
    }

    code.push_str("}\n");

    code
}

/// Convert a DtsType to a Husk type string.
fn type_to_husk_string(ty: &DtsType) -> String {
    match ty {
        DtsType::Primitive(p) => match p {
            Primitive::String => "String".to_string(),
            Primitive::Number => "f64".to_string(),
            Primitive::Boolean => "bool".to_string(),
            Primitive::Void => "()".to_string(),
            Primitive::Null | Primitive::Undefined => "()".to_string(),
            Primitive::Any | Primitive::Unknown => "JsValue".to_string(),
            Primitive::Never => "!".to_string(),
            Primitive::Object => "JsObject".to_string(),
            Primitive::Symbol => "JsValue".to_string(),
            Primitive::BigInt => "i64".to_string(),
        },
        DtsType::Named { name, type_args } => {
            if type_args.is_empty() {
                name.clone()
            } else {
                let args: Vec<_> = type_args.iter().map(type_to_husk_string).collect();
                format!("{}<{}>", name, args.join(", "))
            }
        }
        DtsType::Array(inner) => format!("[{}]", type_to_husk_string(inner)),
        DtsType::Union(types) => {
            // For unions, use JsValue as fallback
            if types.len() == 2 {
                // Check for Option pattern
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
                if non_null.len() == 1 {
                    return format!("Option<{}>", type_to_husk_string(non_null[0]));
                }
            }
            "JsValue".to_string()
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

/// Convert PascalCase or camelCase to snake_case.
fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }
    result
}

/// Generate builders for all eligible interfaces.
pub fn generate_all_builders(
    interfaces: &[DtsInterface],
    config: &BuilderConfig,
) -> Vec<GeneratedBuilder> {
    interfaces
        .iter()
        .filter(|i| should_generate_builder(i, config))
        .map(|i| generate_builder(i, config))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::PropertyMember;

    fn make_test_interface() -> DtsInterface {
        DtsInterface {
            name: "RequestOptions".to_string(),
            type_params: vec![],
            extends: vec![],
            members: vec![
                InterfaceMember::Property(PropertyMember {
                    name: "url".to_string(),
                    ty: DtsType::Primitive(Primitive::String),
                    optional: false,
                    readonly: false,
                }),
                InterfaceMember::Property(PropertyMember {
                    name: "method".to_string(),
                    ty: DtsType::Primitive(Primitive::String),
                    optional: true,
                    readonly: false,
                }),
                InterfaceMember::Property(PropertyMember {
                    name: "headers".to_string(),
                    ty: DtsType::Primitive(Primitive::Object),
                    optional: true,
                    readonly: false,
                }),
                InterfaceMember::Property(PropertyMember {
                    name: "body".to_string(),
                    ty: DtsType::Primitive(Primitive::String),
                    optional: true,
                    readonly: false,
                }),
                InterfaceMember::Property(PropertyMember {
                    name: "timeout".to_string(),
                    ty: DtsType::Primitive(Primitive::Number),
                    optional: true,
                    readonly: false,
                }),
            ],
        }
    }

    #[test]
    fn test_should_generate_builder() {
        let interface = make_test_interface();
        let config = BuilderConfig::default();

        assert!(should_generate_builder(&interface, &config));
    }

    #[test]
    fn test_generate_builder() {
        let interface = make_test_interface();
        let config = BuilderConfig::default();

        let builder = generate_builder(&interface, &config);

        assert_eq!(builder.interface_name, "RequestOptions");
        assert_eq!(builder.builder_name, "RequestOptionsBuilder");
        assert_eq!(builder.required_props.len(), 1); // url
        assert_eq!(builder.optional_props.len(), 4); // method, headers, body, timeout
        assert!(builder.code.contains("fn new(url: String) -> Self"));
        assert!(builder.code.contains("fn method(mut self, value: String) -> Self"));
    }

    #[test]
    fn test_to_snake_case() {
        assert_eq!(to_snake_case("methodName"), "method_name");
        assert_eq!(to_snake_case("HTTPRequest"), "h_t_t_p_request");
        assert_eq!(to_snake_case("simple"), "simple");
    }
}
