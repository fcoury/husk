//! TypeScript utility type expansion.
//!
//! This module handles expanding TypeScript utility types into their underlying
//! structure. This is needed because Husk doesn't have these utility types,
//! so we need to expand them at the type level.
//!
//! Supported utility types:
//! - `Partial<T>` → All properties become optional
//! - `Required<T>` → All properties become required
//! - `Readonly<T>` → All properties become readonly
//! - `Pick<T, K>` → Subset of properties
//! - `Omit<T, K>` → Exclude properties
//! - `Record<K, V>` → Index signature type
//! - `Exclude<T, U>` → Exclude types from union
//! - `Extract<T, U>` → Extract types from union
//! - `NonNullable<T>` → Remove null/undefined
//! - `ReturnType<T>` → Extract return type
//! - `Parameters<T>` → Extract parameter types
//! - `Awaited<T>` → Unwrap Promise

use crate::ast::{DtsType, ObjectMember, Primitive, TypeParam};
use std::collections::HashMap;

/// Maximum recursion depth for type expansion to prevent stack overflow on circular types.
const MAX_EXPANSION_DEPTH: usize = 64;

/// A registry of known types for utility type expansion.
#[derive(Debug, Clone, Default)]
pub struct TypeRegistry {
    /// Named types (interfaces, type aliases) by name
    types: HashMap<String, DtsType>,
}

impl TypeRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a type by name.
    pub fn register(&mut self, name: String, ty: DtsType) {
        self.types.insert(name, ty);
    }

    /// Look up a type by name.
    pub fn get(&self, name: &str) -> Option<&DtsType> {
        self.types.get(name)
    }
}

/// Context for type expansion, including type parameter bindings.
#[derive(Debug, Clone, Default)]
pub struct ExpansionContext {
    /// Type parameter bindings: T → concrete type
    bindings: HashMap<String, DtsType>,
}

impl ExpansionContext {
    /// Create an empty context.
    pub fn new() -> Self {
        Self::default()
    }

    /// Bind a type parameter to a concrete type.
    pub fn bind(&mut self, name: String, ty: DtsType) {
        self.bindings.insert(name, ty);
    }

    /// Look up a type parameter binding.
    pub fn get(&self, name: &str) -> Option<&DtsType> {
        self.bindings.get(name)
    }

    /// Create a context with bindings from type parameters and arguments.
    pub fn from_params_and_args(params: &[TypeParam], args: &[DtsType]) -> Self {
        let mut ctx = Self::new();
        for (param, arg) in params.iter().zip(args.iter()) {
            ctx.bind(param.name.clone(), arg.clone());
        }
        ctx
    }
}

/// Expand a type, resolving utility types and substituting type parameters.
pub fn expand_type(
    ty: &DtsType,
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
) -> DtsType {
    expand_type_with_depth(ty, registry, ctx, 0)
}

/// Internal type expansion with depth tracking to prevent stack overflow.
fn expand_type_with_depth(
    ty: &DtsType,
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    // Bail out to prevent stack overflow on circular types
    if depth > MAX_EXPANSION_DEPTH {
        return ty.clone();
    }

    match ty {
        // Named type - might be a utility type or type parameter
        DtsType::Named { name, type_args } => {
            // Check if it's a type parameter
            if let Some(bound) = ctx.get(name) {
                return bound.clone();
            }

            // Check for utility types (pass depth to maintain recursion tracking)
            match name.as_str() {
                "Partial" => expand_partial(type_args, registry, ctx, depth),
                "Required" => expand_required(type_args, registry, ctx, depth),
                "Readonly" => expand_readonly(type_args, registry, ctx, depth),
                "Pick" => expand_pick(type_args, registry, ctx, depth),
                "Omit" => expand_omit(type_args, registry, ctx, depth),
                "Record" => expand_record(type_args, registry, ctx, depth),
                "Exclude" => expand_exclude(type_args, registry, ctx, depth),
                "Extract" => expand_extract(type_args, registry, ctx, depth),
                "NonNullable" => expand_non_nullable(type_args, registry, ctx, depth),
                "ReturnType" => expand_return_type(type_args, registry, ctx, depth),
                "Parameters" => expand_parameters(type_args, registry, ctx, depth),
                "Awaited" => expand_awaited(type_args, registry, ctx, depth),
                "Promise" => expand_promise(type_args, registry, ctx, depth),
                "Array" => expand_array(type_args, registry, ctx, depth),
                _ => {
                    // Try to expand from registry
                    if let Some(def) = registry.get(name) {
                        // TODO: Handle type arguments properly
                        expand_type_with_depth(def, registry, ctx, depth + 1)
                    } else {
                        // Unknown type, expand type args and return
                        let expanded_args: Vec<_> = type_args
                            .iter()
                            .map(|arg| expand_type_with_depth(arg, registry, ctx, depth + 1))
                            .collect();
                        DtsType::Named {
                            name: name.clone(),
                            type_args: expanded_args,
                        }
                    }
                }
            }
        }

        // Union - expand each member
        DtsType::Union(types) => {
            let expanded: Vec<_> = types
                .iter()
                .map(|t| expand_type_with_depth(t, registry, ctx, depth + 1))
                .collect();
            DtsType::Union(expanded)
        }

        // Intersection - expand each member
        DtsType::Intersection(types) => {
            let expanded: Vec<_> = types
                .iter()
                .map(|t| expand_type_with_depth(t, registry, ctx, depth + 1))
                .collect();
            DtsType::Intersection(expanded)
        }

        // Array - expand element type
        DtsType::Array(elem) => {
            let expanded = expand_type_with_depth(elem, registry, ctx, depth + 1);
            DtsType::Array(Box::new(expanded))
        }

        // Tuple - expand element types
        DtsType::Tuple(elements) => {
            let expanded: Vec<_> = elements
                .iter()
                .map(|elem| crate::ast::TupleElement {
                    ty: expand_type_with_depth(&elem.ty, registry, ctx, depth + 1),
                    name: elem.name.clone(),
                    optional: elem.optional,
                    rest: elem.rest,
                })
                .collect();
            DtsType::Tuple(expanded)
        }

        // Object - expand member types
        DtsType::Object(members) => {
            let expanded: Vec<_> = members
                .iter()
                .map(|m| expand_object_member_with_depth(m, registry, ctx, depth + 1))
                .collect();
            DtsType::Object(expanded)
        }

        // Function - expand param and return types
        DtsType::Function(func) => {
            let expanded_params: Vec<_> = func
                .params
                .iter()
                .map(|p| crate::ast::Param {
                    name: p.name.clone(),
                    ty: expand_type_with_depth(&p.ty, registry, ctx, depth + 1),
                    optional: p.optional,
                    rest: p.rest,
                })
                .collect();
            let expanded_return = expand_type_with_depth(&func.return_type, registry, ctx, depth + 1);

            DtsType::Function(Box::new(crate::ast::FunctionType {
                type_params: func.type_params.clone(),
                params: expanded_params,
                return_type: Box::new(expanded_return),
                this_param: func.this_param.clone(),
            }))
        }

        // Other types - return as-is
        _ => ty.clone(),
    }
}

#[allow(dead_code)]
fn expand_object_member(
    member: &ObjectMember,
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
) -> ObjectMember {
    expand_object_member_with_depth(member, registry, ctx, 0)
}

fn expand_object_member_with_depth(
    member: &ObjectMember,
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> ObjectMember {
    match member {
        ObjectMember::Property {
            name,
            ty,
            optional,
            readonly,
        } => ObjectMember::Property {
            name: name.clone(),
            ty: expand_type_with_depth(ty, registry, ctx, depth),
            optional: *optional,
            readonly: *readonly,
        },
        ObjectMember::Method {
            name,
            type_params,
            params,
            return_type,
            optional,
            this_param,
        } => {
            let expanded_params: Vec<_> = params
                .iter()
                .map(|p| crate::ast::Param {
                    name: p.name.clone(),
                    ty: expand_type_with_depth(&p.ty, registry, ctx, depth),
                    optional: p.optional,
                    rest: p.rest,
                })
                .collect();
            ObjectMember::Method {
                name: name.clone(),
                type_params: type_params.clone(),
                params: expanded_params,
                return_type: return_type.as_ref().map(|rt| expand_type_with_depth(rt, registry, ctx, depth)),
                optional: *optional,
                this_param: this_param.as_ref().map(|tp| Box::new(expand_type_with_depth(tp, registry, ctx, depth))),
            }
        }
        ObjectMember::IndexSignature(sig) => ObjectMember::IndexSignature(crate::ast::IndexSignature {
            key_name: sig.key_name.clone(),
            key_type: expand_type_with_depth(&sig.key_type, registry, ctx, depth),
            value_type: expand_type_with_depth(&sig.value_type, registry, ctx, depth),
            readonly: sig.readonly,
        }),
        ObjectMember::CallSignature(sig) => ObjectMember::CallSignature(crate::ast::CallSignature {
            type_params: sig.type_params.clone(),
            params: sig
                .params
                .iter()
                .map(|p| crate::ast::Param {
                    name: p.name.clone(),
                    ty: expand_type_with_depth(&p.ty, registry, ctx, depth),
                    optional: p.optional,
                    rest: p.rest,
                })
                .collect(),
            return_type: sig.return_type.as_ref().map(|rt| expand_type_with_depth(rt, registry, ctx, depth)),
            this_param: sig.this_param.as_ref().map(|tp| Box::new(expand_type_with_depth(tp, registry, ctx, depth))),
        }),
        ObjectMember::ConstructSignature(sig) => ObjectMember::ConstructSignature(crate::ast::ConstructSignature {
            type_params: sig.type_params.clone(),
            params: sig
                .params
                .iter()
                .map(|p| crate::ast::Param {
                    name: p.name.clone(),
                    ty: expand_type_with_depth(&p.ty, registry, ctx, depth),
                    optional: p.optional,
                    rest: p.rest,
                })
                .collect(),
            return_type: sig.return_type.as_ref().map(|rt| expand_type_with_depth(rt, registry, ctx, depth)),
        }),
    }
}

/// Partial<T> - make all properties optional
///
/// Note: This currently only affects `ObjectMember::Property` members.
/// Methods, index signatures, and call signatures are left unchanged,
/// which differs from full TypeScript semantics where mapped types
/// operate over all members.
fn expand_partial(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let Some(inner) = type_args.first() else {
        return DtsType::Primitive(Primitive::Any);
    };

    let expanded = expand_type_with_depth(inner, registry, ctx, depth + 1);

    match expanded {
        DtsType::Object(members) => {
            let partial_members: Vec<_> = members
                .into_iter()
                .map(|m| match m {
                    ObjectMember::Property {
                        name,
                        ty,
                        readonly,
                        ..
                    } => ObjectMember::Property {
                        name,
                        ty,
                        optional: true, // Make optional
                        readonly,
                    },
                    other => other,
                })
                .collect();
            DtsType::Object(partial_members)
        }
        _ => expanded,
    }
}

/// Required<T> - make all properties required
///
/// Note: This currently only affects `ObjectMember::Property` members.
/// Methods, index signatures, and call signatures are left unchanged,
/// which differs from full TypeScript semantics where mapped types
/// operate over all members.
fn expand_required(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let Some(inner) = type_args.first() else {
        return DtsType::Primitive(Primitive::Any);
    };

    let expanded = expand_type_with_depth(inner, registry, ctx, depth + 1);

    match expanded {
        DtsType::Object(members) => {
            let required_members: Vec<_> = members
                .into_iter()
                .map(|m| match m {
                    ObjectMember::Property {
                        name,
                        ty,
                        readonly,
                        ..
                    } => ObjectMember::Property {
                        name,
                        ty,
                        optional: false, // Make required
                        readonly,
                    },
                    other => other,
                })
                .collect();
            DtsType::Object(required_members)
        }
        _ => expanded,
    }
}

/// Readonly<T> - make all properties readonly
///
/// Note: This currently only affects `ObjectMember::Property` members.
/// Index signatures (which can also have `readonly` in TypeScript) and
/// other member types are left unchanged.
fn expand_readonly(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let Some(inner) = type_args.first() else {
        return DtsType::Primitive(Primitive::Any);
    };

    let expanded = expand_type_with_depth(inner, registry, ctx, depth + 1);

    match expanded {
        DtsType::Object(members) => {
            let readonly_members: Vec<_> = members
                .into_iter()
                .map(|m| match m {
                    ObjectMember::Property {
                        name,
                        ty,
                        optional,
                        ..
                    } => ObjectMember::Property {
                        name,
                        ty,
                        optional,
                        readonly: true, // Make readonly
                    },
                    other => other,
                })
                .collect();
            DtsType::Object(readonly_members)
        }
        _ => expanded,
    }
}

/// Pick<T, K> - select subset of properties
fn expand_pick(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let (Some(inner), Some(keys)) = (type_args.first(), type_args.get(1)) else {
        return DtsType::Primitive(Primitive::Any);
    };

    let expanded = expand_type_with_depth(inner, registry, ctx, depth + 1);
    let key_set = extract_key_literals(keys);

    match expanded {
        DtsType::Object(members) => {
            let picked_members: Vec<_> = members
                .into_iter()
                .filter(|m| match m {
                    ObjectMember::Property { name, .. } => key_set.contains(name),
                    ObjectMember::Method { name, .. } => key_set.contains(name),
                    _ => false,
                })
                .collect();
            DtsType::Object(picked_members)
        }
        _ => expanded,
    }
}

/// Omit<T, K> - exclude properties
fn expand_omit(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let (Some(inner), Some(keys)) = (type_args.first(), type_args.get(1)) else {
        return DtsType::Primitive(Primitive::Any);
    };

    let expanded = expand_type_with_depth(inner, registry, ctx, depth + 1);
    let key_set = extract_key_literals(keys);

    match expanded {
        DtsType::Object(members) => {
            let omitted_members: Vec<_> = members
                .into_iter()
                .filter(|m| match m {
                    ObjectMember::Property { name, .. } => !key_set.contains(name),
                    ObjectMember::Method { name, .. } => !key_set.contains(name),
                    _ => true,
                })
                .collect();
            DtsType::Object(omitted_members)
        }
        _ => expanded,
    }
}

/// Record<K, V> - create index signature
fn expand_record(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let (Some(key_type), Some(value_type)) = (type_args.first(), type_args.get(1)) else {
        return DtsType::Primitive(Primitive::Any);
    };

    let expanded_key = expand_type_with_depth(key_type, registry, ctx, depth + 1);
    let expanded_value = expand_type_with_depth(value_type, registry, ctx, depth + 1);

    // Check if key is a string literal union → create object type
    if let DtsType::Union(types) = &expanded_key {
        let all_strings = types.iter().all(|t| matches!(t, DtsType::StringLiteral(_)));
        if all_strings {
            let members: Vec<_> = types
                .iter()
                .filter_map(|t| {
                    if let DtsType::StringLiteral(name) = t {
                        Some(ObjectMember::Property {
                            name: name.clone(),
                            ty: expanded_value.clone(),
                            optional: false,
                            readonly: false,
                        })
                    } else {
                        None
                    }
                })
                .collect();
            return DtsType::Object(members);
        }
    }

    // Otherwise create index signature
    DtsType::Object(vec![ObjectMember::IndexSignature(crate::ast::IndexSignature {
        key_name: "key".to_string(),
        key_type: expanded_key,
        value_type: expanded_value,
        readonly: false,
    })])
}

/// Exclude<T, U> - exclude types from union
fn expand_exclude(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let (Some(union_type), Some(excluded)) = (type_args.first(), type_args.get(1)) else {
        return DtsType::Primitive(Primitive::Any);
    };

    let expanded_union = expand_type_with_depth(union_type, registry, ctx, depth + 1);
    let expanded_excluded = expand_type_with_depth(excluded, registry, ctx, depth + 1);

    match expanded_union {
        DtsType::Union(types) => {
            let excluded_set = flatten_union(&expanded_excluded);
            let filtered: Vec<_> = types
                .into_iter()
                .filter(|t| !excluded_set.contains(t))
                .collect();

            if filtered.len() == 1 {
                filtered.into_iter().next().unwrap()
            } else if filtered.is_empty() {
                DtsType::Primitive(Primitive::Never)
            } else {
                DtsType::Union(filtered)
            }
        }
        _ => expanded_union,
    }
}

/// Extract<T, U> - extract types from union
fn expand_extract(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let (Some(union_type), Some(extracted)) = (type_args.first(), type_args.get(1)) else {
        return DtsType::Primitive(Primitive::Any);
    };

    let expanded_union = expand_type_with_depth(union_type, registry, ctx, depth + 1);
    let expanded_extracted = expand_type_with_depth(extracted, registry, ctx, depth + 1);

    match expanded_union {
        DtsType::Union(types) => {
            let extract_set = flatten_union(&expanded_extracted);
            let filtered: Vec<_> = types
                .into_iter()
                .filter(|t| extract_set.contains(t))
                .collect();

            if filtered.len() == 1 {
                filtered.into_iter().next().unwrap()
            } else if filtered.is_empty() {
                DtsType::Primitive(Primitive::Never)
            } else {
                DtsType::Union(filtered)
            }
        }
        _ => expanded_union,
    }
}

/// NonNullable<T> - remove null/undefined
fn expand_non_nullable(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let Some(inner) = type_args.first() else {
        return DtsType::Primitive(Primitive::Any);
    };

    let expanded = expand_type_with_depth(inner, registry, ctx, depth + 1);

    match expanded {
        DtsType::Union(types) => {
            let filtered: Vec<_> = types
                .into_iter()
                .filter(|t| {
                    !matches!(
                        t,
                        DtsType::Primitive(Primitive::Null)
                            | DtsType::Primitive(Primitive::Undefined)
                    )
                })
                .collect();

            if filtered.len() == 1 {
                filtered.into_iter().next().unwrap()
            } else if filtered.is_empty() {
                DtsType::Primitive(Primitive::Never)
            } else {
                DtsType::Union(filtered)
            }
        }
        _ => expanded,
    }
}

/// ReturnType<T> - extract return type of function
fn expand_return_type(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let Some(inner) = type_args.first() else {
        return DtsType::Primitive(Primitive::Any);
    };

    let expanded = expand_type_with_depth(inner, registry, ctx, depth + 1);

    match expanded {
        DtsType::Function(func) => *func.return_type,
        _ => DtsType::Primitive(Primitive::Any),
    }
}

/// Parameters<T> - extract parameter types as tuple
fn expand_parameters(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let Some(inner) = type_args.first() else {
        return DtsType::Primitive(Primitive::Any);
    };

    let expanded = expand_type_with_depth(inner, registry, ctx, depth + 1);

    match expanded {
        DtsType::Function(func) => {
            let elements: Vec<_> = func
                .params
                .into_iter()
                .map(|p| crate::ast::TupleElement {
                    ty: p.ty,
                    name: Some(p.name),
                    optional: p.optional,
                    rest: p.rest,
                })
                .collect();
            DtsType::Tuple(elements)
        }
        _ => DtsType::Primitive(Primitive::Any),
    }
}

/// Awaited<T> - unwrap Promise
fn expand_awaited(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let Some(inner) = type_args.first() else {
        return DtsType::Primitive(Primitive::Any);
    };

    let expanded = expand_type_with_depth(inner, registry, ctx, depth + 1);

    // Recursively unwrap Promise types
    unwrap_promise(&expanded)
}

/// Promise<T> - keep as JsPromise<T>
fn expand_promise(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let Some(inner) = type_args.first() else {
        return DtsType::Named {
            name: "JsPromise".to_string(),
            type_args: vec![DtsType::Primitive(Primitive::Any)],
        };
    };

    let expanded = expand_type_with_depth(inner, registry, ctx, depth + 1);

    DtsType::Named {
        name: "JsPromise".to_string(),
        type_args: vec![expanded],
    }
}

/// Array<T> - convert to [T]
fn expand_array(
    type_args: &[DtsType],
    registry: &TypeRegistry,
    ctx: &ExpansionContext,
    depth: usize,
) -> DtsType {
    let Some(inner) = type_args.first() else {
        return DtsType::Array(Box::new(DtsType::Primitive(Primitive::Any)));
    };

    let expanded = expand_type_with_depth(inner, registry, ctx, depth + 1);
    DtsType::Array(Box::new(expanded))
}

/// Extract string literal keys from a type (for Pick/Omit).
fn extract_key_literals(ty: &DtsType) -> std::collections::HashSet<String> {
    let mut keys = std::collections::HashSet::new();

    match ty {
        DtsType::StringLiteral(s) => {
            keys.insert(s.clone());
        }
        DtsType::Union(types) => {
            for t in types {
                keys.extend(extract_key_literals(t));
            }
        }
        _ => {}
    }

    keys
}

/// Flatten a union type into a list of types.
fn flatten_union(ty: &DtsType) -> Vec<DtsType> {
    let mut types = Vec::new();

    match ty {
        DtsType::Union(union_types) => {
            for t in union_types {
                types.extend(flatten_union(t));
            }
        }
        other => {
            types.push(other.clone());
        }
    }

    types
}

/// Unwrap Promise<T> to T (recursively).
fn unwrap_promise(ty: &DtsType) -> DtsType {
    match ty {
        DtsType::Named { name, type_args } if name == "Promise" || name == "JsPromise" => {
            if let Some(inner) = type_args.first() {
                unwrap_promise(inner)
            } else {
                DtsType::Primitive(Primitive::Any)
            }
        }
        _ => ty.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_partial() {
        let obj = DtsType::Object(vec![
            ObjectMember::Property {
                name: "name".to_string(),
                ty: DtsType::Primitive(Primitive::String),
                optional: false,
                readonly: false,
            },
            ObjectMember::Property {
                name: "age".to_string(),
                ty: DtsType::Primitive(Primitive::Number),
                optional: false,
                readonly: false,
            },
        ]);

        let partial_ty = DtsType::Named {
            name: "Partial".to_string(),
            type_args: vec![obj],
        };

        let registry = TypeRegistry::new();
        let ctx = ExpansionContext::new();
        let expanded = expand_type(&partial_ty, &registry, &ctx);

        if let DtsType::Object(members) = expanded {
            assert!(members.iter().all(|m| {
                matches!(m, ObjectMember::Property { optional: true, .. })
            }));
        } else {
            panic!("expected object type");
        }
    }

    #[test]
    fn test_pick() {
        let obj = DtsType::Object(vec![
            ObjectMember::Property {
                name: "name".to_string(),
                ty: DtsType::Primitive(Primitive::String),
                optional: false,
                readonly: false,
            },
            ObjectMember::Property {
                name: "age".to_string(),
                ty: DtsType::Primitive(Primitive::Number),
                optional: false,
                readonly: false,
            },
            ObjectMember::Property {
                name: "email".to_string(),
                ty: DtsType::Primitive(Primitive::String),
                optional: false,
                readonly: false,
            },
        ]);

        let pick_ty = DtsType::Named {
            name: "Pick".to_string(),
            type_args: vec![
                obj,
                DtsType::Union(vec![
                    DtsType::StringLiteral("name".to_string()),
                    DtsType::StringLiteral("age".to_string()),
                ]),
            ],
        };

        let registry = TypeRegistry::new();
        let ctx = ExpansionContext::new();
        let expanded = expand_type(&pick_ty, &registry, &ctx);

        if let DtsType::Object(members) = expanded {
            assert_eq!(members.len(), 2);
        } else {
            panic!("expected object type");
        }
    }

    #[test]
    fn test_record() {
        let record_ty = DtsType::Named {
            name: "Record".to_string(),
            type_args: vec![
                DtsType::Union(vec![
                    DtsType::StringLiteral("a".to_string()),
                    DtsType::StringLiteral("b".to_string()),
                ]),
                DtsType::Primitive(Primitive::Number),
            ],
        };

        let registry = TypeRegistry::new();
        let ctx = ExpansionContext::new();
        let expanded = expand_type(&record_ty, &registry, &ctx);

        if let DtsType::Object(members) = expanded {
            assert_eq!(members.len(), 2);
        } else {
            panic!("expected object type");
        }
    }

    #[test]
    fn test_non_nullable() {
        let nullable_ty = DtsType::Named {
            name: "NonNullable".to_string(),
            type_args: vec![DtsType::Union(vec![
                DtsType::Primitive(Primitive::String),
                DtsType::Primitive(Primitive::Null),
                DtsType::Primitive(Primitive::Undefined),
            ])],
        };

        let registry = TypeRegistry::new();
        let ctx = ExpansionContext::new();
        let expanded = expand_type(&nullable_ty, &registry, &ctx);

        assert_eq!(expanded, DtsType::Primitive(Primitive::String));
    }
}
