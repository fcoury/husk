//! Oxc-based TypeScript declaration file parser.
//!
//! This module provides an alternative parser implementation using the Oxc
//! (Oxidation Compiler) library for high-performance AST traversal with
//! arena-based memory management.
//!
//! The Oxc parser is used for:
//! - Multi-file resolution and import graph walking
//! - More complete TypeScript syntax support
//! - Better performance on large declaration files

use oxc_allocator::Allocator;
use oxc_ast::ast::*;
use oxc_ast_visit::Visit;
use oxc_parser::Parser;
use oxc_span::SourceType;
use std::path::Path;

// Import specific items from crate::ast to avoid conflicts with oxc_ast
use crate::ast::{
    CallSignature, ClassMember, ClassMethod, ClassProperty, ConstructSignature, DtsClass,
    DtsExport, DtsFile, DtsFunction, DtsInterface, DtsItem, DtsModule, DtsNamespace, DtsType,
    DtsTypeAlias, DtsVariable, IndexSignature, InterfaceMember, MappedModifier, MethodMember,
    ObjectMember, Param, Primitive, PropertyMember, TemplateLiteralPart, TupleElement, TypeParam,
    Visibility,
};
// Alias our FunctionType to avoid conflict with oxc_ast::ast::FunctionType
use crate::ast::FunctionType as HuskFunctionType;

/// Parse a .d.ts source string using the Oxc parser.
///
/// This function provides the same interface as `crate::parse` but uses
/// Oxc's high-performance parser under the hood.
pub fn parse_with_oxc(src: &str, filename: &str) -> Result<DtsFile, OxcParseError> {
    let allocator = Allocator::default();
    let source_type = SourceType::from_path(Path::new(filename)).unwrap_or_else(|_| {
        // Default to .d.ts if path inference fails
        SourceType::d_ts()
    });

    let parser_return = Parser::new(&allocator, src, source_type).parse();

    if parser_return.panicked {
        let errors: Vec<String> = parser_return
            .errors
            .iter()
            .map(|e| e.to_string())
            .collect();
        return Err(OxcParseError {
            message: errors.join("; "),
        });
    }

    // Visit the AST and collect declarations
    let mut visitor = DtsVisitor::new(src);
    visitor.visit_program(&parser_return.program);

    Ok(DtsFile {
        items: visitor.items,
    })
}

/// Error from Oxc parsing.
#[derive(Debug, Clone)]
pub struct OxcParseError {
    pub message: String,
}

impl std::fmt::Display for OxcParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "oxc parse error: {}", self.message)
    }
}

impl std::error::Error for OxcParseError {}

/// Visitor that converts Oxc AST to our DtsFile AST.
struct DtsVisitor<'a> {
    #[allow(dead_code)]
    src: &'a str,
    items: Vec<DtsItem>,
}

impl<'a> DtsVisitor<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src,
            items: Vec::new(),
        }
    }

    /// Convert an Oxc type annotation to our DtsType.
    fn convert_type(&self, ts_type: &TSType<'_>) -> DtsType {
        match ts_type {
            TSType::TSStringKeyword(_) => DtsType::Primitive(Primitive::String),
            TSType::TSNumberKeyword(_) => DtsType::Primitive(Primitive::Number),
            TSType::TSBooleanKeyword(_) => DtsType::Primitive(Primitive::Boolean),
            TSType::TSVoidKeyword(_) => DtsType::Primitive(Primitive::Void),
            TSType::TSNullKeyword(_) => DtsType::Primitive(Primitive::Null),
            TSType::TSUndefinedKeyword(_) => DtsType::Primitive(Primitive::Undefined),
            TSType::TSAnyKeyword(_) => DtsType::Primitive(Primitive::Any),
            TSType::TSUnknownKeyword(_) => DtsType::Primitive(Primitive::Unknown),
            TSType::TSNeverKeyword(_) => DtsType::Primitive(Primitive::Never),
            TSType::TSObjectKeyword(_) => DtsType::Primitive(Primitive::Object),
            TSType::TSSymbolKeyword(_) => DtsType::Primitive(Primitive::Symbol),
            TSType::TSBigIntKeyword(_) => DtsType::Primitive(Primitive::BigInt),
            TSType::TSThisType(_) => DtsType::This,

            TSType::TSLiteralType(lit) => match &lit.literal {
                TSLiteral::StringLiteral(s) => DtsType::StringLiteral(s.value.to_string()),
                TSLiteral::NumericLiteral(n) => {
                    // Prefer raw string for exact representation, fall back to value for cases where raw is None
                    let raw_str = n
                        .raw
                        .as_ref()
                        .map(|r| r.as_str().to_string())
                        .unwrap_or_else(|| n.value.to_string());
                    DtsType::NumberLiteral(raw_str)
                }
                TSLiteral::BooleanLiteral(b) => DtsType::BooleanLiteral(b.value),
                TSLiteral::BigIntLiteral(bi) => DtsType::NumberLiteral(bi.raw.as_ref().map(|r| r.to_string()).unwrap_or_default()),
                TSLiteral::TemplateLiteral(_) => DtsType::Primitive(Primitive::String),
                TSLiteral::UnaryExpression(_) => DtsType::Primitive(Primitive::Number),
            },

            TSType::TSTypeReference(type_ref) => {
                let name = self.type_name_to_string(&type_ref.type_name);
                let type_args = type_ref
                    .type_arguments
                    .as_ref()
                    .map(|params| params.params.iter().map(|t| self.convert_type(t)).collect())
                    .unwrap_or_default();

                DtsType::Named { name, type_args }
            }

            TSType::TSUnionType(union) => {
                let types: Vec<DtsType> = union.types.iter().map(|t| self.convert_type(t)).collect();
                DtsType::Union(types)
            }

            TSType::TSIntersectionType(inter) => {
                let types: Vec<DtsType> =
                    inter.types.iter().map(|t| self.convert_type(t)).collect();
                DtsType::Intersection(types)
            }

            TSType::TSArrayType(arr) => {
                let element = self.convert_type(&arr.element_type);
                DtsType::Array(Box::new(element))
            }

            TSType::TSTupleType(tuple) => {
                let elements: Vec<TupleElement> = tuple
                    .element_types
                    .iter()
                    .map(|elem| self.convert_tuple_element(elem))
                    .collect();
                DtsType::Tuple(elements)
            }

            TSType::TSFunctionType(func) => {
                let type_params = self.convert_type_params(func.type_parameters.as_ref());
                let params = self.convert_formal_params(&func.params);
                let return_type = self.convert_type(&func.return_type.type_annotation);

                DtsType::Function(Box::new(HuskFunctionType {
                    type_params,
                    params,
                    return_type: Box::new(return_type),
                    this_param: None, // TODO: Extract this param if present
                }))
            }

            TSType::TSTypeLiteral(obj) => {
                let members: Vec<ObjectMember> = obj
                    .members
                    .iter()
                    .filter_map(|m| self.convert_type_element(m))
                    .collect();
                DtsType::Object(members)
            }

            TSType::TSParenthesizedType(paren) => {
                let inner = self.convert_type(&paren.type_annotation);
                DtsType::Parenthesized(Box::new(inner))
            }

            TSType::TSTypeQuery(query) => {
                let name = self.expr_name_to_string(&query.expr_name);
                DtsType::TypeOf(name)
            }

            TSType::TSTypeOperatorType(op) => match op.operator {
                TSTypeOperatorOperator::Keyof => {
                    let inner = self.convert_type(&op.type_annotation);
                    DtsType::KeyOf(Box::new(inner))
                }
                TSTypeOperatorOperator::Readonly => {
                    // Readonly doesn't change the structure, just pass through
                    self.convert_type(&op.type_annotation)
                }
                TSTypeOperatorOperator::Unique => {
                    // unique symbol -> Symbol
                    DtsType::Primitive(Primitive::Symbol)
                }
            },

            TSType::TSIndexedAccessType(idx) => DtsType::IndexAccess {
                object: Box::new(self.convert_type(&idx.object_type)),
                index: Box::new(self.convert_type(&idx.index_type)),
            },

            TSType::TSConditionalType(cond) => DtsType::Conditional {
                check: Box::new(self.convert_type(&cond.check_type)),
                extends: Box::new(self.convert_type(&cond.extends_type)),
                true_type: Box::new(self.convert_type(&cond.true_type)),
                false_type: Box::new(self.convert_type(&cond.false_type)),
            },

            TSType::TSMappedType(mapped) => {
                let key_name = mapped.type_parameter.name.name.to_string();
                let key_constraint = mapped
                    .type_parameter
                    .constraint
                    .as_ref()
                    .map(|c| self.convert_type(c))
                    .unwrap_or(DtsType::Primitive(Primitive::Any));
                let value_type = mapped
                    .type_annotation
                    .as_ref()
                    .map(|t| self.convert_type(t))
                    .unwrap_or(DtsType::Primitive(Primitive::Any));

                // readonly and optional are now Option<TSMappedTypeModifierOperator>
                let readonly_mod = mapped.readonly.map(|m| self.convert_mapped_modifier(m));
                let optional_mod = mapped.optional.map(|m| self.convert_mapped_modifier(m));

                DtsType::Mapped {
                    key_name,
                    key_constraint: Box::new(key_constraint),
                    value_type: Box::new(value_type),
                    readonly: readonly_mod,
                    optional: optional_mod,
                }
            }

            TSType::TSInferType(infer) => {
                DtsType::Infer(infer.type_parameter.name.name.to_string())
            }

            TSType::TSTemplateLiteralType(template) => {
                let mut parts = Vec::new();

                // Add quasis (string parts) and types alternately
                for (i, quasi) in template.quasis.iter().enumerate() {
                    let raw_str = quasi.value.raw.as_str();
                    if !raw_str.is_empty() {
                        parts.push(TemplateLiteralPart::String(raw_str.to_string()));
                    }
                    if i < template.types.len() {
                        parts.push(TemplateLiteralPart::Type(
                            self.convert_type(&template.types[i]),
                        ));
                    }
                }

                DtsType::TemplateLiteral(parts)
            }

            TSType::TSConstructorType(ctor) => {
                // Constructor types like `new (args) => T`
                let type_params = self.convert_type_params(ctor.type_parameters.as_ref());
                let params = self.convert_formal_params(&ctor.params);
                let return_type = self.convert_type(&ctor.return_type.type_annotation);

                DtsType::Function(Box::new(HuskFunctionType {
                    type_params,
                    params,
                    return_type: Box::new(return_type),
                    this_param: None,
                }))
            }

            TSType::TSImportType(_) => {
                // import("module").Type - fallback to Any for now
                // TODO: Use the Resolver to actually look up this type from the imported module
                DtsType::Primitive(Primitive::Any)
            }

            TSType::TSNamedTupleMember(named) => {
                // Named tuple member used as a type - extract the inner element type
                // element_type is a TSTupleElement, so we need to convert it
                self.convert_tuple_element(&named.element_type).ty
            }

            TSType::JSDocNullableType(_)
            | TSType::JSDocNonNullableType(_)
            | TSType::JSDocUnknownType(_) => {
                // JSDoc types - fallback to Any
                DtsType::Primitive(Primitive::Any)
            }

            TSType::TSIntrinsicKeyword(_) => {
                // intrinsic keyword - used in TS lib for uppercase/lowercase/etc
                DtsType::Primitive(Primitive::Any)
            }

            TSType::TSTypePredicate(_) => {
                // Type predicate (x is T) - treat as boolean for return type purposes
                DtsType::Primitive(Primitive::Boolean)
            }
        }
    }

    fn convert_mapped_modifier(&self, modifier: TSMappedTypeModifierOperator) -> MappedModifier {
        match modifier {
            TSMappedTypeModifierOperator::True => MappedModifier::Preserve,
            TSMappedTypeModifierOperator::Plus => MappedModifier::Add,
            TSMappedTypeModifierOperator::Minus => MappedModifier::Remove,
        }
    }

    fn convert_tuple_element(&self, elem: &TSTupleElement<'_>) -> TupleElement {
        match elem {
            TSTupleElement::TSOptionalType(opt) => TupleElement {
                ty: self.convert_type(&opt.type_annotation),
                name: None,
                optional: true,
                rest: false,
            },
            TSTupleElement::TSRestType(rest) => TupleElement {
                ty: self.convert_type(&rest.type_annotation),
                name: None,
                optional: false,
                rest: true,
            },
            TSTupleElement::TSNamedTupleMember(named) => {
                // element_type is a TSTupleElement, recurse to get the underlying type
                let inner = self.convert_tuple_element(&named.element_type);
                TupleElement {
                    ty: inner.ty,
                    name: Some(named.label.name.to_string()),
                    optional: named.optional,
                    rest: inner.rest,
                }
            }
            // Other TSTupleElement variants are TSType variants
            other => {
                // Try to convert as a TSType - TSTupleElement deref to TSType for most variants
                // We need to pattern match on specific types that ARE TSType-compatible
                let ty = self.convert_tuple_element_as_type(other);
                TupleElement {
                    ty,
                    name: None,
                    optional: false,
                    rest: false,
                }
            }
        }
    }

    fn convert_tuple_element_as_type(&self, elem: &TSTupleElement<'_>) -> DtsType {
        // TSTupleElement variants that map directly to TSType
        match elem {
            TSTupleElement::TSOptionalType(opt) => self.convert_type(&opt.type_annotation),
            TSTupleElement::TSRestType(rest) => self.convert_type(&rest.type_annotation),
            TSTupleElement::TSNamedTupleMember(named) => {
                self.convert_tuple_element(&named.element_type).ty
            }
            // All other variants are actual TSType variants wrapped in TSTupleElement
            // The TSTupleElement enum includes all TSType variants
            other => {
                // Safety: TSTupleElement is a superset of TSType with extra variants
                // For non-special variants, we can use the Into impl or match exhaustively
                // Let's handle the common cases:
                match other {
                    TSTupleElement::TSAnyKeyword(_) => DtsType::Primitive(Primitive::Any),
                    TSTupleElement::TSBigIntKeyword(_) => DtsType::Primitive(Primitive::BigInt),
                    TSTupleElement::TSBooleanKeyword(_) => DtsType::Primitive(Primitive::Boolean),
                    TSTupleElement::TSNeverKeyword(_) => DtsType::Primitive(Primitive::Never),
                    TSTupleElement::TSNullKeyword(_) => DtsType::Primitive(Primitive::Null),
                    TSTupleElement::TSNumberKeyword(_) => DtsType::Primitive(Primitive::Number),
                    TSTupleElement::TSObjectKeyword(_) => DtsType::Primitive(Primitive::Object),
                    TSTupleElement::TSStringKeyword(_) => DtsType::Primitive(Primitive::String),
                    TSTupleElement::TSSymbolKeyword(_) => DtsType::Primitive(Primitive::Symbol),
                    TSTupleElement::TSUndefinedKeyword(_) => DtsType::Primitive(Primitive::Undefined),
                    TSTupleElement::TSUnknownKeyword(_) => DtsType::Primitive(Primitive::Unknown),
                    TSTupleElement::TSVoidKeyword(_) => DtsType::Primitive(Primitive::Void),
                    TSTupleElement::TSThisType(_) => DtsType::This,
                    TSTupleElement::TSTypeReference(type_ref) => {
                        let name = self.type_name_to_string(&type_ref.type_name);
                        let type_args = type_ref
                            .type_arguments
                            .as_ref()
                            .map(|params| params.params.iter().map(|t| self.convert_type(t)).collect())
                            .unwrap_or_default();
                        DtsType::Named { name, type_args }
                    }
                    TSTupleElement::TSUnionType(union) => {
                        let types: Vec<DtsType> = union.types.iter().map(|t| self.convert_type(t)).collect();
                        DtsType::Union(types)
                    }
                    TSTupleElement::TSArrayType(arr) => {
                        let element = self.convert_type(&arr.element_type);
                        DtsType::Array(Box::new(element))
                    }
                    TSTupleElement::TSLiteralType(lit) => match &lit.literal {
                        TSLiteral::StringLiteral(s) => DtsType::StringLiteral(s.value.to_string()),
                        TSLiteral::NumericLiteral(n) => {
                            let raw_str = n
                                .raw
                                .as_ref()
                                .map(|r| r.as_str().to_string())
                                .unwrap_or_else(|| n.value.to_string());
                            DtsType::NumberLiteral(raw_str)
                        }
                        TSLiteral::BooleanLiteral(b) => DtsType::BooleanLiteral(b.value),
                        _ => DtsType::Primitive(Primitive::Any),
                    },
                    _ => DtsType::Primitive(Primitive::Any),
                }
            }
        }
    }

    fn convert_type_params(
        &self,
        params: Option<&oxc_allocator::Box<'_, TSTypeParameterDeclaration<'_>>>,
    ) -> Vec<TypeParam> {
        params
            .map(|p| {
                p.params
                    .iter()
                    .map(|tp| TypeParam {
                        name: tp.name.name.to_string(),
                        constraint: tp.constraint.as_ref().map(|c| self.convert_type(c)),
                        default: tp.default.as_ref().map(|d| self.convert_type(d)),
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    fn convert_formal_params(&self, params: &FormalParameters<'_>) -> Vec<Param> {
        params
            .items
            .iter()
            .map(|p| {
                let name = self.binding_pattern_name(&p.pattern);
                let optional = p.pattern.optional;
                let ty = p
                    .pattern
                    .type_annotation
                    .as_ref()
                    .map(|ta| self.convert_type(&ta.type_annotation))
                    .unwrap_or(DtsType::Primitive(Primitive::Any));

                Param {
                    name,
                    ty,
                    optional,
                    rest: false,
                }
            })
            .chain(params.rest.iter().map(|rest| {
                let name = self.binding_pattern_name(&rest.argument);
                let ty = rest
                    .argument
                    .type_annotation
                    .as_ref()
                    .map(|ta| self.convert_type(&ta.type_annotation))
                    .unwrap_or(DtsType::Primitive(Primitive::Any));

                Param {
                    name,
                    ty,
                    optional: false,
                    rest: true,
                }
            }))
            .collect()
    }

    fn binding_pattern_name(&self, pattern: &BindingPattern<'_>) -> String {
        match &pattern.kind {
            BindingPatternKind::BindingIdentifier(id) => id.name.to_string(),
            BindingPatternKind::ObjectPattern(_) => "_obj".to_string(),
            BindingPatternKind::ArrayPattern(_) => "_arr".to_string(),
            BindingPatternKind::AssignmentPattern(assign) => {
                self.binding_pattern_name(&assign.left)
            }
        }
    }

    fn type_name_to_string(&self, type_name: &TSTypeName<'_>) -> String {
        match type_name {
            TSTypeName::IdentifierReference(id) => id.name.to_string(),
            TSTypeName::QualifiedName(qualified) => {
                let left = self.type_name_to_string(&qualified.left);
                let right = qualified.right.name.to_string();
                format!("{}.{}", left, right)
            }
            TSTypeName::ThisExpression(_) => "this".to_string(),
        }
    }

    fn expr_name_to_string(&self, expr_name: &TSTypeQueryExprName<'_>) -> String {
        match expr_name {
            TSTypeQueryExprName::IdentifierReference(id) => id.name.to_string(),
            TSTypeQueryExprName::QualifiedName(qualified) => {
                let left = self.type_name_to_string(&qualified.left);
                let right = qualified.right.name.to_string();
                format!("{}.{}", left, right)
            }
            TSTypeQueryExprName::TSImportType(_) => "import".to_string(),
            TSTypeQueryExprName::ThisExpression(_) => "this".to_string(),
        }
    }

    fn convert_type_element(&self, member: &TSSignature<'_>) -> Option<ObjectMember> {
        match member {
            TSSignature::TSPropertySignature(prop) => {
                let name = match &prop.key {
                    PropertyKey::StaticIdentifier(id) => id.name.to_string(),
                    PropertyKey::StringLiteral(s) => s.value.to_string(),
                    PropertyKey::NumericLiteral(n) => n.value.to_string(),
                    _ => return None,
                };

                let ty = prop
                    .type_annotation
                    .as_ref()
                    .map(|ta| self.convert_type(&ta.type_annotation))
                    .unwrap_or(DtsType::Primitive(Primitive::Any));

                Some(ObjectMember::Property {
                    name,
                    ty,
                    optional: prop.optional,
                    readonly: prop.readonly,
                })
            }

            TSSignature::TSMethodSignature(method) => {
                let name = match &method.key {
                    PropertyKey::StaticIdentifier(id) => id.name.to_string(),
                    PropertyKey::StringLiteral(s) => s.value.to_string(),
                    _ => return None,
                };

                let type_params = self.convert_type_params(method.type_parameters.as_ref());
                let params = self.convert_formal_params(&method.params);
                let return_type = method
                    .return_type
                    .as_ref()
                    .map(|rt| self.convert_type(&rt.type_annotation));

                Some(ObjectMember::Method {
                    name,
                    type_params,
                    params,
                    return_type,
                    optional: method.optional,
                })
            }

            TSSignature::TSCallSignatureDeclaration(call) => {
                let type_params = self.convert_type_params(call.type_parameters.as_ref());
                let params = self.convert_formal_params(&call.params);
                let return_type = call
                    .return_type
                    .as_ref()
                    .map(|rt| self.convert_type(&rt.type_annotation));

                Some(ObjectMember::CallSignature(CallSignature {
                    type_params,
                    params,
                    return_type,
                }))
            }

            TSSignature::TSConstructSignatureDeclaration(ctor) => {
                let type_params = self.convert_type_params(ctor.type_parameters.as_ref());
                let params = self.convert_formal_params(&ctor.params);
                let return_type = ctor
                    .return_type
                    .as_ref()
                    .map(|rt| self.convert_type(&rt.type_annotation));

                Some(ObjectMember::ConstructSignature(ConstructSignature {
                    type_params,
                    params,
                    return_type,
                }))
            }

            TSSignature::TSIndexSignature(idx) => {
                // Index signature: [key: KeyType]: ValueType
                let (key_name, key_type) = if let Some(param) = idx.parameters.first() {
                    let name = param.name.to_string();
                    let ty = self.convert_type(&param.type_annotation.type_annotation);
                    (name, ty)
                } else {
                    ("key".to_string(), DtsType::Primitive(Primitive::String))
                };

                let value_type = self.convert_type(&idx.type_annotation.type_annotation);

                Some(ObjectMember::IndexSignature(IndexSignature {
                    key_name,
                    key_type,
                    value_type,
                    readonly: idx.readonly,
                }))
            }
        }
    }

    fn convert_function_declaration(&self, decl: &Function<'_>) -> Option<DtsFunction> {
        let name = decl.id.as_ref()?.name.to_string();
        let type_params = self.convert_type_params(decl.type_parameters.as_ref());
        let params = self.convert_formal_params(&decl.params);
        let return_type = decl
            .return_type
            .as_ref()
            .map(|rt| self.convert_type(&rt.type_annotation));

        Some(DtsFunction {
            name,
            type_params,
            params,
            return_type,
        })
    }

    fn convert_interface_declaration(
        &self,
        decl: &TSInterfaceDeclaration<'_>,
    ) -> DtsInterface {
        let name = decl.id.name.to_string();
        let type_params = self.convert_type_params(decl.type_parameters.as_ref());

        let extends: Vec<DtsType> = decl
            .extends
            .iter()
            .map(|heritage| {
                let name = match &heritage.expression {
                    Expression::Identifier(id) => id.name.to_string(),
                    _ => "Unknown".to_string(),
                };
                let type_args = heritage
                    .type_arguments
                    .as_ref()
                    .map(|params| {
                        params.params.iter().map(|t| self.convert_type(t)).collect()
                    })
                    .unwrap_or_default();
                DtsType::Named { name, type_args }
            })
            .collect();

        let members: Vec<InterfaceMember> = decl
            .body
            .body
            .iter()
            .filter_map(|sig| self.convert_interface_member(sig))
            .collect();

        DtsInterface {
            name,
            type_params,
            extends,
            members,
        }
    }

    fn convert_interface_member(&self, sig: &TSSignature<'_>) -> Option<InterfaceMember> {
        match sig {
            TSSignature::TSPropertySignature(prop) => {
                let name = match &prop.key {
                    PropertyKey::StaticIdentifier(id) => id.name.to_string(),
                    PropertyKey::StringLiteral(s) => s.value.to_string(),
                    PropertyKey::NumericLiteral(n) => n.value.to_string(),
                    _ => return None,
                };

                let ty = prop
                    .type_annotation
                    .as_ref()
                    .map(|ta| self.convert_type(&ta.type_annotation))
                    .unwrap_or(DtsType::Primitive(Primitive::Any));

                Some(InterfaceMember::Property(PropertyMember {
                    name,
                    ty,
                    optional: prop.optional,
                    readonly: prop.readonly,
                }))
            }

            TSSignature::TSMethodSignature(method) => {
                let name = match &method.key {
                    PropertyKey::StaticIdentifier(id) => id.name.to_string(),
                    PropertyKey::StringLiteral(s) => s.value.to_string(),
                    _ => return None,
                };

                let type_params = self.convert_type_params(method.type_parameters.as_ref());
                let params = self.convert_formal_params(&method.params);
                let return_type = method
                    .return_type
                    .as_ref()
                    .map(|rt| self.convert_type(&rt.type_annotation));

                Some(InterfaceMember::Method(MethodMember {
                    name,
                    type_params,
                    params,
                    return_type,
                    optional: method.optional,
                }))
            }

            TSSignature::TSCallSignatureDeclaration(call) => {
                let type_params = self.convert_type_params(call.type_parameters.as_ref());
                let params = self.convert_formal_params(&call.params);
                let return_type = call
                    .return_type
                    .as_ref()
                    .map(|rt| self.convert_type(&rt.type_annotation));

                Some(InterfaceMember::CallSignature(CallSignature {
                    type_params,
                    params,
                    return_type,
                }))
            }

            TSSignature::TSConstructSignatureDeclaration(ctor) => {
                let type_params = self.convert_type_params(ctor.type_parameters.as_ref());
                let params = self.convert_formal_params(&ctor.params);
                let return_type = ctor
                    .return_type
                    .as_ref()
                    .map(|rt| self.convert_type(&rt.type_annotation));

                Some(InterfaceMember::ConstructSignature(ConstructSignature {
                    type_params,
                    params,
                    return_type,
                }))
            }

            TSSignature::TSIndexSignature(idx) => {
                let (key_name, key_type) = if let Some(param) = idx.parameters.first() {
                    let name = param.name.to_string();
                    let ty = self.convert_type(&param.type_annotation.type_annotation);
                    (name, ty)
                } else {
                    ("key".to_string(), DtsType::Primitive(Primitive::String))
                };

                let value_type = self.convert_type(&idx.type_annotation.type_annotation);

                Some(InterfaceMember::IndexSignature(IndexSignature {
                    key_name,
                    key_type,
                    value_type,
                    readonly: idx.readonly,
                }))
            }
        }
    }

    fn convert_class_declaration(&self, decl: &Class<'_>) -> Option<DtsClass> {
        let name = decl.id.as_ref()?.name.to_string();
        let type_params = self.convert_type_params(decl.type_parameters.as_ref());

        let extends = decl.super_class.as_ref().map(|sc| {
            if let Expression::Identifier(id) = sc {
                let type_args = decl
                    .super_type_arguments
                    .as_ref()
                    .map(|params| params.params.iter().map(|t| self.convert_type(t)).collect())
                    .unwrap_or_default();
                DtsType::Named {
                    name: id.name.to_string(),
                    type_args,
                }
            } else {
                DtsType::Primitive(Primitive::Object)
            }
        });

        let implements: Vec<DtsType> = decl
            .implements
            .iter()
            .map(|impl_decl| {
                let name = self.type_name_to_string(&impl_decl.expression);
                let type_args = impl_decl
                    .type_arguments
                    .as_ref()
                    .map(|params| {
                        params.params.iter().map(|t| self.convert_type(t)).collect()
                    })
                    .unwrap_or_default();
                DtsType::Named { name, type_args }
            })
            .collect();

        let members: Vec<ClassMember> = decl
            .body
            .body
            .iter()
            .filter_map(|elem| self.convert_class_element(elem))
            .collect();

        Some(DtsClass {
            name,
            type_params,
            extends,
            implements,
            members,
        })
    }

    fn convert_class_element(&self, elem: &ClassElement<'_>) -> Option<ClassMember> {
        match elem {
            ClassElement::PropertyDefinition(prop) => {
                let name = match &prop.key {
                    PropertyKey::StaticIdentifier(id) => id.name.to_string(),
                    PropertyKey::StringLiteral(s) => s.value.to_string(),
                    _ => return None,
                };

                let ty = prop
                    .type_annotation
                    .as_ref()
                    .map(|ta| self.convert_type(&ta.type_annotation));

                let visibility = match prop.accessibility {
                    Some(TSAccessibility::Public) | None => Visibility::Public,
                    Some(TSAccessibility::Protected) => Visibility::Protected,
                    Some(TSAccessibility::Private) => Visibility::Private,
                };

                Some(ClassMember::Property(ClassProperty {
                    name,
                    ty,
                    optional: prop.optional,
                    readonly: prop.readonly,
                    is_static: prop.r#static,
                    visibility,
                }))
            }

            ClassElement::MethodDefinition(method) => {
                let name = match &method.key {
                    PropertyKey::StaticIdentifier(id) => id.name.to_string(),
                    PropertyKey::StringLiteral(s) => s.value.to_string(),
                    _ => return None,
                };

                // Handle constructor
                if method.kind == MethodDefinitionKind::Constructor {
                    let type_params =
                        self.convert_type_params(method.value.type_parameters.as_ref());
                    let params = self.convert_formal_params(&method.value.params);
                    let return_type = method
                        .value
                        .return_type
                        .as_ref()
                        .map(|rt| self.convert_type(&rt.type_annotation));

                    return Some(ClassMember::Constructor(ConstructSignature {
                        type_params,
                        params,
                        return_type,
                    }));
                }

                let type_params = self.convert_type_params(method.value.type_parameters.as_ref());
                let params = self.convert_formal_params(&method.value.params);
                let return_type = method
                    .value
                    .return_type
                    .as_ref()
                    .map(|rt| self.convert_type(&rt.type_annotation));

                let visibility = match method.accessibility {
                    Some(TSAccessibility::Public) | None => Visibility::Public,
                    Some(TSAccessibility::Protected) => Visibility::Protected,
                    Some(TSAccessibility::Private) => Visibility::Private,
                };

                Some(ClassMember::Method(ClassMethod {
                    name,
                    type_params,
                    params,
                    return_type,
                    is_static: method.r#static,
                    visibility,
                }))
            }

            ClassElement::TSIndexSignature(idx) => {
                let (key_name, key_type) = if let Some(param) = idx.parameters.first() {
                    let name = param.name.to_string();
                    let ty = self.convert_type(&param.type_annotation.type_annotation);
                    (name, ty)
                } else {
                    ("key".to_string(), DtsType::Primitive(Primitive::String))
                };

                let value_type = self.convert_type(&idx.type_annotation.type_annotation);

                Some(ClassMember::IndexSignature(IndexSignature {
                    key_name,
                    key_type,
                    value_type,
                    readonly: idx.readonly,
                }))
            }

            ClassElement::AccessorProperty(_) | ClassElement::StaticBlock(_) => {
                // Not handled in basic conversion
                None
            }
        }
    }

    fn convert_type_alias(&self, decl: &TSTypeAliasDeclaration<'_>) -> DtsTypeAlias {
        DtsTypeAlias {
            name: decl.id.name.to_string(),
            type_params: self.convert_type_params(decl.type_parameters.as_ref()),
            ty: self.convert_type(&decl.type_annotation),
        }
    }

    fn convert_variable_declaration(&self, decl: &VariableDeclaration<'_>) -> Vec<DtsVariable> {
        let is_const = decl.kind == VariableDeclarationKind::Const;

        decl.declarations
            .iter()
            .filter_map(|d| {
                let name = self.binding_pattern_name(&d.id);
                let ty = d
                    .id
                    .type_annotation
                    .as_ref()
                    .map(|ta| self.convert_type(&ta.type_annotation))
                    .unwrap_or(DtsType::Primitive(Primitive::Any));

                Some(DtsVariable { name, ty, is_const })
            })
            .collect()
    }

    fn convert_module_declaration(
        &self,
        decl: &TSModuleDeclaration<'_>,
    ) -> Option<DtsItem> {
        // Check if it's an ambient module (string literal name)
        let (name, is_ambient) = match &decl.id {
            TSModuleDeclarationName::Identifier(id) => (id.name.to_string(), false),
            TSModuleDeclarationName::StringLiteral(s) => (s.value.to_string(), true),
        };

        let items: Vec<DtsItem> = if let Some(body) = &decl.body {
            self.collect_module_body_items(body)
        } else {
            Vec::new()
        };

        // Decide if it's a namespace or module based on the declaration type
        // Ambient modules use string literal names: declare module "express" { }
        // Namespaces use identifier names: declare namespace Foo { }
        if is_ambient {
            Some(DtsItem::Module(DtsModule {
                name,
                items,
                is_ambient: true,
            }))
        } else {
            Some(DtsItem::Namespace(DtsNamespace { name, items }))
        }
    }

    fn collect_module_body_items(&self, body: &TSModuleDeclarationBody<'_>) -> Vec<DtsItem> {
        match body {
            TSModuleDeclarationBody::TSModuleDeclaration(nested) => {
                if let Some(item) = self.convert_module_declaration(nested) {
                    vec![item]
                } else {
                    Vec::new()
                }
            }
            TSModuleDeclarationBody::TSModuleBlock(block) => {
                block
                    .body
                    .iter()
                    .flat_map(|stmt| self.convert_statement_to_items(stmt))
                    .collect()
            }
        }
    }

    fn convert_statement_to_items(&self, stmt: &Statement<'_>) -> Vec<DtsItem> {
        match stmt {
            Statement::TSInterfaceDeclaration(decl) => {
                vec![DtsItem::Interface(self.convert_interface_declaration(decl))]
            }
            Statement::TSTypeAliasDeclaration(decl) => {
                vec![DtsItem::TypeAlias(self.convert_type_alias(decl))]
            }
            Statement::ClassDeclaration(decl) => self
                .convert_class_declaration(decl)
                .map(|c| vec![DtsItem::Class(c)])
                .unwrap_or_default(),
            Statement::FunctionDeclaration(decl) => self
                .convert_function_declaration(decl)
                .map(|f| vec![DtsItem::Function(f)])
                .unwrap_or_default(),
            Statement::VariableDeclaration(decl) => self
                .convert_variable_declaration(decl)
                .into_iter()
                .map(DtsItem::Variable)
                .collect(),
            Statement::TSModuleDeclaration(decl) => self
                .convert_module_declaration(decl)
                .into_iter()
                .collect(),
            Statement::ExportNamedDeclaration(export) => {
                if let Some(decl) = &export.declaration {
                    self.convert_declaration_to_items(decl)
                } else {
                    vec![]
                }
            }
            Statement::ExportDefaultDeclaration(export) => match &export.declaration {
                ExportDefaultDeclarationKind::FunctionDeclaration(decl) => self
                    .convert_function_declaration(decl)
                    .map(|f| vec![DtsItem::Function(f)])
                    .unwrap_or_default(),
                ExportDefaultDeclarationKind::ClassDeclaration(decl) => self
                    .convert_class_declaration(decl)
                    .map(|c| vec![DtsItem::Class(c)])
                    .unwrap_or_default(),
                ExportDefaultDeclarationKind::TSInterfaceDeclaration(decl) => {
                    vec![DtsItem::Interface(self.convert_interface_declaration(decl))]
                }
                _ => vec![],
            },
            Statement::TSExportAssignment(assign) => {
                if let Expression::Identifier(id) = &assign.expression {
                    vec![DtsItem::Export(DtsExport::Equals(id.name.to_string()))]
                } else {
                    vec![]
                }
            }
            _ => vec![],
        }
    }

    fn convert_declaration_to_items(&self, decl: &Declaration<'_>) -> Vec<DtsItem> {
        match decl {
            Declaration::FunctionDeclaration(func) => self
                .convert_function_declaration(func)
                .map(|f| vec![DtsItem::Function(f)])
                .unwrap_or_default(),
            Declaration::ClassDeclaration(class) => self
                .convert_class_declaration(class)
                .map(|c| vec![DtsItem::Class(c)])
                .unwrap_or_default(),
            Declaration::VariableDeclaration(var) => self
                .convert_variable_declaration(var)
                .into_iter()
                .map(DtsItem::Variable)
                .collect(),
            Declaration::TSInterfaceDeclaration(iface) => {
                vec![DtsItem::Interface(self.convert_interface_declaration(iface))]
            }
            Declaration::TSTypeAliasDeclaration(alias) => {
                vec![DtsItem::TypeAlias(self.convert_type_alias(alias))]
            }
            Declaration::TSModuleDeclaration(module) => {
                self.convert_module_declaration(module).into_iter().collect()
            }
            Declaration::TSEnumDeclaration(_) => vec![], // Enums not yet supported
            Declaration::TSImportEqualsDeclaration(_) => vec![],
        }
    }
}

impl<'a> Visit<'a> for DtsVisitor<'a> {
    fn visit_program(&mut self, program: &Program<'a>) {
        // Process all statements in the program
        for stmt in &program.body {
            self.items.extend(self.convert_statement_to_items(stmt));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_function() {
        let src = "declare function add(a: number, b: number): number;";
        let file = parse_with_oxc(src, "test.d.ts").unwrap();

        assert_eq!(file.items.len(), 1);
        if let DtsItem::Function(f) = &file.items[0] {
            assert_eq!(f.name, "add");
            assert_eq!(f.params.len(), 2);
            assert!(matches!(
                f.return_type,
                Some(DtsType::Primitive(Primitive::Number))
            ));
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn test_parse_interface() {
        let src = r#"
            interface Person {
                name: string;
                age?: number;
            }
        "#;
        let file = parse_with_oxc(src, "test.d.ts").unwrap();

        if let DtsItem::Interface(i) = &file.items[0] {
            assert_eq!(i.name, "Person");
            assert_eq!(i.members.len(), 2);
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn test_parse_generic_type() {
        let src = "type Callback<T> = (value: T) => void;";
        let file = parse_with_oxc(src, "test.d.ts").unwrap();

        if let DtsItem::TypeAlias(t) = &file.items[0] {
            assert_eq!(t.name, "Callback");
            assert_eq!(t.type_params.len(), 1);
        } else {
            panic!("expected type alias");
        }
    }
}
