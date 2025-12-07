//! Convert an Oxc TypeScript AST into the Husk `DtsFile` representation.
//!
//! This is intentionally partial: it handles the common declaration shapes we
//! rely on for early importer work (interfaces, type aliases, declared
//! functions/variables, and a core set of types). Unsupported shapes degrade to
//! `any` so downstream codegen can continue.

use crate::ast::{FunctionType as HuskFunctionType, *};
use oxc_ast::ast as oxc;

/// Convert a parsed Oxc program into a Husk `DtsFile`.
pub fn convert_program(program: &oxc::Program<'_>) -> Result<DtsFile, Vec<String>> {
    let mut items = Vec::new();
    let mut errors = Vec::new();

    for stmt in &program.body {
        if let Some(converted) = convert_statement(stmt, &mut errors) {
            match converted {
                Converted::Item(it) => items.push(it),
                Converted::Items(mut many) => items.append(&mut many),
            }
        }
    }

    if errors.is_empty() {
        Ok(DtsFile { items })
    } else {
        Err(errors)
    }
}

enum Converted {
    Item(DtsItem),
    Items(Vec<DtsItem>),
}

fn convert_statement(stmt: &oxc::Statement<'_>, errors: &mut Vec<String>) -> Option<Converted> {
    match stmt {
        oxc::Statement::TSTypeAliasDeclaration(alias) => Some(Converted::Item(DtsItem::TypeAlias(
            convert_type_alias(alias),
        ))),
        oxc::Statement::TSInterfaceDeclaration(interface) => Some(Converted::Item(
            DtsItem::Interface(convert_interface(interface)),
        )),
        oxc::Statement::ClassDeclaration(class) if class.declare => {
            convert_class(class).map(|c| Converted::Item(DtsItem::Class(c)))
        }
        oxc::Statement::TSEnumDeclaration(en) => {
            Some(Converted::Item(DtsItem::TypeAlias(convert_enum(en))))
        }
        oxc::Statement::FunctionDeclaration(func) if func.declare => {
            if let Some(fun) = convert_function_decl(func) {
                Some(Converted::Item(DtsItem::Function(fun)))
            } else {
                errors.push(format!(
                    "skipping function without identifier at {:?}",
                    func.span
                ));
                None
            }
        }
        oxc::Statement::VariableDeclaration(var) if var.declare => {
            Some(Converted::Items(convert_declared_vars(var)))
        }
        oxc::Statement::TSModuleDeclaration(module) if module.declare => {
            convert_module(module, errors).map(Converted::Item)
        }
        oxc::Statement::ExportNamedDeclaration(export) => {
            let mut items = Vec::new();
            if let Some(decl) = &export.declaration {
                if let Some(conv) = convert_declaration(decl, errors) {
                    match conv {
                        Converted::Item(it) => items.push(it),
                        Converted::Items(mut many) => items.append(&mut many),
                    }
                }
            }

            if !export.specifiers.is_empty() {
                let named = export
                    .specifiers
                    .iter()
                    .map(|spec| {
                        let orig = module_export_name(&spec.local);
                        let alias = module_export_name(&spec.exported);
                        let alias_opt = if orig == alias { None } else { Some(alias) };
                        (orig, alias_opt)
                    })
                    .collect::<Vec<_>>();
                if !named.is_empty() {
                    items.push(DtsItem::Export(DtsExport::Named(named)));
                }
            }

            if items.is_empty() {
                None
            } else if items.len() == 1 {
                Some(Converted::Item(items.remove(0)))
            } else {
                Some(Converted::Items(items))
            }
        }
        oxc::Statement::TSExportAssignment(assign) => {
            if let Some(name) = expr_to_name(&assign.expression) {
                Some(Converted::Item(DtsItem::Export(DtsExport::Equals(name))))
            } else {
                None
            }
        }
        oxc::Statement::ExportDefaultDeclaration(export) => match &export.declaration {
            oxc::ExportDefaultDeclarationKind::Identifier(id) => Some(Converted::Item(
                DtsItem::Export(DtsExport::Default(id.name.to_string())),
            )),
            _ => None,
        },
        oxc::Statement::ExportAllDeclaration(decl) => {
            let star = "*".to_string();
            let alias = decl.exported.as_ref().map(module_export_name);
            Some(Converted::Item(DtsItem::Export(DtsExport::Named(vec![(
                star, alias,
            )]))))
        }
        oxc::Statement::TSNamespaceExportDeclaration(ns) => Some(Converted::Item(DtsItem::Export(
            DtsExport::Named(vec![(ns.id.name.to_string(), None)]),
        ))),
        _ => None,
    }
}

fn convert_declaration(decl: &oxc::Declaration<'_>, errors: &mut Vec<String>) -> Option<Converted> {
    match decl {
        oxc::Declaration::TSTypeAliasDeclaration(alias) => Some(Converted::Item(
            DtsItem::TypeAlias(convert_type_alias(alias)),
        )),
        oxc::Declaration::TSInterfaceDeclaration(interface) => Some(Converted::Item(
            DtsItem::Interface(convert_interface(interface)),
        )),
        oxc::Declaration::ClassDeclaration(class) if class.declare => {
            convert_class(class).map(|c| Converted::Item(DtsItem::Class(c)))
        }
        oxc::Declaration::TSEnumDeclaration(en) => {
            Some(Converted::Item(DtsItem::TypeAlias(convert_enum(en))))
        }
        oxc::Declaration::FunctionDeclaration(func) if func.declare => {
            if let Some(fun) = convert_function_decl(func) {
                Some(Converted::Item(DtsItem::Function(fun)))
            } else {
                errors.push(format!(
                    "skipping function without identifier at {:?}",
                    func.span
                ));
                None
            }
        }
        oxc::Declaration::VariableDeclaration(var) if var.declare => {
            Some(Converted::Items(convert_declared_vars(var)))
        }
        oxc::Declaration::TSModuleDeclaration(module) if module.declare => {
            convert_module(module, errors).map(Converted::Item)
        }
        _ => None,
    }
}

fn module_export_name(name: &oxc::ModuleExportName<'_>) -> String {
    match name {
        oxc::ModuleExportName::IdentifierName(id) => id.name.to_string(),
        oxc::ModuleExportName::IdentifierReference(id) => id.name.to_string(),
        oxc::ModuleExportName::StringLiteral(lit) => lit.value.to_string(),
    }
}

fn map_access(acc: Option<oxc::TSAccessibility>) -> Visibility {
    match acc {
        Some(oxc::TSAccessibility::Private) => Visibility::Private,
        Some(oxc::TSAccessibility::Protected) => Visibility::Protected,
        _ => Visibility::Public,
    }
}

fn convert_type_alias(alias: &oxc::TSTypeAliasDeclaration<'_>) -> DtsTypeAlias {
    DtsTypeAlias {
        name: alias.id.name.to_string(),
        type_params: convert_type_params(alias.type_parameters.as_deref()),
        ty: convert_type(&alias.type_annotation),
    }
}

fn convert_interface(interface: &oxc::TSInterfaceDeclaration<'_>) -> DtsInterface {
    let members = interface
        .body
        .body
        .iter()
        .filter_map(convert_signature)
        .collect();

    DtsInterface {
        name: interface.id.name.to_string(),
        type_params: convert_type_params(interface.type_parameters.as_deref()),
        extends: interface
            .extends
            .iter()
            .map(convert_interface_heritage)
            .collect(),
        members,
    }
}

fn convert_interface_heritage(heritage: &oxc::TSInterfaceHeritage<'_>) -> DtsType {
    if let Some(name) = expr_to_name(&heritage.expression) {
        DtsType::Named {
            name,
            type_args: heritage
                .type_arguments
                .as_ref()
                .map(|args| args.params.iter().map(convert_type).collect())
                .unwrap_or_default(),
        }
    } else {
        DtsType::Primitive(Primitive::Any)
    }
}

fn convert_function_decl(func: &oxc::Function<'_>) -> Option<DtsFunction> {
    let name = func.id.as_ref()?.name.to_string();
    let type_params = convert_type_params(func.type_parameters.as_deref());
    let params = convert_params(&func.params);
    let this_param = func
        .this_param
        .as_ref()
        .and_then(|t| t.type_annotation.as_ref())
        .map(|ann| convert_type(&ann.type_annotation));
    let return_type = convert_type(&func.return_type.as_ref()?.type_annotation);

    let mut all_params = params;
    if let Some(this_ty) = this_param {
        all_params.insert(
            0,
            Param {
                name: "this".into(),
                ty: this_ty,
                optional: false,
                rest: false,
            },
        );
    }

    Some(DtsFunction {
        name,
        type_params,
        params: all_params,
        return_type: Some(return_type),
    })
}

fn convert_declared_vars(var: &oxc::VariableDeclaration<'_>) -> Vec<DtsItem> {
    let mut out = Vec::new();
    for decl in &var.declarations {
        if let oxc::BindingPatternKind::BindingIdentifier(id) = &decl.id.kind {
            let ty = decl
                .id
                .type_annotation
                .as_ref()
                .map(|ann| convert_type(&ann.type_annotation))
                .unwrap_or(DtsType::Primitive(Primitive::Any));
            out.push(DtsItem::Variable(DtsVariable {
                name: id.name.to_string(),
                ty,
                is_const: matches!(var.kind, oxc::VariableDeclarationKind::Const),
            }));
        }
    }
    out
}

fn convert_module(
    module: &oxc::TSModuleDeclaration<'_>,
    errors: &mut Vec<String>,
) -> Option<DtsItem> {
    let name = match &module.id {
        oxc::TSModuleDeclarationName::Identifier(id) => id.name.to_string(),
        oxc::TSModuleDeclarationName::StringLiteral(lit) => lit.value.to_string(),
    };

    let mut items = Vec::new();
    if let Some(body) = &module.body {
        match body {
            oxc::TSModuleDeclarationBody::TSModuleBlock(block) => {
                for stmt in &block.body {
                    if let Some(conv) = convert_statement(stmt, errors) {
                        match conv {
                            Converted::Item(it) => items.push(it),
                            Converted::Items(mut many) => items.append(&mut many),
                        }
                    }
                }
            }
            oxc::TSModuleDeclarationBody::TSModuleDeclaration(inner) => {
                if let Some(item) = convert_module(inner, errors) {
                    items.push(item);
                }
            }
        }
    }

    Some(DtsItem::Module(DtsModule { name, items }))
}

fn convert_class(class: &oxc::Class<'_>) -> Option<DtsClass> {
    let name = class.id.as_ref()?.name.to_string();
    let type_params = convert_type_params(class.type_parameters.as_deref());
    let extends = class
        .super_class
        .as_ref()
        .and_then(expr_to_name)
        .map(|n| DtsType::Named {
            name: n,
            type_args: class
                .super_type_arguments
                .as_ref()
                .map(|args| args.params.iter().map(convert_type).collect())
                .unwrap_or_default(),
        });
    let implements = class
        .implements
        .iter()
        .map(|impl_| DtsType::Named {
            name: convert_ts_type_name(&impl_.expression),
            type_args: impl_
                .type_arguments
                .as_ref()
                .map(|args| args.params.iter().map(convert_type).collect())
                .unwrap_or_default(),
        })
        .collect();

    let mut members = Vec::new();
    for elem in &class.body.body {
        match elem {
            oxc::ClassElement::MethodDefinition(m) => {
                if let Some(name_str) = property_key_to_string(&m.key) {
                    let mut params = convert_params(&m.value.params);
                    if let Some(this_ty) = m
                        .value
                        .this_param
                        .as_ref()
                        .and_then(|t| t.type_annotation.as_ref())
                        .map(|ann| convert_type(&ann.type_annotation))
                    {
                        params.insert(
                            0,
                            Param {
                                name: "this".into(),
                                ty: this_ty,
                                optional: false,
                                rest: false,
                            },
                        );
                    }

                    if m.kind == oxc::MethodDefinitionKind::Constructor {
                        members.push(ClassMember::Constructor(ConstructSignature {
                            type_params: convert_type_params(m.value.type_parameters.as_deref()),
                            params,
                            return_type: Some(DtsType::Named {
                                name: name.clone(),
                                type_args: Vec::new(),
                            }),
                        }));
                    } else {
                        members.push(ClassMember::Method(ClassMethod {
                            name: name_str,
                            type_params: convert_type_params(m.value.type_parameters.as_deref()),
                            params,
                            return_type: m
                                .value
                                .return_type
                                .as_ref()
                                .map(|ann| convert_type(&ann.type_annotation)),
                            is_static: m.r#static,
                            visibility: map_access(m.accessibility),
                        }));
                    }
                }
            }
            oxc::ClassElement::PropertyDefinition(p) => {
                if let Some(name_str) = property_key_to_string(&p.key) {
                    members.push(ClassMember::Property(ClassProperty {
                        name: name_str,
                        ty: p
                            .type_annotation
                            .as_ref()
                            .map(|ann| convert_type(&ann.type_annotation)),
                        optional: p.optional,
                        readonly: p.readonly,
                        is_static: p.r#static,
                        visibility: map_access(p.accessibility),
                    }));
                }
            }
            oxc::ClassElement::TSIndexSignature(idx) => {
                let (key_name, key_type) = idx
                    .parameters
                    .first()
                    .map(convert_index_parameter)
                    .unwrap_or_else(|| ("key".into(), DtsType::Primitive(Primitive::Any)));
                members.push(ClassMember::IndexSignature(IndexSignature {
                    key_name,
                    key_type,
                    value_type: convert_type(&idx.type_annotation.type_annotation),
                    readonly: idx.readonly,
                }));
            }
            _ => {}
        }
    }

    Some(DtsClass {
        name,
        type_params,
        extends,
        implements,
        members,
    })
}

fn convert_enum(en: &oxc::TSEnumDeclaration<'_>) -> DtsTypeAlias {
    let mut variants = Vec::new();
    for member in &en.body.members {
        let val = member.initializer.as_ref().and_then(|init| match init {
            oxc::Expression::StringLiteral(s) => Some(DtsType::StringLiteral(s.value.to_string())),
            oxc::Expression::NumericLiteral(n) => Some(DtsType::NumberLiteral(n.value.to_string())),
            _ => None,
        });
        if let Some(v) = val {
            variants.push(v);
        } else {
            match &member.id {
                oxc::TSEnumMemberName::Identifier(id) => {
                    variants.push(DtsType::StringLiteral(id.name.to_string()))
                }
                oxc::TSEnumMemberName::String(lit) => {
                    variants.push(DtsType::StringLiteral(lit.value.to_string()))
                }
                _ => variants.push(DtsType::Primitive(Primitive::Number)),
            }
        }
    }
    let ty = if variants.is_empty() {
        DtsType::Primitive(Primitive::Any)
    } else if variants.len() == 1 {
        variants.remove(0)
    } else {
        DtsType::Union(variants)
    };

    DtsTypeAlias {
        name: en.id.name.to_string(),
        type_params: Vec::new(),
        ty,
    }
}

fn convert_signature(sig: &oxc::TSSignature<'_>) -> Option<InterfaceMember> {
    match sig {
        oxc::TSSignature::TSPropertySignature(prop) => {
            let name = property_key_to_string(&prop.key)?;
            let ty = prop
                .type_annotation
                .as_ref()
                .map(|ann| convert_type(&ann.type_annotation))
                .unwrap_or(DtsType::Primitive(Primitive::Any));
            Some(InterfaceMember::Property(PropertyMember {
                name,
                ty,
                optional: prop.optional,
                readonly: prop.readonly,
            }))
        }
        oxc::TSSignature::TSMethodSignature(method) => {
            let name = property_key_to_string(&method.key)?;
            let type_params = convert_type_params(method.type_parameters.as_deref());
            let mut params = convert_params(&method.params);
            if let Some(this_ty) = method
                .this_param
                .as_ref()
                .and_then(|t| t.type_annotation.as_ref())
                .map(|ann| convert_type(&ann.type_annotation))
            {
                params.insert(
                    0,
                    Param {
                        name: "this".into(),
                        ty: this_ty,
                        optional: false,
                        rest: false,
                    },
                );
            }
            let return_type = method
                .return_type
                .as_ref()
                .map(|ann| convert_type(&ann.type_annotation));
            Some(InterfaceMember::Method(MethodMember {
                name,
                type_params,
                params,
                return_type,
                optional: method.optional,
            }))
        }
        oxc::TSSignature::TSCallSignatureDeclaration(call) => {
            let type_params = convert_type_params(call.type_parameters.as_deref());
            let mut params = convert_params(&call.params);
            if let Some(this_ty) = call
                .this_param
                .as_ref()
                .and_then(|t| t.type_annotation.as_ref())
                .map(|ann| convert_type(&ann.type_annotation))
            {
                params.insert(
                    0,
                    Param {
                        name: "this".into(),
                        ty: this_ty,
                        optional: false,
                        rest: false,
                    },
                );
            }
            let return_type = call
                .return_type
                .as_ref()
                .map(|ann| convert_type(&ann.type_annotation));
            Some(InterfaceMember::CallSignature(CallSignature {
                type_params,
                params,
                return_type,
            }))
        }
        oxc::TSSignature::TSConstructSignatureDeclaration(cons) => {
            let type_params = convert_type_params(cons.type_parameters.as_deref());
            let params = convert_params(&cons.params);
            let return_type = cons
                .return_type
                .as_ref()
                .map(|ann| convert_type(&ann.type_annotation));
            Some(InterfaceMember::ConstructSignature(ConstructSignature {
                type_params,
                params,
                return_type,
            }))
        }
        oxc::TSSignature::TSIndexSignature(idx) => {
            let (key_name, key_type) = idx
                .parameters
                .first()
                .map(convert_index_parameter)
                .unwrap_or_else(|| ("key".into(), DtsType::Primitive(Primitive::Any)));
            Some(InterfaceMember::IndexSignature(IndexSignature {
                key_name,
                key_type,
                value_type: convert_type(&idx.type_annotation.type_annotation),
                readonly: idx.readonly,
            }))
        }
    }
}

fn convert_index_parameter(param: &oxc::TSIndexSignatureName<'_>) -> (String, DtsType) {
    (
        param.name.to_string(),
        convert_type(&param.type_annotation.type_annotation),
    )
}

fn property_key_to_string(key: &oxc::PropertyKey<'_>) -> Option<String> {
    match key {
        oxc::PropertyKey::StaticIdentifier(id) => Some(id.name.to_string()),
        oxc::PropertyKey::Identifier(id) => Some(id.name.to_string()),
        oxc::PropertyKey::StringLiteral(lit) => Some(lit.value.to_string()),
        oxc::PropertyKey::NumericLiteral(num) => Some(num.value.to_string()),
        oxc::PropertyKey::BigIntLiteral(num) => Some(num.value.to_string()),
        oxc::PropertyKey::TemplateLiteral(t) if t.quasis.len() == 1 => {
            Some(t.quasis[0].value.raw.to_string())
        }
        _ => None,
    }
}

fn expr_to_name(expr: &oxc::Expression<'_>) -> Option<String> {
    match expr {
        oxc::Expression::Identifier(id) => Some(id.name.to_string()),
        oxc::Expression::StaticMemberExpression(member) => {
            let obj = expr_to_name(&member.object)?;
            Some(format!("{obj}.{}", member.property.name))
        }
        _ => None,
    }
}

fn convert_params(params: &oxc::FormalParameters<'_>) -> Vec<Param> {
    let mut out = Vec::new();
    for item in &params.items {
        if let Some(param) = convert_param(item) {
            out.push(param);
        }
    }
    if let Some(rest) = &params.rest {
        if let Some((name, ty)) = binding_to_name_and_type(&rest.argument) {
            out.push(Param {
                name,
                ty,
                optional: false,
                rest: true,
            });
        }
    }
    out
}

fn convert_param(param: &oxc::FormalParameter<'_>) -> Option<Param> {
    binding_to_name_and_type(&param.pattern).map(|(name, ty)| Param {
        name,
        ty,
        optional: param.pattern.optional,
        rest: false,
    })
}

fn binding_to_name_and_type(pattern: &oxc::BindingPattern<'_>) -> Option<(String, DtsType)> {
    match &pattern.kind {
        oxc::BindingPatternKind::BindingIdentifier(id) => Some((
            id.name.to_string(),
            pattern
                .type_annotation
                .as_ref()
                .map(|ann| convert_type(&ann.type_annotation))
                .unwrap_or(DtsType::Primitive(Primitive::Any)),
        )),
        _ => None,
    }
}

fn convert_type_params(params: Option<&oxc::TSTypeParameterDeclaration<'_>>) -> Vec<TypeParam> {
    params
        .map(|p| {
            p.params
                .iter()
                .map(|param| TypeParam {
                    name: param.name.name.to_string(),
                    constraint: param.constraint.as_ref().map(convert_type),
                    default: param.default.as_ref().map(convert_type),
                })
                .collect()
        })
        .unwrap_or_default()
}

fn convert_ts_type_name(name: &oxc::TSTypeName<'_>) -> String {
    match name {
        oxc::TSTypeName::IdentifierReference(id) => id.name.to_string(),
        oxc::TSTypeName::QualifiedName(q) => {
            let left = convert_ts_type_name(&q.left);
            format!("{left}.{}", q.right.name)
        }
        oxc::TSTypeName::ThisExpression(_) => "this".into(),
    }
}

fn convert_type(ty: &oxc::TSType<'_>) -> DtsType {
    match ty {
        // Keywords
        oxc::TSType::TSStringKeyword(_) => DtsType::Primitive(Primitive::String),
        oxc::TSType::TSNumberKeyword(_) => DtsType::Primitive(Primitive::Number),
        oxc::TSType::TSBooleanKeyword(_) => DtsType::Primitive(Primitive::Boolean),
        oxc::TSType::TSVoidKeyword(_) => DtsType::Primitive(Primitive::Void),
        oxc::TSType::TSNullKeyword(_) => DtsType::Primitive(Primitive::Null),
        oxc::TSType::TSUndefinedKeyword(_) => DtsType::Primitive(Primitive::Undefined),
        oxc::TSType::TSAnyKeyword(_) => DtsType::Primitive(Primitive::Any),
        oxc::TSType::TSUnknownKeyword(_) => DtsType::Primitive(Primitive::Unknown),
        oxc::TSType::TSNeverKeyword(_) => DtsType::Primitive(Primitive::Never),
        oxc::TSType::TSObjectKeyword(_) => DtsType::Primitive(Primitive::Object),
        oxc::TSType::TSSymbolKeyword(_) => DtsType::Primitive(Primitive::Symbol),
        oxc::TSType::TSBigIntKeyword(_) => DtsType::Primitive(Primitive::BigInt),
        oxc::TSType::TSThisType(_) => DtsType::This,

        // Literal
        oxc::TSType::TSLiteralType(lit) => match &lit.literal {
            oxc::TSLiteral::BooleanLiteral(b) => DtsType::BooleanLiteral(b.value),
            oxc::TSLiteral::NumericLiteral(n) => DtsType::NumberLiteral(n.value.to_string()),
            oxc::TSLiteral::BigIntLiteral(n) => DtsType::NumberLiteral(n.value.to_string()),
            oxc::TSLiteral::StringLiteral(s) => DtsType::StringLiteral(s.value.to_string()),
            _ => DtsType::Primitive(Primitive::Any),
        },

        // Type references
        oxc::TSType::TSTypeReference(r) => DtsType::Named {
            name: convert_ts_type_name(&r.type_name),
            type_args: r
                .type_arguments
                .as_ref()
                .map(|args| args.params.iter().map(convert_type).collect())
                .unwrap_or_default(),
        },

        // Arrays and tuples
        oxc::TSType::TSArrayType(arr) => DtsType::Array(Box::new(convert_type(&arr.element_type))),
        oxc::TSType::TSTupleType(tuple) => DtsType::Tuple(
            tuple
                .element_types
                .iter()
                .map(|el| match el {
                    oxc::TSTupleElement::TSRestType(rest) => TupleElement {
                        ty: convert_type(&rest.type_annotation),
                        name: None,
                        optional: false,
                        rest: true,
                    },
                    oxc::TSTupleElement::TSOptionalType(opt) => TupleElement {
                        ty: convert_type(&opt.type_annotation),
                        name: None,
                        optional: true,
                        rest: false,
                    },
                    _ => TupleElement {
                        ty: DtsType::Primitive(Primitive::Any),
                        name: None,
                        optional: false,
                        rest: false,
                    },
                })
                .collect(),
        ),

        // Functions
        oxc::TSType::TSFunctionType(fun) => {
            let type_params = convert_type_params(fun.type_parameters.as_deref());
            let mut params = convert_params(&fun.params);
            if let Some(this_ty) = fun
                .this_param
                .as_ref()
                .and_then(|t| t.type_annotation.as_ref())
                .map(|ann| convert_type(&ann.type_annotation))
            {
                params.insert(
                    0,
                    Param {
                        name: "this".into(),
                        ty: this_ty,
                        optional: false,
                        rest: false,
                    },
                );
            }
            let ret = convert_type(&fun.return_type.type_annotation);
            DtsType::Function(Box::new(HuskFunctionType {
                type_params,
                params,
                return_type: Box::new(ret),
                this_param: None,
            }))
        }

        // Unions / intersections
        oxc::TSType::TSUnionType(u) => DtsType::Union(u.types.iter().map(convert_type).collect()),
        oxc::TSType::TSIntersectionType(i) => {
            DtsType::Intersection(i.types.iter().map(convert_type).collect())
        }

        // keyof
        oxc::TSType::TSTypeOperatorType(op) => match op.operator {
            oxc::TSTypeOperatorOperator::Keyof => {
                DtsType::KeyOf(Box::new(convert_type(&op.type_annotation)))
            }
            _ => DtsType::Primitive(Primitive::Any),
        },

        // typeof
        oxc::TSType::TSTypeQuery(query) => match &query.expr_name {
            oxc::TSTypeQueryExprName::IdentifierReference(id) => {
                DtsType::TypeOf(id.name.to_string())
            }
            oxc::TSTypeQueryExprName::QualifiedName(q) => {
                let left = convert_ts_type_name(&q.left);
                DtsType::TypeOf(format!("{left}.{}", q.right.name))
            }
            _ => DtsType::Primitive(Primitive::Any),
        },

        // Indexed access
        oxc::TSType::TSIndexedAccessType(idx) => DtsType::IndexAccess {
            object: Box::new(convert_type(&idx.object_type)),
            index: Box::new(convert_type(&idx.index_type)),
        },

        // Conditional
        oxc::TSType::TSConditionalType(cond) => DtsType::Conditional {
            check: Box::new(convert_type(&cond.check_type)),
            extends: Box::new(convert_type(&cond.extends_type)),
            true_type: Box::new(convert_type(&cond.true_type)),
            false_type: Box::new(convert_type(&cond.false_type)),
        },

        // Object literal type
        oxc::TSType::TSTypeLiteral(obj) => DtsType::Object(
            obj.members
                .iter()
                .filter_map(convert_object_member)
                .collect(),
        ),

        // Parenthesized
        oxc::TSType::TSParenthesizedType(inner) => {
            DtsType::Parenthesized(Box::new(convert_type(&inner.type_annotation)))
        }

        // Fallbacks
        _ => DtsType::Primitive(Primitive::Any),
    }
}

fn convert_object_member(member: &oxc::TSSignature<'_>) -> Option<ObjectMember> {
    match member {
        oxc::TSSignature::TSPropertySignature(prop) => {
            let name = property_key_to_string(&prop.key)?;
            let ty = prop
                .type_annotation
                .as_ref()
                .map(|ann| convert_type(&ann.type_annotation))
                .unwrap_or(DtsType::Primitive(Primitive::Any));
            Some(ObjectMember::Property {
                name,
                ty,
                optional: prop.optional,
                readonly: prop.readonly,
            })
        }
        oxc::TSSignature::TSMethodSignature(method) => {
            let name = property_key_to_string(&method.key)?;
            let type_params = convert_type_params(method.type_parameters.as_deref());
            let mut params = convert_params(&method.params);
            if let Some(this_ty) = method
                .this_param
                .as_ref()
                .and_then(|t| t.type_annotation.as_ref())
                .map(|ann| convert_type(&ann.type_annotation))
            {
                params.insert(
                    0,
                    Param {
                        name: "this".into(),
                        ty: this_ty,
                        optional: false,
                        rest: false,
                    },
                );
            }
            let return_type = method
                .return_type
                .as_ref()
                .map(|ann| convert_type(&ann.type_annotation));
            Some(ObjectMember::Method {
                name,
                type_params,
                params,
                return_type,
                optional: method.optional,
            })
        }
        oxc::TSSignature::TSCallSignatureDeclaration(call) => {
            Some(ObjectMember::CallSignature(CallSignature {
                type_params: convert_type_params(call.type_parameters.as_deref()),
                params: convert_params(&call.params),
                return_type: call
                    .return_type
                    .as_ref()
                    .map(|ann| convert_type(&ann.type_annotation)),
            }))
        }
        oxc::TSSignature::TSConstructSignatureDeclaration(cons) => {
            Some(ObjectMember::ConstructSignature(ConstructSignature {
                type_params: convert_type_params(cons.type_parameters.as_deref()),
                params: convert_params(&cons.params),
                return_type: cons
                    .return_type
                    .as_ref()
                    .map(|ann| convert_type(&ann.type_annotation)),
            }))
        }
        oxc::TSSignature::TSIndexSignature(idx) => {
            Some(ObjectMember::IndexSignature(IndexSignature {
                key_name: idx
                    .parameters
                    .first()
                    .map(convert_index_parameter)
                    .map(|(n, _)| n)
                    .unwrap_or_else(|| "key".into()),
                key_type: idx
                    .parameters
                    .first()
                    .map(convert_index_parameter)
                    .map(|(_, t)| t)
                    .unwrap_or(DtsType::Primitive(Primitive::Any)),
                value_type: convert_type(&idx.type_annotation.type_annotation),
                readonly: idx.readonly,
            }))
        }
    }
}
