use swc_ecma_ast::{TsType, TsKeywordTypeKind, TsEntityName};

#[derive(Debug, Clone)]
pub enum HuskType {
    String,
    Number,
    Boolean,
    Any,
    Void,
    Custom(String),
    Qualified(String, String), // module, type
    Array(Box<HuskType>),
}

impl HuskType {
    pub fn to_string(&self) -> String {
        match self {
            HuskType::String => "string".to_string(),
            HuskType::Number => "i32".to_string(), // Default to i32, could be configurable
            HuskType::Boolean => "bool".to_string(),
            HuskType::Any => "any".to_string(),
            HuskType::Void => "".to_string(), // No return type in Husk
            HuskType::Custom(name) => name.clone(),
            HuskType::Qualified(module, name) => format!("{}::{}", module, name),
            HuskType::Array(inner) => format!("Vec<{}>", inner.to_string()),
        }
    }
}

pub fn convert_ts_type(ts_type: &TsType) -> HuskType {
    match ts_type {
        TsType::TsKeywordType(keyword) => convert_keyword_type(&keyword.kind),
        TsType::TsTypeRef(type_ref) => {
            convert_ts_entity_name(&type_ref.type_name)
        }
        TsType::TsArrayType(array) => {
            let inner = convert_ts_type(&array.elem_type);
            HuskType::Array(Box::new(inner))
        }
        TsType::TsUnionOrIntersectionType(_) => HuskType::Any, // Simplify unions to any
        TsType::TsFnOrConstructorType(_) => HuskType::Any, // Function types to any
        TsType::TsTypeLit(_) => HuskType::Any, // Object literals to any
        _ => HuskType::Any, // Default fallback
    }
}

fn convert_keyword_type(kind: &TsKeywordTypeKind) -> HuskType {
    match kind {
        TsKeywordTypeKind::TsStringKeyword => HuskType::String,
        TsKeywordTypeKind::TsNumberKeyword => HuskType::Number,
        TsKeywordTypeKind::TsBooleanKeyword => HuskType::Boolean,
        TsKeywordTypeKind::TsVoidKeyword => HuskType::Void,
        TsKeywordTypeKind::TsAnyKeyword => HuskType::Any,
        TsKeywordTypeKind::TsUnknownKeyword => HuskType::Any,
        TsKeywordTypeKind::TsNullKeyword => HuskType::Any,
        TsKeywordTypeKind::TsUndefinedKeyword => HuskType::Any,
        _ => HuskType::Any,
    }
}

fn convert_ts_entity_name(entity: &TsEntityName) -> HuskType {
    match entity {
        TsEntityName::Ident(ident) => {
            HuskType::Custom(ident.sym.to_string())
        }
        TsEntityName::TsQualifiedName(qual_name) => {
            // Extract the module path
            let module = extract_module_path(&qual_name.left);
            let type_name = qual_name.right.sym.to_string();
            HuskType::Qualified(module, type_name)
        }
    }
}

fn extract_module_path(entity: &TsEntityName) -> String {
    match entity {
        TsEntityName::Ident(ident) => ident.sym.to_string(),
        TsEntityName::TsQualifiedName(qual_name) => {
            let left = extract_module_path(&qual_name.left);
            let right = qual_name.right.sym.to_string();
            format!("{}.{}", left, right)
        }
    }
}