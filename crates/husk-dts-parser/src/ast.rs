//! AST types for TypeScript declaration files.

use std::fmt;

/// A parsed .d.ts file containing multiple declarations.
#[derive(Debug, Clone, PartialEq)]
pub struct DtsFile {
    pub items: Vec<DtsItem>,
}

/// A top-level declaration in a .d.ts file.
#[derive(Debug, Clone, PartialEq)]
pub enum DtsItem {
    Function(DtsFunction),
    Interface(DtsInterface),
    TypeAlias(DtsTypeAlias),
    Class(DtsClass),
    Variable(DtsVariable),
    Namespace(DtsNamespace),
    Module(DtsModule),
    Export(DtsExport),
}

/// A function declaration: `declare function name<T>(params): ReturnType;`
#[derive(Debug, Clone, PartialEq)]
pub struct DtsFunction {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub return_type: Option<DtsType>,
    /// Explicit `this` parameter type annotation (e.g., `function foo(this: Window, ...): void`)
    pub this_param: Option<Box<DtsType>>,
}

/// An interface declaration: `interface Name<T> extends Base { members }`
#[derive(Debug, Clone, PartialEq)]
pub struct DtsInterface {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub extends: Vec<DtsType>,
    pub members: Vec<InterfaceMember>,
}

/// A type alias: `type Name<T> = Type;`
#[derive(Debug, Clone, PartialEq)]
pub struct DtsTypeAlias {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub ty: DtsType,
}

/// A class declaration: `declare class Name<T> extends Base implements Interface { members }`
#[derive(Debug, Clone, PartialEq)]
pub struct DtsClass {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub extends: Option<DtsType>,
    pub implements: Vec<DtsType>,
    pub members: Vec<ClassMember>,
}

/// A variable declaration: `declare const/let/var name: Type;`
#[derive(Debug, Clone, PartialEq)]
pub struct DtsVariable {
    pub name: String,
    pub ty: DtsType,
    pub is_const: bool,
}

/// A namespace declaration: `declare namespace Name { ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct DtsNamespace {
    pub name: String,
    pub items: Vec<DtsItem>,
}

/// A module declaration: `declare module "name" { ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct DtsModule {
    pub name: String,
    pub items: Vec<DtsItem>,
    /// Whether this is an ambient module (string literal name like `declare module "express"`)
    /// vs a regular module declaration. Ambient modules augment existing packages.
    pub is_ambient: bool,
}

/// An export statement
#[derive(Debug, Clone, PartialEq)]
pub enum DtsExport {
    /// `export = name;`
    Equals(String),
    /// `export default name;`
    Default(String),
    /// `export { name, name2 as alias };`
    Named(Vec<(String, Option<String>)>),
}

/// A generic type parameter with optional constraint.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    pub name: String,
    /// Optional constraint: `T extends Foo`
    pub constraint: Option<DtsType>,
    /// Optional default: `T = DefaultType`
    pub default: Option<DtsType>,
}

/// A function/method parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: DtsType,
    pub optional: bool,
    pub rest: bool,
}

/// A member of an interface.
#[derive(Debug, Clone, PartialEq)]
pub enum InterfaceMember {
    Property(PropertyMember),
    Method(MethodMember),
    CallSignature(CallSignature),
    ConstructSignature(ConstructSignature),
    IndexSignature(IndexSignature),
}

/// A property in an interface: `name?: Type;`
#[derive(Debug, Clone, PartialEq)]
pub struct PropertyMember {
    pub name: String,
    pub ty: DtsType,
    pub optional: bool,
    pub readonly: bool,
}

/// A method in an interface: `name<T>(params): ReturnType;`
#[derive(Debug, Clone, PartialEq)]
pub struct MethodMember {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub return_type: Option<DtsType>,
    pub optional: bool,
    /// If this method has an explicit `this` parameter type
    pub this_param: Option<Box<DtsType>>,
}

/// A call signature: `(params): ReturnType;`
#[derive(Debug, Clone, PartialEq)]
pub struct CallSignature {
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub return_type: Option<DtsType>,
    /// If this call signature has an explicit `this` parameter type
    pub this_param: Option<Box<DtsType>>,
}

/// A construct signature: `new (params): ReturnType;`
#[derive(Debug, Clone, PartialEq)]
pub struct ConstructSignature {
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub return_type: Option<DtsType>,
}

/// An index signature: `[key: string]: Type;`
#[derive(Debug, Clone, PartialEq)]
pub struct IndexSignature {
    pub key_name: String,
    pub key_type: DtsType,
    pub value_type: DtsType,
    pub readonly: bool,
}

/// A member of a class.
#[derive(Debug, Clone, PartialEq)]
pub enum ClassMember {
    Property(ClassProperty),
    Method(ClassMethod),
    Constructor(ConstructSignature),
    IndexSignature(IndexSignature),
}

/// A property in a class.
#[derive(Debug, Clone, PartialEq)]
pub struct ClassProperty {
    pub name: String,
    pub ty: Option<DtsType>,
    pub optional: bool,
    pub readonly: bool,
    pub is_static: bool,
    pub visibility: Visibility,
}

/// A method in a class.
#[derive(Debug, Clone, PartialEq)]
pub struct ClassMethod {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub return_type: Option<DtsType>,
    pub is_static: bool,
    pub visibility: Visibility,
    /// If this method has an explicit `this` parameter type
    pub this_param: Option<Box<DtsType>>,
}

/// Visibility modifier for class members.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Visibility {
    #[default]
    Public,
    Protected,
    Private,
}

/// A TypeScript type.
#[derive(Debug, Clone, PartialEq)]
pub enum DtsType {
    /// A named type with optional type arguments: `Promise<T>`, `Array<T>`
    Named {
        name: String,
        type_args: Vec<DtsType>,
    },
    /// A primitive type: `string`, `number`, `boolean`, `void`, etc.
    Primitive(Primitive),
    /// A string literal type: `"GET"`, `"POST"`
    StringLiteral(String),
    /// A number literal type: `1`, `2`, `3`
    NumberLiteral(String),
    /// A boolean literal type: `true`, `false`
    BooleanLiteral(bool),
    /// A union type: `A | B | C`
    Union(Vec<DtsType>),
    /// An intersection type: `A & B & C`
    Intersection(Vec<DtsType>),
    /// A function type: `(a: T, b: U) => R`
    Function(Box<FunctionType>),
    /// An object literal type: `{ key: Type, key2?: Type2 }`
    Object(Vec<ObjectMember>),
    /// An array type: `T[]`
    Array(Box<DtsType>),
    /// A tuple type: `[T, U, V]`
    Tuple(Vec<TupleElement>),
    /// A typeof type: `typeof foo`
    TypeOf(String),
    /// A keyof type: `keyof T`
    KeyOf(Box<DtsType>),
    /// An indexed access type: `T[K]`
    IndexAccess {
        object: Box<DtsType>,
        index: Box<DtsType>,
    },
    /// A conditional type: `T extends U ? X : Y`
    Conditional {
        check: Box<DtsType>,
        extends: Box<DtsType>,
        true_type: Box<DtsType>,
        false_type: Box<DtsType>,
    },
    /// A mapped type: `{ [K in keyof T]: T[K] }`
    Mapped {
        key_name: String,
        key_constraint: Box<DtsType>,
        value_type: Box<DtsType>,
        readonly: Option<MappedModifier>,
        optional: Option<MappedModifier>,
    },
    /// An infer type: `infer T`
    Infer(String),
    /// A template literal type: `` `prefix_${T}` ``
    TemplateLiteral(Vec<TemplateLiteralPart>),
    /// A parenthesized type: `(T)`
    Parenthesized(Box<DtsType>),
    /// A this type: `this`
    This,
}

/// A primitive TypeScript type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Primitive {
    String,
    Number,
    Boolean,
    Void,
    Null,
    Undefined,
    Any,
    Unknown,
    Never,
    Object,
    Symbol,
    BigInt,
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::String => write!(f, "string"),
            Primitive::Number => write!(f, "number"),
            Primitive::Boolean => write!(f, "boolean"),
            Primitive::Void => write!(f, "void"),
            Primitive::Null => write!(f, "null"),
            Primitive::Undefined => write!(f, "undefined"),
            Primitive::Any => write!(f, "any"),
            Primitive::Unknown => write!(f, "unknown"),
            Primitive::Never => write!(f, "never"),
            Primitive::Object => write!(f, "object"),
            Primitive::Symbol => write!(f, "symbol"),
            Primitive::BigInt => write!(f, "bigint"),
        }
    }
}

/// A function type: `(params) => ReturnType`
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub return_type: Box<DtsType>,
    /// If this function has a `this` parameter
    pub this_param: Option<Box<DtsType>>,
}

/// A member of an object literal type.
#[derive(Debug, Clone, PartialEq)]
pub enum ObjectMember {
    Property {
        name: String,
        ty: DtsType,
        optional: bool,
        readonly: bool,
    },
    Method {
        name: String,
        type_params: Vec<TypeParam>,
        params: Vec<Param>,
        return_type: Option<DtsType>,
        optional: bool,
        /// If this method has an explicit `this` parameter type
        this_param: Option<Box<DtsType>>,
    },
    CallSignature(CallSignature),
    ConstructSignature(ConstructSignature),
    IndexSignature(IndexSignature),
}

/// An element in a tuple type.
#[derive(Debug, Clone, PartialEq)]
pub struct TupleElement {
    pub ty: DtsType,
    pub name: Option<String>,
    pub optional: bool,
    pub rest: bool,
}

/// A modifier in a mapped type: `+`, `-`, or nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MappedModifier {
    Add,
    Remove,
    Preserve,
}

/// A part of a template literal type.
#[derive(Debug, Clone, PartialEq)]
pub enum TemplateLiteralPart {
    String(String),
    Type(DtsType),
}
