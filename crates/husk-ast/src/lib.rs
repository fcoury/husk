//! Core AST definitions for the language.

use std::ops::Range;

/// A span in the source file, represented as a byte range.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub range: Range<usize>,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { range: start..end }
    }
}

/// An identifier with its source span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

/// Literal values.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Int(i64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not, // !
    Neg, // -
}

/// Binary operators (MVP subset).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
}

/// Expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Ident(Ident),
    /// Path-like expression, e.g. `Enum::Variant`.
    /// For the MVP this is primarily used for enum constructors.
    Path {
        segments: Vec<Ident>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Field {
        base: Box<Expr>,
        member: Ident,
    },
    MethodCall {
        receiver: Box<Expr>,
        method: Ident,
        args: Vec<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Block(Block),
    /// Struct instantiation expression, e.g. `Point { x: 1, y: 2 }`.
    Struct {
        /// The struct type name (possibly a path like `module::Type`).
        name: Vec<Ident>,
        /// Field initializers.
        fields: Vec<FieldInit>,
    },
}

/// A field initializer in a struct expression.
#[derive(Debug, Clone, PartialEq)]
pub struct FieldInit {
    pub name: Ident,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

/// A block of statements delimited by `{}`.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

/// A pattern used in `match` expressions (MVP subset).
#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Wildcard,       // _
    Binding(Ident), // x
    EnumUnit {
        path: Vec<Ident>, // e.g., Message::Quit -> [Message, Quit]
    },
    EnumTuple {
        path: Vec<Ident>,
        fields: Vec<Pattern>,
    },
    EnumStruct {
        path: Vec<Ident>,
        fields: Vec<(Ident, Pattern)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

/// A single `match` arm.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expr: Expr,
}

/// Statements.
#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Let {
        mutable: bool,
        name: Ident,
        ty: Option<TypeExpr>,
        value: Option<Expr>,
    },
    Expr(Expr),
    Semi(Expr),
    Return {
        value: Option<Expr>,
    },
    If {
        cond: Expr,
        then_branch: Block,
        else_branch: Option<Box<Stmt>>, // usually another If or a Block stmt
    },
    While {
        cond: Expr,
        body: Block,
    },
    Break,
    Continue,
    Block(Block),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

/// Simple type expressions for the MVP.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeExprKind {
    Named(Ident),
    Generic { name: Ident, args: Vec<TypeExpr> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeExpr {
    pub kind: TypeExprKind,
    pub span: Span,
}

/// Function parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: Ident,
    pub ty: TypeExpr,
}

/// Item-level definitions.
#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Fn {
        name: Ident,
        type_params: Vec<Ident>,
        params: Vec<Param>,
        ret_type: Option<TypeExpr>,
        body: Vec<Stmt>,
    },
    Struct {
        name: Ident,
        type_params: Vec<Ident>,
        fields: Vec<StructField>,
    },
    Enum {
        name: Ident,
        type_params: Vec<Ident>,
        variants: Vec<EnumVariant>,
    },
    TypeAlias {
        name: Ident,
        ty: TypeExpr,
    },
    ExternBlock {
        abi: String,
        items: Vec<ExternItem>,
    },
    Use {
        /// Path like `crate::foo::bar`
        path: Vec<Ident>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Ident,
    pub ty: TypeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: Ident,
    pub fields: EnumVariantFields,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariantFields {
    /// Tuple-like variant: Variant(Type1, Type2, ...)
    Tuple(Vec<TypeExpr>),
    /// Struct-like variant: Variant { field: Type, ... }
    Struct(Vec<StructField>),
    /// Unit variant: Variant
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub visibility: Visibility,
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

/// Items that may appear inside an `extern` block.
#[derive(Debug, Clone, PartialEq)]
pub enum ExternItemKind {
    /// Bare function declaration: `fn foo() -> i32;`
    /// Generates globalThis.foo() calls.
    Fn {
        name: Ident,
        params: Vec<Param>,
        ret_type: Option<TypeExpr>,
    },
    /// Module import declaration: `mod express;` or `mod "package-name" as alias;`
    /// This declares a dependency on a JS module by package name.
    /// When `items` is empty, imports the default export.
    /// When `items` has functions, generates named imports for those functions.
    Mod {
        /// The npm package name (e.g., "express", "@scope/pkg", "lodash-es")
        package: String,
        /// The identifier to use in Husk code (derived from package or explicit alias)
        binding: Ident,
        /// Optional nested function declarations to import from this module.
        /// If non-empty, generates `import { fn1, fn2 } from "package";`
        items: Vec<ModItem>,
    },
}

/// Items that may appear inside a `mod` block within an extern block.
#[derive(Debug, Clone, PartialEq)]
pub struct ModItem {
    pub kind: ModItemKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModItemKind {
    Fn {
        name: Ident,
        params: Vec<Param>,
        ret_type: Option<TypeExpr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternItem {
    pub kind: ExternItemKind,
    pub span: Span,
}

/// A source file (compilation unit).
#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub items: Vec<Item>,
}
