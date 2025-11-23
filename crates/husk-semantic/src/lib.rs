//! Name resolution and early semantic analysis for Husk.
//!
//! This crate currently defines:
//! - A basic symbol representation for top-level items.
//! - A resolver that collects top-level symbols from a `husk_ast::File`.

use std::collections::HashMap;

use husk_ast::{
    Block, Expr, ExprKind, File, Ident, Item, ItemKind, LiteralKind, Param, Span, Stmt, StmtKind,
    TypeExpr, TypeExprKind,
};
use husk_types::{PrimitiveType, Type};

/// Unique identifier for a symbol within a module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

/// Kinds of symbols that can be defined at the top level.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Struct,
    Enum,
    TypeAlias,
    ExternFn,
}

/// A resolved symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub id: SymbolId,
    pub name: String,
    pub kind: SymbolKind,
    pub span: Span,
}

/// A collection of top-level symbols for a single Husk module/file.
#[derive(Debug, Default)]
pub struct ModuleSymbols {
    pub symbols: Vec<Symbol>,
    pub by_name: HashMap<String, SymbolId>,
    pub errors: Vec<SemanticError>,
}

impl ModuleSymbols {
    /// Resolve top-level symbols from an AST `File`.
    pub fn from_file(file: &File) -> Self {
        let mut resolver = Resolver::new();
        resolver.collect(file);
        resolver.finish()
    }

    /// Look up a symbol by name.
    pub fn get(&self, name: &str) -> Option<&Symbol> {
        let id = *self.by_name.get(name)?;
        self.symbols.get(id.0 as usize)
    }
}

/// A semantic error produced during name resolution or later phases.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemanticError {
    pub message: String,
    pub span: Span,
}

/// The result of running semantic analysis (name resolution + type checking) on a Husk file.
#[derive(Debug)]
pub struct SemanticResult {
    pub symbols: ModuleSymbols,
    pub type_errors: Vec<SemanticError>,
}

/// Run full semantic analysis (name resolution + type checking) over the given file.
pub fn analyze_file(file: &File) -> SemanticResult {
    let symbols = ModuleSymbols::from_file(file);
    let mut checker = TypeChecker::new();
    checker.build_type_env(file);
    let type_errors = checker.check_file(file);
    SemanticResult {
        symbols,
        type_errors,
    }
}

struct Resolver {
    symbols: Vec<Symbol>,
    by_name: HashMap<String, SymbolId>,
    errors: Vec<SemanticError>,
}

// =============== Type environment and type checking ===============

/// Information about a struct type.
#[derive(Debug, Clone)]
struct StructDef {
    type_params: Vec<String>,
    fields: HashMap<String, TypeExpr>,
}

/// Information about an enum type.
#[derive(Debug, Clone)]
struct EnumDef {
    type_params: Vec<String>,
}

/// Information about a function type.
#[derive(Debug, Clone)]
struct FnDef {
    params: Vec<Param>,
    ret_type: Option<TypeExpr>,
}

#[derive(Debug, Default)]
struct TypeEnv {
    structs: HashMap<String, StructDef>,
    enums: HashMap<String, EnumDef>,
    type_aliases: HashMap<String, TypeExpr>,
    functions: HashMap<String, FnDef>,
}

struct TypeChecker {
    env: TypeEnv,
    errors: Vec<SemanticError>,
}

impl TypeChecker {
    fn new() -> Self {
        Self {
            env: TypeEnv::default(),
            errors: Vec::new(),
        }
    }

    fn build_type_env(&mut self, file: &File) {
        for item in &file.items {
            match &item.kind {
                ItemKind::Struct {
                    name,
                    type_params,
                    fields,
                } => {
                    let def = StructDef {
                        type_params: type_params.iter().map(|id| id.name.clone()).collect(),
                        fields: fields
                            .iter()
                            .map(|f| (f.name.name.clone(), f.ty.clone()))
                            .collect(),
                    };
                    self.env.structs.insert(name.name.clone(), def);
                }
                ItemKind::Enum {
                    name, type_params, ..
                } => {
                    let def = EnumDef {
                        type_params: type_params.iter().map(|id| id.name.clone()).collect(),
                    };
                    self.env.enums.insert(name.name.clone(), def);
                }
                ItemKind::TypeAlias { name, ty } => {
                    self.env.type_aliases.insert(name.name.clone(), ty.clone());
                }
                ItemKind::Fn {
                    name,
                    params,
                    ret_type,
                    ..
                } => {
                    let def = FnDef {
                        params: params.clone(),
                        ret_type: ret_type.clone(),
                    };
                    self.env.functions.insert(name.name.clone(), def);
                }
                ItemKind::ExternBlock { items, .. } => {
                    for ext in items {
                        let husk_ast::ExternItemKind::Fn {
                            name,
                            params,
                            ret_type,
                        } = &ext.kind;
                        let def = FnDef {
                            params: params.clone(),
                            ret_type: ret_type.clone(),
                        };
                        self.env.functions.insert(name.name.clone(), def);
                    }
                }
            }
        }
    }

    fn check_file(&mut self, file: &File) -> Vec<SemanticError> {
        // Type check each function body independently.
        for item in &file.items {
            if let ItemKind::Fn {
                name,
                params,
                ret_type,
                body,
                ..
            } = &item.kind
            {
                self.check_fn(name, params, ret_type.as_ref(), body, item.span.clone());
            }
        }
        self.errors.clone()
    }

    fn check_fn(
        &mut self,
        name: &Ident,
        params: &[Param],
        ret_type_expr: Option<&TypeExpr>,
        body: &[Stmt],
        span: Span,
    ) {
        let ret_ty = if let Some(ty_expr) = ret_type_expr {
            self.resolve_type_expr(ty_expr, &[])
        } else {
            Type::Primitive(PrimitiveType::Unit)
        };

        let mut locals: HashMap<String, Type> = HashMap::new();

        // Parameters must have explicit types.
        for param in params {
            let ty = self.resolve_type_expr(&param.ty, &[]);
            if locals.insert(param.name.name.clone(), ty).is_some() {
                self.errors.push(SemanticError {
                    message: format!(
                        "duplicate parameter name `{}` in function `{}`",
                        param.name.name, name.name
                    ),
                    span: param.name.span.clone(),
                });
            }
        }

        let mut ctx = FnContext {
            tcx: self,
            locals,
            ret_ty,
            in_loop: false,
        };

        for stmt in body {
            ctx.check_stmt(stmt);
        }

        let _ = span; // reserved for potential future checks (e.g., missing returns).
    }

    fn resolve_type_expr(&mut self, ty: &TypeExpr, generic_params: &[String]) -> Type {
        match &ty.kind {
            TypeExprKind::Named(id) => {
                self.resolve_named_type(&id.name, &[], ty.span.clone(), generic_params)
            }
            TypeExprKind::Generic { name, args } => {
                let resolved_args: Vec<Type> = args
                    .iter()
                    .map(|a| self.resolve_type_expr(a, generic_params))
                    .collect();
                self.resolve_named_type(&name.name, &resolved_args, ty.span.clone(), generic_params)
            }
        }
    }

    fn resolve_named_type(
        &mut self,
        name: &str,
        args: &[Type],
        span: Span,
        generic_params: &[String],
    ) -> Type {
        // Generic parameter in scope?
        if generic_params.contains(&name.to_string()) {
            return Type::Named {
                name: name.to_string(),
                args: Vec::new(),
            };
        }

        // Primitive types.
        let prim = match name {
            "i32" => Some(Type::Primitive(PrimitiveType::I32)),
            "bool" => Some(Type::Primitive(PrimitiveType::Bool)),
            "String" => Some(Type::Primitive(PrimitiveType::String)),
            "()" => Some(Type::Primitive(PrimitiveType::Unit)),
            _ => None,
        };
        if let Some(t) = prim {
            if !args.is_empty() {
                self.errors.push(SemanticError {
                    message: format!("primitive type `{}` does not take type arguments", name),
                    span,
                });
            }
            return t;
        }

        // Known struct or enum: check arity.
        if let Some(def) = self.env.structs.get(name) {
            if def.type_params.len() != args.len() {
                self.errors.push(SemanticError {
                    message: format!(
                        "struct `{}` expects {} type argument(s), got {}",
                        name,
                        def.type_params.len(),
                        args.len()
                    ),
                    span,
                });
            }
            return Type::Named {
                name: name.to_string(),
                args: args.to_vec(),
            };
        }

        if let Some(def) = self.env.enums.get(name).cloned() {
            if def.type_params.len() != args.len() {
                self.errors.push(SemanticError {
                    message: format!(
                        "enum `{}` expects {} type argument(s), got {}",
                        name,
                        def.type_params.len(),
                        args.len()
                    ),
                    span,
                });
            }
            return Type::Named {
                name: name.to_string(),
                args: args.to_vec(),
            };
        }

        // Type alias: expand once.
        if let Some(alias) = self.env.type_aliases.get(name).cloned() {
            return self.resolve_type_expr(&alias, generic_params);
        }

        // Unknown type.
        self.errors.push(SemanticError {
            message: format!("unknown type `{}`", name),
            span,
        });
        Type::Named {
            name: name.to_string(),
            args: args.to_vec(),
        }
    }
}

struct FnContext<'a> {
    tcx: &'a mut TypeChecker,
    locals: HashMap<String, Type>,
    ret_ty: Type,
    in_loop: bool,
}

impl<'a> FnContext<'a> {
    fn check_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Let {
                mutable: _,
                name,
                ty,
                value,
            } => {
                let annotated_ty = ty.as_ref().map(|t| self.tcx.resolve_type_expr(t, &[]));
                let value_ty = value.as_ref().map(|expr| self.check_expr(expr));

                let final_ty = match (annotated_ty, value_ty) {
                    (Some(a), Some(v)) => {
                        if !self.types_compatible(&a, &v) {
                            self.tcx.errors.push(SemanticError {
                                message: format!(
                                    "mismatched types in `let`: expected `{:?}`, found `{:?}`",
                                    a, v
                                ),
                                span: stmt.span.clone(),
                            });
                        }
                        a
                    }
                    (Some(a), None) => a,
                    (None, Some(v)) => v,
                    (None, None) => {
                        self.tcx.errors.push(SemanticError {
                            message: "cannot infer type for `let` without annotation or value"
                                .to_string(),
                            span: stmt.span.clone(),
                        });
                        Type::Primitive(PrimitiveType::Unit)
                    }
                };

                if self.locals.insert(name.name.clone(), final_ty).is_some() {
                    self.tcx.errors.push(SemanticError {
                        message: format!("duplicate local binding `{}`", name.name),
                        span: name.span.clone(),
                    });
                }
            }
            StmtKind::Expr(expr) | StmtKind::Semi(expr) => {
                let _ = self.check_expr(expr);
            }
            StmtKind::Return { value } => {
                let actual_ty = if let Some(expr) = value {
                    self.check_expr(expr)
                } else {
                    Type::Primitive(PrimitiveType::Unit)
                };
                if !self.types_compatible(&self.ret_ty, &actual_ty) {
                    self.tcx.errors.push(SemanticError {
                        message: format!(
                            "mismatched return type: expected `{:?}`, found `{:?}`",
                            self.ret_ty, actual_ty
                        ),
                        span: stmt.span.clone(),
                    });
                }
            }
            StmtKind::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_ty = self.check_expr(cond);
                if !matches!(cond_ty, Type::Primitive(PrimitiveType::Bool)) {
                    self.tcx.errors.push(SemanticError {
                        message: "if condition must have type `bool`".to_string(),
                        span: cond.span.clone(),
                    });
                }

                self.check_block(then_branch);
                if let Some(else_stmt) = else_branch {
                    self.check_stmt(else_stmt);
                }
            }
            StmtKind::While { cond, body } => {
                let cond_ty = self.check_expr(cond);
                if !matches!(cond_ty, Type::Primitive(PrimitiveType::Bool)) {
                    self.tcx.errors.push(SemanticError {
                        message: "while condition must have type `bool`".to_string(),
                        span: cond.span.clone(),
                    });
                }

                let prev_in_loop = self.in_loop;
                self.in_loop = true;
                self.check_block(body);
                self.in_loop = prev_in_loop;
            }
            StmtKind::Break | StmtKind::Continue => {
                if !self.in_loop {
                    self.tcx.errors.push(SemanticError {
                        message: format!(
                            "`{}` used outside of loop",
                            if matches!(stmt.kind, StmtKind::Break) {
                                "break"
                            } else {
                                "continue"
                            }
                        ),
                        span: stmt.span.clone(),
                    });
                }
            }
            StmtKind::Block(block) => self.check_block(block),
        }
    }

    fn check_block(&mut self, block: &Block) {
        let old_locals = self.locals.clone();
        for stmt in &block.stmts {
            self.check_stmt(stmt);
        }
        self.locals = old_locals;
    }

    fn check_expr(&mut self, expr: &Expr) -> Type {
        match &expr.kind {
            ExprKind::Literal(lit) => match lit.kind {
                LiteralKind::Int(_) => Type::Primitive(PrimitiveType::I32),
                LiteralKind::Bool(_) => Type::Primitive(PrimitiveType::Bool),
                LiteralKind::String(_) => Type::Primitive(PrimitiveType::String),
            },
            ExprKind::Ident(id) => {
                if let Some(ty) = self.locals.get(&id.name) {
                    return ty.clone();
                }
                // Try top-level function.
                if let Some(fn_def) = self.tcx.env.functions.get(&id.name).cloned() {
                    let param_types: Vec<Type> = fn_def
                        .params
                        .iter()
                        .map(|p| self.tcx.resolve_type_expr(&p.ty, &[]))
                        .collect();
                    let ret_ty = if let Some(ret_expr) = fn_def.ret_type.as_ref() {
                        self.tcx.resolve_type_expr(ret_expr, &[])
                    } else {
                        Type::Primitive(PrimitiveType::Unit)
                    };
                    return Type::Function {
                        params: param_types,
                        ret: Box::new(ret_ty),
                    };
                }

                self.tcx.errors.push(SemanticError {
                    message: format!("unknown identifier `{}`", id.name),
                    span: id.span.clone(),
                });
                Type::Primitive(PrimitiveType::Unit)
            }
            ExprKind::Call { callee, args } => {
                let callee_ty = self.check_expr(callee);
                let (param_tys, ret_ty) = match callee_ty {
                    Type::Function { params, ret } => (params, *ret),
                    other => {
                        self.tcx.errors.push(SemanticError {
                            message: format!("cannot call non-function type `{:?}`", other),
                            span: expr.span.clone(),
                        });
                        return Type::Primitive(PrimitiveType::Unit);
                    }
                };

                if param_tys.len() != args.len() {
                    self.tcx.errors.push(SemanticError {
                        message: format!(
                            "function expects {} argument(s), got {}",
                            param_tys.len(),
                            args.len()
                        ),
                        span: expr.span.clone(),
                    });
                }

                for (i, arg) in args.iter().enumerate() {
                    let arg_ty = self.check_expr(arg);
                    if let Some(expected) = param_tys.get(i) {
                        if !self.types_compatible(expected, &arg_ty) {
                            self.tcx.errors.push(SemanticError {
                                message: format!(
                                    "mismatched argument type at position {}: expected `{:?}`, found `{:?}`",
                                    i, expected, arg_ty
                                ),
                                span: arg.span.clone(),
                            });
                        }
                    }
                }

                ret_ty
            }
            ExprKind::Field { base, member } => {
                let base_ty = self.check_expr(base);
                if let Type::Named { name, args } = base_ty {
                    if let Some(def) = self.tcx.env.structs.get(&name).cloned() {
                        if let Some(field_ty_expr) = def.fields.get(&member.name) {
                            // For now, ignore generic substitution and just resolve as-is.
                            let _ = args;
                            return self.tcx.resolve_type_expr(field_ty_expr, &def.type_params);
                        }
                        self.tcx.errors.push(SemanticError {
                            message: format!(
                                "no field named `{}` on struct `{}`",
                                member.name, name
                            ),
                            span: member.span.clone(),
                        });
                    } else {
                        self.tcx.errors.push(SemanticError {
                            message: format!(
                                "field access on non-struct type `{:?}`",
                                Type::Named { name, args }
                            ),
                            span: expr.span.clone(),
                        });
                    }
                } else {
                    self.tcx.errors.push(SemanticError {
                        message: "field access is only supported on struct types".to_string(),
                        span: expr.span.clone(),
                    });
                }
                Type::Primitive(PrimitiveType::Unit)
            }
            ExprKind::MethodCall {
                receiver,
                method: _,
                args,
            } => {
                // MVP: just type-check receiver and arguments, but treat result as unit.
                let _ = self.check_expr(receiver);
                for arg in args {
                    let _ = self.check_expr(arg);
                }
                Type::Primitive(PrimitiveType::Unit)
            }
            ExprKind::Unary { op, expr: inner } => {
                let inner_ty = self.check_expr(inner);
                match op {
                    husk_ast::UnaryOp::Not => {
                        if !matches!(inner_ty, Type::Primitive(PrimitiveType::Bool)) {
                            self.tcx.errors.push(SemanticError {
                                message: "operator `!` expects operand of type `bool`".to_string(),
                                span: expr.span.clone(),
                            });
                        }
                        Type::Primitive(PrimitiveType::Bool)
                    }
                    husk_ast::UnaryOp::Neg => {
                        if !matches!(inner_ty, Type::Primitive(PrimitiveType::I32)) {
                            self.tcx.errors.push(SemanticError {
                                message: "unary `-` expects operand of type `i32`".to_string(),
                                span: expr.span.clone(),
                            });
                        }
                        Type::Primitive(PrimitiveType::I32)
                    }
                }
            }
            ExprKind::Binary { op, left, right } => {
                let left_ty = self.check_expr(left);
                let right_ty = self.check_expr(right);
                use husk_ast::BinaryOp::*;
                match op {
                    Add | Sub | Mul | Div => {
                        if !matches!(left_ty, Type::Primitive(PrimitiveType::I32))
                            || !matches!(right_ty, Type::Primitive(PrimitiveType::I32))
                        {
                            self.tcx.errors.push(SemanticError {
                                message: "arithmetic operators expect operands of type `i32`"
                                    .to_string(),
                                span: expr.span.clone(),
                            });
                        }
                        Type::Primitive(PrimitiveType::I32)
                    }
                    Eq | NotEq | Lt | Gt | Le | Ge => {
                        if !self.types_compatible(&left_ty, &right_ty) {
                            self.tcx.errors.push(SemanticError {
                                message: "comparison operators require operands of the same type"
                                    .to_string(),
                                span: expr.span.clone(),
                            });
                        }
                        Type::Primitive(PrimitiveType::Bool)
                    }
                    And | Or => {
                        if !matches!(left_ty, Type::Primitive(PrimitiveType::Bool))
                            || !matches!(right_ty, Type::Primitive(PrimitiveType::Bool))
                        {
                            self.tcx.errors.push(SemanticError {
                                message: "logical operators expect operands of type `bool`"
                                    .to_string(),
                                span: expr.span.clone(),
                            });
                        }
                        Type::Primitive(PrimitiveType::Bool)
                    }
                }
            }
            ExprKind::Match { .. } => {
                // `match` expressions are not yet fully supported.
                self.tcx.errors.push(SemanticError {
                    message: "`match` expressions are not yet supported by the type checker"
                        .to_string(),
                    span: expr.span.clone(),
                });
                Type::Primitive(PrimitiveType::Unit)
            }
            ExprKind::Block(block) => {
                self.check_block(block);
                Type::Primitive(PrimitiveType::Unit)
            }
        }
    }

    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        expected == actual
    }
}

impl Resolver {
    fn new() -> Self {
        Self {
            symbols: Vec::new(),
            by_name: HashMap::new(),
            errors: Vec::new(),
        }
    }

    fn finish(self) -> ModuleSymbols {
        ModuleSymbols {
            symbols: self.symbols,
            by_name: self.by_name,
            errors: self.errors,
        }
    }

    fn collect(&mut self, file: &File) {
        for item in &file.items {
            self.collect_item(item);
        }
    }

    fn collect_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Fn { name, .. } => self.add_symbol(name, SymbolKind::Function),
            ItemKind::Struct { name, .. } => self.add_symbol(name, SymbolKind::Struct),
            ItemKind::Enum { name, .. } => self.add_symbol(name, SymbolKind::Enum),
            ItemKind::TypeAlias { name, .. } => self.add_symbol(name, SymbolKind::TypeAlias),
            ItemKind::ExternBlock { items, .. } => {
                for ext in items {
                    let husk_ast::ExternItemKind::Fn { name, .. } = &ext.kind;
                    self.add_symbol(name, SymbolKind::ExternFn);
                }
            }
        }
    }

    fn add_symbol(&mut self, ident: &Ident, kind: SymbolKind) {
        let name = ident.name.clone();
        if let Some(existing_id) = self.by_name.get(&name).copied() {
            // Duplicate symbol; record an error but keep the first definition.
            if let Some(existing) = self.symbols.get(existing_id.0 as usize) {
                self.errors.push(SemanticError {
                    message: format!("duplicate definition of `{}`", name),
                    span: ident.span.clone(),
                });
                // Optionally attach a note in the future pointing to `existing.span`.
                let _ = existing;
            }
            return;
        }

        let id = SymbolId(self.symbols.len() as u32);
        let symbol = Symbol {
            id,
            name: name.clone(),
            kind,
            span: ident.span.clone(),
        };
        self.symbols.push(symbol);
        self.by_name.insert(name, id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use husk_ast::{
        Expr, ExprKind, File, Ident, Item, ItemKind, Literal, LiteralKind, Span, Stmt, StmtKind,
        TypeExpr, TypeExprKind,
    };

    fn ident(name: &str, start: usize) -> Ident {
        Ident {
            name: name.to_string(),
            span: Span {
                range: start..start + name.len(),
            },
        }
    }

    #[test]
    fn collects_unique_top_level_symbols() {
        let f = File {
            items: vec![
                Item {
                    kind: ItemKind::Fn {
                        name: ident("foo", 0),
                        type_params: Vec::new(),
                        params: Vec::new(),
                        ret_type: None,
                        body: Vec::new(),
                    },
                    span: Span { range: 0..3 },
                },
                Item {
                    kind: ItemKind::Struct {
                        name: ident("Bar", 10),
                        type_params: Vec::new(),
                        fields: Vec::new(),
                    },
                    span: Span { range: 10..13 },
                },
            ],
        };

        let module = ModuleSymbols::from_file(&f);
        assert!(module.errors.is_empty());
        assert!(module.get("foo").is_some());
        assert!(module.get("Bar").is_some());
        assert_eq!(module.symbols.len(), 2);
    }

    #[test]
    fn reports_duplicate_definitions() {
        let f = File {
            items: vec![
                Item {
                    kind: ItemKind::Fn {
                        name: ident("foo", 0),
                        type_params: Vec::new(),
                        params: Vec::new(),
                        ret_type: None,
                        body: Vec::new(),
                    },
                    span: Span { range: 0..3 },
                },
                Item {
                    kind: ItemKind::Struct {
                        name: ident("foo", 10),
                        type_params: Vec::new(),
                        fields: Vec::new(),
                    },
                    span: Span { range: 10..13 },
                },
            ],
        };

        let module = ModuleSymbols::from_file(&f);
        assert_eq!(module.symbols.len(), 1);
        assert_eq!(module.errors.len(), 1);
        assert!(module.get("foo").is_some());
    }

    fn type_ident(name: &str, start: usize) -> TypeExpr {
        let id = ident(name, start);
        TypeExpr {
            kind: TypeExprKind::Named(id.clone()),
            span: id.span,
        }
    }

    #[test]
    fn analyze_well_typed_function_with_primitives() {
        // fn main() -> i32 {
        //     let x: i32 = 1;
        //     x
        // }
        let x_ident = ident("x", 20);
        let one_lit = Expr {
            kind: ExprKind::Literal(Literal {
                kind: LiteralKind::Int(1),
                span: Span { range: 30..31 },
            }),
            span: Span { range: 30..31 },
        };
        let let_stmt = Stmt {
            kind: StmtKind::Let {
                mutable: false,
                name: x_ident.clone(),
                ty: Some(type_ident("i32", 25)),
                value: Some(one_lit),
            },
            span: Span { range: 20..32 },
        };
        let ret_expr = Expr {
            kind: ExprKind::Ident(x_ident.clone()),
            span: x_ident.span.clone(),
        };
        let ret_stmt = Stmt {
            kind: StmtKind::Return {
                value: Some(ret_expr),
            },
            span: Span { range: 40..45 },
        };

        let file = File {
            items: vec![Item {
                kind: ItemKind::Fn {
                    name: ident("main", 0),
                    type_params: Vec::new(),
                    params: Vec::new(),
                    ret_type: Some(type_ident("i32", 10)),
                    body: vec![let_stmt, ret_stmt],
                },
                span: Span { range: 0..50 },
            }],
        };

        let result = analyze_file(&file);
        assert!(
            result.symbols.errors.is_empty(),
            "name errors: {:?}",
            result.symbols.errors
        );
        assert!(
            result.type_errors.is_empty(),
            "type errors: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn analyze_reports_mismatched_let_type() {
        // fn main() {
        //   let x: i32 = "oops";
        // }
        let x_ident = ident("x", 15);
        let string_lit = Expr {
            kind: ExprKind::Literal(Literal {
                kind: LiteralKind::String("oops".to_string()),
                span: Span { range: 25..31 },
            }),
            span: Span { range: 25..31 },
        };
        let let_stmt = Stmt {
            kind: StmtKind::Let {
                mutable: false,
                name: x_ident.clone(),
                ty: Some(type_ident("i32", 20)),
                value: Some(string_lit),
            },
            span: Span { range: 15..32 },
        };
        let file = File {
            items: vec![Item {
                kind: ItemKind::Fn {
                    name: ident("main", 0),
                    type_params: Vec::new(),
                    params: Vec::new(),
                    ret_type: None,
                    body: vec![let_stmt],
                },
                span: Span { range: 0..40 },
            }],
        };

        let result = analyze_file(&file);
        assert!(
            result
                .type_errors
                .iter()
                .any(|e| e.message.contains("mismatched types in `let`")),
            "expected mismatched let type error, got: {:?}",
            result.type_errors
        );
    }
}
