//! Name resolution and early semantic analysis for Husk.
//!
//! This crate currently defines:
//! - A basic symbol representation for top-level items.
//! - A resolver that collects top-level symbols from a `husk_ast::File`.

use std::collections::{HashMap, HashSet};
use std::sync::OnceLock;

use husk_ast::{
    Block, Expr, ExprKind, File, Ident, Item, ItemKind, LiteralKind, MatchArm, Param, Pattern,
    PatternKind, Span, Stmt, StmtKind, TypeExpr, TypeExprKind,
};
use husk_parser::parse_str;
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
    ExternMod,
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

/// Options controlling semantic analysis.
#[derive(Debug, Clone, Copy)]
pub struct SemanticOptions {
    /// If true, inject the stdlib prelude (Option/Result) into the type environment.
    pub prelude: bool,
}

impl Default for SemanticOptions {
    fn default() -> Self {
        Self { prelude: true }
    }
}

/// Run semantic analysis (name resolution + type checking) over the given file with options.
pub fn analyze_file_with_options(file: &File, opts: SemanticOptions) -> SemanticResult {
    let symbols = ModuleSymbols::from_file(file);

    let mut checker = TypeChecker::new();
    if opts.prelude {
        checker.build_type_env(prelude_file());
    }
    checker.build_type_env(file);
    let type_errors = checker.check_file(file);
    SemanticResult {
        symbols,
        type_errors,
    }
}

/// Run full semantic analysis with the stdlib prelude enabled (default).
pub fn analyze_file(file: &File) -> SemanticResult {
    analyze_file_with_options(file, SemanticOptions::default())
}

/// Run semantic analysis without injecting the stdlib prelude.
pub fn analyze_file_without_prelude(file: &File) -> SemanticResult {
    analyze_file_with_options(file, SemanticOptions { prelude: false })
}

static PRELUDE_SRC: &str = include_str!("../../../stdlib/core.hk");
static PRELUDE_AST: OnceLock<File> = OnceLock::new();

fn prelude_file() -> &'static File {
    PRELUDE_AST.get_or_init(|| {
        let parsed = parse_str(PRELUDE_SRC);
        if !parsed.errors.is_empty() {
            panic!("failed to parse stdlib prelude: {:?}", parsed.errors);
        }
        parsed.file.expect("stdlib prelude parse produced no AST")
    })
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
    variant_names: Vec<String>,
}

/// Information about a function type.
#[derive(Debug, Clone)]
struct FnDef {
    params: Vec<Param>,
    ret_type: Option<TypeExpr>,
}

/// Information about an imported JS module.
/// The module name becomes a callable identifier that returns an opaque type.
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct ModuleDef {
    name: String,
}

#[derive(Debug, Default)]
struct TypeEnv {
    structs: HashMap<String, StructDef>,
    enums: HashMap<String, EnumDef>,
    type_aliases: HashMap<String, TypeExpr>,
    functions: HashMap<String, FnDef>,
    /// Imported JS modules (from `mod name;` in extern blocks).
    /// These become callable identifiers.
    modules: HashMap<String, ModuleDef>,
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
                    name,
                    type_params,
                    variants,
                } => {
                    let def = EnumDef {
                        type_params: type_params.iter().map(|id| id.name.clone()).collect(),
                        variant_names: variants.iter().map(|v| v.name.name.clone()).collect(),
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
                        match &ext.kind {
                            husk_ast::ExternItemKind::Fn {
                                name,
                                params,
                                ret_type,
                            } => {
                                let def = FnDef {
                                    params: params.clone(),
                                    ret_type: ret_type.clone(),
                                };
                                self.env.functions.insert(name.name.clone(), def);
                            }
                            husk_ast::ExternItemKind::Mod { binding, .. } => {
                                // Module imports become callable identifiers.
                                let def = ModuleDef {
                                    name: binding.name.clone(),
                                };
                                self.env.modules.insert(binding.name.clone(), def);
                            }
                        }
                    }
                }
                ItemKind::Use { .. } => {}
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
    fn check_path_expr(&mut self, expr: &Expr, segments: &[Ident]) -> Type {
        // MVP: treat `Enum::Variant` as constructing a value of enum type `Enum`.
        if segments.len() >= 2 {
            let enum_name = &segments[0].name;
            let variant_name = &segments[segments.len() - 1].name;

            if let Some(def) = self.tcx.env.enums.get(enum_name).cloned() {
                if !def.variant_names.iter().any(|v| v == variant_name) {
                    self.tcx.errors.push(SemanticError {
                        message: format!(
                            "unknown variant `{}` on enum `{}`",
                            variant_name, enum_name
                        ),
                        span: expr.span.clone(),
                    });
                }
                // For now, generics are erased at runtime; we return the bare enum type.
                return Type::Named {
                    name: enum_name.clone(),
                    args: Vec::new(),
                };
            }
            // Fallback: unknown enum name.
            self.tcx.errors.push(SemanticError {
                message: format!("unknown enum `{}` in path expression", enum_name),
                span: expr.span.clone(),
            });
        } else {
            self.tcx.errors.push(SemanticError {
                message: "path expression must have at least two segments".to_string(),
                span: expr.span.clone(),
            });
        }
        Type::Primitive(PrimitiveType::Unit)
    }

    fn check_match_expr(&mut self, expr: &Expr, scrutinee: &Expr, arms: &[MatchArm]) -> Type {
        let scrut_ty = self.check_expr(scrutinee);

        // Try to interpret scrutinee as an enum.
        let enum_info = match &scrut_ty {
            Type::Named { name, .. } => self
                .tcx
                .env
                .enums
                .get(name)
                .map(|def| (name.clone(), def.variant_names.clone())),
            _ => None,
        };

        let mut result_ty: Option<Type> = None;
        let mut seen_variants: HashSet<String> = HashSet::new();
        let mut has_catch_all = false;

        for arm in arms {
            // Track patterns for exhaustiveness.
            match &arm.pattern.kind {
                PatternKind::Wildcard | PatternKind::Binding(_) => {
                    has_catch_all = true;
                }
                PatternKind::EnumUnit { path } => {
                    if let Some((enum_name, variant_names)) = &enum_info {
                        // Expect path like Enum::Variant
                        if path.len() == 2 && path[0].name == *enum_name {
                            let variant = path[1].name.clone();
                            if !variant_names.contains(&variant) {
                                self.tcx.errors.push(SemanticError {
                                    message: format!(
                                        "unknown variant `{}` for enum `{}`",
                                        variant, enum_name
                                    ),
                                    span: arm.pattern.span.clone(),
                                });
                            } else {
                                seen_variants.insert(variant);
                            }
                        } else {
                            self.tcx.errors.push(SemanticError {
                                message: format!(
                                    "enum pattern must use `{0}::Variant` for enum `{0}`",
                                    enum_name
                                ),
                                span: arm.pattern.span.clone(),
                            });
                        }
                    }
                }
                // Tuple/struct patterns not yet used in exhaustiveness.
                PatternKind::EnumTuple { .. } | PatternKind::EnumStruct { .. } => {}
            }

            // Type-check the arm expression in a fresh scope with any bindings from the pattern.
            let saved_locals = self.locals.clone();
            self.bind_pattern_locals(&arm.pattern, &scrut_ty);
            let arm_ty = self.check_expr(&arm.expr);
            self.locals = saved_locals;

            match &mut result_ty {
                None => result_ty = Some(arm_ty),
                Some(expected) => {
                    if !self.types_compatible(expected, &arm_ty) {
                        self.tcx.errors.push(SemanticError {
                            message: format!(
                                "mismatched types in match arms: expected `{:?}`, found `{:?}`",
                                expected, arm_ty
                            ),
                            span: arm.expr.span.clone(),
                        });
                    }
                }
            }
        }

        // Exhaustiveness for simple enums.
        if let Some((enum_name, variant_names)) = enum_info {
            if !has_catch_all {
                for variant in &variant_names {
                    if !seen_variants.contains(variant) {
                        self.tcx.errors.push(SemanticError {
                            message: format!(
                                "non-exhaustive match on enum `{}`: missing variant `{}`",
                                enum_name, variant
                            ),
                            span: expr.span.clone(),
                        });
                    }
                }
            }
        }

        result_ty.unwrap_or(Type::Primitive(PrimitiveType::Unit))
    }

    fn bind_pattern_locals(&mut self, pat: &Pattern, scrut_ty: &Type) {
        match &pat.kind {
            PatternKind::Wildcard => {}
            PatternKind::Binding(id) => {
                if self
                    .locals
                    .insert(id.name.clone(), scrut_ty.clone())
                    .is_some()
                {
                    self.tcx.errors.push(SemanticError {
                        message: format!("duplicate binding `{}` in pattern", id.name),
                        span: id.span.clone(),
                    });
                }
            }
            PatternKind::EnumUnit { .. } => {}
            PatternKind::EnumTuple { .. } | PatternKind::EnumStruct { .. } => {
                // Not yet supported for bindings.
            }
        }
    }
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
            ExprKind::Path { segments } => self.check_path_expr(expr, segments),
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

                // Try imported JS module (from `mod name;` in extern block).
                // Modules are treated as callable with any args, returning an opaque type.
                if self.tcx.env.modules.contains_key(&id.name) {
                    // Return a function type that accepts any args and returns an opaque type.
                    // For MVP, we use an empty params list and return a named type.
                    return Type::Function {
                        params: Vec::new(),
                        ret: Box::new(Type::Named {
                            name: id.name.clone(),
                            args: Vec::new(),
                        }),
                    };
                }

                self.tcx.errors.push(SemanticError {
                    message: format!("unknown identifier `{}`", id.name),
                    span: id.span.clone(),
                });
                Type::Primitive(PrimitiveType::Unit)
            }
            ExprKind::Call { callee, args } => {
                // Check if the callee is a module import (which accepts any arguments)
                let is_module_call = match &callee.kind {
                    ExprKind::Ident(id) => self.tcx.env.modules.contains_key(&id.name),
                    _ => false,
                };

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

                // Skip arity checking for module imports - they accept any number of args
                if !is_module_call && param_tys.len() != args.len() {
                    self.tcx.errors.push(SemanticError {
                        message: format!(
                            "function expects {} argument(s), got {}",
                            param_tys.len(),
                            args.len()
                        ),
                        span: expr.span.clone(),
                    });
                }

                // Type-check arguments (skip for module calls since we don't know the signature)
                if !is_module_call {
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
                } else {
                    // Still type-check the arguments, but don't enforce types
                    for arg in args.iter() {
                        let _ = self.check_expr(arg);
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
                    Add => {
                        // Add supports both i32 + i32 and String + String
                        if matches!(left_ty, Type::Primitive(PrimitiveType::String))
                            && matches!(right_ty, Type::Primitive(PrimitiveType::String))
                        {
                            Type::Primitive(PrimitiveType::String)
                        } else if matches!(left_ty, Type::Primitive(PrimitiveType::I32))
                            && matches!(right_ty, Type::Primitive(PrimitiveType::I32))
                        {
                            Type::Primitive(PrimitiveType::I32)
                        } else {
                            self.tcx.errors.push(SemanticError {
                                message: "`+` requires operands of the same type (`i32` or `String`)"
                                    .to_string(),
                                span: expr.span.clone(),
                            });
                            Type::Primitive(PrimitiveType::I32)
                        }
                    }
                    Sub | Mul | Div => {
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
            ExprKind::Match { scrutinee, arms } => self.check_match_expr(expr, scrutinee, arms),
            ExprKind::Block(block) => {
                self.check_block(block);
                Type::Primitive(PrimitiveType::Unit)
            }
            ExprKind::Struct { name, fields } => {
                // Type-check field expressions and resolve to the struct type.
                for field in fields {
                    self.check_expr(&field.value);
                }
                // Use the last segment of the path as the type name.
                let type_name = name.last().map(|id| id.name.clone()).unwrap_or_default();
                Type::Named {
                    name: type_name,
                    args: Vec::new(),
                }
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
                    match &ext.kind {
                        husk_ast::ExternItemKind::Fn { name, .. } => {
                            self.add_symbol(name, SymbolKind::ExternFn);
                        }
                        husk_ast::ExternItemKind::Mod { binding, .. } => {
                            self.add_symbol(binding, SymbolKind::ExternMod);
                        }
                    }
                }
            }
            ItemKind::Use { .. } => {}
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
        EnumVariant, EnumVariantFields, Expr, ExprKind, File, Ident, Item, ItemKind, Literal,
        LiteralKind, MatchArm, Pattern, PatternKind, Span, Stmt, StmtKind, TypeExpr, TypeExprKind,
    };
    use husk_parser::parse_str;

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
                    visibility: husk_ast::Visibility::Private,
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
                    visibility: husk_ast::Visibility::Private,
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
                    visibility: husk_ast::Visibility::Private,
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
                    visibility: husk_ast::Visibility::Private,
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
                visibility: husk_ast::Visibility::Private,
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
                visibility: husk_ast::Visibility::Private,
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

    #[test]
    fn path_expr_for_enum_variant_has_enum_type() {
        // enum Color { Red, Blue }
        // fn make_red() -> Color { Color::Red }
        let color_ident = ident("Color", 0);
        let enum_item = Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::Enum {
                name: color_ident.clone(),
                type_params: Vec::new(),
                variants: vec![
                    EnumVariant {
                        name: ident("Red", 10),
                        fields: EnumVariantFields::Unit,
                    },
                    EnumVariant {
                        name: ident("Blue", 20),
                        fields: EnumVariantFields::Unit,
                    },
                ],
            },
            span: Span { range: 0..30 },
        };

        let path_expr = Expr {
            kind: ExprKind::Path {
                segments: vec![color_ident.clone(), ident("Red", 40)],
            },
            span: Span { range: 30..45 },
        };
        let ret_stmt = Stmt {
            kind: StmtKind::Return {
                value: Some(path_expr),
            },
            span: Span { range: 30..50 },
        };
        let fn_item = Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::Fn {
                name: ident("make_red", 30),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: Some(type_ident("Color", 35)),
                body: vec![ret_stmt],
            },
            span: Span { range: 30..60 },
        };

        let file = File {
            items: vec![enum_item, fn_item],
        };

        let result = analyze_file(&file);
        assert!(
            result.type_errors.is_empty(),
            "expected no type errors, got {:?}",
            result.type_errors
        );
    }

    #[test]
    fn match_on_enum_exhaustive_is_ok() {
        // enum Color { Red, Blue }
        // fn f(c: Color) -> i32 {
        //     return match c {
        //         Color::Red => 1,
        //         Color::Blue => 2,
        //     };
        // }
        let color_ident = ident("Color", 0);
        let enum_item = Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::Enum {
                name: color_ident.clone(),
                type_params: Vec::new(),
                variants: vec![
                    EnumVariant {
                        name: ident("Red", 10),
                        fields: EnumVariantFields::Unit,
                    },
                    EnumVariant {
                        name: ident("Blue", 20),
                        fields: EnumVariantFields::Unit,
                    },
                ],
            },
            span: Span { range: 0..30 },
        };

        let c_ident = ident("c", 40);
        let param = Param {
            name: c_ident.clone(),
            ty: type_ident("Color", 42),
        };
        let scrutinee = Expr {
            kind: ExprKind::Ident(c_ident.clone()),
            span: c_ident.span.clone(),
        };
        let pat_red = Pattern {
            kind: PatternKind::EnumUnit {
                path: vec![color_ident.clone(), ident("Red", 60)],
            },
            span: Span { range: 50..63 },
        };
        let pat_blue = Pattern {
            kind: PatternKind::EnumUnit {
                path: vec![color_ident.clone(), ident("Blue", 70)],
            },
            span: Span { range: 64..78 },
        };
        let arm_red = MatchArm {
            pattern: pat_red,
            expr: Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Int(1),
                    span: Span { range: 80..81 },
                }),
                span: Span { range: 80..81 },
            },
        };
        let arm_blue = MatchArm {
            pattern: pat_blue,
            expr: Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Int(2),
                    span: Span { range: 90..91 },
                }),
                span: Span { range: 90..91 },
            },
        };
        let match_expr = Expr {
            kind: ExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms: vec![arm_red, arm_blue],
            },
            span: Span { range: 50..100 },
        };
        let ret_stmt = Stmt {
            kind: StmtKind::Return {
                value: Some(match_expr),
            },
            span: Span { range: 50..105 },
        };
        let fn_item = Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::Fn {
                name: ident("f", 40),
                type_params: Vec::new(),
                params: vec![param],
                ret_type: Some(type_ident("i32", 45)),
                body: vec![ret_stmt],
            },
            span: Span { range: 40..110 },
        };

        let file = File {
            items: vec![enum_item, fn_item],
        };

        let result = analyze_file(&file);
        assert!(
            result.type_errors.is_empty(),
            "expected no type errors, got {:?}",
            result.type_errors
        );
    }

    #[test]
    fn match_on_enum_non_exhaustive_reports_error() {
        // enum Color { Red, Blue }
        // fn f(c: Color) -> i32 {
        //     return match c {
        //         Color::Red => 1,
        //     };
        // }
        let color_ident = ident("Color", 0);
        let enum_item = Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::Enum {
                name: color_ident.clone(),
                type_params: Vec::new(),
                variants: vec![
                    EnumVariant {
                        name: ident("Red", 10),
                        fields: EnumVariantFields::Unit,
                    },
                    EnumVariant {
                        name: ident("Blue", 20),
                        fields: EnumVariantFields::Unit,
                    },
                ],
            },
            span: Span { range: 0..30 },
        };

        let c_ident = ident("c", 40);
        let param = Param {
            name: c_ident.clone(),
            ty: type_ident("Color", 42),
        };
        let scrutinee = Expr {
            kind: ExprKind::Ident(c_ident.clone()),
            span: c_ident.span.clone(),
        };
        let pat_red = Pattern {
            kind: PatternKind::EnumUnit {
                path: vec![color_ident.clone(), ident("Red", 60)],
            },
            span: Span { range: 50..63 },
        };
        let arm_red = MatchArm {
            pattern: pat_red,
            expr: Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Int(1),
                    span: Span { range: 80..81 },
                }),
                span: Span { range: 80..81 },
            },
        };
        let match_expr = Expr {
            kind: ExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms: vec![arm_red],
            },
            span: Span { range: 50..100 },
        };
        let ret_stmt = Stmt {
            kind: StmtKind::Return {
                value: Some(match_expr),
            },
            span: Span { range: 50..105 },
        };
        let fn_item = Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::Fn {
                name: ident("f", 40),
                type_params: Vec::new(),
                params: vec![param],
                ret_type: Some(type_ident("i32", 45)),
                body: vec![ret_stmt],
            },
            span: Span { range: 40..110 },
        };

        let file = File {
            items: vec![enum_item, fn_item],
        };

        let result = analyze_file(&file);
        assert!(
            result
                .type_errors
                .iter()
                .any(|e| e.message.contains("non-exhaustive match on enum `Color`")),
            "expected non-exhaustive match error, got {:?}",
            result.type_errors
        );
    }

    #[test]
    fn module_imports_accept_any_number_of_arguments() {
        // extern "js" { mod express; }
        // fn main() {
        //     let app = express();           // 0 args - should be fine
        //     let app2 = express(42);        // 1 arg - should be fine
        //     let app3 = express(1, 2, 3);   // 3 args - should be fine
        // }
        let express_ident = ident("express", 0);
        let extern_item = husk_ast::ExternItem {
            kind: husk_ast::ExternItemKind::Mod {
                package: "express".to_string(),
                binding: express_ident.clone(),
            },
            span: Span { range: 0..15 },
        };
        let extern_block = Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::ExternBlock {
                abi: "js".to_string(),
                items: vec![extern_item],
            },
            span: Span { range: 0..20 },
        };

        // let app = express();
        let call_0_args = Expr {
            kind: ExprKind::Call {
                callee: Box::new(Expr {
                    kind: ExprKind::Ident(express_ident.clone()),
                    span: express_ident.span.clone(),
                }),
                args: vec![],
            },
            span: Span { range: 30..40 },
        };
        let let_app = Stmt {
            kind: StmtKind::Let {
                mutable: false,
                name: ident("app", 25),
                ty: None,
                value: Some(call_0_args),
            },
            span: Span { range: 25..45 },
        };

        // let app2 = express(42);
        let call_1_arg = Expr {
            kind: ExprKind::Call {
                callee: Box::new(Expr {
                    kind: ExprKind::Ident(express_ident.clone()),
                    span: express_ident.span.clone(),
                }),
                args: vec![Expr {
                    kind: ExprKind::Literal(Literal {
                        kind: LiteralKind::Int(42),
                        span: Span { range: 60..62 },
                    }),
                    span: Span { range: 60..62 },
                }],
            },
            span: Span { range: 50..65 },
        };
        let let_app2 = Stmt {
            kind: StmtKind::Let {
                mutable: false,
                name: ident("app2", 45),
                ty: None,
                value: Some(call_1_arg),
            },
            span: Span { range: 45..70 },
        };

        // let app3 = express(1, 2, 3);
        let call_3_args = Expr {
            kind: ExprKind::Call {
                callee: Box::new(Expr {
                    kind: ExprKind::Ident(express_ident.clone()),
                    span: express_ident.span.clone(),
                }),
                args: vec![
                    Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: Span { range: 80..81 },
                        }),
                        span: Span { range: 80..81 },
                    },
                    Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(2),
                            span: Span { range: 83..84 },
                        }),
                        span: Span { range: 83..84 },
                    },
                    Expr {
                        kind: ExprKind::Literal(Literal {
                            kind: LiteralKind::Int(3),
                            span: Span { range: 86..87 },
                        }),
                        span: Span { range: 86..87 },
                    },
                ],
            },
            span: Span { range: 75..90 },
        };
        let let_app3 = Stmt {
            kind: StmtKind::Let {
                mutable: false,
                name: ident("app3", 70),
                ty: None,
                value: Some(call_3_args),
            },
            span: Span { range: 70..95 },
        };

        let fn_item = Item {
            visibility: husk_ast::Visibility::Private,
            kind: ItemKind::Fn {
                name: ident("main", 100),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: vec![let_app, let_app2, let_app3],
            },
            span: Span { range: 100..150 },
        };

        let file = File {
            items: vec![extern_block, fn_item],
        };

        let result = analyze_file(&file);
        // Should have no type errors - module imports accept any number of arguments
        assert!(
            result.type_errors.is_empty(),
            "expected no type errors for module calls with any args, got {:?}",
            result.type_errors
        );
    }

    #[test]
    fn prelude_option_available_by_default() {
        let src = r#"
fn main() {
    let _v: Option<i32>;
}
"#;
        let parsed = parse_str(src);
        assert!(
            parsed.errors.is_empty(),
            "parse errors: {:?}",
            parsed.errors
        );
        let file = parsed.file.expect("parser produced no AST");
        let result = analyze_file(&file);
        assert!(
            result.symbols.errors.is_empty() && result.type_errors.is_empty(),
            "semantic errors: symbols={:?}, types={:?}",
            result.symbols.errors,
            result.type_errors
        );
    }
}
