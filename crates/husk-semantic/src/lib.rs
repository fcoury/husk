//! Name resolution and early semantic analysis for Husk.
//!
//! This crate currently defines:
//! - A basic symbol representation for top-level items.
//! - A resolver that collects top-level symbols from a `husk_ast::File`.

use std::collections::{HashMap, HashSet};
use std::sync::OnceLock;

use husk_ast::{
    Block, CfgPredicate, ClosureParam, EnumVariantFields, Expr, ExprKind, File, FormatSegment,
    Ident, Item, ItemKind, LiteralKind, MatchArm, Param, Pattern, PatternKind, Span, Stmt,
    StmtKind, TypeExpr, TypeExprKind,
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
    ExternStatic,
    Trait,
    Impl,
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

/// Maps variable binding and usage spans to their resolved unique names.
///
/// During code generation, variable references need unique names to handle
/// shadowing correctly. JavaScript doesn't allow redeclaring `let` in the
/// same scope, so we use alpha-conversion (renaming) to generate unique names.
///
/// The map uses byte ranges (start, end) as keys since `Span` doesn't implement Hash.
/// Values are the resolved names (e.g., "x", "x$1", "x$2").
pub type NameResolution = HashMap<(usize, usize), String>;

/// The result of running semantic analysis (name resolution + type checking) on a Husk file.
#[derive(Debug)]
pub struct SemanticResult {
    pub symbols: ModuleSymbols,
    pub type_errors: Vec<SemanticError>,
    /// Maps variable spans to their resolved unique names for codegen.
    pub name_resolution: NameResolution,
}

/// Options controlling semantic analysis.
#[derive(Debug, Clone, Default)]
pub struct SemanticOptions {
    /// If true, inject the stdlib prelude (Option/Result) into the type environment.
    pub prelude: bool,
    /// Cfg flags that are currently enabled (e.g., "test" for test mode).
    pub cfg_flags: HashSet<String>,
}

impl SemanticOptions {
    /// Create options with prelude enabled (default behavior).
    pub fn with_prelude() -> Self {
        Self {
            prelude: true,
            cfg_flags: HashSet::new(),
        }
    }

    /// Create options with the test cfg flag enabled.
    pub fn with_test() -> Self {
        let mut flags = HashSet::new();
        flags.insert("test".to_string());
        Self {
            prelude: true,
            cfg_flags: flags,
        }
    }
}

/// Evaluate a cfg predicate against a set of enabled flags.
fn evaluate_cfg(predicate: &CfgPredicate, flags: &HashSet<String>) -> bool {
    match predicate {
        CfgPredicate::Flag(name) => flags.contains(name),
        CfgPredicate::KeyValue { key, value } => {
            // For key-value predicates like cfg(feature = "foo"),
            // check if the flag "feature=foo" is enabled
            let combined = format!("{}={}", key, value);
            flags.contains(&combined)
        }
        CfgPredicate::All(predicates) => predicates.iter().all(|p| evaluate_cfg(p, flags)),
        CfgPredicate::Any(predicates) => predicates.iter().any(|p| evaluate_cfg(p, flags)),
        CfgPredicate::Not(predicate) => !evaluate_cfg(predicate, flags),
    }
}

/// Check if an item should be included based on its cfg attributes.
fn item_passes_cfg(item: &Item, flags: &HashSet<String>) -> bool {
    // If the item has a cfg predicate, evaluate it
    if let Some(predicate) = item.cfg_predicate() {
        evaluate_cfg(predicate, flags)
    } else {
        // No cfg attribute means always include
        true
    }
}

/// Filter a file's items based on cfg predicates.
pub fn filter_items_by_cfg(file: &File, flags: &HashSet<String>) -> File {
    File {
        items: file
            .items
            .iter()
            .filter(|item| item_passes_cfg(item, flags))
            .cloned()
            .collect(),
    }
}

/// Run semantic analysis (name resolution + type checking) over the given file with options.
pub fn analyze_file_with_options(file: &File, opts: SemanticOptions) -> SemanticResult {
    // Filter items based on cfg predicates
    let filtered_file = filter_items_by_cfg(file, &opts.cfg_flags);

    let symbols = ModuleSymbols::from_file(&filtered_file);

    let mut checker = TypeChecker::new();
    if opts.prelude {
        checker.build_type_env(prelude_file());
        checker.build_type_env(js_globals_file());
    }
    checker.build_type_env(&filtered_file);
    let (type_errors, name_resolution) = checker.check_file(&filtered_file);
    SemanticResult {
        symbols,
        type_errors,
        name_resolution,
    }
}

/// Run full semantic analysis with the stdlib prelude enabled (default).
pub fn analyze_file(file: &File) -> SemanticResult {
    analyze_file_with_options(file, SemanticOptions::with_prelude())
}

/// Run semantic analysis without injecting the stdlib prelude.
pub fn analyze_file_without_prelude(file: &File) -> SemanticResult {
    analyze_file_with_options(
        file,
        SemanticOptions {
            prelude: false,
            cfg_flags: HashSet::new(),
        },
    )
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

static JS_GLOBALS_SRC: &str = include_str!("../../../std/js/globals.hk");
static JS_GLOBALS_AST: OnceLock<File> = OnceLock::new();

fn js_globals_file() -> &'static File {
    JS_GLOBALS_AST.get_or_init(|| {
        let parsed = parse_str(JS_GLOBALS_SRC);
        if !parsed.errors.is_empty() {
            panic!("failed to parse JS globals: {:?}", parsed.errors);
        }
        parsed.file.expect("JS globals parse produced no AST")
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

/// Information about an enum variant.
#[derive(Debug, Clone)]
struct VariantDef {
    name: String,
    fields: EnumVariantFields,
}

/// Information about an enum type.
#[derive(Debug, Clone)]
struct EnumDef {
    type_params: Vec<String>,
    variants: Vec<VariantDef>,
}

/// Information about a type parameter with optional trait bounds.
#[derive(Debug, Clone, PartialEq)]
struct TypeParamInfo {
    name: String,
    /// Trait bounds on this type parameter (e.g., `T: PartialEq + Clone`)
    bounds: Vec<String>,
}

/// Information about a function type.
#[derive(Debug, Clone)]
struct FnDef {
    type_params: Vec<TypeParamInfo>,
    params: Vec<Param>,
    ret_type: Option<TypeExpr>,
}

impl FnDef {
    /// Get just the type parameter names (for use in type resolution).
    fn type_param_names(&self) -> Vec<String> {
        self.type_params.iter().map(|tp| tp.name.clone()).collect()
    }
}

/// Convert an AST TypeParam to a TypeParamInfo.
fn type_param_to_info(tp: &husk_ast::TypeParam) -> TypeParamInfo {
    TypeParamInfo {
        name: tp.name.name.clone(),
        bounds: tp.bounds.iter().map(type_expr_to_trait_name).collect(),
    }
}

/// Extract the trait name from a TypeExpr (for bounds).
fn type_expr_to_trait_name(ty: &husk_ast::TypeExpr) -> String {
    use husk_ast::TypeExprKind;
    match &ty.kind {
        TypeExprKind::Named(name) => name.name.clone(),
        TypeExprKind::Generic { name, args } => {
            let arg_strs: Vec<String> = args.iter().map(type_expr_to_trait_name).collect();
            format!("{}<{}>", name.name, arg_strs.join(", "))
        }
        TypeExprKind::Function { .. } => "Fn".to_string(), // Simplified
        TypeExprKind::Array(elem) => format!("[{}]", type_expr_to_trait_name(elem)),
    }
}

/// Information about an imported JS module.
/// The module name becomes a callable identifier that returns an opaque type.
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct ModuleDef {
    name: String,
    /// Return type for calling this module as a function.
    /// Inferred from the first struct in the same extern block if not the capitalized name.
    ret_type: Option<String>,
    /// Functions defined within this module (for extern mod blocks with functions).
    functions: HashMap<String, FnDef>,
}

/// Information about a trait definition.
#[derive(Debug, Clone)]
struct TraitInfo {
    #[allow(dead_code)]
    type_params: Vec<String>,
    /// Supertraits that this trait requires (e.g., `Eq: PartialEq`)
    supertraits: Vec<String>,
    /// Method signatures: name -> (params, return type)
    methods: HashMap<String, MethodSig>,
}

/// A method signature (for traits and impls).
#[derive(Debug, Clone)]
struct MethodSig {
    #[allow(dead_code)]
    receiver: Option<husk_ast::SelfReceiver>,
    #[allow(dead_code)]
    params: Vec<Param>,
    #[allow(dead_code)]
    ret_type: Option<TypeExpr>,
}

/// Information about an impl block.
#[derive(Debug, Clone)]
struct ImplInfo {
    /// The trait being implemented (None for inherent impl).
    /// Used by `verify_trait_impls` to check method completeness.
    trait_name: Option<String>,
    /// The type this impl is for
    self_ty_name: String,
    /// Methods defined in this impl
    methods: HashMap<String, MethodInfo>,
    /// Extern properties defined in this impl
    properties: HashMap<String, PropertyInfo>,
    /// Location of the impl block for error reporting
    span: Span,
}

/// Information about an extern property in an impl block.
#[derive(Debug, Clone)]
struct PropertyInfo {
    /// The property type
    ty: TypeExpr,
    /// Whether the property has a getter
    #[allow(dead_code)]
    has_getter: bool,
    /// Whether the property has a setter
    #[allow(dead_code)]
    has_setter: bool,
}

/// Information about a method in an impl block.
#[derive(Debug, Clone)]
struct MethodInfo {
    #[allow(dead_code)]
    receiver: Option<husk_ast::SelfReceiver>,
    #[allow(dead_code)]
    params: Vec<Param>,
    ret_type: Option<TypeExpr>,
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
    /// Trait definitions
    traits: HashMap<String, TraitInfo>,
    /// Impl blocks (can have multiple impls for the same type)
    impls: Vec<ImplInfo>,
    /// Static/global variables (from `static name: Type;` in extern blocks)
    statics: HashMap<String, TypeExpr>,
}

impl TypeEnv {
    /// Check if a type implements a trait (directly or through supertrait requirements).
    ///
    /// This performs the following checks:
    /// 1. Looks for a direct `impl Trait for Type` block
    /// 2. For supertraits, checks if all required supertraits are also implemented
    ///
    /// Returns true if the type implements the trait (including via supertrait satisfaction).
    ///
    /// Note: For generic types like `Vec<i32>`, we also check the base type `Vec`
    /// since impls are registered on the base struct name.
    fn type_implements_trait(&self, type_name: &str, trait_name: &str) -> bool {
        // Extract base type name for generic types (e.g., "Vec<i32>" -> "Vec")
        let base_type_name = type_name.split('<').next().unwrap_or(type_name);

        // Check for direct trait implementation
        for impl_info in &self.impls {
            // Match either the exact type name or the base type for generics
            if impl_info.self_ty_name == type_name || impl_info.self_ty_name == base_type_name {
                if let Some(ref impl_trait) = impl_info.trait_name {
                    if impl_trait == trait_name {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Get the list of missing supertrait implementations for a type implementing a trait.
    ///
    /// Returns a list of all transitively missing supertraits that are required but not implemented.
    /// For example, if Eq: PartialEq and PartialEq: SomeTrait, and SomeTrait is not implemented,
    /// this will return both PartialEq (if missing) and SomeTrait.
    fn missing_supertraits(&self, type_name: &str, trait_name: &str) -> Vec<String> {
        let mut missing = Vec::new();
        let mut visited = std::collections::HashSet::new();
        self.collect_missing_supertraits_recursive(type_name, trait_name, &mut missing, &mut visited);
        missing
    }

    /// Recursive helper for missing_supertraits with cycle detection.
    fn collect_missing_supertraits_recursive(
        &self,
        type_name: &str,
        trait_name: &str,
        missing: &mut Vec<String>,
        visited: &mut std::collections::HashSet<String>,
    ) {
        // Guard against cycles
        if visited.contains(trait_name) {
            return;
        }
        visited.insert(trait_name.to_string());

        if let Some(trait_info) = self.traits.get(trait_name) {
            for supertrait in &trait_info.supertraits {
                // Check if this supertrait is missing
                if !self.type_implements_trait(type_name, supertrait) {
                    if !missing.contains(supertrait) {
                        missing.push(supertrait.clone());
                    }
                }
                // Recursively check the supertrait's supertraits
                self.collect_missing_supertraits_recursive(type_name, supertrait, missing, visited);
            }
        }
    }
}

/// Extract a simple type name from a TypeExpr (for impl/trait lookups).
fn type_expr_to_name(ty: &TypeExpr) -> String {
    match &ty.kind {
        TypeExprKind::Named(ident) => ident.name.clone(),
        TypeExprKind::Generic { name, .. } => name.name.clone(),
        TypeExprKind::Function { .. } => "<fn>".to_string(),
        TypeExprKind::Array(elem) => format!("[{}]", type_expr_to_name(elem)),
    }
}

struct TypeChecker {
    env: TypeEnv,
    errors: Vec<SemanticError>,
    /// Maps variable spans to their resolved unique names for codegen.
    name_resolution: NameResolution,
}

impl TypeChecker {
    fn new() -> Self {
        Self {
            env: TypeEnv::default(),
            errors: Vec::new(),
            name_resolution: HashMap::new(),
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
                        variants: variants
                            .iter()
                            .map(|v| VariantDef {
                                name: v.name.name.clone(),
                                fields: v.fields.clone(),
                            })
                            .collect(),
                    };
                    self.env.enums.insert(name.name.clone(), def);
                }
                ItemKind::TypeAlias { name, ty } => {
                    self.env.type_aliases.insert(name.name.clone(), ty.clone());
                }
                ItemKind::Fn {
                    name,
                    type_params,
                    params,
                    ret_type,
                    ..
                } => {
                    let def = FnDef {
                        type_params: type_params.iter().map(type_param_to_info).collect(),
                        params: params.clone(),
                        ret_type: ret_type.clone(),
                    };
                    self.env.functions.insert(name.name.clone(), def);
                }
                ItemKind::ExternBlock { items, .. } => {
                    // First pass: collect all struct names in this extern block
                    let mut struct_names: Vec<String> = Vec::new();
                    for ext in items {
                        if let husk_ast::ExternItemKind::Struct { name, .. } = &ext.kind {
                            struct_names.push(name.name.clone());
                        }
                    }

                    // Second pass: register all items
                    for ext in items {
                        match &ext.kind {
                            husk_ast::ExternItemKind::Fn {
                                name,
                                params,
                                ret_type,
                            } => {
                                let def = FnDef {
                                    type_params: Vec::new(), // Extern functions don't have generics
                                    params: params.clone(),
                                    ret_type: ret_type.clone(),
                                };
                                self.env.functions.insert(name.name.clone(), def);
                            }
                            husk_ast::ExternItemKind::Mod { binding, items: mod_items, .. } => {
                                if mod_items.is_empty() {
                                    // Simple module import becomes a callable identifier.
                                    // Try to infer return type from struct in same block:
                                    // 1. First, try capitalizing the first letter (e.g., express -> Express)
                                    // 2. If that doesn't match, use the first struct in the block
                                    let capitalized = {
                                        let mut chars = binding.name.chars();
                                        match chars.next() {
                                            Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
                                            None => binding.name.clone(),
                                        }
                                    };
                                    let ret_type = if struct_names.contains(&capitalized) {
                                        Some(capitalized)
                                    } else {
                                        // Use the first struct in the block as a fallback
                                        struct_names.first().cloned()
                                    };
                                    let def = ModuleDef {
                                        name: binding.name.clone(),
                                        ret_type,
                                        functions: HashMap::new(),
                                    };
                                    self.env.modules.insert(binding.name.clone(), def);
                                } else {
                                    // Mod block with functions - create module with its functions.
                                    let mut functions = HashMap::new();
                                    for mod_item in mod_items {
                                        // ModItemKind has only Fn variant (MVP scope)
                                        let husk_ast::ModItemKind::Fn {
                                            name,
                                            params,
                                            ret_type,
                                        } = &mod_item.kind;
                                        let def = FnDef {
                                            type_params: Vec::new(), // Mod functions don't have generics
                                            params: params.clone(),
                                            ret_type: ret_type.clone(),
                                        };
                                        functions.insert(name.name.clone(), def);
                                    }
                                    let module_def = ModuleDef {
                                        name: binding.name.clone(),
                                        ret_type: None,
                                        functions,
                                    };
                                    self.env.modules.insert(binding.name.clone(), module_def);
                                }
                            }
                            husk_ast::ExternItemKind::Struct { name, type_params } => {
                                // Register extern struct as a type
                                let def = StructDef {
                                    type_params: type_params.iter().map(|p| p.name.clone()).collect(),
                                    fields: HashMap::new(), // Extern structs are opaque
                                };
                                self.env.structs.insert(name.name.clone(), def);
                            }
                            husk_ast::ExternItemKind::Static { name, ty } => {
                                // Register extern static variable
                                self.env.statics.insert(name.name.clone(), ty.clone());
                            }
                        }
                    }
                }
                ItemKind::Use { .. } => {}
                ItemKind::Trait(trait_def) => {
                    let mut methods = HashMap::new();
                    for item in &trait_def.items {
                        // TraitItemKind has only Method variant (MVP scope)
                        let husk_ast::TraitItemKind::Method(method) = &item.kind;
                        methods.insert(
                            method.name.name.clone(),
                            MethodSig {
                                receiver: method.receiver,
                                params: method.params.clone(),
                                ret_type: method.ret_type.clone(),
                            },
                        );
                    }
                    let info = TraitInfo {
                        type_params: trait_def
                            .type_params
                            .iter()
                            .map(|p| p.name.name.clone())
                            .collect(),
                        supertraits: trait_def
                            .supertraits
                            .iter()
                            .map(type_expr_to_name)
                            .collect(),
                        methods,
                    };
                    self.env.traits.insert(trait_def.name.name.clone(), info);
                }
                ItemKind::Impl(impl_block) => {
                    let self_ty_name = type_expr_to_name(&impl_block.self_ty);
                    let trait_name = impl_block.trait_ref.as_ref().map(type_expr_to_name);

                    let mut methods = HashMap::new();
                    let mut properties = HashMap::new();
                    for item in &impl_block.items {
                        match &item.kind {
                            husk_ast::ImplItemKind::Method(method) => {
                                methods.insert(
                                    method.name.name.clone(),
                                    MethodInfo {
                                        receiver: method.receiver,
                                        params: method.params.clone(),
                                        ret_type: method.ret_type.clone(),
                                    },
                                );
                            }
                            husk_ast::ImplItemKind::Property(prop) => {
                                properties.insert(
                                    prop.name.name.clone(),
                                    PropertyInfo {
                                        ty: prop.ty.clone(),
                                        has_getter: prop.has_getter(),
                                        has_setter: prop.has_setter(),
                                    },
                                );
                            }
                        }
                    }
                    self.env.impls.push(ImplInfo {
                        trait_name,
                        self_ty_name,
                        methods,
                        properties,
                        span: impl_block.span.clone(),
                    });
                }
            }
        }
    }

    /// Verify that all trait implementations provide required methods and supertraits.
    fn verify_trait_impls(&mut self) {
        // Collect supertrait errors separately to avoid borrow issues
        let mut supertrait_errors: Vec<(String, String, Vec<String>, Span)> = Vec::new();

        for impl_info in &self.env.impls {
            // Only check trait impls (skip inherent impls)
            let Some(trait_name) = &impl_info.trait_name else {
                continue;
            };

            let Some(trait_info) = self.env.traits.get(trait_name) else {
                // Unknown trait - could report error, but may be external
                continue;
            };

            // Check all required trait methods are implemented
            for method_name in trait_info.methods.keys() {
                if !impl_info.methods.contains_key(method_name) {
                    self.errors.push(SemanticError {
                        message: format!(
                            "impl of trait `{}` for `{}` is missing method `{}`",
                            trait_name, impl_info.self_ty_name, method_name
                        ),
                        span: impl_info.span.clone(),
                    });
                }
            }

            // Check that all supertraits are also implemented
            // For example, `impl Eq for Foo` requires `impl PartialEq for Foo`
            let missing = self.env.missing_supertraits(&impl_info.self_ty_name, trait_name);
            if !missing.is_empty() {
                supertrait_errors.push((
                    trait_name.clone(),
                    impl_info.self_ty_name.clone(),
                    missing,
                    impl_info.span.clone(),
                ));
            }
        }

        // Report supertrait errors
        for (trait_name, type_name, missing, span) in supertrait_errors {
            let missing_list = missing.join("`, `");
            self.errors.push(SemanticError {
                message: format!(
                    "the trait bound `{}: {}` is not satisfied: missing implementation of supertrait{} `{}`",
                    type_name,
                    trait_name,
                    if missing.len() > 1 { "s" } else { "" },
                    missing_list
                ),
                span,
            });
        }
    }

    fn check_file(&mut self, file: &File) -> (Vec<SemanticError>, NameResolution) {
        // Verify trait implementations
        self.verify_trait_impls();

        // Type check each function body independently.
        for item in &file.items {
            if let ItemKind::Fn {
                name,
                type_params,
                params,
                ret_type,
                body,
                ..
            } = &item.kind
            {
                self.check_fn(name, type_params, params, ret_type.as_ref(), body, item.span.clone());
            }
        }
        (self.errors.clone(), std::mem::take(&mut self.name_resolution))
    }

    fn check_fn(
        &mut self,
        name: &Ident,
        type_params: &[husk_ast::TypeParam],
        params: &[Param],
        ret_type_expr: Option<&TypeExpr>,
        body: &[Stmt],
        span: Span,
    ) {
        // Convert type_params to Vec<String> for resolve_type_expr
        let generic_params: Vec<String> = type_params
            .iter()
            .map(|tp| tp.name.name.clone())
            .collect();

        let ret_ty = if let Some(ty_expr) = ret_type_expr {
            self.resolve_type_expr(ty_expr, &generic_params)
        } else {
            Type::Primitive(PrimitiveType::Unit)
        };

        let mut locals: HashMap<String, Type> = HashMap::new();
        let mut shadow_counts: HashMap<String, u32> = HashMap::new();
        let mut resolved_names: HashMap<String, String> = HashMap::new();

        // Parameters must have explicit types.
        for param in params {
            let ty = self.resolve_type_expr(&param.ty, &generic_params);
            if locals.insert(param.name.name.clone(), ty).is_some() {
                self.errors.push(SemanticError {
                    message: format!(
                        "duplicate parameter name `{}` in function `{}`",
                        param.name.name, name.name
                    ),
                    span: param.name.span.clone(),
                });
            }
            // Register parameter in name resolution (no shadowing for params)
            // Set shadow_counts to 1 because the parameter uses slot 0 (plain name).
            // Next shadowing will get name$1.
            let resolved = param.name.name.clone();
            shadow_counts.insert(param.name.name.clone(), 1);
            resolved_names.insert(param.name.name.clone(), resolved.clone());
            self.name_resolution.insert(
                (param.name.span.range.start, param.name.span.range.end),
                resolved,
            );
        }

        let mut ctx = FnContext {
            tcx: self,
            locals,
            shadow_counts,
            resolved_names,
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
            TypeExprKind::Function { params, ret } => {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|p| self.resolve_type_expr(p, generic_params))
                    .collect();
                let ret_type = self.resolve_type_expr(ret, generic_params);
                Type::Function {
                    params: param_types,
                    ret: Box::new(ret_type),
                }
            }
            TypeExprKind::Array(elem_ty) => {
                let elem = self.resolve_type_expr(elem_ty, generic_params);
                Type::Array(Box::new(elem))
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
            "f64" => Some(Type::Primitive(PrimitiveType::F64)),
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
    /// Maps variable names to their current shadow count.
    /// When a variable is shadowed, we increment its count.
    shadow_counts: HashMap<String, u32>,
    /// Maps variable names to their current resolved name (e.g., "x" -> "x$1").
    /// Used to resolve variable references to the correct shadowed version.
    resolved_names: HashMap<String, String>,
    ret_ty: Type,
    in_loop: bool,
}

impl<'a> FnContext<'a> {
    /// Bind a variable with shadowing support.
    ///
    /// Returns the resolved name (e.g., "x", "x$1", "x$2") and registers it
    /// in the name resolution map.
    fn bind_variable(&mut self, name: &str, ty: Type, span: &Span) -> String {
        // Get or initialize the shadow count for this variable name
        let count = self.shadow_counts.entry(name.to_string()).or_insert(0);
        let resolved = if *count == 0 {
            name.to_string()
        } else {
            format!("{}${}", name, count)
        };
        *count += 1;

        // Update the current resolved name for this variable
        self.resolved_names.insert(name.to_string(), resolved.clone());

        // Add to locals for type checking
        self.locals.insert(name.to_string(), ty);

        // Register in name resolution map
        self.tcx.name_resolution.insert(
            (span.range.start, span.range.end),
            resolved.clone(),
        );

        resolved
    }

    fn check_path_expr(&mut self, expr: &Expr, segments: &[Ident]) -> Type {
        // Handle `Enum::Variant` paths.
        if segments.len() >= 2 {
            let enum_name = &segments[0].name;
            let variant_name = &segments[segments.len() - 1].name;

            if let Some(def) = self.tcx.env.enums.get(enum_name).cloned() {
                let variant = def.variants.iter().find(|v| &v.name == variant_name);

                if variant.is_none() {
                    self.tcx.errors.push(SemanticError {
                        message: format!(
                            "unknown variant `{}` on enum `{}`",
                            variant_name, enum_name
                        ),
                        span: expr.span.clone(),
                    });
                }

                let enum_ty = Type::Named {
                    name: enum_name.clone(),
                    args: Vec::new(),
                };

                // Check variant fields to determine the type:
                // - Unit variants: return the enum type directly
                // - Tuple/Struct variants: return a constructor function type
                match variant.map(|v| &v.fields) {
                    Some(EnumVariantFields::Unit) | None => {
                        // Unit variant or unknown variant - return enum type directly
                        return enum_ty;
                    }
                    Some(EnumVariantFields::Tuple(field_types)) => {
                        // Tuple variant - return function type with field types as params
                        let params: Vec<Type> = field_types
                            .iter()
                            .map(|ty| self.tcx.resolve_type_expr(ty, &def.type_params))
                            .collect();
                        return Type::Function {
                            params,
                            ret: Box::new(enum_ty),
                        };
                    }
                    Some(EnumVariantFields::Struct(_fields)) => {
                        // Struct variants are constructed differently (not as function calls)
                        // For now, return the enum type
                        return enum_ty;
                    }
                }
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
            Type::Named { name, .. } => self.tcx.env.enums.get(name).map(|def| {
                let variant_names: Vec<String> =
                    def.variants.iter().map(|v| v.name.clone()).collect();
                (name.clone(), variant_names)
            }),
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
            let saved_shadow_counts = self.shadow_counts.clone();
            let saved_resolved_names = self.resolved_names.clone();
            let mut pattern_bindings = HashSet::new();
            self.bind_pattern_locals(&arm.pattern, &scrut_ty, &mut pattern_bindings);
            let arm_ty = self.check_expr(&arm.expr);
            self.locals = saved_locals;
            self.shadow_counts = saved_shadow_counts;
            self.resolved_names = saved_resolved_names;

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

    /// Bind pattern locals with shadowing support.
    ///
    /// The `pattern_bindings` set tracks bindings within the current pattern
    /// to detect duplicate bindings like `(x, x)` which should still be an error.
    /// Shadowing outer variables is allowed.
    fn bind_pattern_locals(
        &mut self,
        pat: &Pattern,
        scrut_ty: &Type,
        pattern_bindings: &mut HashSet<String>,
    ) {
        match &pat.kind {
            PatternKind::Wildcard => {}
            PatternKind::Binding(id) => {
                // Check for duplicate binding within the same pattern (e.g., `(x, x)`)
                if !pattern_bindings.insert(id.name.clone()) {
                    self.tcx.errors.push(SemanticError {
                        message: format!(
                            "identifier `{}` is bound more than once in the same pattern",
                            id.name
                        ),
                        span: id.span.clone(),
                    });
                }
                // Allow shadowing outer variables
                self.bind_variable(&id.name, scrut_ty.clone(), &id.span);
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
                // Use check_expr_with_expected to enable closure parameter inference
                // from type annotation: `let f: fn(i32) -> i32 = |x| x + 1`
                let value_ty = value
                    .as_ref()
                    .map(|expr| self.check_expr_with_expected(expr, annotated_ty.as_ref()));

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

                // Bind the variable with shadowing support
                self.bind_variable(&name.name, final_ty, &name.span);
            }
            StmtKind::Assign { target, op, value } => {
                // Validate target is assignable (lvalue: ident, field, or index)
                if !self.is_lvalue(target) {
                    self.tcx.errors.push(SemanticError {
                        message: "invalid assignment target".to_string(),
                        span: target.span.clone(),
                    });
                    return;
                }

                let target_ty = self.check_expr(target);
                let value_ty = self.check_expr(value);

                match op {
                    husk_ast::AssignOp::Assign => {
                        if !self.types_compatible(&target_ty, &value_ty) {
                            self.tcx.errors.push(SemanticError {
                                message: format!(
                                    "type mismatch: cannot assign `{:?}` to `{:?}`",
                                    value_ty, target_ty
                                ),
                                span: value.span.clone(),
                            });
                        }
                    }
                    husk_ast::AssignOp::AddAssign
                    | husk_ast::AssignOp::SubAssign
                    | husk_ast::AssignOp::ModAssign => {
                        // Compound assignment requires numeric types
                        if !self.is_numeric(&target_ty) {
                            self.tcx.errors.push(SemanticError {
                                message: format!(
                                    "compound assignment requires numeric type, found `{:?}`",
                                    target_ty
                                ),
                                span: target.span.clone(),
                            });
                        }
                        if !self.types_compatible(&target_ty, &value_ty) {
                            self.tcx.errors.push(SemanticError {
                                message: format!(
                                    "type mismatch in compound assignment: `{:?}` vs `{:?}`",
                                    target_ty, value_ty
                                ),
                                span: value.span.clone(),
                            });
                        }
                    }
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
            StmtKind::Loop { body } => {
                let prev_in_loop = self.in_loop;
                self.in_loop = true;
                self.check_block(body);
                self.in_loop = prev_in_loop;
            }
            StmtKind::ForIn {
                binding,
                iterable,
                body,
            } => {
                let iter_ty = self.check_expr(iterable);

                // Extract element type from iterable ([T], Vec<T>, Range<T>, String)
                let elem_ty = match &iter_ty {
                    Type::Array(elem) => (**elem).clone(),
                    Type::Named { name, args } if name == "Vec" && !args.is_empty() => {
                        args[0].clone()
                    }
                    Type::Named { name, args } if name == "Range" && !args.is_empty() => {
                        args[0].clone() // Range<i32> yields i32
                    }
                    // Also allow String iteration (iterates over chars as strings)
                    Type::Primitive(PrimitiveType::String) => {
                        Type::Primitive(PrimitiveType::String)
                    }
                    _ => {
                        self.tcx.errors.push(SemanticError {
                            message: format!(
                                "for-in loop requires iterable collection, found `{:?}`",
                                iter_ty
                            ),
                            span: iterable.span.clone(),
                        });
                        Type::unit()
                    }
                };

                // Save current scope state
                let old_locals = self.locals.clone();
                let old_shadow_counts = self.shadow_counts.clone();
                let old_resolved_names = self.resolved_names.clone();

                // Bind loop variable with shadowing support
                self.bind_variable(&binding.name, elem_ty, &binding.span);

                // Check body in loop context
                let prev_in_loop = self.in_loop;
                self.in_loop = true;
                self.check_block(body);
                self.in_loop = prev_in_loop;

                // Restore scope state
                self.locals = old_locals;
                self.shadow_counts = old_shadow_counts;
                self.resolved_names = old_resolved_names;
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
        let old_shadow_counts = self.shadow_counts.clone();
        let old_resolved_names = self.resolved_names.clone();
        for stmt in &block.stmts {
            self.check_stmt(stmt);
        }
        self.locals = old_locals;
        self.shadow_counts = old_shadow_counts;
        self.resolved_names = old_resolved_names;
    }

    fn check_expr(&mut self, expr: &Expr) -> Type {
        match &expr.kind {
            ExprKind::Literal(lit) => match lit.kind {
                LiteralKind::Int(_) => Type::Primitive(PrimitiveType::I32),
                LiteralKind::Float(_) => Type::Primitive(PrimitiveType::F64),
                LiteralKind::Bool(_) => Type::Primitive(PrimitiveType::Bool),
                LiteralKind::String(_) => Type::Primitive(PrimitiveType::String),
            },
            ExprKind::Path { segments } => self.check_path_expr(expr, segments),
            ExprKind::Ident(id) => {
                if let Some(ty) = self.locals.get(&id.name) {
                    // Register the resolved name for this variable usage
                    if let Some(resolved) = self.resolved_names.get(&id.name) {
                        self.tcx.name_resolution.insert(
                            (id.span.range.start, id.span.range.end),
                            resolved.clone(),
                        );
                    }
                    return ty.clone();
                }
                // Try top-level function.
                if let Some(fn_def) = self.tcx.env.functions.get(&id.name).cloned() {
                    // Pass the function's type params so generic types like T are recognized
                    let type_param_names = fn_def.type_param_names();
                    let param_types: Vec<Type> = fn_def
                        .params
                        .iter()
                        .map(|p| self.tcx.resolve_type_expr(&p.ty, &type_param_names))
                        .collect();
                    let ret_ty = if let Some(ret_expr) = fn_def.ret_type.as_ref() {
                        self.tcx.resolve_type_expr(ret_expr, &type_param_names)
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
                if let Some(module_def) = self.tcx.env.modules.get(&id.name) {
                    // Use the stored return type if available, otherwise use the module name
                    let ret_type_name = module_def.ret_type.clone()
                        .unwrap_or_else(|| id.name.clone());
                    return Type::Function {
                        params: Vec::new(),
                        ret: Box::new(Type::Named {
                            name: ret_type_name,
                            args: Vec::new(),
                        }),
                    };
                }

                // Try extern static variable (from `static name: Type;` in extern block).
                if let Some(ty_expr) = self.tcx.env.statics.get(&id.name).cloned() {
                    return self.tcx.resolve_type_expr(&ty_expr, &[]);
                }

                // Built-in functions
                match id.name.as_str() {
                    "parse_int" => {
                        // parse_int(s: String, radix: i32) -> i32
                        return Type::Function {
                            params: vec![
                                Type::Primitive(PrimitiveType::String),
                                Type::Primitive(PrimitiveType::I32),
                            ],
                            ret: Box::new(Type::Primitive(PrimitiveType::I32)),
                        };
                    }
                    "assert" => {
                        // assert(condition: bool) -> ()
                        return Type::Function {
                            params: vec![Type::Primitive(PrimitiveType::Bool)],
                            ret: Box::new(Type::Primitive(PrimitiveType::Unit)),
                        };
                    }
                    _ => {}
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

                // Get the function name for looking up generic type parameters
                let fn_name = match &callee.kind {
                    ExprKind::Ident(id) => Some(id.name.clone()),
                    _ => None,
                };

                // Get the function definition for generic type inference
                let fn_def = fn_name.as_ref().and_then(|name| {
                    self.tcx.env.functions.get(name).cloned()
                });

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

                // Collect type substitutions for generic functions
                let mut substitutions = std::collections::HashMap::new();
                let type_param_names: Vec<String> = fn_def.as_ref()
                    .map(|d| d.type_param_names())
                    .unwrap_or_default();

                // Type-check arguments with expected types for closure inference
                // (skip for module calls since we don't know the signature)
                if !is_module_call {
                    for (i, arg) in args.iter().enumerate() {
                        let expected = param_tys.get(i);
                        // Use check_expr_with_expected to enable closure parameter inference
                        let arg_ty = self.check_expr_with_expected(arg, expected);

                        // Collect substitutions for generic type parameters
                        if let Some(param_ty) = expected {
                            self.unify_types(param_ty, &arg_ty, &type_param_names, &mut substitutions);
                        }

                        // For generic functions, we skip strict type checking since types
                        // like T should match any concrete type
                        if let Some(expected) = expected {
                            // Only check compatibility for non-generic parameter types
                            let is_generic_param = match expected {
                                Type::Named { name, args } if args.is_empty() => {
                                    type_param_names.contains(name)
                                }
                                _ => false,
                            };
                            if !is_generic_param && !self.types_compatible(expected, &arg_ty) {
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

                // Verify trait bounds for generic type parameters
                if let Some(ref fn_def) = fn_def {
                    for type_param in &fn_def.type_params {
                        if !type_param.bounds.is_empty() {
                            // Get the substituted type for this type parameter
                            if let Some(concrete_type) = substitutions.get(&type_param.name) {
                                // Extract the type name for trait lookup
                                let concrete_type_name = self.type_to_name(concrete_type);

                                // Check each trait bound
                                for bound in &type_param.bounds {
                                    if !self.tcx.env.type_implements_trait(&concrete_type_name, bound) {
                                        self.tcx.errors.push(SemanticError {
                                            message: format!(
                                                "the trait bound `{}: {}` is not satisfied",
                                                concrete_type_name, bound
                                            ),
                                            span: expr.span.clone(),
                                        });
                                    }
                                }
                            }
                        }
                    }
                }

                // Special handling for assert_eq and assert_ne: enforce PartialEq on arguments
                // These are extern "js" functions that accept JsValue, but semantically we want
                // to require PartialEq on the types being compared.
                if let Some(ref name) = fn_name {
                    if name == "assert_eq" || name == "assert_ne" {
                        // Check first two arguments for PartialEq
                        for (i, arg) in args.iter().take(2).enumerate() {
                            let arg_ty = self.check_expr(arg);
                            let arg_type_name = self.type_to_name(&arg_ty);

                            // Skip if type is JsValue or an extern type (already opaque)
                            if arg_type_name != "JsValue" && !arg_type_name.starts_with('?') {
                                if !self.tcx.env.type_implements_trait(&arg_type_name, "PartialEq") {
                                    self.tcx.errors.push(SemanticError {
                                        message: format!(
                                            "the trait bound `{}: PartialEq` is not satisfied",
                                            arg_type_name
                                        ),
                                        span: args.get(i).map(|a| a.span.clone()).unwrap_or_else(|| expr.span.clone()),
                                    });
                                }
                            }
                        }
                    }
                }

                // Apply substitutions to the return type for generic functions
                if !type_param_names.is_empty() {
                    self.apply_substitutions(&ret_ty, &substitutions)
                } else {
                    ret_ty
                }
            }
            ExprKind::Field { base, member } => {
                let base_ty = self.check_expr(base);

                // Handle .length on arrays and strings
                if member.name == "length" {
                    match &base_ty {
                        Type::Array(_) | Type::Primitive(PrimitiveType::String) => {
                            return Type::Primitive(PrimitiveType::I32);
                        }
                        _ => {}
                    }
                }

                if let Type::Named { name, args } = base_ty {
                    // First, check regular struct fields
                    if let Some(def) = self.tcx.env.structs.get(&name).cloned() {
                        if let Some(field_ty_expr) = def.fields.get(&member.name) {
                            // For now, ignore generic substitution and just resolve as-is.
                            let _ = args;
                            return self.tcx.resolve_type_expr(field_ty_expr, &def.type_params);
                        }
                    }

                    // Then check extern properties from impl blocks
                    let prop_ty = self.tcx.env.impls.iter()
                        .find(|info| info.self_ty_name == name)
                        .and_then(|info| info.properties.get(&member.name))
                        .map(|prop| prop.ty.clone());
                    if let Some(ty) = prop_ty {
                        return self.tcx.resolve_type_expr(&ty, &[]);
                    }

                    // Not found in either
                    if self.tcx.env.structs.contains_key(&name) {
                        self.tcx.errors.push(SemanticError {
                            message: format!(
                                "no field named `{}` on struct `{}`",
                                member.name, name
                            ),
                            span: member.span.clone(),
                        });
                    } else {
                        // Extern struct - try to be permissive for JS FFI
                        // Return Unit as fallback (JS property access is dynamic)
                        return Type::Primitive(PrimitiveType::Unit);
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
                method,
                args,
            } => {
                let method_name = &method.name;

                // Check if receiver is an extern module (e.g., Array.from, Math.ceil)
                if let ExprKind::Ident(ref id) = receiver.kind {
                    // Clone the return type to avoid borrow conflicts
                    let module_fn_ret_type: Option<Option<TypeExpr>> = self
                        .tcx
                        .env
                        .modules
                        .get(&id.name)
                        .and_then(|m| m.functions.get(method_name))
                        .map(|fn_info| fn_info.ret_type.clone());

                    if let Some(ret_type_opt) = module_fn_ret_type {
                        // Type-check arguments
                        for arg in args {
                            let _ = self.check_expr(arg);
                        }
                        // Return the function's return type
                        if let Some(ret_ty) = ret_type_opt {
                            return self.tcx.resolve_type_expr(&ret_ty, &[]);
                        }
                        return Type::Primitive(PrimitiveType::Unit);
                    }
                }

                // Type-check receiver and arguments
                let receiver_ty = self.check_expr(receiver);
                for arg in args {
                    let _ = self.check_expr(arg);
                }

                // Handle built-in methods on primitive types
                match &receiver_ty {
                    Type::Primitive(PrimitiveType::String) => {
                        match method_name.as_str() {
                            "split" => return Type::Array(Box::new(Type::Primitive(PrimitiveType::String))),
                            "trim" => return Type::Primitive(PrimitiveType::String),
                            "len" => return Type::Primitive(PrimitiveType::I32),
                            "char_at" => return Type::Primitive(PrimitiveType::String),
                            "slice" => return Type::Primitive(PrimitiveType::String),
                            "substring" => return Type::Primitive(PrimitiveType::String),
                            _ => {}
                        }
                    }
                    Type::Array(elem_ty) => {
                        match method_name.as_str() {
                            "len" => return Type::Primitive(PrimitiveType::I32),
                            "push" => return Type::Primitive(PrimitiveType::Unit),
                            "slice" => return receiver_ty.clone(),
                            "some" | "every" | "filter" => return Type::Primitive(PrimitiveType::Bool),
                            "map" => return receiver_ty.clone(), // simplified - should infer from closure
                            "reduce" => return (**elem_ty).clone(),
                            _ => {}
                        }
                    }
                    _ => {}
                }

                // Handle Result/Option unwrap methods specially to extract inner type
                if let Type::Named { name, args } = &receiver_ty {
                    match (name.as_str(), method_name.as_str()) {
                        ("Result", "unwrap") | ("Result", "expect") => {
                            // Result<T, E>.unwrap() returns T
                            if let Some(ok_type) = args.first() {
                                return ok_type.clone();
                            }
                        }
                        ("Option", "unwrap") | ("Option", "expect") => {
                            // Option<T>.unwrap() returns T
                            if let Some(inner_type) = args.first() {
                                return inner_type.clone();
                            }
                        }
                        _ => {}
                    }
                }

                // Try to resolve the method's return type from impl blocks
                let receiver_type_name = match &receiver_ty {
                    Type::Named { name, .. } => Some(name.clone()),
                    _ => None,
                };

                // Look up the method in impl blocks and get its return type.
                // Use Option<Option<TypeExpr>> to distinguish "method not found" from "method found with no return type"
                let method_lookup_result: Option<Option<TypeExpr>> = if let Some(ref type_name) = receiver_type_name
                {
                    let mut found = None;
                    for impl_info in &self.tcx.env.impls {
                        if impl_info.self_ty_name == *type_name {
                            if let Some(method_info) = impl_info.methods.get(method_name) {
                                // Found the method - wrap ret_type in Some to indicate success
                                found = Some(method_info.ret_type.clone());
                                break;
                            }
                        }
                    }
                    found
                } else {
                    None
                };

                // Resolve the return type
                match method_lookup_result {
                    Some(Some(ret_type_expr)) => {
                        // Method found with explicit return type
                        self.tcx.resolve_type_expr(&ret_type_expr, &[])
                    }
                    Some(None) => {
                        // Method found but returns unit (no explicit return type)
                        Type::Primitive(PrimitiveType::Unit)
                    }
                    None => {
                        // Method not found - return Unit as fallback
                        Type::Primitive(PrimitiveType::Unit)
                    }
                }
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
                                message:
                                    "`+` requires operands of the same type (`i32` or `String`)"
                                        .to_string(),
                                span: expr.span.clone(),
                            });
                            Type::Primitive(PrimitiveType::I32)
                        }
                    }
                    Sub | Mul | Div | Mod => {
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
                                message: format!(
                                    "cannot compare `{}` with `{}`",
                                    self.format_type(&left_ty),
                                    self.format_type(&right_ty)
                                ),
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
            ExprKind::FormatPrint { format, args, newline: _ } => {
                // Count placeholders (excluding escaped braces which are literals)
                let mut placeholders: Vec<&husk_ast::FormatPlaceholder> = Vec::new();
                for segment in &format.segments {
                    if let FormatSegment::Placeholder(ph) = segment {
                        placeholders.push(ph);
                    }
                }

                // Check for mixing explicit numeric positions (like {0}) with implicit positions ({}).
                // Named placeholders like {x} are allowed to mix with implicit {} since the parser
                // synthesizes arguments for them separately.
                let has_explicit_numeric_position = placeholders
                    .iter()
                    .any(|ph| ph.position.is_some() && ph.name.is_none());
                let has_implicit_position = placeholders
                    .iter()
                    .any(|ph| ph.position.is_none() && ph.name.is_none());

                if has_explicit_numeric_position && has_implicit_position {
                    self.tcx.errors.push(SemanticError {
                        message: "cannot mix positional and implicit argument indexing".to_string(),
                        span: format.span.clone(),
                    });
                }

                // Validate argument count
                // Named placeholders have synthesized arguments with explicit positions
                let has_explicit_position = placeholders.iter().any(|ph| ph.position.is_some());
                if has_explicit_position {
                    // With explicit positions, check that all indices are in bounds
                    for ph in &placeholders {
                        if let Some(pos) = ph.position {
                            if pos >= args.len() {
                                self.tcx.errors.push(SemanticError {
                                    message: format!(
                                        "positional argument {} is out of range (only {} argument(s) provided)",
                                        pos,
                                        args.len()
                                    ),
                                    span: ph.span.clone(),
                                });
                            }
                        }
                    }
                } else {
                    // With implicit positions, count must match exactly
                    let placeholder_count = placeholders.len();
                    if placeholder_count != args.len() {
                        self.tcx.errors.push(SemanticError {
                            message: format!(
                                "format string requires {} argument(s), but {} provided",
                                placeholder_count,
                                args.len()
                            ),
                            span: expr.span.clone(),
                        });
                    }
                }

                // Type-check all format arguments
                let arg_types: Vec<Type> = args.iter().map(|arg| self.check_expr(arg)).collect();

                // Validate type compatibility for numeric format specifiers
                for (i, ph) in placeholders.iter().enumerate() {
                    let arg_index = ph.position.unwrap_or(i);
                    if let Some(arg_ty) = arg_types.get(arg_index) {
                        // Numeric format specifiers require i32
                        if let Some(ty_char) = ph.spec.ty {
                            match ty_char {
                                'x' | 'X' | 'b' | 'o' => {
                                    if !matches!(arg_ty, Type::Primitive(PrimitiveType::I32)) {
                                        self.tcx.errors.push(SemanticError {
                                            message: format!(
                                                "format specifier `:{ty_char}` requires numeric type, found `{arg_ty:?}`"
                                            ),
                                            span: ph.span.clone(),
                                        });
                                    }
                                }
                                '?' => {
                                    // Debug format works with any type
                                }
                                _ => {}
                            }
                        }
                    }
                }

                // println returns unit
                Type::Primitive(PrimitiveType::Unit)
            }
            ExprKind::Format { format, args } => {
                // Same validation as FormatPrint
                let mut placeholders: Vec<&husk_ast::FormatPlaceholder> = Vec::new();
                for segment in &format.segments {
                    if let FormatSegment::Placeholder(ph) = segment {
                        placeholders.push(ph);
                    }
                }

                // Check for mixing explicit numeric positions (like {0}) with implicit positions ({}).
                // Named placeholders like {x} are allowed to mix with implicit {} since the parser
                // synthesizes arguments for them separately.
                let has_explicit_numeric_position = placeholders
                    .iter()
                    .any(|ph| ph.position.is_some() && ph.name.is_none());
                let has_implicit_position = placeholders
                    .iter()
                    .any(|ph| ph.position.is_none() && ph.name.is_none());

                if has_explicit_numeric_position && has_implicit_position {
                    self.tcx.errors.push(SemanticError {
                        message: "cannot mix positional and implicit argument indexing".to_string(),
                        span: format.span.clone(),
                    });
                }

                // Named placeholders have synthesized arguments with explicit positions
                let has_explicit_position = placeholders.iter().any(|ph| ph.position.is_some());
                if has_explicit_position {
                    for ph in &placeholders {
                        if let Some(pos) = ph.position {
                            if pos >= args.len() {
                                self.tcx.errors.push(SemanticError {
                                    message: format!(
                                        "positional argument {} is out of range (only {} argument(s) provided)",
                                        pos,
                                        args.len()
                                    ),
                                    span: ph.span.clone(),
                                });
                            }
                        }
                    }
                } else {
                    let placeholder_count = placeholders.len();
                    if placeholder_count != args.len() {
                        self.tcx.errors.push(SemanticError {
                            message: format!(
                                "format string requires {} argument(s), but {} provided",
                                placeholder_count,
                                args.len()
                            ),
                            span: expr.span.clone(),
                        });
                    }
                }

                let arg_types: Vec<Type> = args.iter().map(|arg| self.check_expr(arg)).collect();

                for (i, ph) in placeholders.iter().enumerate() {
                    let arg_index = ph.position.unwrap_or(i);
                    if let Some(arg_ty) = arg_types.get(arg_index) {
                        if let Some(ty_char) = ph.spec.ty {
                            match ty_char {
                                'x' | 'X' | 'b' | 'o' => {
                                    if !matches!(arg_ty, Type::Primitive(PrimitiveType::I32)) {
                                        self.tcx.errors.push(SemanticError {
                                            message: format!(
                                                "format specifier `:{ty_char}` requires numeric type, found `{arg_ty:?}`"
                                            ),
                                            span: ph.span.clone(),
                                        });
                                    }
                                }
                                '?' => {}
                                _ => {}
                            }
                        }
                    }
                }

                // format returns String
                Type::Primitive(PrimitiveType::String)
            }
            ExprKind::Closure {
                params,
                ret_type,
                body,
            } => self.check_closure_expr(expr, params, ret_type.as_ref(), body, None),
            ExprKind::Array { elements } => {
                if elements.is_empty() {
                    // Empty array - for now, allow it with unit element type
                    // A more complete implementation would require type annotation
                    Type::Array(Box::new(Type::unit()))
                } else {
                    // Infer element type from first element
                    let first_ty = self.check_expr(&elements[0]);

                    // Check all elements have compatible types
                    for (i, elem) in elements.iter().enumerate().skip(1) {
                        let elem_ty = self.check_expr(elem);
                        // For now, just check they match (could be smarter about unions/coercion)
                        if elem_ty != first_ty {
                            self.tcx.errors.push(SemanticError {
                                message: format!(
                                    "array element {} has type `{:?}`, expected `{:?}`",
                                    i, elem_ty, first_ty
                                ),
                                span: elem.span.clone(),
                            });
                        }
                    }

                    Type::Array(Box::new(first_ty))
                }
            }
            ExprKind::Index { base, index } => {
                let base_ty = self.check_expr(base);

                // Check if this is a slice operation (index is a Range) or simple indexing
                let is_slice = matches!(index.kind, ExprKind::Range { .. });

                if is_slice {
                    // Slice operation: arr[start..end], arr[..], etc.
                    // Check the range expression (will validate start/end are i32)
                    let _range_ty = self.check_expr(index);

                    // Slicing an array returns an array of the same element type
                    match base_ty {
                        Type::Array(_) => base_ty,
                        Type::Named { ref name, ref args } if name == "Vec" && !args.is_empty() => {
                            // Vec slice returns Vec
                            base_ty
                        }
                        Type::Named { .. } => Type::Named {
                            name: "JsValue".to_string(),
                            args: vec![],
                        },
                        _ => {
                            self.tcx.errors.push(SemanticError {
                                message: format!("cannot slice type {:?}", base_ty),
                                span: base.span.clone(),
                            });
                            Type::unit()
                        }
                    }
                } else {
                    // Simple index operation: arr[i]
                    let index_ty = self.check_expr(index);

                    // Verify index is integer
                    let is_valid_index = matches!(index_ty, Type::Primitive(PrimitiveType::I32))
                        || matches!(&index_ty, Type::Named { name, .. } if name == "number");
                    if !is_valid_index {
                        self.tcx.errors.push(SemanticError {
                            message: format!("array index must be integer, found {:?}", index_ty),
                            span: index.span.clone(),
                        });
                    }

                    // Extract element type from [T]
                    match base_ty {
                        Type::Array(elem_ty) => (*elem_ty).clone(),
                        // Also accept Vec<T> for backwards compat
                        Type::Named { ref name, ref args } if name == "Vec" && !args.is_empty() => {
                            args[0].clone()
                        }
                        // Allow indexing any type that we don't know (e.g. extern types, JsValue)
                        Type::Named { .. } => Type::Named {
                            name: "JsValue".to_string(),
                            args: vec![],
                        },
                        _ => {
                            self.tcx.errors.push(SemanticError {
                                message: format!("cannot index into type {:?}", base_ty),
                                span: base.span.clone(),
                            });
                            Type::unit()
                        }
                    }
                }
            }
            ExprKind::Range {
                start,
                end,
                inclusive: _,
            } => {
                // Verify start is integer if present
                if let Some(start_expr) = start {
                    let start_ty = self.check_expr(start_expr);
                    if !matches!(start_ty, Type::Primitive(PrimitiveType::I32)) {
                        self.tcx.errors.push(SemanticError {
                            message: format!("range start must be i32, found {:?}", start_ty),
                            span: start_expr.span.clone(),
                        });
                    }
                }

                // Verify end is integer if present
                if let Some(end_expr) = end {
                    let end_ty = self.check_expr(end_expr);
                    if !matches!(end_ty, Type::Primitive(PrimitiveType::I32)) {
                        self.tcx.errors.push(SemanticError {
                            message: format!("range end must be i32, found {:?}", end_ty),
                            span: end_expr.span.clone(),
                        });
                    }
                }

                // Range type - acts like an iterable of i32
                Type::Named {
                    name: "Range".to_string(),
                    args: vec![Type::Primitive(PrimitiveType::I32)],
                }
            }
            ExprKind::Assign { target, op: _, value } => {
                // Type check both sides
                let _target_ty = self.check_expr(target);
                let value_ty = self.check_expr(value);

                // Assignment expression returns the assigned value
                value_ty
            }
            ExprKind::JsLiteral { .. } => {
                // Raw JavaScript literals are treated as dynamically typed (JsValue)
                // They can evaluate to any JavaScript value at runtime.
                Type::Named {
                    name: "JsValue".to_string(),
                    args: Vec::new(),
                }
            }
        }
    }

    /// Check expression with optional expected type for bidirectional inference.
    /// This enables closure parameter type inference from call-site context.
    fn check_expr_with_expected(&mut self, expr: &Expr, expected: Option<&Type>) -> Type {
        match &expr.kind {
            ExprKind::Closure {
                params,
                ret_type,
                body,
            } => self.check_closure_expr(expr, params, ret_type.as_ref(), body, expected),
            _ => self.check_expr(expr),
        }
    }

    /// Check a closure expression and return its function type.
    /// If `expected` is provided and is a function type, use it to infer parameter types.
    fn check_closure_expr(
        &mut self,
        _expr: &Expr,
        params: &[ClosureParam],
        ret_type: Option<&TypeExpr>,
        body: &Expr,
        expected: Option<&Type>,
    ) -> Type {
        // Extract expected parameter types if available
        let expected_params = match expected {
            Some(Type::Function { params, .. }) => Some(params.as_slice()),
            _ => None,
        };

        // Extract expected return type if available
        let expected_ret = match expected {
            Some(Type::Function { ret, .. }) => Some(ret.as_ref()),
            _ => None,
        };

        // Resolve parameter types
        let mut param_types = Vec::new();
        let mut closure_locals = self.locals.clone();
        let mut closure_shadow_counts = self.shadow_counts.clone();
        let mut closure_resolved_names = self.resolved_names.clone();

        for (i, param) in params.iter().enumerate() {
            let ty = if let Some(type_expr) = &param.ty {
                // Explicit annotation provided - use it
                let annotated = self.tcx.resolve_type_expr(type_expr, &[]);

                // Validate against expected type if available
                if let Some(expected_params) = expected_params {
                    if let Some(expected_ty) = expected_params.get(i) {
                        if !self.types_compatible(expected_ty, &annotated) {
                            self.tcx.errors.push(SemanticError {
                                message: format!(
                                    "closure parameter type `{:?}` does not match expected `{:?}`",
                                    annotated, expected_ty
                                ),
                                span: param.name.span.clone(),
                            });
                        }
                    }
                }
                annotated
            } else if let Some(expected_params) = expected_params {
                // No annotation - infer from expected type
                if let Some(expected_ty) = expected_params.get(i) {
                    expected_ty.clone()
                } else {
                    // More params than expected - error
                    self.tcx.errors.push(SemanticError {
                        message: format!(
                            "closure has more parameters than expected (expected {}, got {})",
                            expected_params.len(),
                            params.len()
                        ),
                        span: param.name.span.clone(),
                    });
                    Type::Primitive(PrimitiveType::Unit)
                }
            } else {
                // No context available - require annotation
                self.tcx.errors.push(SemanticError {
                    message: format!(
                        "cannot infer type for closure parameter `{}`. \
                         Add type annotation: `|{}: Type|`",
                        param.name.name, param.name.name
                    ),
                    span: param.name.span.clone(),
                });
                Type::Primitive(PrimitiveType::Unit)
            };

            param_types.push(ty.clone());
            closure_locals.insert(param.name.name.clone(), ty);

            // Register closure parameter in name resolution (may shadow outer variables)
            let count = closure_shadow_counts.entry(param.name.name.clone()).or_insert(0);
            let resolved = if *count == 0 {
                param.name.name.clone()
            } else {
                format!("{}${}", param.name.name, count)
            };
            *count += 1;
            closure_resolved_names.insert(param.name.name.clone(), resolved.clone());
            self.tcx.name_resolution.insert(
                (param.name.span.range.start, param.name.span.range.end),
                resolved,
            );
        }

        // Check arity mismatch (fewer params than expected)
        if let Some(expected_params) = expected_params {
            if params.len() < expected_params.len() {
                self.tcx.errors.push(SemanticError {
                    message: format!(
                        "closure has fewer parameters than expected (expected {}, got {})",
                        expected_params.len(),
                        params.len()
                    ),
                    span: _expr.span.clone(),
                });
            }
        }

        // Resolve return type if specified
        let expected_ret_ty = if let Some(ret_expr) = ret_type {
            self.tcx.resolve_type_expr(ret_expr, &[])
        } else if let Some(expected_ret) = expected_ret {
            // Use expected return type for body checking
            expected_ret.clone()
        } else {
            // Will be inferred from body
            Type::Primitive(PrimitiveType::Unit)
        };

        // Create a nested context for the closure body
        let old_locals = std::mem::replace(&mut self.locals, closure_locals);
        let old_shadow_counts = std::mem::replace(&mut self.shadow_counts, closure_shadow_counts);
        let old_resolved_names = std::mem::replace(&mut self.resolved_names, closure_resolved_names);
        let old_ret_ty = std::mem::replace(&mut self.ret_ty, expected_ret_ty.clone());

        // Check the body and infer return type
        let body_ty = self.check_expr(body);

        // Restore the outer context
        self.locals = old_locals;
        self.shadow_counts = old_shadow_counts;
        self.resolved_names = old_resolved_names;
        self.ret_ty = old_ret_ty;

        // Use explicit return type if specified, otherwise infer from body
        let actual_ret_ty = if ret_type.is_some() {
            expected_ret_ty
        } else {
            body_ty
        };

        Type::Function {
            params: param_types,
            ret: Box::new(actual_ret_ty),
        }
    }

    fn format_type(&self, ty: &Type) -> String {
        match ty {
            Type::Primitive(p) => match p {
                PrimitiveType::I32 => "i32".to_string(),
                PrimitiveType::F64 => "f64".to_string(),
                PrimitiveType::Bool => "bool".to_string(),
                PrimitiveType::String => "String".to_string(),
                PrimitiveType::Unit => "()".to_string(),
            },
            Type::Array(inner) => format!("[{}]", self.format_type(inner)),
            Type::Named { name, args } if args.is_empty() => name.clone(),
            Type::Named { name, args } => {
                let args_str = args.iter()
                    .map(|a| self.format_type(a))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", name, args_str)
            }
            Type::Function { params, ret } => {
                let params_str = params.iter()
                    .map(|p| self.format_type(p))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn({}) -> {}", params_str, self.format_type(ret))
            }
            Type::Var(id) => format!("?{}", id.0),
        }
    }

    /// Convert a Type to a string name for trait lookup.
    fn type_to_name(&self, ty: &Type) -> String {
        match ty {
            Type::Named { name, args } => {
                if args.is_empty() {
                    name.clone()
                } else {
                    let arg_strs: Vec<String> = args.iter().map(|a| self.type_to_name(a)).collect();
                    format!("{}<{}>", name, arg_strs.join(", "))
                }
            }
            Type::Primitive(prim) => match prim {
                PrimitiveType::I32 => "i32".to_string(),
                PrimitiveType::F64 => "f64".to_string(),
                PrimitiveType::Bool => "bool".to_string(),
                PrimitiveType::String => "String".to_string(),
                PrimitiveType::Unit => "()".to_string(),
            },
            Type::Array(elem) => format!("[{}]", self.type_to_name(elem)),
            Type::Function { params, ret } => {
                let param_strs: Vec<String> = params.iter().map(|p| self.type_to_name(p)).collect();
                format!("fn({}) -> {}", param_strs.join(", "), self.type_to_name(ret))
            }
            Type::Var(id) => format!("?{}", id.0),
        }
    }

    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        self.types_compatible_inner(expected, actual)
    }

    fn types_compatible_inner(&self, expected: &Type, actual: &Type) -> bool {
        // JsValue is compatible with any type (it's JavaScript's dynamic "any" type)
        // This allows passing primitives to functions expecting JsValue (e.g., assert_eq)
        if let Type::Named { name, args } = expected {
            if name == "JsValue" && args.is_empty() {
                return true;
            }
        }

        // Empty array [()] is compatible with any array type
        // This allows `[] == [1, 2, 3]` without explicit type annotation
        if let (Type::Array(expected_elem), Type::Array(actual_elem)) = (expected, actual) {
            if matches!(actual_elem.as_ref(), Type::Primitive(PrimitiveType::Unit)) {
                return true;
            }
            if matches!(expected_elem.as_ref(), Type::Primitive(PrimitiveType::Unit)) {
                return true;
            }
        }

        // Generic type parameters (like T, U) are compatible with any type
        // This enables type inference to work with explicit closure annotations
        if let Type::Named { name, args } = expected {
            if args.is_empty() {
                // Check if this is a generic type parameter (single uppercase letter
                // or a name that's not a known type)
                let is_generic = name.len() == 1 && name.chars().next().unwrap().is_uppercase()
                    || (!self.tcx.env.structs.contains_key(name)
                        && !self.tcx.env.enums.contains_key(name)
                        && !matches!(name.as_str(), "i32" | "bool" | "String" | "Unit"));
                if is_generic {
                    return true;
                }
            }
        }

        // Handle function types: compare structurally with generic-aware compatibility
        if let (
            Type::Function { params: expected_params, ret: expected_ret },
            Type::Function { params: actual_params, ret: actual_ret }
        ) = (expected, actual) {
            if expected_params.len() != actual_params.len() {
                return false;
            }
            // Check each parameter is compatible
            for (exp, act) in expected_params.iter().zip(actual_params.iter()) {
                if !self.types_compatible_inner(exp, act) {
                    return false;
                }
            }
            // Check return type is compatible
            return self.types_compatible_inner(expected_ret, actual_ret);
        }

        // Handle Named types: if both are the same enum/struct name, treat as compatible
        // even if type args differ (MVP approach for generic enums like Result/Option)
        if let (
            Type::Named { name: expected_name, .. },
            Type::Named { name: actual_name, args: actual_args }
        ) = (expected, actual) {
            if expected_name == actual_name {
                // If actual has no type args (from enum constructor), allow it
                if actual_args.is_empty() {
                    return true;
                }
            }
        }

        expected == actual
    }

    /// Check if an expression is a valid assignment target (lvalue).
    fn is_lvalue(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Ident(_) => true,
            ExprKind::Field { .. } => true,
            ExprKind::Index { .. } => true,
            _ => false,
        }
    }

    /// Check if a type is numeric (supports arithmetic operations).
    fn is_numeric(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::Primitive(PrimitiveType::I32) | Type::Primitive(PrimitiveType::F64)
        )
    }

    /// Unify a parameter type with a concrete argument type, collecting substitutions
    /// for generic type parameters. This enables type inference for generic functions.
    fn unify_types(
        &self,
        param_ty: &Type,
        arg_ty: &Type,
        type_params: &[String],
        substitutions: &mut std::collections::HashMap<String, Type>,
    ) {
        match param_ty {
            Type::Named { name, args } if args.is_empty() && type_params.contains(name) => {
                // This is a generic type parameter - record or check substitution
                if let Some(existing) = substitutions.get(name) {
                    // Already have a substitution - should be compatible
                    // For now, we just accept the first one
                    let _ = existing;
                } else {
                    substitutions.insert(name.clone(), arg_ty.clone());
                }
            }
            Type::Named { name, args } => {
                // Concrete named type - recursively unify type arguments
                if let Type::Named { name: arg_name, args: arg_args } = arg_ty {
                    if name == arg_name && args.len() == arg_args.len() {
                        for (param_arg, arg_arg) in args.iter().zip(arg_args.iter()) {
                            self.unify_types(param_arg, arg_arg, type_params, substitutions);
                        }
                    }
                }
            }
            Type::Function { params, ret } => {
                // Function type - recursively unify params and return
                if let Type::Function { params: arg_params, ret: arg_ret } = arg_ty {
                    if params.len() == arg_params.len() {
                        for (p, a) in params.iter().zip(arg_params.iter()) {
                            self.unify_types(p, a, type_params, substitutions);
                        }
                        self.unify_types(ret, arg_ret, type_params, substitutions);
                    }
                }
            }
            _ => {
                // Primitive types don't contribute to substitutions
            }
        }
    }

    /// Apply collected substitutions to a type, replacing generic parameters
    /// with their inferred concrete types.
    fn apply_substitutions(
        &self,
        ty: &Type,
        substitutions: &std::collections::HashMap<String, Type>,
    ) -> Type {
        match ty {
            Type::Named { name, args } if args.is_empty() => {
                // Could be a generic parameter
                if let Some(concrete) = substitutions.get(name) {
                    concrete.clone()
                } else {
                    ty.clone()
                }
            }
            Type::Named { name, args } => {
                // Apply substitutions to type arguments
                Type::Named {
                    name: name.clone(),
                    args: args.iter()
                        .map(|a| self.apply_substitutions(a, substitutions))
                        .collect(),
                }
            }
            Type::Function { params, ret } => {
                Type::Function {
                    params: params.iter()
                        .map(|p| self.apply_substitutions(p, substitutions))
                        .collect(),
                    ret: Box::new(self.apply_substitutions(ret, substitutions)),
                }
            }
            _ => ty.clone(),
        }
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
                        husk_ast::ExternItemKind::Mod { binding, items, .. } => {
                            if items.is_empty() {
                                self.add_symbol(binding, SymbolKind::ExternMod);
                            } else {
                                // Register functions from mod block
                                for mod_item in items {
                                    // ModItemKind has only Fn variant (MVP scope)
                                    let husk_ast::ModItemKind::Fn { name, .. } = &mod_item.kind;
                                    self.add_symbol(name, SymbolKind::ExternFn);
                                }
                            }
                        }
                        husk_ast::ExternItemKind::Struct { name, .. } => {
                            self.add_symbol(name, SymbolKind::Struct);
                        }
                        husk_ast::ExternItemKind::Static { name, .. } => {
                            self.add_symbol(name, SymbolKind::ExternStatic);
                        }
                    }
                }
            }
            ItemKind::Use { .. } => {}
            ItemKind::Trait(trait_def) => {
                self.add_symbol(&trait_def.name, SymbolKind::Trait);
            }
            ItemKind::Impl(impl_block) => {
                // Impl blocks don't define a named symbol, but we track them
                // We can use a synthetic name for debugging/tracking
                let self_ty_name = type_expr_to_name(&impl_block.self_ty);
                let impl_name = if let Some(trait_ref) = &impl_block.trait_ref {
                    let trait_name = type_expr_to_name(trait_ref);
                    format!("<impl {} for {}>", trait_name, self_ty_name)
                } else {
                    format!("<impl {}>", self_ty_name)
                };
                // Create a synthetic ident for the impl
                let synth_ident = Ident {
                    name: impl_name,
                    span: impl_block.span.clone(),
                };
                self.add_symbol(&synth_ident, SymbolKind::Impl);
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
                    attributes: Vec::new(),
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
                    attributes: Vec::new(),
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
                    attributes: Vec::new(),
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
                    attributes: Vec::new(),
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
                attributes: Vec::new(),
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
                attributes: Vec::new(),
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
            attributes: Vec::new(),
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
            attributes: Vec::new(),
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
            attributes: Vec::new(),
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
            attributes: Vec::new(),
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
            attributes: Vec::new(),
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
            attributes: Vec::new(),
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
                items: Vec::new(),
                is_global: false,
            },
            span: Span { range: 0..15 },
        };
        let extern_block = Item {
            attributes: Vec::new(),
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
            attributes: Vec::new(),
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

    #[test]
    fn prelude_jsvalue_available_by_default() {
        // JsValue and jsvalue_get should be available without explicit declaration
        let src = r#"
fn main() {
    let _v: JsValue;
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

    #[test]
    fn prelude_js_globals_not_available_with_no_prelude() {
        // With --no-prelude, JsValue should not be available
        let src = r#"
fn main() {
    let _v: JsValue;
}
"#;
        let parsed = parse_str(src);
        assert!(
            parsed.errors.is_empty(),
            "parse errors: {:?}",
            parsed.errors
        );
        let file = parsed.file.expect("parser produced no AST");
        let result = analyze_file_without_prelude(&file);
        // Should have a type error for unknown type JsValue
        assert!(
            result.type_errors.iter().any(|e| e.message.contains("unknown type `JsValue`")),
            "expected unknown type error for JsValue, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn loop_allows_break_inside() {
        let src = r#"
fn main() {
    loop {
        break;
    }
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
            result.type_errors.is_empty(),
            "unexpected type errors: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn loop_allows_continue_inside() {
        let src = r#"
fn main() {
    loop {
        continue;
    }
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
            result.type_errors.is_empty(),
            "unexpected type errors: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn break_outside_loop_reports_error() {
        let src = r#"
fn main() {
    break;
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
            result.type_errors.iter().any(|e| e.message.contains("`break` used outside of loop")),
            "expected break outside loop error, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn continue_outside_loop_reports_error() {
        let src = r#"
fn main() {
    continue;
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
            result.type_errors.iter().any(|e| e.message.contains("`continue` used outside of loop")),
            "expected continue outside loop error, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn nested_loop_allows_break() {
        let src = r#"
fn main() {
    loop {
        loop {
            break;
        }
        break;
    }
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
            result.type_errors.is_empty(),
            "unexpected type errors: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn string_substring_returns_string() {
        // substring should be typed as returning String
        let src = r#"
fn test(s: String) -> String {
    s.substring(0)
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
            result.type_errors.is_empty(),
            "expected no type errors for String.substring(), got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn shadow_param_in_loop_generates_unique_names() {
        // When shadowing a function parameter inside a loop with `let s = s.something()`,
        // the new variable should get a unique name (s$1) while the RHS reference
        // should still use the original parameter name (s).
        let src = r#"
fn process(s: String) -> String {
    for n in 0..3 {
        let s = s.substring(n);
    }
    s
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
            result.type_errors.is_empty(),
            "expected no type errors, got: {:?}",
            result.type_errors
        );

        // Check that name resolution produces different names for LHS and RHS
        // The new `s` binding (LHS) should be renamed to avoid JS "cannot access before init" error
        let mut found_shadowed_binding = false;
        for ((_start, _end), resolved_name) in &result.name_resolution {
            if resolved_name.starts_with("s$") {
                found_shadowed_binding = true;
                break;
            }
        }
        assert!(
            found_shadowed_binding,
            "expected shadowed variable 's' to be renamed to 's$N', but name_resolution only has: {:?}",
            result.name_resolution
        );
    }

    // ========== Trait Bound Tests ==========

    #[test]
    fn supertrait_parsing() {
        // Test that trait Eq: PartialEq parses correctly
        let src = r#"
trait PartialEq {}
trait Eq: PartialEq {}
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
            result.type_errors.is_empty(),
            "unexpected type errors: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn trait_impl_requires_supertrait() {
        // Implementing Eq without PartialEq should error
        let src = r#"
trait PartialEq {}
trait Eq: PartialEq {}

struct Foo {}

impl Eq for Foo {}
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
            result.type_errors.iter().any(|e| e.message.contains("missing implementation of supertrait") && e.message.contains("PartialEq")),
            "expected supertrait error, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn trait_impl_with_supertrait_satisfied() {
        // Implementing both PartialEq and Eq should succeed
        let src = r#"
trait PartialEq {}
trait Eq: PartialEq {}

struct Foo {}

impl PartialEq for Foo {}
impl Eq for Foo {}
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
            result.type_errors.is_empty(),
            "unexpected type errors: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn function_with_trait_bound_parses() {
        // Test that function with trait bound parses correctly
        let src = r#"
trait PartialEq {}

fn compare<T: PartialEq>(a: T, b: T) -> bool {
    true
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
            result.type_errors.is_empty(),
            "unexpected type errors: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn function_call_with_trait_bound_satisfied() {
        // Calling a generic function with a type that implements the bound should succeed
        let src = r#"
trait PartialEq {}

impl PartialEq for i32 {}

fn compare<T: PartialEq>(a: T, b: T) -> bool {
    true
}

fn main() {
    let x = compare(1, 2);
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
            result.type_errors.is_empty(),
            "unexpected type errors: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn function_call_with_trait_bound_not_satisfied() {
        // Calling a generic function with a type that doesn't implement the bound should error
        let src = r#"
trait PartialEq {}

struct NoEq {}

fn compare<T: PartialEq>(a: T, b: T) -> bool {
    true
}

fn main() {
    let x = NoEq {};
    let y = NoEq {};
    let z = compare(x, y);
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
            result.type_errors.iter().any(|e| e.message.contains("NoEq: PartialEq") && e.message.contains("not satisfied")),
            "expected trait bound error, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn transitive_supertrait_chain_requires_all() {
        // A: B: C means implementing A requires implementing both B and C
        let src = r#"
trait Base {}
trait Middle: Base {}
trait Top: Middle {}

struct Foo {}

// Only implementing Top without Middle or Base should fail
impl Top for Foo {}
"#;
        let parsed = parse_str(src);
        assert!(
            parsed.errors.is_empty(),
            "parse errors: {:?}",
            parsed.errors
        );
        let file = parsed.file.expect("parser produced no AST");
        let result = analyze_file(&file);
        // Should error about missing Middle (direct supertrait)
        assert!(
            result.type_errors.iter().any(|e| e.message.contains("Middle")),
            "expected error about missing Middle, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn transitive_supertrait_chain_satisfied() {
        // Implementing all traits in the chain should succeed
        let src = r#"
trait Base {}
trait Middle: Base {}
trait Top: Middle {}

struct Foo {}

impl Base for Foo {}
impl Middle for Foo {}
impl Top for Foo {}
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
            result.type_errors.is_empty(),
            "unexpected type errors: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn transitive_supertrait_missing_indirect() {
        // Middle: Base, implementing Middle without Base should report Base as missing
        let src = r#"
trait Base {}
trait Middle: Base {}

struct Foo {}

impl Middle for Foo {}
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
            result.type_errors.iter().any(|e| e.message.contains("Base")),
            "expected error about missing Base, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn generic_type_matches_base_type_impl() {
        // A generic struct with trait impl should satisfy bounds when instantiated
        // e.g., Vec<i32> should match impl PartialEq for Vec
        let src = r#"
trait PartialEq {}

struct Container<T> {
    value: T,
}

impl PartialEq for Container {}

fn compare<T: PartialEq>(a: T, b: T) -> bool {
    true
}

fn main() {
    let c1 = Container { value: 42 };
    let c2 = Container { value: 43 };
    let result = compare(c1, c2);
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
            result.type_errors.is_empty(),
            "expected no errors for generic type with base impl, got: {:?}",
            result.type_errors
        );
    }
}
