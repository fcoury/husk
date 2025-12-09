//! Minimal JavaScript AST and pretty-printer for Husk codegen.
//!
//! This crate currently defines:
//! - A small JS AST (expressions, statements, modules).
//! - A pretty-printer that renders the AST to a JS source string.
//! - A minimal lowering from Husk AST to this JS AST for simple functions.
//! - Source map generation for debugging.

use husk_ast::{
    Block, EnumVariantFields, Expr, ExprKind, ExternItemKind, File, FormatSegment, FormatSpec,
    Ident, ImplItemKind, ItemKind, LiteralKind, Param, Pattern, PatternKind, Span, Stmt, StmtKind,
    StructField, TypeExpr, TypeExprKind, TypeParam,
};
use husk_runtime_js::std_preamble_js;
use husk_semantic::{
    NameResolution, TypeResolution, VariantCallMap, VariantPatternMap, get_prelude_file,
};
use sourcemap::SourceMapBuilder;
use std::collections::HashMap;
use std::path::Path;

/// Normalize type name for lookup to match format used by type_resolution.
/// Converts array types from "T[]" format (from type_expr_to_js_name) to "[T]" format
/// (used by format_type in type_resolution).
fn normalize_type_name_for_lookup(type_name: &str) -> String {
    // Convert "T[]" to "[T]" format to match type_resolution
    if type_name.ends_with("[]") {
        let elem_type = &type_name[..type_name.len() - 2];
        format!("[{}]", elem_type)
    } else {
        type_name.to_string()
    }
}

/// Convert snake_case to camelCase.
/// Examples:
/// - "status_code" -> "statusCode"
/// - "last_insert_rowid" -> "lastInsertRowid"
/// - "body" -> "body" (no change if no underscores)
fn snake_to_camel(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut capitalize_next = false;
    for c in s.chars() {
        if c == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_ascii_uppercase());
            capitalize_next = false;
        } else {
            result.push(c);
        }
    }
    result
}

/// Tracks extern property accessors (getters and setters) for codegen.
///
/// When an extern impl method matches the getter/setter pattern, we need to
/// generate property access instead of method calls:
/// - `obj.prop()` → `obj.prop` (getter)
/// - `obj.set_prop(value)` → `obj.prop = value` (setter)
#[derive(Debug, Clone, Default)]
struct PropertyAccessors {
    /// Maps (type_name, method_name) -> property_name for getters
    /// e.g., ("Request", "body") -> "body"
    getters: HashMap<(String, String), String>,
    /// Maps (type_name, method_name) -> property_name for setters
    /// e.g., ("Request", "set_body") -> "body"
    setters: HashMap<(String, String), String>,
    /// Maps (type_name, method_name) -> js_name for #[js_name] overrides
    /// This allows explicit control over the JavaScript method name.
    /// e.g., ("String", "len") -> "length" (for String.len() -> str.length)
    method_js_names: HashMap<(String, String), String>,
    /// Set of (type_name, method_name) pairs for extern "js" methods.
    /// Only extern "js" methods get automatic snake_to_camel conversion;
    /// user-defined Husk methods keep their original names.
    extern_methods: std::collections::HashSet<(String, String)>,
    /// Set of (type_name, method_name) pairs for methods with #[this] binding.
    /// These methods require .call(thisArg, ...) invocation in JavaScript.
    this_binding_methods: std::collections::HashSet<(String, String)>,
    /// Set of enum names that are marked with #[untagged].
    /// Untagged enums serialize without a tag field, matching TypeScript's untagged unions.
    untagged_enums: std::collections::HashSet<String>,
}

/// Collect only #[js_name] method name mappings from a file.
///
/// This function only extracts #[js_name] overrides which are type-keyed and safe
/// to use from the stdlib prelude without affecting user-defined types.
/// Note: we don't add these to extern_methods because the extern_methods check is
/// type-agnostic and would incorrectly rename user methods with the same name.
fn collect_js_name_mappings(file: &File, accessors: &mut PropertyAccessors) {
    // JS properties that should be accessed as properties, not method calls
    const JS_PROPERTY_NAMES: &[&str] = &["size", "length"];

    for item in &file.items {
        if let ItemKind::Impl(impl_block) = &item.kind {
            let type_name = type_expr_to_js_name(&impl_block.self_ty);
            // Normalize array types from "T[]" to "[T]" format to match type_resolution
            let normalized_type_name = normalize_type_name_for_lookup(&type_name);
            for impl_item in &impl_block.items {
                if let ImplItemKind::Method(method) = &impl_item.kind {
                    // Collect #[js_name] overrides for method name mapping
                    if let Some(js_name) = method.js_name() {
                        let method_name = &method.name.name;
                        accessors.method_js_names.insert(
                            (normalized_type_name.clone(), method_name.clone()),
                            js_name.to_string(),
                        );

                        // If the js_name maps to a known JS property, register as getter
                        // This allows methods like len() -> size/length to be emitted
                        // as property access instead of method calls
                        if JS_PROPERTY_NAMES.contains(&js_name)
                            && method.params.is_empty()
                            && method.ret_type.is_some()
                        {
                            accessors.getters.insert(
                                (normalized_type_name.clone(), method_name.clone()),
                                js_name.to_string(),
                            );
                        }
                    }
                }
            }
        }
    }
}

/// Collect property accessors and method info from a file's impl blocks.
///
/// This function extracts #[js_name] overrides, extern method info, and getter/setter
/// patterns from impl blocks and adds them to the provided accessors map.
/// This should only be called for user files, not the stdlib prelude.
fn collect_accessors_from_file(file: &File, accessors: &mut PropertyAccessors) {
    for item in &file.items {
        if let ItemKind::Impl(impl_block) = &item.kind {
            let type_name = type_expr_to_js_name(&impl_block.self_ty);
            // Normalize array types from "T[]" to "[T]" format to match type_resolution
            let normalized_type_name = normalize_type_name_for_lookup(&type_name);
            for impl_item in &impl_block.items {
                match &impl_item.kind {
                    // Handle explicit extern properties with #[getter]/#[setter]
                    ImplItemKind::Property(prop) => {
                        let prop_name = &prop.name.name;
                        // Get the JS name: use #[js_name] if present, otherwise snake_to_camel
                        let js_name = prop
                            .js_name()
                            .map(|s| s.to_string())
                            .unwrap_or_else(|| snake_to_camel(prop_name));

                        if prop.has_getter() {
                            accessors
                                .getters
                                .insert((normalized_type_name.clone(), prop_name.clone()), js_name.clone());
                        }
                        if prop.has_setter() {
                            accessors
                                .setters
                                .insert((normalized_type_name.clone(), prop_name.clone()), js_name);
                        }
                    }
                    // Handle extern methods for #[js_name] and getter/setter heuristics
                    ImplItemKind::Method(method) => {
                        let method_name = &method.name.name;

                        // Collect #[js_name] overrides for method name mapping
                        if let Some(js_name) = method.js_name() {
                            accessors.method_js_names.insert(
                                (normalized_type_name.clone(), method_name.clone()),
                                js_name.to_string(),
                            );
                        }

                        if method.is_extern && method.receiver.is_some() {
                            // Track this as an extern method for snake_to_camel conversion
                            let base_name = strip_variadic_suffix(method_name);
                            accessors
                                .extern_methods
                                .insert((normalized_type_name.clone(), base_name.clone()));

                            // Check if the first parameter has #[this] attribute
                            if let Some(first_param) = method.params.first() {
                                if first_param.attributes.iter().any(|a| a.name.name == "this") {
                                    accessors
                                        .this_binding_methods
                                        .insert((normalized_type_name.clone(), base_name.clone()));
                                }
                            }

                            // Methods that should NOT be treated as getters
                            const NON_GETTER_METHODS: &[&str] = &[
                                "all",
                                "toJsValue",
                                "open",
                                "run",
                                "iterate",
                                "bind",
                                "pluck",
                                "raw",
                                "columns",
                            ];
                            let is_non_getter = NON_GETTER_METHODS.contains(&method_name.as_str());

                            // Check if it's a getter: no params, has return type, not excluded
                            if method.params.is_empty()
                                && method.ret_type.is_some()
                                && !is_non_getter
                            {
                                accessors.getters.insert(
                                    (normalized_type_name.clone(), method_name.clone()),
                                    method_name.clone(),
                                );
                            }
                            // Check if it's a setter: starts with "set_", one param, no return
                            else if method_name.starts_with("set_")
                                && method.params.len() == 1
                                && method.ret_type.is_none()
                            {
                                let prop_name =
                                    method_name.strip_prefix("set_").unwrap().to_string();
                                accessors
                                    .setters
                                    .insert((normalized_type_name.clone(), method_name.clone()), prop_name);
                            }
                        }
                    }
                }
            }
        }

        // Collect untagged enums for variant construction
        if let ItemKind::Enum { name, .. } = &item.kind {
            if item.is_untagged() {
                accessors.untagged_enums.insert(name.name.clone());
            }
        }
    }
}

/// Context for code generation, carrying compile-time state.
///
/// This struct is threaded through all lowering functions to provide:
/// - Property accessors for extern type method rewrites
/// - Source file path for compile-time operations like include_str
/// - Name resolution for variable shadowing
/// - Type resolution for conversion methods (.into(), .parse(), .try_into())
/// - Variant call map for imported enum variant constructor calls
/// - Variant pattern map for imported enum variant patterns in match
#[derive(Debug, Clone)]
struct CodegenContext<'a> {
    /// Property accessors for extern types
    accessors: &'a PropertyAccessors,
    /// Path to the current source file being compiled (for include_str, etc.)
    source_path: Option<&'a Path>,
    /// Name resolution map from semantic analysis for variable shadowing.
    /// Maps (span_start, span_end) -> resolved_name (e.g., "x", "x$1", "x$2")
    name_resolution: &'a NameResolution,
    /// Type resolution map from semantic analysis for conversion methods.
    /// Maps (span_start, span_end) -> type_name (e.g., "String", "i32")
    type_resolution: &'a TypeResolution,
    /// Maps call spans to (enum_name, variant_name) for imported variant constructor calls.
    variant_calls: &'a VariantCallMap,
    /// Maps pattern spans to (enum_name, variant_name) for imported variant patterns in match.
    variant_patterns: &'a VariantPatternMap,
}

impl<'a> CodegenContext<'a> {
    fn new(
        accessors: &'a PropertyAccessors,
        name_resolution: &'a NameResolution,
        type_resolution: &'a TypeResolution,
        variant_calls: &'a VariantCallMap,
        variant_patterns: &'a VariantPatternMap,
    ) -> Self {
        Self {
            accessors,
            source_path: None,
            name_resolution,
            type_resolution,
            variant_calls,
            variant_patterns,
        }
    }

    fn with_source_path(
        accessors: &'a PropertyAccessors,
        source_path: &'a Path,
        name_resolution: &'a NameResolution,
        type_resolution: &'a TypeResolution,
        variant_calls: &'a VariantCallMap,
        variant_patterns: &'a VariantPatternMap,
    ) -> Self {
        Self {
            accessors,
            source_path: Some(source_path),
            name_resolution,
            type_resolution,
            variant_calls,
            variant_patterns,
        }
    }

    /// Get the resolved name for a variable at the given span.
    /// Returns the original name if not found in resolution map.
    fn resolve_name(&self, name: &str, span: &Span) -> String {
        self.name_resolution
            .get(&(span.range.start, span.range.end))
            .cloned()
            .unwrap_or_else(|| name.to_string())
    }

    /// Generate a fresh temporary variable name.
    fn fresh_temp(&self, prefix: &str) -> String {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        format!("{}_{}", prefix, COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

/// Handle include_str("path") - reads file at compile time and returns contents as string literal.
///
/// The path is resolved relative to the source file being compiled.
/// Panics with a clear error message if the file cannot be read.
fn handle_include_str(args: &[Expr], ctx: &CodegenContext) -> JsExpr {
    // Validate: exactly one argument
    if args.len() != 1 {
        panic!("include_str: expected 1 argument, got {}", args.len());
    }

    // Extract the path argument (must be a string literal)
    let path_arg = match &args[0].kind {
        ExprKind::Literal(lit) => match &lit.kind {
            LiteralKind::String(s) => s.clone(),
            _ => {
                panic!("include_str: argument must be a string literal");
            }
        },
        _ => {
            panic!("include_str: argument must be a string literal");
        }
    };

    // Resolve path relative to source file or current directory as fallback
    let base_dir = match ctx.source_path {
        Some(path) => path.parent().unwrap_or(Path::new(".")).to_path_buf(),
        None => {
            // Fallback: resolve relative to current working directory
            std::env::current_dir().unwrap_or_else(|_| Path::new(".").to_path_buf())
        }
    };
    let full_path = base_dir.join(&path_arg);

    // Read file contents
    match std::fs::read_to_string(&full_path) {
        Ok(contents) => JsExpr::String(contents),
        Err(e) => {
            panic!(
                "include_str: failed to read '{}': {}",
                full_path.display(),
                e
            );
        }
    }
}

/// A JavaScript module (ES module) consisting of a list of statements.
#[derive(Debug, Clone, PartialEq)]
pub struct JsModule {
    pub body: Vec<JsStmt>,
}

/// Source span for mapping back to Husk source.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SourceSpan {
    /// Source line (0-indexed)
    pub line: u32,
    /// Source column (0-indexed)
    pub column: u32,
}

/// Compute line and column (0-indexed) from a byte offset in source text.
pub fn offset_to_line_col(source: &str, offset: usize) -> (u32, u32) {
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    (line, col)
}

/// Pattern element for array destructuring (supports nesting).
#[derive(Debug, Clone, PartialEq)]
pub enum DestructurePattern {
    /// A simple binding: `a`
    Binding(String),
    /// A wildcard/ignored position: `_` (not actually bound)
    Wildcard,
    /// A nested array pattern: `[a, b]`
    Array(Vec<DestructurePattern>),
}

/// JavaScript statements (subset).
#[derive(Debug, Clone, PartialEq)]
pub enum JsStmt {
    /// `import name from "source";`
    Import { name: String, source: String },
    /// `import { name1, name2, ... } from "source";`
    NamedImport { names: Vec<String>, source: String },
    /// `const name = require("source");`
    Require { name: String, source: String },
    /// `const { name1, name2, ... } = require("source");`
    NamedRequire { names: Vec<String>, source: String },
    /// `export { name1, name2, ... };`
    ExportNamed { names: Vec<String> },
    /// `function name(params) { body }`
    Function {
        name: String,
        params: Vec<String>,
        body: Vec<JsStmt>,
        /// Optional source span for source map generation.
        source_span: Option<SourceSpan>,
    },
    /// `return expr;`
    Return(JsExpr),
    /// `let name = expr;`
    Let { name: String, init: Option<JsExpr> },
    /// `let [pattern, ...] = expr;` (array destructuring, supports nesting)
    LetDestructure {
        pattern: Vec<DestructurePattern>,
        init: Option<JsExpr>,
    },
    /// Expression statement: `expr;`
    Expr(JsExpr),
    /// `try { ... } catch (e) { ... }`
    TryCatch {
        try_block: Vec<JsStmt>,
        catch_ident: String,
        catch_block: Vec<JsStmt>,
    },
    /// `if (cond) { ... } else { ... }`
    If {
        cond: JsExpr,
        then_block: Vec<JsStmt>,
        else_block: Option<Vec<JsStmt>>,
    },
    /// `for (const binding of iterable) { body }`
    ForOf {
        binding: String,
        iterable: JsExpr,
        body: Vec<JsStmt>,
    },
    /// C-style for loop: `for (let binding = start; binding < end; binding++)`
    For {
        binding: String,
        start: JsExpr,
        end: JsExpr,
        inclusive: bool,
        body: Vec<JsStmt>,
    },
    /// For loop over a range object: `for (let binding = range.start; binding < range.end; binding++)`
    ForRange {
        binding: String,
        range_expr: JsExpr,
        body: Vec<JsStmt>,
    },
    /// Assignment statement: `target = value`, `target += value`, etc.
    Assign {
        target: JsExpr,
        op: JsAssignOp,
        value: JsExpr,
    },
    /// `while (cond) { body }`
    While { cond: JsExpr, body: Vec<JsStmt> },
    /// `break;`
    Break,
    /// `continue;`
    Continue,
    /// A bare block: `{ stmts }`
    Block(Vec<JsStmt>),
    /// Multiple statements emitted at the same level (no block wrapper).
    /// Used for let-else to emit the check and bindings without creating a scope.
    Sequence(Vec<JsStmt>),
    /// `throw expr;`
    Throw(JsExpr),
}

/// Assignment operators in JS.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JsAssignOp {
    Assign,    // =
    AddAssign, // +=
    SubAssign, // -=
    ModAssign, // %=
}

/// JavaScript expressions (subset).
#[derive(Debug, Clone, PartialEq)]
pub enum JsExpr {
    Ident(String),
    Number(i64),
    BigInt(i64),
    Float(f64),
    Bool(bool),
    String(String),
    /// Object literal: `{ key: value, ... }`.
    Object(Vec<(String, JsExpr)>),
    /// Property access: `object.property`.
    Member {
        object: Box<JsExpr>,
        property: String,
    },
    /// Assignment expression: `left = right` or `left += right` etc.
    Assignment {
        left: Box<JsExpr>,
        op: JsAssignOp,
        right: Box<JsExpr>,
    },
    /// Conditional (ternary) expression: `test ? then_branch : else_branch`.
    Conditional {
        test: Box<JsExpr>,
        then_branch: Box<JsExpr>,
        else_branch: Box<JsExpr>,
    },
    Call {
        callee: Box<JsExpr>,
        args: Vec<JsExpr>,
    },
    Binary {
        op: JsBinaryOp,
        left: Box<JsExpr>,
        right: Box<JsExpr>,
    },
    /// Immediately Invoked Function Expression: `(function() { body })()`.
    Iife {
        body: Vec<JsStmt>,
    },
    /// Function expression: `function(params) { body }`.
    Function {
        params: Vec<String>,
        body: Vec<JsStmt>,
    },
    /// Constructor call: `new Foo(args)`.
    New {
        constructor: String,
        args: Vec<JsExpr>,
    },
    /// Arrow function: `(x, y) => expr` or `(x, y) => { ... }`.
    Arrow {
        params: Vec<String>,
        body: Vec<JsStmt>,
    },
    /// Array literal: `[elem1, elem2, ...]`.
    Array(Vec<JsExpr>),
    /// Computed member access (indexing): `object[index]`.
    Index {
        object: Box<JsExpr>,
        index: Box<JsExpr>,
    },
    /// Raw JavaScript code, emitted directly (wrapped in parentheses for safety).
    Raw(String),
    /// Unary expression: `!expr` or `-expr`.
    Unary {
        op: JsUnaryOp,
        expr: Box<JsExpr>,
    },
}

/// Unary operators in JS.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JsUnaryOp {
    Not, // !
    Neg, // -
}

/// Binary operators in JS (subset we need).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JsBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    EqEq,
    NotEq,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    StrictEq, // ===
    StrictNe, // !==
}

/// Output target style.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JsTarget {
    Esm,
    Cjs,
}

/// Lower a Husk AST file into a JS module.
///
/// This is intentionally minimal for now and only supports:
/// - Top-level functions.
/// - Simple statements (`let`, `return expr`, expression statements).
/// - Basic expressions (literals, identifiers, calls, and arithmetic/boolean binary ops).
///
/// `call_main` controls whether a zero-argument `main` function is
/// automatically invoked at the end of the module. Library-style
/// consumers should set this to `false` so `main` can be called
/// explicitly by a host.
///
/// `name_resolution` provides the mapping from variable spans to resolved names
/// for handling variable shadowing. Use an empty HashMap if shadowing is not needed.
/// `type_resolution` provides the mapping from expression spans to resolved types
/// for type-dependent operations like .into(), .parse(), .try_into().
/// `variant_calls` provides the mapping from call spans to enum/variant names
/// for imported variant constructor calls.
/// `variant_patterns` provides the mapping from pattern spans to enum/variant names
/// for imported variant patterns in match expressions.
pub fn lower_file_to_js(
    file: &File,
    call_main: bool,
    target: JsTarget,
    name_resolution: &NameResolution,
    type_resolution: &TypeResolution,
    variant_calls: &VariantCallMap,
    variant_patterns: &VariantPatternMap,
) -> JsModule {
    lower_file_to_js_with_source(
        file,
        call_main,
        target,
        None,
        None,
        name_resolution,
        type_resolution,
        variant_calls,
        variant_patterns,
    )
}

/// Lower a Husk AST file into a JS module with source text for accurate source maps.
///
/// When `source` is provided, source spans will have accurate line/column info.
/// When `source_path` is provided, compile-time operations like `include_str` can resolve
/// relative file paths.
/// `name_resolution` provides the mapping from variable spans to resolved names
/// for handling variable shadowing.
/// `type_resolution` provides the mapping from expression spans to resolved types
/// for type-dependent operations like .into(), .parse(), .try_into().
/// `variant_calls` provides the mapping from call spans to enum/variant names
/// for imported variant constructor calls.
/// `variant_patterns` provides the mapping from pattern spans to enum/variant names
/// for imported variant patterns in match expressions.
pub fn lower_file_to_js_with_source(
    file: &File,
    call_main: bool,
    target: JsTarget,
    source: Option<&str>,
    source_path: Option<&Path>,
    name_resolution: &NameResolution,
    type_resolution: &TypeResolution,
    variant_calls: &VariantCallMap,
    variant_patterns: &VariantPatternMap,
) -> JsModule {
    use std::collections::HashSet;

    let mut imports = Vec::new();
    let mut body = Vec::new();
    let mut has_main_entry = false;
    let mut fn_names: Vec<String> = Vec::new();

    // Collect extern struct names - these are opaque JS types
    let mut extern_structs: HashSet<String> = HashSet::new();
    for item in &file.items {
        if let ItemKind::ExternBlock { abi, items } = &item.kind {
            if abi == "js" {
                for ext in items {
                    if let ExternItemKind::Struct { name, .. } = &ext.kind {
                        extern_structs.insert(name.name.clone());
                    }
                }
            }
        }
    }

    // Collect property accessors from extern impl blocks
    // First collect js_name mappings from stdlib prelude (for #[js_name] like split_once -> __husk_split_once)
    // Only collect js_name mappings from prelude - not getters/setters/extern_methods which would
    // incorrectly affect user-defined types with the same method names.
    // Then collect full accessors from user file.
    let mut accessors = PropertyAccessors::default();
    collect_js_name_mappings(get_prelude_file(), &mut accessors);
    collect_accessors_from_file(file, &mut accessors);

    // Create codegen context with accessors, optional source path, name resolution, type resolution, and variant maps
    let ctx = match source_path {
        Some(path) => CodegenContext::with_source_path(
            &accessors,
            path,
            name_resolution,
            type_resolution,
            variant_calls,
            variant_patterns,
        ),
        None => CodegenContext::new(
            &accessors,
            name_resolution,
            type_resolution,
            variant_calls,
            variant_patterns,
        ),
    };

    // First pass: collect module imports
    for item in &file.items {
        if let ItemKind::ExternBlock { abi, items } = &item.kind {
            if abi == "js" {
                for ext in items {
                    if let ExternItemKind::Mod {
                        package,
                        binding,
                        items: mod_items,
                        is_global,
                    } = &ext.kind
                    {
                        // Skip import/require for global JS objects (Array, Math, JSON, etc.)
                        if *is_global {
                            continue;
                        }

                        if mod_items.is_empty() {
                            // Simple default import: import binding from "package";
                            match target {
                                JsTarget::Esm => imports.push(JsStmt::Import {
                                    name: binding.name.clone(),
                                    source: package.clone(),
                                }),
                                JsTarget::Cjs => imports.push(JsStmt::Require {
                                    name: binding.name.clone(),
                                    source: package.clone(),
                                }),
                            }
                        } else {
                            // Named imports: import { fn1, fn2 } from "package";
                            let names: Vec<String> = mod_items
                                .iter()
                                .filter_map(|mi| match &mi.kind {
                                    husk_ast::ModItemKind::Fn { name, .. } => {
                                        Some(name.name.clone())
                                    }
                                })
                                .collect();
                            match target {
                                JsTarget::Esm => imports.push(JsStmt::NamedImport {
                                    names,
                                    source: package.clone(),
                                }),
                                JsTarget::Cjs => imports.push(JsStmt::NamedRequire {
                                    names,
                                    source: package.clone(),
                                }),
                            }
                        }
                    }
                }
            }
        }
    }

    // Second pass: generate constructor functions for structs (needed for prototype methods)
    // Skip extern structs - they're opaque JS types and don't need constructors
    for item in &file.items {
        if let ItemKind::Struct { name, fields, .. } = &item.kind {
            if !extern_structs.contains(&name.name) {
                body.push(lower_struct_constructor(&name.name, fields));
            }
        }
    }

    // Third pass: process functions and extern functions
    for item in &file.items {
        match &item.kind {
            ItemKind::Fn {
                name,
                params,
                body: fn_body,
                ..
            } => {
                body.push(lower_fn_with_span(
                    &name.name, params, fn_body, &item.span, source, &ctx,
                ));
                fn_names.push(name.name.clone());
                if name.name == "main" && params.is_empty() {
                    has_main_entry = true;
                }
            }
            ItemKind::ExternBlock { abi, items } => {
                if abi == "js" {
                    // Functions that are already defined in the runtime preamble
                    // and should not have wrappers generated
                    const PREAMBLE_FUNCTIONS: &[&str] = &[
                        "JsObject_new",
                        "jsvalue_get",
                        "jsvalue_getString",
                        "jsvalue_getNumber",
                        "jsvalue_getBool",
                        "jsvalue_getArray",
                        "jsvalue_isNull",
                        "jsvalue_toString",
                        "jsvalue_toBool",
                        "jsvalue_toNumber",
                        "express_json",
                    ];

                    for ext in items {
                        match &ext.kind {
                            ExternItemKind::Fn {
                                name,
                                params,
                                ret_type,
                            } => {
                                // Skip functions that are already in the preamble
                                if !PREAMBLE_FUNCTIONS.contains(&name.name.as_str()) {
                                    // Use #[js_name] if specified, otherwise use the Husk function name
                                    let js_name = ext.js_name().map(|s| s.to_string());
                                    // Check for #[module_call] attribute for class module callables
                                    let module_call = ext.module_call().map(|s| s.to_string());
                                    // Check for #[ns] attribute for namespace member functions
                                    let namespace = ext.namespace().map(|s| s.to_string());
                                    body.push(lower_extern_fn(
                                        name,
                                        params,
                                        ret_type.as_ref(),
                                        js_name.as_deref(),
                                        module_call.as_deref(),
                                        namespace.as_deref(),
                                    ));
                                }
                            }
                            ExternItemKind::Mod { .. } => {
                                // Already handled in first pass
                            }
                            ExternItemKind::Struct { .. } => {
                                // Extern structs don't generate any code -
                                // they're opaque JS types
                            }
                            ExternItemKind::Static { .. } => {
                                // Static declarations don't generate code -
                                // they declare global JS variables
                            }
                            ExternItemKind::Const { .. } => {
                                // Const declarations don't generate code -
                                // they declare global JS constants accessible directly
                            }
                            ExternItemKind::Impl { .. } => {
                                // Impl blocks in extern are just declarations -
                                // the actual methods live on JS objects
                            }
                        }
                    }
                }
            }
            ItemKind::Impl(impl_block) => {
                // Generate prototype methods for impl blocks
                let self_ty_name = type_expr_to_js_name(&impl_block.self_ty);

                for impl_item in &impl_block.items {
                    if let ImplItemKind::Method(method) = &impl_item.kind {
                        // Skip extern methods - they're just declarations for JS interop
                        // The actual method calls will go directly to the JS object
                        if method.is_extern {
                            continue;
                        }
                        body.push(lower_impl_method(
                            &self_ty_name,
                            method,
                            &impl_item.span,
                            source,
                            &ctx,
                        ));
                    }
                }
            }
            ItemKind::Trait(_) => {
                // Traits don't generate code - they're purely compile-time constructs
            }
            _ => {}
        }
    }
    if has_main_entry && call_main {
        // Wrap main() call to check for ? operator early returns (Err/None)
        // This ensures errors are reported rather than silently swallowed
        body.push(JsStmt::Expr(JsExpr::Call {
            callee: Box::new(JsExpr::Ident("__husk_run_main".to_string())),
            args: vec![JsExpr::Ident("main".to_string())],
        }));
    }

    // Choose export style based on whether we have imports (ESM) or not (CommonJS)
    if !fn_names.is_empty() {
        match target {
            JsTarget::Esm => body.push(JsStmt::ExportNamed { names: fn_names }),
            JsTarget::Cjs => {
                let exports_obj = JsExpr::Object(
                    fn_names
                        .into_iter()
                        .map(|name| (name.clone(), JsExpr::Ident(name)))
                        .collect(),
                );
                let assign = JsExpr::Assignment {
                    left: Box::new(JsExpr::Member {
                        object: Box::new(JsExpr::Ident("module".to_string())),
                        property: "exports".to_string(),
                    }),
                    op: JsAssignOp::Assign,
                    right: Box::new(exports_obj),
                };
                body.push(JsStmt::Expr(assign));
            }
        }
    }

    // Combine imports at the top, then the rest of the body
    let mut full_body = imports;
    full_body.append(&mut body);

    JsModule { body: full_body }
}

/// Check if a slice of statements contains any Try expressions (? operator).
fn contains_try_expr(stmts: &[Stmt]) -> bool {
    fn check_expr(expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Try { .. } => true,
            ExprKind::Binary { left, right, .. } => check_expr(left) || check_expr(right),
            ExprKind::Unary { expr, .. } => check_expr(expr),
            ExprKind::Call { callee, args, .. } => {
                check_expr(callee) || args.iter().any(check_expr)
            }
            ExprKind::MethodCall { receiver, args, .. } => {
                check_expr(receiver) || args.iter().any(check_expr)
            }
            ExprKind::Field { base, .. } => check_expr(base),
            ExprKind::Index { base, index } => check_expr(base) || check_expr(index),
            ExprKind::Block(block) => contains_try_expr(&block.stmts),
            ExprKind::If {
                cond,
                then_branch,
                else_branch,
            } => check_expr(cond) || check_expr(then_branch) || check_expr(else_branch),
            ExprKind::Match { scrutinee, arms } => {
                check_expr(scrutinee) || arms.iter().any(|arm| check_expr(&arm.expr))
            }
            ExprKind::Closure { body, .. } => check_expr(body),
            ExprKind::Array { elements } | ExprKind::Tuple { elements } => {
                elements.iter().any(check_expr)
            }
            ExprKind::Struct { fields, .. } => fields.iter().any(|f| check_expr(&f.value)),
            ExprKind::Range { start, end, .. } => {
                start.as_ref().map_or(false, |e| check_expr(e))
                    || end.as_ref().map_or(false, |e| check_expr(e))
            }
            ExprKind::Assign { target, value, .. } => check_expr(target) || check_expr(value),
            ExprKind::TupleField { base, .. } => check_expr(base),
            ExprKind::Cast { expr, .. } => check_expr(expr),
            ExprKind::FormatPrint { args, .. } | ExprKind::Format { args, .. } => {
                args.iter().any(check_expr)
            }
            _ => false,
        }
    }

    fn check_stmt(stmt: &Stmt) -> bool {
        match &stmt.kind {
            StmtKind::Expr(expr) | StmtKind::Semi(expr) => check_expr(expr),
            StmtKind::Let {
                value,
                else_block,
                ..
            } => {
                value.as_ref().map_or(false, check_expr)
                    || else_block.as_ref().map_or(false, |b| contains_try_expr(&b.stmts))
            }
            StmtKind::ForIn { iterable, body, .. } => {
                check_expr(iterable) || contains_try_expr(&body.stmts)
            }
            StmtKind::While { cond, body } => check_expr(cond) || contains_try_expr(&body.stmts),
            StmtKind::If {
                cond,
                then_branch,
                else_branch,
            } => {
                check_expr(cond)
                    || contains_try_expr(&then_branch.stmts)
                    || else_branch.as_ref().map_or(false, |b| check_stmt(b))
            }
            StmtKind::IfLet {
                scrutinee,
                then_branch,
                else_branch,
                ..
            } => {
                check_expr(scrutinee)
                    || contains_try_expr(&then_branch.stmts)
                    || else_branch.as_ref().map_or(false, |b| check_stmt(b))
            }
            StmtKind::Return { value } => value.as_ref().map_or(false, check_expr),
            StmtKind::Block(block) => contains_try_expr(&block.stmts),
            StmtKind::Loop { body } => contains_try_expr(&body.stmts),
            _ => false,
        }
    }

    stmts.iter().any(check_stmt)
}

fn lower_fn_with_span(
    name: &str,
    params: &[Param],
    body: &[Stmt],
    span: &Span,
    source: Option<&str>,
    ctx: &CodegenContext,
) -> JsStmt {
    let js_params: Vec<String> = params.iter().map(|p| p.name.name.clone()).collect();
    let mut js_body = Vec::new();
    for (i, stmt) in body.iter().enumerate() {
        let is_last = i + 1 == body.len();
        if is_last {
            js_body.push(lower_tail_stmt(stmt, ctx));
        } else {
            js_body.push(lower_stmt(stmt, ctx));
        }
    }

    // If the function uses ? operator, wrap body in try-catch to handle early returns
    let final_body = if contains_try_expr(body) {
        // Catch block: if (e && e.__husk_early_return !== undefined) return e.__husk_early_return; throw e;
        let catch_block = vec![
            JsStmt::If {
                cond: JsExpr::Binary {
                    op: JsBinaryOp::And,
                    left: Box::new(JsExpr::Ident("__e".to_string())),
                    right: Box::new(JsExpr::Binary {
                        op: JsBinaryOp::StrictNe,
                        left: Box::new(JsExpr::Member {
                            object: Box::new(JsExpr::Ident("__e".to_string())),
                            property: "__husk_early_return".to_string(),
                        }),
                        right: Box::new(JsExpr::Ident("undefined".to_string())),
                    }),
                },
                then_block: vec![JsStmt::Return(JsExpr::Member {
                    object: Box::new(JsExpr::Ident("__e".to_string())),
                    property: "__husk_early_return".to_string(),
                })],
                else_block: None,
            },
            JsStmt::Throw(JsExpr::Ident("__e".to_string())),
        ];
        vec![JsStmt::TryCatch {
            try_block: js_body,
            catch_ident: "__e".to_string(),
            catch_block,
        }]
    } else {
        js_body
    };

    // Convert byte offset to line/column if source is provided
    let source_span = source.map(|src| {
        let (line, column) = offset_to_line_col(src, span.range.start);
        SourceSpan { line, column }
    });

    JsStmt::Function {
        name: name.to_string(),
        params: js_params,
        body: final_body,
        source_span,
    }
}

/// Generate a constructor function for a struct.
///
/// For `struct Point { x: i32, y: i32 }`, generates:
/// ```js
/// function Point(x, y) { this.x = x; this.y = y; }
/// ```
fn lower_struct_constructor(name: &str, fields: &[StructField]) -> JsStmt {
    let param_names: Vec<String> = fields.iter().map(|f| f.name.name.clone()).collect();

    // Generate: this.field = field; for each field
    let mut body_stmts = Vec::new();
    for field in fields {
        let field_name = field.name.name.clone();
        body_stmts.push(JsStmt::Expr(JsExpr::Assignment {
            left: Box::new(JsExpr::Member {
                object: Box::new(JsExpr::Ident("this".to_string())),
                property: field_name.clone(),
            }),
            op: JsAssignOp::Assign,
            right: Box::new(JsExpr::Ident(field_name)),
        }));
    }

    JsStmt::Function {
        name: name.to_string(),
        params: param_names,
        body: body_stmts,
        source_span: None,
    }
}

/// Extract a JavaScript-usable type name from a TypeExpr.
/// Convert a TypeExpr to a JavaScript-compatible type name string.
///
/// This function produces unambiguous type names for use in codegen:
/// - Generic types use only their base name (not parameters), e.g., `Map<K, V>` -> `"Map"`
/// - Arrays format as `T[]`, e.g., `[String]` -> `"String[]"`
/// - Tuples format as `[T1, T2]`, e.g., `(i32, String)` -> `"[i32, String]"`
/// - Function types format as `((params) => ret)`
/// - ImplTrait uses the underlying trait type name
///
/// This formatting contract is relied upon by codegen for type-based dispatch
/// (e.g., checking if a type starts with "Range" or "Map"). Changes to this
/// function may break type string matching in codegen.
fn type_expr_to_js_name(ty: &TypeExpr) -> String {
    match &ty.kind {
        TypeExprKind::Named(ident) => ident.name.clone(),
        TypeExprKind::Generic { name, .. } => name.name.clone(),
        TypeExprKind::Function { params, ret } => {
            // Generate a TypeScript-style function type name for documentation
            let param_names: Vec<String> = params.iter().map(type_expr_to_js_name).collect();
            format!(
                "(({}) => {})",
                param_names.join(", "),
                type_expr_to_js_name(ret)
            )
        }
        TypeExprKind::Array(elem) => {
            format!("{}[]", type_expr_to_js_name(elem))
        }
        TypeExprKind::Tuple(types) => {
            // TypeScript tuple type: [T1, T2, T3]
            let type_names: Vec<String> = types.iter().map(type_expr_to_js_name).collect();
            format!("[{}]", type_names.join(", "))
        }
        TypeExprKind::ImplTrait { trait_ty } => {
            // For impl Trait, use the underlying trait type name
            type_expr_to_js_name(trait_ty)
        }
    }
}

/// Lower an impl method to JavaScript.
///
/// For methods with a receiver (`&self`, `self`, etc.), we generate:
///   `TypeName.prototype.methodName = function(params) { body };`
///
/// For static methods (no receiver), we generate:
///   `TypeName.methodName = function(params) { body };`
fn lower_impl_method(
    type_name: &str,
    method: &husk_ast::ImplMethod,
    span: &Span,
    source: Option<&str>,
    ctx: &CodegenContext,
) -> JsStmt {
    let method_name = &method.name.name;

    // Build params: for methods with receiver, we don't include `self` in params
    // since it becomes `this` in JavaScript
    let js_params: Vec<String> = method.params.iter().map(|p| p.name.name.clone()).collect();

    // Lower the body
    let mut js_body = Vec::new();
    for (i, stmt) in method.body.iter().enumerate() {
        let is_last = i + 1 == method.body.len();
        if is_last {
            js_body.push(lower_tail_stmt(stmt, ctx));
        } else {
            js_body.push(lower_stmt(stmt, ctx));
        }
    }

    // If the method uses ? operator, wrap body in try-catch to handle early returns
    let final_body = if contains_try_expr(&method.body) {
        // Catch block: if (e && e.__husk_early_return !== undefined) return e.__husk_early_return; throw e;
        let catch_block = vec![
            JsStmt::If {
                cond: JsExpr::Binary {
                    op: JsBinaryOp::And,
                    left: Box::new(JsExpr::Ident("__e".to_string())),
                    right: Box::new(JsExpr::Binary {
                        op: JsBinaryOp::StrictNe,
                        left: Box::new(JsExpr::Member {
                            object: Box::new(JsExpr::Ident("__e".to_string())),
                            property: "__husk_early_return".to_string(),
                        }),
                        right: Box::new(JsExpr::Ident("undefined".to_string())),
                    }),
                },
                then_block: vec![JsStmt::Return(JsExpr::Member {
                    object: Box::new(JsExpr::Ident("__e".to_string())),
                    property: "__husk_early_return".to_string(),
                })],
                else_block: None,
            },
            JsStmt::Throw(JsExpr::Ident("__e".to_string())),
        ];
        vec![JsStmt::TryCatch {
            try_block: js_body,
            catch_ident: "__e".to_string(),
            catch_block,
        }]
    } else {
        js_body
    };

    // Convert byte offset to line/column if source is provided
    let _source_span = source.map(|src| {
        let (line, column) = offset_to_line_col(src, span.range.start);
        SourceSpan { line, column }
    });

    // Generate: TypeName.prototype.methodName = function(params) { body };
    // or for static: TypeName.methodName = function(params) { body };
    let target = if method.receiver.is_some() {
        // Instance method: TypeName.prototype.methodName
        JsExpr::Member {
            object: Box::new(JsExpr::Member {
                object: Box::new(JsExpr::Ident(type_name.to_string())),
                property: "prototype".to_string(),
            }),
            property: method_name.clone(),
        }
    } else {
        // Static method: TypeName.methodName
        JsExpr::Member {
            object: Box::new(JsExpr::Ident(type_name.to_string())),
            property: method_name.clone(),
        }
    };

    // Create the function expression
    let func_expr = JsExpr::Function {
        params: js_params,
        body: final_body,
    };

    // Generate assignment: target = function(...) { ... }
    JsStmt::Expr(JsExpr::Assignment {
        left: Box::new(target),
        op: JsAssignOp::Assign,
        right: Box::new(func_expr),
    })
}

fn lower_tail_stmt(stmt: &Stmt, ctx: &CodegenContext) -> JsStmt {
    match &stmt.kind {
        // Treat a trailing expression statement as an implicit `return expr;`,
        // to mirror Rust-style expression-bodied functions.
        StmtKind::Expr(expr) => JsStmt::Return(lower_expr(expr, ctx)),
        // For if statements in tail position, wrap branch results in returns.
        StmtKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let js_cond = lower_expr(cond, ctx);
            let then_block: Vec<JsStmt> = then_branch
                .stmts
                .iter()
                .enumerate()
                .map(|(i, s)| {
                    if i + 1 == then_branch.stmts.len() {
                        lower_tail_stmt(s, ctx)
                    } else {
                        lower_stmt(s, ctx)
                    }
                })
                .collect();
            let else_block = else_branch.as_ref().map(|else_stmt| {
                // else_branch is a Box<Stmt>, which may be another If or a Block
                match &else_stmt.kind {
                    StmtKind::Block(block) => block
                        .stmts
                        .iter()
                        .enumerate()
                        .map(|(i, s)| {
                            if i + 1 == block.stmts.len() {
                                lower_tail_stmt(s, ctx)
                            } else {
                                lower_stmt(s, ctx)
                            }
                        })
                        .collect(),
                    StmtKind::If { .. } => vec![lower_tail_stmt(else_stmt, ctx)],
                    _ => vec![lower_tail_stmt(else_stmt, ctx)],
                }
            });
            JsStmt::If {
                cond: js_cond,
                then_block,
                else_block,
            }
        }
        _ => lower_stmt(stmt, ctx),
    }
}

fn lower_extern_fn(
    name: &husk_ast::Ident,
    params: &[Param],
    ret_type: Option<&TypeExpr>,
    js_name: Option<&str>,
    module_call: Option<&str>,
    namespace: Option<&str>,
) -> JsStmt {
    let js_params: Vec<String> = params.iter().map(|p| p.name.name.clone()).collect();
    // Use js_name for the JS call, but name.name for the Husk function name
    let call_name = js_name.unwrap_or(&name.name);
    let body = lower_extern_body(call_name, &js_params, ret_type, module_call, namespace);
    JsStmt::Function {
        name: name.name.clone(),
        params: js_params,
        body,
        source_span: None,
    }
}

fn is_result_type(ret_type: &TypeExpr) -> bool {
    match &ret_type.kind {
        TypeExprKind::Generic { name, args } => name.name == "Result" && args.len() == 2,
        _ => false,
    }
}

/// Check if a return type indicates a constructor (returns a struct, not JsValue/primitive).
fn is_constructor_return_type(ret_type: Option<&TypeExpr>) -> bool {
    match ret_type {
        None => false,
        Some(ty) => match &ty.kind {
            // Named types that are NOT primitives or JsValue are likely structs
            TypeExprKind::Named(ident) => {
                let name = &ident.name;
                // Primitives and JS interop types don't need `new`
                !matches!(
                    name.as_str(),
                    "JsValue"
                        | "String"
                        | "bool"
                        | "f64"
                        | "i64"
                        | "i32"
                        | "u32"
                        | "u64"
                        | "()"
                        | "JsPromise"
                        | "JsArray"
                        | "JsObject"
                )
            }
            // Generic types like JsPromise<T>, JsArray<T>, Option<T> don't need `new`
            TypeExprKind::Generic { name, .. } => !matches!(
                name.name.as_str(),
                "JsPromise" | "JsArray" | "Option" | "Result"
            ),
            _ => false,
        },
    }
}

fn lower_extern_body(
    name: &str,
    param_names: &[String],
    ret_type: Option<&TypeExpr>,
    module_call: Option<&str>,
    namespace: Option<&str>,
) -> Vec<JsStmt> {
    let args: Vec<JsExpr> = param_names.iter().cloned().map(JsExpr::Ident).collect();

    // Determine the call expression based on attributes:
    // 1. module_call - call the module binding directly
    //    - If return type is a struct, use `new` (constructor)
    //    - Otherwise, just call (function)
    // 2. namespace - access through namespace member (namespace.name)
    // 3. Neither - use globalThis.name for standard extern functions
    let call = if let Some(module_binding) = module_call {
        // For module_call, check if we need `new` based on return type
        if is_constructor_return_type(ret_type) {
            JsExpr::New {
                constructor: module_binding.to_string(),
                args,
            }
        } else {
            JsExpr::Call {
                callee: Box::new(JsExpr::Ident(module_binding.to_string())),
                args,
            }
        }
    } else if let Some(ns) = namespace {
        JsExpr::Call {
            callee: Box::new(JsExpr::Member {
                object: Box::new(JsExpr::Ident(ns.to_string())),
                property: name.to_string(),
            }),
            args,
        }
    } else {
        JsExpr::Call {
            callee: Box::new(JsExpr::Member {
                object: Box::new(JsExpr::Ident("globalThis".to_string())),
                property: name.to_string(),
            }),
            args,
        }
    };

    if let Some(ret_ty) = ret_type {
        if is_result_type(ret_ty) {
            // Wrap in Ok/Err with try/catch.
            let ok_call = JsExpr::Call {
                callee: Box::new(JsExpr::Ident("Ok".to_string())),
                args: vec![call],
            };
            let try_block = vec![JsStmt::Return(ok_call)];

            let err_call = JsExpr::Call {
                callee: Box::new(JsExpr::Ident("Err".to_string())),
                args: vec![JsExpr::Ident("e".to_string())],
            };
            let catch_block = vec![JsStmt::Return(err_call)];

            vec![JsStmt::TryCatch {
                try_block,
                catch_ident: "e".to_string(),
                catch_block,
            }]
        } else {
            vec![JsStmt::Return(call)]
        }
    } else {
        vec![JsStmt::Expr(call)]
    }
}

fn lower_stmt(stmt: &Stmt, ctx: &CodegenContext) -> JsStmt {
    match &stmt.kind {
        StmtKind::Let {
            mutable: _,
            pattern,
            ty: _,
            value,
            else_block,
        } => lower_let_pattern(pattern, value.as_ref(), else_block.as_ref(), ctx),
        StmtKind::Expr(expr) | StmtKind::Semi(expr) => {
            // Special case: match expressions in statement position should be lowered
            // to if/else statements, not ternary expressions. This allows break/continue
            // to work properly in match arms.
            if let ExprKind::Match { scrutinee, arms } = &expr.kind {
                return lower_match_stmt(scrutinee, arms, ctx);
            }
            JsStmt::Expr(lower_expr(expr, ctx))
        }
        StmtKind::Return { value } => {
            let expr = value
                .as_ref()
                .map(|e| lower_expr(e, ctx))
                // Represent `return;` as `return undefined;` for now.
                .unwrap_or_else(|| JsExpr::Ident("undefined".to_string()));
            JsStmt::Return(expr)
        }
        StmtKind::Block(block) => {
            // Flatten block into a single function body-like statement sequence.
            // For now we represent it as an expression statement of the last
            // expression, discarding internal structure. This is a placeholder
            // until we add proper JS block statements.
            if let Some(last) = block.stmts.last() {
                lower_stmt(last, ctx)
            } else {
                JsStmt::Expr(JsExpr::Ident("undefined".to_string()))
            }
        }
        StmtKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let js_cond = lower_expr(cond, ctx);
            let then_block: Vec<JsStmt> = then_branch
                .stmts
                .iter()
                .map(|s| lower_stmt(s, ctx))
                .collect();
            let else_block = else_branch.as_ref().map(|else_stmt| {
                // else_branch is a Box<Stmt>, which may be another If or a Block
                match &else_stmt.kind {
                    StmtKind::Block(block) => {
                        block.stmts.iter().map(|s| lower_stmt(s, ctx)).collect()
                    }
                    StmtKind::If { .. } => vec![lower_stmt(else_stmt, ctx)],
                    _ => vec![lower_stmt(else_stmt, ctx)],
                }
            });
            JsStmt::If {
                cond: js_cond,
                then_block,
                else_block,
            }
        }
        StmtKind::ForIn {
            binding,
            iterable,
            body,
        } => {
            // Check if iterable is a range expression with both bounds
            if let ExprKind::Range {
                start: Some(start),
                end: Some(end),
                inclusive,
            } = &iterable.kind
            {
                let js_start = lower_expr(start, ctx);
                let js_end = lower_expr(end, ctx);
                let js_body: Vec<JsStmt> = body.stmts.iter().map(|s| lower_stmt(s, ctx)).collect();

                JsStmt::For {
                    binding: ctx.resolve_name(&binding.name, &binding.span),
                    start: js_start,
                    end: js_end,
                    inclusive: *inclusive,
                    body: js_body,
                }
            } else {
                // Check if iterable is a Range type variable (from semantic analysis)
                // Range objects are {start, end} and need to be iterated with a traditional for loop
                let iterable_type = ctx
                    .type_resolution
                    .get(&(iterable.span.range.start, iterable.span.range.end));

                if let Some(ty) = iterable_type {
                    if ty.starts_with("Range") {
                        // Generate: for (let binding = iterable.start; binding < iterable.end; binding++)
                        let js_iterable = lower_expr(iterable, ctx);
                        let js_body: Vec<JsStmt> =
                            body.stmts.iter().map(|s| lower_stmt(s, ctx)).collect();

                        return JsStmt::ForRange {
                            binding: ctx.resolve_name(&binding.name, &binding.span),
                            range_expr: js_iterable,
                            body: js_body,
                        };
                    }
                }

                // Check if iterable needs into_iter() call (for collections that implement IntoIterator)
                let iterable_type = ctx
                    .type_resolution
                    .get(&(iterable.span.range.start, iterable.span.range.end));

                let js_iterable = if let Some(ty) = iterable_type {
                    // If it's a collection type (array, string, range), call into_iter()
                    // Note: Iterator types don't need into_iter() - they're already iterators
                    let js_base = lower_expr(iterable, ctx);
                    if ty.starts_with("[") {
                        // Array: use __husk_array_into_iter
                        JsExpr::Call {
                            callee: Box::new(JsExpr::Ident("__husk_array_into_iter".to_string())),
                            args: vec![js_base],
                        }
                    } else if ty == "String" {
                        // String: use __husk_string_into_iter
                        JsExpr::Call {
                            callee: Box::new(JsExpr::Ident("__husk_string_into_iter".to_string())),
                            args: vec![js_base],
                        }
                    } else if ty.starts_with("Range") {
                        // Range: use __husk_range_into_iter
                        JsExpr::Call {
                            callee: Box::new(JsExpr::Ident("__husk_range_into_iter".to_string())),
                            args: vec![js_base],
                        }
                    } else {
                        // Already an iterator or other iterable - use as-is
                        js_base
                    }
                } else {
                    // No type info - use as-is (will work if it's already iterable)
                    lower_expr(iterable, ctx)
                };

                let js_body: Vec<JsStmt> = body.stmts.iter().map(|s| lower_stmt(s, ctx)).collect();
                JsStmt::ForOf {
                    binding: ctx.resolve_name(&binding.name, &binding.span),
                    iterable: js_iterable,
                    body: js_body,
                }
            }
        }
        StmtKind::While { cond, body } => {
            let js_cond = lower_expr(cond, ctx);
            let js_body: Vec<JsStmt> = body.stmts.iter().map(|s| lower_stmt(s, ctx)).collect();
            JsStmt::While {
                cond: js_cond,
                body: js_body,
            }
        }
        StmtKind::Loop { body } => {
            // loop { ... } becomes while(true) { ... }
            let js_body: Vec<JsStmt> = body.stmts.iter().map(|s| lower_stmt(s, ctx)).collect();
            JsStmt::While {
                cond: JsExpr::Bool(true),
                body: js_body,
            }
        }
        StmtKind::Break => JsStmt::Break,
        StmtKind::Continue => JsStmt::Continue,
        StmtKind::Assign { target, op, value } => {
            let js_op = match op {
                husk_ast::AssignOp::Assign => JsAssignOp::Assign,
                husk_ast::AssignOp::AddAssign => JsAssignOp::AddAssign,
                husk_ast::AssignOp::SubAssign => JsAssignOp::SubAssign,
                husk_ast::AssignOp::ModAssign => JsAssignOp::ModAssign,
            };
            JsStmt::Assign {
                target: lower_expr(target, ctx),
                op: js_op,
                value: lower_expr(value, ctx),
            }
        }
        StmtKind::IfLet {
            pattern,
            scrutinee,
            then_branch,
            else_branch,
        } => lower_if_let_stmt(pattern, scrutinee, then_branch, else_branch, ctx),
    }
}

/// Convert a Husk pattern to a JS destructure pattern (supports nesting).
fn pattern_to_destructure(pattern: &Pattern, ctx: &CodegenContext) -> DestructurePattern {
    match &pattern.kind {
        PatternKind::Binding(name) => {
            DestructurePattern::Binding(ctx.resolve_name(&name.name, &name.span))
        }
        PatternKind::Wildcard => DestructurePattern::Wildcard,
        PatternKind::Tuple { fields } => {
            let elements = fields
                .iter()
                .map(|p| pattern_to_destructure(p, ctx))
                .collect();
            DestructurePattern::Array(elements)
        }
        // For other patterns (enum patterns, etc.), treat as wildcard
        _ => DestructurePattern::Wildcard,
    }
}

/// Lower a let statement with a pattern (handles both simple identifiers and tuple patterns).
fn lower_let_pattern(
    pattern: &Pattern,
    value: Option<&Expr>,
    else_block: Option<&Block>,
    ctx: &CodegenContext,
) -> JsStmt {
    match &pattern.kind {
        PatternKind::Binding(name) => {
            // Check if this is an imported variant (e.g., `let None = opt else { }`)
            let span_key = (pattern.span.range.start, pattern.span.range.end);
            if let Some((_, variant_name)) = ctx.variant_patterns.get(&span_key) {
                // This is a variant pattern - needs tag check
                if let Some(blk) = else_block {
                    return lower_let_refutable_binding(name, variant_name, value, blk, ctx);
                }
            }
            // Simple identifier: let x = expr;
            JsStmt::Let {
                name: ctx.resolve_name(&name.name, &name.span),
                init: value.map(|e| lower_expr(e, ctx)),
            }
        }
        PatternKind::Tuple { fields } => {
            // Tuple pattern: let (a, b, c) = expr; or let ((a, b), c) = expr;
            // Convert to JS array destructuring with nested support
            let elements: Vec<DestructurePattern> = fields
                .iter()
                .map(|p| pattern_to_destructure(p, ctx))
                .collect();

            JsStmt::LetDestructure {
                pattern: elements,
                init: value.map(|e| lower_expr(e, ctx)),
            }
        }
        PatternKind::Wildcard => {
            // let _ = expr; - just evaluate the expression for side effects
            if let Some(e) = value {
                JsStmt::Expr(lower_expr(e, ctx))
            } else {
                JsStmt::Expr(JsExpr::Ident("undefined".to_string()))
            }
        }
        PatternKind::EnumUnit { path } | PatternKind::EnumTuple { path, .. } => {
            // Refutable pattern: needs else block
            if let Some(blk) = else_block {
                lower_let_refutable(pattern, path, value, blk, ctx)
            } else {
                // Graceful fallback - semantic should catch this
                emit_unreachable_error()
            }
        }
        PatternKind::EnumStruct { path, fields } => {
            // Refutable struct pattern: needs else block
            if let Some(blk) = else_block {
                lower_let_refutable_struct(path, fields, value, blk, ctx)
            } else {
                emit_unreachable_error()
            }
        }
    }
}

/// Emit a visible error marker when codegen encounters an unreachable state.
fn emit_unreachable_error() -> JsStmt {
    JsStmt::Expr(JsExpr::Raw(
        "/* SEMANTIC ERROR: unreachable - missing else block */ undefined".to_string(),
    ))
}

/// Lower a refutable binding pattern (imported variant like `None`, `Err`)
fn lower_let_refutable_binding(
    _name: &Ident,
    variant_name: &str,
    value: Option<&Expr>,
    else_block: &Block,
    ctx: &CodegenContext,
) -> JsStmt {
    let expr = match value {
        Some(e) => e,
        None => {
            // Should not happen - semantic should catch
            return JsStmt::Expr(JsExpr::Ident("undefined".to_string()));
        }
    };
    let value_js = lower_expr(expr, ctx);

    let temp_name = ctx.fresh_temp("__letelse");
    let temp_expr = JsExpr::Ident(temp_name.clone());

    // Build tag check: if (__tmp.tag !== "VariantName") { else_block }
    let condition = JsExpr::Binary {
        op: JsBinaryOp::StrictNe,
        left: Box::new(JsExpr::Member {
            object: Box::new(temp_expr.clone()),
            property: "tag".to_string(),
        }),
        right: Box::new(JsExpr::String(variant_name.to_string())),
    };

    let else_stmts: Vec<JsStmt> = else_block
        .stmts
        .iter()
        .map(|s| lower_stmt(s, ctx))
        .collect();

    // Unit variants have no bindings - just tag check
    // Use Sequence to emit statements at same level (no block scope)
    JsStmt::Sequence(vec![
        JsStmt::Let {
            name: temp_name,
            init: Some(value_js),
        },
        JsStmt::If {
            cond: condition,
            then_block: else_stmts,
            else_block: None,
        },
    ])
}

/// Lower a refutable pattern (EnumUnit or EnumTuple)
fn lower_let_refutable(
    pattern: &Pattern,
    path: &[Ident],
    value: Option<&Expr>,
    else_block: &Block,
    ctx: &CodegenContext,
) -> JsStmt {
    let expr = match value {
        Some(e) => e,
        None => {
            return JsStmt::Expr(JsExpr::Ident("undefined".to_string()));
        }
    };
    let value_js = lower_expr(expr, ctx);

    let temp_name = ctx.fresh_temp("__letelse");
    let temp_expr = JsExpr::Ident(temp_name.clone());

    // Build tag check
    let variant = path.last().map(|id| id.name.clone()).unwrap_or_default();
    let condition = JsExpr::Binary {
        op: JsBinaryOp::StrictNe,
        left: Box::new(JsExpr::Member {
            object: Box::new(temp_expr.clone()),
            property: "tag".to_string(),
        }),
        right: Box::new(JsExpr::String(variant)),
    };

    let else_stmts: Vec<JsStmt> = else_block
        .stmts
        .iter()
        .map(|s| lower_stmt(s, ctx))
        .collect();

    // Extract bindings from pattern
    let bindings = extract_refutable_pattern_bindings(pattern, &temp_expr, ctx);

    let mut stmts = vec![
        JsStmt::Let {
            name: temp_name,
            init: Some(value_js),
        },
        JsStmt::If {
            cond: condition,
            then_block: else_stmts,
            else_block: None,
        },
    ];
    stmts.extend(bindings.into_iter().map(|(name, accessor)| JsStmt::Let {
        name,
        init: Some(accessor),
    }));

    // Use Sequence to emit statements at same level (no block scope)
    JsStmt::Sequence(stmts)
}

/// Lower a refutable struct pattern (EnumStruct)
fn lower_let_refutable_struct(
    path: &[Ident],
    fields: &[(Ident, Pattern)],
    value: Option<&Expr>,
    else_block: &Block,
    ctx: &CodegenContext,
) -> JsStmt {
    let expr = match value {
        Some(e) => e,
        None => {
            return JsStmt::Expr(JsExpr::Ident("undefined".to_string()));
        }
    };
    let value_js = lower_expr(expr, ctx);

    let temp_name = ctx.fresh_temp("__letelse");
    let temp_expr = JsExpr::Ident(temp_name.clone());

    // Build tag check
    let variant = path.last().map(|id| id.name.clone()).unwrap_or_default();
    let condition = JsExpr::Binary {
        op: JsBinaryOp::StrictNe,
        left: Box::new(JsExpr::Member {
            object: Box::new(temp_expr.clone()),
            property: "tag".to_string(),
        }),
        right: Box::new(JsExpr::String(variant)),
    };

    let else_stmts: Vec<JsStmt> = else_block
        .stmts
        .iter()
        .map(|s| lower_stmt(s, ctx))
        .collect();

    // Extract struct field bindings
    let bindings = extract_struct_field_bindings(fields, &temp_expr, ctx);

    let mut stmts = vec![
        JsStmt::Let {
            name: temp_name,
            init: Some(value_js),
        },
        JsStmt::If {
            cond: condition,
            then_block: else_stmts,
            else_block: None,
        },
    ];
    stmts.extend(bindings.into_iter().map(|(name, accessor)| JsStmt::Let {
        name,
        init: Some(accessor),
    }));

    // Use Sequence to emit statements at same level (no block scope)
    JsStmt::Sequence(stmts)
}

/// Extract bindings from struct enum pattern fields.
fn extract_struct_field_bindings(
    fields: &[(Ident, Pattern)],
    scrutinee_js: &JsExpr,
    ctx: &CodegenContext,
) -> Vec<(String, JsExpr)> {
    let mut bindings = Vec::new();
    for (field_name, sub_pattern) in fields {
        // Access the field from scrutinee.value.field_name
        let field_accessor = JsExpr::Member {
            object: Box::new(JsExpr::Member {
                object: Box::new(scrutinee_js.clone()),
                property: "value".to_string(),
            }),
            property: field_name.name.clone(),
        };

        match &sub_pattern.kind {
            PatternKind::Binding(ident) => {
                bindings.push((ctx.resolve_name(&ident.name, &ident.span), field_accessor));
            }
            PatternKind::Wildcard => {
                // Wildcard - no binding needed
            }
            _ => {
                // Nested patterns not supported - semantic should have caught this
            }
        }
    }
    bindings
}

/// Extract bindings from a refutable pattern for if-let or let-else.
/// Uses CodegenContext for proper name resolution.
fn extract_refutable_pattern_bindings(
    pattern: &Pattern,
    scrutinee_js: &JsExpr,
    ctx: &CodegenContext,
) -> Vec<(String, JsExpr)> {
    match &pattern.kind {
        PatternKind::EnumTuple { fields, .. } => {
            // Access via __temp.value for single field, __temp["0"], __temp["1"] for multiple
            if fields.len() == 1 {
                if let PatternKind::Binding(ident) = &fields[0].kind {
                    let accessor = JsExpr::Member {
                        object: Box::new(scrutinee_js.clone()),
                        property: "value".to_string(),
                    };
                    return vec![(ctx.resolve_name(&ident.name, &ident.span), accessor)];
                }
            } else {
                // Multiple fields: access via ["0"], ["1"], etc. (no .value wrapper)
                let mut bindings = Vec::new();
                for (i, field) in fields.iter().enumerate() {
                    if let PatternKind::Binding(ident) = &field.kind {
                        let accessor = JsExpr::Index {
                            object: Box::new(scrutinee_js.clone()),
                            index: Box::new(JsExpr::String(i.to_string())),
                        };
                        bindings.push((ctx.resolve_name(&ident.name, &ident.span), accessor));
                    }
                }
                return bindings;
            }
            Vec::new()
        }
        PatternKind::EnumStruct { fields, .. } => {
            extract_struct_field_bindings(fields, scrutinee_js, ctx)
        }
        _ => Vec::new(),
    }
}

/// Lower an if-let statement
fn lower_if_let_stmt(
    pattern: &Pattern,
    scrutinee: &Expr,
    then_branch: &Block,
    else_branch: &Option<Box<Stmt>>,
    ctx: &CodegenContext,
) -> JsStmt {
    let scrutinee_js = lower_expr(scrutinee, ctx);
    let temp_name = ctx.fresh_temp("__iflet");
    let temp_expr = JsExpr::Ident(temp_name.clone());

    // Build condition test
    let condition = pattern_test_for_if_let(pattern, &temp_expr, ctx).unwrap_or(JsExpr::Bool(true));

    // Extract bindings and build then block
    let bindings = extract_refutable_pattern_bindings(pattern, &temp_expr, ctx);
    let mut then_stmts: Vec<JsStmt> = bindings
        .into_iter()
        .map(|(name, accessor)| JsStmt::Let {
            name,
            init: Some(accessor),
        })
        .collect();
    then_stmts.extend(then_branch.stmts.iter().map(|s| lower_stmt(s, ctx)));

    let else_block = else_branch.as_ref().map(|else_stmt| match &else_stmt.kind {
        StmtKind::Block(block) => block.stmts.iter().map(|s| lower_stmt(s, ctx)).collect(),
        StmtKind::If { .. } | StmtKind::IfLet { .. } => vec![lower_stmt(else_stmt, ctx)],
        _ => vec![lower_stmt(else_stmt, ctx)],
    });

    JsStmt::Block(vec![
        JsStmt::Let {
            name: temp_name,
            init: Some(scrutinee_js),
        },
        JsStmt::If {
            cond: condition,
            then_block: then_stmts,
            else_block,
        },
    ])
}

/// Pattern test for if-let - generates the condition to check if pattern matches
fn pattern_test_for_if_let(
    pattern: &Pattern,
    scrutinee_js: &JsExpr,
    ctx: &CodegenContext,
) -> Option<JsExpr> {
    match &pattern.kind {
        PatternKind::EnumUnit { path }
        | PatternKind::EnumTuple { path, .. }
        | PatternKind::EnumStruct { path, .. } => {
            let variant = path.last().map(|id| id.name.clone()).unwrap_or_default();
            Some(JsExpr::Binary {
                op: JsBinaryOp::StrictEq,
                left: Box::new(JsExpr::Member {
                    object: Box::new(scrutinee_js.clone()),
                    property: "tag".to_string(),
                }),
                right: Box::new(JsExpr::String(variant)),
            })
        }
        PatternKind::Binding(_) => {
            // Check variant_patterns for imported unit variants
            let span_key = (pattern.span.range.start, pattern.span.range.end);
            if let Some((_, variant_name)) = ctx.variant_patterns.get(&span_key) {
                Some(JsExpr::Binary {
                    op: JsBinaryOp::StrictEq,
                    left: Box::new(JsExpr::Member {
                        object: Box::new(scrutinee_js.clone()),
                        property: "tag".to_string(),
                    }),
                    right: Box::new(JsExpr::String(variant_name.clone())),
                })
            } else {
                None // True binding - catch-all
            }
        }
        _ => None,
    }
}

/// Strip numeric suffix from method names for variadic function emulation.
/// E.g., "run1" -> "run", "run2" -> "run", "run" -> "run"
/// This allows Husk to declare `fn run1(self, p1: T)`, `fn run2(self, p1: T, p2: U)`
/// but emit `.run(p1)`, `.run(p1, p2)` in JavaScript for variadic JS functions.
fn strip_variadic_suffix(method_name: &str) -> String {
    // Check if the method ends with one or more digits
    let last_non_digit = method_name
        .char_indices()
        .rev()
        .find(|(_, c)| !c.is_ascii_digit());

    match last_non_digit {
        Some((idx, _)) if idx < method_name.len() - 1 => {
            // Has trailing digits - strip them
            method_name[..=idx].to_string()
        }
        _ => {
            // No trailing digits or all digits - return as-is
            method_name.to_string()
        }
    }
}

/// Generate JavaScript code for type conversion methods (.into(), .parse(), .try_into()).
///
/// Based on the target type, generates the appropriate JavaScript conversion:
/// - `.into()` for infallible conversions (From trait)
/// - `.parse()` for parsing strings (TryFrom<String>)
/// - `.try_into()` for fallible conversions (TryFrom trait)
fn lower_conversion_method(
    receiver: &Expr,
    method_name: &str,
    target_type: &str,
    ctx: &CodegenContext,
) -> JsExpr {
    let receiver_js = lower_expr(receiver, ctx);

    match method_name {
        "into" => {
            // Generate infallible conversion based on target type
            match target_type {
                "String" => {
                    // value.into() -> String(value)
                    // Use String() constructor to avoid issues with number literals like `42.toString()`
                    JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("String".to_string())),
                        args: vec![receiver_js],
                    }
                }
                "i64" => {
                    // value.into() -> BigInt(value)
                    JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("BigInt".to_string())),
                        args: vec![receiver_js],
                    }
                }
                "f64" => {
                    // value.into() -> Number(value)
                    JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("Number".to_string())),
                        args: vec![receiver_js],
                    }
                }
                _ => {
                    // For other types, just return the value (identity conversion or user-defined)
                    receiver_js
                }
            }
        }
        "parse" => {
            // Generate parsing conversion based on target type
            // Returns Result<TargetType, String>
            let parse_call = match target_type {
                "i32" => {
                    // "str".parse::<i32>() -> __husk_parse_i32(str)
                    JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_parse_i32".to_string())),
                        args: vec![receiver_js],
                    }
                }
                "i64" => {
                    // "str".parse::<i64>() -> __husk_parse_i64(str)
                    JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_parse_i64".to_string())),
                        args: vec![receiver_js],
                    }
                }
                "f64" => {
                    // "str".parse::<f64>() -> __husk_parse_f64(str)
                    JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_parse_f64".to_string())),
                        args: vec![receiver_js],
                    }
                }
                _ => {
                    // Unsupported parse target - return error Result
                    JsExpr::Object(vec![
                        ("tag".to_string(), JsExpr::String("Err".to_string())),
                        (
                            "value".to_string(),
                            JsExpr::String(format!("cannot parse to {}", target_type)),
                        ),
                    ])
                }
            };
            parse_call
        }
        "try_into" => {
            // Generate fallible conversion based on target type
            // Returns Result<TargetType, String>
            match target_type {
                "i32" => {
                    // value.try_into::<i32>() -> __husk_try_into_i32(value)
                    JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_try_into_i32".to_string())),
                        args: vec![receiver_js],
                    }
                }
                _ => {
                    // For other types, wrap in Ok
                    JsExpr::Object(vec![
                        ("tag".to_string(), JsExpr::String("Ok".to_string())),
                        ("value".to_string(), receiver_js),
                    ])
                }
            }
        }
        _ => receiver_js,
    }
}

fn lower_expr(expr: &Expr, ctx: &CodegenContext) -> JsExpr {
    match &expr.kind {
        ExprKind::Literal(lit) => match &lit.kind {
            LiteralKind::Int(n) => JsExpr::Number(*n),
            LiteralKind::Float(f) => JsExpr::Float(*f),
            LiteralKind::Bool(b) => JsExpr::Bool(*b),
            LiteralKind::String(s) => JsExpr::String(s.clone()),
        },
        ExprKind::Ident(id) => {
            // Translate `self` to `this` for JavaScript method bodies
            if id.name == "self" {
                JsExpr::Ident("this".to_string())
            } else if let Some((_enum_name, variant_name)) = ctx
                .variant_calls
                .get(&(expr.span.range.start, expr.span.range.end))
            {
                // Imported unit variant (e.g., `None` from `use Option::*`)
                JsExpr::Object(vec![(
                    "tag".to_string(),
                    JsExpr::String(variant_name.clone()),
                )])
            } else {
                // Use resolved name from name_resolution if available
                JsExpr::Ident(ctx.resolve_name(&id.name, &id.span))
            }
        }
        ExprKind::Path { segments } => {
            // MVP: treat `Enum::Variant` as a tagged union value `{ tag: "Variant" }`.
            // We ignore the enum name and any intermediate segments here.
            let variant = segments
                .last()
                .map(|id| id.name.clone())
                .unwrap_or_else(|| "Unknown".to_string());
            JsExpr::Object(vec![("tag".to_string(), JsExpr::String(variant))])
        }
        ExprKind::Field { base, member } => {
            let field_name = &member.name;
            // Check if this field is an extern property (look up by field name across all types)
            let js_property = ctx
                .accessors
                .getters
                .iter()
                .find(|((_, prop_name), _)| prop_name == field_name)
                .map(|(_, js_name)| js_name.clone())
                .unwrap_or_else(|| field_name.clone());

            JsExpr::Member {
                object: Box::new(lower_expr(base, ctx)),
                property: js_property,
            }
        }
        ExprKind::MethodCall {
            receiver,
            method,
            type_args: _, // Turbofish types are used for semantic analysis, not codegen
            args,
        } => {
            // NOTE: Method dispatch relies on string-based type matching from type_resolution
            // (e.g., "String", "Map<...>", "[T]", "Range"). This is acceptable for the current
            // implementation but is brittle if type formatting changes (modules, aliases, etc.).
            // Long-term, consider using structured type tags (canonical type IDs or enum for
            // known builtins) in TypeResolution for more robust dispatch.
            //
            // Try to determine receiver type for getter/setter lookup.
            // For now, we use a heuristic: look up by method name across all types.
            // A more robust solution would require type information from semantic analysis.
            let method_name = &method.name;

            // Check if this is a getter (no args, matches getter pattern)
            if args.is_empty() {
                // Try type-specific getter lookup using semantic analysis
                let receiver_type_from_semantic = ctx
                    .type_resolution
                    .get(&(receiver.span.range.start, receiver.span.range.end));

                if let Some(recv_type) = receiver_type_from_semantic {
                    // Extract base type name (e.g., "Map<String, i32>" -> "Map")
                    // For arrays like "[String]", normalize to "T[]" which is how they're registered
                    let (base_type, lookup_type) = if recv_type.starts_with('[') {
                        // Array type - normalize to T[] for getter lookup
                        (recv_type.as_str(), "T[]".to_string())
                    } else if let Some(idx) = recv_type.find('<') {
                        let base = &recv_type[..idx];
                        (base, base.to_string())
                    } else {
                        (recv_type.as_str(), recv_type.clone())
                    };

                    // Only apply getter lookup for known stdlib types
                    // User-defined types should not have their methods converted to property access
                    let is_stdlib_type =
                        matches!(base_type, "String" | "Map" | "Set" | "Range" | "Array")
                            || recv_type.starts_with("[");

                    if is_stdlib_type {
                        if let Some(prop) = ctx
                            .accessors
                            .getters
                            .get(&(lookup_type, method_name.clone()))
                        {
                            return JsExpr::Member {
                                object: Box::new(lower_expr(receiver, ctx)),
                                property: prop.clone(),
                            };
                        }
                    }
                }

                // Only fall back to type-agnostic lookup for user-defined extern methods
                // (not for stdlib-like method names like 'len' which could conflict)
                for ((type_name, m), prop) in &ctx.accessors.getters {
                    // Skip stdlib types in fallback - they should use type-specific lookup
                    // Arrays use "T[]" as type name, generic types use "Name<T>" etc.
                    let is_stdlib = matches!(
                        type_name.as_str(),
                        "String" | "Map" | "Set" | "Range" | "Array"
                    ) || type_name.starts_with("[")
                        || type_name.ends_with("[]") // T[] for arrays
                        || type_name.contains('<'); // Generic types like Map<K, V>
                    if is_stdlib {
                        continue;
                    }
                    if m == method_name {
                        // Found a getter for a user-defined type
                        return JsExpr::Member {
                            object: Box::new(lower_expr(receiver, ctx)),
                            property: prop.clone(),
                        };
                    }
                }
            }

            // Check if this is a setter (one arg, method starts with "set_")
            if args.len() == 1 && method_name.starts_with("set_") {
                // Search for any type that has this method as a setter
                for ((_, m), prop) in &ctx.accessors.setters {
                    if m == method_name {
                        // Found a setter! Emit property assignment instead of method call.
                        return JsExpr::Assignment {
                            left: Box::new(JsExpr::Member {
                                object: Box::new(lower_expr(receiver, ctx)),
                                property: prop.clone(),
                            }),
                            op: JsAssignOp::Assign,
                            right: Box::new(lower_expr(&args[0], ctx)),
                        };
                    }
                }
            }

            // Handle Result/Option unwrap and expect methods
            if method_name == "unwrap" && args.is_empty() {
                // result.unwrap() -> __husk_unwrap(result)
                return JsExpr::Call {
                    callee: Box::new(JsExpr::Ident("__husk_unwrap".to_string())),
                    args: vec![lower_expr(receiver, ctx)],
                };
            }
            if method_name == "expect" && args.len() == 1 {
                // result.expect("msg") -> __husk_expect(result, "msg")
                return JsExpr::Call {
                    callee: Box::new(JsExpr::Ident("__husk_expect".to_string())),
                    args: vec![lower_expr(receiver, ctx), lower_expr(&args[0], ctx)],
                };
            }

            // Handle tuple.to_array()
            if method_name == "to_array" && args.is_empty() {
                return JsExpr::Call {
                    callee: Box::new(JsExpr::Ident("__husk_tuple_to_array".to_string())),
                    args: vec![lower_expr(receiver, ctx)],
                };
            }

            // Handle Range.contains(item) and Range.is_empty()
            // Ranges are plain JS objects { start, end }, so we use free functions
            // Check if receiver type is Range (from semantic analysis)
            let receiver_type_from_semantic_range = ctx
                .type_resolution
                .get(&(receiver.span.range.start, receiver.span.range.end));
            if let Some(recv_type) = receiver_type_from_semantic_range {
                if recv_type.starts_with("Range") {
                    match method_name.as_str() {
                        "contains" if args.len() == 1 => {
                            return JsExpr::Call {
                                callee: Box::new(JsExpr::Ident(
                                    "__husk_range_contains".to_string(),
                                )),
                                args: vec![lower_expr(receiver, ctx), lower_expr(&args[0], ctx)],
                            };
                        }
                        "is_empty" if args.is_empty() => {
                            return JsExpr::Call {
                                callee: Box::new(JsExpr::Ident(
                                    "__husk_range_is_empty".to_string(),
                                )),
                                args: vec![lower_expr(receiver, ctx)],
                            };
                        }
                        _ => {}
                    }
                }

                // Handle Map.keys() and Map.values()
                // These use helper functions since JS Map.keys()/values() return iterators,
                // but Husk expects arrays
                if recv_type.starts_with("Map") {
                    match method_name.as_str() {
                        "keys" if args.is_empty() => {
                            return JsExpr::Call {
                                callee: Box::new(JsExpr::Ident("__husk_map_keys".to_string())),
                                args: vec![lower_expr(receiver, ctx)],
                            };
                        }
                        "values" if args.is_empty() => {
                            return JsExpr::Call {
                                callee: Box::new(JsExpr::Ident("__husk_map_values".to_string())),
                                args: vec![lower_expr(receiver, ctx)],
                            };
                        }
                        _ => {}
                    }
                }

                // Handle Set.values()
                // Similar to Map, JS Set.values() returns an iterator, but Husk expects arrays
                if recv_type.starts_with("Set") {
                    if method_name == "values" && args.is_empty() {
                        return JsExpr::Call {
                            callee: Box::new(JsExpr::Ident("__husk_set_values".to_string())),
                            args: vec![lower_expr(receiver, ctx)],
                        };
                    }
                }

                // Handle iterator methods: iter(), into_iter()
                // These create iterator objects from collections
                match method_name.as_str() {
                    "iter" if args.is_empty() => {
                        // Check if this is an array, string, or range iterator
                        if recv_type.starts_with("[") {
                            // Array iterator: create iterator over array elements
                            return JsExpr::Call {
                                callee: Box::new(JsExpr::Ident("__husk_array_iter".to_string())),
                                args: vec![lower_expr(receiver, ctx)],
                            };
                        } else if recv_type == "String" {
                            // String iterator: create iterator over string characters
                            return JsExpr::Call {
                                callee: Box::new(JsExpr::Ident("__husk_string_iter".to_string())),
                                args: vec![lower_expr(receiver, ctx)],
                            };
                        } else if recv_type.starts_with("Range") {
                            // Range iterator: create iterator over range values
                            return JsExpr::Call {
                                callee: Box::new(JsExpr::Ident("__husk_range_iter".to_string())),
                                args: vec![lower_expr(receiver, ctx)],
                            };
                        }
                    }
                    "into_iter" if args.is_empty() => {
                        // Similar to iter() but consumes the collection
                        if recv_type.starts_with("[") {
                            return JsExpr::Call {
                                callee: Box::new(JsExpr::Ident(
                                    "__husk_array_into_iter".to_string(),
                                )),
                                args: vec![lower_expr(receiver, ctx)],
                            };
                        } else if recv_type == "String" {
                            return JsExpr::Call {
                                callee: Box::new(JsExpr::Ident(
                                    "__husk_string_into_iter".to_string(),
                                )),
                                args: vec![lower_expr(receiver, ctx)],
                            };
                        } else if recv_type.starts_with("Range") {
                            return JsExpr::Call {
                                callee: Box::new(JsExpr::Ident(
                                    "__husk_range_into_iter".to_string(),
                                )),
                                args: vec![lower_expr(receiver, ctx)],
                            };
                        }
                    }
                    _ => {}
                }
            }

            // Handle iterator adaptor methods on iterator objects
            // These methods chain iterators together (lazy evaluation)
            // Only dispatch to iterator helpers if receiver is actually an Iterator type
            let receiver_type = ctx
                .type_resolution
                .get(&(receiver.span.range.start, receiver.span.range.end));
            let is_iterator_from_type = receiver_type
                .as_ref()
                .map(|ty| {
                    // Check for both formats: "impl Iterator<...>" (from format_type) and "Iterator<..." (from type_to_name)
                    ty.starts_with("impl Iterator") || ty.starts_with("Iterator<")
                })
                .unwrap_or(false);
            
            // Also check if receiver is a method call that returns an iterator
            // (e.g., arr.iter(), arr.iter().map(...), etc.)
            let is_iterator_from_method_call = if let ExprKind::MethodCall { method, .. } = &receiver.kind {
                let method_name = &method.name;
                // Methods that return iterators: iter, into_iter, map, filter, enumerate, take, skip
                matches!(method_name.as_str(), "iter" | "into_iter" | "map" | "filter" | "enumerate" | "take" | "skip")
            } else {
                false
            };
            
            let is_iterator = is_iterator_from_type || is_iterator_from_method_call;

            if is_iterator {
                match method_name.as_str() {
                "map" if args.len() == 1 => {
                    // Check if receiver is an iterator (from type resolution or previous iterator call)
                    // For now, we'll generate the call and let the runtime handle it
                    let js_receiver = lower_expr(receiver, ctx);
                    let js_closure = lower_expr(&args[0], ctx);
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_iterator_map".to_string())),
                        args: vec![js_receiver, js_closure],
                    };
                }
                "filter" if args.len() == 1 => {
                    let js_receiver = lower_expr(receiver, ctx);
                    let js_closure = lower_expr(&args[0], ctx);
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_iterator_filter".to_string())),
                        args: vec![js_receiver, js_closure],
                    };
                }
                "collect" if args.is_empty() => {
                    // collect() consumes the iterator into an array
                    let js_receiver = lower_expr(receiver, ctx);
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_iterator_collect".to_string())),
                        args: vec![js_receiver],
                    };
                }
                "for_each" if args.len() == 1 => {
                    // for_each() consumes the iterator by calling closure on each element
                    let js_receiver = lower_expr(receiver, ctx);
                    let js_closure = lower_expr(&args[0], ctx);
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_iterator_for_each".to_string())),
                        args: vec![js_receiver, js_closure],
                    };
                }
                "fold" if args.len() == 2 => {
                    let js_receiver = lower_expr(receiver, ctx);
                    let js_init = lower_expr(&args[0], ctx);
                    let js_closure = lower_expr(&args[1], ctx);
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_iterator_fold".to_string())),
                        args: vec![js_receiver, js_init, js_closure],
                    };
                }
                "enumerate" if args.is_empty() => {
                    let js_receiver = lower_expr(receiver, ctx);
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_iterator_enumerate".to_string())),
                        args: vec![js_receiver],
                    };
                }
                "take" if args.len() == 1 => {
                    let js_receiver = lower_expr(receiver, ctx);
                    let js_n = lower_expr(&args[0], ctx);
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_iterator_take".to_string())),
                        args: vec![js_receiver, js_n],
                    };
                }
                "skip" if args.len() == 1 => {
                    let js_receiver = lower_expr(receiver, ctx);
                    let js_n = lower_expr(&args[0], ctx);
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_iterator_skip".to_string())),
                        args: vec![js_receiver, js_n],
                    };
                }
                "find" if args.len() == 1 => {
                    let js_receiver = lower_expr(receiver, ctx);
                    let js_closure = lower_expr(&args[0], ctx);
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_iterator_find".to_string())),
                        args: vec![js_receiver, js_closure],
                    };
                }
                _ => {}
                }
            }

            // Handle type conversion methods (.into(), .parse(), .try_into())
            // The semantic phase records the resolved target type in type_resolution
            if (method_name == "into" || method_name == "parse" || method_name == "try_into")
                && args.is_empty()
            {
                if let Some(target_type) = ctx
                    .type_resolution
                    .get(&(expr.span.range.start, expr.span.range.end))
                {
                    return lower_conversion_method(receiver, method_name, target_type, ctx);
                }
            }

            // Default: emit as a regular method call
            // Strip numeric suffix for variadic function emulation (run1, run2, run3 -> run)
            // Use #[js_name] override if present, otherwise convert snake_case to camelCase
            // (but only for extern "js" methods; user-defined Husk methods keep their names)
            let base_method_name = strip_variadic_suffix(&method.name);

            // Try to determine receiver type for smarter method name resolution.
            // First check type_resolution from semantic analysis for the receiver's type.
            // This allows us to handle String variables (not just literals) correctly.
            let receiver_type_from_semantic = ctx
                .type_resolution
                .get(&(receiver.span.range.start, receiver.span.range.end))
                .cloned();

            // Fall back to literal detection for string literals
            let receiver_is_string_literal = matches!(
                &receiver.kind,
                ExprKind::Literal(lit) if matches!(lit.kind, LiteralKind::String(_))
            );

            // Determine if receiver is a String (either from semantic analysis or literal detection)
            let receiver_is_string = receiver_type_from_semantic
                .as_ref()
                .is_some_and(|t| t == "String")
                || receiver_is_string_literal;

            // Check if this method is an extern "js" method for any type in user code,
            // OR if the receiver is a known stdlib type (like String, Set, Map).
            let receiver_is_known_stdlib = receiver_type_from_semantic.as_ref().is_some_and(|t| {
                t == "String"
                    || t.starts_with("Set")
                    || t.starts_with("Map")
                    || t.starts_with("Range")
                    || t.starts_with("[") // arrays
            });
            let is_extern_method = receiver_is_string
                || receiver_is_known_stdlib
                || ctx
                    .accessors
                    .extern_methods
                    .iter()
                    .any(|(_, m)| m == &base_method_name);

            // Look up #[js_name] override, but only for extern methods or known stdlib types.
            // User-defined methods should keep their original names even if they
            // match stdlib method names (e.g., user's MyList.len() stays as .len()).
            //
            // Extract base type name from receiver type (e.g., "Map<String, i32>" -> "Map")
            let receiver_base_type = receiver_type_from_semantic
                .as_ref()
                .map(|t| {
                    // Strip generic args: "Map<K, V>" -> "Map", "Set<T>" -> "Set"
                    if let Some(idx) = t.find('<') {
                        &t[..idx]
                    } else {
                        t.as_str()
                    }
                })
                .map(String::from);

            // NOTE: method_js_names for non-extern user methods are currently recorded
            // but only honored when is_extern_method is true. If #[js_name] on pure Husk
            // methods (non-extern) should take effect in the future, this guard needs to
            // be relaxed or the PropertyAccessors documentation updated accordingly.
            let js_name_override = if is_extern_method {
                // Try type-specific lookup first (e.g., String.len -> length, Map.len -> size)
                if let Some(ref base_type) = receiver_base_type {
                    ctx.accessors
                        .method_js_names
                        .get(&(base_type.clone(), base_method_name.clone()))
                        .cloned()
                        .or_else(|| {
                            // Fall back to any-type lookup for backwards compatibility
                            ctx.accessors
                                .method_js_names
                                .iter()
                                .find(|((_, m), _)| m == &base_method_name)
                                .map(|(_, js_name)| js_name.clone())
                        })
                } else if receiver_is_string {
                    ctx.accessors
                        .method_js_names
                        .get(&("String".to_string(), base_method_name.clone()))
                        .cloned()
                        .or_else(|| {
                            ctx.accessors
                                .method_js_names
                                .iter()
                                .find(|((_, m), _)| m == &base_method_name)
                                .map(|(_, js_name)| js_name.clone())
                        })
                } else {
                    ctx.accessors
                        .method_js_names
                        .iter()
                        .find(|((_, m), _)| m == &base_method_name)
                        .map(|(_, js_name)| js_name.clone())
                }
            } else {
                None
            };

            let js_method_name = js_name_override.unwrap_or_else(|| {
                // Only apply snake_to_camel for extern "js" methods
                if is_extern_method {
                    snake_to_camel(&base_method_name)
                } else {
                    base_method_name.clone()
                }
            });

            // Check if this method has #[this] binding - if so, use .call(thisArg, ...)
            let is_this_binding = ctx
                .accessors
                .this_binding_methods
                .iter()
                .any(|(_, m)| m == &base_method_name);

            if is_this_binding && !args.is_empty() {
                // Generate: receiver.method.call(firstArg, ...restArgs)
                // The first argument becomes the `this` context for the JS call
                let lowered_args: Vec<_> = args.iter().map(|a| lower_expr(a, ctx)).collect();
                let (this_arg, rest_args) = lowered_args.split_first().unwrap();

                let mut call_args = vec![this_arg.clone()];
                call_args.extend(rest_args.iter().cloned());

                JsExpr::Call {
                    callee: Box::new(JsExpr::Member {
                        object: Box::new(JsExpr::Member {
                            object: Box::new(lower_expr(receiver, ctx)),
                            property: js_method_name,
                        }),
                        property: "call".to_string(),
                    }),
                    args: call_args,
                }
            } else {
                // Normal method call: receiver.method(args)
                JsExpr::Call {
                    callee: Box::new(JsExpr::Member {
                        object: Box::new(lower_expr(receiver, ctx)),
                        property: js_method_name,
                    }),
                    args: args.iter().map(|a| lower_expr(a, ctx)).collect(),
                }
            }
        }
        ExprKind::Call {
            callee,
            type_args: _,
            args,
        } => {
            // Handle built-in functions like println -> console.log
            if let ExprKind::Ident(ref id) = callee.kind {
                if id.name == "println" {
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Member {
                            object: Box::new(JsExpr::Ident("console".to_string())),
                            property: "log".to_string(),
                        }),
                        args: args.iter().map(|a| lower_expr(a, ctx)).collect(),
                    };
                }
                // print -> process.stdout.write (no newline)
                if id.name == "print" {
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Member {
                            object: Box::new(JsExpr::Member {
                                object: Box::new(JsExpr::Ident("process".to_string())),
                                property: "stdout".to_string(),
                            }),
                            property: "write".to_string(),
                        }),
                        args: args.iter().map(|a| lower_expr(a, ctx)).collect(),
                    };
                }
                // Rewrite express() to __husk_express() to patch use -> use_middleware
                if id.name == "express" {
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_express".to_string())),
                        args: args.iter().map(|a| lower_expr(a, ctx)).collect(),
                    };
                }

                // Handle include_str("path") - compile-time file inclusion
                if id.name == "include_str" {
                    return handle_include_str(args, ctx);
                }

                // Handle parse_int -> parseInt
                if id.name == "parse_int" {
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("parseInt".to_string())),
                        args: args.iter().map(|a| lower_expr(a, ctx)).collect(),
                    };
                }

                // Handle parseLong -> BigInt (for i64)
                if id.name == "parseLong" {
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("BigInt".to_string())),
                        args: args.iter().map(|a| lower_expr(a, ctx)).collect(),
                    };
                }
            }

            // Handle imported variant construction: Some(x) -> {tag: "Some", value: x}
            // Check if this call is an imported variant (e.g., from `use Option::*`)
            if let Some((enum_name, variant_name)) = ctx
                .variant_calls
                .get(&(expr.span.range.start, expr.span.range.end))
            {
                // Check if this is an untagged enum
                let is_untagged = ctx.accessors.untagged_enums.contains(enum_name);

                if is_untagged {
                    // Untagged enum: just emit the value directly without a tag
                    if args.len() == 1 {
                        return lower_expr(&args[0], ctx);
                    } else if args.is_empty() {
                        // Unit variant in untagged enum - emit the variant name as string
                        return JsExpr::String(variant_name.clone());
                    } else {
                        // Multiple args: emit as array (tuple-like)
                        return JsExpr::Array(args.iter().map(|a| lower_expr(a, ctx)).collect());
                    }
                }

                let mut fields = vec![("tag".to_string(), JsExpr::String(variant_name.clone()))];

                // Add the value(s) based on argument count
                if args.len() == 1 {
                    fields.push(("value".to_string(), lower_expr(&args[0], ctx)));
                } else {
                    for (i, arg) in args.iter().enumerate() {
                        fields.push((i.to_string(), lower_expr(arg, ctx)));
                    }
                }

                return JsExpr::Object(fields);
            }

            // Handle enum variant construction: Option::Some(x) -> {tag: "Some", value: x}
            if let ExprKind::Path { segments } = &callee.kind {
                // Handle built-in type static methods
                if segments.len() == 2 {
                    let type_name = &segments[0].name;
                    let method_name = &segments[1].name;

                    // Set::new() -> __husk_set_new()
                    if type_name == "Set" && method_name == "new" {
                        return JsExpr::Call {
                            callee: Box::new(JsExpr::Ident("__husk_set_new".to_string())),
                            args: vec![],
                        };
                    }

                    // Map::new() -> __husk_map_new()
                    if type_name == "Map" && method_name == "new" {
                        return JsExpr::Call {
                            callee: Box::new(JsExpr::Ident("__husk_map_new".to_string())),
                            args: vec![],
                        };
                    }
                }

                let variant = segments
                    .last()
                    .map(|id| id.name.clone())
                    .unwrap_or_else(|| "Unknown".to_string());

                // Get the enum name from the path (e.g., "Option" from "Option::Some")
                let enum_name = if segments.len() >= 2 {
                    segments[segments.len() - 2].name.clone()
                } else {
                    String::new()
                };

                // Check if this is an untagged enum
                let is_untagged = ctx.accessors.untagged_enums.contains(&enum_name);

                if is_untagged {
                    // Untagged enum: just emit the value directly without a tag
                    if args.len() == 1 {
                        return lower_expr(&args[0], ctx);
                    } else if args.is_empty() {
                        // Unit variant in untagged enum - emit the variant name as string
                        return JsExpr::String(variant);
                    } else {
                        // Multiple args: emit as array (tuple-like)
                        return JsExpr::Array(args.iter().map(|a| lower_expr(a, ctx)).collect());
                    }
                }

                let mut fields = vec![("tag".to_string(), JsExpr::String(variant))];

                // Add the value(s) based on argument count
                if args.len() == 1 {
                    // Single argument: { tag: "Variant", value: arg }
                    fields.push(("value".to_string(), lower_expr(&args[0], ctx)));
                } else {
                    // Multiple arguments: { tag: "Variant", "0": arg0, "1": arg1, ... }
                    for (i, arg) in args.iter().enumerate() {
                        fields.push((i.to_string(), lower_expr(arg, ctx)));
                    }
                }

                return JsExpr::Object(fields);
            }

            JsExpr::Call {
                callee: Box::new(lower_expr(callee, ctx)),
                args: args.iter().map(|a| lower_expr(a, ctx)).collect(),
            }
        }
        ExprKind::Binary { op, left, right } => {
            use husk_ast::BinaryOp::*;

            // Use deep equality for == and != to handle arrays and objects
            match op {
                Eq => {
                    return JsExpr::Call {
                        callee: Box::new(JsExpr::Ident("__husk_eq".to_string())),
                        args: vec![lower_expr(left, ctx), lower_expr(right, ctx)],
                    };
                }
                NotEq => {
                    // !__husk_eq(left, right)
                    return JsExpr::Unary {
                        op: JsUnaryOp::Not,
                        expr: Box::new(JsExpr::Call {
                            callee: Box::new(JsExpr::Ident("__husk_eq".to_string())),
                            args: vec![lower_expr(left, ctx), lower_expr(right, ctx)],
                        }),
                    };
                }
                _ => {}
            }

            let js_op = match op {
                Add => JsBinaryOp::Add,
                Sub => JsBinaryOp::Sub,
                Mul => JsBinaryOp::Mul,
                Div => JsBinaryOp::Div,
                Mod => JsBinaryOp::Mod,
                Eq | NotEq => unreachable!(), // handled above
                Lt => JsBinaryOp::Lt,
                Gt => JsBinaryOp::Gt,
                Le => JsBinaryOp::Le,
                Ge => JsBinaryOp::Ge,
                And => JsBinaryOp::And,
                Or => JsBinaryOp::Or,
            };
            JsExpr::Binary {
                op: js_op,
                left: Box::new(lower_expr(left, ctx)),
                right: Box::new(lower_expr(right, ctx)),
            }
        }
        ExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => JsExpr::Conditional {
            test: Box::new(lower_expr(cond, ctx)),
            then_branch: Box::new(lower_expr(then_branch, ctx)),
            else_branch: Box::new(lower_expr(else_branch, ctx)),
        },
        ExprKind::Match { scrutinee, arms } => lower_match_expr(scrutinee, arms, ctx),
        ExprKind::Struct { name, fields } => {
            // Lower struct instantiation to a constructor call.
            // `Point { x: 1, y: 2 }` -> `new Point(1, 2)`
            // The order of args must match the order of fields in the struct definition.
            // For now, we pass them in the order they appear in the instantiation.
            let args: Vec<JsExpr> = fields.iter().map(|f| lower_expr(&f.value, ctx)).collect();
            // name is a Vec<Ident> representing the path (e.g., ["Point"] or ["module", "Point"])
            let constructor_name = name
                .last()
                .map(|id| id.name.clone())
                .unwrap_or_else(|| "UnknownType".to_string());
            JsExpr::New {
                constructor: constructor_name,
                args,
            }
        }
        ExprKind::Block(block) => {
            // Lower a block expression to an IIFE (Immediately Invoked Function Expression).
            // This allows blocks to be used as expressions in JavaScript.
            let mut body: Vec<JsStmt> = block.stmts.iter().map(|s| lower_stmt(s, ctx)).collect();
            // Wrap the last statement in a return if it's an expression.
            if let Some(last) = body.pop() {
                match last {
                    JsStmt::Expr(expr) => body.push(JsStmt::Return(expr)),
                    other => body.push(other),
                }
            }
            JsExpr::Iife { body }
        }
        ExprKind::Unary { op, expr: inner } => {
            let js_op = match op {
                husk_ast::UnaryOp::Not => JsUnaryOp::Not,
                husk_ast::UnaryOp::Neg => JsUnaryOp::Neg,
            };
            JsExpr::Unary {
                op: js_op,
                expr: Box::new(lower_expr(inner, ctx)),
            }
        }
        ExprKind::FormatPrint {
            format,
            args,
            newline,
        } => {
            // Generate console.log (with newline) or process.stdout.write (without newline)
            // Build the formatted string by concatenating literal segments and formatted args.
            lower_format_print(&format.segments, args, *newline, ctx)
        }
        ExprKind::Format { format, args } => {
            // Generate the formatted string (without console.log).
            lower_format_string(&format.segments, args, ctx)
        }
        ExprKind::Closure {
            params,
            ret_type: _,
            body,
        } => {
            // Lower closure to JS arrow function.
            // `|x, y| x + y` -> `(x, y) => x + y`
            // `|x: i32| -> i32 { x + 1 }` -> `(x) => { return x + 1; }`
            let js_params: Vec<String> = params
                .iter()
                .map(|p| ctx.resolve_name(&p.name.name, &p.name.span))
                .collect();

            // Check if the body is a block or a simple expression
            let js_body = match &body.kind {
                ExprKind::Block(block) => {
                    // For block bodies, lower each statement
                    let mut stmts: Vec<JsStmt> = Vec::new();
                    for (i, stmt) in block.stmts.iter().enumerate() {
                        let is_last = i + 1 == block.stmts.len();
                        if is_last {
                            stmts.push(lower_tail_stmt(stmt, ctx));
                        } else {
                            stmts.push(lower_stmt(stmt, ctx));
                        }
                    }
                    stmts
                }
                _ => {
                    // For expression bodies, wrap in a return statement
                    vec![JsStmt::Return(lower_expr(body, ctx))]
                }
            };

            JsExpr::Arrow {
                params: js_params,
                body: js_body,
            }
        }
        ExprKind::Array { elements } => {
            let js_elements: Vec<JsExpr> = elements.iter().map(|e| lower_expr(e, ctx)).collect();
            JsExpr::Array(js_elements)
        }
        ExprKind::Index { base, index } => {
            // Check if this is a slice operation (index is a Range)
            if let ExprKind::Range {
                start,
                end,
                inclusive,
            } = &index.kind
            {
                // Generate arr.slice(start, end) call
                let base_js = lower_expr(base, ctx);
                let slice_callee = JsExpr::Member {
                    object: Box::new(base_js),
                    property: "slice".to_string(),
                };

                let mut args = Vec::new();

                // Start argument: default to 0 if not present
                match start {
                    Some(s) => args.push(lower_expr(s, ctx)),
                    None => args.push(JsExpr::Number(0)),
                }

                // End argument: omit for arr[start..] (slice to end), or adjust for inclusive
                if let Some(e) = end {
                    if *inclusive {
                        // For inclusive range, add 1 to the end
                        args.push(JsExpr::Binary {
                            op: JsBinaryOp::Add,
                            left: Box::new(lower_expr(e, ctx)),
                            right: Box::new(JsExpr::Number(1)),
                        });
                    } else {
                        args.push(lower_expr(e, ctx));
                    }
                }
                // If end is None, we don't add a second argument - slice(start) goes to the end

                JsExpr::Call {
                    callee: Box::new(slice_callee),
                    args,
                }
            } else {
                // Simple index: arr[i]
                JsExpr::Index {
                    object: Box::new(lower_expr(base, ctx)),
                    index: Box::new(lower_expr(index, ctx)),
                }
            }
        }
        ExprKind::Range {
            start,
            end,
            inclusive,
        } => {
            // Range expressions can be used standalone (e.g., stored in arrays).
            // Generate an object with explicit inclusivity so we don't mutate the end bound.
            let start_js = match start {
                Some(s) => lower_expr(s, ctx),
                None => JsExpr::Number(0),
            };
            let end_js = match end {
                Some(e) => lower_expr(e, ctx),
                None => JsExpr::Ident("Infinity".to_string()),
            };
            JsExpr::Object(vec![
                ("start".to_string(), start_js),
                ("end".to_string(), end_js),
                ("inclusive".to_string(), JsExpr::Bool(*inclusive)),
            ])
        }
        ExprKind::Assign { target, op, value } => {
            let js_op = match op {
                husk_ast::AssignOp::Assign => JsAssignOp::Assign,
                husk_ast::AssignOp::AddAssign => JsAssignOp::AddAssign,
                husk_ast::AssignOp::SubAssign => JsAssignOp::SubAssign,
                husk_ast::AssignOp::ModAssign => JsAssignOp::ModAssign,
            };
            JsExpr::Assignment {
                left: Box::new(lower_expr(target, ctx)),
                op: js_op,
                right: Box::new(lower_expr(value, ctx)),
            }
        }
        ExprKind::JsLiteral { code } => {
            // Emit raw JavaScript code wrapped in parentheses for safe precedence
            JsExpr::Raw(format!("({})", code))
        }
        ExprKind::Cast {
            expr: inner,
            target_ty,
        } => {
            let js_inner = lower_expr(inner, ctx);

            // Generate JavaScript conversion based on target type
            match &target_ty.kind {
                TypeExprKind::Named(ident) => match ident.name.as_str() {
                    "i32" => {
                        // f64/i64/bool → i32: truncate and convert to 32-bit signed integer
                        // Math.trunc handles the float→int conversion
                        // | 0 ensures 32-bit signed integer semantics (handles overflow wrapping)
                        // We need to emit this as raw JS since JsBinaryOp doesn't have BitOr
                        let mut inner_str = String::new();
                        write_expr(&js_inner, &mut inner_str);
                        JsExpr::Raw(format!("(Math.trunc(Number({})) | 0)", inner_str))
                    }
                    "i64" => {
                        // i32/f64/bool → i64: Convert to BigInt
                        // BigInt() handles the conversion, Math.trunc needed for f64
                        let mut inner_str = String::new();
                        write_expr(&js_inner, &mut inner_str);
                        JsExpr::Raw(format!("BigInt(Math.trunc({}))", inner_str))
                    }
                    "f64" => {
                        // i32/i64/bool → f64: Use Number() to ensure boolean/BigInt becomes numeric
                        JsExpr::Call {
                            callee: Box::new(JsExpr::Ident("Number".to_string())),
                            args: vec![js_inner],
                        }
                    }
                    "String" => {
                        // any → String: use String() constructor
                        JsExpr::Call {
                            callee: Box::new(JsExpr::Ident("String".to_string())),
                            args: vec![js_inner],
                        }
                    }
                    "bool" => {
                        // Should have been caught by semantic analysis
                        panic!(
                            "invalid cast to bool should have been rejected by semantic analysis"
                        )
                    }
                    _ => {
                        // Unknown type, pass through (shouldn't happen for valid casts)
                        js_inner
                    }
                },
                _ => {
                    // Non-simple type casts not supported, pass through
                    js_inner
                }
            }
        }
        ExprKind::Tuple { elements } => {
            // Tuples are represented as JavaScript arrays
            let js_elements: Vec<JsExpr> = elements.iter().map(|e| lower_expr(e, ctx)).collect();
            JsExpr::Array(js_elements)
        }
        ExprKind::TupleField { base, index } => {
            // Tuple field access: tuple.0 -> tuple[0]
            let js_base = lower_expr(base, ctx);
            JsExpr::Index {
                object: Box::new(js_base),
                index: Box::new(JsExpr::Number(*index as i64)),
            }
        }
        ExprKind::Try { expr: inner } => {
            // ? operator: unwrap Result/Option, throw on Err/None for early return
            // expr? becomes __husk_try(expr)
            let inner_js = lower_expr(inner, ctx);
            JsExpr::Call {
                callee: Box::new(JsExpr::Ident("__husk_try".to_string())),
                args: vec![inner_js],
            }
        }
    }
}

/// Lower a FormatPrint expression to a console.log (println) or process.stdout.write (print) call.
fn lower_format_print(
    segments: &[FormatSegment],
    args: &[Expr],
    newline: bool,
    ctx: &CodegenContext,
) -> JsExpr {
    // Track which argument we're using for implicit positioning
    let mut implicit_index = 0;
    let mut parts: Vec<JsExpr> = Vec::new();

    for segment in segments {
        match segment {
            FormatSegment::Literal(text) => {
                if !text.is_empty() {
                    parts.push(JsExpr::String(text.clone()));
                }
            }
            FormatSegment::Placeholder(ph) => {
                // Determine which argument to use
                let arg_index = ph.position.unwrap_or_else(|| {
                    let idx = implicit_index;
                    implicit_index += 1;
                    idx
                });

                if let Some(arg) = args.get(arg_index) {
                    let arg_js = lower_expr(arg, ctx);
                    let formatted = format_arg(arg_js, &ph.spec);
                    parts.push(formatted);
                }
            }
        }
    }

    // Build the formatted string
    // If we only have one part, use it directly
    // If we have multiple parts, concatenate them
    let output_arg = if parts.is_empty() {
        JsExpr::String(String::new())
    } else if parts.len() == 1 {
        parts.pop().unwrap()
    } else {
        // Concatenate all parts with +
        let mut iter = parts.into_iter();
        let mut acc = iter.next().unwrap();
        for part in iter {
            acc = JsExpr::Binary {
                op: JsBinaryOp::Add,
                left: Box::new(acc),
                right: Box::new(part),
            };
        }
        acc
    };

    if newline {
        // println -> console.log (adds newline automatically)
        JsExpr::Call {
            callee: Box::new(JsExpr::Member {
                object: Box::new(JsExpr::Ident("console".to_string())),
                property: "log".to_string(),
            }),
            args: vec![output_arg],
        }
    } else {
        // print -> process.stdout.write (no newline)
        JsExpr::Call {
            callee: Box::new(JsExpr::Member {
                object: Box::new(JsExpr::Member {
                    object: Box::new(JsExpr::Ident("process".to_string())),
                    property: "stdout".to_string(),
                }),
                property: "write".to_string(),
            }),
            args: vec![output_arg],
        }
    }
}

/// Lower a Format expression to a string (without console.log).
fn lower_format_string(segments: &[FormatSegment], args: &[Expr], ctx: &CodegenContext) -> JsExpr {
    // Track which argument we're using for implicit positioning
    let mut implicit_index = 0;
    let mut parts: Vec<JsExpr> = Vec::new();

    for segment in segments {
        match segment {
            FormatSegment::Literal(text) => {
                if !text.is_empty() {
                    parts.push(JsExpr::String(text.clone()));
                }
            }
            FormatSegment::Placeholder(ph) => {
                // Determine which argument to use
                let arg_index = ph.position.unwrap_or_else(|| {
                    let idx = implicit_index;
                    implicit_index += 1;
                    idx
                });

                if let Some(arg) = args.get(arg_index) {
                    let arg_js = lower_expr(arg, ctx);
                    let formatted = format_arg(arg_js, &ph.spec);
                    parts.push(formatted);
                }
            }
        }
    }

    // Build the formatted string
    // If we only have one part, return it directly
    // If we have multiple parts, concatenate them
    if parts.is_empty() {
        JsExpr::String(String::new())
    } else if parts.len() == 1 {
        parts.pop().unwrap()
    } else {
        // Concatenate all parts with +
        let mut iter = parts.into_iter();
        let mut acc = iter.next().unwrap();
        for part in iter {
            acc = JsExpr::Binary {
                op: JsBinaryOp::Add,
                left: Box::new(acc),
                right: Box::new(part),
            };
        }
        acc
    }
}

/// Format an argument according to the format spec.
fn format_arg(arg: JsExpr, spec: &FormatSpec) -> JsExpr {
    // Check for special type specifiers
    match spec.ty {
        Some('?') => {
            // Debug format: __husk_fmt_debug(arg, pretty)
            let pretty = spec.alternate;
            JsExpr::Call {
                callee: Box::new(JsExpr::Ident("__husk_fmt_debug".to_string())),
                args: vec![arg, JsExpr::Bool(pretty)],
            }
        }
        Some('x') | Some('X') | Some('b') | Some('o') => {
            // Numeric format: __husk_fmt_num(value, base, width, precision, fill, align, sign, alternate, zeroPad, uppercase)
            let base = match spec.ty {
                Some('x') | Some('X') => 16,
                Some('b') => 2,
                Some('o') => 8,
                _ => 10,
            };
            let uppercase = spec.ty == Some('X');

            JsExpr::Call {
                callee: Box::new(JsExpr::Ident("__husk_fmt_num".to_string())),
                args: vec![
                    arg,
                    JsExpr::Number(base),
                    spec.width
                        .map_or(JsExpr::Number(0), |w| JsExpr::Number(w as i64)),
                    spec.precision
                        .map_or(JsExpr::Ident("null".to_string()), |p| {
                            JsExpr::Number(p as i64)
                        }),
                    spec.fill.map_or(JsExpr::Ident("null".to_string()), |c| {
                        JsExpr::String(c.to_string())
                    }),
                    spec.align.map_or(JsExpr::Ident("null".to_string()), |c| {
                        JsExpr::String(c.to_string())
                    }),
                    JsExpr::Bool(spec.sign),
                    JsExpr::Bool(spec.alternate),
                    JsExpr::Bool(spec.zero_pad),
                    JsExpr::Bool(uppercase),
                ],
            }
        }
        None if has_formatting(spec) => {
            // Basic display format with formatting options
            // Use __husk_fmt_num for numeric formatting (zero-pad, width with numbers)
            if spec.zero_pad || spec.sign {
                // Use numeric formatting
                JsExpr::Call {
                    callee: Box::new(JsExpr::Ident("__husk_fmt_num".to_string())),
                    args: vec![
                        arg,
                        JsExpr::Number(10), // base 10
                        spec.width
                            .map_or(JsExpr::Number(0), |w| JsExpr::Number(w as i64)),
                        spec.precision
                            .map_or(JsExpr::Ident("null".to_string()), |p| {
                                JsExpr::Number(p as i64)
                            }),
                        spec.fill.map_or(JsExpr::Ident("null".to_string()), |c| {
                            JsExpr::String(c.to_string())
                        }),
                        spec.align.map_or(JsExpr::Ident("null".to_string()), |c| {
                            JsExpr::String(c.to_string())
                        }),
                        JsExpr::Bool(spec.sign),
                        JsExpr::Bool(spec.alternate),
                        JsExpr::Bool(spec.zero_pad),
                        JsExpr::Bool(false), // uppercase
                    ],
                }
            } else if spec.width.is_some() {
                // Use string padding for width without zero-pad
                let str_arg = JsExpr::Call {
                    callee: Box::new(JsExpr::Ident("String".to_string())),
                    args: vec![arg],
                };
                JsExpr::Call {
                    callee: Box::new(JsExpr::Ident("__husk_fmt_pad".to_string())),
                    args: vec![
                        str_arg,
                        JsExpr::Number(spec.width.unwrap_or(0) as i64),
                        spec.fill.map_or(JsExpr::Ident("null".to_string()), |c| {
                            JsExpr::String(c.to_string())
                        }),
                        spec.align.map_or(JsExpr::Ident("null".to_string()), |c| {
                            JsExpr::String(c.to_string())
                        }),
                    ],
                }
            } else {
                // Just convert to string with __husk_fmt
                JsExpr::Call {
                    callee: Box::new(JsExpr::Ident("__husk_fmt".to_string())),
                    args: vec![arg],
                }
            }
        }
        None => {
            // Simple {} - just use __husk_fmt for proper display
            JsExpr::Call {
                callee: Box::new(JsExpr::Ident("__husk_fmt".to_string())),
                args: vec![arg],
            }
        }
        _ => {
            // Unknown format specifier, just convert to string
            JsExpr::Call {
                callee: Box::new(JsExpr::Ident("__husk_fmt".to_string())),
                args: vec![arg],
            }
        }
    }
}

/// Check if a format spec has any non-default formatting options.
fn has_formatting(spec: &FormatSpec) -> bool {
    spec.fill.is_some()
        || spec.align.is_some()
        || spec.sign
        || spec.alternate
        || spec.zero_pad
        || spec.width.is_some()
        || spec.precision.is_some()
}

/// Extract bindings from a pattern for use in match arms.
/// Returns a list of (binding_name, accessor_expr) pairs.
fn extract_pattern_bindings(
    pattern: &husk_ast::Pattern,
    scrutinee_js: &JsExpr,
) -> Vec<(String, JsExpr)> {
    use husk_ast::PatternKind;

    match &pattern.kind {
        PatternKind::EnumTuple { fields, .. } => {
            let mut bindings = Vec::new();
            for (i, field) in fields.iter().enumerate() {
                if let PatternKind::Binding(ident) = &field.kind {
                    // For single-field tuple, use .value; for multi-field, use ["0"], ["1"], etc.
                    let accessor = if fields.len() == 1 {
                        JsExpr::Member {
                            object: Box::new(scrutinee_js.clone()),
                            property: "value".to_string(),
                        }
                    } else {
                        JsExpr::Index {
                            object: Box::new(scrutinee_js.clone()),
                            index: Box::new(JsExpr::String(i.to_string())),
                        }
                    };
                    bindings.push((ident.name.clone(), accessor));
                }
            }
            bindings
        }
        PatternKind::Tuple { fields } => {
            // Tuple pattern: (x, y, z) -> x = scrutinee[0], y = scrutinee[1], etc.
            let mut bindings = Vec::new();
            for (i, field) in fields.iter().enumerate() {
                let accessor = JsExpr::Index {
                    object: Box::new(scrutinee_js.clone()),
                    index: Box::new(JsExpr::Number(i as i64)),
                };
                // Recursively extract bindings from nested patterns
                match &field.kind {
                    PatternKind::Binding(ident) => {
                        bindings.push((ident.name.clone(), accessor));
                    }
                    PatternKind::Wildcard => {
                        // Ignore wildcards
                    }
                    PatternKind::Tuple { .. } => {
                        // Nested tuple pattern - extract recursively
                        bindings.extend(extract_pattern_bindings(field, &accessor));
                    }
                    _ => {}
                }
            }
            bindings
        }
        _ => Vec::new(),
    }
}

fn lower_match_expr(scrutinee: &Expr, arms: &[husk_ast::MatchArm], ctx: &CodegenContext) -> JsExpr {
    use husk_ast::PatternKind;

    let scrutinee_js = lower_expr(scrutinee, ctx);

    // Build nested conditional expression from the arms, folding from the end.
    // For a catch-all (wildcard or true binding) arm, we treat its body as the final `else`.
    // Note: A Binding pattern might actually be an imported unit variant (e.g., `None` from `use Option::*`),
    // so we check the variant_patterns map to distinguish.
    fn pattern_test(
        pattern: &husk_ast::Pattern,
        scrutinee_js: &JsExpr,
        variant_patterns: &VariantPatternMap,
    ) -> Option<JsExpr> {
        match &pattern.kind {
            PatternKind::EnumUnit { path } | PatternKind::EnumTuple { path, .. } => {
                // Use the last path segment as the variant tag string.
                let variant = path
                    .last()
                    .map(|id| id.name.clone())
                    .unwrap_or_else(|| "Unknown".to_string());
                let tag_access = JsExpr::Member {
                    object: Box::new(scrutinee_js.clone()),
                    property: "tag".to_string(),
                };
                Some(JsExpr::Binary {
                    op: JsBinaryOp::EqEq,
                    left: Box::new(tag_access),
                    right: Box::new(JsExpr::String(variant)),
                })
            }
            PatternKind::Binding(_) => {
                // Check if this binding is actually an imported unit variant
                if let Some((_enum_name, variant_name)) =
                    variant_patterns.get(&(pattern.span.range.start, pattern.span.range.end))
                {
                    let tag_access = JsExpr::Member {
                        object: Box::new(scrutinee_js.clone()),
                        property: "tag".to_string(),
                    };
                    Some(JsExpr::Binary {
                        op: JsBinaryOp::EqEq,
                        left: Box::new(tag_access),
                        right: Box::new(JsExpr::String(variant_name.clone())),
                    })
                } else {
                    // True binding pattern - acts as catch-all
                    None
                }
            }
            PatternKind::Wildcard => None,
            PatternKind::EnumStruct { .. } => None,
            // Tuple patterns are always matched (irrefutable)
            PatternKind::Tuple { .. } => None,
        }
    }

    // Lower an arm body, potentially wrapping it with let bindings for enum tuple patterns.
    fn lower_arm_body(
        arm: &husk_ast::MatchArm,
        scrutinee_js: &JsExpr,
        ctx: &CodegenContext,
    ) -> JsExpr {
        let bindings = extract_pattern_bindings(&arm.pattern, scrutinee_js);
        if bindings.is_empty() {
            lower_expr(&arm.expr, ctx)
        } else {
            // Wrap in an IIFE to introduce bindings:
            // (function() { let x = scrutinee.value; return <body>; })()
            let mut body = Vec::new();
            for (name, accessor) in bindings {
                body.push(JsStmt::Let {
                    name,
                    init: Some(accessor),
                });
            }
            body.push(JsStmt::Return(lower_expr(&arm.expr, ctx)));
            JsExpr::Iife { body }
        }
    }

    if arms.is_empty() {
        return JsExpr::Ident("undefined".to_string());
    }

    // Start from the last arm as the innermost expression.
    let mut iter = arms.iter().rev();
    let last_arm = iter.next().unwrap();
    let mut acc = lower_arm_body(last_arm, &scrutinee_js, ctx);

    for arm in iter {
        if let Some(test) = pattern_test(&arm.pattern, &scrutinee_js, ctx.variant_patterns) {
            let then_expr = lower_arm_body(arm, &scrutinee_js, ctx);
            acc = JsExpr::Conditional {
                test: Box::new(test),
                then_branch: Box::new(then_expr),
                else_branch: Box::new(acc),
            };
        } else {
            // Catch-all arm: replace accumulator with its expression.
            acc = lower_arm_body(arm, &scrutinee_js, ctx);
        }
    }

    acc
}

/// Lower a match expression to if/else statements (for use in statement position).
/// This allows break/continue in match arms to work correctly.
fn lower_match_stmt(scrutinee: &Expr, arms: &[husk_ast::MatchArm], ctx: &CodegenContext) -> JsStmt {
    use husk_ast::PatternKind;

    let scrutinee_js = lower_expr(scrutinee, ctx);

    // Generate condition for pattern matching.
    // Note: A Binding pattern might actually be an imported unit variant (e.g., `None` from `use Option::*`),
    // so we check the variant_patterns map to distinguish.
    fn pattern_test(
        pattern: &husk_ast::Pattern,
        scrutinee_js: &JsExpr,
        variant_patterns: &VariantPatternMap,
    ) -> Option<JsExpr> {
        match &pattern.kind {
            PatternKind::EnumUnit { path } | PatternKind::EnumTuple { path, .. } => {
                let variant = path
                    .last()
                    .map(|id| id.name.clone())
                    .unwrap_or_else(|| "Unknown".to_string());
                let tag_access = JsExpr::Member {
                    object: Box::new(scrutinee_js.clone()),
                    property: "tag".to_string(),
                };
                Some(JsExpr::Binary {
                    op: JsBinaryOp::EqEq,
                    left: Box::new(tag_access),
                    right: Box::new(JsExpr::String(variant)),
                })
            }
            PatternKind::Binding(_) => {
                // Check if this binding is actually an imported unit variant
                if let Some((_enum_name, variant_name)) =
                    variant_patterns.get(&(pattern.span.range.start, pattern.span.range.end))
                {
                    let tag_access = JsExpr::Member {
                        object: Box::new(scrutinee_js.clone()),
                        property: "tag".to_string(),
                    };
                    Some(JsExpr::Binary {
                        op: JsBinaryOp::EqEq,
                        left: Box::new(tag_access),
                        right: Box::new(JsExpr::String(variant_name.clone())),
                    })
                } else {
                    // True binding pattern - acts as catch-all
                    None
                }
            }
            PatternKind::Wildcard => None,
            PatternKind::EnumStruct { .. } => None,
            // Tuple patterns are always matched (irrefutable)
            PatternKind::Tuple { .. } => None,
        }
    }

    // Lower arm body to statements, including any pattern bindings
    fn lower_arm_body_stmts(
        arm: &husk_ast::MatchArm,
        scrutinee_js: &JsExpr,
        ctx: &CodegenContext,
    ) -> Vec<JsStmt> {
        let bindings = extract_pattern_bindings(&arm.pattern, scrutinee_js);
        let mut stmts = Vec::new();

        // Add let bindings for pattern variables
        for (name, accessor) in bindings {
            stmts.push(JsStmt::Let {
                name,
                init: Some(accessor),
            });
        }

        // Lower the arm expression - if it's a block, expand its statements
        match &arm.expr.kind {
            ExprKind::Block(block) => {
                for stmt in &block.stmts {
                    stmts.push(lower_stmt(stmt, ctx));
                }
            }
            _ => {
                stmts.push(JsStmt::Expr(lower_expr(&arm.expr, ctx)));
            }
        }

        stmts
    }

    if arms.is_empty() {
        return JsStmt::Expr(JsExpr::Ident("undefined".to_string()));
    }

    // Build nested if/else chain from arms
    let mut result: Option<JsStmt> = None;

    for arm in arms.iter().rev() {
        let body = lower_arm_body_stmts(arm, &scrutinee_js, ctx);

        if let Some(test) = pattern_test(&arm.pattern, &scrutinee_js, ctx.variant_patterns) {
            let else_block = result.map(|s| vec![s]);
            result = Some(JsStmt::If {
                cond: test,
                then_block: body,
                else_block,
            });
        } else {
            // Catch-all arm - just use its body as the innermost else
            // For simplicity, wrap in a block-like structure
            if body.len() == 1 {
                result = Some(body.into_iter().next().unwrap());
            } else {
                // Multiple statements - wrap in if(true) for now
                result = Some(JsStmt::If {
                    cond: JsExpr::Bool(true),
                    then_block: body,
                    else_block: None,
                });
            }
        }
    }

    result.unwrap_or_else(|| JsStmt::Expr(JsExpr::Ident("undefined".to_string())))
}

impl JsModule {
    /// Render the module to a JavaScript source string.
    pub fn to_source(&self) -> String {
        let mut out = String::new();
        for stmt in &self.body {
            write_stmt(stmt, 0, &mut out);
            out.push('\n');
        }
        out
    }

    /// Render the module to a JavaScript source string with the Husk preamble.
    /// Import/require statements are placed at the very top, followed by the preamble,
    /// then the rest of the code.
    pub fn to_source_with_preamble(&self) -> String {
        let mut out = String::new();

        // First, output all import/require statements at the top
        let mut has_imports = false;
        for stmt in &self.body {
            if is_import_stmt(stmt) {
                write_stmt(stmt, 0, &mut out);
                out.push('\n');
                has_imports = true;
            }
        }
        if has_imports {
            out.push('\n');
        }

        // Then output the preamble
        out.push_str(std_preamble_js());
        if !out.ends_with('\n') {
            out.push('\n');
        }
        out.push('\n');

        // Finally output the rest of the code (excluding imports/requires)
        for stmt in &self.body {
            if !is_import_stmt(stmt) {
                write_stmt(stmt, 0, &mut out);
                out.push('\n');
            }
        }

        out
    }

    /// Render the module to JavaScript source with a source map.
    /// Returns (js_source, source_map_json).
    pub fn to_source_with_sourcemap(
        &self,
        source_file: &str,
        source_content: &str,
    ) -> (String, String) {
        let mut out = String::new();
        let mut builder = SourceMapBuilder::new(Some(source_file));

        // Add source file to the source map
        let source_id = builder.add_source(source_file);
        builder.set_source_contents(source_id, Some(source_content));

        // Count preamble lines
        let preamble = std_preamble_js();
        let preamble_lines = preamble.lines().count() as u32;

        // First, output all import/require statements at the top
        let mut has_imports = false;
        for stmt in &self.body {
            if is_import_stmt(stmt) {
                write_stmt(stmt, 0, &mut out);
                out.push('\n');
                has_imports = true;
            }
        }
        let import_lines = if has_imports {
            out.lines().count() as u32 + 1 // +1 for blank line
        } else {
            0
        };
        if has_imports {
            out.push('\n');
        }

        // Then output the preamble
        out.push_str(preamble);
        if !out.ends_with('\n') {
            out.push('\n');
        }
        out.push('\n');

        // Track line number as we output code
        let mut current_line = import_lines + preamble_lines + 1; // +1 for blank line after preamble

        // Finally output the rest of the code (excluding imports/requires)
        for stmt in &self.body {
            if !is_import_stmt(stmt) {
                // Add source mapping for functions
                if let JsStmt::Function {
                    name,
                    source_span: Some(span),
                    ..
                } = stmt
                {
                    builder.add(
                        current_line,
                        0,
                        span.line,
                        span.column,
                        Some(source_file),
                        Some(name.as_str()),
                        false,
                    );
                }
                write_stmt(stmt, 0, &mut out);
                out.push('\n');
                current_line += count_newlines_in_stmt(stmt) + 1;
            }
        }

        let source_map = builder.into_sourcemap();
        let mut sm_out = Vec::new();
        source_map
            .to_writer(&mut sm_out)
            .expect("failed to write source map");
        let sm_json = String::from_utf8(sm_out).expect("source map is utf8");

        (out, sm_json)
    }
}

/// Count newlines in a statement (for line tracking in source maps)
fn count_newlines_in_stmt(stmt: &JsStmt) -> u32 {
    match stmt {
        JsStmt::Function { body, .. } => {
            // function header + body lines + closing brace
            1 + body
                .iter()
                .map(|s| count_newlines_in_stmt(s) + 1)
                .sum::<u32>()
        }
        JsStmt::TryCatch {
            try_block,
            catch_block,
            ..
        } => {
            // try { + try body + } catch { + catch body + }
            2 + try_block
                .iter()
                .map(|s| count_newlines_in_stmt(s) + 1)
                .sum::<u32>()
                + catch_block
                    .iter()
                    .map(|s| count_newlines_in_stmt(s) + 1)
                    .sum::<u32>()
        }
        JsStmt::If {
            then_block,
            else_block,
            ..
        } => {
            // if { + then body + } else { + else body + }
            let then_lines = then_block
                .iter()
                .map(|s| count_newlines_in_stmt(s) + 1)
                .sum::<u32>();
            let else_lines = else_block.as_ref().map_or(0, |eb| {
                1 + eb
                    .iter()
                    .map(|s| count_newlines_in_stmt(s) + 1)
                    .sum::<u32>()
            });
            1 + then_lines + else_lines
        }
        JsStmt::Block(stmts) => {
            // { + body lines + }
            1 + stmts
                .iter()
                .map(|s| count_newlines_in_stmt(s) + 1)
                .sum::<u32>()
        }
        JsStmt::Sequence(stmts) => {
            // Statements at same level, each followed by newline except last
            stmts
                .iter()
                .map(|s| count_newlines_in_stmt(s) + 1)
                .sum::<u32>()
                .saturating_sub(1)
        }
        JsStmt::ForRange { body, range_expr, .. } => {
            // ForRange may emit an extra const line if range_expr is not a simple identifier
            // for (let binding = range.start; ...) { + body + }
            let extra_line = if !is_simple_identifier(range_expr) { 1 } else { 0 };
            1 + extra_line + body
                .iter()
                .map(|s| count_newlines_in_stmt(s) + 1)
                .sum::<u32>()
        }
        _ => 0, // single-line statements
    }
}

fn indent(level: usize, out: &mut String) {
    for _ in 0..level {
        out.push_str("    ");
    }
}

/// Check if a statement is an import/require statement.
fn is_import_stmt(stmt: &JsStmt) -> bool {
    matches!(
        stmt,
        JsStmt::Import { .. }
            | JsStmt::NamedImport { .. }
            | JsStmt::Require { .. }
            | JsStmt::NamedRequire { .. }
    )
}

fn write_stmt(stmt: &JsStmt, indent_level: usize, out: &mut String) {
    match stmt {
        JsStmt::Import { name, source } => {
            indent(indent_level, out);
            out.push_str("import ");
            out.push_str(name);
            out.push_str(" from \"");
            out.push_str(&source.replace('"', "\\\""));
            out.push_str("\";");
        }
        JsStmt::NamedImport { names, source } => {
            // Universal pattern that works with both CJS and ESM packages:
            // import * as __pkg from "pkg";
            // const _pkg = __pkg.default || __pkg;
            // const { a, b, c } = _pkg;
            let safe_name = source.replace('-', "_").replace('@', "").replace('/', "_");
            let star_name = format!("__{}", safe_name);
            let pkg_name = format!("_{}", safe_name);

            indent(indent_level, out);
            out.push_str("import * as ");
            out.push_str(&star_name);
            out.push_str(" from \"");
            out.push_str(&source.replace('"', "\\\""));
            out.push_str("\";\n");

            indent(indent_level, out);
            out.push_str("const ");
            out.push_str(&pkg_name);
            out.push_str(" = ");
            out.push_str(&star_name);
            out.push_str(".default || ");
            out.push_str(&star_name);
            out.push_str(";\n");

            indent(indent_level, out);
            out.push_str("const { ");
            for (i, name) in names.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(name);
            }
            out.push_str(" } = ");
            out.push_str(&pkg_name);
            out.push(';');
        }
        JsStmt::Require { name, source } => {
            indent(indent_level, out);
            out.push_str("const ");
            out.push_str(name);
            out.push_str(" = require(\"");
            out.push_str(&source.replace('"', "\\\""));
            out.push_str("\");");
        }
        JsStmt::NamedRequire { names, source } => {
            indent(indent_level, out);
            out.push_str("const { ");
            for (i, name) in names.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(name);
            }
            out.push_str(" } = require(\"");
            out.push_str(&source.replace('"', "\\\""));
            out.push_str("\");");
        }
        JsStmt::ExportNamed { names } => {
            indent(indent_level, out);
            out.push_str("export { ");
            for (i, name) in names.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(name);
            }
            out.push_str(" };");
        }
        JsStmt::Function {
            name, params, body, ..
        } => {
            indent(indent_level, out);
            out.push_str("function ");
            out.push_str(name);
            out.push('(');
            for (i, p) in params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(p);
            }
            out.push_str(") {\n");
            for s in body {
                write_stmt(s, indent_level + 1, out);
                out.push('\n');
            }
            indent(indent_level, out);
            out.push('}');
        }
        JsStmt::Return(expr) => {
            indent(indent_level, out);
            out.push_str("return ");
            write_expr(expr, out);
            out.push(';');
        }
        JsStmt::Let { name, init } => {
            indent(indent_level, out);
            out.push_str("let ");
            out.push_str(name);
            if let Some(expr) = init {
                out.push_str(" = ");
                write_expr(expr, out);
            }
            out.push(';');
        }
        JsStmt::LetDestructure { pattern, init } => {
            indent(indent_level, out);
            out.push_str("let ");
            write_destructure_pattern_list(pattern, out);
            if let Some(expr) = init {
                out.push_str(" = ");
                write_expr(expr, out);
            }
            out.push(';');
        }
        JsStmt::Expr(expr) => {
            indent(indent_level, out);
            write_expr(expr, out);
            out.push(';');
        }
        JsStmt::TryCatch {
            try_block,
            catch_ident,
            catch_block,
        } => {
            indent(indent_level, out);
            out.push_str("try {\n");
            for s in try_block {
                write_stmt(s, indent_level + 1, out);
                out.push('\n');
            }
            indent(indent_level, out);
            out.push_str("} catch (");
            out.push_str(catch_ident);
            out.push_str(") {\n");
            for s in catch_block {
                write_stmt(s, indent_level + 1, out);
                out.push('\n');
            }
            indent(indent_level, out);
            out.push('}');
        }
        JsStmt::If {
            cond,
            then_block,
            else_block,
        } => {
            indent(indent_level, out);
            out.push_str("if (");
            write_expr(cond, out);
            out.push_str(") {\n");
            for s in then_block {
                write_stmt(s, indent_level + 1, out);
                out.push('\n');
            }
            indent(indent_level, out);
            out.push('}');
            if let Some(else_stmts) = else_block {
                // Check if the else block contains a single If statement (else if)
                if else_stmts.len() == 1 {
                    if let JsStmt::If { .. } = &else_stmts[0] {
                        out.push_str(" else ");
                        // Write without indent since it's an else-if chain
                        write_stmt(&else_stmts[0], 0, out);
                        // Trim leading whitespace that write_stmt added
                        return;
                    }
                }
                out.push_str(" else {\n");
                for s in else_stmts {
                    write_stmt(s, indent_level + 1, out);
                    out.push('\n');
                }
                indent(indent_level, out);
                out.push('}');
            }
        }
        JsStmt::ForOf {
            binding,
            iterable,
            body,
        } => {
            indent(indent_level, out);
            out.push_str("for (const ");
            out.push_str(binding);
            out.push_str(" of ");
            write_expr(iterable, out);
            out.push_str(") {\n");
            for s in body {
                write_stmt(s, indent_level + 1, out);
                out.push('\n');
            }
            indent(indent_level, out);
            out.push('}');
        }
        JsStmt::For {
            binding,
            start,
            end,
            inclusive,
            body,
        } => {
            indent(indent_level, out);
            out.push_str("for (let ");
            out.push_str(binding);
            out.push_str(" = ");
            write_expr(start, out);
            out.push_str("; ");
            out.push_str(binding);
            if *inclusive {
                out.push_str(" <= ");
            } else {
                out.push_str(" < ");
            }
            write_expr(end, out);
            out.push_str("; ");
            out.push_str(binding);
            out.push_str("++) {\n");
            for s in body {
                write_stmt(s, indent_level + 1, out);
                out.push('\n');
            }
            indent(indent_level, out);
            out.push('}');
        }
        JsStmt::ForRange {
            binding,
            range_expr,
            body,
        } => {
            // If range_expr is not a simple identifier, bind it to a temporary first
            // to avoid multiple evaluations (which could have side effects or be expensive).
            if !is_simple_identifier(range_expr) {
                let temp_name = format!("__range_{}", binding);
                indent(indent_level, out);
                out.push_str("const ");
                out.push_str(&temp_name);
                out.push_str(" = ");
                write_expr(range_expr, out);
                out.push_str(";\n");
                // Use the temporary identifier in the for loop
                indent(indent_level, out);
                out.push_str("for (let ");
                out.push_str(binding);
                out.push_str(" = ");
                out.push_str(&temp_name);
                out.push_str(".start; ");
                out.push_str(binding);
                out.push_str(" < (");
                out.push_str(&temp_name);
                out.push_str(".inclusive ? ");
                out.push_str(&temp_name);
                out.push_str(".end + 1 : ");
                out.push_str(&temp_name);
                out.push_str(".end); ");
                out.push_str(binding);
                out.push_str("++) {\n");
            } else {
                // Simple identifier - safe to evaluate multiple times
                indent(indent_level, out);
                out.push_str("for (let ");
                out.push_str(binding);
                out.push_str(" = ");
                write_expr(range_expr, out);
                out.push_str(".start; ");
                out.push_str(binding);
                out.push_str(" < (");
                write_expr(range_expr, out);
                out.push_str(".inclusive ? ");
                write_expr(range_expr, out);
                out.push_str(".end + 1 : ");
                write_expr(range_expr, out);
                out.push_str(".end); ");
                out.push_str(binding);
                out.push_str("++) {\n");
            }
            for s in body {
                write_stmt(s, indent_level + 1, out);
                out.push('\n');
            }
            indent(indent_level, out);
            out.push('}');
        }
        JsStmt::Assign { target, op, value } => {
            indent(indent_level, out);
            write_expr(target, out);
            out.push_str(match op {
                JsAssignOp::Assign => " = ",
                JsAssignOp::AddAssign => " += ",
                JsAssignOp::SubAssign => " -= ",
                JsAssignOp::ModAssign => " %= ",
            });
            write_expr(value, out);
            out.push(';');
        }
        JsStmt::While { cond, body } => {
            indent(indent_level, out);
            out.push_str("while (");
            write_expr(cond, out);
            out.push_str(") {\n");
            for s in body {
                write_stmt(s, indent_level + 1, out);
                out.push('\n');
            }
            indent(indent_level, out);
            out.push('}');
        }
        JsStmt::Break => {
            indent(indent_level, out);
            out.push_str("break;");
        }
        JsStmt::Continue => {
            indent(indent_level, out);
            out.push_str("continue;");
        }
        JsStmt::Block(stmts) => {
            indent(indent_level, out);
            out.push_str("{\n");
            for s in stmts {
                write_stmt(s, indent_level + 1, out);
                out.push('\n');
            }
            indent(indent_level, out);
            out.push('}');
        }
        JsStmt::Sequence(stmts) => {
            // Emit statements at same level without wrapping in a block.
            // No leading indent since this is a pseudo-statement.
            for (i, s) in stmts.iter().enumerate() {
                if i > 0 {
                    out.push('\n');
                }
                write_stmt(s, indent_level, out);
            }
        }
        JsStmt::Throw(expr) => {
            indent(indent_level, out);
            out.push_str("throw ");
            write_expr(expr, out);
            out.push(';');
        }
    }
}

/// Write a single destructure pattern element (binding, wildcard, or nested array).
fn write_destructure_pattern(pattern: &DestructurePattern, out: &mut String) {
    match pattern {
        DestructurePattern::Binding(name) => out.push_str(name),
        DestructurePattern::Wildcard => out.push('_'),
        DestructurePattern::Array(elements) => {
            write_destructure_pattern_list(elements, out);
        }
    }
}

/// Write an array destructure pattern like `[a, [b, c], _]`.
fn write_destructure_pattern_list(patterns: &[DestructurePattern], out: &mut String) {
    out.push('[');
    for (i, pat) in patterns.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        write_destructure_pattern(pat, out);
    }
    out.push(']');
}

/// Check if an expression is a simple identifier (safe to evaluate multiple times).
fn is_simple_identifier(expr: &JsExpr) -> bool {
    matches!(expr, JsExpr::Ident(_))
}

fn write_expr(expr: &JsExpr, out: &mut String) {
    match expr {
        JsExpr::Ident(name) => out.push_str(name),
        JsExpr::Number(n) => out.push_str(&n.to_string()),
        JsExpr::BigInt(n) => {
            out.push_str(&n.to_string());
            out.push('n');
        }
        JsExpr::Float(f) => out.push_str(&f.to_string()),
        JsExpr::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        JsExpr::Object(props) => {
            out.push('{');
            for (i, (key, value)) in props.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(key);
                out.push_str(": ");
                write_expr(value, out);
            }
            out.push('}');
        }
        JsExpr::Member { object, property } => {
            write_expr(object, out);
            out.push('.');
            out.push_str(property);
        }
        JsExpr::String(s) => {
            out.push('"');
            for ch in s.chars() {
                match ch {
                    '\\' => out.push_str("\\\\"),
                    '"' => out.push_str("\\\""),
                    '\n' => out.push_str("\\n"),
                    '\r' => out.push_str("\\r"),
                    '\t' => out.push_str("\\t"),
                    '\0' => out.push_str("\\0"),
                    c if c.is_control() => {
                        out.push_str(&format!("\\u{:04x}", c as u32));
                    }
                    c => out.push(c),
                }
            }
            out.push('"');
        }
        JsExpr::Assignment { left, op, right } => {
            write_expr(left, out);
            match op {
                JsAssignOp::Assign => out.push_str(" = "),
                JsAssignOp::AddAssign => out.push_str(" += "),
                JsAssignOp::SubAssign => out.push_str(" -= "),
                JsAssignOp::ModAssign => out.push_str(" %= "),
            }
            write_expr(right, out);
        }
        JsExpr::Call { callee, args } => {
            write_expr(callee, out);
            out.push('(');
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_expr(arg, out);
            }
            out.push(')');
        }
        JsExpr::Conditional {
            test,
            then_branch,
            else_branch,
        } => {
            out.push('(');
            write_expr(test, out);
            out.push_str(" ? ");
            write_expr(then_branch, out);
            out.push_str(" : ");
            write_expr(else_branch, out);
            out.push(')');
        }
        JsExpr::Binary { op, left, right } => {
            write_expr(left, out);
            out.push(' ');
            out.push_str(match op {
                JsBinaryOp::Add => "+",
                JsBinaryOp::Sub => "-",
                JsBinaryOp::Mul => "*",
                JsBinaryOp::Div => "/",
                JsBinaryOp::Mod => "%",
                JsBinaryOp::EqEq => "==",
                JsBinaryOp::NotEq => "!=",
                JsBinaryOp::Lt => "<",
                JsBinaryOp::Gt => ">",
                JsBinaryOp::Le => "<=",
                JsBinaryOp::Ge => ">=",
                JsBinaryOp::And => "&&",
                JsBinaryOp::Or => "||",
                JsBinaryOp::StrictEq => "===",
                JsBinaryOp::StrictNe => "!==",
            });
            out.push(' ');
            write_expr(right, out);
        }
        JsExpr::Iife { body } => {
            out.push_str("(function() {\n");
            for s in body {
                write_stmt(s, 1, out);
                out.push('\n');
            }
            out.push_str("})()");
        }
        JsExpr::Function { params, body } => {
            out.push_str("function(");
            for (i, p) in params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(p);
            }
            out.push_str(") {\n");
            for s in body {
                write_stmt(s, 1, out);
                out.push('\n');
            }
            out.push('}');
        }
        JsExpr::New { constructor, args } => {
            out.push_str("new ");
            out.push_str(constructor);
            out.push('(');
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_expr(arg, out);
            }
            out.push(')');
        }
        JsExpr::Arrow { params, body } => {
            // Generate arrow function: `(x, y) => { ... }` or `(x) => expr`
            out.push('(');
            for (i, p) in params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(p);
            }
            out.push_str(") => ");

            // If body is a single return statement, emit concise form
            if body.len() == 1 {
                if let JsStmt::Return(expr) = &body[0] {
                    write_expr(expr, out);
                    return;
                }
            }

            // Otherwise emit block form
            out.push_str("{\n");
            for s in body {
                write_stmt(s, 1, out);
                out.push('\n');
            }
            out.push('}');
        }
        JsExpr::Array(elements) => {
            out.push('[');
            for (i, elem) in elements.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_expr(elem, out);
            }
            out.push(']');
        }
        JsExpr::Index { object, index } => {
            write_expr(object, out);
            out.push('[');
            write_expr(index, out);
            out.push(']');
        }
        JsExpr::Raw(code) => {
            // Emit raw JavaScript code directly (already wrapped in parentheses)
            out.push_str(code);
        }
        JsExpr::Unary { op, expr } => {
            let op_str = match op {
                JsUnaryOp::Not => "!",
                JsUnaryOp::Neg => "-",
            };
            out.push_str(op_str);
            // Wrap in parens for safety with complex expressions
            out.push('(');
            write_expr(expr, out);
            out.push(')');
        }
    }
}

// ========== TypeScript .d.ts emission ==========

/// Convert a Husk AST file into a `.d.ts` TypeScript declaration string.
pub fn file_to_dts(file: &File) -> String {
    let mut out = String::new();

    for item in &file.items {
        match &item.kind {
            ItemKind::Struct {
                name,
                type_params,
                fields,
            } => {
                write_struct_dts(name, type_params, fields, &mut out);
                out.push('\n');
            }
            ItemKind::Enum {
                name,
                type_params,
                variants,
            } => {
                write_enum_dts(name, type_params, variants, &mut out);
                out.push('\n');
            }
            ItemKind::Fn {
                name,
                type_params,
                params,
                ret_type,
                ..
            } => {
                write_fn_dts(name, type_params, params, ret_type.as_ref(), &mut out);
                out.push('\n');
            }
            _ => {}
        }
    }

    out
}

fn write_struct_dts(name: &Ident, type_params: &[Ident], fields: &[StructField], out: &mut String) {
    out.push_str("export interface ");
    out.push_str(&name.name);
    write_simple_type_params(type_params, out);
    out.push_str(" {\n");
    for field in fields {
        out.push_str("    ");
        out.push_str(&field.name.name);
        out.push_str(": ");
        write_type_expr(&field.ty, out);
        out.push_str(";\n");
    }
    out.push_str("}\n");
}

fn write_enum_dts(
    name: &Ident,
    type_params: &[Ident],
    variants: &[husk_ast::EnumVariant],
    out: &mut String,
) {
    out.push_str("export type ");
    out.push_str(&name.name);
    write_simple_type_params(type_params, out);
    out.push_str(" =\n");

    for (i, variant) in variants.iter().enumerate() {
        out.push_str("  | { tag: \"");
        out.push_str(&variant.name.name);
        out.push('"');

        match &variant.fields {
            EnumVariantFields::Unit => {}
            EnumVariantFields::Tuple(tys) => {
                if tys.len() == 1 {
                    out.push_str("; value: ");
                    write_type_expr(&tys[0], out);
                } else {
                    for (idx, ty) in tys.iter().enumerate() {
                        out.push_str("; value");
                        out.push(char::from(b'0' + (idx as u8)));
                        out.push_str(": ");
                        write_type_expr(ty, out);
                    }
                }
            }
            EnumVariantFields::Struct(fields) => {
                for field in fields {
                    out.push_str("; ");
                    out.push_str(&field.name.name);
                    out.push_str(": ");
                    write_type_expr(&field.ty, out);
                }
            }
        }

        out.push_str(" }");
        if i + 1 < variants.len() {
            out.push('\n');
        } else {
            out.push_str(";\n");
        }
    }
}

fn write_fn_dts(
    name: &Ident,
    type_params: &[TypeParam],
    params: &[Param],
    ret_type: Option<&TypeExpr>,
    out: &mut String,
) {
    out.push_str("export function ");
    out.push_str(&name.name);
    write_type_params(type_params, out);
    out.push('(');
    for (i, param) in params.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(&param.name.name);
        out.push_str(": ");
        write_type_expr(&param.ty, out);
    }
    out.push(')');
    out.push_str(": ");
    if let Some(ret) = ret_type {
        write_type_expr(ret, out);
    } else {
        out.push_str("void");
    }
    out.push_str(";\n");
}

fn write_type_params(type_params: &[TypeParam], out: &mut String) {
    if !type_params.is_empty() {
        out.push('<');
        for (i, tp) in type_params.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            out.push_str(&tp.name.name);
            // Write trait bounds if present (for TypeScript, we use `extends`)
            if !tp.bounds.is_empty() {
                out.push_str(" extends ");
                for (j, bound) in tp.bounds.iter().enumerate() {
                    if j > 0 {
                        out.push_str(" & ");
                    }
                    write_type_expr(bound, out);
                }
            }
        }
        out.push('>');
    }
}

/// Write type params for structs/enums (which use Ident, not TypeParam)
fn write_simple_type_params(type_params: &[Ident], out: &mut String) {
    if !type_params.is_empty() {
        out.push('<');
        for (i, tp) in type_params.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            out.push_str(&tp.name);
        }
        out.push('>');
    }
}

fn write_type_expr(ty: &TypeExpr, out: &mut String) {
    match &ty.kind {
        TypeExprKind::Named(id) => match id.name.as_str() {
            "i32" => out.push_str("number"),
            "bool" => out.push_str("boolean"),
            "String" => out.push_str("string"),
            "()" => out.push_str("void"),
            other => out.push_str(other),
        },
        TypeExprKind::Generic { name, args } => {
            out.push_str(&name.name);
            out.push('<');
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_type_expr(arg, out);
            }
            out.push('>');
        }
        TypeExprKind::Function { params, ret } => {
            // Generate TypeScript function type: `(x: T, y: U) => R`
            out.push('(');
            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(&format!("arg{}: ", i));
                write_type_expr(param, out);
            }
            out.push_str(") => ");
            write_type_expr(ret, out);
        }
        TypeExprKind::Array(elem) => {
            // Generate TypeScript array type: T[]
            write_type_expr(elem, out);
            out.push_str("[]");
        }
        TypeExprKind::Tuple(types) => {
            // Generate TypeScript tuple type: [T1, T2, T3]
            out.push('[');
            for (i, ty) in types.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_type_expr(ty, out);
            }
            out.push(']');
        }
        TypeExprKind::ImplTrait { trait_ty } => {
            // For impl Trait, write the underlying trait type
            write_type_expr(trait_ty, out);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use husk_ast::{
        ExprKind as HuskExprKind, Ident as HuskIdent, Literal as HuskLiteral,
        LiteralKind as HuskLiteralKind, MatchArm as HuskMatchArm, Pattern as HuskPattern,
        PatternKind as HuskPatternKind, Span as HuskSpan, TypeExpr as HuskTypeExpr,
        TypeExprKind as HuskTypeExprKind,
    };

    #[test]
    fn prints_simple_function() {
        let module = JsModule {
            body: vec![JsStmt::Function {
                name: "main".to_string(),
                params: vec!["a".into(), "b".into()],
                body: vec![
                    JsStmt::Let {
                        name: "x".to_string(),
                        init: Some(JsExpr::Binary {
                            op: JsBinaryOp::Add,
                            left: Box::new(JsExpr::Ident("a".into())),
                            right: Box::new(JsExpr::Ident("b".into())),
                        }),
                    },
                    JsStmt::Return(JsExpr::Ident("x".into())),
                ],
                source_span: None,
            }],
        };

        let src = module.to_source();
        assert!(src.contains("function main"));
        assert!(src.contains("let x = a + b;"));
        assert!(src.contains("return x;"));
    }

    #[test]
    fn appends_module_exports_for_top_level_functions() {
        // file with two top-level functions: main and helper
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let main_fn = husk_ast::Item {
            attributes: Vec::new(),
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("main", 0),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: Vec::new(),
            },
            span: span(0, 10),
        };
        let helper_fn = husk_ast::Item {
            attributes: Vec::new(),
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("helper", 20),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: Vec::new(),
            },
            span: span(20, 30),
        };

        let file = husk_ast::File {
            items: vec![main_fn, helper_fn],
        };

        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let module = lower_file_to_js(
            &file,
            true,
            JsTarget::Cjs,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );
        let src = module.to_source();

        assert!(src.contains("function main()"));
        assert!(src.contains("function helper()"));
        assert!(
            src.contains("module.exports = {main: main, helper: helper};")
                || src.contains("module.exports = {main: main, helper: helper}")
        );
    }

    #[test]
    fn prints_object_and_member_access() {
        let obj = JsExpr::Object(vec![
            ("tag".to_string(), JsExpr::String("Red".to_string())),
            ("value".to_string(), JsExpr::Number(1)),
        ]);
        let member = JsExpr::Member {
            object: Box::new(JsExpr::Ident("color".into())),
            property: "tag".to_string(),
        };

        let mut out = String::new();
        write_expr(&obj, &mut out);
        assert!(out.contains("{tag: \"Red\", value: 1}"));

        out.clear();
        write_expr(&member, &mut out);
        assert_eq!(out, "color.tag");
    }

    #[test]
    fn lowers_simple_enum_match_to_conditional() {
        // Construct a minimal AST for:
        // enum Color { Red, Blue }
        // fn f(c: Color) -> i32 {
        //     match c {
        //         Color::Red => 1,
        //         Color::Blue => 2,
        //     }
        // }

        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let c_ident = ident("c", 0);
        let scrutinee = husk_ast::Expr {
            kind: HuskExprKind::Ident(c_ident.clone()),
            span: c_ident.span.clone(),
        };

        let color_ident = ident("Color", 10);
        let red_pat = HuskPattern {
            kind: HuskPatternKind::EnumUnit {
                path: vec![color_ident.clone(), ident("Red", 20)],
            },
            span: span(10, 23),
        };
        let blue_pat = HuskPattern {
            kind: HuskPatternKind::EnumUnit {
                path: vec![color_ident.clone(), ident("Blue", 30)],
            },
            span: span(24, 38),
        };

        let arm_red = HuskMatchArm {
            pattern: red_pat,
            expr: husk_ast::Expr {
                kind: HuskExprKind::Literal(HuskLiteral {
                    kind: HuskLiteralKind::Int(1),
                    span: span(40, 41),
                }),
                span: span(40, 41),
            },
        };
        let arm_blue = HuskMatchArm {
            pattern: blue_pat,
            expr: husk_ast::Expr {
                kind: HuskExprKind::Literal(HuskLiteral {
                    kind: HuskLiteralKind::Int(2),
                    span: span(50, 51),
                }),
                span: span(50, 51),
            },
        };

        let match_expr = husk_ast::Expr {
            kind: HuskExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms: vec![arm_red, arm_blue],
            },
            span: span(0, 60),
        };

        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let js = lower_expr(
            &match_expr,
            &CodegenContext::new(
                &PropertyAccessors::default(),
                &empty_resolution,
                &empty_type_resolution,
                &empty_variant_calls,
                &empty_variant_patterns,
            ),
        );
        let mut out = String::new();
        write_expr(&js, &mut out);

        // Should produce something like: (c.tag == "Red" ? 1 : 2)
        assert!(out.contains("c.tag"));
        assert!(out.contains("=="));
        assert!(out.contains("\"Red\""));
        assert!(out.contains(" ? "));
        assert!(out.contains(" : "));
    }

    #[test]
    fn adds_main_call_for_zero_arg_main_function() {
        // Build a minimal Husk file with:
        // fn main() { }
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let main_ident = ident("main", 0);
        let fn_item = husk_ast::Item {
            attributes: Vec::new(),
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: main_ident.clone(),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: Vec::new(),
            },
            span: span(0, 10),
        };

        let file = husk_ast::File {
            items: vec![fn_item],
        };

        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let module = lower_file_to_js(
            &file,
            true,
            JsTarget::Cjs,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );
        let src = module.to_source();

        assert!(src.contains("function main()"));
        assert!(src.contains("__husk_run_main(main);"));
    }

    #[test]
    fn emits_basic_dts_for_struct_enum_and_function() {
        // struct User { name: String, id: i32 }
        // enum Color { Red, Blue }
        // fn add(a: i32, b: i32) -> i32 { a + b }

        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        // struct User { name: String, id: i32 }
        let user_ident = ident("User", 0);
        let name_field_ident = ident("name", 10);
        let id_field_ident = ident("id", 20);
        let string_ty_ident = ident("String", 30);
        let i32_ty_ident = ident("i32", 40);

        let string_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Named(string_ty_ident.clone()),
            span: string_ty_ident.span.clone(),
        };
        let i32_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Named(i32_ty_ident.clone()),
            span: i32_ty_ident.span.clone(),
        };

        let user_struct = husk_ast::Item {
            attributes: Vec::new(),
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Struct {
                name: user_ident.clone(),
                type_params: Vec::new(),
                fields: vec![
                    husk_ast::StructField {
                        name: name_field_ident.clone(),
                        ty: string_ty.clone(),
                    },
                    husk_ast::StructField {
                        name: id_field_ident.clone(),
                        ty: i32_ty.clone(),
                    },
                ],
            },
            span: span(0, 50),
        };

        // enum Color { Red, Blue }
        let color_ident = ident("Color", 60);
        let enum_item = husk_ast::Item {
            attributes: Vec::new(),
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Enum {
                name: color_ident.clone(),
                type_params: Vec::new(),
                variants: vec![
                    husk_ast::EnumVariant {
                        name: ident("Red", 70),
                        fields: husk_ast::EnumVariantFields::Unit,
                    },
                    husk_ast::EnumVariant {
                        name: ident("Blue", 80),
                        fields: husk_ast::EnumVariantFields::Unit,
                    },
                ],
            },
            span: span(60, 100),
        };

        // fn add(a: i32, b: i32) -> i32 { a + b }
        let add_ident = ident("add", 110);
        let a_ident = ident("a", 120);
        let b_ident = ident("b", 130);

        let a_param = husk_ast::Param {
            attributes: Vec::new(),
            name: a_ident.clone(),
            ty: i32_ty.clone(),
        };
        let b_param = husk_ast::Param {
            attributes: Vec::new(),
            name: b_ident.clone(),
            ty: i32_ty.clone(),
        };
        let ret_ty = i32_ty.clone();

        let add_fn = husk_ast::Item {
            attributes: Vec::new(),
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: add_ident.clone(),
                type_params: Vec::new(),
                params: vec![a_param, b_param],
                ret_type: Some(ret_ty),
                body: Vec::new(),
            },
            span: span(110, 150),
        };

        let file = husk_ast::File {
            items: vec![user_struct, enum_item, add_fn],
        };

        let dts = file_to_dts(&file);

        assert!(dts.contains("export interface User"));
        assert!(dts.contains("name: string;"));
        assert!(dts.contains("id: number;"));

        assert!(dts.contains("export type Color"));
        assert!(dts.contains("{ tag: \"Red\" }"));
        assert!(dts.contains("{ tag: \"Blue\" }"));

        assert!(dts.contains("export function add"));
        assert!(dts.contains("a: number"));
        assert!(dts.contains("b: number"));
        assert!(dts.contains("): number;"));
    }

    #[test]
    fn lowers_extern_js_result_function_with_try_catch() {
        // extern "js" { fn parse(text: String) -> Result<i32, String>; }
        // Expect:
        // function parse(text) {
        //   try {
        //     return Ok(globalThis.parse(text));
        //   } catch (e) {
        //     return Err(e);
        //   }
        // }

        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let name = ident("parse", 0);
        let param_ident = ident("text", 10);
        let string_ty_ident = ident("String", 20);
        let result_ident = ident("Result", 30);
        let i32_ident = ident("i32", 40);
        let err_ident = ident("String", 50);

        let string_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Named(string_ty_ident.clone()),
            span: string_ty_ident.span.clone(),
        };
        let i32_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Named(i32_ident.clone()),
            span: i32_ident.span.clone(),
        };
        let err_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Named(err_ident.clone()),
            span: err_ident.span.clone(),
        };

        let result_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Generic {
                name: result_ident.clone(),
                args: vec![i32_ty, err_ty],
            },
            span: result_ident.span.clone(),
        };

        let param = husk_ast::Param {
            attributes: Vec::new(),
            name: param_ident.clone(),
            ty: string_ty,
        };

        let ext_item = husk_ast::ExternItem {
            attributes: Vec::new(),
            kind: husk_ast::ExternItemKind::Fn {
                name: name.clone(),
                params: vec![param],
                ret_type: Some(result_ty),
            },
            span: span(0, 60),
        };

        let file = husk_ast::File {
            items: vec![husk_ast::Item {
                attributes: Vec::new(),
                visibility: husk_ast::Visibility::Private,
                kind: husk_ast::ItemKind::ExternBlock {
                    abi: "js".to_string(),
                    items: vec![ext_item],
                },
                span: span(0, 60),
            }],
        };

        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let module = lower_file_to_js(
            &file,
            true,
            JsTarget::Cjs,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );
        let src = module.to_source();

        assert!(src.contains("function parse"));
        assert!(src.contains("try {"));
        assert!(src.contains("return Ok("));
        assert!(src.contains("globalThis.parse"));
        assert!(src.contains("} catch (e) {"));
        assert!(src.contains("return Err(e);"));
    }

    #[test]
    fn generates_esm_imports_for_extern_mod_declarations() {
        // extern "js" {
        //     mod express;
        //     mod fs;
        // }
        // fn main() { }

        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let express_mod = husk_ast::ExternItem {
            attributes: Vec::new(),
            kind: husk_ast::ExternItemKind::Mod {
                package: "express".to_string(),
                binding: ident("express", 0),
                items: Vec::new(),
                is_global: false,
            },
            span: span(0, 10),
        };

        let fs_mod = husk_ast::ExternItem {
            attributes: Vec::new(),
            kind: husk_ast::ExternItemKind::Mod {
                package: "fs".to_string(),
                binding: ident("fs", 20),
                items: Vec::new(),
                is_global: false,
            },
            span: span(20, 25),
        };

        let main_fn = husk_ast::Item {
            attributes: Vec::new(),
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("main", 40),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: Vec::new(),
            },
            span: span(40, 50),
        };

        let file = husk_ast::File {
            items: vec![
                husk_ast::Item {
                    attributes: Vec::new(),
                    visibility: husk_ast::Visibility::Private,
                    kind: husk_ast::ItemKind::ExternBlock {
                        abi: "js".to_string(),
                        items: vec![express_mod, fs_mod],
                    },
                    span: span(0, 30),
                },
                main_fn,
            ],
        };

        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let module = lower_file_to_js(
            &file,
            true,
            JsTarget::Esm,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );
        let src = module.to_source();

        // Check for ESM imports at the top
        assert!(src.contains("import express from \"express\";"));
        assert!(src.contains("import fs from \"fs\";"));

        // Check for ESM exports instead of CommonJS (explicit ESM target)
        assert!(src.contains("export { main }"));
        assert!(!src.contains("module.exports"));
    }

    #[test]
    fn esm_exports_even_without_imports_when_target_forced() {
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let main_fn = husk_ast::Item {
            attributes: Vec::new(),
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("main", 0),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: Vec::new(),
            },
            span: span(0, 10),
        };

        let file = husk_ast::File {
            items: vec![main_fn],
        };
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let module = lower_file_to_js(
            &file,
            true,
            JsTarget::Esm,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );
        let src = module.to_source();

        assert!(src.contains("export { main }"));
        assert!(!src.contains("module.exports"));
    }

    #[test]
    fn generates_cjs_requires_for_extern_mod_declarations() {
        // extern "js" { mod express; } targeting CommonJS.

        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let express_mod = husk_ast::ExternItem {
            attributes: Vec::new(),
            kind: husk_ast::ExternItemKind::Mod {
                package: "express".to_string(),
                binding: ident("express", 0),
                items: Vec::new(),
                is_global: false,
            },
            span: span(0, 10),
        };

        let main_fn = husk_ast::Item {
            attributes: Vec::new(),
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("main", 20),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: Vec::new(),
            },
            span: span(20, 30),
        };

        let file = husk_ast::File {
            items: vec![
                husk_ast::Item {
                    attributes: Vec::new(),
                    visibility: husk_ast::Visibility::Private,
                    kind: husk_ast::ItemKind::ExternBlock {
                        abi: "js".to_string(),
                        items: vec![express_mod],
                    },
                    span: span(0, 15),
                },
                main_fn,
            ],
        };

        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let module = lower_file_to_js(
            &file,
            true,
            JsTarget::Cjs,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );
        let src = module.to_source();

        assert!(src.contains("const express = require(\"express\");"));
        assert!(src.contains("module.exports"));
        assert!(!src.contains("export {"));
    }

    #[test]
    fn lowers_format_to_string_concatenation() {
        // Test that format("hello {}!", name) generates string concatenation
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        // Build: let s = format("Hello, {}!", name);
        let format_expr = husk_ast::Expr {
            kind: HuskExprKind::Format {
                format: husk_ast::FormatString {
                    span: span(0, 12),
                    segments: vec![
                        husk_ast::FormatSegment::Literal("Hello, ".to_string()),
                        husk_ast::FormatSegment::Placeholder(husk_ast::FormatPlaceholder {
                            position: None,
                            name: None,
                            spec: husk_ast::FormatSpec::default(),
                            span: span(7, 9),
                        }),
                        husk_ast::FormatSegment::Literal("!".to_string()),
                    ],
                },
                args: vec![husk_ast::Expr {
                    kind: HuskExprKind::Ident(ident("name", 20)),
                    span: span(20, 24),
                }],
            },
            span: span(0, 25),
        };

        let accessors = PropertyAccessors::default();
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let ctx = CodegenContext::new(
            &accessors,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );
        let js_expr = lower_expr(&format_expr, &ctx);
        let mut js_str = String::new();
        write_expr(&js_expr, &mut js_str);

        // Should generate: "Hello, " + __husk_fmt(name) + "!"
        assert!(
            js_str.contains("\"Hello, \""),
            "expected 'Hello, ' literal in output: {}",
            js_str
        );
        assert!(
            js_str.contains("__husk_fmt"),
            "expected __husk_fmt call in output: {}",
            js_str
        );
        assert!(
            js_str.contains("\"!\""),
            "expected '!' literal in output: {}",
            js_str
        );
        // Should NOT contain console.log
        assert!(
            !js_str.contains("console.log"),
            "format should not generate console.log: {}",
            js_str
        );
    }

    #[test]
    fn lowers_format_with_hex_specifier() {
        // Test that format("{:x}", num) generates __husk_fmt_num call
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let format_expr = husk_ast::Expr {
            kind: HuskExprKind::Format {
                format: husk_ast::FormatString {
                    span: span(0, 5),
                    segments: vec![husk_ast::FormatSegment::Placeholder(
                        husk_ast::FormatPlaceholder {
                            position: None,
                            name: None,
                            spec: husk_ast::FormatSpec {
                                ty: Some('x'),
                                ..Default::default()
                            },
                            span: span(0, 4),
                        },
                    )],
                },
                args: vec![husk_ast::Expr {
                    kind: HuskExprKind::Ident(ident("num", 10)),
                    span: span(10, 13),
                }],
            },
            span: span(0, 15),
        };

        let accessors = PropertyAccessors::default();
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let ctx = CodegenContext::new(
            &accessors,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );
        let js_expr = lower_expr(&format_expr, &ctx);
        let mut js_str = String::new();
        write_expr(&js_expr, &mut js_str);

        // Should generate __husk_fmt_num with base 16
        assert!(
            js_str.contains("__husk_fmt_num"),
            "expected __husk_fmt_num call in output: {}",
            js_str
        );
        assert!(
            js_str.contains("16"),
            "expected base 16 for hex format: {}",
            js_str
        );
    }

    #[test]
    fn lowers_format_simple_string_returns_string_directly() {
        // Test that format("hello") generates just the string
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };

        let format_expr = husk_ast::Expr {
            kind: HuskExprKind::Format {
                format: husk_ast::FormatString {
                    span: span(0, 7),
                    segments: vec![husk_ast::FormatSegment::Literal("hello".to_string())],
                },
                args: vec![],
            },
            span: span(0, 10),
        };

        let accessors = PropertyAccessors::default();
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let ctx = CodegenContext::new(
            &accessors,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );
        let js_expr = lower_expr(&format_expr, &ctx);
        let mut js_str = String::new();
        write_expr(&js_expr, &mut js_str);

        // Should generate just "hello"
        assert_eq!(js_str, "\"hello\"");
    }

    #[test]
    fn lowers_loop_to_while_true() {
        // Test that loop { break; } generates while (true) { break; }
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let loop_stmt = husk_ast::Stmt {
            kind: husk_ast::StmtKind::Loop {
                body: husk_ast::Block {
                    stmts: vec![husk_ast::Stmt {
                        kind: husk_ast::StmtKind::Break,
                        span: span(10, 16),
                    }],
                    span: span(5, 18),
                },
            },
            span: span(0, 20),
        };

        let main_fn = husk_ast::Item {
            attributes: Vec::new(),
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("main", 0),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: vec![loop_stmt],
            },
            span: span(0, 25),
        };

        let file = husk_ast::File {
            items: vec![main_fn],
        };
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let module = lower_file_to_js(
            &file,
            true,
            JsTarget::Cjs,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );
        let src = module.to_source();

        assert!(
            src.contains("while (true)"),
            "expected 'while (true)' in output: {}",
            src
        );
        assert!(
            src.contains("break;"),
            "expected 'break;' in output: {}",
            src
        );
    }

    #[test]
    fn lowers_while_with_condition() {
        // Test that while cond { ... } generates while (cond) { ... }
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let while_stmt = husk_ast::Stmt {
            kind: husk_ast::StmtKind::While {
                cond: husk_ast::Expr {
                    kind: HuskExprKind::Ident(ident("running", 6)),
                    span: span(6, 13),
                },
                body: husk_ast::Block {
                    stmts: vec![husk_ast::Stmt {
                        kind: husk_ast::StmtKind::Continue,
                        span: span(16, 25),
                    }],
                    span: span(14, 27),
                },
            },
            span: span(0, 28),
        };

        let main_fn = husk_ast::Item {
            attributes: Vec::new(),
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("main", 0),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: vec![while_stmt],
            },
            span: span(0, 30),
        };

        let file = husk_ast::File {
            items: vec![main_fn],
        };
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let module = lower_file_to_js(
            &file,
            true,
            JsTarget::Cjs,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );
        let src = module.to_source();

        assert!(
            src.contains("while (running)"),
            "expected 'while (running)' in output: {}",
            src
        );
        assert!(
            src.contains("continue;"),
            "expected 'continue;' in output: {}",
            src
        );
    }

    #[test]
    fn lowers_break_and_continue() {
        // Test that break and continue statements generate proper JS
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };

        let break_stmt = husk_ast::Stmt {
            kind: husk_ast::StmtKind::Break,
            span: span(0, 6),
        };

        let continue_stmt = husk_ast::Stmt {
            kind: husk_ast::StmtKind::Continue,
            span: span(7, 16),
        };

        let accessors = PropertyAccessors::default();
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let ctx = CodegenContext::new(
            &accessors,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );

        let js_break = lower_stmt(&break_stmt, &ctx);
        let js_continue = lower_stmt(&continue_stmt, &ctx);

        assert!(matches!(js_break, JsStmt::Break), "expected JsStmt::Break");
        assert!(
            matches!(js_continue, JsStmt::Continue),
            "expected JsStmt::Continue"
        );
    }

    #[test]
    fn lowers_enum_variant_with_single_arg_to_tagged_object() {
        // Test that Option::Some(x) generates {tag: "Some", value: x}
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        // Build: Option::Some(42)
        let callee = husk_ast::Expr {
            kind: HuskExprKind::Path {
                segments: vec![ident("Option", 0), ident("Some", 8)],
            },
            span: span(0, 12),
        };

        let arg = husk_ast::Expr {
            kind: HuskExprKind::Literal(HuskLiteral {
                kind: HuskLiteralKind::Int(42),
                span: span(13, 15),
            }),
            span: span(13, 15),
        };

        let call_expr = husk_ast::Expr {
            kind: HuskExprKind::Call {
                callee: Box::new(callee),
                type_args: vec![],
                args: vec![arg],
            },
            span: span(0, 16),
        };

        let accessors = PropertyAccessors::default();
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let ctx = CodegenContext::new(
            &accessors,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );

        let js_expr = lower_expr(&call_expr, &ctx);

        // Should be an object with tag and value fields
        if let JsExpr::Object(fields) = js_expr {
            assert_eq!(fields.len(), 2, "expected 2 fields (tag and value)");
            assert_eq!(fields[0].0, "tag");
            assert!(matches!(&fields[0].1, JsExpr::String(s) if s == "Some"));
            assert_eq!(fields[1].0, "value");
            assert!(matches!(&fields[1].1, JsExpr::Number(42)));
        } else {
            panic!("expected JsExpr::Object, got {:?}", js_expr);
        }
    }

    #[test]
    fn lowers_enum_variant_with_multiple_args_to_indexed_object() {
        // Test that MyEnum::Pair(a, b) generates {tag: "Pair", "0": a, "1": b}
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        // Build: MyEnum::Pair(1, 2)
        let callee = husk_ast::Expr {
            kind: HuskExprKind::Path {
                segments: vec![ident("MyEnum", 0), ident("Pair", 8)],
            },
            span: span(0, 12),
        };

        let arg1 = husk_ast::Expr {
            kind: HuskExprKind::Literal(HuskLiteral {
                kind: HuskLiteralKind::Int(1),
                span: span(13, 14),
            }),
            span: span(13, 14),
        };

        let arg2 = husk_ast::Expr {
            kind: HuskExprKind::Literal(HuskLiteral {
                kind: HuskLiteralKind::Int(2),
                span: span(16, 17),
            }),
            span: span(16, 17),
        };

        let call_expr = husk_ast::Expr {
            kind: HuskExprKind::Call {
                callee: Box::new(callee),
                type_args: vec![],
                args: vec![arg1, arg2],
            },
            span: span(0, 18),
        };

        let accessors = PropertyAccessors::default();
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let ctx = CodegenContext::new(
            &accessors,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );

        let js_expr = lower_expr(&call_expr, &ctx);

        // Should be an object with tag and indexed fields
        if let JsExpr::Object(fields) = js_expr {
            assert_eq!(fields.len(), 3, "expected 3 fields (tag, 0, 1)");
            assert_eq!(fields[0].0, "tag");
            assert!(matches!(&fields[0].1, JsExpr::String(s) if s == "Pair"));
            assert_eq!(fields[1].0, "0");
            assert!(matches!(&fields[1].1, JsExpr::Number(1)));
            assert_eq!(fields[2].0, "1");
            assert!(matches!(&fields[2].1, JsExpr::Number(2)));
        } else {
            panic!("expected JsExpr::Object, got {:?}", js_expr);
        }
    }

    #[test]
    fn lowers_unit_enum_variant_to_tagged_object() {
        // Test that Option::None generates {tag: "None"}
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        // Build: Option::None (as a Path expression, not a Call)
        let path_expr = husk_ast::Expr {
            kind: HuskExprKind::Path {
                segments: vec![ident("Option", 0), ident("None", 8)],
            },
            span: span(0, 12),
        };

        let accessors = PropertyAccessors::default();
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let ctx = CodegenContext::new(
            &accessors,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );

        let js_expr = lower_expr(&path_expr, &ctx);

        // Should be an object with only a tag field
        if let JsExpr::Object(fields) = js_expr {
            assert_eq!(fields.len(), 1, "expected 1 field (tag only)");
            assert_eq!(fields[0].0, "tag");
            assert!(matches!(&fields[0].1, JsExpr::String(s) if s == "None"));
        } else {
            panic!("expected JsExpr::Object, got {:?}", js_expr);
        }
    }

    #[test]
    fn lower_match_stmt_with_break() {
        // Test that match with break generates if/else with break
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        // Build: match x { Option::Some(v) => break, Option::None => {} }
        let scrutinee = husk_ast::Expr {
            kind: HuskExprKind::Ident(ident("x", 0)),
            span: span(0, 1),
        };

        let some_arm = husk_ast::MatchArm {
            pattern: husk_ast::Pattern {
                kind: HuskPatternKind::EnumTuple {
                    path: vec![ident("Option", 0), ident("Some", 7)],
                    fields: vec![husk_ast::Pattern {
                        kind: HuskPatternKind::Binding(ident("v", 12)),
                        span: span(12, 13),
                    }],
                },
                span: span(0, 14),
            },
            expr: husk_ast::Expr {
                kind: HuskExprKind::Block(husk_ast::Block {
                    stmts: vec![husk_ast::Stmt {
                        kind: husk_ast::StmtKind::Break,
                        span: span(18, 23),
                    }],
                    span: span(17, 24),
                }),
                span: span(17, 24),
            },
        };

        let none_arm = husk_ast::MatchArm {
            pattern: husk_ast::Pattern {
                kind: HuskPatternKind::EnumUnit {
                    path: vec![ident("Option", 26), ident("None", 33)],
                },
                span: span(26, 38),
            },
            expr: husk_ast::Expr {
                kind: HuskExprKind::Block(husk_ast::Block {
                    stmts: vec![],
                    span: span(42, 44),
                }),
                span: span(42, 44),
            },
        };

        let accessors = PropertyAccessors::default();
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let ctx = CodegenContext::new(
            &accessors,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );

        let js_stmt = lower_match_stmt(&scrutinee, &[some_arm, none_arm], &ctx);

        // Should generate: if (x.tag == "Some") { let v = x.value; break; } else if (x.tag == "None") { }
        if let JsStmt::If {
            cond,
            then_block,
            else_block,
        } = js_stmt
        {
            // Check the condition is x.tag == "Some"
            if let JsExpr::Binary { op, left, right } = cond {
                assert!(matches!(op, JsBinaryOp::EqEq));
                if let JsExpr::Member { property, .. } = *left {
                    assert_eq!(property, "tag");
                } else {
                    panic!("expected Member access for tag");
                }
                assert!(matches!(*right, JsExpr::String(s) if s == "Some"));
            } else {
                panic!("expected Binary comparison");
            }

            // Check the then block has let v = x.value; break;
            assert_eq!(then_block.len(), 2, "expected 2 statements (let + break)");
            assert!(matches!(&then_block[0], JsStmt::Let { name, .. } if name == "v"));
            assert!(matches!(&then_block[1], JsStmt::Break));

            // Check there is an else block
            assert!(else_block.is_some());
        } else {
            panic!("expected JsStmt::If, got {:?}", js_stmt);
        }
    }

    #[test]
    fn lower_match_stmt_with_continue() {
        // Test that match with continue generates if/else with continue
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        // Build: match x { _ => continue }
        let scrutinee = husk_ast::Expr {
            kind: HuskExprKind::Ident(ident("x", 0)),
            span: span(0, 1),
        };

        let wildcard_arm = husk_ast::MatchArm {
            pattern: husk_ast::Pattern {
                kind: HuskPatternKind::Wildcard,
                span: span(0, 1),
            },
            expr: husk_ast::Expr {
                kind: HuskExprKind::Block(husk_ast::Block {
                    stmts: vec![husk_ast::Stmt {
                        kind: husk_ast::StmtKind::Continue,
                        span: span(5, 13),
                    }],
                    span: span(4, 14),
                }),
                span: span(4, 14),
            },
        };

        let accessors = PropertyAccessors::default();
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let ctx = CodegenContext::new(
            &accessors,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );

        let js_stmt = lower_match_stmt(&scrutinee, &[wildcard_arm], &ctx);

        // Wildcard match should just be the body statements
        assert!(
            matches!(&js_stmt, JsStmt::Continue),
            "expected Continue, got {:?}",
            js_stmt
        );
    }

    #[test]
    fn lower_match_stmt_generates_if_else_chain() {
        // Test that match generates if/else chain, not IIFE with ternary
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        // Build: match x { Color::Red => 1, Color::Blue => 2 }
        let scrutinee = husk_ast::Expr {
            kind: HuskExprKind::Ident(ident("x", 0)),
            span: span(0, 1),
        };

        let red_arm = husk_ast::MatchArm {
            pattern: husk_ast::Pattern {
                kind: HuskPatternKind::EnumUnit {
                    path: vec![ident("Color", 0), ident("Red", 7)],
                },
                span: span(0, 10),
            },
            expr: husk_ast::Expr {
                kind: HuskExprKind::Literal(husk_ast::Literal {
                    kind: husk_ast::LiteralKind::Int(1),
                    span: span(14, 15),
                }),
                span: span(14, 15),
            },
        };

        let blue_arm = husk_ast::MatchArm {
            pattern: husk_ast::Pattern {
                kind: HuskPatternKind::EnumUnit {
                    path: vec![ident("Color", 17), ident("Blue", 24)],
                },
                span: span(17, 28),
            },
            expr: husk_ast::Expr {
                kind: HuskExprKind::Literal(husk_ast::Literal {
                    kind: husk_ast::LiteralKind::Int(2),
                    span: span(32, 33),
                }),
                span: span(32, 33),
            },
        };

        let accessors = PropertyAccessors::default();
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let ctx = CodegenContext::new(
            &accessors,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );

        let js_stmt = lower_match_stmt(&scrutinee, &[red_arm, blue_arm], &ctx);

        // Should generate if/else chain
        if let JsStmt::If {
            cond: _,
            then_block,
            else_block,
        } = js_stmt
        {
            // First arm body should be an expression 1
            assert_eq!(then_block.len(), 1);
            if let JsStmt::Expr(JsExpr::Number(n)) = &then_block[0] {
                assert_eq!(*n, 1);
            } else {
                panic!("expected Expr(Number(1)), got {:?}", then_block[0]);
            }

            // Should have an else block with the second arm
            assert!(else_block.is_some());
            let else_stmts = else_block.unwrap();
            assert_eq!(else_stmts.len(), 1);
            if let JsStmt::If { then_block, .. } = &else_stmts[0] {
                assert_eq!(then_block.len(), 1);
                if let JsStmt::Expr(JsExpr::Number(n)) = &then_block[0] {
                    assert_eq!(*n, 2);
                } else {
                    panic!("expected Expr(Number(2)), got {:?}", then_block[0]);
                }
            } else {
                panic!("expected nested If for second arm");
            }
        } else {
            panic!("expected JsStmt::If, got {:?}", js_stmt);
        }
    }

    #[test]
    fn lowers_imported_unit_variant() {
        // Test: use Option::*; let x = None;
        // Should generate: {tag: "None"}
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        let none_ident = ident("None", 0);
        let none_expr = husk_ast::Expr {
            kind: HuskExprKind::Ident(none_ident.clone()),
            span: none_ident.span.clone(),
        };

        let accessors = PropertyAccessors::default();
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let mut variant_calls = HashMap::new();
        variant_calls.insert((0, 4), ("Option".to_string(), "None".to_string()));
        let empty_variant_patterns = HashMap::new();

        let ctx = CodegenContext::new(
            &accessors,
            &empty_resolution,
            &empty_type_resolution,
            &variant_calls,
            &empty_variant_patterns,
        );

        let js_expr = lower_expr(&none_expr, &ctx);

        if let JsExpr::Object(fields) = js_expr {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].0, "tag");
            assert!(matches!(&fields[0].1, JsExpr::String(s) if s == "None"));
        } else {
            panic!("expected JsExpr::Object, got {:?}", js_expr);
        }
    }

    #[test]
    fn lowers_imported_variant_constructor() {
        // Test: use Option::*; let x = Some(42);
        // Should generate: {tag: "Some", value: 42}
        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        // Build: Some(42) where Some is an imported variant
        let some_ident = ident("Some", 0);
        let callee = husk_ast::Expr {
            kind: HuskExprKind::Ident(some_ident.clone()),
            span: some_ident.span.clone(),
        };

        let arg = husk_ast::Expr {
            kind: HuskExprKind::Literal(HuskLiteral {
                kind: HuskLiteralKind::Int(42),
                span: span(5, 7),
            }),
            span: span(5, 7),
        };

        let call_expr = husk_ast::Expr {
            kind: HuskExprKind::Call {
                callee: Box::new(callee),
                type_args: vec![],
                args: vec![arg],
            },
            span: span(0, 8),
        };

        let accessors = PropertyAccessors::default();
        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let mut variant_calls = HashMap::new();
        // Register the call span as an imported variant
        variant_calls.insert((0, 8), ("Option".to_string(), "Some".to_string()));
        let empty_variant_patterns = HashMap::new();

        let ctx = CodegenContext::new(
            &accessors,
            &empty_resolution,
            &empty_type_resolution,
            &variant_calls,
            &empty_variant_patterns,
        );

        let js_expr = lower_expr(&call_expr, &ctx);

        if let JsExpr::Object(fields) = js_expr {
            assert_eq!(fields.len(), 2, "expected 2 fields (tag and value)");
            assert_eq!(fields[0].0, "tag");
            assert!(matches!(&fields[0].1, JsExpr::String(s) if s == "Some"));
            assert_eq!(fields[1].0, "value");
            assert!(matches!(&fields[1].1, JsExpr::Number(42)));
        } else {
            panic!("expected JsExpr::Object, got {:?}", js_expr);
        }
    }

    #[test]
    fn nested_tuple_destructuring_generates_nested_array_pattern() {
        // Test that let ((a, b), c) = expr; generates let [[a, b], c] = expr;
        use DestructurePattern::*;

        let pattern = vec![
            Array(vec![Binding("a".to_string()), Binding("b".to_string())]),
            Binding("c".to_string()),
        ];

        let stmt = JsStmt::LetDestructure {
            pattern,
            init: Some(JsExpr::Ident("expr".to_string())),
        };

        let mut out = String::new();
        write_stmt(&stmt, 0, &mut out);

        assert_eq!(out, "let [[a, b], c] = expr;");
    }

    #[test]
    fn deeply_nested_tuple_destructuring() {
        // Test that let (((a, b), c), d) = expr; generates let [[[a, b], c], d] = expr;
        use DestructurePattern::*;

        let pattern = vec![
            Array(vec![
                Array(vec![Binding("a".to_string()), Binding("b".to_string())]),
                Binding("c".to_string()),
            ]),
            Binding("d".to_string()),
        ];

        let stmt = JsStmt::LetDestructure {
            pattern,
            init: Some(JsExpr::Ident("expr".to_string())),
        };

        let mut out = String::new();
        write_stmt(&stmt, 0, &mut out);

        assert_eq!(out, "let [[[a, b], c], d] = expr;");
    }

    #[test]
    fn tuple_destructuring_with_wildcards() {
        // Test that let (a, _, c) = expr; generates let [a, _, c] = expr;
        use DestructurePattern::*;

        let pattern = vec![Binding("a".to_string()), Wildcard, Binding("c".to_string())];

        let stmt = JsStmt::LetDestructure {
            pattern,
            init: Some(JsExpr::Ident("expr".to_string())),
        };

        let mut out = String::new();
        write_stmt(&stmt, 0, &mut out);

        assert_eq!(out, "let [a, _, c] = expr;");
    }

    #[test]
    fn generates_call_for_this_binding_method() {
        // Test that methods with #[this] attribute on first parameter
        // generate .call(thisArg, ...) invocations
        //
        // impl EventTarget {
        //     extern "js" fn add_event_listener(&self, #[this] target: Element, event: String);
        // }
        //
        // Calling: event_target.add_event_listener(element, "click")
        // Should generate: eventTarget.addEventListener.call(element, "click")

        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        // Build the type expression
        let event_target_ident = ident("EventTarget", 0);
        let element_ident = ident("Element", 20);
        let string_ident = ident("String", 30);

        let event_target_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Named(event_target_ident.clone()),
            span: event_target_ident.span.clone(),
        };
        let element_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Named(element_ident.clone()),
            span: element_ident.span.clone(),
        };
        let string_ty = HuskTypeExpr {
            kind: HuskTypeExprKind::Named(string_ident.clone()),
            span: string_ident.span.clone(),
        };

        // Create the #[this] attribute
        let this_attr = husk_ast::Attribute {
            name: ident("this", 40),
            value: None,
            cfg_predicate: None,
            span: span(40, 46),
        };

        // First param has #[this] attribute
        let target_param = husk_ast::Param {
            attributes: vec![this_attr],
            name: ident("target", 50),
            ty: element_ty,
        };
        // Second param is normal
        let event_param = husk_ast::Param {
            attributes: Vec::new(),
            name: ident("event", 60),
            ty: string_ty,
        };

        // Create the impl method
        let method = husk_ast::ImplMethod {
            attributes: Vec::new(),
            name: ident("add_event_listener", 70),
            receiver: Some(husk_ast::SelfReceiver::Ref),
            params: vec![target_param, event_param],
            ret_type: None,
            body: Vec::new(),
            is_extern: true,
        };

        // Create the impl block
        let impl_block = husk_ast::ImplBlock {
            type_params: Vec::new(),
            trait_ref: None,
            self_ty: event_target_ty,
            items: vec![husk_ast::ImplItem {
                kind: husk_ast::ImplItemKind::Method(method),
                span: span(70, 150),
            }],
            span: span(0, 200),
        };

        // Create a function that calls the method
        let fn_body = vec![husk_ast::Stmt {
            kind: husk_ast::StmtKind::Semi(husk_ast::Expr {
                kind: HuskExprKind::MethodCall {
                    receiver: Box::new(husk_ast::Expr {
                        kind: HuskExprKind::Ident(ident("event_target", 300)),
                        span: span(300, 312),
                    }),
                    method: ident("add_event_listener", 313),
                    type_args: Vec::new(),
                    args: vec![
                        husk_ast::Expr {
                            kind: HuskExprKind::Ident(ident("element", 332)),
                            span: span(332, 339),
                        },
                        husk_ast::Expr {
                            kind: HuskExprKind::Literal(HuskLiteral {
                                kind: HuskLiteralKind::String("click".to_string()),
                                span: span(341, 348),
                            }),
                            span: span(341, 348),
                        },
                    ],
                },
                span: span(300, 350),
            }),
            span: span(300, 351),
        }];

        let test_fn = husk_ast::Item {
            attributes: Vec::new(),
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("test_fn", 400),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: fn_body,
            },
            span: span(400, 500),
        };

        let file = husk_ast::File {
            items: vec![
                husk_ast::Item {
                    attributes: Vec::new(),
                    visibility: husk_ast::Visibility::Private,
                    kind: husk_ast::ItemKind::Impl(impl_block),
                    span: span(0, 200),
                },
                test_fn,
            ],
        };

        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let module = lower_file_to_js(
            &file,
            true,
            JsTarget::Cjs,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );
        let src = module.to_source();

        // Verify that the call uses .call() with element as the this argument
        assert!(
            src.contains("addEventListener.call(element,"),
            "Expected addEventListener.call(element, ...) but got:\n{}",
            src
        );
    }

    #[test]
    fn generates_untagged_enum_variant_without_tag() {
        // Test that #[untagged] enums emit values directly without a tag field
        // This is important for TypeScript interop with untagged unions

        use husk_ast::{
            Ident as HuskIdent, Span as HuskSpan, TypeExpr as HuskTypeExpr,
            TypeExprKind as HuskTypeExprKind,
        };

        let span = |s: usize, e: usize| HuskSpan {
            range: s..e,
            file: None,
        };
        let ident = |name: &str, s: usize| HuskIdent {
            name: name.to_string(),
            span: span(s, s + name.len()),
        };

        // Create an untagged enum with the #[untagged] attribute
        let untagged_attr = husk_ast::Attribute {
            name: ident("untagged", 0),
            value: None,
            cfg_predicate: None,
            span: span(0, 10),
        };

        let string_ty_ident = ident("String", 34);
        let i32_ty_ident = ident("i32", 49);

        let untagged_enum = husk_ast::Item {
            attributes: vec![untagged_attr],
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Enum {
                name: ident("StringOrNumber", 15),
                type_params: Vec::new(),
                variants: vec![
                    husk_ast::EnumVariant {
                        name: ident("Str", 30),
                        fields: husk_ast::EnumVariantFields::Tuple(vec![HuskTypeExpr {
                            kind: HuskTypeExprKind::Named(string_ty_ident.clone()),
                            span: span(34, 40),
                        }]),
                    },
                    husk_ast::EnumVariant {
                        name: ident("Num", 45),
                        fields: husk_ast::EnumVariantFields::Tuple(vec![HuskTypeExpr {
                            kind: HuskTypeExprKind::Named(i32_ty_ident.clone()),
                            span: span(49, 52),
                        }]),
                    },
                ],
            },
            span: span(0, 60),
        };

        // Create a function that constructs untagged enum variants
        let fn_body = vec![husk_ast::Stmt {
            kind: husk_ast::StmtKind::Let {
                mutable: false,
                pattern: husk_ast::Pattern {
                    kind: husk_ast::PatternKind::Binding(ident("x", 100)),
                    span: span(100, 101),
                },
                ty: None,
                value: Some(husk_ast::Expr {
                    kind: husk_ast::ExprKind::Call {
                        callee: Box::new(husk_ast::Expr {
                            kind: husk_ast::ExprKind::Path {
                                segments: vec![ident("StringOrNumber", 105), ident("Str", 120)],
                            },
                            span: span(105, 123),
                        }),
                        type_args: Vec::new(),
                        args: vec![husk_ast::Expr {
                            kind: husk_ast::ExprKind::Literal(husk_ast::Literal {
                                kind: husk_ast::LiteralKind::String("hello".to_string()),
                                span: span(124, 131),
                            }),
                            span: span(124, 131),
                        }],
                    },
                    span: span(105, 132),
                }),
                else_block: None,
            },
            span: span(96, 133),
        }];

        let test_fn = husk_ast::Item {
            attributes: Vec::new(),
            visibility: husk_ast::Visibility::Private,
            kind: husk_ast::ItemKind::Fn {
                name: ident("test_fn", 200),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_type: None,
                body: fn_body,
            },
            span: span(200, 300),
        };

        let file = husk_ast::File {
            items: vec![untagged_enum, test_fn],
        };

        let empty_resolution = HashMap::new();
        let empty_type_resolution = HashMap::new();
        let empty_variant_calls = HashMap::new();
        let empty_variant_patterns = HashMap::new();
        let module = lower_file_to_js(
            &file,
            true,
            JsTarget::Cjs,
            &empty_resolution,
            &empty_type_resolution,
            &empty_variant_calls,
            &empty_variant_patterns,
        );
        let src = module.to_source();

        // For untagged enums, the variant should emit just the value, not {tag: ..., value: ...}
        // StringOrNumber::Str("hello") should emit just "hello", not {tag: "Str", value: "hello"}
        assert!(
            src.contains("let x = \"hello\";"),
            "Expected untagged enum to emit raw value 'let x = \"hello\";' but got:\n{}",
            src
        );
        assert!(
            !src.contains("tag"),
            "Expected no 'tag' field for untagged enum but found one in:\n{}",
            src
        );
    }
}
