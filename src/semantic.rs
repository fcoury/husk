use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::{
    ast::visitor::AstVisitor,
    builtin_methods::MethodSignatureRegistry,
    error::{Error, Result},
    lexer::Lexer,
    package_resolver::PackageResolver,
    parser::{Expr, ExternItem, Operator, Parser, Stmt, UnaryOp, UseItems, UsePath, UsePrefix},
    span::Span,
    test_registry::TestRegistry,
    types::{Type, TypeEnvironment},
};

// Type aliases to simplify complex types
type FunctionSignature = (Vec<(String, Type)>, Type, Span);
type StructFields = HashMap<String, Type>;
type EnumVariants = HashMap<String, Vec<Type>>;

/// Semantic analyzer implemented using the visitor pattern
pub struct SemanticVisitor {
    type_env: TypeEnvironment,
    structs: HashMap<String, StructFields>,
    functions: HashMap<String, FunctionSignature>,
    enums: HashMap<String, EnumVariants>,
    match_bound_vars: HashMap<String, Type>,
    loop_depth: u32,
    imported_names: HashMap<String, Type>,
    in_async_function: bool,
    package_resolver: Option<PackageResolver>,
    current_file: Option<PathBuf>,
    project_root: Option<PathBuf>,
    analyzed_modules: HashMap<PathBuf, ()>, // Cache to avoid circular imports
    current_impl_type: Option<String>, // Track the current type being impl'd for Self resolution
    method_signatures: MethodSignatureRegistry,
    test_registry: TestRegistry,
}

impl Default for SemanticVisitor {
    fn default() -> Self {
        Self::new()
    }
}

impl SemanticVisitor {
    pub fn new() -> Self {
        let mut visitor = SemanticVisitor {
            type_env: TypeEnvironment::new(),
            structs: HashMap::new(),
            functions: HashMap::new(),
            enums: HashMap::new(),
            match_bound_vars: HashMap::new(),
            loop_depth: 0,
            imported_names: HashMap::new(),
            in_async_function: false,
            package_resolver: None,
            current_file: None,
            project_root: None,
            analyzed_modules: HashMap::new(),
            current_impl_type: None,
            method_signatures: MethodSignatureRegistry::new(),
            test_registry: TestRegistry::new(),
        };
        visitor.init_standard_library();
        visitor
    }

    /// Create a new semantic visitor with package resolution enabled
    pub fn with_package_resolver() -> Result<Self> {
        let mut visitor = Self::new();
        visitor.package_resolver = Some(PackageResolver::from_current_dir()?);
        Ok(visitor)
    }

    /// Create a new semantic visitor with file context for local module resolution
    pub fn with_context(current_file: Option<PathBuf>, project_root: Option<PathBuf>) -> Self {
        let mut visitor = Self::new();
        visitor.current_file = current_file;
        visitor.project_root = project_root;
        visitor
    }

    /// Resolve the path for a local module
    fn resolve_module_path(&self, path: &UsePath, span: &Span) -> Result<PathBuf> {
        let base_path = path.segments.join("/");

        let base_dir = match path.prefix {
            UsePrefix::Local => {
                // local:: - from project root or current file's directory
                match &self.project_root {
                    Some(root) => root.clone(),
                    None => {
                        // If no project root, try current file's directory
                        match &self.current_file {
                            Some(current) => {
                                let parent = current.parent().ok_or_else(|| {
                                    Error::new_semantic(
                                        "Current file has no parent directory".to_string(),
                                        *span,
                                    )
                                })?;
                                parent.to_path_buf()
                            }
                            None => PathBuf::new(),
                        }
                    }
                }
            }
            UsePrefix::Self_ => {
                // self:: - from current file's directory
                match &self.current_file {
                    Some(current) => {
                        let parent = current.parent().ok_or_else(|| {
                            Error::new_semantic(
                                "Current file has no parent directory".to_string(),
                                *span,
                            )
                        })?;
                        parent.to_path_buf()
                    }
                    None => {
                        return Err(Error::new_semantic(
                            "Cannot use self:: imports without current file context".to_string(),
                            *span,
                        ))
                    }
                }
            }
            UsePrefix::Super(count) => {
                // super:: - from parent directory
                match &self.current_file {
                    Some(current) => {
                        let mut parent = current.parent().ok_or_else(|| {
                            Error::new_semantic(
                                "Current file has no parent directory".to_string(),
                                *span,
                            )
                        })?;

                        // Go up 'count' directories
                        for _ in 0..count {
                            parent = parent.parent().ok_or_else(|| {
                                Error::new_semantic(
                                    "Too many super:: levels, reached filesystem root".to_string(),
                                    *span,
                                )
                            })?;
                        }

                        parent.to_path_buf()
                    }
                    None => {
                        return Err(Error::new_semantic(
                            "Cannot use super:: imports without current file context".to_string(),
                            *span,
                        ))
                    }
                }
            }
            UsePrefix::None => {
                // Should have been caught earlier
                return Err(Error::new_semantic(
                    "External packages not supported for local module analysis".to_string(),
                    *span,
                ));
            }
        };

        // Try both .hk and .husk extensions
        let hk_path = base_dir.join(&base_path).with_extension("hk");
        let husk_path = base_dir.join(&base_path).with_extension("husk");

        if hk_path.exists() {
            Ok(hk_path)
        } else if husk_path.exists() {
            Ok(husk_path)
        } else {
            // Return the .hk path for the error message
            Ok(hk_path)
        }
    }

    /// Analyze a local module and extract type information
    fn analyze_local_module(&mut self, module_path: &Path, span: &Span) -> Result<()> {
        // Avoid circular imports
        if self.analyzed_modules.contains_key(module_path) {
            return Ok(());
        }

        // Mark as being analyzed
        self.analyzed_modules.insert(module_path.to_path_buf(), ());

        // Read the file
        let contents = std::fs::read_to_string(module_path).map_err(|e| {
            Error::new_semantic(
                format!("Failed to read module '{}': {}", module_path.display(), e),
                *span,
            )
        })?;

        // Parse the module
        let mut lexer = Lexer::new(contents);
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse().map_err(|e| {
            Error::new_semantic(
                format!("Failed to parse module '{}': {}", module_path.display(), e),
                *span,
            )
        })?;

        // Create a new semantic analyzer for the module
        let mut module_analyzer = SemanticVisitor::with_context(
            Some(module_path.to_path_buf()),
            self.project_root.clone(),
        );

        // Analyze the module and extract type information
        for stmt in &stmts {
            module_analyzer.analyze_module_item(stmt)?;
        }

        // Import the extracted types into our current analyzer
        self.merge_module_types(&module_analyzer);

        Ok(())
    }

    /// Analyze a single module item to extract type information
    fn analyze_module_item(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Struct(name, _generics, fields, _span) => {
                let mut field_types = HashMap::new();
                for (field_name, field_type) in fields {
                    field_types.insert(
                        field_name.clone(),
                        Type::from_string(field_type).unwrap_or(Type::Unknown),
                    );
                }
                self.structs.insert(name.clone(), field_types.clone());

                // Add constructor function
                let params: Vec<(String, Type)> = fields
                    .iter()
                    .map(|(field_name, type_str)| {
                        (
                            field_name.clone(),
                            Type::from_string(type_str).unwrap_or(Type::Unknown),
                        )
                    })
                    .collect();

                let struct_type = Type::Struct {
                    name: name.clone(),
                    fields: params.clone(),
                };

                self.functions.insert(
                    format!("{name}::new"),
                    (params, struct_type, Span::default()),
                );
            }
            Stmt::Enum(name, _generics, variants, _span) => {
                let mut variant_types = HashMap::new();
                for variant in variants {
                    match variant {
                        crate::parser::EnumVariant::Unit(variant_name) => {
                            variant_types.insert(variant_name.clone(), vec![]);
                        }
                        crate::parser::EnumVariant::Tuple(variant_name, type_strs) => {
                            let variant_types_vec: Vec<Type> = type_strs
                                .iter()
                                .map(|t| Type::from_string(t).unwrap_or(Type::Unknown))
                                .collect();
                            variant_types.insert(variant_name.clone(), variant_types_vec);
                        }
                        crate::parser::EnumVariant::Struct(variant_name, _fields) => {
                            // For now, treat struct variants as having no associated type
                            variant_types.insert(variant_name.clone(), vec![]);
                        }
                    }
                }
                self.enums.insert(name.clone(), variant_types);
            }
            Stmt::Function(
                _attrs,
                _visibility,
                name,
                _generics,
                params,
                return_type,
                _body,
                _span,
            ) => {
                let param_types: Vec<(String, Type)> = params
                    .iter()
                    .map(|(param_name, param_type)| {
                        (
                            param_name.clone(),
                            self.resolve_type_from_module(param_type),
                        )
                    })
                    .collect();
                let ret_type = if return_type.is_empty() {
                    Type::Unit
                } else {
                    self.resolve_type_from_module(return_type)
                };
                self.functions
                    .insert(name.clone(), (param_types, ret_type, Span::default()));
            }
            Stmt::Impl(struct_name, methods, _span) => {
                // Process methods in impl blocks
                for method in methods {
                    if let Stmt::Function(
                        _attrs,
                        _visibility,
                        method_name,
                        _generics,
                        params,
                        return_type,
                        _body,
                        _method_span,
                    ) = method
                    {
                        let param_types: Vec<(String, Type)> = params
                            .iter()
                            .map(|(param_name, param_type)| {
                                let resolved_type = if param_name == "self" {
                                    // For self parameter, use the struct type
                                    if let Some(fields) = self.structs.get(struct_name) {
                                        Type::Struct {
                                            name: struct_name.to_string(),
                                            fields: fields
                                                .iter()
                                                .map(|(name, ty)| (name.clone(), ty.clone()))
                                                .collect(),
                                        }
                                    } else if let Some(variants) = self.enums.get(struct_name) {
                                        Type::Enum {
                                            name: struct_name.to_string(),
                                            variants: variants.clone(),
                                        }
                                    } else {
                                        Type::Unknown
                                    }
                                } else {
                                    self.resolve_type_from_module(param_type)
                                };
                                (param_name.clone(), resolved_type)
                            })
                            .collect();
                        let ret_type = if return_type.is_empty() {
                            Type::Unit
                        } else {
                            self.resolve_type_from_module(return_type)
                        };

                        // Register as static method with struct name prefix
                        let full_method_name = format!("{struct_name}::{method_name}");
                        self.functions
                            .insert(full_method_name, (param_types, ret_type, Span::default()));
                    }
                }
            }
            // For other statements, just ignore them for now
            _ => {}
        }
        Ok(())
    }

    /// Resolve a type name to the correct Type enum based on analyzed module information
    fn resolve_type_from_module(&self, type_name: &str) -> Type {
        // Check if it's an enum
        if let Some(variants) = self.enums.get(type_name) {
            return Type::Enum {
                name: type_name.to_string(),
                variants: variants.clone(),
            };
        }

        // Check if it's a struct
        if let Some(fields) = self.structs.get(type_name) {
            return Type::Struct {
                name: type_name.to_string(),
                fields: fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.clone()))
                    .collect(),
            };
        }

        // Fall back to Type::from_string for built-in types
        Type::from_string(type_name).unwrap_or(Type::Unknown)
    }

    /// Merge type information from a module analyzer into the current one
    fn merge_module_types(&mut self, module_analyzer: &SemanticVisitor) {
        // Merge structs
        for (name, fields) in &module_analyzer.structs {
            self.structs.insert(name.clone(), fields.clone());
        }

        // Merge enums
        for (name, variants) in &module_analyzer.enums {
            self.enums.insert(name.clone(), variants.clone());
        }

        // Merge functions
        for (name, (params, ret_type, span)) in &module_analyzer.functions {
            self.functions
                .insert(name.clone(), (params.clone(), ret_type.clone(), *span));
        }
    }

    fn process_extern_item(&mut self, item: &ExternItem, prefix: &str) -> Result<()> {
        match item {
            ExternItem::Function(name, _, params, return_type) => {
                let full_name = if prefix.is_empty() {
                    name.clone()
                } else {
                    format!("{prefix}::{name}")
                };

                // Convert parameter types
                let param_types: Vec<(String, Type)> = params
                    .iter()
                    .map(|(param_name, type_str)| {
                        (
                            param_name.clone(),
                            Type::from_string(type_str).unwrap_or(Type::Unknown),
                        )
                    })
                    .collect();

                let ret_type = Type::from_string(return_type).unwrap_or(Type::Unknown);

                // Register extern function
                self.functions.insert(
                    full_name.clone(),
                    (param_types, ret_type.clone(), Span::default()),
                );

                // Register as imported name
                self.imported_names.insert(
                    full_name,
                    Type::Function {
                        params: vec![],
                        return_type: Box::new(ret_type),
                    },
                );
            }
            ExternItem::Type(name, _) => {
                let full_name = if prefix.is_empty() {
                    name.clone()
                } else {
                    format!("{prefix}::{name}")
                };

                // Register as an opaque external type
                self.type_env.define(full_name.clone(), Type::Unknown);
                // IMPORTANT: When Type::from_string is called with "express::Application",
                // it creates a Type::Struct { name: "express::Application", ... }
                // So we register the full name to match
                self.imported_names.insert(full_name, Type::Unknown);
            }
            ExternItem::Mod(name, items) => {
                let new_prefix = if prefix.is_empty() {
                    name.clone()
                } else {
                    format!("{prefix}::{name}")
                };

                // Recursively process nested items
                for item in items {
                    self.process_extern_item(item, &new_prefix)?;
                }
            }
            ExternItem::Impl(type_name, items) => {
                // Process impl block methods
                for item in items {
                    if let ExternItem::Function(method_name, _, params, return_type) = item {
                        // For impl blocks inside extern mod, we need to include the module prefix
                        let full_type_name = if prefix.is_empty() {
                            type_name.clone()
                        } else {
                            format!("{prefix}::{type_name}")
                        };
                        let full_method_name = format!("{full_type_name}::{method_name}");

                        // Convert parameter types, filtering out 'self'
                        let param_types: Vec<(String, Type)> = params
                            .iter()
                            .filter(|(param_name, _)| param_name != "self")
                            .map(|(param_name, type_str)| {
                                (
                                    param_name.clone(),
                                    Type::from_string(type_str).unwrap_or(Type::Unknown),
                                )
                            })
                            .collect();

                        let ret_type = Type::from_string(return_type).unwrap_or(Type::Unknown);

                        // Register method
                        self.functions.insert(
                            full_method_name.clone(),
                            (param_types, ret_type.clone(), Span::default()),
                        );

                        self.imported_names.insert(
                            full_method_name,
                            Type::Function {
                                params: vec![],
                                return_type: Box::new(ret_type),
                            },
                        );
                    }
                }
            }
        }
        Ok(())
    }

    fn init_standard_library(&mut self) {
        // print and println accept any number of arguments
        self.functions.insert(
            "print".to_string(),
            (vec![], Type::Unit, Span { start: 0, end: 0 }),
        );
        self.functions.insert(
            "println".to_string(),
            (vec![], Type::Unit, Span { start: 0, end: 0 }),
        );
        // format! is a special macro-like function that returns a string
        self.functions.insert(
            "format!".to_string(),
            (vec![], Type::String, Span { start: 0, end: 0 }),
        );

        // Register built-in Option enum
        self.type_env.define(
            "Option".to_string(),
            Type::Enum {
                name: "Option".to_string(),
                variants: {
                    let mut variants = HashMap::new();
                    variants.insert("Some".to_string(), vec![Type::Unknown]); // Generic T
                    variants.insert("None".to_string(), vec![]);
                    variants
                },
            },
        );
        self.enums.insert("Option".to_string(), {
            let mut variants = HashMap::new();
            variants.insert("Some".to_string(), vec![Type::Unknown]);
            variants.insert("None".to_string(), vec![]);
            variants
        });

        // Register built-in Result enum
        self.type_env.define(
            "Result".to_string(),
            Type::Enum {
                name: "Result".to_string(),
                variants: {
                    let mut variants = HashMap::new();
                    variants.insert("Ok".to_string(), vec![Type::Unknown]); // Generic T
                    variants.insert("Err".to_string(), vec![Type::Unknown]); // Generic E
                    variants
                },
            },
        );
        self.enums.insert("Result".to_string(), {
            let mut variants = HashMap::new();
            variants.insert("Ok".to_string(), vec![Type::Unknown]);
            variants.insert("Err".to_string(), vec![Type::Unknown]);
            variants
        });

        // Register implicit variant constructors as functions
        // These return generic Result/Option types that will be inferred from context
        self.functions.insert(
            "Ok".to_string(),
            (
                vec![("value".to_string(), Type::Unknown)],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );

        self.functions.insert(
            "Err".to_string(),
            (
                vec![("error".to_string(), Type::Unknown)],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );

        self.functions.insert(
            "Some".to_string(),
            (
                vec![("value".to_string(), Type::Unknown)],
                Type::Enum {
                    name: "Option".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );

        self.functions.insert(
            "None".to_string(),
            (
                vec![],
                Type::Enum {
                    name: "Option".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );

        // Register IO functions
        // File reading functions
        self.functions.insert(
            "read_file".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );
        self.functions.insert(
            "read_file_bytes".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );
        self.functions.insert(
            "read_lines".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );

        // File writing functions
        self.functions.insert(
            "write_file".to_string(),
            (
                vec![
                    ("path".to_string(), Type::String),
                    ("contents".to_string(), Type::String),
                ],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );
        self.functions.insert(
            "write_file_bytes".to_string(),
            (
                vec![
                    ("path".to_string(), Type::String),
                    ("data".to_string(), Type::Array(Box::new(Type::Int))),
                ],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );
        self.functions.insert(
            "append_file".to_string(),
            (
                vec![
                    ("path".to_string(), Type::String),
                    ("contents".to_string(), Type::String),
                ],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );

        // Path checking functions
        self.functions.insert(
            "exists".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Bool,
                Span::default(),
            ),
        );
        self.functions.insert(
            "is_file".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Bool,
                Span::default(),
            ),
        );
        self.functions.insert(
            "is_dir".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Bool,
                Span::default(),
            ),
        );

        // Directory operations
        self.functions.insert(
            "create_dir".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );
        self.functions.insert(
            "create_dir_all".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );
        self.functions.insert(
            "remove_dir".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );
        self.functions.insert(
            "remove_dir_all".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );
        self.functions.insert(
            "read_dir".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );

        // Console IO functions
        self.functions.insert(
            "read_line".to_string(),
            (
                vec![],
                Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                },
                Span::default(),
            ),
        );

        // Async file operation functions
        self.functions.insert(
            "read_file_async".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Promise(Box::new(Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                })),
                Span::default(),
            ),
        );
        self.functions.insert(
            "read_file_bytes_async".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Promise(Box::new(Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                })),
                Span::default(),
            ),
        );
        self.functions.insert(
            "read_lines_async".to_string(),
            (
                vec![("path".to_string(), Type::String)],
                Type::Promise(Box::new(Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                })),
                Span::default(),
            ),
        );
        self.functions.insert(
            "write_file_async".to_string(),
            (
                vec![
                    ("path".to_string(), Type::String),
                    ("contents".to_string(), Type::String),
                ],
                Type::Promise(Box::new(Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                })),
                Span::default(),
            ),
        );
        self.functions.insert(
            "write_file_bytes_async".to_string(),
            (
                vec![
                    ("path".to_string(), Type::String),
                    ("data".to_string(), Type::Array(Box::new(Type::Int))),
                ],
                Type::Promise(Box::new(Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                })),
                Span::default(),
            ),
        );
        self.functions.insert(
            "append_file_async".to_string(),
            (
                vec![
                    ("path".to_string(), Type::String),
                    ("contents".to_string(), Type::String),
                ],
                Type::Promise(Box::new(Type::Enum {
                    name: "Result".to_string(),
                    variants: HashMap::new(),
                })),
                Span::default(),
            ),
        );
    }

    pub fn analyze(&mut self, stmts: &[Stmt]) -> Result<()> {
        // First pass: collect all enum and struct definitions
        self.collect_type_definitions(stmts)?;

        // Second pass: collect all function signatures for forward declarations
        self.collect_function_signatures(stmts)?;

        // Third pass: analyze function bodies and other statements
        self.visit_statements(stmts)?;
        Ok(())
    }

    /// First pass: collect all enum and struct definitions
    fn collect_type_definitions(&mut self, stmts: &[Stmt]) -> Result<()> {
        for stmt in stmts {
            match stmt {
                Stmt::Enum(name, _generic_params, variants, span) => {
                    // Process enum definition immediately
                    self.visit_enum(name, _generic_params, variants, span)?;
                }
                Stmt::Struct(name, _generic_params, fields, _span) => {
                    // Process struct definition immediately
                    let struct_fields: HashMap<String, Type> = fields
                        .iter()
                        .map(|(field_name, field_type)| {
                            let resolved_type =
                                Type::from_string(field_type).unwrap_or(Type::Unknown);
                            (field_name.clone(), resolved_type)
                        })
                        .collect();

                    self.structs.insert(name.clone(), struct_fields);

                    // Register in type environment
                    self.type_env.define(
                        name.clone(),
                        Type::Struct {
                            name: name.clone(),
                            fields: vec![], // Fields stored separately for now
                        },
                    );
                }
                _ => {
                    // Skip other statements in this pass
                }
            }
        }
        Ok(())
    }

    /// Second pass: collect function signatures without analyzing bodies
    /// This enables forward function declarations
    /// Resolve a type by name, checking enums and structs first, then falling back to built-in types
    fn resolve_type_by_name(&self, type_name: &str) -> Type {
        // Check if it's a registered enum first
        if let Some(variants) = self.enums.get(type_name) {
            return Type::Enum {
                name: type_name.to_string(),
                variants: variants.clone(),
            };
        }

        // Check if it's a registered struct
        if let Some(fields) = self.structs.get(type_name) {
            let mut field_types: Vec<(String, Type)> = fields
                .iter()
                .map(|(name, t)| (name.clone(), t.clone()))
                .collect();
            // Sort fields by name to ensure consistent ordering
            field_types.sort_by(|a, b| a.0.cmp(&b.0));
            return Type::Struct {
                name: type_name.to_string(),
                fields: field_types,
            };
        }

        // Fall back to built-in types
        Type::from_string(type_name).unwrap_or(Type::Unknown)
    }

    fn collect_function_signatures(&mut self, stmts: &[Stmt]) -> Result<()> {
        for stmt in stmts {
            match stmt {
                Stmt::Function(_, _, name, _generic_params, params, return_type, _body, span) => {
                    // Convert parameter types (same logic as visit_function)
                    let param_types: Vec<(String, Type)> = params
                        .iter()
                        .map(|(param_name, type_str)| {
                            // Special handling for 'self' parameter
                            if param_name == "self" && name.contains("::") {
                                // Extract type name from method name
                                let type_name = name.split("::").next().unwrap();
                                let self_type = self.resolve_type_by_name(type_name);
                                (param_name.clone(), self_type)
                            } else {
                                // Resolve the type dynamically - this will use correct enum variants when available
                                let param_type = self.resolve_type_by_name(type_str);
                                (param_name.clone(), param_type)
                            }
                        })
                        .collect();

                    // Parse return type using dynamic resolution
                    let ret_type = self.resolve_type_by_name(return_type);

                    // Register function signature for forward declarations
                    self.functions
                        .insert(name.to_string(), (param_types, ret_type, *span));
                }
                Stmt::Impl(struct_name, methods, _span) => {
                    // Process methods in impl blocks
                    for method in methods {
                        if let Stmt::Function(
                            _attrs,
                            _visibility,
                            method_name,
                            _generics,
                            params,
                            return_type,
                            _body,
                            _method_span,
                        ) = method
                        {
                            let param_types: Vec<(String, Type)> = params
                                .iter()
                                .map(|(param_name, param_type)| {
                                    let resolved_type = if param_name == "self" {
                                        // For self parameter, use the struct type
                                        if let Some(fields) = self.structs.get(struct_name) {
                                            Type::Struct {
                                                name: struct_name.to_string(),
                                                fields: fields
                                                    .iter()
                                                    .map(|(name, ty)| (name.clone(), ty.clone()))
                                                    .collect(),
                                            }
                                        } else if let Some(variants) = self.enums.get(struct_name) {
                                            Type::Enum {
                                                name: struct_name.to_string(),
                                                variants: variants.clone(),
                                            }
                                        } else {
                                            eprintln!("Warning: Type resolution failed for struct '{struct_name}' in method '{method_name}', falling back to Type::Unknown");
                                            Type::Unknown
                                        }
                                    } else {
                                        self.resolve_type_by_name(param_type)
                                    };
                                    (param_name.clone(), resolved_type)
                                })
                                .collect();
                            let ret_type = if return_type.is_empty() {
                                Type::Unit
                            } else {
                                self.resolve_type_by_name(return_type)
                            };

                            // Register as static method with struct name prefix
                            let full_method_name = format!("{struct_name}::{method_name}");
                            self.functions
                                .insert(full_method_name, (param_types, ret_type, Span::default()));
                        }
                    }
                }
                _ => {
                    // Skip other statements in this pass
                }
            }
        }
        Ok(())
    }

    /// Visit a list of statements without creating a new scope
    /// Used for if-else blocks that should share the same scope as their parent
    fn visit_statements_no_scope(&mut self, stmts: &[Stmt]) -> Result<Type> {
        let mut result_type = Type::Unit;

        // Visit all statements
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;

            match stmt {
                // If the last statement is an expression without semicolon,
                // return that expression's type
                Stmt::Expression(expr, false) if is_last => {
                    result_type = self.visit_expr(expr)?;
                }
                // Otherwise, just visit the statement normally
                _ => {
                    self.visit_stmt(stmt)?;
                }
            }
        }

        Ok(result_type)
    }

    /// Get the collected type information for AST transformation
    pub fn get_type_info(
        &self,
    ) -> (
        HashMap<String, StructFields>,      // structs
        HashMap<String, EnumVariants>,      // enums
        HashMap<String, FunctionSignature>, // functions
    ) {
        (
            self.structs.clone(),
            self.enums.clone(),
            self.functions.clone(),
        )
    }

    /// Get the test registry containing all discovered tests
    pub fn get_test_registry(&self) -> &TestRegistry {
        &self.test_registry
    }

    /// Helper method to check if a type is numeric
    fn is_numeric_type(&self, ty: &Type) -> bool {
        matches!(ty, Type::Int | Type::Float)
    }
}

impl AstVisitor<Type> for SemanticVisitor {
    type Error = Error;

    // Override visit_stmt to handle test discovery
    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<Type> {
        match stmt {
            Stmt::Function(
                attrs,
                _is_pub,
                name,
                generic_params,
                params,
                return_type,
                body,
                span,
            ) => {
                // Register test function if it has the #[test] attribute
                self.test_registry.register_test(name.clone(), *span, attrs);
                // Continue with normal function processing
                self.visit_function(name, generic_params, params, return_type, body, span)
            }
            Stmt::AsyncFunction(
                attrs,
                _is_pub,
                name,
                generic_params,
                params,
                return_type,
                body,
                span,
            ) => {
                // Register async test function if it has the #[test] attribute
                self.test_registry.register_test(name.clone(), *span, attrs);
                // Continue with normal async function processing
                self.visit_async_function(name, generic_params, params, return_type, body, span)
            }
            Stmt::Module(attrs, name, body, span) => {
                // Enter the module
                self.test_registry.enter_module(name, attrs);
                // Visit the module
                let result = self.visit_module(name, body, span);
                // Exit the module
                self.test_registry.exit_module();
                result
            }
            // For all other statements, use the default implementation
            _ => {
                // Call parent's visit_stmt implementation
                match stmt {
                    Stmt::Let(name, expr, span) => self.visit_let(name, expr, span),
                    Stmt::Struct(name, generic_params, fields, span) => {
                        self.visit_struct(name, generic_params, fields, span)
                    }
                    Stmt::Enum(name, generic_params, variants, span) => {
                        self.visit_enum(name, generic_params, variants, span)
                    }
                    Stmt::Impl(struct_name, methods, span) => {
                        self.visit_impl(struct_name, methods, span)
                    }
                    Stmt::Match(expr, arms, span) => self.visit_match(expr, arms, span),
                    Stmt::ForLoop(pattern, iterable, body, span) => {
                        self.visit_for_loop(pattern, iterable, body, span)
                    }
                    Stmt::While(condition, body, span) => self.visit_while(condition, body, span),
                    Stmt::Loop(body, span) => self.visit_loop(body, span),
                    Stmt::Break(span) => self.visit_break(span),
                    Stmt::Continue(span) => self.visit_continue(span),
                    Stmt::Return(expr, span) => self.visit_return(expr.as_ref(), span),
                    Stmt::Expression(expr, has_semicolon) => {
                        self.visit_expression_stmt(expr, *has_semicolon)
                    }
                    Stmt::Use(path, items, span) => self.visit_use(path, items, span),
                    Stmt::ExternFunction(name, generic_params, params, return_type, span) => {
                        self.visit_extern_function(name, generic_params, params, return_type, span)
                    }
                    Stmt::ExternMod(name, items, span) => self.visit_extern_mod(name, items, span),
                    Stmt::ExternType(name, generic_params, span) => {
                        self.visit_extern_type(name, generic_params, span)
                    }
                    // These cases are already handled above
                    Stmt::Function(_, _, _, _, _, _, _, _)
                    | Stmt::AsyncFunction(_, _, _, _, _, _, _, _)
                    | Stmt::Module(_, _, _, _) => unreachable!(),
                }
            }
        }
    }

    // ===== Expression visit methods =====

    fn visit_int(&mut self, _value: i64, _span: &Span) -> Result<Type> {
        Ok(Type::Int)
    }

    fn visit_float(&mut self, _value: f64, _span: &Span) -> Result<Type> {
        Ok(Type::Float)
    }

    fn visit_bool(&mut self, _value: bool, _span: &Span) -> Result<Type> {
        Ok(Type::Bool)
    }

    fn visit_unit(&mut self, _span: &Span) -> Result<Type> {
        Ok(Type::Unit)
    }

    fn visit_string(&mut self, _value: &str, _span: &Span) -> Result<Type> {
        Ok(Type::String)
    }

    fn visit_identifier(&mut self, name: &str, span: &Span) -> Result<Type> {
        if let Some(var_type) = self.match_bound_vars.get(name) {
            return Ok(var_type.clone());
        }

        match self.type_env.lookup(name) {
            Some(var_type) => Ok(var_type.clone()),
            None => {
                // Check if it's an imported type
                if let Some(imported_type) = self.imported_names.get(name) {
                    Ok(imported_type.clone())
                } else {
                    Err(Error::new_semantic(
                        format!("Variable '{name}' not found in scope"),
                        *span,
                    ))
                }
            }
        }
    }

    fn visit_array(&mut self, elements: &[Expr], span: &Span) -> Result<Type> {
        if elements.is_empty() {
            return Ok(Type::Array(Box::new(Type::Unknown)));
        }

        let first_type = self.visit_expr(&elements[0])?;

        // Check that all elements have the same type
        for element in elements.iter().skip(1) {
            let element_type = self.visit_expr(element)?;
            if element_type != first_type {
                return Err(Error::new_semantic(
                    format!(
                        "Array elements must have the same type. Expected {first_type}, found {element_type}"
                    ),
                    *span,
                ));
            }
        }

        Ok(Type::Array(Box::new(first_type)))
    }

    fn visit_tuple(&mut self, elements: &[Expr], _span: &Span) -> Result<Type> {
        let mut element_types = Vec::new();
        for element in elements {
            element_types.push(self.visit_expr(element)?);
        }
        Ok(Type::Tuple(element_types))
    }

    fn visit_array_index(&mut self, array: &Expr, index: &Expr, span: &Span) -> Result<Type> {
        let array_type = self.visit_expr(array)?;
        let index_type = self.visit_expr(index)?;

        match (&array_type, &index_type) {
            (Type::Array(element_type), Type::Int) => {
                // Single element access
                Ok(*element_type.clone())
            }
            (Type::Array(element_type), Type::Range) => {
                // Array slicing - returns array of same element type
                Ok(Type::Array(element_type.clone()))
            }
            (Type::Array(_), _) => Err(Error::new_semantic(
                format!("Array index must be an integer or range, found {index_type}"),
                *span,
            )),
            _ => Err(Error::new_semantic(
                format!("Cannot index non-array type: {array_type}"),
                *span,
            )),
        }
    }

    fn visit_range(
        &mut self,
        start: Option<&Expr>,
        end: Option<&Expr>,
        _inclusive: bool,
        span: &Span,
    ) -> Result<Type> {
        // Check start expression if present
        if let Some(start_expr) = start {
            let start_type = self.visit_expr(start_expr)?;
            if !self.is_numeric_type(&start_type) {
                return Err(Error::new_semantic(
                    format!("Range start must be numeric, found {start_type}"),
                    *span,
                ));
            }
        }

        // Check end expression if present
        if let Some(end_expr) = end {
            let end_type = self.visit_expr(end_expr)?;
            if !self.is_numeric_type(&end_type) {
                return Err(Error::new_semantic(
                    format!("Range end must be numeric, found {end_type}"),
                    *span,
                ));
            }
        }

        Ok(Type::Range)
    }

    fn visit_binary_op(
        &mut self,
        left: &Expr,
        op: &Operator,
        right: &Expr,
        span: &Span,
    ) -> Result<Type> {
        let left_type = self.visit_expr(left)?;
        let right_type = self.visit_expr(right)?;

        match op {
            Operator::Plus
            | Operator::Minus
            | Operator::Multiply
            | Operator::Divide
            | Operator::Modulo => {
                // If either type is Unknown (imported), we can't check types at compile time
                if left_type == Type::Unknown || right_type == Type::Unknown {
                    return Ok(Type::Unknown);
                }

                if !self.is_numeric_type(&left_type) || !self.is_numeric_type(&right_type) {
                    return Err(Error::new_semantic(
                        format!(
                            "Binary operation {op:?} requires numeric types, found {left_type} and {right_type}"
                        ),
                        *span,
                    ));
                }

                // Return the more general type (float if either is float)
                if left_type == Type::Float || right_type == Type::Float {
                    Ok(Type::Float)
                } else {
                    Ok(Type::Int)
                }
            }
            Operator::Equals
            | Operator::NotEquals
            | Operator::LessThan
            | Operator::GreaterThan
            | Operator::LessThanEquals
            | Operator::GreaterThanEquals => {
                // For now, allow comparison of any types (will refine later)
                Ok(Type::Bool)
            }
            Operator::And | Operator::Or => {
                if left_type != Type::Bool || right_type != Type::Bool {
                    return Err(Error::new_semantic(
                        format!(
                            "Logical operation {op:?} requires bool types, found {left_type} and {right_type}"
                        ),
                        *span,
                    ));
                }
                Ok(Type::Bool)
            }
        }
    }

    fn visit_unary_op(&mut self, op: &UnaryOp, expr: &Expr, _span: &Span) -> Result<Type> {
        let expr_type = self.visit_expr(expr)?;

        match op {
            UnaryOp::Neg => {
                if !self.is_numeric_type(&expr_type) {
                    return Err(Error::new_semantic(
                        format!("Unary negation requires numeric type, found {expr_type}"),
                        *_span,
                    ));
                }
                Ok(expr_type)
            }
            UnaryOp::Not => {
                if expr_type != Type::Bool {
                    return Err(Error::new_semantic(
                        format!("Logical NOT requires bool type, found {expr_type}"),
                        *_span,
                    ));
                }
                Ok(Type::Bool)
            }
        }
    }

    fn visit_assign(&mut self, left: &Expr, right: &Expr, span: &Span) -> Result<Type> {
        let right_type = self.visit_expr(right)?;

        match left {
            Expr::Identifier(name, _) => {
                if self.type_env.lookup(name).is_none() {
                    return Err(Error::new_semantic(
                        format!("Cannot assign to undefined variable '{name}'"),
                        *span,
                    ));
                }
                Ok(right_type)
            }
            Expr::MemberAccess(object, _field, _) => {
                let _object_type = self.visit_expr(object)?;
                // TODO: Verify field exists and is assignable
                Ok(right_type)
            }
            Expr::ArrayIndex(array, index, _) => {
                let _array_type = self.visit_expr(array)?;
                let _index_type = self.visit_expr(index)?;
                // TODO: Verify array element is assignable
                Ok(right_type)
            }
            _ => Err(Error::new_semantic(
                "Invalid assignment target".to_string(),
                *span,
            )),
        }
    }

    fn visit_compound_assign(
        &mut self,
        left: &Expr,
        op: &Operator,
        right: &Expr,
        span: &Span,
    ) -> Result<Type> {
        // Compound assignment is like a binary op followed by assignment
        let left_type = self.visit_expr(left)?;
        let right_type = self.visit_expr(right)?;

        // Check the operation is valid
        match op {
            Operator::Plus
            | Operator::Minus
            | Operator::Multiply
            | Operator::Divide
            | Operator::Modulo => {
                if !self.is_numeric_type(&left_type) || !self.is_numeric_type(&right_type) {
                    return Err(Error::new_semantic(
                        format!(
                            "Compound assignment {op:?} requires numeric types, found {left_type} and {right_type}"
                        ),
                        *span,
                    ));
                }
            }
            _ => {
                return Err(Error::new_semantic(
                    format!("Invalid operator for compound assignment: {op:?}"),
                    *span,
                ));
            }
        }

        // Return the type of the assignment (same as left side)
        Ok(left_type)
    }

    fn visit_function_call(&mut self, name: &str, args: &[Expr], span: &Span) -> Result<Type> {
        // Handle method calls with dot notation
        let (func_name, arg_exprs, _needs_self) = if name.contains('.') {
            let (target_var, method_name) = name.split_once('.').unwrap();

            // Get the type of the target variable
            let target_type = self.type_env.lookup(target_var).ok_or_else(|| {
                Error::new_semantic(format!("Undefined variable: {target_var}"), *span)
            })?;

            // Extract struct name from the type
            let (struct_name, _struct_type) = match target_type {
                Type::Struct { name, .. } => {
                    // Get the actual struct fields
                    let fields = self.structs.get(name).ok_or_else(|| {
                        Error::new_semantic(format!("Struct '{name}' not found"), *span)
                    })?;

                    let field_vec: Vec<(String, Type)> =
                        fields.iter().map(|(k, v)| (k.clone(), v.clone())).collect();

                    (
                        name.clone(),
                        Type::Struct {
                            name: name.clone(),
                            fields: field_vec,
                        },
                    )
                }
                _ => {
                    return Err(Error::new_semantic(
                        format!("{target_var} is not a struct instance"),
                        *span,
                    ))
                }
            };

            // Build the full method name
            let full_method_name = format!("{struct_name}::{method_name}");

            // Create a synthetic expression for the self argument
            let mut args_with_self = vec![Expr::Identifier(target_var.to_string(), *span)];
            args_with_self.extend(args.iter().cloned());

            (full_method_name, args_with_self, true)
        } else {
            (name.to_string(), args.to_vec(), false)
        };

        // Check if it's a method call or regular function
        let (param_types, return_type) =
            if let Some((params, ret_type, _)) = self.functions.get(&func_name) {
                (params.clone(), ret_type.clone())
            } else if self.imported_names.contains_key(&func_name) {
                // It's an imported name - we don't know its type yet, so assume it's valid
                // Visit arguments for side effects
                for arg in &arg_exprs {
                    self.visit_expr(arg)?;
                }
                return Ok(Type::Unknown);
            } else if let Some(var_type) = self.type_env.lookup(&func_name) {
                // Check if it's a variable containing a function/closure
                match var_type {
                    Type::Function {
                        params,
                        return_type,
                    } => (
                        params
                            .iter()
                            .map(|t| ("_".to_string(), t.clone()))
                            .collect(),
                        (**return_type).clone(),
                    ),
                    _ => {
                        return Err(Error::new_semantic(
                            format!("'{func_name}' is not a function"),
                            *span,
                        ));
                    }
                }
            } else {
                return Err(Error::new_semantic(
                    format!("Function '{func_name}' not found"),
                    *span,
                ));
            };

        // Special handling for print/println - they accept any arguments
        if func_name == "print" || func_name == "println" {
            for arg in &arg_exprs {
                self.visit_expr(arg)?;
            }
            return Ok(Type::Unit);
        }

        // Special handling for format! - requires at least one string argument
        if func_name == "format!" {
            if arg_exprs.is_empty() {
                return Err(Error::new_semantic(
                    "format! requires at least one argument (the format string)".to_string(),
                    *span,
                ));
            }

            // First argument must be a string
            let format_str_type = self.visit_expr(&arg_exprs[0])?;
            if format_str_type != Type::String {
                return Err(Error::new_semantic(
                    format!("format! first argument must be a string, found {format_str_type}"),
                    *span,
                ));
            }

            // Count placeholders in format string (if it's a literal)
            if let Expr::String(format_str, _) = &arg_exprs[0] {
                let placeholder_count =
                    format_str.matches("{}").count() - format_str.matches("{{}}").count();
                let arg_count = arg_exprs.len() - 1;

                if placeholder_count != arg_count {
                    return Err(Error::new_semantic(
                        format!("format! expects {placeholder_count} arguments after format string, but {arg_count} were provided"),
                        *span,
                    ));
                }
            }

            // Type check remaining arguments
            for arg in &arg_exprs[1..] {
                self.visit_expr(arg)?;
            }

            return Ok(Type::String);
        }

        // Check argument count
        if arg_exprs.len() != param_types.len() {
            return Err(Error::new_semantic(
                format!(
                    "Function '{}' expects {} arguments, but {} were provided",
                    func_name,
                    param_types.len(),
                    arg_exprs.len()
                ),
                *span,
            ));
        }

        // Check argument types
        for (i, (arg, (param_name, expected_type))) in
            arg_exprs.iter().zip(param_types.iter()).enumerate()
        {
            let arg_type = self.visit_expr(arg)?;

            // Special handling for self parameters - only check struct name compatibility
            let types_compatible = if param_name == "self" {
                match (&arg_type, expected_type) {
                    (Type::Struct { name: name1, .. }, Type::Struct { name: name2, .. }) => {
                        name1 == name2
                    }
                    _ => arg_type.is_assignable_to(expected_type),
                }
            } else {
                arg_type.is_assignable_to(expected_type)
            };

            if !types_compatible {
                return Err(Error::new_semantic(
                    format!(
                        "Function '{}' argument {} type mismatch: expected {}, found {}",
                        func_name,
                        i + 1,
                        expected_type,
                        arg_type
                    ),
                    *span,
                ));
            }
        }

        Ok(return_type)
    }

    fn visit_struct_init(
        &mut self,
        name: &str,
        fields: &[(String, Expr)],
        span: &Span,
    ) -> Result<Type> {
        // Check if this is an enum variant construction (e.g., Command::Process { ... })
        if let Some((enum_name, variant_name)) = name.split_once("::") {
            // Check if it's an enum variant
            if let Some(enum_variants) = self.enums.get(enum_name) {
                if let Some(_variant_type) = enum_variants.get(variant_name) {
                    // For struct-like enum variants, we need to check fields
                    // For now, just visit all field expressions and return the enum type
                    for (_, field_expr) in fields {
                        self.visit_expr(field_expr)?;
                    }

                    return Ok(self.resolve_type_by_name(enum_name));
                }

                return Err(Error::new_semantic(
                    format!("Enum '{enum_name}' has no variant '{variant_name}'"),
                    *span,
                ));
            }
        }

        let struct_fields = match self.structs.get(name) {
            Some(fields) => fields.clone(),
            None => {
                // Check if it's an imported struct
                if self.imported_names.contains_key(name) {
                    // Visit field expressions for side effects
                    for (_, field_expr) in fields {
                        self.visit_expr(field_expr)?;
                    }
                    return Ok(Type::Unknown);
                }

                return Err(Error::new_semantic(
                    format!("Struct '{name}' not found"),
                    *span,
                ));
            }
        };

        // Check that all required fields are provided
        if fields.len() != struct_fields.len() {
            return Err(Error::new_semantic(
                format!(
                    "Struct '{}' expects {} fields, but {} were provided",
                    name,
                    struct_fields.len(),
                    fields.len()
                ),
                *span,
            ));
        }

        // Check each field
        for (field_name, field_expr) in fields {
            let expected_type = match struct_fields.get(field_name) {
                Some(ty) => ty.clone(),
                None => {
                    return Err(Error::new_semantic(
                        format!("Struct '{name}' has no field '{field_name}'"),
                        *span,
                    ));
                }
            };

            let field_type = self.visit_expr(field_expr)?;
            if !field_type.is_assignable_to(&expected_type) {
                return Err(Error::new_semantic(
                    format!(
                        "Struct '{name}' field '{field_name}' type mismatch: expected {expected_type}, found {field_type}"
                    ),
                    *span,
                ));
            }
        }

        // Return the complete struct type with proper fields
        Ok(self.resolve_type_by_name(name))
    }

    fn visit_member_access(&mut self, object: &Expr, field: &str, span: &Span) -> Result<Type> {
        // Check if this is an enum variant constructor (e.g., Color.Red)
        if let Expr::Identifier(enum_name, _) = object {
            if let Some(enum_variants) = self.enums.get(enum_name) {
                if enum_variants.contains_key(field) {
                    // This is an enum variant constructor
                    return Ok(Type::Enum {
                        name: enum_name.clone(),
                        variants: enum_variants.clone(),
                    });
                }
            }
        }

        let object_type = self.visit_expr(object)?;

        match &object_type {
            Type::Unknown => {
                // It's an imported value - we don't know its type yet
                Ok(Type::Unknown)
            }
            Type::Struct { name, .. } => {
                let struct_fields = match self.structs.get(name) {
                    Some(fields) => fields,
                    None => {
                        return Err(Error::new_semantic(
                            format!("Struct '{name}' not found in type information"),
                            *span,
                        ));
                    }
                };

                match struct_fields.get(field) {
                    Some(field_type) => Ok(field_type.clone()),
                    None => Err(Error::new_semantic(
                        format!("Struct '{name}' has no field '{field}'"),
                        *span,
                    )),
                }
            }
            Type::Tuple(element_types) => {
                // Handle tuple index access
                if let Ok(index) = field.parse::<usize>() {
                    if index < element_types.len() {
                        Ok(element_types[index].clone())
                    } else {
                        Err(Error::new_semantic(
                            format!(
                                "Tuple index {} out of bounds (tuple has {} elements)",
                                index,
                                element_types.len()
                            ),
                            *span,
                        ))
                    }
                } else {
                    Err(Error::new_semantic(
                        format!("Invalid tuple index '{field}'. Expected a number."),
                        *span,
                    ))
                }
            }
            _ => Err(Error::new_semantic(
                format!(
                    "Cannot access field '{field}' on non-struct/non-tuple type: {object_type}"
                ),
                *span,
            )),
        }
    }

    fn visit_enum_variant_or_method_call(
        &mut self,
        target: &Expr,
        call: &str,
        args: &[Expr],
        span: &Span,
    ) -> Result<Type> {
        // This handles both enum variant construction and method calls
        if let Expr::Identifier(type_name, _) = target {
            // Handle Self:: calls
            let resolved_type_name = if type_name == "Self" {
                match &self.current_impl_type {
                    Some(impl_type) => impl_type.clone(),
                    None => {
                        return Err(Error::new_semantic(
                            "Self can only be used inside impl blocks".to_string(),
                            *span,
                        ));
                    }
                }
            } else {
                type_name.clone()
            };

            // Check if it's an imported type - but don't return Unknown yet!
            // We still need to check for method calls on imported types
            let _is_imported = self.imported_names.contains_key(&resolved_type_name);

            // Check if it's an enum variant
            if let Some(enum_variants) = self.enums.get(&resolved_type_name).cloned() {
                if let Some(variant_types) = enum_variants.get(call) {
                    if !variant_types.is_empty() {
                        if args.len() != variant_types.len() {
                            return Err(Error::new_semantic(
                                format!(
                                    "Enum variant '{}::{}' expects {} argument{}, but {} {} provided",
                                    resolved_type_name,
                                    call,
                                    variant_types.len(),
                                    if variant_types.len() == 1 { "" } else { "s" },
                                    args.len(),
                                    if args.len() == 1 { "was" } else { "were" }
                                ),
                                *span,
                            ));
                        }

                        // Check each argument type
                        for (i, (arg, expected_type)) in
                            args.iter().zip(variant_types.iter()).enumerate()
                        {
                            let arg_type = self.visit_expr(arg)?;
                            // For built-in generic enums (Option, Result), accept any type
                            if (resolved_type_name == "Option" || resolved_type_name == "Result")
                                && *expected_type == Type::Unknown
                            {
                                // Accept any type for generic built-in enums
                            } else if !arg_type.is_assignable_to(expected_type) {
                                return Err(Error::new_semantic(
                                    format!(
                                        "Enum variant '{}::{}' argument {} expects {}, found {}",
                                        resolved_type_name,
                                        call,
                                        i + 1,
                                        expected_type,
                                        arg_type
                                    ),
                                    *span,
                                ));
                            }
                        }
                    } else {
                        // Unit variant (no arguments)
                        if !args.is_empty() {
                            return Err(Error::new_semantic(
                                format!(
                                    "Enum variant '{}::{}' takes no arguments, but {} {} provided",
                                    resolved_type_name,
                                    call,
                                    args.len(),
                                    if args.len() == 1 { "was" } else { "were" }
                                ),
                                *span,
                            ));
                        }
                    }
                    return Ok(self.resolve_type_by_name(&resolved_type_name));
                }
            }

            // Check if it's a method call
            let method_name = format!("{resolved_type_name}::{call}");
            if let Some((param_types, return_type, _)) = self.functions.get(&method_name).cloned() {
                // Check arguments
                if args.len() != param_types.len() {
                    return Err(Error::new_semantic(
                        format!(
                            "Method '{}' expects {} arguments, but {} were provided",
                            method_name,
                            param_types.len(),
                            args.len()
                        ),
                        *span,
                    ));
                }

                for (i, (arg, (param_name, expected_type))) in
                    args.iter().zip(param_types.iter()).enumerate()
                {
                    let arg_type = self.visit_expr(arg)?;

                    // Special handling for self parameters - only check struct name compatibility
                    let types_compatible = if param_name == "self" {
                        match (&arg_type, expected_type) {
                            (
                                Type::Struct { name: name1, .. },
                                Type::Struct { name: name2, .. },
                            ) => name1 == name2,
                            _ => arg_type.is_assignable_to(expected_type),
                        }
                    } else {
                        arg_type.is_assignable_to(expected_type)
                    };

                    if !types_compatible {
                        return Err(Error::new_semantic(
                            format!(
                                "Method '{}' argument {} type mismatch: expected {}, found {}",
                                method_name,
                                i + 1,
                                expected_type,
                                arg_type
                            ),
                            *span,
                        ));
                    }
                }

                return Ok(return_type);
            }
        }

        Err(Error::new_semantic(
            format!("Unknown enum variant or method: {target}::{call}"),
            *span,
        ))
    }

    // ===== Statement visit methods =====

    fn visit_let(&mut self, name: &str, expr: &Expr, _span: &Span) -> Result<Type> {
        let expr_type = self.visit_expr(expr)?;
        self.type_env.define(name.to_string(), expr_type.clone());
        Ok(Type::Unit)
    }

    fn visit_function(
        &mut self,
        name: &str,
        _generic_params: &[String],
        params: &[(String, String)],
        return_type: &str,
        body: &[Stmt],
        span: &Span,
    ) -> Result<Type> {
        // Convert parameter types
        let param_types: Vec<(String, Type)> = params
            .iter()
            .map(|(param_name, type_str)| {
                // Special handling for 'self' parameter
                if param_name == "self" && name.contains("::") {
                    // Extract type name from method name
                    let type_name = name.split("::").next().unwrap();
                    let self_type = self.resolve_type_by_name(type_name);
                    (param_name.clone(), self_type)
                } else {
                    // Resolve the type dynamically - this will use correct enum variants when available
                    let param_type = self.resolve_type_by_name(type_str);
                    (param_name.clone(), param_type)
                }
            })
            .collect();

        // Parse return type using dynamic resolution
        let ret_type = self.resolve_type_by_name(return_type);

        // Function signature already registered in collect_function_signatures
        // No need to re-register here as it was done in the first pass

        // Create new scope for function body
        self.type_env.push_scope();

        // Add parameters to scope
        for (param_name, param_type) in &param_types {
            // Special handling for 'self' parameter in methods
            if param_name == "self" {
                // Extract type name from method name (e.g., "Point::new" -> "Point")
                if let Some(type_name) = name.split("::").next() {
                    // Check if it's a struct or enum
                    let self_type = if self.structs.contains_key(type_name) {
                        Type::Struct {
                            name: type_name.to_string(),
                            fields: vec![],
                        }
                    } else if self.enums.contains_key(type_name) {
                        Type::Enum {
                            name: type_name.to_string(),
                            variants: HashMap::new(),
                        }
                    } else {
                        Type::Unknown
                    };

                    self.type_env.define("self".to_string(), self_type);
                }
            } else {
                self.type_env.define(param_name.clone(), param_type.clone());
            }
        }

        // Analyze function body
        for stmt in body {
            self.visit_stmt(stmt)?;
        }

        // Check return type matches last expression (if any)
        if let Some(Stmt::Expression(expr, has_semicolon)) = body.last() {
            // If the expression has a semicolon, it's a statement that returns unit
            if !has_semicolon {
                let expr_type = self.visit_expr(expr)?;
                if !expr_type.is_assignable_to(&ret_type) {
                    self.type_env.pop_scope();
                    return Err(Error::new_semantic(
                        format!(
                            "Function {name} return type mismatch: expected {ret_type}, found {expr_type}"
                        ),
                        *span,
                    ));
                }
            }
        }

        // Clean up scope
        self.type_env.pop_scope();

        Ok(Type::Unit)
    }

    fn visit_struct(
        &mut self,
        name: &str,
        _generic_params: &[String],
        fields: &[(String, String)],
        _span: &Span,
    ) -> Result<Type> {
        // Register struct type
        self.type_env.define(
            name.to_string(),
            Type::Struct {
                name: name.to_string(),
                fields: vec![], // Fields stored separately
            },
        );

        // Convert and store fields
        let mut struct_fields = HashMap::new();
        for (field_name, field_type_str) in fields {
            // First try to look up the type in the type environment
            let field_type = if let Some(registered_type) = self.type_env.lookup(field_type_str) {
                registered_type.clone()
            } else {
                // Fall back to Type::from_string for built-in types
                Type::from_string(field_type_str).unwrap_or(Type::Unknown)
            };
            struct_fields.insert(field_name.clone(), field_type);
        }

        self.structs.insert(name.to_string(), struct_fields);
        Ok(Type::Unit)
    }

    fn visit_enum(
        &mut self,
        name: &str,
        _generic_params: &[String],
        variants: &[crate::parser::EnumVariant],
        _span: &Span,
    ) -> Result<Type> {
        // Register enum type
        self.type_env.define(
            name.to_string(),
            Type::Enum {
                name: name.to_string(),
                variants: HashMap::new(), // Variants stored separately
            },
        );

        // Convert and store variants
        let mut enum_variants = HashMap::new();
        for variant in variants {
            match variant {
                crate::parser::EnumVariant::Unit(name) => {
                    enum_variants.insert(name.clone(), vec![]);
                }
                crate::parser::EnumVariant::Tuple(name, type_strs) => {
                    let variant_types: Vec<Type> = type_strs
                        .iter()
                        .map(|t| self.resolve_type_by_name(t))
                        .collect();
                    enum_variants.insert(name.clone(), variant_types);
                }
                crate::parser::EnumVariant::Struct(name, fields) => {
                    // For struct variants, we'll create a struct type
                    let struct_fields: Vec<(String, crate::types::Type)> = fields
                        .iter()
                        .map(|(field_name, field_type_str)| {
                            (
                                field_name.clone(),
                                self.resolve_type_by_name(field_type_str),
                            )
                        })
                        .collect();

                    enum_variants.insert(
                        name.clone(),
                        vec![crate::types::Type::Struct {
                            name: name.clone(),
                            fields: struct_fields,
                        }],
                    );
                }
            }
        }

        self.enums.insert(name.to_string(), enum_variants);
        Ok(Type::Unit)
    }

    fn visit_impl(&mut self, struct_name: &str, methods: &[Stmt], _span: &Span) -> Result<Type> {
        // Set current impl type for Self resolution
        self.current_impl_type = Some(struct_name.to_string());

        // First pass: register all method signatures
        for method in methods {
            if let Stmt::Function(_, _, name, _, params, return_type, _body, method_span) = method {
                // Convert parameter types
                let mut param_types: Vec<(String, Type)> = vec![];

                // Check if first parameter is 'self' (instance method) or not (static method)
                let is_instance_method = !params.is_empty() && params[0].0 == "self";

                if is_instance_method {
                    // For instance methods, add the appropriate type as the first parameter
                    let self_type = if self.structs.contains_key(struct_name) {
                        Type::Struct {
                            name: struct_name.to_string(),
                            fields: vec![], // Fields will be filled in by the struct definition
                        }
                    } else if self.enums.contains_key(struct_name) {
                        Type::Enum {
                            name: struct_name.to_string(),
                            variants: HashMap::new(), // Variants stored separately
                        }
                    } else {
                        Type::Unknown
                    };

                    param_types.push(("self".to_string(), self_type));

                    // Then add remaining parameters
                    for (param_name, param_type_str) in params.iter().skip(1) {
                        param_types.push((
                            param_name.clone(),
                            self.resolve_type_by_name(param_type_str),
                        ));
                    }
                } else {
                    // Include all parameters for static methods
                    for (param_name, param_type_str) in params.iter() {
                        param_types.push((
                            param_name.clone(),
                            self.resolve_type_by_name(param_type_str),
                        ));
                    }
                }

                let ret_type = self.resolve_type_by_name(return_type);

                // Register method (only if not already registered from collect_function_signatures)
                let method_name = format!("{struct_name}::{name}");
                self.functions
                    .entry(method_name)
                    .or_insert_with(|| (param_types.clone(), ret_type.clone(), *method_span));
            }
        }

        // Second pass: analyze method bodies
        for method in methods {
            if let Stmt::Function(_, _, name, _, params, return_type, body, method_span) = method {
                // Now analyze the method body just like a regular function
                self.visit_function(
                    &format!("{struct_name}::{name}"),
                    &[], // No generic params for now
                    params,
                    return_type,
                    body,
                    method_span,
                )?;
            }
        }

        // Clear current impl type
        self.current_impl_type = None;

        Ok(Type::Unit)
    }

    fn visit_match(
        &mut self,
        expr: &Expr,
        arms: &[(Expr, Vec<Stmt>)],
        span: &Span,
    ) -> Result<Type> {
        let expr_type = self.visit_expr(expr)?;

        // For enum matching, track which variants have been matched
        // We need to distinguish between exhaustive patterns (with bindings) and specific patterns (with literals)
        let mut matched_variants = std::collections::HashSet::new();
        let mut exhaustive_variants = std::collections::HashSet::new(); // Variants with binding patterns
        let mut has_wildcard = false;
        let mut wildcard_position = None;
        let enum_name = match &expr_type {
            Type::Enum { name, .. } => Some(name.clone()),
            _ => None,
        };

        // Track the type of each arm for consistency checking
        let mut arm_types = Vec::new();

        for (i, (pattern, body)) in arms.iter().enumerate() {
            // Save current match-bound variables
            let saved_vars = self.match_bound_vars.clone();

            // Analyze pattern and bind variables
            match pattern {
                Expr::Identifier(name, _) if name == "_" => {
                    // Check for unreachable patterns after wildcard
                    if has_wildcard {
                        return Err(Error::new_semantic(
                            format!(
                                "Unreachable pattern: wildcard already exists at arm {}",
                                wildcard_position.unwrap()
                            ),
                            pattern.span(),
                        ));
                    }
                    has_wildcard = true;
                    wildcard_position = Some(i);
                }
                Expr::EnumVariantOrMethodCall {
                    target, call, args, ..
                } => {
                    // Check for unreachable patterns after wildcard
                    if has_wildcard {
                        return Err(Error::new_semantic(
                            format!(
                                "Unreachable pattern: wildcard already covers this case at arm {}",
                                wildcard_position.unwrap()
                            ),
                            pattern.span(),
                        ));
                    }

                    if let Expr::Identifier(enum_type, _) = &**target {
                        if let Some(ref expected_enum) = enum_name {
                            if enum_type != expected_enum {
                                return Err(Error::new_semantic(
                                    format!("Expected enum '{expected_enum}', found '{enum_type}'"),
                                    pattern.span(),
                                ));
                            }
                        }

                        // Check if this pattern has already been exhaustively matched
                        if exhaustive_variants.contains(call) {
                            return Err(Error::new_semantic(
                                format!("Unreachable pattern: enum variant '{enum_type}::{call}' already has a catch-all pattern"),
                                pattern.span(),
                            ));
                        }

                        // Check if this is a binding pattern or a literal pattern
                        let is_exhaustive = if args.is_empty() {
                            // Unit variant is always exhaustive
                            true
                        } else {
                            // Check if the argument is a binding (variable) or a literal
                            matches!(&args[0], Expr::Identifier(name, _) if name != "_")
                        };

                        if is_exhaustive {
                            // This pattern catches all values for this variant
                            exhaustive_variants.insert(call.clone());
                        } else {
                            // This is a specific pattern (literal or wildcard)
                            // Only check for exact duplicate patterns
                            // Note: In a complete implementation, we'd track the specific literal values
                        }

                        // Track this variant as matched (for exhaustiveness checking)
                        matched_variants.insert(call.clone());

                        // Bind variables from enum variant
                        if !args.is_empty() {
                            // Get the variant types for this enum variant
                            if let Some(enum_variants) = self.enums.get(enum_type) {
                                if let Some(variant_types) = enum_variants.get(call) {
                                    // Check that the number of arguments matches the variant's arity
                                    if args.len() != variant_types.len() {
                                        return Err(Error::new_semantic(
                                            format!(
                                                "Enum variant '{}::{}' expects {} arguments, but {} were provided",
                                                enum_type, call, variant_types.len(), args.len()
                                            ),
                                            pattern.span(),
                                        ));
                                    }

                                    // Bind each argument to its corresponding type
                                    for (arg, variant_type) in args.iter().zip(variant_types.iter())
                                    {
                                        match arg {
                                            Expr::Identifier(var_name, _) if var_name != "_" => {
                                                // Simple identifier binding
                                                // For built-in generic enums with Unknown type, infer from usage context
                                                if (enum_type == "Option" || enum_type == "Result")
                                                    && *variant_type == Type::Unknown
                                                {
                                                    // For now, bind as Unknown and let type inference figure it out
                                                    self.match_bound_vars
                                                        .insert(var_name.clone(), Type::Unknown);
                                                } else {
                                                    self.match_bound_vars.insert(
                                                        var_name.clone(),
                                                        variant_type.clone(),
                                                    );
                                                }
                                            }
                                            Expr::StructPattern(_struct_name, fields, _) => {
                                                // Nested struct pattern (e.g., Ok(Command::Process { input, output }))
                                                if let Type::Struct {
                                                    name: _,
                                                    fields: struct_fields,
                                                } = variant_type
                                                {
                                                    // Convert Vec<(String, Type)> to HashMap for easy lookup
                                                    let field_types: HashMap<String, Type> =
                                                        struct_fields.iter().cloned().collect();

                                                    // Bind variables from the struct pattern fields
                                                    for (field_name, opt_var_name) in fields {
                                                        let field_type = field_types
                                                            .get(field_name)
                                                            .cloned()
                                                            .unwrap_or(Type::Unknown);

                                                        if let Some(var_name) = opt_var_name {
                                                            // Field bound to a different variable name
                                                            self.match_bound_vars.insert(
                                                                var_name.clone(),
                                                                field_type,
                                                            );
                                                        } else {
                                                            // Shorthand: field name is also the variable name
                                                            self.match_bound_vars.insert(
                                                                field_name.clone(),
                                                                field_type,
                                                            );
                                                        }
                                                    }
                                                }
                                            }
                                            _ => {
                                                // Other patterns (literals, etc.) don't bind variables
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                Expr::Tuple(elements, _) => {
                    // Handle tuple patterns
                    if has_wildcard {
                        return Err(Error::new_semantic(
                            format!(
                                "Unreachable pattern: wildcard already covers this case at arm {}",
                                wildcard_position.unwrap()
                            ),
                            pattern.span(),
                        ));
                    }

                    // Check that the expression being matched is also a tuple
                    if let Type::Tuple(elem_types) = &expr_type {
                        if elements.len() != elem_types.len() {
                            return Err(Error::new_semantic(
                                format!(
                                    "Tuple pattern has {} elements but expression has {}",
                                    elements.len(),
                                    elem_types.len()
                                ),
                                pattern.span(),
                            ));
                        }

                        // Check each element of the tuple pattern
                        for (i, (elem_pattern, elem_type)) in
                            elements.iter().zip(elem_types.iter()).enumerate()
                        {
                            match elem_pattern {
                                Expr::Identifier(name, _) if name == "_" => {
                                    // Wildcard, no binding needed
                                }
                                Expr::Identifier(name, _) => {
                                    // Bind the variable to the element type
                                    self.match_bound_vars
                                        .insert(name.clone(), elem_type.clone());
                                }
                                Expr::Int(_, _) | Expr::String(_, _) | Expr::Bool(_, _) => {
                                    // Literal patterns - just check type compatibility
                                    let literal_type = self.visit_expr(elem_pattern)?;
                                    if literal_type != *elem_type {
                                        return Err(Error::new_semantic(
                                            format!("Pattern type mismatch at tuple element {i}: expected {elem_type}, found {literal_type}"),
                                            elem_pattern.span(),
                                        ));
                                    }
                                }
                                _ => {
                                    // For now, other nested patterns are not supported
                                    return Err(Error::new_semantic(
                                        format!("Unsupported pattern in tuple at element {i}"),
                                        elem_pattern.span(),
                                    ));
                                }
                            }
                        }
                    } else {
                        return Err(Error::new_semantic(
                            format!(
                                "Cannot match tuple pattern against non-tuple type {expr_type}"
                            ),
                            pattern.span(),
                        ));
                    }
                }
                Expr::StructPattern(pattern_name, fields, _) => {
                    // Handle struct-like enum variant patterns (e.g., Command::Process { input, output })
                    if has_wildcard {
                        return Err(Error::new_semantic(
                            format!(
                                "Unreachable pattern: wildcard already covers this case at arm {}",
                                wildcard_position.unwrap()
                            ),
                            pattern.span(),
                        ));
                    }

                    // Check if this is an enum variant pattern
                    if let Some((enum_type_name, variant_name)) = pattern_name.split_once("::") {
                        // Verify it matches the expression type
                        if let Type::Enum { name, .. } = &expr_type {
                            if name != enum_type_name {
                                return Err(Error::new_semantic(
                                    format!(
                                        "Pattern type mismatch: expected {expr_type}, found {enum_type_name}"
                                    ),
                                    pattern.span(),
                                ));
                            }

                            // Track this variant as matched
                            matched_variants.insert(variant_name.to_string());

                            // Look up the variant's field types
                            let field_types =
                                if let Some(enum_variants) = self.enums.get(enum_type_name) {
                                    if let Some(variant_types) = enum_variants.get(variant_name) {
                                        if variant_types.len() == 1 {
                                            if let Type::Struct { fields, .. } = &variant_types[0] {
                                                // Convert Vec<(String, Type)> to HashMap for easy lookup
                                                fields
                                                    .iter()
                                                    .cloned()
                                                    .collect::<HashMap<String, Type>>()
                                            } else {
                                                HashMap::new()
                                            }
                                        } else {
                                            HashMap::new()
                                        }
                                    } else {
                                        HashMap::new()
                                    }
                                } else {
                                    HashMap::new()
                                };

                            // Bind variables from the struct pattern fields
                            for (field_name, opt_var_name) in fields {
                                // Skip the special ".." rest pattern marker
                                if field_name == ".." {
                                    continue;
                                }

                                let field_type = field_types
                                    .get(field_name)
                                    .cloned()
                                    .unwrap_or(Type::Unknown);

                                if let Some(var_name) = opt_var_name {
                                    // Field bound to a different variable name
                                    self.match_bound_vars.insert(var_name.clone(), field_type);
                                } else {
                                    // Shorthand: field name is also the variable name
                                    self.match_bound_vars.insert(field_name.clone(), field_type);
                                }
                            }
                        } else {
                            return Err(Error::new_semantic(
                                format!(
                                    "Cannot match enum pattern against non-enum type {expr_type}"
                                ),
                                pattern.span(),
                            ));
                        }
                    } else {
                        // Handle plain struct patterns (e.g., Config { host, port, .. })
                        if let Type::Struct { name, .. } = &expr_type {
                            if name != pattern_name {
                                return Err(Error::new_semantic(
                                    format!(
                                        "Pattern type mismatch: expected {expr_type}, found {pattern_name}"
                                    ),
                                    pattern.span(),
                                ));
                            }

                            // Look up the struct's field types
                            let field_types =
                                if let Some(struct_fields) = self.structs.get(pattern_name) {
                                    // struct_fields is already HashMap<String, Type>
                                    struct_fields.clone()
                                } else {
                                    HashMap::new()
                                };

                            // Bind variables from the struct pattern fields
                            for (field_name, opt_var_name) in fields {
                                // Skip the special ".." rest pattern marker
                                if field_name == ".." {
                                    continue;
                                }

                                let field_type = field_types
                                    .get(field_name)
                                    .cloned()
                                    .unwrap_or(Type::Unknown);

                                if let Some(var_name) = opt_var_name {
                                    // Field bound to a different variable name
                                    self.match_bound_vars.insert(var_name.clone(), field_type);
                                } else {
                                    // Shorthand: field name is also the variable name
                                    self.match_bound_vars.insert(field_name.clone(), field_type);
                                }
                            }
                        } else {
                            return Err(Error::new_semantic(
                                format!(
                                    "Cannot match struct pattern {pattern_name} against non-struct type {expr_type}"
                                ),
                                pattern.span(),
                            ));
                        }
                    }
                }
                Expr::FunctionCall(name, args, _) => {
                    // Handle implicit Result/Option constructors (Ok, Err, Some, None)
                    if matches!(name.as_str(), "Ok" | "Err" | "Some" | "None") {
                        // Determine which enum we're dealing with
                        let (enum_name, variant_name) = match name.as_str() {
                            "Ok" | "Err" => ("Result", name.as_str()),
                            "Some" | "None" => ("Option", name.as_str()),
                            _ => unreachable!(),
                        };

                        // Check if we're matching the right enum type
                        match &expr_type {
                            Type::Enum {
                                name: matched_enum, ..
                            } if matched_enum == enum_name => {
                                // Track this variant as matched
                                matched_variants.insert(variant_name.to_string());

                                // Bind variables from the argument
                                if !args.is_empty() {
                                    match &args[0] {
                                        Expr::Identifier(var_name, _) if var_name != "_" => {
                                            // Simple identifier binding
                                            self.match_bound_vars
                                                .insert(var_name.clone(), Type::Unknown);
                                        }
                                        Expr::StructPattern(_struct_name, fields, _) => {
                                            // Nested struct pattern (e.g., Ok(Command::Process { input, output }))
                                            // We need to infer the type from context or use Unknown
                                            for (field_name, opt_var_name) in fields {
                                                if let Some(var_name) = opt_var_name {
                                                    self.match_bound_vars
                                                        .insert(var_name.clone(), Type::Unknown);
                                                } else {
                                                    self.match_bound_vars
                                                        .insert(field_name.clone(), Type::Unknown);
                                                }
                                            }
                                        }
                                        _ => {
                                            // Other patterns don't bind variables
                                        }
                                    }
                                }
                            }
                            _ => {
                                return Err(Error::new_semantic(
                                    format!("Cannot match {name} pattern against type {expr_type}"),
                                    pattern.span(),
                                ));
                            }
                        }
                    } else {
                        // Regular function call patterns - just check type compatibility
                        let pattern_type = self.visit_expr(pattern)?;
                        if pattern_type != expr_type {
                            return Err(Error::new_semantic(
                                format!(
                                    "Pattern type mismatch: expected {expr_type}, found {pattern_type}"
                                ),
                                pattern.span(),
                            ));
                        }
                    }
                }
                _ => {
                    // For now, other patterns just check type compatibility
                    let pattern_type = self.visit_expr(pattern)?;
                    if pattern_type != expr_type {
                        return Err(Error::new_semantic(
                            format!(
                                "Pattern type mismatch: expected {expr_type}, found {pattern_type}"
                            ),
                            pattern.span(),
                        ));
                    }
                }
            }

            // Analyze body with bound variables and get its type
            let arm_type = self.visit_block(body, span)?;
            arm_types.push(arm_type);

            // Restore match-bound variables
            self.match_bound_vars = saved_vars;
        }

        // Check for exhaustiveness (only for enums for now)
        if let Some(enum_name) = enum_name {
            if let Some(enum_variants) = self.enums.get(&enum_name) {
                // Use the tracked wildcard instead of re-scanning
                if !has_wildcard {
                    for variant_name in enum_variants.keys() {
                        if !matched_variants.contains(variant_name) {
                            return Err(Error::new_semantic(
                                format!(
                                    "Non-exhaustive match: variant '{enum_name}::{variant_name}' not covered"
                                ),
                                *span,
                            ));
                        }
                    }
                }
            }
        }

        // Check that all arms return the same type
        if arm_types.is_empty() {
            return Ok(Type::Unit);
        }

        let first_arm_type = &arm_types[0];
        for (i, arm_type) in arm_types.iter().enumerate().skip(1) {
            // If either type is Unknown, allow it (for generic types)
            if *arm_type != Type::Unknown
                && *first_arm_type != Type::Unknown
                && arm_type != first_arm_type
            {
                // Special case: if both are the same enum type but one has more specific type info, allow it
                match (first_arm_type, arm_type) {
                    (Type::Enum { name: name1, .. }, Type::Enum { name: name2, .. })
                        if name1 == name2 =>
                    {
                        // Same enum type (e.g., both Result), allow even if generic parameters differ
                        continue;
                    }
                    _ => {
                        return Err(Error::new_semantic(
                            format!("Match arms have inconsistent types: arm 0 returns {first_arm_type}, arm {i} returns {arm_type}"),
                            *span,
                        ));
                    }
                }
            }
        }

        Ok(first_arm_type.clone())
    }

    fn visit_for_loop(
        &mut self,
        pattern: &Expr,
        iterable: &Expr,
        body: &[Stmt],
        _span: &Span,
    ) -> Result<Type> {
        let iterable_type = self.visit_expr(iterable)?;

        // Create new scope for loop
        self.type_env.push_scope();

        // Determine loop variable type based on iterable
        let element_type = match iterable_type {
            Type::Array(ref element_type) => *element_type.clone(),
            Type::Range => Type::Int,
            _ => {
                self.type_env.pop_scope();
                return Err(Error::new_semantic(
                    format!("Cannot iterate over type: {iterable_type}"),
                    iterable.span(),
                ));
            }
        };

        // Handle different pattern types
        match pattern {
            Expr::Identifier(name, _) => {
                // Simple identifier case
                self.type_env.define(name.clone(), element_type);
            }
            Expr::Tuple(elements, _) => {
                // Tuple destructuring case
                match element_type {
                    Type::Tuple(ref tuple_types) => {
                        if elements.len() != tuple_types.len() {
                            self.type_env.pop_scope();
                            return Err(Error::new_semantic(
                                format!("Tuple pattern has {} elements but iterable contains tuples with {} elements",
                                    elements.len(), tuple_types.len()),
                                pattern.span(),
                            ));
                        }

                        // Bind each tuple element to its corresponding variable
                        for (elem_pattern, elem_type) in elements.iter().zip(tuple_types.iter()) {
                            match elem_pattern {
                                Expr::Identifier(name, _) => {
                                    self.type_env.define(name.clone(), elem_type.clone());
                                }
                                _ => {
                                    self.type_env.pop_scope();
                                    return Err(Error::new_semantic(
                                        "Only identifiers are supported in tuple patterns"
                                            .to_string(),
                                        elem_pattern.span(),
                                    ));
                                }
                            }
                        }
                    }
                    _ => {
                        self.type_env.pop_scope();
                        return Err(Error::new_semantic(
                            format!("Cannot destructure non-tuple type {element_type} in for loop"),
                            pattern.span(),
                        ));
                    }
                }
            }
            _ => {
                self.type_env.pop_scope();
                return Err(Error::new_semantic(
                    "Only identifiers and tuples are supported in for loop patterns".to_string(),
                    pattern.span(),
                ));
            }
        }

        // Track loop depth for break/continue
        self.loop_depth += 1;

        // Analyze loop body
        for stmt in body {
            self.visit_stmt(stmt)?;
        }

        self.loop_depth -= 1;
        self.type_env.pop_scope();

        Ok(Type::Unit)
    }

    fn visit_while(&mut self, condition: &Expr, body: &[Stmt], _span: &Span) -> Result<Type> {
        // Check condition is boolean
        let condition_type = self.visit_expr(condition)?;
        if condition_type != Type::Bool {
            return Err(Error::new_semantic(
                format!("While condition must be boolean, found {condition_type}"),
                condition.span(),
            ));
        }

        // Track loop depth for break/continue
        self.loop_depth += 1;

        // Analyze loop body
        for stmt in body {
            self.visit_stmt(stmt)?;
        }

        self.loop_depth -= 1;

        Ok(Type::Unit)
    }

    fn visit_loop(&mut self, body: &[Stmt], _span: &Span) -> Result<Type> {
        // Track loop depth for break/continue
        self.loop_depth += 1;

        // Analyze loop body
        for stmt in body {
            self.visit_stmt(stmt)?;
        }

        self.loop_depth -= 1;

        Ok(Type::Unit)
    }

    fn visit_break(&mut self, span: &Span) -> Result<Type> {
        if self.loop_depth == 0 {
            return Err(Error::new_semantic(
                "'break' outside of loop".to_string(),
                *span,
            ));
        }
        Ok(Type::Unit)
    }

    fn visit_continue(&mut self, span: &Span) -> Result<Type> {
        if self.loop_depth == 0 {
            return Err(Error::new_semantic(
                "'continue' outside of loop".to_string(),
                *span,
            ));
        }
        Ok(Type::Unit)
    }

    fn visit_return(&mut self, expr: Option<&Expr>, _span: &Span) -> Result<Type> {
        // TODO: Implement return statement semantic analysis
        // - Check if we're inside a function
        // - Type-check the return expression against function return type
        // - Handle both `return;` and `return expr;` forms
        match expr {
            Some(return_expr) => {
                let _return_type = self.visit_expr(return_expr)?;
                // TODO: Validate against current function's return type
                Ok(Type::Unit)
            }
            None => {
                // `return;` - returns Unit
                Ok(Type::Unit)
            }
        }
    }

    fn visit_expression_stmt(&mut self, expr: &Expr, _has_semicolon: bool) -> Result<Type> {
        // For now, just analyze the expression
        // Later, when implementing expression-based semantics, we'll use has_semicolon
        self.visit_expr(expr)?;
        Ok(Type::Unit)
    }

    fn visit_extern_function(
        &mut self,
        name: &str,
        _generic_params: &[String],
        params: &[(String, String)],
        return_type: &str,
        _span: &Span,
    ) -> Result<Type> {
        // Convert parameter types
        let param_types: Vec<(String, Type)> = params
            .iter()
            .map(|(param_name, type_str)| {
                (
                    param_name.clone(),
                    Type::from_string(type_str).unwrap_or(Type::Unknown),
                )
            })
            .collect();

        let ret_type = Type::from_string(return_type).unwrap_or(Type::Unknown);

        // Register extern function signature
        self.functions
            .insert(name.to_string(), (param_types, ret_type.clone(), *_span));

        // Also register as imported name
        self.imported_names.insert(
            name.to_string(),
            Type::Function {
                params: vec![], // We don't track parameter types in imported_names
                return_type: Box::new(ret_type),
            },
        );

        Ok(Type::Unit)
    }

    fn visit_extern_mod(&mut self, name: &str, items: &[ExternItem], _span: &Span) -> Result<Type> {
        // Process all items in the extern mod block with the module name as prefix
        for item in items {
            self.process_extern_item(item, name)?;
        }
        Ok(Type::Unit)
    }

    fn visit_extern_type(
        &mut self,
        name: &str,
        _generic_params: &[String],
        _span: &Span,
    ) -> Result<Type> {
        // Register the extern type in our type system
        // For now, treat all extern types as Unknown/Any since they're from external systems
        self.type_env.define(name.to_string(), Type::Unknown);
        self.imported_names.insert(name.to_string(), Type::Unknown);
        Ok(Type::Unit)
    }

    fn visit_async_function(
        &mut self,
        name: &str,
        _generic_params: &[String],
        params: &[(String, String)],
        return_type: &str,
        body: &[Stmt],
        span: &Span,
    ) -> Result<Type> {
        // Track that we're in an async function
        let prev_async = self.in_async_function;
        self.in_async_function = true;

        // Convert parameter types
        let param_types: Vec<(String, Type)> = params
            .iter()
            .map(|(param_name, type_str)| {
                (
                    param_name.clone(),
                    Type::from_string(type_str).unwrap_or(Type::Unknown),
                )
            })
            .collect();

        // Parse return type and wrap in Promise
        let inner_return_type = Type::from_string(return_type).unwrap_or(Type::Unknown);
        let async_return_type = Type::Promise(Box::new(inner_return_type.clone()));

        // Store function signature with Promise return type
        self.functions.insert(
            name.to_string(),
            (param_types.clone(), async_return_type.clone(), *span),
        );

        // Create a new scope for the function body
        self.type_env.push_scope();

        // Add parameters to the new scope
        for (param_name, param_type) in &param_types {
            self.type_env.define(param_name.clone(), param_type.clone());
        }

        // Analyze function body
        for stmt in body {
            self.visit_stmt(stmt)?;
        }

        // Pop the function scope
        self.type_env.pop_scope();

        // Restore async context
        self.in_async_function = prev_async;

        Ok(Type::Unit)
    }

    fn visit_module(&mut self, _name: &str, body: &[Stmt], _span: &Span) -> Result<Type> {
        // Process module body statements
        for stmt in body {
            self.visit_stmt(stmt)?;
        }
        Ok(Type::Unit)
    }

    fn visit_match_expr(
        &mut self,
        expr: &Expr,
        arms: &[(Expr, Vec<Stmt>)],
        span: &Span,
    ) -> Result<Type> {
        // For now, use the same implementation as visit_match
        self.visit_match(expr, arms, span)
    }

    fn visit_await(&mut self, expr: &Expr, span: &Span) -> Result<Type> {
        // Check that we're in an async function
        if !self.in_async_function {
            return Err(Error::new_semantic(
                ".await can only be used inside async functions",
                *span,
            ));
        }

        // Type check the expression
        let expr_type = self.visit_expr(expr)?;

        // Check if the expression is a Promise and unwrap it
        match &expr_type {
            Type::Promise(inner) => Ok((**inner).clone()),
            Type::Unknown => Ok(Type::Unknown), // Allow unknown types for extern functions
            _ => Err(Error::new_semantic(
                format!(".await can only be used on Promise types, found {expr_type}"),
                *span,
            )),
        }
    }

    fn visit_try(&mut self, expr: &Expr, span: &Span) -> Result<Type> {
        // Type check the expression
        let expr_type = self.visit_expr(expr)?;

        // Check if the expression is a Result type and unwrap the Ok type
        match &expr_type {
            Type::Enum { name, .. } if name == "Result" => {
                // For built-in Result type, we assume it has Ok and Err variants
                // In a proper implementation, we'd track the generic type parameters
                // For now, return Unknown to allow the operation
                Ok(Type::Unknown)
            }
            Type::Unknown => Ok(Type::Unknown), // Allow unknown types for extern functions
            _ => Err(Error::new_semantic(
                format!("? operator can only be used on Result types, found {expr_type}"),
                *span,
            )),
        }
    }

    fn visit_await_try(&mut self, expr: &Expr, span: &Span) -> Result<Type> {
        // Check that we're in an async function
        if !self.in_async_function {
            return Err(Error::new_semantic(
                ".await? can only be used inside async functions",
                *span,
            ));
        }

        // Type check the expression
        let expr_type = self.visit_expr(expr)?;

        // Check if the expression is a Promise and convert it to Result
        match &expr_type {
            Type::Promise(_inner) => {
                // .await? converts Promise<T> to Result<T, JsError>
                // For now, we'll return Result<T, Unknown> to represent Result<T, JsError>
                Ok(Type::Enum {
                    name: "Result".to_string(),
                    variants: std::collections::HashMap::new(), // Will be filled by standard library
                })
            }
            Type::Unknown => Ok(Type::Enum {
                name: "Result".to_string(),
                variants: std::collections::HashMap::new(),
            }), // Allow unknown types for extern functions
            _ => Err(Error::new_semantic(
                format!(".await? can only be used on Promise types, found {expr_type}"),
                *span,
            )),
        }
    }

    fn visit_closure(
        &mut self,
        params: &[(String, Option<String>)],
        ret_type: &Option<String>,
        body: &Expr,
        _span: &Span,
    ) -> Result<Type> {
        // Enter a new scope for the closure
        self.type_env.push_scope();

        // Process parameters and add them to the scope
        let mut param_types = Vec::new();
        for (param_name, param_type_str) in params {
            let param_type = if let Some(type_str) = param_type_str {
                Type::from_string(type_str).unwrap_or(Type::Unknown)
            } else {
                Type::Unknown // Type inference not implemented yet
            };

            self.type_env.define(param_name.clone(), param_type.clone());
            param_types.push(param_type);
        }

        // Type check the body
        let body_type = self.visit_expr(body)?;

        // Check return type if specified
        let actual_return_type = if let Some(ret_type_str) = ret_type {
            let expected_type = Type::from_string(ret_type_str).unwrap_or(Type::Unknown);

            if !body_type.is_assignable_to(&expected_type) {
                return Err(Error::new_semantic(
                    format!("Closure body returns {body_type} but expected {expected_type}"),
                    body.span(),
                ));
            }

            expected_type
        } else {
            body_type
        };

        // Exit the closure scope
        self.type_env.pop_scope();

        // Return the function type
        Ok(Type::Function {
            params: param_types,
            return_type: Box::new(actual_return_type),
        })
    }

    fn visit_use(&mut self, path: &UsePath, items: &UseItems, span: &Span) -> Result<Type> {
        match &path.prefix {
            UsePrefix::None => {
                let package_name = &path.segments[0];

                // Check if it's a Node.js built-in module
                if is_nodejs_builtin(package_name) {
                    // Register imported names as Unknown for Node.js built-ins
                    match items {
                        UseItems::Named(imports) => {
                            for (import_name, alias) in imports {
                                let name_to_register = alias.as_ref().unwrap_or(import_name);
                                self.imported_names
                                    .insert(name_to_register.clone(), Type::Unknown);
                            }
                        }
                        UseItems::Single => {
                            self.imported_names
                                .insert(package_name.clone(), Type::Unknown);
                        }
                        UseItems::All => {
                            // For wildcard imports, we can't pre-register names
                        }
                    }
                    return Ok(Type::Unit);
                }

                // Check if it's an external:: import
                let (actual_package_name, is_external_import) =
                    if package_name == "external" && path.segments.len() > 1 {
                        (&path.segments[1], true)
                    } else {
                        (package_name, false)
                    };

                // External package - use package resolver if available
                if let Some(ref mut resolver) = self.package_resolver {
                    match resolver.resolve_package(actual_package_name) {
                        Ok(_resolved_package) => {
                            // Register imported names as Unknown for now
                            // In a full implementation, we'd load type definitions
                            match items {
                                UseItems::Named(imports) => {
                                    for (import_name, alias) in imports {
                                        let name_to_register =
                                            alias.as_ref().unwrap_or(import_name);
                                        self.imported_names
                                            .insert(name_to_register.clone(), Type::Unknown);
                                    }
                                }
                                UseItems::All => {
                                    // For wildcard imports, we can't pre-register names
                                }
                                UseItems::Single => {
                                    // Handle external::package::default specially
                                    if is_external_import
                                        && path.segments.len() == 3
                                        && path.segments[2] == "default"
                                    {
                                        // Register the package name (express) instead of "default"
                                        self.imported_names
                                            .insert(actual_package_name.clone(), Type::Unknown);
                                    } else {
                                        // Register the package name itself
                                        self.imported_names
                                            .insert(actual_package_name.clone(), Type::Unknown);
                                    }
                                }
                            }
                            Ok(Type::Unit)
                        }
                        Err(_e) => {
                            // Package resolution failed - this could be because:
                            // 1. Package is not installed yet
                            // 2. Package will be resolved by import maps during transpilation
                            // Register as Unknown to allow transpiler to handle it
                            match items {
                                UseItems::Named(imports) => {
                                    for (import_name, alias) in imports {
                                        let name_to_register =
                                            alias.as_ref().unwrap_or(import_name);
                                        self.imported_names
                                            .insert(name_to_register.clone(), Type::Unknown);
                                    }
                                }
                                UseItems::All => {
                                    // For wildcard imports, we can't pre-register names
                                }
                                UseItems::Single => {
                                    // Handle external::package::default specially
                                    if is_external_import
                                        && path.segments.len() == 3
                                        && path.segments[2] == "default"
                                    {
                                        // Register the package name (express) instead of "default"
                                        self.imported_names
                                            .insert(actual_package_name.clone(), Type::Unknown);
                                    } else {
                                        // Register the package name itself
                                        self.imported_names
                                            .insert(actual_package_name.clone(), Type::Unknown);
                                    }
                                }
                            }
                            Ok(Type::Unit)
                        }
                    }
                } else {
                    // No package resolver - register as Unknown to avoid errors
                    match items {
                        UseItems::Named(imports) => {
                            for (import_name, alias) in imports {
                                let name_to_register = alias.as_ref().unwrap_or(import_name);
                                self.imported_names
                                    .insert(name_to_register.clone(), Type::Unknown);
                            }
                        }
                        UseItems::Single => {
                            // Handle external::package::default specially
                            if is_external_import
                                && path.segments.len() == 3
                                && path.segments[2] == "default"
                            {
                                // Register the package name (express) instead of "default"
                                self.imported_names
                                    .insert(actual_package_name.clone(), Type::Unknown);
                            } else if let Some(module_name) = path.segments.last() {
                                self.imported_names
                                    .insert(module_name.clone(), Type::Unknown);
                            }
                        }
                        UseItems::All => {
                            // Can't pre-register wildcard imports
                        }
                    }
                    Ok(Type::Unit)
                }
            }
            _ => {
                // Local imports - analyze the module to extract type information
                let module_path = self.resolve_module_path(path, span)?;
                self.analyze_local_module(&module_path, span)?;

                // Now register the imported names based on what we found
                match items {
                    UseItems::Named(imports) => {
                        for (import_name, alias) in imports {
                            let name_to_register = alias.as_ref().unwrap_or(import_name);

                            // Check if it's a struct/enum/function we know about
                            let imported_type = if let Some(fields) = self.structs.get(import_name)
                            {
                                Type::Struct {
                                    name: import_name.clone(),
                                    fields: fields
                                        .iter()
                                        .map(|(name, ty)| (name.clone(), ty.clone()))
                                        .collect(),
                                }
                            } else if let Some(variants) = self.enums.get(import_name) {
                                Type::Enum {
                                    name: import_name.clone(),
                                    variants: variants.clone(),
                                }
                            } else if self.functions.contains_key(import_name) {
                                let (_, ret_type, _) = self.functions.get(import_name).unwrap();
                                ret_type.clone()
                            } else {
                                Type::Unknown
                            };

                            self.imported_names
                                .insert(name_to_register.clone(), imported_type);
                        }
                    }
                    UseItems::Single => {
                        if let Some(module_name) = path.segments.last() {
                            // For single imports, check if it's a known type
                            let imported_type = if let Some(fields) = self.structs.get(module_name)
                            {
                                Type::Struct {
                                    name: module_name.clone(),
                                    fields: fields
                                        .iter()
                                        .map(|(name, ty)| (name.clone(), ty.clone()))
                                        .collect(),
                                }
                            } else if let Some(variants) = self.enums.get(module_name) {
                                Type::Enum {
                                    name: module_name.clone(),
                                    variants: variants.clone(),
                                }
                            } else if self.functions.contains_key(module_name) {
                                let (_, ret_type, _) = self.functions.get(module_name).unwrap();
                                ret_type.clone()
                            } else {
                                Type::Unknown
                            };

                            self.imported_names
                                .insert(module_name.clone(), imported_type);
                        }
                    }
                    UseItems::All => {
                        // Can't pre-register wildcard imports
                    }
                }
                Ok(Type::Unit)
            }
        }
    }

    fn visit_block(&mut self, stmts: &[Stmt], _span: &Span) -> Result<Type> {
        // Push a new scope for the block
        self.type_env.push_scope();

        let mut block_type = Type::Unit;

        // Visit all statements in the block
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;

            match stmt {
                // If the last statement is an expression without semicolon,
                // the block evaluates to that expression's type
                Stmt::Expression(expr, false) if is_last => {
                    block_type = self.visit_expr(expr)?;
                }
                // Otherwise, just visit the statement normally
                _ => {
                    self.visit_stmt(stmt)?;
                }
            }
        }

        // Pop the scope
        self.type_env.pop_scope();

        Ok(block_type)
    }

    fn visit_if_expr(
        &mut self,
        condition: &Expr,
        then_block: &[Stmt],
        else_block: &[Stmt],
        span: &Span,
    ) -> Result<Type> {
        // Check condition is boolean
        let condition_type = self.visit_expr(condition)?;
        if condition_type != Type::Bool {
            return Err(Error::new_semantic(
                format!("If condition must be boolean, found {condition_type}"),
                condition.span(),
            ));
        }

        // Analyze then block and get its type (without creating new scope)
        let then_type = self.visit_statements_no_scope(then_block)?;

        // If there's no else block, the if expression can only return Unit
        if else_block.is_empty() {
            // If then block returns non-Unit, this is an error for expression context
            if then_type != Type::Unit {
                return Err(Error::new_semantic(
                    format!(
                        "If expression without else branch cannot return non-unit type {then_type}"
                    ),
                    *span,
                ));
            }
            return Ok(Type::Unit);
        }

        // Analyze else block and get its type (without creating new scope)
        let else_type = self.visit_statements_no_scope(else_block)?;

        // Both branches must return the same type for if expression
        if then_type != else_type {
            return Err(Error::new_semantic(
                format!("If expression branches have incompatible types: then returns {then_type}, else returns {else_type}"),
                *span,
            ));
        }

        Ok(then_type)
    }

    fn visit_method_call(
        &mut self,
        object: &Expr,
        method: &str,
        args: &[Expr],
        span: &Span,
    ) -> Result<Type> {
        // Check if this is actually an enum variant constructor call
        if let Expr::Identifier(enum_name, _) = object {
            if let Some(enum_variants) = self.enums.get(enum_name) {
                if enum_variants.contains_key(method) {
                    // This is actually an enum variant constructor, not a method call
                    let target_expr = object.clone();
                    return self.visit_enum_variant_or_method_call(
                        &target_expr,
                        method,
                        args,
                        span,
                    );
                }
            }
        }

        let object_type = self.visit_expr(object)?;

        match &object_type {
            Type::String => {
                // Look up method in registry
                if let Some(signature) = self.method_signatures.get_string_method(method) {
                    // Clone signature to avoid borrowing issues
                    let signature = signature.clone();

                    // Validate argument count
                    if args.len() != signature.param_types.len() {
                        return Err(Error::new_semantic(
                            format!(
                                "String method '{}' expects {} argument{}, but {} {} provided",
                                method,
                                signature.param_types.len(),
                                if signature.param_types.len() == 1 {
                                    ""
                                } else {
                                    "s"
                                },
                                args.len(),
                                if args.len() == 1 { "was" } else { "were" }
                            ),
                            *span,
                        ));
                    }

                    // Validate argument types
                    for (i, arg) in args.iter().enumerate() {
                        let arg_type = self.visit_expr(arg)?;
                        let expected_type = &signature.param_types[i];

                        // Skip validation for Unknown types (generic placeholders)
                        if *expected_type != Type::Unknown && arg_type != *expected_type {
                            return Err(Error::new_semantic(
                                format!(
                                    "String method '{}' argument {} expects {}, found {}",
                                    method,
                                    i + 1,
                                    expected_type,
                                    arg_type
                                ),
                                *span,
                            ));
                        }
                    }

                    Ok(signature.return_type.clone())
                } else {
                    // TEMPORARY: Keep hardcoded methods for backwards compatibility
                    match method {
                        "len" => {
                            if !args.is_empty() {
                                return Err(Error::new_semantic(
                                    format!(
                                    "String method '{}' expects 0 arguments, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            Ok(Type::Int)
                        }
                        "trim" => {
                            if !args.is_empty() {
                                return Err(Error::new_semantic(
                                    format!(
                                    "String method '{}' expects 0 arguments, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            Ok(Type::String)
                        }
                        "substring" => {
                            if args.len() != 2 {
                                return Err(Error::new_semantic(
                                    format!(
                                    "String method '{}' expects 2 arguments, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            // Check argument types
                            for arg in args {
                                let arg_type = self.visit_expr(arg)?;
                                if arg_type != Type::Int {
                                    return Err(Error::new_semantic(
                                        format!(
                                            "String method '{method}' expects int arguments, found {arg_type}"
                                        ),
                                        *span,
                                    ));
                                }
                            }
                            Ok(Type::String)
                        }
                        "split" => {
                            if args.len() != 1 {
                                return Err(Error::new_semantic(
                                    format!(
                                    "String method '{}' expects 1 argument, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            let arg_type = self.visit_expr(&args[0])?;
                            if arg_type != Type::String {
                                return Err(Error::new_semantic(
                                    format!(
                                        "String method '{method}' expects string argument, found {arg_type}"
                                    ),
                                    *span,
                                ));
                            }
                            Ok(Type::Array(Box::new(Type::String)))
                        }
                        "chars" => {
                            if !args.is_empty() {
                                return Err(Error::new_semantic(
                                    format!(
                                    "String method '{}' expects 0 arguments, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            Ok(Type::Array(Box::new(Type::String)))
                        }
                        "to_lowercase" => {
                            if !args.is_empty() {
                                return Err(Error::new_semantic(
                                    format!(
                                    "String method '{}' expects 0 arguments, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            Ok(Type::String)
                        }
                        "to_uppercase" => {
                            if !args.is_empty() {
                                return Err(Error::new_semantic(
                                    format!(
                                    "String method '{}' expects 0 arguments, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            Ok(Type::String)
                        }
                        "contains" => {
                            if args.len() != 1 {
                                return Err(Error::new_semantic(
                                    format!(
                                    "String method '{}' expects 1 argument, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            let arg_type = self.visit_expr(&args[0])?;
                            if arg_type != Type::String {
                                return Err(Error::new_semantic(
                                    format!(
                                        "String method '{method}' expects string argument, found {arg_type}"
                                    ),
                                    *span,
                                ));
                            }
                            Ok(Type::Bool)
                        }
                        "starts_with" => {
                            if args.len() != 1 {
                                return Err(Error::new_semantic(
                                    format!(
                                    "String method '{}' expects 1 argument, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            let arg_type = self.visit_expr(&args[0])?;
                            if arg_type != Type::String {
                                return Err(Error::new_semantic(
                                    format!(
                                        "String method '{method}' expects string argument, found {arg_type}"
                                    ),
                                    *span,
                                ));
                            }
                            Ok(Type::Bool)
                        }
                        "ends_with" => {
                            if args.len() != 1 {
                                return Err(Error::new_semantic(
                                    format!(
                                    "String method '{}' expects 1 argument, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            let arg_type = self.visit_expr(&args[0])?;
                            if arg_type != Type::String {
                                return Err(Error::new_semantic(
                                    format!(
                                        "String method '{method}' expects string argument, found {arg_type}"
                                    ),
                                    *span,
                                ));
                            }
                            Ok(Type::Bool)
                        }
                        "replace" => {
                            if args.len() != 2 {
                                return Err(Error::new_semantic(
                                    format!(
                                    "String method '{}' expects 2 arguments, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            // Check both arguments are strings
                            for arg in args {
                                let arg_type = self.visit_expr(arg)?;
                                if arg_type != Type::String {
                                    return Err(Error::new_semantic(
                                        format!(
                                            "String method '{method}' expects string arguments, found {arg_type}"
                                        ),
                                        *span,
                                    ));
                                }
                            }
                            Ok(Type::String)
                        }
                        _ => Err(Error::new_semantic(
                            format!("Unknown method '{method}' for string type"),
                            *span,
                        )),
                    }
                }
            }
            Type::Array(elem_type) => {
                // Look up method in registry
                if let Some(signature) = self.method_signatures.get_array_method(method) {
                    // Clone signature to avoid borrowing issues
                    let signature = signature.clone();

                    // Validate argument count
                    if args.len() != signature.param_types.len() {
                        return Err(Error::new_semantic(
                            format!(
                                "Array method '{}' expects {} argument{}, but {} {} provided",
                                method,
                                signature.param_types.len(),
                                if signature.param_types.len() == 1 {
                                    ""
                                } else {
                                    "s"
                                },
                                args.len(),
                                if args.len() == 1 { "was" } else { "were" }
                            ),
                            *span,
                        ));
                    }

                    // Validate argument types
                    for (i, arg) in args.iter().enumerate() {
                        let arg_type = self.visit_expr(arg)?;
                        let expected_type = &signature.param_types[i];

                        // Skip validation for Unknown types (generic placeholders)
                        // Also accept element type for contains/push methods
                        if *expected_type != Type::Unknown && arg_type != *expected_type {
                            // Special case for array methods that take element type
                            if (method == "contains" || method == "push") && arg_type == **elem_type
                            {
                                continue;
                            }
                            // Special case for concat which takes array of same type
                            if method == "concat" && arg_type == object_type {
                                continue;
                            }
                            return Err(Error::new_semantic(
                                format!(
                                    "Array method '{}' argument {} expects {}, found {}",
                                    method,
                                    i + 1,
                                    expected_type,
                                    arg_type
                                ),
                                *span,
                            ));
                        }
                    }

                    // Handle return types that depend on element type
                    let return_type = match method {
                        "reverse" | "slice" | "push" | "concat" => object_type.clone(),
                        _ => signature.return_type.clone(),
                    };

                    Ok(return_type)
                } else {
                    // TEMPORARY: Keep hardcoded methods for backwards compatibility
                    match method {
                        "len" => {
                            if !args.is_empty() {
                                return Err(Error::new_semantic(
                                    format!(
                                    "Array method '{}' expects 0 arguments, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            Ok(Type::Int)
                        }
                        "push" => {
                            if args.len() != 1 {
                                return Err(Error::new_semantic(
                                    format!(
                                    "Array method '{}' expects 1 argument, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            let arg_type = self.visit_expr(&args[0])?;
                            if arg_type != **elem_type {
                                return Err(Error::new_semantic(
                                    format!(
                                        "Array method '{method}' expects {elem_type} argument, found {arg_type}"
                                    ),
                                    *span,
                                ));
                            }
                            Ok(Type::Unit)
                        }
                        "pop" => {
                            if !args.is_empty() {
                                return Err(Error::new_semantic(
                                    format!(
                                    "Array method '{}' expects 0 arguments, but {} were provided",
                                    method,
                                    args.len()
                                ),
                                    *span,
                                ));
                            }
                            // Return Option<T>
                            Ok(Type::Unknown) // For now, as we need proper Option<T> support
                        }
                        _ => Err(Error::new_semantic(
                            format!("Unknown method '{method}' for array type"),
                            *span,
                        )),
                    }
                }
            }
            Type::Struct { name, .. } => {
                // Check if this is an extern type
                let is_extern = self.imported_names.contains_key(name);

                let target_expr = Expr::Identifier(name.clone(), *span);

                if is_extern {
                    // For extern types, don't prepend self - it's already handled
                    self.visit_enum_variant_or_method_call(&target_expr, method, args, span)
                } else {
                    // For regular struct methods, prepend 'self' (the object) as the first argument
                    let mut method_args = vec![object.clone()];
                    method_args.extend_from_slice(args);
                    self.visit_enum_variant_or_method_call(&target_expr, method, &method_args, span)
                }
            }
            Type::Enum { name, .. } => {
                // Check if this is an extern type
                let is_extern = self.imported_names.contains_key(name);

                let target_expr = Expr::Identifier(name.clone(), *span);

                if is_extern {
                    // For extern types, don't prepend self - it's already handled
                    self.visit_enum_variant_or_method_call(&target_expr, method, args, span)
                } else {
                    // For regular enum methods, prepend 'self' (the object) as the first argument
                    let mut method_args = vec![object.clone()];
                    method_args.extend_from_slice(args);
                    self.visit_enum_variant_or_method_call(&target_expr, method, &method_args, span)
                }
            }
            _ => Err(Error::new_semantic(
                format!("Cannot call method '{method}' on type {object_type}"),
                *span,
            )),
        }
    }

    fn visit_cast(&mut self, expr: &Expr, target_type: &str, _span: &Span) -> Result<Type> {
        // Type check the expression being cast
        let expr_type = self.visit_expr(expr)?;

        // Parse the target type
        let target = Type::from_string(target_type).unwrap_or(Type::Unknown);

        // For now, allow all casts and return the target type
        // In a more complete implementation, we would check if the cast is valid
        // e.g., numeric types can be cast to each other, but not to strings
        match (&expr_type, &target) {
            // Allow unknown types (for extern values)
            (Type::Unknown, _) | (_, Type::Unknown) => Ok(target),

            // Allow same-type casts (no-op)
            _ if expr_type == target => Ok(target),

            // Numeric casts
            (Type::Int, Type::Float) | (Type::Float, Type::Int) => Ok(target),

            // String to numeric parsing would fail at runtime
            (Type::String, Type::Int) | (Type::String, Type::Float) => Ok(target),

            // Numeric to string conversion
            (Type::Int, Type::String) | (Type::Float, Type::String) => Ok(target),

            // For now, allow other casts and let runtime/transpiler handle them
            _ => Ok(target),
        }
    }

    fn visit_struct_pattern(
        &mut self,
        _variant: &str,
        _fields: &[(String, Option<String>)],
        _span: &Span,
    ) -> Result<Type> {
        // Struct patterns are used for pattern matching
        // We need to validate that the variant exists and has the required fields
        // For now, we'll just return Unit type since patterns don't have runtime values

        // TODO: Validate that the variant exists in the enum/struct definitions
        // TODO: Validate that all fields exist in the variant
        // TODO: Add field variables to scope for the match arm

        Ok(Type::Unit)
    }

    fn visit_object_literal(&mut self, fields: &[(String, Expr)], _span: &Span) -> Result<Type> {
        // Object literals are untyped JavaScript objects
        // We type-check all the field values but return a generic Object type

        for (_key, value) in fields {
            // Type-check each value expression
            self.visit_expr(value)?;
        }

        // Return a generic Object type
        // In the future, we could support more specific object types
        Ok(Type::Unknown)
    }

    fn visit_macro_call(&mut self, name: &str, args: &[Expr], span: &Span) -> Result<Type> {
        match name {
            "print" | "println" | "eprint" | "eprintln" => {
                // These macros accept format string + arguments like format!
                if args.is_empty() && (name == "print" || name == "eprint") {
                    return Err(Error::new_semantic(
                        format!("{name}! requires at least one argument"),
                        *span,
                    ));
                }

                // Type-check all arguments
                for arg in args {
                    self.visit_expr(arg)?;
                }

                // print!/eprint! return int, println!/eprintln! return unit
                Ok(if name == "print" || name == "eprint" {
                    Type::Int
                } else {
                    Type::Unit
                })
            }
            "format" => {
                // format! macro
                if args.is_empty() {
                    return Err(Error::new_semantic(
                        "format! requires at least one argument (the format string)".to_string(),
                        *span,
                    ));
                }

                // First argument must be a string
                let first_arg_type = self.visit_expr(&args[0])?;
                if first_arg_type != Type::String {
                    return Err(Error::new_semantic(
                        "format! first argument must be a string".to_string(),
                        args[0].span(),
                    ));
                }

                // Count placeholders in format string
                if let Expr::String(format_str, _) = &args[0] {
                    let placeholder_count =
                        format_str.matches("{}").count() - format_str.matches("{{").count() * 2;
                    let arg_count = args.len() - 1;

                    if placeholder_count != arg_count {
                        return Err(Error::new_semantic(
                            format!("format! expects {placeholder_count} arguments after format string, but {arg_count} were provided"),
                            *span,
                        ));
                    }
                }

                // Type-check remaining arguments
                for arg in &args[1..] {
                    self.visit_expr(arg)?;
                }

                Ok(Type::String)
            }
            _ => Err(Error::new_semantic(
                format!("Unknown macro: {name}!"),
                *span,
            )),
        }
    }
}

/// Check if a module name is a Node.js built-in module
fn is_nodejs_builtin(module: &str) -> bool {
    matches!(
        module,
        "assert"
            | "buffer"
            | "child_process"
            | "cluster"
            | "console"
            | "constants"
            | "crypto"
            | "dgram"
            | "dns"
            | "domain"
            | "events"
            | "fs"
            | "http"
            | "https"
            | "module"
            | "net"
            | "os"
            | "path"
            | "perf_hooks"
            | "process"
            | "punycode"
            | "querystring"
            | "readline"
            | "repl"
            | "stream"
            | "string_decoder"
            | "sys"
            | "timers"
            | "tls"
            | "tty"
            | "url"
            | "util"
            | "v8"
            | "vm"
            | "worker_threads"
            | "zlib"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn analyze_code(code: &str) -> Result<()> {
        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse()?;
        let mut analyzer = SemanticVisitor::new();
        analyzer.analyze(&ast)
    }

    #[test]
    fn test_variable_scoping() {
        // Test basic variable definition and lookup
        let code = r#"
            let x = 42;
            let y = x;
        "#;
        assert!(analyze_code(code).is_ok());

        // Test undefined variable
        let code = r#"
            let y = x;
        "#;
        assert!(analyze_code(code).is_err());
    }

    #[test]
    fn test_block_scoping() {
        // Test variable shadowing in blocks
        let code = r#"
            let x = 42;
            {
                let x = "hello";
                let y = x;
            }
            let z = x;
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_function_parameter_scoping() {
        // Test function parameters are accessible
        let code = r#"
            fn add(a: int, b: int) -> int {
                a + b
            }
        "#;
        assert!(analyze_code(code).is_ok());

        // Test parameters in if-else blocks (regression test)
        let code = r#"
            fn test(n: int) -> int {
                if n == 0 {
                    n
                } else {
                    n + 1
                }
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_binary_operation_type_checking() {
        // Test int + int
        let code = r#"
            let x = 1 + 2;
        "#;
        assert!(analyze_code(code).is_ok());

        // Test float + int (mixed arithmetic)
        let code = r#"
            let x = 1.5 + 2;
        "#;
        assert!(analyze_code(code).is_ok());

        // Test string concatenation (should fail - not implemented)
        let code = r#"
            let x = "hello" + "world";
        "#;
        assert!(analyze_code(code).is_err());
    }

    #[test]
    fn test_array_type_checking() {
        // Test homogeneous array
        let code = r#"
            let arr = [1, 2, 3];
        "#;
        assert!(analyze_code(code).is_ok());

        // Test array indexing
        let code = r#"
            let arr = [1, 2, 3];
            let x = arr[0];
        "#;
        assert!(analyze_code(code).is_ok());

        // Test array slicing with range
        let code = r#"
            let arr = [1, 2, 3, 4, 5];
            let slice = arr[1..3];
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_if_expression_type_checking() {
        // Test if statement (not expression) with consistent types
        let code = r#"
            fn test() -> int {
                if true {
                    42
                } else {
                    0
                }
            }
        "#;
        assert!(analyze_code(code).is_ok());

        // Test type mismatch in branches
        let code = r#"
            fn test() -> int {
                if true {
                    42
                } else {
                    "hello"
                }
            }
        "#;
        assert!(analyze_code(code).is_err());

        // Test if without else returning non-unit
        let code = r#"
            fn test() -> int {
                if true { 42 }
            }
        "#;
        let result = analyze_code(code);
        assert!(result.is_err(), "Expected error but got: {result:?}");
    }

    #[test]
    fn test_function_call_type_checking() {
        // Test correct function call
        let code = r#"
            fn add(a: int, b: int) -> int {
                a + b
            }
            let x = add(1, 2);
        "#;
        assert!(analyze_code(code).is_ok());

        // Test wrong argument count
        let code = r#"
            fn add(a: int, b: int) -> int {
                a + b
            }
            let x = add(1);
        "#;
        assert!(analyze_code(code).is_err());

        // Test undefined function
        let code = r#"
            let x = unknown_func();
        "#;
        assert!(analyze_code(code).is_err());
    }

    #[test]
    fn test_return_statement_type_checking() {
        // Test return with correct type
        let code = r#"
            fn get_number() -> int {
                return 42;
            }
        "#;
        assert!(analyze_code(code).is_ok());

        // Test return without value in unit function
        let code = r#"
            fn do_nothing() -> unit {
                return;
            }
        "#;
        assert!(analyze_code(code).is_ok());

        // Test early return in if
        let code = r#"
            fn abs(n: int) -> int {
                if n < 0 {
                    return 0 - n;
                }
                n
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_match_exhaustiveness() {
        // Test exhaustive match
        let code = r#"
            enum Option {
                Some(int),
                None,
            }

            let opt = Option::Some(42);
            match opt {
                Option::Some(x) => x,
                Option::None => 0,
            }
        "#;
        assert!(analyze_code(code).is_ok());

        // TODO: Non-exhaustive match detection for function parameters requires
        // proper enum type resolution in Type::from_string
        // Currently, custom types in function parameters are treated as structs

        // Test match with wildcard
        let code = r#"
            enum Option {
                Some(int),
                None,
            }

            let opt = Option::None;
            match opt {
                Option::Some(x) => x,
                _ => 0,
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_match_arm_type_consistency() {
        // Test all arms return same type
        let code = r#"
            enum Option {
                Some(int),
                None,
            }

            fn test(opt: Option) -> int {
                match opt {
                    Option::Some(x) => x,
                    Option::None => 0,
                }
            }
        "#;
        assert!(analyze_code(code).is_ok());

        // Test arms with different types
        let code = r#"
            enum Option {
                Some(int),
                None,
            }

            fn test(opt: Option) -> int {
                match opt {
                    Option::Some(x) => x,
                    Option::None => "zero",
                }
            }
        "#;
        assert!(analyze_code(code).is_err());
    }

    #[test]
    fn test_struct_field_access() {
        // Test valid field access
        let code = r#"
            struct Point {
                x: int,
                y: int,
            }

            let p = Point { x: 1, y: 2 };
            let x = p.x;
        "#;
        assert!(analyze_code(code).is_ok());

        // Test invalid field access
        let code = r#"
            struct Point {
                x: int,
                y: int,
            }

            let p = Point { x: 1, y: 2 };
            let z = p.z;
        "#;
        assert!(analyze_code(code).is_err());
    }

    #[test]
    fn test_loop_control_flow() {
        // Test break/continue in loops
        let code = r#"
            fn test() -> unit {
                for i in 0..10 {
                    if i == 5 {
                        break;
                    }
                    continue;
                }
            }
        "#;
        assert!(analyze_code(code).is_ok());

        // Test break outside loop
        let code = r#"
            fn test() -> unit {
                break;
            }
        "#;
        assert!(analyze_code(code).is_err());

        // Test continue outside loop
        let code = r#"
            fn test() -> unit {
                continue;
            }
        "#;
        assert!(analyze_code(code).is_err());
    }

    #[test]
    fn test_block_type_inference() {
        // Test block with expression
        let code = r#"
            let x = {
                let y = 42;
                y + 1
            };
        "#;
        assert!(analyze_code(code).is_ok());

        // Test block with semicolon (returns unit)
        let code = r#"
            fn test() -> unit {
                {
                    let y = 42;
                    y + 1;
                }
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_recursive_function() {
        // Test recursive function definition
        let code = r#"
            fn factorial(n: int) -> int {
                if n == 0 {
                    1
                } else {
                    n * factorial(n - 1)
                }
            }
        "#;
        assert!(analyze_code(code).is_ok());
    }

    #[test]
    fn test_unary_negation_types() {
        // Valid: negate int
        assert!(analyze_code("let x = -5;").is_ok());

        // Valid: negate float
        assert!(analyze_code("let x = -3.14;").is_ok());

        // Invalid: negate bool
        let err = analyze_code("let x = -true;").unwrap_err();
        assert!(err
            .to_string()
            .contains("Unary negation requires numeric type"));

        // Invalid: negate string
        let err = analyze_code("let x = -\"hello\";").unwrap_err();
        assert!(err
            .to_string()
            .contains("Unary negation requires numeric type"));
    }

    #[test]
    fn test_unary_not_types() {
        // Valid: NOT bool
        assert!(analyze_code("let x = !true;").is_ok());
        assert!(analyze_code("let x = !false;").is_ok());

        // Invalid: NOT int
        let err = analyze_code("let x = !5;").unwrap_err();
        assert!(err.to_string().contains("Logical NOT requires bool type"));

        // Invalid: NOT string
        let err = analyze_code("let x = !\"hello\";").unwrap_err();
        assert!(err.to_string().contains("Logical NOT requires bool type"));
    }

    #[test]
    fn test_unary_in_expressions() {
        // Negation in arithmetic
        assert!(analyze_code("let x = 5 + -3;").is_ok());
        assert!(analyze_code("let x = -5 * 2;").is_ok());

        // NOT in boolean expressions
        assert!(analyze_code("let x = !true == false;").is_ok());

        // Double negation
        assert!(analyze_code("let x = --5;").is_ok());
        assert!(analyze_code("let x = !!true;").is_ok());
    }

    #[test]
    fn test_static_method_call_type_resolution() {
        // Test that static method calls return the correct type
        let code = r#"
            enum LogLevel {
                Info,
                Error,
            }

            struct Logger {
                level: LogLevel,
            }

            impl Logger {
                fn new(level: LogLevel) -> Logger {
                    Logger { level: level }
                }
            }

            let logger = Logger::new(LogLevel::Info);
        "#;

        // This should pass - static method should return Logger type
        let result = analyze_code(code);
        assert!(
            result.is_ok(),
            "Expected static method call to be valid, but got: {result:?}"
        );

        // Now let's also test the specific case where we want to check the return type
        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut analyzer = SemanticVisitor::new();

        // Analyze the code
        analyzer.analyze(&ast).unwrap();

        // Debug: Print registered functions
        println!("DEBUG: Registered functions:");
        for (name, (params, return_type, _)) in &analyzer.functions {
            println!("  {name}: {params:?} -> {return_type:?}");
        }

        // Debug: Print type environment
        println!("DEBUG: Type environment for 'logger':");
        if let Some(logger_type) = analyzer.type_env.lookup("logger") {
            println!("  logger: {logger_type:?}");
        } else {
            println!("  logger: NOT FOUND");
        }

        // Check that Logger is properly registered
        if let Some(logger_type) = analyzer.type_env.lookup("logger") {
            match logger_type {
                Type::Struct { name, .. } => {
                    assert_eq!(
                        name, "Logger",
                        "Expected logger variable to have Logger type, got struct with name: {name}"
                    );
                }
                other => {
                    panic!("Expected logger variable to have Logger struct type, got: {other:?}");
                }
            }
        } else {
            panic!("Logger variable not found in type environment");
        }
    }

    #[test]
    fn test_compound_assignment_types() {
        // Valid: numeric compound assignment
        let code = r#"
            let x = 5;
            x += 3;
            x -= 2;
            x *= 4;
            x /= 2;
            x %= 3;
        "#;
        assert!(analyze_code(code).is_ok());

        // Valid: float compound assignment
        let code = r#"
            let y = 5.0;
            y += 3.5;
        "#;
        assert!(analyze_code(code).is_ok());

        // Invalid: non-numeric compound assignment
        let err = analyze_code(
            r#"
            let s = "hello";
            s += " world";
        "#,
        )
        .unwrap_err();
        assert!(err.to_string().contains("Compound assignment"));
    }
}
