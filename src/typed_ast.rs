use crate::{
    parser::Operator,
    span::Span,
    types::Type,
};

/// Typed AST - created after semantic analysis with full type information
#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpr {
    // Literals
    Int(i64, Span),
    Float(f64, Span),
    String(String, Span),
    Bool(bool, Span),
    
    // Variables and identifiers
    Identifier(String, Type, Span),
    
    // Arrays
    Array(Vec<TypedExpr>, Type, Span),
    ArrayIndex(Box<TypedExpr>, Box<TypedExpr>, Type, Span),
    
    // Binary operations
    BinaryOp(Box<TypedExpr>, Operator, Box<TypedExpr>, Type, Span),
    
    // Function calls
    FunctionCall {
        name: String,
        args: Vec<TypedExpr>,
        return_type: Type,
        span: Span,
    },
    
    // Method calls (instance methods)
    MethodCall {
        receiver: Box<TypedExpr>,
        method: String,
        args: Vec<TypedExpr>,
        return_type: Type,
        span: Span,
    },
    
    // Static method calls (clearly distinguished from enum variants)
    StaticMethodCall {
        type_name: String,
        method: String,
        args: Vec<TypedExpr>,
        return_type: Type,
        span: Span,
    },
    
    // Struct operations
    StructInit {
        struct_name: String,
        fields: Vec<(String, TypedExpr)>,
        span: Span,
    },
    
    MemberAccess {
        object: Box<TypedExpr>,
        field: String,
        field_type: Type,
        span: Span,
    },
    
    // Enum operations (clearly distinguished from static methods)
    EnumVariant {
        enum_name: String,
        variant: String,
        data: Option<Box<TypedExpr>>,
        span: Span,
    },
    
    // Assignment operations
    Assign(Box<TypedExpr>, Box<TypedExpr>, Span),
    CompoundAssign(Box<TypedExpr>, Operator, Box<TypedExpr>, Span),
    
    // Range
    Range(Option<Box<TypedExpr>>, Option<Box<TypedExpr>>, bool, Span),
    
    // Block expression
    Block(Vec<TypedStmt>, Type, Span),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedStmt {
    // Variable declaration
    Let {
        name: String,
        expr: TypedExpr,
        var_type: Type,
        span: Span,
    },
    
    // Function definition
    Function {
        name: String,
        params: Vec<(String, Type)>,
        return_type: Type,
        body: Vec<TypedStmt>,
        span: Span,
    },
    
    // Type definitions
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
        span: Span,
    },
    
    Enum {
        name: String,
        variants: Vec<(String, Option<Type>)>,
        span: Span,
    },
    
    // Implementation block
    Impl {
        struct_name: String,
        methods: Vec<TypedStmt>,
        span: Span,
    },
    
    // Control flow
    If {
        condition: TypedExpr,
        then_body: Vec<TypedStmt>,
        else_body: Vec<TypedStmt>,
        span: Span,
    },
    
    While {
        condition: TypedExpr,
        body: Vec<TypedStmt>,
        span: Span,
    },
    
    ForLoop {
        variable: String,
        iterable: TypedExpr,
        body: Vec<TypedStmt>,
        span: Span,
    },
    
    Loop {
        body: Vec<TypedStmt>,
        span: Span,
    },
    
    Match {
        expr: TypedExpr,
        arms: Vec<(TypedPattern, Vec<TypedStmt>)>,
        span: Span,
    },
    
    // Control flow statements
    Break(Span),
    Continue(Span),
    Return(Option<TypedExpr>, Span),
    
    // Expression statement
    Expression(TypedExpr, bool), // bool indicates if semicolon is present
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedPattern {
    // Pattern matching for match expressions
    EnumVariant {
        enum_name: String,
        variant: String,
        binding: Option<String>,
    },
    Identifier(String),
    Wildcard,
}

/// AST transformer - converts untyped AST to typed AST after semantic analysis
pub struct AstTransformer {
    // We'll need access to type information from semantic analysis
    structs: std::collections::HashMap<String, std::collections::HashMap<String, Type>>,
    enums: std::collections::HashMap<String, std::collections::HashMap<String, Option<Type>>>,
    functions: std::collections::HashMap<String, (Vec<(String, Type)>, Type)>,
}

impl AstTransformer {
    pub fn new(
        structs: std::collections::HashMap<String, std::collections::HashMap<String, Type>>,
        enums: std::collections::HashMap<String, std::collections::HashMap<String, Option<Type>>>,
        functions: std::collections::HashMap<String, (Vec<(String, Type)>, Type)>,
    ) -> Self {
        Self {
            structs,
            enums,
            functions,
        }
    }
    
    pub fn transform_program(&self, stmts: &[crate::parser::Stmt]) -> Result<Vec<TypedStmt>, crate::Error> {
        stmts.iter()
            .map(|stmt| self.transform_stmt(stmt))
            .collect()
    }
    
    fn transform_stmt(&self, stmt: &crate::parser::Stmt) -> Result<TypedStmt, crate::Error> {
        use crate::parser::Stmt;
        
        match stmt {
            Stmt::Let(name, expr, span) => {
                let typed_expr = self.transform_expr(expr)?;
                let var_type = self.infer_expr_type(&typed_expr);
                Ok(TypedStmt::Let {
                    name: name.clone(),
                    expr: typed_expr,
                    var_type,
                    span: *span,
                })
            }
            Stmt::Expression(expr, has_semicolon) => {
                let typed_expr = self.transform_expr(expr)?;
                Ok(TypedStmt::Expression(typed_expr, *has_semicolon))
            }
            // TODO: Implement other statement transformations
            _ => todo!("Transform other statement types"),
        }
    }
    
    fn transform_expr(&self, expr: &crate::parser::Expr) -> Result<TypedExpr, crate::Error> {
        use crate::parser::Expr;
        
        match expr {
            Expr::Int(n, span) => Ok(TypedExpr::Int(*n, *span)),
            Expr::Float(f, span) => Ok(TypedExpr::Float(*f, *span)),
            Expr::String(s, span) => Ok(TypedExpr::String(s.clone(), *span)),
            Expr::Bool(b, span) => Ok(TypedExpr::Bool(*b, *span)),
            
            Expr::Identifier(name, span) => {
                // In a real implementation, we'd look up the type
                let id_type = Type::Unknown; // Placeholder
                Ok(TypedExpr::Identifier(name.clone(), id_type, *span))
            }
            
            Expr::EnumVariantOrMethodCall { target, call, args, span, .. } => {
                // This is where we disambiguate!
                if let Expr::Identifier(type_name, _) = target.as_ref() {
                    if self.enums.contains_key(type_name) {
                        // It's an enum variant
                        let data = if args.is_empty() {
                            None
                        } else if args.len() == 1 {
                            Some(Box::new(self.transform_expr(&args[0])?))
                        } else {
                            return Err(crate::Error::new_semantic(
                                format!("Enum variant can only have 0 or 1 argument, found {}", args.len()),
                                *span,
                            ));
                        };
                        
                        Ok(TypedExpr::EnumVariant {
                            enum_name: type_name.clone(),
                            variant: call.clone(),
                            data,
                            span: *span,
                        })
                    } else if self.structs.contains_key(type_name) {
                        // It's a static method call
                        let typed_args = args.iter()
                            .map(|arg| self.transform_expr(arg))
                            .collect::<Result<Vec<_>, _>>()?;
                        
                        let return_type = if let Some((_, ret_type)) = self.functions.get(&format!("{}::{}", type_name, call)) {
                            ret_type.clone()
                        } else {
                            Type::Unknown
                        };
                        
                        Ok(TypedExpr::StaticMethodCall {
                            type_name: type_name.clone(),
                            method: call.clone(),
                            args: typed_args,
                            return_type,
                            span: *span,
                        })
                    } else {
                        Err(crate::Error::new_semantic(
                            format!("Unknown type: {}", type_name),
                            *span,
                        ))
                    }
                } else {
                    Err(crate::Error::new_semantic(
                        "Complex expressions in type position not yet supported",
                        *span,
                    ))
                }
            }
            
            Expr::Block(stmts, span) => {
                // Transform all statements in the block
                let typed_stmts = stmts.iter()
                    .map(|stmt| self.transform_stmt(stmt))
                    .collect::<Result<Vec<_>, _>>()?;
                
                // Determine the type of the block
                // The type is the type of the last expression (if no semicolon)
                let block_type = if let Some(last_stmt) = stmts.last() {
                    match last_stmt {
                        crate::parser::Stmt::Expression(expr, false) => {
                            // Last statement is an expression without semicolon
                            let typed_expr = self.transform_expr(expr)?;
                            self.infer_expr_type(&typed_expr)
                        }
                        _ => Type::Unit,
                    }
                } else {
                    Type::Unit
                };
                
                Ok(TypedExpr::Block(typed_stmts, block_type, *span))
            }
            
            // TODO: Implement other expression transformations
            _ => todo!("Transform other expression types"),
        }
    }
    
    fn infer_expr_type(&self, expr: &TypedExpr) -> Type {
        match expr {
            TypedExpr::Int(_, _) => Type::Int,
            TypedExpr::Float(_, _) => Type::Float,
            TypedExpr::String(_, _) => Type::String,
            TypedExpr::Bool(_, _) => Type::Bool,
            TypedExpr::Identifier(_, typ, _) => typ.clone(),
            TypedExpr::EnumVariant { enum_name, .. } => Type::Enum {
                name: enum_name.clone(),
                variants: std::collections::HashMap::new(), // We don't need full variant info here
            },
            TypedExpr::StaticMethodCall { return_type, .. } => return_type.clone(),
            TypedExpr::Block(_, block_type, _) => block_type.clone(),
            _ => Type::Unknown,
        }
    }
}