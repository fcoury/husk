use std::collections::HashMap;

use crate::{
    ast::visitor::AstVisitor,
    error::{Error, Result},
    parser::{Expr, Operator, Stmt},
    span::Span,
    types::{Type, TypeEnvironment},
};

/// Semantic analyzer implemented using the visitor pattern
pub struct SemanticVisitor {
    type_env: TypeEnvironment,
    structs: HashMap<String, HashMap<String, Type>>,
    functions: HashMap<String, (Vec<(String, Type)>, Type, Span)>,
    enums: HashMap<String, HashMap<String, Option<Type>>>,
    match_bound_vars: HashMap<String, Type>,
    loop_depth: u32,
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
        };
        visitor.init_standard_library();
        visitor
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
    }

    pub fn analyze(&mut self, stmts: &[Stmt]) -> Result<()> {
        self.visit_statements(stmts)?;
        Ok(())
    }


    /// Helper method to check if a type is numeric
    fn is_numeric_type(&self, ty: &Type) -> bool {
        matches!(ty, Type::Int | Type::Float)
    }
}

impl AstVisitor<Type> for SemanticVisitor {
    type Error = Error;

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

    fn visit_string(&mut self, _value: &str, _span: &Span) -> Result<Type> {
        Ok(Type::String)
    }

    fn visit_identifier(&mut self, name: &str, span: &Span) -> Result<Type> {
        if let Some(var_type) = self.match_bound_vars.get(name) {
            return Ok(var_type.clone());
        }

        match self.type_env.lookup(name) {
            Some(var_type) => Ok(var_type.clone()),
            None => Err(Error::new_semantic(
                format!("Variable '{}' not found in scope", name),
                *span,
            )),
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
                        "Array elements must have the same type. Expected {}, found {}",
                        first_type.to_string(),
                        element_type.to_string()
                    ),
                    *span,
                ));
            }
        }

        Ok(Type::Array(Box::new(first_type)))
    }

    fn visit_array_index(&mut self, array: &Expr, index: &Expr, span: &Span) -> Result<Type> {
        let array_type = self.visit_expr(array)?;
        let index_type = self.visit_expr(index)?;

        if index_type != Type::Int {
            return Err(Error::new_semantic(
                format!("Array index must be an integer, found {}", index_type.to_string()),
                *span,
            ));
        }

        match array_type {
            Type::Array(element_type) => Ok(*element_type),
            _ => Err(Error::new_semantic(
                format!("Cannot index non-array type: {}", array_type.to_string()),
                *span,
            )),
        }
    }

    fn visit_range(&mut self, start: Option<&Expr>, end: Option<&Expr>, _inclusive: bool, span: &Span) -> Result<Type> {
        // Check start expression if present
        if let Some(start_expr) = start {
            let start_type = self.visit_expr(start_expr)?;
            if !self.is_numeric_type(&start_type) {
                return Err(Error::new_semantic(
                    format!("Range start must be numeric, found {}", start_type.to_string()),
                    *span,
                ));
            }
        }

        // Check end expression if present
        if let Some(end_expr) = end {
            let end_type = self.visit_expr(end_expr)?;
            if !self.is_numeric_type(&end_type) {
                return Err(Error::new_semantic(
                    format!("Range end must be numeric, found {}", end_type.to_string()),
                    *span,
                ));
            }
        }

        Ok(Type::Range)
    }

    fn visit_binary_op(&mut self, left: &Expr, op: &Operator, right: &Expr, span: &Span) -> Result<Type> {
        let left_type = self.visit_expr(left)?;
        let right_type = self.visit_expr(right)?;

        match op {
            Operator::Plus | Operator::Minus | Operator::Multiply | Operator::Divide | Operator::Modulo => {
                if !self.is_numeric_type(&left_type) || !self.is_numeric_type(&right_type) {
                    return Err(Error::new_semantic(
                        format!(
                            "Binary operation {:?} requires numeric types, found {} and {}",
                            op, left_type.to_string(), right_type.to_string()
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
            Operator::Equals | Operator::LessThan | Operator::GreaterThan | Operator::LessThanEquals | Operator::GreaterThanEquals => {
                // For now, allow comparison of any types (will refine later)
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
                        format!("Cannot assign to undefined variable '{}'", name),
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

    fn visit_compound_assign(&mut self, left: &Expr, op: &Operator, right: &Expr, span: &Span) -> Result<Type> {
        // Compound assignment is like a binary op followed by assignment
        let left_type = self.visit_expr(left)?;
        let right_type = self.visit_expr(right)?;

        // Check the operation is valid
        match op {
            Operator::Plus | Operator::Minus | Operator::Multiply | Operator::Divide | Operator::Modulo => {
                if !self.is_numeric_type(&left_type) || !self.is_numeric_type(&right_type) {
                    return Err(Error::new_semantic(
                        format!(
                            "Compound assignment {:?} requires numeric types, found {} and {}",
                            op, left_type.to_string(), right_type.to_string()
                        ),
                        *span,
                    ));
                }
            }
            _ => {
                return Err(Error::new_semantic(
                    format!("Invalid operator for compound assignment: {:?}", op),
                    *span,
                ));
            }
        }

        // Return the type of the assignment (same as left side)
        Ok(left_type)
    }

    fn visit_function_call(&mut self, name: &str, args: &[Expr], span: &Span) -> Result<Type> {
        // Handle method calls with dot notation
        let (func_name, adjusted_args) = if name.contains('.') {
            let (target_var, method_name) = name.split_once('.').unwrap();
            
            // Get the type of the target variable
            let target_type = self.type_env.lookup(target_var)
                .ok_or_else(|| Error::new_semantic(
                    format!("Undefined variable: {}", target_var),
                    *span,
                ))?;
            
            // Extract struct name from the type
            let struct_name = match target_type {
                Type::Struct { name, .. } => name.clone(),
                _ => return Err(Error::new_semantic(
                    format!("{} is not a struct instance", target_var),
                    *span,
                )),
            };
            
            // Build the full method name
            let full_method_name = format!("{}::{}", struct_name, method_name);
            
            // For type checking, we don't need to evaluate the target, just check types
            (full_method_name, args)
        } else {
            (name.to_string(), args)
        };
        
        // Check if it's a method call or regular function
        let (param_types, return_type) = if let Some((params, ret_type, _)) = self.functions.get(&func_name) {
            (params.clone(), ret_type.clone())
        } else {
            return Err(Error::new_semantic(
                format!("Function '{}' not found", func_name),
                *span,
            ));
        };

        // Special handling for print/println - they accept any arguments
        if func_name == "print" || func_name == "println" {
            for arg in adjusted_args {
                self.visit_expr(arg)?;
            }
            return Ok(Type::Unit);
        }

        // Check argument count
        if adjusted_args.len() != param_types.len() {
            return Err(Error::new_semantic(
                format!(
                    "Function '{}' expects {} arguments, but {} were provided",
                    func_name,
                    param_types.len(),
                    adjusted_args.len()
                ),
                *span,
            ));
        }

        // Check argument types
        for (i, (arg, (_, expected_type))) in adjusted_args.iter().zip(param_types.iter()).enumerate() {
            let arg_type = self.visit_expr(arg)?;
            if arg_type != *expected_type {
                return Err(Error::new_semantic(
                    format!(
                        "Function '{}' argument {} type mismatch: expected {}, found {}",
                        func_name,
                        i + 1,
                        expected_type.to_string(),
                        arg_type.to_string()
                    ),
                    *span,
                ));
            }
        }

        Ok(return_type)
    }

    fn visit_struct_init(&mut self, name: &str, fields: &[(String, Expr)], span: &Span) -> Result<Type> {
        let struct_fields = match self.structs.get(name) {
            Some(fields) => fields.clone(),
            None => {
                return Err(Error::new_semantic(
                    format!("Struct '{}' not found", name),
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
                        format!("Struct '{}' has no field '{}'", name, field_name),
                        *span,
                    ));
                }
            };

            let field_type = self.visit_expr(field_expr)?;
            if field_type != expected_type {
                return Err(Error::new_semantic(
                    format!(
                        "Struct '{}' field '{}' type mismatch: expected {}, found {}",
                        name,
                        field_name,
                        expected_type.to_string(),
                        field_type.to_string()
                    ),
                    *span,
                ));
            }
        }

        Ok(Type::Struct {
            name: name.to_string(),
            fields: vec![], // Fields are stored separately
        })
    }

    fn visit_member_access(&mut self, object: &Expr, field: &str, span: &Span) -> Result<Type> {
        let object_type = self.visit_expr(object)?;

        match &object_type {
            Type::Struct { name, .. } => {
                let struct_fields = match self.structs.get(name) {
                    Some(fields) => fields,
                    None => {
                        return Err(Error::new_semantic(
                            format!("Struct '{}' not found in type information", name),
                            *span,
                        ));
                    }
                };

                match struct_fields.get(field) {
                    Some(field_type) => Ok(field_type.clone()),
                    None => Err(Error::new_semantic(
                        format!("Struct '{}' has no field '{}'", name, field),
                        *span,
                    )),
                }
            }
            _ => Err(Error::new_semantic(
                format!("Cannot access field '{}' on non-struct type: {}", field, object_type.to_string()),
                *span,
            )),
        }
    }

    fn visit_enum_variant_or_method_call(&mut self, target: &Expr, call: &str, args: &[Expr], span: &Span) -> Result<Type> {
        // This handles both enum variant construction and method calls
        if let Expr::Identifier(type_name, _) = target {
            // Check if it's an enum variant
            if let Some(enum_variants) = self.enums.get(type_name).cloned() {
                if let Some(variant_type) = enum_variants.get(call) {
                    match variant_type {
                        Some(associated_type) => {
                            if args.len() != 1 {
                                return Err(Error::new_semantic(
                                    format!(
                                        "Enum variant '{}::{}' expects 1 argument, but {} were provided",
                                        type_name, call, args.len()
                                    ),
                                    *span,
                                ));
                            }
                            let arg_type = self.visit_expr(&args[0])?;
                            if arg_type != *associated_type {
                                return Err(Error::new_semantic(
                                    format!(
                                        "Enum variant '{}::{}' expects {}, found {}",
                                        type_name, call, associated_type.to_string(), arg_type.to_string()
                                    ),
                                    *span,
                                ));
                            }
                        }
                        None => {
                            if !args.is_empty() {
                                return Err(Error::new_semantic(
                                    format!(
                                        "Enum variant '{}::{}' takes no arguments, but {} were provided",
                                        type_name, call, args.len()
                                    ),
                                    *span,
                                ));
                            }
                        }
                    }
                    return Ok(Type::Enum {
                        name: type_name.clone(),
                        variants: HashMap::new(), // Variants stored separately
                    });
                }
            }

            // Check if it's a method call
            let method_name = format!("{}::{}", type_name, call);
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

                for (i, (arg, (_, expected_type))) in args.iter().zip(param_types.iter()).enumerate() {
                    let arg_type = self.visit_expr(arg)?;
                    if arg_type != *expected_type {
                        return Err(Error::new_semantic(
                            format!(
                                "Method '{}' argument {} type mismatch: expected {}, found {}",
                                method_name,
                                i + 1,
                                expected_type.to_string(),
                                arg_type.to_string()
                            ),
                            *span,
                        ));
                    }
                }

                return Ok(return_type);
            }
        }

        Err(Error::new_semantic(
            format!("Unknown enum variant or method: {}::{}", target, call),
            *span,
        ))
    }

    // ===== Statement visit methods =====

    fn visit_let(&mut self, name: &str, expr: &Expr, _span: &Span) -> Result<Type> {
        let expr_type = self.visit_expr(expr)?;
        self.type_env.define(name.to_string(), expr_type.clone());
        Ok(Type::Unit)
    }

    fn visit_function(&mut self, name: &str, params: &[(String, String)], return_type: &str, body: &[Stmt], span: &Span) -> Result<Type> {
        // Convert parameter types
        let param_types: Vec<(String, Type)> = params
            .iter()
            .map(|(name, type_str)| {
                (
                    name.clone(),
                    Type::from_string(type_str).unwrap_or(Type::Unknown),
                )
            })
            .collect();
        
        let ret_type = Type::from_string(return_type).unwrap_or(Type::Unknown);
        
        // Register function (if it's not already registered by impl)
        if !self.functions.contains_key(name) {
            self.functions.insert(
                name.to_string(),
                (param_types.clone(), ret_type.clone(), *span),
            );
        }

        // Create new scope for function body
        self.type_env.push_scope();

        // Add parameters to scope
        for (param_name, param_type) in &param_types {
            // Special handling for 'self' parameter in methods
            if param_name == "self" {
                // Extract struct name from method name (e.g., "Point::new" -> "Point")
                if let Some(struct_name) = name.split("::").next() {
                    self.type_env.define(
                        "self".to_string(),
                        Type::Struct {
                            name: struct_name.to_string(),
                            fields: vec![],
                        },
                    );
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
        if let Some(Stmt::Expression(expr, _)) = body.last() {
            let expr_type = self.visit_expr(expr)?;
            if expr_type != ret_type {
                self.type_env.pop_scope();
                return Err(Error::new_semantic(
                    format!(
                        "Function {} return type mismatch: expected {}, found {}",
                        name,
                        ret_type.to_string(),
                        expr_type.to_string()
                    ),
                    *span,
                ));
            }
        }

        // Clean up scope
        self.type_env.pop_scope();

        Ok(Type::Unit)
    }

    fn visit_struct(&mut self, name: &str, fields: &[(String, String)], _span: &Span) -> Result<Type> {
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
            let field_type = Type::from_string(field_type_str).unwrap_or(Type::Unknown);
            struct_fields.insert(field_name.clone(), field_type);
        }

        self.structs.insert(name.to_string(), struct_fields);
        Ok(Type::Unit)
    }

    fn visit_enum(&mut self, name: &str, variants: &[(String, String)], _span: &Span) -> Result<Type> {
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
        for (variant_name, variant_type_str) in variants {
            let variant_type = if variant_type_str == "unit" {
                None
            } else {
                Some(Type::from_string(variant_type_str).unwrap_or(Type::Unknown))
            };
            enum_variants.insert(variant_name.to_string(), variant_type);
        }

        self.enums.insert(name.to_string(), enum_variants);
        Ok(Type::Unit)
    }

    fn visit_impl(&mut self, struct_name: &str, methods: &[Stmt], _span: &Span) -> Result<Type> {
        for method in methods {
            if let Stmt::Function(name, params, return_type, body, method_span) = method {
                // Convert parameter types
                let mut param_types: Vec<(String, Type)> = vec![];
                
                // Check if first parameter is 'self' (instance method) or not (static method)
                let is_instance_method = !params.is_empty() && params[0].0 == "self";
                
                if is_instance_method {
                    // Skip 'self' parameter for instance methods
                    for (param_name, param_type_str) in params.iter().skip(1) {
                        param_types.push((
                            param_name.clone(),
                            Type::from_string(param_type_str).unwrap_or(Type::Unknown),
                        ));
                    }
                } else {
                    // Include all parameters for static methods
                    for (param_name, param_type_str) in params.iter() {
                        param_types.push((
                            param_name.clone(),
                            Type::from_string(param_type_str).unwrap_or(Type::Unknown),
                        ));
                    }
                }

                let ret_type = Type::from_string(return_type).unwrap_or(Type::Unknown);
                
                // Register method
                self.functions.insert(
                    format!("{}::{}", struct_name, name),
                    (param_types.clone(), ret_type.clone(), *method_span),
                );
                
                // Now analyze the method body just like a regular function
                self.visit_function(
                    &format!("{}::{}", struct_name, name),
                    params,
                    return_type,
                    body,
                    method_span,
                )?;
            }
        }

        Ok(Type::Unit)
    }

    fn visit_if(&mut self, condition: &Expr, then_block: &[Stmt], else_block: &[Stmt], _span: &Span) -> Result<Type> {
        // Check condition is boolean
        let condition_type = self.visit_expr(condition)?;
        if condition_type != Type::Bool {
            return Err(Error::new_semantic(
                format!("If condition must be boolean, found {}", condition_type.to_string()),
                condition.span(),
            ));
        }

        // Check then block
        for stmt in then_block {
            self.visit_stmt(stmt)?;
        }

        // Check else block
        for stmt in else_block {
            self.visit_stmt(stmt)?;
        }

        Ok(Type::Unit)
    }

    fn visit_match(&mut self, expr: &Expr, arms: &[(Expr, Vec<Stmt>)], span: &Span) -> Result<Type> {
        let expr_type = self.visit_expr(expr)?;

        // For enum matching, track which variants have been matched
        let mut matched_variants = std::collections::HashSet::new();
        let enum_name = match &expr_type {
            Type::Enum { name, .. } => Some(name.clone()),
            _ => None,
        };

        for (pattern, body) in arms {
            // Save current match-bound variables
            let saved_vars = self.match_bound_vars.clone();

            // Analyze pattern and bind variables
            match pattern {
                Expr::Identifier(name, _) if name == "_" => {
                    // Wildcard pattern matches anything
                }
                Expr::EnumVariantOrMethodCall { target, call, args, .. } => {
                    if let Expr::Identifier(enum_type, _) = &**target {
                        if let Some(ref expected_enum) = enum_name {
                            if enum_type != expected_enum {
                                return Err(Error::new_semantic(
                                    format!("Expected enum '{}', found '{}'", expected_enum, enum_type),
                                    pattern.span(),
                                ));
                            }
                        }

                        // Track this variant as matched
                        matched_variants.insert(call.clone());

                        // Bind variables from enum variant
                        if !args.is_empty() {
                            if let Expr::Identifier(var_name, _) = &args[0] {
                                if let Some(enum_variants) = self.enums.get(enum_type) {
                                    if let Some(Some(variant_type)) = enum_variants.get(call) {
                                        self.match_bound_vars.insert(var_name.clone(), variant_type.clone());
                                    }
                                }
                            }
                        }
                    }
                }
                _ => {
                    // For now, other patterns just check type compatibility
                    let pattern_type = self.visit_expr(pattern)?;
                    if pattern_type != expr_type {
                        return Err(Error::new_semantic(
                            format!("Pattern type mismatch: expected {}, found {}", 
                                expr_type.to_string(), pattern_type.to_string()),
                            pattern.span(),
                        ));
                    }
                }
            }

            // Analyze body with bound variables
            for stmt in body {
                self.visit_stmt(stmt)?;
            }

            // Restore match-bound variables
            self.match_bound_vars = saved_vars;
        }

        // Check for exhaustiveness (only for enums for now)
        if let Some(enum_name) = enum_name {
            if let Some(enum_variants) = self.enums.get(&enum_name) {
                let has_wildcard = arms.iter().any(|(pattern, _)| {
                    matches!(pattern, Expr::Identifier(name, _) if name == "_")
                });

                if !has_wildcard {
                    for variant_name in enum_variants.keys() {
                        if !matched_variants.contains(variant_name) {
                            return Err(Error::new_semantic(
                                format!("Non-exhaustive match: variant '{}::{}' not covered", 
                                    enum_name, variant_name),
                                *span,
                            ));
                        }
                    }
                }
            }
        }

        Ok(Type::Unit)
    }

    fn visit_for_loop(&mut self, variable: &str, iterable: &Expr, body: &[Stmt], _span: &Span) -> Result<Type> {
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
                    format!("Cannot iterate over type: {}", iterable_type.to_string()),
                    iterable.span(),
                ));
            }
        };

        // Add loop variable to scope
        self.type_env.define(variable.to_string(), element_type);

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
                format!("While condition must be boolean, found {}", condition_type.to_string()),
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

    fn visit_expression_stmt(&mut self, expr: &Expr, _has_semicolon: bool) -> Result<Type> {
        // For now, just analyze the expression
        // Later, when implementing expression-based semantics, we'll use has_semicolon
        self.visit_expr(expr)?;
        Ok(Type::Unit)
    }
}