use std::io::{self, Write};

use indexmap::IndexMap;

use crate::{
    ast::visitor::AstVisitor,
    error::{Error, Result},
    parser::{Expr, Operator, Stmt},
    span::Span,
};

#[derive(Debug, PartialEq)]
pub enum ControlFlow {
    Normal,
    Break,
    Continue,
    Return(Value),
}

#[derive(Clone, Debug)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Array(Vec<Value>),
    Range(Option<i64>, Option<i64>, bool),
    Function(Function),
    Struct(String, IndexMap<String, String>),
    Enum(String, IndexMap<String, String>),
    StructInstance(String, IndexMap<String, Value>),
    EnumVariant(String, String, Option<Box<Value>>),
}

impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Unit => "()".to_string(),
            Value::Int(n) => n.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::String(s) => s.clone(),
            Value::Array(elements) => {
                let elements_str: Vec<String> = elements.iter().map(|e| e.to_string()).collect();
                format!("[{}]", elements_str.join(", "))
            }
            Value::Range(start, end, inclusive) => {
                let start_str = start
                    .as_ref()
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "None".to_string());
                let end_str = end
                    .as_ref()
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "None".to_string());
                format!(
                    "{}..{}{}",
                    start_str,
                    end_str,
                    if *inclusive { "=" } else { "" }
                )
            }
            Value::Function(_) => "function".to_string(),
            Value::Struct(name, _) => format!("struct {}", name),
            Value::StructInstance(name, fields) => {
                let mut field_strings = Vec::new();
                for (name, value) in fields {
                    field_strings.push(format!("{}: {:?}", name, value));
                }
                format!("struct {} {{{}}}", name, field_strings.join(", "))
            }
            Value::Enum(name, _) => format!("enum {}", name),
            Value::EnumVariant(name, variant, value) => match value {
                Some(value) => format!("{}::{}({})", name, variant, value),
                None => format!("{}::{}", name, variant),
            },
        }
    }

    pub fn type_str(&self) -> String {
        match self {
            Value::Unit => "void".to_string(),
            Value::Int(_) => "int".to_string(),
            Value::Float(_) => "float".to_string(),
            Value::Bool(_) => "bool".to_string(),
            Value::String(_) => "string".to_string(),
            Value::Array(_) => "array".to_string(),
            Value::Range(_, _, _) => "range".to_string(),
            Value::Function(_) => "function".to_string(),
            Value::Struct(name, _) => format!("struct {name}"),
            Value::StructInstance(name, _) => format!("struct instance {name}"),
            Value::Enum(name, _) => format!("enum {name}"),
            Value::EnumVariant(name, variant, value) => match value {
                Some(value) => format!("{name}::{variant}({value})"),
                None => format!("{name}::{variant}"),
            },
        }
    }

    pub fn as_mut_instance(&mut self) -> Option<&mut IndexMap<String, Value>> {
        match self {
            Value::StructInstance(_, fields) => Some(fields),
            _ => None,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Unit, Value::Unit) => true,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Function(_), Value::Function(_)) => false, // Functions are not comparable
            (Value::Struct(a, _), Value::Struct(b, _)) => a == b,
            (Value::StructInstance(a_name, a_values), Value::StructInstance(b_name, b_values)) => {
                a_name == b_name && a_values == b_values
            }
            (
                Value::Range(a_start, a_end, a_inclusive),
                Value::Range(b_start, b_end, b_inclusive),
            ) => a_start == b_start && a_end == b_end && a_inclusive == b_inclusive,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Function {
    UserDefined(
        String,
        Vec<(String, String)>,
        Vec<Stmt>,
        IndexMap<String, Value>,
    ),
    BuiltIn(fn(&[Value]) -> Result<Value>),
}

pub fn stdlib_print(args: &[Value]) -> Result<Value> {
    for arg in args {
        print!("{}", arg);
    }
    io::stdout()
        .flush()
        .map_err(|e| Error::new_runtime(format!("IO error: {}", e), Span::default()))?;
    Ok(Value::Int(0)) // print returns 0 on success
}

pub fn stdlib_println(args: &[Value]) -> Result<Value> {
    for arg in args {
        match arg {
            Value::EnumVariant(_, _, Some(value)) => print!("{}", value),
            _ => print!("{}", arg),
        }
    }
    println!();
    Ok(Value::Unit)
}

/// Interpreter implemented using the visitor pattern
pub struct InterpreterVisitor {
    environment: IndexMap<String, Value>,
    global_environment: IndexMap<String, Value>,
    control_flow: ControlFlow,
}

impl InterpreterVisitor {
    pub fn new() -> Self {
        let mut visitor = InterpreterVisitor {
            environment: IndexMap::new(),
            global_environment: IndexMap::new(),
            control_flow: ControlFlow::Normal,
        };
        visitor.init_standard_library();
        visitor
    }

    fn init_standard_library(&mut self) {
        self.global_environment.insert(
            "print".to_string(),
            Value::Function(Function::BuiltIn(stdlib_print)),
        );
        self.global_environment.insert(
            "println".to_string(),
            Value::Function(Function::BuiltIn(stdlib_println)),
        );
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<Value> {
        let mut last_value = Value::Unit;
        for stmt in stmts {
            last_value = self.visit_stmt(stmt)?;
            
            // Check for early return
            match &self.control_flow {
                ControlFlow::Return(value) => return Ok(value.clone()),
                ControlFlow::Break | ControlFlow::Continue => {
                    return Err(Error::new_runtime(
                        "Break or continue outside of loop".to_string(),
                        Span::default(),
                    ));
                }
                ControlFlow::Normal => {}
            }
        }
        Ok(last_value)
    }

    /// Get a variable from the environment
    fn get_var(&self, name: &str) -> Option<Value> {
        self.environment.get(name)
            .or_else(|| self.global_environment.get(name))
            .cloned()
    }

    /// Set a variable in the environment
    fn set_var(&mut self, name: String, value: Value) {
        self.environment.insert(name, value);
    }

    /// Push a new scope
    fn push_scope(&mut self) -> IndexMap<String, Value> {
        std::mem::take(&mut self.environment)
    }

    /// Pop a scope
    fn pop_scope(&mut self, old_env: IndexMap<String, Value>) {
        self.environment = old_env;
    }

    /// Evaluate a binary operation
    fn evaluate_binary_op(&self, left: Value, op: &Operator, right: Value, span: Span) -> Result<Value> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => match op {
                Operator::Plus => Ok(Value::Int(a + b)),
                Operator::Minus => Ok(Value::Int(a - b)),
                Operator::Multiply => Ok(Value::Int(a * b)),
                Operator::Divide => {
                    if b == 0 {
                        Err(Error::new_runtime("Division by zero".to_string(), span))
                    } else {
                        Ok(Value::Int(a / b))
                    }
                }
                Operator::Modulo => {
                    if b == 0 {
                        Err(Error::new_runtime("Division by zero".to_string(), span))
                    } else {
                        Ok(Value::Int(a % b))
                    }
                }
                Operator::Equals => Ok(Value::Bool(a == b)),
                Operator::LessThan => Ok(Value::Bool(a < b)),
                Operator::GreaterThan => Ok(Value::Bool(a > b)),
                Operator::LessThanEquals => Ok(Value::Bool(a <= b)),
                Operator::GreaterThanEquals => Ok(Value::Bool(a >= b)),
            },
            (Value::Float(a), Value::Float(b)) => match op {
                Operator::Plus => Ok(Value::Float(a + b)),
                Operator::Minus => Ok(Value::Float(a - b)),
                Operator::Multiply => Ok(Value::Float(a * b)),
                Operator::Divide => {
                    if b == 0.0 {
                        Err(Error::new_runtime("Division by zero".to_string(), span))
                    } else {
                        Ok(Value::Float(a / b))
                    }
                }
                Operator::Modulo => Ok(Value::Float(a % b)),
                Operator::Equals => Ok(Value::Bool(a == b)),
                Operator::LessThan => Ok(Value::Bool(a < b)),
                Operator::GreaterThan => Ok(Value::Bool(a > b)),
                Operator::LessThanEquals => Ok(Value::Bool(a <= b)),
                Operator::GreaterThanEquals => Ok(Value::Bool(a >= b)),
            },
            (Value::Int(a), Value::Float(b)) => {
                self.evaluate_binary_op(Value::Float(a as f64), op, Value::Float(b), span)
            }
            (Value::Float(a), Value::Int(b)) => {
                self.evaluate_binary_op(Value::Float(a), op, Value::Float(b as f64), span)
            }
            (Value::Bool(a), Value::Bool(b)) => match op {
                Operator::Equals => Ok(Value::Bool(a == b)),
                _ => Err(Error::new_runtime(
                    format!("Invalid operation {:?} for boolean values", op),
                    span,
                )),
            },
            (Value::String(a), Value::String(b)) => match op {
                Operator::Equals => Ok(Value::Bool(a == b)),
                _ => Err(Error::new_runtime(
                    format!("Invalid operation {:?} for string values", op),
                    span,
                )),
            },
            (Value::EnumVariant(type1, variant1, data1), Value::EnumVariant(type2, variant2, data2)) => {
                match op {
                    Operator::Equals => Ok(Value::Bool(type1 == type2 && variant1 == variant2 && data1 == data2)),
                    _ => Err(Error::new_runtime(
                        format!("Invalid operation {:?} for enum values", op),
                        span,
                    )),
                }
            }
            _ => Err(Error::new_runtime(
                format!("Type mismatch for binary operation {:?}", op),
                span,
            )),
        }
    }

    /// Execute a function call
    fn execute_function_call(&mut self, func: Value, args: Vec<Value>, span: Span) -> Result<Value> {
        match func {
            Value::Function(Function::BuiltIn(func)) => func(&args),
            Value::Function(Function::UserDefined(_, params, body, closure)) => {
                if args.len() != params.len() {
                    return Err(Error::new_runtime(
                        format!("Expected {} arguments, got {}", params.len(), args.len()),
                        span,
                    ));
                }

                // Save current environment and use closure
                let saved_env = self.push_scope();
                // Instead of completely replacing environment, merge closure with current functions
                let mut new_env = closure.clone();
                // Preserve function definitions from current environment
                for (name, value) in &self.environment {
                    if let Value::Function(_) = value {
                        new_env.insert(name.clone(), value.clone());
                    }
                }
                self.environment = new_env;

                // Bind arguments
                for ((name, _), value) in params.iter().zip(args.iter()) {
                    self.set_var(name.clone(), value.clone());
                }

                // Execute function body
                let mut result = Value::Unit;
                for stmt in &body {
                    result = self.visit_stmt(stmt)?;
                    
                    // Check for return
                    if let ControlFlow::Return(value) = &self.control_flow {
                        result = value.clone();
                        self.control_flow = ControlFlow::Normal;
                        break;
                    }
                }

                // Restore environment
                self.pop_scope(saved_env);
                Ok(result)
            }
            _ => Err(Error::new_runtime(
                "Expected function value".to_string(),
                span,
            )),
        }
    }
}

impl AstVisitor<Value> for InterpreterVisitor {
    type Error = Error;

    // ===== Expression visit methods =====

    fn visit_int(&mut self, value: i64, _span: &Span) -> Result<Value> {
        Ok(Value::Int(value))
    }

    fn visit_float(&mut self, value: f64, _span: &Span) -> Result<Value> {
        Ok(Value::Float(value))
    }

    fn visit_bool(&mut self, value: bool, _span: &Span) -> Result<Value> {
        Ok(Value::Bool(value))
    }

    fn visit_string(&mut self, value: &str, _span: &Span) -> Result<Value> {
        Ok(Value::String(value.to_string()))
    }

    fn visit_identifier(&mut self, name: &str, span: &Span) -> Result<Value> {
        self.get_var(name)
            .ok_or_else(|| Error::new_runtime(format!("Undefined variable: {}", name), *span))
    }

    fn visit_array(&mut self, elements: &[Expr], _span: &Span) -> Result<Value> {
        let mut values = Vec::new();
        for element in elements {
            values.push(self.visit_expr(element)?);
        }
        Ok(Value::Array(values))
    }

    fn visit_array_index(&mut self, array: &Expr, index: &Expr, span: &Span) -> Result<Value> {
        let array_value = self.visit_expr(array)?;
        let index_value = self.visit_expr(index)?;

        match (&array_value, &index_value) {
            (Value::Array(elements), Value::Int(i)) => {
                if *i < 0 || *i >= elements.len() as i64 {
                    return Err(Error::new_runtime(
                        format!("Array index out of bounds: {}", i),
                        *span,
                    ));
                }
                Ok(elements[*i as usize].clone())
            }
            (Value::Array(_), _) => Err(Error::new_runtime(
                format!("Array index must be an integer, found {:?}", index_value),
                *span,
            )),
            _ => Err(Error::new_runtime(
                "Cannot index non-array value".to_string(),
                *span,
            )),
        }
    }

    fn visit_range(&mut self, start: Option<&Expr>, end: Option<&Expr>, inclusive: bool, _span: &Span) -> Result<Value> {
        let start_val = match start {
            Some(expr) => match self.visit_expr(expr)? {
                Value::Int(n) => Some(n),
                _ => return Err(Error::new_runtime("Range start must be an integer".to_string(), expr.span())),
            },
            None => None,
        };

        let end_val = match end {
            Some(expr) => match self.visit_expr(expr)? {
                Value::Int(n) => Some(n),
                _ => return Err(Error::new_runtime("Range end must be an integer".to_string(), expr.span())),
            },
            None => None,
        };

        Ok(Value::Range(start_val, end_val, inclusive))
    }

    fn visit_binary_op(&mut self, left: &Expr, op: &Operator, right: &Expr, span: &Span) -> Result<Value> {
        let left_val = self.visit_expr(left)?;
        let right_val = self.visit_expr(right)?;
        self.evaluate_binary_op(left_val, op, right_val, *span)
    }

    fn visit_assign(&mut self, left: &Expr, right: &Expr, span: &Span) -> Result<Value> {
        let value = self.visit_expr(right)?;
        
        match left {
            Expr::Identifier(name, _) => {
                self.set_var(name.clone(), value);
                Ok(Value::Unit)
            }
            Expr::MemberAccess(object, field, _) => {
                match self.visit_expr(object)? {
                    Value::StructInstance(struct_name, mut fields) => {
                        fields.insert(field.clone(), value);
                        // Need to update the struct in the environment
                        if let Expr::Identifier(var_name, _) = &**object {
                            self.set_var(var_name.clone(), Value::StructInstance(struct_name, fields));
                        }
                        Ok(Value::Unit)
                    }
                    _ => Err(Error::new_runtime(
                        "Cannot assign to field of non-struct value".to_string(),
                        *span,
                    )),
                }
            }
            Expr::ArrayIndex(array_expr, index_expr, _) => {
                let index = match self.visit_expr(index_expr)? {
                    Value::Int(i) => i as usize,
                    _ => return Err(Error::new_runtime(
                        "Array index must be an integer".to_string(),
                        *span,
                    )),
                };
                
                match self.visit_expr(array_expr)? {
                    Value::Array(mut elements) => {
                        if index >= elements.len() {
                            return Err(Error::new_runtime(
                                format!("Array index out of bounds: {}", index),
                                *span,
                            ));
                        }
                        elements[index] = value;
                        // Need to update the array in the environment
                        if let Expr::Identifier(var_name, _) = &**array_expr {
                            self.set_var(var_name.clone(), Value::Array(elements));
                        }
                        Ok(Value::Unit)
                    }
                    _ => Err(Error::new_runtime(
                        "Cannot index non-array value".to_string(),
                        *span,
                    )),
                }
            }
            _ => Err(Error::new_runtime(
                "Invalid assignment target".to_string(),
                *span,
            )),
        }
    }

    fn visit_compound_assign(&mut self, left: &Expr, op: &Operator, right: &Expr, span: &Span) -> Result<Value> {
        match left {
            Expr::Identifier(name, _) => {
                let left_val = self.get_var(name)
                    .ok_or_else(|| Error::new_runtime(format!("Undefined variable: {}", name), *span))?;
                let right_val = self.visit_expr(right)?;
                let result = self.evaluate_binary_op(left_val, op, right_val, *span)?;
                self.set_var(name.clone(), result);
                Ok(Value::Unit)
            }
            _ => Err(Error::new_runtime(
                "Invalid compound assignment target".to_string(),
                *span,
            )),
        }
    }

    fn visit_function_call(&mut self, name: &str, args: &[Expr], span: &Span) -> Result<Value> {
        // Handle method calls with dot notation
        let (func_name, arg_values) = if name.contains('.') {
            let (target_var, method_name) = name.split_once('.').unwrap();
            
            // Get the target struct instance
            let target_value = self.get_var(target_var)
                .ok_or_else(|| Error::new_runtime(format!("Undefined variable: {}", target_var), *span))?;
            
            // Extract the struct type name
            let struct_name = match &target_value {
                Value::StructInstance(name, _) => name.clone(),
                _ => return Err(Error::new_runtime(
                    format!("{} is not a struct instance", target_var),
                    *span,
                )),
            };
            
            // Build the full method name
            let full_method_name = format!("{}::{}", struct_name, method_name);
            
            // Evaluate arguments and prepend 'self'
            let mut args_with_self = vec![target_value];
            for arg in args {
                args_with_self.push(self.visit_expr(arg)?);
            }
            
            (full_method_name, args_with_self)
        } else {
            // Regular function call
            let mut values = Vec::new();
            for arg in args {
                values.push(self.visit_expr(arg)?);
            }
            (name.to_string(), values)
        };
        
        let func = self.get_var(&func_name)
            .ok_or_else(|| Error::new_runtime(format!("Function '{}' not found", func_name), *span))?;

        self.execute_function_call(func, arg_values, *span)
    }

    fn visit_struct_init(&mut self, name: &str, fields: &[(String, Expr)], span: &Span) -> Result<Value> {
        // Check if struct exists
        let _struct_def = self.get_var(name)
            .ok_or_else(|| Error::new_runtime(format!("Undefined struct: {}", name), *span))?;

        let mut field_values = IndexMap::new();
        for (field_name, field_expr) in fields {
            let value = self.visit_expr(field_expr)?;
            field_values.insert(field_name.clone(), value);
        }

        Ok(Value::StructInstance(name.to_string(), field_values))
    }

    fn visit_member_access(&mut self, object: &Expr, field: &str, span: &Span) -> Result<Value> {
        let obj_value = self.visit_expr(object)?;
        
        match obj_value {
            Value::StructInstance(_, fields) => {
                fields.get(field)
                    .cloned()
                    .ok_or_else(|| Error::new_runtime(
                        format!("Field '{}' not found", field),
                        *span,
                    ))
            }
            _ => {
                // Check if it's a method call (will be handled by function call)
                if let Expr::Identifier(struct_name, _) = object {
                    let method_name = format!("{}::{}", struct_name, field);
                    self.get_var(&method_name)
                        .ok_or_else(|| Error::new_runtime(
                            format!("Field or method '{}' not found", field),
                            *span,
                        ))
                } else {
                    Err(Error::new_runtime(
                        format!("Cannot access field '{}' on non-struct value", field),
                        *span,
                    ))
                }
            }
        }
    }

    fn visit_enum_variant_or_method_call(&mut self, target: &Expr, call: &str, args: &[Expr], span: &Span) -> Result<Value> {
        if let Expr::Identifier(type_name, _) = target {
            // Check if it's an enum variant
            if let Some(Value::Enum(enum_name, _)) = self.get_var(type_name) {
                if enum_name == *type_name {
                    let data = if args.is_empty() {
                        None
                    } else if args.len() == 1 {
                        Some(Box::new(self.visit_expr(&args[0])?))
                    } else {
                        return Err(Error::new_runtime(
                            format!("Enum variant takes at most 1 argument, got {}", args.len()),
                            *span,
                        ));
                    };
                    return Ok(Value::EnumVariant(type_name.clone(), call.to_string(), data));
                }
            }

            // Check if it's a method call
            let method_name = format!("{}::{}", type_name, call);
            if let Some(func) = self.get_var(&method_name) {
                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(self.visit_expr(arg)?);
                }
                return self.execute_function_call(func, arg_values, *span);
            }
        }

        Err(Error::new_runtime(
            format!("Unknown enum variant or method: {}::{}", target, call),
            *span,
        ))
    }

    // ===== Statement visit methods =====

    fn visit_let(&mut self, name: &str, expr: &Expr, _span: &Span) -> Result<Value> {
        let value = self.visit_expr(expr)?;
        self.set_var(name.to_string(), value);
        Ok(Value::Unit)
    }

    fn visit_function(&mut self, name: &str, params: &[(String, String)], _return_type: &str, body: &[Stmt], _span: &Span) -> Result<Value> {
        // For recursive functions, we need to create a closure that includes the function itself
        // Step 1: Create closure that includes current environment
        let mut closure = self.environment.clone();
        
        // Step 2: Create a temporary function with current closure
        let temp_func = Value::Function(Function::UserDefined(
            name.to_string(),
            params.to_vec(),
            body.to_vec(),
            closure.clone(),
        ));
        
        // Step 3: Add this function to the closure for recursion
        closure.insert(name.to_string(), temp_func);
        
        // Step 4: Create the final function with closure that includes itself
        let final_func = Value::Function(Function::UserDefined(
            name.to_string(),
            params.to_vec(),
            body.to_vec(),
            closure,
        ));
        
        // Step 5: Register the function in the global environment so it's always accessible
        self.global_environment.insert(name.to_string(), final_func);
        
        Ok(Value::Unit)
    }

    fn visit_struct(&mut self, name: &str, fields: &[(String, String)], _span: &Span) -> Result<Value> {
        let mut field_map = IndexMap::new();
        for (field_name, field_type) in fields {
            field_map.insert(field_name.clone(), field_type.clone());
        }
        self.set_var(name.to_string(), Value::Struct(name.to_string(), field_map));
        Ok(Value::Unit)
    }

    fn visit_enum(&mut self, name: &str, variants: &[(String, String)], _span: &Span) -> Result<Value> {
        let mut variant_map = IndexMap::new();
        for (variant_name, variant_type) in variants {
            variant_map.insert(variant_name.clone(), variant_type.clone());
        }
        self.set_var(name.to_string(), Value::Enum(name.to_string(), variant_map));
        Ok(Value::Unit)
    }

    fn visit_impl(&mut self, struct_name: &str, methods: &[Stmt], span: &Span) -> Result<Value> {
        // Verify struct exists
        match self.get_var(struct_name) {
            Some(Value::Struct(_, _)) => {},
            _ => return Err(Error::new_runtime(
                format!("Undefined struct: {}", struct_name),
                *span,
            )),
        }

        // Register methods
        for method in methods {
            if let Stmt::Function(method_name, params, _, body, _) = method {
                let func = Value::Function(Function::UserDefined(
                    method_name.clone(),
                    params.clone(),
                    body.clone(),
                    self.environment.clone(),
                ));
                
                let full_name = format!("{}::{}", struct_name, method_name);
                self.set_var(full_name, func);
            }
        }

        Ok(Value::Unit)
    }

    fn visit_if(&mut self, condition: &Expr, then_block: &[Stmt], else_block: &[Stmt], _span: &Span) -> Result<Value> {
        let condition_val = self.visit_expr(condition)?;
        
        match condition_val {
            Value::Bool(true) => {
                let mut result = Value::Unit;
                for stmt in then_block {
                    result = self.visit_stmt(stmt)?;
                    if self.control_flow != ControlFlow::Normal {
                        return Ok(result);
                    }
                }
                Ok(result)
            }
            Value::Bool(false) => {
                let mut result = Value::Unit;
                for stmt in else_block {
                    result = self.visit_stmt(stmt)?;
                    if self.control_flow != ControlFlow::Normal {
                        return Ok(result);
                    }
                }
                Ok(result)
            }
            _ => Err(Error::new_runtime(
                "If condition must be a boolean".to_string(),
                condition.span(),
            )),
        }
    }

    fn visit_match(&mut self, expr: &Expr, arms: &[(Expr, Vec<Stmt>)], span: &Span) -> Result<Value> {
        let match_value = self.visit_expr(expr)?;
        
        for (pattern, body) in arms {
            // Simple pattern matching for now
            let matches = match pattern {
                Expr::Identifier(name, _) if name == "_" => true, // Wildcard
                Expr::EnumVariantOrMethodCall { target, call, args, .. } => {
                    if let (Expr::Identifier(enum_name, _), Value::EnumVariant(val_enum, val_variant, val_data)) = 
                        (&**target, &match_value) {
                        if enum_name == val_enum && call == val_variant {
                            // Bind pattern variables if any
                            if !args.is_empty() && args.len() == 1 {
                                if let (Expr::Identifier(var_name, _), Some(data)) = (&args[0], val_data) {
                                    self.set_var(var_name.clone(), *data.clone());
                                }
                            }
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
                _ => {
                    // Direct value comparison
                    let pattern_val = self.visit_expr(pattern)?;
                    match (&match_value, &pattern_val) {
                        (Value::Int(a), Value::Int(b)) => a == b,
                        (Value::Bool(a), Value::Bool(b)) => a == b,
                        (Value::String(a), Value::String(b)) => a == b,
                        _ => false,
                    }
                }
            };

            if matches {
                let mut result = Value::Unit;
                for stmt in body {
                    result = self.visit_stmt(stmt)?;
                    if self.control_flow != ControlFlow::Normal {
                        return Ok(result);
                    }
                }
                return Ok(result);
            }
        }

        Err(Error::new_runtime(
            "Non-exhaustive match".to_string(),
            *span,
        ))
    }

    fn visit_for_loop(&mut self, variable: &str, iterable: &Expr, body: &[Stmt], span: &Span) -> Result<Value> {
        let iter_value = self.visit_expr(iterable)?;
        
        match iter_value {
            Value::Array(elements) => {
                for element in elements {
                    self.set_var(variable.to_string(), element);
                    
                    for stmt in body {
                        self.visit_stmt(stmt)?;
                        
                        match self.control_flow {
                            ControlFlow::Break => {
                                self.control_flow = ControlFlow::Normal;
                                return Ok(Value::Unit);
                            }
                            ControlFlow::Continue => {
                                self.control_flow = ControlFlow::Normal;
                                break;
                            }
                            ControlFlow::Return(_) => return Ok(Value::Unit),
                            ControlFlow::Normal => {}
                        }
                    }
                }
            }
            Value::Range(start, end, inclusive) => {
                let start = start.unwrap_or(0);
                let end = end.ok_or_else(|| Error::new_runtime(
                    "Cannot iterate over unbounded range".to_string(),
                    *span,
                ))?;
                
                let range: Box<dyn Iterator<Item = i64>> = if inclusive {
                    Box::new(start..=end)
                } else {
                    Box::new(start..end)
                };
                
                for i in range {
                    self.set_var(variable.to_string(), Value::Int(i));
                    
                    for stmt in body {
                        self.visit_stmt(stmt)?;
                        
                        match self.control_flow {
                            ControlFlow::Break => {
                                self.control_flow = ControlFlow::Normal;
                                return Ok(Value::Unit);
                            }
                            ControlFlow::Continue => {
                                self.control_flow = ControlFlow::Normal;
                                break;
                            }
                            ControlFlow::Return(_) => return Ok(Value::Unit),
                            ControlFlow::Normal => {}
                        }
                    }
                }
            }
            _ => return Err(Error::new_runtime(
                format!("Cannot iterate over {:?}", iter_value),
                *span,
            )),
        }

        Ok(Value::Unit)
    }

    fn visit_while(&mut self, condition: &Expr, body: &[Stmt], _span: &Span) -> Result<Value> {
        loop {
            let condition_val = self.visit_expr(condition)?;
            
            match condition_val {
                Value::Bool(true) => {
                    for stmt in body {
                        self.visit_stmt(stmt)?;
                        
                        match self.control_flow {
                            ControlFlow::Break => {
                                self.control_flow = ControlFlow::Normal;
                                return Ok(Value::Unit);
                            }
                            ControlFlow::Continue => {
                                self.control_flow = ControlFlow::Normal;
                                break;
                            }
                            ControlFlow::Return(_) => return Ok(Value::Unit),
                            ControlFlow::Normal => {}
                        }
                    }
                }
                Value::Bool(false) => break,
                _ => return Err(Error::new_runtime(
                    "While condition must be a boolean".to_string(),
                    condition.span(),
                )),
            }
        }
        
        Ok(Value::Unit)
    }

    fn visit_loop(&mut self, body: &[Stmt], _span: &Span) -> Result<Value> {
        loop {
            for stmt in body {
                self.visit_stmt(stmt)?;
                
                match self.control_flow {
                    ControlFlow::Break => {
                        self.control_flow = ControlFlow::Normal;
                        return Ok(Value::Unit);
                    }
                    ControlFlow::Continue => {
                        self.control_flow = ControlFlow::Normal;
                        break;
                    }
                    ControlFlow::Return(_) => return Ok(Value::Unit),
                    ControlFlow::Normal => {}
                }
            }
        }
    }

    fn visit_break(&mut self, _span: &Span) -> Result<Value> {
        self.control_flow = ControlFlow::Break;
        Ok(Value::Unit)
    }

    fn visit_continue(&mut self, _span: &Span) -> Result<Value> {
        self.control_flow = ControlFlow::Continue;
        Ok(Value::Unit)
    }

    fn visit_expression_stmt(&mut self, expr: &Expr, _has_semicolon: bool) -> Result<Value> {
        // For now, just evaluate the expression
        // Later, when implementing expression-based semantics, we'll use has_semicolon
        self.visit_expr(expr)
    }

    fn visit_block(&mut self, stmts: &[Stmt], _span: &Span) -> Result<Value> {
        // TODO: In the future, we should implement proper scoping for blocks
        // For now, we'll execute statements in the current scope
        
        let mut block_value = Value::Unit;
        
        // Execute all statements in the block
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            
            match stmt {
                // If the last statement is an expression without semicolon,
                // the block evaluates to that expression's value
                Stmt::Expression(expr, false) if is_last => {
                    block_value = self.visit_expr(expr)?;
                }
                // Otherwise, just execute the statement normally
                _ => {
                    self.visit_stmt(stmt)?;
                    
                    // Check for control flow changes
                    match self.control_flow {
                        ControlFlow::Break | ControlFlow::Continue | ControlFlow::Return(_) => {
                            // If we hit a break/continue/return, stop executing and bubble up
                            return Ok(Value::Unit);
                        }
                        ControlFlow::Normal => {}
                    }
                }
            }
        }
        
        Ok(block_value)
    }
}