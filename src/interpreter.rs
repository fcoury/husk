use std::{
    collections::HashMap,
    io::{self, Write},
};

use crate::{
    error::{Error, Result},
    parser::{Expr, Operator, Stmt},
    span::Span,
};

pub struct Interpreter {
    environment: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interpreter = Interpreter {
            environment: HashMap::new(),
        };
        interpreter.init_standard_library();
        interpreter
    }

    fn init_standard_library(&mut self) {
        self.environment.insert(
            "print".to_string(),
            Value::Function(Function::BuiltIn(stdlib_print)),
        );
        self.environment.insert(
            "println".to_string(),
            Value::Function(Function::BuiltIn(stdlib_println)),
        );
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<()> {
        for stmt in stmts {
            self.execute_stmt(stmt)?;
        }
        Ok(())
    }

    fn execute_stmt(&mut self, stmt: &Stmt) -> Result<Value> {
        match stmt {
            Stmt::Let(name, expr, _) => {
                let value = self.evaluate_expr(expr)?;
                self.environment.insert(name.clone(), value);
            }
            Stmt::Struct(name, fields, _) => {
                let mut field_map = HashMap::new();
                for (field_name, field_type) in fields {
                    field_map.insert(field_name.clone(), field_type.clone());
                }
                self.environment
                    .insert(name.clone(), Value::Struct(name.clone(), field_map));
            }
            Stmt::Function(name, params, _, body, _) => {
                let func = Value::Function(Function::UserDefined(
                    params.clone(),
                    body.clone(),
                    self.environment.clone(),
                ));
                self.environment.insert(name.clone(), func);
            }
            Stmt::If(condition, then_block, else_block, _) => {
                let condition_value = self.evaluate_expr(condition)?;
                if let Value::Bool(true) = condition_value {
                    for stmt in then_block {
                        self.execute_stmt(stmt)?;
                    }
                } else {
                    for stmt in else_block {
                        self.execute_stmt(stmt)?;
                    }
                }
            }
            Stmt::Expression(expr) => {
                self.evaluate_expr(expr)?;
            }
            Stmt::ReturnExpression(expr) => {
                let value = self.evaluate_expr(expr)?;
                return Ok(value);
            }
        }
        Ok(Value::Void)
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Int(n, _) => Ok(Value::Int(*n)),
            Expr::Float(f, _) => Ok(Value::Float(*f)),
            Expr::String(s, _) => Ok(Value::String(s.to_string())),
            Expr::Bool(b, _) => Ok(Value::Bool(*b)),
            Expr::Identifier(name, span) => {
                self.environment.get(name).cloned().ok_or_else(|| {
                    Error::new_runtime(format!("Undefined variable: {}", name), *span)
                })
            }
            Expr::BinaryOp(left, op, right, span) => {
                let left_val = self.evaluate_expr(left)?;
                let right_val = self.evaluate_expr(right)?;
                self.evaluate_binary_op(left_val, op, right_val, *span)
            }
            Expr::FunctionCall(name, args, span) => {
                let func = self.environment.get(name).cloned().ok_or_else(|| {
                    Error::new_runtime(format!("Undefined function: {}", name), *span)
                })?;

                match func {
                    Value::Function(Function::BuiltIn(func)) => {
                        let evaluated_args: Result<Vec<Value>> =
                            args.iter().map(|arg| self.evaluate_expr(arg)).collect();
                        func(&evaluated_args?)
                    }
                    Value::Function(Function::UserDefined(params, body, func_env)) => {
                        if args.len() != params.len() {
                            return Err(Error::new_runtime(
                                format!("Function {} called with wrong number of arguments", name),
                                *span,
                            ));
                        }

                        let mut local_env = func_env;
                        for ((param_name, _), arg_expr) in params.iter().zip(args.iter()) {
                            let arg_value = self.evaluate_expr(arg_expr)?;
                            local_env.insert(param_name.clone(), arg_value);
                        }

                        let mut interpreter = Interpreter {
                            environment: local_env,
                        };

                        let mut result = Value::Void;
                        for stmt in body {
                            result = interpreter.execute_stmt(&stmt)?;
                        }

                        Ok(result)
                    }
                    _ => Err(Error::new_runtime(
                        format!("{} is not a function", name),
                        *span,
                    )),
                }
            }
            Expr::StructInit(name, fields, span) => {
                let struct_type = self.environment.get(name).cloned().ok_or_else(|| {
                    Error::new_runtime(format!("Undefined struct: {}", name), *span)
                })?;

                let Value::Struct(_, struct_fields) = struct_type else {
                    return Err(Error::new_runtime(
                        format!("{} is not a struct", name),
                        *span,
                    ));
                };

                let mut instance_fields = HashMap::new();
                for (field_name, field_expr) in fields.iter() {
                    let Some(expected_type) = struct_fields.get(field_name) else {
                        return Err(Error::new_runtime(
                            format!("Missing field {} in struct {}", field_name, name),
                            *span,
                        ));
                    };

                    let field_value = self.evaluate_expr(field_expr)?;
                    let field_type = field_value.type_str();

                    if field_type != *expected_type {
                        return Err(Error::new_runtime(
                            format!(
                                "Field {} in struct {} has wrong type: expected {}, got {}",
                                field_name, name, expected_type, field_type
                            ),
                            *span,
                        ));
                    }
                    instance_fields.insert(field_name.clone(), field_value);
                }

                Ok(Value::StructInstance(name.to_string(), instance_fields))
            }
            Expr::MemberAccess(struct_expr, field_name, span) => {
                let struct_value = self.evaluate_expr(struct_expr)?;
                match struct_value {
                    Value::StructInstance(_name, fields) => {
                        fields.get(field_name).cloned().ok_or_else(|| {
                            Error::new_runtime(
                                format!("Field {} not found in struct instance", field_name),
                                *span,
                            )
                        })
                    }
                    _ => Err(Error::new_runtime(
                        "Field access on non-struct value".to_string(),
                        *span,
                    )),
                }
            }
        }
    }

    fn evaluate_binary_op(
        &self,
        left: Value,
        op: &Operator,
        right: Value,
        span: Span,
    ) -> Result<Value> {
        match (left, right) {
            (Value::Int(left), Value::Int(right)) => match op {
                Operator::Plus => Ok(Value::Int(left + right)),
                Operator::Minus => Ok(Value::Int(left - right)),
                Operator::Multiply => Ok(Value::Int(left * right)),
                Operator::Divide => Ok(Value::Int(left / right)),
                // _ => Err(Error::new_runtime(
                //     format!("Unsupported binary operator: {:#?}", op),
                //     span,
                // )),
            },
            _ => Err(Error::new_runtime(
                "Binary operation on non-numeric values".to_string(),
                span,
            )),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Void,
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Function(Function),
    Struct(String, HashMap<String, String>),
    StructInstance(String, HashMap<String, Value>),
}

impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Void => "void".to_string(),
            Value::Int(n) => n.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::String(s) => s.clone(),
            Value::Function(_) => "function".to_string(),
            Value::Struct(name, _) => format!("struct {}", name),
            Value::StructInstance(name, fields) => {
                let mut field_strings = Vec::new();
                for (name, value) in fields {
                    field_strings.push(format!("{}: {:?}", name, value));
                }
                format!("struct {} {{{}}}", name, field_strings.join(", "))
            }
        }
    }

    pub fn type_str(&self) -> String {
        match self {
            Value::Void => "void".to_string(),
            Value::Int(_) => "int".to_string(),
            Value::Float(_) => "float".to_string(),
            Value::Bool(_) => "bool".to_string(),
            Value::String(_) => "string".to_string(),
            Value::Function(_) => "function".to_string(),
            Value::Struct(name, _) => format!("struct {name}"),
            Value::StructInstance(name, _) => format!("struct instance {name}"),
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
            (Value::Void, Value::Void) => true,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Function(_), Value::Function(_)) => false, // Functions are not comparable
            (Value::Struct(a, _), Value::Struct(b, _)) => a == b,
            (Value::StructInstance(a_name, a_values), Value::StructInstance(b_name, b_values)) => {
                a_name == b_name && a_values == b_values
            }
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Function {
    UserDefined(Vec<(String, String)>, Vec<Stmt>, HashMap<String, Value>),
    BuiltIn(fn(&[Value]) -> Result<Value>),
}

fn stdlib_print(args: &[Value]) -> Result<Value> {
    for arg in args {
        print!("{}", arg);
    }
    io::stdout()
        .flush()
        .map_err(|e| Error::new_runtime(format!("IO error: {}", e), Span::default()))?;
    Ok(Value::Int(0)) // print returns 0 on success
}

fn stdlib_println(args: &[Value]) -> Result<Value> {
    stdlib_print(args)?;
    println!();
    Ok(Value::Int(0)) // println returns 0 on success
}
