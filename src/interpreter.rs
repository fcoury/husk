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
            Stmt::Function(name, params, _, body, _) => {
                let func = Value::Function(Function::UserDefined(
                    params.clone(),
                    body.clone(),
                    self.environment.clone(),
                ));
                self.environment.insert(name.clone(), func);
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

#[derive(Clone)]
pub enum Value {
    Void,
    Int(i64),
    Float(f64),
    String(String),
    Function(Function),
    // Function {
    //     params: Vec<(String, String)>, // Parameter names and their types
    //     body: Vec<Stmt>,
    //     environment: HashMap<String, Value>, // Closure environment
    // },
    // Add other types as needed
}

#[derive(Clone)]
pub enum Function {
    UserDefined(Vec<(String, String)>, Vec<Stmt>, HashMap<String, Value>),
    BuiltIn(fn(&[Value]) -> Result<Value>),
}

fn stdlib_print(args: &[Value]) -> Result<Value> {
    for arg in args {
        match arg {
            Value::Int(n) => print!("{}", n),
            Value::String(s) => print!("{}", s),
            _ => {
                return Err(Error::new_runtime(
                    "Unsupported type for print".to_string(),
                    Span::default(),
                ))
            }
        }
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
