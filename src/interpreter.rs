use std::io::{self, Write};

use indexmap::IndexMap;

use crate::{
    error::{Error, Result},
    parser::{Expr, Operator, Stmt},
    span::Span,
};

pub struct Interpreter {
    environment: IndexMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interpreter = Interpreter {
            environment: IndexMap::new(),
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

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<Value> {
        let mut value = Value::Unit;
        for stmt in stmts {
            (value, _) = self.execute_stmt(stmt)?;
        }
        Ok(value)
    }

    pub fn get_var(&self, name: &str) -> Option<Value> {
        self.environment.get(name).cloned()
    }

    fn execute_stmt(&mut self, stmt: &Stmt) -> Result<(Value, ControlFlow)> {
        match stmt {
            Stmt::Let(name, expr, _) => {
                let value = self.evaluate_expr(expr)?;
                self.environment.insert(name.clone(), value);
            }
            Stmt::Struct(name, fields, _) => {
                let mut field_map = IndexMap::new();
                for (field_name, field_type) in fields {
                    field_map.insert(field_name.clone(), field_type.clone());
                }
                self.environment
                    .insert(name.clone(), Value::Struct(name.clone(), field_map));
            }
            Stmt::Enum(name, variants, _) => {
                let mut variant_map = IndexMap::new();
                for (variant_name, variant_type) in variants {
                    variant_map.insert(variant_name.clone(), variant_type.clone());
                }
                self.environment
                    .insert(name.clone(), Value::Enum(name.clone(), variant_map));
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
                let mut result = (Value::Unit, ControlFlow::Normal);
                let condition_value = self.evaluate_expr(condition)?;
                if let Value::Bool(true) = condition_value {
                    for stmt in then_block {
                        result = self.execute_stmt(stmt)?;
                    }
                } else {
                    for stmt in else_block {
                        result = self.execute_stmt(stmt)?;
                    }
                }
                return Ok(result);
            }
            Stmt::Match(expr, arms, _) => {
                let expr_value = self.evaluate_expr(expr)?;
                match expr_value {
                    Value::EnumVariant(ref name, ref variant, ref value) => {
                        return self
                            .execute_enum_match(&arms, name, variant, value)
                            .map(|v| (v, ControlFlow::Normal));
                    }
                    _ => {}
                }
            }
            Stmt::Expression(expr) => {
                let value = self.evaluate_expr(expr)?;
                return Ok((value, ControlFlow::Normal));
            }
            Stmt::ForLoop(variable, iterable, body, span) => {
                let iterable_value = self.evaluate_expr(iterable)?;
                match iterable_value {
                    Value::Array(elements) => {
                        for element in elements {
                            self.environment.insert(variable.clone(), element.clone());
                            for stmt in body {
                                let (_, control_flow) = self.execute_stmt(stmt)?;
                                match control_flow {
                                    ControlFlow::Break => {
                                        return Ok((Value::Unit, ControlFlow::Normal))
                                    }
                                    ControlFlow::Continue => break,
                                    ControlFlow::Normal => {}
                                }
                            }
                        }
                    }
                    Value::Range(start, end, inclusive) => {
                        let start = start.unwrap_or(0);
                        let end = end.unwrap_or(i64::MAX);

                        if inclusive {
                            for i in start..=end {
                                self.environment.insert(variable.clone(), Value::Int(i));
                                for stmt in body {
                                    let (_, control_flow) = self.execute_stmt(stmt)?;
                                    match control_flow {
                                        ControlFlow::Break => {
                                            return Ok((Value::Unit, ControlFlow::Normal))
                                        }
                                        ControlFlow::Continue => break,
                                        ControlFlow::Normal => {}
                                    }
                                }
                            }
                        } else {
                            for i in start..end {
                                self.environment.insert(variable.clone(), Value::Int(i));
                                for stmt in body {
                                    let (_, control_flow) = self.execute_stmt(stmt)?;
                                    match control_flow {
                                        ControlFlow::Break => {
                                            return Ok((Value::Unit, ControlFlow::Normal))
                                        }
                                        ControlFlow::Continue => break,
                                        ControlFlow::Normal => {}
                                    }
                                }
                            }
                        }
                    }
                    _ => {
                        return Err(Error::new_runtime(
                            "For loop iterable must be an array or range".to_string(),
                            *span,
                        ))
                    }
                }
            }
            Stmt::Break(_) => return Ok((Value::Unit, ControlFlow::Break)),
            Stmt::Continue(_) => return Ok((Value::Unit, ControlFlow::Continue)),
        }
        Ok((Value::Unit, ControlFlow::Normal))
    }

    fn execute_enum_match(
        &mut self,
        arms: &[(Expr, Vec<Stmt>)],
        name: &str,
        variant: &str,
        value: &Option<Box<Value>>,
    ) -> Result<Value> {
        let mut res = Value::Unit;

        for (expr, body) in arms {
            let Expr::EnumVariant {
                name: arm_name,
                variant: arm_variant,
                value: arm_value,
                span: _,
            } = expr
            else {
                // FIXME: not expected?
                return Err(Error::new_runtime(
                    format!("Undefined variant '{}'", expr),
                    expr.span(),
                ));
            };

            if arm_name != name {
                unreachable!("Enum match with wrong enum name");
            }

            if arm_variant != variant {
                continue;
            }

            let mut local_env = self.environment.clone();

            let Some(arm_value) = arm_value else {
                for stmt in body {
                    (res, _) = self.execute_stmt(stmt).unwrap();
                }
                return Ok(res);
            };

            let Expr::Identifier(param_name, _) = arm_value.as_ref() else {
                unimplemented!("Enum match without binding");
            };

            if let Some(value) = value {
                local_env.insert(param_name.to_string(), *value.clone());
            }

            let mut interpreter = Interpreter {
                environment: local_env,
            };

            for stmt in body {
                (res, _) = interpreter.execute_stmt(stmt).unwrap();
            }
        }

        Ok(res)
    }

    fn evaluate_parts(&mut self, name: &str, span: Span) -> Result<Value> {
        let parts: Vec<&str> = name.split("::").collect();
        let first_part = parts.first().unwrap();
        let rest = &parts[1..];

        let value = self.environment.get(*first_part).cloned().ok_or_else(|| {
            Error::new_runtime(format!("Undefined variable: {}", first_part), span)
        })?;

        match value {
            Value::Enum(_, variants) => {
                let variant_name = rest.first().unwrap();
                let _variant_type = variants.get(*variant_name).ok_or_else(|| {
                    Error::new_runtime(format!("Undefined variant: {}", variant_name), span)
                })?;

                Ok(Value::EnumVariant(
                    first_part.to_string(),
                    variant_name.to_string(),
                    None,
                ))
            }
            _ => {
                return Err(Error::new_runtime(
                    format!("{} is not a valid identifier", name),
                    span,
                ));
            }
        }
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Int(n, _) => Ok(Value::Int(*n)),
            Expr::Float(f, _) => Ok(Value::Float(*f)),
            Expr::String(s, _) => Ok(Value::String(s.to_string())),
            Expr::Bool(b, _) => Ok(Value::Bool(*b)),
            Expr::Identifier(name, span) => {
                if name.contains("::") {
                    return self.evaluate_parts(name, *span);
                }

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

                        let mut result = Value::Unit;
                        for stmt in body {
                            (result, _) = interpreter.execute_stmt(&stmt)?;
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

                let mut instance_fields = IndexMap::new();
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
            Expr::EnumVariant {
                name,
                variant,
                value,
                span: _,
            } => {
                let value = value
                    .as_ref()
                    .map(|expr| self.evaluate_expr(expr))
                    .transpose()?
                    .map(Box::new);
                Ok(Value::EnumVariant(
                    name.to_string(),
                    variant.to_string(),
                    value,
                ))
            }
            Expr::Assign(left, right, span) => {
                match left.as_ref() {
                    Expr::Identifier(name, _) => {
                        // TODO: mutability
                        let value = self.evaluate_expr(right)?;
                        self.environment.insert(name.clone(), value);
                        Ok(Value::Unit)
                    }
                    Expr::MemberAccess(access_expr, field_name, _) => match access_expr.as_ref() {
                        Expr::Identifier(name, span) => {
                            let new_value = self.evaluate_expr(right)?;

                            let Some(value) = self.environment.get_mut(name) else {
                                return Err(Error::new_runtime(
                                    format!("Undefined struct instance: {}", name),
                                    *span,
                                ));
                            };

                            let Some(instance) = value.as_mut_instance() else {
                                return Err(Error::new_runtime(
                                    format!("{} is not a struct instance", name),
                                    *span,
                                ));
                            };

                            instance.get_mut(field_name).map(|field| *field = new_value);
                            Ok(Value::Unit)
                        }
                        _ => Err(Error::new_runtime(
                            "Invalid assignment target".to_string(),
                            *span,
                        )),
                    },
                    _ => Err(Error::new_runtime(
                        "Invalid assignment target".to_string(),
                        *span,
                    )),
                }
            }
            Expr::CompoundAssign(left, op, right, span) => match left.as_ref() {
                Expr::Identifier(name, _) => {
                    let left_value = self.environment.get(name).cloned().ok_or_else(|| {
                        Error::new_runtime(format!("Undefined variable: {}", name), *span)
                    })?;
                    let right_value = self.evaluate_expr(right)?;
                    let result = self.evaluate_binary_op(left_value, op, right_value, *span)?;
                    self.environment.insert(name.clone(), result);
                    Ok(Value::Unit)
                }
                _ => Err(Error::new_runtime(
                    "Invalid compound assignment target".to_string(),
                    *span,
                )),
            },
            Expr::Array(elements, _) => {
                let mut values = Vec::new();
                for element in elements {
                    values.push(self.evaluate_expr(element)?);
                }
                Ok(Value::Array(values))
            }
            Expr::ArrayIndex(array_expr, index_expr, span) => {
                let array = self.evaluate_expr(array_expr)?;
                let index = self.evaluate_expr(index_expr)?;

                match (array, index) {
                    (Value::Array(elements), Value::Int(i)) => {
                        if i < 0 || i >= elements.len() as i64 {
                            return Err(Error::new_runtime(
                                format!("Array index out of bounds: {}", i),
                                *span,
                            ));
                        }
                        Ok(elements[i as usize].clone())
                    }
                    (Value::Array(elements), Value::Range(start, end, inclusive)) => {
                        let start = start.unwrap_or(0);
                        let end = end.unwrap_or(elements.len() as i64);
                        let end = if inclusive { end + 1 } else { end };

                        let mut result = Vec::new();
                        for i in start..end {
                            result.push(elements[i as usize].clone());
                        }
                        Ok(Value::Array(result))
                    }
                    (Value::Array(_), index) => Err(Error::new_runtime(
                        format!("Array index must be an integer, got {:?}", index),
                        *span,
                    )),
                    (_, _) => Err(Error::new_runtime(
                        "Cannot perform array access on non-array value".to_string(),
                        *span,
                    )),
                }
            }
            Expr::Range(start, end, inclusive, span) => {
                let start_val = start.as_ref().map(|e| self.evaluate_expr(e)).transpose()?;
                let end_val = end.as_ref().map(|e| self.evaluate_expr(e)).transpose()?;

                match (start_val, end_val) {
                    (Some(Value::Int(s)), Some(Value::Int(e))) => {
                        Ok(Value::Range(Some(s), Some(e), *inclusive))
                    }
                    (Some(Value::Int(s)), None) => Ok(Value::Range(Some(s), None, *inclusive)),
                    (None, Some(Value::Int(e))) => Ok(Value::Range(None, Some(e), *inclusive)),
                    (None, None) => Ok(Value::Range(None, None, *inclusive)),
                    _ => Err(Error::new_runtime(
                        "Invalid range values".to_string(),
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
                Operator::Equals => Ok(Value::Bool(left == right)),
                Operator::Modulo => Ok(Value::Int(left % right)),
                Operator::LessThan => Ok(Value::Bool(left < right)),
                Operator::LessThanEquals => Ok(Value::Bool(left <= right)),
                Operator::GreaterThan => Ok(Value::Bool(left > right)),
                Operator::GreaterThanEquals => Ok(Value::Bool(left >= right)),
            },
            (Value::Float(left), Value::Float(right)) => match op {
                Operator::Plus => Ok(Value::Float(left + right)),
                Operator::Minus => Ok(Value::Float(left - right)),
                Operator::Multiply => Ok(Value::Float(left * right)),
                Operator::Divide => Ok(Value::Float(left / right)),
                Operator::Equals => Ok(Value::Bool(left == right)),
                Operator::Modulo => Ok(Value::Float(left % right)),
                Operator::LessThan => Ok(Value::Bool(left < right)),
                Operator::LessThanEquals => Ok(Value::Bool(left <= right)),
                Operator::GreaterThan => Ok(Value::Bool(left > right)),
                Operator::GreaterThanEquals => Ok(Value::Bool(left >= right)),
            },
            (Value::Int(_), Value::Float(_)) => Err(Error::new_runtime(
                "Can't compare integer with float".to_string(),
                span,
            )),
            (Value::Float(_), Value::Int(_)) => Err(Error::new_runtime(
                "Can't compare float with integer".to_string(),
                span,
            )),
            _ => Err(Error::new_runtime(
                "Binary operation on non-numeric values".to_string(),
                span,
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ControlFlow {
    Normal,
    Break,
    Continue,
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
    UserDefined(Vec<(String, String)>, Vec<Stmt>, IndexMap<String, Value>),
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

#[cfg(test)]
mod tests {
    use crate::{Lexer, Parser};

    use super::*;

    fn run_code(code: &str, interpreter: Option<&mut Interpreter>) -> Result<Value> {
        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = match parser.parse() {
            Ok(ast) => ast,
            Err(e) => panic!("{}", e.pretty_print(code)),
        };

        let interpreter = match interpreter {
            Some(interpreter) => interpreter,
            None => &mut Interpreter::new(),
        };
        interpreter.interpret(&ast)
    }

    #[test]
    fn test_fn_struct_return() {
        let code = r#"
            struct Point {
                x: int,
                y: int,
            }

            fn make_point(x: int, y: int) -> Point {
                Point { x: x, y: y }
            }

            let p = make_point(1, 2);
            p
        "#;

        assert_eq!(
            run_code(code, None).unwrap(),
            Value::StructInstance(
                "Point".to_string(),
                vec![
                    ("x".to_string(), Value::Int(1)),
                    ("y".to_string(), Value::Int(2))
                ]
                .into_iter()
                .collect()
            )
        );
    }

    #[test]
    fn test_if_expr_eq() {
        let code = r#"
            let x = 1;
            let y = 2;
            if x == y {
                1
            } else {
                0
            }
        "#;

        // FIXME: identify why this is returning void
        assert_eq!(run_code(code, None).unwrap(), Value::Int(0));
    }

    #[test]
    fn test_enum() {
        let code = r#"
            enum Color {
                Red,
                Green,
                Blue,
            }

            let c = Color::Red;
            c
        "#;

        let val = run_code(code, None).unwrap();

        let Value::EnumVariant(name, variant, _) = val else {
            panic!("Expected EnumVariant, got {:?}", val);
        };

        assert_eq!(name, "Color");
        assert_eq!(variant, "Red");
    }

    #[test]
    fn test_enum_with_associated_value() {
        let code = r#"
            enum Name {
                Existing(String),
                NotExisting,
            }

            let n = Name::Existing("Alice");
            n
        "#;

        let val = run_code(code, None).unwrap();

        let Value::EnumVariant(name, variant, value) = val else {
            panic!("Expected EnumVariant, got {:?}", val);
        };

        assert_eq!(name, "Name");
        assert_eq!(variant, "Existing");
        assert_eq!(value.unwrap().as_ref(), &Value::String("Alice".to_string()));
    }

    #[test]
    fn test_match_enum() {
        let mut int = Interpreter::new();

        let code = r#"
            enum Name {
                Existing(String),
                NotExisting,
            }
        "#;
        run_code(code, Some(&mut int)).unwrap();

        let code = r#"
            let n = Name::Existing("Alice");
            match n {
                Name::Existing(name) => name,
                Name::NotExisting => "Not existing",
            }
        "#;

        match run_code(code, Some(&mut int)) {
            Ok(val) => assert_eq!(val, Value::String("Alice".to_string())),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }

        let code = r#"
            let n = Name::NotExisting;
            match n {
                Name::Existing(name) => name,
                Name::NotExisting => "Not existing",
            }
        "#;

        match run_code(code, Some(&mut int)) {
            Ok(val) => assert_eq!(val, Value::String("Not existing".to_string())),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_enum_invalid_member() {
        let code = r#"
            enum Option {
              None,
              Some(string),
            }
                      
            let option = Option::Some("Hello");
            match option {
              None => println("None"),
              Some(value) => println(value),
            }
        "#;

        match run_code(code, None) {
            Ok(_) => panic!("Expected error"),
            Err(e) => assert_eq!(
                e.pretty_print(code),
                "error: 8:15 - Undefined variant 'None'\n              None => println(\"None\"),\n              ^^^^"
            ),
        }
    }

    #[test]
    fn test_line_comments() {
        let code = r#"
            // This is a comment
            let x = 1; // This is another comment
            x
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(val, Value::Int(1)),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_block_comments() {
        let code = r#"
            /* 
             * This is a 
             * multi-line comment 
             */
            let x = 1; /* This is another comment */
            x
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(val, Value::Int(1)),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_array_decl() {
        let code = r#"
            let arr = [1,2,3,4,5];
            arr
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(
                val,
                Value::Array(vec![
                    Value::Int(1),
                    Value::Int(2),
                    Value::Int(3),
                    Value::Int(4),
                    Value::Int(5)
                ])
            ),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_array_access() {
        let code = r#"
            let arr = [1,2,3,4,5];
            arr[2]
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(val, Value::Int(3)),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_range() {
        let code = r#"
            let r = 1..5;
            r
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(val, Value::Range(Some(1), Some(5), false)),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_inclusive_range() {
        let code = r#"
            let r = 1..=5;
            r
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(val, Value::Range(Some(1), Some(5), true)),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_range_of_array() {
        let code = r#"
            let arr = [1,2,3,4,5];
            arr[1..3]
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(val, Value::Array(vec![Value::Int(2), Value::Int(3)])),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_until_end_range_of_array() {
        let code = r#"
            let arr = [1,2,3,4,5];
            arr[1..]
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(
                val,
                Value::Array(vec![
                    Value::Int(2),
                    Value::Int(3),
                    Value::Int(4),
                    Value::Int(5)
                ])
            ),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_interpret_for_loop_array() {
        let code = r#"
            let a = [1, 2, 3, 4, 5];
            let sum = 0;
            for x in a {
                sum = sum + x;
            }
            sum
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(val, Value::Int(15)),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_interpret_for_loop_range() {
        let code = r#"
            let sum = 0;
            for i in 1..=5 {
                sum = sum + i;
            }
            sum
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(val, Value::Int(15)),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_for_loop_with_break() {
        let code = r#"
            let a = [1, 2, 3, 4, 5];
            let sum = 0;
            for x in a {
                if x == 3 {
                    break;
                }
                sum = sum + x;
            }
            sum
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(val, Value::Int(3)),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_for_loop_with_continue() {
        let code = r#"
            let a = [1, 2, 3, 4, 5];
            let sum = 0;
            for x in a {
                if x == 3 {
                    continue;
                }
                sum = sum + x;
            }
            sum
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(val, Value::Int(12)),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_plus_compound_assignment() {
        let code = r#"
            let x = 1;
            x += 2;
            x
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(val, Value::Int(3)),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }

    #[test]
    fn test_minus_compound_assignment() {
        let code = r#"
            let x = 3;
            x -= 2;
            x
        "#;

        match run_code(code, None) {
            Ok(val) => assert_eq!(val, Value::Int(1)),
            Err(e) => panic!("{}", e.pretty_print(code)),
        }
    }
}
