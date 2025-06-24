use crate::{
    parser::{Expr, Stmt, Operator, UnaryOp, UsePath, UseItems, ExternItem, EnumVariant},
    span::Span,
};

/// Combined visitor trait for expressions and statements
pub trait AstVisitor<T> {
    type Error;

    // ===== Expression visiting methods =====
    
    /// Visit an expression and dispatch to the appropriate method
    fn visit_expr(&mut self, expr: &Expr) -> std::result::Result<T, Self::Error> {
        match expr {
            Expr::Int(value, span) => self.visit_int(*value, span),
            Expr::Float(value, span) => self.visit_float(*value, span),
            Expr::Bool(value, span) => self.visit_bool(*value, span),
            Expr::Unit(span) => self.visit_unit(span),
            Expr::String(value, span) => self.visit_string(value, span),
            Expr::Identifier(name, span) => self.visit_identifier(name, span),
            Expr::Array(elements, span) => self.visit_array(elements, span),
            Expr::ArrayIndex(array, index, span) => self.visit_array_index(array, index, span),
            Expr::Range(start, end, inclusive, span) => self.visit_range(start.as_deref(), end.as_deref(), *inclusive, span),
            Expr::BinaryOp(left, op, right, span) => self.visit_binary_op(left, op, right, span),
            Expr::UnaryOp(op, expr, span) => self.visit_unary_op(op, expr, span),
            Expr::Assign(left, right, span) => self.visit_assign(left, right, span),
            Expr::CompoundAssign(left, op, right, span) => self.visit_compound_assign(left, op, right, span),
            Expr::FunctionCall(name, args, span) => self.visit_function_call(name, args, span),
            Expr::StructInit(name, fields, span) => self.visit_struct_init(name, fields, span),
            Expr::MemberAccess(object, field, span) => self.visit_member_access(object, field, span),
            Expr::EnumVariantOrMethodCall { target, call, args, span, .. } => {
                self.visit_enum_variant_or_method_call(target, call, args, span)
            }
            Expr::Block(stmts, span) => self.visit_block(stmts, span),
            Expr::If(condition, then_block, else_block, span) => {
                self.visit_if_expr(condition, then_block, else_block, span)
            }
            Expr::Match(expr, arms, span) => self.visit_match_expr(expr, arms, span),
            Expr::Await(expr, span) => self.visit_await(expr, span),
            Expr::Try(expr, span) => self.visit_try(expr, span),
            Expr::AwaitTry(expr, span) => self.visit_await_try(expr, span),
            Expr::Closure(params, ret_type, body, span) => self.visit_closure(params, ret_type, body, span),
            Expr::MethodCall(object, method, args, span) => self.visit_method_call(object, method, args, span),
            Expr::Cast(expr, target_type, span) => self.visit_cast(expr, target_type, span),
            Expr::StructPattern(variant, fields, span) => self.visit_struct_pattern(variant, fields, span),
        }
    }

    // Expression visit methods
    fn visit_int(&mut self, value: i64, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_float(&mut self, value: f64, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_bool(&mut self, value: bool, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_unit(&mut self, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_string(&mut self, value: &str, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_identifier(&mut self, name: &str, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_array(&mut self, elements: &[Expr], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_array_index(&mut self, array: &Expr, index: &Expr, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_range(&mut self, start: Option<&Expr>, end: Option<&Expr>, inclusive: bool, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_binary_op(&mut self, left: &Expr, op: &Operator, right: &Expr, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_unary_op(&mut self, op: &UnaryOp, expr: &Expr, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_assign(&mut self, left: &Expr, right: &Expr, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_compound_assign(&mut self, left: &Expr, op: &Operator, right: &Expr, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_function_call(&mut self, name: &str, args: &[Expr], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_struct_init(&mut self, name: &str, fields: &[(String, Expr)], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_member_access(&mut self, object: &Expr, field: &str, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_enum_variant_or_method_call(&mut self, target: &Expr, call: &str, args: &[Expr], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_block(&mut self, stmts: &[Stmt], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_if_expr(&mut self, condition: &Expr, then_block: &[Stmt], else_block: &[Stmt], span: &Span) -> std::result::Result<T, Self::Error>;

    // ===== Statement visiting methods =====
    
    /// Visit a statement and dispatch to the appropriate method
    fn visit_stmt(&mut self, stmt: &Stmt) -> std::result::Result<T, Self::Error> {
        match stmt {
            Stmt::Let(name, expr, span) => self.visit_let(name, expr, span),
            Stmt::Function(_is_pub, name, generic_params, params, return_type, body, span) => {
                self.visit_function(name, generic_params, params, return_type, body, span)
            }
            Stmt::Struct(name, generic_params, fields, span) => self.visit_struct(name, generic_params, fields, span),
            Stmt::Enum(name, generic_params, variants, span) => self.visit_enum(name, generic_params, variants, span),
            Stmt::Impl(struct_name, methods, span) => self.visit_impl(struct_name, methods, span),
            Stmt::Match(expr, arms, span) => self.visit_match(expr, arms, span),
            Stmt::ForLoop(variable, iterable, body, span) => {
                self.visit_for_loop(variable, iterable, body, span)
            }
            Stmt::While(condition, body, span) => self.visit_while(condition, body, span),
            Stmt::Loop(body, span) => self.visit_loop(body, span),
            Stmt::Break(span) => self.visit_break(span),
            Stmt::Continue(span) => self.visit_continue(span),
            Stmt::Return(expr, span) => self.visit_return(expr.as_ref(), span),
            Stmt::Expression(expr, has_semicolon) => self.visit_expression_stmt(expr, *has_semicolon),
            Stmt::Use(path, items, span) => self.visit_use(path, items, span),
            Stmt::ExternFunction(name, generic_params, params, return_type, span) => {
                self.visit_extern_function(name, generic_params, params, return_type, span)
            }
            Stmt::ExternMod(name, items, span) => self.visit_extern_mod(name, items, span),
            Stmt::ExternType(name, generic_params, span) => {
                self.visit_extern_type(name, generic_params, span)
            }
            Stmt::AsyncFunction(_is_pub, name, generic_params, params, return_type, body, span) => {
                self.visit_async_function(name, generic_params, params, return_type, body, span)
            }
        }
    }

    // Statement visit methods
    fn visit_let(&mut self, name: &str, expr: &Expr, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_function(&mut self, name: &str, generic_params: &[String], params: &[(String, String)], return_type: &str, body: &[Stmt], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_struct(&mut self, name: &str, generic_params: &[String], fields: &[(String, String)], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_enum(&mut self, name: &str, generic_params: &[String], variants: &[EnumVariant], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_impl(&mut self, struct_name: &str, methods: &[Stmt], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_match(&mut self, expr: &Expr, arms: &[(Expr, Vec<Stmt>)], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_for_loop(&mut self, variable: &str, iterable: &Expr, body: &[Stmt], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_while(&mut self, condition: &Expr, body: &[Stmt], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_loop(&mut self, body: &[Stmt], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_break(&mut self, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_continue(&mut self, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_return(&mut self, expr: Option<&Expr>, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_expression_stmt(&mut self, expr: &Expr, has_semicolon: bool) -> std::result::Result<T, Self::Error>;
    fn visit_use(&mut self, path: &UsePath, items: &UseItems, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_extern_function(&mut self, name: &str, generic_params: &[String], params: &[(String, String)], return_type: &str, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_extern_mod(&mut self, name: &str, items: &[ExternItem], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_extern_type(&mut self, name: &str, generic_params: &[String], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_async_function(&mut self, name: &str, generic_params: &[String], params: &[(String, String)], return_type: &str, body: &[Stmt], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_match_expr(&mut self, expr: &Expr, arms: &[(Expr, Vec<Stmt>)], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_await(&mut self, expr: &Expr, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_try(&mut self, expr: &Expr, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_await_try(&mut self, expr: &Expr, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_closure(&mut self, params: &[(String, Option<String>)], ret_type: &Option<String>, body: &Expr, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_method_call(&mut self, object: &Expr, method: &str, args: &[Expr], span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_cast(&mut self, expr: &Expr, target_type: &str, span: &Span) -> std::result::Result<T, Self::Error>;
    fn visit_struct_pattern(&mut self, variant: &str, fields: &[(String, Option<String>)], span: &Span) -> std::result::Result<T, Self::Error>;

    // ===== Helper methods =====
    
    /// Visit a list of statements
    fn visit_statements(&mut self, stmts: &[Stmt]) -> std::result::Result<Vec<T>, Self::Error> {
        let mut results = Vec::new();
        for stmt in stmts {
            results.push(self.visit_stmt(stmt)?);
        }
        Ok(results)
    }
}