//! AST visitor for formatting Husk code.

use husk_ast::*;
use husk_lexer::Trivia;

use crate::config::FormatConfig;
use crate::trivia_map::TriviaMap;

/// Formatter visitor that walks the AST and produces formatted output.
pub struct Formatter<'a> {
    config: &'a FormatConfig,
    trivia_map: &'a TriviaMap,
    indent: usize,
    output: String,
    /// Track if we're at the start of a line (for indent handling)
    at_line_start: bool,
}

impl<'a> Formatter<'a> {
    pub fn new(config: &'a FormatConfig, trivia_map: &'a TriviaMap) -> Self {
        Self {
            config,
            trivia_map,
            indent: 0,
            output: String::new(),
            at_line_start: true,
        }
    }

    pub fn format_file(mut self, file: &File) -> String {
        for (i, item) in file.items.iter().enumerate() {
            // Determine trivia span - if item has attributes, trivia is attached to first attribute
            let trivia_span = if let Some(first_attr) = item.attributes.first() {
                &first_attr.span
            } else {
                &item.span
            };
            let has_leading_trivia = !self.trivia_map.leading_at(trivia_span.range.start).is_empty();

            // Add blank line between items (except for the first)
            // Only add if there's no leading trivia - trivia will provide its own spacing
            if i > 0 && !has_leading_trivia && !self.output.ends_with("\n\n") {
                if !self.output.ends_with('\n') {
                    self.newline();
                }
                self.newline();
            }

            // Emit leading trivia (comments, blank lines before this item)
            self.emit_leading_trivia(trivia_span);

            self.format_item(item);

            // Emit trailing trivia (end-of-line comments)
            self.emit_trailing_trivia(&item.span);
        }

        // Ensure file ends with newline
        if !self.output.is_empty() && !self.output.ends_with('\n') {
            self.newline();
        }

        self.output
    }

    fn emit_leading_trivia(&mut self, span: &Span) {
        let trivia = self.trivia_map.leading_at(span.range.start);
        let mut consecutive_newlines = 0;

        for t in trivia {
            match t {
                Trivia::LineComment(text) => {
                    // Preserve one blank line before comment if there were multiple newlines
                    if consecutive_newlines > 1 && !self.output.is_empty() {
                        self.newline();
                    }
                    consecutive_newlines = 0;
                    self.write_indent();
                    self.write(text);
                    self.newline();
                }
                Trivia::Newline(_) => {
                    consecutive_newlines += 1;
                }
                Trivia::Whitespace(_) => {
                    // Normalize whitespace - don't preserve original
                }
            }
        }

        // Preserve one blank line if there were multiple newlines before this item
        if consecutive_newlines > 1 && !self.output.is_empty() && !self.output.ends_with("\n\n") {
            self.newline();
        }
    }

    fn emit_trailing_trivia(&mut self, span: &Span) {
        let trivia = self.trivia_map.trailing_at(span.range.end);
        for t in trivia {
            if let Trivia::LineComment(text) = t {
                self.write(" ");
                self.write(text);
            }
        }
    }

    /// Emit any comments/trivia that appear before a closing brace.
    /// Call this before writing `}` for any block that may have standalone comments.
    fn emit_block_end_trivia(&mut self, block_end: usize) {
        let trivia = self.trivia_map.leading_before_close(block_end);
        let mut consecutive_newlines = 0;

        for t in trivia {
            match t {
                Trivia::LineComment(text) => {
                    // Preserve one blank line before comment if there were multiple newlines
                    if consecutive_newlines > 1 && !self.output.is_empty() {
                        self.newline();
                    }
                    consecutive_newlines = 0;
                    self.write_indent();
                    self.write(text);
                    self.newline();
                }
                Trivia::Newline(_) => {
                    consecutive_newlines += 1;
                }
                Trivia::Whitespace(_) => {
                    // Normalize whitespace - don't preserve original
                }
            }
        }
    }

    fn format_item(&mut self, item: &Item) {
        // Format attributes
        for attr in &item.attributes {
            self.format_attribute(attr);
        }

        // Format visibility
        if item.visibility == Visibility::Public {
            self.write_indent();
            self.write("pub ");
            self.at_line_start = false;
        }

        match &item.kind {
            ItemKind::Fn {
                name,
                type_params,
                params,
                ret_type,
                body,
            } => {
                self.format_fn(name, type_params, params, ret_type, body, item.visibility == Visibility::Public, item.span.range.end);
            }
            ItemKind::Struct {
                name,
                type_params,
                fields,
            } => {
                self.format_struct(name, type_params, fields, item.visibility == Visibility::Public, item.span.range.end);
            }
            ItemKind::Enum {
                name,
                type_params,
                variants,
            } => {
                self.format_enum(name, type_params, variants, item.visibility == Visibility::Public, item.span.range.end);
            }
            ItemKind::TypeAlias { name, ty } => {
                self.format_type_alias(name, ty, item.visibility == Visibility::Public);
            }
            ItemKind::ExternBlock { abi, items } => {
                self.format_extern_block(abi, items, item.span.range.end);
            }
            ItemKind::Use { path } => {
                self.format_use(path, item.visibility == Visibility::Public);
            }
            ItemKind::Trait(trait_def) => {
                self.format_trait(trait_def, item.visibility == Visibility::Public, item.span.range.end);
            }
            ItemKind::Impl(impl_block) => {
                self.format_impl(impl_block, item.span.range.end);
            }
        }
    }

    fn format_attribute(&mut self, attr: &Attribute) {
        self.write_indent();
        self.write("#[");
        self.write(&attr.name.name);
        if let Some(value) = &attr.value {
            self.write(" = \"");
            self.write(value);
            self.write("\"");
        } else if let Some(pred) = &attr.cfg_predicate {
            self.write("(");
            self.format_cfg_predicate(pred);
            self.write(")");
        }
        self.write("]");
        self.newline();
    }

    fn format_cfg_predicate(&mut self, pred: &CfgPredicate) {
        match pred {
            CfgPredicate::Flag(flag) => self.write(flag),
            CfgPredicate::KeyValue { key, value } => {
                self.write(key);
                self.write(" = \"");
                self.write(value);
                self.write("\"");
            }
            CfgPredicate::All(preds) => {
                self.write("all(");
                for (i, p) in preds.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_cfg_predicate(p);
                }
                self.write(")");
            }
            CfgPredicate::Any(preds) => {
                self.write("any(");
                for (i, p) in preds.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_cfg_predicate(p);
                }
                self.write(")");
            }
            CfgPredicate::Not(inner) => {
                self.write("not(");
                self.format_cfg_predicate(inner);
                self.write(")");
            }
        }
    }

    fn format_fn(
        &mut self,
        name: &Ident,
        type_params: &[Ident],
        params: &[Param],
        ret_type: &Option<TypeExpr>,
        body: &[Stmt],
        has_visibility: bool,
        span_end: usize,
    ) {
        if !has_visibility {
            self.write_indent();
        }
        self.write("fn ");
        self.write(&name.name);

        // Type parameters
        if !type_params.is_empty() {
            self.write("<");
            for (i, tp) in type_params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(&tp.name);
            }
            self.write(">");
        }

        // Parameters
        self.write("(");
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.write(&param.name.name);
            self.write(": ");
            self.format_type(&param.ty);
        }
        self.write(")");

        // Return type
        if let Some(ret) = ret_type {
            self.write(" -> ");
            self.format_type(ret);
        }

        // Body
        self.write(" {");
        self.newline();
        self.indent += 1;
        self.format_stmts(body);
        self.emit_block_end_trivia(span_end);
        self.indent -= 1;
        self.write_indent();
        self.write("}");
        self.newline();
    }

    fn format_struct(
        &mut self,
        name: &Ident,
        type_params: &[Ident],
        fields: &[StructField],
        has_visibility: bool,
        span_end: usize,
    ) {
        if !has_visibility {
            self.write_indent();
        }
        self.write("struct ");
        self.write(&name.name);

        if !type_params.is_empty() {
            self.write("<");
            for (i, tp) in type_params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(&tp.name);
            }
            self.write(">");
        }

        self.write(" {");
        self.newline();
        self.indent += 1;
        for field in fields {
            self.write_indent();
            self.write(&field.name.name);
            self.write(": ");
            self.format_type(&field.ty);
            self.write(",");
            self.newline();
        }
        self.emit_block_end_trivia(span_end);
        self.indent -= 1;
        self.write_indent();
        self.write("}");
        self.newline();
    }

    fn format_enum(
        &mut self,
        name: &Ident,
        type_params: &[Ident],
        variants: &[EnumVariant],
        has_visibility: bool,
        span_end: usize,
    ) {
        if !has_visibility {
            self.write_indent();
        }
        self.write("enum ");
        self.write(&name.name);

        if !type_params.is_empty() {
            self.write("<");
            for (i, tp) in type_params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(&tp.name);
            }
            self.write(">");
        }

        self.write(" {");
        self.newline();
        self.indent += 1;
        for variant in variants {
            self.write_indent();
            self.write(&variant.name.name);
            match &variant.fields {
                EnumVariantFields::Unit => {}
                EnumVariantFields::Tuple(types) => {
                    self.write("(");
                    for (i, ty) in types.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.format_type(ty);
                    }
                    self.write(")");
                }
                EnumVariantFields::Struct(fields) => {
                    self.write(" {");
                    self.newline();
                    self.indent += 1;
                    for field in fields {
                        self.write_indent();
                        self.write(&field.name.name);
                        self.write(": ");
                        self.format_type(&field.ty);
                        self.write(",");
                        self.newline();
                    }
                    self.indent -= 1;
                    self.write_indent();
                    self.write("}");
                }
            }
            self.write(",");
            self.newline();
        }
        self.emit_block_end_trivia(span_end);
        self.indent -= 1;
        self.write_indent();
        self.write("}");
        self.newline();
    }

    fn format_type_alias(&mut self, name: &Ident, ty: &TypeExpr, has_visibility: bool) {
        if !has_visibility {
            self.write_indent();
        }
        self.write("type ");
        self.write(&name.name);
        self.write(" = ");
        self.format_type(ty);
        self.write(";");
        self.newline();
    }

    fn format_extern_block(&mut self, abi: &str, items: &[ExternItem], span_end: usize) {
        self.write_indent();
        self.write("extern \"");
        self.write(abi);
        self.write("\" {");
        self.newline();
        self.indent += 1;
        for item in items {
            self.emit_leading_trivia(&item.span);
            self.format_extern_item(item);
            self.emit_trailing_trivia(&item.span);
        }
        self.emit_block_end_trivia(span_end);
        self.indent -= 1;
        self.write_indent();
        self.write("}");
        self.newline();
    }

    fn format_extern_item(&mut self, item: &ExternItem) {
        match &item.kind {
            ExternItemKind::Fn {
                name,
                params,
                ret_type,
            } => {
                self.write_indent();
                self.write("fn ");
                self.write(&name.name);
                self.write("(");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&param.name.name);
                    self.write(": ");
                    self.format_type(&param.ty);
                }
                self.write(")");
                if let Some(ret) = ret_type {
                    self.write(" -> ");
                    self.format_type(ret);
                }
                self.write(";");
                self.newline();
            }
            ExternItemKind::Mod {
                package,
                binding,
                items,
                is_global,
            } => {
                self.write_indent();
                self.write("mod ");
                if *is_global {
                    self.write("global ");
                }
                if package != &binding.name {
                    self.write("\"");
                    self.write(package);
                    self.write("\" as ");
                }
                self.write(&binding.name);
                if items.is_empty() {
                    self.write(";");
                } else {
                    self.write(" {");
                    self.newline();
                    self.indent += 1;
                    for mod_item in items {
                        self.emit_leading_trivia(&mod_item.span);
                        self.format_mod_item(mod_item);
                        self.emit_trailing_trivia(&mod_item.span);
                    }
                    self.indent -= 1;
                    self.write_indent();
                    self.write("}");
                }
                self.newline();
            }
            ExternItemKind::Struct { name, type_params } => {
                self.write_indent();
                self.write("struct ");
                self.write(&name.name);
                if !type_params.is_empty() {
                    self.write("<");
                    for (i, tp) in type_params.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.write(&tp.name);
                    }
                    self.write(">");
                }
                self.write(";");
                self.newline();
            }
            ExternItemKind::Static { name, ty } => {
                self.write_indent();
                self.write("static ");
                self.write(&name.name);
                self.write(": ");
                self.format_type(ty);
                self.write(";");
                self.newline();
            }
        }
    }

    fn format_mod_item(&mut self, item: &ModItem) {
        match &item.kind {
            ModItemKind::Fn {
                name,
                params,
                ret_type,
            } => {
                self.write_indent();
                self.write("fn ");
                self.write(&name.name);
                self.write("(");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&param.name.name);
                    self.write(": ");
                    self.format_type(&param.ty);
                }
                self.write(")");
                if let Some(ret) = ret_type {
                    self.write(" -> ");
                    self.format_type(ret);
                }
                self.write(";");
                self.newline();
            }
        }
    }

    fn format_use(&mut self, path: &[Ident], has_visibility: bool) {
        if !has_visibility {
            self.write_indent();
        }
        self.write("use ");
        for (i, segment) in path.iter().enumerate() {
            if i > 0 {
                self.write("::");
            }
            self.write(&segment.name);
        }
        self.write(";");
        self.newline();
    }

    fn format_trait(&mut self, trait_def: &TraitDef, has_visibility: bool, span_end: usize) {
        if !has_visibility {
            self.write_indent();
        }
        self.write("trait ");
        self.write(&trait_def.name.name);

        if !trait_def.type_params.is_empty() {
            self.write("<");
            for (i, tp) in trait_def.type_params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(&tp.name.name);
                if !tp.bounds.is_empty() {
                    self.write(": ");
                    for (j, bound) in tp.bounds.iter().enumerate() {
                        if j > 0 {
                            self.write(" + ");
                        }
                        self.format_type(bound);
                    }
                }
            }
            self.write(">");
        }

        self.write(" {");
        self.newline();
        self.indent += 1;
        for item in &trait_def.items {
            self.emit_leading_trivia(&item.span);
            self.format_trait_item(item, item.span.range.end);
            self.emit_trailing_trivia(&item.span);
        }
        self.emit_block_end_trivia(span_end);
        self.indent -= 1;
        self.write_indent();
        self.write("}");
        self.newline();
    }

    fn format_trait_item(&mut self, item: &TraitItem, span_end: usize) {
        match &item.kind {
            TraitItemKind::Method(method) => {
                self.write_indent();
                self.write("fn ");
                self.write(&method.name.name);
                self.write("(");
                if let Some(receiver) = &method.receiver {
                    match receiver {
                        SelfReceiver::Value => self.write("self"),
                        SelfReceiver::Ref => self.write("&self"),
                        SelfReceiver::RefMut => self.write("&mut self"),
                    }
                    if !method.params.is_empty() {
                        self.write(", ");
                    }
                }
                for (i, param) in method.params.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&param.name.name);
                    self.write(": ");
                    self.format_type(&param.ty);
                }
                self.write(")");
                if let Some(ret) = &method.ret_type {
                    self.write(" -> ");
                    self.format_type(ret);
                }
                if method.default_body.is_some() {
                    self.write(" {");
                    self.newline();
                    self.indent += 1;
                    self.format_stmts(method.default_body.as_ref().unwrap());
                    self.emit_block_end_trivia(span_end);
                    self.indent -= 1;
                    self.write_indent();
                    self.write("}");
                } else {
                    self.write(";");
                }
                self.newline();
            }
        }
    }

    fn format_impl(&mut self, impl_block: &ImplBlock, span_end: usize) {
        self.write_indent();
        self.write("impl");

        if !impl_block.type_params.is_empty() {
            self.write("<");
            for (i, tp) in impl_block.type_params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(&tp.name.name);
                if !tp.bounds.is_empty() {
                    self.write(": ");
                    for (j, bound) in tp.bounds.iter().enumerate() {
                        if j > 0 {
                            self.write(" + ");
                        }
                        self.format_type(bound);
                    }
                }
            }
            self.write(">");
        }

        self.write(" ");
        if let Some(trait_ref) = &impl_block.trait_ref {
            self.format_type(trait_ref);
            self.write(" for ");
        }
        self.format_type(&impl_block.self_ty);

        self.write(" {");
        self.newline();
        self.indent += 1;
        for item in &impl_block.items {
            self.emit_leading_trivia(&item.span);
            self.format_impl_item(item, item.span.range.end);
            self.emit_trailing_trivia(&item.span);
        }
        self.emit_block_end_trivia(span_end);
        self.indent -= 1;
        self.write_indent();
        self.write("}");
        self.newline();
    }

    fn format_impl_item(&mut self, item: &ImplItem, span_end: usize) {
        match &item.kind {
            ImplItemKind::Method(method) => {
                self.write_indent();
                if method.is_extern {
                    self.write("extern \"js\" ");
                }
                self.write("fn ");
                self.write(&method.name.name);
                self.write("(");
                if let Some(receiver) = &method.receiver {
                    match receiver {
                        SelfReceiver::Value => self.write("self"),
                        SelfReceiver::Ref => self.write("&self"),
                        SelfReceiver::RefMut => self.write("&mut self"),
                    }
                    if !method.params.is_empty() {
                        self.write(", ");
                    }
                }
                for (i, param) in method.params.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&param.name.name);
                    self.write(": ");
                    self.format_type(&param.ty);
                }
                self.write(")");
                if let Some(ret) = &method.ret_type {
                    self.write(" -> ");
                    self.format_type(ret);
                }
                if method.is_extern {
                    self.write(";");
                } else {
                    self.write(" {");
                    self.newline();
                    self.indent += 1;
                    self.format_stmts(&method.body);
                    self.emit_block_end_trivia(span_end);
                    self.indent -= 1;
                    self.write_indent();
                    self.write("}");
                }
                self.newline();
            }
            ImplItemKind::Property(prop) => {
                for attr in &prop.attributes {
                    self.format_attribute(attr);
                }
                self.write_indent();
                self.write("extern \"js\" ");
                self.write(&prop.name.name);
                self.write(": ");
                self.format_type(&prop.ty);
                self.write(";");
                self.newline();
            }
        }
    }

    fn format_type(&mut self, ty: &TypeExpr) {
        match &ty.kind {
            TypeExprKind::Named(ident) => {
                self.write(&ident.name);
            }
            TypeExprKind::Generic { name, args } => {
                self.write(&name.name);
                self.write("<");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_type(arg);
                }
                self.write(">");
            }
            TypeExprKind::Function { params, ret } => {
                self.write("fn(");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_type(param);
                }
                self.write(") -> ");
                self.format_type(ret);
            }
            TypeExprKind::Array(elem) => {
                self.write("[");
                self.format_type(elem);
                self.write("]");
            }
        }
    }

    fn format_stmts(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.emit_leading_trivia(&stmt.span);
            self.format_stmt(stmt);
            self.emit_trailing_trivia(&stmt.span);
            self.newline();
        }
    }

    fn format_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Let {
                mutable,
                name,
                ty,
                value,
            } => {
                self.write_indent();
                self.write("let ");
                if *mutable {
                    self.write("mut ");
                }
                self.write(&name.name);
                if let Some(ty) = ty {
                    self.write(": ");
                    self.format_type(ty);
                }
                if let Some(val) = value {
                    self.write(" = ");
                    self.format_expr(val);
                }
                self.write(";");
            }
            StmtKind::Assign { target, op, value } => {
                self.write_indent();
                self.format_expr(target);
                self.write(" ");
                match op {
                    AssignOp::Assign => self.write("="),
                    AssignOp::AddAssign => self.write("+="),
                    AssignOp::SubAssign => self.write("-="),
                    AssignOp::ModAssign => self.write("%="),
                }
                self.write(" ");
                self.format_expr(value);
                self.write(";");
            }
            StmtKind::Expr(expr) => {
                self.write_indent();
                self.format_expr(expr);
            }
            StmtKind::Semi(expr) => {
                self.write_indent();
                self.format_expr(expr);
                self.write(";");
            }
            StmtKind::Return { value } => {
                self.write_indent();
                self.write("return");
                if let Some(val) = value {
                    self.write(" ");
                    self.format_expr(val);
                }
                self.write(";");
            }
            StmtKind::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.write_indent();
                self.write("if ");
                self.format_expr(cond);
                self.write(" {");
                self.newline();
                self.indent += 1;
                self.format_stmts(&then_branch.stmts);
                self.emit_block_end_trivia(then_branch.span.range.end);
                self.indent -= 1;
                self.write_indent();
                self.write("}");
                if let Some(else_stmt) = else_branch {
                    self.write(" else ");
                    // Check if it's an else-if
                    if let StmtKind::If { .. } = &else_stmt.kind {
                        // Don't add newline, format_stmt_inline will handle the if
                        self.at_line_start = false;
                        self.format_stmt_inline(else_stmt);
                    } else if let StmtKind::Block(block) = &else_stmt.kind {
                        self.write("{");
                        self.newline();
                        self.indent += 1;
                        self.format_stmts(&block.stmts);
                        self.emit_block_end_trivia(block.span.range.end);
                        self.indent -= 1;
                        self.write_indent();
                        self.write("}");
                    }
                }
            }
            StmtKind::While { cond, body } => {
                self.write_indent();
                self.write("while ");
                self.format_expr(cond);
                self.write(" {");
                self.newline();
                self.indent += 1;
                self.format_stmts(&body.stmts);
                self.emit_block_end_trivia(body.span.range.end);
                self.indent -= 1;
                self.write_indent();
                self.write("}");
            }
            StmtKind::ForIn {
                binding,
                iterable,
                body,
            } => {
                self.write_indent();
                self.write("for ");
                self.write(&binding.name);
                self.write(" in ");
                self.format_expr(iterable);
                self.write(" {");
                self.newline();
                self.indent += 1;
                self.format_stmts(&body.stmts);
                self.emit_block_end_trivia(body.span.range.end);
                self.indent -= 1;
                self.write_indent();
                self.write("}");
            }
            StmtKind::Break => {
                self.write_indent();
                self.write("break;");
            }
            StmtKind::Continue => {
                self.write_indent();
                self.write("continue;");
            }
            StmtKind::Block(block) => {
                self.write_indent();
                self.write("{");
                self.newline();
                self.indent += 1;
                self.format_stmts(&block.stmts);
                self.emit_block_end_trivia(block.span.range.end);
                self.indent -= 1;
                self.write_indent();
                self.write("}");
            }
        }
    }

    /// Format a statement inline (for else-if chains)
    fn format_stmt_inline(&mut self, stmt: &Stmt) {
        if let StmtKind::If {
            cond,
            then_branch,
            else_branch,
        } = &stmt.kind
        {
            self.write("if ");
            self.format_expr(cond);
            self.write(" {");
            self.newline();
            self.indent += 1;
            self.format_stmts(&then_branch.stmts);
            self.emit_block_end_trivia(then_branch.span.range.end);
            self.indent -= 1;
            self.write_indent();
            self.write("}");
            if let Some(else_stmt) = else_branch {
                self.write(" else ");
                if let StmtKind::If { .. } = &else_stmt.kind {
                    self.format_stmt_inline(else_stmt);
                } else if let StmtKind::Block(block) = &else_stmt.kind {
                    self.write("{");
                    self.newline();
                    self.indent += 1;
                    self.format_stmts(&block.stmts);
                    self.emit_block_end_trivia(block.span.range.end);
                    self.indent -= 1;
                    self.write_indent();
                    self.write("}");
                }
            }
        }
    }

    fn format_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Literal(lit) => {
                match &lit.kind {
                    LiteralKind::Int(n) => self.write(&n.to_string()),
                    LiteralKind::Float(n) => self.write(&n.to_string()),
                    LiteralKind::Bool(b) => self.write(if *b { "true" } else { "false" }),
                    LiteralKind::String(s) => {
                        self.write("\"");
                        self.write(s);
                        self.write("\"");
                    }
                }
            }
            ExprKind::Ident(ident) => {
                self.write(&ident.name);
            }
            ExprKind::Path { segments } => {
                for (i, segment) in segments.iter().enumerate() {
                    if i > 0 {
                        self.write("::");
                    }
                    self.write(&segment.name);
                }
            }
            ExprKind::Call { callee, args } => {
                self.format_expr(callee);
                self.write("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_expr(arg);
                }
                self.write(")");
            }
            ExprKind::Field { base, member } => {
                self.format_expr(base);
                self.write(".");
                self.write(&member.name);
            }
            ExprKind::MethodCall {
                receiver,
                method,
                args,
            } => {
                self.format_expr(receiver);
                self.write(".");
                self.write(&method.name);
                self.write("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_expr(arg);
                }
                self.write(")");
            }
            ExprKind::Unary { op, expr } => {
                match op {
                    UnaryOp::Not => self.write("!"),
                    UnaryOp::Neg => self.write("-"),
                }
                self.format_expr(expr);
            }
            ExprKind::Binary { op, left, right } => {
                self.format_expr(left);
                self.write(" ");
                match op {
                    BinaryOp::Add => self.write("+"),
                    BinaryOp::Sub => self.write("-"),
                    BinaryOp::Mul => self.write("*"),
                    BinaryOp::Div => self.write("/"),
                    BinaryOp::Mod => self.write("%"),
                    BinaryOp::Eq => self.write("=="),
                    BinaryOp::NotEq => self.write("!="),
                    BinaryOp::Lt => self.write("<"),
                    BinaryOp::Gt => self.write(">"),
                    BinaryOp::Le => self.write("<="),
                    BinaryOp::Ge => self.write(">="),
                    BinaryOp::And => self.write("&&"),
                    BinaryOp::Or => self.write("||"),
                }
                self.write(" ");
                self.format_expr(right);
            }
            ExprKind::Match { scrutinee, arms } => {
                self.write("match ");
                self.format_expr(scrutinee);
                self.write(" {");
                self.newline();
                self.indent += 1;
                for arm in arms {
                    self.write_indent();
                    self.format_pattern(&arm.pattern);
                    self.write(" => ");
                    self.format_expr(&arm.expr);
                    self.write(",");
                    self.newline();
                }
                self.indent -= 1;
                self.write_indent();
                self.write("}");
            }
            ExprKind::Block(block) => {
                self.write("{");
                self.newline();
                self.indent += 1;
                self.format_stmts(&block.stmts);
                self.emit_block_end_trivia(block.span.range.end);
                self.indent -= 1;
                self.write_indent();
                self.write("}");
            }
            ExprKind::Struct { name, fields } => {
                for (i, segment) in name.iter().enumerate() {
                    if i > 0 {
                        self.write("::");
                    }
                    self.write(&segment.name);
                }
                self.write(" { ");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&field.name.name);
                    self.write(": ");
                    self.format_expr(&field.value);
                }
                self.write(" }");
            }
            ExprKind::FormatPrint {
                format,
                args,
                newline,
            } => {
                if *newline {
                    self.write("println(");
                } else {
                    self.write("print(");
                }
                self.format_format_string(format);
                for arg in args {
                    self.write(", ");
                    self.format_expr(arg);
                }
                self.write(")");
            }
            ExprKind::Format { format, args } => {
                self.write("format(");
                self.format_format_string(format);
                for arg in args {
                    self.write(", ");
                    self.format_expr(arg);
                }
                self.write(")");
            }
            ExprKind::Closure {
                params,
                ret_type,
                body,
            } => {
                self.write("|");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&param.name.name);
                    if let Some(ty) = &param.ty {
                        self.write(": ");
                        self.format_type(ty);
                    }
                }
                self.write("|");
                if let Some(ret) = ret_type {
                    self.write(" -> ");
                    self.format_type(ret);
                }
                self.write(" ");
                self.format_expr(body);
            }
            ExprKind::Array { elements } => {
                self.write("[");
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_expr(elem);
                }
                self.write("]");
            }
            ExprKind::Index { base, index } => {
                self.format_expr(base);
                self.write("[");
                self.format_expr(index);
                self.write("]");
            }
            ExprKind::Range {
                start,
                end,
                inclusive,
            } => {
                if let Some(s) = start {
                    self.format_expr(s);
                }
                if *inclusive {
                    self.write("..=");
                } else {
                    self.write("..");
                }
                if let Some(e) = end {
                    self.format_expr(e);
                }
            }
            ExprKind::Assign { target, op, value } => {
                self.format_expr(target);
                self.write(" ");
                match op {
                    AssignOp::Assign => self.write("="),
                    AssignOp::AddAssign => self.write("+="),
                    AssignOp::SubAssign => self.write("-="),
                    AssignOp::ModAssign => self.write("%="),
                }
                self.write(" ");
                self.format_expr(value);
            }
            ExprKind::JsLiteral { code } => {
                self.write("js { ");
                self.write(code);
                self.write(" }");
            }
        }
    }

    fn format_format_string(&mut self, format: &FormatString) {
        self.write("\"");
        for segment in &format.segments {
            match segment {
                FormatSegment::Literal(text) => {
                    // Escape any special characters
                    for c in text.chars() {
                        match c {
                            '"' => self.write("\\\""),
                            '\\' => self.write("\\\\"),
                            '\n' => self.write("\\n"),
                            '\r' => self.write("\\r"),
                            '\t' => self.write("\\t"),
                            _ => self.output.push(c),
                        }
                    }
                }
                FormatSegment::Placeholder(placeholder) => {
                    self.write("{");
                    if let Some(pos) = placeholder.position {
                        self.write(&pos.to_string());
                    }
                    if let Some(name) = &placeholder.name {
                        self.write(name);
                    }
                    // Format spec
                    let spec = &placeholder.spec;
                    if spec.ty.is_some()
                        || spec.width.is_some()
                        || spec.precision.is_some()
                        || spec.alternate
                        || spec.sign
                    {
                        self.write(":");
                        if let Some(fill) = spec.fill {
                            self.output.push(fill);
                        }
                        if let Some(align) = spec.align {
                            self.output.push(align);
                        }
                        if spec.sign {
                            self.write("+");
                        }
                        if spec.alternate {
                            self.write("#");
                        }
                        if spec.zero_pad {
                            self.write("0");
                        }
                        if let Some(width) = spec.width {
                            self.write(&width.to_string());
                        }
                        if let Some(precision) = spec.precision {
                            self.write(".");
                            self.write(&precision.to_string());
                        }
                        if let Some(ty) = spec.ty {
                            self.output.push(ty);
                        }
                    }
                    self.write("}");
                }
            }
        }
        self.write("\"");
    }

    fn format_pattern(&mut self, pattern: &Pattern) {
        match &pattern.kind {
            PatternKind::Wildcard => self.write("_"),
            PatternKind::Binding(ident) => self.write(&ident.name),
            PatternKind::EnumUnit { path } => {
                for (i, segment) in path.iter().enumerate() {
                    if i > 0 {
                        self.write("::");
                    }
                    self.write(&segment.name);
                }
            }
            PatternKind::EnumTuple { path, fields } => {
                for (i, segment) in path.iter().enumerate() {
                    if i > 0 {
                        self.write("::");
                    }
                    self.write(&segment.name);
                }
                self.write("(");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_pattern(field);
                }
                self.write(")");
            }
            PatternKind::EnumStruct { path, fields } => {
                for (i, segment) in path.iter().enumerate() {
                    if i > 0 {
                        self.write("::");
                    }
                    self.write(&segment.name);
                }
                self.write(" { ");
                for (i, (name, pattern)) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&name.name);
                    self.write(": ");
                    self.format_pattern(pattern);
                }
                self.write(" }");
            }
        }
    }

    fn write(&mut self, s: &str) {
        self.output.push_str(s);
        self.at_line_start = false;
    }

    fn write_indent(&mut self) {
        if self.at_line_start {
            let indent_str = self.config.indent_str();
            for _ in 0..self.indent {
                self.output.push_str(&indent_str);
            }
        }
        self.at_line_start = false;
    }

    fn newline(&mut self) {
        self.output.push('\n');
        self.at_line_start = true;
    }
}
