use anyhow::Result;
use std::collections::HashMap;
use swc_ecma_ast::*;

use crate::dts::types::{convert_ts_type, HuskType};

#[derive(Debug)]
pub struct HuskExternModule {
    pub name: String,
    pub imports: Vec<ImportInfo>,
    pub functions: Vec<HuskExternFunction>,
    pub types: Vec<HuskExternType>,
    pub implementations: Vec<HuskExternImpl>,
    pub namespaces: Vec<HuskNamespace>,
}

#[derive(Debug)]
pub struct ImportInfo {
    pub source: String,
    pub alias: String,
    pub import_type: ImportType,
}

#[derive(Debug)]
pub enum ImportType {
    Namespace,     // import * as foo from "bar"
    Default,       // import foo from "bar"
    Named(String), // import { foo } from "bar"
}

#[derive(Debug)]
pub struct HuskNamespace {
    pub name: String,
    pub functions: Vec<HuskExternFunction>,
    pub variables: Vec<HuskNamespaceVar>,
    pub types: Vec<HuskExternType>,
}

#[derive(Debug)]
pub struct HuskNamespaceVar {
    pub name: String,
    pub var_type: HuskType,
}

#[derive(Debug)]
pub struct HuskExternFunction {
    pub name: String,
    pub params: Vec<(String, HuskType)>,
    pub return_type: Option<HuskType>,
}

#[derive(Debug)]
pub struct HuskExternType {
    pub name: String,
    pub extends: Vec<String>, // List of extended interfaces
}

#[derive(Debug)]
pub struct HuskExternImpl {
    pub type_name: String,
    pub methods: Vec<HuskExternMethod>,
}

#[derive(Debug)]
pub struct HuskExternMethod {
    pub name: String,
    pub params: Vec<(String, HuskType)>,
    pub return_type: Option<HuskType>,
    pub has_self: bool,
}

pub fn convert_dts_to_husk(module: &Module, module_name: String) -> Result<HuskExternModule> {
    eprintln!("[DEBUG] Starting conversion for module: {}", module_name);
    eprintln!("[DEBUG] Module has {} top-level items", module.body.len());
    
    let mut husk_module = HuskExternModule {
        name: module_name,
        imports: Vec::new(),
        functions: Vec::new(),
        types: Vec::new(),
        implementations: Vec::new(),
        namespaces: Vec::new(),
    };

    let mut interfaces: HashMap<String, Vec<HuskExternMethod>> = HashMap::new();

    for (i, item) in module.body.iter().enumerate() {
        eprintln!("[DEBUG] Processing item {} of {}", i + 1, module.body.len());
        match item {
            ModuleItem::ModuleDecl(decl) => process_module_decl(decl, &mut husk_module, &mut interfaces)?,
            ModuleItem::Stmt(stmt) => process_stmt(stmt, &mut husk_module, &mut interfaces)?,
        }
    }

    // Convert collected interfaces to implementations
    eprintln!("[DEBUG] Converting {} interfaces to implementations", interfaces.len());
    for (type_name, methods) in interfaces {
        eprintln!("[DEBUG] Creating implementation for {} with {} methods", type_name, methods.len());
        husk_module.implementations.push(HuskExternImpl {
            type_name,
            methods,
        });
    }

    eprintln!("[DEBUG] Conversion complete:");
    eprintln!("[DEBUG]   - Imports: {}", husk_module.imports.len());
    eprintln!("[DEBUG]   - Functions: {}", husk_module.functions.len());
    eprintln!("[DEBUG]   - Types: {}", husk_module.types.len());
    eprintln!("[DEBUG]   - Implementations: {}", husk_module.implementations.len());
    eprintln!("[DEBUG]   - Namespaces: {}", husk_module.namespaces.len());
    
    Ok(husk_module)
}

fn process_module_decl(
    decl: &ModuleDecl,
    husk_module: &mut HuskExternModule,
    interfaces: &mut HashMap<String, Vec<HuskExternMethod>>,
) -> Result<()> {
    eprintln!("[DEBUG] Processing module declaration: {:?}", std::mem::discriminant(decl));
    
    match decl {
        ModuleDecl::ExportDecl(export) => {
            eprintln!("[DEBUG] Found ExportDecl");
            process_decl(&export.decl, husk_module, interfaces)?;
        }
        ModuleDecl::TsExportAssignment(export_assignment) => {
            eprintln!("[DEBUG] Found TsExportAssignment: {:?}", export_assignment);
            // Handle export = syntax if needed
        }
        ModuleDecl::Import(import) => {
            eprintln!("[DEBUG] Found Import: src={:?}, specifiers count={}", 
                import.src.value, import.specifiers.len());
            eprintln!("[DEBUG]   Import details:");
            
            let source = import.src.value.to_string();
            
            for spec in &import.specifiers {
                match spec {
                    ImportSpecifier::Named(named) => {
                        let imported_name = named.imported.as_ref()
                            .map(|i| match i {
                                ModuleExportName::Ident(id) => id.sym.to_string(),
                                ModuleExportName::Str(s) => s.value.to_string(),
                            })
                            .unwrap_or_else(|| named.local.sym.to_string());
                        let local_name = named.local.sym.to_string();
                        
                        eprintln!("[DEBUG]     Named import: {} as {}", imported_name, local_name);
                        
                        husk_module.imports.push(ImportInfo {
                            source: source.clone(),
                            alias: local_name,
                            import_type: ImportType::Named(imported_name),
                        });
                    }
                    ImportSpecifier::Default(default) => {
                        let local_name = default.local.sym.to_string();
                        eprintln!("[DEBUG]     Default import: {}", local_name);
                        
                        husk_module.imports.push(ImportInfo {
                            source: source.clone(),
                            alias: local_name,
                            import_type: ImportType::Default,
                        });
                    }
                    ImportSpecifier::Namespace(ns) => {
                        let local_name = ns.local.sym.to_string();
                        eprintln!("[DEBUG]     Namespace import: * as {}", local_name);
                        
                        husk_module.imports.push(ImportInfo {
                            source: source.clone(),
                            alias: local_name,
                            import_type: ImportType::Namespace,
                        });
                    }
                }
            }
        }
        ModuleDecl::ExportNamed(export) => {
            eprintln!("[DEBUG] Found ExportNamed: src={:?}, specifiers count={}", 
                export.src.as_ref().map(|s| &s.value), export.specifiers.len());
        }
        ModuleDecl::ExportDefaultDecl(_export) => {
            eprintln!("[DEBUG] Found ExportDefaultDecl");
        }
        ModuleDecl::ExportDefaultExpr(_export) => {
            eprintln!("[DEBUG] Found ExportDefaultExpr");
        }
        ModuleDecl::ExportAll(export) => {
            eprintln!("[DEBUG] Found ExportAll: src={:?}", export.src.value);
        }
        ModuleDecl::TsImportEquals(import) => {
            eprintln!("[DEBUG] Found TsImportEquals: id={:?}", import.id.sym);
        }
        ModuleDecl::TsNamespaceExport(ns_export) => {
            eprintln!("[DEBUG] Found TsNamespaceExport: id={:?}", ns_export.id.sym);
        }
    }
    Ok(())
}

fn process_stmt(
    stmt: &Stmt,
    husk_module: &mut HuskExternModule,
    interfaces: &mut HashMap<String, Vec<HuskExternMethod>>,
) -> Result<()> {
    eprintln!("[DEBUG] Processing statement: {:?}", std::mem::discriminant(stmt));
    
    match stmt {
        Stmt::Decl(decl) => {
            eprintln!("[DEBUG] Statement is a declaration");
            process_decl(decl, husk_module, interfaces)?;
        }
        Stmt::Block(block) => {
            eprintln!("[DEBUG] Found Block statement with {} statements", block.stmts.len());
        }
        Stmt::Empty(_) => {
            eprintln!("[DEBUG] Found Empty statement");
        }
        Stmt::Debugger(_) => {
            eprintln!("[DEBUG] Found Debugger statement");
        }
        Stmt::With(_) => {
            eprintln!("[DEBUG] Found With statement");
        }
        Stmt::Return(_) => {
            eprintln!("[DEBUG] Found Return statement");
        }
        Stmt::Labeled(_) => {
            eprintln!("[DEBUG] Found Labeled statement");
        }
        Stmt::Break(_) => {
            eprintln!("[DEBUG] Found Break statement");
        }
        Stmt::Continue(_) => {
            eprintln!("[DEBUG] Found Continue statement");
        }
        Stmt::If(_) => {
            eprintln!("[DEBUG] Found If statement");
        }
        Stmt::Switch(_) => {
            eprintln!("[DEBUG] Found Switch statement");
        }
        Stmt::Throw(_) => {
            eprintln!("[DEBUG] Found Throw statement");
        }
        Stmt::Try(_) => {
            eprintln!("[DEBUG] Found Try statement");
        }
        Stmt::While(_) => {
            eprintln!("[DEBUG] Found While statement");
        }
        Stmt::DoWhile(_) => {
            eprintln!("[DEBUG] Found DoWhile statement");
        }
        Stmt::For(_) => {
            eprintln!("[DEBUG] Found For statement");
        }
        Stmt::ForIn(_) => {
            eprintln!("[DEBUG] Found ForIn statement");
        }
        Stmt::ForOf(_) => {
            eprintln!("[DEBUG] Found ForOf statement");
        }
        Stmt::Expr(_) => {
            eprintln!("[DEBUG] Found Expr statement");
        }
    }
    Ok(())
}

fn process_decl(
    decl: &Decl,
    husk_module: &mut HuskExternModule,
    interfaces: &mut HashMap<String, Vec<HuskExternMethod>>,
) -> Result<()> {
    eprintln!("[DEBUG] Processing declaration: {:?}", std::mem::discriminant(decl));
    
    match decl {
        Decl::TsInterface(interface) => {
            eprintln!("[DEBUG] Found TsInterface: name={}", interface.id.sym);
            process_interface(interface, husk_module, interfaces)?;
        }
        Decl::TsTypeAlias(type_alias) => {
            eprintln!("[DEBUG] Found TsTypeAlias: name={}", type_alias.id.sym);
            // Add as a type
            husk_module.types.push(HuskExternType {
                name: type_alias.id.sym.to_string(),
                extends: Vec::new(),
            });
        }
        Decl::Fn(fn_decl) => {
            eprintln!("[DEBUG] Found Function: name={}", fn_decl.ident.sym);
            if let Some(func) = process_function_decl(fn_decl) {
                husk_module.functions.push(func);
            }
        }
        Decl::Class(class) => {
            eprintln!("[DEBUG] Found Class: name={}", class.ident.sym);
        }
        Decl::Var(var) => {
            eprintln!("[DEBUG] Found Var declaration: kind={:?}", var.kind);
        }
        Decl::TsEnum(ts_enum) => {
            eprintln!("[DEBUG] Found TsEnum: name={}", ts_enum.id.sym);
        }
        Decl::TsModule(module) => {
            let namespace_name = match &module.id {
                TsModuleName::Ident(ident) => ident.sym.to_string(),
                TsModuleName::Str(s) => s.value.to_string(),
            };
            eprintln!("[DEBUG] Found TsModule (namespace): name={:?}, global={}", 
                namespace_name,
                module.global
            );
            
            let mut namespace = HuskNamespace {
                name: namespace_name,
                functions: Vec::new(),
                variables: Vec::new(),
                types: Vec::new(),
            };
            
            // Process namespace body if it exists
            if let Some(body) = &module.body {
                eprintln!("[DEBUG] Processing namespace body");
                match body {
                    TsNamespaceBody::TsModuleBlock(block) => {
                        eprintln!("[DEBUG] Namespace has {} items in body", block.body.len());
                        process_namespace_body(&block.body, &mut namespace)?;
                    }
                    TsNamespaceBody::TsNamespaceDecl(ns) => {
                        eprintln!("[DEBUG] Nested namespace declaration: {:?}", ns.id.sym);
                    }
                }
            }
            
            if !namespace.functions.is_empty() || !namespace.variables.is_empty() || !namespace.types.is_empty() {
                husk_module.namespaces.push(namespace);
            }
        }
        Decl::Using(_using) => {
            eprintln!("[DEBUG] Found Using declaration (skipped)");
        }
    }
    Ok(())
}

fn process_namespace_body(
    body: &[ModuleItem],
    namespace: &mut HuskNamespace,
) -> Result<()> {
    for item in body {
        match item {
            ModuleItem::ModuleDecl(decl) => {
                match decl {
                    ModuleDecl::ExportDecl(export) => {
                        process_namespace_decl(&export.decl, namespace)?;
                    }
                    _ => {
                        eprintln!("[DEBUG] Skipping namespace module decl: {:?}", std::mem::discriminant(decl));
                    }
                }
            }
            ModuleItem::Stmt(stmt) => {
                if let Stmt::Decl(decl) = stmt {
                    process_namespace_decl(decl, namespace)?;
                }
            }
        }
    }
    Ok(())
}

fn process_namespace_decl(
    decl: &Decl,
    namespace: &mut HuskNamespace,
) -> Result<()> {
    match decl {
        Decl::Fn(fn_decl) => {
            eprintln!("[DEBUG] Found function in namespace: {}", fn_decl.ident.sym);
            if let Some(func) = process_function_decl(fn_decl) {
                namespace.functions.push(func);
            }
        }
        Decl::Var(var) => {
            eprintln!("[DEBUG] Processing var declaration in namespace");
            for decl in &var.decls {
                if let Pat::Ident(ident) = &decl.name {
                    let var_name = ident.id.sym.to_string();
                    let var_type = if let Some(type_ann) = &ident.type_ann {
                        convert_ts_type(&type_ann.type_ann)
                    } else {
                        HuskType::Any
                    };
                    eprintln!("[DEBUG] Found var in namespace: {} : {:?}", var_name, var_type);
                    namespace.variables.push(HuskNamespaceVar {
                        name: var_name,
                        var_type,
                    });
                }
            }
        }
        Decl::TsInterface(interface) => {
            eprintln!("[DEBUG] Found interface in namespace: {}", interface.id.sym);
            namespace.types.push(HuskExternType {
                name: interface.id.sym.to_string(),
                extends: Vec::new(),
            });
        }
        Decl::TsTypeAlias(type_alias) => {
            eprintln!("[DEBUG] Found type alias in namespace: {}", type_alias.id.sym);
            namespace.types.push(HuskExternType {
                name: type_alias.id.sym.to_string(),
                extends: Vec::new(),
            });
        }
        _ => {
            eprintln!("[DEBUG] Skipping namespace decl: {:?}", std::mem::discriminant(decl));
        }
    }
    Ok(())
}

fn process_interface(
    interface: &TsInterfaceDecl,
    husk_module: &mut HuskExternModule,
    interfaces: &mut HashMap<String, Vec<HuskExternMethod>>,
) -> Result<()> {
    let interface_name = interface.id.sym.to_string();
    eprintln!("[DEBUG] Processing interface: {} with {} members", 
        interface_name, interface.body.body.len());
    
    // Collect extended interfaces
    let mut extends = Vec::new();
    if !interface.extends.is_empty() {
        eprintln!("[DEBUG]   Interface extends {} other interfaces:", interface.extends.len());
        for (i, extend) in interface.extends.iter().enumerate() {
            // Extract the extended interface name
            let extended_name = extract_expr_type_name(&extend.expr);
            eprintln!("[DEBUG]     Extends[{}]: {}", i, extended_name);
            extends.push(extended_name);
        }
    }
    
    // Add type declaration
    husk_module.types.push(HuskExternType {
        name: interface_name.clone(),
        extends,
    });

    // Process interface members
    let mut methods = Vec::new();
    
    for (i, member) in interface.body.body.iter().enumerate() {
        eprintln!("[DEBUG]   Member {}: {:?}", i, std::mem::discriminant(member));
        match member {
            TsTypeElement::TsMethodSignature(method) => {
                eprintln!("[DEBUG]     Method signature found");
                if let Some(husk_method) = process_method_signature(method) {
                    eprintln!("[DEBUG]     Added method: {}", husk_method.name);
                    methods.push(husk_method);
                }
            }
            TsTypeElement::TsPropertySignature(prop) => {
                eprintln!("[DEBUG]     Property signature found");
                // Convert properties to getter methods
                if let Some(getter) = process_property_as_getter(prop) {
                    eprintln!("[DEBUG]     Added property as getter: {}", getter.name);
                    methods.push(getter);
                }
            }
            TsTypeElement::TsIndexSignature(_) => {
                eprintln!("[DEBUG]     Index signature (skipped)");
            }
            TsTypeElement::TsCallSignatureDecl(_) => {
                eprintln!("[DEBUG]     Call signature (skipped)");
            }
            TsTypeElement::TsConstructSignatureDecl(_) => {
                eprintln!("[DEBUG]     Construct signature (skipped)");
            }
            TsTypeElement::TsGetterSignature(_) => {
                eprintln!("[DEBUG]     Getter signature (skipped)");
            }
            TsTypeElement::TsSetterSignature(_) => {
                eprintln!("[DEBUG]     Setter signature (skipped)");
            }
        }
    }

    eprintln!("[DEBUG] Interface {} collected {} methods", interface_name, methods.len());
    if !methods.is_empty() {
        interfaces.insert(interface_name, methods);
    }

    Ok(())
}

fn process_method_signature(method: &TsMethodSignature) -> Option<HuskExternMethod> {
    let name = match method.key.as_ref() {
        Expr::Ident(ident) => ident.sym.to_string(),
        _ => return None,
    };

    let mut params = Vec::new();
    let has_self = true; // Methods typically have self in Husk

    for param in &method.params {
        if let TsFnParam::Ident(ident) = param {
            let param_name = ident.id.sym.to_string();
            let param_type = if let Some(type_ann) = &ident.type_ann {
                convert_ts_type(&type_ann.type_ann)
            } else {
                HuskType::Any
            };
            params.push((param_name, param_type));
        }
    }

    let return_type = method.type_ann.as_ref().map(|ann| convert_ts_type(&ann.type_ann));

    Some(HuskExternMethod {
        name,
        params,
        return_type,
        has_self,
    })
}

fn process_property_as_getter(prop: &TsPropertySignature) -> Option<HuskExternMethod> {
    let name = match prop.key.as_ref() {
        Expr::Ident(ident) => ident.sym.to_string(),
        _ => return None,
    };

    let return_type = prop.type_ann.as_ref().map(|ann| convert_ts_type(&ann.type_ann));

    Some(HuskExternMethod {
        name,
        params: Vec::new(),
        return_type,
        has_self: true,
    })
}

fn process_function_decl(fn_decl: &FnDecl) -> Option<HuskExternFunction> {
    let name = fn_decl.ident.sym.to_string();
    let mut params = Vec::new();

    for param in &fn_decl.function.params {
        if let Pat::Ident(ident) = &param.pat {
            let param_name = ident.id.sym.to_string();
            let param_type = if let Some(type_ann) = &ident.type_ann {
                convert_ts_type(&type_ann.type_ann)
            } else {
                HuskType::Any
            };
            params.push((param_name, param_type));
        }
    }

    let return_type = fn_decl.function.return_type.as_ref()
        .map(|ann| convert_ts_type(&ann.type_ann));

    Some(HuskExternFunction {
        name,
        params,
        return_type,
    })
}

fn extract_expr_type_name(expr: &Expr) -> String {
    match expr {
        Expr::Ident(ident) => ident.sym.to_string(),
        Expr::Member(member) => {
            // Handle qualified names like core.Application
            let obj = extract_expr_type_name(&member.obj);
            if let MemberProp::Ident(prop) = &member.prop {
                format!("{}::{}", obj, prop.sym)
            } else {
                obj
            }
        }
        _ => "unknown".to_string(),
    }
}

impl HuskExternModule {
    pub fn to_husk_code(&self) -> String {
        let mut output = String::new();

        // Output import information as comments
        if !self.imports.is_empty() {
            output.push_str("// TypeScript imports (for reference):\n");
            for import in &self.imports {
                match &import.import_type {
                    ImportType::Namespace => {
                        output.push_str(&format!("// import * as {} from \"{}\";\n", 
                            import.alias, import.source));
                    }
                    ImportType::Default => {
                        output.push_str(&format!("// import {} from \"{}\";\n", 
                            import.alias, import.source));
                    }
                    ImportType::Named(name) => {
                        if name == &import.alias {
                            output.push_str(&format!("// import {{ {} }} from \"{}\";\n", 
                                name, import.source));
                        } else {
                            output.push_str(&format!("// import {{ {} as {} }} from \"{}\";\n", 
                                name, import.alias, import.source));
                        }
                    }
                }
            }
            output.push('\n');
        }

        // Output top-level functions
        for func in &self.functions {
            output.push_str(&format!("extern fn {}(", func.name));
            
            let params: Vec<String> = func.params.iter()
                .map(|(name, typ)| format!("{}: {}", name, typ.to_string()))
                .collect();
            output.push_str(&params.join(", "));
            output.push(')');
            
            if let Some(ret_type) = &func.return_type {
                let ret_str = ret_type.to_string();
                if !ret_str.is_empty() {
                    output.push_str(&format!(" -> {}", ret_str));
                }
            }
            
            output.push_str(";\n\n");
        }

        // Output module with types and implementations
        if !self.types.is_empty() || !self.implementations.is_empty() {
            output.push_str(&format!("extern mod {} {{\n", self.name));
            
            // Type declarations
            for typ in &self.types {
                if !typ.extends.is_empty() {
                    output.push_str(&format!("    // {} extends {}\n", typ.name, typ.extends.join(", ")));
                }
                output.push_str(&format!("    type {};\n", typ.name));
            }
            
            if !self.types.is_empty() && !self.implementations.is_empty() {
                output.push('\n');
            }
            
            // Implementations
            for (i, impl_block) in self.implementations.iter().enumerate() {
                if i > 0 {
                    output.push('\n');
                }
                
                output.push_str(&format!("    impl {} {{\n", impl_block.type_name));
                
                for method in &impl_block.methods {
                    output.push_str("        fn ");
                    output.push_str(&method.name);
                    output.push('(');
                    
                    let mut all_params = Vec::new();
                    if method.has_self {
                        all_params.push("self".to_string());
                    }
                    
                    for (name, typ) in &method.params {
                        all_params.push(format!("{}: {}", name, typ.to_string()));
                    }
                    
                    output.push_str(&all_params.join(", "));
                    output.push(')');
                    
                    if let Some(ret_type) = &method.return_type {
                        let ret_str = ret_type.to_string();
                        if !ret_str.is_empty() {
                            output.push_str(&format!(" -> {}", ret_str));
                        }
                    }
                    
                    output.push_str(";\n");
                }
                
                output.push_str("    }\n");
            }
            
            output.push_str("}\n");
        }

        // Output namespaces
        for namespace in &self.namespaces {
            output.push('\n');
            output.push_str(&format!("// Namespace: {}\n", namespace.name));
            
            // Namespace functions
            for func in &namespace.functions {
                output.push_str(&format!("extern fn {}${}(", namespace.name, func.name));
                
                let params: Vec<String> = func.params.iter()
                    .map(|(name, typ)| format!("{}: {}", name, typ.to_string()))
                    .collect();
                output.push_str(&params.join(", "));
                output.push(')');
                
                if let Some(ret_type) = &func.return_type {
                    let ret_str = ret_type.to_string();
                    if !ret_str.is_empty() {
                        output.push_str(&format!(" -> {}", ret_str));
                    }
                }
                
                output.push_str(";\n");
            }
            
            // Namespace variables as extern constants
            for var in &namespace.variables {
                output.push_str(&format!("extern const {}${}: {};\n", 
                    namespace.name, var.name, var.var_type.to_string()));
            }
            
            // Namespace types
            if !namespace.types.is_empty() {
                output.push('\n');
                output.push_str(&format!("extern mod {} {{\n", namespace.name));
                for typ in &namespace.types {
                    if !typ.extends.is_empty() {
                        output.push_str(&format!("    // {} extends {}\n", typ.name, typ.extends.join(", ")));
                    }
                    output.push_str(&format!("    type {};\n", typ.name));
                }
                output.push_str("}\n");
            }
        }

        output
    }
}