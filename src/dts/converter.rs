use anyhow::Result;
use std::collections::HashMap;
use swc_ecma_ast::*;

use crate::dts::types::{convert_ts_type, HuskType};

#[derive(Debug)]
pub struct HuskExternModule {
    pub name: String,
    pub functions: Vec<HuskExternFunction>,
    pub types: Vec<HuskExternType>,
    pub implementations: Vec<HuskExternImpl>,
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
    let mut husk_module = HuskExternModule {
        name: module_name,
        functions: Vec::new(),
        types: Vec::new(),
        implementations: Vec::new(),
    };

    let mut interfaces: HashMap<String, Vec<HuskExternMethod>> = HashMap::new();

    for item in &module.body {
        match item {
            ModuleItem::ModuleDecl(decl) => process_module_decl(decl, &mut husk_module, &mut interfaces)?,
            ModuleItem::Stmt(stmt) => process_stmt(stmt, &mut husk_module, &mut interfaces)?,
        }
    }

    // Convert collected interfaces to implementations
    for (type_name, methods) in interfaces {
        husk_module.implementations.push(HuskExternImpl {
            type_name,
            methods,
        });
    }

    Ok(husk_module)
}

fn process_module_decl(
    decl: &ModuleDecl,
    husk_module: &mut HuskExternModule,
    interfaces: &mut HashMap<String, Vec<HuskExternMethod>>,
) -> Result<()> {
    match decl {
        ModuleDecl::ExportDecl(export) => {
            process_decl(&export.decl, husk_module, interfaces)?;
        }
        ModuleDecl::TsExportAssignment(_) => {
            // Handle export = syntax if needed
        }
        _ => {}
    }
    Ok(())
}

fn process_stmt(
    stmt: &Stmt,
    husk_module: &mut HuskExternModule,
    interfaces: &mut HashMap<String, Vec<HuskExternMethod>>,
) -> Result<()> {
    if let Stmt::Decl(decl) = stmt {
        process_decl(decl, husk_module, interfaces)?;
    }
    Ok(())
}

fn process_decl(
    decl: &Decl,
    husk_module: &mut HuskExternModule,
    interfaces: &mut HashMap<String, Vec<HuskExternMethod>>,
) -> Result<()> {
    match decl {
        Decl::TsInterface(interface) => {
            process_interface(interface, husk_module, interfaces)?;
        }
        Decl::TsTypeAlias(type_alias) => {
            // Add as a type
            husk_module.types.push(HuskExternType {
                name: type_alias.id.sym.to_string(),
            });
        }
        Decl::Fn(fn_decl) => {
            if let Some(func) = process_function_decl(fn_decl) {
                husk_module.functions.push(func);
            }
        }
        _ => {}
    }
    Ok(())
}

fn process_interface(
    interface: &TsInterfaceDecl,
    husk_module: &mut HuskExternModule,
    interfaces: &mut HashMap<String, Vec<HuskExternMethod>>,
) -> Result<()> {
    let interface_name = interface.id.sym.to_string();
    
    // Add type declaration
    husk_module.types.push(HuskExternType {
        name: interface_name.clone(),
    });

    // Process interface members
    let mut methods = Vec::new();
    
    for member in &interface.body.body {
        match member {
            TsTypeElement::TsMethodSignature(method) => {
                if let Some(husk_method) = process_method_signature(method) {
                    methods.push(husk_method);
                }
            }
            TsTypeElement::TsPropertySignature(prop) => {
                // Convert properties to getter methods
                if let Some(getter) = process_property_as_getter(prop) {
                    methods.push(getter);
                }
            }
            _ => {}
        }
    }

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

impl HuskExternModule {
    pub fn to_husk_code(&self) -> String {
        let mut output = String::new();

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

        output
    }
}