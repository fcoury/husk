use crate::{
    ast::visitor::AstVisitor,
    builtin_methods::TranspilerMethodRegistry,
    error::{Error, Result},
    package_resolver::PackageResolver,
    parser::{Expr, ExternItem, Operator, Stmt, UnaryOp, UseItems, UsePath, UsePrefix},
    span::Span,
};

pub struct JsTranspiler {
    indent_level: usize,
    package_resolver: Option<PackageResolver>,
    method_registry: TranspilerMethodRegistry,
}

impl JsTranspiler {
    pub fn new() -> Self {
        Self {
            indent_level: 0,
            package_resolver: None,
            method_registry: TranspilerMethodRegistry::new(),
        }
    }

    /// Create a new transpiler with package resolution enabled
    pub fn with_package_resolver() -> Result<Self> {
        Ok(Self {
            indent_level: 0,
            package_resolver: Some(PackageResolver::from_current_dir()?),
            method_registry: TranspilerMethodRegistry::new(),
        })
    }

    pub fn generate(&mut self, stmts: &[Stmt]) -> Result<String> {
        self.transpile(stmts)
    }

    pub fn transpile(&mut self, stmts: &[Stmt]) -> Result<String> {
        let mut output = String::new();
        output.push_str("function println(...args) { console.log(...args); }\n");
        // Add the intelligent await bridge for .await? operator
        output.push_str("async function __husk_await_bridge(promise) {\n");
        output.push_str("  try {\n");
        output.push_str("    const value = await promise;\n");
        output.push_str("    // Check if the resolved value is already a Husk Result\n");
        output.push_str("    if (value && typeof value === 'object' && (value.type === 'Ok' || value.type === 'Err')) {\n");
        output.push_str("      return value; // It's already a Result, pass it through\n");
        output.push_str("    }\n");
        output.push_str("    return { type: 'Ok', value }; // Wrap the success value\n");
        output.push_str("  } catch (error) {\n");
        output.push_str("    return __husk_map_error(error);\n");
        output.push_str("  }\n");
        output.push_str("}\n");

        // Add error mapping helper function
        output.push_str("function __husk_map_error(error) {\n");
        output.push_str("  if (error && typeof error === 'object' && (error.type === 'Ok' || error.type === 'Err')) {\n");
        output.push_str("    return error; // Already a Husk Result\n");
        output.push_str("  }\n");
        output.push_str("  \n");
        output.push_str("  let errorPayload;\n");
        output.push_str("  if (error instanceof Error) {\n");
        output.push_str("    errorPayload = {\n");
        output.push_str("      name: error.name,\n");
        output.push_str("      message: error.message,\n");
        output.push_str("      stack: error.stack || null\n");
        output.push_str("    };\n");
        output.push_str("  } else if (error instanceof DOMException) {\n");
        output.push_str("    errorPayload = {\n");
        output.push_str("      name: error.name,\n");
        output.push_str("      message: error.message,\n");
        output.push_str("      code: error.code\n");
        output.push_str("    };\n");
        output.push_str("  } else if (typeof error === 'string') {\n");
        output.push_str("    errorPayload = {\n");
        output.push_str("      message: error,\n");
        output.push_str("      stack: (new Error()).stack || null\n");
        output.push_str("    };\n");
        output.push_str("  } else {\n");
        output.push_str("    errorPayload = {\n");
        output.push_str("      value: error,\n");
        output.push_str("      stack: (new Error()).stack || null\n");
        output.push_str("    };\n");
        output.push_str("  }\n");
        output.push_str("  \n");
        output.push_str("  return { type: 'Err', value: errorPayload };\n");
        output.push_str("}\n");

        // Add safe function call wrapper
        output.push_str("function __husk_safe_call(fn, ...args) {\n");
        output.push_str("  try {\n");
        output.push_str("    const result = fn(...args);\n");
        output.push_str("    return { type: 'Ok', value: result };\n");
        output.push_str("  } catch (error) {\n");
        output.push_str("    return __husk_map_error(error);\n");
        output.push_str("  }\n");
        output.push_str("}\n");

        output.push_str("function __format__(formatStr, ...args) {\n");
        output.push_str("  let result = '';\n");
        output.push_str("  let argIndex = 0;\n");
        output.push_str("  let i = 0;\n");
        output.push_str("  while (i < formatStr.length) {\n");
        output.push_str("    if (formatStr[i] === '{' && i + 1 < formatStr.length) {\n");
        output.push_str("      if (formatStr[i + 1] === '{') {\n");
        output.push_str("        result += '{';\n");
        output.push_str("        i += 2;\n");
        output.push_str("      } else if (formatStr[i + 1] === '}') {\n");
        output.push_str("        if (argIndex < args.length) {\n");
        output.push_str("          result += String(args[argIndex++]);\n");
        output.push_str("        }\n");
        output.push_str("        i += 2;\n");
        output.push_str("      } else {\n");
        output.push_str("        result += formatStr[i];\n");
        output.push_str("        i++;\n");
        output.push_str("      }\n");
        output.push_str("    } else if (formatStr[i] === '}' && i + 1 < formatStr.length && formatStr[i + 1] === '}') {\n");
        output.push_str("      result += '}';\n");
        output.push_str("      i += 2;\n");
        output.push_str("    } else {\n");
        output.push_str("      result += formatStr[i];\n");
        output.push_str("      i++;\n");
        output.push_str("    }\n");
        output.push_str("  }\n");
        output.push_str("  return result;\n");
        output.push_str("}\n");

        // Add IO functions
        output.push_str("\n// File I/O functions\n");
        output.push_str("const fs = require('fs');\n");
        output.push_str("const path = require('path');\n\n");

        // read_file
        output.push_str("function read_file(filePath) {\n");
        output.push_str("  try {\n");
        output.push_str("    return { type: 'Ok', value: fs.readFileSync(filePath, 'utf8') };\n");
        output.push_str("  } catch (e) {\n");
        output.push_str("    return { type: 'Err', value: e.message };\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        // read_file_bytes
        output.push_str("function read_file_bytes(filePath) {\n");
        output.push_str("  try {\n");
        output.push_str("    const buffer = fs.readFileSync(filePath);\n");
        output.push_str("    return { type: 'Ok', value: Array.from(buffer) };\n");
        output.push_str("  } catch (e) {\n");
        output.push_str("    return { type: 'Err', value: e.message };\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        // read_lines
        output.push_str("function read_lines(filePath) {\n");
        output.push_str("  try {\n");
        output.push_str("    const content = fs.readFileSync(filePath, 'utf8');\n");
        output.push_str("    const lines = content.split('\\n');\n");
        output.push_str("    // Remove last empty line if file ends with newline\n");
        output.push_str("    if (lines[lines.length - 1] === '') lines.pop();\n");
        output.push_str("    return { type: 'Ok', value: lines };\n");
        output.push_str("  } catch (e) {\n");
        output.push_str("    return { type: 'Err', value: e.message };\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        // write_file
        output.push_str("function write_file(filePath, contents) {\n");
        output.push_str("  try {\n");
        output.push_str("    fs.writeFileSync(filePath, contents);\n");
        output.push_str("    return undefined; // unit\n");
        output.push_str("  } catch (e) {\n");
        output.push_str("    throw new Error(e.message);\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        // write_file_bytes
        output.push_str("function write_file_bytes(filePath, data) {\n");
        output.push_str("  try {\n");
        output.push_str("    const buffer = Buffer.from(data);\n");
        output.push_str("    fs.writeFileSync(filePath, buffer);\n");
        output.push_str("    return undefined; // unit\n");
        output.push_str("  } catch (e) {\n");
        output.push_str("    throw new Error(e.message);\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        // append_file
        output.push_str("function append_file(filePath, contents) {\n");
        output.push_str("  try {\n");
        output.push_str("    fs.appendFileSync(filePath, contents);\n");
        output.push_str("    return undefined; // unit\n");
        output.push_str("  } catch (e) {\n");
        output.push_str("    throw new Error(e.message);\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        // Path functions
        output.push_str("function exists(filePath) {\n");
        output.push_str("  try {\n");
        output.push_str("    fs.accessSync(filePath);\n");
        output.push_str("    return true;\n");
        output.push_str("  } catch {\n");
        output.push_str("    return false;\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        output.push_str("function is_file(filePath) {\n");
        output.push_str("  try {\n");
        output.push_str("    return fs.statSync(filePath).isFile();\n");
        output.push_str("  } catch {\n");
        output.push_str("    return false;\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        output.push_str("function is_dir(filePath) {\n");
        output.push_str("  try {\n");
        output.push_str("    return fs.statSync(filePath).isDirectory();\n");
        output.push_str("  } catch {\n");
        output.push_str("    return false;\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        output.push_str("function create_dir(dirPath) {\n");
        output.push_str("  try {\n");
        output.push_str("    fs.mkdirSync(dirPath);\n");
        output.push_str("    return { type: 'Ok', value: null };\n");
        output.push_str("  } catch (error) {\n");
        output.push_str("    if (error.code === 'EEXIST') {\n");
        output.push_str(
            "      return { type: 'Err', error: `Directory already exists: ${dirPath}` };\n",
        );
        output.push_str("    } else if (error.code === 'EACCES') {\n");
        output.push_str("      return { type: 'Err', error: `Permission denied: ${dirPath}` };\n");
        output.push_str("    } else if (error.code === 'ENOENT') {\n");
        output.push_str(
            "      return { type: 'Err', error: `Parent directory not found: ${dirPath}` };\n",
        );
        output.push_str("    } else {\n");
        output.push_str("      return { type: 'Err', error: `IO error creating directory ${dirPath}: ${error.message}` };\n");
        output.push_str("    }\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        output.push_str("function create_dir_all(dirPath) {\n");
        output.push_str("  try {\n");
        output.push_str("    fs.mkdirSync(dirPath, { recursive: true });\n");
        output.push_str("    return { type: 'Ok', value: null };\n");
        output.push_str("  } catch (error) {\n");
        output.push_str("    if (error.code === 'EACCES') {\n");
        output.push_str("      return { type: 'Err', error: `Permission denied: ${dirPath}` };\n");
        output.push_str("    } else {\n");
        output.push_str("      return { type: 'Err', error: `IO error creating directories ${dirPath}: ${error.message}` };\n");
        output.push_str("    }\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        output.push_str("function remove_dir(dirPath) {\n");
        output.push_str("  try {\n");
        output.push_str("    fs.rmdirSync(dirPath);\n");
        output.push_str("    return { type: 'Ok', value: null };\n");
        output.push_str("  } catch (error) {\n");
        output.push_str("    if (error.code === 'ENOENT') {\n");
        output
            .push_str("      return { type: 'Err', error: `Directory not found: ${dirPath}` };\n");
        output.push_str("    } else if (error.code === 'EACCES') {\n");
        output.push_str("      return { type: 'Err', error: `Permission denied: ${dirPath}` };\n");
        output.push_str("    } else if (error.code === 'ENOTEMPTY') {\n");
        output
            .push_str("      return { type: 'Err', error: `Directory not empty: ${dirPath}` };\n");
        output.push_str("    } else {\n");
        output.push_str("      return { type: 'Err', error: `IO error removing directory ${dirPath}: ${error.message}` };\n");
        output.push_str("    }\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        output.push_str("function remove_dir_all(dirPath) {\n");
        output.push_str("  try {\n");
        output.push_str("    fs.rmSync(dirPath, { recursive: true });\n");
        output.push_str("    return { type: 'Ok', value: null };\n");
        output.push_str("  } catch (error) {\n");
        output.push_str("    if (error.code === 'ENOENT') {\n");
        output
            .push_str("      return { type: 'Err', error: `Directory not found: ${dirPath}` };\n");
        output.push_str("    } else if (error.code === 'EACCES') {\n");
        output.push_str("      return { type: 'Err', error: `Permission denied: ${dirPath}` };\n");
        output.push_str("    } else {\n");
        output.push_str("      return { type: 'Err', error: `IO error removing directory ${dirPath}: ${error.message}` };\n");
        output.push_str("    }\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        output.push_str("function read_dir(dirPath) {\n");
        output.push_str("  try {\n");
        output.push_str("    const entries = fs.readdirSync(dirPath, { withFileTypes: true });\n");
        output.push_str("    const result = entries.map(entry => ({\n");
        output.push_str("      name: entry.name,\n");
        output.push_str("      is_file: entry.isFile(),\n");
        output.push_str("      is_dir: entry.isDirectory()\n");
        output.push_str("    }));\n");
        output.push_str("    return { type: 'Ok', value: result };\n");
        output.push_str("  } catch (error) {\n");
        output.push_str("    if (error.code === 'ENOENT') {\n");
        output
            .push_str("      return { type: 'Err', error: `Directory not found: ${dirPath}` };\n");
        output.push_str("    } else if (error.code === 'EACCES') {\n");
        output.push_str("      return { type: 'Err', error: `Permission denied: ${dirPath}` };\n");
        output.push_str("    } else if (error.code === 'ENOTDIR') {\n");
        output.push_str("      return { type: 'Err', error: `Not a directory: ${dirPath}` };\n");
        output.push_str("    } else {\n");
        output.push_str("      return { type: 'Err', error: `IO error reading directory ${dirPath}: ${error.message}` };\n");
        output.push_str("    }\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        // Console IO functions
        output.push_str("// Console I/O functions\n");
        output.push_str("const readline = require('readline');\n\n");

        output.push_str("function read_line() {\n");
        output.push_str("  try {\n");
        output.push_str("    const rl = readline.createInterface({\n");
        output.push_str("      input: process.stdin,\n");
        output.push_str("      output: process.stdout\n");
        output.push_str("    });\n");
        output.push_str("    return new Promise((resolve) => {\n");
        output.push_str("      rl.question('', (answer) => {\n");
        output.push_str("        rl.close();\n");
        output.push_str("        resolve({ type: 'Ok', value: answer });\n");
        output.push_str("      });\n");
        output.push_str("    });\n");
        output.push_str("  } catch (error) {\n");
        output.push_str("    return { type: 'Err', value: error.message };\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        output.push_str("function eprint(message) {\n");
        output.push_str("  try {\n");
        output.push_str("    const bytesWritten = process.stderr.write(message);\n");
        output.push_str("    return bytesWritten ? message.length : 0;\n");
        output.push_str("  } catch {\n");
        output.push_str("    return 0;\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        output.push_str("function eprintln(message) {\n");
        output.push_str("  try {\n");
        output.push_str("    process.stderr.write(message + '\\n');\n");
        output.push_str("    return undefined; // unit\n");
        output.push_str("  } catch {\n");
        output.push_str("    return undefined; // unit\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        // Async file I/O functions
        output.push_str("// Async File I/O functions\n");
        output.push_str("const fsPromises = require('fs').promises;\n\n");

        // read_file_async
        output.push_str("async function read_file_async(filePath) {\n");
        output.push_str("  try {\n");
        output.push_str("    const content = await fsPromises.readFile(filePath, 'utf8');\n");
        output.push_str("    return { type: 'Ok', value: content };\n");
        output.push_str("  } catch (e) {\n");
        output.push_str("    return { type: 'Err', value: e.message };\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        // read_file_bytes_async
        output.push_str("async function read_file_bytes_async(filePath) {\n");
        output.push_str("  try {\n");
        output.push_str("    const buffer = await fsPromises.readFile(filePath);\n");
        output.push_str("    return { type: 'Ok', value: Array.from(buffer) };\n");
        output.push_str("  } catch (e) {\n");
        output.push_str("    return { type: 'Err', value: e.message };\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        // read_lines_async
        output.push_str("async function read_lines_async(filePath) {\n");
        output.push_str("  try {\n");
        output.push_str("    const content = await fsPromises.readFile(filePath, 'utf8');\n");
        output.push_str("    const lines = content.split('\\n');\n");
        output.push_str("    if (lines[lines.length - 1] === '') lines.pop();\n");
        output.push_str("    return { type: 'Ok', value: lines };\n");
        output.push_str("  } catch (e) {\n");
        output.push_str("    return { type: 'Err', value: e.message };\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        // write_file_async
        output.push_str("async function write_file_async(filePath, contents) {\n");
        output.push_str("  try {\n");
        output.push_str("    await fsPromises.writeFile(filePath, contents);\n");
        output.push_str("    return { type: 'Ok', value: undefined };\n");
        output.push_str("  } catch (e) {\n");
        output.push_str("    return { type: 'Err', value: e.message };\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        // write_file_bytes_async
        output.push_str("async function write_file_bytes_async(filePath, data) {\n");
        output.push_str("  try {\n");
        output.push_str("    const buffer = Buffer.from(data);\n");
        output.push_str("    await fsPromises.writeFile(filePath, buffer);\n");
        output.push_str("    return { type: 'Ok', value: undefined };\n");
        output.push_str("  } catch (e) {\n");
        output.push_str("    return { type: 'Err', value: e.message };\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        // append_file_async
        output.push_str("async function append_file_async(filePath, contents) {\n");
        output.push_str("  try {\n");
        output.push_str("    await fsPromises.appendFile(filePath, contents);\n");
        output.push_str("    return { type: 'Ok', value: undefined };\n");
        output.push_str("  } catch (e) {\n");
        output.push_str("    return { type: 'Err', value: e.message };\n");
        output.push_str("  }\n");
        output.push_str("}\n\n");

        for stmt in stmts {
            let js_code = self.visit_stmt(stmt)?;
            output.push_str(&js_code);
            output.push_str(";\n");
        }

        Ok(output)
    }

    fn indent(&self) -> String {
        "  ".repeat(self.indent_level)
    }

    fn generate_body(&mut self, stmts: &[Stmt]) -> Result<String> {
        let mut result = String::new();
        for (i, stmt) in stmts.iter().enumerate() {
            let stmt_str = self.visit_stmt(stmt)?;
            let is_last = i == stmts.len() - 1;

            // Check if this is a control flow statement that shouldn't be returned
            let is_control_flow = matches!(
                stmt,
                Stmt::Break(_) | Stmt::Continue(_) | Stmt::Return(_, _)
            );

            // Check if this is an expression statement without semicolon or a match expression (should be returned)
            let should_return = is_last
                && !is_control_flow
                && (matches!(stmt, Stmt::Expression(_, false))
                    || matches!(stmt, Stmt::Match(_, _, _)));

            if should_return {
                result.push_str(&format!("return {};", stmt_str));
            } else {
                result.push_str(&format!("{};", stmt_str));
            }
        }
        Ok(result)
    }

    fn generate_match_condition(&self, _expr: &Expr, pattern: &Expr) -> (String, String) {
        match pattern {
            Expr::EnumVariantOrMethodCall {
                target, call, args, ..
            } => {
                if let Expr::Identifier(type_name, _) = &**target {
                    // Special handling for Option
                    if type_name == "Option" {
                        match call.as_str() {
                            "None" => {
                                return (
                                    "(_matched && _matched.type === 'None')".to_string(),
                                    String::new(),
                                )
                            }
                            "Some" => {
                                if let Some(Expr::Identifier(bind_name, _)) = args.first() {
                                    return (
                                        "(_matched && _matched.type === 'Some')".to_string(),
                                        format!("const {} = _matched.value;\n", bind_name),
                                    );
                                }
                                return (
                                    "(_matched && _matched.type === 'Some')".to_string(),
                                    String::new(),
                                );
                            }
                            _ => {}
                        }
                    }

                    // Special handling for Result
                    if type_name == "Result" {
                        match call.as_str() {
                            "Ok" => {
                                if let Some(Expr::Identifier(bind_name, _)) = args.first() {
                                    return (
                                        "(_matched && typeof _matched === 'object' && 'Ok' in _matched)".to_string(),
                                        format!("const {} = _matched.Ok;\n", bind_name),
                                    );
                                }
                                return (
                                    "(_matched && typeof _matched === 'object' && 'Ok' in _matched)".to_string(),
                                    String::new(),
                                );
                            }
                            "Err" => {
                                if let Some(Expr::Identifier(bind_name, _)) = args.first() {
                                    return (
                                        "(_matched && typeof _matched === 'object' && 'Err' in _matched)".to_string(),
                                        format!("const {} = _matched.Err;\n", bind_name),
                                    );
                                }
                                return (
                                    "(_matched && typeof _matched === 'object' && 'Err' in _matched)".to_string(),
                                    String::new(),
                                );
                            }
                            _ => {}
                        }
                    }
                }

                // Default enum handling
                if let Some(value) = args.first() {
                    if let Expr::Identifier(bind_name, _) = value {
                        (
                            format!("_matched instanceof {}.{}", target, call),
                            format!("const {} = _matched.value;\n", bind_name),
                        )
                    } else {
                        (
                            format!("_matched instanceof {}.{}", target, call),
                            String::new(),
                        )
                    }
                } else {
                    (format!("_matched === {}.{}", target, call), String::new())
                }
            }
            Expr::Identifier(var_name, _) => {
                if var_name == "_" {
                    ("true".to_string(), String::new())
                } else {
                    (
                        "true".to_string(),
                        format!("const {} = _matched;\n", var_name),
                    )
                }
            }
            Expr::Tuple(patterns, _) => {
                // Handle tuple patterns
                let mut conditions = vec!["Array.isArray(_matched)".to_string()];
                conditions.push(format!("_matched.length === {}", patterns.len()));

                let mut bindings = String::new();

                for (i, pattern) in patterns.iter().enumerate() {
                    match pattern {
                        Expr::Identifier(name, _) if name == "_" => {
                            // Wildcard pattern, no condition or binding needed
                        }
                        Expr::Identifier(name, _) => {
                            // Bind the element to a variable
                            bindings.push_str(&format!("const {} = _matched[{}];\n", name, i));
                        }
                        Expr::Int(value, _) => {
                            // Match against literal int
                            conditions.push(format!("_matched[{}] === {}", i, value));
                        }
                        Expr::String(value, _) => {
                            // Match against literal string
                            conditions.push(format!("_matched[{}] === \"{}\"", i, value));
                        }
                        Expr::Bool(value, _) => {
                            // Match against literal bool
                            conditions.push(format!("_matched[{}] === {}", i, value));
                        }
                        _ => {
                            // For nested patterns, we'd need more complex handling
                            // For now, just match anything
                        }
                    }
                }

                (format!("({})", conditions.join(" && ")), bindings)
            }
            Expr::StructPattern(variant, fields, _) => {
                // Handle struct-like enum patterns (e.g., Command::Process { input, output })
                // Convert :: to . for JavaScript
                let js_variant = variant.replace("::", ".");

                // Check if it's the right variant
                let condition = if variant.contains("::") {
                    format!("_matched instanceof {}", js_variant)
                } else {
                    // For plain struct patterns without enum
                    "_matched && typeof _matched === 'object'".to_string()
                };

                // Generate bindings for fields
                let mut bindings = String::new();
                for (field_name, opt_var_name) in fields {
                    // Skip the special ".." rest pattern marker
                    if field_name == ".." {
                        continue;
                    }

                    let var_name = opt_var_name.as_ref().unwrap_or(field_name);
                    bindings.push_str(&format!("const {} = _matched.{};\n", var_name, field_name));
                }

                (condition, bindings)
            }
            _ => ("true".to_string(), String::new()),
        }
    }

    fn generate_enum(&self, name: &str, variants: &[crate::parser::EnumVariant]) -> String {
        let mut output = String::new();

        // Generate base enum class
        output.push_str(&format!("class {} {{\n", name));
        output.push_str("  constructor(variant, value) {\n");
        output.push_str("    this.variant = variant;\n");
        output.push_str("    this.value = value;\n");
        output.push_str("  }\n");
        output.push_str("}\n");

        // Generate variant constructors
        for variant in variants {
            let (variant_name, is_unit) = match variant {
                crate::parser::EnumVariant::Unit(name) => (name, true),
                crate::parser::EnumVariant::Tuple(name, _) => (name, false),
                crate::parser::EnumVariant::Struct(name, _) => (name, false),
            };

            output.push_str(&format!(
                "{}.{} = class extends {} {{",
                name, variant_name, name
            ));
            output.push_str(&format!(
                "constructor(value) {{ super('{}', value); }}",
                variant_name
            ));
            output.push_str("};\n");

            // Generate static instance only for unit variants (variants without data)
            if is_unit {
                output.push_str(&format!(
                    "{}.{} = new {}.{}();\n",
                    name, variant_name, name, variant_name
                ));
            }
        }

        output
    }
}

impl AstVisitor<String> for JsTranspiler {
    type Error = Error;

    /// Override visit_stmt to handle function visibility
    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<String> {
        match stmt {
            // Handle function statements with visibility
            Stmt::Function(is_pub, name, generic_params, params, return_type, body, span) => {
                let function_code =
                    self.visit_function(name, generic_params, params, return_type, body, span)?;
                if *is_pub {
                    Ok(format!("export {}", function_code))
                } else {
                    Ok(function_code)
                }
            }
            // Handle async function statements with visibility
            Stmt::AsyncFunction(is_pub, name, generic_params, params, return_type, body, span) => {
                let function_code = self.visit_async_function(
                    name,
                    generic_params,
                    params,
                    return_type,
                    body,
                    span,
                )?;
                if *is_pub {
                    Ok(format!("export {}", function_code))
                } else {
                    Ok(function_code)
                }
            }
            // Delegate all other statements to the default implementation
            _ => {
                // Use the default visitor implementation for all other statement types
                match stmt {
                    Stmt::Let(name, expr, span) => self.visit_let(name, expr, span),
                    Stmt::Struct(name, generic_params, fields, span) => {
                        self.visit_struct(name, generic_params, fields, span)
                    }
                    Stmt::Enum(name, generic_params, variants, span) => {
                        self.visit_enum(name, generic_params, variants, span)
                    }
                    Stmt::Impl(struct_name, methods, span) => {
                        self.visit_impl(struct_name, methods, span)
                    }
                    Stmt::Match(expr, arms, span) => self.visit_match(expr, arms, span),
                    Stmt::ForLoop(pattern, iterable, body, span) => {
                        self.visit_for_loop(pattern, iterable, body, span)
                    }
                    Stmt::While(condition, body, span) => self.visit_while(condition, body, span),
                    Stmt::Loop(body, span) => self.visit_loop(body, span),
                    Stmt::Break(span) => self.visit_break(span),
                    Stmt::Continue(span) => self.visit_continue(span),
                    Stmt::Return(expr, span) => self.visit_return(expr.as_ref(), span),
                    Stmt::Expression(expr, has_semicolon) => {
                        self.visit_expression_stmt(expr, *has_semicolon)
                    }
                    Stmt::Use(path, items, span) => self.visit_use(path, items, span),
                    Stmt::ExternFunction(name, generic_params, params, return_type, span) => {
                        self.visit_extern_function(name, generic_params, params, return_type, span)
                    }
                    Stmt::ExternMod(name, items, span) => self.visit_extern_mod(name, items, span),
                    Stmt::ExternType(name, generic_params, span) => {
                        self.visit_extern_type(name, generic_params, span)
                    }
                    _ => unreachable!("All statement types should be handled above"),
                }
            }
        }
    }

    // ===== Expression visit methods =====

    fn visit_int(&mut self, value: i64, _span: &Span) -> Result<String> {
        Ok(value.to_string())
    }

    fn visit_float(&mut self, value: f64, _span: &Span) -> Result<String> {
        Ok(value.to_string())
    }

    fn visit_string(&mut self, value: &str, _span: &Span) -> Result<String> {
        // Escape special characters for JavaScript string literals
        let escaped = value
            .replace('\\', "\\\\") // Backslash must be escaped first
            .replace('\"', "\\\"") // Escape double quotes
            .replace('\n', "\\n") // Escape newlines
            .replace('\r', "\\r") // Escape carriage returns
            .replace('\t', "\\t") // Escape tabs
            .replace('\0', "\\0"); // Escape null bytes

        Ok(format!("\"{}\"", escaped))
    }

    fn visit_bool(&mut self, value: bool, _span: &Span) -> Result<String> {
        Ok(value.to_string())
    }

    fn visit_unit(&mut self, _span: &Span) -> Result<String> {
        Ok("void 0".to_string()) // JavaScript representation of unit/void
    }

    fn visit_identifier(&mut self, name: &str, _span: &Span) -> Result<String> {
        Ok(name.to_string())
    }

    fn visit_binary_op(
        &mut self,
        left: &Expr,
        op: &Operator,
        right: &Expr,
        _span: &Span,
    ) -> Result<String> {
        let left_str = self.visit_expr(left)?;
        let right_str = self.visit_expr(right)?;
        let op_str = match op {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::Equals => "===",
            Operator::NotEquals => "!==",
            Operator::Modulo => "%",
            Operator::LessThan => "<",
            Operator::LessThanEquals => "<=",
            Operator::GreaterThan => ">",
            Operator::GreaterThanEquals => ">=",
            Operator::And => "&&",
            Operator::Or => "||",
        };
        Ok(format!("({} {} {})", left_str, op_str, right_str))
    }

    fn visit_unary_op(&mut self, op: &UnaryOp, expr: &Expr, _span: &Span) -> Result<String> {
        let expr_str = self.visit_expr(expr)?;
        match op {
            UnaryOp::Neg => Ok(format!("(-{})", expr_str)),
            UnaryOp::Not => Ok(format!("(!{})", expr_str)),
        }
    }

    fn visit_assign(&mut self, left: &Expr, right: &Expr, _span: &Span) -> Result<String> {
        let left_str = self.visit_expr(left)?;
        let right_str = self.visit_expr(right)?;
        Ok(format!("{} = {}", left_str, right_str))
    }

    fn visit_compound_assign(
        &mut self,
        left: &Expr,
        op: &Operator,
        right: &Expr,
        _span: &Span,
    ) -> Result<String> {
        let left_str = self.visit_expr(left)?;
        let right_str = self.visit_expr(right)?;
        let op_str = match op {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::Modulo => "%",
            _ => {
                return Err(Error::new_transpile(
                    format!("Invalid compound assignment operator: {:?}", op),
                    *_span,
                ))
            }
        };
        Ok(format!("{} {}= {}", left_str, op_str, right_str))
    }

    fn visit_function_call(&mut self, name: &str, args: &[Expr], span: &Span) -> Result<String> {
        // Special handling for format!
        if name == "format!" {
            if args.is_empty() {
                return Err(Error::new_transpile(
                    "format! requires at least one argument".to_string(),
                    *span,
                ));
            }

            // First argument must be a string literal
            if let Expr::String(format_str, _) = &args[0] {
                let mut template_parts = Vec::new();
                let mut arg_index = 1;
                let mut chars = format_str.chars().peekable();
                let mut current_part = String::new();

                while let Some(ch) = chars.next() {
                    if ch == '{' {
                        if chars.peek() == Some(&'{') {
                            // Escaped {{
                            chars.next();
                            current_part.push('{');
                        } else if chars.peek() == Some(&'}') {
                            // Placeholder {}
                            chars.next();
                            template_parts.push(current_part.clone());
                            current_part.clear();

                            if arg_index < args.len() {
                                let arg_str = self.visit_expr(&args[arg_index])?;
                                template_parts.push(format!("${{{}}}", arg_str));
                                arg_index += 1;
                            }
                        } else {
                            current_part.push(ch);
                        }
                    } else if ch == '}' {
                        if chars.peek() == Some(&'}') {
                            // Escaped }}
                            chars.next();
                            current_part.push('}');
                        } else {
                            current_part.push(ch);
                        }
                    } else {
                        current_part.push(ch);
                    }
                }

                if !current_part.is_empty() {
                    template_parts.push(current_part);
                }

                // Return as template literal
                Ok(format!("`{}`", template_parts.join("")))
            } else {
                // Dynamic format string - use a helper function
                let args_str = args
                    .iter()
                    .map(|arg| self.visit_expr(arg))
                    .collect::<Result<Vec<_>>>()?
                    .join(", ");
                Ok(format!("__format__({})", args_str))
            }
        } else {
            // Regular function call
            let args_str = args
                .iter()
                .map(|arg| self.visit_expr(arg))
                .collect::<Result<Vec<_>>>()?
                .join(", ");

            // Handle method calls with dot notation
            if name.contains('.') {
                let (obj, method) = name.split_once('.').unwrap();
                Ok(format!("{}.{}({})", obj, method, args_str))
            } else {
                Ok(format!("{}({})", name, args_str))
            }
        }
    }

    fn visit_struct_init(
        &mut self,
        name: &str,
        fields: &[(String, Expr)],
        _span: &Span,
    ) -> Result<String> {
        // Convert :: to . for JavaScript
        let js_name = name.replace("::", ".");

        let mut result = String::new();
        result.push_str("(function() {");
        result.push_str(&format!(
            "const __INSTANCE__ = Object.create({}.prototype);",
            js_name
        ));

        for (field_name, field_expr) in fields {
            let field_value = self.visit_expr(field_expr)?;
            result.push_str(&format!("__INSTANCE__.{} = {};", field_name, field_value));
        }

        result.push_str("return __INSTANCE__;");
        result.push_str("})()");
        Ok(result)
    }

    fn visit_member_access(&mut self, object: &Expr, field: &str, _span: &Span) -> Result<String> {
        let obj_str = self.visit_expr(object)?;
        Ok(format!("{}.{}", obj_str, field))
    }

    fn visit_enum_variant_or_method_call(
        &mut self,
        target: &Expr,
        call: &str,
        args: &[Expr],
        _span: &Span,
    ) -> Result<String> {
        let target_str = self.visit_expr(target)?;

        // Special handling for built-in Option type
        if target_str == "Option" {
            match call {
                "None" => return Ok("{ type: 'None', value: null }".to_string()),
                "Some" => {
                    if !args.is_empty() {
                        let arg_str = self.visit_expr(&args[0])?;
                        return Ok(format!("{{ type: 'Some', value: {} }}", arg_str));
                    } else {
                        return Ok("{ type: 'Some', value: undefined }".to_string());
                    }
                }
                _ => {}
            }
        }

        // Special handling for built-in Result type
        if target_str == "Result" {
            match call {
                "Ok" => {
                    if !args.is_empty() {
                        let arg_str = self.visit_expr(&args[0])?;
                        return Ok(format!("{{ type: 'Ok', value: {} }}", arg_str));
                    } else {
                        return Ok("{ type: 'Ok', value: undefined }".to_string());
                    }
                }
                "Err" => {
                    if !args.is_empty() {
                        let arg_str = self.visit_expr(&args[0])?;
                        return Ok(format!("{{ type: 'Err', value: {} }}", arg_str));
                    } else {
                        return Ok("{ type: 'Err', value: undefined }".to_string());
                    }
                }
                _ => {}
            }
        }

        // Check if this is a method call on a primitive type that we have in our registry
        let arg_strs: Vec<String> = args
            .iter()
            .map(|arg| self.visit_expr(arg))
            .collect::<Result<Vec<_>>>()?;

        // For methods that exist on both types, we need to be smarter
        // Check if this looks like a string method based on the arguments
        if call == "find" || call == "position" {
            // If the first argument looks like a string literal, it's probably a string method
            if !args.is_empty() {
                if let Expr::String(_, _) = &args[0] {
                    // It's a string method
                    if let Some(method_impl) = self.method_registry.get_string_method(call) {
                        return Ok(method_impl(&target_str, &arg_strs));
                    }
                } else {
                    // It's likely an array method (expects a closure/function)
                    if let Some(method_impl) = self.method_registry.get_array_method(call) {
                        return Ok(method_impl(&target_str, &arg_strs));
                    }
                }
            }
        }

        // Try array methods first (for methods like slice that exist on both types)
        if let Some(method_impl) = self.method_registry.get_array_method(call) {
            return Ok(method_impl(&target_str, &arg_strs));
        }

        // Try string methods
        if let Some(method_impl) = self.method_registry.get_string_method(call) {
            return Ok(method_impl(&target_str, &arg_strs));
        }

        // Default handling for other enums and unknown methods
        if args.is_empty() {
            if call == "new" || call.chars().next().unwrap().is_lowercase() {
                Ok(format!("{}.{}()", target_str, call))
            } else {
                Ok(format!("{}.{}", target_str, call))
            }
        } else {
            let args_str = arg_strs.join(", ");
            // Check if it's a static method call (like Point::new) or enum variant
            // For now, we'll assume static methods don't need 'new' prefix
            if call == "new" || call.chars().next().unwrap().is_lowercase() {
                Ok(format!("{}.{}({})", target_str, call, args_str))
            } else {
                Ok(format!("new {}.{}({})", target_str, call, args_str))
            }
        }
    }

    fn visit_array(&mut self, elements: &[Expr], _span: &Span) -> Result<String> {
        let elements_str = elements
            .iter()
            .map(|elem| self.visit_expr(elem))
            .collect::<Result<Vec<_>>>()?
            .join(", ");
        Ok(format!("[{}]", elements_str))
    }

    fn visit_tuple(&mut self, elements: &[Expr], _span: &Span) -> Result<String> {
        // In JavaScript, we'll represent tuples as arrays
        let elements_str = elements
            .iter()
            .map(|elem| self.visit_expr(elem))
            .collect::<Result<Vec<_>>>()?
            .join(", ");
        Ok(format!("[{}]", elements_str))
    }

    fn visit_array_index(&mut self, array: &Expr, index: &Expr, _span: &Span) -> Result<String> {
        let array_str = self.visit_expr(array)?;

        match index {
            Expr::Range(start, end, inclusive, _) => {
                let mut end_str = String::new();
                if let Some(end_expr) = end {
                    end_str = self.visit_expr(end_expr)?;
                    if *inclusive {
                        end_str = format!("{} + 1", end_str);
                    }
                }

                match (start, end) {
                    (Some(start_expr), Some(_)) => {
                        let start_str = self.visit_expr(start_expr)?;
                        Ok(format!("{}.slice({}, {})", array_str, start_str, end_str))
                    }
                    (Some(start_expr), None) => {
                        let start_str = self.visit_expr(start_expr)?;
                        Ok(format!("{}.slice({})", array_str, start_str))
                    }
                    (None, Some(_)) => Ok(format!("{}.slice(0, {})", array_str, end_str)),
                    (None, None) => Ok(array_str),
                }
            }
            _ => {
                let index_str = self.visit_expr(index)?;
                Ok(format!("{}[{}]", array_str, index_str))
            }
        }
    }

    fn visit_range(
        &mut self,
        _start: Option<&Expr>,
        _end: Option<&Expr>,
        _inclusive: bool,
        _span: &Span,
    ) -> Result<String> {
        // Ranges are handled in context (for loops, array slicing)
        Ok("/* range expression */".to_string())
    }

    fn visit_block(&mut self, stmts: &[Stmt], _span: &Span) -> Result<String> {
        // In JavaScript, we can use an IIFE (Immediately Invoked Function Expression)
        // to create a block that returns a value
        let mut result = "(() => {\n".to_string();

        self.indent_level += 1;

        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            let indent = self.indent();

            match stmt {
                // If the last statement is an expression without semicolon,
                // we need to return its value
                Stmt::Expression(expr, false) if is_last => {
                    let expr_str = self.visit_expr(expr)?;
                    result.push_str(&format!("{}return {};\n", indent, expr_str));
                }
                // Otherwise, generate the statement normally
                _ => {
                    let stmt_str = self.visit_stmt(stmt)?;
                    result.push_str(&format!("{}{}\n", indent, stmt_str));
                }
            }
        }

        self.indent_level -= 1;
        result.push_str(&format!("{}}})()", self.indent()));

        Ok(result)
    }

    // ===== Statement visit methods =====

    fn visit_let(&mut self, name: &str, expr: &Expr, _span: &Span) -> Result<String> {
        let value = self.visit_expr(expr)?;
        Ok(format!("let {} = {}", name, value))
    }

    fn visit_function(
        &mut self,
        name: &str,
        _generic_params: &[String],
        params: &[(String, String)],
        _return_type: &str,
        body: &[Stmt],
        _span: &Span,
    ) -> Result<String> {
        let params_str = params
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>()
            .join(", ");

        self.indent_level += 1;
        let body_str = self.generate_body(body)?;
        self.indent_level -= 1;

        Ok(format!(
            "function {}({}) {{\n{}\n}}",
            name, params_str, body_str
        ))
    }

    fn visit_while(&mut self, condition: &Expr, body: &[Stmt], _span: &Span) -> Result<String> {
        let condition_str = self.visit_expr(condition)?;

        self.indent_level += 1;
        let body_str = body
            .iter()
            .map(|stmt| self.visit_stmt(stmt))
            .collect::<Result<Vec<_>>>()?
            .join("\n");
        self.indent_level -= 1;

        Ok(format!("while ({}) {{\n{}\n}}", condition_str, body_str))
    }

    fn visit_loop(&mut self, body: &[Stmt], _span: &Span) -> Result<String> {
        self.indent_level += 1;
        let body_str = body
            .iter()
            .map(|stmt| self.visit_stmt(stmt))
            .collect::<Result<Vec<_>>>()?
            .join("\n");
        self.indent_level -= 1;

        Ok(format!("while (true) {{\n{}\n}}", body_str))
    }

    fn visit_for_loop(
        &mut self,
        pattern: &Expr,
        iterable: &Expr,
        body: &[Stmt],
        _span: &Span,
    ) -> Result<String> {
        // Convert pattern to JavaScript destructuring
        let pattern_str = match pattern {
            Expr::Identifier(name, _) => name.clone(),
            Expr::Tuple(elements, _) => {
                // Generate tuple destructuring like [a, b]
                let element_names: Result<Vec<_>> = elements
                    .iter()
                    .map(|elem| match elem {
                        Expr::Identifier(name, _) => Ok(name.clone()),
                        _ => Err(Error::new_semantic(
                            "Only identifiers are supported in for loop tuple patterns".to_string(),
                            elem.span(),
                        )),
                    })
                    .collect();
                format!("[{}]", element_names?.join(", "))
            }
            _ => {
                return Err(Error::new_semantic(
                    "Only identifiers and tuples are supported in for loop patterns".to_string(),
                    pattern.span(),
                ));
            }
        };

        match iterable {
            Expr::Range(start, end, inclusive, _) => {
                match (start, end) {
                    (Some(start_expr), Some(end_expr)) => {
                        // For ranges, we can only use simple identifiers, not destructuring
                        let var = match pattern {
                            Expr::Identifier(name, _) => name,
                            _ => {
                                return Err(Error::new_semantic(
                                    "Range-based for loops only support simple identifiers"
                                        .to_string(),
                                    pattern.span(),
                                ));
                            }
                        };

                        let start_str = self.visit_expr(start_expr)?;
                        let end_str = self.visit_expr(end_expr)?;
                        let op = if *inclusive { "<=" } else { "<" };

                        self.indent_level += 1;
                        let body_str = body
                            .iter()
                            .map(|stmt| self.visit_stmt(stmt))
                            .collect::<Result<Vec<_>>>()?
                            .join("\n");
                        self.indent_level -= 1;

                        Ok(format!(
                            "for (let {} = {}; {} {} {}; {}++) {{\n{}\n}}",
                            var, start_str, var, op, end_str, var, body_str
                        ))
                    }
                    _ => Ok("/* Invalid range */".to_string()),
                }
            }
            _ => {
                let iter_str = self.visit_expr(iterable)?;

                self.indent_level += 1;
                let body_str = body
                    .iter()
                    .map(|stmt| self.visit_stmt(stmt))
                    .collect::<Result<Vec<_>>>()?
                    .join("\n");
                self.indent_level -= 1;

                Ok(format!(
                    "for (const {} of {}) {{\n{}\n}}",
                    pattern_str, iter_str, body_str
                ))
            }
        }
    }

    fn visit_break(&mut self, _span: &Span) -> Result<String> {
        Ok("break".to_string())
    }

    fn visit_continue(&mut self, _span: &Span) -> Result<String> {
        Ok("continue".to_string())
    }

    fn visit_return(&mut self, expr: Option<&Expr>, _span: &Span) -> Result<String> {
        match expr {
            Some(return_expr) => {
                let expr_js = self.visit_expr(return_expr)?;
                Ok(format!("return {}", expr_js))
            }
            None => Ok("return".to_string()),
        }
    }

    fn visit_expression_stmt(&mut self, expr: &Expr, has_semicolon: bool) -> Result<String> {
        let expr_js = self.visit_expr(expr)?;

        // If the expression has a semicolon, it's a statement that shouldn't return a value
        // If no semicolon, it can return its value (for block expressions)
        if has_semicolon {
            // Use void operator to ensure no return value in expression context
            Ok(format!("void ({})", expr_js))
        } else {
            Ok(expr_js)
        }
    }

    fn visit_extern_function(
        &mut self,
        _name: &str,
        _generic_params: &[String],
        _params: &[(String, String)],
        _return_type: &str,
        _span: &Span,
    ) -> Result<String> {
        // Extern declarations don't generate any JavaScript code
        // They're only used for type checking
        Ok(String::new())
    }

    fn visit_extern_mod(
        &mut self,
        _name: &str,
        _items: &[ExternItem],
        _span: &Span,
    ) -> Result<String> {
        // Extern declarations don't generate any JavaScript code
        // They're only used for type checking
        Ok(String::new())
    }

    fn visit_extern_type(
        &mut self,
        _name: &str,
        _generic_params: &[String],
        _span: &Span,
    ) -> Result<String> {
        // Extern type declarations don't generate any JavaScript code
        // They're only used for type checking
        Ok(String::new())
    }

    fn visit_async_function(
        &mut self,
        name: &str,
        _generic_params: &[String],
        params: &[(String, String)],
        _return_type: &str,
        body: &[Stmt],
        _span: &Span,
    ) -> Result<String> {
        let params_str = params
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>()
            .join(", ");

        let body_str = self.generate_body(body)?;

        Ok(format!(
            "{}async function {}({}) {{\n{}{}}}",
            self.indent(),
            name,
            params_str,
            body_str,
            self.indent()
        ))
    }

    fn visit_match_expr(
        &mut self,
        expr: &Expr,
        arms: &[(Expr, Vec<Stmt>)],
        _span: &Span,
    ) -> Result<String> {
        let expr_str = self.visit_expr(expr)?;
        let mut result = "((function(_matched) {\n".to_string();
        self.indent_level += 1;

        for (i, (pattern, body)) in arms.iter().enumerate() {
            let (condition, binding) = self.generate_match_condition(expr, pattern);

            if i == 0 {
                result.push_str(&format!("{}if ({}) {{\n", self.indent(), condition));
            } else {
                result.push_str(&format!("{}}} else if ({}) {{\n", self.indent(), condition));
            }

            self.indent_level += 1;
            if !binding.is_empty() {
                result.push_str(&format!("{}{}", self.indent(), binding));
            }

            let body_str = self.generate_body(body)?;
            result.push_str(&body_str);
            result.push('\n');
            self.indent_level -= 1;
        }

        result.push_str(&format!("{}}} else {{\n", self.indent()));
        self.indent_level += 1;
        result.push_str(&format!(
            "{}throw new Error('No pattern matched');\n",
            self.indent()
        ));
        self.indent_level -= 1;
        result.push_str(&format!("{}}}\n", self.indent()));
        self.indent_level -= 1;
        result.push_str(&format!("{}}})({})", self.indent(), expr_str));

        Ok(result)
    }

    fn visit_await(&mut self, expr: &Expr, _span: &Span) -> Result<String> {
        let expr_str = self.visit_expr(expr)?;
        Ok(format!("await {}", expr_str))
    }

    fn visit_try(&mut self, expr: &Expr, _span: &Span) -> Result<String> {
        let expr_str = self.visit_expr(expr)?;
        // Generate JS code that checks if the result is an error and returns early if so
        Ok(format!("(() => {{ const __result = {}; if (__result.type === 'Err') {{ return __result; }} return __result.value; }})()", expr_str))
    }

    fn visit_await_try(&mut self, expr: &Expr, _span: &Span) -> Result<String> {
        let expr_str = self.visit_expr(expr)?;
        // Generate the intelligent await bridge that handles both Promises and Promise<Result>
        Ok(format!("await __husk_await_bridge({})", expr_str))
    }

    fn visit_closure(
        &mut self,
        params: &[(String, Option<String>)],
        _ret_type: &Option<String>,
        body: &Expr,
        _span: &Span,
    ) -> Result<String> {
        // Generate parameter list
        let param_list = params
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>()
            .join(", ");

        // Check if body is a block or single expression
        let body_str = self.visit_expr(body)?;

        // Generate arrow function
        if body_str.starts_with('{') {
            // Block body - no need for return, it's already in the block
            Ok(format!("({}) => {}", param_list, body_str))
        } else {
            // Single expression - implicit return
            Ok(format!("({}) => {}", param_list, body_str))
        }
    }

    fn visit_use(&mut self, path: &UsePath, items: &UseItems, _span: &Span) -> Result<String> {
        match &path.prefix {
            UsePrefix::None => {
                // Check if it's a Node.js built-in module
                let package_name = &path.segments[0];
                if is_nodejs_builtin(package_name) {
                    // Node.js built-in modules don't need package resolution
                    return self.generate_basic_import(path, items);
                }

                // External package - use package resolver if available
                if let Some(ref mut resolver) = self.package_resolver {
                    let subpath_string = if path.segments.len() > 1 {
                        Some(path.segments[1..].join("/"))
                    } else {
                        None
                    };
                    let subpath = subpath_string.as_deref();

                    match resolver.resolve_package(package_name) {
                        Ok(resolved_package) => {
                            // Extract import names
                            let import_names = match items {
                                UseItems::Named(imports) => imports
                                    .iter()
                                    .map(|(name, alias)| alias.as_ref().unwrap_or(name).clone())
                                    .collect(),
                                UseItems::Single => vec![package_name.clone()],
                                UseItems::All => vec!["*".to_string()],
                            };

                            let import_statement = resolver.generate_import_statement(
                                &resolved_package,
                                &import_names,
                                subpath,
                            );
                            Ok(import_statement)
                        }
                        Err(_) => {
                            // Fall back to basic import generation if package resolution fails
                            self.generate_basic_import(path, items)
                        }
                    }
                } else {
                    // No package resolver - fall back to basic import handling
                    self.generate_basic_import(path, items)
                }
            }
            _ => {
                // Local imports - use basic generation
                self.generate_basic_import(path, items)
            }
        }
    }

    fn visit_struct(
        &mut self,
        name: &str,
        _generic_params: &[String],
        fields: &[(String, String)],
        _span: &Span,
    ) -> Result<String> {
        let params = fields
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>()
            .join(", ");

        let mut result = String::new();
        result.push_str(&format!("function {}({}) {{", name, params));

        for (field_name, _) in fields {
            result.push_str(&format!("this.{} = {};", field_name, field_name));
        }

        result.push('}');
        Ok(result)
    }

    fn visit_enum(
        &mut self,
        name: &str,
        _generic_params: &[String],
        variants: &[crate::parser::EnumVariant],
        _span: &Span,
    ) -> Result<String> {
        Ok(self.generate_enum(name, variants))
    }

    fn visit_impl(&mut self, struct_name: &str, methods: &[Stmt], span: &Span) -> Result<String> {
        let mut output = String::new();

        for method in methods {
            let Stmt::Function(_, name, _, params, _return_type, body, _) = method else {
                return Err(Error::new_transpile(
                    "Impl methods must be function definitions",
                    *span,
                ));
            };

            let result = if !params.is_empty() && params[0].0 == "self" {
                // Instance method
                let fn_params = params[1..]
                    .iter()
                    .map(|(name, _)| name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");

                self.indent_level += 1;
                let fn_body = self.generate_body(body)?;
                self.indent_level -= 1;

                let fn_body = format!("const self = this;\n{}", fn_body);
                format!(
                    "{}.prototype.{} = function({}) {{\n{}\n}};\n",
                    struct_name, name, fn_params, fn_body
                )
            } else {
                // Static method
                let fn_params = params
                    .iter()
                    .map(|(name, _)| name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");

                self.indent_level += 1;
                let fn_body = self.generate_body(body)?;
                self.indent_level -= 1;

                format!(
                    "{}.{} = function({}) {{\n{}\n}};\n",
                    struct_name, name, fn_params, fn_body
                )
            };

            output.push_str(&result);
        }

        Ok(output)
    }

    fn visit_match(
        &mut self,
        expr: &Expr,
        arms: &[(Expr, Vec<Stmt>)],
        _span: &Span,
    ) -> Result<String> {
        let expr_str = self.visit_expr(expr)?;

        let cases_str = arms
            .iter()
            .map(|(pattern, stmts)| {
                let (condition, binding) = self.generate_match_condition(expr, pattern);

                self.indent_level += 1;
                // Use generate_body to ensure proper return handling
                let body_str = self.generate_body(stmts)?;
                self.indent_level -= 1;

                Ok(format!(
                    "if ({}) {{\n{}{}\n}}",
                    condition, binding, body_str
                ))
            })
            .collect::<Result<Vec<_>>>()?
            .join(" else ");

        Ok(format!(
            "(() => {{\nconst _matched = {};\n{}\n}})()",
            expr_str, cases_str
        ))
    }

    fn visit_if_expr(
        &mut self,
        condition: &Expr,
        then_block: &[Stmt],
        else_block: &[Stmt],
        _span: &Span,
    ) -> Result<String> {
        let condition_str = self.visit_expr(condition)?;

        // Check if any block contains control flow statements
        let contains_control_flow = |block: &[Stmt]| -> bool {
            block.iter().any(|stmt| {
                matches!(
                    stmt,
                    Stmt::Break(_) | Stmt::Continue(_) | Stmt::Return(_, _)
                )
            })
        };

        let has_control_flow =
            contains_control_flow(then_block) || contains_control_flow(else_block);

        if has_control_flow {
            // Generate as a regular if statement (no IIFE wrapper)
            let mut result = format!("if ({}) {{\n", condition_str);

            self.indent_level += 1;
            for stmt in then_block {
                let stmt_str = self.visit_stmt(stmt)?;
                result.push_str(&format!("{}{}\n", self.indent(), stmt_str));
            }
            self.indent_level -= 1;

            result.push_str(&format!("{}}}", self.indent()));

            if !else_block.is_empty() {
                result.push_str(" else {\n");

                self.indent_level += 1;
                for stmt in else_block {
                    let stmt_str = self.visit_stmt(stmt)?;
                    result.push_str(&format!("{}{}\n", self.indent(), stmt_str));
                }
                self.indent_level -= 1;

                result.push_str(&format!("{}}}", self.indent()));
            }

            Ok(result)
        } else {
            // Generate as an IIFE for the if expression
            let mut result = "(() => {\n".to_string();
            result.push_str(&format!("  if ({}) {{\n", condition_str));

            self.indent_level += 2;
            let then_str = self.generate_body(then_block)?;
            self.indent_level -= 2;

            result.push_str(&format!("    {}\n", then_str));
            result.push_str("  }");

            if !else_block.is_empty() {
                result.push_str(" else {\n");

                self.indent_level += 2;
                let else_str = self.generate_body(else_block)?;
                self.indent_level -= 2;

                result.push_str(&format!("    {}\n", else_str));
                result.push_str("  }");
            }

            result.push_str("\n})()");
            Ok(result)
        }
    }

    fn visit_method_call(
        &mut self,
        object: &Expr,
        method: &str,
        args: &[Expr],
        _span: &Span,
    ) -> Result<String> {
        let obj_str = self.visit_expr(object)?;

        // Evaluate arguments
        let arg_strs: Vec<String> = args
            .iter()
            .map(|arg| self.visit_expr(arg))
            .collect::<Result<Vec<_>>>()?;

        // Note: Since the transpiler doesn't have type information, we check both
        // string and array method registries. This means methods with the same name
        // on different types (like "len") will use whichever is checked first.
        // This is a limitation of the current approach but works because JavaScript
        // uses the same method names for similar operations (e.g., .length for both).

        // For methods that exist on both types, we need to be smarter
        // Check if this looks like a string method based on the arguments
        if method == "find" || method == "position" {
            // If the first argument looks like a string literal, it's probably a string method
            if !args.is_empty() {
                if let Expr::String(_, _) = &args[0] {
                    // It's a string method
                    if let Some(method_impl) = self.method_registry.get_string_method(method) {
                        return Ok(method_impl(&obj_str, &arg_strs));
                    }
                } else {
                    // It's likely an array method (expects a closure/function)
                    if let Some(method_impl) = self.method_registry.get_array_method(method) {
                        return Ok(method_impl(&obj_str, &arg_strs));
                    }
                }
            }
        }

        // Try array methods first (for methods like slice that exist on both types)
        if let Some(method_impl) = self.method_registry.get_array_method(method) {
            return Ok(method_impl(&obj_str, &arg_strs));
        }

        // Try string methods
        if let Some(method_impl) = self.method_registry.get_string_method(method) {
            return Ok(method_impl(&obj_str, &arg_strs));
        }

        // Default: assume direct method mapping for unknown methods
        if arg_strs.is_empty() {
            Ok(format!("{}.{}()", obj_str, method))
        } else {
            Ok(format!("{}.{}({})", obj_str, method, arg_strs.join(", ")))
        }
    }

    fn visit_cast(&mut self, expr: &Expr, target_type: &str, _span: &Span) -> Result<String> {
        let expr_str = self.visit_expr(expr)?;

        // Generate JavaScript type casting/conversion
        match target_type {
            "int" => Ok(format!("Math.floor(Number({}))", expr_str)),
            "float" => Ok(format!("Number({})", expr_str)),
            "string" => Ok(format!("String({})", expr_str)),
            "bool" => Ok(format!("Boolean({})", expr_str)),
            _ => {
                // For custom types, we trust the type system and just return the expression
                // This allows for TypeScript-style type assertions
                Ok(expr_str)
            }
        }
    }

    fn visit_struct_pattern(
        &mut self,
        variant: &str,
        fields: &[(String, Option<String>)],
        _span: &Span,
    ) -> Result<String> {
        // Struct patterns are used for pattern matching and should not generate standalone JavaScript
        // They are only meaningful in the context of match expressions
        // For now, we'll generate a comment indicating this is a pattern
        Ok(format!(
            "/* struct pattern: {} {{ {} }} */",
            variant,
            fields
                .iter()
                .map(|(field, rename)| {
                    match rename {
                        Some(var_name) => format!("{}: {}", field, var_name),
                        None => field.clone(),
                    }
                })
                .collect::<Vec<_>>()
                .join(", ")
        ))
    }

    fn visit_object_literal(&mut self, fields: &[(String, Expr)], _span: &Span) -> Result<String> {
        let mut field_strings = Vec::new();

        for (key, value) in fields {
            let value_str = self.visit_expr(value)?;

            // Check if the key needs to be quoted (if it's not a valid JS identifier)
            let key_str = if key
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c == '$')
                && key
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_alphabetic() || c == '_' || c == '$')
            {
                // Valid identifier, no quotes needed
                key.clone()
            } else {
                // Need to quote the key
                format!("\"{}\"", key.replace('\"', "\\\""))
            };

            field_strings.push(format!("{}: {}", key_str, value_str));
        }

        Ok(format!("{{ {} }}", field_strings.join(", ")))
    }

    fn visit_macro_call(&mut self, name: &str, args: &[Expr], _span: &Span) -> Result<String> {
        match name {
            "print" => {
                // print! macro
                if args.is_empty() {
                    return Ok("process.stdout.write(\"\")".to_string());
                }

                // Evaluate all arguments
                let mut arg_strings = Vec::new();
                for arg in args {
                    arg_strings.push(self.visit_expr(arg)?);
                }

                // If first arg is a string literal, convert to template literal
                if let Expr::String(format_str, _) = &args[0] {
                    if args.len() > 1 {
                        // Has placeholders
                        let template =
                            self.format_to_template_literal(format_str, &arg_strings[1..])?;
                        Ok(format!("process.stdout.write({})", template))
                    } else {
                        // No placeholders, just print the string
                        Ok(format!("process.stdout.write({})", arg_strings[0]))
                    }
                } else {
                    // Not a string literal, use String() to convert
                    Ok(format!("process.stdout.write(String({}))", arg_strings[0]))
                }
            }
            "println" => {
                // println! macro
                if args.is_empty() {
                    return Ok("console.log()".to_string());
                }

                // Evaluate all arguments
                let mut arg_strings = Vec::new();
                for arg in args {
                    arg_strings.push(self.visit_expr(arg)?);
                }

                // If first arg is a string literal, convert to template literal
                if let Expr::String(format_str, _) = &args[0] {
                    if args.len() > 1 {
                        // Has placeholders
                        let template =
                            self.format_to_template_literal(format_str, &arg_strings[1..])?;
                        Ok(format!("console.log({})", template))
                    } else {
                        // No placeholders, just print the string
                        Ok(format!("console.log({})", arg_strings[0]))
                    }
                } else {
                    // Not a string literal, use console.log directly
                    Ok(format!("console.log({})", arg_strings[0]))
                }
            }
            "format" => {
                // format! macro
                if args.is_empty() {
                    return Err(Error::new_transpile(
                        "format! requires at least one argument".to_string(),
                        *_span,
                    ));
                }

                // Evaluate all arguments
                let mut arg_strings = Vec::new();
                for arg in args {
                    arg_strings.push(self.visit_expr(arg)?);
                }

                // First argument must be a string literal
                if let Expr::String(format_str, _) = &args[0] {
                    if args.len() > 1 {
                        // Has placeholders
                        self.format_to_template_literal(format_str, &arg_strings[1..])
                    } else {
                        // No placeholders, return the string as-is
                        Ok(arg_strings[0].clone())
                    }
                } else {
                    // Not a string literal, error
                    Err(Error::new_transpile(
                        "format! first argument must be a string literal".to_string(),
                        *_span,
                    ))
                }
            }
            _ => Err(Error::new_transpile(
                format!("Unknown macro: {}!", name),
                *_span,
            )),
        }
    }
}

impl JsTranspiler {
    /// Convert a format string with {} placeholders to a JavaScript template literal
    fn format_to_template_literal(&self, format_str: &str, args: &[String]) -> Result<String> {
        let mut result = String::from("`");
        let mut arg_index = 0;
        let mut chars = format_str.chars().peekable();

        while let Some(ch) = chars.next() {
            match ch {
                '{' => {
                    if chars.peek() == Some(&'{') {
                        // Escaped {{
                        chars.next();
                        result.push('{');
                    } else if chars.peek() == Some(&'}') {
                        // Placeholder {}
                        chars.next();
                        if arg_index >= args.len() {
                            return Err(Error::new_transpile(
                                format!("Not enough arguments for format string: expected at least {}, got {}",
                                       arg_index + 1, args.len()),
                                Span::default(),
                            ));
                        }
                        result.push_str("${");
                        result.push_str(&args[arg_index]);
                        result.push('}');
                        arg_index += 1;
                    } else {
                        result.push(ch);
                    }
                }
                '}' => {
                    if chars.peek() == Some(&'}') {
                        // Escaped }}
                        chars.next();
                        result.push('}');
                    } else {
                        result.push(ch);
                    }
                }
                '`' => result.push_str("\\`"),
                '$' => result.push_str("\\$"),
                '\\' => result.push_str("\\\\"),
                '\n' => result.push_str("\\n"),
                '\r' => result.push_str("\\r"),
                '\t' => result.push_str("\\t"),
                _ => result.push(ch),
            }
        }

        result.push('`');
        Ok(result)
    }

    /// Generate basic import statement without package resolution
    fn generate_basic_import(&self, path: &UsePath, items: &UseItems) -> Result<String> {
        // Handle special cases for Node.js built-in modules
        let (final_module_path, final_items) = if path.prefix == UsePrefix::None
            && !path.segments.is_empty()
        {
            let base_module = &path.segments[0];

            if is_nodejs_builtin(base_module) && path.segments.len() > 1 {
                match base_module.as_str() {
                    "fs" => {
                        if path.segments.len() >= 2 && path.segments[1] == "promises" {
                            // fs::promises is a valid submodule
                            if path.segments.len() > 2 {
                                // fs::promises::{readFile} - import from fs/promises
                                ("fs/promises".to_string(), items.clone())
                            } else {
                                // fs::promises - import fs/promises as a whole
                                ("fs/promises".to_string(), items.clone())
                            }
                        } else if path.segments.len() == 3 && path.segments[1] == "constants" {
                            // fs::constants::F_OK - import F_OK from fs
                            match items {
                                UseItems::Single => {
                                    let import_name = path.segments[2].clone();
                                    (
                                        "fs".to_string(),
                                        UseItems::Named(vec![(import_name.clone(), None)]),
                                    )
                                }
                                UseItems::Named(_) => {
                                    // Already named imports, use as is but from 'fs'
                                    ("fs".to_string(), items.clone())
                                }
                                _ => ("fs".to_string(), items.clone()),
                            }
                        } else {
                            // Other fs submodules - import from fs directly
                            ("fs".to_string(), items.clone())
                        }
                    }
                    "path" | "crypto" | "util" | "stream" | "os" | "process" => {
                        // These modules export everything directly
                        if path.segments.len() == 2 && matches!(items, UseItems::Single) {
                            // e.g., path::join - import { join } from 'path'
                            let import_name = path.segments[1].clone();
                            (
                                base_module.clone(),
                                UseItems::Named(vec![(import_name, None)]),
                            )
                        } else if path.segments.len() > 2 {
                            // e.g., process::env::NODE_ENV would be invalid, but handle gracefully
                            (base_module.clone(), items.clone())
                        } else {
                            (base_module.clone(), items.clone())
                        }
                    }
                    _ => {
                        // For other modules, use the default behavior
                        let module_segments = path.segments.clone();
                        (module_segments.join("/"), items.clone())
                    }
                }
            } else {
                // Non-builtin or single-segment paths
                let module_segments = path.segments.clone();
                (module_segments.join("/"), items.clone())
            }
        } else {
            // Local imports - handle the path construction
            let (module_segments, import_name) = match (&path.prefix, items) {
                (UsePrefix::Local | UsePrefix::Self_ | UsePrefix::Super(_), UseItems::Single)
                    if path.segments.len() > 1 =>
                {
                    // Split off the last segment as the import name
                    let mut segs = path.segments.clone();
                    let import = segs.pop().unwrap();
                    (segs, Some(import))
                }
                _ => (path.segments.clone(), None),
            };

            let module_path = match &path.prefix {
                UsePrefix::None => {
                    // Should not reach here, but handle it
                    module_segments.join("/")
                }
                UsePrefix::Local => {
                    // From project root - add .js extension for ES modules
                    let base_path = module_segments.join("/");
                    if base_path.ends_with(".js") || base_path.ends_with(".mjs") {
                        format!("./{}", base_path)
                    } else {
                        format!("./{}.js", base_path)
                    }
                }
                UsePrefix::Self_ => {
                    // Current directory - add .js extension for ES modules
                    let base_path = module_segments.join("/");
                    if base_path.ends_with(".js") || base_path.ends_with(".mjs") {
                        format!("./{}", base_path)
                    } else {
                        format!("./{}.js", base_path)
                    }
                }
                UsePrefix::Super(count) => {
                    // Parent directories - add .js extension for ES modules
                    let mut prefix = String::new();
                    for _ in 0..*count {
                        prefix.push_str("../");
                    }
                    let base_path = module_segments.join("/");
                    if base_path.ends_with(".js") || base_path.ends_with(".mjs") {
                        format!("{}{}", prefix, base_path)
                    } else {
                        format!("{}{}.js", prefix, base_path)
                    }
                }
            };

            // For local imports, reconstruct items if we split off the import name
            let final_items = if let Some(name) = import_name {
                UseItems::Named(vec![(name, None)])
            } else {
                items.clone()
            };

            (module_path, final_items)
        };

        // Generate the import statement
        let import_stmt = match &final_items {
            UseItems::All => {
                format!("import * from '{}'", final_module_path)
            }
            UseItems::Single => {
                if path.segments.len() == 1 && path.prefix == UsePrefix::None {
                    // Single external package import
                    format!("import '{}'", final_module_path)
                } else {
                    // This shouldn't happen after our transformations above
                    // But handle it gracefully
                    let module_name = path.segments.last().unwrap_or(&String::new()).clone();
                    format!("import {{ {} }} from '{}'", module_name, final_module_path)
                }
            }
            UseItems::Named(items) => {
                let imports = items
                    .iter()
                    .map(|(name, alias)| {
                        if let Some(alias) = alias {
                            format!("{} as {}", name, alias)
                        } else {
                            name.clone()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("import {{ {} }} from '{}'", imports, final_module_path)
            }
        };

        Ok(import_stmt)
    }
}

/// Check if a module name is a Node.js built-in module
fn is_nodejs_builtin(module: &str) -> bool {
    matches!(
        module,
        "assert"
            | "buffer"
            | "child_process"
            | "cluster"
            | "console"
            | "constants"
            | "crypto"
            | "dgram"
            | "dns"
            | "domain"
            | "events"
            | "fs"
            | "http"
            | "https"
            | "module"
            | "net"
            | "os"
            | "path"
            | "perf_hooks"
            | "process"
            | "punycode"
            | "querystring"
            | "readline"
            | "repl"
            | "stream"
            | "string_decoder"
            | "sys"
            | "timers"
            | "tls"
            | "tty"
            | "url"
            | "util"
            | "v8"
            | "vm"
            | "worker_threads"
            | "zlib"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn transpile_expr(input: &str) -> Result<String> {
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse()?;

        // Extract the first expression from the parsed statements
        if let Some(Stmt::Expression(expr, _)) = stmts.first() {
            let mut transpiler = JsTranspiler::new();
            transpiler.visit_expr(expr)
        } else {
            Err(Error::new_parse(
                "Expected expression".to_string(),
                Span::new(0, 0),
            ))
        }
    }

    fn transpile_stmt(input: &str) -> Result<String> {
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse()?;

        // Get the first statement
        if let Some(stmt) = stmts.first() {
            let mut transpiler = JsTranspiler::new();
            transpiler.visit_stmt(stmt)
        } else {
            Err(Error::new_parse(
                "Expected statement".to_string(),
                Span::new(0, 0),
            ))
        }
    }

    fn transpile_program(input: &str) -> Result<String> {
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let program = parser.parse()?;
        let mut transpiler = JsTranspiler::new();
        transpiler.transpile(&program)
    }

    #[test]
    fn test_transpile_literals() {
        assert_eq!(transpile_expr("42").unwrap(), "42");
        assert_eq!(transpile_expr("3.14").unwrap(), "3.14");
        assert_eq!(transpile_expr("true").unwrap(), "true");
        assert_eq!(transpile_expr("false").unwrap(), "false");
        assert_eq!(transpile_expr("\"hello\"").unwrap(), "\"hello\"");
    }

    #[test]
    fn test_transpile_identifiers() {
        assert_eq!(transpile_expr("x").unwrap(), "x");
        assert_eq!(transpile_expr("myVar").unwrap(), "myVar");
    }

    #[test]
    fn test_transpile_binary_operations() {
        assert_eq!(transpile_expr("1 + 2").unwrap(), "(1 + 2)");
        assert_eq!(transpile_expr("5 - 3").unwrap(), "(5 - 3)");
        assert_eq!(transpile_expr("4 * 6").unwrap(), "(4 * 6)");
        assert_eq!(transpile_expr("10 / 2").unwrap(), "(10 / 2)");
        assert_eq!(transpile_expr("7 % 3").unwrap(), "(7 % 3)");
    }

    #[test]
    fn test_transpile_comparison_operations() {
        assert_eq!(transpile_expr("a == b").unwrap(), "(a === b)");
        assert_eq!(transpile_expr("x < y").unwrap(), "(x < y)");
        assert_eq!(transpile_expr("x <= y").unwrap(), "(x <= y)");
        assert_eq!(transpile_expr("x > y").unwrap(), "(x > y)");
        assert_eq!(transpile_expr("x >= y").unwrap(), "(x >= y)");
    }

    #[test]
    fn test_transpile_array_literal() {
        assert_eq!(transpile_expr("[1, 2, 3]").unwrap(), "[1, 2, 3]");
        assert_eq!(transpile_expr("[\"a\", \"b\"]").unwrap(), "[\"a\", \"b\"]");
        assert_eq!(transpile_expr("[]").unwrap(), "[]");
    }

    #[test]
    fn test_transpile_array_access() {
        assert_eq!(transpile_expr("arr[0]").unwrap(), "arr[0]");
        assert_eq!(transpile_expr("matrix[i]").unwrap(), "matrix[i]");
    }

    #[test]
    fn test_transpile_array_slicing() {
        assert_eq!(transpile_expr("arr[1..3]").unwrap(), "arr.slice(1, 3)");
        assert_eq!(transpile_expr("arr[1..=3]").unwrap(), "arr.slice(1, 3 + 1)");
        assert_eq!(transpile_expr("arr[2..]").unwrap(), "arr.slice(2)");
        assert_eq!(transpile_expr("arr[..5]").unwrap(), "arr.slice(0, 5)");
    }

    #[test]
    fn test_transpile_function_call() {
        assert_eq!(transpile_expr("foo()").unwrap(), "foo()");
        assert_eq!(transpile_expr("add(1, 2)").unwrap(), "add(1, 2)");
        assert_eq!(
            transpile_expr("println!(\"hello\")").unwrap(),
            "console.log(\"hello\")"
        );
    }

    #[test]
    fn test_transpile_member_access() {
        assert_eq!(transpile_expr("obj.field").unwrap(), "obj.field");
        assert_eq!(transpile_expr("person.name").unwrap(), "person.name");
    }

    #[test]
    fn test_transpile_assignment() {
        // Assignments need semicolons when parsed as statements
        assert_eq!(transpile_stmt("x = 5;").unwrap(), "void (x = 5)");
        assert_eq!(
            transpile_stmt("arr[0] = 10;").unwrap(),
            "void (arr[0] = 10)"
        );
        assert_eq!(
            transpile_stmt("obj.field = \"value\";").unwrap(),
            "void (obj.field = \"value\")"
        );
    }

    #[test]
    fn test_transpile_let_statement() {
        assert_eq!(transpile_stmt("let x = 5;").unwrap(), "let x = 5");
        assert_eq!(
            transpile_stmt("let name = \"Alice\";").unwrap(),
            "let name = \"Alice\""
        );
    }

    #[test]
    fn test_transpile_function_definition() {
        let input = "fn add(a: int, b: int) -> int { a + b }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("function add(a, b)"));
        assert!(result.contains("return (a + b);"));
    }

    #[test]
    fn test_transpile_while_loop() {
        let input = "while x < 10 { x = x + 1; }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("while ((x < 10))"));
        assert!(result.contains("x = (x + 1)"));
    }

    #[test]
    fn test_transpile_for_loop_range() {
        let input = "for i in 0..5 { println!(i); }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("for (let i = 0; i < 5; i++)"));
        assert!(result.contains("console.log(i)"));
    }

    #[test]
    fn test_transpile_for_loop_array() {
        let input = "for x in arr { println!(x); }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("for (const x of arr)"));
        assert!(result.contains("console.log(x)"));
    }

    #[test]
    fn test_transpile_infinite_loop() {
        let input = "loop { break; }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("while (true)"));
        assert!(result.contains("break"));
    }

    #[test]
    fn test_transpile_break_continue() {
        assert_eq!(transpile_stmt("break;").unwrap(), "break");
        assert_eq!(transpile_stmt("continue;").unwrap(), "continue");
    }

    #[test]
    fn test_transpile_return_statement() {
        assert_eq!(transpile_stmt("return;").unwrap(), "return");
        assert_eq!(transpile_stmt("return 42;").unwrap(), "return 42");
        assert_eq!(transpile_stmt("return x + y;").unwrap(), "return (x + y)");
    }

    #[test]
    fn test_transpile_struct_definition() {
        let input = "struct Point { x: int, y: int }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("function Point(x, y)"));
        assert!(result.contains("this.x = x;"));
        assert!(result.contains("this.y = y;"));
    }

    #[test]
    fn test_transpile_struct_instantiation() {
        let input = "Point { x: 10, y: 20 }";
        let result = transpile_expr(input).unwrap();
        assert!(result.contains("Object.create(Point.prototype)"));
        assert!(result.contains("__INSTANCE__.x = 10;"));
        assert!(result.contains("__INSTANCE__.y = 20;"));
    }

    #[test]
    fn test_transpile_enum_definition() {
        let input = "enum Option { Some(int), None }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("class Option"));
        assert!(result.contains("Option.Some = class extends Option"));
        assert!(result.contains("Option.None = class extends Option"));
        assert!(result.contains("Option.None = new Option.None()"));
    }

    #[test]
    fn test_transpile_enum_variant() {
        assert_eq!(
            transpile_expr("Option::None").unwrap(),
            "{ type: 'None', value: null }"
        );
        assert_eq!(
            transpile_expr("Option::Some(5)").unwrap(),
            "{ type: 'Some', value: 5 }"
        );
    }

    #[test]
    fn test_transpile_impl_block() {
        let input = r#"
            impl Rectangle {
                fn area(self) -> int {
                    self.width * self.height
                }
                
                fn new(w: int, h: int) -> Rectangle {
                    Rectangle { width: w, height: h }
                }
            }
        "#;
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        let mut transpiler = JsTranspiler::new();

        // Find the impl statement
        for stmt in &program {
            if let Stmt::Impl(_, _, _) = stmt {
                let result = transpiler.visit_stmt(stmt).unwrap();
                assert!(result.contains("Rectangle.prototype.area = function()"));
                assert!(result.contains("const self = this;"));
                assert!(result.contains("Rectangle.new = function(w, h)"));
                break;
            }
        }
    }

    #[test]
    fn test_transpile_match_expression() {
        // Match needs to be in a function context for proper parsing
        let input = r#"
            fn test_match() -> int {
                match opt {
                    Option::Some(n) => n,
                    Option::None => 0,
                }
            }
        "#;
        let result = transpile_program(input).unwrap();
        assert!(result.contains("const _matched = opt;"));
        assert!(result.contains("if ((_matched && _matched.type === 'Some'))"));
        assert!(result.contains("const n = _matched.value;"));
        assert!(result.contains("return n;"));
        assert!(result.contains("else if ((_matched && _matched.type === 'None'))"));
        assert!(result.contains("return 0;"));
    }

    #[test]
    fn test_transpile_if_expression() {
        let input = "if x > 0 { x } else { 0 }";
        let result = transpile_expr(input).unwrap();
        assert!(result.contains("(() => {"));
        assert!(result.contains("if ((x > 0))"));
        assert!(result.contains("return x;"));
        assert!(result.contains("return 0;"));
        assert!(result.contains("})()"));
    }

    #[test]
    fn test_transpile_block_expression() {
        // Block expressions need to be parsed as complete programs
        let input = "fn test() -> int { let x = 5; x + 1 }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("let x = 5;"));
        assert!(result.contains("return (x + 1);"));
    }

    #[test]
    fn test_transpile_expression_with_semicolon() {
        let input = "x = 5;";
        let result = transpile_stmt(input).unwrap();
        assert_eq!(result, "void (x = 5)");
    }

    #[test]
    fn test_transpile_parentheses() {
        assert_eq!(transpile_expr("(1 + 2) * 3").unwrap(), "((1 + 2) * 3)");
        assert_eq!(transpile_expr("(x)").unwrap(), "x");
    }

    #[test]
    fn test_transpile_complete_program() {
        let input = r#"
            fn main() -> void {
                let x = 5;
                if x > 0 {
                    println!("positive");
                } else {
                    println!("non-positive");
                }
            }
        "#;
        let result = transpile_program(input).unwrap();
        assert!(result.contains("function main()"));
        assert!(result.contains("let x = 5;"));
    }

    #[test]
    fn test_transpile_nested_expressions() {
        let input = "arr[if cond { 0 } else { 1 }]";
        let result = transpile_expr(input).unwrap();
        assert!(result.contains("arr[(() => {"));
        assert!(result.contains("if (cond)"));
    }

    #[test]
    fn test_transpile_method_call() {
        // Test static method call
        assert_eq!(
            transpile_expr("Point::new(1, 2)").unwrap(),
            "Point.new(1, 2)"
        );

        // Test method call on dot notation (for future implementation)
        let input = "obj.method(arg)";
        let result = transpile_expr(input).unwrap();
        assert_eq!(result, "obj.method(arg)");
    }

    #[test]
    fn test_transpile_match_with_wildcard() {
        let input = r#"
            fn test_wildcard() -> string {
                match x {
                    1 => "one",
                    2 => "two",
                    _ => "other",
                }
            }
        "#;
        let result = transpile_program(input).unwrap();
        assert!(result.contains("const _matched = x;"));
        // The wildcard pattern should generate a simple "if (true)" condition without binding
        assert!(result.contains("} else if (true) {\nreturn \"other\";"));
    }

    #[test]
    fn test_transpile_for_inclusive_range() {
        let input = "for i in 1..=5 { println!(i); }";
        let result = transpile_stmt(input).unwrap();
        assert!(result.contains("for (let i = 1; i <= 5; i++)"));
    }

    #[test]
    fn test_transpile_compound_assignment() {
        // Compound assignment needs to be parsed as a statement
        assert_eq!(transpile_stmt("x += 1;").unwrap(), "void (x += 1)");
        assert_eq!(transpile_stmt("x -= 1;").unwrap(), "void (x -= 1)");
        assert_eq!(transpile_stmt("x *= 2;").unwrap(), "void (x *= 2)");
        assert_eq!(transpile_stmt("x /= 2;").unwrap(), "void (x /= 2)");
        assert_eq!(transpile_stmt("x %= 3;").unwrap(), "void (x %= 3)");
    }

    #[test]
    fn test_transpile_function_with_return() {
        let input = r#"
            fn factorial(n: int) -> int {
                if n <= 1 {
                    return 1;
                }
                return n * factorial(n - 1);
            }
        "#;
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        let mut transpiler = JsTranspiler::new();

        for stmt in &program {
            if let Stmt::Function(_, _, _, _, _, _, _) = stmt {
                let result = transpiler.visit_stmt(stmt).unwrap();
                assert!(result.contains("function factorial(n)"));
                assert!(result.contains("return 1"));
                assert!(result.contains("return (n * factorial((n - 1)))"));
                break;
            }
        }
    }

    #[test]
    fn test_transpile_unary_negation() {
        assert_eq!(transpile_expr("-5").unwrap(), "(-5)");
        assert_eq!(transpile_expr("-3.14").unwrap(), "(-3.14)");
        assert_eq!(transpile_expr("-x").unwrap(), "(-x)");
    }

    #[test]
    fn test_transpile_unary_not() {
        assert_eq!(transpile_expr("!true").unwrap(), "(!true)");
        assert_eq!(transpile_expr("!false").unwrap(), "(!false)");
        assert_eq!(transpile_expr("!flag").unwrap(), "(!flag)");
    }

    #[test]
    fn test_transpile_unary_double() {
        assert_eq!(transpile_expr("--5").unwrap(), "(-(-5))");
        assert_eq!(transpile_expr("!!true").unwrap(), "(!(!true))");
    }

    #[test]
    fn test_transpile_unary_in_expressions() {
        assert_eq!(transpile_expr("5 + -3").unwrap(), "(5 + (-3))");
        assert_eq!(transpile_expr("-5 * 2").unwrap(), "((-5) * 2)");
        assert_eq!(
            transpile_expr("!true == false").unwrap(),
            "((!true) === false)"
        );
    }

    #[test]
    fn test_transpile_unary_with_parentheses() {
        assert_eq!(transpile_expr("-(5 + 3)").unwrap(), "(-(5 + 3))");
        assert_eq!(transpile_expr("!(x > 5)").unwrap(), "(!(x > 5))");
    }

    #[test]
    fn test_transpile_local_imports() {
        // Test that local imports get .js extension for ES modules
        // Single segment path does side-effect import
        let mut lexer = Lexer::new("use local::utils;".to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse().unwrap();
        let mut transpiler = JsTranspiler::new();

        if let Stmt::Use(_, _, _) = &stmts[0] {
            let result = transpiler.visit_stmt(&stmts[0]).unwrap();
            assert_eq!(result, "import { utils } from './utils.js'");
        }

        // Test self:: imports
        let mut lexer = Lexer::new("use self::components;".to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse().unwrap();
        let mut transpiler = JsTranspiler::new();

        if let Stmt::Use(_, _, _) = &stmts[0] {
            let result = transpiler.visit_stmt(&stmts[0]).unwrap();
            assert_eq!(result, "import { components } from './components.js'");
        }

        // Test super:: imports
        let mut lexer = Lexer::new("use super::shared;".to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse().unwrap();
        let mut transpiler = JsTranspiler::new();

        if let Stmt::Use(_, _, _) = &stmts[0] {
            let result = transpiler.visit_stmt(&stmts[0]).unwrap();
            assert_eq!(result, "import { shared } from '../shared.js'");
        }

        // Test multi-segment path with named import
        let mut lexer = Lexer::new("use local::modules::auth;".to_string());
        let tokens = lexer.lex_all();
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse().unwrap();
        let mut transpiler = JsTranspiler::new();

        if let Stmt::Use(_, _, _) = &stmts[0] {
            let result = transpiler.visit_stmt(&stmts[0]).unwrap();
            assert_eq!(result, "import { auth } from './modules.js'");
        }
    }
}
