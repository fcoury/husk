mod ast;
mod error;
mod error_tests;
mod integration_error_tests;
mod interpreter;
mod lexer;
mod parser;
mod parser_error_tests;
mod repl;
mod runtime_error_tests;
mod semantic;
mod semantic_error_tests;
mod span;
mod transpiler;
mod transpiler_error_tests;
mod types;

#[cfg(test)]
mod interpreter_modules_test;

#[cfg(test)]
mod extern_test;

#[cfg(test)]
mod async_test;

#[cfg(test)]
mod promise_type_test;

#[cfg(test)]
mod test_generic_extern;

#[cfg(test)]
mod generic_extern_comprehensive_test;

#[cfg(test)]
mod closure_test;

#[cfg(test)]
mod format_test;

#[cfg(test)]
mod option_result_test;

#[cfg(test)]
mod try_operator_test;

pub use error::{Error, Result};
pub use interpreter::{InterpreterVisitor, Value};
pub use lexer::Lexer;
pub use parser::Parser;
pub use repl::repl;
pub use semantic::SemanticVisitor;
use transpiler::JsTranspiler;

pub fn execute_script(code: impl Into<String>) -> Result<Value> {
    let code = code.into();
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // Use visitor pattern for semantic analysis
    let mut analyzer = SemanticVisitor::new();
    analyzer.analyze(&ast)?;

    // Use visitor pattern for interpretation
    let mut interpreter = InterpreterVisitor::new();
    interpreter.interpret(&ast)
}

pub fn execute_script_with_context(
    code: impl Into<String>,
    current_file: Option<std::path::PathBuf>,
    project_root: Option<std::path::PathBuf>,
) -> Result<Value> {
    let code = code.into();
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // Use visitor pattern for semantic analysis
    let mut analyzer = SemanticVisitor::new();
    analyzer.analyze(&ast)?;

    // Use visitor pattern for interpretation with context
    let mut interpreter = InterpreterVisitor::with_context(current_file, project_root);
    interpreter.interpret(&ast)
}

pub fn transpile_to_js(code: impl Into<String>) -> Result<String> {
    let code = code.into();
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // Use visitor pattern for semantic analysis
    let mut analyzer = SemanticVisitor::new();
    analyzer.analyze(&ast)?;

    let mut js_generator = JsTranspiler::new();
    Ok(js_generator.generate(&ast)?)
}
