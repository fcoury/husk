mod ast;
mod error;
mod interpreter;
mod interpreter_visitor;
mod lexer;
mod parser;
mod repl;
mod semantic;
mod semantic_visitor;
mod span;
mod transpiler;
mod types;

pub use error::{Error, Result};
pub use interpreter::{Interpreter, Value};
pub use lexer::Lexer;
pub use parser::Parser;
pub use repl::repl;
pub use semantic::SemanticAnalyzer;
use transpiler::JsTranspiler;

pub fn execute_script(code: impl Into<String>) -> Result<Value> {
    let code = code.into();
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&ast)?;

    let mut interpreter = Interpreter::new();
    interpreter.interpret(&ast)
}

pub fn transpile_to_js(code: impl Into<String>) -> Result<String> {
    let code = code.into();
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&ast)?;

    let js_generator = JsTranspiler::new();
    Ok(js_generator.generate(&ast)?)
}
