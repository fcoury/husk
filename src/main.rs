use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

mod error;
mod interpreter;
mod lexer;
mod parser;
mod repl;
mod semantic;
mod span;

fn main() -> anyhow::Result<()> {
    let code = r#"
        fn add(x: int, y: int) -> int {
            x + y;
        }

        let five = 5; 
        println(five);
        let x = 10;
        println(x);
        let y = 20;
        println(y);
        let result = x + y + five + 3;
        println(result);

        let final = add(x, result);
        println(final);
    "#;
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    let mut analyzer = semantic::SemanticAnalyzer::new();
    let result = analyzer.analyze(&ast);

    if let Err(error) = result {
        println!("{}", error.pretty_print(code));
        return Ok(());
    }

    let mut interpreter = Interpreter::new();
    interpreter.interpret(&ast)?;

    Ok(())
}
