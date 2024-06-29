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
            println("x: ", x);
            println("y: ", y);
            let res = x + y;
            println("res: ", res);
            res
        }

        fn p(s: string) {
            println(s);
        }

        let name = "Felipe";

        let five = 5; 
        let x = 10;
        let y = 20;
        let y = 1;
        let result = x + y + five + 3;
        println(result);

        let final = add(x, result);
        p(name);
        println("Final: ", final);
    "#;
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(error) => {
            println!("{}", error.pretty_print(code));
            return Ok(());
        }
    };

    let mut analyzer = semantic::SemanticAnalyzer::new();
    let result = analyzer.analyze(&ast);

    if let Err(error) = result {
        println!("{}", error.pretty_print(code));
        return Ok(());
    }

    let mut interpreter = Interpreter::new();
    match interpreter.interpret(&ast) {
        Ok(_) => {}
        Err(error) => {
            println!("{}", error.pretty_print(code));
        }
    }

    Ok(())
}
