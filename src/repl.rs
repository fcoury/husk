use std::io::{self, Write};

use crate::{interpreter::Interpreter, lexer::Lexer, parser::Parser};

#[allow(dead_code)]
pub fn repl() -> io::Result<()> {
    let mut interpreter = Interpreter::new();

    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        let input = input.trim();
        if input == "exit" {
            break;
        }

        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        match parser.parse() {
            Ok(ast) => match interpreter.interpret(&ast) {
                Ok(val) => println!("{}", val),
                Err(e) => println!("{}", e.pretty_print(input)),
            },
            Err(e) => println!("{}", e.pretty_print(input)),
        }
    }

    Ok(())
}
