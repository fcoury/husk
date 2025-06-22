use rustyline::{error::ReadlineError, history::FileHistory, CompletionType, Config, Editor};
use simple_home_dir::home_dir;

use crate::{interpreter::InterpreterVisitor, lexer::Lexer, parser::Parser};

#[allow(dead_code)]
pub fn repl() -> anyhow::Result<()> {
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .build();
    let history_file = format!("{}/.husk_history", home_dir().unwrap().to_str().unwrap());
    let mut rl: Editor<(), FileHistory> = Editor::with_config(config)?;
    let mut interpreter = InterpreterVisitor::new();
    rl.load_history(&history_file).unwrap_or_default();

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                if line == "exit" {
                    break;
                }

                if line == "clear" {
                    // clear screen
                    println!("\x1B[2J\x1B[1;1H");
                    continue;
                }

                rl.add_history_entry(line.as_str())?;
                let mut lexer = Lexer::new(&line);
                let tokens = lexer.lex_all();

                let mut parser = Parser::new(tokens);
                match parser.parse() {
                    Ok(ast) => match interpreter.interpret(&ast) {
                        Ok(val) => println!("{}", val),
                        Err(e) => println!("{}", e.pretty_print(line)),
                    },
                    Err(e) => println!("{}", e.pretty_print(line)),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
        rl.append_history(&history_file).unwrap();
    }

    Ok(())
}
