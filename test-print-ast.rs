use husk::lexer::Lexer;
use husk::parser::Parser;

fn main() {
    let code = r#"
enum Command {
    Process { input: string },
}

fn main() {
    let cmd = 42;
    match cmd {
        Command::Process { input } => {
            println("process");
        },
        _ => println("other")
    }
}
"#;

    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens);
    
    match parser.parse() {
        Ok(ast) => {
            println!("Parse successful!");
            println!("AST: {:#?}", ast);
        }
        Err(e) => {
            println!("Parse error: {}", e);
        }
    }
}