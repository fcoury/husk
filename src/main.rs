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
    repl::repl();
    if true {
        return Ok(());
    }
    let code = r#"
        struct Person {
            name: string,
            age: int,
        }

        let person = Person { name: "Felipe", age: 44 };
        println(person.name);
        println(person.age);
        println(person);

        person.age = 22;
        println(person.age);
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
