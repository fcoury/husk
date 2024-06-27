use lexer::Lexer;
use parser::Parser;

mod lexer;
mod parser;
mod semantic;
mod span;

fn main() {
    let code = r#"
        fn add(x, y) {
            x + y;
        }

        let five = 5; 
        let x = 10;
        let y = 20;
        let result = x + y + five + 3;

        add(x, result);
    "#;
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let prog = parser.parse();

    let mut analyzer = semantic::SemanticAnalyzer::new();
    let result = analyzer.analyze(&prog);

    if result.is_ok() {
        println!("Semantic analysis passed!");
    } else {
        let error = result.err().unwrap();
        println!("{}", error.pretty_print(code));
    }
}
