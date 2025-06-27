use husk::{Lexer, Parser, SemanticVisitor};
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Read the test file
    let code = fs::read_to_string("examples/test-discovery.husk")?;

    // Lex and parse
    let mut lexer = Lexer::new(code.clone());
    let tokens = lexer.lex_all();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // Run semantic analysis with test discovery
    let mut analyzer = SemanticVisitor::new();
    analyzer.analyze(&ast)?;

    // Get the test registry
    let test_registry = analyzer.get_test_registry();

    // Print discovered tests
    println!("Test Discovery Results:");
    println!("======================");

    let stats = test_registry.test_stats();
    println!("{}", stats.summary());
    println!();

    for test in test_registry.all_tests() {
        println!("Test: {}", test.qualified_name());
        println!(
            "  - Location: line {}, col {}",
            test.span.line_number(&code),
            test.span.column_number(&code)
        );
        println!("  - Ignored: {}", test.ignore);
        println!("  - Should panic: {}", test.should_panic);
        if let Some(msg) = &test.panic_message {
            println!("  - Panic message: {}", msg);
        }
        println!();
    }

    // Test filtering
    println!("Filtering tests matching 'ignored':");
    for test in test_registry.filter_tests("ignored") {
        println!("  - {}", test.qualified_name());
    }

    Ok(())
}
