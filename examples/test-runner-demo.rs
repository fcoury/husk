use husk::test_runner::{TestConfig, TestRunner};
use husk::{Lexer, Parser, SemanticVisitor};
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Read the test file
    let code = fs::read_to_string("examples/test-valid.husk")?;

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

    // Create test runner with default config
    let config = TestConfig {
        show_timing: true,
        show_output: true, // Show output from all tests
        ..Default::default()
    };

    let runner = TestRunner::new(ast, config);

    // Run the tests
    let results = runner.run_tests(test_registry);

    // Exit with appropriate code (handled by runner)
    Ok(())
}
