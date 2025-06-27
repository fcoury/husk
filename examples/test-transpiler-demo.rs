use husk_lang::test_transpiler::{TestRunner, TestTranspileConfig, TestTranspiler};
use husk_lang::{Lexer, Parser, SemanticVisitor};
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

    println!("Discovered {} tests:", test_registry.all_tests().len());
    for test in test_registry.all_tests() {
        println!("  - {}", test.qualified_name());
    }
    println!();

    // Create test transpiler
    let mut transpiler = TestTranspiler::new();

    // Test different configurations
    let configs = vec![
        (
            "standalone",
            TestTranspileConfig {
                runner: TestRunner::Standalone,
                include_timing: true,
                ..Default::default()
            },
        ),
        (
            "node",
            TestTranspileConfig {
                runner: TestRunner::Node,
                node_compat: true,
                ..Default::default()
            },
        ),
        (
            "jest",
            TestTranspileConfig {
                runner: TestRunner::Jest,
                node_compat: true,
                ..Default::default()
            },
        ),
    ];

    for (name, config) in configs {
        println!("=== {} Test Harness ===", name.to_uppercase());

        // Generate JavaScript test harness
        let js_code = transpiler.generate_test_harness(&ast, test_registry, &config)?;

        // Write to file
        let filename = format!("examples/test-{}.js", name);
        fs::write(&filename, &js_code)?;

        println!("Generated test harness: {}", filename);
        println!("Preview (first 500 chars):");
        println!("{}", &js_code.chars().take(500).collect::<String>());
        if js_code.len() > 500 {
            println!("... (truncated)");
        }
        println!();
    }

    Ok(())
}
