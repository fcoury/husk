use std::path::{Path, PathBuf};

use clap::{Parser, Subcommand};
use husk_lang::repl;
use serde_json;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Optional subcommand
    #[clap(subcommand)]
    cmd: Option<Command>,

    /// Run a hash script
    file: Option<std::path::PathBuf>,

    /// Disable colored output
    #[clap(long, global = true)]
    no_color: bool,
}

#[derive(Parser, Debug)]
struct Run {
    /// Run a Husk script
    file: PathBuf,
}

#[derive(Parser, Debug)]
struct Build {
    /// Target platform (node-esm, node-cjs, browser)
    #[clap(long)]
    target: Option<String>,

    /// Watch for changes and rebuild
    #[clap(long)]
    watch: bool,

    /// Skip package.json generation
    #[clap(long)]
    skip_package_json: bool,
}

#[derive(Parser, Debug)]
struct New {
    /// The name of the project to create
    name: String,
}

#[derive(Parser, Debug)]
struct Test {
    /// Test file or directory to run (defaults to current directory)
    path: Option<PathBuf>,

    /// Filter tests by name pattern
    #[clap(long)]
    filter: Option<String>,

    /// Run ignored tests
    #[clap(long)]
    include_ignored: bool,

    /// Stop on first failure
    #[clap(long)]
    fail_fast: bool,

    /// Show output from passing tests
    #[clap(long)]
    show_output: bool,

    /// Show timing information
    #[clap(long)]
    show_timing: bool,

    /// Use transpiler mode (generate JavaScript)
    #[clap(long)]
    transpile: bool,

    /// JavaScript test runner (standalone, node, jest, mocha)
    #[clap(long, default_value = "standalone")]
    js_runner: String,

    /// Number of test threads (interpreter mode only)
    #[clap(long, default_value = "1")]
    test_threads: usize,

    /// Disable colored output
    #[clap(long)]
    no_color: bool,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Start the Husk REPL
    Repl,

    /// Run a Husk script
    Run(Run),

    /// Transpile a Husk script to JavaScript
    Compile(Compile),

    /// Build the entire project from husk.toml
    Build(Build),

    /// Create a new Husk project
    New(New),

    /// Run tests
    Test(Test),
}

#[derive(Parser, Debug)]
struct Compile {
    /// The Husk script to transpile
    file: std::path::PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let no_color = cli.no_color;

    if let Some(file) = cli.file {
        return run_command(file, no_color);
    }

    match cli.cmd {
        Some(Command::Run(run)) => run_command(run.file, no_color)?,
        Some(Command::Compile(compile)) => compile_command(compile, no_color)?,
        Some(Command::Build(build)) => build_command(build, no_color)?,
        Some(Command::New(new)) => new_command(new)?,
        Some(Command::Test(test)) => test_command(test, no_color)?,
        Some(Command::Repl) | None => repl()?,
    };
    Ok(())
}

fn run_command(file: PathBuf, no_color: bool) -> anyhow::Result<()> {
    let code = std::fs::read_to_string(&file)?;

    // Determine project root (for now, use parent of src directory if it exists)
    let project_root = file
        .parent()
        .and_then(|p| p.file_name())
        .and_then(|name| {
            if name == "src" {
                file.parent()?.parent()
            } else {
                None
            }
        })
        .map(|p| p.to_path_buf());

    match husk_lang::execute_script_with_context(&code, Some(file), project_root) {
        Ok(_) => {}
        Err(e) => {
            let mut stderr = Vec::new();
            e.write_colored(&mut stderr, &code, no_color).unwrap();
            std::process::exit(1);
        }
    }

    Ok(())
}

fn build_command(cli: Build, _no_color: bool) -> anyhow::Result<()> {
    use husk_lang::HuskConfig;
    use std::fs;

    // Load husk.toml configuration
    let (config, project_root) = match HuskConfig::find_and_load() {
        Ok((config, root)) => (config, root),
        Err(e) => {
            eprintln!("Error: Could not find husk.toml: {e}");
            eprintln!("Run 'husk init' to create a new project or ensure you're in a Husk project directory.");
            std::process::exit(1);
        }
    };

    println!("Building project: {}", config.package.name);

    // Generate package.json by default (unless skipped)
    if !cli.skip_package_json {
        let package_json_path = project_root.join("package.json");
        let new_package_json = config.generate_package_json()?;

        // Check if package.json exists and if it needs updating
        let should_generate = if package_json_path.exists() {
            match fs::read_to_string(&package_json_path) {
                Ok(existing) => {
                    // Compare dependencies to see if regeneration is needed
                    match (
                        serde_json::from_str::<serde_json::Value>(&existing),
                        serde_json::from_str::<serde_json::Value>(&new_package_json),
                    ) {
                        (Ok(existing_json), Ok(new_json)) => {
                            // Only regenerate if dependencies changed
                            existing_json.get("dependencies") != new_json.get("dependencies")
                                || existing_json.get("devDependencies")
                                    != new_json.get("devDependencies")
                        }
                        _ => true, // Regenerate if we can't parse either
                    }
                }
                Err(_) => true, // Regenerate if we can't read the file
            }
        } else {
            true // No existing package.json
        };

        if should_generate {
            fs::write(&package_json_path, &new_package_json)?;
            println!("✓ Generated package.json");

            // Remind user to run npm install if needed
            if !project_root.join("node_modules").exists() {
                println!("  → Run 'npm install' to install dependencies");
            }
        }
    }

    // Determine target
    let target = cli
        .target
        .as_deref()
        .or(if config.build.target.is_empty() {
            None
        } else {
            Some(config.build.target.as_str())
        })
        .unwrap_or("node-esm");

    println!("Target: {target}");

    // Create output directory
    let src_dir = project_root.join(&config.build.src);
    let out_dir = project_root.join(&config.build.out);

    if !src_dir.exists() {
        eprintln!("Error: Source directory '{}' not found", config.build.src);
        std::process::exit(1);
    }

    fs::create_dir_all(&out_dir)?;

    // Find all .husk files in src directory
    let husk_files = find_husk_files(&src_dir)?;

    if husk_files.is_empty() {
        println!("No .husk files found in {}", config.build.src);
        return Ok(());
    }

    println!("Found {} Husk files to compile", husk_files.len());

    // Compile each file
    for husk_file in husk_files {
        let relative_path = husk_file.strip_prefix(&src_dir)?;
        let js_file = out_dir.join(relative_path).with_extension("js");

        // Create parent directory if needed
        if let Some(parent) = js_file.parent() {
            fs::create_dir_all(parent)?;
        }

        let code = fs::read_to_string(&husk_file)?;

        match husk_lang::transpile_to_js_with_target(&code, target) {
            Ok(js) => {
                fs::write(&js_file, js)?;
                println!(
                    "  {} -> {}",
                    husk_file.strip_prefix(&project_root)?.display(),
                    js_file.strip_prefix(&project_root)?.display()
                );
            }
            Err(e) => {
                eprintln!("Error compiling {}: {}", husk_file.display(), e);
                std::process::exit(1);
            }
        }
    }

    println!("Build completed successfully!");

    if cli.watch {
        println!("Watch mode not implemented yet - use a file watcher tool like 'watchexec'");
        println!("Example: watchexec -e husk 'husk build'");
    }

    Ok(())
}

fn find_husk_files(dir: &std::path::Path) -> anyhow::Result<Vec<std::path::PathBuf>> {
    let mut husk_files = Vec::new();

    if dir.is_dir() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                husk_files.extend(find_husk_files(&path)?);
            } else if let Some(ext) = path.extension() {
                if ext == "husk" || ext == "hk" {
                    husk_files.push(path);
                }
            }
        }
    }

    Ok(husk_files)
}

fn compile_command(cli: Compile, no_color: bool) -> anyhow::Result<()> {
    let code = std::fs::read_to_string(cli.file)?;
    match husk_lang::transpile_to_js_with_packages(&code) {
        Ok(js) => println!("{js}"),
        Err(e) => {
            let mut stderr = Vec::new();
            e.write_colored(&mut stderr, &code, no_color).unwrap();
            std::process::exit(1);
        }
    }

    Ok(())
}

fn new_command(cli: New) -> anyhow::Result<()> {
    use std::fs;
    use std::path::Path;

    let project_name = &cli.name;
    let project_path = Path::new(project_name);

    // Check if directory already exists
    if project_path.exists() {
        anyhow::bail!("Directory '{}' already exists", project_name);
    }

    // Create project directory
    fs::create_dir_all(project_path)?;

    // Create src directory
    let src_path = project_path.join("src");
    fs::create_dir_all(&src_path)?;

    // Create husk.toml
    let husk_toml_content = format!(
        r#"[package]
name = "{project_name}"
version = "0.1.0"
description = "A new Husk project"
author = ""
license = "MIT"

[dependencies]

[dev-dependencies]

[build]
src = "src"
out = "dist"
target = ""
module = "esm"
"#
    );

    fs::write(project_path.join("husk.toml"), husk_toml_content)?;

    // Create main.husk
    let main_husk_content = r#"fn main() {
    println!("Hello from Husk!");
}
"#;

    fs::write(src_path.join("main.husk"), main_husk_content)?;

    // Create .gitignore
    let gitignore_content = r#"/dist/
/node_modules/
*.log
.DS_Store
"#;

    fs::write(project_path.join(".gitignore"), gitignore_content)?;

    println!("Created new Husk project '{project_name}'");
    println!();
    println!("To get started:");
    println!("  cd {project_name}");
    println!("  husk build");
    println!("  husk run src/main.husk");

    Ok(())
}

fn test_command(cli: Test, no_color: bool) -> anyhow::Result<()> {
    use husk_lang::test_runner::{TestConfig, TestRunner};
    use husk_lang::{Lexer, Parser, SemanticVisitor};
    use std::fs;

    // Determine the path to search for tests
    let test_path = cli.path.clone().unwrap_or_else(|| PathBuf::from("."));

    // Collect all .husk files to process
    let mut test_files = Vec::new();
    collect_husk_files(&test_path, &mut test_files)?;

    if test_files.is_empty() {
        println!("No test files found in {test_path:?}");
        return Ok(());
    }

    println!(
        "Discovered {} test file{}",
        test_files.len(),
        if test_files.len() == 1 { "" } else { "s" }
    );

    let mut all_test_results = Vec::new();
    let mut total_tests = 0;

    let num_files = test_files.len();
    for file_path in &test_files {
        // Read and parse the file
        let code = fs::read_to_string(file_path)?;

        let mut lexer = Lexer::new(code.clone());
        let tokens = lexer.lex_all();

        let mut parser = Parser::new(tokens);
        let ast = match parser.parse() {
            Ok(ast) => ast,
            Err(e) => {
                eprint!("Parse error in {file_path:?}: ");
                let mut stderr = Vec::new();
                e.write_colored(&mut stderr, &code, no_color).unwrap();
                continue;
            }
        };

        // Run semantic analysis with test discovery
        let mut analyzer = SemanticVisitor::new();
        if let Err(e) = analyzer.analyze(&ast) {
            eprint!("Semantic error in {file_path:?}: ");
            let mut stderr = Vec::new();
            e.write_colored(&mut stderr, &code, no_color).unwrap();
            continue;
        }

        // Get the test registry
        let test_registry = analyzer.get_test_registry();
        let file_tests = test_registry.all_tests();

        if file_tests.is_empty() {
            continue; // No tests in this file
        }

        total_tests += file_tests.len();

        if cli.transpile {
            // Use transpiler mode
            let results = run_transpiler_tests(&ast, test_registry, &cli)?;
            all_test_results.extend(results);
        } else {
            // Use interpreter mode
            let config = TestConfig {
                show_output: cli.show_output,
                capture_output: true,
                fail_fast: cli.fail_fast,
                include_ignored: cli.include_ignored,
                filter: cli.filter.clone(),
                test_threads: cli.test_threads,
                show_timing: cli.show_timing,
                no_color: cli.no_color || no_color,
            };

            let runner = TestRunner::new(ast, config);
            let results = runner.run_tests_with_source(test_registry, &code);
            all_test_results.extend(results);
        }
    }

    if total_tests == 0 {
        println!("No tests found");
        return Ok(());
    }

    // Print overall summary if we ran tests from multiple files
    if num_files > 1 {
        let passed = all_test_results
            .iter()
            .filter(|r| r.passed && !r.ignored)
            .count();
        let failed = all_test_results.iter().filter(|r| !r.passed).count();
        let ignored = all_test_results.iter().filter(|r| r.ignored).count();

        println!("\n=== Overall Test Summary ===");
        println!("Files processed: {num_files}");
        println!("Total tests: {total_tests}");
        println!("Passed: {passed}, Failed: {failed}, Ignored: {ignored}");

        if failed > 0 {
            std::process::exit(1);
        }
    }

    Ok(())
}

fn collect_husk_files(path: &Path, files: &mut Vec<PathBuf>) -> anyhow::Result<()> {
    use std::fs;

    if path.is_file() {
        if path.extension().and_then(|s| s.to_str()) == Some("husk") {
            files.push(path.to_path_buf());
        }
    } else if path.is_dir() {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            collect_husk_files(&path, files)?;
        }
    }
    Ok(())
}

fn run_transpiler_tests(
    ast: &[husk_lang::Stmt],
    registry: &husk_lang::TestRegistry,
    cli: &Test,
) -> anyhow::Result<Vec<husk_lang::TestResult>> {
    use husk_lang::test_transpiler::{
        TestRunner as JsTestRunner, TestTranspileConfig, TestTranspiler,
    };
    use std::fs;
    use std::process::{Command, Stdio};

    // Create transpiler config
    let js_runner = match cli.js_runner.as_str() {
        "node" => JsTestRunner::Node,
        "jest" => JsTestRunner::Jest,
        "mocha" => JsTestRunner::Mocha,
        _ => JsTestRunner::Standalone,
    };

    let config = TestTranspileConfig {
        node_compat: true,
        es_modules: false,
        runner: js_runner,
        include_timing: cli.show_timing,
    };

    // Generate test harness
    let mut transpiler = TestTranspiler::new();
    let js_code = transpiler.generate_test_harness(ast, registry, &config)?;

    // Write to temporary file
    let temp_file = "/tmp/husk_test_harness.js";
    fs::write(temp_file, js_code)?;

    // Run the JavaScript tests
    println!("Running tests in JavaScript mode...");
    let output = Command::new("node")
        .arg(temp_file)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;

    // Print the output
    print!("{}", String::from_utf8_lossy(&output.stdout));
    if !output.stderr.is_empty() {
        eprint!("{}", String::from_utf8_lossy(&output.stderr));
    }

    // Clean up
    let _ = fs::remove_file(temp_file);

    // For transpiler mode, we don't have detailed test results,
    // so we return an empty vec. The exit code will be checked by the process.
    if !output.status.success() {
        std::process::exit(1);
    }

    Ok(Vec::new())
}
