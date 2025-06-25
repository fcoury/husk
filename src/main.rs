use std::path::PathBuf;

use clap::{Parser, Subcommand};
use husk::repl;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Optional subcommand
    #[clap(subcommand)]
    cmd: Option<Command>,

    /// Run a hash script
    file: Option<std::path::PathBuf>,
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

    /// Generate package.json from husk.toml
    #[clap(long)]
    generate_package_json: bool,
}

#[derive(Parser, Debug)]
struct New {
    /// The name of the project to create
    name: String,
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
}

#[derive(Parser, Debug)]
struct Compile {
    /// The Husk script to transpile
    file: std::path::PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    if let Some(file) = cli.file {
        return run_command(file);
    }

    Ok(match cli.cmd {
        Some(Command::Run(run)) => run_command(run.file)?,
        Some(Command::Compile(compile)) => compile_command(compile)?,
        Some(Command::Build(build)) => build_command(build)?,
        Some(Command::New(new)) => new_command(new)?,
        Some(Command::Repl) | None => repl()?,
    })
}

fn run_command(file: PathBuf) -> anyhow::Result<()> {
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

    match husk::execute_script_with_context(&code, Some(file), project_root) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e.pretty_print(code));
            std::process::exit(1);
        }
    }

    Ok(())
}

fn build_command(cli: Build) -> anyhow::Result<()> {
    use husk::HuskConfig;
    use std::fs;

    // Load husk.toml configuration
    let (config, project_root) = match HuskConfig::find_and_load() {
        Ok((config, root)) => (config, root),
        Err(e) => {
            eprintln!("Error: Could not find husk.toml: {}", e);
            eprintln!("Run 'husk init' to create a new project or ensure you're in a Husk project directory.");
            std::process::exit(1);
        }
    };

    println!("Building project: {}", config.package.name);

    // Generate package.json if requested
    if cli.generate_package_json {
        let package_json = config.generate_package_json()?;
        let package_json_path = project_root.join("package.json");
        fs::write(&package_json_path, package_json)?;
        println!("Generated package.json");
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

    println!("Target: {}", target);

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

        match husk::transpile_to_js_with_packages(&code) {
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

fn compile_command(cli: Compile) -> anyhow::Result<()> {
    let code = std::fs::read_to_string(cli.file)?;
    match husk::transpile_to_js_with_packages(&code) {
        Ok(js) => println!("{}", js),
        Err(e) => {
            eprintln!("{}", e.pretty_print(code));
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
name = "{}"
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
"#,
        project_name
    );

    fs::write(project_path.join("husk.toml"), husk_toml_content)?;

    // Create main.husk
    let main_husk_content = r#"fn main() {
    println("Hello from Husk!");
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

    println!("Created new Husk project '{}'", project_name);
    println!();
    println!("To get started:");
    println!("  cd {}", project_name);
    println!("  husk build");
    println!("  husk run src/main.husk");

    Ok(())
}
