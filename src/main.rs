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

#[derive(Subcommand, Debug)]
enum Command {
    /// Start the Husk REPL
    Repl,

    /// Run a Husk script
    Run(Run),

    /// Transpile a Husk script to JavaScript
    Compile(Compile),
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
        Some(Command::Repl) | None => repl()?,
    })
}

fn run_command(file: PathBuf) -> anyhow::Result<()> {
    let code = std::fs::read_to_string(&file)?;
    
    // Determine project root (for now, use parent of src directory if it exists)
    let project_root = file.parent()
        .and_then(|p| p.file_name())
        .and_then(|name| if name == "src" { file.parent()?.parent() } else { None })
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

fn compile_command(cli: Compile) -> anyhow::Result<()> {
    let code = std::fs::read_to_string(cli.file)?;
    match husk::transpile_to_js(&code) {
        Ok(js) => println!("{}", js),
        Err(e) => {
            eprintln!("{}", e.pretty_print(code));
            std::process::exit(1);
        }
    }

    Ok(())
}
