use clap::{Parser, Subcommand};
use rusk::{execute_script, repl};

#[derive(Parser, Debug)]
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Executes a rusk script
    Run { file: std::path::PathBuf },

    /// Runs the rusk REPL
    Repl,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli {
        Cli {
            command: Command::Run { file },
        } => {
            let code = std::fs::read_to_string(file)?;
            execute_script(code)?;
        }
        Cli {
            command: Command::Repl,
        } => {
            let _ = repl();
        }
    }

    Ok(())
}
