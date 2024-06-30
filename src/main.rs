use clap::{Parser, Subcommand};
use husk::{execute_script, repl};

#[derive(Parser, Debug)]
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Executes a husk script
    Run { file: std::path::PathBuf },

    /// Runs the husk REPL
    Repl,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli {
        Cli {
            command: Command::Run { file },
        } => {
            let code = std::fs::read_to_string(file)?;
            match execute_script(&code) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{}", e.pretty_print(code));
                    std::process::exit(1);
                }
            }
        }
        Cli {
            command: Command::Repl,
        } => {
            let _ = repl();
        }
    }

    Ok(())
}
