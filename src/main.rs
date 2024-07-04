use clap::Parser;
use husk::{execute_script, repl};

#[derive(Parser, Debug)]
struct Cli {
    /// Run a Husk script
    file: Option<std::path::PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    if let Some(file) = cli.file {
        let code = std::fs::read_to_string(file)?;
        match execute_script(&code) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("{}", e.pretty_print(code));
                std::process::exit(1);
            }
        }
    } else {
        let _ = repl();
    }

    Ok(())
}
