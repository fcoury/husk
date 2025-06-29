use anyhow::{Context, Result};
use clap::Args;
use std::fs;
use std::path::Path;

use husk_lang::dts::{convert_dts_to_husk, parse_dts_file};

#[derive(Debug, Args)]
pub struct Dts2ExternCommand {
    /// Input .d.ts file path
    input: String,

    /// Output file path (default: stdout)
    #[arg(short, long)]
    output: Option<String>,

    /// Module name (default: inferred from filename)
    #[arg(short, long)]
    module: Option<String>,

    /// Simplify complex types to 'any'
    #[arg(long)]
    simplify: bool,

    /// Only convert matching exports (regex pattern)
    #[arg(long)]
    filter: Option<String>,
}

impl Dts2ExternCommand {
    pub fn execute(&self) -> Result<()> {
        let input_path = Path::new(&self.input);
        
        // Verify input file exists and has .d.ts extension
        if !input_path.exists() {
            anyhow::bail!("Input file does not exist: {}", self.input);
        }
        
        if !self.input.ends_with(".d.ts") {
            anyhow::bail!("Input file must be a TypeScript declaration file (.d.ts)");
        }

        // Infer module name if not provided
        let module_name = if let Some(ref name) = self.module {
            name.clone()
        } else {
            input_path
                .file_stem()
                .and_then(|s| s.to_str())
                .map(|s| s.replace(".d", ""))
                .unwrap_or_else(|| "unnamed".to_string())
        };

        // Parse the .d.ts file
        let parsed = parse_dts_file(input_path)
            .context("Failed to parse TypeScript declaration file")?;

        // Convert to Husk extern declarations
        let husk_module = convert_dts_to_husk(&parsed.module, module_name)
            .context("Failed to convert TypeScript to Husk")?;

        // Generate output
        let output_code = husk_module.to_husk_code();

        // Write output
        if let Some(ref output_path) = self.output {
            fs::write(output_path, &output_code)
                .context("Failed to write output file")?;
            println!("Successfully converted {} to {}", self.input, output_path);
        } else {
            print!("{}", output_code);
        }

        Ok(())
    }
}