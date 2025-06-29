use anyhow::{Context, Result};
use clap::Args;
use std::fs;
use std::path::Path;

use husk_lang::dts::{convert_dts_to_husk, parse_dts_string};

#[derive(Debug, Args)]
pub struct Dts2ExternCommand {
    /// Input .d.ts file path or URL
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
        // Check if input is a URL
        let (content, filename) = if self.input.starts_with("http://") || self.input.starts_with("https://") {
            // Fetch content from URL
            let response = reqwest::blocking::get(&self.input)
                .context("Failed to fetch URL")?;
            
            if !response.status().is_success() {
                anyhow::bail!("HTTP request failed with status: {}", response.status());
            }
            
            let content = response.text()
                .context("Failed to read response body")?;
            
            // Extract filename from URL
            let url_path = self.input.split('/').last().unwrap_or("unnamed.d.ts");
            let filename = url_path.to_string();
            
            (content, filename)
        } else {
            // Read from file
            let input_path = Path::new(&self.input);
            
            if !input_path.exists() {
                anyhow::bail!("Input file does not exist: {}", self.input);
            }
            
            let content = fs::read_to_string(input_path)
                .context("Failed to read input file")?;
            
            let filename = input_path
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("unnamed.d.ts")
                .to_string();
                
            (content, filename)
        };
        
        // Verify .d.ts extension
        if !filename.ends_with(".d.ts") {
            anyhow::bail!("Input must be a TypeScript declaration file (.d.ts)");
        }

        // Infer module name if not provided
        let module_name = if let Some(ref name) = self.module {
            name.clone()
        } else {
            filename
                .strip_suffix(".d.ts")
                .or_else(|| filename.strip_suffix(".ts"))
                .unwrap_or(&filename)
                .replace(".d", "")
                .to_string()
        };

        // Parse the .d.ts content
        let parsed = parse_dts_string(&content, &filename)
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