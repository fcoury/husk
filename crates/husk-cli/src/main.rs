use std::env;
use std::fs;
use std::path::Path;

use husk_parser::parse_str;

fn main() {
    let mut args = env::args().skip(1);
    let path = match args.next() {
        Some(p) => p,
        None => {
            eprintln!("Usage: langc <source-file>");
            std::process::exit(1);
        }
    };

    let content = match fs::read_to_string(&path) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("Failed to read {}: {err}", path);
            std::process::exit(1);
        }
    };

    let result = parse_str(&content);

    if !result.errors.is_empty() {
        eprintln!("Failed to parse {}:", path);
        for err in result.errors {
            eprintln!("- {} at {:?}", err.message, err.span.range);
        }
        std::process::exit(1);
    }

    let file_name = Path::new(&path)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(&path);
    println!("Successfully parsed {file_name}");
}
