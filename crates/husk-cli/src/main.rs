use std::env;
use std::fs;
use std::path::Path;

use husk_parser::parse_str;
use husk_semantic::analyze_file;

fn main() {
    let mut args = env::args().skip(1);
    let first = match args.next() {
        Some(a) => a,
        None => {
            eprintln!("Usage: huskc [check] <source-file>");
            std::process::exit(1);
        }
    };

    let (mode, path) = if first == "check" {
        let path = match args.next() {
            Some(p) => p,
            None => {
                eprintln!("Usage: huskc check <source-file>");
                std::process::exit(1);
            }
        };
        (Mode::Check, path)
    } else {
        (Mode::ParseOnly, first)
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

    let file = match result.file {
        Some(f) => f,
        None => {
            eprintln!("No AST produced for {}", path);
            std::process::exit(1);
        }
    };

    match mode {
        Mode::ParseOnly => {
            let file_name = Path::new(&path)
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or(&path);
            println!("Successfully parsed {file_name}");
        }
        Mode::Check => {
            let semantic = analyze_file(&file);
            let mut had_errors = false;
            for err in semantic
                .symbols
                .errors
                .iter()
                .chain(semantic.type_errors.iter())
            {
                had_errors = true;
                eprintln!("- {} at {:?}", err.message, err.span.range);
            }
            if had_errors {
                std::process::exit(1);
            } else {
                let file_name = Path::new(&path)
                    .file_name()
                    .and_then(|s| s.to_str())
                    .unwrap_or(&path);
                println!("Successfully type-checked {file_name}");
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Mode {
    ParseOnly,
    Check,
}
