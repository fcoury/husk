use std::env;
use std::fs;
use std::path::Path;

use husk_codegen_js::lower_file_to_js;
use husk_parser::parse_str;
use husk_semantic::analyze_file;

fn main() {
    let mut args = env::args().skip(1);
    let first = match args.next() {
        Some(a) => a,
        None => {
            eprintln!("Usage: huskc [check|compile] <source-file>");
            std::process::exit(1);
        }
    };

    let (mode, path) = match first.as_str() {
        "check" => {
            let path = match args.next() {
                Some(p) => p,
                None => {
                    eprintln!("Usage: huskc check <source-file>");
                    std::process::exit(1);
                }
            };
            (Mode::Check, path)
        }
        "compile" => {
            let path = match args.next() {
                Some(p) => p,
                None => {
                    eprintln!("Usage: huskc compile <source-file>");
                    std::process::exit(1);
                }
            };
            (Mode::Compile, path)
        }
        _ => (Mode::ParseOnly, first),
    };

    eprintln!("[huskc] reading source: {path}");
    let content = match fs::read_to_string(&path) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("Failed to read {}: {err}", path);
            std::process::exit(1);
        }
    };

    eprintln!("[huskc] parsing");
    let result = parse_str(&content);

    if !result.errors.is_empty() {
        eprintln!("Failed to parse {}:", path);
        for err in result.errors {
            eprintln!("- {} at {:?}", err.message, err.span.range);
        }
        std::process::exit(1);
    }

    eprintln!("[huskc] parse complete, {} error(s)", result.errors.len());
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
            eprintln!("[huskc] running semantic analysis");
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
                eprintln!("[huskc] semantic analysis OK");
                let file_name = Path::new(&path)
                    .file_name()
                    .and_then(|s| s.to_str())
                    .unwrap_or(&path);
                println!("Successfully type-checked {file_name}");
            }
        }
        Mode::Compile => {
            // First run semantic analysis; if there are errors, report and abort.
            eprintln!("[huskc] running semantic analysis");
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
            }

            eprintln!("[huskc] lowering to JS");
            let module = lower_file_to_js(&file);
            let js = module.to_source();
            println!("{js}");
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Mode {
    ParseOnly,
    Check,
    Compile,
}
