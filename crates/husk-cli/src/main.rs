use std::env;
use std::fs;
use std::path::Path;

use clap::{Parser, Subcommand};
use husk_codegen_js::lower_file_to_js;
use husk_parser::parse_str;
use husk_semantic::analyze_file;

#[derive(Parser, Debug)]
#[command(name = "huskc", version, about = "The Husk compiler CLI")]
struct Cli {
    /// Enable verbose debug logging (or set HUSKC_DEBUG=1)
    #[arg(short, long)]
    debug: bool,

    #[command(subcommand)]
    command: Option<Command>,

    /// Source file (when no subcommand is used)
    file: Option<String>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Parse and type-check a file
    Check {
        /// Source file to check
        file: String,
    },
    /// Compile a file to JavaScript and print to stdout
    Compile {
        /// Source file to compile
        file: String,
    },
}

fn main() {
    let cli = Cli::parse();

    if cli.debug {
        #[allow(unsafe_code)]
        unsafe {
            std::env::set_var("HUSKC_DEBUG", "1");
        }
    }

    match &cli.command {
        Some(Command::Check { file }) => run_check(file),
        Some(Command::Compile { file }) => run_compile(file),
        None => {
            // Default: just parse the file if provided.
            let file = match &cli.file {
                Some(f) => f,
                None => {
                    eprintln!("Usage: huskc [--debug] [check|compile] <source-file>");
                    std::process::exit(1);
                }
            };
            run_parse_only(file);
        }
    }
}

fn run_parse_only(path: &str) {
    debug_log(&format!("[huskc] reading source: {path}"));
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("Failed to read {}: {err}", path);
            std::process::exit(1);
        }
    };

    debug_log("[huskc] parsing");
    let result = parse_str(&content);

    if !result.errors.is_empty() {
        eprintln!("Failed to parse {}:", path);
        for err in result.errors {
            eprintln!("- {} at {:?}", err.message, err.span.range);
        }
        std::process::exit(1);
    }

    debug_log(&format!(
        "[huskc] parse complete, {} error(s)",
        result.errors.len()
    ));
    let _file = match result.file {
        Some(f) => f,
        None => {
            eprintln!("No AST produced for {}", path);
            std::process::exit(1);
        }
    };

    let file_name = Path::new(path)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(path);
    println!("Successfully parsed {file_name}");
}

fn run_check(path: &str) {
    debug_log(&format!("[huskc] reading source: {path}"));
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("Failed to read {}: {err}", path);
            std::process::exit(1);
        }
    };

    debug_log("[huskc] parsing");
    let result = parse_str(&content);

    if !result.errors.is_empty() {
        eprintln!("Failed to parse {}:", path);
        for err in result.errors {
            eprintln!("- {} at {:?}", err.message, err.span.range);
        }
        std::process::exit(1);
    }

    debug_log(&format!(
        "[huskc] parse complete, {} error(s)",
        result.errors.len()
    ));
    let file = match result.file {
        Some(f) => f,
        None => {
            eprintln!("No AST produced for {}", path);
            std::process::exit(1);
        }
    };

    debug_log("[huskc] running semantic analysis");
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

    debug_log("[huskc] semantic analysis OK");
    let file_name = Path::new(path)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(path);
    println!("Successfully type-checked {file_name}");
}

fn run_compile(path: &str) {
    debug_log(&format!("[huskc] reading source: {path}"));
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("Failed to read {}: {err}", path);
            std::process::exit(1);
        }
    };

    debug_log("[huskc] parsing");
    let result = parse_str(&content);

    if !result.errors.is_empty() {
        eprintln!("Failed to parse {}:", path);
        for err in result.errors {
            eprintln!("- {} at {:?}", err.message, err.span.range);
        }
        std::process::exit(1);
    }

    debug_log(&format!(
        "[huskc] parse complete, {} error(s)",
        result.errors.len()
    ));
    let file = match result.file {
        Some(f) => f,
        None => {
            eprintln!("No AST produced for {}", path);
            std::process::exit(1);
        }
    };

    debug_log("[huskc] running semantic analysis");
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

    debug_log("[huskc] lowering to JS");
    let module = lower_file_to_js(&file);
    let js = module.to_source();
    println!("{js}");
}

fn debug_log(msg: &str) {
    match env::var("HUSKC_DEBUG") {
        Ok(val) if val == "1" || val.eq_ignore_ascii_case("true") => {
            eprintln!("{msg}");
        }
        _ => {}
    }
}
