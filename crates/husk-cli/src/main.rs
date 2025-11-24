use std::env;
use std::fs;
use std::path::Path;

use clap::{Parser, Subcommand};
use husk_codegen_js::{file_to_dts, lower_file_to_js};
mod dts_import;
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
        /// Also emit a `.d.ts` declaration file next to the source
        #[arg(long)]
        emit_dts: bool,
        /// Compile in library mode (do not auto-call `main`)
        #[arg(long)]
        lib: bool,
    },
    /// Import a `.d.ts` file and generate Husk `extern \"js\"` declarations
    ImportDts {
        /// Path to the `.d.ts` file to import
        file: String,
        /// Optional output path for the generated Husk file (stdout if omitted)
        #[arg(short, long)]
        out: Option<String>,
        /// Include a `mod` declaration to import the npm module.
        /// For valid identifiers: `mod name;`
        /// For packages with hyphens/scopes: `mod "pkg" as alias;`
        #[arg(short, long, value_name = "NAME")]
        module: Option<String>,
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

    match cli.command {
        Some(Command::Check { file }) => run_check(&file),
        Some(Command::Compile {
            file,
            emit_dts,
            lib,
        }) => run_compile(&file, emit_dts, lib),
        Some(Command::ImportDts { file, out, module }) => {
            run_import_dts(&file, out.as_deref(), module.as_deref())
        }
        None => {
            // Default: just parse the file if provided.
            let file = match &cli.file {
                Some(f) => f,
                None => {
                    eprintln!("Usage: huskc [--debug] [check|compile|import-dts] <source-file>");
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

fn run_compile(path: &str, emit_dts: bool, lib: bool) {
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
    let module = lower_file_to_js(&file, !lib);
    let js = module.to_source_with_preamble();
    println!("{js}");

    if emit_dts {
        debug_log("[huskc] emitting .d.ts");
        let dts = file_to_dts(&file);
        let dts_path = Path::new(path).with_extension("d.ts");
        if let Err(err) = fs::write(&dts_path, dts) {
            eprintln!("Failed to write {}: {err}", dts_path.display());
            std::process::exit(1);
        }
    }
}

fn run_import_dts(path: &str, out: Option<&str>, module: Option<&str>) {
    debug_log(&format!("[huskc] reading .d.ts: {path}"));
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("Failed to read {}: {err}", path);
            std::process::exit(1);
        }
    };

    debug_log("[huskc] importing .d.ts");
    let husk = dts_import::import_dts_str_with_module(&content, module);

    if let Some(out_path) = out {
        if let Err(err) = fs::write(out_path, husk) {
            eprintln!("Failed to write {}: {err}", out_path);
            std::process::exit(1);
        }
    } else {
        println!("{husk}");
    }
}

fn debug_log(msg: &str) {
    match env::var("HUSKC_DEBUG") {
        Ok(val) if val == "1" || val.eq_ignore_ascii_case("true") => {
            eprintln!("{msg}");
        }
        _ => {}
    }
}
