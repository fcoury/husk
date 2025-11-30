use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command as ProcessCommand, Stdio};

use clap::{Parser, Subcommand, ValueEnum};
use husk_codegen_js::{JsTarget, file_to_dts, lower_file_to_js, lower_file_to_js_with_source};
use husk_dts_parser::{CodegenOptions, parse as parse_dts, generate as generate_husk};
mod diagnostic;
mod load;
use diagnostic::SourceDb;
use husk_parser::parse_str;
use husk_semantic::{SemanticOptions, analyze_file_with_options};

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
    /// Build the project to dist/ directory
    Build {
        /// Entry point file (default: auto-detect src/main.hk or main.hk)
        file: Option<String>,
        /// Output directory
        #[arg(short, long, default_value = "dist")]
        output: String,
        /// JavaScript output target: esm or cjs (default: auto-detect from package.json)
        #[arg(long, value_enum)]
        target: Option<Target>,
        /// Compile in library mode (do not auto-call `main`)
        #[arg(long)]
        lib: bool,
        /// Also emit a `.d.ts` declaration file
        #[arg(long)]
        emit_dts: bool,
        /// Generate source maps
        #[arg(long, default_value = "true")]
        source_map: bool,
        /// Disable stdlib prelude injection (Option/Result)
        #[arg(long)]
        no_prelude: bool,
        /// Clean output directory before build
        #[arg(long)]
        clean: bool,
        /// Suppress non-error output
        #[arg(short, long)]
        quiet: bool,
    },
    /// Parse and type-check a file
    Check {
        /// Source file to check
        file: String,
        /// Disable stdlib prelude injection (Option/Result)
        #[arg(long)]
        no_prelude: bool,
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
        /// JavaScript output target: esm or cjs
        #[arg(long, value_enum, default_value = "cjs")]
        target: Target,
        /// Disable stdlib prelude injection (Option/Result)
        #[arg(long)]
        no_prelude: bool,
        /// Output file path (required for source maps, optional otherwise)
        #[arg(short, long)]
        output: Option<String>,
        /// Generate source map (requires --output)
        #[arg(long)]
        source_map: bool,
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
    /// Create a new Husk project
    New {
        /// Name of the project (also used as directory name)
        name: String,
    },
    /// Build and run the project
    Run {
        /// Entry point file (default: auto-detect src/main.hk or main.hk)
        file: Option<String>,
        /// JavaScript output target: esm or cjs (default: auto-detect from package.json)
        #[arg(long, value_enum)]
        target: Option<Target>,
        /// Disable stdlib prelude injection (Option/Result)
        #[arg(long)]
        no_prelude: bool,
        /// Arguments to pass to the program
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Watch for file changes and recompile
    Watch {
        /// Source file to watch (entry point)
        file: String,
        /// Output file path
        #[arg(short, long)]
        output: String,
        /// JavaScript output target: esm or cjs
        #[arg(long, value_enum, default_value = "esm")]
        target: Target,
        /// Compile in library mode (do not auto-call `main`)
        #[arg(long)]
        lib: bool,
        /// Disable stdlib prelude injection (Option/Result)
        #[arg(long)]
        no_prelude: bool,
    },
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
enum Target {
    Esm,
    Cjs,
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
        Some(Command::Build {
            file,
            output,
            target,
            lib,
            emit_dts,
            source_map,
            no_prelude,
            clean,
            quiet,
        }) => run_build(
            file.as_deref(),
            &output,
            target,
            lib,
            emit_dts,
            source_map,
            no_prelude,
            clean,
            quiet,
        ),
        Some(Command::Check { file, no_prelude }) => run_check(&file, no_prelude),
        Some(Command::Compile {
            file,
            emit_dts,
            lib,
            target,
            no_prelude,
            output,
            source_map,
        }) => run_compile(
            &file,
            emit_dts,
            lib,
            target,
            no_prelude,
            output.as_deref(),
            source_map,
        ),
        Some(Command::ImportDts { file, out, module }) => {
            run_import_dts(&file, out.as_deref(), module.as_deref())
        }
        Some(Command::New { name }) => run_new(&name),
        Some(Command::Run {
            file,
            target,
            no_prelude,
            args,
        }) => run_run(file.as_deref(), target, no_prelude, &args),
        Some(Command::Watch {
            file,
            output,
            target,
            lib,
            no_prelude,
        }) => run_watch(&file, &output, target, lib, no_prelude),
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
        let source_db = SourceDb::new(path.to_string(), content);
        for err in &result.errors {
            source_db.report_parse_error(&err.message, err.span.range.clone());
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

fn run_check(path: &str, no_prelude: bool) {
    debug_log(&format!("[huskc] building module graph from {path}"));

    // Read the source for error reporting
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("Failed to read {}: {err}", path);
            std::process::exit(1);
        }
    };

    let graph = match load::load_graph(Path::new(path)) {
        Ok(g) => g,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };
    let file = match load::assemble_root(&graph) {
        Ok(f) => f,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };

    debug_log("[huskc] running semantic analysis");
    let semantic = analyze_file_with_options(
        &file,
        SemanticOptions {
            prelude: !no_prelude,
        },
    );

    let errors: Vec<_> = semantic
        .symbols
        .errors
        .iter()
        .chain(semantic.type_errors.iter())
        .collect();

    if !errors.is_empty() {
        let source_db = SourceDb::new(path.to_string(), content);
        for err in errors {
            source_db.report_semantic_error(&err.message, err.span.range.clone());
        }
        std::process::exit(1);
    }

    debug_log("[huskc] semantic analysis OK");
    let file_name = Path::new(path)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(path);
    println!("Successfully type-checked {file_name}");
}

fn run_compile(
    path: &str,
    emit_dts: bool,
    lib: bool,
    target: Target,
    no_prelude: bool,
    output: Option<&str>,
    source_map: bool,
) {
    // Validate source_map requires output
    if source_map && output.is_none() {
        eprintln!("Error: --source-map requires --output");
        std::process::exit(1);
    }

    debug_log(&format!("[huskc] building module graph from {path}"));

    // Read the source for error reporting and source maps
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("Failed to read {}: {err}", path);
            std::process::exit(1);
        }
    };

    let graph = match load::load_graph(Path::new(path)) {
        Ok(g) => g,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };
    let file = match load::assemble_root(&graph) {
        Ok(f) => f,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };

    debug_log("[huskc] running semantic analysis");
    let semantic = analyze_file_with_options(
        &file,
        SemanticOptions {
            prelude: !no_prelude,
        },
    );

    let errors: Vec<_> = semantic
        .symbols
        .errors
        .iter()
        .chain(semantic.type_errors.iter())
        .collect();

    if !errors.is_empty() {
        let source_db = SourceDb::new(path.to_string(), content.clone());
        for err in errors {
            source_db.report_semantic_error(&err.message, err.span.range.clone());
        }
        std::process::exit(1);
    }

    debug_log("[huskc] lowering to JS");
    let js_target = match target {
        Target::Esm => JsTarget::Esm,
        Target::Cjs => JsTarget::Cjs,
    };

    if source_map {
        // Generate with source map
        let module = lower_file_to_js_with_source(&file, !lib, js_target, Some(&content));
        let source_file = Path::new(path)
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or(path);
        let (mut js, map_json) = module.to_source_with_sourcemap(source_file, &content);

        let output_path = output.unwrap();
        let map_path = format!("{}.map", output_path);

        // Append source map reference to JS
        let map_name = Path::new(&map_path)
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or(&map_path);
        js.push_str(&format!("//# sourceMappingURL={}\n", map_name));

        // Ensure output directory exists
        if let Some(parent) = Path::new(output_path).parent() {
            if !parent.as_os_str().is_empty() {
                if let Err(err) = fs::create_dir_all(parent) {
                    eprintln!("Failed to create output directory: {err}");
                    std::process::exit(1);
                }
            }
        }

        // Write JS file
        if let Err(err) = fs::write(output_path, &js) {
            eprintln!("Failed to write {}: {err}", output_path);
            std::process::exit(1);
        }

        // Write source map
        if let Err(err) = fs::write(&map_path, &map_json) {
            eprintln!("Failed to write {}: {err}", map_path);
            std::process::exit(1);
        }

        debug_log(&format!("[huskc] wrote {} and {}", output_path, map_path));
    } else {
        // Standard output (no source map)
        let module = lower_file_to_js(&file, !lib, js_target);
        let js = module.to_source_with_preamble();

        if let Some(output_path) = output {
            // Write to file
            if let Some(parent) = Path::new(output_path).parent() {
                if !parent.as_os_str().is_empty() {
                    if let Err(err) = fs::create_dir_all(parent) {
                        eprintln!("Failed to create output directory: {err}");
                        std::process::exit(1);
                    }
                }
            }
            if let Err(err) = fs::write(output_path, &js) {
                eprintln!("Failed to write {}: {err}", output_path);
                std::process::exit(1);
            }
        } else {
            // Print to stdout
            println!("{js}");
        }
    }

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

    debug_log("[huskc] parsing .d.ts");
    let file = match parse_dts(&content) {
        Ok(f) => f,
        Err(err) => {
            eprintln!("Parse error in {}: {}", path, err);
            std::process::exit(1);
        }
    };

    debug_log("[huskc] generating Husk code");
    let options = CodegenOptions {
        module_name: module.map(String::from),
        verbose: env::var("HUSKC_DEBUG").map(|v| v == "1" || v.eq_ignore_ascii_case("true")).unwrap_or(false),
    };
    let result = generate_husk(&file, &options);

    // Print warnings to stderr if any
    for warning in &result.warnings {
        debug_log(&format!("[huskc] warning: {}", warning.message));
    }

    if let Some(out_path) = out {
        if let Err(err) = fs::write(out_path, &result.code) {
            eprintln!("Failed to write {}: {err}", out_path);
            std::process::exit(1);
        }
        debug_log(&format!("[huskc] wrote {}", out_path));
    } else {
        print!("{}", result.code);
    }
}

fn run_new(name: &str) {
    let project_dir = Path::new(name);

    if project_dir.exists() {
        eprintln!("Error: directory '{}' already exists", name);
        std::process::exit(1);
    }

    // Create project structure
    let src_dir = project_dir.join("src");
    if let Err(err) = fs::create_dir_all(&src_dir) {
        eprintln!("Failed to create directory: {err}");
        std::process::exit(1);
    }

    // Write main.hk
    let main_hk = r#"fn main() {
    let message = "Hello from Husk!";
}
"#;
    if let Err(err) = fs::write(src_dir.join("main.hk"), main_hk) {
        eprintln!("Failed to write main.hk: {err}");
        std::process::exit(1);
    }

    // Write package.json
    let package_json = format!(
        r#"{{
  "name": "{}",
  "version": "0.1.0",
  "type": "module",
  "scripts": {{
    "build": "huskc build",
    "start": "huskc run"
  }}
}}
"#,
        name
    );
    if let Err(err) = fs::write(project_dir.join("package.json"), package_json) {
        eprintln!("Failed to write package.json: {err}");
        std::process::exit(1);
    }

    // Write README.md
    let readme = format!(
        r#"# {}

A Husk project.

## Getting Started

```bash
# Build the project
npm run build

# Run the project
npm start
```

## Project Structure

```
{}/
├── src/
│   └── main.hk    # Entry point
├── package.json
└── README.md
```
"#,
        name, name
    );
    if let Err(err) = fs::write(project_dir.join("README.md"), readme) {
        eprintln!("Failed to write README.md: {err}");
        std::process::exit(1);
    }

    // Create dist directory
    if let Err(err) = fs::create_dir_all(project_dir.join("dist")) {
        eprintln!("Failed to create dist directory: {err}");
        std::process::exit(1);
    }

    // Write .gitignore
    let gitignore = r#"dist/
node_modules/
"#;
    if let Err(err) = fs::write(project_dir.join(".gitignore"), gitignore) {
        eprintln!("Failed to write .gitignore: {err}");
        std::process::exit(1);
    }

    println!("Created new Husk project: {}", name);
    println!();
    println!("To get started:");
    println!("  cd {}", name);
    println!("  npm run build");
    println!("  npm start");
}

fn run_watch(path: &str, output: &str, target: Target, lib: bool, no_prelude: bool) {
    use notify_debouncer_mini::{new_debouncer, notify::RecursiveMode};
    use std::sync::mpsc::channel;
    use std::time::Duration;

    let entry_path = Path::new(path);
    let output_path = Path::new(output);

    // Get the directory to watch (parent of entry file)
    let watch_dir = entry_path.parent().unwrap_or(Path::new(".")).to_path_buf();

    println!("Watching {} for changes...", watch_dir.display());
    println!("Output: {}", output_path.display());
    println!("Press Ctrl+C to stop.\n");

    // Initial compile
    compile_to_file(path, output, target, lib, no_prelude);

    // Set up file watcher
    let (tx, rx) = channel();
    let mut debouncer = match new_debouncer(Duration::from_millis(200), tx) {
        Ok(d) => d,
        Err(err) => {
            eprintln!("Failed to create file watcher: {err}");
            std::process::exit(1);
        }
    };

    if let Err(err) = debouncer
        .watcher()
        .watch(&watch_dir, RecursiveMode::Recursive)
    {
        eprintln!("Failed to watch directory: {err}");
        std::process::exit(1);
    }

    // Watch loop
    loop {
        match rx.recv() {
            Ok(Ok(events)) => {
                // Check if any .hk files changed
                let hk_changed = events
                    .iter()
                    .any(|e| e.path.extension().map(|ext| ext == "hk").unwrap_or(false));

                if hk_changed {
                    println!("\n[{}] File changed, recompiling...", chrono_lite_time());
                    compile_to_file(path, output, target, lib, no_prelude);
                }
            }
            Ok(Err(errs)) => {
                eprintln!("Watch error: {errs}");
            }
            Err(err) => {
                eprintln!("Channel error: {err}");
                break;
            }
        }
    }
}

/// Simple time formatting without external crate
fn chrono_lite_time() -> String {
    use std::time::SystemTime;
    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default();
    let secs = now.as_secs();
    let hours = (secs / 3600) % 24;
    let mins = (secs / 60) % 60;
    let secs = secs % 60;
    format!("{:02}:{:02}:{:02}", hours, mins, secs)
}

/// Compile and write to file, reporting errors but not exiting
fn compile_to_file(path: &str, output: &str, target: Target, lib: bool, no_prelude: bool) {
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("Failed to read {}: {err}", path);
            return;
        }
    };

    let graph = match load::load_graph(Path::new(path)) {
        Ok(g) => g,
        Err(err) => {
            eprintln!("{err}");
            return;
        }
    };

    let file = match load::assemble_root(&graph) {
        Ok(f) => f,
        Err(err) => {
            eprintln!("{err}");
            return;
        }
    };

    let semantic = analyze_file_with_options(
        &file,
        SemanticOptions {
            prelude: !no_prelude,
        },
    );

    let errors: Vec<_> = semantic
        .symbols
        .errors
        .iter()
        .chain(semantic.type_errors.iter())
        .collect();

    if !errors.is_empty() {
        let source_db = SourceDb::new(path.to_string(), content);
        for err in errors {
            source_db.report_semantic_error(&err.message, err.span.range.clone());
        }
        return;
    }

    let js_target = match target {
        Target::Esm => JsTarget::Esm,
        Target::Cjs => JsTarget::Cjs,
    };
    let module = lower_file_to_js(&file, !lib, js_target);
    let js = module.to_source_with_preamble();

    // Ensure output directory exists
    if let Some(parent) = Path::new(output).parent() {
        if !parent.as_os_str().is_empty() {
            if let Err(err) = fs::create_dir_all(parent) {
                eprintln!("Failed to create output directory: {err}");
                return;
            }
        }
    }

    if let Err(err) = fs::write(output, js) {
        eprintln!("Failed to write {}: {err}", output);
        return;
    }

    println!("Compiled {} -> {}", path, output);
}

fn debug_log(msg: &str) {
    match env::var("HUSKC_DEBUG") {
        Ok(val) if val == "1" || val.eq_ignore_ascii_case("true") => {
            eprintln!("{msg}");
        }
        _ => {}
    }
}

/// Detect entry point file. Checks src/main.hk first, then main.hk.
fn detect_entry_point() -> Result<PathBuf, String> {
    let candidates = ["src/main.hk", "main.hk"];
    for candidate in candidates {
        let path = PathBuf::from(candidate);
        if path.exists() {
            return Ok(path);
        }
    }
    Err("No entry point found. Expected src/main.hk or main.hk.\n\
         Hint: Run 'huskc new <name>' to create a new project,\n\
         or specify a file: 'huskc build myfile.hk'"
        .into())
}

/// Detect target from package.json. Returns ESM if "type": "module", otherwise CJS.
fn detect_target_from_package_json() -> Target {
    let pkg_path = Path::new("package.json");
    if let Ok(content) = fs::read_to_string(pkg_path) {
        // Simple JSON parsing for "type" field
        if content.contains("\"type\"") && content.contains("\"module\"") {
            return Target::Esm;
        }
    }
    Target::Cjs
}

/// Check for missing node_modules when package.json exists
fn check_npm_deps() -> Option<String> {
    let pkg_json = Path::new("package.json");
    let node_modules = Path::new("node_modules");

    if pkg_json.exists() && !node_modules.exists() {
        Some("Warning: node_modules not found. Run 'npm install' before executing.".into())
    } else {
        None
    }
}

#[allow(clippy::too_many_arguments)]
fn run_build(
    file: Option<&str>,
    output_dir: &str,
    target: Option<Target>,
    lib: bool,
    emit_dts: bool,
    source_map: bool,
    no_prelude: bool,
    clean: bool,
    quiet: bool,
) {
    // Detect or use provided entry point
    let entry_path = match file {
        Some(f) => PathBuf::from(f),
        None => match detect_entry_point() {
            Ok(p) => p,
            Err(msg) => {
                eprintln!("Error: {msg}");
                std::process::exit(2);
            }
        },
    };

    let entry_str = entry_path.to_string_lossy();
    debug_log(&format!("[huskc] building from entry point: {}", entry_str));

    // Detect or use provided target
    let js_target = match target {
        Some(t) => t,
        None => detect_target_from_package_json(),
    };

    // Clean output directory if requested
    let output_path = Path::new(output_dir);
    if clean && output_path.exists() {
        if let Err(err) = fs::remove_dir_all(output_path) {
            eprintln!("Failed to clean output directory: {err}");
            std::process::exit(1);
        }
    }

    // Create output directory
    if let Err(err) = fs::create_dir_all(output_path) {
        eprintln!("Failed to create output directory: {err}");
        std::process::exit(1);
    }

    // Read source for error reporting
    let content = match fs::read_to_string(&entry_path) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("Failed to read {}: {err}", entry_str);
            std::process::exit(1);
        }
    };

    // Load module graph
    let graph = match load::load_graph(&entry_path) {
        Ok(g) => g,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };

    let file_ast = match load::assemble_root(&graph) {
        Ok(f) => f,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };

    // Semantic analysis
    debug_log("[huskc] running semantic analysis");
    let semantic = analyze_file_with_options(
        &file_ast,
        SemanticOptions {
            prelude: !no_prelude,
        },
    );

    let errors: Vec<_> = semantic
        .symbols
        .errors
        .iter()
        .chain(semantic.type_errors.iter())
        .collect();

    if !errors.is_empty() {
        let source_db = SourceDb::new(entry_str.to_string(), content.clone());
        for err in errors {
            source_db.report_semantic_error(&err.message, err.span.range.clone());
        }
        std::process::exit(1);
    }

    // Determine output file name
    let stem = entry_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main");
    let js_file = output_path.join(format!("{stem}.js"));

    debug_log("[huskc] lowering to JS");
    let codegen_target = match js_target {
        Target::Esm => JsTarget::Esm,
        Target::Cjs => JsTarget::Cjs,
    };

    if source_map {
        // Generate with source map
        let module = lower_file_to_js_with_source(&file_ast, !lib, codegen_target, Some(&content));
        let source_file = entry_path
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or(&entry_str);
        let (mut js, map_json) = module.to_source_with_sourcemap(source_file, &content);

        let map_file = output_path.join(format!("{stem}.js.map"));
        let map_name = format!("{stem}.js.map");
        js.push_str(&format!("//# sourceMappingURL={}\n", map_name));

        // Write JS file
        if let Err(err) = fs::write(&js_file, &js) {
            eprintln!("Failed to write {}: {err}", js_file.display());
            std::process::exit(1);
        }

        // Write source map
        if let Err(err) = fs::write(&map_file, &map_json) {
            eprintln!("Failed to write {}: {err}", map_file.display());
            std::process::exit(1);
        }

        if !quiet {
            println!("Built {} -> {}", entry_str, js_file.display());
            println!("      {} -> {}", "", map_file.display());
        }
    } else {
        // No source map
        let module = lower_file_to_js(&file_ast, !lib, codegen_target);
        let js = module.to_source_with_preamble();

        if let Err(err) = fs::write(&js_file, &js) {
            eprintln!("Failed to write {}: {err}", js_file.display());
            std::process::exit(1);
        }

        if !quiet {
            println!("Built {} -> {}", entry_str, js_file.display());
        }
    }

    // Emit .d.ts if requested
    if emit_dts {
        debug_log("[huskc] emitting .d.ts");
        let dts = file_to_dts(&file_ast);
        let dts_file = output_path.join(format!("{stem}.d.ts"));
        if let Err(err) = fs::write(&dts_file, dts) {
            eprintln!("Failed to write {}: {err}", dts_file.display());
            std::process::exit(1);
        }
        if !quiet {
            println!("      {} -> {}", "", dts_file.display());
        }
    }
}

fn run_run(file: Option<&str>, target: Option<Target>, no_prelude: bool, args: &[String]) {
    // Build first (to dist/, quiet mode)
    run_build(
        file, "dist", target, false, // not lib mode - we need main()
        false, // no dts
        true,  // source maps for better stack traces
        no_prelude, false, // don't clean
        true,  // quiet
    );

    // Determine the JS file to run
    let entry_path = match file {
        Some(f) => PathBuf::from(f),
        None => match detect_entry_point() {
            Ok(p) => p,
            Err(msg) => {
                eprintln!("Error: {msg}");
                std::process::exit(2);
            }
        },
    };

    let stem = entry_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main");
    let js_file = PathBuf::from("dist").join(format!("{stem}.js"));

    // Check for missing npm deps
    if let Some(warning) = check_npm_deps() {
        eprintln!("{warning}");
    }

    // Execute with Node.js
    debug_log(&format!("[huskc] running with node: {}", js_file.display()));

    let status = ProcessCommand::new("node")
        .arg("--enable-source-maps")
        .arg(&js_file)
        .args(args)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status();

    match status {
        Ok(s) => {
            let code = s.code().unwrap_or(1);
            std::process::exit(code);
        }
        Err(err) => {
            if err.kind() == std::io::ErrorKind::NotFound {
                eprintln!("Error: Node.js not found");
                eprintln!("  'huskc run' requires Node.js to execute the compiled code");
                eprintln!();
                eprintln!("  Hint: Install Node.js from https://nodejs.org");
                std::process::exit(3);
            } else {
                eprintln!("Failed to execute node: {err}");
                std::process::exit(3);
            }
        }
    }
}
