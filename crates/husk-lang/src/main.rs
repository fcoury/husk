use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command as ProcessCommand, Stdio};

use clap::{Parser, Subcommand, ValueEnum};
use husk_codegen_js::{JsTarget, Platform as CodegenPlatform, file_to_dts, lower_file_to_js, lower_file_to_js_with_source};
use husk_dts_parser::{
    CodegenOptions as DtsCodegenOptions, DtsFile, UnionStrategy, generate as generate_husk,
    generate_from_module as generate_husk_from_module, parse as parse_dts,
};
mod config;
mod diagnostic;
mod load;
use config::HuskConfig;
use diagnostic::{SourceDb, report_load_error, multi_file_db_from_graph};
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
        /// Platform target: node or browser (default: node)
        #[arg(long, value_enum)]
        platform: Option<Platform>,
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
        /// Source file to watch (entry point). Detected from husk.toml or src/main.hk if not provided.
        file: Option<String>,
        /// Output file path. Defaults to dist/<entry>.js if not provided.
        #[arg(short, long)]
        output: Option<String>,
        /// JavaScript output target: esm or cjs
        #[arg(long, value_enum)]
        target: Option<Target>,
        /// Compile in library mode (do not auto-call `main`)
        #[arg(long)]
        lib: bool,
        /// Disable stdlib prelude injection (Option/Result)
        #[arg(long)]
        no_prelude: bool,
        /// Run the compiled output after each change
        #[arg(long, short = 'r')]
        run: bool,
        /// Custom command to execute after compilation
        #[arg(long, short = 'x')]
        exec: Option<String>,
        /// Arguments to pass to the executed program
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Manage TypeScript definition (.d.ts) imports
    Dts {
        #[command(subcommand)]
        action: DtsAction,
    },
    /// Run tests
    Test {
        /// Source file containing tests (default: auto-detect)
        file: Option<String>,
        /// Filter tests by name pattern
        #[arg(long)]
        filter: Option<String>,
        /// Disable stdlib prelude injection (Option/Result)
        #[arg(long)]
        no_prelude: bool,
        /// Suppress non-error output
        #[arg(short, long)]
        quiet: bool,
    },
    /// Format Husk source files
    Fmt {
        /// Files or directories to format (default: current directory)
        #[arg(default_value = ".")]
        paths: Vec<PathBuf>,
        /// Check if files are formatted without modifying
        #[arg(long)]
        check: bool,
        /// Write formatted output to stdout instead of file
        #[arg(long)]
        print: bool,
        /// Maximum line length
        #[arg(long, default_value = "100")]
        max_width: usize,
        /// Indent size in spaces
        #[arg(long, default_value = "4")]
        indent_size: usize,
    },
}

#[derive(Subcommand, Debug)]
enum DtsAction {
    /// Add a new package for dts import
    Add {
        /// NPM package name (e.g., "express")
        package: String,
        /// TypeScript types package (e.g., "@types/express")
        #[arg(long)]
        types: Option<String>,
        /// Output file path for generated .hk file
        #[arg(long)]
        output: Option<String>,
    },
    /// Update/regenerate .hk files from .d.ts
    Update {
        /// Specific package to update (updates all if omitted)
        package: Option<String>,
        /// Use legacy parser instead of Oxc (not recommended)
        #[arg(long)]
        legacy: bool,
        /// Follow import graph and include dependencies
        #[arg(long)]
        follow_imports: bool,
        /// Generate diagnostic report
        #[arg(long)]
        report: bool,
        /// Report format: markdown or json
        #[arg(long, value_enum, default_value = "markdown")]
        report_format: ReportFormat,
        /// Report output path (default: dts-report.md or dts-report.json)
        #[arg(long)]
        report_path: Option<String>,
    },
    /// List configured dts entries
    List,
    /// Remove a dts entry
    Remove {
        /// NPM package name to remove
        package: String,
    },
}

/// JavaScript module format for output.
#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
enum Target {
    Esm,
    Cjs,
}

/// Platform target for code generation.
/// Browser platform implies ESM module format.
#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum, Default)]
pub enum Platform {
    /// Server-side execution (Node.js)
    #[default]
    Node,
    /// Client-side execution (Browser)
    Browser,
}

impl From<Platform> for CodegenPlatform {
    fn from(p: Platform) -> Self {
        match p {
            Platform::Node => CodegenPlatform::Node,
            Platform::Browser => CodegenPlatform::Browser,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum, Default)]
enum ReportFormat {
    /// Markdown format (default)
    #[default]
    Markdown,
    /// JSON format
    Json,
}

fn main() {
    let cli = Cli::parse();

    if cli.debug {
        #[allow(unsafe_code)]
        unsafe {
            std::env::set_var("HUSKC_DEBUG", "1");
        }
    }

    // Load project configuration
    let config = HuskConfig::load_or_default();

    match cli.command {
        Some(Command::Build {
            file,
            output,
            target,
            platform,
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
            platform,
            lib,
            emit_dts,
            source_map,
            no_prelude,
            clean,
            quiet,
            &config,
        ),
        Some(Command::Check { file, no_prelude }) => run_check(&file, no_prelude, &config),
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
        }) => run_run(file.as_deref(), target, no_prelude, &args, &config),
        Some(Command::Watch {
            file,
            output,
            target,
            lib,
            no_prelude,
            run,
            exec,
            args,
        }) => run_watch(
            file.as_deref(),
            output.as_deref(),
            target,
            lib,
            no_prelude,
            run,
            exec.as_deref(),
            &args,
            &config,
        ),
        Some(Command::Dts { action }) => run_dts(action, &config),
        Some(Command::Test {
            file,
            filter,
            no_prelude,
            quiet,
        }) => run_test(
            file.as_deref(),
            filter.as_deref(),
            no_prelude,
            quiet,
            &config,
        ),
        Some(Command::Fmt {
            paths,
            check,
            print,
            max_width,
            indent_size,
        }) => run_fmt(&paths, check, print, max_width, indent_size),
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

fn run_check(path: &str, no_prelude: bool, config: &HuskConfig) {
    debug_log(&format!("[huskc] building module graph from {path}"));

    // Resolve prelude setting: CLI flag overrides config
    let use_prelude = if no_prelude {
        false
    } else {
        config.build.prelude()
    };

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
            report_load_error(&err);
            std::process::exit(1);
        }
    };
    let file = match load::assemble_root(&graph) {
        Ok(f) => f,
        Err(err) => {
            report_load_error(&err);
            std::process::exit(1);
        }
    };

    debug_log("[huskc] running semantic analysis");
    let semantic = analyze_file_with_options(
        &file,
        SemanticOptions {
            prelude: use_prelude,
            cfg_flags: HashSet::new(),
        },
    );

    let errors: Vec<_> = semantic
        .symbols
        .errors
        .iter()
        .chain(semantic.type_errors.iter())
        .collect();

    if !errors.is_empty() {
        let source_db = multi_file_db_from_graph(path, &content, &graph);
        for err in errors {
            source_db.report_semantic_error(&err.message, &err.span);
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
            report_load_error(&err);
            std::process::exit(1);
        }
    };
    let file = match load::assemble_root(&graph) {
        Ok(f) => f,
        Err(err) => {
            report_load_error(&err);
            std::process::exit(1);
        }
    };

    debug_log("[huskc] running semantic analysis");
    let semantic = analyze_file_with_options(
        &file,
        SemanticOptions {
            prelude: !no_prelude,
            cfg_flags: HashSet::new(),
        },
    );

    let errors: Vec<_> = semantic
        .symbols
        .errors
        .iter()
        .chain(semantic.type_errors.iter())
        .collect();

    if !errors.is_empty() {
        let source_db = multi_file_db_from_graph(path, &content, &graph);
        for err in errors {
            source_db.report_semantic_error(&err.message, &err.span);
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
        let source_path = Path::new(path);
        let module = lower_file_to_js_with_source(
            &file,
            !lib,
            js_target,
            Some(&content),
            Some(source_path),
            &semantic.name_resolution,
            &semantic.type_resolution,
            &semantic.variant_calls,
            &semantic.variant_patterns,
        );
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
        let source_path = Path::new(path);
        let module = lower_file_to_js_with_source(
            &file,
            !lib,
            js_target,
            None,
            Some(source_path),
            &semantic.name_resolution,
            &semantic.type_resolution,
            &semantic.variant_calls,
            &semantic.variant_patterns,
        );
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
    let file_module_name = out.and_then(|p| file_module_name_from_path(Path::new(p)));
    let options = DtsCodegenOptions {
        module_name: module.map(String::from),
        file_module_name,
        verbose: env::var("HUSKC_DEBUG")
            .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
            .unwrap_or(false),
        ..Default::default()
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
    println("Hello from Husk!");
}
"#;
    if let Err(err) = fs::write(src_dir.join("main.hk"), main_hk) {
        eprintln!("Failed to write main.hk: {err}");
        std::process::exit(1);
    }

    // Write husk.toml
    let husk_toml = format!(
        r#"[package]
name = "{}"
version = "0.1.0"

[build]
entry = "src/main.hk"
output = "dist"
target = "auto"
source_maps = true
"#,
        name
    );
    if let Err(err) = fs::write(project_dir.join("husk.toml"), husk_toml) {
        eprintln!("Failed to write husk.toml: {err}");
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
├── husk.toml      # Project configuration
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

fn run_watch(
    file: Option<&str>,
    output: Option<&str>,
    target: Option<Target>,
    lib: bool,
    no_prelude: bool,
    run_after: bool,
    exec_cmd: Option<&str>,
    args: &[String],
    config: &HuskConfig,
) {
    use notify_debouncer_mini::{new_debouncer, notify::RecursiveMode};
    use std::process::Child;
    use std::sync::mpsc::channel;
    use std::time::Duration;

    // Determine entry file (same logic as run command)
    let entry_path = match file {
        Some(f) => PathBuf::from(f),
        None => {
            if let Some(ref entry) = config.build.entry {
                PathBuf::from(entry)
            } else {
                match detect_entry_point() {
                    Ok(p) => p,
                    Err(msg) => {
                        eprintln!("Error: {msg}");
                        std::process::exit(2);
                    }
                }
            }
        }
    };

    // Determine output file
    let output_dir = config.build.output_dir();
    let stem = entry_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main");
    let default_output = format!("{}/{}.js", output_dir, stem);
    let output_str = output.map(|s| s.to_string()).unwrap_or(default_output);
    let output_path = Path::new(&output_str);

    // Determine target
    let effective_target = target.unwrap_or(Target::Esm);

    let path = entry_path.to_string_lossy().to_string();

    // Get the directory to watch (parent of entry file)
    let watch_dir = entry_path.parent().unwrap_or(Path::new(".")).to_path_buf();

    // Get debounce time from config or use default
    let debounce_ms = config
        .watch
        .as_ref()
        .and_then(|w| w.debounce_ms)
        .unwrap_or(200);

    // Get ignore patterns from config
    let ignore_patterns: Vec<&str> = config
        .watch
        .as_ref()
        .and_then(|w| w.ignore.as_ref())
        .map(|v| v.iter().map(|s| s.as_str()).collect())
        .unwrap_or_default();

    // Determine run mode from CLI args or config
    let should_run = run_after || config.watch.as_ref().and_then(|w| w.run).unwrap_or(false);
    let effective_exec = exec_cmd.or_else(|| config.watch.as_ref().and_then(|w| w.exec.as_deref()));

    println!("Watching {} for changes...", watch_dir.display());
    println!("Output: {}", output_path.display());
    if !ignore_patterns.is_empty() {
        println!("Ignoring: {:?}", ignore_patterns);
    }
    if effective_exec.is_some() {
        println!("Exec: {}", effective_exec.unwrap());
    } else if should_run {
        println!("Mode: compile + run");
    }
    println!("Press Ctrl+C to stop.\n");

    // Track the running child process
    let mut child_process: Option<Child> = None;

    // Helper to kill existing process
    let kill_child = |child: &mut Option<Child>| {
        if let Some(mut c) = child.take() {
            let _ = c.kill();
            let _ = c.wait();
        }
    };

    // Helper to spawn run command (node with output file)
    let spawn_run = |output: &str, args: &[String], config: &HuskConfig| -> Option<Child> {
        let default_node_args = vec!["--enable-source-maps".to_string()];
        let node_args = config
            .run
            .as_ref()
            .and_then(|r| r.node_args.as_ref())
            .unwrap_or(&default_node_args);

        let mut cmd = ProcessCommand::new("node");
        cmd.args(node_args)
            .arg(output)
            .args(args)
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit());

        // Set env vars from config
        if let Some(ref run_config) = config.run {
            if let Some(ref env_vars) = run_config.env {
                for (key, value) in env_vars {
                    cmd.env(key, value);
                }
            }
        }

        match cmd.spawn() {
            Ok(child) => Some(child),
            Err(e) => {
                eprintln!("Failed to run: {e}");
                None
            }
        }
    };

    // Helper to spawn custom exec command
    let spawn_exec = |exec_cmd: &str, args: &[String]| -> Option<Child> {
        let parts: Vec<&str> = exec_cmd.split_whitespace().collect();
        if parts.is_empty() {
            return None;
        }

        let mut cmd = ProcessCommand::new(parts[0]);
        cmd.args(&parts[1..])
            .args(args)
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit());

        match cmd.spawn() {
            Ok(child) => Some(child),
            Err(e) => {
                eprintln!("Failed to execute '{}': {e}", exec_cmd);
                None
            }
        }
    };

    // Initial compile
    let compile_success = compile_to_file(&path, &output_str, effective_target, lib, no_prelude);

    // Initial run if compile succeeded
    if compile_success {
        if let Some(exec) = effective_exec {
            child_process = spawn_exec(exec, args);
        } else if should_run {
            child_process = spawn_run(&output_str, args, config);
        }
    }

    // Set up file watcher
    let (tx, rx) = channel();
    let mut debouncer = match new_debouncer(Duration::from_millis(debounce_ms), tx) {
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
                // Check if any .hk files changed (excluding ignored patterns)
                let hk_changed = events.iter().any(|e| {
                    let is_hk = e.path.extension().map(|ext| ext == "hk").unwrap_or(false);
                    if !is_hk {
                        return false;
                    }
                    // Check against ignore patterns (simple substring match)
                    let path_str = e.path.to_string_lossy();
                    !ignore_patterns.iter().any(|pat| {
                        // Simple glob-like matching: check if pattern appears in path
                        let pat_clean = pat.trim_matches('*');
                        path_str.contains(pat_clean)
                    })
                });

                if hk_changed {
                    // Kill existing process before recompiling
                    kill_child(&mut child_process);

                    println!("\n[{}] File changed, recompiling...", chrono_lite_time());
                    let compile_success =
                        compile_to_file(&path, &output_str, effective_target, lib, no_prelude);

                    // Run after compile if successful
                    if compile_success {
                        if let Some(exec) = effective_exec {
                            child_process = spawn_exec(exec, args);
                        } else if should_run {
                            child_process = spawn_run(&output_str, args, config);
                        }
                    }
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

    // Cleanup on exit
    kill_child(&mut child_process);
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

/// Compile and write to file, reporting errors but not exiting.
/// Returns true if compilation succeeded, false otherwise.
fn compile_to_file(path: &str, output: &str, target: Target, lib: bool, no_prelude: bool) -> bool {
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("Failed to read {}: {err}", path);
            return false;
        }
    };

    let graph = match load::load_graph(Path::new(path)) {
        Ok(g) => g,
        Err(err) => {
            report_load_error(&err);
            return false;
        }
    };

    let file = match load::assemble_root(&graph) {
        Ok(f) => f,
        Err(err) => {
            report_load_error(&err);
            return false;
        }
    };

    let semantic = analyze_file_with_options(
        &file,
        SemanticOptions {
            prelude: !no_prelude,
            cfg_flags: HashSet::new(),
        },
    );

    let errors: Vec<_> = semantic
        .symbols
        .errors
        .iter()
        .chain(semantic.type_errors.iter())
        .collect();

    if !errors.is_empty() {
        let source_db = multi_file_db_from_graph(path, &content, &graph);
        for err in errors {
            source_db.report_semantic_error(&err.message, &err.span);
        }
        return false;
    }

    let js_target = match target {
        Target::Esm => JsTarget::Esm,
        Target::Cjs => JsTarget::Cjs,
    };
    let entry_path = Path::new(path);
    let module = lower_file_to_js_with_source(
        &file,
        !lib,
        js_target,
        Some(&content),
        Some(entry_path),
        &semantic.name_resolution,
        &semantic.type_resolution,
        &semantic.variant_calls,
        &semantic.variant_patterns,
    );
    let js = module.to_source_with_preamble();

    // Ensure output directory exists
    if let Some(parent) = Path::new(output).parent() {
        if !parent.as_os_str().is_empty() {
            if let Err(err) = fs::create_dir_all(parent) {
                eprintln!("Failed to create output directory: {err}");
                return false;
            }
        }
    }

    if let Err(err) = fs::write(output, js) {
        eprintln!("Failed to write {}: {err}", output);
        return false;
    }

    println!("Compiled {} -> {}", path, output);
    true
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

/// Detect platform from package.json.
/// - If "browser" field exists, return Browser (strongest signal)
/// - Otherwise return Node (default)
fn detect_platform_from_package_json() -> Platform {
    let pkg_path = Path::new("package.json");
    if let Ok(content) = fs::read_to_string(pkg_path) {
        // Check for "browser" field - strongest signal for browser platform
        if content.contains("\"browser\"") {
            debug_log("[huskc] Info: Auto-detected platform 'browser' from package.json browser field");
            return Platform::Browser;
        }
    }
    Platform::Node
}

#[allow(clippy::too_many_arguments)]
fn run_build(
    file: Option<&str>,
    output_dir: &str,
    target: Option<Target>,
    platform: Option<Platform>,
    cli_lib: bool,
    cli_emit_dts: bool,
    cli_source_map: bool,
    no_prelude: bool,
    clean: bool,
    quiet: bool,
    config: &HuskConfig,
) {
    // Auto-update dts files if configured
    if let Some(ref dts_options) = config.dts_options {
        if dts_options.auto_update.unwrap_or(false) && !config.dts.is_empty() {
            debug_log("[huskc] auto-updating dts files before build");
            run_dts_update(
                None,
                false,
                false,
                false,
                ReportFormat::Markdown,
                None,
                config,
            );
        }
    }

    // Resolve entry point: CLI arg > config > auto-detect
    let entry_path = match file {
        Some(f) => PathBuf::from(f),
        None => {
            if let Some(ref entry) = config.build.entry {
                PathBuf::from(entry)
            } else {
                match detect_entry_point() {
                    Ok(p) => p,
                    Err(msg) => {
                        eprintln!("Error: {msg}");
                        std::process::exit(2);
                    }
                }
            }
        }
    };

    let entry_str = entry_path.to_string_lossy();

    // Show project name if available
    if let Some(ref pkg) = config.package {
        debug_log(&format!("[huskc] building {} v{}", pkg.name, pkg.version));
    }
    debug_log(&format!("[huskc] building from entry point: {}", entry_str));

    // Resolve platform: CLI arg > config > default (Node)
    let effective_platform = platform.unwrap_or_else(|| {
        match config.build.platform() {
            config::ConfigPlatform::Browser => Platform::Browser,
            config::ConfigPlatform::Node => Platform::Node,
            config::ConfigPlatform::Auto => detect_platform_from_package_json(),
        }
    });

    // Resolve target: CLI arg > config > auto-detect
    let js_target = match target {
        Some(t) => t,
        None => {
            match config.build.target.as_deref() {
                Some("esm") => Target::Esm,
                Some("cjs") => Target::Cjs,
                _ => detect_target_from_package_json(), // "auto" or unset
            }
        }
    };

    // Validate: Browser platform requires ESM module format
    if effective_platform == Platform::Browser && js_target == Target::Cjs {
        eprintln!("error: Browser platform requires ESM module format");
        eprintln!("  CJS is not supported for browser targets");
        eprintln!();
        eprintln!("  help: Remove `target = \"cjs\"` or use `--platform node`");
        eprintln!("  note: Modern browsers and bundlers require ES modules");
        std::process::exit(1);
    }

    debug_log(&format!("[huskc] platform: {:?}", effective_platform));

    // Resolve build options: CLI flags override config
    let lib = cli_lib || config.build.lib();
    let emit_dts = cli_emit_dts || config.build.emit_dts();
    let source_map = cli_source_map && config.build.source_maps(); // both must be true

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
            report_load_error(&err);
            std::process::exit(1);
        }
    };

    let file_ast = match load::assemble_root(&graph) {
        Ok(f) => f,
        Err(err) => {
            report_load_error(&err);
            std::process::exit(1);
        }
    };

    // Resolve prelude: CLI flag overrides config
    let use_prelude = if no_prelude {
        false
    } else {
        config.build.prelude()
    };

    // Semantic analysis - filter out #[cfg(test)] items for build
    debug_log("[huskc] running semantic analysis");
    let cfg_flags = HashSet::new(); // No test flag during build
    let filtered_ast = husk_semantic::filter_items_by_cfg(&file_ast, &cfg_flags);
    let semantic = analyze_file_with_options(
        &file_ast,
        SemanticOptions {
            prelude: use_prelude,
            cfg_flags: cfg_flags.clone(),
        },
    );

    let errors: Vec<_> = semantic
        .symbols
        .errors
        .iter()
        .chain(semantic.type_errors.iter())
        .collect();

    if !errors.is_empty() {
        let source_db = multi_file_db_from_graph(&entry_str, &content, &graph);
        for err in errors {
            source_db.report_semantic_error(&err.message, &err.span);
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
        let module = lower_file_to_js_with_source(
            &filtered_ast,
            !lib,
            codegen_target,
            Some(&content),
            Some(&entry_path),
            &semantic.name_resolution,
            &semantic.type_resolution,
            &semantic.variant_calls,
            &semantic.variant_patterns,
        );
        let source_file = entry_path
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or(&entry_str);
        let (mut js, map_json) = module.to_source_with_sourcemap_for_platform(source_file, &content, effective_platform.into());

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
        let module = lower_file_to_js(
            &filtered_ast,
            !lib,
            codegen_target,
            &semantic.name_resolution,
            &semantic.type_resolution,
            &semantic.variant_calls,
            &semantic.variant_patterns,
        );
        let js = module.to_source_with_preamble_for_platform(effective_platform.into());

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
        let dts = file_to_dts(&filtered_ast);
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

fn run_run(
    file: Option<&str>,
    target: Option<Target>,
    no_prelude: bool,
    args: &[String],
    config: &HuskConfig,
) {
    // Use config output dir or default to "dist"
    let output_dir = config.build.output_dir();

    // Build first (quiet mode)
    run_build(
        file, output_dir, target, None, // platform: default to Node for run command
        false, // not lib mode - we need main()
        false, // no dts
        true,  // source maps for better stack traces
        no_prelude, false, // don't clean
        true,  // quiet
        config,
    );

    // Determine the JS file to run
    let entry_path = match file {
        Some(f) => PathBuf::from(f),
        None => {
            if let Some(ref entry) = config.build.entry {
                PathBuf::from(entry)
            } else {
                match detect_entry_point() {
                    Ok(p) => p,
                    Err(msg) => {
                        eprintln!("Error: {msg}");
                        std::process::exit(2);
                    }
                }
            }
        }
    };

    let stem = entry_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main");
    let js_file = PathBuf::from(output_dir).join(format!("{stem}.js"));

    // Execute with Node.js
    debug_log(&format!("[huskc] running with node: {}", js_file.display()));

    // Get node args from config or use default
    let default_node_args = vec!["--enable-source-maps".to_string()];
    let node_args = config
        .run
        .as_ref()
        .and_then(|r| r.node_args.as_ref())
        .unwrap_or(&default_node_args);

    let mut cmd = ProcessCommand::new("node");
    cmd.args(node_args)
        .arg(&js_file)
        .args(args)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());

    // Set environment variables from config
    if let Some(ref run_config) = config.run {
        if let Some(ref env_vars) = run_config.env {
            for (key, value) in env_vars {
                cmd.env(key, value);
            }
        }
    }

    let status = cmd.status();

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

// ========== Test runner ==========

/// Discover test functions from an AST file.
/// Returns a list of (test_name, is_ignored, expected_panic_message) tuples.
fn discover_tests(file: &husk_ast::File) -> Vec<(String, bool, Option<String>)> {
    let mut tests = Vec::new();
    for item in &file.items {
        if item.is_test() {
            if let husk_ast::ItemKind::Fn { name, .. } = &item.kind {
                let is_ignored = item.is_ignored();
                let expected_panic = item.expected_panic_message().map(|s| s.to_string());
                tests.push((name.name.clone(), is_ignored, expected_panic));
            }
        }
    }
    tests
}

fn run_test(
    file: Option<&str>,
    filter: Option<&str>,
    no_prelude: bool,
    quiet: bool,
    config: &HuskConfig,
) {
    // Find entry point
    let path = if let Some(f) = file {
        f.to_string()
    } else {
        // Auto-detect: look for src/main.hk, main.hk, or src/lib.hk
        if Path::new("src/main.hk").exists() {
            "src/main.hk".to_string()
        } else if Path::new("main.hk").exists() {
            "main.hk".to_string()
        } else if Path::new("src/lib.hk").exists() {
            "src/lib.hk".to_string()
        } else {
            eprintln!("No source file found. Please specify a file or create src/main.hk");
            std::process::exit(1);
        }
    };

    if !quiet {
        let file_display = Path::new(&path)
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or(&path);
        println!("   Running tests in {file_display}");
        println!();
    }

    // Resolve prelude setting
    let use_prelude = if no_prelude {
        false
    } else {
        config.build.prelude()
    };

    // Read the source
    let content = match fs::read_to_string(&path) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("Failed to read {}: {err}", path);
            std::process::exit(1);
        }
    };

    // Parse
    let result = parse_str(&content);
    if !result.errors.is_empty() {
        let source_db = SourceDb::new(path.clone(), content);
        for err in &result.errors {
            source_db.report_parse_error(&err.message, err.span.range.clone());
        }
        std::process::exit(1);
    }

    let mut file_ast = match result.file {
        Some(f) => f,
        None => {
            eprintln!("No AST produced for {}", path);
            std::process::exit(1);
        }
    };

    // Enable test cfg flag for semantic analysis
    let mut cfg_flags = HashSet::new();
    cfg_flags.insert("test".to_string());

    // Semantic analysis with test flag
    let semantic = analyze_file_with_options(
        &file_ast,
        SemanticOptions {
            prelude: use_prelude,
            cfg_flags: cfg_flags.clone(),
        },
    );

    let errors: Vec<_> = semantic
        .symbols
        .errors
        .iter()
        .chain(semantic.type_errors.iter())
        .collect();

    if !errors.is_empty() {
        let source_db = SourceDb::new(path.clone(), content.clone());
        for err in &errors {
            source_db.report_parse_error(&err.message, err.span.range.clone());
        }
        std::process::exit(1);
    }

    // Filter items for test mode (include #[cfg(test)] items)
    file_ast = husk_semantic::filter_items_by_cfg(&file_ast, &cfg_flags);

    // Discover tests
    let all_tests = discover_tests(&file_ast);

    // Apply filter if provided
    let tests: Vec<_> = if let Some(pattern) = filter {
        all_tests
            .into_iter()
            .filter(|(name, _, _)| name.contains(pattern))
            .collect()
    } else {
        all_tests
    };

    if tests.is_empty() {
        if !quiet {
            println!("running 0 tests");
            println!();
            println!("test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out");
        }
        return;
    }

    if !quiet {
        println!(
            "running {} test{}",
            tests.len(),
            if tests.len() == 1 { "" } else { "s" }
        );
    }

    // Compile to JS with lib mode (don't auto-call main)
    let source_path = Path::new(&path);
    let module = lower_file_to_js_with_source(
        &file_ast,
        false,
        JsTarget::Cjs,
        Some(&content),
        Some(source_path),
        &semantic.name_resolution,
        &semantic.type_resolution,
        &semantic.variant_calls,
        &semantic.variant_patterns,
    );

    // Generate source map for better error messages
    let source_file = source_path
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(&path);
    let (js_code, source_map_json) = module.to_source_with_sourcemap(source_file, &content);

    // Build test harness that runs each test
    let mut harness = String::new();
    harness.push_str(&js_code);
    harness.push_str("\n\n// Test harness\n");
    harness.push_str("const __husk_test_results = [];\n");

    for (test_name, is_ignored, expected_panic) in &tests {
        if *is_ignored {
            harness.push_str(&format!(
                r#"__husk_test_results.push({{ name: "{}", status: "ignored" }});
"#,
                test_name
            ));
        } else if let Some(expected) = expected_panic {
            // Test should panic with expected message
            harness.push_str(&format!(
                r#"try {{
    {}();
    __husk_test_results.push({{ name: "{}", status: "failed", message: "expected panic but test completed normally" }});
}} catch (e) {{
    if (e.message && e.message.includes("{}")) {{
        __husk_test_results.push({{ name: "{}", status: "ok" }});
    }} else {{
        __husk_test_results.push({{ name: "{}", status: "failed", message: "panic message mismatch: " + e.message }});
    }}
}}
"#,
                test_name, test_name, expected, test_name, test_name
            ));
        } else {
            // Normal test - should not panic
            harness.push_str(&format!(
                r#"try {{
    {}();
    __husk_test_results.push({{ name: "{}", status: "ok" }});
}} catch (e) {{
    __husk_test_results.push({{ name: "{}", status: "failed", message: e.stack || e.message }});
}}
"#,
                test_name, test_name, test_name
            ));
        }
    }

    // Print results
    harness.push_str(r#"
let passed = 0, failed = 0, ignored = 0;
for (const r of __husk_test_results) {
    if (r.status === "ok") {
        console.log("test " + r.name + " ... ok");
        passed++;
    } else if (r.status === "ignored") {
        console.log("test " + r.name + " ... ignored");
        ignored++;
    } else {
        console.log("test " + r.name + " ... FAILED");
        failed++;
    }
}
console.log("");
if (failed > 0) {
    console.log("failures:");
    console.log("");
    for (const r of __husk_test_results) {
        if (r.status === "failed") {
            console.log("---- " + r.name + " ----");
            console.log(r.message);
            console.log("");
        }
    }
    console.log("failures:");
    for (const r of __husk_test_results) {
        if (r.status === "failed") {
            console.log("    " + r.name);
        }
    }
    console.log("");
}
console.log("test result: " + (failed === 0 ? "ok" : "FAILED") + ". " + passed + " passed; " + failed + " failed; " + ignored + " ignored; 0 measured; 0 filtered out");
process.exit(failed > 0 ? 1 : 0);
"#);

    // Write harness and source map to temp files for source map support
    let temp_dir = env::temp_dir();
    let test_js_path = temp_dir.join("husk_test.js");
    let test_map_path = temp_dir.join("husk_test.js.map");

    // Append source map reference to harness
    harness.push_str("\n//# sourceMappingURL=husk_test.js.map\n");

    // Write the files
    if let Err(err) = fs::write(&test_js_path, &harness) {
        eprintln!("Failed to write temp test file: {err}");
        std::process::exit(1);
    }
    if let Err(err) = fs::write(&test_map_path, &source_map_json) {
        eprintln!("Failed to write temp source map: {err}");
        std::process::exit(1);
    }

    // Run with Node.js with source map support
    let node_cmd = env::var("HUSK_NODE").unwrap_or_else(|_| "node".to_string());
    let result = ProcessCommand::new(&node_cmd)
        .arg("--enable-source-maps")
        .arg(&test_js_path)
        .stdin(Stdio::null())
        .status();

    match result {
        Ok(status) => {
            if !status.success() {
                std::process::exit(status.code().unwrap_or(1));
            }
        }
        Err(err) => {
            eprintln!("Failed to run tests with node: {err}");
            std::process::exit(1);
        }
    }
}

fn run_dts(action: DtsAction, config: &HuskConfig) {
    match action {
        DtsAction::Add {
            package,
            types,
            output,
        } => run_dts_add(&package, types.as_deref(), output.as_deref(), config),
        DtsAction::Update {
            package,
            legacy,
            follow_imports,
            report,
            report_format,
            report_path,
        } => run_dts_update(
            package.as_deref(),
            !legacy,
            follow_imports,
            report,
            report_format,
            report_path.as_deref(),
            config,
        ),
        DtsAction::List => run_dts_list(config),
        DtsAction::Remove { package } => run_dts_remove(&package),
    }
}

/// Derive a Husk module name from an output file path.
/// - Strips the last extension (e.g., .hk)
/// - Strips a trailing `.gen` if present (for generation-gap files)
/// - Replaces non-identifier characters with '_' and prefixes '_' if needed
/// Derive a Husk module name from an output file path.
///
/// Uses the full file stem (everything before the last extension) as the
/// module name and normalizes it into a valid identifier by replacing
/// non-identifier characters with '_' and prefixing with '_' if needed.
fn file_module_name_from_path(path: &Path) -> Option<String> {
    let stem = path.file_stem()?.to_string_lossy();

    let mut ident: String = stem
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect();

    if ident.is_empty() {
        return None;
    }

    let first = ident.chars().next().unwrap();
    if !first.is_ascii_alphabetic() && first != '_' {
        ident.insert(0, '_');
    }

    Some(ident)
}

fn run_dts_add(package: &str, types: Option<&str>, output: Option<&str>, config: &HuskConfig) {
    // Derive types package name if not provided
    let types_pkg = types
        .map(String::from)
        .unwrap_or_else(|| format!("@types/{}", package.replace('@', "").replace('/', "__")));

    // Get default output directory from config or use "src/extern"
    let default_output_dir = config
        .dts_options
        .as_ref()
        .and_then(|o| o.output_dir.as_deref())
        .unwrap_or("src/extern");

    // Derive output path if not provided
    let output_path = output.map(String::from).unwrap_or_else(|| {
        let safe_name = package.replace('@', "").replace('/', "_").replace('-', "_");
        format!("{}/{}.hk", default_output_dir, safe_name)
    });

    // Check if husk.toml exists, if not create a minimal one
    let toml_path = Path::new("husk.toml");
    let mut doc = if toml_path.exists() {
        let content = fs::read_to_string(toml_path).unwrap_or_default();
        content
            .parse::<toml_edit::DocumentMut>()
            .unwrap_or_else(|e| {
                eprintln!("Failed to parse husk.toml: {e}");
                std::process::exit(1);
            })
    } else {
        toml_edit::DocumentMut::new()
    };

    // Create the dts entry
    let mut entry = toml_edit::Table::new();
    entry.insert("package", toml_edit::value(package));
    entry.insert("types", toml_edit::value(&types_pkg));
    entry.insert("output", toml_edit::value(&output_path));

    // Add to dts array
    if !doc.contains_key("dts") {
        doc.insert(
            "dts",
            toml_edit::Item::ArrayOfTables(toml_edit::ArrayOfTables::new()),
        );
    }
    if let Some(dts_array) = doc.get_mut("dts").and_then(|v| v.as_array_of_tables_mut()) {
        dts_array.push(entry);
    }

    // Write back to file
    if let Err(e) = fs::write(toml_path, doc.to_string()) {
        eprintln!("Failed to write husk.toml: {e}");
        std::process::exit(1);
    }

    println!("Added dts entry for '{package}'");
    println!("  types: {types_pkg}");
    println!("  output: {output_path}");
    println!();
    println!(
        "Run 'npm install {types_pkg}' to install types, then 'huskc dts update' to generate .hk file."
    );
}

fn run_dts_update(
    package: Option<&str>,
    use_oxc: bool,
    follow_imports: bool,
    cli_generate_report: bool,
    report_format: ReportFormat,
    report_path: Option<&str>,
    config: &HuskConfig,
) {
    use husk_dts_parser::{diagnostics, generation_gap, oxc_parser, resolver};

    // Wire generate_report from config (CLI flag overrides to enable)
    let generate_report = cli_generate_report
        || config
            .dts_options
            .as_ref()
            .and_then(|o| o.generate_report)
            .unwrap_or(false);

    // Warn if follow_imports is set with legacy parser
    if follow_imports && !use_oxc {
        eprintln!(
            "Warning: --follow-imports is not supported with --legacy parser; ignoring --follow-imports"
        );
    }

    if config.dts.is_empty() {
        eprintln!("No dts entries configured in husk.toml");
        eprintln!("Use 'huskc dts add <package>' to add one.");
        return;
    }

    let entries: Vec<_> = if let Some(pkg) = package {
        config.dts.iter().filter(|e| e.package == pkg).collect()
    } else {
        config.dts.iter().collect()
    };

    if entries.is_empty() {
        eprintln!("No matching dts entries found");
        return;
    }

    // Get global warn level from config
    let warn_level = config
        .dts_options
        .as_ref()
        .and_then(|o| o.warn_level.as_deref())
        .unwrap_or("all");
    let show_warnings = warn_level != "none";
    let verbose_warnings = warn_level == "all";

    // Diagnostic collector for report generation
    let mut diagnostics_collector = if generate_report {
        Some(diagnostics::DiagnosticCollector::new())
    } else {
        None
    };

    // Aggregate codegen metrics for report
    let mut aggregate_metrics = if generate_report {
        Some(diagnostics::CodegenMetrics::new())
    } else {
        None
    };

    for entry in entries {
        println!("Updating {}", entry.package);

        // Get base directory for this entry (per-entry or project root)
        let entry_base_dir = entry
            .base_dir
            .as_ref()
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("."));

        // Find the .d.ts file - prefer explicit types_path if specified
        let dts_path: String = if let Some(ref explicit_path) = entry.types_path {
            // Use explicit path (relative to entry base_dir)
            let full_path = entry_base_dir.join(explicit_path);
            if !full_path.exists() {
                eprintln!("  Explicit types_path not found: {}", full_path.display());
                continue;
            }
            full_path.to_string_lossy().into_owned()
        } else {
            // Automatic discovery
            // Build the @types package name: use explicit entry.types or synthesize @types/<package>
            let types_pkg = entry
                .types
                .clone()
                .unwrap_or_else(|| format!("@types/{}", entry.package));

            let dts_paths = [
                // @types package (e.g., @types/express)
                entry_base_dir.join(format!("node_modules/{}/index.d.ts", types_pkg)),
                // Bundled types in main package
                entry_base_dir.join(format!("node_modules/{}/index.d.ts", entry.package)),
                entry_base_dir.join(format!("node_modules/{}/dist/index.d.ts", entry.package)),
            ];

            match dts_paths.iter().find(|p| p.exists()) {
                Some(p) => p.to_string_lossy().into_owned(),
                None => {
                    eprintln!("  Could not find .d.ts file for {}", entry.package);
                    eprintln!("  Tried: {:?}", dts_paths);
                    eprintln!(
                        "  Make sure the types package is installed: npm install {}",
                        types_pkg
                    );
                    continue;
                }
            }
        };
        let dts_path = &dts_path;

        // Parse .d.ts using either legacy or Oxc parser
        // Also collect resolved module when follow_imports is enabled for multi-file codegen
        // We also track module identity for CommonJS modules with `export =`
        //
        // Automatically use oxc parser if follow_imports is enabled for this entry
        let should_use_oxc = use_oxc || entry.follow_imports.unwrap_or(false);
        let should_follow_imports = follow_imports || entry.follow_imports.unwrap_or(false);

        let (dts_file, resolved_module, module_identity): (
            DtsFile,
            Option<resolver::ResolvedModule>,
            Option<resolver::ModuleIdentity>,
        ) = if should_use_oxc {
            // Use Oxc parser
            if should_follow_imports {
                // Use resolver for multi-file resolution
                // Get module paths from global config or use default, resolved relative to entry base_dir
                let module_paths = config
                    .dts_options
                    .as_ref()
                    .and_then(|opts| opts.node_modules_paths.as_ref())
                    .map(|paths| paths.iter().map(|p| entry_base_dir.join(p)).collect())
                    .unwrap_or_else(|| vec![entry_base_dir.join("node_modules")]);

                // Get max depth from per-entry config or use default
                let max_depth = entry.max_import_depth.or(Some(10));

                let options = resolver::ResolveOptions {
                    base_dir: Some(entry_base_dir.clone()),
                    module_paths,
                    follow_references: true,
                    max_depth,
                };
                let mut resolver = resolver::Resolver::new(options);
                let resolved = resolver.resolve(Path::new(dts_path));

                // Report resolution errors
                for err in &resolved.errors {
                    eprintln!("  Resolution error: {}", err);
                }

                // Get the module identity before we extract the entry file
                let identity = resolved.get_module_identity(&resolved.entry_path);
                let identity = if identity == resolver::ModuleIdentity::StandardModule {
                    None
                } else {
                    Some(identity)
                };

                // Get the entry file (we still need it for diagnostics in single-file mode)
                match resolved.files.get(&resolved.entry_path) {
                    Some(f) => (f.clone(), Some(resolved), identity),
                    None => {
                        eprintln!("  Failed to resolve {}", dts_path);
                        continue;
                    }
                }
            } else {
                // Single-file parsing with Oxc
                let dts_content = match fs::read_to_string(dts_path) {
                    Ok(c) => c,
                    Err(e) => {
                        eprintln!("  Failed to read {}: {e}", dts_path);
                        continue;
                    }
                };

                match oxc_parser::parse_with_oxc(&dts_content, dts_path) {
                    Ok(f) => (f, None, None),
                    Err(e) => {
                        eprintln!("  Failed to parse {} with Oxc: {}", dts_path, e.message);
                        continue;
                    }
                }
            }
        } else {
            // Use legacy parser (no follow_imports or module identity support)
            let dts_content = match fs::read_to_string(dts_path) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("  Failed to read {}: {e}", dts_path);
                    continue;
                }
            };

            match parse_dts(&dts_content) {
                Ok(f) => (f, None, None),
                Err(e) => {
                    eprintln!("  Failed to parse {}: {e}", dts_path);
                    continue;
                }
            }
        };

        // Collect diagnostics if generating report
        if let Some(ref mut collector) = diagnostics_collector {
            // If we have resolved module (from follow_imports), process all files
            if let Some(ref resolved) = resolved_module {
                for (path, file) in &resolved.files {
                    collector.process_file(file, path.to_string_lossy().as_ref());
                }
            } else {
                // Single file mode - just process the entry file
                collector.process_file(&dts_file, dts_path);
            }
        }

        // Generate Husk code
        // Parse union strategy from config (per-entry or global default)
        let union_strategy = entry
            .union_strategy
            .as_deref()
            .or(config.dts_options.as_ref().and_then(|o| o.default_union_strategy.as_deref()))
            .map(|s| match s {
                "enum" => UnionStrategy::Enum,
                "jsvalue" => UnionStrategy::JsValue,
                "auto" => UnionStrategy::Auto,
                other => {
                    eprintln!(
                        "Warning: unknown union_strategy '{}', using 'auto'. Valid values: enum, jsvalue, auto",
                        other
                    );
                    UnionStrategy::Auto
                }
            })
            .unwrap_or(UnionStrategy::Auto);

        // When follow_imports is enabled, use entry_file_only mode to avoid
        // polluting the output with types from all dependencies
        let follow_imports_enabled = entry.follow_imports.unwrap_or(false);

        let options = DtsCodegenOptions {
            module_name: Some(entry.package.clone()),
            file_module_name: file_module_name_from_path(Path::new(&entry.output)),
            verbose: env::var("HUSKC_DEBUG").map(|v| v == "1").unwrap_or(false),
            module_identity,
            union_strategy,
            generate_builders: entry
                .generate_builders
                .or(config
                    .dts_options
                    .as_ref()
                    .and_then(|o| o.default_generate_builders))
                .unwrap_or(false),
            builder_min_optional: entry.builder_min_optional.unwrap_or(3),
            expand_utility_types: entry
                .expand_utility_types
                .or(config
                    .dts_options
                    .as_ref()
                    .and_then(|o| o.default_expand_utility_types))
                .unwrap_or(true),
            use_extern_const: entry
                .use_extern_const
                .or(config
                    .dts_options
                    .as_ref()
                    .and_then(|o| o.default_use_extern_const))
                .unwrap_or(false),
            // Enable entry_file_only when following imports to avoid dependency pollution
            entry_file_only: follow_imports_enabled,
            // Pass through include/exclude filters for AST-level filtering
            include: entry.include.clone().unwrap_or_default(),
            exclude: entry.exclude.clone().unwrap_or_default(),
        };

        // Generate code from resolved module (multi-file) or single file
        let result = if let Some(ref resolved) = resolved_module {
            generate_husk_from_module(resolved, &options)
        } else {
            generate_husk(&dts_file, &options)
        };

        // Aggregate codegen metrics for report
        if let Some(ref mut metrics) = aggregate_metrics {
            metrics.aggregate(&result.metrics);
        }

        // Note: include/exclude filtering is now done at codegen level via
        // the should_emit_type() helper, which properly filters both struct
        // declarations and their impl blocks.

        // Determine if generation gap pattern is enabled (per-entry or global, default true)
        let use_generation_gap = entry
            .generation_gap
            .or(config.dts_options.as_ref().and_then(|o| o.generation_gap))
            .unwrap_or(true);

        // Write output using generation gap pattern
        let output_path = Path::new(&entry.output);
        let writer = generation_gap::GenerationGapWriter::new(use_generation_gap);

        match writer.write(output_path, &result.code) {
            Ok(write_result) => {
                if use_generation_gap {
                    println!("  Generated {}", write_result.gen_path.display());
                    if write_result.wrapper_created {
                        println!(
                            "  Created wrapper {}",
                            write_result.wrapper_path.as_ref().unwrap().display()
                        );
                    }
                } else {
                    println!("  Generated {}", write_result.gen_path.display());
                }
            }
            Err(e) => {
                eprintln!("  Failed to write {}: {e}", entry.output);
                continue;
            }
        }

        // Print warnings based on warn_level
        if show_warnings && !result.warnings.is_empty() {
            if verbose_warnings {
                for warning in &result.warnings {
                    eprintln!("  warning: {}", warning.message);
                }
            } else {
                println!(
                    "  {} warnings (use warn_level = \"all\" to see details)",
                    result.warnings.len()
                );
            }
        }
    }

    // Generate diagnostic report if requested
    if let Some(collector) = diagnostics_collector {
        let (report_content, default_path) = match report_format {
            ReportFormat::Markdown => {
                // Pre-codegen analysis report
                let mut report = collector.generate_report("DTS Conversion Report");

                // Append actual codegen metrics if available
                if let Some(metrics) = aggregate_metrics {
                    report.push_str("\n\n---\n\n");
                    report.push_str(&metrics.to_markdown("Actual Codegen Results"));
                }

                (report, "dts-report.md")
            }
            ReportFormat::Json => {
                // JSON report format - use only actual codegen metrics
                let report = if let Some(metrics) = aggregate_metrics {
                    metrics.to_json()
                } else {
                    // Empty metrics if no codegen happened
                    diagnostics::CodegenMetrics::new().to_json()
                };

                (report, "dts-report.json")
            }
        };

        let output_path = report_path.unwrap_or(default_path);
        match fs::write(output_path, &report_content) {
            Ok(()) => println!("\nGenerated {}", output_path),
            Err(e) => eprintln!("\nFailed to write report: {e}"),
        }
    }
}

fn run_dts_list(config: &HuskConfig) {
    // Display project info if available
    if let Some(ref pkg) = config.package {
        println!("Project: {} v{}", pkg.name, pkg.version);
        if let Some(ref desc) = pkg.description {
            println!("  {}", desc);
        }
        if let Some(ref authors) = pkg.authors {
            println!("  Authors: {}", authors.join(", "));
        }
        if let Some(ref license) = pkg.license {
            println!("  License: {}", license);
        }
        if let Some(ref repo) = pkg.repository {
            println!("  Repository: {}", repo);
        }
        println!();
    }

    if config.dts.is_empty() {
        println!("No dts entries configured in husk.toml");
        println!("Use 'huskc dts add <package>' to add one.");
        return;
    }

    println!("Configured dts entries:");
    println!();
    for entry in &config.dts {
        let types = entry.types.as_deref().unwrap_or("-");
        let version = entry.version.as_deref().unwrap_or("-");
        println!(
            "  {} (types: {}, version: {})",
            entry.package, types, version
        );
        println!("    -> {}", entry.output);
        if let Some(ref include) = entry.include {
            println!("    include: {:?}", include);
        }
        if let Some(ref exclude) = entry.exclude {
            println!("    exclude: {:?}", exclude);
        }
    }
}

fn run_dts_remove(package: &str) {
    let toml_path = Path::new("husk.toml");
    if !toml_path.exists() {
        eprintln!("No husk.toml found");
        return;
    }

    let content = fs::read_to_string(toml_path).unwrap_or_default();
    let mut doc = match content.parse::<toml_edit::DocumentMut>() {
        Ok(d) => d,
        Err(e) => {
            eprintln!("Failed to parse husk.toml: {e}");
            std::process::exit(1);
        }
    };

    let mut removed = false;
    if let Some(dts_array) = doc.get_mut("dts").and_then(|v| v.as_array_of_tables_mut()) {
        let mut i = 0;
        while i < dts_array.len() {
            if let Some(pkg) = dts_array
                .get(i)
                .and_then(|t| t.get("package"))
                .and_then(|v| v.as_str())
            {
                if pkg == package {
                    dts_array.remove(i);
                    removed = true;
                    continue;
                }
            }
            i += 1;
        }
    }

    if removed {
        if let Err(e) = fs::write(toml_path, doc.to_string()) {
            eprintln!("Failed to write husk.toml: {e}");
            std::process::exit(1);
        }
        println!("Removed dts entry for '{package}'");
    } else {
        eprintln!("No dts entry found for '{package}'");
    }
}

// ============================================================================
// Format command
// ============================================================================

fn run_fmt(paths: &[PathBuf], check: bool, print: bool, max_width: usize, indent_size: usize) {
    let config = husk_fmt::FormatConfig::default()
        .with_line_length(max_width)
        .with_indent_size(indent_size);

    let files = match discover_husk_files(paths) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Error discovering files: {e}");
            std::process::exit(2);
        }
    };

    if files.is_empty() {
        eprintln!("No .hk files found");
        std::process::exit(0);
    }

    let mut unformatted = Vec::new();
    let mut had_errors = false;

    for file in &files {
        let source = match fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error reading {}: {e}", file.display());
                had_errors = true;
                continue;
            }
        };

        let formatted = match husk_fmt::format_str(&source, &config) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Error formatting {}: {e}", file.display());
                had_errors = true;
                continue;
            }
        };

        if source != formatted {
            if check {
                unformatted.push(file.clone());
            } else if print {
                println!("{formatted}");
            } else {
                // Atomic write: write to temp file then rename
                let temp = file.with_extension("hk.tmp");
                if let Err(e) = fs::write(&temp, &formatted) {
                    eprintln!("Error writing {}: {e}", temp.display());
                    had_errors = true;
                    continue;
                }
                if let Err(e) = fs::rename(&temp, file) {
                    eprintln!(
                        "Error renaming {} to {}: {e}",
                        temp.display(),
                        file.display()
                    );
                    // Try to clean up temp file
                    let _ = fs::remove_file(&temp);
                    had_errors = true;
                    continue;
                }
                debug_log(&format!("[huskc fmt] formatted {}", file.display()));
            }
        }
    }

    if had_errors {
        std::process::exit(2);
    }

    if check && !unformatted.is_empty() {
        eprintln!("The following files need formatting:");
        for f in &unformatted {
            eprintln!("  {}", f.display());
        }
        std::process::exit(1);
    }

    if !check && !print {
        println!(
            "Formatted {} file{}",
            files.len(),
            if files.len() == 1 { "" } else { "s" }
        );
    }
}

/// Discover all .hk files in the given paths.
fn discover_husk_files(paths: &[PathBuf]) -> Result<Vec<PathBuf>, std::io::Error> {
    let mut files = Vec::new();
    let excluded_dirs: HashSet<&str> = ["node_modules", "target", ".git", "dist"]
        .into_iter()
        .collect();

    for path in paths {
        if path.is_file() {
            if path.extension().is_some_and(|e| e == "hk") {
                files.push(path.clone());
            }
        } else if path.is_dir() {
            discover_husk_files_recursive(path, &excluded_dirs, &mut files)?;
        }
    }

    Ok(files)
}

fn discover_husk_files_recursive(
    dir: &Path,
    excluded: &HashSet<&str>,
    files: &mut Vec<PathBuf>,
) -> Result<(), std::io::Error> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

        if path.is_dir() {
            if !excluded.contains(file_name) && !file_name.starts_with('.') {
                discover_husk_files_recursive(&path, excluded, files)?;
            }
        } else if path.extension().is_some_and(|e| e == "hk") {
            files.push(path);
        }
    }
    Ok(())
}
