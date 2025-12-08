//! Rich error reporting using codespan-reporting.

use crate::load::LoadError;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use husk_ast::Span;
use std::collections::HashMap;
use std::ops::Range;

/// Emit a diagnostic to stderr using the given file database.
fn emit_diagnostic_to_stderr(
    files: &SimpleFiles<String, String>,
    diagnostic: &Diagnostic<usize>,
) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();
    let _ = term::emit(&mut writer.lock(), &config, files, diagnostic);
}

/// A source file database for codespan-reporting.
pub struct SourceDb {
    files: SimpleFiles<String, String>,
    file_id: usize,
}

impl SourceDb {
    /// Create a new source database with a single file.
    pub fn new(name: String, source: String) -> Self {
        let mut files = SimpleFiles::new();
        let file_id = files.add(name, source);
        Self { files, file_id }
    }

    /// Report a parse error with source context.
    pub fn report_parse_error(&self, message: &str, span: Range<usize>) {
        let diagnostic = Diagnostic::error().with_message(message).with_labels(vec![
            Label::primary(self.file_id, span).with_message("error occurs here"),
        ]);

        emit_diagnostic_to_stderr(&self.files, &diagnostic);
    }
}

/// A multi-file source database for reporting errors across multiple files.
/// This is used when module assembly combines code from different source files.
pub struct MultiFileSourceDb {
    files: SimpleFiles<String, String>,
    /// Maps file path to file ID in the database
    file_ids: HashMap<String, usize>,
    /// The default file ID (for spans without a file path)
    default_file_id: usize,
}

impl MultiFileSourceDb {
    /// Create a new multi-file source database with a default file.
    /// Additional files can be added with `add_file`.
    pub fn new(default_name: String, default_source: String) -> Self {
        let mut files = SimpleFiles::new();
        let default_file_id = files.add(default_name.clone(), default_source);
        let mut file_ids = HashMap::new();
        file_ids.insert(default_name, default_file_id);
        Self {
            files,
            file_ids,
            default_file_id,
        }
    }

    /// Add a source file to the database.
    pub fn add_file(&mut self, name: String, source: String) {
        let file_id = self.files.add(name.clone(), source);
        self.file_ids.insert(name, file_id);
    }

    /// Get the file ID for a given span.
    /// Returns the default file ID if the span has no file path or the file is not found.
    fn file_id_for_span(&self, span: &Span) -> usize {
        if let Some(file_path) = span.file_path() {
            self.file_ids.get(file_path).copied().unwrap_or(self.default_file_id)
        } else {
            self.default_file_id
        }
    }

    /// Report a semantic error with source context.
    /// Uses the span's file path to determine which file to display.
    pub fn report_semantic_error(&self, message: &str, span: &Span) {
        let file_id = self.file_id_for_span(span);
        let diagnostic = Diagnostic::error()
            .with_message(message)
            .with_labels(vec![Label::primary(file_id, span.range.clone())]);

        emit_diagnostic_to_stderr(&self.files, &diagnostic);
    }
}

/// Create a MultiFileSourceDb from a ModuleGraph and the entry file's source.
/// This is useful for error reporting after module assembly.
pub fn multi_file_db_from_graph(
    entry_path: &str,
    entry_source: &str,
    graph: &crate::load::ModuleGraph,
) -> MultiFileSourceDb {
    let mut db = MultiFileSourceDb::new(entry_path.to_string(), entry_source.to_string());
    for module in graph.modules.values() {
        let file_path: &str = &module.file_path;
        // Only add if it's not the entry file (already added as default)
        if file_path != entry_path {
            db.add_file(file_path.to_string(), module.source.clone());
        }
    }
    db
}

/// Report a LoadError with pretty formatting for parse errors.
pub fn report_load_error(err: &LoadError) {
    match err {
        LoadError::Parse {
            path,
            source_code,
            errors,
        } => {
            let source_db = SourceDb::new(path.clone(), source_code.clone());
            for e in errors {
                source_db.report_parse_error(&e.message, e.span.range.clone());
            }
        }
        _ => eprintln!("{err}"),
    }
}
