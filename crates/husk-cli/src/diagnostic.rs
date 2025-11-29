//! Rich error reporting using codespan-reporting.

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::ops::Range;

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
        let diagnostic = Diagnostic::error()
            .with_message(message)
            .with_labels(vec![
                Label::primary(self.file_id, span).with_message("error occurs here")
            ]);

        self.emit_diagnostic(&diagnostic);
    }

    /// Report a semantic error with source context.
    pub fn report_semantic_error(&self, message: &str, span: Range<usize>) {
        let diagnostic = Diagnostic::error()
            .with_message(message)
            .with_labels(vec![Label::primary(self.file_id, span)]);

        self.emit_diagnostic(&diagnostic);
    }

    fn emit_diagnostic(&self, diagnostic: &Diagnostic<usize>) {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = term::Config::default();
        let _ = term::emit(&mut writer.lock(), &config, &self.files, diagnostic);
    }
}
