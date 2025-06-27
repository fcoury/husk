use std::fmt;
use std::io::Write;

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use thiserror::Error;

use crate::span::Span;

#[derive(Error, Debug)]
pub enum Error {
    Semantic(String, Span),
    Parse(String, Span),
    Runtime(String, Span),
    Transpiler(String, Span),
    Config(String),
}

#[allow(dead_code)]
impl Error {
    pub fn new_semantic(message: impl Into<String>, span: Span) -> Self {
        Error::Semantic(message.into(), span)
    }

    pub fn new_parse(message: impl Into<String>, span: Span) -> Self {
        Error::Parse(message.into(), span)
    }

    pub fn new_runtime(message: impl Into<String>, span: Span) -> Self {
        Error::Runtime(message.into(), span)
    }

    pub fn new_transpile(message: impl Into<String>, span: Span) -> Self {
        Error::Transpiler(message.into(), span)
    }

    pub fn new_config(message: impl Into<String>) -> Self {
        Error::Config(message.into())
    }

    pub fn pretty_print(&self, code: impl Into<String>) -> String {
        match self {
            Error::Semantic(message, span) => pretty_print(code, message, span),
            Error::Parse(message, span) => pretty_print(code, message, span),
            Error::Runtime(message, span) => pretty_print(code, message, span),
            Error::Transpiler(message, span) => pretty_print(code, message, span),
            Error::Config(message) => format!("Configuration Error: {}", message),
        }
    }

    pub fn pretty_print_colored(&self, code: impl Into<String>, no_color: bool) -> String {
        // Use a string buffer to build the colored output
        let mut buffer = Vec::new();
        self.write_colored(&mut buffer, code, no_color).unwrap();
        String::from_utf8(buffer).unwrap()
    }

    pub fn write_colored<W: Write>(
        &self,
        _writer: &mut W,
        code: impl Into<String>,
        no_color: bool,
    ) -> std::io::Result<()> {
        let code = code.into();

        // Determine color choice
        let color_choice = if no_color {
            ColorChoice::Never
        } else if atty::is(atty::Stream::Stderr) {
            ColorChoice::Auto
        } else {
            ColorChoice::Never
        };

        // Only use color if we're writing to a TTY
        if color_choice == ColorChoice::Auto {
            let mut stderr = StandardStream::stderr(color_choice);

            // Write error type in red
            stderr.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
            write!(&mut stderr, "error")?;
            stderr.reset()?;

            // Write location info
            if let Some(span) = self.span() {
                write!(&mut stderr, ": ")?;
                stderr.set_color(ColorSpec::new().set_bold(true))?;
                write!(
                    &mut stderr,
                    "{}:{}",
                    span.line_number(&code),
                    span.column_number(&code)
                )?;
                stderr.reset()?;
            }

            // Write message
            write!(&mut stderr, " - {}", self.message())?;

            // Write code snippet with underline
            if let Some(span) = self.span() {
                writeln!(&mut stderr)?;
                let snippet = span.pretty_print(&code);

                // Split the snippet into lines
                let lines: Vec<&str> = snippet.lines().collect();

                // Write the code line
                if !lines.is_empty() {
                    writeln!(&mut stderr, "{}", lines[0])?;
                }

                // Write the underline in red
                if lines.len() > 1 {
                    stderr.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
                    writeln!(&mut stderr, "{}", lines[1])?;
                    stderr.reset()?;
                }
            } else {
                writeln!(&mut stderr)?;
            }
        } else {
            // No color - just print plain text
            eprintln!("{}", self.pretty_print(&code));
        }

        Ok(())
    }

    fn span(&self) -> Option<&Span> {
        match self {
            Error::Semantic(_, span) => Some(span),
            Error::Parse(_, span) => Some(span),
            Error::Runtime(_, span) => Some(span),
            Error::Transpiler(_, span) => Some(span),
            Error::Config(_) => None,
        }
    }

    fn message(&self) -> &str {
        match self {
            Error::Semantic(message, _) => message,
            Error::Parse(message, _) => message,
            Error::Runtime(message, _) => message,
            Error::Transpiler(message, _) => message,
            Error::Config(message) => message,
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Semantic(message, span) => {
                write!(f, "Semantic error: {} at {:?}", message, span)
            }
            Error::Parse(message, span) => write!(f, "Parse error: {} at {:?}", message, span),
            Error::Runtime(message, span) => write!(f, "Runtime error: {} at {:?}", message, span),
            Error::Transpiler(message, span) => {
                write!(f, "Transpiler error: {} at {:?}", message, span)
            }
            Error::Config(message) => write!(f, "Configuration error: {}", message),
        }
    }
}

fn pretty_print(code: impl Into<String>, message: impl Into<String>, span: &Span) -> String {
    let code: String = code.into();
    let message: String = message.into();
    format!(
        "error: {}:{} - {}\n{}",
        span.line_number(&code),
        span.column_number(&code),
        message,
        span.pretty_print(&code)
    )
}
