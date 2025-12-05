//! Document state management for the LSP server.
//!
//! This module handles storing document content and provides utilities for
//! converting between byte offsets and LSP positions (line:column).

use tower_lsp::lsp_types::Position;

/// A document being tracked by the LSP server.
#[derive(Debug, Clone)]
pub struct Document {
    text: String,
    version: i32,
    /// Byte offsets where each line starts (including line 0 at offset 0).
    line_offsets: Vec<usize>,
}

impl Document {
    /// Create a new document with the given content.
    pub fn new(text: String, version: i32) -> Self {
        let line_offsets = compute_line_offsets(&text);
        Self {
            text,
            version,
            line_offsets,
        }
    }

    /// Get the document text.
    pub fn text(&self) -> String {
        self.text.clone()
    }

    /// Get the document version.
    #[allow(dead_code)]
    pub fn version(&self) -> i32 {
        self.version
    }

    /// Convert an LSP position (line, character) to a byte offset.
    pub fn position_to_offset(&self, position: Position) -> usize {
        let line = position.line as usize;
        let character = position.character as usize;

        if line >= self.line_offsets.len() {
            return self.text.len();
        }

        let line_start = self.line_offsets[line];
        let line_end = if line + 1 < self.line_offsets.len() {
            self.line_offsets[line + 1]
        } else {
            self.text.len()
        };

        // LSP uses UTF-16 code units for character offset
        // For simplicity, we treat it as byte offset (works for ASCII)
        // TODO: Proper UTF-16 handling
        let offset = line_start + character;
        offset.min(line_end).min(self.text.len())
    }

    /// Convert a byte offset to an LSP position (line, character).
    pub fn offset_to_position(&self, offset: usize) -> Position {
        let offset = offset.min(self.text.len());

        // Binary search to find the line
        let line = match self.line_offsets.binary_search(&offset) {
            Ok(line) => line,
            Err(line) => line.saturating_sub(1),
        };

        let line_start = self.line_offsets.get(line).copied().unwrap_or(0);
        let character = offset.saturating_sub(line_start);

        Position {
            line: line as u32,
            character: character as u32,
        }
    }

    /// Get the word at a given byte offset.
    pub fn word_at_offset(&self, offset: usize) -> Option<String> {
        self.word_span_at_offset(offset).map(|(word, _, _)| word)
    }

    /// Get the word and its span at a given byte offset.
    /// Returns (word, start_offset, end_offset) if found.
    pub fn word_span_at_offset(&self, offset: usize) -> Option<(String, usize, usize)> {
        if offset >= self.text.len() {
            return None;
        }

        let bytes = self.text.as_bytes();

        // If we're not on an identifier character, return None
        if !is_ident_char(bytes[offset]) {
            return None;
        }

        // Find the start of the word
        let mut start = offset;
        while start > 0 && is_ident_char(bytes[start - 1]) {
            start -= 1;
        }

        // Find the end of the word
        let mut end = offset;
        while end < bytes.len() && is_ident_char(bytes[end]) {
            end += 1;
        }

        if start == end {
            return None;
        }

        Some((self.text[start..end].to_string(), start, end))
    }
}

/// Compute the byte offset of each line start.
fn compute_line_offsets(text: &str) -> Vec<usize> {
    let mut offsets = vec![0];

    for (i, byte) in text.bytes().enumerate() {
        if byte == b'\n' {
            offsets.push(i + 1);
        }
    }

    offsets
}

/// Check if a byte is a valid identifier character.
fn is_ident_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_conversion() {
        let doc = Document::new("fn main() {\n    println!(\"hello\");\n}\n".to_string(), 1);

        // Line 0, character 0 -> offset 0
        assert_eq!(
            doc.position_to_offset(Position {
                line: 0,
                character: 0
            }),
            0
        );

        // Line 0, character 3 -> offset 3 (after "fn ")
        assert_eq!(
            doc.position_to_offset(Position {
                line: 0,
                character: 3
            }),
            3
        );

        // Line 1, character 0 -> start of line 1
        assert_eq!(
            doc.position_to_offset(Position {
                line: 1,
                character: 0
            }),
            12
        );

        // Offset 0 -> line 0, char 0
        assert_eq!(
            doc.offset_to_position(0),
            Position {
                line: 0,
                character: 0
            }
        );

        // Offset 3 -> line 0, char 3
        assert_eq!(
            doc.offset_to_position(3),
            Position {
                line: 0,
                character: 3
            }
        );

        // Offset 12 -> line 1, char 0
        assert_eq!(
            doc.offset_to_position(12),
            Position {
                line: 1,
                character: 0
            }
        );
    }

    #[test]
    fn test_word_at_offset() {
        let doc = Document::new("fn main() {\n    let x = 42;\n}\n".to_string(), 1);

        // "fn" at offset 0
        assert_eq!(doc.word_at_offset(0), Some("fn".to_string()));
        assert_eq!(doc.word_at_offset(1), Some("fn".to_string()));

        // "main" at offset 3
        assert_eq!(doc.word_at_offset(3), Some("main".to_string()));
        assert_eq!(doc.word_at_offset(5), Some("main".to_string()));

        // "let" at line 1
        assert_eq!(doc.word_at_offset(16), Some("let".to_string()));

        // Character 2 is 'n' in 'fn', part of the word
        assert_eq!(doc.word_at_offset(2), None); // offset 2 is space after 'fn'
    }
}
