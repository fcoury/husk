#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }

    pub fn line_number(&self, code: &str) -> usize {
        // code[..self.start].chars().filter(|&c| c == '\n').count() + 1
        code[..self.start].chars().filter(|&c| c == '\n').count()
    }

    pub fn column_number(&self, code: &str) -> usize {
        let line_start = code[..self.start].rfind('\n').map_or(0, |pos| pos + 1);
        self.start - line_start + 1
    }

    pub fn to_line_column(self, code: &str) -> (usize, usize) {
        (self.line_number(code), self.column_number(code))
    }

    pub fn pretty_print(&self, code: &str) -> String {
        let code: String = code.into();
        let code_chars: Vec<char> = code.chars().collect();

        // Validate span bounds
        if self.start > code_chars.len() || self.end > code_chars.len() || self.start > self.end {
            return format!(
                "<invalid span: start={}, end={}, code_len={}>",
                self.start,
                self.end,
                code_chars.len()
            );
        }

        // Find the start of the line containing the span
        let line_start = code_chars[..self.start]
            .iter()
            .rposition(|&c| c == '\n')
            .map_or(0, |pos| pos + 1);

        // Find the end of the line containing the span
        let line_end = code_chars[self.end..]
            .iter()
            .position(|&c| c == '\n')
            .map_or(code_chars.len(), |pos| self.end + pos);

        // Extract the line and prepare the arrow
        let line: String = code_chars[line_start..line_end].iter().collect();
        let arrow_start = self.start - line_start;
        let arrow_end = self.end - line_start;
        let arrow = format!("{}{}", " ".repeat(arrow_start), "^".repeat(arrow_end - arrow_start));

        format!("{}\n{}", line, arrow)
    }

    pub fn location(&self, code: impl Into<String>) -> String {
        let code = code.into();
        format!(
            "line {}, column {}",
            self.line_number(&code),
            self.column_number(&code)
        )
    }

    pub fn default() -> Span {
        Span { start: 0, end: 0 }
    }
}
