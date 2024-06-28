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

    pub fn pretty_print(&self, code: &str) -> String {
        let code: String = code.into();
        let code_chars: Vec<char> = code.chars().collect();

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
        let arrow = " ".repeat(arrow_start) + &"^".repeat(arrow_end - arrow_start);

        format!("{}\n{}", line, arrow)
    }

    pub fn default() -> Span {
        Span { start: 0, end: 0 }
    }
}
