use std::fs;
use std::path::Path;

use husk_dts_parser::DtsDiagnostics;

pub fn write_dts_report(output_path: &str, diag: &DtsDiagnostics) -> std::io::Result<()> {
    let path = Path::new(output_path);
    let report_path = path.with_extension("dts-report.md");

    let mut content = String::new();
    content.push_str("# huskc dts report\n\n");
    if diag.warnings.is_empty() {
        content.push_str("No warnings.\n");
    } else {
        content.push_str("## Warnings\n\n");
        for w in &diag.warnings {
            content.push_str("- ");
            content.push_str(w);
            content.push('\n');
        }
    }

    fs::write(report_path, content)
}
