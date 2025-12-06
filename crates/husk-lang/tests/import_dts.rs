//! Tests for the `huskc import-dts` command.

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use tempfile::tempdir;

fn workspace_root() -> PathBuf {
    let mut dir = std::env::current_dir().expect("failed to read current dir");
    let mut found: Option<PathBuf> = None;
    loop {
        if dir.join("Cargo.toml").exists() {
            found = Some(dir.clone());
        }
        if !dir.pop() {
            break;
        }
    }
    found.expect("failed to resolve workspace root from current dir")
}

fn huskc_command() -> Command {
    let root = workspace_root();
    let mut cmd = Command::new("cargo");
    cmd.args(["run", "-q", "-p", "husk-lang", "--"]);
    cmd.current_dir(root);
    cmd
}

#[test]
fn import_dts_basic_function() {
    let dir = tempdir().unwrap();
    let dts_path = dir.path().join("test.d.ts");
    fs::write(&dts_path, "declare function greet(name: string): void;").unwrap();

    let output = huskc_command()
        .arg("import-dts")
        .arg(&dts_path)
        .output()
        .unwrap();

    assert!(output.status.success(), "command failed: {:?}", output);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("fn greet(name: String)"),
        "expected 'fn greet(name: String)' in output, got:\n{}",
        stdout
    );
}

#[test]
fn import_dts_with_output_file() {
    let dir = tempdir().unwrap();
    let dts_path = dir.path().join("api.d.ts");
    fs::write(&dts_path, "declare function fetch(url: string): Promise<any>;").unwrap();

    let out_path = dir.path().join("api.hk");
    let status = huskc_command()
        .arg("import-dts")
        .arg(&dts_path)
        .args(["-o", out_path.to_str().unwrap()])
        .status()
        .unwrap();

    assert!(status.success(), "command failed");
    assert!(out_path.exists(), "output file should exist");

    let content = fs::read_to_string(&out_path).unwrap();
    assert!(
        content.contains("fn fetch(url: String)"),
        "expected 'fn fetch(url: String)' in output file, got:\n{}",
        content
    );
}

#[test]
fn import_dts_with_module_flag() {
    let dir = tempdir().unwrap();
    let dts_path = dir.path().join("express.d.ts");
    fs::write(
        &dts_path,
        r#"
declare function express(): Express;
interface Express {
    listen(port: number): void;
}
"#,
    )
    .unwrap();

    let out_path = dir.path().join("express.hk");
    let status = huskc_command()
        .arg("import-dts")
        .arg(&dts_path)
        .args(["-o", out_path.to_str().unwrap()])
        .args(["-m", "express"])
        .status()
        .unwrap();

    assert!(status.success(), "command failed");

    let content = fs::read_to_string(&out_path).unwrap();
    assert!(
        content.contains("mod express;"),
        "expected 'mod express;' in output file, got:\n{}",
        content
    );
}

#[test]
fn import_dts_file_not_found() {
    let output = huskc_command()
        .arg("import-dts")
        .arg("/nonexistent/path/to/file.d.ts")
        .output()
        .unwrap();

    assert!(
        !output.status.success(),
        "command should fail for missing file"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Failed to read"),
        "expected 'Failed to read' in stderr, got:\n{}",
        stderr
    );
}

#[test]
fn import_dts_interface_generates_impl_block() {
    let dir = tempdir().unwrap();
    let dts_path = dir.path().join("response.d.ts");
    fs::write(
        &dts_path,
        r#"
interface Response {
    status(code: number): Response;
    json(data: any): void;
    send(body: string): void;
}
"#,
    )
    .unwrap();

    let output = huskc_command()
        .arg("import-dts")
        .arg(&dts_path)
        .output()
        .unwrap();

    assert!(output.status.success(), "command failed: {:?}", output);
    let stdout = String::from_utf8_lossy(&output.stdout);

    // Should generate struct and impl block
    assert!(
        stdout.contains("struct Response;"),
        "expected 'struct Response;' in output, got:\n{}",
        stdout
    );
    assert!(
        stdout.contains("impl Response"),
        "expected 'impl Response' in output, got:\n{}",
        stdout
    );
    assert!(
        stdout.contains("fn status(self, code: f64) -> Response"),
        "expected method signature in output, got:\n{}",
        stdout
    );
}

#[test]
fn import_dts_class_with_methods() {
    let dir = tempdir().unwrap();
    let dts_path = dir.path().join("database.d.ts");
    fs::write(
        &dts_path,
        r#"
declare class Database {
    constructor(path: string);
    exec(sql: string): void;
    close(): void;
}
"#,
    )
    .unwrap();

    let output = huskc_command()
        .arg("import-dts")
        .arg(&dts_path)
        .output()
        .unwrap();

    assert!(output.status.success(), "command failed: {:?}", output);
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(
        stdout.contains("struct Database;"),
        "expected 'struct Database;' in output, got:\n{}",
        stdout
    );
    assert!(
        stdout.contains("fn exec(self, sql: String)"),
        "expected 'fn exec' method in output, got:\n{}",
        stdout
    );
    assert!(
        stdout.contains("fn close(self)"),
        "expected 'fn close' method in output, got:\n{}",
        stdout
    );
}
