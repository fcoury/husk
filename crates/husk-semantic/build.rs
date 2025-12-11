use std::env;
use std::fs;
use std::path::Path;

fn main() {
    // Copy stdlib/core.hk from workspace root to src/stdlib/core.hk
    // This ensures the file is always in sync and available for packaging
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let crate_root = Path::new(&manifest_dir);
    let workspace_root = crate_root.parent().unwrap().parent().unwrap();

    let src_file = workspace_root.join("stdlib").join("core.hk");
    let dst_dir = crate_root.join("src").join("stdlib");
    let dst_file = dst_dir.join("core.hk");

    // Create destination directory if it doesn't exist
    fs::create_dir_all(&dst_dir).expect("failed to create src/stdlib directory");

    // Copy the file
    fs::copy(&src_file, &dst_file).unwrap_or_else(|e| {
        panic!(
            "failed to copy {} to {}: {}",
            src_file.display(),
            dst_file.display(),
            e
        )
    });

    // Tell Cargo to rerun if the source file changes
    println!("cargo:rerun-if-changed={}", src_file.display());
}
