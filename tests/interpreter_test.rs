use std::env;
use std::process::Command;

#[test]
fn test_scripts() -> anyhow::Result<()> {
    // Get the current directory to understand where the test is running
    let current_dir = env::current_dir()?;
    println!("Running from: {current_dir:?}");

    // executes script.sh and checks for exit code
    let output = Command::new("bash").arg("tests/scripts/test.sh").output()?;

    if !output.status.success() {
        let stdout = String::from_utf8(output.stdout).unwrap();
        let stderr = String::from_utf8(output.stderr).unwrap();
        panic!("script.sh failed: \n\nSTDOUT:\n{stdout}\n\nSTDERR:\n{stderr}");
    }

    Ok(())
}
