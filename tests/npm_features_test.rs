use std::env;
use std::process::Command;

#[test]
fn test_npm_features() -> anyhow::Result<()> {
    // Set the current directory to the integration tests folder
    let original_dir = env::current_dir()?;
    env::set_current_dir("tests/integration")?;

    // Execute the npm features integration test script
    let output = Command::new("bash").arg("test_npm_features.sh").output();

    // Restore original directory
    env::set_current_dir(original_dir)?;

    let output = output?;

    if !output.status.success() {
        let stdout = String::from_utf8(output.stdout).unwrap();
        let stderr = String::from_utf8(output.stderr).unwrap();
        panic!("test_npm_features.sh failed: \n\nSTDOUT:\n{stdout}\n\nSTDERR:\n{stderr}");
    }

    Ok(())
}
