use std::process::Command;

#[test]
fn test_scripts() -> anyhow::Result<()> {
    // executes script.sh and checks for exit code
    let output = Command::new("bash").arg("tests/scripts/test.sh").output()?;

    if !output.status.success() {
        let output = String::from_utf8(output.stdout).unwrap();
        panic!("script.sh failed: \n\n{}", output);
    }

    Ok(())
}
