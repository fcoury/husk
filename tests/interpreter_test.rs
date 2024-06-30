use std::fs;

use rusk::execute_script;

#[test]
fn test_scripts() -> anyhow::Result<()> {
    for entry in fs::read_dir("tests/scripts")? {
        let entry = entry?;
        if let Some(extension) = entry.path().extension() {
            if extension != "rk" {
                continue;
            }
        }
        println!("Running test: {:?}", entry.path());
        let contents = fs::read_to_string(entry.path())?;
        let _ = execute_script(contents);
    }

    Ok(())
}
