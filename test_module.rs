use husk_lang::transpile_to_js_with_target;

fn main() {
    let code = r#"
        use fs::readFile;
        
        pub fn main() {
            let content = readFile("test.txt");
            println\!(content);
        }
    "#;
    
    let js = transpile_to_js_with_target(code, "node-cjs").unwrap();
    println\!("=== Generated JavaScript for node-cjs ===");
    println\!("{}", js);
}
