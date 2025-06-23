#[cfg(test)]
mod async_test {
    use crate::{execute_script, transpile_to_js};

    #[test]
    fn test_async_function_interpreter_error() {
        let code = r#"
async fn fetch_data() -> string {
    "data"
}

fetch_data()
"#;
        
        let result = execute_script(code);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.to_string().contains("Async functions are not supported in interpreter mode"));
    }
    
    #[test]
    fn test_await_interpreter_error() {
        let code = r#"
extern fn fetch(url: string) -> Promise<string>;

async fn main() -> string {
    let result = fetch("url").await;
    result
}

main()
"#;
        
        let result = execute_script(code);
        assert!(result.is_err());
        let error = result.unwrap_err();
        // Should error because we're calling async function from interpreter
        assert!(error.to_string().contains("Async functions are not supported in interpreter mode"));
    }
    
    #[test]
    fn test_await_outside_async_error() {
        let code = r#"
extern fn fetch(url: string) -> Promise<string>;

fn regular_function() {
    let result = fetch("url").await;
    result
}
"#;
        
        let result = execute_script(code);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.to_string().contains(".await can only be used inside async functions"));
    }
    
    #[test]
    fn test_async_function_transpile() {
        let code = r#"
extern fn fetch(url: string) -> Promise<string>;

async fn fetch_user(id: int) -> string {
    let response = fetch("/api/users").await;
    response
}
"#;
        
        let result = transpile_to_js(code).unwrap();
        assert!(result.contains("async function fetch_user(id)"));
        assert!(result.contains("await fetch"));
    }
    
    #[test]
    fn test_nested_await_transpile() {
        let code = r#"
extern fn fetch_data() -> Promise<string>;
extern fn transform(data: string) -> Promise<string>;

async fn process_data() -> string {
    let data = fetch_data().await;
    let processed = transform(data).await;
    processed
}
"#;
        
        let result = transpile_to_js(code).unwrap();
        assert!(result.contains("async function process_data()"));
        assert!(result.contains("await fetch_data()"));
        assert!(result.contains("await transform(data)"));
    }
    
    #[test]
    fn test_await_chain_transpile() {
        let code = r#"
extern fn fetch(url: string) -> Promise<string>;

async fn chain_example() -> string {
    let result = fetch("/api").await;
    result
}
"#;
        
        let result = transpile_to_js(code).unwrap();
        assert!(result.contains("await fetch(\"/api\")"));
    }
}