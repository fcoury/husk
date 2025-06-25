#[cfg(test)]
mod tests {
    use crate::{execute_script, transpile_to_js};

    #[test]
    fn test_promise_type_parsing() {
        let code = r#"
extern fn fetch(url: string) -> Promise<string>;

async fn get_data() -> string {
    let result = fetch("https://api.example.com").await;
    result
}
"#;

        let result = transpile_to_js(code);
        assert!(result.is_ok());
        let js = result.unwrap();
        assert!(js.contains("async function get_data()"));
        assert!(js.contains("await fetch"));
    }

    #[test]
    fn test_await_on_non_promise_error() {
        let code = r#"
fn regular_function() -> string {
    "not a promise"
}

async fn test() -> string {
    let result = regular_function().await;
    result
}
"#;

        let result = execute_script(code);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error
            .to_string()
            .contains(".await can only be used on Promise types"));
    }

    #[test]
    fn test_nested_promise_types() {
        let code = r#"
extern fn fetch_array(url: string) -> Promise<array<string>>;

async fn get_items() -> array<string> {
    let items = fetch_array("/api/items").await;
    items
}
"#;

        let result = transpile_to_js(code);
        assert!(result.is_ok());
        let js = result.unwrap();
        assert!(js.contains("async function get_items()"));
        assert!(js.contains("await fetch_array"));
    }

    #[test]
    fn test_async_function_returns_promise() {
        let code = r#"
async fn async_add(a: int, b: int) -> int {
    a + b
}

// This should work - async functions return Promise<T>
extern fn process_promise(p: Promise<int>) -> void;

fn main() {
    let promise = async_add(1, 2);
    process_promise(promise);
}
"#;

        let result = transpile_to_js(code);
        assert!(result.is_ok());
    }
}
