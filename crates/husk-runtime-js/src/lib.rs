//! JavaScript runtime ("preamble") for Husk-generated code.
//!
//! This is a small, embedded JS snippet that provides:
//! - `Ok` / `Err` constructors for `Result`-like values.
//! - A simple `panic` function.
//! - A minimal `matchEnum` helper for tagged unions (optional use).

/// Semantic version of the Husk JS runtime.
pub const HUSK_RUNTIME_VERSION: &str = "0.1.0";

/// Return the standard JS preamble used by Husk-generated code.
///
/// This is intentionally tiny for the MVP. The API surface of this
/// preamble (and the corresponding `HUSK_RUNTIME_VERSION`) form the
/// compatibility contract between compiled Husk code and the runtime.
pub fn std_preamble_js() -> &'static str {
    r#"// Husk standard preamble (runtime helpers)
// Husk runtime v0.1.0
const HUSK_RUNTIME_VERSION = "0.1.0";

function Ok(value) {
    return { tag: "Ok", value };
}

function Err(error) {
    return { tag: "Err", error };
}

function panic(message) {
    throw new Error("[Husk panic] " + String(message));
}

function matchEnum(value, handlers) {
    var tag = value && value.tag;
    var handler = handlers[tag];
    if (typeof handler !== "function") {
        throw new Error("[Husk match] non-exhaustive handlers for tag " + String(tag));
    }
    return handler(value);
}
"#
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preamble_contains_core_helpers() {
        let src = std_preamble_js();
        assert!(src.contains("function Ok"));
        assert!(src.contains("function Err"));
        assert!(src.contains("function panic"));
        assert!(src.contains("function matchEnum"));
        assert!(src.contains("HUSK_RUNTIME_VERSION = \"0.1.0\""));
    }
}
