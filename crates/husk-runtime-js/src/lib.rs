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

// Format a value for display (basic {} placeholder)
function __husk_fmt(value) {
    if (value === null || value === undefined) {
        return String(value);
    }
    if (typeof value === "object" && value.tag !== undefined) {
        // Tagged union (enum) - format as Variant or Variant(...)
        return __husk_fmt_debug(value);
    }
    return String(value);
}

// Format a value for debug output ({:?} and {:#?})
function __husk_fmt_debug(value, pretty) {
    if (value === null) return "null";
    if (value === undefined) return "undefined";
    if (typeof value === "string") return '"' + value.replace(/\\/g, "\\\\").replace(/"/g, '\\"').replace(/\n/g, "\\n").replace(/\r/g, "\\r").replace(/\t/g, "\\t") + '"';
    if (typeof value === "number" || typeof value === "boolean") return String(value);
    if (Array.isArray(value)) {
        var items = value.map(function(v) { return __husk_fmt_debug(v, pretty); });
        if (pretty) return "[\n  " + items.join(",\n  ") + "\n]";
        return "[" + items.join(", ") + "]";
    }
    if (typeof value === "object") {
        // Tagged union (enum variant)
        if (value.tag !== undefined) {
            var tag = value.tag;
            // Check for tuple-style fields (0, 1, 2, ...)
            var tupleFields = [];
            var i = 0;
            while (value.hasOwnProperty(String(i))) {
                tupleFields.push(__husk_fmt_debug(value[String(i)], pretty));
                i++;
            }
            if (tupleFields.length > 0) {
                return tag + "(" + tupleFields.join(", ") + ")";
            }
            // Check for struct-style or special fields
            var keys = Object.keys(value).filter(function(k) { return k !== "tag"; });
            if (keys.length === 0) return tag;
            if (keys.length === 1 && keys[0] === "value") {
                return tag + "(" + __husk_fmt_debug(value.value, pretty) + ")";
            }
            if (keys.length === 1 && keys[0] === "error") {
                return tag + "(" + __husk_fmt_debug(value.error, pretty) + ")";
            }
            // General struct fields
            var fields = keys.map(function(k) { return k + ": " + __husk_fmt_debug(value[k], pretty); });
            if (pretty) return tag + " {\n  " + fields.join(",\n  ") + "\n}";
            return tag + " { " + fields.join(", ") + " }";
        }
        // Regular object (struct)
        var entries = Object.keys(value).map(function(k) { return k + ": " + __husk_fmt_debug(value[k], pretty); });
        if (entries.length === 0) return "{}";
        if (pretty) return "{\n  " + entries.join(",\n  ") + "\n}";
        return "{ " + entries.join(", ") + " }";
    }
    return String(value);
}

// Format a number with specified options
// base: 10 (default), 16 (hex), 2 (binary), 8 (octal)
// width: minimum width (0 for none)
// precision: decimal places for floats (null for none)
// fill: padding character (default space)
// align: '<' left, '>' right (default), '^' center
// sign: true to always show + for positive
// alternate: true to show 0x/0b/0o prefix
// zeroPad: true to pad with zeros
// uppercase: true for uppercase hex
function __husk_fmt_num(value, base, width, precision, fill, align, sign, alternate, zeroPad, uppercase) {
    base = base || 10;
    width = width || 0;
    fill = fill || (zeroPad ? "0" : " ");
    align = align || ">";

    var prefix = "";
    var numStr;

    if (base === 16) {
        numStr = Math.abs(value).toString(16);
        if (uppercase) numStr = numStr.toUpperCase();
        if (alternate && value !== 0) prefix = uppercase ? "0X" : "0x";
    } else if (base === 2) {
        numStr = Math.abs(value).toString(2);
        if (alternate && value !== 0) prefix = "0b";
    } else if (base === 8) {
        numStr = Math.abs(value).toString(8);
        if (alternate && value !== 0) prefix = "0o";
    } else {
        numStr = Math.abs(value).toString(10);
    }

    // Handle precision for decimal numbers
    if (precision !== null && precision !== undefined && base === 10) {
        var parts = numStr.split(".");
        if (precision === 0) {
            numStr = parts[0];
        } else {
            var dec = parts[1] || "";
            while (dec.length < precision) dec += "0";
            numStr = parts[0] + "." + dec.substring(0, precision);
        }
    }

    // Handle sign
    var signStr = "";
    if (value < 0) {
        signStr = "-";
    } else if (sign) {
        signStr = "+";
    }

    // Combine parts
    var result = signStr + prefix + numStr;

    // Apply width/padding
    if (result.length < width) {
        var padLen = width - result.length;
        var padding = "";
        for (var i = 0; i < padLen; i++) padding += fill;

        if (align === "<") {
            result = result + padding;
        } else if (align === "^") {
            var left = Math.floor(padLen / 2);
            var right = padLen - left;
            result = padding.substring(0, left) + result + padding.substring(0, right);
        } else {
            // Right align (default)
            if (zeroPad && (signStr || prefix)) {
                // Zero padding goes after sign/prefix
                result = signStr + prefix + padding + numStr;
            } else {
                result = padding + result;
            }
        }
    }

    return result;
}

// Pad a string to a given width
function __husk_fmt_pad(str, width, fill, align) {
    if (!width || str.length >= width) return str;
    fill = fill || " ";
    align = align || ">"; // default to right align (matches Rust for numbers)

    var padLen = width - str.length;
    var padding = "";
    for (var i = 0; i < padLen; i++) padding += fill;

    if (align === ">") {
        return padding + str;
    } else if (align === "^") {
        var left = Math.floor(padLen / 2);
        var right = padLen - left;
        return padding.substring(0, left) + str + padding.substring(0, right);
    } else {
        return str + padding;
    }
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

    #[test]
    fn preamble_contains_format_helpers() {
        let src = std_preamble_js();
        assert!(src.contains("function __husk_fmt("));
        assert!(src.contains("function __husk_fmt_debug("));
        assert!(src.contains("function __husk_fmt_num("));
        assert!(src.contains("function __husk_fmt_pad("));
    }
}
