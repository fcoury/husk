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
    return { tag: "Err", value: error };
}

function panic(message) {
    throw new Error("[Husk panic] " + String(message));
}

// Try operator (?) helper - unwraps Result/Option, throws on Err/None
function __husk_try(value) {
    if (value === null || value === undefined) {
        throw { __husk_early_return: { tag: "None" } };
    }
    if (value.tag === "Err") {
        throw { __husk_early_return: value };
    }
    if (value.tag === "None") {
        throw { __husk_early_return: value };
    }
    // Ok or Some - unwrap and return the inner value
    return value.value;
}

// Wrap a function to catch ? operator early returns
function __husk_try_wrap(fn) {
    return function() {
        try {
            return fn.apply(this, arguments);
        } catch (e) {
            if (e && e.__husk_early_return !== undefined) {
                return e.__husk_early_return;
            }
            throw e;
        }
    };
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

// JsValue field accessor helpers
// These are free functions that match the Husk stdlib declarations

// Get a field from an object as JsValue (returns null for Option::None)
function jsvalue_get(obj, key) {
    if (obj == null) return null;
    var v = obj[key];
    return v !== undefined ? v : null;
}

// Get a string field from an object (returns null for Option::None)
function jsvalue_getString(obj, key) {
    if (obj == null) return null;
    var v = obj[key];
    return typeof v === "string" ? v : null;
}

// Get a numeric field from an object (returns null for Option::None)
function jsvalue_getNumber(obj, key) {
    if (obj == null) return null;
    var v = obj[key];
    return typeof v === "number" ? v : null;
}

// Get a boolean field from an object (returns null for Option::None)
function jsvalue_getBool(obj, key) {
    if (obj == null) return null;
    var v = obj[key];
    return typeof v === "boolean" ? v : null;
}

// Get an array field from an object (returns null for Option::None)
function jsvalue_getArray(obj, key) {
    if (obj == null) return null;
    var v = obj[key];
    return Array.isArray(v) ? v : null;
}

// Type checking helpers
function jsvalue_isNull(obj) {
    return obj === null;
}

function jsvalue_isUndefined(obj) {
    return obj === undefined;
}

function jsvalue_isObject(obj) {
    return obj !== null && typeof obj === "object" && !Array.isArray(obj);
}

function jsvalue_isArray(obj) {
    return Array.isArray(obj);
}

// Value coercion helpers
function jsvalue_toString(obj) {
    if (obj == null) return "";
    return String(obj);
}

function jsvalue_toBool(obj) {
    return Boolean(obj);
}

function jsvalue_toNumber(obj) {
    if (obj == null) return 0;
    return Number(obj);
}

// JsObject builder class for fluent API
// Usage: JsObject_new().setString("key", "value").setNumber("count", 42).toJsValue()
function JsObject_new() {
    var obj = {};
    var builder = {
        setString: function(key, value) {
            obj[key] = value;
            return builder;
        },
        setNumber: function(key, value) {
            obj[key] = value;
            return builder;
        },
        setBool: function(key, value) {
            obj[key] = value;
            return builder;
        },
        set: function(key, value) {
            obj[key] = value;
            return builder;
        },
        toJsValue: function() {
            return obj;
        }
    };
    return builder;
}

// Express.js helper - provides express.json() middleware
// This helper allows Husk code to access express.json() as a function
function express_json() {
    if (typeof express !== 'undefined' && typeof express.json === 'function') {
        return express.json();
    }
    throw new Error('express_json: express module not available');
}

// Express.js wrapper - patches app.use to app.use_middleware alias
// 'use' is a reserved keyword in Husk, so we use 'use_middleware' instead
function __husk_express() {
    if (typeof express === 'undefined' || typeof express !== 'function') {
        throw new Error('__husk_express: express module not available');
    }
    var app = express.apply(null, arguments);
    if (app && typeof app.use === 'function') {
        app.use_middleware = app.use.bind(app);
    }
    return app;
}

// ========== Test assertion helpers ==========

// Deep equality comparison for assert_eq/assert_ne
function __husk_eq(a, b) {
    if (a === b) return true;
    if (a == null || b == null) return a === b;
    if (typeof a !== typeof b) return false;
    if (typeof a !== "object") return a === b;

    // Handle arrays
    if (Array.isArray(a)) {
        if (!Array.isArray(b) || a.length !== b.length) return false;
        for (var i = 0; i < a.length; i++) {
            if (!__husk_eq(a[i], b[i])) return false;
        }
        return true;
    }

    // Handle objects (including tagged enums)
    var keysA = Object.keys(a);
    var keysB = Object.keys(b);
    if (keysA.length !== keysB.length) return false;
    for (var i = 0; i < keysA.length; i++) {
        var key = keysA[i];
        if (!b.hasOwnProperty(key) || !__husk_eq(a[key], b[key])) return false;
    }
    return true;
}

// Basic assert - panics if condition is false
function assert(condition, message) {
    if (!condition) {
        var msg = message ? "assertion failed: " + message : "assertion failed";
        throw new Error("[Husk assertion] " + msg);
    }
}

// Assert with message - panics with the given message if condition is false
function assert_msg(condition, message) {
    if (!condition) {
        throw new Error("[Husk assertion] assertion failed: " + message);
    }
}

// Assert equality - panics with detailed message if values are not equal
function assert_eq(left, right, message) {
    if (!__husk_eq(left, right)) {
        var msg = message ? message + "\n" : "";
        msg += "assertion `left == right` failed\n";
        msg += "  left: " + __husk_fmt_debug(left) + "\n";
        msg += " right: " + __husk_fmt_debug(right);
        throw new Error("[Husk assertion] " + msg);
    }
}

// Assert inequality - panics with detailed message if values are equal
function assert_ne(left, right, message) {
    if (__husk_eq(left, right)) {
        var msg = message ? message + "\n" : "";
        msg += "assertion `left != right` failed\n";
        msg += "  left: " + __husk_fmt_debug(left) + "\n";
        msg += " right: " + __husk_fmt_debug(right);
        throw new Error("[Husk assertion] " + msg);
    }
}

// ========== Result/Option unwrap helpers ==========

// Unwrap a Result or Option, panicking if Err/None
function __husk_unwrap(value) {
    if (value === null || value === undefined) {
        throw new Error("[Husk panic] called unwrap on None value");
    }
    if (typeof value === "object" && value.tag !== undefined) {
        if (value.tag === "Ok") {
            return value.value;
        }
        if (value.tag === "Err") {
            throw new Error("[Husk panic] called unwrap on Err: " + __husk_fmt_debug(value.value));
        }
        if (value.tag === "Some") {
            return value.value;
        }
        if (value.tag === "None") {
            throw new Error("[Husk panic] called unwrap on None");
        }
    }
    // Not a Result/Option, return as-is
    return value;
}

// Unwrap a Result or Option with custom error message
function __husk_expect(value, message) {
    if (value === null || value === undefined) {
        throw new Error("[Husk panic] " + message);
    }
    if (typeof value === "object" && value.tag !== undefined) {
        if (value.tag === "Ok") {
            return value.value;
        }
        if (value.tag === "Err") {
            throw new Error("[Husk panic] " + message + ": " + __husk_fmt_debug(value.value));
        }
        if (value.tag === "Some") {
            return value.value;
        }
        if (value.tag === "None") {
            throw new Error("[Husk panic] " + message);
        }
    }
    // Not a Result/Option, return as-is
    return value;
}

// Parse string to i32, returns Result<i32, String>
function __husk_parse_i32(str) {
    var n = parseInt(str, 10);
    if (isNaN(n)) {
        return { tag: "Err", value: "invalid digit found in string" };
    }
    if (n < -2147483648 || n > 2147483647) {
        return { tag: "Err", value: "number too large or too small for i32" };
    }
    return { tag: "Ok", value: n };
}

// Parse string to i64, returns Result<i64, String>
function __husk_parse_i64(str) {
    try {
        var n = BigInt(str);
        return { tag: "Ok", value: n };
    } catch (e) {
        return { tag: "Err", value: "invalid digit found in string" };
    }
}

// Parse string to f64, returns Result<f64, String>
function __husk_parse_f64(str) {
    var n = parseFloat(str);
    if (isNaN(n) && str.trim() !== "NaN") {
        return { tag: "Err", value: "invalid float literal" };
    }
    return { tag: "Ok", value: n };
}

// Try to convert i64 to i32, returns Result<i32, String>
function __husk_try_into_i32(value) {
    var n;
    if (typeof value === "bigint") {
        if (value < -2147483648n || value > 2147483647n) {
            return { tag: "Err", value: "out of range integral type conversion" };
        }
        n = Number(value);
    } else {
        n = value;
        if (n < -2147483648 || n > 2147483647) {
            return { tag: "Err", value: "out of range integral type conversion" };
        }
    }
    return { tag: "Ok", value: n | 0 };
}

// Split string at first occurrence of delimiter, returns Option<(String, String)>
// Returns {tag: "None"} if delimiter not found, or {tag: "Some", value: [before, after]} if found
// Added to String.prototype so it can be called as str.split_once(delimiter)
String.prototype.__husk_split_once = function(delimiter) {
    var index = this.indexOf(delimiter);
    if (index === -1) {
        return {tag: "None"};
    }
    return {tag: "Some", value: [this.slice(0, index), this.slice(index + delimiter.length)]};
};

// Convert tuple (represented as array in JS) to array wrapped in Result
// Since we verify homogeneity at compile time, this always succeeds
function __husk_tuple_to_array(tuple) {
    if (!Array.isArray(tuple)) {
        return { tag: "Err", value: "not a tuple" };
    }
    // Return a copy to ensure immutability
    return { tag: "Ok", value: tuple.slice() };
}

// Range helpers: work for both exclusive (start..end) and inclusive (start..=end) ranges
function __husk_range_end_exclusive(range) {
    return range.inclusive ? range.end + 1 : range.end;
}

function __husk_range_contains(range, item) {
    const endExclusive = __husk_range_end_exclusive(range);
    return item >= range.start && item < endExclusive;
}

function __husk_range_is_empty(range) {
    const endExclusive = __husk_range_end_exclusive(range);
    return range.start >= endExclusive;
}

// Set helpers
function __husk_set_new() {
    return new Set();
}

function __husk_set_values(set) {
    return Array.from(set.values());
}

// Map helpers
function __husk_map_new() {
    return new Map();
}

function __husk_map_keys(map) {
    return Array.from(map.keys());
}

function __husk_map_values(map) {
    return Array.from(map.values());
}

// Run main() and handle ? operator early returns
// If main returns Err or None, report the error and exit with code 1
function __husk_run_main(main) {
    var result = main();
    if (result && typeof result === "object") {
        if (result.tag === "Err") {
            console.error("Error: " + __husk_fmt_debug(result.value));
            if (typeof process !== "undefined" && process.exit) {
                process.exit(1);
            }
        } else if (result.tag === "None") {
            console.error("Error: unexpected None");
            if (typeof process !== "undefined" && process.exit) {
                process.exit(1);
            }
        }
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

    #[test]
    fn preamble_contains_assertion_helpers() {
        let src = std_preamble_js();
        assert!(src.contains("function __husk_eq("));
        assert!(src.contains("function assert("));
        assert!(src.contains("function assert_eq("));
        assert!(src.contains("function assert_ne("));
    }

    #[test]
    fn preamble_contains_split_once() {
        let src = std_preamble_js();
        assert!(src.contains("String.prototype.__husk_split_once"));
    }

    #[test]
    fn preamble_contains_range_helpers() {
        let src = std_preamble_js();
        assert!(src.contains("function __husk_range_contains("));
        assert!(src.contains("function __husk_range_is_empty("));
    }

    #[test]
    fn preamble_contains_set_helper() {
        let src = std_preamble_js();
        assert!(src.contains("function __husk_set_new("));
    }

    #[test]
    fn preamble_contains_map_helpers() {
        let src = std_preamble_js();
        assert!(src.contains("function __husk_map_new("));
        assert!(src.contains("function __husk_map_keys("));
        assert!(src.contains("function __husk_map_values("));
    }
}
