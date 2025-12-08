//! JavaScript runtime ("preamble") for Husk-generated code.
//!
//! This is a small, embedded JS snippet that provides:
//! - `Ok` / `Err` constructors for `Result`-like values.
//! - A simple `panic` function.
//! - A minimal `matchEnum` helper for tagged unions (optional use).
//!
//! The runtime can be customized by platform:
//! - Node: Full runtime with process.exit support
//! - Browser: Throws errors instead of process.exit

/// Semantic version of the Husk JS runtime.
pub const HUSK_RUNTIME_VERSION: &str = "0.1.0";

/// Platform target for preamble generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Platform {
    /// Server-side execution (Node.js)
    #[default]
    Node,
    /// Client-side execution (Browser)
    Browser,
}

/// Return the standard JS preamble for the specified platform.
///
/// The preamble differs between platforms:
/// - Node: panic uses process.exit(1)
/// - Browser: panic throws an Error (no process.exit)
pub fn std_preamble_js_for_platform(platform: Platform) -> &'static str {
    match platform {
        Platform::Node => std_preamble_node_js(),
        Platform::Browser => std_preamble_browser_js(),
    }
}

/// Return the standard JS preamble used by Husk-generated code.
/// Uses the Node platform by default for backwards compatibility.
///
/// This is intentionally tiny for the MVP. The API surface of this
/// preamble (and the corresponding `HUSK_RUNTIME_VERSION`) form the
/// compatibility contract between compiled Husk code and the runtime.
pub fn std_preamble_js() -> &'static str {
    std_preamble_node_js()
}

/// Node.js-specific preamble.
fn std_preamble_node_js() -> &'static str {
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

// JsValue function call helpers
// Call a JsValue as a function with 0-3 arguments
function jsvalue_call0(fn) {
    if (typeof fn !== 'function') return null;
    return fn();
}

function jsvalue_call1(fn, a) {
    if (typeof fn !== 'function') return null;
    return fn(a);
}

function jsvalue_call2(fn, a, b) {
    if (typeof fn !== 'function') return null;
    return fn(a, b);
}

function jsvalue_call3(fn, a, b, c) {
    if (typeof fn !== 'function') return null;
    return fn(a, b, c);
}

// JsValue arithmetic helpers
function jsvalue_add_num(a, b) { return a + b; }
function jsvalue_sub_num(a, b) { return a - b; }
function jsvalue_mul_num(a, b) { return a * b; }
function jsvalue_div_num(a, b) { return a / b; }

// Primitive to JsValue conversion helpers
function jsvalue_from_int(n) { return n; }
function jsvalue_from_float(n) { return n; }
function jsvalue_from_bool(b) { return b; }
function jsvalue_null() { return null; }
function jsvalue_undefined() { return undefined; }

// Browser globals access
function get_document() { return typeof document !== 'undefined' ? document : null; }
function get_window() { return typeof window !== 'undefined' ? window : null; }

// JsValue method call helpers
function jsvalue_method0(obj, method) { return obj[method](); }
function jsvalue_method1(obj, method, arg) { return obj[method](arg); }
function jsvalue_method2(obj, method, arg1, arg2) { return obj[method](arg1, arg2); }
function jsvalue_method1_str(obj, method, arg) { return obj[method](arg); }

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

// ========== JSX Runtime ==========

// JSX runtime for hx blocks
// Delegates to React.createElement when React is available (imported as 'react')
// Falls back to creating plain VDOM objects for non-React usage
// Usage: _jsx("div", { className: "container", children: "Hello" })
function _jsx(type, props) {
    // Check if React is available (imported as 'react' from extern mod)
    if (typeof react !== 'undefined' && react && react.createElement) {
        var children = props && props.children;
        var newProps = {};
        for (var k in props) {
            if (k !== 'children') newProps[k] = props[k];
        }
        if (Array.isArray(children)) {
            return react.createElement.apply(react, [type, newProps].concat(children));
        } else if (children !== undefined) {
            return react.createElement(type, newProps, children);
        } else {
            return react.createElement(type, newProps);
        }
    }
    // Fallback: create plain VDOM object
    return {
        $$typeof: Symbol.for("react.element"),
        type: type,
        props: props || {},
        key: props && props.key || null,
        ref: null,
    };
}

// Fragment component for multiple children without a wrapper element
var Fragment = typeof react !== 'undefined' && react && react.Fragment ? react.Fragment : Symbol.for("react.fragment");
"#
}

/// Browser-specific preamble.
/// This is similar to the Node preamble but avoids any Node-specific globals
/// like process, Buffer, require, module.exports, __dirname, __filename.
fn std_preamble_browser_js() -> &'static str {
    r#"// Husk standard preamble (runtime helpers) - Browser
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

// JsValue function call helpers
// Call a JsValue as a function with 0-3 arguments
function jsvalue_call0(fn) {
    if (typeof fn !== 'function') return null;
    return fn();
}

function jsvalue_call1(fn, a) {
    if (typeof fn !== 'function') return null;
    return fn(a);
}

function jsvalue_call2(fn, a, b) {
    if (typeof fn !== 'function') return null;
    return fn(a, b);
}

function jsvalue_call3(fn, a, b, c) {
    if (typeof fn !== 'function') return null;
    return fn(a, b, c);
}

// JsValue arithmetic helpers
function jsvalue_add_num(a, b) { return a + b; }
function jsvalue_sub_num(a, b) { return a - b; }
function jsvalue_mul_num(a, b) { return a * b; }
function jsvalue_div_num(a, b) { return a / b; }

// Primitive to JsValue conversion helpers
function jsvalue_from_int(n) { return n; }
function jsvalue_from_float(n) { return n; }
function jsvalue_from_bool(b) { return b; }
function jsvalue_null() { return null; }
function jsvalue_undefined() { return undefined; }

// Browser globals access
function get_document() { return typeof document !== 'undefined' ? document : null; }
function get_window() { return typeof window !== 'undefined' ? window : null; }

// JsValue method call helpers
function jsvalue_method0(obj, method) { return obj[method](); }
function jsvalue_method1(obj, method, arg) { return obj[method](arg); }
function jsvalue_method2(obj, method, arg1, arg2) { return obj[method](arg1, arg2); }
function jsvalue_method1_str(obj, method, arg) { return obj[method](arg); }

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

// ========== JSX Runtime ==========

// JSX runtime for hx blocks
// Delegates to React.createElement when React is available (imported as 'react')
// Falls back to creating plain VDOM objects for non-React usage
// Usage: _jsx("div", { className: "container", children: "Hello" })
function _jsx(type, props) {
    // Check if React is available (imported as 'react' from extern mod)
    if (typeof react !== 'undefined' && react && react.createElement) {
        var children = props && props.children;
        var newProps = {};
        for (var k in props) {
            if (k !== 'children') newProps[k] = props[k];
        }
        if (Array.isArray(children)) {
            return react.createElement.apply(react, [type, newProps].concat(children));
        } else if (children !== undefined) {
            return react.createElement(type, newProps, children);
        } else {
            return react.createElement(type, newProps);
        }
    }
    // Fallback: create plain VDOM object
    return {
        $$typeof: Symbol.for("react.element"),
        type: type,
        props: props || {},
        key: props && props.key || null,
        ref: null,
    };
}

// Fragment component for multiple children without a wrapper element
var Fragment = typeof react !== 'undefined' && react && react.Fragment ? react.Fragment : Symbol.for("react.fragment");
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
    fn preamble_contains_jsx_runtime() {
        let src = std_preamble_js();
        assert!(src.contains("function _jsx("));
        assert!(src.contains("var Fragment"));
    }

    #[test]
    fn browser_preamble_contains_jsx_runtime() {
        let preamble = std_preamble_browser_js();
        assert!(preamble.contains("function _jsx("));
        assert!(preamble.contains("var Fragment"));
    }

    // ========== Preamble Conformance Tests ==========
    // These tests ensure platform preambles don't contain cross-platform globals

    #[test]
    fn browser_preamble_no_node_globals() {
        let preamble = std_preamble_browser_js();

        // Must not contain Node globals
        assert!(
            !preamble.contains("process."),
            "Browser preamble must not reference process"
        );
        assert!(
            !preamble.contains("Buffer"),
            "Browser preamble must not reference Buffer"
        );
        assert!(
            !preamble.contains("require("),
            "Browser preamble must not use require()"
        );
        assert!(
            !preamble.contains("module.exports"),
            "Browser preamble must not use module.exports"
        );
        assert!(
            !preamble.contains("__dirname"),
            "Browser preamble must not reference __dirname"
        );
        assert!(
            !preamble.contains("__filename"),
            "Browser preamble must not reference __filename"
        );
    }

    #[test]
    fn node_preamble_no_browser_globals() {
        let preamble = std_preamble_node_js();

        // Should not assume browser globals (though these are less critical)
        assert!(
            !preamble.contains("window."),
            "Node preamble should not reference window"
        );
        assert!(
            !preamble.contains("document."),
            "Node preamble should not reference document"
        );
    }

    #[test]
    fn browser_preamble_has_core_helpers() {
        let preamble = std_preamble_browser_js();

        // Ensure browser preamble has all core functionality
        assert!(preamble.contains("function Ok"));
        assert!(preamble.contains("function Err"));
        assert!(preamble.contains("function panic"));
        assert!(preamble.contains("function matchEnum"));
        assert!(preamble.contains("function __husk_fmt("));
        assert!(preamble.contains("function assert_eq("));
        assert!(preamble.contains("HUSK_RUNTIME_VERSION"));
    }

    #[test]
    fn platform_preamble_selection() {
        let node = std_preamble_js_for_platform(Platform::Node);
        let browser = std_preamble_js_for_platform(Platform::Browser);

        // Both should have runtime version
        assert!(node.contains("HUSK_RUNTIME_VERSION"));
        assert!(browser.contains("HUSK_RUNTIME_VERSION"));

        // They should be different (browser has "- Browser" comment)
        assert!(browser.contains("- Browser"));
        assert!(!node.contains("- Browser"));
    }
}
