//! Minimal `.d.ts` importer: parse a small subset of TypeScript
//! declaration files and generate Husk `extern "js"` declarations.

/// Import a `.d.ts` source string and return Husk `extern "js"` code.
///
/// This is intentionally narrow: it supports only simple function
/// declarations of the form:
///
/// - `export declare function foo(a: number, b: string): boolean;`
/// - `declare function foo(a: number): void;`
/// - `export function foo(a: number): number;`
pub fn import_dts_str(src: &str) -> String {
    let mut fns = Vec::new();

    for line in src.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with("//") {
            continue;
        }

        if let Some(func) = parse_function_decl(line) {
            fns.push(func);
        }
    }

    if fns.is_empty() {
        return String::new();
    }

    let mut out = String::new();
    out.push_str("extern \"js\" {\n");
    for f in fns {
        out.push_str("    fn ");
        out.push_str(&f.name);
        out.push('(');
        for (i, (name, ty)) in f.params.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            out.push_str(name);
            out.push_str(": ");
            out.push_str(ty);
        }
        out.push(')');
        if let Some(ret) = f.ret_type {
            out.push_str(" -> ");
            out.push_str(&ret);
        }
        out.push_str(";\n");
    }
    out.push_str("}\n");

    out
}

#[derive(Debug, Clone)]
struct ImportedFn {
    name: String,
    params: Vec<(String, String)>,
    ret_type: Option<String>,
}

fn parse_function_decl(line: &str) -> Option<ImportedFn> {
    // Strip trailing semicolon.
    let mut s = line.trim_end_matches(';').trim();

    // Remove leading modifiers: export/declare.
    if let Some(rest) = s.strip_prefix("export ") {
        s = rest.trim_start();
    }
    if let Some(rest) = s.strip_prefix("declare ") {
        s = rest.trim_start();
    }

    // Require `function` keyword.
    let rest = s.strip_prefix("function ")?;
    let rest = rest.trim_start();

    // Split at '(' to get the name.
    let open_paren = rest.find('(')?;
    let name = rest[..open_paren].trim().to_string();
    let after_name = &rest[open_paren + 1..];

    // Find closing ')'.
    let close_paren = after_name.find(')')?;
    let params_str = &after_name[..close_paren];
    let after_params = after_name[close_paren + 1..].trim();

    let params = parse_params(params_str);
    let ret_type = parse_return_type(after_params);

    Some(ImportedFn {
        name,
        params,
        ret_type,
    })
}

fn parse_params(params_str: &str) -> Vec<(String, String)> {
    let mut params = Vec::new();
    for part in params_str.split(',') {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }
        let mut pieces = part.split(':');
        let name = match pieces.next() {
            Some(n) => n.trim(),
            None => continue,
        };
        let ty = match pieces.next() {
            Some(t) => t.trim(),
            None => continue,
        };
        if name.is_empty() || ty.is_empty() {
            continue;
        }
        params.push((name.to_string(), map_ts_type_to_husk(ty)));
    }
    params
}

fn parse_return_type(after_params: &str) -> Option<String> {
    let after = after_params.trim_start();
    if !after.starts_with(':') {
        return None;
    }
    let ty_str = after[1..].trim();
    if ty_str.is_empty() {
        None
    } else {
        Some(map_ts_type_to_husk(ty_str))
    }
}

fn map_ts_type_to_husk(ts: &str) -> String {
    let ts = ts.trim();
    // For union types like `string | null`, take the first branch.
    let first = ts.split('|').next().unwrap_or(ts).trim();
    match first {
        "number" => "i32".to_string(),
        "boolean" => "bool".to_string(),
        "string" => "String".to_string(),
        "void" => "()".to_string(),
        other => other.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn imports_simple_function_declarations() {
        let dts = r#"
export declare function add(a: number, b: number): number;
declare function log(message: string): void;
"#;
        let husk = import_dts_str(dts);

        assert!(husk.contains("extern \"js\" {"));
        assert!(husk.contains("fn add(a: i32, b: i32) -> i32;"));
        assert!(husk.contains("fn log(message: String) -> ();"));
        assert!(husk.trim_end().ends_with('}'));
    }

    #[test]
    fn imports_real_world_like_declarations_and_typechecks() {
        use husk_parser::parse_str;
        use husk_semantic::analyze_file;

        // Derived from real npm packages (`has-symbols` and `math-intrinsics`),
        // simplified to focus on the subset of `.d.ts` that the importer
        // currently understands (plain `declare function` signatures).
        let dts = r#"
declare function hasNativeSymbols(): boolean;
export = hasNativeSymbols;

declare function mod(number: number, modulo: number): number;
export = mod;
"#;

        let husk = import_dts_str(dts);

        assert!(
            husk.contains("fn hasNativeSymbols() -> bool;"),
            "missing hasNativeSymbols extern in:\n{}",
            husk
        );
        assert!(
            husk.contains("fn mod(number: i32, modulo: i32) -> i32;"),
            "missing mod extern in:\n{}",
            husk
        );

        // The generated Husk externs should parse and typecheck end-to-end.
        let parsed = parse_str(&husk);
        assert!(
            parsed.errors.is_empty(),
            "parse errors for generated externs: {:?}\nsource:\n{}",
            parsed.errors,
            husk
        );
        let file = parsed
            .file
            .expect("parser produced no AST for generated externs");
        let sem = analyze_file(&file);
        assert!(
            sem.symbols.errors.is_empty() && sem.type_errors.is_empty(),
            "semantic errors for generated externs: symbols={:?}, types={:?}\nsource:\n{}",
            sem.symbols.errors,
            sem.type_errors,
            husk
        );
    }
}
