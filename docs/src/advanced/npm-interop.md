# npm Interop

Husk calls npm packages through explicit `extern "js"` declarations. The
compiler emits normal JavaScript imports or requires, while Husk uses the
declared types for checking calls.

## Direct Module Imports

Declare a package inside an `extern "js"` block:

```husk
extern "js" {
    #[default]
    mod express {
        #[default]
        fn express() -> ExpressApp;
        fn json() -> Middleware;
    }
}
```

For scoped or hyphenated packages, give the module a Husk-safe alias:

```husk
extern "js" {
    #[default]
    mod "better-sqlite3" as better_sqlite3 {
        #[default]
        fn better_sqlite3(filename: String) -> Database;
    }
}
```

With `--target esm`, Husk emits `import` statements. With `--target cjs`, it
emits `require` calls.

## Generated Bindings

Use `import-dts` for one-off conversion:

```bash
huskc import-dts node_modules/@types/express/index.d.ts --module express > src/express.hk
```

For projects, configure dependencies in `husk.toml` and regenerate them:

```toml
[dts_options]
generation_gap = true
generate_report = true

[[dts]]
package = "express"
types = "@types/express"
output = "src/express.hk"
follow_imports = true
```

```bash
huskc dts update express --follow-imports --report
```

When `generation_gap` is enabled, generated declarations go to a `.gen.hk`
file and the wrapper `.hk` file stays hand-editable.

## Wrapper Pattern

Raw TypeScript declarations can be too broad for a pleasant Husk API. Prefer
using generated files as input and writing a curated wrapper for the parts your
program actually uses.

The `examples/express_sqlite` app uses this pattern:

- generated d.ts imports document the available JS surface
- wrapper modules define `ExpressApp`, `Request`, `Response`, `Database`, and
  `TodoStatements`
- app code calls named helpers such as `request_body_string` and
  `response_error`
- `JsValue` remains only at JSON and unknown-JS boundaries

## JsValue Boundaries

`JsValue` is the escape hatch for values Husk cannot represent precisely. It is
expected for untyped JSON bodies, broad TypeScript unions, `any`, `unknown`, and
complex generic APIs.

Good wrappers keep `JsValue` local. Convert from it near the boundary with
helpers like `jsvalue_get`, `jsvalue_isNull`, `jsvalue_toString`, and
`jsvalue_toNumber`, then pass ordinary Husk types through the rest of the app.
