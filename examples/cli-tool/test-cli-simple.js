warning: unused variable: `variant_type`
   --> src/semantic.rs:985:29
    |
985 |                 if let Some(variant_type) = enum_variants.get(variant_name) {
    |                             ^^^^^^^^^^^^ help: if this is intentional, prefix it with an underscore: `_variant_type`
    |
    = note: `#[warn(unused_variables)]` on by default

warning: fields `name`, `dependencies`, and `peer_dependencies` are never read
  --> src/package_resolver.rs:49:9
   |
48 | pub struct PackageJson {
   |            ----------- fields in this struct
49 |     pub name: Option<String>,
   |         ^^^^
...
56 |     pub dependencies: Option<HashMap<String, String>>,
   |         ^^^^^^^^^^^^
57 |     #[serde(rename = "peerDependencies")]
58 |     pub peer_dependencies: Option<HashMap<String, String>>,
   |         ^^^^^^^^^^^^^^^^^
   |
   = note: `PackageJson` has derived impls for the traits `Clone` and `Debug`, but these are intentionally ignored during dead code analysis
   = note: `#[warn(dead_code)]` on by default

error: 33:42 - Variable 'input' not found in scope
            println("Processing from " + input + " to " + output);
                                         ^^^^^
