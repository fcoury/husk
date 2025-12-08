- In crates/husk-dts-parser/src/codegen.rs around lines 950 to 972, the current
  logic applies options.use_extern_const to all DtsVariable entries and emits
  extern const even for mutable bindings; change the branch to only emit extern
  const when options.use_extern_const is true AND v.is_const is true, otherwise
  fall back to the legacy function-based representation (i.e., keep generating
  GeneratedFn for non-const variables) and update the metrics accordingly
  (increment extern_consts only for true consts and legacy_const_functions for the
  non-const fallback).

- In crates/husk-dts-parser/src/oxc_parser.rs around lines 644 to 659, the
  function convert_function_declaration currently ignores decl.this_param so
  top-level functions with an explicit this: T are dropped; update the function to
  extract decl.this_param (if present), convert its type annotation via the same
  convert_type call used elsewhere, and set the resulting Option on the returned
  DtsFunction (i.e., populate the this_param field) while preserving existing
  type_params, params, and return_type behavior.

- In crates/husk-dts-parser/src/utility_types.rs around lines 616 to 652,
  expand_non_nullable currently only strips null/undefined from DtsType::Union and
  returns primitives unchanged; this makes NonNullable<null> and
  NonNullable<undefined> return null/undefined instead of never. Update the match
  to special-case when expanded is DtsType::Primitive(Primitive::Null) or
  DtsType::Primitive(Primitive::Undefined) and return
  DtsType::Primitive(Primitive::Never); keep the existing union-filter branch
  intact and return other non-null/undefined expanded types unchanged.

- In crates/husk-lang/src/main.rs around lines 1855–1895, the auto‑discovery
  fallback sets types_pkg to entry.package (contradicting the “Default to
  @types/package” comment) so the first search path and the npm install hint point
  at the wrong package; change the fallback to synthesize the @types package name
  when entry.types is None (e.g. build a string "@types/<entry.package>" and use
  that value for dts_paths and the install hint), making sure you use an owned
  String for the synthesized name when needed so it can be referenced in path
  construction and error messages.

crates/husk-lang/src/load.rs (1)
449-457: Extern consts are discoverable but not actually imported into the assembled root

matches_extern_item_name now recognizes ExternItemKind::Const, so find_pub_item can resolve use crate::foo::CONST_NAME; against an extern block. However, in assemble_root’s UseKind::Item branch, extern blocks are only included when they contain struct items that haven’t yet been marked in included_extern_blocks:

For an extern block containing only consts (and/or fns/statics), ext_items.iter().all(...) over structs is vacuously true, so all_structs_included is true and the block is skipped.
The seen_names set still records CONST_NAME, but no corresponding item is ever pushed into items, leaving the constant effectively undefined in the assembled root.
This means imports of extern consts will “succeed” at load time but won’t actually surface a usable symbol in the unified AST.

Consider adjusting the inclusion logic so that an extern block is added when any requested extern member (struct or const) from that block is being imported, or alternatively track inclusion per extern item name instead of only per struct name.

crates/husk-dts-parser/src/parser.rs (1)
1497-1597: Template-literal type parsing is byte-based and corrupts non-ASCII contents.

parse_template_literal_type walks self.src.as_bytes() and pushes src_bytes[pos] as char into current_string. For multi-byte UTF‑8 characters this produces one bogus char per byte, so TemplateLiteralPart::String values diverge from the original text (though brace matching and slicing for embedded types remain correct).

Consider switching to char_indices() (or at least iterating over chars() while tracking byte offsets) so string parts remain valid UTF‑8 representations of the template contents.

crates/husk-dts-parser/src/codegen.rs (1)
1132-1213: Alias expansion via type_aliases can infinite-recursively expand self-referential aliases.

map_named_type unconditionally inlines aliases via:

if let Some(alias_ty) = self.type_aliases.get(simple_name).cloned() {
return self.map_type(&alias_ty);
}
For common patterns like:

type Node = Node[]; // or
type Node = { next?: Node; }
map_type on Node will recursively call itself through map_named_type with no visited-set or depth limit, risking stack overflow during codegen.

Consider adding a simple recursion guard, e.g.:

pass a visited_aliases: &mut HashSet<String>/depth counter into map_type / map_named_type, and
when you detect a cycle, fall back to "JsValue" (and/or emit a Simplified warning + metric).
crates/husk-lang/src/main.rs (1)
2053-2091: Line‑based include/exclude filtering can leave syntactically broken output

The include/exclude filtering operates per line:

let filtered_lines: Vec<&str> = result.code.lines().filter(|line| {
let is_declaration = line.contains("extern struct")
|| line.contains("extern fn")
|| line.trim().starts_with("fn ");
…
}).collect();
result.code = filtered_lines.join("\n");
For multi‑line declarations (e.g., extern struct or extern fn whose signature or body spans several lines), this can:

Drop only the header line while leaving the body/closing braces in place, producing invalid Husk code.
Remove or keep associated comments/docstrings inconsistently relative to the declaration.
Given the comment that this is a temporary post‑processing filter, this is probably acceptable for now, but it’s a real sharp edge once include/exclude is used. Longer‑term, it would be more robust to:

Filter at the AST level, or
At least operate on declaration blocks (from header through closing brace/semicolon) rather than on individual lines.
The GenerationGapWriter integration below looks fine and doesn’t suffer from this issue.

Also applies to: 2093-2121

crates/husk-lang/src/load.rs (1)
255-276: Const-only extern blocks won't be included.

The duplicate-checking logic tracks only struct names in included_extern_blocks (lines 266-271) and uses all_structs_included to determine whether to add an extern block. For extern blocks containing no structs (only consts or functions), ext_items.iter().all(...) returns true (vacuous truth), causing line 265's condition !all_structs_included to be false. This means the block is never added to items, breaking imports like use crate::foo::MY_CONST; when MY_CONST is in a struct-less extern block.

Consider one of these fixes:

Track const names as well as struct names:
if let ItemKind::ExternBlock { items: ext_items, .. } = &export.kind {

- let all_structs_included = ext_items.iter().all(|ext| {
-        if let ExternItemKind::Struct { name, .. } = &ext.kind {

* let all_items_included = ext_items.iter().all(|ext| {
*        match &ext.kind {
*            ExternItemKind::Struct { name, .. } | ExternItemKind::Const { name, .. } => {
                 included_extern_blocks.contains(&name.name)
*            }
*            _ => true

-        } else {
-            true
         }
  });
- if !all_structs_included {
-        // Mark all structs as included

* if !all_items_included {
*        // Mark all structs and consts as included
         for ext in ext_items {

-            if let ExternItemKind::Struct { name, .. } = &ext.kind {
-                included_extern_blocks.insert(name.name.clone());

*            match &ext.kind {
*                ExternItemKind::Struct { name, .. } | ExternItemKind::Const { name, .. } => {
*                    included_extern_blocks.insert(name.name.clone());
*                }
*                _ => {}
               }
           }
           items.push(export.clone());
       }
  Track extern blocks by a unique identifier (e.g., a hash of the block's span or items) instead of relying on struct names as a proxy.

Add special-case handling for blocks with no trackable items:

- let has_trackable_items = ext_items.iter().any(|ext| {
-        matches!(&ext.kind, ExternItemKind::Struct { .. } | ExternItemKind::Const { .. })
- });
-

* if !all_structs_included {

- if !all_structs_included || !has_trackable_items {
  // Mark all structs as included
  for ext in ext_items {

In crates/husk-dts-parser/src/codegen.rs:

> - fn emit_callable_module_function(&mut self, mod_alias: &str, exported_name: &str) {

-        // Find the exported function in our collected functions
-        if let Some(f) = self.functions.iter().find(|f| f.name == exported_name) {
-            writeln!(
-                self.output,
-                "    // {} is the callable entry point",
-                exported_name
-            )
-            .unwrap();
-
-            if let Some(comment) = &f.comment {
-                writeln!(self.output, "    // {}", comment).unwrap();
-            }
-
-            // Emit the function with the module alias as the binding name
-            // This makes `mod_alias(...)` the callable form
-            write!(self.output, "    fn \"{}\" as {}", exported_name, mod_alias).unwrap();
-
-            if !f.type_params.is_empty() {
-                write!(self.output, "<{}>", f.type_params.join(", ")).unwrap();
-            }
-
-            write!(self.output, "(").unwrap();
-            for (i, (name, ty)) in f.params.iter().enumerate() {
-                if i > 0 {
-                    write!(self.output, ", ").unwrap();
-                }
-                write!(self.output, "{}: {}", name, ty).unwrap();
-            }
-            write!(self.output, ")").unwrap();

             if let Some(ret) = &f.return_type {
                 write!(self.output, " -> {}", ret).unwrap();
             }

             writeln!(self.output, ";").unwrap();
         }

- }
  ⚠️ Potential issue | 🟠 Major

Callable entry point function may be emitted twice for Hybrid modules.

emit_callable_module_function finds the function in self.functions but doesn't remove or mark it. When emit_standard_structs_and_functions is called afterward (line 1739 for Hybrid modules), the same function will be emitted again with its original name.

Consider either:

Removing the matched function from self.functions after emitting it as the callable entry point.
Filtering it out in emit_standard_structs_and_functions.
Using a HashSet to track already-emitted function names.
fn emit_callable_module_function(&mut self, mod_alias: &str, exported_name: &str) {

-        // Find the exported function in our collected functions
-        if let Some(f) = self.functions.iter().find(|f| f.name == exported_name) {

*        // Find and remove the exported function from our collected functions
*        let idx = self.functions.iter().position(|f| f.name == exported_name);
*        if let Some(idx) = idx {
*            let f = self.functions.remove(idx);
             writeln!(
                 self.output,
                 "    // {} is the callable entry point",
                 exported_name
             )
             .unwrap();
             // ... rest of emission using f

NITPICKS

🧹 Nitpick comments (17)
crates/husk-parser/Cargo.toml (1)
13-15: Redundant husk-lexer dev-dependency.

husk-lexer is already declared as a regular dependency on line 11. Listing it again under [dev-dependencies] is redundant—regular dependencies are already available in tests and benchmarks.

[dev-dependencies]
criterion = { version = "0.5", features = ["html_reports"] }
-husk-lexer.workspace = true
crates/husk-fmt/src/visitor.rs (1)
597-628: Consider adding block-end trivia handling for consistency.

The formatting logic looks good overall, but regular impl blocks call emit_block_end_trivia before the closing brace (see line 838 in format_impl). Adding this call would ensure comments appearing before the closing } are properly formatted.

Apply this diff:

                 }
                 self.indent -= 1;

-                self.emit_block_end_trivia(item.span.range.end);
                   self.write_indent();
                   self.write("}");
                   self.newline();
  Note: ExternItemKind::Mod also doesn't call this, but for consistency with how other blocks (impl, trait, struct, enum, fn) are formatted, it's recommended to include it.

crates/husk-ast/src/lib.rs (3)
429-434: Param now carries attributes; shape looks good and consistent

Adding attributes: Vec<Attribute> to Param lines up with how attributes are already modeled on items, impl methods, and extern properties. This should give you the flexibility you need for #[this] and other param-level annotations without introducing any obvious invariants issues.

Only note: since Param is pub, this is a source-breaking change for external constructors, but that’s expected for an AST evolution and seems fine within this PR’s scope.

If you find yourself repeatedly searching for specific param attributes (e.g., #[this]) across crates, consider adding small helpers on Param (e.g., fn has_attr(&self, name: &str)) to avoid scattering stringly-typed lookups.

699-704: Item::is_untagged helper is clear and matches existing attribute helpers

The new is_untagged method mirrors the existing pattern (is_test, is_ignored, should_panic) and the documentation nicely ties it to TS-style untagged unions. Implementation is straightforward and efficient.

One minor longer-term thought: we now have several hard‑coded attribute name strings ("cfg", "test", "ignore", "should_panic", "untagged", etc.). If this list keeps growing, a small central enum/consts module for attribute names could help reduce typo risk, but it’s not necessary for this PR.

754-770: New Const and Impl extern variants look sound; watch for duplication with existing impl AST

The Const variant cleanly distinguishes immutable JS bindings from Static, and the Impl variant gives you a focused representation for extern impl blocks with just the pieces you need (type_params, self_ty, items). The docs clearly communicate the intended semantics, and the field choices are consistent with the rest of the AST.

One design consideration: we now have two impl representations (ImplBlock and ExternItemKind::Impl) that are structurally similar but not identical. That’s totally fine if extern impls are intentionally a restricted subset, but if they ever need to gain features parallel to ImplBlock (e.g., trait refs or shared helpers), you might eventually want a shared struct or a wrapper instead of two parallel shapes to keep them in sync.

crates/husk-parser/src/lib.rs (1)
714-763: Extern impl block parsing is well‑structured; consider adding targeted tests

The new support for impl blocks inside extern "js" (including type params, self_ty, and a list of extern‑only methods) plus parse_extern_impl_method matches the existing ImplItem / ImplMethod shape and enforces the “semicolon only, no body” constraint nicely. The progress guards in both the extern‑impl loop and parse_impl_item are a good defensive fix against non‑advancing error recovery.

I don’t see tests in this file that specifically exercise extern "js" { impl T { fn ...; } } parsing; if they’re not elsewhere, it would be worth adding at least one happy‑path and one error‑recovery case to lock in this new surface.

Also applies to: 949-959, 1306-1358

crates/husk-parser/examples/profile_parser.rs (1)
1-38: Profiling example is clear; note that parsing currently includes a second lex pass

The example does what it advertises: separate timings for lexing and for parse_str, plus a simple throughput summary. One thing to be aware of is that parse_str internally lexes again, so total_time is actually lex (manual) + lex+parse (inside parse_str). If you ever want “pure parse” timing, you could refactor to construct a Parser directly from the first tokens instead. As‑is, it’s a fine, self‑contained profiling tool.

crates/husk-dts-parser/tests/integration.rs (3)
1090-1251: Real @types/express integration test is great; consider avoiding hard-coded /tmp

The resolver + generate_from_module + lex/parse pass provides strong end-to-end coverage against real-world Express types, and the early-return when node_modules is missing makes it safe on machines without fixtures installed.

The only portability concern is writing to "/tmp/generated.husk" which can fail on non-Unix platforms or systems without that directory. Using std::env::temp_dir().join("generated_express.husk") or fixtures_dir.join("generated-express.husk") would make this more robust without changing test intent.

- std::fs::write("/tmp/generated.husk", &result.code).expect("save generated code");

* let path = std::env::temp_dir().join("generated-express.husk");
* std::fs::write(&path, &result.code).expect("save generated code");
* println!("Saved generated code to {}", path.display());
  1253-1390: Real @types/better-sqlite3 integration test mirrors Express; same /tmp portability concern

The resolution + codegen + lex/parse path here is a nice parallel to the Express test and validates the more complex better-sqlite3 surface.

As above, writing to a hard-coded "/tmp/generated-sqlite.husk" can break on non-Unix systems. Reusing std::env::temp_dir().join("generated-sqlite.husk") (or a path under fixtures_dir) would keep CI/platform support broader while still leaving a debug artifact.

- std::fs::write("/tmp/generated-sqlite.husk", &result.code).expect("save generated code");
- println!("Saved generated code to /tmp/generated-sqlite.husk");

* let path = std::env::temp_dir().join("generated-sqlite.husk");
* std::fs::write(&path, &result.code).expect("save generated code");
* println!("Saved generated code to {}", path.display());
  1391-1589: E2E better-sqlite3 → JavaScript → Node test is thorough; consider tightening error checks

This test is a very strong end-to-end guard (Husk parse → semantic → JS codegen → Node execution) and sensibly skips when Node or better-sqlite3 isn’t available.

Two small refinements you might consider:

Right now sem.type_errors are only logged, not made fatal. If we don’t actually expect type errors here, asserting sem.type_errors.is_empty() (or filtering to known-benign ones) would turn this into a stronger regression check.
If test runtime ever becomes an issue, marking this as #[ignore] and documenting it as an opt-in “full pipeline” check (run in CI via an explicit command) could keep default cargo test snappy while retaining coverage.
crates/husk-codegen-js/src/lib.rs (2)
568-575: #[this] binding lowering is correct; consider keying lookups by type as a future improvement

The collection step correctly:

restricts this_binding_methods to extern methods with a receiver, and
inspects only the first parameter’s attributes for #[this].
The call-site lowering emits receiver.methodName.call(thisArg, ...) and uses the first argument as the this context, which matches how you want to apply DOM/EventTarget-style methods.

One potential refinement: lookups currently treat any method named like a #[this]-bound extern as requiring .call(...), regardless of receiver type. If we ever have a non-extern method with the same name on another type, it will also be lowered via .call. Tightening the check to include the type name (with help from type_resolution or by recording a map keyed by type + method) would remove that risk, at the cost of slightly more plumbing.

Also applies to: 1927-1961

616-622: Untagged enum handling for both imported and path-based variants looks consistent

Collecting untagged enums via item.is_untagged() and storing their names in accessors.untagged_enums is straightforward and leverages the existing attribute helper.
For imported variants (variant_calls), you now:
emit the raw inner value for single-arg variants,
emit the variant name string for unit variants, and
emit an array for multi-arg (tuple-like) variants.
The same shape is used for direct Enum::Variant(...) calls by extracting enum_name from the path head.
That behavior matches typical “untagged union” semantics and is well-covered by the new test that expects let x = "hello"; with no tag field.

One thing to keep in mind is that untagged detection is based on the enum’s name string; if an enum is re-exported or aliased under a different name, those alias paths won’t currently be recognized as untagged. That’s fine for now, but if alias-heavy code appears you may want to hook into richer type information from semantic analysis.

Also applies to: 2020-2035, 2058-2079

crates/husk-semantic/src/lib.rs (3)
855-895: Extern const/impl wiring into TypeEnv looks correct; consider small cleanups

The new ExternItemKind::Const and ExternItemKind::Impl branches correctly reuse the existing statics and ImplInfo machinery, so lookups via ExprKind::Ident and method/property resolution will work as for extern static and regular impl blocks.

Two minor follow‑ups you might consider:

The TypeEnv::statics doc comment still mentions only static name: Type;; it now effectively holds both static and const from extern blocks, so updating the comment would avoid confusion.
The ImplInfo construction logic for extern impls is nearly identical to the ItemKind::Impl arm below; factoring that into a small helper (e.g. fn push_impl(&mut self, self_ty: &TypeExpr, items: &[husk_ast::ImplItem], trait_name: Option<String>, span: &Span)) would reduce duplication and chances of divergence next time you tweak the representation.
4404-4417: Resolver support for extern const and extern impl symbols is consistent

Treating ExternItemKind::Const as SymbolKind::ExternStatic keeps the symbol surface aligned with how TypeEnv::statics is used, and synthesizing a <extern impl Type> symbol for ExternItemKind::Impl mirrors the existing synthetic names for regular impl blocks, which is useful for tooling/diagnostics.

One small nuance: multiple extern impl blocks for the same self_ty will now produce duplicate <extern impl Type> symbol errors, just like multiple regular impl blocks; if you ever need to support that pattern, you may want to bake a disambiguator into the synthetic name (e.g. a counter), but that's consistent with prior behavior and can be deferred.

4787-4792: Param struct updates in tests match the new public shape

Initializing Param { attributes: Vec::new(), name, ty } keeps these hand‑rolled AST nodes in sync with the updated husk_ast::Param and avoids test breakage; the change is straightforward and correct.

If more tests start constructing Param manually, a small helper (e.g. fn param(name: Ident, ty: TypeExpr) -> Param) could DRY up the boilerplate, but that’s optional.

Also applies to: 4896-4901

crates/husk-dts-parser/src/codegen.rs (2)
373-382: Prefer !overloads.is_empty() over overloads.len() > 0.

This is more idiomatic Rust and avoids a Clippy warning (clippy::len_zero). Same applies to line 434.

-        if overloads.len() > 0 {

*        if !overloads.is_empty() {
  1858-1864: Redundant need_comma = false inside the loop.

Line 1863 sets need_comma = false but this value is never read again within the loop iteration. The comment says "Only relevant for first param" but the reset happens after every param, which is unnecessary.

Consider removing the redundant assignment:

             for (i, (name, ty)) in f.params.iter().enumerate() {
                 if i > 0 || need_comma {
                     write!(self.output, ", ").unwrap();
                 }
                 write!(self.output, "{}: {}", name, ty).unwrap();

-                need_comma = false; // Only relevant for first param
             }

🧹 Nitpick comments (14)
crates/husk-ast/src/lib.rs (1)
427-434: Param now carries attributes – aligns with parser changes

Adding attributes: Vec<Attribute> to Param is consistent with the new parameter-attribute parsing and looks correct. Just be aware this is a public struct layout change; if husk-ast is consumed outside this workspace, it’s a breaking change for downstream struct literals.

features.md (1)
29-31: Clean up stray empty fenced code blocks

Lines 29–31 contain two empty ``` fences without a language, which also triggers MD040 (“fenced-code-language”). Unless you intend to show an empty code block, it’s cleaner to remove these lines or specify a language and meaningful contents.

crates/husk-codegen-js/src/lib.rs (2)
568-575: Align #[this]-binding lookup with variadic suffix stripping

this_binding_methods is populated with the raw method name (e.g., "run1"), but lookup in lower_expr uses base_method_name = strip_variadic_suffix(&method.name) and then checks:

let is*this_binding = ctx.accessors.this_binding_methods
.iter()
.any(|(*, m)| m == &base_method_name);
For extern methods that use the variadic suffix convention (e.g., add_event_listener1, run2) and are marked with #[this], this will fail to detect the binding, so calls won’t use .call(thisArg, ...) even though the method was marked.

Consider normalizing the name consistently at insertion time, e.g., store strip_variadic_suffix in this_binding_methods (and possibly extern_methods) so detection and storage use the same notion of the method name.

Also applies to: 1923-1957

617-622: Untagged enum construction logic looks correct and is well covered by tests

The new untagged_enums collection and the adjusted Call lowering for both imported variants and direct Enum::Variant(...) calls correctly emit raw values / strings / arrays instead of { tag, value } when the enum is marked #[untagged]. The generates_untagged_enum_variant_without_tag test exercises the path and asserts no tag is present, which is good coverage.

If you later need symmetry for path-only Enum::Variant expressions (without a call), you might also want to consult untagged_enums in the ExprKind::Path arm, but the current behavior is coherent for constructor-style usage.

Also applies to: 2016-2032, 2054-2075, 5532-5670

crates/husk-dts-parser/src/diagnostics.rs (1)
768-823: Consider surfacing extern const metrics in aggregation/report outputs

CodegenMetrics tracks extern_consts and legacy_const_functions, but:

aggregate doesn’t combine these fields.
to_markdown / to_json don’t expose them.
If you intend to compare the new extern-const behavior against the legacy function-wrapper approach at a project level, it would be useful to aggregate and report these counters alongside the existing metrics.

Also applies to: 928-1147

crates/husk-dts-parser/src/parser.rs (2)
488-505: Legacy parser drops this parameter information.

You’ve threaded this_param: None into function/call/member constructors and parse_params explicitly skips this parameters, so the hand-written parser never populates this_param even though the AST supports it. Only the Oxc-based path (in oxc_parser.rs) actually preserves explicit this annotations.

If parse() is still used anywhere user-facing (outside tests), consider either:

wiring this capture into parse_params / the relevant constructors, or
documenting that this_param is only guaranteed when going through the Oxc parser.
Also applies to: 541-560, 1199-1265, 1271-1283, 897-913

816-851: Module vs namespace semantics differ from Oxc path.

Here, declare module "foo" and declare module Foo both become DtsModule { name, is_ambient } (with is_ambient = true only for string literals). In the Oxc-based converter, identifier-named declarations are turned into DtsNamespace while string-literal-named ones become DtsModule { is_ambient: true }.

If downstream code (resolver/codegen) relies on “module == ambient string-literal module” and “namespace == identifier module”, this asymmetry can lead to different AST shapes depending on which parser is used. It may be worth aligning the legacy parser to:

treat identifier-based module as DtsNamespace, or
at least add a comment/tests clarifying the intentional divergence.
crates/husk-dts-parser/src/oxc_parser.rs (2)
34-52: Non-panicking Oxc parse errors are silently ignored.

parse_with_oxc only fails when parser_return.panicked is true; any entries in parser_return.errors are discarded even though they may indicate real syntax issues in the .d.ts.

Consider either:

treating a non-empty parser_return.errors as an Err(OxcParseError { .. }), or
at least exposing them (e.g., aggregated into the OxcParseError message or a side-channel) so callers can surface diagnostics instead of silently proceeding with a possibly incomplete AST.
668-685: Interface extends handling loses qualified/complex base names.

In convert_interface_declaration, bases are rendered as:

let name = match &heritage.expression {
Expression::Identifier(id) => id.name.to*string(),
* => "Unknown".to_string(),
};
So interface Foo extends ns.Bar {} or more complex expressions become a "Unknown" named type, which then flows into codegen.

It would be more faithful (and robust for downstream tools) to reuse type_name_to_string / a similar helper on the appropriate AST variant, or at least emit a warning when falling back to "Unknown" so this simplification is visible.

crates/husk-dts-parser/src/codegen.rs (2)
91-141: Module-graph–aware generate_from_module looks well-structured; add end-to-end coverage.

The new generate_from_module pipeline (dependency-ordered items, cycle warnings, pre-pass struct collection, two-pass emission + metrics) makes sense for multi-file/module-aware codegen and matches how the resolver likely wants to feed data in.

Given this now ties together resolver + parser + codegen, it’s a good candidate for additional integration tests that exercise real-world module graphs (imports, cycles, identity heuristics) to guard regressions.
Based on learnings, consider adding or extending integration tests that run the full flow.

1033-1085: Keyof/index access resolution is a nice improvement; note its reliance on the type registry.

The new DtsType::KeyOf and DtsType::IndexAccess branches plus helpers (extract_known_keys, get_property_type\*, resolve_index_access) are a solid step toward more accurate mappings, and the metrics hooks give good visibility into when resolution succeeds vs falls back.

One thing to be aware of: the type registry is only populated for non-generic interfaces and type aliases when expand_utility_types is enabled, so keyof/index access resolution on other shapes will always follow the fallback path to String/JsValue. That’s fine as long as it’s intentional; if you want keyof/index access improvements even when not expanding utility types, you may want to register interfaces/aliases unconditionally (or split registry responsibilities).

Also applies to: 1307-1423

crates/husk-lang/src/main.rs (3)
1163-1167: Clarify whether DTS auto‑update during build should ever use Oxc/reporting

run_build’s auto‑update path always calls:

run_dts_update(None, false, false, false, ReportFormat::Markdown, None, config);
So even if config enables Oxc, follow_imports, or generate_report, the auto‑update path will always use the legacy single‑file parser with no report. If that’s intentional for stability (builds stick to the stable path), a brief comment here would help. If not, consider threading relevant defaults from config.dts_options into this call.

2009-2039: Handle invalid union_strategy strings and align HUSKC_DEBUG handling

Two small robustness points in the DtsCodegenOptions construction:

union_strategy silently treats any unknown string as UnionStrategy::Auto. A typo in husk.toml (e.g., enuum) would be ignored with no feedback. Consider validating more strictly and at least emitting a warning when the value is not one of "enum" / "jsvalue" / "auto".
Here verbose only checks for HUSKC_DEBUG == "1", while run_import_dts also accepts "true" (case‑insensitive). To avoid surprising differences between subcommands, consider factoring out a small helper like fn is_debug_enabled() -> bool and using it in both places.
1838-1851: Wire dts.options.generate_report into report generation, or document the CLI‑only behavior

You correctly gate diagnostics collection and CodegenMetrics aggregation on the generate_report flag and support both Markdown and JSON outputs. However, the config field:

pub struct DtsOptions {
…
pub generate_report: Option<bool>,
}
is not consulted here or in run_dts, so:

huskc dts update only generates a report when --report is passed, regardless of any [dts_options] generate_report = true setting in husk.toml.
run_build’s auto‑update path also hard‑codes generate_report = false.
If you want config to act as a default, you could treat generate_report as:

let generate_report = cli_report
|| config.dts_options
.as_ref()
.and_then(|o| o.generate_report)
.unwrap_or(false);
with the CLI flag overriding to “on”. If the current CLI‑only behavior is intentional, a brief doc comment on generate_report in DtsOptions would help avoid confusion.

The diagnostics/report generation logic itself looks solid.

Also applies to: 2135-2168
