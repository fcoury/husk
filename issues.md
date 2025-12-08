crates/husk-codegen-js/src/lib.rs (3)
63-69: #[this] and #[untagged] tracking is a good fit for PropertyAccessors; be mindful of name collisions

Storing this_binding_methods and untagged_enums in PropertyAccessors cleanly centralizes JS interop metadata. One thing to watch is that callers (in lower_expr’s method-call branch) currently check this_binding_methods by method name only, ignoring the recorded type_name, and use the base name after strip_variadic_suffix. That makes it possible for:

Different types with the same method name to share #[this] behavior unintentionally.
Methods marked #[this] on variadic-style names (e.g. run1, run2) to be missed if you ever normalize names differently at insertion vs lookup.
If these scenarios become likely, consider normalizing the name consistently at insertion time (e.g., store the stripped base name) and/or incorporating receiver type information at lookup.

155-201: Clarify/include behavior for include_str when source_path is unavailable

handle_include_str correctly enforces a string literal argument and resolves paths relative to ctx.source_path, but it unconditionally panics when source_path is None. Since lower_file_to_js always builds a CodegenContext with source_path: None, any user of that public entrypoint who writes include_str(...) will see a panic instead of a more controlled failure.

Consider either:

Documenting that include_str is only supported when going through lower_file_to_js_with_source (with a non-None source_path), or
Providing a safer fallback (e.g., resolving relative to std::env::current_dir() or returning a compile-time error) when source_path is missing.
This would make the behavior clearer for downstream users of husk-codegen-js’s public API.

2000-2089: Method-call lowering with #[this], js_name, and variadic suffixes is mostly solid; consider tightening heuristics

The ExprKind::MethodCall lowering now cleanly handles:

Getters/setters via accessors.getters / setters.
unwrap/expect shims.
.into / .parse / .try_into based on type_resolution.
extern JS naming via strip_variadic_suffix, method_js_names, and extern_methods. #[this] methods via .call(thisArg, ...), as covered by generates_call_for_this_binding_method.
Two heuristics you might refine over time:

this_binding_methods is keyed by (type_name, method_name) but lookups only test the (potentially stripped) method name, so cross-type methods with the same name all get #[this] behavior once any one impl is marked. Using the recorded type_name (via semantic type info on the receiver) would avoid this.
For variadic-style externs where the Rust name ends with digits, strip_variadic_suffix changes the lookup key; if you ever combine #[this] or #[js_name] with that pattern, you’ll want insertion and lookup to agree on the normalized name.
The current behavior is still coherent for the common cases you’re testing; these adjustments would just harden it against more exotic extern surfaces.

crates/husk-lang/src/diagnostic.rs (1)
83-99: Consider extracting the shared emit_diagnostic logic.

Both SourceDb and MultiFileSourceDb have identical emit_diagnostic implementations. Consider extracting to a free function or shared trait to reduce duplication:

fn emit*diagnostic_to_stderr<'a, F: codespan_reporting::files::Files<'a, FileId = usize>>(
files: &'a F,
diagnostic: &Diagnostic<usize>,
) {
let writer = StandardStream::stderr(ColorChoice::Auto);
let config = term::Config::default();
let * = term::emit(&mut writer.lock(), &config, files, diagnostic);
}
crates/husk-dts-parser/src/oxc_parser.rs (1)
840-888: Class extends loses qualified or non-identifier superclasses

convert_class_declaration only recognizes Expression::Identifier for decl.super_class; anything else (e.g. extends ns.Base, extends Foo.Bar) is coerced to Primitive::Object, so class inheritance information is dropped for those patterns. That can affect downstream codegen or diagnostics that rely on accurate class hierarchies.

If you care about class inheritance for qualified or expression-based superclasses, consider handling additional Expression variants (e.g. member expressions) via a helper that mirrors type_name_to_string, instead of falling back to Object.

crates/husk-dts-parser/tests/integration.rs (2)
1353-1390: Strengthen UTF‑8 template literal assertions to fail when AST shape changes

test_template_literal_type_with_utf8 only inspects alias.ty if the first item happens to be a TypeAlias; if parsing ever changes (e.g. extra items, different ordering), the test will silently skip the core assertions and still pass.

Consider asserting on the expected variant and panicking otherwise (mirroring other tests in this file), so regressions in how template literals are represented can’t slip through unnoticed.

1719-2053: Heavy E2E test assumes node/npm and may run npm ci during cargo test

test_better_sqlite3_e2e_execution:

Asserts that node and npm are available on PATH, failing the test suite otherwise.
Calls ensure_better_sqlite3_ready, which may run npm ci inside tests/fixtures and assert its success.
Executes real JS against better-sqlite3, depending on a working native build for the host.
This is great for full-stack coverage but can make cargo test slow, fragile, and environment-dependent (especially on CI or contributors without node/npm installed).

Consider marking this as #[ignore] or gating it behind an env flag (e.g. if std::env::var("HUSK_E2E").is_err() { return; }), and/or treating missing node/npm as a skip instead of a hard assertion.

crates/husk-lang/src/load.rs (1)
484-535: Preserve original extern block ABI instead of hard‑coding "js"

filter_extern_block synthesizes a new ItemKind::ExternBlock with:

kind: husk_ast::ItemKind::ExternBlock {
abi: "js".to_string(),
items: filtered,
}
If you ever introduce non‑"js" ABIs in extern blocks, or reuse this helper for other ABIs, this will silently change semantics compared to the original item. It’s safer to copy the abi from the source extern block (e.g., by passing it in as a parameter or deriving it from ext_items’ owning item) rather than hard‑coding "js".

crates/husk-dts-parser/src/parser.rs (2)
904-946: Legacy parser still drops explicit this parameters

parse_params skips this parameters entirely:

if self.check(&TokenKind::This) {
self.advance();
self.expect(&TokenKind::Colon)?;
let \_this_type = self.parse_type()?;
// Skip this parameter (handled separately in FunctionType)
...
}
but parse_function always constructs DtsFunction with this_param: None, and there’s no path that captures the parsed \_this_type. As a result, top‑level functions with explicit this: T are modeled differently depending on whether they were parsed by the legacy parser (parse) or the Oxc parser (parse_with_oxc).

If you need consistent AST behavior across both parsing paths, consider threading an optional this_type out of parse_params and wiring it into DtsFunction.this_param, similar to how Oxc mapping already does.

1510-1621: Template literal type parsing is robust but relatively heavy per interpolation

parse_template_literal_type does a UTF‑8 aware scan over the source slice and, for each ${...} interpolation, re‑invokes crate::parse on a synthesized type T = <expr>; snippet to obtain a DtsType. This is a reasonable first implementation and your new tests cover both ASCII and UTF‑8 cases, but it does mean:

O(n) rescans of the source per interpolation, plus
Full lexer/parser re-entry for every ${...}.
If template literal types become common in large libdefs, this may become a noticeable hotspot. Longer term, it might be worth reusing the existing token stream (or a lighter-weight expression parser) for the ${...} body instead of reparsing from a string.

crates/husk-dts-parser/src/utility_types.rs (2)
193-213: this_param on function types is not expanded by utility machinery

In expand_type_with_depth, the DtsType::Function branch expands parameter and return types but leaves this_param as a shallow clone:

DtsType::Function(Box::new(FunctionType {
type_params: func.type_params.clone(),
params: expanded_params,
return_type: Box::new(expanded_return),
this_param: func.this_param.clone(),
}))
whereas expand_object_member_with_depth does expand this_param for methods and call signatures.

This inconsistency means utility expansion (e.g. through aliases or registry lookups) won’t touch types that appear only in a standalone function’s this parameter, even though other positions are expanded. For consistency, consider mapping this_param through expand_type_with_depth in the function branch as well.

808-820: unwrap_promise ignores nested unions inside Promise<T>

unwrap_promise recursively unwraps Promise/JsPromise by pattern‑matching on:

DtsType::Named { name, type_args } if name == "Promise" || name == "JsPromise" => { ... }
but simply clones any other shape. This means Awaited<Promise<string | number>> will yield string | number only if the union is in type_args[0] and further Promise nesting is not inside a union or other wrapper. If you want closer alignment with TypeScript’s Awaited, you may eventually want to propagate into unions (e.g., mapping Awaited<Promise<A | B>> to Awaited<A> | Awaited<B>).

Not a correctness bug today, but something to keep in mind if you rely on Awaited semantics for more complex promise types.

crates/husk-lang/src/main.rs (1)
2172-2209: Consider letting CLI --follow-imports also enable entry_file_only

entry_file_only is currently derived only from entry.follow_imports:

let follow_imports_enabled = entry.follow_imports.unwrap_or(false);
…
entry_file_only: follow_imports_enabled,
so running huskc dts update --follow-imports without setting follow_imports in husk.toml will:

Resolve a multi‑file graph (because should_follow_imports ORs the CLI flag),
but still emit declarations for all dependencies, not just the entry file.
Given the comment about “avoiding dependency pollution”, it might be clearer to also respect the CLI flag here, e.g.:

-        let follow_imports_enabled = entry.follow_imports.unwrap_or(false);

*        let follow_imports_enabled =
*            follow_imports || entry.follow_imports.unwrap_or(false);
  …

-            entry_file_only: follow_imports_enabled,

*            entry_file_only: follow_imports_enabled,
  if the intended behavior is “whenever imports are followed, default to entry‑file‑only output”.

crates/husk-ast/src/lib.rs (1)
590-616: Propagate file paths into ExternProperty in impl blocks

ImplBlock::set_file_path and ExternItem::set_file_path currently only handle ImplItemKind::Method and ignore ImplItemKind::Property(ExternProperty). As a result, spans and attribute/name/type locations on extern properties never get a file set, which weakens multi-file diagnostics around #[getter]/#[setter]/#[js_name] on those properties.

You can extend the existing matches to cover Property as well. For example:

impl SetFilePath for ImplBlock {
fn set_file_path(&mut self, file: Arc<str>) {
…

-        for item in &mut self.items {
-            item.span.set_file_path(file.clone());
-            if let ImplItemKind::Method(method) = &mut item.kind {

*        for item in &mut self.items {
*            item.span.set_file_path(file.clone());
*            match &mut item.kind {
*                ImplItemKind::Method(method) => {
                     method.name.set_file_path(file.clone());
                     for param in &mut method.params {
                         param.name.set_file_path(file.clone());
                         param.ty.set_file_path(file.clone());
                     }
                     if let Some(ret) = &mut method.ret_type {
                         ret.set_file_path(file.clone());
                     }
                     for stmt in &mut method.body {
                         stmt.set_file_path(file.clone());
                     }

-                }

*                }
*                ImplItemKind::Property(prop) => {
*                    for attr in &mut prop.attributes {
*                        attr.span.set_file_path(file.clone());
*                        attr.name.set_file_path(file.clone());
*                    }
*                    prop.name.set_file_path(file.clone());
*                    prop.ty.set_file_path(file.clone());
*                }
*            }
           }
      }

  }
  and similarly in ExternItem::set_file_path:

          match &mut self.kind {
              …

-            ExternItemKind::Impl { type_params, self_ty, items } => {

*            ExternItemKind::Impl { type_params, self_ty, items } => {
                …

-                for item in items {
-                    item.span.set_file_path(file.clone());
-                    if let ImplItemKind::Method(method) = &mut item.kind {

*                for item in items {
*                    item.span.set_file_path(file.clone());
*                    match &mut item.kind {
*                        ImplItemKind::Method(method) => {
                             method.name.set_file_path(file.clone());
                             for param in &mut method.params {
                                 param.name.set_file_path(file.clone());
                                 param.ty.set_file_path(file.clone());
                             }
                             if let Some(ret) = &mut method.ret_type {
                                 ret.set_file_path(file.clone());
                             }
                             for stmt in &mut method.body {
                                 stmt.set_file_path(file.clone());
                             }

-                    }

*                        }
*                        ImplItemKind::Property(prop) => {
*                            for attr in &mut prop.attributes {
*                                attr.span.set_file_path(file.clone());
*                                attr.name.set_file_path(file.clone());
*                            }
*                            prop.name.set_file_path(file.clone());
*                            prop.ty.set_file_path(file.clone());
*                        }
*                    }
                   }
              }
              …
          }
  This keeps extern property diagnostics aligned with the rest of the AST’s file-path propagation.

Also applies to: 1247-1277, 1279-1344

crates/husk-parser/src/lib.rs (1)
3020-3087: Format placeholder span computation still mixes char offsets with byte-based spans

parse_format_string tracks char_offset and placeholder_start in terms of chars(), but then computes placeholder spans as span.range.start + placeholder_start .. span.range.start + char_offset, where span.range is byte-based. For non-ASCII format strings this will misalign spans (pre-existing behavior, not introduced by this PR).

If you expect diagnostics or tooling to rely on precise spans in Unicode-heavy format strings, consider switching to char_indices() and carrying byte offsets instead of char counts.

crates/husk-dts-parser/src/diagnostics.rs (1)
374-447: Union/utility/keyof analysis logic is coherent; union recursion trade-off is acknowledged

DtsType::Union is classified via analyze_union into UnionStrategy variants and only contributes to union-related stats/diagnostics (Option/enum/bool/JsValue/etc.) without recursing into member types. This keeps the union reporting focused but means nested utility/keyof usages inside unions aren’t reflected in utility_types_expanded or blocked_features.
DtsType::Named:
Increments utility_types_expanded exactly once per utility type occurrence via is_utility_type, avoiding the earlier double-count from both alias and usage.
Uses has_keyof_pattern to detect keyof anywhere under its type arguments, emits a single BlockedFeature error at the Named level, and intentionally does not recurse into type_args in that case, which prevents duplicate Named+KeyOf diagnostics.
DtsType::KeyOf still emits a warning + blocked_feature counter for standalone keyof usages.
This is a reasonable balance between detail and noise. If you later need per-variant stats inside unions/intersections, you can extend analyze_type to recurse into those collections as well.

Also applies to: 449-467, 493-547
