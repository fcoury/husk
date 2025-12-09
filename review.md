examples/feature_match/simple_match.hk (1)
100-104: Update tests to use index_of / last_index_of instead of indexOf / lastIndexOf

The tests still call the old camelCase methods, which will break once the string API is renamed to snake_case.

Suggested diff:

-    assert_eq(s.indexOf("hello"), 0);
-    assert_eq(s.lastIndexOf("hello"), 12);
-    assert_eq(s.indexOf("xyz"), -1);
+    assert_eq(s.index_of("hello"), 0);
+    assert_eq(s.last_index_of("hello"), 12);
+    assert_eq(s.index_of("xyz"), -1);

crates/husk-codegen-js/src/lib.rs lines 925-993: contains_try_expr currently
ignores let-else else_block and doesn't handle IfLet, so ? inside those branches
won't be detected; update check_stmt to inspect StmtKind::LetElse (or the
crate's let-else variant) by checking both the value and the else_block stmts,
and add handling for StmtKind::IfLet (or IfLet pattern) to check the condition,
then_branch stmts and else_branch (similar to existing If handling); ensure you
replace the final `_ => false` arm with the new branches (not duplicate) so all
branches return whether they contain Try expressions.

In crates/husk-parser/src/lib.rs around lines 872 to 889, the loop that parses
trait methods uses a hard method_count guard that breaks silently after 100
methods; remove the method_count variable and the if method_count > 100 { break;
} branch so the loop relies on existing progress checks (pos_before,
synchronize_item, advance) to avoid infinite loops; alternatively, if you want
an explicit limit, replace the silent break with emitting a parser
error/diagnostic when the limit is exceeded before breaking so the user is
informed.

In stdlib/core.hk around lines 279-280, the map method is declared as fn
map(self, f: fn(T) -> T) -> impl Iterator<T>, which incorrectly forces output
type to equal input type; change the signature to introduce a type parameter for
the output (e.g., fn map<U>(self, f: fn(T) -> U) -> impl Iterator<U>) so callers
can map to different types, or if that's not feasible due to current
type-inference/FFI constraints, add a clear comment above this declaration
explaining the restriction and why a generic U cannot be used.

crates/husk-types/src/lib.rs (1)
40-46: LGTM! Consider adding a constructor helper for consistency.

The new ImplTrait variant is well-documented and follows the existing pattern for complex Type variants. The implementation correctly stores the underlying trait type in a Box.

Optionally, consider adding a constructor helper for consistency with other Type variants:

+    pub fn impl_trait(trait_ty: Type) -> Self {
+        Type::ImplTrait {
+            trait_ty: Box::new(trait_ty),
+        }
+    }
crates/husk-semantic/src/lib.rs (7)
197-239: Deduplicate Type formatting helpers to avoid divergence

You now have both a free fn format_type(ty: &Type) and TypeChecker::format_type(&self, &Type) that implement essentially the same formatting (including ImplTrait). This duplication risks subtle divergence over time and makes it harder to keep diagnostics consistent.

Consider consolidating to a single implementation (e.g., keep the free helper and have TypeChecker::format_type delegate to it, or vice versa) and use it everywhere errors/hover strings need a human‑readable type name.

Also applies to: 4570-4610

250-253: Tighten prelude loader: remove stray stderr flushes and simplify API

Two points here:

prelude_file() now does multiple std::io::stderr().flush().ok(); calls before and inside OnceLock::get_or_init. These don’t log anything, add a tiny bit of overhead, and look like leftover debugging. They can safely be removed or gated under a debug flag.

get_prelude_file() is a thin public wrapper over prelude_file(), which is fine for codegen, but it might be clearer to make prelude_file itself pub or keep only the public name and mark the internal one as a simple alias to reduce indirection.

Neither is a correctness issue, but cleaning this up will make the prelude integration easier to follow.

Also applies to: 293-307, 310-315

404-405: Builtin Range/Set/Map typing and field access look coherent, with a small caveat

The additions for:

TypeExprKind::ImplTrait in type_expr_to_trait_name / type_expr_to_name / substitute_type_param
Built‑in handling of Range<T>, Set<T>, and Map<K, V> in resolve_named_type
Special‑casing Set::new() and Map::new() in check_path_expr
Range field access (start / end / inclusive)
Range expression typing (ExprKind::Range → Range<i32>)
are internally consistent and match the intended iterator/range model.

One thing to be aware of: by short‑circuiting resolve_named_type for Range / Set / Map, user‑defined types or aliases with these names are effectively shadowed and can’t override the builtin behaviour. If that’s intentional (treating them as reserved names), this is fine; if not, you may want to gate these branches behind a “no existing struct/enum/alias with this name” check.

Also applies to: 617-618, 660-663, 1495-1571, 1788-1814, 2821-2847, 3799-3832

1168-1205: Iterator/collection typing is mostly solid; clarify generic matching and .collect() behaviour

The iterator plumbing (impl‑trait typing, generic impl lookup via Set<T> / Map<K, V> / [T], and substitution in method return types) looks good and will drive codegen cleanly.

For .collect() specifically:

check_collect_method correctly uses turbofish or contextual expected type and records the resolved collection type in type_resolution.
When no type info is available, defaulting to [Elem] is a reasonable MVP.
Two small behavioural points worth calling out:

check_collect_method doesn’t currently verify that the target collection’s element type matches the iterator’s elem_ty; any target type is accepted as long as it can be resolved. If you care about catching obvious mismatches (e.g., let xs: [String] = iter_of_i32.collect();), this is a natural place to at least sanity‑check element types once you have a FromIterator story.

In method resolution, the matching of generic impls is string‑based (Set<T>, Map<K, V>, [T]). That works for the current surface but is a bit brittle long‑term (e.g., if you ever allow different parameter names). Longer‑term, you may want a more structured representation of “base type + arity” instead of hard‑coded T/K/V in the substitution paths.

Given the current goals, this is acceptable, but documenting these constraints/assumptions near check_collect_method and resolve_named_type would help future maintainers.

Also applies to: 1495-1571, 3274-3339, 4318-4365

1259-1295: Main/Termination validation aligns with intended shape constraints

The new main return‑type validation:

Accepts (), or Result<T, E> (two generic args), and rejects other return types.
Uses the same formatted type strings as the rest of diagnostics and gives a clear, Rust‑like help message mentioning Termination.
The accompanying tests (main_returning_unit_is_valid, main_returning_result_is_valid, main_returning_i32_is_invalid, main_returning_string_is_invalid, and the mixed main/helper cases) exercise both the positive and negative paths.

If, in the future, you want to honour the actual Termination trait from the prelude, you could tighten this to call env.type_implements_trait(<ret_ty_name>, "Termination") instead of hard‑coding Result/(), but for now the explicit shape check is simple and matches the current tests.

Also applies to: 8235-8289

695-703: Remove no-op loop in build_type_env and reconsider generic params in let annotations

Two unrelated observations in this area:

The first for (_idx, item) in file.items.iter().enumerate() loop in build_type_env only matches on ItemKind::Trait / ItemKind::Impl and does nothing (besides computing an unused _self_ty_name). This is dead work and can just be removed to keep the function focused.

In check_stmt’s StmtKind::Let arm, annotated types are resolved with self.tcx.resolve_type_expr(t, &[]). That means type annotations like let x: T = ...; inside a generic fn foo<T>(...) will treat T as an unknown, non‑generic type rather than a type parameter. If the language is meant to allow generics in local type annotations (which is the usual expectation), you’ll likely want to thread the enclosing function’s generic parameter names into FnContext and use them here instead of an empty slice.

The first is just cleanup; the second is a potential semantic limitation worth confirming against your language design.

Also applies to: 2120-2150

2951-3060: Iterator and array higher-order methods: good coverage, small inference gaps

The HOF support you’ve added for:

Array methods (some / every / filter / map / reduce / forEach / find / findIndex / findLastIndex / sort / sortBy), and
Iterator methods (map / filter / collect / for_each / fold / enumerate / take / skip / find),
with closure parameter inference via check_expr_with_expected, is nicely structured and should make iterator usage pleasant.

Two small gaps to be aware of:

The .collect() fast‑path in check_expr_with_expected only recognizes receivers of type impl Iterator<T> (via Type::ImplTrait). If users ever write explicit Iterator<T> types for variables/params, .collect() on those will fall back to the generic check_expr path and ignore turbofish/context. You may eventually want to mirror the ImplTrait+Named("Iterator") logic used earlier in iterator_elem_ty.

For some array methods (e.g., reduce), the accumulator/result type is constrained to the element type T. That’s good enough for many cases but doesn’t model the more general Fn(B, T) -> B with an independent accumulator type. If/when you need more expressiveness there, this would be the place to relax that assumption.

Overall, though, the new inference paths look correct and are in line with the iterator goals of this PR.

Also applies to: 3062-3191, 3196-3243, 4055-4131, 4318-4365

crates/husk-lang/tests/builtin_types.rs (2)
104-124: Consider testing that .size is not followed by other characters.

The test checks js.contains("m.size") and !js.contains("m.size()"), but this could match m.size_other if such a pattern existed. The current assertions are likely sufficient for this codebase, but a regex match like m\.size\b would be more precise.

426-439: The assertion condition may be overly permissive.

The test checks for either "? (function()" or "1 < 2 ?" in the output. If the codegen implementation changes, this test might pass incorrectly. Consider tightening to verify the specific expected output pattern.

-    assert!(
-        js.contains("? (function()") || js.contains("1 < 2 ?"),
-        "expected ternary/IIFE in JS output, got:\n{}",
-        js
-    );
+    // If-expressions should generate a ternary operator pattern
+    assert!(
+        js.contains("1 < 2 ?") || js.contains("(1 < 2) ?"),
+        "expected ternary operator in JS output, got:\n{}",
+        js
+    );
stdlib/core.hk (1)
515-535: Remove or track commented-out IntoIterator implementations.

These commented-out implementations reference "temporarily commented out to debug hang". If the hang is resolved, these should be uncommented. If they're intentionally excluded, they should be removed to avoid code rot.

Consider either:

Removing the commented code if the implementations aren't needed
Uncommenting if the hang is resolved
Adding a TODO with an issue reference if this needs future work
-// NOTE: Temporarily commented out to debug hang - these call self.into_iter() which might cause resolution issues
-// impl<T> IntoIterator<T> for [T] {
-//     fn into_iter(self) -> impl Iterator<T> {
-//         self.into_iter()
-//     }
-// }
-
-// String implements IntoIterator (yields String characters)
-// impl IntoIterator<String> for String {
-//     fn into_iter(self) -> impl Iterator<String> {
-//         self.into_iter()
-//     }
-// }
-
-// Range implements IntoIterator (yields i32 values)
-// impl IntoIterator<i32> for Range {
-//     fn into_iter(self) -> impl Iterator<i32> {
-//         self.into_iter()
-//     }
-// }
crates/husk-codegen-js/src/lib.rs (2)
1450-1532: Avoid multiple evaluations of range_expr in ForRange codegen

The ForIn lowering for range-typed variables emits a JsStmt::ForRange { binding, range_expr, body }, and write_stmt prints it as:

for (let i = RANGE.start;
    i < (RANGE.inclusive ? RANGE.end + 1 : RANGE.end);
    i++) { ... }
where RANGE is range_expr, written out four times (initializer, condition, both branches of the ternary).

If range_expr is not a simple identifier/property but an arbitrary expression (e.g. get_range()), this will cause it to be evaluated multiple times, which is both semantically wrong (if it has side effects or is expensive) and inefficient.

A safer pattern would be to evaluate range_expr once into a temporary and iterate over that, e.g.:

const __range = <range_expr>;
for (let i = __range.start;
    i < (__range.inclusive ? __range.end + 1 : __range.end);
    i++) {
...
}
One way to achieve this is:

Change lower_stmt’s Range-typed branch to emit a JsStmt::Sequence of a let __range = <expr> followed by a JsStmt::ForRange whose range_expr is just JsExpr::Ident("__range"), or
Extend JsStmt::ForRange to carry both the binding and a pre-bound range identifier instead of the full expression.
That keeps this PR’s AST shape but avoids repeated evaluation in the generated JS.

Also applies to: 4127-4158

2185-2595: Iterator / collection method lowering is powerful but relies on string-typed type info

The new branches in ExprKind::MethodCall for:

type-directed getters (String.len → .length, Map.len → .size),
Range.contains / .is_empty,
Map.keys / .values, Set.values,
iter() / into_iter() for arrays, strings, and ranges, and
iterator adaptors (map, filter, collect, for_each, fold, enumerate, take, skip, find)
look consistent with the iterator design described in the PR and nicely leverage type_resolution plus the new PropertyAccessors.

Two caveats to keep in mind:

The dispatch relies on string patterns in type_resolution (e.g. ty == "String", ty.starts_with("Map"), ty.starts_with("[")). Any future changes in how semantic pretty-prints types (e.g. adding namespaces/modules, or changing array formatting) will silently break these hooks. It would be safer long term if TypeResolution carried a more structured tag (enum or canonical type ID) that codegen could pattern-match on.

Iterator adaptors are currently triggered purely by method names and arity ("map" with one arg, "collect" with none, etc.) without re-checking that the receiver type is an actual iterator. Today that’s probably fine because only Iterator implementors will have these methods, but it does mean that a non-iterator type with a legitimate map method would get rewritten to __husk_iterator_map(...) unexpectedly.

Neither is a blocker for this PR, but they’re good candidates for future tightening, ideally alongside additional semantic metadata indicating “this is an Iterator” instead of inferring solely from strings.

Also applies to: 2407-2483

crates/husk-parser/src/lib.rs (2)
913-989: Trait methods: extern handling and attributes

The new parse_trait_method correctly supports extern "js" fn before fn and fixes the earlier hanging cases around parameter lists and return types.

Two points to consider:

Attributes are parsed into _attributes and then discarded. If trait methods are ever meant to carry attributes (e.g., cfg, js_name), those will currently be silently ignored.
The extra if current == RParen { advance(); } after parse_method_param_list() is a nice safety net for error paths; on successful parses the RParen has already been consumed, so this shouldn’t regress valid cases.
If attributes on trait methods are expected in future, wiring _attributes into TraitMethod (or TraitItem) would be a useful follow-up.

1643-1919: Impl Trait and function-type parsing: behavior matches PR goals, with one caveat

The parse_type_expr additions:

Handle impl Trait by wrapping the following type expression in TypeExprKind::ImplTrait, with a start..end span covering the entire impl Trait<...>.
Allow fn(...) types with:
Parameter types parsed via parse_type_expr (with & stripped and ignored for now).
Optional return type: without -> you default to unit (); with -> but a parsing failure you also fall back to () while still recording the parse error.
Add a defensive loop counter (loop_count) and forward progress in generic arguments to avoid infinite loops.
All of this is consistent with “impl Trait return types” and the stated plan to treat references in type position as syntactic sugar for now.

One thing to watch: when you see impl followed by another impl, you just return None without emitting a dedicated diagnostic. This will be reported later as a generic “expected type”, which might be slightly opaque for the user. If this pattern is actually arising in real code, consider emitting a more specific error before returning None.

crates/husk-semantic/src/stdlib/core.hk (4)
201-204: PartialEq impls for Option and Result: verify intended generic behavior

impl PartialEq for Option {} and impl PartialEq for Result {} are written without explicit type parameters. Depending on how your type system models generics, this likely means “all instantiations of Option and Result are PartialEq”, which is what you want.

If, however, your semantic layer distinguishes between Option as a type constructor and Option<T> as a concrete type, you may need explicit generics here (e.g., impl<T> PartialEq for Option<T> {}).

Given the rest of the codebase, this is probably fine, but worth double‑checking.

262-319: Iterator trait: map/fold signatures are currently type-preserving

The Iterator<T> trait and its extern methods look consistent with the JS runtime hooks (__husk_iterator_*) and iterator goals. One design nuance:

map(self, f: fn(T) -> T) -> impl Iterator<T> and fold(self, init: T, f: fn(T, T) -> T) -> T are strictly type‑preserving in the item type (T), unlike Rust’s map/fold which allow changing the item/accumulator types.
If you intend to support map that changes the item type (e.g., Iterator<i32> -> Iterator<String>) or a fold with a different accumulator type, you’ll eventually need more type parameters here.

For the initial MVP this is acceptable, but documenting this limitation (and possibly naming it out in comments) would help avoid surprises.

333-335: IntoIterator trait defined but concrete impls are commented out

You define:

trait IntoIterator<T> {
fn into_iter(self) -> impl Iterator<T>;
}
but the concrete impls for [T], String, and Range are commented out with a note about hangs due to self.into_iter() resolution.

Given that you’ve added inherent into_iter methods on String, [T], and Range, for ... in lowering can still work by targeting those methods directly, but the IntoIterator trait remains effectively unused.

Once the resolution issue is understood, consider:

Reinstating the impls with bodies that call the inherent extern "js" methods explicitly (to avoid recursive trait resolution), or
Clarifying in comments that IntoIterator is currently only a marker trait and not relied on by for desugaring yet.
Also applies to: 515-535

460-509: Array [T] iterator methods: double-check reference semantics vs Iterator<T>

For arrays you add:

impl<T> [T] {
#[js_name = "__husk_array_iter"]
extern "js" fn iter(&self) -> impl Iterator<T>;

#[js_name = "__husk_array_into_iter"]
extern "js" fn into_iter(self) -> impl Iterator<T>;
}
The doc comment says the iterator “yields references to each element”, but the type is Iterator<T>, not Iterator<&T> (there’s no reference type in signatures yet).

If the runtime actually yields owned T values, the comment is slightly misleading; if it yields reference‑like handles, the mismatch is only in wording. It’d be clearer either to:

Align the comment with the actual runtime behavior (owned vs referenced semantics), or
Once reference types are modeled more explicitly, switch to an Iterator<&T> form for iter(&self).