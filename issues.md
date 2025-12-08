crates/husk-lang/src/load.rs (1)
214-295: Extern const imports and Phase 2 extern filtering have gaps in coverage

When importing an extern const via use (e.g., use crate::foo::CONST;), the const is located correctly by find_pub_item but never actually included in the assembled root. The issue is that Phase 1 only populates imported_types for structs and enums (line 248–254), not for extern consts. When filter_extern_block checks imported_types.contains(&name.name) for consts (line 526–532), the lookup always fails.

Phase 2's inline extern block handling diverges from filter_extern_block in three ways:

The fn allowlist (line 335) is ["express", "better_sqlite3"] instead of ["express", "better_sqlite3", "json"].
The filter explicitly rejects all Const items (line 340: \_ => false).
Mods are filtered with true but never affect has_new (lines 357–362 only check structs and fns), so extern blocks containing only mods are dropped.
Meanwhile, included_mods and included_consts are maintained at the top but only exercised in filter_extern_block, not in Phase 2.

To consolidate the logic and fix these gaps:

Track extern consts in Phase 1 imported_types (lines 248–254):
ItemKind::ExternBlock { items: ext_items, .. } => {
// If we're importing from an extern block, track the type
for ext in ext_items {

-                                   if let ExternItemKind::Struct { name, .. } = &ext.kind {
-                                       if name.name == item_name {
-                                           imported_types.insert(name.name.clone());
-                                       }
-                                   }

*                                   match &ext.kind {
*                                       ExternItemKind::Struct { name, .. } |
*                                       ExternItemKind::Const { name, .. } => {
*                                           if name.name == item_name {
*                                               imported_types.insert(name.name.clone());
*                                           }
*                                       }
*                                       _ => {}
*                                   }
                                  }
                              }
  Reuse filter_extern_block in Phase 2 (lines 333–384) instead of inline filtering:
  ItemKind::ExternBlock {

-                   items: ext_items, ..
-               } => {
-                   let filtered: Vec<_> = ext_items
-                       .iter()
-                       .filter(|ext| match &ext.kind {
-                           ExternItemKind::Struct { name, .. } => {
-                               imported_types.contains(&name.name)
-                           }
-                           ExternItemKind::Fn { name, .. } => {
-                               name.name == "express" || name.name == "better_sqlite3"
-                           }
-                           ExternItemKind::Mod { .. } => true,
-                           _ => false,
-                       })
-                       .cloned()
-                       .collect();
-
-                   if filtered.is_empty() {
-                       continue;
-                   }
-
-                   let mut has_new = false;
-                   for ext in &filtered {
-                       match &ext.kind {
-                           ExternItemKind::Struct { name, .. } => {
-                               if included_structs.insert(name.name.clone()) {
-                                   has_new = true;
-                               }
-                           }
-                           ExternItemKind::Fn { name, .. } => {
-                               if included_fns.insert(name.name.clone()) {
-                                   has_new = true;
-                               }
-                           }
-                           _ => {}
-                       }
-                   }
-                   if has_new {
-                       let mut new_block = item.clone();
-                       if let ItemKind::ExternBlock {
-                           items: block_items, ..
-                       } = &mut new_block.kind
-                       {
-                           *block_items = filtered;
-                       }
-                       new_block.set_file_path(module.file_path.clone());
-                       items.push(new_block);
-                   }
-               }

*                   abi,
*                   items: ext_items,
*               } => {
*                   if let Some(mut filtered) = filter_extern_block(
*                       ext_items,
*                       abi,
*                       &imported_types,
*                       &mut included_structs,
*                       &mut included_fns,
*                       &mut included_mods,
*                       &mut included_consts,
*                   ) {
*                       filtered.set_file_path(module.file_path.clone());
*                       items.push(filtered);
*                   }
*               }
  This eliminates duplication, ensures consistent treatment of consts and json across both phases, and fully exercises the included\_\* tracking already in place.

In crates/husk-ast/src/lib.rs around lines 1156 to 1165, the SetFilePath
implementation for ItemKind::Fn sets file paths for param.name and param.ty but
omits param.attributes; update the loop that processes params to also iterate
over each param.attributes and call set_file_path(file.clone()) on each
attribute (mirroring how ExternProperty/ExternItem handle attributes), ensuring
attributes receive the file path as well.

In crates/husk-ast/src/lib.rs around lines 1226–1229, trait method parameters
are not having their file-path attributes propagated; add the same propagation
loop used for regular function parameters by iterating over &mut method.params
and calling set_file_path(file.clone()) on each param.name and param.ty so trait
method parameter names and types receive the file path attribute.

In crates/husk-ast/src/lib.rs around lines 1263 to 1275, the SetFilePath
implementation for ImplItemKind::Method currently sets file paths for the method
name, params, return type and body but omits the method's attributes; update
this block to also propagate file paths to the method.attributes (the same way
ImplItemKind::Property handles ExternProperty attributes) by iterating over the
method's attributes and calling set_file_path with file.clone() for each
attribute (or otherwise delegating to the attributes collection's set_file_path
helper) so method attributes receive the same file-path propagation.

In crates/husk-ast/src/lib.rs around lines 1265–1268, the loop that applies
file-path to impl method parameters sets it on param.name and param.ty but
doesn't propagate the file path to the parameter attributes; update the loop to
also set the file path on each parameter's attributes (e.g., iterate over
param.attrs or call the attrs' set_file_path method with file.clone()) so all
parameter metadata receives the same file-path propagation.

In crates/husk-ast/src/lib.rs around lines 1299–1302, extern function parameters
currently have only their name and type file_path set; you must also propagate
the file_path to each parameter's attributes so attribute-related data is
processed for extern params. Update the loop to call the same set_file_path (or
equivalent file-path propagation method) on the parameter.attributes (or attrs)
field for each param (and any other per-parameter metadata that holds
attributes), ensuring attributes receive the file path just like name and ty.

In crates/husk-ast/src/lib.rs around lines 1330 to 1342, the SetFilePath
handling for ImplItemKind::Method updates method name, params, return type and
body but skips method attributes; update this block to also iterate over
method.attrs (or equivalent attribute field) and call
set_file_path(file.clone()) on each attribute so extern impl methods propagate
file-path to their attributes consistently with other nodes.

In crates/husk-ast/src/lib.rs around lines 1332–1335, the loop that sets file
paths for extern impl method parameters only updates param.name and param.ty;
you must also propagate the file path to the parameter attributes. Update the
loop to set the file path on the parameter attributes (e.g., call the
appropriate set_file_path on param.attrs or each attribute container) using the
same file.clone(), so all parameter-related nodes get the file path.

In crates/husk-ast/src/lib.rs around lines 1372 to 1375, module function
parameters currently have their name and type file_path set but their attributes
are not; update the loop to also propagate the file path to each parameter
attribute by iterating param.attrs and calling set_file_path(file.clone()) for
each attribute so all parameter attributes receive the same file path context.
