# AGENTS Instructions (Root Scope)

These instructions apply to all files in this repository unless overridden by a more specific `AGENTS.md` in a subdirectory.

## General Coding Guidance

- Favor small, focused changes that clearly map to items in `roadmap.md`.
- Keep the Rust workspace structure consistent with what is described in `plan.md`, unless you intentionally update the plan.
- Follow existing patterns in the codebase for style, error handling, and module organization.

## Testing Guidance

- For each substantial task:
  - Prefer adding or updating **unit tests** near the beginning of the work (to clarify intent) or at the end (to lock in behavior), depending on what is more natural for the change.
  - Consider **integration tests** when touching multiple crates or end-to-end flows (e.g., parsing + type checking + codegen), or when reproducing/guarding against regressions.
- Keep tests small and focused, aligned with the items in `roadmap.md`.
- When adding new language features or fixing bugs, consider adding or updating **examples** under `examples/` (both feature-focused and integration-style) so they are exercised by the integration tests in `tests/examples.rs`.

## Formatting

- After completing a task (and updating `roadmap.md`), run `cargo fmt --all` at the workspace root to keep formatting consistent across all crates.

## Linting

- After completing a task, run `cargo clippy --all -- -D warnings` at the workspace root and for each warning reported, evaluate whether it's a problem or if it's related to unused code, if it's something we will use in the future, if we forgot to implement something or if we should just remove the code.

## Testing

- After completing a task, run `cargo test --all` at the workspace root to ensure all tests pass across all crates.
