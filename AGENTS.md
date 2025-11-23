# AGENTS Instructions (Root Scope)

These instructions apply to all files in this repository unless overridden by a more specific `AGENTS.md` in a subdirectory.

## Planning and Roadmapping

- Before starting any new task, or before continuing to the next task:
  - Read `plan.md` to understand the overall design, architecture, and priorities.
  - Review `roadmap.md` to see current progress and decide where the next change fits.

- After finishing a task:
  - Update `roadmap.md` to reflect the new status of any relevant items (e.g., change `[ ]` to `[x]`).
  - If the work materially affects the project direction or design, update `plan.md` as needed to keep it accurate.

- Always consult `plan.md` before making significant decisions about design or architecture.
- Use `roadmap.md` as the primary checklist for tracking progress.
- When you complete a unit of work, ensure the relevant checkboxes in `roadmap.md` are updated in the same change set when possible.

## General Coding Guidance

- Favor small, focused changes that clearly map to items in `roadmap.md`.
- Keep the Rust workspace structure consistent with what is described in `plan.md`, unless you intentionally update the plan.
- Follow existing patterns in the codebase for style, error handling, and module organization.

## Testing Guidance

- For each substantial task:
  - Prefer adding or updating **unit tests** near the beginning of the work (to clarify intent) or at the end (to lock in behavior), depending on what is more natural for the change.
  - Consider **integration tests** when touching multiple crates or end-to-end flows (e.g., parsing + type checking + codegen), or when reproducing/guarding against regressions.
- Keep tests small and focused, aligned with the items in `roadmap.md`.

## Formatting

- After completing a task (and updating `roadmap.md`), run `cargo fmt --all` at the workspace root to keep formatting consistent across all crates.

## Linting

- After completing a task, run `cargo clippy --all -- -D warnings` at the workspace root and for each warning reported, evaluate whether it's a problem or if it's related to unused code, if it's something we will use in the future, if we forgot to implement something or if we should just remove the code.

## Testing

- After completing a task, run `cargo test --all` at the workspace root to ensure all tests pass across all crates.
