#!/usr/bin/env bash
set -euo pipefail

# Simple helper to compile a Husk source file and run the generated JS with Node.
#
# Usage:
#   scripts/run_node_example.sh path/to/file.hk
#
# The script:
#   - Invokes `cargo run --bin huskc -- compile <file>` at the workspace root.
#   - Writes the generated JS to `target/husk-node-example.js`.
#   - Executes it with `node`.

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

if [[ $# -ne 1 ]]; then
  echo "Usage: scripts/run_node_example.sh path/to/file.hk" >&2
  exit 1
fi

SRC="$1"

if [[ ! -f "$SRC" ]]; then
  echo "Source file not found: $SRC" >&2
  exit 1
fi

OUT_JS="target/husk-node-example.js"

echo "[husk] compiling $SRC -> $OUT_JS"
cargo run --quiet --bin huskc -- compile "$SRC" > "$OUT_JS"

echo "[husk] running under Node: $OUT_JS"
node "$OUT_JS"

