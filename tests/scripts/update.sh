#!/bin/bash

# Directory containing the scripts with .hk extension
SCRIPT_DIR="tests/scripts"
# Flag to determine if files should be overwritten
FORCE_UPDATE=false

# Parse command line arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --force) FORCE_UPDATE=true ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

# Loop through all .hk files in the specified directory
for file in "$SCRIPT_DIR"/*.hk;
do
  # Check if there are any .hk files
  if [ ! -e "$file" ]; then
    echo "No .hk files found in $SCRIPT_DIR."
    exit 1
  fi

  # Extract the base name of the file (without extension)
  base_name=$(basename "$file" .hk)

  # Paths for .out and .err files
  out_file="$SCRIPT_DIR/$base_name.hk.out"
  err_file="$SCRIPT_DIR/$base_name.hk.err"

  # Check if the .out and .err files already exist
  if [ -f "$out_file" ] || [ -f "$err_file" ]; then
    if [ "$FORCE_UPDATE" = false ]; then
      echo "$base_name.hk.out and/or $base_name.hk.err exists, skipping."
      continue
    fi
  fi

  # Run the command and update .out and .err files
  cargo run -q -- "$file" > "$out_file" 2> "$err_file"

  echo "Updated $base_name.hk.out and $base_name.hk.err"
done

