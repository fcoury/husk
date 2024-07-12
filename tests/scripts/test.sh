#!/bin/bash

# Directory containing the scripts with .hk extension
SCRIPT_DIR="tests/scripts"
# Temporary files to capture stdout and stderr
TEMP_STDOUT="temp-stdout"
TEMP_STDERR="temp-stderr"
# Variable to track if any test fails
ANY_TEST_FAILED=0
# Flag for transpile mode
TRANSPILE_MODE=0

# Check for transpile option
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -t|--transpile) TRANSPILE_MODE=1 ;;
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
  base_name=$(basename "$file")

  # Run the script and capture the output
  if [ $TRANSPILE_MODE -eq 1 ]; then
    # ./target/debug/husk compile "$file" | node > "$TEMP_STDOUT" 2> "$TEMP_STDERR"
    SCRIPT_PATH="$(realpath "$0")"
    SCRIPT_DIR="$(dirname "$SCRIPT_PATH")"
    $SCRIPT_DIR/transpile.sh "$file" > "$TEMP_STDOUT" 2> "$TEMP_STDERR"
  else
    ./target/debug/husk "$file" > "$TEMP_STDOUT" 2> "$TEMP_STDERR"
  fi
  echo "$base_name"

  # Compare stdout if the .out file exists
  if [ $TRANSPILE_MODE -eq 1 ]; then
    out_file="$SCRIPT_DIR/$base_name.tr.out"
  else
    out_file="$SCRIPT_DIR/$base_name.out"
  fi

  if [ -f "$out_file" ]; then
    if diff --color=always "$TEMP_STDOUT" "$out_file" > /dev/null; then
      echo "  stdout ✅ "
    else
      echo "  stdout ❌"
      diff "$TEMP_STDOUT" "$out_file"
      ANY_TEST_FAILED=1
    fi
  else
    echo "  skipped stdout"
  fi
  
  # Compare stderr if the .err file exists
  if [ $TRANSPILE_MODE -eq 1 ]; then
    err_file="$SCRIPT_DIR/$base_name.tr.err"
  else
    err_file="$SCRIPT_DIR/$base_name.err"
  fi

  if [ -f "$err_file" ]; then
    if diff --color=always "$TEMP_STDERR" "$err_file" > /dev/null; then
      echo "  stderr ✅ "
    else
      echo "  stderr ❌"
      diff "$TEMP_STDERR" "$err_file"
      ANY_TEST_FAILED=1
    fi
  else
    echo "  skipped stderr"
  fi

  echo ""
done

# Cleanup temporary files
rm -f "$TEMP_STDOUT" "$TEMP_STDERR"

# Exit with error code if any test failed
if [ $ANY_TEST_FAILED -ne 0 ]; then
  echo "Some tests failed."
  exit 1
else
  exit 0
fi
