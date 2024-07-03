#!/bin/bash

# Directory containing the scripts with .hk extension
SCRIPT_DIR="tests/scripts"
# Temporary files to capture stdout and stderr
TEMP_STDOUT="temp-stdout"
TEMP_STDERR="temp-stderr"

# Variable to track if any test fails
ANY_TEST_FAILED=0

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
  ./target/debug/husk run "$file" > "$TEMP_STDOUT" 2> "$TEMP_STDERR"

  # Compare stdout if the .out file exists
  if [ -f "$SCRIPT_DIR/$base_name.out" ]; then
    if diff -q "$TEMP_STDOUT" "$SCRIPT_DIR/$base_name.out" > /dev/null; then
      echo "stdout matches for $base_name"
    else
      echo "stdout does not match for $base_name"
      ANY_TEST_FAILED=1
    fi
  fi

  # Compare stderr if the .err file exists
  if [ -f "$SCRIPT_DIR/$base_name.err" ]; then
    if diff -q "$TEMP_STDERR" "$SCRIPT_DIR/$base_name.err" > /dev/null; then
      echo "stderr matches for $base_name"
    else
      echo "stderr does not match for $base_name"
      ANY_TEST_FAILED=1
    fi
  fi
done

# Cleanup temporary files
rm -f "$TEMP_STDOUT" "$TEMP_STDERR"


# Exit with error code if any test failed
if [ $ANY_TEST_FAILED -ne 0 ]; then
  echo Some tests failed.
  exit 1
else
  exit 0
fi
