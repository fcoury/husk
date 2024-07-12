#!/bin/bash

# Check if a file argument is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <path_to_husk_file>"
    exit 1
fi

# The input file
INPUT_FILE="$1"

# Temporary file to store cargo output
TEMP_OUTPUT=$(mktemp)

# Run cargo command and store its output
if cargo run -q -- compile "$INPUT_FILE" > "$TEMP_OUTPUT"; then
    # If cargo command succeeded, pipe its output to node
    node < "$TEMP_OUTPUT"
    EXIT_CODE=$?
else
    # If cargo command failed, output its stderr and stdout
    cat "$TEMP_OUTPUT"
    EXIT_CODE=1
fi

# Clean up temporary file
rm "$TEMP_OUTPUT"

# Exit with the appropriate code
exit $EXIT_CODE
