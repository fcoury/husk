#!/bin/bash

# Directory containing the scripts with .hk extension
SCRIPT_DIR="tests/scripts"

# Flag to determine if files should be overwritten
FORCE_UPDATE=false

# Specific test file to update (optional)
SPECIFIC_TEST=""

# Parse command line arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --force) FORCE_UPDATE=true ;;
        *.hk) SPECIFIC_TEST="$1" ;;
        *)
            # Check if it's a test name without .hk extension
            if [[ "$1" != --* ]] && [ -f "$SCRIPT_DIR/$1.hk" ]; then
                SPECIFIC_TEST="$1.hk"
            else
                echo "Unknown parameter passed: $1"
                echo "Usage: $0 [--force] [test_name.hk | test_name]"
                echo "  --force: Overwrite existing output files"
                echo "  test_name: Specific test to update (with or without .hk extension)"
                exit 1
            fi
            ;;
    esac
    shift
done

# Function to run command and update files
update_files() {
    local file=$1
    local out_file=$2
    local err_file=$3
    local command=$4

    if [ -f "$out_file" ] || [ -f "$err_file" ]; then
        if [ "$FORCE_UPDATE" = false ]; then
            echo "$(basename "$out_file") and/or $(basename "$err_file") exists, skipping."
            return
        fi
    fi

    eval "$command" > "$out_file" 2> "$err_file"
    echo "Updated $(basename "$out_file") and $(basename "$err_file")"
}

# Function to process a single file
process_file() {
    local file=$1

    # Extract the base name of the file (without extension)
    base_name=$(basename "$file" .hk)

    # Paths for normal .out and .err files
    out_file="$SCRIPT_DIR/$base_name.hk.out"
    err_file="$SCRIPT_DIR/$base_name.hk.err"

    # Paths for transpiled .out and .err files
    tr_out_file="$SCRIPT_DIR/$base_name.hk.tr.out"
    tr_err_file="$SCRIPT_DIR/$base_name.hk.tr.err"

    echo "Processing $(basename "$file")..."

    # Update normal version
    update_files "$file" "$out_file" "$err_file" "cargo run -q -- \"$file\""

    # Update transpiled version
    SCRIPT_PATH="$(realpath "$0")"
    SCRIPT_DIR_PATH="$(dirname "$SCRIPT_PATH")"
    update_files "$file" "$tr_out_file" "$tr_err_file" "$SCRIPT_DIR_PATH/transpile.sh \"$file\""

    echo "Finished processing $(basename "$file")"
    echo
}

# If a specific test is provided, process only that test
if [ -n "$SPECIFIC_TEST" ]; then
    # Handle both with and without .hk extension
    if [[ "$SPECIFIC_TEST" == *.hk ]]; then
        test_file="$SCRIPT_DIR/$SPECIFIC_TEST"
    else
        test_file="$SCRIPT_DIR/$SPECIFIC_TEST.hk"
    fi

    if [ ! -f "$test_file" ]; then
        echo "Error: Test file '$test_file' not found."
        exit 1
    fi

    echo "Updating specific test: $(basename "$test_file")"
    echo
    process_file "$test_file"
    exit 0
fi

# Original behavior: Loop through all .hk files in the specified directory
echo "Updating all tests in $SCRIPT_DIR..."
echo

for file in "$SCRIPT_DIR"/*.hk; do
    # Check if there are any .hk files
    if [ ! -e "$file" ]; then
        echo "No .hk files found in $SCRIPT_DIR."
        exit 1
    fi

    process_file "$file"
done

echo "All tests have been processed."
