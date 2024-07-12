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

# Loop through all .hk files in the specified directory
for file in "$SCRIPT_DIR"/*.hk; do
    # Check if there are any .hk files
    if [ ! -e "$file" ]; then
        echo "No .hk files found in $SCRIPT_DIR."
        exit 1
    fi
    
    # Extract the base name of the file (without extension)
    base_name=$(basename "$file" .hk)
    
    # Paths for normal .out and .err files
    out_file="$SCRIPT_DIR/$base_name.hk.out"
    err_file="$SCRIPT_DIR/$base_name.hk.err"
    
    # Paths for transpiled .out and .err files
    tr_out_file="$SCRIPT_DIR/$base_name.hk.tr.out"
    tr_err_file="$SCRIPT_DIR/$base_name.hk.tr.err"
    
    # Update normal version
    update_files "$file" "$out_file" "$err_file" "cargo run -q -- \"$file\""
    
    # Update transpiled version
    # update_files "$file" "$tr_out_file" "$tr_err_file" "cargo run -q -- compile \"$file\" | node"
    SCRIPT_PATH="$(realpath "$0")"
    SCRIPT_DIR="$(dirname "$SCRIPT_PATH")"
    update_files "$file" "$tr_out_file" "$tr_err_file" "$SCRIPT_DIR/transpile.sh \"$file\""
done
