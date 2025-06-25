#!/bin/bash

# Directory containing the scripts with .hk extension
SCRIPT_DIR="tests/scripts"

# Flag for bare output
BARE_OUTPUT=false

# Parse command line arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --bare) BARE_OUTPUT=true ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

# Colors for output (optional, remove if you don't want colors)
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Counters
total_files=0
missing_any=0
missing_normal=0
missing_transpiled=0

if [ "$BARE_OUTPUT" = false ]; then
    echo "Checking for missing output files in $SCRIPT_DIR..."
    echo
fi

# Loop through all .hk files in the specified directory
for file in "$SCRIPT_DIR"/*.hk; do
    # Check if there are any .hk files
    if [ ! -e "$file" ]; then
        echo "No .hk files found in $SCRIPT_DIR."
        exit 1
    fi

    total_files=$((total_files + 1))

    # Extract the base name of the file (without extension)
    base_name=$(basename "$file" .hk)

    # Paths for normal .out and .err files
    out_file="$SCRIPT_DIR/$base_name.hk.out"
    err_file="$SCRIPT_DIR/$base_name.hk.err"

    # Paths for transpiled .out and .err files
    tr_out_file="$SCRIPT_DIR/$base_name.hk.tr.out"
    tr_err_file="$SCRIPT_DIR/$base_name.hk.tr.err"

    # Check what's missing
    missing_files=()
    file_has_missing=false

    # Check normal version files
    normal_missing=false
    if [ ! -f "$out_file" ]; then
        missing_files+=("$(basename "$out_file")")
        normal_missing=true
    fi
    if [ ! -f "$err_file" ]; then
        missing_files+=("$(basename "$err_file")")
        normal_missing=true
    fi

    # Check transpiled version files
    transpiled_missing=false
    if [ ! -f "$tr_out_file" ]; then
        missing_files+=("$(basename "$tr_out_file")")
        transpiled_missing=true
    fi
    if [ ! -f "$tr_err_file" ]; then
        missing_files+=("$(basename "$tr_err_file")")
        transpiled_missing=true
    fi

    # Report missing files
    if [ ${#missing_files[@]} -gt 0 ]; then
        file_has_missing=true
        missing_any=$((missing_any + 1))

        if [ "$normal_missing" = true ]; then
            missing_normal=$((missing_normal + 1))
        fi

        if [ "$transpiled_missing" = true ]; then
            missing_transpiled=$((missing_transpiled + 1))
        fi

        if [ "$BARE_OUTPUT" = true ]; then
            echo "$base_name.hk"
        else
            echo -e "${RED}$base_name.hk${NC} is missing:"
            for missing in "${missing_files[@]}"; do
                echo -e "  ${YELLOW}- $missing${NC}"
            done
            echo
        fi
    fi
done

# Summary
if [ "$BARE_OUTPUT" = false ]; then
    echo "=================================="
    echo "SUMMARY:"
    echo "=================================="
    echo -e "Total .hk files found: ${GREEN}$total_files${NC}"
    echo -e "Files missing output files: ${RED}$missing_any${NC}"
    echo -e "Files missing normal outputs (.out/.err): ${RED}$missing_normal${NC}"
    echo -e "Files missing transpiled outputs (.tr.out/.tr.err): ${RED}$missing_transpiled${NC}"

    if [ $missing_any -eq 0 ]; then
        echo -e "\n${GREEN}✓ All test files have their expected output files!${NC}"
    else
        echo -e "\n${YELLOW}Run './update.sh --force' to generate missing output files.${NC}"
    fi
fi
