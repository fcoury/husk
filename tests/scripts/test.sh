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

# Flag to stop on first error
STOP_ON_ERROR=0

# Specific test file to run (optional)
SPECIFIC_TEST=""

# Check for command line options
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -t|--transpile) TRANSPILE_MODE=1 ;;
        -s|--stop|--stop-on-error) STOP_ON_ERROR=1 ;;
        *.hk) SPECIFIC_TEST="$1" ;;
        *)
            # Check if it's a test name without .hk extension
            if [[ "$1" != --* ]] && [[ "$1" != -* ]] && [ -f "$SCRIPT_DIR/$1.hk" ]; then
                SPECIFIC_TEST="$1.hk"
            else
                echo "Unknown parameter passed: $1"
                echo "Usage: $0 [-t|--transpile] [-s|--stop|--stop-on-error] [test_name.hk | test_name]"
                echo "  -t, --transpile: Run tests in transpile mode"
                echo "  -s, --stop, --stop-on-error: Stop on first test failure"
                echo "  test_name: Specific test to run (with or without .hk extension)"
                exit 1
            fi
            ;;
    esac
    shift
done

# Function to cleanup and exit
cleanup_and_exit() {
    rm -f "$TEMP_STDOUT" "$TEMP_STDERR"
    exit $1
}

# Function to handle test failure
handle_test_failure() {
    ANY_TEST_FAILED=1
    if [ $STOP_ON_ERROR -eq 1 ]; then
        echo ""
        echo "Stopping on first error (--stop-on-error flag used)."
        cleanup_and_exit 1
    fi
}

# Function to process a single test file
process_test_file() {
    local file=$1
    local base_name=$(basename "$file")

    echo "$base_name"

    # Run the script and capture the output
    if [ $TRANSPILE_MODE -eq 1 ]; then
        # ./target/debug/husk compile "$file" | node > "$TEMP_STDOUT" 2> "$TEMP_STDERR"
        SCRIPT_PATH="$(realpath "$0")"
        SCRIPT_DIR_PATH="$(dirname "$SCRIPT_PATH")"
        $SCRIPT_DIR_PATH/transpile.sh "$file" > "$TEMP_STDOUT" 2> "$TEMP_STDERR"
    else
        ./target/debug/husk "$file" > "$TEMP_STDOUT" 2> "$TEMP_STDERR"
    fi

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
            handle_test_failure
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
            handle_test_failure
        fi
    else
        echo "  skipped stderr"
    fi

    echo ""
}

# If a specific test is provided, run only that test
if [ -n "$SPECIFIC_TEST" ]; then
    # Handle both with and without .hk extension
    if [[ "$SPECIFIC_TEST" == *.hk ]]; then
        test_file="$SCRIPT_DIR/$SPECIFIC_TEST"
    else
        test_file="$SCRIPT_DIR/$SPECIFIC_TEST.hk"
    fi

    if [ ! -f "$test_file" ]; then
        echo "Error: Test file '$test_file' not found."
        cleanup_and_exit 1
    fi

    echo "Running specific test: $(basename "$test_file")"
    echo
    process_test_file "$test_file"

    # Cleanup and exit
    rm -f "$TEMP_STDOUT" "$TEMP_STDERR"

    if [ $ANY_TEST_FAILED -ne 0 ]; then
        echo "Test failed."
        exit 1
    else
        echo "Test passed."
        exit 0
    fi
fi

# Original behavior: Loop through all .hk files in the specified directory
echo "Running all tests in $SCRIPT_DIR..."
echo

for file in "$SCRIPT_DIR"/*.hk;
do
  # Check if there are any .hk files
  if [ ! -e "$file" ]; then
    echo "No .hk files found in $SCRIPT_DIR."
    cleanup_and_exit 1
  fi

  process_test_file "$file"
done

# Cleanup temporary files
rm -f "$TEMP_STDOUT" "$TEMP_STDERR"

# Exit with error code if any test failed
if [ $ANY_TEST_FAILED -ne 0 ]; then
  echo "Some tests failed."
  exit 1
else
  echo "All tests passed."
  exit 0
fi
