#!/usr/bin/env bash

# Script to check syntax of all .cpp, .h files in the project using clang++
# Logs paths of files with errors to ./check.*.err.log

CLANG=${CLANG:-clang++}

# assume script is in util/arch/, project root is three levels up
PROJECT_ROOT="$(dirname "$(dirname "$(dirname "$0")")")"

function check_clang() {
    command -v "$1" >/dev/null 2>&1 || { echo >&2 "Error: $1 is not installed. Aborting."; exit 1; }
}

function file_syntax_check() {
    local file=$1
    local log_file=$2
    if ! "$CLANG" -ferror-limit=1 -std=c++23 -fsyntax-only -I "$PROJECT_ROOT/sdk/include" -I "$PROJECT_ROOT/sdk/include/luabind" -I "$PROJECT_ROOT/sdk/include/lua" "$file" 2>/dev/null; then
        echo "$file" >> "$log_file"
    fi
}

function check_cpp_syntax() {
    local log_file=${1:-./check.cpp.err.log}
    > "$log_file"  # Clear the log file

    # Number of threads to use (can be overridden with JOBS env)
    local jobs=${JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || nproc 2>/dev/null || echo 1)}
    local total_cpus
    total_cpus=$(getconf _NPROCESSORS_ONLN 2>/dev/null || nproc 2>/dev/null || echo 1)

    echo "Using $jobs parallel job(s) (detected $total_cpus CPU thread(s))"
    if [ "$jobs" -ge "$total_cpus" ]; then
        echo "Putting all those CPU threads to work..."
        echo "Tip: if your system becomes unresponsive, rerun with JOBS less than $jobs (e.g. JOBS=$((jobs/2)))."
    fi

    # Temporary directory to collect error entries (avoids race conditions)
    local tmpdir
    tmpdir=$(mktemp -d) || { echo "Failed to create tmpdir"; exit 1; }

    export CLANG PROJECT_ROOT TMPDIR="$tmpdir"

    # Run checks in parallel. Each failing check writes a small file into TMPDIR.
    find "$PROJECT_ROOT" -name "*.cpp" -type f -print0 | \
      xargs -0 -P "$jobs" -I{} bash -c '
        file="$1"
        if ! "$CLANG" -ferror-limit=1 -std=c++23 -fsyntax-only -I "$PROJECT_ROOT/sdk/include" -I "$PROJECT_ROOT/sdk/include/luabind" -I "$PROJECT_ROOT/sdk/include/lua" "$file" >/dev/null 2>&1; then
          tmpf=$(mktemp "$TMPDIR/file.XXXXXX") || tmpf="$TMPDIR/file.$$.$RANDOM"
          printf "%s\n" "$file" > "$tmpf"
        fi
      ' _ {}

    # Aggregate results if any
    if comp=$(find "$tmpdir" -type f -name 'file.*' -print -quit); then
        cat "$tmpdir"/file.* > "$log_file"
    fi

    rm -rf "$tmpdir"
}

function check_headers_syntax() {
    local log_file=${1:-./check.h.err.log}
    > "$log_file"  # Clear the log file

    # Number of threads to use (can be overridden with JOBS env)
    local jobs=${JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || nproc 2>/dev/null || echo 1)}
    local total_cpus
    total_cpus=$(getconf _NPROCESSORS_ONLN 2>/dev/null || nproc 2>/dev/null || echo 1)

    echo "Using $jobs parallel job(s) (detected $total_cpus CPU thread(s))"
    if [ "$jobs" -ge "$total_cpus" ]; then
        echo "Putting all those CPU threads to work..."
        echo "Tip: if your system becomes unresponsive, rerun with JOBS less than $jobs (e.g. JOBS=$((jobs/2)))."
    fi

    # Temporary directory to collect error entries
    local tmpdir
    tmpdir=$(mktemp -d) || { echo "Failed to create tmpdir"; exit 1; }

    export CLANG PROJECT_ROOT TMPDIR="$tmpdir"

    find "$PROJECT_ROOT" -name "*.h" -type f -print0 | \
      xargs -0 -P "$jobs" -I{} bash -c '
        file="$1"
        if ! "$CLANG" -ferror-limit=1 -std=c++23 -fsyntax-only -I "$PROJECT_ROOT/sdk/include" -I "$PROJECT_ROOT/sdk/include/luabind" -I "$PROJECT_ROOT/sdk/include/lua" "$file" >/dev/null 2>&1; then
          tmpf=$(mktemp "$TMPDIR/file.XXXXXX") || tmpf="$TMPDIR/file.$$.$RANDOM"
          printf "%s\n" "$file" > "$tmpf"
        fi
      ' _ {}

    # Aggregate results if any
    if comp=$(find "$tmpdir" -type f -name 'file.*' -print -quit); then
        cat "$tmpdir"/file.* > "$log_file"
    fi

    rm -rf "$tmpdir"
}

# Check if clang++ is available
check_clang "$CLANG"

# Find all .cpp files recursively from the project root (assuming script is in util/arch/)
echo "Starting syntax check for c++ files..."

LOG_FILE="./check.cpp.err.log"
> "$LOG_FILE"  # Clear the log file
check_cpp_syntax "$LOG_FILE"

echo "Syntax check complete for c++. Errors logged in $LOG_FILE"

# Idem for header files
echo "Starting syntax check for header files..."

LOG_FILE="./check.h.err.log"
> "$LOG_FILE"  # Clear the log file
check_headers_syntax "$LOG_FILE"

echo "Syntax check complete for h. Errors logged in $LOG_FILE"

echo "All done."