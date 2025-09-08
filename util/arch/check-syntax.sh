#!/usr/bin/env bash

# Script to check syntax of all .cpp, .h files in the project using clang++
# Logs paths of files with errors to ./check.*.err.log

CLANG=${CLANG:-clang++}
PROJECT_ROOT="$(dirname "$(dirname "$(dirname "$0")")")"

# Parse simple CLI flags
KEEP_LOGS=0
while [ $# -gt 0 ]; do
    case "$1" in
        -k|--keep-logs) KEEP_LOGS=1; shift ;;
        *) break ;;
    esac
done

function check_clang() {
    command -v "$1" >/dev/null 2>&1 || { echo >&2 "Error: $1 is not installed. Aborting."; exit 1; }
}

function check_cpp_syntax() {
    local log_file=${1:-./check.cpp.err.log}
    if [ "$KEEP_LOGS" -eq 1 ] && [ -s "$log_file" ]; then
        echo "Skipping C++ syntax check because '$log_file' exists and --keep-logs was specified."
        return 0
    fi
    > "$log_file"

    local jobs=${JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || nproc 2>/dev/null || echo 1)}
    local total_cpus
    total_cpus=$(getconf _NPROCESSORS_ONLN 2>/dev/null || nproc 2>/dev/null || echo 1)
    echo "Using $jobs parallel job(s) (detected $total_cpus CPU thread(s))"
    if [ "$jobs" -ge "$total_cpus" ]; then
        echo "Putting all those CPU threads to work..."
        echo "Tip: if your system becomes unresponsive, rerun with JOBS less than $jobs (e.g. JOBS=$((jobs/2)))."
    fi

    local tmpdir
    tmpdir=$(mktemp -d) || { echo "Failed to create tmpdir"; exit 1; }
    export CLANG PROJECT_ROOT TMPDIR="$tmpdir"

    # Each worker appends to its own file (errors_<BASHPID>.log) to avoid collisions and mktemp portability issues.
    find "$PROJECT_ROOT" -name "*.cpp" -type f -print0 2>/dev/null | \
      xargs -0 -P "$jobs" -I{} bash -c '
        file="$1"
        if ! "$CLANG" -ferror-limit=1 -std=c++23 -fsyntax-only \
             -I "$PROJECT_ROOT/sdk/include" -I "$PROJECT_ROOT/sdk/include/luabind" -I "$PROJECT_ROOT/sdk/include/lua" \
             "$file" >/dev/null 2>&1; then
          printf "%s\n" "$file" >> "$TMPDIR/errors_$BASHPID.log"
        fi
      ' _ {}

    # Aggregate results
    if comp=$(find "$tmpdir" -type f -name 'errors_*.log' -print -quit 2>/dev/null); then
        cat "$tmpdir"/errors_*.log > "$log_file"
    fi
    rm -rf "$tmpdir"
}

function check_headers_syntax() {
    local log_file=${1:-./check.h.err.log}
    if [ "$KEEP_LOGS" -eq 1 ] && [ -s "$log_file" ]; then
        echo "Skipping header syntax check because '$log_file' exists and --keep-logs was specified."
        return 0
    fi
    > "$log_file"

    local jobs=${JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || nproc 2>/dev/null || echo 1)}
    local total_cpus
    total_cpus=$(getconf _NPROCESSORS_ONLN 2>/dev/null || nproc 2>/dev/null || echo 1)
    echo "Using $jobs parallel job(s) (detected $total_cpus CPU thread(s))"
    if [ "$jobs" -ge "$total_cpus" ]; then
        echo "Putting all those CPU threads to work..."
        echo "Tip: if your system becomes unresponsive, rerun with JOBS less than $jobs (e.g. JOBS=$((jobs/2)))."
    fi

    local tmpdir
    tmpdir=$(mktemp -d) || { echo "Failed to create tmpdir"; exit 1; }
    export CLANG PROJECT_ROOT TMPDIR="$tmpdir"

    find "$PROJECT_ROOT" -name "*.h" -type f -print0 2>/dev/null | \
      xargs -0 -P "$jobs" -I{} bash -c '
        file="$1"
        if ! "$CLANG" -ferror-limit=1 -std=c++23 -fsyntax-only \
             -I "$PROJECT_ROOT/sdk/include" -I "$PROJECT_ROOT/sdk/include/luabind" -I "$PROJECT_ROOT/sdk/include/lua" \
             "$file" >/dev/null 2>&1; then
          printf "%s\n" "$file" >> "$TMPDIR/errors_$BASHPID.log"
        fi
      ' _ {}

    if comp=$(find "$tmpdir" -type f -name 'errors_*.log' -print -quit 2>/dev/null); then
        cat "$tmpdir"/errors_*.log > "$log_file"
    fi
    rm -rf "$tmpdir"
}

check_clang "$CLANG"

echo "Starting syntax check for C++ files..."
check_cpp_syntax "./check.cpp.err.log"
echo "Syntax check complete for C++. Errors logged in ./check.cpp.err.log"

echo "Starting syntax check for header files..."
check_headers_syntax "./check.h.err.log"
echo "Syntax check complete for headers. Errors logged in ./check.h.err.log"

echo "All done."