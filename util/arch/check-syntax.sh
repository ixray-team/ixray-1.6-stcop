#!/usr/bin/env bash

# Script to check syntax of all .cpp files in the project using clang++
# Logs paths of files with errors to ./check.cpp.err.log

LOG_FILE="./check.cpp.err.log"
> "$LOG_FILE"  # Clear the log file

# Find all .cpp files recursively from the project root (assuming script is in util/arch/)
PROJECT_ROOT="$(dirname "$(dirname "$(dirname "$0")")")"

find "$PROJECT_ROOT" -name "*.cpp" -type f | while read -r file; do
    echo "Checking $file"
    if ! /usr/bin/clang++ -ferror-limit=1 -std=c++23 -fsyntax-only -I "$PROJECT_ROOT/sdk/include" -I "$PROJECT_ROOT/sdk/include/luabind" -I "$PROJECT_ROOT/sdk/include/lua" "$file" 2>/dev/null; then
        echo "$file" >> "$LOG_FILE"
    fi
done

echo "Syntax check complete. Errors logged in $LOG_FILE"