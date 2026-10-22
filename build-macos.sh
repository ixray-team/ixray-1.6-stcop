#!/bin/bash
# Builds the engine on macOS from a clean checkout.
#
#   ./build-macos.sh [build-directory]
#
# The script checks the tools the build needs, installs the missing ones with
# Homebrew, then configures and builds. Everything else (DXMT, SDL3, vkd3d, TBB,
# lzo2, RedImage, yaml-cpp, Steam SDK, freetype, ogg, theora) is downloaded by
# CMake itself.
set -euo pipefail

REPO_DIR="$(cd "$(dirname "$0")" && pwd)"
BUILD_DIR="${1:-${REPO_DIR}/build}"

if [ "$(uname -s)" != "Darwin" ]; then
    echo "ERROR: this script builds the macOS port only" >&2
    exit 1
fi

# tool command -> brew formula
declare -a MISSING=()
require() {
    local command_name="$1"
    local formula="${2:-$1}"
    if command -v "${command_name}" >/dev/null 2>&1; then
        return
    fi
    if [ "${command_name}" = "llvm-config" ]; then
        if [ -x "/opt/homebrew/opt/llvm@15/bin/llvm-config" ]; then
            return
        fi
    fi
    MISSING+=("${formula}")
}

require cmake
require ninja
require meson
require ispc
require wget
require llvm-config llvm@15

if [ ${#MISSING[@]} -gt 0 ]; then
    if ! command -v brew >/dev/null 2>&1; then
        echo "ERROR: missing tools: ${MISSING[*]}" >&2
        echo "Install Homebrew (https://brew.sh) and run the script again." >&2
        exit 1
    fi
    echo "Installing: ${MISSING[*]}"
    brew install "${MISSING[@]}"
fi

echo "Repository: ${REPO_DIR}"
echo "Build dir:  ${BUILD_DIR}"

cmake -S "${REPO_DIR}" -B "${BUILD_DIR}" -G Ninja -DCMAKE_BUILD_TYPE=Release

# The build is incremental: a second run only rebuilds what changed.
ninja -C "${BUILD_DIR}"

echo
echo "Done. Binaries are in ${BUILD_DIR}/bin/Release"
echo "To play, put that directory (or its libraries) next to the game data"
echo "with fsgame.ltx from this repository and start xrEngine."
