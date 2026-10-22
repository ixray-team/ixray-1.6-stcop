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

# macOS runs the game as a GUI app only through a bundle: launching the binary from a
# shell leaves it inactive, the window never becomes key and the first click is eaten by
# activation. Build the bundle next to the binaries, with the icon from src/Assets.
make_app_bundle() {
    local app="${BUILD_DIR}/IX-Ray.app"
    local icon_src="${REPO_DIR}/src/Assets/logo_macOS_1024x1024.png"
    local contents="${app}/Contents"

    rm -rf "${app}"
    mkdir -p "${contents}/MacOS" "${contents}/Resources"

    cat > "${contents}/MacOS/launch" <<'LAUNCH'
#!/bin/sh
# Started by LaunchServices from the bundle. Run the engine from the directory that
# holds the game data: ../run if present (the usual layout), otherwise the binaries.
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "${HERE}/../../.." && pwd)"
for dir in "${ROOT}/run" "${ROOT}/../run" "${ROOT}/bin/Release"; do
    if [ -x "${dir}/xrEngine" ]; then
        cd "${dir}" || exit 1
        exec ./xrEngine "$@"
    fi
done
echo "xrEngine not found next to ${ROOT}" >&2
exit 1
LAUNCH
    chmod +x "${contents}/MacOS/launch"

    cat > "${contents}/Info.plist" <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>CFBundleExecutable</key><string>launch</string>
	<key>CFBundleIdentifier</key><string>org.ixray.engine</string>
	<key>CFBundleName</key><string>IX-Ray</string>
	<key>CFBundlePackageType</key><string>APPL</string>
	<key>CFBundleIconFile</key><string>IXRay</string>
	<key>LSMinimumSystemVersion</key><string>12.0</string>
	<key>NSHighResolutionCapable</key><true/>
</dict>
</plist>
PLIST

    if [ -f "${icon_src}" ]; then
        local iconset="${BUILD_DIR}/IXRay.iconset"
        rm -rf "${iconset}"
        mkdir -p "${iconset}"
        sips -z 16 16     "${icon_src}" --out "${iconset}/icon_16x16.png"      >/dev/null
        sips -z 32 32     "${icon_src}" --out "${iconset}/icon_16x16@2x.png"   >/dev/null
        sips -z 32 32     "${icon_src}" --out "${iconset}/icon_32x32.png"      >/dev/null
        sips -z 64 64     "${icon_src}" --out "${iconset}/icon_32x32@2x.png"   >/dev/null
        sips -z 128 128   "${icon_src}" --out "${iconset}/icon_128x128.png"    >/dev/null
        sips -z 256 256   "${icon_src}" --out "${iconset}/icon_128x128@2x.png" >/dev/null
        sips -z 256 256   "${icon_src}" --out "${iconset}/icon_256x256.png"    >/dev/null
        sips -z 512 512   "${icon_src}" --out "${iconset}/icon_256x256@2x.png" >/dev/null
        sips -z 512 512   "${icon_src}" --out "${iconset}/icon_512x512.png"    >/dev/null
        sips -z 1024 1024 "${icon_src}" --out "${iconset}/icon_512x512@2x.png" >/dev/null
        iconutil -c icns "${iconset}" -o "${contents}/Resources/IXRay.icns"
        rm -rf "${iconset}"
    fi

    codesign --force --sign - "${app}" >/dev/null 2>&1 || true
}

make_app_bundle

echo
echo "Done. Binaries are in ${BUILD_DIR}/bin/Release"
echo "App bundle: ${BUILD_DIR}/IX-Ray.app"
echo "To play, put the game data next to the bundle (or into ${BUILD_DIR}/run)"
echo "and start it with: open \"${BUILD_DIR}/IX-Ray.app\""
