#!/bin/bash
set -euo pipefail

# IX-Ray 1.6 STCOP Local Build Script for Arch Linux
# This script follows the same steps as the CI for ubuntu-latest

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Configuration
PRESET="Engine"
PLATFORM="x64"
CONFIG="${1:-RelWithDebInfo}"  # Allow config override via command line
VULKAN_SDK_VERSION="vulkan-sdk-1.4.309.0"

echo "=========================================="
echo "IX-Ray 1.6 STCOP Build Script for Arch Linux"
echo "Configuration: $CONFIG"
echo "=========================================="

# Color output functions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if running on Arch Linux
if ! command -v pacman >/dev/null 2>&1; then
    log_warning "This script is designed for Arch Linux. You may need to adapt package names for your distribution."
fi

# Function to check and install dependencies
check_dependencies() {
    log_info "Checking system dependencies..."
    
    local missing_packages=()
    local arch_packages=(
        # Build tools
        "cmake" "ninja" "pkgconf" "git" "mono" "wget"
        # Core libraries  
        "vulkan-icd-loader" "vulkan-headers" "vulkan-validation-layers"
        "mesa" "libx11" "libxext" "libxrandr" "libxinerama" "libxcursor" "libxi"
        # Audio/codec libraries
        "openal" "libogg" "libvorbis" "libtheora"
        # System libraries
        "zlib" "freetype2" "lzo" "intel-tbb" "luajit"
        # Development tools
        "gcc" "clang"
    )
    
    for package in "${arch_packages[@]}"; do
        if ! pacman -Qi "$package" >/dev/null 2>&1; then
            missing_packages+=("$package")
        fi
    done
    
    if [ ${#missing_packages[@]} -gt 0 ]; then
        log_warning "Missing packages: ${missing_packages[*]}"
        read -p "Install missing packages? [y/N] " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            sudo pacman -S --needed "${missing_packages[@]}"
        else
            log_error "Cannot proceed without required dependencies"
            exit 1
        fi
    else
        log_success "All dependencies are installed"
    fi
    
    # Optional CUDA check
    if ! pacman -Qi "cuda" >/dev/null 2>&1; then
        log_info "CUDA toolkit not installed (optional for GPU acceleration)"
    fi
}

# Setup environment
setup_environment() {
    log_info "Setting up build environment..."
    
    # Vulkan environment
    export VULKAN_SDK=/usr
    export VK_SDK_PATH=/usr
    
    # Compiler selection (prefer clang)
    if command -v clang >/dev/null 2>&1; then
        export IXRAY_C_COMPILER=clang
        export IXRAY_CXX_COMPILER=clang++
        log_info "Using Clang compiler: $(clang --version | head -n1)"
    else
        export IXRAY_C_COMPILER=gcc
        export IXRAY_CXX_COMPILER=g++
        log_info "Using GCC compiler: $(gcc --version | head -n1)"
    fi
    
    # IX-Ray build flags
    export IXRAY_USE_VK=ON
    export IXRAY_VULKAN_TAG_VERSION="$VULKAN_SDK_VERSION"
    export IXRAY_ENABLE_TBB_FETCH=OFF  # Use system TBB
    export IXRAY_FORCE_FETCH_NVTT=ON
    
    log_success "Environment configured"
}

# Setup NuGet
setup_nuget() {
    log_info "Setting up NuGet package manager..."
    
    # Create directories
    mkdir -p build/x64/Engine-Linux/packages
    mkdir -p ~/.nuget/packages
    export NUGET_PACKAGES="$HOME/.nuget/packages"
    
    # Install NuGet if not available
    if ! command -v nuget >/dev/null 2>&1; then
        local nuget_version="5.11.0"
        local nuget_exe="nuget.exe"
        
        if [ ! -f "$nuget_exe" ]; then
            log_info "Downloading NuGet $nuget_version..."
            wget -q "https://dist.nuget.org/win-x86-commandline/v${nuget_version}/nuget.exe" -O "$nuget_exe"
        fi
        
        # Create wrapper script
        cat > nuget <<EOF
#!/usr/bin/env bash
set -euo pipefail
mono "$PWD/$nuget_exe" "\$@"
EOF
        chmod +x nuget
        export PATH="$PWD:$PATH"
        
        log_success "NuGet installed"
    else
        log_info "NuGet already available"
    fi
}

# Restore NuGet packages
restore_packages() {
    log_info "Restoring NuGet packages..."
    
    if ! nuget restore cmake/linux/Packages.config -SolutionDirectory build/x64/Engine-Linux -Verbosity minimal; then
        log_warning "Primary restore failed, trying fallback..."
        if ! nuget restore cmake/linux/Packages.config -SolutionDirectory build -Verbosity detailed; then
            log_error "Package restore failed"
            exit 1
        fi
    fi
    
    # Verify and fix LuaJIT
    local luajit_path="build/x64/Engine-Linux/packages/IXRay.Packages.LuaJIT.Runtimes.linux-x64.2023.8.23.1-open/runtimes/linux-x64/native/libluajit.so"
    
    if [ ! -f "$luajit_path" ]; then
        log_warning "LuaJIT library missing, attempting direct install..."
        nuget install IXRay.Packages.LuaJIT.Runtimes.linux-x64 -Version 2023.8.23.1-open -OutputDirectory build/x64/Engine-Linux/packages
        
        # Create symlink if needed
        local luajit_dir="$(dirname "$luajit_path")"
        if [ -d "$luajit_dir" ]; then
            cd "$luajit_dir"
            for lib in libluajit*.so; do
                if [ "$lib" != "libluajit.so" ] && [ -f "$lib" ]; then
                    ln -sf "$lib" libluajit.so
                    log_info "Created LuaJIT symlink: $lib -> libluajit.so"
                    break
                fi
            done
            cd - >/dev/null
        fi
    fi
    
    if [ -f "$luajit_path" ]; then
        log_success "LuaJIT package verified"
    else
        log_error "LuaJIT package still missing after install"
        exit 1
    fi
    
    log_success "Packages restored"
}

# Configure build
configure_build() {
    log_info "Configuring build with CMake..."
    
    cmake \
        --preset "${PRESET}-${PLATFORM}-Linux" \
        -DIXRAY_USE_VK="$IXRAY_USE_VK" \
        -DIXRAY_USE_R1=OFF \
        -DIXRAY_USE_R2=OFF \
        -DIXRAY_FORCE_NO_D3D=ON \
        -DIXRAY_VULKAN_TAG_VERSION="$IXRAY_VULKAN_TAG_VERSION" \
        -DIXRAY_ENABLE_TBB_FETCH="$IXRAY_ENABLE_TBB_FETCH" \
        -DIXRAY_FORCE_FETCH_NVTT="$IXRAY_FORCE_FETCH_NVTT" \
        -DCMAKE_BUILD_TYPE="$CONFIG"
    
    log_success "Configuration complete"
}

# Verify Vulkan setup
verify_vulkan() {
    log_info "Verifying Vulkan setup..."
    
    echo "==== Vulkan headers ===="
    if ls /usr/include/vulkan/*.h >/dev/null 2>&1; then
        ls -1 /usr/include/vulkan/*.h | head -n5
        log_success "Vulkan headers found"
    else
        log_error "Vulkan headers missing in /usr/include/vulkan"
    fi
    
    echo "==== Vulkan libraries ===="
    if /sbin/ldconfig -p | grep -E 'libvulkan.so'; then
        log_success "Vulkan libraries found"
    else
        log_warning "Vulkan libraries not found in ldconfig"
    fi
    
    echo "==== CMake feature flags ===="
    if [ -f "build/${PLATFORM}/Engine-Linux/CMakeCache.txt" ]; then
        grep -E 'IXRAY_USE_R1|IXRAY_USE_R2|IXRAY_USE_VK|IXRAY_FORCE_NO_D3D' "build/${PLATFORM}/Engine-Linux/CMakeCache.txt" || true
    fi
}

# Build project
build_project() {
    log_info "Building IX-Ray engine..."
    
    local build_start=$(date +%s)
    
    cmake --build --preset "${PRESET}-${PLATFORM}-Linux-${CONFIG}"
    
    local build_end=$(date +%s)
    local build_time=$((build_end - build_start))
    
    log_success "Build completed in ${build_time} seconds"
}

# Show build results
show_results() {
    log_info "Build results:"
    
    local build_dir="build/${PLATFORM}/Engine-Linux/bin/${CONFIG}"
    
    if [ -d "$build_dir" ]; then
        echo "==== Binaries in $build_dir ===="
        ls -la "$build_dir" || true
        log_success "Build artifacts created successfully"
    else
        log_error "Build directory not found: $build_dir"
        return 1
    fi
    
    # Show library directory too
    local lib_dir="build/${PLATFORM}/Engine-Linux/lib/${CONFIG}"
    if [ -d "$lib_dir" ]; then
        echo "==== Libraries in $lib_dir ===="
        ls -la "$lib_dir" || true
    fi
}

# Cleanup function
cleanup() {
    log_info "Cleaning up temporary files..."
    # Remove nuget executable and wrapper if we created them
    if [ -f "nuget.exe" ] && [ -f "nuget" ]; then
        rm -f nuget.exe nuget
        log_info "Removed temporary NuGet files"
    fi
}

# Trap to ensure cleanup on exit
trap cleanup EXIT

# Main execution
main() {
    log_info "Starting IX-Ray 1.6 STCOP build process..."
    
    check_dependencies
    setup_environment
    setup_nuget
    restore_packages
    configure_build
    verify_vulkan
    build_project
    show_results
    
    log_success "Build process completed successfully!"
    echo ""
    echo "You can now run the built binaries from:"
    echo "  build/${PLATFORM}/Engine-Linux/bin/${CONFIG}/"
    echo ""
    echo "To build a different configuration, run:"
    echo "  $0 [Debug|RelWithDebInfo|Release]"
}

# Run main function
main "$@"
