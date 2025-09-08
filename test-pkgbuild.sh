#!/bin/bash
# Test script for IX-Ray PKGBUILD validation

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "=========================================="
echo "IX-Ray PKGBUILD Validation Test"
echo "=========================================="

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Test 1: Check PKGBUILD syntax
test_pkgbuild_syntax() {
    log_info "Testing PKGBUILD syntax..."
    
    if [ ! -f "PKGBUILD" ]; then
        log_error "PKGBUILD not found"
        return 1
    fi
    
    # Basic syntax check
    if bash -n PKGBUILD; then
        log_success "PKGBUILD syntax is valid"
    else
        log_error "PKGBUILD has syntax errors"
        return 1
    fi
}

# Test 2: Check required PKGBUILD variables
test_pkgbuild_variables() {
    log_info "Testing PKGBUILD variables..."
    
    source PKGBUILD 2>/dev/null || {
        log_error "Failed to source PKGBUILD"
        return 1
    }
    
    local required_vars=(
        "pkgname" "pkgver" "pkgrel" "pkgdesc" 
        "arch" "url" "license" "depends" "source"
    )
    
    for var in "${required_vars[@]}"; do
        if [ -z "${!var:-}" ]; then
            log_error "Required variable '$var' is missing"
            return 1
        fi
    done
    
    log_success "All required PKGBUILD variables are present"
}

# Test 3: Check makepkg availability
test_makepkg() {
    log_info "Testing makepkg availability..."
    
    if command -v makepkg >/dev/null 2>&1; then
        log_success "makepkg is available"
        makepkg --version | head -n1
    else
        log_warning "makepkg not found - install 'base-devel' package"
    fi
}

# Test 4: Check dependencies availability
test_dependencies() {
    log_info "Testing dependency availability..."
    
    source PKGBUILD 2>/dev/null
    
    local available=0
    local total=0
    
    for dep in "${depends[@]}" "${makedepends[@]}"; do
        # Remove version constraints
        dep=$(echo "$dep" | sed 's/[<>=].*//')
        total=$((total + 1))
        
        if pacman -Qi "$dep" >/dev/null 2>&1; then
            available=$((available + 1))
        else
            log_warning "Dependency '$dep' not installed"
        fi
    done
    
    log_info "Dependencies: $available/$total available"
    
    if [ "$available" -eq "$total" ]; then
        log_success "All dependencies are available"
    else
        log_warning "Some dependencies are missing"
    fi
}

# Test 5: Validate CMake presets
test_cmake_presets() {
    log_info "Testing CMake presets..."
    
    if [ ! -f "CMakePresets.json" ]; then
        log_error "CMakePresets.json not found"
        return 1
    fi
    
    # Check if our target preset exists
    if grep -q "Engine-x64-Linux" CMakePresets.json; then
        log_success "Engine-x64-Linux preset found"
    else
        log_error "Engine-x64-Linux preset not found in CMakePresets.json"
        return 1
    fi
}

# Test 6: Check source availability
test_source_availability() {
    log_info "Testing source availability..."
    
    # Test if git repository is accessible
    if git ls-remote --heads https://github.com/wasertech/ixray-1.6-stcop.git dev/vk >/dev/null 2>&1; then
        log_success "Source repository is accessible"
    else
        log_warning "Source repository may not be accessible"
    fi
}

# Test 7: Dry-run makepkg
test_makepkg_dryrun() {
    log_info "Testing makepkg dry-run..."
    
    if command -v makepkg >/dev/null 2>&1; then
        # Test PKGBUILD validation without actually building
        if makepkg --nobuild --nodeps --skipinteg 2>/dev/null; then
            log_success "makepkg dry-run successful"
            
            # Clean up
            rm -rf src/ pkg/ *.pkg.tar.* 2>/dev/null || true
        else
            log_warning "makepkg dry-run failed"
        fi
    else
        log_info "Skipping makepkg dry-run (makepkg not available)"
    fi
}

# Test 8: Check build script
test_build_script() {
    log_info "Testing build script..."
    
    if [ -f "build-arch.sh" ] && [ -x "build-arch.sh" ]; then
        log_success "build-arch.sh is executable"
        
        # Test script syntax
        if bash -n build-arch.sh; then
            log_success "build-arch.sh syntax is valid"
        else
            log_error "build-arch.sh has syntax errors"
            return 1
        fi
    else
        log_error "build-arch.sh not found or not executable"
        return 1
    fi
}

# Run all tests
run_all_tests() {
    local failed=0
    
    test_pkgbuild_syntax || failed=$((failed + 1))
    test_pkgbuild_variables || failed=$((failed + 1))
    test_makepkg || true  # Don't fail on this
    test_dependencies || true  # Don't fail on this
    test_cmake_presets || failed=$((failed + 1))
    test_source_availability || true  # Don't fail on this
    test_makepkg_dryrun || true  # Don't fail on this
    test_build_script || failed=$((failed + 1))
    
    echo ""
    echo "=========================================="
    if [ "$failed" -eq 0 ]; then
        log_success "All critical tests passed!"
        echo ""
        echo "You can now:"
        echo "  1. Build with makepkg: makepkg -si"
        echo "  2. Build manually: ./build-arch.sh"
        echo "  3. Submit to AUR (if ready)"
    else
        log_error "$failed critical test(s) failed"
        echo ""
        echo "Please fix the issues before proceeding."
        return 1
    fi
    echo "=========================================="
}

# Main execution
main() {
    if [ "${1:-}" = "--help" ] || [ "${1:-}" = "-h" ]; then
        echo "Usage: $0 [--help|-h]"
        echo ""
        echo "This script validates the IX-Ray PKGBUILD and build environment."
        echo "It checks syntax, dependencies, and performs basic validation."
        exit 0
    fi
    
    run_all_tests
}

main "$@"
