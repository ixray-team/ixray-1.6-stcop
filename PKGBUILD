pkgname=ixray-1.6-stcop
pkgver=1.6.stcop
pkgrel=1
pkgdesc="IX-Ray 1.6 STCOP - S.T.A.L.K.E.R. engine with Vulkan support"
arch=('x86_64')
url="https://github.com/wasertech/ixray-1.6-stcop"
license=('custom')
depends=(
    'glibc'
    'gcc-libs'
    'vulkan-icd-loader'
    'vulkan-headers'
    'mesa'
    'libx11'
    'libxext'
    'libxrandr'
    'libxinerama'
    'libxcursor'
    'libxi'
    'openal'
    'libogg'
    'libvorbis'
    'libtheora'
    'zlib'
    'freetype2'
    'lzo'
    'intel-tbb'
    'luajit'
)
makedepends=(
    'cmake'
    'ninja'
    'pkgconf'
    'git'
    'vulkan-validation-layers'
    'mono'
    'wget'
)
optdepends=(
    'nvidia-cuda-toolkit: for CUDA acceleration support'
    'vulkan-tools: for Vulkan utilities and debugging'
)
source=(
    "ixray-1.6-stcop::git+https://github.com/wasertech/ixray-1.6-stcop.git#branch=dev/vk"
)
sha256sums=('SKIP')

# Build configuration
_preset="Engine"
_platform="x64"
_config="RelWithDebInfo"

prepare() {
    cd "$srcdir/$pkgname"
    git submodule update --init --recursive || true
        # Assure l'exécutabilité du script de build centralisé ou le crée si absent
        if [ -f util/arch/build.sh ]; then
                chmod +x util/arch/build.sh || true
        else
                echo "[prepare] util/arch/build.sh absent -> création d'une version embarquée" >&2
                install -d util/arch
                cat > util/arch/build.sh <<'EOF'
#!/bin/bash
set -euo pipefail

PRESET="Engine"
PLATFORM="x64"
CONFIG="${1:-RelWithDebInfo}"
VULKAN_SDK_VERSION="vulkan-sdk-1.4.309.0"

echo "== IX-Ray minimal build.sh (fallback) =="

# Environnement Vulkan
export VULKAN_SDK=/usr
export VK_SDK_PATH=/usr

# Choix compilateur
if command -v clang >/dev/null 2>&1; then
    export IXRAY_C_COMPILER=clang
    export IXRAY_CXX_COMPILER=clang++
else
    export IXRAY_C_COMPILER=gcc
    export IXRAY_CXX_COMPILER=g++
fi

export IXRAY_USE_VK=ON
export IXRAY_VULKAN_TAG_VERSION="$VULKAN_SDK_VERSION"
export IXRAY_ENABLE_TBB_FETCH=OFF
export IXRAY_FORCE_FETCH_NVTT=ON

# NuGet (cache local dans l'arborescence de build)
mkdir -p build/x64/Engine-Linux/packages
export NUGET_PACKAGES="$(pwd)/build/.nuget"
mkdir -p "$NUGET_PACKAGES"

if ! command -v nuget >/dev/null 2>&1; then
    NUGET_VERSION=5.11.0
    wget -q "https://dist.nuget.org/win-x86-commandline/v${NUGET_VERSION}/nuget.exe" -O nuget.exe
    cat > nuget <<'EOS'
#!/usr/bin/env bash
set -euo pipefail
mono "$(pwd)/nuget.exe" "$@"
EOS
    chmod +x nuget
    export PATH="$(pwd):$PATH"
fi

echo "Restoring NuGet packages..."
nuget restore cmake/linux/Packages.config -SolutionDirectory build/x64/Engine-Linux -Verbosity minimal || \
 nuget restore cmake/linux/Packages.config -SolutionDirectory build -Verbosity detailed || true

# LuaJIT vérification
LJ="build/x64/Engine-Linux/packages/IXRay.Packages.LuaJIT.Runtimes.linux-x64.2023.8.23.1-open/runtimes/linux-x64/native"
if [ ! -f "$LJ/libluajit.so" ]; then
    nuget install IXRay.Packages.LuaJIT.Runtimes.linux-x64 -Version 2023.8.23.1-open -OutputDirectory build/x64/Engine-Linux/packages || true
    if [ -d "$LJ" ]; then
        (cd "$LJ" && for f in libluajit*.so; do [ "$f" != "libluajit.so" ] && ln -sf "$f" libluajit.so && break; done)
    fi
fi

echo "Configuring..."
cmake --preset "${PRESET}-${PLATFORM}-Linux" \
    -DIXRAY_USE_VK=ON \
    -DIXRAY_USE_R1=OFF -DIXRAY_USE_R2=OFF -DIXRAY_FORCE_NO_D3D=ON \
    -DIXRAY_VULKAN_TAG_VERSION="$IXRAY_VULKAN_TAG_VERSION" \
    -DIXRAY_ENABLE_TBB_FETCH=OFF -DIXRAY_FORCE_FETCH_NVTT=ON \
    -DCMAKE_BUILD_TYPE="$CONFIG"

echo "Building..."
cmake --build --preset "${PRESET}-${PLATFORM}-Linux-${CONFIG}"

echo "== Build fallback terminé =="
EOF
                chmod +x util/arch/build.sh || true
        fi
}

build() {
    cd "$srcdir/$pkgname"
    if [ -x util/arch/build.sh ]; then
        util/arch/build.sh "$_config"
    else
        echo "util/arch/build.sh manquant ou non exécutable" >&2
        return 1
    fi
}

check() {
    cd "$srcdir/$pkgname"
    local build_dir="build/x64/Engine-Linux/bin/$_config"
    if [ ! -d "$build_dir" ]; then
        echo "Build directory not found: $build_dir" >&2
        return 1
    fi
    ls -la "$build_dir" || true
    /sbin/ldconfig -p | grep -E 'libvulkan.so' || echo "Vulkan library check failed"
}

package() {
    cd "$srcdir/$pkgname"
    local build_dir="build/x64/Engine-Linux"

    if [ -d "$build_dir/bin/$_config" ]; then
        install -dm755 "$pkgdir/usr/bin"
        install -Dm755 "$build_dir/bin/$_config/"* "$pkgdir/usr/bin/" 2>/dev/null || true
    fi
    if [ -d "$build_dir/lib/$_config" ]; then
        install -dm755 "$pkgdir/usr/lib/ixray"
        cp -r "$build_dir/lib/$_config/"* "$pkgdir/usr/lib/ixray/" 2>/dev/null || true
    fi
    if [ -d gamedata ]; then
        install -dm755 "$pkgdir/usr/share/ixray"
        cp -r gamedata "$pkgdir/usr/share/ixray/"
    fi
    for conf in fsgame.ltx fs.ltx fsfactory.ltx datapack.ltx .xrignore; do
        [ -f "$conf" ] && install -Dm644 "$conf" "$pkgdir/usr/share/ixray/$conf"
    done
    install -dm755 "$pkgdir/usr/share/doc/$pkgname"
    for doc in README.md doc/*.md; do
        [ -f "$doc" ] && install -Dm644 "$doc" "$pkgdir/usr/share/doc/$pkgname/"
    done
    [ -f LICENSE ] && install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}

# vim:set ts=4 sw=4 et:
