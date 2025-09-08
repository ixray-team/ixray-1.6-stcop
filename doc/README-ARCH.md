# IX-Ray 1.6 STCOP - Build Instructions for Arch Linux

This version of IX-Ray is based on the S.T.A.L.K.E.R. engine with Vulkan support, optimized for Arch Linux.

## Quick Install with PKGBUILD

### Prerequisites
```bash
# Install build dependencies
sudo pacman -S --needed base-devel git

# Clone the repository
git clone https://github.com/wasertech/ixray-1.6-stcop.git
cd ixray-1.6-stcop
```

### Build with makepkg
```bash
# Build and install
makepkg -si

# Or for build only
makepkg
```

## Manual build with the script

Alternatively, you can use the provided build script:

```bash
# Make the script executable (if necessary)
chmod +x build-arch.sh

# Build in RelWithDebInfo mode (default)
./build-arch.sh

# Build in Debug mode
./build-arch.sh Debug

# Release Mode Build
./build-arch.sh Release
```

## System Dependencies

### Required
- **Build Tools**: `cmake`, `ninja`, `pkgconf`, `git`, `gcc`, `clang`
- **Vulkan**: `vulkan-icd-loader`, `vulkan-headers`, `vulkan-validation-layers`
- **Graphics**: `mesa`, `libx11`, `libxext`, `libxrandr`, `libxinerama`, `libxcursor`, `libxi`
- **Audio**: `openal`, `libogg`, `libvorbis`, `libtheora`
- **System**: `zlib`, `freetype2`, `lzo`, `intel-tbb`, `luajit`
- **NuGet**: `mono`, `wget`

### Optional
- **CUDA**: `cuda` (for GPU acceleration)
- **Vulkan Tools**: `vulkan-tools` (for debugging)

### Automatic dependency installation
```bash
sudo pacman -S --needed cmake ninja pkgconf git vulkan-icd-loader vulkan-headers \
vulkan-validation-layers mesa libx11 libxext libxrandr libxinerama libxcursor \
libxi openal libogg libvorbis libtheora zlib freetype2 lzo intel-tbb luajit \
mono wget gcc clang
```

## Build Configuration

The build uses the following parameters by default:
- **Vulkan**: Enabled (`IXRAY_USE_VK=ON`)
- **Direct3D**: Disabled (`IXRAY_FORCE_NO_D3D=ON`)
- **R1/R2 Rendering**: Disabled (`IXRAY_USE_R1=OFF`, `IXRAY_USE_R2=OFF`)
- **Unity Build**: Enabled for faster builds
- **TBB**: Uses the system version of Arch Linux

## Build Structure

After the build, the files are organized as follows:
```
build/x64/Engine-Linux/
├── bin/[CONFIG]/ # Executables
├── lib/[CONFIG]/ # Libraries
└── packages/ # NuGet Packages
```

## Build Verification

The script verifies Automatically:
- Presence of Vulkan headers in `/usr/include/vulkan`
- Vulkan libraries in the system
- CMake configuration flags
- Build artifacts created

## Troubleshooting

### Vulkan Error
```bash
# Check Vulkan installation
vulkan-tools
ls /usr/include/vulkan/
```

### LuaJIT Error
The script automatically creates the necessary symlinks for LuaJIT from NuGet packages.

### NuGet Error
The script automatically downloads and configures NuGet with Mono.

### Compilation error
Check that all git submodules are initialized:
```bash
git submodule update --init --recursive
```

## Custom configuration

To modify the build configuration, edit the environment variables in the script:
- `IXRAY_USE_VK`: Vulkan support
- `IXRAY_ENABLE_TBB_FETCH`: Automatic TBB download
- `IXRAY_FORCE_FETCH_NVTT`: Force NVTT download

## Usage

After installation, the binaries are available in:
- **With PKGBUILD**: `/usr/bin/`
- **Manual build**: `build/x64/Engine-Linux/bin/[CONFIG]/`

The game data is located in:
- **With PKGBUILD**: `/usr/share/ixray/`
- **Manual Build**: `gamedata/`

## Support

For Arch Linux-specific issues, create an issue on the GitHub repository, mentioning:
- Arch Linux version
- Graphics driver version
- Full build log
- Hardware configuration (GPU, etc.)

## Contributions

Contributions to improve Arch Linux support are welcome, including:
- PKGBUILD optimizations
- Build script improvements
- Testing on different configurations
- Documentation