# Build Errors & Resolutions – Quick Reference

## 1. C/C++ Compiler Not Detected (CMake, Linux)

**Symptoms:**

- `CMAKE_C_COMPILER` / `CMAKE_CXX_COMPILER` not found
- "C compiler identification is unknown" (CMake error)

**Fix (CI / GitHub Actions):** Add after the Linux dependency install step in `.github/workflows/build-engine.yml`:

```yaml
- name: Install build-essential (Linux)
  if: runner.os == 'Linux'
  run: |
    sudo apt-get update
    sudo apt-get install -y build-essential
```

Ensures `gcc` and `g++` are available. Verify no step overrides `CC` / `CXX`.

## 2. C++ Syntax Errors / Missing D3D Types & FVF Constants

**Symptoms:**

- Errors like: unexpected tokens, missing type specifier, `D3DFVF_XYZ` undeclared
- Files: `src/Layers/xrRender/R_DStreams.h`, `src/Layers/xrRender/FVF.h`
- Triggered when NOT building Direct3D (e.g. Vulkan-only build)

**Resolution Checklist:**

- Guard D3D-only code:

```cpp
#if XR_HAVE_D3D
  // D3D declarations
#endif
```

- For Vulkan-only builds add compile definition:

```cmake
target_compile_definitions(<target> PRIVATE XR_FORCE_NO_D3D)
```

- Always include fallback constants via `#include "xrRender/FVF.h"` where FVF macros used.
- Provide stub / fallback structs & enums under `#else` when D3D disabled.

## 3. Vulkan SDK Not Found (CMake)

**Symptoms:**

- CMake: "Vulkan SDK not found. Provide VULKAN_SDK env path..."

**Linux (system packages):**

```sh
sudo apt-get update
sudo apt-get install -y libvulkan-dev vulkan-tools
```

Set environment for CMake if it expects `VULKAN_SDK`:

```sh
echo "VULKAN_SDK=/usr" >> $GITHUB_ENV
```

**GitHub Actions snippet:**

```yaml
- name: Install Vulkan dependencies
  run: |
    sudo apt-get update
    sudo apt-get install -y libvulkan-dev vulkan-tools

- name: Set VULKAN_SDK for system packages
  run: echo "VULKAN_SDK=/usr" >> $GITHUB_ENV
```

If using LunarG official SDK, set path to its root (e.g. `/opt/vulkan-sdk/<ver>`).

## 4. Summary Checklist

- Linux builds: install `build-essential` (gcc/g++).
- Vulkan builds: install `libvulkan-dev` + tools, export `VULKAN_SDK`.
- Non-D3D builds: set `XR_FORCE_NO_D3D`, guard D3D code, ensure FVF constants available.

---

Keep this file updated when new recurring errors arise.
