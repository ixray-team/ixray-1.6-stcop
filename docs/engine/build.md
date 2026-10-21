# Build

CMake ≥ 3.26, C++23. Compiler settings: `cmake/msvc.cmake` for MSVC, `cmake/clang.cmake` for Clang or Apple, `cmake/gcc.cmake` otherwise. Generate into `build/`. Startup project: `xrEngine` (`xrCompress` if `IXRAY_COMPRESSOR_ONLY`). Output: `build/bin/<Config>/`. Edit `src/` and `cmake/`. `build/` is generated.

Default Windows targets (`src/CMakeLists.txt`). Compressor-only builds retain `xrCore` and omit the other listed engine targets:

| Project | Output | Source |
| --- | --- | --- |
| `xrCore` | `xrCore.dll` | `src/xrCore` |
| `xrSound` | `xrSound.dll` | `src/xrSound` |
| `xrNetServer` | `xrNetServer.dll` | `src/xrNetServer` |
| `xrEngineCore` | `xrEngineCore.dll` | `src/xrEngine` |
| `xrPhysics` | `xrPhysics.dll` | `src/xrPhysics` |
| `xrScripts` | `xrScripts.dll` | `src/xrScripts` |
| `xrUI` | `xrUI.dll` | `src/xrUI` |
| `xrRHI` | `xrRHI.dll` | `src/xrRHI` |
| `xrRender_R1` | `xrRender_R1.dll` | `src/Layers/xrRenderPC_R1` |
| `xrRender_R2` | `xrRender_R2.dll` | `src/Layers/xrRenderPC_R2` |
| `xrRender_R4` | `xrRender_R4.dll` | `src/Layers/xrRenderPC_R4` |
| `xrGame` | `xrGame.dll` | `src/xrGame` |
| `xrEngine` | `xrEngine.exe` | `src/xrPlay` |
| `xrCrashDumpHelper` | `xrCrashDumpHelper.exe` | `src/xrCrashDumpHelper` |

`xrCrashDumpHelper` is Windows-only and is built with `xrPlay`. R1/R2 are controlled by `IXRAY_USE_R1` / `IXRAY_USE_R2`. R4, `xrGame`, and `xrPlay` are added when `IXR_TEST_CI` is true; all are skipped in compressor-only builds. `xrRHI` includes D3D9 and D3D11 backends (DXVK Native on Linux). Missing R4 triggers an R2 load attempt; if no renderer loaded, `CEngineAPI::Initialize` falls back to R1. See [rendering.md](rendering.md).

## Options

Root `CMakeLists.txt`.

| Option | Default | Effect |
| --- | --- | --- |
| `IXRAY_USE_R1` | ON | D3D9 static renderer |
| `IXRAY_USE_R2` | ON | D3D9 dynamic renderer |
| `IXRAY_UTILS` | OFF | `xrAI`, `xrLC`, `xrCompress`, other tools |
| `IXRAY_UTILS_OPTIX` | OFF | Require CUDA/OptiX lighting in Windows x64 MSVC `xrLevelBuilder` |
| `IXRAY_EDITORS` | OFF | `src/Editors` |
| `IXRAY_PLUGINS` | OFF | Max / Maya |
| `IXRAY_TESTS` | OFF | `src/test` |
| `IXRAY_MP` | OFF | `xrRender_DS0`; `xrServer`, `xrServerCLI`, `xrGameSpy` also require `IXR_TEST_CI` |
| `IXRAY_MP_SQL` | OFF | FreeMP SQL |
| `IXRAY_ASAN` | OFF | full rebuild required |
| `IXRAY_USE_MIMALLOC` | ON | mimalloc 3.5.3 engine heap; automatically bypassed with `IXRAY_ASAN=ON`. Set OFF for CRT comparison |
| `IXRAY_LDEBUG` | OFF | `LUABIND_DEBUG_SCRIPTS`; affects MSVC Debug exception flags |
| `IXRAY_UNITYBUILD` | ON | `xrGame` unity batches. `IXRAY_BUILD_BATCH_SIZE` default 32 |
| `IXRAY_PROFILER` | ON | Optick unless `IXRAY_PROFILER_TRACY` is enabled. `PROF_EVENT` / `PROF_FRAME` |
| `IXRAY_PROFILER_TRACY` | OFF | Tracy |
| `IXRAY_USE_COMPRESSOR` | ON | `xrCompress` target, even with `IXRAY_UTILS=OFF` |
| `IXRAY_COMPRESSOR_ONLY` | OFF | CI: compressor target only |
| `DEVIXRAY_ENABLE_SHIPPING` | OFF | Shipping cfg |

Defines: Debug = `DEBUG` + `DEBUG_DRAW`. RelWithDebInfo = `DEBUG_DRAW`. Release = `MASTER_GOLD`. Shipping defines `MASTER_GOLD` and `SHIPPING_BUILD`.

Unity errors point at the batch, not the `.cpp`. Set `IXRAY_UNITYBUILD=OFF` to map them.

Deps: `src/3rd-party`, `cmake/modules.cmake` (LuaJIT, luabind, ODE, Ogg/Vorbis, FreeType, ImGui, Theora, OpenAL). Windows packages via NuGet, except DLSS 310.9.1: CMake downloads the official NVIDIA release tag archive, verifies SHA-256, and extracts it into `build/dep/DLSS-310.9.1`. Offline builds require the extracted SDK or the cached `build/dep/DLSS-310.9.1.zip` archive. The release DLL and `DLSS-LICENSE.txt` are copied to the output directory. SDL: window and input. Audio playback uses OpenAL.

## xrLevelBuilder CUDA/OptiX

Enable the NVIDIA backend to download its build dependencies automatically:

```powershell
cmake --preset Utilities-x64-Windows -DIXRAY_UTILS_OPTIX=ON
cmake --build --preset Utilities-x64-Windows-RelWithDebInfo --target xrLevelBuilder
```

Without explicit SDK roots, configuration downloads SHA-256-pinned OptiX 9.0.0 headers
and the NVCC, CRT, CUDA Runtime, NVML development, CCCL and NVVM components of CUDA 13.4.3.
The NVIDIA release manifest is also pinned by SHA-256.
They are extracted under `build/x64/Utilities-Windows/_deps/nvidia/`, outside the published
NuGet packages. No system-wide CUDA installation or GPU is needed to compile PTX.
MSVC must be supported by the selected CUDA version; use the VS 2022 preset for the
downloaded CUDA 13.4.3 toolchain. The NVIDIA backend requires a compatible GPU and
driver at runtime. Use a driver supporting CUDA 13.4 PTX (R615 or newer); the lower
OptiX 9.0.0 driver minimum alone does not guarantee compatibility with newer PTX.

To use installed SDKs or an offline setup, pass `-DCUDAToolkit_ROOT=<cuda-root>` and
`-DOptiX_ROOT=<optix-root>` (the directory containing
`include/`). CUDA must be at least 12.8 and OptiX at least 9. Their accompanying licenses
must be present; nonstandard layouts can specify `IXRAY_CUDA_LICENSE_FILE` and
`IXRAY_OPTIX_LICENSE_FILE`. An enabled OptiX backend fails configuration if a required
dependency is missing. Leave `IXRAY_UTILS_OPTIX=OFF` for CPU-only builds.

Ship `CuTrace.ptx` beside `xrLevelBuilder.exe` and preserve `licenses/NVIDIA/` from the
build output. Driver DLLs and the `_deps/nvidia/` build directory must not be included in
the utility archive. CI builds both Debug and RelWithDebInfo with OptiX enabled and
preserves these runtime files when publishing. GPU execution needs a separate GPU test.

Downloading and using the SDKs is governed by the [CUDA license](https://docs.nvidia.com/cuda/eula/index.html)
and [OptiX license](https://github.com/NVIDIA/optix-dev/blob/v9.0.0/LICENSE.txt).
See [NVIDIA distribution requirements](../../doc/NVIDIA-Dependencies.txt) when publishing utilities.

Sources: [root options](../../CMakeLists.txt), [engine targets](../../src/CMakeLists.txt), [sound dependencies](../../src/xrSound/CMakeLists.txt).
