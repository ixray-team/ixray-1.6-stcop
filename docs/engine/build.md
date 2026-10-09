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

R1/R2 are controlled by `IXRAY_USE_R1` / `IXRAY_USE_R2`. R4, `xrGame`, and `xrPlay` are added when `IXR_TEST_CI` is true; all are skipped in compressor-only builds. `xrRHI` includes D3D9 and D3D11 backends (DXVK Native on Linux). Missing R4 triggers an R2 load attempt; if no renderer loaded, `CEngineAPI::Initialize` falls back to R1. See [rendering.md](rendering.md).

## Options

Root `CMakeLists.txt`.

| Option | Default | Effect |
| --- | --- | --- |
| `IXRAY_USE_R1` | ON | D3D9 static renderer |
| `IXRAY_USE_R2` | ON | D3D9 dynamic renderer |
| `IXRAY_UTILS` | OFF | `xrAI`, `xrLC`, `xrCompress`, other tools |
| `IXRAY_EDITORS` | OFF | `src/Editors` |
| `IXRAY_PLUGINS` | OFF | Max / Maya |
| `IXRAY_TESTS` | OFF | `src/test` |
| `IXRAY_MP` | OFF | `xrRender_DS0`; `xrServer`, `xrServerCLI`, `xrGameSpy` also require `IXR_TEST_CI` |
| `IXRAY_MP_SQL` | OFF | FreeMP SQL |
| `IXRAY_ASAN` | OFF | full rebuild required |
| `IXRAY_LDEBUG` | OFF | `LUABIND_DEBUG_SCRIPTS`; affects MSVC Debug exception flags |
| `IXRAY_UNITYBUILD` | ON | `xrGame` unity batches. `IXRAY_BUILD_BATCH_SIZE` default 32 |
| `IXRAY_PROFILER` | ON | Optick unless `IXRAY_PROFILER_TRACY` is enabled. `PROF_EVENT` / `PROF_FRAME` |
| `IXRAY_PROFILER_TRACY` | OFF | Tracy |
| `IXRAY_USE_COMPRESSOR` | ON | `xrCompress` target, even with `IXRAY_UTILS=OFF` |
| `IXRAY_COMPRESSOR_ONLY` | OFF | CI: compressor target only |
| `DEVIXRAY_ENABLE_SHIPPING` | OFF | Shipping cfg |

Defines: Debug = `DEBUG` + `DEBUG_DRAW`. RelWithDebInfo = `DEBUG_DRAW`. Release = `MASTER_GOLD`. Shipping defines `MASTER_GOLD` and `SHIPPING_BUILD`.

Unity errors point at the batch, not the `.cpp`. Set `IXRAY_UNITYBUILD=OFF` to map them.

Deps: `src/3rd-party`, `cmake/modules.cmake` (LuaJIT, luabind, ODE, Ogg/Vorbis, FreeType, ImGui, Theora, OpenAL). Windows packages via NuGet. SDL: window and input. Audio playback uses OpenAL.

Sources: [root options](../../CMakeLists.txt), [engine targets](../../src/CMakeLists.txt), [sound dependencies](../../src/xrSound/CMakeLists.txt).
