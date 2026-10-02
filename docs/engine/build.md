# Build

CMake ≥ 3.26, C++23, `cmake/msvc.cmake` on Windows. Generate into `build/`. Startup project: `xrEngine` (`xrCompress` if `IXRAY_COMPRESSOR_ONLY`). Output: `build/bin/<Config>/`. Edit `src/` and `cmake/`. `build/` is generated.

Default link (`src/CMakeLists.txt`), skipped when `IXRAY_COMPRESSOR_ONLY`:

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
| `xrRender_R4` | `xrRender_R4.dll` | `src/Layers/xrRenderPC_R4` |
| `xrGame` | `xrGame.dll` | `src/xrGame` |
| `xrEngine` | `xrEngine.exe` | `src/xrPlay` |

Missing `xrRender_R4.dll` fails in `InitializeNotDedicated`.

## Options

Root `CMakeLists.txt`.

| Option | Default | Effect |
| --- | --- | --- |
| `IXRAY_UTILS` | OFF | `xrAI`, `xrLC`, `xrCompress`, other tools |
| `IXRAY_EDITORS` | OFF | `src/Editors` |
| `IXRAY_PLUGINS` | OFF | Max / Maya |
| `IXRAY_TESTS` | OFF | `src/test` |
| `IXRAY_MP` | OFF | `xrRender_DS0`, `xrServer`, `xrServerCLI`, `xrGameSpy` |
| `IXRAY_MP_SQL` | OFF | FreeMP SQL |
| `IXRAY_ASAN` | OFF | full rebuild required |
| `IXRAY_LDEBUG` | OFF | `LUA_DEBUG` |
| `IXRAY_UNITYBUILD` | ON | `xrGame` unity batches. `IXRAY_BUILD_BATCH_SIZE` default 32 |
| `IXRAY_PROFILER` | ON | Optick. `PROF_EVENT` / `PROF_FRAME` |
| `IXRAY_PROFILER_TRACY` | OFF | Tracy |
| `IXRAY_ENABLE_RESONANCEAUDIO` | ON | reverb in `xrSound` |
| `IXRAY_USE_COMPRESSOR` | ON | compressor support |
| `IXRAY_COMPRESSOR_ONLY` | OFF | CI: compressor target only |
| `DEVIXRAY_ENABLE_SHIPPING` | OFF | Shipping cfg |

Defines: Debug = `DEBUG` + `DEBUG_DRAW`. RelWithDebInfo = `DEBUG_DRAW`. Release = `MASTER_GOLD`. Shipping adds `SHIPPING_BUILD`.

Unity errors point at the batch, not the `.cpp`. Set `IXRAY_UNITYBUILD=OFF` to map them.

Deps: `src/3rd-party`, `cmake/modules.cmake` (LuaJIT, luabind, ODE, Ogg/Vorbis, FreeType, ImGui, Theora). Windows packages via NuGet. SDL: window, input, audio.
