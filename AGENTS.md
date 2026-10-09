# AGENTS.md

C++23 X-Ray 1.6 fork. Engine internals: [docs/engine](docs/engine/README.md). Style: [docs/engine/style.md](docs/engine/style.md), [doc/code-style-cpp.md](doc/code-style-cpp.md). Modder wiki: `docs/docs`.

`xrEngine.exe` is `src/xrPlay`. `xrEngineCore.dll` is `src/xrEngine`. Startup project: `xrEngine`.

| Path | Binary | Owns |
| --- | --- | --- |
| `src/xrPlay` | `xrEngine.exe` | `CApplication::Run`, SDL window, splash |
| `src/xrCore` | `xrCore.dll` | `Memory`, `FS`, `CInifile`, log, `xrDebug`, addons |
| `src/xrEngine` | `xrEngineCore.dll` | `Device`, loop, input, console, scheduler, `CEngineAPI` |
| `src/xrRHI` | `xrRHI.dll` | D3D9 and D3D11 backends (`D3D9/`, `D3D11/`); Linux uses DXVK Native |
| `src/Layers/xrRenderPC_R1` | `xrRender_R1.dll` | D3D9 static lighting (`renderer_r1`) |
| `src/Layers/xrRenderPC_R2` | `xrRender_R2.dll` | D3D9 dynamic lighting (`renderer_r2`) |
| `src/Layers/xrRenderPC_R4` | `xrRender_R4.dll` | D3D11 dynamic lighting (`renderer_r4`) |
| `src/Layers/xrRender`, `xrRenderDX9`, `xrRenderDX10` | Shared renderer sources | Common scene, resources, blenders, and DX9/DX11 code compiled into renderer DLLs |
| `src/Layers/xrRenderDS_R0` | `xrRender_DS0.dll` | Dedicated server (`IXRAY_MP`) |
| `src/xrGame` | `xrGame.dll` | Level, actor, AI, ALife, weapons, `*_script.cpp` |
| `src/xrScripts` | `xrScripts.dll` | LuaJIT + luabind. `ai().script_engine()` |
| `src/xrPhysics` | `xrPhysics.dll` | ODE via `CPHWorld` / `CPHShell` |
| `src/xrSound` | `xrSound.dll` | OpenAL audio, Ogg/Vorbis |
| `src/xrUI` | `xrUI.dll` | XML widgets |
| `src/xrNetServer` | `xrNetServer.dll` | Packets |
| `src/xrServer`, `src/xrGameSpy` | exe / dll | `IXRAY_MP` only |

Keep dependencies downward. Do not add game or renderer implementation dependencies to `xrCore`; existing collision code uses shared render interfaces from `src/Include/xrRender`.

## Frame

`CApplication::Run` → `BeginPlay` → `InitEngine` → `MigrateToGameWindow` → `EngineLoopAndDestroy` → `Device.Run` → `message_loop` (SDL, then `on_idle`).

`on_idle` (`src/xrEngine/device.cpp`): if `g_loading_events` is non-empty, run one functor and `LoadDraw`. Else launch `PreRenderThread` (`seqParallelRender`, rain, particles), run `FrameMove` (`seqFrame`) on the primary thread, launch `GameThread` (scheduler, `seqFrameMT`, `seqParallel` including level scripts/GC, sound events), and process `seqRender` on the primary thread. Wait for secondary tasks before `EndRender`.

`NEW_INSTANCE(clsid)` → `xrFactory_Create` in `xrGame`.

## Edit map

| Change | File |
| --- | --- |
| Boot, console, input, dt | `src/xrEngine/x_ray.cpp`, `device.cpp`, `XR_IOConsole.cpp` |
| Render passes | `src/Layers/xrRenderPC_R1/FStaticRender.cpp`, `src/Layers/xrRenderPC_R2/r2_R_render.cpp`, `src/Layers/xrRenderPC_R4/r4_R_render.cpp` |
| Renderer selection / API | `src/xrEngine/EngineAPI.cpp`, `xr_ioc_cmd.cpp`, `Device_create_render.cpp` |
| Shared render resources / blenders | `src/Layers/xrRender`, `src/Layers/xrRenderDX9` |
| GPU | `src/xrRHI` |
| Actor, inventory, weapons | `Actor.*`, `inventory*.*`, `Weapon*` |
| Stalker AI | `src/xrGame/ai/stalker`, `Legacy/StalkerPlanner`, `stalker_*`, `cover_manager.*`, `ai_space.*` |
| ALife | `alife_*` |
| Level tick / net | `Level.cpp`, `Level_start.cpp` |
| Lua binding | `src/xrScripts`, matching `*_script.cpp` |
| Physics | `PHWorld.*`, `PHShell.*` |
| Files, ltx, DLTX, addons | `LocatorAPI.*`, `xr_ini.*`, `xrAddons.*` |

## Autotest

`xrEngine.exe -autotest … -autotest_cmd <cmds>`. `-autotest_cmd` must be last. Flags, exit code, and the renderer selection and motion-vector shader option: [docs/engine/lifecycle.md](docs/engine/lifecycle.md).

## Build

CMake ≥ 3.26, x64, output `build/bin/<Config>/`. Details: [docs/engine/build.md](docs/engine/build.md).

`IXRAY_USE_R1` and `IXRAY_USE_R2` default on; R4 is added when `IXR_TEST_CI` is true (see `src/CMakeLists.txt`). `IXRAY_UNITYBUILD` defaults on for `xrGame` (batch 32). `IXRAY_MP` adds the dedicated renderer; `xrServer` / `xrServerCLI` / `xrGameSpy` also require `IXR_TEST_CI`. `IXRAY_EDITORS`, `IXRAY_UTILS`, `IXRAY_PLUGINS`, `IXRAY_TESTS` default off. `IXRAY_ASAN` requires a full rebuild. `IXRAY_PROFILER` (Optick) defaults on.

## Code rules

- Types: `u32`, `xr_vector`, `xr_string`, `xr_string_view`, `xr_hash_map`, `xr_hash_set`, `xr_unique_ptr`, `shared_str`. `#pragma once`. `std::` only when no IXR alias exists, or before `Memory._initialize`.
- Locks: `xrCriticalSection` + `xrCriticalSectionGuard`, or `xrSRWLock` + `xrSRWLockGuard` (`shared = true` for readers). No `std::mutex`, `std::shared_mutex`, or `std::lock_guard`.
- RAII for memory, files, threads, mutexes, OS handles, SDL, and GPU objects. A one-off resource in the whole engine can be acquired and released by hand.
- Stateless helpers go in a `namespace`. State, resources, and services go in a `class`. One interface for alternate implementations. Platform headers and APIs stay in `Platform` code.
- New names: PascalCase for types, namespaces, functions, variables, fields, and constants (no `_` or `m_` field prefix). `g_` globals, `I` interfaces, `UPPER_CASE` macros. Keep existing public names. Checked by `.clang-tidy`.
- Formatting: `.clang-format` (tabs, Allman braces, braces always, no column limit, includes are not sorted).
- UTF-8, CRLF, one trailing empty line. Comments in English. Hacks: `// HACK:`.
- Client renderers are separate DLLs: `xrRender_R1`, `xrRender_R2`, `xrRender_R4`. `renderer_r1`, `renderer_r2`, and `renderer_r4` select them via `rsR2` / `rsR4` in `xr_ioc_cmd.cpp`. `CEngineAPI::InitializeNotDedicated` tries R4, then R2 on load failure; `Initialize` falls back to R1. `GetAPI()` returns D3D11 for `rsR4`, otherwise D3D9. `CreateRendererList` checks renderer library presence. This branch has no `ELightingMode`, `LightingModeIsStatic()` / `LightingModeIsDynamic()`, or `renderer_r4_static`.
- `VERIFY` is enabled only with `DEBUG`. `R_ASSERT` is stripped with `SHIPPING_BUILD`; checks that must survive Shipping need explicit control flow (or `CHECK_OR_EXIT` for fatal errors).
- Swapchain and Lua stay off `PreRenderThread`. `GRHI` draws stay on the primary thread.
