# AGENTS.md

C++23 X-Ray 1.6 fork. Engine internals: [docs/engine](docs/engine/README.md). Style: [docs/engine/style.md](docs/engine/style.md), [doc/code-style-cpp.md](doc/code-style-cpp.md). Modder wiki: `docs/docs`.

`xrEngine.exe` is `src/xrPlay`. `xrEngineCore.dll` is `src/xrEngine`. Startup project: `xrEngine`.

| Path | Binary | Owns |
| --- | --- | --- |
| `src/xrPlay` | `xrEngine.exe` | `CApplication::Run`, SDL window, splash |
| `src/xrCore` | `xrCore.dll` | `Memory`, `FS`, `CInifile`, log, `xrDebug`, addons |
| `src/xrEngine` | `xrEngineCore.dll` | `Device`, loop, input, console, scheduler, `CEngineAPI` |
| `src/xrRHI` | `xrRHI.dll` | D3D11 only (`ERHI_API_LAYER::D3D11`) |
| `src/Layers/xrRenderPC_R4` | `xrRender_R4.dll` | Client scene. Static and dynamic lighting |
| `src/Layers/xrRenderDS_R0` | `xrRender_DS0.dll` | Dedicated server (`IXRAY_MP`) |
| `src/xrGame` | `xrGame.dll` | Level, actor, AI, ALife, weapons, `*_script.cpp` |
| `src/xrScripts` | `xrScripts.dll` | LuaJIT + luabind. `ai().script_engine()` |
| `src/xrPhysics` | `xrPhysics.dll` | ODE via `CPHWorld` / `CPHShell` |
| `src/xrSound` | `xrSound.dll` | SDL audio, Ogg/Vorbis |
| `src/xrUI` | `xrUI.dll` | XML widgets |
| `src/xrNetServer` | `xrNetServer.dll` | Packets |
| `src/xrServer`, `src/xrGameSpy` | exe / dll | `IXRAY_MP` only |

Call downward only. `xrCore` has no game or renderer includes.

## Frame

`CApplication::Run` → `BeginPlay` → `InitEngine` → `MigrateToGameWindow` → `EngineLoopAndDestroy` → `Device.Run` → `message_loop` (SDL, then `on_idle`).

`on_idle` (`src/xrEngine/device.cpp`): if `g_loading_events` is non-empty, run one functor and `LoadDraw`. Else `FrameMove` (`seqFrame`) on the primary thread, `PreRenderThread` (sun, reflections, particles), `GameThread` (scheduler, `seqFrameMT`, sound, Lua GC), then `seqRender`.

`NEW_INSTANCE(clsid)` → `xrFactory_Create` in `xrGame`.

## Edit map

| Change | File |
| --- | --- |
| Boot, console, input, dt | `src/xrEngine/x_ray.cpp`, `device.cpp`, `XR_IOConsole.cpp` |
| Pass or lighting mode | `src/Layers/xrRenderPC_R4/r4_R_render.cpp`, `EngineAPI.cpp` |
| GPU | `src/xrRHI` |
| Actor, inventory, weapons | `Actor.*`, `inventory*.*`, `Weapon*` |
| Stalker AI | `stalker_*`, `cover_manager.*`, `ai_space.*` |
| ALife | `alife_*` |
| Level tick / net | `Level.cpp`, `Level_start.cpp` |
| Lua binding | `src/xrScripts`, matching `*_script.cpp` |
| Physics | `PHWorld.*`, `PHShell.*` |
| Files, ltx, DLTX, addons | `LocatorAPI.*`, `xr_ini.*`, `xrAddons.*` |

## Autotest

`xrEngine.exe -autotest … -autotest_cmd <cmds>`. `-autotest_cmd` must be last. Flags, exit code, and the motion-vector cvar order: [docs/engine/lifecycle.md](docs/engine/lifecycle.md).

## Build

CMake ≥ 3.26, x64, output `build/bin/<Config>/`. Details: [docs/engine/build.md](docs/engine/build.md).

`IXRAY_UNITYBUILD` defaults on for `xrGame` (batch 32). `IXRAY_MP` adds the dedicated renderer and `xrServer`. `IXRAY_EDITORS`, `IXRAY_UTILS`, `IXRAY_PLUGINS`, `IXRAY_TESTS` default off. `IXRAY_ASAN` requires a full rebuild. `IXRAY_PROFILER` (Optick) defaults on.

## Code rules

- Types: `u32`, `xr_vector`, `xr_string`, `xr_string_view`, `xr_hash_map`, `xr_hash_set`, `xr_unique_ptr`, `shared_str`. `#pragma once`. `std::` only when no IXR alias exists, or before `Memory._initialize`.
- RAII for memory, files, threads, mutexes, OS handles, SDL, and GPU objects. A one-off resource in the whole engine can be acquired and released by hand.
- Stateless helpers go in a `namespace`. State, resources, and services go in a `class`. One interface for alternate implementations. Platform headers and APIs stay in `Platform` code.
- New names: PascalCase types, `_camelCase` private fields, `g_` globals, `I` interfaces. Keep existing public names.
- UTF-8, CRLF, one trailing empty line. Comments in English. Hacks: `// HACK:`.
- Client renderer is `xrRender_R4`. Mode is `ELightingMode`: `renderer_r4_static` (alias `renderer_r1`) or `renderer_r4` (alias `renderer_r2`). Locked in `CEngineAPI::InitializeNotDedicated`. Test with `LightingModeIsStatic()` / `LightingModeIsDynamic()`.
- `VERIFY` is stripped from `MASTER_GOLD`. Shipping checks use `R_ASSERT`.
- Swapchain and Lua stay off `PreRenderThread`. `GRHI` draws stay on the primary thread.
