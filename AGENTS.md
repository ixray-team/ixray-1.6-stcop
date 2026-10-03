# AGENTS.md

C++23 X-Ray 1.6 fork. Engine internals: [docs/engine](docs/engine/README.md). Style: [docs/engine/style.md](docs/engine/style.md), [doc/code-style-cpp.md](doc/code-style-cpp.md). Modder wiki: `docs/docs`.

`xrEngine.exe` is `src/xrPlay`. `xrEngineCore.dll` is `src/xrEngine`. Startup project: `xrEngine`.

| Path | Binary | Owns |
| --- | --- | --- |
| `src/xrPlay` | `xrEngine.exe` | `CApplication::Run`, SDL window, splash |
| `src/xrCore` | `xrCore.dll` | `Memory`, `FS`, `CInifile`, log, `xrDebug`, addons |
| `src/xrEngine` | `xrEngineCore.dll` | `Device`, loop, input, console, scheduler, `CEngineAPI` |
| `src/xrRHI` | `xrRHI.dll` | D3D11 and D3D12 (Windows) |
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

## Renderer

R4 lighting mode and graphics API are independent. Console fonts share the R4 batching path; preserve draw order and the 16-bit quad-index limit. Backend selection, stream uploads and console performance: [docs/engine/rendering.md](docs/engine/rendering.md).

Terrain is an editor heightmap converted to ordinary static level meshes. Implicit lighting follows the base texture's `flImplicitLighted` metadata: base alpha stores hemi; `_lm.dds` stores RGB light + sun alpha; `_mask` blends four detail materials. Editor chunks are not runtime terrain tiles. Authoring, xrLC bake, R4 shading and current export/persistence defects: [docs/engine/terrain.md](docs/engine/terrain.md).

Reflections: [docs/engine/reflections.md](docs/engine/reflections.md). Preserve async collection before `FrameMove`; GPU capture stays on the primary thread. Trace points must cross current-view/world/captured-view spaces with translation. CPU pass constants and HLSL layouts change together. SSR reads current-frame opaque lighting from `rt_sslr_scene`, prepared by combine element 4 before tracing; water shares this immutable source. Previous matrices/jitter apply only to temporal history. Preserve the source copy/pass ordering and RT/SRV separation. SSR has four compute stages, six SSR blender elements, and three temporal UAVs; keep color/depth and surface histories paired. Zero history depth is invalid; negative depth identifies HUD.

R4 uses `shaders/d3d11` on both APIs. Keep shared reflection shaders synchronized across `gamedata`, `gamedata_soc`, `gamedata_cs`, and `gamedata_coc`; preserve variant-specific water/combine code. Preserve nearest valid captured-surface VSLR gap filling when strict refinement misses, but uncovered full-distance endpoints must use weather sky. The capture cube does not render sky. Glossy world history uses the mirrored trace hit; validate each history tap against the receiver plane and paired surface metadata. Detail grass has no VSLR capture path yet. The user confirmed the current-frame source/sky fixes work on 2026-10-03; the later foreground-occlusion fallback awaits user validation. It searches at most 24 neighboring cube depths only when a covered foreground surface leaves no forward candidate; preserve the sky exit and finite-ray bounds. Cross-API/overlay coverage and GPU cost remain unmeasured. The extra source pass is a full-screen combine, reusing existing light accumulation; geometry, shadows and individual lights are not rendered twice. Combine element 4 and SSR element 4 are different passes. Keep `SSLR_SOURCE_PASS` scoped to source compilation, and apply restored graphics targets before compute reads the source.

## Frame

`CApplication::Run` → `BeginPlay` → `InitEngine` → `MigrateToGameWindow` → `EngineLoopAndDestroy` → `Device.Run` → `message_loop` (SDL, then `on_idle`).

`on_idle` (`src/xrEngine/device.cpp`): if `g_loading_events` is non-empty, run one functor and `LoadDraw`. Otherwise snapshot reflections with `BeginReflectionCollect`, call `ResetSunCollect`, launch `PreRenderThread` (sun, reflections, particles), then run `FrameMove` (`seqFrame`) on the primary thread. Launch `GameThread` (scheduler, `seqFrameMT`, sound, Lua GC), then run `seqRender`. Reflection collection uses the captured camera, which can differ from the render camera updated by `FrameMove`.

`NEW_INSTANCE(clsid)` → `xrFactory_Create` in `xrGame`.

## Edit map

| Change | File |
| --- | --- |
| Boot, console, input, dt | `src/xrEngine/x_ray.cpp`, `device.cpp`, `XR_IOConsole.cpp` |
| Pass or lighting mode | `src/Layers/xrRenderPC_R4/r4_R_render.cpp`, `EngineAPI.cpp` |
| GPU | `src/xrRHI` |
| Terrain authoring / bake / material | `src/Editors/LevelEditor/Editor/Terrain`, `Entry/Terrain`, `Tools/Terrain`; `src/utils/xrLC_Light/xrLight_Implicit*`; `src/Layers/xrRenderPC_R4/Blender_BmmD.cpp` |
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

## Runtime diagnosis

For runtime bug work, the user builds and tests. Do not run local builds/tests or add temporary debug switches, probe dumps, shader toggles, or instrumentation. Supply concrete fixes and exact runtime/RenderDoc checks. Use Debug for checks and Debug plus RelWithDebInfo for final validation; never Release. Rendering verification requires a loaded level and actual output; use the existing headless autotest for repeatable captures/timings. Compilation alone does not establish visual correctness or performance.

## Code rules

- Types: `u32`, `xr_vector`, `xr_string`, `xr_string_view`, `xr_hash_map`, `xr_hash_set`, `xr_unique_ptr`, `shared_str`. `#pragma once`. `std::` only when no IXR alias exists, or before `Memory._initialize`.
- RAII for memory, files, threads, mutexes, OS handles, SDL, and GPU objects. A one-off resource in the whole engine can be acquired and released by hand.
- Stateless helpers go in a `namespace`. State, resources, and services go in a `class`. One interface for alternate implementations. Platform headers and APIs stay in `Platform` code.
- New names: PascalCase types, `_camelCase` private fields, `g_` globals, `I` interfaces. Keep existing public names.
- UTF-8, CRLF, one trailing empty line. Comments in English. Hacks: `// HACK:`.
- Client renderer is `xrRender_R4`. Mode is `ELightingMode`: `renderer_r4_static` (alias `renderer_r1`) or `renderer_r4` (alias `renderer_r2`). Locked in `CEngineAPI::InitializeNotDedicated`. Test with `LightingModeIsStatic()` / `LightingModeIsDynamic()`.
- `VERIFY` is stripped from `MASTER_GOLD`. Shipping checks use `R_ASSERT`.
- Swapchain and Lua stay off `PreRenderThread`. `GRHI` draws stay on the primary thread.
