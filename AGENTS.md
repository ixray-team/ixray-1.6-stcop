# AGENTS.md

C++23 X-Ray 1.6 fork. Engine internals: [docs/engine](docs/engine/README.md). Style: [docs/engine/style.md](docs/engine/style.md), [doc/code-style-cpp.md](doc/code-style-cpp.md). Modder wiki: `docs/docs`.

`xrEngine.exe` is `src/xrPlay`. `xrEngineCore.dll` is `src/xrEngine`. Startup project: `xrEngine`.

| Path | Binary | Owns |
| --- | --- | --- |
| `src/xrPlay` | `xrEngine.exe` | `CApplication::Run`, SDL window |
| `src/xrCrashDumpHelper` | `xrCrashDumpHelper.exe` | Crash dump helper process (Windows, no engine/profiler dependencies) |
| `src/xrCore` | `xrCore.dll` | `Memory`, `FS`, `CInifile`, log, `xrDebug`, addons |
| `src/xrEngine` | `xrEngineCore.dll` | `Device`, loop, input, console, scheduler, `CEngineAPI` |
| `src/xrRHI` | `xrRHI.dll` | D3D9, D3D11 and D3D12 backends (`D3D9/`, `D3D11/`, `D3D12/`); Linux uses DXVK Native |
| `src/Layers/xrRenderIXR` | `xrRender_IXR.dll` | The client renderer: one forward+ frame over the baked light (`renderer_simplified`), D3D11 / D3D12 through NRI |
| `src/Layers/xrRenderDS_R0` | `xrRender_DS0.dll` | Dedicated server (`IXRAY_MP`) |
| `src/xrGame` | `xrGame.dll` | Level, actor, AI, ALife, weapons, `*_script.cpp` |
| `src/xrScripts` | `xrScripts.dll` | LuaJIT + luabind. `ai().script_engine()` |
| `src/xrPhysics` | `xrPhysics.dll` | ODE via `CPHWorld` / `CPHShell` |
| `src/xrSound` | `xrSound.dll` | OpenAL audio, Ogg/Vorbis |
| `src/xrUI` | `xrUI.dll` | XML widgets |
| `src/xrNetServer` | `xrNetServer.dll` | Packets |
| `src/xrServer`, `src/xrGameSpy` | exe / dll | `IXRAY_MP` only |

Keep dependencies downward. Do not add game or renderer implementation dependencies to `xrCore`; existing collision code uses shared render interfaces from `src/Include/xrRender`.

Coordinate system: left-handed (LH), with +Y pointing up.

## Renderer

Dynamic geometry (UI, fonts, debug draws, stat graphs, particles, glows, flares, sky, clouds, thunderbolt, portals, rain, puddles, editor helpers) goes through `RImplementation.Dynamic` (`DynamicGeometry`): 32 B records the VS pulls at t57 (`g_RingVertices`), drawn through NRI by `NriPass::DrawDynamic` with the pass and state set on the command list; quads draw with `DrawQuads`, no index buffer (indexed producers expand on the CPU). A view bound by hand reaches it only when `SetResource` is given its surface or an NRI view of the same data; a register read with nothing bound gets a zero view of its kind. Fullscreen passes (`DrawFullscreen`), clears, copies, mips and dispatches (`NriPass::Dispatch`, the skinning pass included) go through NRI too. There is no old-backend fallback: what NRI cannot take is a caller error, logged once a reason and not drawn. The graph's LOD cards (`r_dsgraph_render_lods`) go through the same stream. On D3D11 consecutive dynamic draws form one replayed run: anything else that reaches the context must call `FlushNriRun` first (`GfxCmdList` draws, dispatches, clears, copies, queries and `BeginNriCommands` do). Preserve draw order. Details: [docs/engine/rendering.md](docs/engine/rendering.md#dynamic-geometry-stream).

Terrain is an editor heightmap converted to ordinary static level meshes. Implicit lighting follows the base texture's `flImplicitLighted` metadata: base alpha stores hemi; `_lm.dds` stores RGB light + sun alpha; `_mask` blends four detail materials. Editor chunks are not runtime terrain tiles. Authoring, xrLC bake, R4 shading and current export/persistence defects: [docs/engine/terrain.md](docs/engine/terrain.md).

## Frame

`CApplication::Run` → `BeginPlay` → `InitEngine` → `MigrateToGameWindow` → `EngineLoopAndDestroy` → `Device.Run` → `message_loop` (a swap-chain-only resize when the window's size, `rs_v_sync` or `r_frame_queue` changed, `GRHI->BeginFrame`, the renderer's `PaceFrame` wait, SDL input, then `on_idle`). Sizes: swap chain = window, targets = `vid_mode` (made by `vid_restart`; a windowed window sized by its frame becomes `vid_mode` once it rests), view <= targets fitted into the window by the gamma pass: [docs/engine/rendering.md](docs/engine/rendering.md#view-rectangle).

`on_idle` (`src/xrEngine/device.cpp`): if `g_loading_events` is non-empty, run one functor and `LoadDraw`. Else launch `PreRenderThread` (`seqParallelRender`, the reflection cube's graphs, rain, particles), run `FrameMove` (`seqFrame`) on the primary thread, launch `GameThread` (sound events, scheduler, `seqParallel` including level scripts, `seqFrameMT`, `LuaGC`, sound update), and process `seqRender` and the frame's end on the primary thread (the sun cascades' portals, and the main view's sector walk on a level the scene does not draw whole, run on the task pool while the main view collects its dynamic objects, and with `r_parallel_views 1`, default 0, on D3D12 the sun cascades and the lamp atlas pages record their NRI command buffers there). On D3D12 `NriDevice::EndFrame` hands the frame's command buffers, its fence signal and its present to the submit thread (`NriSubmitThread`: `QueueSubmit`, `QueuePresent`); D3D11 submits and presents in place. Then join both threads, and `EndRender` runs the `rs_fps_limit` limiter.

`NEW_INSTANCE(clsid)` → `xrFactory_Create` in `xrGame`.

## Edit map

| Change | File |
| --- | --- |
| Boot, console, input, dt | `src/xrEngine/x_ray.cpp`, `device.cpp`, `XR_IOConsole.cpp` |
| Render passes | `src/Layers/xrRenderIXR/ixr_R_render.cpp` (`RenderForwardLevel`), `Forward*.cpp` |
| Renderer selection / API | `src/xrEngine/RenderFeatures.cpp`, `EngineAPI.cpp`, `xr_ioc_cmd.cpp`, `Device_create_render.cpp` |
| Shared render resources / blenders | `src/Layers/xrRenderIXR` (`r__blenders.cpp`, `Blender_*.cpp`, `ResourceManager*.cpp`) |
| GPU | `src/xrRHI` |
| Terrain authoring / bake / material | `src/Editors/LevelEditor/Editor/Terrain`, `Entry/Terrain`, `Tools/Terrain`; `src/utils/xrLC_Light/xrLight_Implicit*`; `src/Layers/xrRenderIXR/Blender_BmmD.cpp` |
| Actor, inventory, weapons | `Actor.*`, `inventory*.*`, `Weapon*` |
| Stalker AI | `src/xrGame/ai/stalker`, `Legacy/StalkerPlanner`, `stalker_*`, `cover_manager.*`, `ai_space.*` |
| ALife | `alife_*` |
| Level tick / net | `Level.cpp`, `Level_start.cpp` |
| Lua binding | `src/xrScripts`, matching `*_script.cpp` |
| Physics | `PHWorld.*`, `PHShell.*` |
| Files, ltx, DLTX, addons | `LocatorAPI.*`, `xr_ini.*`, `xrAddons.*` |

## ImGui editors and utilities

In-game ImGui tools live in `src/xrGame/ImUtils` (editors, panels, picking, gizmo) plus `src/xrGame/LevelInspector.*`; shell-level tools are in `src/xrPlay/UI*.cpp`. They are opened from **Game → Editors** and require `DEBUG_DRAW`; `Alt` toggles mouse capture.

Documentation is part of the change:

- Before changing an in-game ImGui editor or utility, read its document in `docs/engine/` (index: [docs/engine/README.md](docs/engine/README.md)) and start the research there instead of re-deriving behavior from the sources. Existing examples: [docs/engine/ai-grid-editor.md](docs/engine/ai-grid-editor.md), [docs/engine/cinema-film-maker-editor.md](docs/engine/cinema-film-maker-editor.md).
- Every change to such an editor must update its document in the same change: what the tool does, its controls, data sources (ltx/configs), limitations, and the post-build check list. The document is written for people who use the tool (modders), so keep it behaviour-level: no class names, function names or engine internals — those stay in the sources.
- If the document does not exist yet, create `docs/engine/<tool>.md` and add it to the [docs/engine/README.md](docs/engine/README.md) table, so later work on that tree, on new features, or on new ImGui utilities starts from the written contract.

## Autotest

`xrEngine.exe -autotest … -autotest_cmd <cmds>`. `-autotest_cmd` must be last. Flags, exit code, and the renderer selection and motion-vector shader option: [docs/engine/lifecycle.md](docs/engine/lifecycle.md).

## Build

Never build the project unless the user explicitly requests a build. This includes individual targets and test executables.

CMake ≥ 3.30, x64, output `build/bin/<Config>/`. Details: [docs/engine/build.md](docs/engine/build.md).

`xrRender_IXR` is added when `IXR_TEST_CI` or `IXRAY_EDITORS` is true (see `src/CMakeLists.txt`). `IXRAY_UNITYBUILD` defaults on for `xrGame` (batch 32). `IXRAY_MP` adds the dedicated renderer; `xrServer` / `xrServerCLI` / `xrGameSpy` also require `IXR_TEST_CI`. `IXRAY_EDITORS`, `IXRAY_UTILS`, `IXRAY_PLUGINS`, `IXRAY_TESTS` default off. `IXRAY_ASAN` requires a full rebuild. `IXRAY_PROFILER` (Optick) defaults on.

## Runtime diagnosis

For runtime bug work, the user builds and tests. Do not run local builds/tests or add temporary debug switches, probe dumps, shader toggles, or instrumentation. Supply concrete fixes and exact runtime/RenderDoc checks. Use Debug for checks and Debug plus RelWithDebInfo for final validation; never Release. Rendering verification requires a loaded level and actual output; use the existing headless autotest for repeatable captures/timings. Compilation alone does not establish visual correctness or performance.

## Code rules

- Unix style: code must be cross-platform across Windows, Linux, and macOS and must build with Clang. Use portable types (`u32`, `s32`, `u64`, `size_t`, `char`, ...), never WinAPI types (`DWORD`, `WORD`, `BOOL`, `HANDLE`, `LPCSTR`, `UINT`, ...). No WinAPI calls in shared code; if WinAPI is required, keep it under `#ifdef _WIN32` (or in `Platform` code) and write a cross-platform equivalent for Linux and macOS. No MSVC-only extensions.
- Types: `u32`, `xr_vector`, `xr_string`, `xr_string_view`, `xr_hash_map`, `xr_hash_set`, `xr_unique_ptr`, `shared_str`. `#pragma once`. `std::` only when no IXR alias exists, or before `Memory._initialize`.
- Locks: `xrCriticalSection` + `xrCriticalSectionGuard`, or `xrSRWLock` + `xrSRWLockGuard` (`shared = true` for readers). No `std::mutex`, `std::shared_mutex`, or `std::lock_guard`.
- RAII for memory, files, threads, mutexes, OS handles, SDL, and GPU objects. A one-off resource in the whole engine can be acquired and released by hand.
- Stateless helpers go in a `namespace`. State, resources, and services go in a `class`. One interface for alternate implementations. Platform headers and APIs stay in `Platform` code.
- New names: PascalCase for types, namespaces, functions, variables, fields, and constants (no `_` or `m_` field prefix). `g_` globals, `I` interfaces, `UPPER_CASE` macros. Keep existing public names. Checked by `.clang-tidy`.
- Formatting: `.clang-format` (tabs, Allman braces, braces always, no column limit, includes are not sorted).
- UTF-8, CRLF, one trailing empty line. Comments in English. Hacks: `// HACK:`.
- One client renderer DLL, `xrRender_IXR`, with one frame: the forward+ level frame (`RenderForwardLevel`). The deferred (advanced) frame is removed: `renderer_simplified` is the only preset; `renderer_advanced`, `renderer_r2`, `renderer_r4` and `-r2` / `-r4` still parse and run it with one log line (`RenderPresetParseToken`). `renderer_api d3d11|d3d12` picks the API.
- `VERIFY` is enabled only with `DEBUG`. `R_ASSERT` is stripped with `SHIPPING_BUILD`; checks that must survive Shipping need explicit control flow (or `CHECK_OR_EXIT` for fatal errors).
- Swapchain and Lua stay off `PreRenderThread`. `GRHI` draws stay on the primary thread. Only the submit thread submits and presents a D3D12 frame; anything else that reaches the queue (`Flush`, `DrainAll`, `UploadData`, ImGui's own windows) or makes the swap chain waits for it with `NriDevice::Idle` first, and the swap chain is made, resized and destroyed on the primary thread only.

## graphify

This project has a knowledge graph at graphify-out/ with god nodes, community structure, and cross-file relationships.

When the user types `/graphify`, use the installed graphify skill or instructions before doing anything else.

Rules:
- When graphify-out/graph.json exists, run `graphify query "<question>"` before your first grep, search or source-file read for a codebase question, and put this rule in every subagent prompt that explores code. Read raw files only for lines the graph or an earlier read already located. Use `graphify path "<A>" "<B>"` for relationships and `graphify explain "<concept>"` for focused concepts. These return a scoped subgraph, usually much smaller than GRAPH_REPORT.md or raw grep output.
- Dirty graphify-out/ files are expected after hooks or incremental updates; dirty graph files are not a reason to skip graphify. Only skip graphify if the task is about stale or incorrect graph output, or the user explicitly says not to use it.
- If graphify-out/wiki/index.md exists, use it for broad navigation instead of raw source browsing.
- Read graphify-out/GRAPH_REPORT.md only for broad architecture review or when query/path/explain do not surface enough context.
- After modifying code, run `graphify update .` to keep the graph current (AST-only, no API cost).
- A committed graphify-out/graph.json is rewritten whole on every build, so a large diff to it in a pull request is expected. Do not flag it as churn or suggest skipping the graph rebuild or its hooks.
