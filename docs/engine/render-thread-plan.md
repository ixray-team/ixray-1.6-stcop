# Render thread plan

Status (2026-11-25): stages 0 to 3 done for the camera, 4.0 to 4.6 done; the time and the frame counter are still read from `Device` (stage 6 decides them). Items 1 to 7 of stage 4's list and stage 5 (items 8 and 9) are done, and stage 6 as an experimental third value, `r_render_thread pipelined`: the game's frame runs beside the draw of the frame before it. Stage 7 is done but for the menu line. Next: a level change, a reset and a long session in the pipelined mode, a scene where the game's frame is the long one, then the menu. With `r_render_thread on` the primary thread collects the main view and the render thread draws the whole frame, the primary waiting: no gain yet. With `r_render_thread on` the frame's end runs on the render thread while the primary thread waits: no gain yet, about 0.1 ms of hand-over a frame.

## Goal

An optional mode in which the whole renderer frame (`seqRender` and the frame's end) runs on its own thread, one frame behind the game. The single-thread frame stays as it is and stays the default until the mode is proven.

## Decisions

Owner, 2026-11-25:

- **Pipelined, one frame behind.** The render thread draws frame N while the primary thread runs `FrameMove` and the game thread for N+1.
- **D3D11 and D3D12.**
- **Optional**, chosen by the user.

Adopted by default (change them if they are wrong):

- The option is `r_render_thread off|on`, applied on restart (`EApply::Restart`), not part of any preset, with a row in the advanced video menu.
- SDL, window messages, the swap chain's create / resize / destroy and `Device.Reset` stay on the primary thread, as today.
- The in-game ImGui tools keep working: their widgets are built on the primary thread, only their draw data crosses to the render thread.
- The editors (`IsEditorMode`) and the dedicated server never use the mode.
- The loading screen (`g_loading_events`, `LoadDraw`) stays single-threaded: the render thread is idle while a level loads.

## Where the time is

Measured on the minimal preset, `test_scene`, D3D12: primary thread 1.17 ms a frame, of which `FrameMove` 0.23 ms and the render 0.62 ms. The game thread (scheduler, scripts, physics) already overlaps the render. Pipelining can overlap `FrameMove` with the render, so the ceiling is the smaller of the two: about 0.2 ms there. That scene is nearly empty; **stage 0 measures a heavy one before anything is built**, and the plan stops there if the ceiling is not worth one frame of latency.

Stage 0, measured 2026-11-25 on the save `vertver - arrival at the skadovsk` (outdoors, Zaton), high preset, 2560x1440, averages of 200 frames:

| | D3D12 | D3D11 |
| --- | --- | --- |
| primary thread (`on_idle`) | 3.33 ms | 3.32 ms |
| `FrameMove` | 0.54 ms | 0.49 ms |
| render (`OnRender` phase) | 2.19 ms | 2.25 ms |
| of it: the collect (`CollectMain`) | 0.17 ms | 0.15 ms |
| of it: HUD and UI (`RenderUI`) | 0.31 ms | 0.23 ms |
| frame end (`Render: End`) | 0.35 ms | 0.33 ms |
| GPU frame | 3.23 ms | 3.38 ms |

So about 2.5 ms of the primary thread's 3.3 ms can leave it, and what stays (game frame, collect, UI recording) is about 1 ms. The frame there is bound by the GPU at this resolution, so the mode gains frame time only where the CPU is the limit: a lower resolution, a weaker CPU, or a scene with more objects. No save with many NPCs was at hand; that case is still to be measured before stage 6.

## The frame today

`message_loop` (`src/xrEngine/device.cpp`): swap chain resize, `GRHI->BeginFrame`, `PaceFrame`, SDL events, `on_idle`.

`on_idle`: `BeginRender` (ImGui platform frame), `ModelDefferClear`, `PreRenderThread.Run()`, `FrameMove` (`seqFrame`), `GameThread.Run()`, `Begin()`, `seqRender`, `End()` (gamma, the scene's frame end, screenshots, ImGui, present), then both threads are joined and `EndRender` runs the limiter.

What the render reads of the live game while it runs:

- the camera and timing fields of `Device`;
- the spatial database (`q_frustum`, lamp and cascade queries), under its shared lock;
- every visible object's `renderable_Render`, its `XFORM` and its skeleton: `CalculateBones` runs game bone callbacks under `UCalc_Mutex`;
- the HUD and the UI (`HUD().RenderUI`, `RenderActiveItemUI`, `OnRenderPPUI_*`), under `ui_lock`;
- the environment, particles, wallmarks, the bullets' tracers, debug primitives;
- about twenty `seqRender` subscribers in the game, the console, the statistics and the editors' helpers.

Hand-offs that already exist: `ui_lock`, `UCalc_Mutex`, the spatial `db_lock`, `ModelPool::Delete` deferred while `g_bRendering`, `StoreVisualMatrix`, the `*_saved` camera copies. The bullets' two buffers (`bp_update_idx`, `bp_render_idx`) were never swapped and did nothing; stage 7 made them one.

Thread rules today: `g_AppInfo.IsPrimaryThread()` stands for "the thread that may touch the GPU" (texture fills, the skinning pass, the model pool, the event manager). D3D12 submits and presents on `NriSubmitThread`; D3D11 does both in place on the calling thread.

## The frame with the mode on

```
primary thread          render thread
---------------         ----------------
events, FrameMove N+1   (waits for packet N)
build packet N+1        draw packet N: seqRender, End, present
wait: slot free    <--  done N
hand over N+1      -->  draw packet N+1
```

One packet is in flight at most: the primary thread blocks when the render thread still draws the frame before. That bounds latency to one frame and memory to two packets.

The render thread becomes the one thread that touches the GPU for a frame. On D3D11 it owns the immediate context; on D3D12 it records and hands its command buffers to the submit thread as the primary thread does today.

### The frame packet

Everything the render reads is either copied into the packet or guarded for the length of a frame:

| Read today | In the mode |
| --- | --- |
| `Device` camera, time, `dwFrame` | copied into the packet; the renderer reads the packet, never `Device` |
| objects' `XFORM`, visibility, `renderable_Render` | the collect (`CollectMain`, the shadow views' collects) runs on the primary thread at the end of `FrameMove` and fills the packet: the render thread draws lists, it does not walk objects |
| skeletons | posed on the primary thread during the collect (`PoseSkeletons`); the bone matrices the skinning pass reads are copied into the packet |
| HUD, UI, console, statistics, debug primitives | their draws are recorded into the packet as dynamic-geometry batches (`DynamicGeometry::BeginRecord` exists) on the primary thread and replayed |
| environment, weather blend | the blended values the renderer uses are copied |
| particles | already simulated on `PreRenderThread`; their vertex fill moves into the collect |
| ImGui | `ImDrawData` is cloned into the packet |

The collect is the bulk of the primary-side work and is where today's render already spends its CPU, so moving it does not by itself gain time: the gain is the GPU-facing half (culling dispatch, pass recording, D3D11 driver time) leaving the primary thread. Stage 0 measures that split.

### Thread identity

`IsPrimaryThread()` is split in two questions: "is this the thread that owns the window" and "is this the thread that owns the GPU". Every current caller is assigned one of them. With the mode off both answer the same thread.

### What stays on the primary thread and must reach the GPU

- `Device.Reset`, `vid_restart`, swap chain resize: the primary thread stops the render thread at a frame boundary (the same fence `PreRenderThread.Wait()` gives the loader today), does the work, and releases it.
- Texture and model loading that fills GPU resources "only on the primary thread": queued to the render thread's frame start.
- Screenshots, RenderDoc captures, the autotest's readback: requested by the primary thread, done by the render thread at its frame end, answered a frame later.
- ImGui platform windows (multi-viewport): created and destroyed on the primary thread, rendered by the render thread.

## Stages

Each stage leaves the single-thread path untouched and is checked with `util/render_check.py` on both APIs, mode off and on.

0. **Measure.** A heavy scene (many NPCs, combat): `FrameMove`, the collect, and the rest of the render, per API. Decide whether to continue. No code.
1. **Thread identity.** Done. `CAppInfo::IsRenderThread()` answers "may this thread touch the graphics device" (the primary thread until a render thread calls `SetRenderThread`); the texture fill (`GpuResource`) and the skinning pass ask it. Everything else that asks `IsPrimaryThread()` means the game's thread and stays. `r_render_thread` is a stored setting applied on restart; its menu line and text wait for stage 6, as a switch that does nothing must not be shown.
2. **Frame end on the render thread.** Done for the frame itself. `dxRenderDeviceRender::End` builds the tools' widgets and their platform windows on the primary thread (`CImGuiManager::Build`, `UpdatePlatform`), then hands `EndScene` (gamma, the scene's frame end, screenshot and autotest readback, ImGui's draw and its platform windows' draw) and the present to `RenderThread` and waits for each; the autotest's steps run on the primary thread between the two. With the mode off the order is what it was. Checked on `test_scene`, D3D11 and D3D12, mode off and on: same picture (mean difference 0.14 of 255, two runs' noise), the render thread's track shows `GAMMA_APPLY`, `ImGui EndRender` and `Present`. Found by it: D3D11's GPU events were pushed into queries that did not exist when the statistics were switched on in the middle of a frame, which the widgets now do before the frame's end (`GPUEvents_PushEvent` skips a frame that was not begun). Also checked with the mode on, both APIs, 2026-11-25: `r_restart`, `r_resolution` to 1920x1080 and back with `r_restart` (the shots have the new size and the same picture), the `screenshot` command (the file is written) and a RenderDoc capture taken by the autotest (`capture` step, the file is written). **Still not checked, as neither can be scripted here:** ImGui's own windows dragged outside the game window, and exclusive fullscreen on D3D11, where a present from another thread may wait for the window's thread while that thread waits for the present.
3. **Packet: camera.** Done 2026-11-25, in two steps.
   - **3a. Cameras a caller set in the middle of a frame.** `IRender_interface::RenderUI` takes the view and the projection of the icon it draws; the 3D icon widget (`UI3dStatic.cpp`) and the weapon preview (`WeaponCustomize.cpp`) pass theirs and no longer write `Device`. The level inspector's three depth-shifted overlays build their view and projection in local matrices and hand them to `SetCacheXform`. Left alone: the demo recorder's map shot and the cinema tools' near plane, which set the frame's camera before the frame is drawn.
   - **3b. The redirect.** `RenderFrame` (`RFrame`, `RenderFrame.h`) is the renderer's copy of the device's camera: position and axes, view, projection, their product, the HUD's and the frame before's matrices, the saved copies, field of view, aspect, near plane, the camera-cut flag. It is taken in `dxRenderDeviceRender::Begin`, `CRender::Calculate` and `CRender::Render`; 323 reads in 50 files of the renderer moved to it. What draws with a camera of its own changes the copy for as long as it draws: an icon (`IconCamera` in `RenderUI`), the wallmarks, and the HUD (`CHudInitializer`, which also writes the HUD's matrices to `Device`, as the game's code that runs while the HUD is drawn reads them there).
   - **Not moved:** `dwFrame` (174 reads) and the time fields (about 70). The device advances them at the start of `FrameMove`, with no call into the renderer there, and the renderer's own code that runs during the game's frame (particles, lights added by the game) must see the same values as the game: they stay on `Device` until stage 6 assigns each read to the game's frame or to the drawn one.
   - **Known difference:** what the renderer does on `PreRenderThread` before the frame begins (the reflection cube's graphs, rain, particles' culling) now sees the camera of the frame before, where it used to race with the game writing this frame's.
   - Checked on D3D12 and D3D11, mode off and on: `test_scene_static` at the minimal preset (world, through the sights, inventory icons) and `shadow_problem` at high, against shots from before the change: the lamp scene differs by 0.01 of 255, the others by what moving grass gives. Not checked: the editors, the weapon preview window, the level inspector's overlays.
4. **The frame on the render thread, then its collect back to the primary.** Changed 2026-11-25: the stage starts from the other end. Hoisting every read of the game out of the draw before any of it moves is months of blind work; instead the whole render phase goes to the render thread at once, with the primary thread waiting, and the parts that read the game are then taken back to the primary thread one at a time, each a step that can be run and compared.
   - **4.0. The whole phase over.** `CRenderDevice::on_idle` runs `seqRender` through `IRenderDeviceRender::RunFrame`, which is the render thread with `r_render_thread on` and the caller otherwise. While the primary thread waits the render thread stands for it: `CAppInfo::SetPrimaryDelegate` makes `IsPrimaryThread()` true there, as only one of the two runs, so the game's code inside the phase behaves as on the primary thread (the event manager, the object list's crow list, the loaders' checks).
   - **4.0 and 4.1, done 2026-11-25.** 4.0 as above. 4.1 is item 1 of the list below: `CRender::CollectFrame` runs on the primary thread, called by `RunFrame` before it hands the scene over, and does what `Render` did first for a level: `Calculate`, the sun's collect where the pre-render stage had not foreseen it, and `CollectForwardLevel` (the occlusion map, the far cascades' start, `render_main`, `GpuScene::BuildObjects`, the lights' order). `Render` and `Calculate` find the frame collected (`CollectedFrame`) and skip it; with the mode off, in the editors, under the menu and on the frame after a reset `Render` collects as before. The rain's map is now drawn after the collect instead of before it. Checked on `test_scene_static` (world, through the sights, inventory) and `shadow_problem`, D3D12 and D3D11, mode off and on: the lamp scene differs by 0.01 of 255 from the shot before the stage, and on D3D11 mode off against on differs by 0.01 to 0.04 where two runs of one mode differ by 0.03 to 0.04. Profile, mode on: `CollectFrame` 0.10 ms on the primary thread, the render phase 1.4 ms on the render thread. Found by it: the autotest's key and button steps ran the game's scripts beside the game thread, as they were given at the frame's end; they are given at the next frame's start now (`Autotest::FrameBegin`).
   - **4.2, done 2026-11-25.** Items 2 and 3 of the list below, which one collect serves. `CRender::CollectForwardLampShadows` runs at the end of `CollectForwardLevel`: the lamps' parts and their places in the atlas, the cache's choice for each part (`PrepareSpotShadow`), the spatial queries of the lamps and of the drawn cascades, each part's portals and hits, then `CollectShadowCasters` (`GpuScene::CollectCasters`: `renderable_Render` and the posing of what only the shadow views see, for the lamps and the cascades at once; a frame without shadowed lamps calls it for the cascades alone). `RenderForwardLampShadows` keeps what reaches the GPU: the cache's depth builds (`BuildSpotShadow`, which `PrepareSpotShadow` used to do in place), the table's upload (`UploadModels`, no longer inside the collect), the bakes, the pages. A part whose depth is built this frame collects as one drawn whole and reads the depth from the same frame on. Checked on `shadow_problem` at high, D3D12 and D3D11, mode off and on: against the shot from before the stage 0.01 of 255 (D3D12 both modes, D3D11 off); two D3D11 runs of one mode differ by up to 0.5 there (a flickering lamp), which covers D3D11 on (0.27). Profile, mode on: `CollectFrame` 0.95 ms on the primary thread in the lamp scene (`LampCollect` 0.20, `SceneCollectCasters` 0.33), `ForwardLampShadows` 0.87 ms on the render thread.
   - **Left for stage 6 by 4.2:** the cache's entries (`SpotShadowPart::Entry`) are chosen by the collect and drawn, baked and freed by the draw; `BeginSpotShadowCacheFrame` and `PrepareSpotShadow` may let a depth go. Two frames at once need the entries' state split by side. The graph was shared too, until 2026-11-25: the collect now walks the parts' portals and classifies the cache's casters in `GraphLampCollect`, and `GraphPass` is the draw's alone.
   - **Found by it, fixed 2026-11-25:** the level's load task (`IGame_Level::Load` on the task pool) runs the renderer's `level_Load` while the primary thread draws the loading screen, and both reached the graphics device: on D3D11 they shared the one replay command buffer (`NriPass.cpp`), and a run in seven crashed in `PipelineLayoutD3D11::SetDescriptorSet`; the mode off had the same two threads. Now the two take turns (`g_RenderLoadLock`, `device.cpp`): the renderer's part of the load holds the lock, a frame of `message_loop` holds it from before `GRHI->BeginFrame` to its end, and a frame that finds the load waiting or at work is not drawn (`LoadDraw` returns, the loading events still run). Eleven D3D11 loads and three D3D12 ones after it, both modes: no crash, the load takes what it took (4.9 to 5.1 s on `shadow_problem`).
   - **4.3, done 2026-11-25: nothing of the game's objects to move.** Item 4 of the list below. A face's graph is built before the frame, on the pre-render stage, from the portals and the level's statics alone (`R_dsgraph_structure::Build`); what moves comes to a face from the frame's table of models (`GpuScene::CullFace`), which the main view's collect fills on the primary thread since 4.1. The one read of the game was the weather's fog distance the faces are cleared with: it is taken with the far plane when the faces are chosen (`begin_reflection_collect`, on the primary thread before `FrameMove`), so it is the frame before's, as the far plane already was. Checked on `test_scene_static` at high with `r_reflection_quality high` (the cube is not drawn below it), D3D12 and D3D11, mode off and on: the cube's pass is in the profile on the render thread (0.25 to 0.31 ms), mode off against on differs by 0.16 to 0.33 of 255 where two runs with the mode off differ by 0.23 (grass, and the faces drawn in turn).
   - **4.4, done 2026-11-25.** Item 5 of the list below. A graph's draw handed an object its light through `CRender::apply_object`, which smoothed the object's tracker there and, at an object's first sight, ran its whole update: rays through the level's collision and a spatial query, on the thread that draws. The smoothing is now the collect's: `SceneCollector::set_Object` does it for every object the collect calls (the HUD's owner with them), `render_main` for the one who looks, and `BuildObjects` as before for the models' records; `apply_object` only reads. Left: the tracker's getters still smooth one that was not smoothed this frame (the blend alone, no rays), which reaches an object only the shadow views called, whose light no depth pass uses; and the draw still follows the packet's pointer to the game's object for its tracker (item 10). Checked on `test_scene_static` at high, D3D12 and D3D11, mode off and on: the weapon in the hands is lit as before, mode off against on differs by 0.04 of 255 on D3D11 and 0.32 on D3D12 (two D3D12 runs of one mode differed by 0.23 in the stage before; no shot from before this stage was kept to compare with).
   - **4.5, done 2026-11-25: the weather as a copy.** Item 6 of the list below. The environment's draws did no work of the game's at draw time (the flares' and the rain's rays are traced in the game's frame); they read the state the game blends, and so did some thirty places of the renderer. Two copies now stand between:
     - **The blended weather.** `RenderFrame` (`RFrame.Env()`, `Wetness`, `WindStrength`, `WindBlast`) holds a copy of the environment's mixer with its sky textures, taken in `dxRenderDeviceRender::Begin` after the game's frame (`SyncWeather`) and released with the device. Every read of `CurrentEnv` in the renderer goes to it (22 files); the one left is the editors' check that a weather is loaded. Before the first frame the accessor answers with the environment's own.
     - **The effects.** `dxLensFlareRender`, `dxThunderboltRender` and `dxRainRender` each keep the state a frame draws: `Take(owner)` copies it in `CRender::TakeWeatherEffects`, called by `CollectForwardLevel` after the view's frustum is made (the rain's drops are culled by it, and their lists are built and copied there under the rain's lock), and the static `Draw` reads nothing else. The frame calls those and `CRender::DrawSky` (the environment's renderer, with the clouds' check on the copy) in place of `CEnvironment::RenderSky`, `RenderClouds`, `RenderFlares` and `RenderLast`; the interface's `Render(owner)` still works for a caller outside the frame, as take and draw at once.
     - **Left:** what the renderer does on `PreRenderThread` reads the copy while `Begin` may be refreshing it (a float each: the particles' fog distance, the grass's wind), where it used to read the mixer while the game blended it; the flare's and the bolt's descriptors are held by pointer (item 10).
     - Checked on `test_scene_static` at high, D3D12 and D3D11, mode off and on: against shots from before the stage 0.00 to 0.39 of 255 (two runs of one build differ by up to 0.33 there), and with `set_weather default_thunder` the storm's sky, fog and rain are drawn in all four. Not seen in a shot: the sun's flare and a thunderbolt.
   - **4.6, done 2026-11-25.** Item 7 of the list below. The lens flares' rays were already traced in the game's frame (`CLensFlare::OnFrame`). The glows were chosen by the collect (`CGlowManager::add`, from `GpuScene::CollectObjects`) but tested at draw time: up to `r1_glows_per_frame` rays a frame through the level's collision, ignoring the one who looks. `CGlowManager::Collect` does that from `CollectForwardLevel`, after `TakeWeatherEffects`; `Render` only sorts, builds the sprites and draws. Left: the selected glows are the game's objects, read by pointer when drawn (item 10). With it the HUD's raindrop time and amount joined the weather's copy (`RFrame.HudRainTime`, `HudRainAmount`). Checked with the lamp graph's change on `shadow_problem` at high, D3D12 and D3D11, mode off and on: 0.01 of 255 against the shot from before stage 4.2 in three runs and 0.52 in one (the flickering lamp's other state), the lamp's glow drawn; `test_scene_static` with the mode on 0.26 and 0.28 against the shots from before 4.5 (run noise).
   - **4.1 and on: what reads the game inside the phase,** to be taken back to the primary thread before stage 6 lets the two run at once. In the order the frame meets them:
     1. the main view's collect (`render_main`, `GpuScene::BuildObjects`): the spatial query, the light tracker's updates, `renderable_Render` of every visible object, the skeletons' posing with the game's bone callbacks, the wallmarks' calculation, the HUD's `Render_Last`;
     2. the lamps' shadow views (`RenderForwardLampShadows`): a spatial query and `renderable_Render` of the casters of each lamp;
     3. the sun cascades' moving casters;
     4. the reflection cube's faces, where they take moving objects;
     5. the light trackers updated while something is drawn (`LightTrack.cpp`);
     6. the environment's sky, clouds, flares, rain and thunderbolt (`Environment().Render*`), which read the weather the game blends;
     7. the glows and the lens flares, which trace rays through the level's collision;
     8. the HUD item's interface (`RenderActiveItemUI`) and the frame's `OnRenderPPUI_main` (the bullets' tracers, the HUD's world primitives, the main menu);
     9. the other subscribers of `seqRender`: the level's `OnRender` (the game's and the world-space interface, `HUD().RenderUI`, debug walks of the object list), the console, the statistics, the cursor, the cell items, the tutorial, the actor, the helicopter, the demo recorder;
     10. what the game may destroy under the frame: visuals (`ModelPool::Delete` waits while `g_bRendering`), lights and glows the frame holds by pointer.
   Items 8 and 9 are stage 5.
5. **The game's own draws recorded on its thread.** Done 2026-11-25 as one recorder for every place (the owner's choice over doing it in steps). `DrawRecorder` (`DrawRecorder.h`) stands between the game's draw code and the command list: pass, scissor, world and view matrices, cull mode, depth, alpha reference, vertices (kept in the list's own bytes), draw, and `Call` for what has no command of its own. With no list open a call runs at once, so the mode off is what it was. With `r_render_thread on`:
   - `dxRenderDeviceRender::RunFrame` collects, then runs the scene's phase (`seqRender`) on the game's thread with a list open (`CRender::FrameDraws`), and hands the render thread one job: play the list;
   - the game asks for the scene from inside that phase (`CLevel::OnRender` calls `Render`): while a list is open `CRender::Render` is `RecordRender`, which keeps the call as an entry of the list and has the game make, each into a list of its own (`SceneDraws`), the draws the scene's frame calls it for: the item's interface into the PDA's target, the active item's 3D interface (with the HUD's camera on the device meanwhile), and the two post-process places; the menu's path records its two. The scene's draw plays each list where it used to call the game (`PlayedScene`);
   - through the recorder now: the interface (`dxUIRender`, all 237 call sites unchanged), the fonts (`dxFontRender`), the debug lines and the debug renderer's state calls (`dxDebugRender`), `SetCacheXform`; as kept calls: `rmNear` / `rmFar` / `rmNormal`, `SetCacheXformOld`, a 3D icon (`CRender::RenderUI` keeps its visuals, camera, tint and scissor; `DrawIcon` draws), and the renderer's own subscribers of the phase (the occlusion map's debug view, the statistics graph);
   - a draw of the game's thread that goes past the recorder while a list is open is told once in the log with its stack (`DynamicGeometry::Lock`).
   - **Left:** `CDrawUtilities` (the statistics' debug shapes with `DEBUG_DRAW`) still draws at once; the tutorial's video item reads the bound texture; `Render` asks the game again which path the frame takes when it is played, where it should use what `RecordRender` decided; `CHudInitializer` writes the device's camera from the render thread while it plays the active item's list; the statistics graph and the recorded icon read the game's objects when played (item 10); the frame counter drawn after the phase (`pFPSCounter`) and the loading screen draw on the game's thread at once, as before.
   - Checked on `test_scene_static` at high, mode off against on: D3D12 world, through the sights (the scope's reticle is the active item's list) and the inventory with its 3D icons 0.13 to 0.19 of 255; D3D11 world, the PDA's map and the main menu over the game 0.00 to 0.06; no draw reported past the recorder. Profile, mode on: the phase's game code 0.06 ms on the primary thread, the scene 1.16 ms on the render thread. Not checked: the bullets' tracers, the console, the tutorial, a dragged item, the debug tools (level inspector, statistics), the editors.
6. **Go pipelined.** Done 2026-11-25 as a third value of the setting, `r_render_thread pipelined`, experimental; `on` is still the mode that waits. **What overlaps is the game's frame and the draw of the frame before it, not the collect:** the game's thread hands a recorded frame over at `End` and goes on; at the next `Begin` it waits for the render thread, and everything of the renderer's that this thread does (the frame's begin, the collect, the recording) happens between that wait and `End`, with the render thread idle. So what the collect writes and the draw reads needs no second copy. Collect beside draw is not in this plan.
   - **6.1. The frame's number and time.** `RenderFrame` copies `dwFrame`, `fTimeGlobal`, `fTimeDelta`, `dwTimeGlobal`, `dwTimeDelta`, `dwTimeContinual` and `dwPrecacheFrame` with the camera; 155 reads of the collect and the draw in 34 files take them from it. Left on the device: the game's frame and the pre-render stage (11), the editors (13), and the marks several phases share (56: light trackers, skeletons, the occlusion map, `light::xform_calc`, a particle effect's cache), which still compare with the device's counter.
   - **6.2. What a frame holds.** A dynamic packet carries its object's light tracker (`DynamicPacket::Track`), asked when the packet is made: `apply_object` no longer goes through the game's object, and the one who looks is asked once a frame (`CRender::ViewerTrack`). A recorded list holds a reference to every shader element and geometry it names. With the mode pipelined, models are always queued for deletion, light trackers are queued (`TrackersToFree`), and the queues (models, deferred models, lights, trackers) are emptied where the frames meet (`CRender::FrameSync`) instead of in the game's frame.
   - **6.3. The hand-over.** `RenderThread` keeps a queue (`Post`, `Wait`). In `dxRenderDeviceRender::Begin` the game's thread waits for it, runs the owed autotest step, frees the queues, begins the device's frame (`GRHI->BeginFrame` and the pacer, which `message_loop` leaves to it in this mode), takes the camera, time and weather, chooses the cube's faces and builds their graphs (they wait for the frames to meet instead of running before the game's frame), runs the frame's own begin, and opens the frame's list: every draw of this thread until `End` is recorded, the scene's phase, the frame counter and the loading screen alike. `End` builds the tools' widgets, closes the list and posts two jobs: play the list and end the scene; present. Before anything else takes the device the render thread is waited for: a reset, a level's load and unload, the swap chain's resize, the gamma, the device's destruction.
   - **The skinned models' pose** was the one thing the first run showed wrong (the HUD's hands and weapon torn apart): the skinning pass posed skeletons and read their bones when it drew. With the mode pipelined every skeleton the collect meets is marked (`SceneCollector::add_Dynamic`), posed and copied at the collect's end (`SkinningPass::Capture`: the bones' render transforms of this frame and the last, the old world matrix), and the pass, the rigid parts and the model table read the copy.
   - **Also changed for it:** `Render` played from a recording takes its path (level, menu, nothing) from what `RecordRender` asked the game, and does not take the device's camera again; `CHudInitializer` leaves the device's camera alone.
   - **Left, known:** the marks shared by several phases (above); a light's fields, a glow's and a particle effect's state are read by the draw while the game writes them; what the game creates in the renderer during its frame (models, shaders, textures, lights, wallmarks) runs beside the draw with the locks it had; the tools' own windows are drawn by the render thread while the window's thread polls their events; the autotest's step runs a frame later, so its profile samples are off by one frame; the statistics graph, a recorded icon and the flare's and bolt's descriptors are the game's objects read when played; `CDrawUtilities` and the tutorial's video still go past the recorder; the frame pacer waits after the game's frame, which costs input latency (stage 7).
   - Checked, D3D12 and D3D11: `shadow_problem` at high 0.01 to 0.02 of 255 against the reference and 0.10 to 0.11 after 1500 more frames; `test_scene_static` world, through the sights, inventory, and a forced storm drawn right (0.7 to 2.7 against the waiting mode: the grass sways a frame apart). Profile, D3D12: the frame's end on the game's thread 0.04 ms where the waiting mode takes 0.46, the wait at `Begin` 2.2 ms (5.1 on D3D11), the whole frame as long as before: this scene's game frame is 0.3 ms, so there is nothing to win in it. Not checked: a heavy scene (where the gain should be), a level change, a reset, the menu, the tools, long sessions.
   - **6.4. The leftovers and an audit, 2026-11-25.** An agent read every way the game's side reaches the renderer between `End` and the next `Begin`, and found fourteen that could break a frame in flight; all are closed:
     - the game's next frame set its camera on the command list the frame in flight draws with, every frame (`SetCacheXform` from `CalculateTransforms`): on the game's thread with nothing recording the call is dropped, and the frame takes its camera from `RFrame` when it begins (`BeginDraw`);
     - the sky's and the ambient cube's texture aliases were swapped by the game's frame every frame (`dxEnvironmentRender::OnFrame`): the game's call is skipped and `dxEnvironmentRender::Sync` does it where the frames meet;
     - a point light's six shadow parts were freed at once when its shadow was switched off (a muzzle flash does it): they join the queue of lights let go, and the light table and the volumetrics skip a light whose parts are not all there; a light's projector texture is held until the frames meet (`TexturesToFree`);
     - the render thread is waited for before: a screenshot, a vector icon's upload into its atlas, the detail cache's rebuild from the console, the mip bias, a colour map's change, the static wallmarks' clearing, a change of assets, a shader recompile, a resource dump;
     - the render thread is the device's thread from its first wake on, so a texture the game's thread makes just after `End` is not filled beside the frame (it waits for the render thread, as a load task's does).
     Also: the draw reads a light tracker as the collect left it (`drawn_hemi` and its like) instead of smoothing it against the device's frame; `light::xform_calc` marks with the renderer's frame; the tutorial's video takes the recorded pass's texture; `CDrawUtilities` draws through the recorder (a fan becomes the list of its triangles, the indexed unit meshes are a kept call).
   - **Still left:** a light's position and colour, a glow's and a particle effect's state are plain values read while the game writes them (a frame's worth of mix at worst); the occlusion map's, the skeletons' and a particle effect's marks compare with the device's counter (the draw no longer poses a skeleton the collect took; the others cost a repeated test); the tools' own windows; the autotest's step a frame late; the statistics graph, a recorded icon's visuals and the flare's and bolt's descriptors are the game's objects read when played; the frame pacer after the game's frame (stage 7).
   - Checked after it, D3D12 and D3D11: `off` and `on` 0.01 of 255 on `shadow_problem` and unchanged elsewhere; `pipelined` 0.01 to 0.02 on `shadow_problem`, the world, the inventory with its icons and a forced storm drawn right, the menu 0.01 to 0.09.
7. **Polish.** Done 2026-11-25, but for the menu.
   - **Pacing.** With the frames one behind the game, a game's frame started as soon as the last one is handed over ends long before the two can meet, and its input is older by that wait when its own frame is drawn. `FramePacer::Hold` (what `Wait` does in this mode, called by `message_loop` before the input is read as in the other modes; only `GRHI->BeginFrame` is left to `Begin`) keeps the game's frame back so that it ends as the frame in flight does. It needs two lengths, both measured where the frames meet (`FramePacer::Meet`, which also keeps `Wait`'s hold on the GPU's frames by `r_frame_queue`): how long the frame in flight takes from its hand-over (`HandOver`, in `End`), and how long the game's frame takes from the input to `Begin`. When the game's thread is not kept waiting at `Begin` the first length is only known not to be longer than it was, so it shrinks by a hundredth a frame until the wait shows it again. A game's frame longer than the frame in flight is never held.
   - **The limiter** (`rs_fps_limit`, `CRenderDevice::EndRender`) sleeps on the game's thread after the hand-over, which holds the game's frames to the rate as before; the pacer counts that sleep as time the frame in flight has already had.
   - **Measured** on `shadow_problem` at high, 2560x1440, by the pacer's own latency (`render_memory_stats`: from the input to the frame's end on the GPU, mean of 128 frames) and the profiler's frame:

     | | D3D12 `off` | D3D12 `on` | D3D12 `pipelined` | D3D11 `off` | D3D11 `pipelined` |
     | --- | --- | --- | --- | --- | --- |
     | latency, ms | 9.8 | 10.3 | 10.2 | 10.4 | 11.6 |
     | frame on the game's thread, ms | 5.62 | 6.13 | 4.75 | 7.04 | 5.50 |

     The pipelined frame is 15 % shorter on D3D12 and 22 % on D3D11 at the latency of the mode off (the hold is 2.1 ms of it). The mode that waits is slower than off, as it was: it is the step the pipelined one was built on, not a mode to play with. Not measured: the latency without the hold, and a scene where the game's frame is the long one.
   - **The bullets' two tracer buffers** (`bp_update_idx`, `bp_render_idx`) were never swapped; they are one now. The tracers are drawn by the game's own code, recorded on its thread, so there is nothing for a second buffer to do.
   - **Not done: the menu.** `r_render_thread` has no line in the video settings yet. `pipelined` has not been through a level change, a reset or a long session, and a setting a player can pick should have.

Stages 1 to 5 are useful on their own (the renderer reading a packet instead of live state is also what the render-graph-manager plan asks for) and can ship with the mode still hidden.

## Stage 5 inventory (2026-11-25)

Read from the sources, not run. What the render phase still runs of the game's once items 1 to 7 are done.

**Where the game draws inside the frame** (`CRender::Render`, `ixr_R_render.cpp`):

1. before the level: `GActorInterface->RenderItemUI()` into `rt_ui_pda` (the PDA, the scope's interface);
2. in the HUD's sorted pass: `g_hud->RenderActiveItemUI()` under `CHudInitializer` (the item's 3D interface, the watch);
3. `OnRenderPPUI_main` after the glows (bullet tracers, the HUD's world primitives, the level inspector, the menu's windows);
4. `OnRenderPPUI_PP` inside the distortion pass;
5. the menu's own path (`render_menu`), when the menu covers the scene.

**After the scene**, the subscribers of `seqRender` in order: the level (`CLevel::OnRender`: the scene itself, then the game's `OnRender`, the world-space interface, `HUD().RenderUI`, and with `DEBUG_DRAW` the debug walks of the object list), the actor and the demo recorder, the statistics, the occlusion map's and the draw utilities' debug output, the dragged cell item, the combo box's list, the main menu, the tutorial, the cursor, the console, the loading screen.

**What they draw through:** `UIRender` (237 call sites: 157 in `xrGame`, 67 in `xrUI`, 10 in the console), the fonts (170 output calls; strings are already kept and drawn at a flush), the debug renderer (lines already kept and drawn at a flush), 3D icons (`IRender_interface::RenderUI`). `UIRender` draws at once: `StartPrimitive` hands out a pointer into the frame's dynamic geometry stream, `FlushPrimitive` draws it, and shader, scissor, transform, cull mode and depth are set on the command list as they are called.

**Why it is not a plain move to the collect:**

- drawing is not a read: `CActor::RenderItemUI` moves the cursor and resets the PDA, the in-game window shows and hides widgets as it draws, the cursor and the tutorial keep once-a-frame state. The windows' `Draw` must stay on the game's thread;
- the draws ask the command list for what is bound: the size of the texture just set (7 places), the scissor, the targets (`CRender::RenderUI` saves and restores them);
- they must land at five places of the frame, between its passes, into three different targets;
- `CHudInitializer`, the level inspector and `rmNear` write the frame's camera while they draw.

**Ways to do it:**

1. a recorder behind `UIRender`, the fonts and the debug renderer: the game's thread draws into a list a place of the frame, the render thread replays each list at its place. About nine kinds of command for the 2D interface, three more for targets, icons and the HUD's camera;
2. the stream's own recording (`DynamicGeometry::BeginRecord`, used for particles) for the same lists: less new code, but it resolves a draw against a command list's state when it is recorded, which the game's thread does not have;
3. in steps: first what is already kept before it is drawn (debug lines, font strings, the world primitives, the tracers) and what follows the scene (console, cursor, statistics, tutorial, menu); the four places inside the frame stay as they are until the first step has proved the recorder;
4. keep the drawing on the render thread behind a copy of the interface's state: the 74 `Draw` functions and the actor's code would have to stop changing the game as they draw. Not realistic.

## Stage 6 inventory (2026-11-25)

Read from the sources, not run.

**Time and the frame counter in the renderer:** 242 reads in 51 files. By when the code runs:

| When | Reads | What follows |
| --- | --- | --- |
| the draw | about 121 | move to a frame number and a time in `RenderFrame`, set from the packet |
| the collect | about 36 | may stay on the game's, if the collect stamps the packet with it |
| several phases | about 60 | each needs a decision, see below |
| the pre-render stage | 7 | already races `FrameMove`, which advances the counter after the stage starts |
| the game's frame | 3 | stay |
| editors only | 11 | stay |

**Once-a-frame marks touched from more than one phase** (they break once the two threads disagree on the frame):

- written by the collect, read by the draw: `CollectedFrame`, `SunCollectFrame`, `RainCollectFrame`, `GpuScene::ModelFrame` (eight reads in the draw), `CountsOf`;
- written by the draw, read by the collect: a cascade's `CaptureFrame`; the spot shadow cache's entries (4.2's leftover);
- the light trackers (`CROS_impl`): the game's scripts and the rain, the collect, and the draw's getters;
- the skeletons (`UCalc_Time`, the visibility box, the held pose, the wallmarks): the game's about forty `CalculateBones` calls and the collect's posing;
- `light::xform_calc` (collect and draw), the occlusion map's marks (pre-render, collect, draw), a particle effect's cache mark (pre-render and draw).

**What the frame holds by pointer and what frees it:**

| Held | Freed by | The draw reads it |
| --- | --- | --- |
| visuals in the graphs' packets | `CModelPool::Delete`: at once, or queued while `g_bRendering` and freed by `CRender::OnFrame` at the start of the next `FrameMove`; pooled ones are reused | every graph draw |
| the game's objects (`IRenderable*`) in the packets | the object list's destroy loop inside `FrameMove` | `apply_object`, for the light tracker |
| light trackers | `ros_destroy` in `~IRenderable`, at once | `apply_object` |
| lights (`SurfaceLights`, `LampParts`) | queued by `light::destroy`, freed by `CRender::OnFrame` at the start of `FrameMove`, with no lock on the queue | the lamps' maps, the light table's upload |
| glows | counted references (atomic); the game rewrites their position | `CGlowManager::Render` |
| static wallmarks | inside the draw itself (`CollectDecals`, by time) | the same function |
| particles | `UpdatePlayDestroyParticles` before the pre-render stage; `~CParticlesObject` asserts no frame is drawn | `CParticleEffect::Render`, under the effect's locks |
| the flare's and the bolt's descriptors | the environment, at unload (not confirmed) | `dxLensFlareRender`, `dxThunderboltRender` |
| shaders and textures | counted references (atomic), freed by whichever thread drops the last | everywhere |

**Hardest:** the lights' queue and the trackers freed at once while the draw reads them; visual deletion keyed to `g_bRendering`, which stops meaning "a frame is being drawn"; the marks shared by three or four phases; the pre-render stage of frame N+1 beside the draw of frame N.

## Risks

- **A read the audit missed.** Anything the render thread still reads live is a race that shows as a rare crash. Stage 4 and 5 run with thread-sanitizer-style asserts: the render thread's entry points assert they are not handed live objects.
- **D3D11.** One mistaken primary-thread touch of the immediate context corrupts the frame or deadlocks the driver. Stage 2 exists to find these while nothing else has moved.
- **Latency.** One frame more from input to picture. Measured in stage 7; the mode is optional for this reason.
- **Lua and game callbacks.** Bone callbacks and UI code are game code; in the mode they run during the collect on the primary thread, never on the render thread.
- **Cost of the copy.** Copying bone matrices and draw lists every frame may eat the gain on light scenes. Stage 0's numbers bound it.

## Not in this plan

Recording passes on the task pool (`r_parallel_views`) is independent and stays as it is. Moving `FrameMove` to the game thread instead was considered and not chosen.
