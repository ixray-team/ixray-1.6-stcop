# Startup and frame

Entry: `CApplication::Run` in `src/xrPlay/Application.cpp`.

```text
BeginPlay
InitEngine
MigrateToGameWindow
EngineLoopAndDestroy → Device.Run
EndPlay → Core._destroy
```

After `BeginPlay`, `Run` calls `SteamWorks.BeginPlay()`. There is no splash thread: the startup screen is drawn in the main window by `CRenderDevice::SetStartupStatus` / `DrawStartupFrame` (`src/xrEngine/Device_startup_frame.cpp`). Non-`DEBUG_DRAW` Windows builds take a single-instance mutex.

## BeginPlay

1. `SDL_Init` (audio, video, gamepad, events)
2. `Debug._initialize`
3. `EnumerateDisplayModes` (fallback 1024×768)
4. `g_AppInfo.Window`: created with the video mode size and shown immediately, except `-autotest`, where it is `SDL_WINDOW_HIDDEN`; the flag is replaced by `SDL_WINDOW_VULKAN` off Windows
5. `EngineLoadStage1(CommandLine)`

## InitEngine

Stages 1–5 are in `src/xrEngine/x_ray.cpp`. Stages between them are in `CApplication::InitEngine`. `Device.Run` is `EngineLoopAndDestroy`, after stage 5 returns.

| Order | Call | Effect |
| --- | --- | --- |
| 1 | `EngineLoadStage1` | `Core._initialize("IXRay")`, `InitSettings` |
| 2 | `EngineLoadStage2` | `Device`, `Engine.Initialize`, `CInput`, string table |
| 2b | `CreateRendererList` | checks that the renderer library is present beside the executable |
| 2c | `new CConsole` | before user script |
| 3 | `EngineLoadStage3` | `Console->Initialize`; select config filename (`user.ltx`, `user_dedicated.ltx`, or `-ltx`) |
| 3b | `ConfigureRenderer` | feature switches read from the config before DLL load (there is no `renderer` command: the one renderer always loads) |
| 3c | `Engine.External.Initialize` | selected renderer DLL (R4/R2/R1; DS0 for dedicated) + `xrGame` |
| 4 | `EngineLoadStage4` | see below |
| 4b | `LoadCustomSettings` | execute `ixray_settings/default_settings*.ltx` |
| 5 | `EngineLoadStage5` | `pApp`, `g_pGamePersistent`, spatial DBs |

`InitSettings`: `$game_config$/system.ltx` → `pSettings`, `game.ltx` → `pGameIni`. Missing file aborts. FS root: `-fsltx` or `fsgame.ltx`.

Stage 4 order: `InitSound1` (`-nosound`) → `execUserScript` (`default_controls`, config file) → `InitSound2` → `-start` / `-load` → `CFontManager` → `Device.InitRenderDevice(Engine.External.GetAPI())` (creates `GRHI`; D3D11 for R4, D3D9 otherwise) → `Device.Create`.

For the game, the `r_resolution` list is enumerated on the first main-thread request for options or console suggestions, not while creating the render device. Reading `r_resolution` from the configuration does not need that list. The editor retains its existing eager enumeration. An early request before the render interface exists, or a first request from another thread, receives an empty list; a later main-thread request retries. Once populated, the list remains cached until render-device destruction, so display changes after that point are not reflected until restart.

Post-build check (Debug, then Debug and RelWithDebInfo): compare `EngineLoadStage4`, `InitRenderDevice` and time to the first main-menu frame in cold and warm `startup.opt` captures. Confirm mode enumeration no longer occurs inside game render-device initialization; driver initialization may move work between calls. Open graphics settings and console `r_resolution` suggestions, verify the supported resolutions and current selection, change resolution and apply `r_restart`, then restart and confirm the saved value. Check windowed and fullscreen modes, multiple displays, and exit without opening settings. The first settings request may still incur enumeration latency. Check the editor separately and load a level to verify rendering.

Stage 5:

```text
pApp = new CEngineApp
g_pGamePersistent = NEW_INSTANCE(CLSID_GAME_PERSISTANT)
g_SpatialSpace / g_SpatialSpacePhysic = new ISpatial_DB
Device.m_pRender->PostCreate()
```

## Loop

`Device.Run`: reset timers, `seqAppStart`, `message_loop`, `seqAppEnd`.

```text
while !quiting:
    IsDrawn = !Device.Window.IsMinimized
    if IsDrawn:
        ResizeSwapChain        # only if the window's size, r_vsync or r_latency changed
        GRHI->BeginFrame()
        PaceFrame()            # renderer: holds until the GPU is r_latency frames behind (normal 2, low 1)
    SDL_PollEvent → on_event   # sets quiting on close or SDL_EVENT_QUIT
    activation check           # focus (main window or a tool window) and minimise, r_background_render
    on_idle(IsDrawn)
```

Before the loop `Device.ReadWindowState` reads the main window once (the start screen consumed its events). Every window event of the main window reads it again into `Device.Window`: pixel size, display, display scale, minimised, focused, occluded, fullscreen. `SDL_EVENT_QUIT` carries no window id and is taken before the main-window filter. The windowed window is resizable by its frame. No event resizes anything itself: at the top of the next loop turn, before `GRHI->BeginFrame`, the swap chain is made again if the window's pixel size, `r_vsync` or `r_latency` differs from what it was made with (at most once a frame, never while minimised), and that frame's view follows the new size. Targets, shaders and the interface are not made again by it. A windowed window whose size differs from the targets and has rested for about 20 frames with the mouse released becomes `r_resolution`, and a reset makes the targets and the interface for it (`on_idle`); a borderless one never does. `r_restart` sizes a windowed window of another size to `r_resolution`. See [rendering.md](rendering.md#view-rectangle) for the sizes.

Input events pass the window filter from the main window and from the OS window that shows the game's picture (the Game window's own when it is out of the main window); window events only from the main window. The game takes input, mouse grab and relative mode in the window that shows its picture; focus in any other of its windows gives the input to the tools. Activation: the game is active while `r_background_render` is on (default), or while the main window or one of its ImGui windows (the Game window's included) has the keyboard focus and the main window is not minimised; on a change `OnWM_Activate` raises `seqAppActivate` / `seqAppDeactivate`. The dedicated server is always active. The mouse leaving the window only shows the system cursor (off Windows); it no longer deactivates the game.

A minimised window draws nothing: `BeginFrame`, the pacer, `Begin` / `seqRender` / `End` (Present) are skipped and each such frame sleeps 10 ms. The simulation keeps running when the game stays active (`r_background_render` on); with it off, minimising deactivates the game as before. A level still loading keeps drawing its load screen.

`on_idle` (`device.cpp`):

| Condition | Body |
| --- | --- |
| `!b_is_Ready` | sleep 100 ms |
| `g_loading_events` non-empty | run front; pop if it returns true; `pApp->LoadDraw()`; return |
| else | particles, `ModelDefferClear`, due `m_time_callbacks`, `UpdatePlayerHud` |
| | `secondary_tasks.run(&XRay::Engine::PreRenderThread)` + primary `FrameMove()` |
| | `secondary_tasks.run(&XRay::Engine::GameThread)` |
| | active client, window not minimised: `Begin`, `seqRender`, `End` (Present, then the frame pacer's fence signal) |
| | join `PreRenderThread` and `GameThread`, `EndRender` (`r_fps_limit`); inactive: sleep 1 ms, minimised: 10 ms |

`FrameMove`: `dwFrame++`, smooth `fTimeDelta` (clamp `EPS_S`…`0.1`), `seqFrame`. Paused: `fTimeDelta = 0` then still clamped. Dedicated server ignores `Pause`.

`g_loading_events` consumers include `CLevel::net_start1`…`net_start6`. `net_start4` pops itself, `push_front`s `net_start_client1`…`6`, returns false so `on_idle` does not pop the new front that frame.

## Threads

`src/xrEngine/EngineThreading.cpp`.

| Thread | Work |
| --- | --- |
| Primary | SDL, `FrameMove`, `seqRender`, swapchain |
| `PreRenderThread` | Discord update, `seqParallelRender`, the sun / rain / reflection graphs (`CollectGraphs`), rain items, `UpdateParticles` (menu closed) |
| `GameThread` | HUD `OnFrameMT`, level sound events, `Sheduler.Update`, `seqParallel` (including `CLevel::script_gc`), `seqFrameMT` (sound if `mtSound`, physics if `mtPhysics`), persistent `OnFrameMT` (framework callback), `LuaGC`, sound update |

`seqFrame`: `CGamePersistent` (weather, DOF, intro), `CLevel` (net, bullets; queues scripts/GC for `GameThread`). Physics and sound use `seqFrameMT` when `mtPhysics` / `mtSound` are enabled, otherwise `seqFrame`. Order within each list is registrator priority.

Shutdown in `EngineLoopAndDestroy`: spatial DBs, `DEL_INSTANCE(g_pGamePersistent)`, `pApp`, input, `cfg_save`, sound, `Device` / unload DLLs. Then `EndPlay`.

## Manual thread stacks (Windows)

Press **Ctrl+Shift+J** to write the engine's thread stacks to `$logs$/thread_stacks_<timestamp>_<ticks>.txt`. Holding the keys produces one file; release and press again for another. After capture completes, a dialog reports success or failure and shows the output path. The file records individual thread capture errors.

A separate Windows thread registers the shortcut after the game window is initialized, so it can respond even when the main loop is hung. The shortcut is system-wide while the game runs; registration failure is recorded in the log. The report includes thread IDs, addresses, modules, and available function names and source lines; matching PDB files provide names. Threads are captured one at a time and may exit or be inaccessible. The process pauses each other thread while walking its stack, which can block if the thread holds a lock needed for stack walking.

Post-build check: press the shortcut in the menu, in a level, and while the main thread is blocked; verify multiple thread IDs and readable frames in the text file. Hold the keys and confirm only one file is created, then release and press again. Verify an error dialog appears when the output folder is unavailable.

## Shader precompile at start

Every shader stage the renderer makes is remembered by its stage, source name and the defines of its own pass (`ShaderManifest`, `learned_shaders.bin` in the application data folder; saved when a level has loaded and when the renderer goes). At the next start the renderer makes them all on worker threads behind the start screen, before the game's own start-up steps (`ShaderManifest::OnAppStart`, registered in `seqAppStart`), with `Precompiling shaders N of M` as the status and the progress bar moving from 78 to 92; the stages are held until the device goes, so a level finds its shaders made. The defines of the settings are not stored: a stage is made with those of the current settings. A stage whose source file is gone is left out. The log's `* shaders at <stage>` lines tell how many stages were made and compiled and what the source checksum, the cache read, the creation and the compile took, summed over threads. With `r__shader_cache off` every start compiles every stage again. The graphics pipelines of the D3D12 backend follow the shaders: a pipeline the device makes at a draw is remembered in `learned_pipelines.bin`, its shaders named by a hash of their code and its input layout stored by its elements (`PipelineManifest.cpp`, `IRHIDevice::LearnedPipelineCount` / `PrecreatePipeline` / `SaveLearnedPipelines`), and the start screen makes them again with `Precompiling pipelines N of M` (bar 86 to 92) once the stages they need are alive. Both lists are written when they have grown: every 512 frames, when a level has loaded and when the renderer goes. A pipeline's key is compared as bytes, so the padding of its depth and blend descriptions is zeroed and a layout is named by its elements, not by the object: before that the same pipeline was made several times over (272 made at draws in a run, 47 distinct). The pipelines the forward level frame and the fullscreen passes make through NRI, and compute pipelines, are still made at first use.

The stages are made in one parallel loop over the whole list, and so are the pipelines: every worker takes the next item as it finishes one, and the start screen shows the running count (`StartupFrameWaitTask` takes a function it calls before each frame it draws). Batches would make the workers wait for the slowest stage of each batch. With the shader cache off, 1550 stages took 27.7 s in batches of 24 and 13.2 s in one loop on a 24-thread processor (2026-11-19); with the cache on the step is under a second either way.

A setting that changes how materials are compiled (`r_anti_aliasing msaa` and `r_msaa`, `r_shading_quality`, `r_shadow_quality`, the occlusion and reflection settings; `r_settings` marks them `recompile`) compiles every material again (`CResourceManager::RecompileShaders`). All of them are compiled on the workers first and then swapped in one at a time; the log line `shaders recompiled for new settings: N in M ms` gives the time. What a visited value made is kept for the run, so going back to it takes about a second; the first visit of a value compiles from source unless the disk shader cache holds it (about 13 s for 1200 materials with the cache off on a 24-thread processor, 2026-11-19).

A learned pipeline names its shaders by the hash of their compiled code, so a pipeline of a shader that was edited since can never be made again. The start marks every pipeline it finds no such shader for (all remembered stages have been made by then), and the next save of `learned_pipelines.bin` leaves those out, with the input layouts nothing refers to any more; the log says how many were dropped. A pipeline dropped by mistake is learned again at the first draw that needs it.

## Level load timing

While a level loads, every load stage logs `* Game Loading Timer: <ms> ms at <stage>` when it begins (`CGamePersistent::SetLoadStageTitle`; the renderer adds lines around the scene's tables and the textures that waited). The difference of two lines in a row is what the first stage took; `Finished for <ms> ms` closes the list. The server's stages run on their own thread, so their lines fall between the client's.

## Autotest

Launch `xrEngine.exe` from the game folder. Implementation: `src/xrEngine/Autotest.cpp`. `FrameBegin` runs from `on_idle`, `FrameEnd` from `dxRenderDeviceRender`.

```text
xrEngine.exe -autotest -autotest_frames 120 -autotest_warmup 150 -autotest_timeout 300 -autotest_cmd r_anti_aliasing taa;start server(jupiter/single/alife/new) client(localhost)
```

| Flag | Default | Effect |
| --- | --- | --- |
| `-autotest` | — | Hide the game window after migration, set `fps_limit` and `main_menu_fps_limit` to 0, gather statistics and write the report |
| `-autotest_frames N` | 120 | Measured frames |
| `-autotest_warmup N` | 150 | Level frames skipped before measuring |
| `-autotest_timeout S` | 300 | `exit` after this many seconds of real time, checked every frame from the first one, start screen and intro included (not inside a blocking load task) |
| `-autotest_out <path>` | `$logs$\autotest.csv` | Timing report. One path token |
| `-autotest_hash` | off | Per-frame CRC of the view (the game's picture, as `shot` takes it) into the `hash` column |
| `-autotest_shot [N]` | off | `$logs$\autotest_NNNN.tga` of the view every N measured frames. Flag with no N means every frame |
| `-autotest_profile` | off | Open the profiler for the run and write its history as `$logs$\profiler_<time>.csv` when the measured frames are done. `DEBUG_DRAW` builds only |
| `-autotest_capture [path]` | off | Take one RenderDoc capture after the measured frames, then exit once the file is written. `path` is a capture name template of one token; default `$logs$\autotest`. The window stays shown. The game has to be started from RenderDoc |
| `-autotest_dt [ms]` | off; no value is 16.667 | Fixed step, 1 to 100 ms: see [picture checks](#picture-checks) |
| `-autotest_cmd <cmds>` | — | Console commands once the main menu is up and `g_loading_events` is empty. Split on `;`. Also runs `keypress_on_start 0` |
| `-autotest_script <file>` | — | Run the steps of a text file instead of measuring, a line each: `start <save>` (loads it from the main menu, or over the level that is up), `menu` (leaves the level for the main menu), `cmd <console command>`, `wait <frames of the loaded level>`, `mouse_down <button>` / `mouse_up <button>` (presses or releases a mouse button for the game, 0 left, 1 right; it stays held until released, so `mouse_down 1`, `wait 60`, `shot` shoots through the sights), `key_down <code>` / `key_up <code>` (the same for a key, by its SDL scancode: 12 is I), `mouse_move <dx> <dy>` (moves the mouse once by so many counts: a positive `dy` looks down), `shot <name>` (writes `$logs$\<name>.tga`: the view, the game's picture without the tools and without bars, of the view's size: the `imgui_game_window 1` picture, the `r_view_scale` part, or the window's size; see [rendering.md](rendering.md#view-rectangle)), `shot_window <name>` (the same file of the whole swap chain, which has the window's size: the tools' windows, the Game window's frame and the bars included; what `shot` wrote before 2026-11-21), `settle [N]` (holds at least N frames of the loaded level, then until the frame number is a multiple of 256; the frame after it is a camera cut; see [picture checks](#picture-checks)), `profile <N>` (the frame profiler's capture of N counted frames: four frames with the clocks stopped while the profiler starts, then a wait of N + 1 frames whose last N are the capture, written as `$logs$\profiler_capture_<time>.csv` by `profiler_end`'s code; `DEBUG_DRAW` builds only, others log `! [autotest] profile: …` and only wait). Their readback buffer follows the larger of the last reset's size and the swap chain's, so a shot after `r_resolution` + `r_restart` to a larger size is whole; the hidden window takes `r_resolution` at every `r_restart`. Lines that start with `;` are skipped. The run ends after the last step and passes when every step was taken. `file` is a path of the system, one token |

`-autotest_cmd` copies the rest of `Core.Params`. Put it last. `-autotest_post_cmd <cmd>` runs one console command on the level when warmup ends (for example `set_weather default_rain`); place it before `-autotest_cmd`. Autotest does not force windowed mode or suppress error dialogs; configure those separately if needed.

CSV columns: `frame,calc_ms,dump_ms,total_ms,calls,verts,polys,static_dips,hash,frame_ms,counted,step`. `calls` is the old backend's DIP / DP of the frame (`rs_stats`' count), `frame_ms` the real time from the start of the frame to the start of the next (0 on the last row). Without a script a row is a measured frame (`counted` 1). With a script a row is every drawn level frame from the end of the load (precache, `settle` and step frames included): `counted` 1 on frames the run counts, `step` 1 on a frame that took a script step or ran its console command (a `shot`'s readback lands there).

A save, a profile and a capture in one run, launched from RenderDoc's *Launch Application* with this command line (the profiler keeps 255 frames, so more measured frames than that are not all exported):

```text
-autotest -autotest_frames 200 -autotest_warmup 300 -autotest_profile -autotest_capture D:\Captures\auto -autotest_cmd start server(<save name>/single/alife/load) client(localhost)
```

The capture is taken after the timing samples, so its cost is not in the report. Without RenderDoc in the process the run logs `! [autotest] capture: RenderDoc is not in the process` and ends as usual.

Log tail:

```text
~ [autotest] frames=… errors=… out=…
~ [autotest] median calc=…ms dump=…ms total=…ms
~ [autotest] old path: DIP/DP … median, … most over … counted frames
~ [autotest] process memory: working set … MB (peak … MB), commit … MB (peak … MB)
~ [autotest] RESULT: PASS|FAIL
```

The medians are over the counted rows. Process memory is the working set and the commit charge on Windows, `VmRSS` / `VmHWM` / `VmSize` / `VmPeak` on Linux, and only the peak resident size from `getrusage` elsewhere.

`errors` counts log lines that contain `[error]`. `Finish` calls `exit(Autotest::Verdict())`: `0` when `errors` is 0 and the sample count reached the frame target, otherwise `1`. It does not go through `EngineLoopAndDestroy`. Under `-autotest`, `cfg_save` writes nothing (`~ [autotest] cfg_save skipped`), so a run never changes the config file; `-ltx <file>` (one token, relative to `$app_data_root$`) is read instead of `user.ltx`, so the user's settings stay out of the run.

### Picture checks

Script waits count frames, while much of the picture moves with time: exposure adaptation, the light tracker of the actor and the HUD, wetness, weather and game time, wind, water and particle animation, TAA history. Two builds at different frame rates, or one save loaded second or sixth in a script, then give different pictures at the same `wait`. `-autotest_dt` and `settle` take that out:

- `-autotest_dt <ms>` sets `Device.FixedDelta` every frame (the step the Cinema export uses). A frame the run counts (a frame of a `wait` on a ready level; with no script, every ready level frame) advances `fTimeDelta`, `fTimeDeltaSmoothing`, `fTimeGlobal`, `dwTimeGlobal`, `dwTimeDelta` and `dwTimeContinual` by exactly the step. So does every frame without a level (start screen, intro, menu), whose sequences and UI wait on that time. Every other frame with a level (loading, precache, `settle`, the frame of each step) gets a step below a microsecond, so the clocks do not move. The frame after each `start` step puts the engine time and `dwTimeContinual` at n hours for the n-th start (forward only; a clock already past it logs `! [autotest] the engine time is past …`): game time, scheduler, animation, animated textures and shader time at a shot depend only on the waits after the `start`, not on how many frames the menu and the load took. Starting a save over a loaded level jumps the old level's clock once before it is replaced; one launch per save avoids that. Pacing is not touched: `fps_limit` stays 0 and the frame pacer uses the real clock.
- Under `-autotest_dt` real-time budgets are off: the scheduler updates every object that is due, game events and spawns are not cut by `g_events_time_budget_ms`, the A-Life switch has no time limit, and the grass cache fills every pending slot in one frame. Seeds are fixed: `::Random` is seeded every frame from the count of counted frames, and the A-Life, spawn and switch generators and the Lua `math.randomseed` of the load screen get seed 1 (the game's own scripts seed from `time_global()`, which now follows the step).
- `settle [N]` holds frames without moving the clocks (with `-autotest_dt`; without it the clocks run on), so asynchronous work (texture streaming, model and spawn loads, the scene's tables) gets real time while the world stands still, until N frames have passed and the frame number is a multiple of 256 (TAA, FSR at native and 2x, and alternating sun cascades then start every shot at the same phase). The next frame is a camera cut: TAA and upscaler history restart, motion blur skips it, depth of field focuses at once, and the exposure jumps to the measured luminance. Put weather and time commands (`cmd set_weather …`, `cmd set_game_time …`) before `settle`.

A script per save: `start <save>`, its `cmd` lines, `settle 120`, `wait 300`, `shot <name>`. With `-autotest_dt 16.667`, the 300 frames are 5 s of game time in every build.

Not deterministic after this:

- Threads: the game thread (scheduler, A-Life, level scripts, sound) runs beside the primary thread's render, and particles, rain and the dsgraph work on other threads; all of them draw from the one `::Random`, so its order inside a frame can change. The CRT `rand()` of the particle noise and gaussian is per thread and not seeded.
- Real time between frames: `Device.TimerAsync()` (mercury ball, server timers) and the network timer of game events (`timeServer()`, 50 ms of real latency) still read the real clock. During frozen level frames events are delivered in real time; the precache hold then lasts until every spawn is done (its 3 s cap counts `dwTimeContinual`, which does not move); a spawn that never completes ends in `-autotest_timeout`.
- Frame history: occlusion (HOM), the spot shadow cache and texture streaming keep state by frame number; with a still camera they settle, which `settle N` gives frames to do. `textures loaded after the scene` in the log tells whether the load waited for textures.
- GPU: cross-vendor and driver differences, and FSR modes whose jitter phase count does not divide 256 (1.5x is 18 phases).
- Gameplay script options (`ixr_options.ixr`) are still the user's.

### render_check.py

`util/render_check.py` (Python 3 and Pillow) runs one launch per save, so the position of a save in a list cannot matter; a launch costs an engine start and a level load each time (13 to 16 s a save, about 90 s per API for the six saves).

```text
python util/render_check.py [--api d3d12|d3d11|both] [--saves render_perf,sun_shafts] [--presets minimal,low,medium,high,ultra] [--profile [N]] [--update-reference] [--threshold 0.5] [--json <file>]
python util/render_check.py --compare <before.json> <after.json>
```

It refuses to start while `xrEngine.exe` or `LevelEditor.exe` is running. Per launch it writes `$app_data_root$\render_check.ltx` (`r_api`, the rows of `gamedata_cs/configs/ixray_settings/rspec_ultra.ltx` read at run time, then the pinned post and pacing settings: `r_vsync off`, `r_fps_limit 0`, `r_bloom_amount 0.04`, `r_saturation 1`, the tonemap and exposure values, `r_vignette off`, `r_sharpening 0`, brightness, contrast and gamma 1, `r_resolution 1920x1080`) and runs `xrEngine.exe -autotest -nointro -autotest_dt 16.667 -autotest_timeout 300 -ltx render_check.ltx -autotest_script <temp>` from the game folder. The shot is converted to `<out>/<api>/<save>.png` (the TGA is deleted) and compared with `<reference>/<api>/<save>.png` by the mean absolute RGB difference (0..255; a `_diff.png` eight times brighter is written next to it). From the log it takes the scene counters (`[scene] materials: N of M …`, `level visuals the main view draws from the graph: N of M`, `LOD cards: N of M drawn by the scene`, `level visuals outside the depth batch: N of M`), which must equal those kept in `<reference>/<api>/scene.json`, and fails on `! NRI`, `! Dynamic geometry`, `is still held after its release`, `FATAL`, shader compile errors, `[error]`, `! [autotest]`, a timeout or a non-zero exit. If the engine is still running 60 s after `--timeout`, the script kills it and fails the save. Every failed save prints the last 15 lines of its log. It prints one table (diff, scene, log, time, textures that waited, PASS / FAIL) and exits 1 on any FAIL. `--update-reference` stores the run's pictures and counters as the reference.

`--presets a,b` runs everything once per `gamedata_cs/configs/ixray_settings/rspec_<name>.ltx` (instead of `--preset <file>`, ultra by default); a preset other than ultra keeps its shots and numbers in `<preset>/<api>/` of the output and the reference. With `--api both`, the minimal and low rows are meant to be read on D3D11.

`--profile [N]` (3 without N) is the baseline every renderer stage is compared with. Each save is launched N times (each launch also takes and compares the shot: the worst diff is shown); after the shot the script runs `profile 600`, `cmd render_memory_stats`, `wait 10`. Per launch it reads:

- the profiler capture named by `* Profiler: capture of … written to profiler_capture_<time>.csv`: the `frame` row (frame ms, worst frame of the window, the frame count, which must be 600), `Primary` `on_idle`, `…/Render: End/Present`, `…/IGame_Level::OnFrame`, `X-Ray Game Thread` `Sheduler`, `GPU` `Frame`, and every zone right under `FORWARD_LEVEL_FRAME` on `Primary` (their sum is `render cpu ms`) and on `GPU`. ForwardLampShadows, CollectMain, RenderSunCascade, phase_post, ScenePrepass, OffscreenReflections, ForwardOcclusion, ForwardOpaque, ForwardSorted, SurfaceLights, ForwardOit and ForwardCull on `Primary`, and the same without CollectMain, SurfaceLights and ForwardCull on `GPU`, are reported as `zone not found: …` when absent (a renamed zone, or a pass the preset does not run); a zone is never read as 0. The GPU rows trail the window by the timer readback latency (a few frames before it, of the same still scene);
- `autotest.csv`: the drawn level frames from the end of the load to the 600th counted frame, step frames left out: their median, how many are above twice it, and the worst;
- the log: `~ [autotest] old path: DIP/DP …`; the graph's packets of the `[scene]` lines that `render_memory_stats` makes the renderer log a few frames later (`main view left by reason: the graph holds N static packets`, `mesh parts left in the graph N`, `HUD parts … left in the graph by reason`, the `sorted list` / `weighed surfaces (WBOIT)` / `HUD's sorted list` leftovers without `weighed apart`, `graph passes in the frame before`, the sun cascades' and the lamp, rain map and spot cache views' `graphs drew …` counts; one frame each, the frame before); `Game Loading Timer: Finished for`, `shaders at the start of the level` + `shaders at the level's shaders` (made, compiled, compile ms), `shader manifest: … stages` / `… pipelines made behind the start screen`, `[scene] forward pipelines: N used`, `forward pipelines made ahead of their first cluster` (summed), the last D3D12 `pso hits/misses`, `Frame latency`; memory: `* video memory: … MB on the device …; N render targets, M MB`, `video memory of the process`, the last `D3D12 live: N surfaces, M buffers`, `texture buckets … MB`, the head pools' MB, the materials' tails and heads MB, the alpha array MB, and `~ [autotest] process memory`. A line that is not there is named (`line not found: …`) under the table.

The medians of the N launches are printed as one table per preset and API (a row a number, a column a save; the other zones are in the json only) and, with `--update-reference`, kept in `<reference>/<api>/profile.json`. A normal run prints each median with its difference from that file in units and percent; beyond 7 % (and, for times, at least 0.01 ms) the cell ends in `!`. `frame ms spread %` is the spread of the N launches. Everything read, launch by launch, goes to `--json` (`<out>/render_check.json`); `--compare before.json after.json` prints the same tables of two such files without launching anything. A profiled launch takes about 20 s (the 600 frames are 10 s of game time, about 5 s of real time on this machine; not measured yet): six saves, N = 3, one preset and API is about 6 minutes; five presets on both APIs about an hour. The reduced baseline is `--profile --api d3d12` on ultra plus `--profile --api d3d11 --presets minimal,low`, about 18 minutes.

The engine and the game folder come from `--engine` and `--game`, or from the `IXRAY_ENGINE` and `IXRAY_GAME` environment variables. Defaults at the top of the script: reference `<game>/_appdata_ixray_/render_check/reference`, output `<game>/_appdata_ixray_/render_check/last`, saves `render_perf`, `sun_shafts` (with `set_weather sun_shafts`), `point_light`, `hud_light_test`, `test_rain`, `water_problem`. Each is also an argument.

For R4 motion-vector checks, `o.dx11_disable_motion_vectors` is initialized from the `DISABLE_MOTION_VECTORS` shader option in `ixr.cpp`; AA/upscaler cvars do not control that option automatically in this branch. There is one renderer, so nothing is selected before testing. Replace `jupiter` with the map. `-autotest_shot 30` checks the game's picture. The velocity target needs a windowed RenderDoc capture.

Sources: [application](../../src/xrPlay/Application.cpp), [load stages](../../src/xrEngine/x_ray.cpp), [device loop](../../src/xrEngine/device.cpp), [thread tasks](../../src/xrEngine/EngineThreading.cpp), [autotest](../../src/xrEngine/Autotest.cpp).
