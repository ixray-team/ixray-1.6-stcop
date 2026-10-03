# Startup and frame

Entry: `CApplication::Run` in `src/xrPlay/Application.cpp`.

```text
BeginPlay
InitEngine
MigrateToGameWindow
EngineLoopAndDestroy → Device.Run
EndPlay → Core._destroy
```

`Run` starts the splash thread unless `-autotest`, then `SteamWorks.BeginPlay()`. Non-`DEBUG_DRAW` Windows builds take a single-instance mutex.

## BeginPlay

1. `SDL_Init` (audio, video, gamepad, events)
2. `Debug._initialize`
3. `EnumerateDisplayModes` (fallback 1024×768)
4. Hidden `g_AppInfo.Window` (`SDL_WINDOW_VULKAN` off Windows)
5. `EngineLoadStage1(CommandLine)`

## InitEngine

Stages 1–5 are in `src/xrEngine/x_ray.cpp`. Stages between them are in `CApplication::InitEngine`. `Device.Run` is `EngineLoopAndDestroy`, after stage 5 returns.

| Order | Call | Effect |
| --- | --- | --- |
| 1 | `EngineLoadStage1` | `Core._initialize("IXRay")`, `InitSettings` |
| 2 | `EngineLoadStage2` | `Device`, `Engine.Initialize`, `CInput`, string table |
| 2b | `CreateRendererList` | tokens `renderer_r4_static`, `renderer_r4` |
| 2c | `new CConsole` | before user script |
| 3 | `EngineLoadStage3` | `Console->Initialize`; `user.ltx`, `user_dedicated.ltx`, or `-ltx` |
| 3b | `ConfigureRenderer` | mode applied before DLL load |
| 3c | `Engine.External.Initialize` | `xrRender_R4` + `xrGame` |
| 4 | `EngineLoadStage4` | see below |
| 4b | `LoadCustomSettings` | `ixray_settings` |
| 5 | `EngineLoadStage5` | `pApp`, `g_pGamePersistent`, spatial DBs |

`InitSettings`: `$game_config$/system.ltx` → `pSettings`, `game.ltx` → `pGameIni`. Missing file aborts. FS root: `-fsltx` or `fsgame.ltx`.

Stage 4 order: `GRHI` → `InitSound1` (`-nosound`) → `execUserScript` (`default_controls`, config file) → `InitSound2` → `-start` / `-load` → `CFontManager` → `Device.InitRenderDevice(D3D11)` → `Device.Create`.

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
    GRHI->BeginFrame()
    SDL_PollEvent → on_event   # sets quiting on close
    on_idle()
```

`on_idle` (`device.cpp`):

| Condition | Body |
| --- | --- |
| `!b_is_Ready` | sleep 100 ms |
| `g_loading_events` non-empty | run front; pop if it returns true; `pApp->LoadDraw()`; return |
| else | particles, `ModelDefferClear`, due `m_time_callbacks`, `UpdatePlayerHud` |
| | `BeginReflectionCollect`, `ResetSunCollect`, then `PreRenderThread.Run()` |
| | `FrameMove()` on the primary thread while collection runs |
| | `GameThread.Run()` |
| | active client: `Begin`, `seqRender`, `End` |
| | `PreRenderThread.Wait()`, `GameThread.Wait()`, `EndRender` |

Reflection collection uses an immutable camera snapshot taken before `FrameMove`. Its six face graphs are consumed only after collector completion, with GPU draws on the primary thread. The render camera can already have moved; VSLR sampling must use the captured transform, including translation. Contracts and validation: [reflections.md](reflections.md).

`FrameMove`: `dwFrame++`, smooth `fTimeDelta` (clamp `EPS_S`…`0.1`), `seqFrame`. Paused: `fTimeDelta = 0` then still clamped. Dedicated server ignores `Pause`.

`g_loading_events` consumers include `CLevel::net_start1`…`net_start6`. `net_start4` pops itself, `push_front`s `net_start_client1`…`6`, returns false so `on_idle` does not pop the new front that frame.

## Threads

`src/xrEngine/EngineThreading.cpp`.

| Thread | Work |
| --- | --- |
| Primary | SDL, `FrameMove`, `seqRender`, swapchain |
| `PreRenderThread` | `seqParallelRender`, reflections, sun cascades, rain items, `UpdateParticles` (menu closed) |
| `GameThread` | HUD `OnFrameMT`, level sound events, `CSaveManager` writes via `async_tasks`, `Sheduler.Update`, `seqParallel`, `seqFrameMT`, `LuaGC`, `Sound->update` |

`seqFrame`: `CGamePersistent` (weather, DOF, intro), `CLevel` (net, bullets, scripts), `CPHWorld`. Order is registrator priority.

Shutdown in `EngineLoopAndDestroy`: spatial DBs, `DEL_INSTANCE(g_pGamePersistent)`, `pApp`, input, `cfg_save`, sound, `Device` / unload DLLs. Then `EndPlay`.

## Autotest

Launch `xrEngine.exe` from the game folder. Implementation: `src/xrEngine/Autotest.cpp`. `FrameBegin` runs from `on_idle`, `FrameEnd` from `dxRenderDeviceRender`.

```text
xrEngine.exe -autotest -autotest_frames 120 -autotest_warmup 150 -autotest_timeout 300 -autotest_cmd r_aa 3;start server(jupiter/single/alife/new) client(localhost)
```

| Flag | Default | Effect |
| --- | --- | --- |
| `-autotest` | — | Hide the window, skip the error dialog, set `fps_limit` and `main_menu_fps_limit` to 0, force windowed, write the report |
| `-autotest_frames N` | 120 | Measured frames |
| `-autotest_warmup N` | 150 | Level frames skipped before measuring |
| `-autotest_timeout S` | 300 | `exit` after this many seconds |
| `-autotest_out <path>` | `$logs$\autotest.csv` | Timing report. One path token |
| `-autotest_hash` | off | Per-frame CRC of the swapchain into the `hash` column |
| `-autotest_shot [N]` | off | `$logs$\autotest_NNNN.tga` every N measured frames. Flag with no N means every frame |
| `-autotest_post_cmd <cmd>` | — | One `Console->Execute` on the frame warmup ends |
| `-autotest_cmd <cmds>` | — | Console commands once the main menu is up and `g_loading_events` is empty. Split on `;`. Also runs `keypress_on_start 0` |

`-autotest_cmd` copies the rest of `Core.Params`. Put it last. `-autotest_post_cmd` is cut at the next ` -autotest` token, so it can sit anywhere. It is one command string, not a `;` list.

CSV columns: `frame,calc_ms,dump_ms,total_ms,calls,verts,polys,static_dips,hash`.

Log tail:

```text
~ [autotest] frames=… errors=… out=…
~ [autotest] median calc=…ms dump=…ms total=…ms
~ [autotest] RESULT: PASS|FAIL
```

`errors` counts log lines that contain `[error]`. `Finish` calls `exit(Autotest::Verdict())`: `0` when `errors` is 0 and the sample count reached the frame target, otherwise `1`. It does not go through `EngineLoopAndDestroy`.

`CRender::level_Load` snapshots `o.dx11_disable_motion_vectors` from `r_aa`, the upscaler (`ps_r_scale_mode`), and `r4_mblur_quality`. Put those in `-autotest_cmd` before `start`. Replace `jupiter` with the map. `-autotest_shot 30` checks the presented image. The velocity target needs a windowed RenderDoc capture.
