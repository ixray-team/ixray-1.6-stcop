# Architecture

| Binary | CMake project | Source |
| --- | --- | --- |
| `xrEngine.exe` | `xrEngine` | `src/xrPlay` |
| `xrEngineCore.dll` | `xrEngineCore` | `src/xrEngine` |

`CEngineAPI::Initialize` (`src/xrEngine/EngineAPI.cpp`) runs after the console exists.

| Call | DLL | Condition |
| --- | --- | --- |
| `InitializeNotDedicated` | `xrRender_R4` / `xrRender_R2` | client; selected by `rsR4` / `rsR2`, R4 load failure tries R2 |
| `Initialize` | `xrRender_R1` | fallback when no renderer DLL loaded |
| `InitializeDedicated` | `xrRender_DS0` | `g_dedicated_server` (`IXRAY_MP`) |
| `Initialize` | `xrGame` | always |
| `Initialize` | `xrGameSpy` | DLL present |

`xrGame` exports:

| Symbol | When |
| --- | --- |
| `xrGameInitialize` | load: `game_global.ltx`, commands, input bindings, luabind allocator |
| `xrFactory_Create` / `xrFactory_Destroy` | `NEW_INSTANCE` / `DEL_INSTANCE` |
| `xrGameShutdown` | `CEngineAPI::Destroy` |

`CLSID_GAME_PERSISTANT` → `CGamePersistent` in stage 5.

## Coupling

| Mechanism | Where | Use |
| --- | --- | --- |
| Globals | `Device`, `Engine`, `Core`, `FS`, `Console`, `pInput`, `pSettings`, `pGameIni`, `Render`, `GRHI`, `g_pGamePersistent`, `g_pGameLevel`, `ai()` | process-wide services |
| `CRegistrator` | `src/xrEngine/pure.h`, lists on `Device` | `seqFrame`, `seqFrameMT`, `seqRender`, `seqAppStart`, `seqAppEnd`, `seqAppActivate`, `seqAppDeactivate`, `seqDeviceReset`, `seqResolutionChanged` |
| Delegates | `Device.seqParallel`, `seqParallelRender`, `ModelDefferClear` | parallel work queues; `ModelDefferClear` runs in primary `on_idle` |
| `CSheduler` | `Engine.Sheduler`, `ISheduled` | sub-frame updates. `GameThread` calls `Update` when not paused |
| Factory | `src/xrServerEntities/object_factory.h` | `CLASS_ID` → server `CSE_*` + client `C*` |

`pureFrame::OnFrame` runs from `FrameMove` on the primary thread. `pureRender::OnRender` runs from `seqRender`. Priority constants: `REG_PRIORITY_LOW` / `NORMAL` / `HIGH` / `CAPTURE`. UI render order is `PureRenderPriority` in `pure.h`.

Spatial DBs from stage 5: `g_SpatialSpace` (game, render), `g_SpatialSpacePhysic` (physics). Query by sphere, box, or frustum.

`CEngineExternal` selects CoP / CS / SoC from `[general] Platform` in `$game_config$/engine_external.ltx`. One `xrGame.dll` for all three.

Module ownership and the edit map are in [AGENTS.md](../../AGENTS.md).

Sources: [DLL loading](../../src/xrEngine/EngineAPI.cpp), [registrators](../../src/xrEngine/pure.h), [platform selection](../../src/xrCore/EngineExternal.cpp).
