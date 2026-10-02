# Gameplay

`xrFactory_Create` (`src/xrGame/xrGame.cpp`) → `object_factory().client_object(clsid)`. Server half is `CSE_*` in `src/xrServerEntities`. Client half is `C*` in `src/xrGame`. Saves write the server object. Per-frame behavior is the client class.

| Object | Lifetime | Owns |
| --- | --- | --- |
| `CGamePersistent` | session | menu, `CEnvironment`, ambient FX, intro, DOF. `OnFrame` runs on the menu |
| `CLevel` (`g_pGameLevel`) | one map | objects, in-process `xrServer`, net queue, bullets, map, AI link |

`IGame_Persistent::Start` splits `spawn/game_type/alife/new_or_load` on `/`. Single-player ALife: `alife` + `single`.

## Level load

`CLevel::net_Start` pushes `g_loading_events`. One step per `on_idle`. Failure: `net_start_result_total = false`, later steps return immediately.

| Step | Work |
| --- | --- |
| `net_start1` | `xrServer` if single, else `xrGameSpyServer`. Resolve level id |
| `net_start2` | `Server->Connect`, `SLS_Default` |
| `net_start3` | port, password, cdkey onto client options |
| `net_start4` | pop self, queue `net_start_client1`…`6`, return false |
| `net_start5` | `M_CLIENTREADY` |
| `net_start6` | bullet manager, `pApp->LoadEnd` |

Client steps stream geom, AI, spawn, objects (`Level_start.cpp`).

## `CLevel::OnFrame`

Primary thread, `seqFrame`, `src/xrGame/Level.cpp`:

1. Feel-touch, `BulletManager`
2. On disconnect: `Event.Defer("kernel:disconnect")`, return
3. `ClientReceive`, `ProcessGameEvents`, correction if `m_bNeed_CrPr`
4. `seqParallel` ← `CMapManager::Update` (clients)
5. inherited `OnFrame`, detail trample
6. `ai().script_engine().script_process(eScriptProcessorLevel)->update()`

`CPHWorld::OnFrame` steps ODE. Game code uses `CPHShell` / `IPHWorld`. Sub-rate work: `ISheduled` on `Engine.Sheduler` (`GameThread`).

## AI

`ai()` is `CAI_Space` (`ai_space.h`), rebuilt per level.

| Accessor | Data |
| --- | --- |
| `game_graph` | locations across levels |
| `level_graph` | walk nodes |
| `cross_table` | game vertex → level vertex |
| `cover_manager` | cover |
| `patrol_paths` | patrols |
| `ef_storage` | planner evaluators |
| `alife` | `CALifeSimulator` if this session is ALife |
| `moving_objects`, `doors` | path blockers |
| `script_engine` | Lua |

Stalkers: `CActionPlanner` + `stalker_*` (movement, combat, objects, sound memory, visual memory). Monsters use the same planner with other evaluators. Path and cover queries go through `ai()`.

ALife: offline = `CSE_*` only, online = client entity. `CALifeUpdateManager` spends a per-frame budget. Switch distance is on the simulator, not `CLevel::OnFrame`.

## Lua

`CScriptEngine` (`src/xrScripts/script_engine.h`): one LuaJIT state, `CScriptProcess` list. Level process updates from `CLevel::OnFrame`.

Export: `DECLARE_SCRIPT_REGISTER_FUNCTION` plus `*_script.cpp`. Common scripts: `gamedata/scripts`. Addon init: `AddonInfo::ScriptInit`. Mod callbacks: `[callbacks]` in `game_global.ltx`. IXR framework frame hook: `IGame_Persistent::ixr_framework_onframe`.

Allocator: `Memory`. GC: `Device.LuaGC` on `GameThread`. `IXRAY_LDEBUG` enables the LuaPanda host.

## Files

| Area | Path under `src/xrGame` |
| --- | --- |
| Player | `Actor.*`, `actor_script.cpp` |
| HUD weapons | `HudItem.*`, `player_hud.*` |
| Weapons | `Weapon.*`, `WeaponMagazined.*` |
| Inventory | `inventory*.*`, `InventoryOwner.*` |
| Items | `InventoryItem.*`, `GameObject.*` |
| Stalkers | `stalker_*.cpp` |
| Zones | `*Zone.*` |
| Dialogs, tasks | `PhraseDialog*`, `GameTask*` |
| UI | `ui/` (widgets in `src/xrUI`) |
| Saves | `src/xrCore/Save/SaveManager.*` (`GameThread`) |

Spawnable class: `CSE_*`, client class, `CLASS_ID`, factory registration, script export. Helper types used only by the actor skip the factory.

## Multiplayer

`IXRAY_MP`: `xrServer` exe, `xrServerCLI`, `xrGameSpy`, `xrRender_DS0`. `GameID() != eGameIDSingle` uses `xrGameSpyServer` and sets `rsDisableObjectsAsCrows`. Modes: `game_cl_*` / `game_sv_*`.

Single-player `xrServer` is inside `xrGame` and builds without `IXRAY_MP`. Missing `xrGameSpy.dll` is ignored.
