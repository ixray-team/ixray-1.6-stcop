# Gameplay

`xrFactory_Create` (`src/xrGame/xrGame.cpp`) → `object_factory().client_object(clsid)`. Server half is `CSE_*` in `src/xrServerEntities`. Client half is `C*` in `src/xrGame`. Saves write the server object. Per-frame behavior is the client class.

| Object | Lifetime | Owns |
| --- | --- | --- |
| `CGamePersistent` | application (stage 5 to shutdown) | menu, `CEnvironment`, ambient FX, intro, DOF. `OnFrame` runs on the menu |
| `CLevel` (`g_pGameLevel`) | one map | objects, in-process `xrServer`, net queue, bullets, map, AI link |

`IGame_Persistent::Start` splits `spawn/game_type/alife/new_or_load` on `/`. Single-player ALife: `alife` + `single`.

## Level load

`CLevel::net_Start` pushes `g_loading_events`. One step per `on_idle`. Failure sets `net_start_result_total = false`; connection-dependent steps skip their work, while `net_start6` still clears/loads the bullet manager and calls `LoadEnd`.

| Step | Work |
| --- | --- |
| `net_start1` | `xrServer` if single, else `xrGameSpyServer`. Resolve level id |
| `net_start2` | `Server->Connect`, `SLS_Default` |
| `net_start3` | port, password, cdkey onto client options |
| `net_start4` | pop self, queue `net_start_client1`…`6`, return false |
| `net_start5` | `M_CLIENTREADY` |
| `net_start6` | bullet manager, `pApp->LoadEnd` |

Client steps load geometry, AI, connection state, and objects in `Level_network_start_client.cpp`.

## `CLevel::OnFrame`

Primary thread, `seqFrame`, `src/xrGame/Level.cpp`:

1. Feel-touch, `BulletManager`
2. On disconnect: `Event.Defer("kernel:disconnect")`, return
3. `ClientReceive`, `ProcessGameEvents`, correction if `m_bNeed_CrPr`
4. `seqParallel` ← `CMapManager::Update` (clients)
5. inherited `OnFrame` and environment game time
6. Queue `CLevelSoundManager::Update` and `CLevel::script_gc` in `seqParallel`; `GameThread` runs them. `script_gc` updates the level script process, physics commanders, and Lua GC. Electronics updates run after queuing those delegates.

`CPHWorld::OnFrame` steps ODE. Game code uses `CPHShell` / `IPHWorld`. Sub-rate work: `ISheduled` on `Engine.Sheduler` (`GameThread`).

## AI

`ai()` lazily creates `CAI_Space` (`ai_space_inline.h`). `CAI_Space::load` / `unload` replace level graph/door data and clear cover data; the service itself is not recreated for each map.

| Accessor | Data |
| --- | --- |
| `game_graph` | locations across levels |
| `level_graph` | walk nodes |
| `cross_table` | level vertex → game vertex and distance |
| `cover_manager` | cover |
| `patrol_paths` | patrols |
| `ef_storage` | planner evaluators |
| `alife` | `CALifeSimulator` if this session is ALife |
| `moving_objects`, `doors` | path blockers |
| `script_engine` | Lua |

Stalkers: `CActionPlanner` + `stalker_*` (movement, combat, objects, sound memory, visual memory). Monster behavior uses state managers in `ai/monsters`. Path and cover queries go through `ai()`.

ALife: offline = `CSE_*` only, online = client entity. `CALifeUpdateManager` spends a per-frame budget. Switch distance is on the simulator, not `CLevel::OnFrame`.

## Lua

`CScriptEngine` (`src/xrScripts/script_engine.h`): one LuaJIT state, `CScriptProcess` list. `CLevel::OnFrame` queues `CLevel::script_gc`; it updates the level process on `GameThread`.

Export: `DECLARE_SCRIPT_REGISTER_FUNCTION` plus `*_script.cpp`. Common scripts: `gamedata/scripts`. Addon init: `AddonInfo::ScriptInit`. Mod callbacks: `[callbacks]` in `game_global.ltx`. IXR framework frame hook: `IGame_Persistent::ixr_framework_onframe`.

Luabind allocations use `Memory` in game mode; the Lua VM is created with `luaL_newstate()`. Level GC is `lua_gc(..., LUA_GCSTEP, psLUA_GCSTEP)` in `CLevel::script_gc` on `GameThread`. `IXRAY_LDEBUG` sets `LUABIND_DEBUG_SCRIPTS`; LuaPanda is loaded in `lua_ext.cpp` when `socket.lua` loads successfully.

## Files

| Area | Path under `src/xrGame` |
| --- | --- |
| Player | `Actor.*`, `actor_script.cpp` |
| HUD weapons | `HudItem.*`, `player_hud.*` |
| Weapons | `Weapon.*`, `WeaponMagazined.*` |
| Inventory | `inventory*.*`, `InventoryOwner.*` |
| Items | `InventoryItem.*`, `GameObject.*` |
| Stalkers | `ai/stalker/`, `Legacy/StalkerPlanner/`, `stalker_*.cpp` |
| Zones | `*Zone.*` |
| Dialogs, tasks | `PhraseDialog*`, `GameTask*` |
| UI | `ui/` (widgets in `src/xrUI`) |
| Saves | `alife_storage_manager.*`, `saved_game_wrapper.*`, `autosave_manager.*` |

Spawnable class: `CSE_*`, client class, `CLASS_ID`, factory registration, script export. Helper types used only by the actor skip the factory.

## Multiplayer

`IXRAY_MP`: `xrServer` exe, `xrServerCLI`, `xrGameSpy`, `xrRender_DS0`. `net_start1` creates `xrGameSpyServer` for non-single server options; `CLevel::OnFrame` sets `rsDisableObjectsAsCrows` when `GameID() != eGameIDSingle`. Modes: `game_cl_*` / `game_sv_*`.

Single-player `xrServer` is compiled into `xrGame` and builds without `IXRAY_MP` (when `IXR_TEST_CI` includes `xrGame`). Missing `xrGameSpy.dll` is ignored.

Sources: [level tick and Lua GC](../../src/xrGame/Level.cpp), [level loading](../../src/xrGame/Level_start.cpp), [client loading](../../src/xrGame/Level_network_start_client.cpp), [save storage](../../src/xrGame/alife_storage_manager.cpp), [AI cross-table](../../src/xrEngine/AI/game_level_cross_table.h).
