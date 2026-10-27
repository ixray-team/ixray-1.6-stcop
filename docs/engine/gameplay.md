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

Gunslinger actor-light detection supplements visual recognition through native memory evidence. Modder documentation: [Russian](../docs/main/ru/gameplay/equipment/actor-light-perception.md), [English](../docs/main/en/gameplay/equipment/actor-light-perception.md). Engine integration and caching: [Actor light perception](actor-light-perception.md).

ALife: offline = `CSE_*` only, online = client entity. `CALifeUpdateManager` spends a per-frame budget. Switch distance is on the simulator, not `CLevel::OnFrame`.

## Lua

`CScriptEngine` (`src/xrScripts/script_engine.h`): one LuaJIT state, `CScriptProcess` list. `CLevel::OnFrame` queues `CLevel::script_gc`; it updates the level process on `GameThread`.

Export: `DECLARE_SCRIPT_REGISTER_FUNCTION` plus `*_script.cpp`. Common scripts: `gamedata/scripts`. Addon init: `AddonInfo::ScriptInit`. Mod callbacks: `[callbacks]` in `game_global.ltx`. IXR framework frame hook: `IGame_Persistent::ixr_framework_onframe`.

Luabind allocations use `Memory` in game mode; the Lua VM is created with `luaL_newstate()`. Level GC is `lua_gc(..., LUA_GCSTEP, psLUA_GCSTEP)` in `CLevel::script_gc` on `GameThread`. `IXRAY_LDEBUG` sets `LUABIND_DEBUG_SCRIPTS`; LuaPanda is loaded in `lua_ext.cpp` when `socket.lua` loads successfully.

## PDA statistics

Modder reference: [PDA statistics](../docs/main/ru/gameplay/general/pda-statistics.md). This applies to the common `gamedata` scripts; the CoC, CS and SoC script sets have separate implementations.

`xr_statistic.set_best_weapon` groups positive hit callback amounts by full item section, with one optional `parent_section` redirect. The guard uses the native `game_object:is_weapon()` predicate (firearms, melee weapons and hand grenades), independent of Lua class tables initialized by a startup callback. Attribution still uses the actor's active item at impact. The stock death callbacks also contribute a synthetic hit of amount `1`.

`CUIRankingWnd` reads the favorite icon's `use_3d` XML attribute, defaulting to `Enable3DIcons`. The 3D model path does not require upgrade icon coordinates. Missing or unsuitable models fall back to the 2D upgrade atlas when all four coordinates are available; stale models are released before that fallback.

`set_best_monster` stores the exact ALife section and stock fallback artwork profile. `misc/pda_statistics.ltx` optionally overrides the comparison rank and each UI texture independently. Missing texture keys select the stock fallback; empty keys disable that layer in `CUIRankingWnd`. Overrides do not alter the creature's ALife rank or retroactively recompute saved statistics.

`CGrenade::Throw` records the thrower as the explosion initiator. `CGrenade::Destroy` preserves it; `CExplosive::Initiator` falls back to the explosive's own ID when no initiator exists. Explosion event packets and blast hits carry that initiator, allowing the death callback to credit player kills. This does not resolve the separate favorite-weapon attribution limitation.

Both `save`/`load` and `Serialize` persist the section, comparison rank, fallback profile and weapon counters. The legacy weapon count uses `u16`. Earlier statistics save layouts are unsupported; start a new game.

## Actor weapon modifiers

`CActor::WeaponModifiers` owns nine coefficients from `WeaponsModifiers.h`, initialized to `1.0`. Lua reads them through `actor:weapons_modifiers(modifier)` and assigns them through `actor:weapons_modifiers(modifier, value)`, with a `weaponsModifiers.*` constant as `modifier`; the getter and setter bind `CScriptGameObject::GetActorWeaponModifier` and `SetActorWeaponModifier`. String lookup is not exposed. See the [modder guide](../docs/main/en/weapon-system/actor-weapon-modifiers.md) for all constants and examples.

`CActor::save(NET_Packet&)` writes an `ActorWeaponModifierSerialization` block after actor statistics. `CActor::load(IReader&)` reads it at the same position. `CGameObject::net_Save` calls actor serialization before `CScriptBinder::save`; `net_Load` restores actor data before `CScriptBinder::load`.

| Field | Encoding |
| --- | --- |
| Signature | `u32`, `0x444F4D57` (`WMOD`) |
| Coefficient count | `u32`, currently `9` |
| Coefficients | One `float` per ID, in `WeaponModifier` order |

The current block is 44 bytes. It uses the packet's scalar writers, not a raw copy of the C++ struct. Enum IDs are part of both the Lua API and the save format: append new values before `Count`; do not reorder or reuse existing IDs.

The loader resets coefficients to neutral values before reading. With less than a complete header it reads nothing; when the signature is absent it restores the reader position so the script binder can read its own data. This supports an otherwise compatible actor save without the modifier block. Older engines without this reader cannot consume the new block.

Stored coefficients pass through `SetCoefficient`; invalid values remain `1.0` and are logged. Missing entries retain defaults; additional entries are consumed but ignored. A declared count requiring more bytes than remain in the reader triggers `CHECK_OR_EXIT`, including in Shipping. The count does not make arbitrary changes to the block encoding compatible.

Sources: [actor save/load](../../src/xrGame/Actor_Network.cpp), [coefficient definitions and validation](../../src/xrGame/WeaponsModifiers.h), [Lua bindings](../../src/xrGame/script_game_object_script3.cpp), [binder serialization order](../../src/xrGame/GameObject.cpp).

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
