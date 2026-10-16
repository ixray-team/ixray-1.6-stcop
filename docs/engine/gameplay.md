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

## Depth of field

`CGamePersistent` owns the single `CDofManager` (`DofManager.*`). The manager selects the active profile, manages its lifetime, interpolates the final DOF and restores the base value. There is no separate HUD overlay, camera DOF effector, or second interpolation in `CGamePersistent`. Its public legacy DOF methods only forward requests to the manager.

`SHudDofDesc::Registry` caches immutable profiles once per section. Loading a HUD, item, animator or scope stores a descriptor pointer in `SHudDofSource`. Each descriptor scans the section's animation keys once, resolves action defaults and the `anm_use`/`anm_show` compatibility alias, and records explicit fields and enable state in `Flags8`. Starting an animation performs a cached alias lookup and records its start time and duration; neither animation start nor frame update reads configs, formats config keys, or probes for parameter existence. The source is passive data, not another controller. Shared descriptors are cleared with the existing descriptor registries during game DLL shutdown.

Priority is dialogue focus, anomaly ray focus, the active HUD animator, weapon action/reload, then aiming. An external effector request and the configured base DOF are fallback targets. Dialogue focus is calculated in the manager from the current actor's talk partner; UI code only controls its own FOV. Zones register their IDs, so leaving one overlapping zone does not cancel another zone's DOF. Cancelling or detaching an animation clears its source; level shutdown resets manager state.

| Command | Default | Effect |
| --- | --- | --- |
| `r2_dof_enable` | renderer preset | Renderer master switch; reload the shader variant after changing it |
| `r2_dynamic_dof` | `false` | Weapon action profiles |
| `r2_const_zoomdof` | `false` | Fixed iron-sight, alternate-sight and grenade-launcher profiles; otherwise use ray-picked focus |
| `enable_dof_reload` | `true` | Cached legacy `reload_dof` profile when dynamic weapon DOF is off |
| `enable_dof_hud_animator` | `false` | Usable-item and PDA profiles, independently of weapon actions |
| `enable_dof_talk` | `true` | Focus relative to the conversation partner |
| `r2_dof_kernel` | `5` | Global blur radius |
| `r2_dof_lens_kernel_scale` | `2` | Blur-radius multiplier outside a rendered 3D lens, from 1 to 4; fades with aiming |
| `r2_dof_sky` | `30` | Sky focus distance |
| `r2_dof_near/focus/far` | `-1.25, 1.4, 600` | Base profile, saved together by `r2_dof` |

3D lenses use a fixed lens profile in either zoom mode. Ordinary collimators and optical scopes do not enable ray-picked focus. `disable_zoom_dof` in the weapon or HUD section suppresses its zoom profile. Native usable-item animators use the manager for both new animation profiles and legacy `dof = near, focus, far, release_time` fallback values.

The `[gunslinger_base]` defaults are `default_zoom_dof_near/focus/far` (0.5, 0.8, 100), `default_action_dof_near/focus/far` (0, 0.5, 5), `default_dof_speed` (5), `default_dof_speed_in/out` (3/1), and `default_dof_time_offset` (-0.4 seconds). Speeds are interpolation rates in inverse seconds. Zero holds the current focus. Non-finite values fall back to defaults; inverted near/focus/far profiles are rejected. Ray focus offsets are cached from `[zone_pick_dof]`. An explicit resource value overrides the engine default. Fixed zoom profiles focus at the configured HUD distance, often 0.5 meters; reducing their far distance can blur the entire landscape. For iron sights that keep the aimed world target sharp, use `r2_const_zoomdof false`: the manager picks the target distance from the HUD ray and uses the cached near/far offsets around it. Keep `r2_dynamic_dof` enabled for weapon actions. Lens profiles keep their own `lens_dof_*` values in either zoom mode.

HUD zoom keys are `zoom_dof_near/focus/far`, `alter_zoom_dof_*`, `gl_dof_*`, and `lens_dof_*`. Alternate zoom inherits the normal profile; other weapon profiles inherit the global zoom defaults. Cached field flags let an attached scope override only its specified lens values. Transition rates are `zoom_in_dof_speed`, `alter_zoom_in_dof_speed`, `gl_in_dof_speed`, and `zoom_out_dof_speed`. PDA animators hold DOF while open; their `zoom_dof_*` overrides inherit the global action profile so the background blurs without a separate zoom action.

Actions use the resolved animation alias, including suffixes: `use_dof_<alias>`, `dof_<alias>_near/focus/far`, `dof_speed_in_<alias>`, `dof_speed_out_<alias>`, and `dof_time_offset_<alias>`. Reload/open/close/add-cartridge aliases and native usable-item animators default to enabled. Other motions require `use_dof_<alias> = true`. Native `anm_show` profiles accept per-field overrides from the old `anm_use` profile, including an explicit disable flag.

The time offset controls release: positive values count from animation start, negative values count back from animation end, and zero keeps DOF for the full duration. The deadline is capped by actual playback duration. Looped motions remain active until stopped unless they specify a positive offset.

R4 writes an R8 lens coverage mask during the existing scope depth-mask pass. Final DOF leaves the lens sharp and excludes it from surrounding blur taps. Clear the mask to zero (`ERTColor::Transparent`); the existing D3D11 `ERTColor::Black` value is white. The lens multiplier modifies the existing `dof_kernel` constant on the CPU. No extra shader uniform or constant buffer is exported for strength or focus.

Final DOF reads raw G-buffer depth (`r2_RT_P`) with point sampling. HUD pixels are identified in the reserved 0..0.02 viewport range and assigned the current focus distance: weapons, hands and HUD items remain sharp and do not contribute color to surrounding scene blur. Scene depth still reconstructs distance through the existing projection matrix. DOF does not depend on the temporal R16 depth target or on motion vectors; no extra textures or shader constants are needed for HUD exclusion.

Advanced video options in all four distributions (`gamedata`, `gamedata_soc`, `gamedata_cs`, `gamedata_coc`) call the same `ui_mm_dof_options` module. Each package includes its copy, Russian and English strings, and the matching DOF/lens shaders. The section exposes every DOF console setting. Disabling the master preserves subordinate settings; base-distance sliders preserve profile ordering during apply/cancel. Per-item animation tuning remains in HUD configs.

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
