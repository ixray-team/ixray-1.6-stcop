# Burer combat ownership

The G09–G11 Gunslinger policy is implemented in `src/xrGame/ai/monsters/burer/burer_combat.{h,cpp}`. It is enabled only in single player with `[gunslinger_base]`. `CBurer::Load` reads configuration once; no tactical path reads INI values or resolves Lua functors.

## Owners and entry points

| Owner | Responsibility |
| --- | --- |
| `CBurer::Combat` / `CBurerCombat` | Cached configuration, actor context, grenade threat count, aim grace period, risk and tele-stop decisions |
| `CStateBurerAttack` | Computes ordinary readiness, applies the ordered Gunslinger matrix, selects a ready state; critical grenade preemption of gravi |
| `CStateBurerShield` | Sole Gunslinger shield lifetime owner; extends the same deadline used for completion |
| `CStateBurerAttackTele` | Candidate filtering, ID-based selection, validation, normal completion versus external interruption, final-throw permission |
| `CTelekinesis` | Owns every controller; preserves thrown controllers during normal attack cleanup and destroys invalid controllers without leaking or skipping successors |
| `STelekineticObject` | Owns its contact callback registration; detaches it before destruction and restores held-object gravity |
| `STelekineticWeaponObject` | Restores weapon fire mode/initiator once and stops weapon fire on release/destruction |
| `STelekineticGrenadeObject` | Preserves active fuses, optionally activates inactive grenades, tracks time since capture/activation without unsigned subtraction from an assumed fuse length |
| `CActor::BurerEffects` | Shared actor camera lock, native blood overlay, knife window and 100 ms stamina recovery period |
| `CLevel::SecretItems` | Native protection metadata keyed by `ALife::_OBJECT_ID` |

`Refresh` caches a context by game timestamp. The context samples actor activity, vision, weapon readiness and active grenade threats, including held grenades through a single spatial query. It does not aggregate candidates and controllers into duplicate counts. Each burer owns its tactical timers; there are no Pascal-style shared combat globals.

The readiness matrix samples probabilities only when selecting an attack. Shield risk and random telekinesis termination use 500 ms decision intervals. `check_start_conditions` itself does not sample attack-selection probabilities. A new telekinesis session resets its stop decision.

## Native actor reactions

`anti_aim_ability` has an opt-in native burer presentation mode. Hit delivery uses an explicit `HitDelivered` flag, separate from camera effector allocation. This guarantees one hit at the animation hit time even when the actor camera reaction is already active. Other monsters retain the existing configured camera presentation.

The actor owns a dynamically allocated camera effector ID. A native removal delegate clears ownership when the effector ends; the ID is not retained after it can be reused by another feature. Ordinary reactions block additional reactions; a risky reaction permits replacement by a normal reaction. `Die` and `net_Destroy` reset actor reaction state. Available animation files are cached by `CActor::Load`.

`CCameraManager` lookup and explicit removal now include pending effectors. ID allocation therefore sees deferred additions, and resetting a pending burer reaction actually cancels it. Deferred installation removes only installed effects, preserving its iteration. The existing public removal signature is unchanged.

`CWeaponKnife::StartBurerSelfKick` requires a loaded `anm_selfkick`, an idle nonpending knife and no controller suicide. Motion marks and animation completion cannot produce a regular strike or controller death from this reaction. No Lua completion callback is involved.

## Completion and lifetime details

The monster state framework calls `critical_finalize` when selecting a different child state, including after a successful `check_completion`. Telekinesis therefore records `CompletionReached`: only a normal completed attack may perform the configured final throw. Burning, death and other outer-state interruptions release held objects directly.

Normal finalization retains `TS_THROW` controllers until collision/timeout cleanup. `GetHeldObjectsCount` counts `TS_RAISE` and `TS_KEEP`; the existing `get_controlled_objects_count` keeps its previous `TS_KEEP` semantics for other consumers.

Selected objects use `ALife::_OBJECT_ID` and `ALife::INVALID_OBJECT_ID`, following commit `d24a49f1f1`. Both the level object and the matching live controller are checked immediately before throwing. `net_Relcase` clears selection/candidate links. Object IDs are never narrowed to `u16`; bone indices and inventory commands keep their existing types.

An active grenade is eligible only above the minimum remaining fuse and without contact detonation. Capture never restarts its timer or changes its initiator. Inactive activation assigns the burer as initiator. A throw applies `min(existing_deadline, now + critical_timer - 1)`, with an underflow guard for zero configuration. Flight still uses the monster's `Tele_Fly_Velocity`.

## Treasure metadata and resources

`level.set_secret_item(id, bool)` is a write-only Lua-to-native bridge used by the CoP treasure manager at spawn, pickup and both load paths. C++ never queries the treasure manager. Loading first removes previously published IDs, then republishes the loaded map. The registry lives with `CLevel`; the existing save formats remain unchanged, including the object-ID helpers introduced by `db3fb26ff6`.

The runtime resource migration removes the unused `GunslingerModBase/scripts/gunsl_burer.script` and changes `burer_superhealthhit_type = 5` to `wound` in `GunslingerModBase/configs/gunslinger_params.ltx`. Numeric hit-type compatibility parsing is intentionally absent, matching the ordinary hit-type loader. SoC/CS container treasure requires no loose-item metadata path because parented items are excluded before pickup.

Modder configuration, fallback/resource differences and manual acceptance scenarios: [RU](../docs/main/ru/gameplay/monsters/burer-combat.md), [EN](../docs/main/en/gameplay/monsters/burer-combat.md). Compilation and in-game acceptance are performed by the user; source inspection and diff checks do not establish runtime behavior.
