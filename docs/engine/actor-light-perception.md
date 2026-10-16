# Actor light perception

Modder documentation: [Russian](../docs/main/ru/gameplay/equipment/actor-light-perception.md), [English](../docs/main/en/gameplay/equipment/actor-light-perception.md). Those pages cover installation, LTX/DLTX examples, parameter semantics and manual verification; this page describes the engine integration.

`ActorLightPerception` implements the Gunslinger G08 detection rules in `xrGame`. It belongs to `CActor`. Stalkers and other creatures use the hook in `CMemoryManager::update`; base monsters use `CBaseMonster::shedule_Update` before the inherited update and native memory/decision processing. The general hook excludes base monsters to avoid duplicate delivery. The scheduled hook still supplies evidence under script control, while existing control rules determine actions. It supplements ordinary visual recognition and does not change `object_luminocity`, renderer lights or shader constants.

## Configuration and activation

The feature is enabled when `[gunslinger_base]` exists in the merged settings. Games without that section retain their normal perception. The five distances are read once in `CActor::Load`, including addon/DLTX overrides. Restart the session after changing them.

| Key | Engine default, m | Gunslinger resource profile, m | Meaning |
| --- | --- | --- | --- |
| `weapon_torch_treasure_dist` | 45 | 45 | Weapon light illumination cutoff |
| `headlamp_treasure_dist` | 45 | 45 | Headlamp illumination cutoff |
| `lefthanded_torch_treasure_dist` | 20 | 20 | Hand light illumination cutoff |
| `light_palevo_dist` | 20 | 10 | Observer-to-point distance that reveals the actor |
| `light_see_dist` | 60 | 60 | Observer-to-point distance that creates suspicion |

Missing keys use engine defaults through the standard `READ_IF_EXISTS` path. Distances must be finite and non-negative. An illumination cutoff of zero prevents distant beam hits but does not hide an active light source. Zero notice/reveal distances disable the corresponding point-distance branch except at coincident points; direct illumination has its own cutoff.

The resource profile already exists in `GunslingerModBase/configs/gunslinger_params.ltx`, shared by the Gunslinger game addons. No duplicated settings or Lua callbacks are required.

## Detection rules

The first active source wins, matching the original order: headlamp in `TORCH_SLOT`, attached hand device, active weapon. Hand and weapon lights must have an installed, enabled `THudLightTorch`, a created light and a non-hidden HUD state. Lasers are not torch sources. Simultaneous sources do not add their distances together.

As in `ActorUtils.pas::IsObjectTorched`, the beam follows the actor camera axis, rather than the animated flashlight bone or full spotlight cone. The first blocking hit is backed off by 1%. If it is the observer and is within the source's illumination cutoff, the actor is revealed, including hits from behind the observer.

Otherwise the observer checks the illuminated hit point and the source position. The observer origin is its object position plus 2 m, with its object forward direction; this deliberately preserves the original head approximation and forward-hemisphere rule. Each point requires an unobstructed trace. A hit range multiplied by 1.01 must reach the point, matching the original tolerance. Static geometry accumulates `fVisTransparencyFactor` and blocks when transmission falls to 0.34 or below. Dynamic objects block immediately, with two ownership exceptions: the emitter/observer and their attached objects are excluded from their outgoing rays; hitting the actor's shell when tracing the source counts as reaching the source, not as an obstruction. Walls still block both camera-to-lit-point and observer-to-point visibility. A visible source can trigger detection when its illuminated point is hidden.

Within `light_palevo_dist`, a visible point reveals the actor. Otherwise, within `light_see_dist`, it produces suspicion. The stronger result wins. These distances are measured from the observer to the point, not necessarily to the actor. Seeing a nearby spot can reveal a more distant actor, matching Gunslinger. A ray that hits nothing does not create an illuminated point in empty space.

## Memory integration

The original patch injected fake ambient/weapon sounds with power 100; its stronger result used the shooting flag. This port writes evidence directly to native memory, bypassing `feel_sound_new`, acoustic thresholds and the current monster gunfire panic/investigation policy. It also queues the standard `CScriptEntity::sound_callback` event: weak evidence uses `SOUND_TYPE_WEAPON | SOUND_TYPE_AMBIENT` (`WPN_torch` in `xr_hear`), strong evidence uses `SOUND_TYPE_WEAPON_SHOOTING` (`WPN_shoot`). As in Gunslinger, the event source is the actor, the delivered position is the observer's position, and power is 100. No Gunslinger functor lookup is introduced; the existing script event queue remains responsible for scenario callbacks.

- Stalkers receive ambient evidence associated with the actor; the reveal result also inserts zero-damage hit memory through the record overload. It does not dispatch a damage callback or force an entry into visual memory. Existing combat registration, enemy selection and relations continue to apply.
- Base monsters receive an interesting `ActorLight` memory stimulus for suspicion, positioned at the visible point, without acquiring an enemy from that weak result. `CBaseMonster::NoticeActorLight` also supplies `Behavior.Attention` and a 1500 ms look interval to the new behavior, independently of its acoustic accumulation settings. Existing combat, panic and external control retain priority. Revealing uses `EnemyMemory.RememberLightEnemy`: fresh evidence resets the inspected/unreachable old-point suppression, records the actor position/vertex/time with evidence `LIGHT`, and alerts a dog's home squad. Existing navigation restrictions and memory expiry still apply. No permanent visual contact is created.

This intentionally adapts the original fake-sound transport: weak monster evidence has an explicit native type because the old generic ambient flags are discarded by `ConvertSoundType`; revealing a monster target does not depend on hearing radius or its acoustic policy. It does not invoke Gunslinger script functors.

Only live hostile observers on the authoritative simulation participate. Dead/destroying actors, ignored actors, peaceful monsters and a camera viewing another entity do not produce this reaction. `aiIgnoreActor` is respected where available. Disabled visual memory does not block this independent light stimulus. Normal AI script control and enemy-selection rules remain responsible for choosing the eventual action.

Scripted bloodsuckers in `GunslingerQuests/configs/scripts/zaton/zat_b38_sleeper_bloodsuckers.ltx` explicitly disable visual memory while forcing a standing sleep animation. Their `sleep_readytofight` scheme handles `WPN_torch` and `WPN_shoot`, gates awakening on `zat_b38_suckercanfight`, and sets `zat_b38_bloodscukers_sleepers_stand_up`. Its normal `on_info` transition releases the sleep animation, restores vision and selects the fighting scheme. Bypassing disabled observers or omitting the standard sound event prevents that transition. The engine must not clear their animation or set those quest flags itself.

## Caching and lifetime

Configuration is stored on the actor. Source selection and the camera ray are cached for one `Device.dwFrame`, shared by all observers. The ray is bounded by the illumination cutoff (with the original 1% adjustment), up to the original 1000 m maximum. Observer distance and direction checks precede their traces. The trace result buffer reuses its allocation; the snapshot stores an object ID rather than a retained object pointer. `reinit`, `net_Destroy` and `net_Relcase` invalidate it. Detection performs no renderer light query, functor lookup or config lookup. A positive result queues the stock sound event, drained by `CScriptEntity::process_sound_callbacks`; the base-monster scheduled hook runs before that drain.

## Diagnostics

On R4, enable `monster_logic_draw on` and aim the HUD ray at the tested observer. While it is targeted, the log receives at most one `[actor_light]` line per second. Stages distinguish missing `[gunslinger_base]`, observer filters, `ai_ignore_actor`, no active source and the visibility result. `result=0/1/2` means none/suspicion/revealed. `source_range` is the selected source cutoff; `beam_hit` is the first blocker ID (`65535` means no dynamic blocker), and `spot` indicates a valid illuminated point. The server/view/hostile/ignored flags help identify a filtered observer; `vision` is informational and `vision=0` does not prevent light detection. Revealed monster memory is labelled `LIGHT` in the existing behavior debug display. Disable with `monster_logic_draw off`.

## Manual acceptance

Build and gameplay validation are performed by the integrator. For every case, isolate one source at a time: headlamp, hand light and weapon light. Use a hostile stalker and a base monster in darkness, outside their ordinary close-detection radius, with no preceding combat or sound memory.

1. Switch the light off: ordinary perception must remain unchanged. Switch it on and illuminate the observer directly, including from behind; compare hits just inside/outside the corresponding illumination cutoff.
2. Hide the actor/source behind a corner and illuminate a wall visible to the observer. Compare observer-to-spot distances below/above `light_palevo_dist`, between the two thresholds, and above `light_see_dist`. Suspicion must not directly insert an enemy snapshot; the nearer result must reveal the actor.
3. Hide both source and illuminated point behind opaque geometry. No G08 evidence should be added. Separately expose the source while hiding the point; source detection must still work.
4. Turn the observer away from the spot/source; indirect detection must stop. Check transparent and opaque materials, a moving blocking object, and aiming into empty sky.
5. Enable several sources together to verify headlamp > hand > weapon priority. Hide/holster a hand or weapon light, interrupt it through the existing electronics failure path, and detach the source.
6. Repeat with friendly/neutral NPCs, peaceful/dead monsters and ignored actor. Separately check scripted sleepers with disabled visual memory: light must still queue `on_sound`; in the Zaton lair, wake-up occurs only when the scenario has entered its ready-to-fight scheme and the existing quest condition is satisfied. Check several observers in the same frame, actor death, object destruction, save/load and level transition.
7. With the new monster acoustic policy enabled, verify that torch detection does not enter its shot-noise panic/accumulation path. Actual gunfire must retain its existing behavior. Existing AI memory can persist after the light is switched off; wait for expiry or restart the encounter before comparing cases.
