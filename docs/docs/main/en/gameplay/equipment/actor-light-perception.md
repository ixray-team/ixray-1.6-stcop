# AI reaction to the actor's torch

> [!IMPORTANT]
> **Availability**: the current branch with the G08 implementation in `xrGame`.
> **Last updated**: 2026-10-16.

## Overview

Hostile NPCs and monsters can notice the actor through an active torch: a beam aimed directly at the observer, a visible illuminated point, or a visible source. Headlamps, hand torches and weapon torches are supported.

The two results are **suspicion** and **revealing the actor**. Suspicion supplies evidence for an AI reaction; revealing also supplies the actor's location. Normal NPC/monster logic decides whether to look around, investigate or attack. This feature supplements ordinary vision and does not require Gunslinger scripts.

## Installation and configuration

Run an updated `xrGame.dll` matching the build configuration used by the game. The feature is enabled when the merged `system.ltx` contains `[gunslinger_base]`, including its included files, addons and DLTX overrides. Without that section, ordinary detection remains in use.

The port already defines these settings in `GunslingerModBase/configs/gunslinger_params.ltx`. Shared C++ code reads them; they do not need copies in each game addon or in individual NPC, monster or torch sections.

Settings are read when the actor loads. Restart the game after editing LTX files. There is currently no console command, menu option or dedicated `bool` switch for this feature. Do not remove the shared `[gunslinger_base]` section just to disable this reaction: it also contains settings for other port features.

### Parameters

All five parameters are `float` distances in meters. Use finite, non-negative numbers; there is no special validation or correction of invalid values.

| Parameter | When absent | Gunslinger profile | Purpose |
| --- | --- | --- | --- |
| `weapon_torch_treasure_dist` | `45` | `45` | Weapon beam illumination cutoff |
| `headlamp_treasure_dist` | `45` | `45` | Headlamp beam illumination cutoff |
| `lefthanded_torch_treasure_dist` | `20` | `20` | Hand torch beam illumination cutoff |
| `light_palevo_dist` | `20` | `10` | Observer-to-visible-point distance that reveals the actor |
| `light_see_dist` | `60` | `60` | Observer-to-visible-point distance that causes suspicion |

The `*_treasure_dist` names are retained from Gunslinger. They limit beam-based detection; they do not set rendered light range or search for stashes. Rendered light color, brightness, cone angle and range do not replace these AI parameters.

Normally `light_palevo_dist` should be smaller than `light_see_dist`. Equal values leave no separate suspicion band. If `light_palevo_dist` is larger, revealing takes precedence.

A zero `*_treasure_dist` prevents illumination of distant objects but **does not hide the active source**. The source can still be noticed through `light_palevo_dist` and `light_see_dist`. Zero values for those two parameters remove their distance branches except at coincident points; direct illumination is checked separately. Setting one parameter to zero does not disable the entire feature.

### Gunslinger profile example

In the existing `[gunslinger_base]` section:

```ini
[gunslinger_base]
weapon_torch_treasure_dist = 45
headlamp_treasure_dist = 45
lefthanded_torch_treasure_dist = 20
light_palevo_dist = 10
light_see_dist = 60
```

Only the relevant lines are shown. Preserve the existing section's other settings and inheritance when editing the original file.

### DLTX example

To reduce headlamp detection distances, create `mod_gunslinger_params_actor_light.ltx` beside `gunslinger_params.ltx`, following the [DLTX rules](../../configs/dltx.md):

```ini
![gunslinger_base]
headlamp_treasure_dist = 25
light_palevo_dist = 8
light_see_dist = 30
```

The headlamp now reveals a directly illuminated observer within 25 m. A visible point or source within 8 m reveals the actor; farther away, up to 30 m, it creates suspicion. The last two settings apply to all three source types, so they also affect hand and weapon torches. The patch assumes the original section already exists and is loaded.

## Visibility rules

### Source selection

Each update uses the first active source:

1. The headlamp in the torch slot.
2. The hand torch on an attached left-hand device.
3. The active weapon's torch.

Distances are neither added together nor selected by their maximum. An enabled headlamp, for example, takes precedence over a weapon torch. Hand and weapon sources require an installed, enabled torch component, a created light and an item state other than hidden. A laser sight is not a torch source.

### Direct illumination

A central ray starts at the actor camera and follows its viewing direction. If the observer is its first blocking hit and the hit falls within the selected source's cutoff, the actor is revealed. The observer's facing direction does not matter for this check, so a direct hit from behind also works.

This preserves Gunslinger's approximation. It does not reproduce the full spotlight cone or the animated torch bone's movement. Illumination at the edge of the rendered cone does not by itself guarantee G08 detection.

### Visible point and source

Without a direct hit, AI checks the beam's illuminated hit point within its cutoff and the source position, represented by the camera position. Each point is checked for distance, the observer's forward hemisphere and obstacles. The stronger result wins:

| Condition | G08 result |
| --- | --- |
| Point is visible within `light_palevo_dist` | Actor revealed |
| Point is visible beyond `light_palevo_dist` but within `light_see_dist` | Suspicion |
| Point is blocked, behind the observer or outside both distances | That point adds no evidence |

Distance is measured **to the checked point**, not necessarily to the actor. A nearby illuminated wall can reveal an actor farther away. A visible source can give the actor away even if its illuminated point is hidden. A ray into empty sky creates no illuminated point; the source check remains active.

Observer traces start 2 m above its object position and use its object direction. This is the original approximation, not an exact eye position for every model. The central ray hit point is moved 1% toward the actor; point visibility also has a 1% tolerance. Results at wall edges or distance boundaries can therefore differ from a visual estimate.

Opaque obstacles block the ray. Static geometry multiplies material `fVisTransparencyFactor` values and blocks when transmission reaches `0.34` or less. Dynamic objects block immediately. Outgoing rays exclude their emitter/observer and attached items. A source trace hitting the actor's body counts as reaching the source: the actor's own model must not hide the torch. Walls in front of the actor still block visibility. If both source and illuminated point are behind an opaque wall, this feature does not detect the actor through that wall.

## Reactions and limitations

NPCs receive a light stimulus in memory. Revealing also adds actor information that allows normal enemy selection to acquire the actor. For monsters, suspicion retains interest in the visible point; revealing records the actor's location and detection time in enemy memory. Revealing deals no damage and does not create permanent visual contact.

Base monsters process the stimulus in their scheduled update before memory processing and decisions. Weak evidence also supplies an attention point to the new behavior for 1500 ms; subsequent signals refresh that interval. Strong evidence is labelled `LIGHT` and cancels suppression of an already inspected actor position: an active torch supplies fresh information even after a previous search ends. Dogs also alert their home squad. Navigation restrictions and combat, panic and external-control priorities remain active.

Processing runs on the server side in compatible single-player modes for live hostile observers. It skips dead/destroying actors or observers, ignored actors and peaceful monsters. Disabled visual memory does not block the independent torch reaction. Additional detection is not calculated while the camera views another entity. The debug `aiIgnoreActor` flag is respected where available.

Original Gunslinger delivered the result through a fake sound, including a shooting flag. This implementation writes evidence directly into native AI memory and queues the standard `on_sound` event: `WPN_torch` for suspicion, `WPN_shoot` for revealing. The emitter is the actor, the delivered position is the observer's position and power is `100`, matching the original. No new Gunslinger function is looked up through Lua functors; the normal sound event queue calls the registered scenario handler. Light does not enter the new acoustic gunshot-panic path. Existing behavior priorities, script control and memory retention still apply.

### Waking scripted bloodsuckers

In the Zaton lair, `GunslingerQuests/configs/scripts/zaton/zat_b38_sleeper_bloodsuckers.ltx` disables bloodsucker vision and forces the sleep animation. The `sleep_readytofight` scheme handles `on_sound` for `WPN_torch` or `WPN_shoot`, gated by `zat_b38_suckercanfight`. The scenario sets `zat_b38_bloodscukers_sleepers_stand_up`, releases the sleep animation, restores vision and selects the fighting scheme.

Writing an enemy only into C++ memory therefore does not replace the standard event: without it, the scenario keeps the monster asleep. The engine delivers the event and preserves quest conditions; it does not force animation release or quest flags. Light does not bypass a scenario phase that has not yet allowed awakening.

Switching the torch off stops new light evidence but **does not erase existing memory**. An NPC can continue searching or fighting. Ordinary vision, hearing and other detection mechanisms also remain active.

## Manual verification

Use a dark area, one hostile NPC or monster, and one source at a time. Exclude recent combat, noise, enemy sharing and ordinary close-range detection before comparing cases. Restart the game after changing parameters.

| Check | Expected result |
| --- | --- |
| Torch off | No new G08 evidence; ordinary detection remains active |
| Central ray hits the observer within the source cutoff | Actor revealed, including a hit from behind |
| Actor hidden around a corner, spot visible within `light_palevo_dist` | Actor revealed through the visible spot |
| Spot visible between `light_palevo_dist` and `light_see_dist` | Suspicion; the weak result does not itself add an enemy to monster `EnemyMemory` |
| Both source and spot behind an opaque obstacle | No additional G08 detection |
| Spot hidden, source visible | Source detection remains available |
| Observer faces away from source and spot, with no direct hit | No indirect G08 detection |
| Beam aimed into the sky | Only the source is checked |
| Several sources enabled | Headlamp → hand torch → weapon torch priority |
| Torch switched off after detection | No new evidence; existing memory may remain |
| Scripted sleeping bloodsucker with vision disabled, scheme ready to fight | The light event reaches `on_sound`; the scenario releases sleep under its existing conditions |

Repeat for headlamp, hand and weapon torches. Also check distance boundaries, transparent materials, moving obstacles, hiding the item, actor death, loading a save and changing levels. With the new monster acoustic policy enabled, torch detection must not enter its gunshot-noise panic handler.

To distinguish G08 from ordinary vision precisely, use breakpoints in `ActorLightPerception::Update`, `CSoundMemoryManager::NoticeActorLight` and the `EnemyMemory` insertion. An attack by itself does not prove that the light reaction fired.

### Reaction diagnostics

On R4, enable `monster_debug_3d_shapes on` for geometry and `monster_debug_3d_text on` for labels. With extended logic active, the yellow light marker shows the source point, accumulated interest and investigation. Light does not add its owner to enemy memory; existing visual perception confirms enemies. Use `monster_debug_hud on` for timings and `monster_debug_hud reset` to reset them.

The `[actor_light]` console trace has been removed. Inspect filters and vanilla detection with breakpoints in `ActorLightPerception::Update` and `CBaseMonster::NoticeActorLight`.

## See also

- [General equipment parameters](general-information.md).
- [Night vision](night-vision.md).
- [DLTX configuration overrides](../../configs/dltx.md).
