# Inventory Volume Mechanic Reference

## Scope

This document describes the inventory volume mechanic and its runtime logic.
The mechanic adds a second constraint on top of the regular weight system.
Every item has its own volume value, the worn outfit and backpack define available capacity.
When the player exceeds base capacity, soft penalties start scaling up.
When the player reaches the hard overload limit, sprint and pickup are blocked.

Single line core formula:

overload = volume_in_ruck / capacity

Where capacity is composed of base actor volume, outfit overrides and backpack overrides.

## Activation

The mechanic is disabled by default. It is activated by a single flag in `gamedata/configs/engine_external.ltx`:

```
[gameplay]
EnableInventoryVolume = true
```

When `false`, the system never reads the config, never computes volume and never applies penalties. All API calls return neutral values. UI ignores volume elements even if they are present in xml.

## Configuration Location

All parameters live in a single file:

```
gamedata/configs/volume_system.ltx
```

The file supports DLTX overrides. Mods can override individual sections and values via `mod_volume_system_*.ltx` files placed next to the base file.

## Configuration Sections

### `[volume_system]`

Base system parameters.

```
base_actor_volume                  = 56.0
default_weight_volume_multiplier   = 1.0
hard_overload_limit                = 1.4
block_pickup_at_hard_limit         = true
recursive_container_volume         = false
```

- `base_actor_volume` is the initial actor capacity without backpack and outfit. If 0, the engine falls back to `[inventory] max_ruck` from `system.ltx`.
- `default_weight_volume_multiplier` is the multiplier for items without explicit volume. Volume is computed as `Weight() * default_weight_volume_multiplier`.
- `hard_overload_limit` is the upper overload boundary. When `overload >= hard_overload_limit`, pickup block triggers and soft penalties reach their maximum.
- `block_pickup_at_hard_limit` if `true`, `CanAddToRuck` returns `false` once the hard limit is reached.
- `recursive_container_volume` optional recursive accounting of attached items. See "Recursion option" below.

### `[soft_penalties]`

Base values for soft penalties. The final penalty value is multiplied by a `smoothstep` curve from 1.0 to `hard_overload_limit`.

```
stamina_power_penalty              = 0.25
max_walk_weight_penalty            = 0.0
aim_sway_penalty                   = 0.0
sprint_block_factor                = 1.2
```

- `stamina_power_penalty` extra stamina drain at overload.
- `max_walk_weight_penalty` subtracted from `max_walk_weight`. Lowers the "cannot walk" threshold.
- `aim_sway_penalty` multiplier for weapon spread.
- `sprint_block_factor` binary threshold. When `overload >= sprint_block_factor`, sprint is disabled.

### `[item_volumes]`

Explicit volume values per item section. Takes top priority.

```
[item_volumes]
medkit          = 0.6
bandage         = 0.2
af_medusa       = 0.3
```

An item not listed here gets volume `Weight() * default_weight_volume_multiplier`.

### `[container_profiles]`

Named capacity profiles. Used to group items with equal capacity values.

```
[container_profiles]
outfit_light     = 56.0
outfit_medium    = 56.0
outfit_heavy     = 56.0
small_backpack   = 45.0
medium_backpack  = 61.0
large_backpack   = 80.0
```

### `[outfit_sections]` and `[container_sections]`

Outfit or backpack section to profile mapping.

```
[outfit_sections]
stalker_outfit          = outfit_medium
exo_outfit              = outfit_heavy

[container_sections]
backpack_small          = small_backpack
backpack_large          = large_backpack
```

### `[outfit_capacity_overrides]` and `[container_capacity_overrides]`

Per-section explicit capacity override. Has priority over the profile mapping.

```
[outfit_capacity_overrides]
exo_outfit              = 64.0
```

## Runtime Formula

1. Item volume:

```
volume(item) = item_volumes[section]                       if set
             | Weight(kg) * default_weight_volume_multiplier  otherwise
```

2. Current ruck volume:

```
ruck_volume = sum( volume(item) for item in m_ruck )
```

3. Capacity:

```
capacity = base_actor_volume
         | applyOutfit(outfit.section)      via outfit_sections and outfit_capacity_overrides
         | applyBackpack(backpack.section)  via container_sections and container_capacity_overrides
```

Inside `applyOutfit` and `applyBackpack` the profile mapping is applied first, then the per-section override.

4. Overload factor:

```
overload = ruck_volume / capacity
```

5. Soft penalty curve:

```
curve = smoothstep(1.0, hard_overload_limit, overload)
```

`smoothstep` is a cubic interpolation `x * x * (3.0 - 2.0 * x)` after linear remap.

6. Final penalties:

```
stamina_power_penalty  = config.stamina_power_penalty   * curve
max_walk_weight_penalty = config.max_walk_weight_penalty * curve
aim_sway_penalty       = config.aim_sway_penalty        * curve
block_sprint           = overload >= sprint_block_factor
block_pickup           = overload >= hard_overload_limit and block_pickup_at_hard_limit
```

7. Pickup check:

```
canAdd = (ruck_volume + volume(item)) <= capacity * hard_overload_limit
```

## Gameplay Behavior

- Below 1.0: no effects.
- From 1.0 to `hard_overload_limit`: stamina drains faster, weapon sway grows, walk speed can be limited.
- At `overload >= sprint_block_factor`: sprint is disabled instantly.
- At `overload >= hard_overload_limit`: maximum soft penalties are reached, new items are rejected if `block_pickup_at_hard_limit = true`.

All penalty values are returned via the `SInventoryVolumePenalty` struct:

```
overloadFactor
curve
staminaPowerPenalty
maxWalkWeightPenalty
aimSwayPenalty
blockSprint
blockPickup
```

## Applies To Player Only

The inventory capacity system applies only to the player (`CActor`). NPCs, merchants, monsters, and machines use the classic logic without a capacity limit. This is a deliberate choice at this stage:

- NPCs are spawned through gulag and combat logic that hands them specific weapons and gear. Applying a volume cap would break that flow and force NPCs into the default pistol fallback.
- Trade dialog with an NPC partner is not blocked by partner-side volume. The player side keeps the standard volume check.
- Stalker pickup, looting and dead-body inventory transfers stay vanilla.

Implementation: `CInventoryVolumeSystem::CanAddToRuck` and `GetPenalty` check `cast_actor()` on the inventory owner and immediately return neutral values for non-actors. The virtual dispatch already exists in the `CInventoryOwner` hierarchy, no additional setup is required.

## Recursion Option

`recursive_container_volume` is an optional accounting of child items via `CAttachmentOwner::attached_objects()`.

Double-count protection:

- If an item has an explicit volume in `[item_volumes]`, that value is final. Recursion does not run for it.
- If volume is computed from weight, the volumes of child items are added on top.

Limits:

- Maximum recursion depth is 8.
- The recursion code is never called while the flag is false.
- Only items implementing `CAttachmentOwner` are traversed.

In a vanilla build without container mods, recursion contributes 0, since standard items do not have `attached_objects` inside `m_ruck`. The option is intended for mods that add real container items.

## Inventory UI

All volume UI nodes are optional and independent. You can plug in any combination of them or none at all if you only want the gameplay modifier without UI feedback.

Two layouts are supported simultaneously: nested inside an `actor_weight_row` container, or flat at the root of the menu. The engine picks the active branch via `NavigateToNode`.

### `actor_weight_row` container layout

Recommended for mods that ship a custom inventory frame and want to group "weight plus volume" into a single visual row.

Supported child nodes:

- `actor_weight_caption` weight label.
- `actor_weight` current weight text.
- `weight_status_bar` weight progress bar. When present, replaces `actor_weight`.
- `actor_weight_max` maximum weight text.
- `volume_caption` volume label.
- `volume_status_bar` volume progress bar, bound to overload factor `ruck_volume / capacity`.
- `actor_volume` current volume text, format `"15.5"`.
- `actor_volume_max` maximum volume text, format `"/ 50.0"`.

### Flat root layout

If `actor_weight_row` is absent, the engine reads the same nodes from the root of `actor_menu`. This stays compatible with the legacy three-element weight layout and lets you add only the volume nodes you need:

- `actor_weight_caption`, `actor_weight`, `actor_weight_max` for weight.
- `volume_caption`, `volume_status_bar`, `actor_volume`, `actor_volume_max` for volume.

### Tooltip

Hovering `weight_status_bar` or `volume_status_bar` shows a tooltip with weight, volume, capacity and overload values.

### Base override files

```
gamedata/configs/ui/mod_actor_menu_volume.xml
gamedata/configs/ui/mod_actor_menu_16_volume.xml
```

The 4:3 file adds a complete `actor_weight_row` with the volume label and bar via `override="add"`. The 16:9 file adds `volume_caption` and `volume_status_bar` directly at the root, without a wrapper, so it cleanly extends the existing legacy weight block.

### C++ architecture

UI initialization is split into two separate functions:

- `CUIActorMenu::InitActorWeightSection` builds only the weight nodes.
- `CUIActorMenu::InitActorVolumeSection` builds only the volume nodes, attaches them to `actor_weight_row` if it was created, otherwise to the menu root.

Both functions are invoked one after the other in `UIActorMenuInitialize.cpp`. Extending the weight block or the volume block is done in the corresponding function and does not touch the other.

## HUD overload indicator

In addition to the inventory UI, the system supports a HUD icon analogous to `indicator_overweight`, `indicator_radiation`, `indicator_bleeding`.

Node: `indicator_overvolume`.

Display logic:

- Hidden when the volume system is disabled or `overload < 0.9`.
- `0.9 <= overload < 1.0`: texture `ui_inGame2_circle_overvolume_yellow` - "running out of space" warning.
- `1.0 <= overload < 1.25`: texture `ui_inGame2_circle_overvolume_orange` - soft overload, stamina and aim penalties active.
- `overload >= 1.25`: texture `ui_inGame2_circle_overvolume_red` - close to the hard limit, sprint and pickup blocks are imminent.

Textures are described in `gamedata/configs/ui/textures_descr/ui_add_indicators.xml` and require no extra atlas authoring.

Wire it up via:

```
gamedata/configs/ui/mod_maingame_volume.xml
gamedata/configs/ui/mod_maingame_16_volume.xml
```

The node is added to the root of `maingame.xml` with `override="add"`. If the file is not included, the indicator is simply not created, the engine does not complain.

## Lua API

`CScriptGameObject` methods for the actor or an inventory owner:

```
get_inventory_volume()           returns current ruck volume
get_inventory_volume_capacity()  returns current capacity
get_inventory_volume_overload()  returns overload factor
get_item_volume(item)            returns volume of a specific item
```

When the system is disabled, all methods return 0.

## Recommended Safe Ranges

These ranges are practical recommendations, not hard engine limits.

1. `base_actor_volume` from 40.0 to 70.0
2. `hard_overload_limit` from 1.2 to 1.6
3. `sprint_block_factor` from 1.05 to `hard_overload_limit`
4. `stamina_power_penalty` from 0.0 to 0.5
5. `aim_sway_penalty` from 0.0 to 0.5
6. Item volumes in `[item_volumes]` from 0.05 to 80.0
7. Backpack profiles in `[container_profiles]` from 40.0 to 120.0

## Minimal Activation Steps

To enable the mechanic you need to:

1. Set `EnableInventoryVolume = true` in `gamedata/configs/engine_external.ltx`.
2. Make sure `gamedata/configs/volume_system.ltx` is present.
3. Optionally include the inventory UI override files: `mod_actor_menu_volume.xml` and `mod_actor_menu_16_volume.xml`.
4. Optionally include the HUD indicator override files: `mod_maingame_volume.xml` and `mod_maingame_16_volume.xml`.
5. Optionally tune soft penalties, `hard_overload_limit` and outfit / backpack capacities for your gameplay.

Steps 3 to 5 are optional. Without UI and HUD overrides the mechanic still runs as a pure gameplay modifier and shows up only through stamina drain, weapon sway, sprint and pickup blocks under overload.
