# Inventory volume

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

The system compares item volume in the backpack with the actor’s capacity. Enable `EnableInventoryVolume = true` in the `[gameplay]` section of `gamedata/configs/engine_external.ltx`. All four bundled resource sets leave it disabled. The repository ships `configs/volume_system.ltx` with CoP. For SoC, CS, and CoC, you can supply your own file with the same schema.

## volume_system.ltx settings

| Section | Key | Type | Code fallback | Bundled CoP value | Effect |
| --- | --- | --- | --- | --- | --- |
| `[volume_system]` | `base_actor_volume` | `float` | `0` | `10` | Base capacity. A near-zero or lower value falls back to `[inventory] max_ruck` when present. |
| `[volume_system]` | `default_weight_volume_multiplier` | `float` | `1` | `1` | Weight multiplier used when no explicit `[item_volumes]` entry exists. |
| `[volume_system]` | `hard_overload_limit` | `float` | `1.4` | `1.4` | Hard overload threshold relative to capacity. For example, `1.4` means 140%. Use a value above `1` for a gradual penalty curve. |
| `[volume_system]` | `block_pickup_at_hard_limit` | `bool` | `true` | `true` | Prevents adding an item to the actor’s backpack if the projected volume exceeds the limit. The check skips ammo boxes and items already in the backpack. |
| `[volume_system]` | `recursive_container_volume` | `bool` | `false` | `true` | Adds attached child item volumes when the item’s own volume is calculated from weight. Traversal is limited to eight levels. |
| `[soft_penalties]` | `stamina_power_penalty` | `float` | `0.25` | `0.25` | Maximum increase in the load used for stamina calculations. |
| `[soft_penalties]` | `max_walk_weight_penalty` | `float` | `0` | `0` | Maximum amount subtracted from the weight limit for walking. |
| `[soft_penalties]` | `aim_sway_penalty` | `float` | `0` | `0` | Maximum addition to the actor’s dispersion multiplier, including while aiming. |
| `[soft_penalties]` | `sprint_block_factor` | `float` | `1.2` | `1.2` | Disables sprinting when the volume-to-capacity ratio reaches the threshold. The check applies above 100% capacity. |

Above 100% capacity, penalties rise smoothly up to `hard_overload_limit`. Pickup checks use projected volume after adding the item. Movement penalties use the current volume.

## Item volume and equipment capacity

In `[item_volumes]`, each key is an exact item section name and its value is the item volume. For equipment, `[container_profiles]` defines named capacities. The `[outfit_sections]` and `[container_sections]` tables map outfits and backpacks to those profiles.

Capacity starts with the base value, then applies the outfit profile and its `[outfit_capacity_overrides]` entry, then the backpack profile and its `[container_capacity_overrides]` entry. Each matching value replaces the previous capacity.

## CoP example

After enabling the feature, create `gamedata/configs/mod_volume_system_example.ltx`. This patch targets the bundled CoP `volume_system.ltx`.

```ini
![volume_system]
base_actor_volume = 35
hard_overload_limit = 1.4

![soft_penalties]
sprint_block_factor = 1.2

![outfit_capacity_overrides]
novice_outfit = 45

![item_volumes]
bandage = 0.2
```

Capacity is 35 without equipment. The rookie outfit raises it to 45 unless a backpack overrides it. A bandage occupies 0.2 volume units.

## See also

- [Engine settings](../main/engine-extension-configuration.md)
- [Configuring LTX and DLTX](../configs/ltx-examples.md)
