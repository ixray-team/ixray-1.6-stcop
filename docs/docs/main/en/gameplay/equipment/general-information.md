# Gameplay: Inventory Items

See [Shadow of Chernobyl anomaly detectors](soc-detectors.md) for the required class ID and migration notes.

See [Night vision](night-vision.md) for shader generations, equipment profiles, the UI edge mask and electronics interference.

## Any Inventory Item (CInventoryItem)
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.3

```ini
;Allows you to set one or more inventory item sections 
;that will be highlighted when hovering over the current item with this list (similar to how ammunition is highlighted for weapons in the inventory)
highlight_related_sections = related_section_a, related_section_b
```
::: details Implementation Example

```ini
  [root_item_section]:identity_immunities ; Parent item with configured highlighting of child items
  highlight_related_sections = related_section_a, related_section_b; item sections that will be highlighted as dependent on this item

  [related_section_a]:identity_immunities; Highlighted item when hovering over root_item_section
  highlight_related_sections = root_item_section; you can add reverse highlighting of the parent item when hovering over the child if needed
  ...

  [related_section_b]:identity_immunities; Highlighted item when hovering over root_item_section
  ...
  ```
:::

## Helmet (CHelmet)

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.0

```ini
use_condition = false; Whether the item has a wear progress bar
```

> [!IMPORTANT]
> **Minimum Version**: 1.2.2
Current console commands: `r4_use_gasmask`, `r2_use_rain_drops` and `r2_rain_drops_mode`. See [Visor glass and rain drops](visor-glass.md) for glass types, item condition, accumulation and wiping animations in the current branch.

```ini
glass_present = true; The item has visor glass
hud_gas_mask_avaliable = true; Allow glass rendering
hud_rain_drops_avaliable = true; Allow rain drops rendering
hud_gas_mask_type = flat; flat (default) or curved
```

> [!IMPORTANT]
> **Minimum Version**: 1.4
```ini
physic_strike_protection = 0.7; physic strike hit protection 
```

## Armor (CCustomOutfit)

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.0

```ini
use_condition = false      ; Whether the item has a wear progress bar
forbid_change_skin = false ; Whether it changes the visual (hands/legs)
```

> [!IMPORTANT]
> **Minimum Version**: 1.2.2
Current console commands: `r4_use_gasmask`, `r2_use_rain_drops` and `r2_rain_drops_mode`. See [Visor glass and rain drops](visor-glass.md) for glass types, item condition, accumulation and wiping animations in the current branch.
```ini
glass_present = true; The item has visor glass
hud_gas_mask_avaliable = true; Allow glass rendering
hud_rain_drops_avaliable = true; Allow rain drops rendering
hud_gas_mask_type = flat; flat (default) or curved
```

> [!IMPORTANT]
> **Minimum Version**: 1.4
```ini
physic_strike_protection = 0.7; physic strike hit protection 
```
> Also supported for upgrades

## Torch (CTorch)

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.0

```ini
snd_click = device\torch_click; Optional sound for turning the flashlight on/off
```

## Backpacks (CBackpack)

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.3

* Optional inventory backpacks with their own slot

```ini
[backpack_test]:identity_immunities
GroupControlSection = spawn_group
$spawn = "devices\dev_backpack"
$prefetch = 16
cform = skeleton
visual = additions\sumka1.ogf
inv_grid_width = 2
inv_grid_height = 2
inv_grid_x = 4
inv_grid_y = 23
slot = 12
default_to_ruck = false
inv_weight = 0.5
inv_name = backpack_test_name
inv_name_short = backpack_test_name
description = backpack_test_descr
cost = 1000
can_trade = true

; Additional weight in inventory (similar to armor)
additional_inventory_weight = 15
additional_inventory_weight2 = 15

; Stamina recovery speed
power_restore_speed = 0.0

; Class of our backpack
class = EQ_BAKPK
```

## Medication intoxication

Set `EnableMedIntoxication = true` in `[gameplay]` in `configs/engine_external.ltx`. Set the actor parameters in the section selected by `condition_sect`, usually `[actor_condition]`. Set `eat_intoxication` in the consumable item’s LTX section.

| Key | Code fallback | Effect |
| --- | ---: | --- |
| `intoxication_critical` | `0.35` | Health damage threshold. The value is clamped to `0–1`. |
| `intoxication_v` | `-0.00025` | Intoxication change per second of simulation time. A negative value decreases intoxication over time. |
| `intoxication_power_v` | `-0.005` | Stamina change coefficient. The delta is scaled by current intoxication and elapsed time. |
| `intoxication_health_v` | `-0.0015` | Health change coefficient above the critical threshold. Its effect increases as intoxication rises. |
| `intoxication_thirst_k` | `0.3` | Intoxication reduction factor for hydration. Applies when thirst is enabled and `eat_thirst` is positive. |
| `eat_intoxication` | `0` | Intoxication delta when the item is consumed. The resulting level is clamped to `0–1`. |

Example DLTX for the actor and radioprotector:

```ini
![actor_condition]
intoxication_critical = 0.35
intoxication_v = -0.00025

![drug_radioprotector]
eat_intoxication = 0.15
```

With the mechanic enabled, one dose adds `0.15` before any other item adjustments. The global switch is stored separately from these sections.

## Artefact modifiers

The artefact section supplies these floating-point keys. The engine sums contributions from artefacts on the belt and weights each one by its condition.

| Key | Fallback | Contribution of one artefact |
| --- | --- | --- |
| `equipment_durability_modifier` | `1` | Adds `(value − 1) × condition` to the equipment wear multiplier. |
| `inventory_weight_modifier` | `1` | Adds `(value − 1) × condition` to the weight multiplier. |
| `jump_height_modifier` | `0` | Adds `value × condition` to the jump modifier. |
| `movement_speed_modifier` | `0` | Adds `value × condition` to the movement modifier. |

For example, `inventory_weight_modifier = 0.9` contributes `−0.1` at full condition and `−0.05` at half condition.

The `degrade_rate` key multiplies condition loss. Its fallback of `0` disables wear in these calculations. For example, `degrade_rate = 0.5` halves the calculated condition loss.
