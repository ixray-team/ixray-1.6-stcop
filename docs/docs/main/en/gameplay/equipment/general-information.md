# Gameplay: Inventory Items
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
* Console commands r_use_gasmask and r_use_rain_drops

```ini
hud_gas_mask_avaliable = true; Whether gas mask rendering is available
hud_rain_drops_avaliable = true; Whether rain drops rendering is available
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
* Console commands r_use_gasmask and r_use_rain_drops
```ini
hud_gas_mask_avaliable = true; Whether gas mask rendering is available
hud_rain_drops_avaliable = true; Whether rain drops rendering is available
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
