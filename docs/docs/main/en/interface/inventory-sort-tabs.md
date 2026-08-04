> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.4 <br>
> **Last updated**: 2026-07-24

# Inventory sort tabs

## Overview

The tab system controls how items are displayed in the inventory, trade, and corpse windows. Two modes are available via LTX: **categories** (filter by category) and **ordering** (sort without filtering). Mode selection is config-time only. The in-memory `m_ruck` order is unchanged; only the UI list order is affected.

Hotkeys `kQ`/`kE` are also supported.

## Mode selection

In `configs/mod_system_ixray.ltx` or your mod LTX:

```ltx
[inventory_sort]
system = categories   ; categories | ordering
```

| Value | XML node | Behavior |
|-------|----------|----------|
| `categories` | `inventory_sort_tabs` | Category filter + grid-size sort |
| `ordering` | `inventory_sort_order_tabs` | Sort only, all items visible |

If the XML node for the selected mode is missing, sort tabs are not shown.

## Categories mode

1. Categories are defined in `CInventorySorter`.
2. Built-in categories: `all`, `weapons`, `ammo`, `armor`, `devices`, `consumables`, `artefacts`, `attachments`.
3. Custom categories are read from `inventory_sort_custom`.
4. Labels and icons are read from `inventory_sort_categories`.

### XML

1. Add `inventory_sort_tabs` via mod override to `configs/ui/actor_menu.xml`.
2. Optional containers for other menu modes:
   1. `inventory_sort_tabs_container_upgrade`
   2. `inventory_sort_tabs_container_trade_actor_bag`
   3. `inventory_sort_tabs_container_trade_partner_bag`
   4. `inventory_sort_tabs_container_deadbody_bag`

## Ordering mode

All items remain visible. Tabs change UI list order only.

| Tab | id | Order | Cycle / Alt |
|-----|----|-------|-------------|
| General | `general` | `GreaterRoomInRuck` (same as "All" in categories) | no |
| By type | `by_type` | Groups by `GetItemCategory`, grid sort inside each group | yes - type focus cycle |
| By weight | `by_weight` | `Weight()`, descending by default | yes - heavy / light |
| By condition | `by_condition` | `GetCondition()` for items with condition; others at the end | yes - better / worse |
| By cost | `by_cost` | `Cost()`, descending by default | yes - expensive / cheap |
| By importance | `by_importance` | `IsQuestItem()` first, grid sort inside each group | no |
| By novelty | `by_novelty` | `GetTakenTime()`, newest first by default | yes - newest / oldest |

### Direction cycle

For invertible modes (`by_weight`, `by_condition`, `by_cost`, `by_novelty`) and for the type cycle (`by_type`):

1. Click the already active tab again.
2. Press `kINV_SORT_CYCLE` (Alt by default).

Weight / condition / cost / novelty captions append `v` (descending) or `^` (ascending). For `by_type`, the caption switches to the current type name in the cycle.

### Take timestamp (novelty)

`CInventoryItem::m_dwTakenTime` (`ALife::_TIME_ID`):

1. On the first `CInventory::Take`, if the stamp is `0`, it is set to `Level().GetGameTime()`.
2. Serialized in `CInventoryItem::save` / `load` before the `H_Parent()` early-return, next to condition.
3. Value `0` (no stamp, including older saves without the field) is pushed to the end when sorting descending.

### LTX for ordering

```ltx
[inventory_sort]
system = ordering

[inventory_sort:ordering]
weight_desc = true
condition_desc = true
cost_desc = true
novelty_desc = true

[inventory_sort_order]
general = 1
by_type = 1
by_weight = 1
by_condition = 1
by_cost = 1
by_importance = 1
by_novelty = 1

[inventory_sort_order:general]
name = st_inv_sort_order_general
hint = st_inv_sort_order_general_hint
```

### XML for ordering

Mod override files (ixray example):

1. `configs/ui/mod_actor_menu_sort_order.xml`
2. `configs/ui/mod_actor_menu_16_sort_order.xml`

The `inventory_sort_order_tabs` node defines seven buttons: `general`, `by_type`, `by_weight`, `by_condition`, `by_cost`, `by_importance`, `by_novelty`. Trade/deadbody/upgrade containers mirror the categories layout.

Strings: `configs/text/rus/ui_st_inventory_sort.xml`, `configs/text/eng/ui_st_inventory_sort.xml`.

## Recommendations

✔️ Correct usage:

1. Pick one mode (`categories` or `ordering`) per mod.
2. Add only the XML node that matches the selected mode.
3. Use `inventory_sort_custom` for non-standard item groups in categories mode.

⚠️ Limitations:

1. Grenades in GRENADE_SLOT appear in the bag only in categories mode with `all` or `ammo` filters.
2. Runtime mode switching is not supported.
3. Ordering direction changes go only through `CycleActiveOrderOption` (re-click / `kINV_SORT_CYCLE`); do not add a separate Alt handler.

✖️ Anti-patterns:

1. Adding both `inventory_sort_tabs` and `inventory_sort_order_tabs` to the same UI.
2. Expecting the actor inventory memory order to change.

## Related

[inventory slots](inventory-slots.md), [UI overview](ui-advanced-features.md)
