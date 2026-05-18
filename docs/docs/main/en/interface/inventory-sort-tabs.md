> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.4

# Inventory sort tabs

## Overview

The tab system sorts items in the inventory, trade, and corpse windows by preset or custom categories. It also supports hotkeys `kQ`/`kE`.

## How it works

1. Categories are defined in `CInventorySorter`.
2. Basic categories: `all`, `weapons`, `ammo`, `armor`, `devices`, `consumables`, `artefacts`, `attachments`.
3. Custom categories are read from `inventory_sort_custom`.
4. Category labels and icons are read from `inventory_sort_categories`.

## XML and config

1. Add `inventory_sort_tabs` to `configs/ui/actor_menu.xml`.
2. For different modes, you can add:
   1. `inventory_sort_tabs_container_upgrade`
   2. `inventory_sort_tabs_container_trade_actor_bag`
   3. `inventory_sort_tabs_container_trade_partner_bag`
   4. `inventory_sort_tabs_container_deadbody_bag`
3. In LTX, describe the `inventory_sort_categories` and `inventory_sort_custom` sections.

Related material: [inventory slots](inventory-slots.md), [UI overview](ui-advanced-features.md).
