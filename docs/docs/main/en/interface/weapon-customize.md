> [!IMPORTANT]
> **Status**: In development

# Weapon customization window

## Overview

The window opens from the "Customize" entry of the weapon context menu. The entry appears for weapons that have `customize_bones` in their config. Every part (sights, mounts, handguards, magazines, lights) is a real item. Parts move from the backpack onto the weapon and back; nothing is created or deleted.

The engine provides three pieces:

1. The menu entry and a call to a Lua function chosen by the mod.
2. The `weapon_customize` module: the slot tree, layouts, conflicts and applying a change in one step.
3. The `CUIWeaponViewport` widget: a 3D view of the weapon with its parts, a camera, slot points, part preview and callouts.

The window itself belongs to the mod. In HD HUD it lives in the `RenewedPerceptionSWP` add-on and loads only with `RenewedPerceptionCoP` and Secret Weapon Pack (`secret-weapon-pack`). Without the mod the entry opens the ImGui debug window as before (`DEBUG_DRAW` builds only).

## What the player sees

1. Left: the slot tree in groups (optics, muzzle, body, ergonomics, tactical, feed). Nested slots, such as a sight on a mount or rails on a handguard, are indented under their parent. Each row shows the installed part, "empty" or "no parts".
2. Center: the weapon in 3D with every part. Each slot has a point: light for a filled slot, grey for an empty one. The selected slot and the slot under the cursor get callouts.
3. Right: parts for the selected slot: the installed one, "Detach part", parts in the backpack with a count, and parts on another slot of this weapon. Parts the player does not own are listed only in god mode (`g_god 1`) and in development mode.
4. Hovering a card previews the part on the weapon before installing it. The preview is only a picture in the window: no item is moved or created, so nothing reaches the weapon or the save.
5. Under the view: build stats. While previewing, the change is shown, green when better and red when worse.
6. Bottom right: the part description, its condition and warnings: the part it replaces, the conflicting parts that will be removed, and everything mounted on them that goes to the backpack too.
7. Header: the weapon name, filled slots and build weight. Footer: hotkey bar for the current bindings.

## Controls

| Input | Action |
| --- | --- |
| LMB drag on the view | Rotate |
| RMB or MMB drag | Pan |
| Wheel over the view | Zoom |
| Click a slot point or a slot row | Select the slot |
| Hover a card | Preview the part |
| Click a card | Install the part; "Detach part" detaches |

Rebindable in the controls menu, group "Weapon customization":

| Action | Keys | Gamepad | Effect |
| --- | --- | --- | --- |
| `wc_slot_prev`, `wc_slot_next` | Q, E | LB, RB | Previous / next slot |
| `wc_part_prev`, `wc_part_next` | W, S / Up, Down | D-pad up, down | Select a part |
| `wc_apply` | Enter / Space | A | Install the selected part |
| `wc_detach` | X / Delete | X | Detach the slot's part |
| `wc_undo`, `wc_redo` | Z, Y | Y (undo) | Undo / redo |
| `wc_revert` | Backspace | Back | Restore the build the window opened with |
| `wc_focus` | F | RS | Camera follows the slot / overview |
| `wc_reset_view` | R | LS | Reset the camera |
| `wc_rotate_left`, `wc_rotate_right` | A, D / Left, Right | D-pad left, right | Rotate by a step |
| `wc_zoom_in`, `wc_zoom_out` | =, - / Num+, Num- | RT, LT | Zoom |
| `wc_close` | | B | Close |

Esc closes the window; the inventory stays underneath.

## Mod setup

```ini
[weapon_customize]
ui_functor = ui_weapon_customize.open
```

The engine calls `ui_functor(obj)` for the weapon. The function returns `true` when it opened a window. With no section, no function, or `false`, the debug window opens. The menu label is the `st_weapon_customize` string, "Customize" when the string is missing.

`configs/mod_system_weapon_customize_swp.ltx` keeps only the entry point, sounds, camera settings and hotkeys. Slot labels (`<slots><slot bone label group order/>`), groups (`<groups><group id label order/>`) and stats (`<stats><stat id label key mode include_root format unit better hide_zero hide_default empty/>`, `mode` = `sum`/`mul`/`max`/`any`/`first`/`count`) live in `configs/ui/weapon_customize.xml` together with styles, colors and panel proportions; the window arranges itself for any aspect ratio, so `_16` and `_21` copies are not needed.

## Lua

`weapon_customize` module:

| Function | Effect |
| --- | --- |
| `slots(root)` | All slots: `owner`, `owner_section`, `bone`, `depth`, `parent` (index of the parent slot in the same list, 0 for top level), `allowed`, and `attached` / `attached_section` for filled slots |
| `layout(root)` | Current layout: `{id, owner, bone, section}` list |
| `signature(root)` | A number that changes with every change of the build |
| `conflicts(root, part, owner, bone)` | Parts removed by installing `part` there |
| `attach(root, part, owner, bone)` | Install from the backpack or from another slot of this weapon |
| `detach(part)` | Move a part, with everything on it, to the backpack |
| `apply_layout(root, layout)` | Bring the build to a layout from `layout()` (undo and revert) |
| `parent(id)`, `root(id)`, `is_customizable(id)`, `actor_parts()`, `open_debug(id)` | Helpers |

Changes reach the weapon through ownership events, so the new layout shows up in `slots` and `signature` a frame or more later.

`CUIWeaponViewport` is created with `xml:InitWeaponViewport(path, parent)`: `SetItem`, `Rotate`, `SetAngles`, `Zoom`, `SetZoom`, `SetZoomLimits`, `SetFov`, `FocusSlot`, `ClearFocus`, `ResetView`, `SetAutoRotate`, `HasSlotPoint`, `GetSlotPoint`, `GetSlotCount`, `GetSlotOwner`, `GetSlotBone`, `GetHoverSlot`, `PopClickedSlot`, `IsDragging`, `SetPreview`, `ClearPreview`, `HasPreview`, `SetCallout`, `ClearCallouts`, `SetSelected`, `SetLineColors`. Points are in widget coordinates.

`xml:InitHotkeyBar(path, parent)` creates a `hotkey_bar` in a script window.

`ui_weapon_customize.hooks` lets another script plug in `opened(wnd)`, `rebuilt(wnd)`, `show_missing()` and `missing_part(wnd, card, slot)` (return `true` to handle a click on a part the player does not own).

Development mode: `scripts/ui_weapon_customize_dev.script` with `configs/mod_system_weapon_customize_dev.ltx` (`[weapon_customize_dev]`: `enabled` spawns a missing part into the backpack when clicked, `auto_install` installs it into the selected slot as a normal undoable install, `spawn_all_on_open` spawns one of every compatible part the weapon lacks when the window opens). Delete both files or set `enabled = false` before release.

## Limitations

1. Parts the player does not own are listed only in god mode or development mode; their conflicts come from the `conflicts` fields only.
2. Undo and revert bring back only parts still in the backpack.
3. The preview draws the part without its own nested slots.
4. Stats use section parameters only; upgrades are not included.
5. The window closes when the weapon leaves the inventory.

## Post-build checks

See the Russian page for the full list: menu entry with HD HUD and SWP, the add-on skipped without SWP, layout on 16:9, 21:9 and 4:3, mouse camera, slot selection and focus, preview and stat deltas, install and replace, Bastion cover conflicts, undo / redo / revert, hotkeys limited to the window, Esc and the close button.
