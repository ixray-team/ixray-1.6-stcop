# Weapons and attachments in Lua

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

The script `game_object` for a magazine-fed weapon exposes ammunition, attachment, and scope methods. Obtain a live weapon object before calling these methods.

## Syntax

| Call | Effect |
| --- | --- |
| `weapon:weapon_get_ammo_section(ammo_type)` | Ammunition section at a zero-based index, or an empty string. |
| `weapon:weapon_addon_attach(item)` | Attaches an existing compatible inventory item. |
| `weapon:weapon_addon_detach(section, spawn_to_inventory)` | Detaches a compatible attachment. The second argument controls whether an item is spawned. |
| `weapon:addons_attach(mask, scope_idx)` | Replaces the attachment mask and refreshes settings and visuals. If the mask attaches a detachable scope, `scope_idx` must be a valid zero-based index. An invalid index cancels the operation. |
| `weapon:weapon_get_scope()` | Current scope index, or `255` when called on an unsupported object type. |
| `weapon:weapon_set_scope(scope_idx)` | Sets a valid zero-based scope index. Use `addons_attach` to refresh settings and visuals. |

The mask uses `0x01` for the scope, `0x02` for the grenade launcher, and `0x04` for the silencer.

## Examples

### Selecting the first scope and a silencer

```lua
local function setup_attachments(weapon)
    weapon:addons_attach(0x01 + 0x04, 0)
end
```

Call the function with a magazine-fed weapon configured for a detachable scope at index `0` and a silencer. The mask replaces the previous attachment state.

## Recommendations and limitations

- Pass an existing inventory item to `weapon_addon_attach`. Its C++ implementation dereferences the argument without a null check.
- Supply an index from the weapon configuration when selecting a detachable scope. The engine indexes the scope list without a bounds check.

## See also

- [Weapon classes and settings](weapon-classes.md)
- [Recoil patterns](shot-pattern-system.md)
- [Lua API](../scripting/new-functions.md)
