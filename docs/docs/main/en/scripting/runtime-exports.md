# Lua API for configs, objects, and UI

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

This page covers exports for reading LTX files, using model bones, inspecting armor, and updating progress bars. Names and signatures follow the Lua registrations in `default`.

## Syntax

### Configs and level objects

| Call | Result |
| --- | --- |
| `get_cached_ini(path)` | Returns a shared read-only `ini_file`. The path is relative to `$game_config$`. Calling it with the same string reuses the cached object. |
| `sim.net_find(id)` | Returns the current level's registered `game_object`, or `nil`. |
| `level.enable_rain(value)` | Enables rain and lightning effects with `true` and disables them with `false`. Weather sections still determine when the effects occur. |

Reacquire a cached `ini_file` after ALife is reinitialized. The cache owns the object.

### Bones

These methods use a `game_object` with a skeletal model. Pass an existing bone name or a valid ID.

| Call | Result |
| --- | --- |
| `obj:bone_position(id_or_name)` | World-space position as a `vector`. |
| `obj:bone_direction(id_or_name)` | The bone's local Z axis in world space as a `vector`. |
| `obj:get_bone_id_by_name(name)` | Numeric ID or `65535` for a missing bone. |
| `obj:get_bone_name_by_id(id)` | Bone name. |
| `obj:get_root_bone_id()` | Root bone ID or `65535` for an object without a skeleton. |
| `obj:get_root_bone_name()` | Root bone name. |

### Armor

| Call | Result |
| --- | --- |
| `obj:cast_CustomOutfit()` | Returns a `CCustomOutfit` or `nil`. |
| `obj:cast_Helmet()` | Returns a `CHelmet` or `nil`. The export name is `cast_Helmet`. |
| `armor:GetDefHitTypeProtection(hit_type)` | Numeric protection for a damage type from the ALife enumeration. |
| `armor:GetBoneArmor(bone_id)` | Numeric armor value for the bone. |

`armor:GetHitTypeProtection(hit_type, bone_id)` takes a numeric bone ID. Use `65535` to calculate without a bone modifier. The result accounts for item condition and the active SoC, CS, or CoP armor balance.

### Progress bar

| Call | Effect |
| --- | --- |
| `bar:ShowBackground(enabled)` | Shows or hides the background. |
| `bar:UseColor(enabled)` | Enables or disables progress-based coloring. |
| `bar:SetColor(argb)` | Sets the fill texture color. Progress-based coloring can overwrite it. |
| `bar:SetMinColor(argb)` | Sets the minimum color. |
| `bar:SetMiddleColor(argb)`, `bar:SetMaxColor(argb)` | Set the midpoint and maximum colors respectively. The XML gradient colors use the `min_color`, `middle_color`, and `max_color` child nodes. |
| `bar:GetProgressStatic()` | Returns the bar's internal fill `CUIStatic`. The bar owns it. |
| `bar:SnapProgressPos()` | Moves the visible position to the target immediately after `SetProgressPos`. |

### Window lookup and inventory sorting

| Call | Effect |
| --- | --- |
| `window:FindWindow(name)` | Searches the current window and its descendants by name. Returns a `CUIWindow` or `nil`. |
| `window:FindStatic(name)` | Uses the same search. Returns a `CUIStatic` or `nil` if the first matching control cannot be cast to `CUIStatic`. |
| `menu:SetInventorySortSystem(system)` | Sets the inventory sort mode. Use `"ordering"` for order-based sorting or `"categories"` for category-based sorting. An empty string leaves the current mode in place. |
| `menu:GetInventorySortSystem()` | Returns `"ordering"` or `"categories"`. |

Call `ActorMenu.get_actor_menu()` after the in-game UI has been created. If the menu is available, use the returned `CUIActorMenu` instance.

### Medication intoxication

`CEntityCondition:ChangeIntoxication(delta)` adds a numeric delta to the actor's intoxication when `[gameplay] EnableMedIntoxication` is enabled. The value is clamped to `0–1`. `CEntityCondition:GetIntoxication()` returns the current value. Use `CActor:conditions()` to obtain the condition object.

### Object class IDs and active items

`clsid_table` is a global Lua table. Its keys are script class names and its values are numeric IDs from the current registration. Iterate over it with `pairs`.

`owner:iterate_activeitems(callback, context)` calls `callback(owner, item)` for the actor's active-slot item and the item in `DEVICE_SLOT` when present. Both callback arguments are `game_object` instances. The method accepts `context` and omits it from the callback arguments.

## Examples

### Read a setting from LTX

```lua
local ini = get_cached_ini("engine_external.ltx")
if ini:line_exist("gameplay", "Enable3DIcons") then
    local enabled = ini:r_bool("gameplay", "Enable3DIcons")
    printf("Enable3DIcons = %s", tostring(enabled))
end
```

### Update a progress bar immediately

```lua
local function show_progress(bar, value)
    bar:ShowBackground(true)
    bar:UseColor(false)
    bar:SetColor(GetARGB(255, 255, 255, 255))
    bar:SetProgressPos(value)
    bar:SnapProgressPos()
end
```

Pass an initialized `CUIProgressBar` and a value in its range.

## See also

- [Lua API reference](new-functions.md)
- [Object callbacks](lua-callbacks.md)
- [Weight progress bar](../interface/weight-progress-bar.md)
