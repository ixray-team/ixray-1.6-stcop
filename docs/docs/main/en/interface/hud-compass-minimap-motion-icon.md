> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.4

# Horizontal compass, minimap and motion icon features for the minimap

## Overview

This feature configures the HUD navigation block: the minimap or the horizontal compass. The motion icon works next to this block and shows the actor's movement state and visibility.

Both widgets are initialized when the HUD loads. Switching between them happens at runtime without reloading the level.

## Default mode and runtime switching

1. `UseCompassBar` in `configs/engine_external.ltx` sets the **default mode** for a new profile when no user choice is stored yet.
2. `UseCompassBar = true` enables the horizontal compass by default.
3. `UseCompassBar = false` enables the minimap by default.
4. `hud_minimap` controls **visibility** of the active navigation block.
5. Runtime navigation type switching is done via Lua API `ActorMenu.get_maingame():SetNavigationMode(bool)`, where `true` means compass bar and `false` means minimap.

## Lua API

```lua
local maingame = ActorMenu.get_maingame()
if maingame then
    maingame:SetNavigationMode(true)   -- compass bar
    maingame:SetNavigationMode(false)  -- minimap
    local isCompass = maingame:IsCompassBarMode()
end
```

Readonly fields `UIZoneMap` and `UICompassBar` are available on `CUIMainIngameWnd`.

## Atlas and compass_bar.xml components

### background

Purpose: panel background and frame.

### strip

Purpose: direction strip.
Logic: the engine shifts UV coordinates depending on camera rotation.

### strip:texture

Purpose: atlas sample rectangle.
Parameters: `x`, `y`, `width`, `height`.

### tex_width

Purpose: actual scale width in pixels.
If set incorrectly, the marker movement speed will not match the viewing angle.

### tex_loop

Purpose: cyclic scrolling.
`1` means seamless wrap, `0` means clamped edges.

### cardinal_points

Purpose: text direction labels.

### active_target

Purpose: selected target marker, distance, and vertical offset.

## Motion icon

1. `state_normal`, `state_crouch`, `state_creep`, `state_climb`, `state_run`, `state_sprint` show the current movement type.
2. `power_progress` shows stamina.
3. `luminosity_overlay` and `noise_overlay` apply visual noise and dimming.
4. Luminosity/noise overlays are created for minimap mode and hidden in compass bar mode. When switching back to the minimap, overlays are restored without recreating the HUD.

## Example default compass bar enable

```ini
[ui]
UseCompassBar = true
```

Related material: [UI overview](ui-advanced-features.md).
