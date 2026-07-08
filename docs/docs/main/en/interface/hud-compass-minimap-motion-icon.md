> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.4 <br>
> **Last updated**: 2026-06-06

# Horizontal compass, minimap and motion icon features for the minimap

## Overview

This feature configures the HUD navigation block: the minimap or the horizontal compass. The motion icon works next to this block and shows the actor's movement state and visibility.

The compass bar is an **optional** UI element. Until it is activated via `SetNavigationMode(true)` or the deprecated boot hint `UseCompassBar`, it is not created, does not load `compass_bar.xml`, and does not affect the minimap, motion icon, or PDA online.

Switching between the minimap and compass bar happens at runtime without reloading the level.

## Default mode and runtime switching

1. Engine default: **minimap**.
2. `UseCompassBar` in `configs/engine_external.ltx` (**deprecated**) is a boot-time hint for mods without Lua. Prefer the Lua API or IXR Options.
3. `hud_minimap` controls **visibility** of the active navigation block.
4. Runtime switching: `ActorMenu.get_maingame():SetNavigationMode(bool)`, where `true` is compass bar and `false` is minimap.
5. Persisting the navigation mode in save/user.ltx is **not implemented**.

## Lua API

```lua
local maingame = ActorMenu.get_maingame()
if maingame then
    maingame:SetNavigationMode(true)   -- compass bar (lazy init)
    maingame:SetNavigationMode(false)  -- minimap
    local isCompass = maingame:IsCompassBarMode()
end
```

Readonly fields `UIZoneMap` and `UICompassBar` are available on `CUIMainIngameWnd`. `UICompassBar` may be `nil` until the compass bar is activated.

## Atlas and compass_bar.xml components

### compass_bar root

| Attribute | Purpose | Default |
|-----------|---------|---------|
| `fov_angle` | Strip field of view in degrees | `45` |
| `fade_in_speed` | Spot fade-in speed | `6` |
| `fade_out_speed` | Spot fade-out speed | `5` |
| `min_visible_alpha` | Minimum visible alpha threshold | `0.01` |
| `fov_fade_inner` | Inner FOV edge fade boundary | `0.30` |
| `fov_fade_outer` | Outer FOV edge fade boundary | `0.70` |
| `fov_fade_edge_lo` | Lower normalized fade edge | `0.05` |
| `fov_fade_edge_hi` | Upper normalized fade edge | `0.95` |

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

| Attribute | Purpose | Default |
|-----------|---------|---------|
| `fake_target_distance` | Projection distance for N/E/S/W labels | `1000` |

### spots

| Attribute | Purpose | Default |
|-----------|---------|---------|
| `collect_interval` | Map spot collect interval in seconds | `0.1` |
| `show` | Show spots on the strip | `1` |

### active_target

Purpose: selected target marker, distance, and vertical offset.

#### distance_text

| Attribute | Purpose | Default |
|-----------|---------|---------|
| `format` / `text_format` | sprintf distance format | `"%.0f m"` |
| `st_format` | String table ID instead of format | - |

## Motion icon

1. `state_normal`, `state_crouch`, `state_creep`, `state_climb`, `state_run`, `state_sprint` show the current movement type.
2. `power_progress` shows stamina.
3. `luminosity_overlay` and `noise_overlay` apply visual noise and dimming.
4. Luminosity/noise overlays are created for minimap mode and hidden in compass bar mode. When switching back to the minimap, overlays are restored without recreating the HUD.

## Examples

Scenario 1: Activation via Lua (recommended)

```lua
ActorMenu.get_maingame():SetNavigationMode(true)
```

Scenario 2: Legacy boot hint via DLTX (deprecated)

```ini
[ui]
UseCompassBar = true
```

Related material: [UI overview](ui-advanced-features.md).
