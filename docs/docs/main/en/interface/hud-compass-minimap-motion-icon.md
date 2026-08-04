> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.4 <br>
> **Last updated**: 2026-07-22

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

`UICompassBar.visible` is synced with `Show()` so a hidden compass is skipped by the child-walk `Update`.

## compass_bar.xml unit contract

| Node | Attributes | Units | Notes |
|------|------------|-------|-------|
| `compass_bar` | `x` `y` `width` `height` | parent-relative fractions (UI base) | always relative, no `<= 1` heuristic |
| `strip` / `cardinal_points` | `x` `y` `width` `height` | parent-relative fractions | strip uses `_stripRel*`, cardinals are clip-relative |
| `strip:texture` | `draw_scale_x/y` or `draw_scale`; legacy `width`/`height` | draw scale vs clip / native | not atlas crop |
| `strip:texture` | `draw_offset_x/y`; legacy `x`/`y` | draw offset px | |
| `strip` | `tex_width` | logical circumference px | strong mismatch vs atlas width logs a warning |
| cardinal `marker` | `width`/`height` | `<= 1` relative to host, else px; `offset_y` px | |
| `active_target` | window `width`/`height`/`x` | px | |
| `active_target` | `active_offset_y` / `offset_y` / legacy `y` | vertical container offset px | priority: `active_offset_y` > `offset_y` > `y` |
| `altitude_arrow` | `altitude_deadzone` | meters | overrides container `active_target` value |
| `distance_text` / arrows / marker | `x` `y` `width` `height` | px | |

HD HUD example (no XML changes required): `strip` `width="0.88"`, texture `width="0.9" height="0.22" y="9"`, `tex_width="1024"`, cardinal tick `width="4"`, `active_target y="0"` as offset.

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

Purpose: dial draw scale and offset (not atlas crop).

| Attribute | Purpose | Default |
|-----------|---------|---------|
| `draw_scale` / `draw_scale_x` / `draw_scale_y` | explicit scale | legacy `width`/`height` |
| `draw_offset_x` / `draw_offset_y` | explicit offset px | legacy `x`/`y` |
| `width` / `height` / `x` / `y` | legacy aliases | `1` / `1` / `0` / `0` |

### tex_width

Purpose: logical scale width in pixels.
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

| Attribute | Purpose | Default |
|-----------|---------|---------|
| `active_offset_y` / `offset_y` / `y` | container vertical offset, px | `0` |
| `altitude_deadzone` | altitude arrow threshold | `1.8` |
| `padding` | strip edge padding | `8` |

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
