> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.4

# Horizontal compass, minimap and motion icon features for the minimap

## Overview

This feature configures the HUD navigation block: either the minimap or the horizontal compass. The motion icon works next to this block and shows the actor’s movement state and visibility.

## Enable logic

1. `UseCompassBar = true` in `configs/engine_external.ltx` enables the horizontal compass.
2. `UseCompassBar = false` restores the minimap.
3. `hud_minimap` controls the block visibility on screen.

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
4. Overlays work in minimap mode. When `UseCompassBar = true` they are not created.

## Example enable

```ini
[ui]
UseCompassBar = true
```

Related material: [UI overview](ui-advanced-features.md).
