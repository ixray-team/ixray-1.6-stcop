# Configuring LTX and DLTX

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

The engine reads parameters from several LTX files. To change an item or font, you can place a separate DLTX file next to the original config. Engine-wide settings belong in the active resource pack’s `engine_external.ltx`.

## Syntax

A DLTX file is named `mod_<base filename without extension>_<suffix>.ltx`. A `![name]` section changes an existing section. A `[new_section]:parent` section creates a section and inherits its parent’s keys.

| What you are changing | Where the engine reads the keys |
| --- | --- |
| Settings for a specific weapon | A weapon section such as `[wpn_gauss]`. |
| Font settings | The font section requested by XML or UI code. |
| Engine-wide settings | The appropriate section in `configs/engine_external.ltx`. |

## Examples

### Configure weapon vibration

For the bundled Gauss rifle in CoP, create `gamedata/configs/weapons/mod_w_gauss_config_example.ltx`. The engine applies it to `w_gauss.ltx`.

```ini
![wpn_gauss]
vibration_time = 0.15
vibration_factor_left = 0.6
vibration_factor_right = 0.25
```

| Key | Meaning |
| --- | --- |
| `vibration_time` | Shot vibration duration in seconds. |
| `vibration_factor_left`, `vibration_factor_right` | Left and right motor intensities, from `0` to `1`. |

With gamepad vibration enabled, a shot produces a short response with the left motor stronger than the right. `![wpn_gauss]` overrides these keys in the existing section. [Other weapon settings](../weapon-system/weapon-config-options.md)

### Add a font for XML

Create `gamedata/configs/mod_system_config_example.ltx`. `system.ltx` includes `fonts.ltx`, so the new section can inherit `font_default`. Use `[example_status_font]` without `!` to create a section.

```ini
[example_status_font]:font_default
size = 16
letter_spacing = 0.5
line_spacing = 3
```

| Key | Meaning |
| --- | --- |
| `size` | Size passed to the engine's font preparation code. |
| `letter_spacing` | Extra spacing added to the character width when measuring a line. |
| `line_spacing` | Extra spacing added to the line height when laying out multiline text. |

Set `font="example_status_font"` on a child `<text>` element to use the font. [XML example using this font](xml-examples.md)

### Enable medication intoxication

Change `EnableMedIntoxication` in the existing `[gameplay]` section of `configs/engine_external.ltx`. The block below is an excerpt from that file. Restart the game after making the change.

```ini
[gameplay]
EnableMedIntoxication = true
```

The switch enables the mechanic. The intoxication delta from a dose comes from `eat_intoxication` in the consumable's section. Use the actor's `intoxication_*` keys to adjust the threshold and subsequent effects. [Parameters and radioprotector example](../gameplay/equipment/general-information.md#medication-intoxication)

## See also

- [DLTX and inheritance](dltx.md)
- [engine_external.ltx reference](../main/engine-extension-configuration.md)

## Configure movement bobbing

The engine reads `[bobbing_effector]` from `system.ltx`. The base values are in `configs/misc/effectors.ltx`, with stock adjustments in `mod_system_ixray.ltx`. Each `*_amplitude` and `*_speed` pair sets the amplitude and phase rate. The `zoom_` prefix selects a separate pair while aiming. Missing numbers default to `0`.

| Movement | Regular | Aiming |
| --- | --- | --- |
| Walking | `walk_amplitude`, `walk_speed` | `zoom_walk_amplitude`, `zoom_walk_speed` |
| Running | `run_amplitude`, `run_speed` | `zoom_run_amplitude`, `zoom_run_speed` |
| Crouching | `crouch_amplitude`, `crouch_speed` | `zoom_crouch_amplitude`, `zoom_crouch_speed` |
| Slow crouching | `slow_crouch_amplitude`, `slow_crouch_speed` | `zoom_slow_crouch_amplitude`, `zoom_slow_crouch_speed` |
| Limping | `limp_amplitude`, `limp_speed` | `zoom_limp_amplitude`, `zoom_limp_speed` |
| Sprinting | `sprint_amplitude`, `sprint_speed` | Uses the same pair. |

`amplitude_delta` controls how quickly the amplitude changes per second and defaults to `1.0`. `affect_hud` applies the effect to the HUD and defaults to `true`. To disable aiming bobbing while walking, set `zoom_walk_amplitude = 0` in the existing section.
