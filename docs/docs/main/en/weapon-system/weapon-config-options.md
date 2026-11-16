# Firing rate, camera effects, and vibration

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

Magazine-fed weapon settings are split between the item LTX section, the HUD section, and the global engine config. These settings control shot timing, shot camera effects, and controller feedback.

## Syntax

### Weapon item section

| Key | Default | Effect |
| --- | --- | --- |
| `rpm` | required | Base shots per minute. The engine computes the interval as `60 / rpm`. |
| `singleshoots_time_delta` | `0.0` | Interval in seconds for one-shot fire mode. A positive value takes priority. |
| `base_dispersioned_bullets_count` | `0` | Number of initial shots handled by base-dispersion logic. The key is read as `u8`. |
| `base_dispersioned_bullets_time_delta` | `0.0` | Interval in seconds while the shot counter is below `base_dispersioned_bullets_count`. Applies when positive and the single-shot interval did not take priority. |
| `vibration_time` | `0.1` | Shot vibration duration in seconds. |
| `vibration_factor_left`, `vibration_factor_right` | `1.0` | Controller motor intensities. Use values from 0 to 1. |
| `trigger_effect_time` | `1.0` | Right adaptive trigger effect duration for a shot in single-shot mode, in seconds. |

The `[gameplay] EnableWeaponAlternateRPMSystem` setting in `engine_external.ltx` changes the timer calculation. With `true`, the engine replaces the timer with the selected interval. With `false`, it adds the interval to the timer. All four bundled configs set the flag to `false`.

### Weapon HUD section

Set camera animation paths in the section referenced by the item's `hud` field, relative to `$game_anims$`.

| Key | Default path | Event |
| --- | --- | --- |
| `cam_shoot` | `camera_effects\weapon\base_shoot.anm` | Shot outside aiming. |
| `cam_aim_shoot` | `camera_effects\weapon\base_aim_shoot.anm` | Shot while aiming. |
| `cam_aim_in`, `cam_aim_out` | `camera_effects\actor_move\aim_in.anm`, `camera_effects\actor_move\aim_out.anm` | Entering and leaving aim. |
| `cam_safemode_in`, `cam_safemode_out` | `camera_effects\actor_move\safemode_in.anm`, `camera_effects\actor_move\safemode_out.anm` | Entering and leaving safe mode. |

If the first file is missing, the event list stays empty. After loading it, the engine looks for variants with `1`, `2`, and later suffixes appended to the **default** filename after its extension, for example `base_shoot.anm1`. Aimed shots require a populated regular `cam_shoot` list and fall back to it when `cam_aim_shoot` is empty. Paired transitions require both entry and exit animations.

## Examples

Set the single-shot interval and vibration in the weapon section.

```ini
rpm = 600
singleshoots_time_delta = 0.18
vibration_time = 0.1
vibration_factor_left = 0.7
vibration_factor_right = 0.35
trigger_effect_time = 0.8
```

The base interval is 0.1 seconds. The one-shot fire mode uses an interval of 0.18 seconds.

## See also

- [Weapon classes](weapon-classes.md)
- [Weapons in Lua](lua-weapon-api.md)
- [Gamepads and adaptive triggers](../gameplay/general/gamepads.md)
