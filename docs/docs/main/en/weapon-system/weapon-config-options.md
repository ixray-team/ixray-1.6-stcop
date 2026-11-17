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

### Weapon HUD transition durations

Set these keys in the weapon HUD section. Durations are in seconds and the engine converts them to whole milliseconds. Each key has a code fallback of `0`.

| Keys | Transition |
| --- | --- |
| `to_crouch_time`, `from_crouch_time` | Enter and leave crouch. |
| `to_slow_crouch_time`, `from_slow_crouch_time` | Enter and leave low crouch. |
| `to_llookout_time`, `from_llookout_time` | Enter and leave left lean. |
| `to_rlookout_time`, `from_rlookout_time` | Right lean durations. The current implementation uses `to_llookout_time` when entering right lean and `from_rlookout_time` when leaving it. |

Example DLTX for the shared `[hud_base]`:

```ini
![hud_base]
to_crouch_time = 0.15
from_crouch_time = 0.15
to_llookout_time = 0.15
from_llookout_time = 0.15
to_rlookout_time = 0.15
from_rlookout_time = 0.15
```

### Stationary machine gun overheating

Set these keys in the stationary machine gun section. Heat increases on each `UpdateFire` call while the gun is working, and cooling is subtracted on every update. These increments are heat units per update.

| Key | Code fallback | Effect |
| --- | ---: | --- |
| `overheat_enabled` | `false` | Enables heat accumulation and cooling. |
| `overheat_time_quant` | `0.025` | Adds heat while the gun is working. |
| `overheat_decr_quant` | `0.002` | Subtracts heat on every update, including while firing. |
| `overheat_threshold` | `110` | Maximum heat. Reaching it blocks firing. |
| `overheat_particles` | `damage_fx\burn_creatures00` | Particle effect that starts at `100` heat. |

Cooling below `100` stops the effect and allows firing again. Set `overheat_threshold` to at least `100` to reach the effect and the firing lockout. A lower threshold caps the heat before either can trigger.

To reproduce the bundled CoC settings, add these keys to `stationary_mgun`:

```ini
overheat_enabled = true
overheat_time_quant = 0.90
overheat_decr_quant = 0.20
overheat_threshold = 200
overheat_particles = industrial_particles\exhaust_workshop_1_small
```

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

## HUD model contact with the environment

Put these keys in the weapon’s HUD section selected by `hud`. Contact detection uses an oriented box and static geometry. While aiming, the current aim factor reduces this effect.

| Key | Type and fallback | Effect |
| --- | --- | --- |
| `collision_box_pos` | vector `0, 0, 0` | Box center in local HUD coordinates. |
| `collision_box_scale` | vector `1, 1, 1` | Box half-extents along the three axes. |
| `collision_stifness` | number `400.0` | Stiffness of the spring that moves the HUD pose. The engine spelling has a single `f`. |
| `collision_damping` | number `40.0` | Spring damping. |
| `collision_dist_min` | number `0.5` | Camera-to-contact distance where the offset reaches full strength. |
| `collision_dist_max` | number `0.8` | Distance where the offset reaches zero. |

Use `0 ≤ collision_dist_min < collision_dist_max ≤ 1`. The engine clamps the contact distance to `0…1` before calculating the effect. For example, `0.35` and `0.7` define the range between the full close-range offset and zero at the far boundary.

## Lighting and laser dots on HUD items

These keys belong in the item section. For a flashlight, set `torch_installed` and supply the required bone and texture. For a laser, set `laser_installed` and its own bone and texture. The bone names must exist in the target model.

### Flashlight

| Key | Type and fallback | Effect |
| --- | --- | --- |
| `torch_installed` | `bool / false` | Enables the flashlight component on a HUD item. |
| `torch_light_bone` | `string / required` | Bone defining the light’s local coordinate system. |
| `torch_spot_texture` | `string / required` | Spot texture for the directional light. |
| `torch_render_type` | `u32 / SPOT` | Main light type. The auxiliary light always uses a point source. |
| `torch_r2_range`, `torch_r2_omni_range` | `float / 15; 0.75` | Main and auxiliary light ranges. |
| `torch_render_shadow`, `torch_omni_shadow` | `bool / true; false` | Enable shadows for the respective lights. |
| `torch_spot_angle` | `float / 75` | Beam cone angle in degrees. |
| `torch_r2_color_r`, `torch_r2_color_g`, `torch_r2_color_b`, `torch_r2_color_a` | `float / 0.60; 0.55; 0.55; 0.80` | RGBA components of the main light. |
| `torch_r2_omni_color_r`, `torch_r2_omni_color_g`, `torch_r2_omni_color_b`, `torch_r2_omni_color_a` | `float / 0.60; 0.55; 0.55; 0.80` | RGBA components of the auxiliary light. |
| `torch_attach_offset_x`, `torch_attach_offset_y`, `torch_attach_offset_z` | `float / 0` | Main light offset on the HUD model. |
| `torch_aim_attach_offset_x`, `torch_aim_attach_offset_y`, `torch_aim_attach_offset_z` | `float / 0` | Light offset while aiming. |
| `torch_world_attach_offset_x`, `torch_world_attach_offset_y`, `torch_world_attach_offset_z` | `float / 0` | Main light offset on the world model. |
| `torch_omni_attach_offset_x`, `torch_omni_attach_offset_y`, `torch_omni_attach_offset_z` | `float / torch_attach_offset_*` | Auxiliary light offset on the HUD model. |
| `torch_omni_world_attach_offset_x`, `torch_omni_world_attach_offset_y`, `torch_omni_world_attach_offset_z` | `float / torch_world_attach_offset_*` | Auxiliary light offset on the world model. |
| `light_directions_by_bones`, `light_dir_bone` | `bool / false, string` | Enables bone-defined light direction. Set the direction bone name when enabled. |
| `torch_cone_bones` | `list` | Comma-separated list of flashlight cone bones. |
| `torch_breaking_params` | `vector / -1, -1, 0` | Start and end condition for interference, followed by the starting probability. |

### Laser dot

| Key | Type and fallback | Effect |
| --- | --- | --- |
| `laser_installed` | `bool` | With `true`, the weapon loader creates the laser component. |
| `laserdot_attach_bone`, `laser_spot_texture` | `string / required` | Light bone and spot texture. |
| `laser_light_distance` | `float / 15` | Light source range. |
| `laser_wrok_distance` | `float / 0.5 × laser_light_distance` | Distance beyond which the light source shifts along the ray. The engine looks up the exact spelling `wrok`. |
| `laser_max_distance` | `float / laser_wrok_distance` | Ray intersection query range. |
| `laser_light_color` | `RGBA / 1, 0, 0, 0` | Light source color. |
| `laser_render_shadow` | `bool / true` | Enables shadows. |
| `laser_spot_angle` | `vector2 / 2, 5` | Initial and final cone angles in degrees. In HUD rendering the angle varies between the working and maximum distances. |
| `laserdot_attach_offset_x`, `laserdot_attach_offset_y`, `laserdot_attach_offset_z` | `float / 0` | Light offset on the HUD model. |
| `laserdot_world_attach_offset_x`, `laserdot_world_attach_offset_y`, `laserdot_world_attach_offset_z` | `float / 0` | Light offset on the world model. |
| `laserdot_attach_offset`, `laserdot_world_attach_offset` | `vector` | Three-component offsets. If present, they replace the individual axis values. |
| `laser_ray_bones` | `list` | Comma-separated list of beam-effect bones. |
| `laser_breaking_params`, `laser_problems_level` | `vector / -1, -1, 0; float / 0` | Condition thresholds and initial interference probability, plus a separate interference level. |

To tune an existing laser, change just the distances. For example, `laser_wrok_distance = 10` and `laser_max_distance = 20` set the range from the start of the light shift to the ray limit.

## Rotational inertia and idle inspection

These keys belong in the HUD section selected by `hud`. While entering aim mode the engine interpolates between the values in the two columns using the aim factor.

| Setting | Normal stance | Aiming | Fallback |
| --- | --- | --- | --- |
| Left and right turn inertia | `hud_yaw_inertia_k` | `hud_zoom_yaw_inertia_k` | `0` |
| Vertical turn inertia | `hud_pitch_inertia_k` | `hud_zoom_pitch_inertia_k` | `0` |
| Roll about the view axis caused by horizontal turning | `hud_roll_inertia_k` | `hud_zoom_roll_inertia_k` | `0` |
| Rate of convergence to the inertia target | `hud_inertia_speed` | `hud_zoom_inertia_speed` | `10` |

Rates and coefficients are floating-point values. For example, `hud_zoom_yaw_inertia_k = 0` removes this axis’s contribution when fully aimed. The Boolean `disable_bore` suppresses the item’s idle inspection animation and defaults to `false`.

## Lua window on a HUD model

In the item section, `script_ui_functor` names a zero-argument Lua function that returns a `CUIDialogWndEx`. The loader reads the other three keys when this name is nonempty. Set `script_ui_bone` to a bone name, `script_ui_position` to a local offset, and `script_ui_rotation` to a rotation in degrees in heading, pitch, bank order. The loader builds the transform from these values.

```ini
script_ui_functor = my_device.create_display
script_ui_bone = display
script_ui_position = 0, 0, 0
script_ui_rotation = 0, 0, 0
```

This example assumes that the model contains a `display` bone and that the function creates or returns its display window.
