# Zones (Bonfires, Anomalies...)
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.0

## General 
* Volumetric lighting (optional)
```ini
volumetric_blowout = true ; Volumetric light (blowout)
volumetric_quality = 1.0  ; Render quality (light/blowout)
volumetric_distance = 1.0 ; Light distance (light/blowout)
volumetric_intensity = 0.7; Light intensity (light/blowout)
```

## Bolt removal in anomalies

Set `delete_bolts` in a zone section implemented by `CAnomalyZone`. It controls removal of thrown bolts when they enter the zone. This is an engine feature and does not require Gunslinger scripts.

| Parameter | Type | Default | Purpose |
| --- | --- | --- | --- |
| `delete_bolts` | `bool` | `true` | Allows the zone to remove bolts entering it |

### Behavior

With `delete_bolts = true`, the zone compares the bolt's destruction deadline with the current game time plus 100 ms:

- If more than 100 ms remain, the zone immediately requests removal through the standard `Destroy()` mechanism. Actual disappearance depends on destruction event processing.
- If 100 ms or less remain, the existing timer is preserved. An already expired timer is also left unchanged.

**100 ms is a comparison threshold, not a removal delay.** The threshold cannot be configured through LTX. The current implementation differs from original Gunslinger, which shortens the remaining lifetime to 100 ms.

With `delete_bolts = false`, the zone neither changes the bolt's lifetime nor requests its removal. The ordinary `destroy_time` timer from the bolt section still applies. The bolt can affect the zone again if its activation logic permits it.

Only bolts (`CBolt`) are affected. Grenades and other items are unchanged. The `ignore_small`, `ignore_nonalive` and entrance particle settings do not disable removal of a bolt already registered as entering the zone.

The setting applies independently to each zone. In overlapping zones, `delete_bolts = false` in one zone does not prevent another from removing the bolt. Leaving a zone does not cancel a destruction request already sent.

### Example: retain bolts in a minefield

For CoP, CS and CoC, create `configs/zones/mod_zone_minefield_bolts.ltx` as a DLTX patch:

```ini
![zone_mine_field]
delete_bolts = false
```

In SoC, place the corresponding file in `configs/misc/`. The target game must contain the `zone_mine_field` section. When editing the original LTX directly, add the parameter to the existing section without the `!` prefix.

The Gunslinger port already includes this exception in `GunslingerModCop/configs/zones/mod_zone_minefield_zzz_guns.ltx`; it needs no additional patch. Set `true` to allow removal. Omitting the parameter also selects `true`. Section inheritance and DLTX follow their usual rules.

The value is read once when the zone loads and cached in a flag. Restart the game after editing the configuration.

### Manual verification

Use fresh bolts with an ordinary lifetime greater than 100 ms.

| Scenario | Expected result |
| --- | --- |
| Enter an ordinary anomaly without `delete_bolts` | Bolt removal is requested |
| Enter a zone with `delete_bolts = false` | The ordinary lifetime applies; repeated activation depends on zone logic |
| Enter with 100 ms or less remaining | The existing timer is preserved |
| Enter overlapping zones with `true` and `false` | The zone with `true` can remove the bolt |
| Enter with a grenade or another item | This setting does not change its behavior |

## Disabling emissions in CoP and CoC

IX-Ray Platform 1.4 adds a disable-emissions toggle to the IXR global options. The script registry stores it as the boolean `disable_emissions` in `ixray_global_options`. It is off by default.

When enabled, scheduled emissions are skipped and anomalies still refresh on the emission schedule. Manual starts through the surge manager are blocked.

## Additional anomaly components

> **Minimum version**: 1.4

Name the component section in the anomaly section. The loader reads the settings from that section. Without a reference, the component keeps its initial state. Movement and electric arcs accept the `draw_debug` option for debug rendering, defaulting to `false`.

### Anomaly movement

Select the section with `movement_options_section`.

| Key | Type and fallback | Effect |
| --- | --- | --- |
| `use_movement` | `bool / false` | Enables the movement component. |
| `use_movement_always_mode` | `bool / false` | Allows movement without a magnetic target. |
| `use_movement_magnetic_on_inside_alive_mode` | `bool / false` | Allows movement toward a living target in the zone. |
| `use_movement_magnetic_on_take_artefacts_mode` | `bool / false` | Enables movement triggered by taking an artefact. |
| `movement_magnetic_on_inside_alive_mode_speed` | `float / 0` | Speed when moving toward a living target. |
| `movement_magnetic_on_take_artefacts_mode_speed` | `float / 0` | Movement speed after an artefact is taken. |
| `milliseconds_time_magnetic_on_take_artefacts` | `float / 0` | Duration of the artefact pickup response, in ms. |
| `movement_speed` | `float / 1.5` | Normal movement and return speed. |
| `movement_radius` | `float / 15` | Radius used to choose destinations for normal movement. |

### Item attraction

Select the section with `gravity_options_section`.

| Key | Type and fallback | Effect |
| --- | --- | --- |
| `use_gravity_effect` | `bool / false` | Enables attraction. A nonpositive radius or processing distance disables it. |
| `gravity_radius` | `float / 10` | Effect radius. |
| `min_gravity_radius_factor` | `float / 0.3` | Radius fraction for the inner area. |
| `max_processing_distance` | `float / 200` | Component processing distance limit. |
| `max_count_spawn_trash` | `u16 / 0` | Maximum number of debris items to spawn. |
| `trash_sections` | `list` | Comma-separated section names of debris items. |

### Electric arcs

Select the section with `electric_curve_options_section`.

| Key | Type and fallback | Effect |
| --- | --- | --- |
| `use_electric_curve` | `bool / false` | Enables the arc component. |
| `electric_curve_particle_path` | `string` | Arc particle effect name. |
| `electric_curve_ground_contact_particle_path` | `string` | Arc contact particle effect name. |
| `max_count_electric_curves` | `u8 / 1` | Maximum arc count. |
| `max_trace_curve_distance` | `float / 15` | Ray query distance limit. |
| `max_processing_distance` | `float / 200` | Component processing distance limit. |
| `max_curve_atack_damage_time` | `float / 5000` | Arc processing time during the blowout phase. |
| `max_curve_atack_radius` | `float / 5` | Arc damage radius. |
| `max_curve_atack_damage` | `float / 0.005` | Maximum arc damage. |
| `max_curve_atack_impulse` | `float / 0.012` | Maximum arc impulse. |
| `change_target_timeout_ms_min`, `change_target_timeout_ms_max` | `float / 500, 5000` | Minimum and maximum random target change delay, in ms. |
| `curve_start_y_offset` | `float / 0` | Vertical offset of the arc origin. |
| `use_cascade_electric_curves` | `bool / false` | Enables cascading arcs. |
| `use_touch_objects_by_curves` | `bool / false` | Enables arc contact with objects in cascade mode. |

### Rain contact

Select the section with `rain_collide_particles_section`.

| Key | Type and fallback | Effect |
| --- | --- | --- |
| `air_particle_path` | `string` | Particle effect at the rain contact point. |
| `ground_particle_path` | `string` | Particle effect at the same X and Z, at the anomaly center’s height. |

### Movement example

Add the section reference to an existing anomaly. Define the settings in the same LTX or a file included by `system.ltx`.

```ini
; existing anomaly section
movement_options_section = example_anomaly_movement

[example_anomaly_movement]
use_movement = true
use_movement_always_mode = true
movement_speed = 1.5
movement_radius = 8
```
