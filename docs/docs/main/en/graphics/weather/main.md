# Weather
## Overview

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimal version**: 1.1

Various weather changes.

## Weather cycle

```ini
tree_amplitude_intensity = 12.0; (0...250) Tree bend in wind
```

## Rain

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimal version**: 1.3

```ini
[rain]
max_desired_items                 = 2500
source_offset                     = 40.0
drop_angle                        = 3.0
drop_max_angle                    = 10.0
drop_max_wind_vel                 = 20.0
max_particles                     = 1000
particles_cache                   = 400
particles_time                    = 0.3
source_rain_radius_render         = 12.5
add_const_dist_coefficient        = 30.0
add_const_dist_coefficient_render = 40.0
```

## Raindrops in a weather frame

> **Minimum version**: 1.4

Put these options in a weather cycle time section such as `[12:00:00]`. They load independently of the global rain spawn area settings.

| Key | Type and fallback | Effect |
| --- | --- | --- |
| `rain_type` | string `default` | `drizzle` lowers drop speeds, `dense` uses a smaller spawn radius, and `spherical` distributes drops in the upper hemisphere. Other values use the standard distribution. |
| `rain_angle` | number `0` | Contributes to the drop tilt together with wind. |
| `rain_angle_rotation` | number `0` | Rain direction azimuth in degrees. The reader converts it to radians. |
| `rain_length`, `rain_width` | numbers `5.0`, `0.3` | Length and width of the rendered drop streak. |
| `rain_speed_min`, `rain_speed_max` | numbers `40.0`, `80.0` | Bounds for random drop speed. For `drizzle` the lower bound is multiplied by `0.5` and the upper bound by `0.7`. |

For example, add these keys to an existing weather time section.

```ini
rain_type = drizzle
rain_length = 3.0
rain_width = 0.2
rain_speed_min = 40
rain_speed_max = 80
```

In old-style weather files, `env_ambient` names the ambient sound section and `thunderbolt` names the lightning collection. The new-style loader uses `ambient` and `thunderbolt_collection`. If a lightning collection is configured, `bolt_period` and `bolt_duration` take precedence over `thunderbolt_period` and `thunderbolt_duration`. The legacy `sun_dir` holds two angles in degrees and is read when `sun_altitude` is absent.

## Ambient environment

In an ambient section, `sound_channels` takes precedence over `snd_channels`, and either one takes precedence over legacy `sounds`. The two channel keys contain comma-separated channel section names. With only `sounds`, channel settings come from the ambient section itself.

`effect_period` sets the minimum and maximum intervals between effects in seconds. `min_effect_period` and `max_effect_period` can replace the respective elements. In a sound channel, `sound_dist` sets the two distance bounds, which `min_distance` and `max_distance` can override. `sound_period` accepts two or four integer intervals in seconds. With two values, the pair is used twice. Without `sound_period`, `period0`, `period1`, `period2`, and `period3` are read in milliseconds.
