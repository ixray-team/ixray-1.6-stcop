# Console commands

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

Enter `help` in the console to list the commands, current values, and argument information available in the running build. The list depends on the renderer, game mode, and build flags. Commands registered by a mod or on first use appear after registration.

Use `1` and `0` for switches in this reference. Both Boolean and bit-mask commands accept those values. Some commands display their current value when called without an argument.

## Syntax

```text
r__screenshot_format ss_png
screenshot test
rs_main_menu_fps_limit 60
```

## Display and rendering

| Command | Values | Effect |
| --- | --- | --- |
| `rs_fps_limit` | `0–1000` | In-game FPS cap. `0` removes the cap. |
| `rs_main_menu_fps_limit` | `0–1000` | Separate main-menu cap. Initial value `144`. `0` removes the cap. |
| `rs_fps_show` | `0 / 1` | Displays the FPS counter. |
| `rs_fps_color_r`, `rs_fps_color_g`, `rs_fps_color_b`, `rs_fps_color_a` | `0–255` | Counter color and opacity. |
| `rs_fps_outline` | `0 / 1` | Outline for the FPS counter. |
| `r__detail_density` | `0.25–1.0` | Detail density. On a loaded level the command also reinitializes the detail cache. |
| `r__detail_radius` | `50–500` | Detail draw radius. |
| `r__shader_cache` | `0 / 1` | Toggles shader cache use. |
| `r2_vignette`, `r2_aberration`, `r2_saturation` | `on / off` | Vignette, chromatic aberration, and saturation correction in R2/R4 post-processing. |
| `r2_cloud_shadows` | `on / off` | Cloud shadows in R2/R4. Restart the renderer to apply. |
| `r4_hud_shadows` | `on / off` | Screen-space HUD shadows in R4. |
| `r4_sslr_water`, `r4_sslr_reflections` | `on / off` | R4 reflections for water and world geometry respectively. |
| `r4_vslr_enable` | `on / off` | Enables the R4 off-screen geometry reflection path. |
| `r2_sun_hom` | `0 / 1` | Allows HOM occlusion in the sun pass when the renderer can prepare HOM. The dynamic renderers share this command name. |
| `r__screenshot_format` | `ss_jpg / ss_tga / ss_png` | Regular screenshot format. Initial value `ss_png`. |
| `screenshot` | `name` | Captures a named screenshot on the client. |

## R4 post-processing

| Command | Values | Effect |
| --- | --- | --- |
| `r2_new_autoexposure` | `0 / 1` | Selects the new exposure adaptation. Initially disabled. |
| `r2_new_bloom_tonemap` | `0 / 1` | Selects the new bloom path. Initially disabled. |
| `r2_autoexposure_key` | `0.01–1.0` | Exposure key for the new adaptation. |
| `r2_autoexposure_min`, `r2_autoexposure_max`, `r2_autoexposure_bias` | `−10–10` | Lower and upper bounds and bias of the new exposure in log space. |
| `r2_autoexposure_speed` | `0–100` | Adaptation speed. |
| `r2_bloom_amount` | `0–3` | Bloom intensity. |
| `r2_bloom_desaturation`, `r2_bloom_tint_amount` | `0–1` | Desaturation and tint strength in the new bloom path. |
| `r2_crossfeed`, `r2_vibrance` | `0 / 1` | Enable the corresponding color operations in the final pass. |
| `r2_tonemap_compression` | `0–10` | Compression for the new tone mapping path. |
| `r2_tonemap_desaturation` | `0–1` | Desaturation for the new tone mapping path. |
| `r2_tonemap_crossfeed` | `0–0.9` | Crossfeed strength when `r2_crossfeed` is enabled. |
| `r2_tonemap_vibrance` | `−1–1` | Vibrance strength when `r2_vibrance` is enabled. |
| `r2_rain_drops_mode` | `texture / procedural` | Selects texture-based or procedural raindrops. |

## Gameplay and input

| Command | Values | Effect |
| --- | --- | --- |
| `hud_contextual_status` | `0 / 1` | Toggles the [contextual HUD](../interface/hud-display-options.md). |
| `hud_crosshair_point` | `0 / 1` | Toggles the crosshair point. |
| `g_hit_slowmo` | `0 / 1` | Toggles actor slowdown from hits. |
| `g_autosaves_enabled` | `0 / 1` | Toggles timed autosaves. |
| `g_autosaves_interval` | `1–120` | Autosave interval in minutes. |
| `g_quicksaves_count` | `1 / 5 / 10 / 20` | Number of slots in the quicksave rotation. |
| `set_game_time` | `HH MM` | In an active server session, passes hours (0–23) and minutes (0–59) to `xr_effects.set_game_time` when available. |
| `gamepad_sens` | `0.1–0.8` | Gamepad sensitivity. |
| `gamepad_invert` | `0 / 1` | Gamepad inversion. |
| `gamepad_vibration` | `on / off`, `true / false`, `1 / 0` | Enables controller vibration through SDL. |
| `gamepad_prefix_override` | `gp_none / xbox1 / ps4 / ps5` | Overrides the button icon family. |

## Binding a command to a key

`bind_console <command> <key>` records a binding. The last token is the key name, such as `kF10`. For Boolean and bit-mask commands, pressing the key toggles the current value. `unbind_console <key>` removes the binding.

```text
bind_console rs_fps_show kF10
unbind_console kF10
```

## Registering commands from Lua

`get_console():register_lua_command(name, handler, tips)` registers a command. The handler receives a 1-indexed argument table. Separate arguments and tips with commas. Use a unique mod-specific name. Re-registering a Lua command updates its handler and tips. Built-in commands are reserved. Register mod commands again after the Lua VM is recreated.

```lua
local console = get_console()
console:register_lua_command("my_mod_echo", function(args)
    printf("%s / %s", args[1] or "", args[2] or "")
end, "first,second")
-- In the console: my_mod_echo first,second
```

## See also

- [Launch arguments](launch-keys.md)
- [Settings presets](../configs/preset-user-parameters.md)
- [Shader options](../graphics/shader-options.md)
