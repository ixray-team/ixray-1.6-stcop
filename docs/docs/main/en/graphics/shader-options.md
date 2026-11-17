# Shader options

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: IX-Ray Platform 1.2

## Overview

The `[shaders_options]` section in `gamedata/configs/engine_external.ltx` supplies global shader compilation macros. The engine reads the file during initialization. Restart the game after editing it.

## Syntax

```ini
[shaders_options]
USE_LEGACY_LIGHT = 1
USE_LEGACY_SKY_TONEMAP = 1
IBL_MAX_LOD = 10
```

The engine skips entries without a value, leaves the macro undefined for an exact `0`, and passes other values as macro text. For `#ifdef` conditions, use `1` to define a macro and `0` or omit the entry to leave it undefined. Numeric options such as `IBL_MAX_LOD` are used in calculations. A line starting with `;` is a comment.

R1, R2, and R4 receive the global macros. The effect of each macro depends on the selected renderer's shaders. Use the exact names from the shader sources.

## Global options

| Macro | Effect |
| --- | --- |
| `USE_LEGACY_LIGHT` | Selects the legacy lighting path in R4 and disables the screen-space and off-screen reflection options initialized alongside it. |
| `USE_SOC_LIGHTING` | Enables compatibility with the SoC lightmap format. |
| `USE_DIFFUSE_SKY_COLOR` | Uses diffuse sky color for ambient lighting in R2, R4, and editor shaders. |
| `USE_BGRA_SKYCOLOR` | Switches sky color channels to BGRA in shaders that use this macro. |
| `USE_LEGACY_SKY_TONEMAP` | Uses the original sky color transform. |
| `USE_NORMAL_HEMI_DISTRIBUTION` | Uses the surface normal in R4 hemisphere lighting. |
| `USE_CGIM_SKY_TWEAK` | Changes R4 hemisphere lighting, including the interior pass. |
| `USE_CGIM_WHITE_TWEAK` | Changes R4 `tonemap` and `detonemap` coefficients. |
| `USE_CGIM_COLOR_TWEAK` | Applies `CGIMTonemap` in the final R4 pass after color adjustments and before the LUT. |
| `USE_CGIM_BLOOM_TWEAK` | Applies `BrokeBloom` in the final R4 pass when `r2_new_bloom_tonemap` is disabled. |
| `IBL_FAKE_IRRADANCE` | Blends the diffuse environment map into PBS specular sampling based on roughness. The source uses the spelling `IRRADANCE`. |
| `IBL_REMAP_IRRADANCE` | Remaps the diffuse environment sampling direction in PBS. |
| `IBL_REMAP_REFLECTIONS` | Remaps the specular environment sampling direction in PBS. |
| `IBL_REMAP_POSITIVE_Y` | Mirrors negative Y directions into the positive hemisphere for specular sampling. |
| `IBL_MAX_LOD` | Sets the multiplier for selecting the environment mip level from roughness. |
| `USE_FULL_SKY_SPHERE` | Changes sky and water reflection sampling for full sky textures in R4. |
| `DISABLE_MOTION_VECTORS` | Disables R4 motion vectors. The renderer reads this option during initialization. |
| `ALLOW_WBOIT_TRANSPARENCY` | Enables WBOIT in R4 transparency passes that support it. |

Custom shaders can use additional macro names from the same section. A macro affects rendering when a shader or the renderer uses it.

## Per-texture THM override

An LTX alongside a THM supplies macros for individual materials. See [THM override](thm-override.md) for the path, example, and precedence rules.

## R4 shader Lua functions

These globals belong to the R4 shader VM, which loads shader `.lua` scripts. Game `.script` files run in a separate VM.

| Call | Effect |
| --- | --- |
| `GetShaderOption(name)` | Returns a `boolean` indicating whether the key is present in the global options loaded from `engine_external.ltx`. |
| `AddShaderOption(name, value)` | Adds a macro with a string value to the renderer's current macro set. Call it before compiling the pass that needs it. |
| `ClearAllShaderOptions()` | Clears the renderer's current macro set. The global `engine_external.ltx` options remain available. |

```lua
if GetShaderOption("MY_SHADER_TWEAK") then
    AddShaderOption("MY_SHADER_TWEAK", "1")
end
```

## See also

- [THM override](thm-override.md)
- [PBR](pbr.md)
- [Shader constants](shader-constants.md)
