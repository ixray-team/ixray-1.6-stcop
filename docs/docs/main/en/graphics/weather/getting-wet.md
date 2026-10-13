# Getting Wet

R4 supports rain-driven surface wetness and two independent puddle implementations: material-based puddles in the PBR pipeline and noise-based terrain puddles in the legacy lighting pipeline. This page describes the current branch.

## Weather and accumulated wetness

`rain_density` in a weather descriptor controls current rain intensity. The engine also maintains `CEnvironment::wetness_factor`, clamped to `[0, 1]`:

| Weather | Change per second |
| --- | --- |
| `rain_density > 0` | Wetness increases by `rain_density * 0.025`. |
| No rain | Wetness decreases by `0.0125`. |

At density `1`, reaching full wetness from dry takes approximately 40 seconds. Drying from full wetness after rain stops takes approximately 80 seconds. Lower rain density increases the accumulation time. Puddles therefore appear gradually and remain while the environment dries; the ripple intensity follows current rainfall.

This state is separate from [visor drops](../../gameplay/equipment/visor-glass.md), which have their own accumulation and wiping callback. Wiping the visor does not affect puddles.

## Legacy terrain puddles

Enable legacy lighting in `configs/engine_external.ltx`:

```ini
[shaders_options]
USE_LEGACY_LIGHT = 1
```

Enable puddles and the existing water SSR in the console:

```text
r4_rain_puddles on
r4_sslr_water on
```

Both options require a game restart. `r4_rain_puddles` is off initially unless enabled by a user profile. It is also available as **Rain puddles (legacy)** in advanced graphics/effects settings. It applies only to R4 with legacy lighting; enabling it in PBR does not change PBR puddles or reflections.

### Shape, textures and eligible surfaces

The puddles are **texture-driven**, rather than entirely generated from mathematical noise. The shader samples a repeating Perlin-noise texture to form their contours, multiplies coverage by accumulated wetness and the geometric slope, and softens the edges. It modifies the terrain's color, gloss and normal, with animated waves, rain ripples and refraction of detail textures.

| Resource relative to the game package | Purpose |
| --- | --- |
| `textures/shaders/rain_puddles_perlin.dds` | Puddle contours. |
| `textures/shaders/rain_puddles_normal.dds` | Two animated layers of surface waves. |
| `textures/shaders/rain_puddles_ripples.dds` | Three animated layers of rain-impact ripples. |

These three textures are shared by all levels. **No separate puddle mask for each level or terrain texture is required.** They are supplied in all four game packages and must be installed together with the shaders.

Puddles use the terrain's texture coordinates and geometric normal. The slope factor is `saturate((WorldNormal.y - 0.997) * 102.997)`, so they appear on nearly horizontal, upward-facing terrain. They also work in the terrain's lower-detail pass. They do not project onto ordinary building roofs, walls, actors or weapons. The material-based PBR implementation remains responsible for its own supported surfaces and mask projection.

### Reflections

The puddle reflection pass calls the existing water shader's `ScreenSpaceLocalReflections` function. It uses scene depth, the puddle normal and reprojection into the previous frame's scene image. The world SSLR/VSLR passes are not enabled for legacy lighting by this feature.

`r4_sslr_water on` enables scene SSR for water and the new puddles. With it off, puddles still reflect the environment sky maps. Those maps also provide the fallback where SSR cannot find a visible scene hit. Reflections fade at screen borders and in fog, as in the existing water path; offscreen objects are not guaranteed to appear.

## Existing PBR puddles

With `USE_LEGACY_LIGHT = 0`, the existing PBR wet-surface shader continues to use `rain_mask.dds`, its level projection bounds, surface geometry and material properties. It changes roughness, specular response, color and normals before the existing reflection pipeline processes the materials. Its mask and SSLR/VSLR behavior are independent of `r4_rain_puddles`.

`r3_dynamic_wet_surfaces` controls the existing dynamic wet-surface pass. It can also add the usual wet-surface splashes and streaks in legacy lighting; the new terrain puddles preserve their coverage through that pass. Terrain puddles themselves use weather wetness and do not require this switch to generate their contours.

`r4_puddles` is another separate option: it controls level-authored volumetric puddle geometry. It is not an alias for the new `r4_rain_puddles` option and does not enable the noise-based terrain effect.

## Troubleshooting and verification

| Symptom | Check |
| --- | --- |
| No new puddles | R4, legacy lighting, `r4_rain_puddles on` before restarting; accumulated wetness and nearly flat terrain in view. |
| Only sky reflections | Enable `r4_sslr_water` and restart. SSR also needs a visible scene hit and previous-frame image data. |
| No puddles on a roof | Expected for the terrain implementation. PBR mask projection is a separate implementation. |
| Puddles remain after rain stops | Expected: environmental wetness dries gradually. |
| Option does nothing in PBR | Expected: it is restricted to legacy lighting. Check the PBR mask and its existing rendering settings instead. |
| Missing textures or shader compilation errors | Install the new shader/helper and all three DDS files from the same game package. |

For a mod check, compare dry weather with sustained rain, then stop rain and check gradual drying. Test water SSR both on and off, including camera movement and screen borders. Compare legacy lighting with `r4_rain_puddles` disabled, and verify that toggling this option does not alter PBR rendering after restart.

## Implementation

`shaders/d3d11/shared/RainPuddles.hlsli` computes coverage and animation; `deffer_impl.ps.hlsl` applies it to terrain. Only the `USE_LEGACY_LIGHT` + `USE_RAIN_PUDDLES` permutation uses this code. Terrain coverage is stored in the legacy surface buffer's third channel; `rain_render.cs.hlsl` preserves it when repacking wet surfaces. PBR packing and its puddle calculations remain unchanged.

`src/Layers/xrRenderPC_R4/RainPuddles.cpp` blends the reflection pass into the combined opaque scene before forward rendering. `BlenderRainPuddles.cpp` binds the existing water SSR inputs. `r4.cpp` restricts the option and shader permutation to legacy lighting. When disabled, the original compact legacy surface format is retained.
