# Getting Wet

R4 supports general wet surfaces, rain puddles and separate level-authored volumetric puddles. Rain puddles use a texture mask in PBR when one is available, otherwise PBR automatically uses procedural contours. Legacy lighting uses procedural terrain puddles.

## Weather and accumulated wetness

`rain_density` in a weather descriptor controls current rainfall. `CEnvironment::wetness_factor` stores accumulated wetness in `[0, 1]`:

| Weather | Change per second |
| --- | --- |
| `rain_density > 0` | Increase by `rain_density * 0.025`. |
| No rain | Decrease by `0.0125`. |

At density `1`, full wetness from dry takes approximately 40 seconds; drying from full wetness takes approximately 80 seconds. Lower rainfall increases accumulation time. Puddles remain while the environment dries; rain-impact ripples follow current rainfall.

This state is separate from [visor drops](../../gameplay/equipment/visor-glass.md). Wiping the visor does not affect puddles.

## Options

```text
r4_rain_puddles on
r3_dynamic_wet_surfaces on
```

**Rain puddles** in advanced graphics settings uses `r4_rain_puddles` for both PBR and legacy lighting. It is initially off unless enabled by a user profile. Restart the game after enabling it so procedural shaders and their resources can be created. Disabling it takes effect immediately in the current frame's material calculations and puddle reflection pass.

`r3_dynamic_wet_surfaces` independently controls general wet-surface shading, splashes and streaks. Turning it off does not disable rain puddles. Turning puddles off does not disable general wet surfaces.

| `r4_rain_puddles` | `r3_dynamic_wet_surfaces` | Result |
| --- | --- | --- |
| on | on | Rain puddles and general wet surfaces. |
| on | off | Rain puddles without general wet-surface shading. |
| off | on | General wet surfaces without rain puddles. |
| off | off | Neither effect. |

`r4_puddles` remains a separate switch for level-authored volumetric puddle geometry. It is not an alias for `r4_rain_puddles`.

## Selecting the puddle algorithm

Lighting is selected in `configs/engine_external.ltx`:

```ini
[shaders_options]
USE_LEGACY_LIGHT = 1
```

Use `0` or omit this option for PBR. Check DLTX `mod_engine_external_*.ltx` files as well: addons can override the selected lighting mode.

| Lighting and resources | Puddle generation | Reflections |
| --- | --- | --- |
| Legacy | Procedural terrain contours. | Existing water SSR and sky-map fallback. |
| PBR with `rain_mask.dds` | Existing projected texture-mask calculation. | Native PBR materials and SSLR/VSLR. |
| PBR without a level `rain_mask.dds` | Procedural contours on terrain only. | Native PBR materials and SSLR/VSLR. |

The renderer checks `$level$/rain_mask.dds` (for example, `levels/pripyat/rain_mask.dds`), including files in mounted level archives. If the level has no mask, the procedural fallback is selected automatically; no extra option or authored mask is needed. The shared `textures/rain_mask.dds` placeholder is retained but does not count as a mask for the current level. Selection and the mask texture are refreshed on level changes and device resets, so a previous level's mask is not reused accidentally. A present but empty level mask still selects the mask implementation. With puddles enabled, the log reports `Rain puddles: PBR procedural (no level rain_mask.dds)` or `Rain puddles: PBR level mask` followed by its path.

The existing PBR mask projection, material coefficients, slope and rain-shadow calculations are retained. The fallback uses the shared Perlin contours, waves and rain-impact ripple algorithm; its coordinates come from the existing PBR level-projection bounds. It modifies the G-buffer's roughness, specular response, color, AO and normal before native reflections. It does not use the legacy reflection overlay or overwrite PBR specular data with a puddle coverage channel.

## Procedural contours and surfaces

Contours are **texture-driven**, rather than generated entirely from mathematical noise:

| Resource relative to the game package | Purpose |
| --- | --- |
| `textures/shaders/rain_puddles_perlin.dds` | Puddle contours. |
| `textures/shaders/rain_puddles_normal.dds` | Two animated surface-wave layers. |
| `textures/shaders/rain_puddles_ripples.dds` | Three animated rain-impact ripple layers. |

All three textures are supplied in all four game packages and shared by all levels. A separate terrain or level puddle mask is not required for the procedural algorithm.

Coverage follows accumulated wetness and the upward geometric normal. Contour edges retain the soft `smoothstep(0, 0.255, Amount)` transition. Legacy procedural puddles keep the slope factor `saturate((0.1 - max(abs(WorldNormal.x), abs(WorldNormal.z))) * 13)` and remain limited to almost horizontal terrain. The PBR fallback is also restricted to terrain (`TERRAIN_ID`), including shallow terrain slopes. Ordinary static geometry (`BASE_ID`), such as separate pavement or roof meshes, is excluded. It uses `1 - smoothstep(0.05, 0.2, max(abs(WorldNormal.x), abs(WorldNormal.z)))` after contour smoothing, so slopes attenuate coverage without changing the contour threshold. Geometric normals are reconstructed over a projected 25 cm radius, bounded by one texel and 2% of the viewport in each axis, to soften transitions between nearby terrain faces. This reuses the existing four neighboring depth samples. Rain-shadow checks still apply. Objects, foliage and HUD geometry are excluded; vertical walls fail the slope check.

Legacy terrain shading also refracts detail-texture coordinates. The PBR fallback works on the already rendered G-buffer and therefore uses native material shading instead of detail-texture refraction. Its water normal follows the reconstructed geometric normal, with waves and rain-impact ripples applied in the surface's tangent frame. Keeping the reflection normal aligned with shallow slopes reduces ground self-reflections at grazing angles. The native PBR reflection algorithm is unchanged.

### Water exclusion and shore transitions

Procedural puddles exclude surfaces under water. At level load, R4 builds a water-height and coverage map from static collision triangles whose game material has both `flLiquid` and `flPassable`. Custom water needs correctly tagged collision surfaces for this exclusion to work.

The map is 2048 by 2048 in `R16G16_UNORM`: normalized height and coverage occupy two 16-bit channels, totaling 16 MiB of GPU memory. It uses one filtered texture sample. Dividing filtered height by coverage avoids mixing valid heights with empty cells. Puddle visibility fades with water coverage and between 2 and 25 cm above the water surface, after contour smoothing. This avoids abrupt shoreline cuts and supports water bodies at different heights independently of camera visibility.

The map is prepared on level load or device reset, without per-frame collision queries. Levels without water use a neutral one-pixel map. PBR levels using a supplied puddle mask do not allocate the procedural water map. That path retains its existing masked behavior.

## Reflections

Legacy puddles call the water shader's existing `ScreenSpaceLocalReflections` function. Enable scene reflections with:

```text
r4_sslr_water on
```

This shader option requires a restart. Without it, legacy puddles still reflect sky maps. Sky maps also provide fallback when SSR has no visible scene hit. Border and fog fading remain part of the water SSR path.

PBR puddles, with either a supplied mask or procedural fallback, use the existing PBR SSLR/VSLR pipeline and material parameters. Their scene reflections are independent of `r4_sslr_water`; this switch still controls water surfaces and the legacy puddle overlay.
