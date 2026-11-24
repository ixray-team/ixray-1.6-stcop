# Getting Wet

The advanced renderer supports general wet surfaces, rain puddles and separate level-authored volumetric puddles. The simplified renderer has general wet surfaces at high shading quality on surfaces with a normal map, and procedural rain puddles on terrain with PBR textures; it has no texture-mask puddles and no level-authored volumetric puddles. Rain puddles use a texture mask in PBR when one is available, otherwise PBR automatically uses procedural contours. Legacy lighting uses procedural terrain puddles.

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
r_wet_surfaces on
```

`r_wet_surfaces` (`off` or `on`, default `on`) is one switch for general wet-surface shading, splashes and streaks, rain puddles in both PBR and legacy lighting, and level-authored volumetric puddles. The effects can no longer be turned on one by one. Run `r_restart` after changing it so procedural shaders and their resources can be created or released. The option is part of the quality presets: `r_quality_preset` sets it, and a different value reads as `custom`.

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

The existing PBR mask projection, material coefficients, slope and rain-shadow calculations are retained. The fallback fills the terrain from its low places up as wetness accumulates, with the shared wave and rain-impact ripple textures; its coordinates come from the existing PBR level-projection bounds. It changes the terrain's roughness, specular response, color, AO and normal as a part of its material, before lighting and native reflections. It does not use the legacy reflection overlay or overwrite PBR specular data with a puddle coverage channel.

## Procedural contours and surfaces

Contours are **texture-driven**, rather than generated entirely from mathematical noise:

| Resource relative to the game package | Purpose |
| --- | --- |
| `textures/shaders/rain_puddles_perlin.dds` | Puddle contours. |
| `textures/shaders/rain_puddles_normal.dds` | Two animated surface-wave layers. |
| `textures/shaders/rain_puddles_ripples.dds` | Three animated rain-impact ripple layers. |

All three textures are supplied in all four game packages and shared by all levels. A separate terrain or level puddle mask is not required for the procedural algorithm.

Coverage follows accumulated wetness and the upward geometric normal. In PBR the water level rises to 0.6 of the terrain layers' height range at full wetness, and terrain below it is under water; layers that let light through hold none. Legacy contour edges retain the soft `smoothstep(0, 0.255, Amount)` transition. Legacy procedural puddles keep the slope factor `saturate((0.1 - max(abs(WorldNormal.x), abs(WorldNormal.z))) * 13)` and remain limited to almost horizontal terrain. The PBR fallback is also restricted to terrain (`TERRAIN_ID`), including shallow terrain slopes. Ordinary static geometry (`BASE_ID`), such as separate pavement or roof meshes, is excluded. It uses `1 - smoothstep(0.05, 0.2, max(abs(WorldNormal.x), abs(WorldNormal.z)))` on the terrain's own normal, so slopes attenuate coverage without changing the water level. Rain-shadow checks still apply. Objects, foliage and HUD geometry are excluded; vertical walls fail the slope check.

Legacy terrain shading also refracts detail-texture coordinates. The PBR fallback is part of the terrain's material and uses native material shading instead of detail-texture refraction. Its water normal follows the terrain's geometric normal, with waves and rain-impact ripples applied in the surface's tangent frame. Keeping the reflection normal aligned with shallow slopes reduces ground self-reflections at grazing angles. The native PBR reflection algorithm is unchanged.

### Water exclusion and shore transitions

Procedural puddles exclude surfaces under water. At level load, R4 builds a water-height and coverage map from static collision triangles whose game material has both `flLiquid` and `flPassable`. Custom water needs correctly tagged collision surfaces for this exclusion to work.

The map is 2048 by 2048 in `R16G16_UNORM`: normalized height and coverage occupy two 16-bit channels, totaling 16 MiB of GPU memory. It uses one filtered texture sample. Dividing filtered height by coverage avoids mixing valid heights with empty cells. Puddle visibility fades with water coverage and between 2 and 25 cm above the water surface, after contour smoothing. This avoids abrupt shoreline cuts and supports water bodies at different heights independently of camera visibility.

The map is prepared on level load or device reset, without per-frame collision queries. Levels without water use a neutral one-pixel map. PBR levels using a supplied puddle mask do not allocate the procedural water map. That path retains its existing masked behavior.

## Reflections

Legacy puddles call the water shader's existing `ScreenSpaceLocalReflections` function. Enable scene reflections with:

```text
r_water_reflections on
```

`r_water_reflections` (`off` or `on`, default `on`) is applied with a shader recompile on the next frame. Without it, legacy puddles still reflect sky maps. Sky maps also provide fallback when SSR has no visible scene hit. Border and fog fading remain part of the water SSR path.

PBR puddles, with either a supplied mask or procedural fallback, use the existing PBR SSLR/VSLR pipeline and material parameters. Their scene reflections are independent of `r_water_reflections`; this switch still controls water surfaces and the legacy puddle overlay.
