# Procedural source and Sun (R4, first implementation)

Optional parameters in each weather time section, with their defaults:

```ini
celestial_mode       = 0
source_intensity     = 1.0
source_angular_size  = 0.53
disk_luminance_scale = 1.0
sun_corona_intensity = 0.00001
```

- `celestial_mode`: 0 = Sun; 1 = Moon lighting (the Moon sprite is not implemented yet); 2 = no directional source. The mode switches at the weather interpolation midpoint, like resource IDs. Fade intensity to zero around a mode change.
- `source_intensity`: relative multiplier, 0..100. The common source gain is the interpolated weather `sun_color` times this value times `r2_sun_lumscale`. It is applied once to SkyView, AP scattering, direct cloud lighting and R4 directional lighting. Cloud ambient already inherits the gain through the octomap. Atmospheric transmittance is independent of source intensity.
- `source_angular_size`: full diameter in degrees, 0.05..10. Changing the diameter preserves the approximate irradiance of the disk before the HDR storage limit. The default is 0.53 degrees.
- `disk_luminance_scale`: visual disk/corona multiplier, 0..100. It does not change lighting of the atmosphere, clouds or geometry.
- `sun_corona_intensity`: retained for config compatibility, currently unused. The artificial corona has been removed; only the uniform disk with an antialiased edge is rendered. Atmospheric scattering around the Sun remains part of SkyView.

`sun_color` remains an artistic RGB tint and strength, not a measured physical illuminance. The current spectral reference and `SKY_RADIANCE_SCALE` are preserved. The atmospheric filter is applied after weather authoring; a red weather tint will further redden the physically attenuated sunset. For a neutral transmittance test use a neutral nonzero `sun_color`.

The renderer preserves the raw configured/dynamic source direction for atmospheric scattering, the disk, and cloud shadows. Legacy `sun_dir` and `sun_color` retain their old horizon clamp/fade for older rendering paths. R4 ground directional lighting uses the raw direction while it is above the horizon and is disabled when the source centre is below it. Atmosphere and clouds can still be illuminated then. Moon and moonless modes use the configured direction instead of the automatic solar trajectory.

For the Sun to follow weather coordinates, enable `ReadSunConfig = true` in the active `engine_external.ltx` (restart required). When false, the existing dynamic-sun path overrides the weather direction unless the renderer uses a static Sun or the weather is old-style. The weather editor now edits `source_dir` and updates the compatible `sun_dir` together. Coordinates are displayed in degrees, matching the file. Historical config naming is preserved: `sun_altitude` is the heading (H) and `sun_longitude` is the pitch (P). Saving weather preserves the unclamped direction and the celestial parameters.

The disk is rendered in `sky.ps`, with approximately one-pixel linear antialiased coverage, without limb darkening or artificial corona. Squared chord distance avoids per-pixel trigonometry and square roots in the disk profile; angular-radius and solid-angle constants are prepared by the CPU binder. The disk profile has no early branch, so atmospheric lookup runs outside the visible disk too; measure the performance tradeoff in-game. Shared atmospheric intersection logic remains unchanged. Transmittance and planet occlusion are evaluated per view ray, so the lower and upper disk can still have different extinction at the horizon. The same atmospheric camera-height mapping is used as SkyView/AP. Composition is `cloud.rgb + cloud.a * sky + sun_visibility * disk`, with no second AP application. The final sky colour is limited to a peak of 60000 while preserving hue to avoid overflow in the RGBA16F scene target; very bright/small disks can reach this storage limit.

Rebuild R4 together with the updated shader: `celestial_params` now stores mode, inverse squared chord radius, disk scale divided by solid angle, and relative corona intensity. If a dark ring remains, compare the sky target before temporal/upscale/sharpen passes with the final image. The existing TAA bicubic filter has negative weights and is a possible source of HDR-edge ringing; it has not been changed by this patch.

`sky_cloud_sun_visibility()` in `common_celestial.hlsli` strengthens cloud occlusion only for the solar disk and corona: `sun_visibility = saturate(1 - (1 - cloud.a) / 0.6)^8`. A clear pixel passes the source unchanged; effective opacity >= 0.6 hides it completely, with a smooth approach to zero. This is an artistic visibility adjustment, not an increase of physical cloud density or a change to cloud lighting. The 0.6 threshold is exposed as `SKY_SUN_CLOUD_OPAQUE_OPACITY`. Cloud alpha still includes distance fading, so distant clouds fading completely into the sky also cease to hide the disk.

The sky pass reads `s_celestial_transmittance_lut` at PS t4 through both the blender and the environment mixer's actual texture list. No new render target is required. The disk is not yet injected into the sky octomaps/reflections. Both legacy LensFlare draw sites in R4 (source sprite and flare/gradient) are temporarily disabled; object lifetime is unchanged.

Mode 2 does not invent a night sky or ambient light: source-driven atmospheric scattering becomes zero. Existing independent ambient/background contributions remain separate. The Moon texture and a night background belong to later stages.

## Runtime checks

1. Rebuild all modules affected by `Environment.h`; the environment descriptor layout changed. Do not mix an old engine/game DLL with the rebuilt renderer.
2. In RenderDoc verify PS t4 contains `shaders/sky/transmittance_lut`, and the sky shader constants contain the expected `celestial_source_color` and `celestial_params`.
3. With neutral nonzero `sun_color`, test the disk at zenith and near the horizon: it should redden and be clipped per pixel, without a second legacy sprite. Test at several FOVs and internal resolutions.
4. Sweep `source_intensity` through 1, 0.5 and 0: SkyView, AP scattering, cloud lighting and the directional light should respond together; AP transmittance must remain unchanged. A large lighting jump or mode switch invalidates cloud history.
5. Check that opaque clouds hide the disk and that clear sky reveals it. Cloud alpha is effective transmittance, including the existing distance fade.
6. Compare mode 0 against mode 1 (same lighting, no solar disk) and mode 2 (zero source contribution). The mode 1 Moon image is intentionally pending the next implementation step.

Offline validation: FXC shader compilation and MSBuild `ClCompile` for R4/xrEngineCore. GPU timings and appearance require an in-game capture.
