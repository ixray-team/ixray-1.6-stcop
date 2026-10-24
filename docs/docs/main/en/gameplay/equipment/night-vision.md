# Night vision

Night vision uses three shader profiles with different color, gain, noise and edge attenuation. The device also draws a separate UI mask. Electronics interference fades the shader effect out while the mask remains visible until the device is switched off.

This page describes the current branch and the `gamedata`, `gamedata_soc`, `gamedata_cs` and `gamedata_coc` packages. The shader profiles, UI mask and electronics interference work with R1, R2 and R4. R1 and R2 apply the shader effect in their final postprocess pass; R4 uses its dedicated night-vision pass. Profiles with `shader_effect_range = 0` retain the existing PPE effect. Upgrading the active helmet or outfit refreshes enabled night vision immediately, without toggling it.

## Equipment and profiles

A helmet or outfit selects a postprocess section through its item configuration:

```ini
nightvision_sect = effector_nightvision_2
```

An equipped helmet takes priority over the outfit, followed by a separate device in the night-vision slot. The separate device also reads `nightvision_sect`; its existing battery and equipment checks remain active. Removing equipment, switching the device off or exhausting its battery ends its effect.

The CoP and CoC packages provide `effector_nightvision_1`, `effector_nightvision_2` and `effector_nightvision_3`. SoC and CS retain their section names: `effector_nightvision` and `effector_nightvision_bad` use generation 1, while `effector_nightvision_good` uses generation 2.

Configure a shader profile in `configs/misc/postprocess.ltx` or another included configuration file:

```ini
[effector_nightvision_2]
shader_effect_range = 2
shader_nvg_color = 0.4, 0.99, 0.83, 10.0
shader_nvg_postprocessing = 0.125, 0.005, 0.0125, 0.9
pp_eff_name = nightvision_2.ppe
pp_eff_cyclic = 1
```

Keep the existing PPE parameters for static lighting and for profiles with `shader_effect_range = 0`. The example extends an existing effect section; it does not replace the package's PPE settings.

| Parameter | Meaning |
| --- | --- |
| `shader_effect_range` | `1`, `2` or `3` selects a shader generation. Missing values default to `1`; values above `3` are clamped to `3`. `0` selects the configured PPE effect even with dynamic lighting. |
| `shader_nvg_color` | Four values: red, green, blue and luminance gain. The provided profiles define all four values. |
| `shader_nvg_postprocessing` | Four values: noise strength, temporal flicker strength, scanline strength and radial attenuation offset. Missing values use the selected generation's defaults. |

Default profiles:

| Generation | RGB | Gain | Noise | Flicker | Scanlines | Radial offset |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | `0.4, 0.9, 0.3` | `8` | `0.175` | `0.009` | `0.025` | `0.8` |
| 2 | `0.4, 0.99, 0.83` | `10` | `0.125` | `0.005` | `0.0125` | `0.9` |
| 3 | `0.8, 0.8, 0.8` | `15` | `0.075` | `0.0025` | `0.005` | `1.0` |

The shader converts scene color to luminance, applies the tint and gain, adds noise and the two small temporal terms, then attenuates the image toward the edges. The independent UI mask adds its own edge darkening.

### Item color overrides and upgrades

An optional `nightvision_color` in a helmet or outfit section overrides the profile color:

```ini
nightvision_sect = effector_nightvision_2
nightvision_color = 0.45, 0.95, 0.7, 12.0
```

The fourth component is gain. Values at or below `1` use the generation's default gain; values above `1` specify a custom gain. Existing three-component item colors therefore keep their tint and receive a generation-dependent gain.

Without an item override, the current `nightvision_sect` supplies `shader_nvg_color`. If that key is also absent, the fallback tint is `0.3, 1.0, 0.2` with the selected generation's gain. A separate device reads its color directly from the profile section.

Upgrades can change `nightvision_sect`. The new profile and its color are resolved on the next activation; there is no live replacement of an already running effect. An explicit item color continues to override the profile after an upgrade.

## UI edge mask

Each package includes `textures/hud/nvg_vignette.dds` and this node in `configs/ui/ui_custom_msgs.xml`:

```xml
<nv_tunnel x="0" y="0" width="1024" height="768" stretch="1">
    <texture>hud\nvg_vignette</texture>
</nv_tunnel>
```

`nv_tunnel` is a single-instance custom static without a lifetime limit. Its coordinates use the UI's reference resolution; stretching fills the current viewport. Mods replacing `ui_custom_msgs.xml` must keep this node or supply an equivalent one. The mask can be replaced by changing its texture while retaining the node name.

The engine maintains the mask when the actor is alive, uses first-person view and has an active, logically enabled night-vision effect. It restores the custom static after UI captions are cleared. Switching off the device, battery depletion, death or leaving first-person view removes the mask. Returning to first-person view restores it if the device remains enabled.

Interference changes shader visibility without switching the device off. Thus a fully suppressed night-vision image still keeps the UI mask, and the device's existing power consumption continues. Weapon and scope night vision use their separate effects and do not create this mask. A 3D night-vision lens with an active `scope_nightvision` PPE profile uses the already processed snapshot and skips its additional material amplification. This prevents applying night vision twice and washing the lens out to white; scopes without an active PPE pass retain their material effect. The same PPE profile remains active while the normal lens fades in and out during zoom animations, so its brightness does not switch to the stronger material effect halfway through the transition. Gauss keeps its `.ppe` effect on 2D scopes. Its 3D `model_scope_gauss` lens shader boosts grayscale, adds a blue tint and applies electronics-interference noise. To use only the shader effect, the Lua lens material must specify `:scope_postprocess(false)`. Setting `true` permits snapshot processing through PPE; when PPE is active, the shader skips its own gain and blue tint while retaining interference noise. The material chooses this behavior independently of profile names in the game component. Target frames still require fully completed zoom.

## Scope lens aberration

R4 supports Gunslinger chromatic aberration for ordinary, night-vision, switchable-reticle and Gauss lenses. The setting retains its original spelling:

```ini
scope_abberation = 0.006
```

Permanent scopes read the weapon section. Detachable scopes read their own section with a fallback to the weapon value. Upgrades can override this parameter; installed values are reapplied when scope parameters reload. The default is `0`.

The scene's red and blue channels move in opposite diagonal directions, with increasing separation towards the lens edge. The reticle and UI are composed afterwards. Coordinates use the scene texture dimensions to support render scaling. Zero strength skips the additional image samples. The parameter uses the existing `m_hud_params.z` channel without adding a shader constant.

## Electronics interference

Night vision uses the existing electronics problems manager. It does not add another emission detector or alter interference for other devices.

The renderer's `m_affects` interface remains unchanged:

| Component | Exported value |
| --- | --- |
| `x` | Current electronics problems counter divided by `10`. |
| `y` | A random value; night vision does not use it for emission flicker. |
| `z` | Target counter divided by `10`. |
| `w` | Whether the counter is decreasing. |

R4 derives `NightVisionInterference` from the same current counter as `m_affects.x`. The manager normally changes the counter smoothly. A local limit of `0.2` normalized units per second also softens immediate script updates while night vision is running; frame delta is capped at `0.1` seconds for this limit. Initial activation takes the current interference value directly, preventing a brief working image when switching on during strong interference.

The shader uses `smoothstep(0.02, 0.4, NightVisionInterference)` to progress from a full effect to no effect. These thresholds correspond to manager counter values `0.2` and `4`. At or above `0.4`, the shader returns the original scene color with no night-vision tint, gain, noise or shader vignette. The UI mask remains visible.

During the transition, two slow pulses at approximately `0.9` and `1.7` Hz slightly reduce the effect's contribution. Their additional attenuation reaches at most `18%`, is strongest halfway through the transition and disappears at its endpoints. They do not brighten the image or randomly switch the whole frame on and off. The profile's small normal temporal flicker also fades with the effect.

When interference decreases, the effect recovers through the same smooth transition. Recovery does not require another activation. If a script intentionally keeps the electronics counter elevated, the corresponding suppression remains active.

## Implementation and manual verification

The game effect chooses the profile and maintains logical device state. Actor updates reset only visor fields in the shared shader parameters; the night-vision profile survives these updates until the next activation. The renderer binds `NightVisionPostprocess` and `NightVisionInterference` for `nvg_render.ps.hlsl`. This pass samples the scene once and uses arithmetic noise; it no longer needs separate depth or blue-noise texture bindings. The mask is a separate UI draw. No GPU performance measurements are implied by these implementation details.

After rebuilding the changed engine/game/renderer code and installing the matching configuration, shaders and UI texture, manually check:

1. The three generations, explicit item color, helmet/outfit priority and a profile-changing upgrade followed by reactivation.
2. An enabled device through rising interference, complete shader suppression and recovery. The edge mask must remain visible during suppression.
3. Activation during strong interference, and restoring a save with an active emission.
4. Manual switch-off, battery exhaustion, equipment removal, actor death and switching between first- and third-person views.
5. The existing PPE route with `shader_effect_range = 0`, and weapon/scope night vision separately.

Related equipment effects: [Visor glass and rain drops](visor-glass.md).
