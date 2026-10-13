# Visor glass and rain drops

IX-Ray supports two kinds of visor glass, item-condition damage, two rain-drop implementations and wiping through first-person animations. Glass belongs to an equipped helmet or outfit. The actor's health does not determine the appearance of the glass.

This page describes the implementation in the current branch. It applies to the `gamedata`, `gamedata_soc`, `gamedata_cs` and `gamedata_coc` packages; available equipment and animation assets depend on the game and mod.

## Quick setup

For an existing helmet or outfit, add these parameters to its **item section**, not its HUD section:

```ini
glass_present = true
hud_gas_mask_avaliable = true
hud_rain_drops_avaliable = true
hud_gas_mask_type = flat
use_condition = true
```

Enable the effects in the console:

```text
r4_use_gasmask true
r2_use_rain_drops true
r2_rain_drops_mode procedural
```

Use R4, equip the item and switch to first-person view. Cracks follow the item's condition. Drops accumulate while rain reaches the camera. Wiping also requires an animation configured as described below.

## Renderer and player settings

| Command | Values | Engine initial value | Effect |
| --- | --- | --- | --- |
| `r4_use_gasmask` | `true`, `false` | `false` | Enables the glass postprocess, including condition-dependent damage, on R4. |
| `r2_use_rain_drops` | `true`, `false` | `false` | Enables visor rain drops on R2 and R4. |
| `r2_rain_drops_mode` | `texture`, `procedural` | `texture` | Selects the rain-drop implementation. |

User profiles and presets can override the initial values. These settings are also available in advanced graphics/postprocess options and take effect without restarting the renderer. Use the exact console names above; the historical names `r_use_gasmask` and `r_use_rain_drops` are not the commands for this implementation.

| Renderer | `flat` glass | `curved` glass | Texture drops | Procedural drops |
| --- | --- | --- | --- | --- |
| R4 / D3D11 | Yes | Yes | Yes | Yes |
| R2 / D3D9 | No | No | Yes | Yes |
| R1 | No | No | No | No |

Glass and drops are independent switches. With glass disabled, eligible equipment can still display drops. With drops disabled, glass and cracks can still appear. On R2 the glass type does not change the image, but an equipped item with glass is still required for drops.

`r2_use_weapon_raindrops` controls a separate weapon effect. Wiping the visor does not clear weapon drops or world wetness.

## Equipment configuration

The following parameters are read by `CArmorBase`, shared by helmets and outfits:

| Parameter | Default when omitted | Meaning |
| --- | --- | --- |
| `glass_present` | `false` | The item has glass and can supply visor effects. |
| `hud_gas_mask_avaliable` | `true` | Allows the glass postprocess for this item. |
| `hud_rain_drops_avaliable` | `true` | Allows rain drops for this item. |
| `hud_gas_mask_type` | `flat` | Selects `flat` or `curved` glass. |
| `use_condition` | `true` | Enables the existing item-condition flag; it does not create a separate glass-health value. |

Keep the spelling **`avaliable`** in both availability parameters. `available` is not an alias. Glass types are the exact lowercase tokens `flat` and `curved`. An unknown token produces a log warning and falls back to `flat`.

Availability flags alone do not give an item glass: `glass_present = true` is required. Values inherited from a parent section also apply. The flags are permissions in addition to the player's global settings, rather than overrides of those settings.

### Choosing the item that supplies the glass

Effects are supplied by the current actor while alive and using first-person view:

1. An equipped helmet with `glass_present = true` takes priority.
2. Otherwise, an equipped outfit with `glass_present = true` supplies the glass.
3. If neither qualifies, the visor effects are disabled.

A helmet without glass does not block an outfit with glass. Type, availability flags and condition all come from the same selected item; the helmet and outfit conditions are not combined. A selected helmet with an effect disabled does not fall back to the outfit for that effect. Items in the backpack do not supply visor effects.

### Flat and curved glass

| Type | Appearance | Damage stages | Suggested use |
| --- | --- | --- | --- |
| `flat` | Ordinary glass with refraction, reflections and condition-dependent cracks. | Ten normal maps. | Flat faceplates and suit visors. |
| `curved` | The original hemispherical glass, with stronger edge distortion, reflections and condensation. | Four original texture states. | Protective helmets with a rounded faceplate. |

Both types support either drop mode on R4. The type names describe the glass shape. Omitting the type selects `flat`; explicitly set `curved` for hemispherical glass.

For example, a CoP/CoC DLTX patch in a file such as `configs/mod_system_visors.ltx` can assign the original glass to the protective helmet and flat glass to the scientific outfit:

```ini
![helm_protective]
glass_present = true
hud_gas_mask_avaliable = true
hud_rain_drops_avaliable = true
hud_gas_mask_type = curved
use_condition = true

![scientific_outfit]
glass_present = true
hud_gas_mask_avaliable = true
hud_rain_drops_avaliable = true
hud_gas_mask_type = flat
use_condition = true
```

Patch only sections present in the target game. For ordinary LTX files, put the same parameter lines inside the item's existing section or a derived item section instead of using the DLTX `![section]` syntax.

## Item condition and cracks

The renderer reads the selected item's existing `GetCondition()` value. A condition of `1` means fully repaired equipment; `0` means fully damaged equipment. This is the item's condition/HP, not actor HP, filter capacity or a new independent glass-health counter. Existing equipment damage and repair mechanics supply changes to this value.

The renderer reads the stored condition regardless of `use_condition`. That flag does not enable or disable cracks, replace `glass_present`, or repair damaged glass when changed.

Flat glass clamps condition to `[0, 1]` and selects its texture with:

```text
DamagePhase = min(floor((1 - Condition) * 10) + 1, 10)
Texture = mask_nm_<DamagePhase>.dds
```

Curved glass selects one of four original states with `min(floor(4 - 4 * Condition), 3)` for normal condition values in `[0, 1]`.

| Item condition | Flat glass texture | Curved glass texture |
| --- | --- | --- |
| `1.00` / 100% | `mask_nm_1.dds` | `Good.dds` |
| `0.75` / 75% | `mask_nm_3.dds` | `Medium.dds` |
| `0.50` / 50% | `mask_nm_6.dds` | `Bad.dds` |
| `0.25` / 25% | `mask_nm_8.dds` | `Broken.dds` |
| `0.00` / 0% | `mask_nm_10.dds` | `Broken.dds` |

Damage transitions are discrete texture changes. Repairing the selected item changes the displayed damage stage. Rain and wiping do not damage or repair the item. Wiping does not remove cracks or the curved shader's condensation.

## Rain modes and accumulation

`texture` uses the existing static droplet normal map, `shaders/fx_hud_droplets.dds`. Its image distortion is scaled by the accumulated amount. `procedural` generates animated drop layers in the shader, including moving drops and trails. It uses the same accumulated amount, so wiping and shelter work in both modes.

Accumulation is controlled by the engine, not just the shader clock:

| Situation | Result |
| --- | --- |
| Rain under open sky | Amount increases by `0.05` per second. |
| Rain while under overhead cover | Amount decreases by `0.05` per second. |
| Weather rain density | Amount is limited to the density, clamped to `[0, 1]`. |
| Rain stops or the rain effect is disabled | Amount clears immediately. |
| Wiping callback | Amount clears immediately; exposed glass then starts accumulating again. |
| Pause | Accumulation and drying stop; rain-stop and disable checks still apply. |

At density `1`, dry glass reaches full amount in approximately 20 seconds, and full glass dries under cover in approximately 20 seconds while rain continues. At density `0.5`, the maximum amount is `0.5`, reached in approximately 10 seconds. When density decreases during normal updates, the amount is capped to the new value.

Cover detection casts a 50-metre ray straight upward from the camera, checking static and dynamic collision geometry while excluding the viewed entity. The first check is immediate; subsequent checks are at most once per second. Entering or leaving cover can therefore take up to approximately one second to affect accumulation. A visual roof must also have collision geometry to count as cover.

Updates run once per rendered frame and use frame time, capped at one second per update. The amount is independent of ordinary frame-rate changes; a long stall does not simulate its entire elapsed duration.

### Persistence and resets

The amount is shared between the two drop modes. Switching `texture` to `procedural`, or back, preserves it. Disabling only the glass postprocess also preserves it if drops remain enabled.

Removing or replacing the selected glass item resets the amount, including replacement with another instance of the same section. Losing eligibility through death or third-person view, changing the viewed entity, disabling visor drops globally or for the selected item, and disconnecting also reset the state. Re-enabling or re-equipping starts with dry glass.

There is no separate saved wetness value for each inventory item. The accumulation belongs to the active view and is not serialized with equipment. Weather stopping clears drops immediately; residual drying after the rain has stopped is not simulated.

## Wiping animations

The actor action is `clear_gasmask`. Bind it in the controls menu or through the console; for example, this assigns it to H:

```text
bind clear_gasmask kH
```

This is an example binding, not a statement about a default key. The action requires an equipped helmet or outfit with glass. It can play through a HUD item's/device's `anm_gasmask` or through a separately configured HUD animator. A glass shader by itself does not supply hand animations.

### HUD items and devices: `anm_gasmask`

Add the base alias to the relevant **HUD section**, pointing it at a wiping motion present in the loaded hand model/OMF:

```ini
anm_gasmask = visor_wipe
```

`visor_wipe` is an example motion name: replace it with an actual motion supplied by the mod. The base alias `anm_gasmask` is required for the availability check, even if additional state-specific aliases are used. An optional `snd_gasmask` sound is configured in the gameplay item/state-animator section, not in place of the motion alias.

Add a motion mark named **`Left`**, with this exact case, to the hand animation at the point where the glass should become clean. This is a motion mark in the animation data, not a bone name or an arbitrary Anim Notify. See [HUD Animator: adding motion marks](../../animation-system/hud-animator.md#adding-motion-marks-to-an-animation).

During the clear-mask `eDeviceSwitch` state, reaching `Left` invokes `CActor::ClearMaskCB()`. The callback sets the drop amount to zero for the currently viewed actor. The reset is instant at the mark; there is no spatial wipe sweep across the screen. The HUD-item path does **not** automatically clear drops merely because `anm_gasmask` finishes: the `Left` mark is required.

The actor first considers the current state animator, then supported combinations of active HUD item and device, then individual item/device animations. Shooting, aiming, pending item actions and device activation can block wiping. A current state animator that cannot wipe also blocks the later paths; the fallback is not a forced interruption of every busy animation.

### Fallback: a dedicated HUD animator

When no item/device path is selected and the item animator is free, the actor can use the section referenced by `configs/game_global.ltx`:

```ini
[glass_mask]
clearmask_animator = clear_gasmask_animator
```

The referenced section must exist in the settings loaded by `system.ltx`. The shipped reference alone is not enough: supply the section and the required motions in the mod. A hand-only example is:

```ini
[clear_gasmask_animator]
hands_position = 0, 0, 0
hands_position_16x9 = 0, 0, 0
hands_orientation = 0, 0, 0
hands_orientation_16x9 = 0, 0, 0
anm_show = visor_wipe
can_sprint = false
```

Replace `visor_wipe` with a motion available on the current HUD hands, adjust the offsets for the model, and include the section's LTX file in `system.ltx`. This path plays **`anm_show`**, unlike the item/device path's **`anm_gasmask`**. It does not require a fake inventory item. See [HUD Animator](../../animation-system/hud-animator.md) for sounds, optional visuals and further section parameters.

The engine attaches the wipe callback to `Left`. Add that mark to synchronize cleaning with the hand. If no `Left` mark is reached, this dedicated item animator invokes the remaining callback at natural animation completion. The callback is consumed once, so completing after the mark does not clear a second time. Stopping the animator before either event discards the callback and does not clean the glass.

Starting an animator manually from Lua does not, by itself, attach the actor's wipe callback. Use the clear-mask action when the animation must reset the accumulated drops.

## Shaders and textures

All paths below are relative to the selected game package:

| Resource | Purpose |
| --- | --- |
| `shaders/d3d11/flat_gasmask.ps.hlsl` | Flat glass, damage/reflections and the selected drop effect in a combined pass. |
| `shaders/d3d11/effector_gasmask.ps.hlsl` | Preserved curved glass shader. |
| `shaders/{d3d11,r2}/raindrops.ps.hlsl` | Separate texture-drop pass. |
| `shaders/{d3d11,r2}/procedural_raindrops.ps.hlsl` | Separate procedural-drop pass. |
| `shaders/{d3d11,r2}/shared/VisorDrops.hlsli` | Shared procedural-drop algorithm for each shader backend. |
| `textures/shaders/gasmasks/mask_nm_1.dds` … `mask_nm_10.dds` | Ten flat-glass damage maps. |
| `textures/shaders/gasmask/{Good,Medium,Bad,Broken}.dds` | Original curved-glass damage maps. |
| `textures/shaders/gasmask/Condensation.dds` | Curved-glass condensation. |
| `textures/shaders/fx_hud_droplets.dds` | Existing texture-mode drop normal map. |

Braces in paths denote alternatives; they are not literal directory names. Preserve the distinction between `gasmasks` for flat maps and `gasmask` for curved maps. Flat glass loads indices 1–10; no `mask_nm_0.dds` is required. Keep shader helpers next to the corresponding backend's shaders.

For R4 flat glass with its postprocess enabled, drops are integrated into the glass pass and the separate drop pass is skipped. Curved glass, or disabled glass postprocessing, uses the separate drop pass. This prevents rendering droplets twice. D3D11 retains the original finite-difference procedural normals; R2 uses screen derivatives to fit its shader instruction limits.

## Troubleshooting

| Symptom | Check |
| --- | --- |
| No glass | R4, `r4_use_gasmask true`, a living actor in first person, equipped `glass_present = true`, and `hud_gas_mask_avaliable = true`. |
| Outfit settings seem ignored | A helmet with glass takes priority, including its type, condition and both availability flags. |
| No drops | `r2_use_rain_drops true`, R2/R4, eligible equipment and `hud_rain_drops_avaliable = true`; active rainfall, its density and overhead collision cover. Allow time to accumulate. |
| Unexpected glass shape | Set `curved` or `flat` on the selected item. Unknown tokens, uppercase tokens and misspellings fall back to flat and warn in the log. |
| Cracks do not match actor HP | They follow the selected equipment's condition. Check damage/repair handling and `use_condition` in the item configuration. |
| `anm_gasmask` plays but drops remain | Add the `Left` motion mark to the actual hand motion used by the clear-mask state. Check that the callback belongs to the viewed actor. |
| No fallback animation | Define the section referenced by `[glass_mask]`, include it in `system.ltx`, supply `anm_show` and the motion asset; check busy animator/item states. |
| Wiping removes drops but cracks remain | Expected: wiping only clears rain accumulation and does not repair equipment or remove condensation. |
| Drops vanish as rain stops | Expected: no post-rain residual wetness is simulated. |

## Mod verification checklist

1. On R4, equip flat glass and enable both effects. Check full, half and near-zero item condition, then repair the item and check the reverse transitions.
2. In steady rain at density `1`, wait approximately 20 seconds outdoors. Enter collision-backed cover and check gradual drying; return outdoors and check accumulation.
3. Trigger `clear_gasmask` and check the reset at `Left`, followed by renewed accumulation. Test both an item/device animation and the dedicated fallback, including interruption before the wipe event.
4. Switch between `texture` and `procedural` while wet; the amount should persist. Disable glass alone and check that drops still work.
5. Equip `curved` glass and check the original distortion, four damage states and both drop modes. Equip a glass helmet over a glass outfit and verify the priority rule.
6. Remove/replace the selected item, change to third person and return, or disable/re-enable drops: each should start dry. Stop the rainfall and check immediate clearing.
7. If the mod supports R2, verify both drop modes there; glass/cracks are only supported on R4.

## Implementation entry points

`src/xrGame/ArmorBase.cpp` loads the item parameters; `Actor.cpp` selects equipment and transfers condition/type to `ShaderParams`. `ActorInput.cpp`, `HudItem.cpp` and `HudAnimatorManager.cpp` connect the wipe action and motion marks to `CActor::ClearMaskCB()`.

`src/xrEngine/VisorRainDrops.cpp` implements accumulation, cover tests, clear and reset; `IGame_Persistent::VisorRain` owns the state. `src/Include/xrRender/VisorGlassType.h` defines `Flat` and `Curved` without a game implementation dependency in the renderer.

The R4 passes are in `src/Layers/xrRenderPC_R4/r4_rendertarget_phase_screen_postprocess.cpp`; glass bindings and texture selection are in `BlenderGasMask.cpp`. Console commands are registered in `src/Layers/xrRender/xrRender_console.cpp`. Shader constant `visor_rain.x` carries accumulated amount, and the flat shader's `mask_control` carries damage phase and drop mode.
