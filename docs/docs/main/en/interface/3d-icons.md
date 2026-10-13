# 3D icons
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.4

## Overview

3D icons display item models in the inventory instead of atlas images. The same icon models can be used in Spawn Manager.

## Enabling inventory icons

Set `Enable3DIcons = true` in `engine_external.ltx`.

## Spawn Manager

Open `Game` → `Spawn Manager` in the ImGui menu (`Alt`). The `3D icons` checkbox initially follows `Enable3DIcons` in `engine_external.ltx` and can then be changed independently, in both table and list views.

Icons use the section's model, rotation and scale settings. Missing or unsupported models fall back to the regular atlas icon. Clicking an icon spawns the item with the current Spawn Manager settings.

Weapon previews use the same addon, upgrade and scope bone rules as `CWeapon`, including `installed_upgrades`, `scopes`/`scopes_sect` and no-scope overrides. Advanced ammo bones use the same `TAmmoBones` calculation as actual weapons; the default preview shows an empty magazine, as on a newly spawned item. Optional addons remain detached in the preview. Inventory icons copy the actual item's visible bones, including installed addons, upgrades and the current ammo count.

## Item configuration

```ini
3d_static_visual_name = dynamics\devices\dev_binoculars\dev_binoculars.ogf
3d_static_rotate = 0.0, 90.0, 0.0
3d_static_scale = 1.0
3d_static_offset = 0.0, 0.0
```

- `3d_static_visual_name`: icon model, relative to the meshes folder. When omitted, the item's world model (`visual`) is used.
- `3d_static_rotate`: rotation around X, Y and Z, in degrees.
- `3d_static_scale`: multiplier for the fitted icon size.
- `3d_static_offset`: X/Y displacement relative to the icon's width/height. The default is `(0, 0)`; `(0.1, 0.1)` moves the model right/down by 10% of its bounds. The model remains clipped to the icon bounds.

## Adjusting icons in game

The console command `3d_icons_adjust 1` enables adjustment. `3d_icons_adjust_value` sets the step. Hover over an inventory icon to select it.

| Key | Action |
| --- | --- |
| Z | Rotate around X |
| X | Rotate around Y |
| C | Rotate around Z |
| V | Change scale |
| B | Write the current values to the log |
| Left Ctrl, held | Reverse the adjustment direction |

Alternatively, open `Game` в†’ `3D Icons Adjust` in the ImGui menu.

The editor provides an inventory list with section/name filtering, item count and location; modified items are highlighted. Select an item in the list or enable `Follow cursor`. Edit the model, rotation and scale with immediate feedback, or use `Reset to config` to restore the section's values.

Enable `Hide item description` to hide item description windows while the editor is open. Descriptions return when the checkbox is cleared or the editor is closed.

`Position inside icon` edits `3d_static_offset` on both axes. `Inventory cells` edits `inv_grid_width` and `inv_grid_height` for all actor inventory items in the selected section. Resizing updates the occupied cells immediately; increasing the size requires free neighbouring cells and sufficient space before the grid edge. Stop dragging before resizing. `inv_grid_x` and `inv_grid_y` remain coordinates in the 2D icon atlas and do not move the 3D model. Offset and grid size are included in logging, clipboard export, saved LTX sections and config reset.

`Log values` writes settings to the log. `Copy config` copies an LTX block to the clipboard. `Save to file` writes modified sections to `$app_data_root$\3d_icons\adjusted.ltx`. The adjustment hotkeys also work while the editor is focused and adjustment mode is enabled.

## DX11 icon lighting

Spawn Manager draws 3D icons from ImGui render callbacks in draw-list order. DX11 preserves coverage alpha when compositing the transparent icon target over the ImGui background; opaque and alpha-tested surfaces provide full coverage. The inventory path retains its background-aware composition.

DX11 Lua materials can define an optional `normal_ui(shader, t_base, t_second, t_detail)` color pass for 3D icons. It derives from the visible `normal` pass and performs the same detail-texture lookup (`bDetail`, `t_detail`). It takes precedence over the legacy `l_special` fallback and is stored separately, so world distortion and emissive passes retain their original behavior. Materials such as `models\pautina` and `models\xdistortcolor` use their base color textures in this pass instead of displaying distortion-buffer data as color. Scene refraction is not reproduced by the icon color pass.

`models\xdistortcolorl` also uses its base color in UI. The visible pass of `models\xdistort` uses only texture alpha, so its UI pass preserves that mask and ignores the distortion RGB channels. `models\artefact2` is provided by the compiled environment-model blender in `shaders.xr` and uses its existing lit `ui_base` pass.

Expand `DX11 icon lighting` in either `Spawn Manager` or `3D Icons Adjust`. With `renderer_r4`, you can change the light's RGB color, intensity, view-space direction and ambient contribution. A zero direction uses `(0, 0, 1)`. `Reset icon lighting` restores the defaults. The color and intensity also tint self-lit icon materials, including forward light-plane emission (`base_lplanes`). The light-plane alpha and additive blending are preserved; world emission retains its original color.

Changes apply immediately to all DX11 3D icons. These debug settings are kept for the current session and reset on restart. They do not change world lighting. R1/R2 do not expose these controls.

## Shared model cache

Inventory cells, upgrade previews, quick slots, pickup icons, the radial weapon menu, PDA favourite-weapon icons, artefact panels and Spawn Manager cache model templates by normalized model path. Each icon has an independent skeleton, animation state and skinning cache; mesh resources remain shared. Rotation, scale and bone visibility remain specific to each icon, so variants can display different addons safely. The lighting panel shows the number of shared models.

Each icon releases its own model instance when destroyed. A template is released when its last icon is destroyed. General-purpose `CUI3dStatic::SetVisual` retains independent model instances for widgets with their own animations. C++ icon widgets use `SetIconVisual`; after manually changing an icon model's bone visibility, call `CaptureIconBones` to save that icon's mask.

Related material: [UI overview](ui-advanced-features.md).
