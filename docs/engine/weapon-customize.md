# Weapon Customize

The **Weapon Customize** window installs and removes attachments on a weapon, on the bones its section names. Open it from the inventory: right-click a weapon whose section has `customize_bones` and pick **Customize**. It needs a loaded level with the actor; it closes when the weapon is gone.

## Controls and data

The window shows the weapon with its attached items (scopes, silencers and other attachments, and theirs in turn). Drag inside the preview to turn it. A marker on each customisable bone leads to a button: **+** when the bone is free, the attached item's icon when it is taken; the tooltip names the bone. The button's menu offers **Detach** and the sections the bone accepts, each with how many the actor carries. Picking one moves it from the inventory onto the bone (an attachment already there goes back to the inventory); with god mode on, a section the actor does not carry is spawned onto the bone.

Data comes from the item's section:

```ini
customize_bones = bone_scope, bone_muzzle
bone_scope      = wpn_addon_scope, wpn_addon_scope_susat
bone_muzzle     = wpn_addon_silencer
```

A bone listed in `customize_bones` without its own line is ignored. Icons in the menus come from `icons_texture` and `inv_grid_*`.

## Limitations

The preview is drawn into the image the Spawn Manager draws its 3D icons in, at its bottom right corner; with both windows open and many Spawn Manager icons visible the two can overlap. When the preview is larger than the game's picture it is drawn smaller and stretched. At most 64 models (the weapon and its attachments) are shown.

## Post-build checks

Use Debug first, then RelWithDebInfo, on D3D12 and on D3D11. Open the window for a weapon with a scope bone: the weapon and its attachments are centred in the preview, undistorted, and turn with the mouse; the bone markers stay on their bones. Install a scope, then detach it: the preview and the weapon's inventory icon show and drop it at once. Resize the window, open the **Game** window, set `r_view_scale 0.5`: the preview stays inside the window. With the Spawn Manager open beside it, its icons and the preview both draw.
