# Spawn Manager

The **Spawn Manager** panel browses configured objects and creates them in a running level. Open it from the in-game tools menu with `DEBUG_DRAW`; Alt toggles mouse capture. It needs a loaded level and ALife simulation.

## Controls and data

Browse the item, weapon, NPC and other categories, use Search and sorting filters, and set **count** before selecting an object to spawn. **View as Table** changes the presentation; **3D icons** enables model previews.

Entries come from the game's loaded configuration sections. Inventory icon sizes come from `inv_grid_width` and `inv_grid_height`. The 3D preview uses `3d_static_visual_name`, falling back to `visual`; 2D icons use `icons_texture`, defaulting to `ui\ui_icon_equipment`.

The tool relies on valid object sections, models and inventory metadata supplied by the mod. Spawned objects change the current game world. The preview is not a replacement for checking the spawned object's gameplay behavior.

3D icons are drawn into a picture of their own and shown like 2D icons, so each one stays inside its cell and is clipped with the panel wherever the panel is: in the main window with or without the **Game** window, with `r_view_scale`, in a panel taken out into its own window on any monitor, and after any window is resized. The icons visible at the same time share an area the size of the game's picture (the **Game** window's picture when it is open, smaller with `r_view_scale`); icons that do not fit into it show their 2D icon until fewer are visible. Half-covered pixels on a model's outline come out slightly darker than with the game's own 3D icons.

## Post-build checks

Use Debug first, then RelWithDebInfo. Load an ALife level, search for an item and compare table, 2D and 3D views. Spawn one item and check it in the world. Repeat on Windows and macOS, including an item that uses the fallback visual or texture.

With **3D icons** on, on D3D12 and on D3D11, check that every model sits centred inside its button, at the button's size, undistorted, and disappears under the panel's edge when the list is scrolled:

- in the main window without the **Game** window, the panel docked on the right and on the left of the picture;
- with **View → Game window** on, the **Game** window made small and the panel beside it;
- with `r_view_scale 0.5`, and with the main window stretched wider than the picture (black bars);
- with the panel taken out of the main window into its own window, on the same monitor and on a second one, the **Game** window inside and taken out;
- after resizing the main window, the **Game** window and the taken-out panel, with the mouse released.

With the **Game** window small or `r_view_scale 0.25` and many weapons visible, the icons that do not fit show 2D icons, without overlapping or garbage. Close and reopen the panel and open the inventory: the inventory's own 3D icons and the HUD look as before.
