# Spawn Manager

The **Spawn Manager** panel browses configured objects and creates them in a running level. Open it from the in-game tools menu with `DEBUG_DRAW`; Alt toggles mouse capture. It needs a loaded level and ALife simulation.

## Controls and data

Browse the item, weapon, NPC and other categories, use Search and sorting filters, and set **count** before selecting an object to spawn. **View as Table** changes the presentation; **3D icons** enables model previews.

Entries come from the game's loaded configuration sections. Inventory icon sizes come from `inv_grid_width` and `inv_grid_height`. The 3D preview uses `3d_static_visual_name`, falling back to `visual`; 2D icons use `icons_texture`, defaulting to `ui\ui_icon_equipment`.

The tool relies on valid object sections, models and inventory metadata supplied by the mod. Spawned objects change the current game world. The preview is not a replacement for checking the spawned object's gameplay behavior.

## Post-build checks

Use Debug first, then RelWithDebInfo. Load an ALife level, search for an item and compare table, 2D and 3D views. Spawn one item and check it in the world. Repeat on Windows and macOS, including an item that uses the fallback visual or texture.
