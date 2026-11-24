# Weather Editor

The **Weather** panel edits the loaded level's environment. Open it from the in-game editor menu with `DEBUG_DRAW`; Alt toggles mouse capture.

## Controls and data

Choose a **Weather cycle**, adjust the current environment parameters and preview changes in the loaded level. **Time factor** changes how fast environment time advances; **Default** restores a factor of 10. Texture fields offer a file browser: use **...** to choose a texture and **OK** or **Cancel** to close it.

The texture browser reads the configured game texture directory. Weather cycles and their ambient, sun and thunderbolt choices come from loaded environment configurations. **Save** writes all modified cycles to their `.ltx` files under `$game_weathers$`. The star in the window title marks pending changes. **Reset** reloads the environment data and discards pending edits.

**ambient_scale** (0 to 4, 1 when a weather section has no such line) multiplies the light a time of day gets from the sky and its ambient colour together, without touching the sun; it blends between times of day like the other values and is saved as `ambient_scale` in the section.

This is a live preview on the current level. Saving modifies the weather configuration files; keep a copy before editing shipped presets. The panel is unavailable until the level is ready. It opens as an ordinary window (560 x 720 the first time) that can be moved, resized and docked; its place and size are remembered.

## Post-build checks

In Debug, then RelWithDebInfo, load a level, open the panel and check that it is a window of moderate size, not one that covers the screen, and that a new size survives closing and reopening it. Then select a cycle and open a texture browser. Confirm a texture and check the sky in the game image, then repeat with Cancel. Save a change and reload the level to verify it. Repeat on Windows and macOS after portability changes.
