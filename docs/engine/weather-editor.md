# Weather Editor

The **Weather** panel edits the loaded level's environment. Open it from the in-game editor menu with `DEBUG_DRAW`; Alt toggles mouse capture.

## Controls and data

Choose a **Weather cycle**, adjust the current environment parameters and preview changes in the loaded level. **Time factor** changes how fast environment time advances; **Default** restores a factor of 10. Texture fields offer a file browser: use **...** to choose a texture and **OK** or **Cancel** to close it.

The texture browser reads the configured game texture directory. Weather cycles and their ambient, sun and thunderbolt choices come from loaded environment configurations. **Save** writes all modified cycles to their `.ltx` files under `$game_weathers$`. The star in the window title marks pending changes. **Reset** reloads the environment data and discards pending edits.

This is a live preview on the current level. Saving modifies the weather configuration files; keep a copy before editing shipped presets. The panel is unavailable until the level is ready.

## Post-build checks

In Debug, then RelWithDebInfo, load a level, select a cycle and open a texture browser. Confirm a texture and check the sky in the game image, then repeat with Cancel. Save a change and reload the level to verify it. Repeat on Windows and macOS after portability changes.
