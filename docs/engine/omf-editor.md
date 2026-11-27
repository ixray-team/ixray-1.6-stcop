# OMF Editor

In-game editor for the motion files (`.omf`) of the HUD hands and the active HUD item. It shows which motion is playing, lets you edit motion flags and motion marks, and writes them back to the `.omf` file without leaving the game.

Open it from **Game → Editors → OMF Editor** (needs a build with `DEBUG_DRAW`). `Alt` toggles mouse capture. A HUD item must be in hands; otherwise the window shows "No active HUD item".

## Window

- **Motion Player** — plays the aliases of the item (`anm_*` from its hud section) and its additive motions, so the motion you edit can be scrubbed and watched on the HUD. Starting a motion cancels the pending end of the motion the item played before, so an idle marked "Stop at end" (for example `pm_idle`) no longer replaces the preview a moment after it starts.
- **Hands / Weapon** — selects whose motion is edited: the hands model or the weapon model of the item. Combined models only have the hands side. When the weapon model has no motion with the name the alias uses (for example `anm_idle = pm_idle`, or `anm_show` from a retarget set), the weapon side shows `idle` of the weapon model, the same motion the game plays in that case.
- **Motion info** — the motion name, the `.omf` file it comes from, its length in keys (30 per second) and in real seconds at the motion speed.
- **Flags** — the motion flags: Stop at end, No mix, Sync part, Use foot steps, Move XForm, Use weapon bone.
- **Motion Marks** — the marks of the motion (`mm_*` names such as `mm_unpend`, `mm_shoot`, `mm_ik`).
  - `Add`/`Del` next to the mark list add or remove a mark; a custom mark name is typed in the field under the list.
  - `Add`/`Del` next to the interval list add or remove an interval of the selected mark.
  - The start and end of an interval are dragged in seconds; double click moves the player to that time, right click types the value, `Frame` sets it to the current player frame.
  - Marks are colored by name, the same colors as in the Motion Player timeline.
- **Reset motion** — restores flags and marks of the motion as they were loaded or last saved.
- **Save OMF** — writes flags and marks of all motions of the file to disk. The status line under the button says where the file was written or why it was not.
- **Motion list** — all motions of the file with their length; a motion that the item never plays is marked "(not played)". Checked motions can be deleted from the file.
- **Bone parts** — the bone partitions of the model and the bones in each.
- **Warnings** — lists of motions repeated in one `.omf`, motions present in several `.omf` files, config keys set twice and keys that use the same motion.

## Data sources

- Motion files: `meshes\...\*.omf` in `gamedata` or in an addon under `ixr_addons`. The editor writes to the real file the game loaded, including an addon folder.
- Aliases and additive motions: the hud section of the item (`anm_*`, `additive_motions`, `retarget_motions` and the retarget sets they reference).

## Saving while the game uses the file

The game keeps motion files open while the models that use them are loaded. Before writing, the editor makes the game keep its own copy of the motion keys and lets go of the file, so **Save OMF** works without restarting. Edited flags and marks take effect immediately because the game already uses the edited values; the file on disk now matches them.

## Limitations

- Deleting motions changes the order of motions in the file. After a delete the game must be restarted to reload the file; **Save OMF** stays off for that file until then.
- Files inside archives (`.db`) cannot be written. Unpack the file to `gamedata` or an addon first.
- Only flags and marks are edited. Keys, speed, power, accrue and falloff are written back as they were loaded.
- Unsaved edits of a file are lost when the game is closed.

## Checks after a build

1. Equip a weapon, open the editor, select a hands motion, add a mark, press **Save OMF**: the status says "Saved [...]" with the addon path, the file on disk changes (size or time), and the game keeps playing without stalls.
2. Repeat on the weapon side.
3. Restart the game: the mark is still there.
4. Make the file read-only and save: the status says "Not saved: can't write [...]" and the file is unchanged.
5. Delete a motion: the status asks for a restart, **Save OMF** is disabled for that file, and after a restart the motion is gone.
6. Play several motions of a saved file (reload, sprint, shots) right after saving: no corrupted poses or crashes.
7. Take the PM or PB, play `anm_reload` and `anm_shoot` in the Motion Player, also paused and scrubbed: the motion stays on screen and does not jump back to idle.
8. On the PM select `anm_idle` and `anm_show`: the weapon motion is `idle` of the weapon model, the Weapon side opens it, and `idle` is not marked "(not played)" in the weapon file list.
9. Run `viewer_lifecycle_test in_game viewer_original_autosave` with the controlled test save. For each parallel viewer, the check opens the motion selector, selects a real non-additive motion, and clicks **Reset motion** and **Idle**. It must operate on the main game's HUD item, even when editor windows initially overlap; no OMF file is saved by this check.
