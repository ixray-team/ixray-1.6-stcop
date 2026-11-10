# Cinema Film Maker Editor

The panel and timeline can be moved outside the game window, including to another monitor; see [in-game ImGui windows](imgui-windows.md). Detached panels keep the scene updating and handle editor input. Object picking, gizmos and the scene color-picker hint stay in the main game image and follow the game window when it moves.

An in-game editor for staging shots and scenes with NPCs. You can place "dummies", move them into position, pick their model and pose, put an item in their hands and build a sequence of animations.

Its rewrite, the [Cinema Editor](cinema-editor.md) (**Game → Editors → Cinema Editor**), is built beside it; both stay available until the new editor reaches parity, and the new one imports this editor's scenes.

Opening: **Game → Cinema Film Maker Editor**. To work with the mouse in the world, press `Alt` (release/capture the cursor). The editor window can be closed; placed NPCs stay in the world and keep animating.

## Typical workflow

1. Press `Alt` to show the cursor, then open the editor.
2. **Actor → Model** tab: find a model (for example, a stalker) and press **Add**. The NPC appears three metres in front of your character, standing on the ground.
3. Click the NPC in the world to select it, then move it into place with the gizmo (Translate/Rotate) or with the fields in the Transform section.
4. **Actor → Animations** section: double-click the animations you need to add them to the playlist, then press **Play**.
5. If the animation expects a weapon, open the **Item** tab and pick the weapon; it ends up in the NPC's hands.
6. Need a copy of the character? Use **Duplicate**.

## Top bar

- **More** sits at the right edge of the top bar; **More → Reload library** rebuilds the list of available animations.
- **Translate / Rotate** is the gizmo mode; **Snap** is the move and rotate step.
- **More → Theme** selects the UI theme (see below).

## UI themes (for OLED and more)

The **More → Theme** menu holds a list of themes; the choice applies immediately and is remembered between game launches.

- **Default**: the usual look, with a semi-transparent panel like the other editors.
- **OLED Black**: pure black background, an almost opaque panel, light text and light-blue accents. Maximum readability and "true black" on OLED.
- **OLED Dim**: the same, but muted, for a dark room and long sessions without glare.
- **OLED Amber**: monochrome amber in a terminal style; small text has no colour fringing.
- **OLED Mint**: black background with mint accents.
- **OLED Magenta**: black background with magenta accents; useful if red and green are hard to tell apart.

Themes change not only the panel but also the timeline (background, played part, marks, playhead) and the gizmo axis colours, so everything stays distinguishable.

Why it is done this way: on OLED, dark-grey shades get "crushed" and produce banding, so OLED themes use a pure black background, panel levels are clearly separated, and light accents replace saturated red and green in small text.

The selected theme and the **Camera spawn** and **Open timelines by default** checkboxes are stored in the engine settings file `_appdata_ixray_\cinema_editor\settings.ltx` (section `[editor]`), next to the logs and saves. Delete the file to get the default settings back.

## Actor tab

The tab combines the collapsible sections **Model**, **Actor settings** and **Animations**. First expand **Model** to choose a model and add an NPC with the **Add** button; **Actor settings** holds the settings of the selected NPC, and the animation library and playlist are in **Animations**. Each section can be collapsed independently.

Inside **Actor settings** the name and model are separated from the playback controls; **Timeline** and **Transform** are child nodes. Each node can be collapsed separately.

Above the sections is the scene control panel:

- The actor list (playing actors are marked `[playing]`) selects the current NPC.
- **Camera spawn** makes new NPCs and particles appear in front of the camera instead of the character; the setting also applies on the **Particles** tab.
- **Duplicate** creates a copy of the selected NPC: model only, model with playlist, or model with playlist and playback. The held item is always copied; the copy appears one metre to the right of the original.
- **Delete** removes the selected NPC.
- **Play All / Stop All** start or stop all NPCs and placed particles in the scene.

- The NPC name and current model.
- **Play / Pause (Resume) / Stop**: playback of the current animation or playlist.
- **Loop**: loop the current animation.
- **Ping-pong**: play back and forth; the clip (or its segment) reaches the end, goes backwards, then forwards again.
- **Speed**: speed from −4 to 4; can be changed while playing.
- The status line shows what is currently playing.
- **Timeline** is a tree: the **Timeline** section at the top, inside it one node per animation (from the playlist, plus the current one if it is not in the list yet), and in each node the timeline bar and two subtrees, **Segments** and **Playback**. Branches are joined by lines so the hierarchy is visible.
  - The **Open timelines by default** checkbox at the top: expand animation nodes immediately or keep them collapsed. The choice is saved in the settings.
  - The **Segments** and **Playback** nodes can be collapsed separately; the timeline stays visible.
  - Dragging the bar moves to the chosen frame (playback is paused); if the animation is not the current one, its preview starts at that frame.
  - **IN/OUT segments**: up to 128 on one timeline. **Ctrl+drag** on the bar adds a new segment, dragging the green/orange flag moves a boundary, right-clicking a segment deletes it. Everything outside the segments is darkened.
  - Segments play in order from left to right: on reaching OUT, playback jumps straight to the IN of the next segment, skipping the gaps between them. After the last segment: with **Loop**, back to the first one; in a playlist, on to the next animation; otherwise it stops at the boundary.
  - **Ping-pong** (the shared checkbox in the actor panel) runs the whole chain back and forth: forwards through the segments, then backwards.
  - Under the bar, in the **Segments** node, is the list of segments (`#1 1.200 - 2.400 s`) with a **Remove** button for each and **Clear segments** to clear the whole chain.
  - If the held item has a bullet fire point, the node also has a **Shots** subtree. **Shift+click** on the bar or **Add shot at playhead** places a shot marker; each marker has a **Time** field and **Remove**. During playback a real bullet is fired at that moment, but only if the item also has `ammo_class` (otherwise the node says so). Up to 256 markers per animation; markers are saved in the scene and in presets.
  - In the **Playback** node: **Speed** is the speed of this particular animation (`base: …` next to it is the base value from the actor panel); different playlist animations can play at different speeds.
  - **Blend**: smoothness of the transition between segments, in seconds. At `0` the transition is a hard cut (instant splice); at `0.15`–`0.3` the pose at OUT blends smoothly into the pose at the next segment's IN. It also works when the chain loops (from the last segment to the first).
  - The segment chain and speed are stored separately for each animation and are saved in the scene and in presets.
- **Transform**: exact coordinates, rotation (H/P/B in degrees) and model scale.
  - **Reset tilt**: straighten the NPC; the heading (view direction) is kept, the P and B tilts are reset to zero.
  - **Scale**: visual scale along X, Y and Z (0.1…5). The **Reset** button next to it returns 1, 1, 1. Scale is saved in the scene and in presets, but does not affect the held item; the weapon keeps its own size.

## Particles tab

The list comes from the effect and group library `gamedata/particles.xr` of the current build. Type part of a name in the search field and pick an effect. **Add to scene** places it three metres in front of the character or, with **Camera spawn** on, in front of the camera. The effect starts immediately. Looped effects keep running; one-shot effects finish on their own.

Selecting an effect in the library does not change the scene by itself. To replace an already placed effect, select it in the scene list, select the new effect in the library and press **Replace selected**. The position and other settings of the placed effect are kept, and playback starts again.

Select a placed effect in the list or by clicking its source in the world. Position and rotation can be changed with the gizmo or the **Position / Rotation** fields, size with the **Scale** field. The **Play** and **Stop** buttons control playback, **Delete** removes the effect. Particles already emitted may stay where they were until the end of their life.

Effects and their positions are saved with the scene through **More → Save scene** and start when it is loaded. A scene can contain only effects. On load, unavailable effects are listed in a warning and skipped; loading replaces the current NPCs and effects. Old scene files without effects remain compatible. A scene may contain up to 1024 effects. When the window is closed they stay in the world; on leaving the level they are removed.

### Effect groups

- In the list of placed particles, select the first effect and **Shift + LMB** the last one: the whole range of visible particles between them is selected. Collapsed groups are not included in the range. **Ctrl + LMB** adds or removes a single effect from the selection; a plain click selects one effect. Selected effects can be dragged into a group together, or gathered into a new one with **New group**.
- With a multiple selection, the fields show the values of the last selected effect. Changing a parameter, **Play**, **Stop**, **Duplicate**, **Delete**, replacing the effect and removing from a group apply to all selected effects. The gizmo moves and rotates them together; numeric fields set the changed value on each one. Editing a curve copies the whole changed channel to the other selected effects. If IN/OUT or wave limits conflict with the new value, the opposite bound is adjusted to keep the range valid.
- Right-clicking a selected effect opens a **Remove** and **Duplicate** menu for the whole selection; right-clicking an unselected one acts on that effect only. **Remove** deletes effects from the scene; **Remove from group** keeps them in the scene and takes them out of their groups. Duplicates stay in the same groups and become selected. Duplicating is unavailable if the copies would not fit within the scene limit.
- To take an effect out of a group, right-click it in the list and choose **Remove from group**, or select the effect and press that button in its settings. If several effects are selected, the action applies to the whole selection. The effects stay in place in the scene and appear under **Ungrouped**; the empty group is kept.
- Drag one placed effect onto another: a group with both effects appears. Its centre is at the effect that the other one was dropped on. You can also press **New group**: the selected effect goes into a new group; if no effect is selected, an empty group is created at the origin.
- The scene list scrolls together with the tab. A group shows its effects and their count; the full effect name is shown on hover. Click a group name to select it and see **Group transform** at the bottom of the tree; the arrow expands and collapses the group. Drag an effect onto a group header or onto an effect inside it to add it to that group. In the selected effect's settings, the **Group** field also moves it to another group or removes it from a group (**None**) without dragging. An effect can belong to one group; nested groups are not supported. Moving an effect between groups does not move it in the world.
- Select a group in the list to move and rotate it with the **Translate / Rotate** gizmo; the shared **Position / Rotation / Scale** fields are below its effects. All child sources move and rotate around the group centre. **Scale** changes the distances from the centre and the sizes of the child effects; the range is limited by the allowed sizes of each effect.
- **Ungroup all** deletes the group and leaves its effects in place. A duplicated effect stays in the same group. Groups, their transforms and membership are saved in the scene; old scenes without groups load as before.
- Group controls move the sources: particles already emitted may stay where they were until the end of their life.

## Actor → Model section

A list of all game models with search by path. Clicking a model applies it to the selected NPC immediately, and the **Add** button adds a new NPC with that model.

When the model changes, the playlist and held item are kept; only the current playback is reset, and the NPC goes to the default pose.

## NPC presets (Presets tab)

Once an NPC is set up (model chosen, items given, animation playlist built), it can be saved as a preset and then recreated with one button.

- **Save selected NPC as preset**: take everything from the current NPC and save it under a name (the model name is filled in by default, for example `stalker_bandit_2`). If a preset with that name already exists, the editor says it will be replaced.
- A preset contains: the model, its X/Y/Z scale, the held item, **Loop**, the shared **Ping-pong** and **Speed**, the animation playlist and the settings of each animation (IN/OUT segment chain, its own speed and **Blend**). Position and rotation are not saved; the NPC always appears where you are looking.
- **Spawn** (or a double-click on the preset) creates a new NPC three metres in front of you, in the `$editor` pose, with the item and playlist from the preset.
- **Apply to selected**: apply the preset to the already selected NPC (model, item and playlist are replaced).
- **Delete**: delete the preset.
- Presets are not tied to a level: the list is shared by the whole game and kept between launches.

Preset animations that the model does not have in the current build are skipped when spawning. If the model is missing entirely, the editor shows a message.

Presets are stored in `_appdata_ixray_\cinema_editor\presets.ltx` (next to the logs and saves). It is an ordinary `.ltx`; the file can be shared with other people together with a scene. If the file cannot be written, the editor shows a message with the path. An item from a preset is given only if it is listed on the **Item** tab.

The new [Cinema Editor](cinema-editor.md#presets) also lists these presets, marked **(old)**, but never writes `presets.ltx`; its own presets go to `actor_presets.ltx`, which this editor does not read.

## Item tab

Items that can be put in an NPC's hands. The list only includes items whose configs describe how to hold them: weapons and some items with attachment settings. Click a name to put the item in the selected NPC's hands; **Remove** takes it away.

The position comes from the configs and is recalculated from the hand bones, so the item stays in the hands in any animation, just as with a regular NPC. If the model has no hand bones (mutants, animals), the item does not appear, and the editor warns about it.

## Actor → Animations section

- Search by animation and file.
- Animations are grouped by file (source), with the number in the file in brackets; effects are marked `[fx]`.
- Single click: preview the animation. **Double-click**: add it to the playlist.
- Right-click: add to the playlist or play a preview; an animation can be dragged straight into the playlist.
- The **Add** button adds the selected animation to the playlist.

### Extra animation sources

At the start of the section is a collapsible child block, **Extra animation sources**. It pulls in animations not only from the level but also from manually specified locations, including archives. When **Animations** is collapsed, the block and the rest of the section are hidden.

- The field takes a path to `.omf` files relative to `gamedata/meshes` or the level folder:
  - `animations\my_mod\*.omf`: take all files in the folder;
  - `animations\my_mod\walk`: a single file (the `.omf` extension can be omitted; the editor adds it).
- The `gamedata\meshes\` prefix can be written in the path; it is stripped automatically. Forward slashes are also accepted.
- Files inside `gamedata.db*` archives work too: if the game can see the file, it can be added.
- The **Add** button checks that the path really contains `.omf` files and loads them for the selected NPC immediately; the new sources appear in the animation list and are available for the playlist.
- The list of added paths is shown under the field; **Remove** takes a path out of the list. Animations already loaded stay with existing NPCs until the level is reloaded.
- Paths are saved in the settings (`_appdata_ixray_\cinema_editor\settings.ltx`, keys `extra_0`, `extra_1`, …), so there is no need to add them again after a restart; new NPCs get them automatically.

## Playlist

- **Play** starts from the selected entry (or the first one), **Stop** returns to the default pose, **Clear** empties the list.
- **Loop**: loop the whole list.
- Entries are reordered by dragging; double-clicking an entry plays from it; right-click plays from that entry or removes it.
- Animations play in turn: the next one starts automatically when the current one ends.
- If an animation does not fit the current model, it is skipped during playback but stays in the list.

## Saving and loading a scene

A level can have any number of scenes; each is saved as a separate file with its own name. **More → Save scene** opens the save window, **More → Load scene** the list of scenes already saved for this level. The path to the scene folder is shown at the bottom of the More menu.

### Saving

- Enter the scene name in the window (the name of the previous save, or a free `scene`, `scene_2` and so on, is filled in by default).
- If a file with that name already exists, the editor says the scene will be overwritten.
- Everything is saved: each NPC with its name, model, scale, position and rotation, Loop, the shared Ping-pong and Speed, the held item, the playlist, and the settings of each animation (IN/OUT segment chain, its own speed and **Blend** value). The current pose is saved too: animation, frame time and pause. So a scene can be saved "on a freeze frame".
- Files are not stored in gamedata but in the engine data folder: `_appdata_ixray_\cinema_scenes\<level>\<name>.ltx` (where the game logs and saves are). They do not end up in a mod and stay private. It is an ordinary `.ltx`; it can be opened and edited by hand.

### Loading

- A list of all scenes saved for the current level opens. Select a scene and press **Load**, or double-click it.
- Loading **replaces** the current scene (the list window says how many NPCs will be replaced); after loading, a summary shows how many NPCs were loaded and what was dropped.
- Scenes are tied to a level: on another level, only that level's scenes are shown. To move a scene, put its file into the current level's folder; the editor warns that it was saved on another level and positions may not match.

### Compatibility warnings

Before loading, the editor checks the file and shows a window with the list of problems:

| Situation | What the editor reports |
| --- | --- |
| The file was saved on another level | That coordinates may not match the current level |
| The file was saved on another platform (SoC / CS / CoP) | That models, animations and items may be missing |
| A model from the scene is not in this build | The NPC is skipped; the window shows its name and model path |
| An item from the scene is not in the configs, or is not on the **Item** tab (cannot be held) | The item is removed, the NPC stays |
| An effect has broken coordinates | The effect is skipped, the rest of the scene loads |
| Playlist animations are missing from the model | Such entries are dropped on load (counted in the summary window) |
| The file is from a newer editor version | Loading is refused |
| The file is corrupted or is not a scene file | A clear message with the path; the scene is not changed |

If there are problems, loading does not start silently: first a window with the list of problems, with **Load** (load anyway) and **Cancel** buttons. After loading, a summary shows how many NPCs were loaded and what was dropped.

Some damaged values are corrected without a warning: a segment or shot marker with a non-numeric or out-of-range time (above 3600 s) is dropped, a damaged animation speed becomes 1, a damaged **Blend** becomes 0, and a damaged NPC speed becomes 1. Animations beyond 128 segments or 256 shot markers are cut to those limits.

## Default pose

After adding, duplicating, changing the model, and on **Stop**, the NPC goes to the `$editor` pose, the same one regular NPCs show in the level editor. If the model has no such animation, an idle animation is chosen; if there is none either, the model goes to its bind pose (it looks "crucified"; that is normal for a model without animations).

## Good to know

- An NPC appears three metres ahead: by default from your character; with the **Camera spawn** checkbox on, from the camera (that is, where you are looking, including from a free camera).
- An NPC appears exactly at the spawn point and is then placed only by hand: with the gizmo, the Position field or a preset. On slopes the feet may hover slightly or sink in; raise or lower the NPC along the Y axis or adjust Position.
- Clicking an NPC selects it; clicking elsewhere clears the selection. A gizmo drag is cancelled with `Escape` or the right mouse button.
- Actors are visible and animate even when the editor window is closed.
- A scene lives until you leave the level unless it is saved with **More → Save scene**; a saved file can be loaded at any time.
- The animation list is built from the models present on the level and from the character's animations; if an animation you need is missing, press **More → Reload library** or place an NPC with the needed model on the level.

## Resetting values

Any adjustable value can be returned to its default: **right-click the field/checkbox → Reset to default**. This works throughout the editor:

- the actor panel (Loop, Ping-pong, Speed, Position, Rotation, Scale);
- the **Actor** tab: **Camera spawn**; the top bar: **Snap** and the move/rotate steps;
- each timeline block: **Speed** (returns to the base value) and **Blend** (to 0);
- the search fields in Actor → Model, Item and Actor → Animations are cleared;
- the **Open timelines by default** checkbox returns to on.

Besides resetting single values, **More → Reset editor settings** resets everything: the theme returns to **Default**, spawning to "in front of the character", the gizmo step to 0.1 / 5°, and timelines to expanded by default. The settings are saved to `settings.ltx` immediately after the reset.

## Troubleshooting

| Symptom | Cause and what to do |
| --- | --- |
| The NPC is in the bind ("crucified") pose | The model has no `$editor` or idle animations; choose an animation manually in the playlist |
| The NPC hovers in the air or sinks into the ground | Slope or stairs; adjust the position with the gizmo or the Position field |
| The item does not appear in the hands | The model has no hand bones, or the item's config does not say how to hold it |
| The item is in the wrong place | The item's config specifies a different attachment point (for example, the belt) instead of the hand |
| An animation you need is not in the list | Press More → Reload library; motions come from the models on the level and the character's animations |
| A playlist animation does not play | It does not fit the current model; the editor skips such entries |
| After loading a scene the NPCs are in the wrong place | The scene was saved on another level (the file was put into the current level's folder); check the warnings |
| The scene you need is not in the load list | It was saved on another level or is in another folder; check the path at the bottom of the More menu |
| Some NPCs are missing after loading | Their models are not in this build or on this platform; they were listed in the window before loading |

## Post-build checks

- Expand **Actor → Actor settings**: the name, model and playback are inside the tree, and **Timeline** and **Transform** are its child nodes. Check expanding and editing them. Make sure **More** is aligned to the right edge of the bar at different window widths and with **Snap** on, opens, and does not overlap neighbouring controls.
- Make sure **Actor settings** has no **Add** button, and that the button in **Actor → Model** adds an NPC with the selected model.
- Expand **Actor → Animations**: **Extra animation sources** is inside the tree with an indent. Collapse and expand **Animations** again; check expanding the sources, the animation search and the playlist.
- Make sure NPC selection, **Camera spawn**, **Duplicate**, **Delete**, **Play All** and **Stop All** are on the **Actor** tab and work after switching tabs. Check **Play All / Stop All** on NPCs and particles, and **Camera spawn** when adding an NPC and a particle. On the other tabs, **More** and the **Translate / Rotate / Snap** gizmo controls must remain available.
- On the **Actor** tab, expand **Model**, choose a model and add an NPC, then expand **Actor settings** and **Animations**. Check model selection, playback, adding animations to the playlist, dragging into the playlist, and behaviour with no NPC selected. Collapse and expand each section again; separate Model and Animations tabs are not shown.
- In the particle list, select a range with Shift + LMB in both directions, including across expanded groups; check Ctrl + LMB, a plain click and a collapsed group. Drag several selected effects into an existing group and gather others with **New group**. Check the shared Position, Rotation, Scale, speed, Loop, IN/OUT, modulation and curve fields on effects with different initial settings; check the gizmo and cancelling with Escape. Check Play, Stop, library replacement, removal from a group, Duplicate and Remove through the panel and through right-clicking selected and unselected effects, the effect count limit and scene saving.
- Remove an effect from a group with right-click and with the button in its settings: the effect stays in place and no longer moves with the group. Check removing the last effect, saving and reloading the scene.
- On the **Particles** tab, select a library effect and make sure the selected scene effect is not replaced until **Replace selected** is pressed; check **Add to scene** and replacement keeping the transform. Add a looped and a one-shot effect, change Position, check Play, Stop and Delete. Save a scene with effects only and a mixed scene, leave the level, load them and check positions and start-up. Load an old scene without effects and a scene with a missing effect: a warning must appear, and the current scene must not be lost before confirmation.
- Drag one effect onto another and make sure a group with both effects appears without a separate height-limited window, and that its transform is shown below them after selecting the group. Create a second group with **New group**, move an effect into it with the **Group** field, and also drag an effect onto a group header and onto an effect inside it; check dragging an effect onto itself. Check scrolling the whole tab with many effects and collapsing with the arrow. Check moving and rotating the group with the gizmo and the fields, scaling, cancelling a gizmo drag with Escape, and **Ungroup all**. Save and load the scene, then move the group again: its effects must be kept and move together. Check an old scene without groups and loading a scene with a missing effect.

1. Add an NPC: it appears in front of the character, standing on the ground, in the `$editor` pose.
2. Click another NPC: the selection switches; clicking elsewhere clears it.
3. Turn on **Camera spawn** and add an NPC: it appears where the camera is looking; move the gizmo along X/Z/Y: the NPC moves freely, without automatic snapping to the surface; **Reset tilt** straightens the tilt.
4. Change **Scale** in Transform (for example 1.5 / 1.5 / 1.5): the model gets bigger, the held item stays its normal size; **Reset** returns 1, 1, 1.
5. Change the model in the list: it applies immediately, the playlist and item are kept.
6. Add several animations by double-clicking and press **Play**: they play in turn, **Loop** loops the list; in the **Extra animation sources** block add a path like `animations\...\*.omf`: the new sources must appear in the animation list.
7. In Timeline there is a block for each playlist animation: on one bar make 2-3 segments with Ctrl+drag, move the flags, delete one with right-click; set **Blend** to 0.2: transitions between segments must be smooth, and hard at 0; turn on **Play**: segments play in order, gaps are skipped; with **Ping-pong** the chain goes back and forth; a clip's own **Speed** does not change the speed of the others; after a restart, chains and blending are kept in the scene.
8. Right-click any field (for example Speed in the actor panel or Blend in a block) → **Reset to default**: the value returns to the base one, and **More → Reset editor settings** resets the theme, spawning and gizmo steps.
9. Give a weapon and play an animation with a weapon: the item is in the hands and follows them.
10. Set up an NPC, Presets tab → Save selected NPC as preset, then Spawn: an identical NPC with the item and playlist appears nearby; the preset survives a restart.
11. Check all three Duplicate variants.
12. Save a scene, leave the level and come back, load the file: NPCs, poses, scales, playlists, trims and items are in place.
13. Put another level's scene file under the current level's name and load it: a warning about the other level must appear and, if that level is from another platform, a warning about model/animation incompatibility.
14. Choose the OLED Black theme in More → Theme and restart the game: the theme is kept; check that the panel, timeline and gizmo axes are recoloured, and that the spawn checkboxes stayed as they were.
15. Leave the level and close the game: no crashes.
16. At the limits: with 256 shot markers on one animation, **Add shot at playhead** is disabled and Shift+click adds nothing; with 128 segments, Ctrl+click adds nothing. In a saved scene, write more than 256 values into a `trim_000_shots` line and more than 128 `start,end` pairs into the matching `trim_000` line, then load it: exactly 256 markers and 128 segments remain. Save the scene and a preset from that NPC, load both again: no crash.
17. Edit a scene file by hand: give an NPC's item a section without `visual` or one that is not on the **Item** tab, and give an effect `position = nan, 0, 0`. Loading lists both in the warnings, the NPC is left without an item, the effect is skipped. Also give a segment the time `nan`: no warning appears for it, and the segment is missing from the **Segments** list. The game does not crash.
18. Camera: unselected path keys are light blue, the selected one is yellow; in the Curve editor the selected key is yellow.
19. Make `presets.ltx` read-only and save a preset: the message "Could not write the presets file:" with the path appears instead of `Preset "<name>" saved (N animations).`, and the game does not crash. Do the same with a scene file and Save scene: "Could not write the scene file:" appears.
20. Smoke test for camera shake: play a camera with shake layers using negative and positive Time offset values, with Stretch to clip on and off. No crash and no visible jumps at the loop point of the shake.
21. Retarget an animation from an extra OMF onto a stalker: cycles that play on the whole body in the source move the whole body, not just one part.
22. Give an NPC a name of 127 characters, duplicate its clip several times (clip context menu → **Duplicate**) and open the settings of each copy: no crash; the names shown are cut to fit the name field.
23. Save a scene with a few muted or grouped tracks: saving completes without a stall, and the `[scene]` section lists `master_track_*` keys only for tracks that are muted, grouped, collapsed or parented. Load it in this build and in the previous build: track mute, groups and collapse are the same.
24. In windowed mode, move the panel and timeline to another monitor and check input and playback. Move the game window across the desktop and check object picking, gizmos and the scene color picker inside its image; clicks outside the game image must not select scene objects. Repeat in Debug and RelWithDebInfo.
