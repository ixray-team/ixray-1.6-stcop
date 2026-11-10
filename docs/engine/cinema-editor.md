# Cinema Editor

The Cinema Editor is the rewrite of the [Cinema Film Maker Editor](cinema-film-maker-editor.md). A scene is a list of objects: actors (fake NPCs that play a sequence of animation blocks), lights, particle effects and one global camera made of camera clips. Every object is one row on the timeline with one clip. Both editors stay available until the new one reaches parity; they use different scene folders and never share objects.

The editor is delivered in stages. This build has the windows, scene files, the import of old scenes, the transport, actor playback, row and clip editing with undo, actor authoring (adding actors, editing animation blocks, the animation chooser), the inspector for actors, the camera and the scene, the camera view, the editor camera, camera authoring (camera clips, pose keys, cut marks, shake clips, Pilot), automation lanes for actors and camera clips, the viewport tools (gizmo, picking, the viewport menu and the view cube), object attachment (parents and look-at targets for actors and camera clips), timeline markers, motion-mark ticks on animation blocks, copy, paste and duplicate, the numeric offset, actor presets, Insert/Remove time, and lights and particle effects in scene files, in the import of old scenes, in playback, in the inspector, in the **Add** menus and in the viewport (helpers, picking, the gizmo and the viewport menu).

Opening: **Game → Editors → Cinema Editor** (the first item). The editor is available in configurations with `DEBUG_DRAW`, like the other in-game ImGui tools. `Alt` toggles ImGui mouse capture; the editor never uses `Alt` for anything else. Without a loaded level, and while a level is still loading, the window only says **Load a level**.

## Layout

- **Timeline**: a full-width strip along the bottom, 28 % of the screen height. Its title is the open scene's name, or **Untitled**, followed by ` *` while there are unsaved changes. It holds the menu bar, the transport row, the ruler, the object rows and the status line.
- **Inspector**: the right edge above the timeline, 24 % of the screen width. It shows the selected objects, or the scene settings when nothing is selected (see [Inspector](#inspector)).
- Both windows float with a see-through background in the engine theme, like the other in-game tools; the **Opacity** slider on the transport row sets it for every editor window (default 0.5). Move, resize or dock them by hand.
- Closing the timeline closes the editor. The scene stays loaded until the level changes.
- The inspector has its own close button; **View → Inspector** shows it again. Whether it is shown is remembered (see [Settings](#settings)).
- **Presets**: closed by default. **View → Presets** opens it as a narrow column left of the inspector; its close button hides it again, and whether it is shown is remembered. It lists the actor presets (see [Presets](#presets)).
- **Shots & Markers**: closed by default. **View → Shots & Markers** opens a window with two tables: the camera clips (name, start, length, FOV, depth of field mode) and the markers (name, time). Click a row: the playhead goes there, and a clip is also selected and focused like a click on it. Double-click a row: rename it in place (`Enter` or a click elsewhere keeps the name, `Escape` cancels; one undo step). `Delete` while the window is focused removes the selected clip or marker (one undo step). Unnamed clips are listed as **Shot N**; a clip's name is also drawn on the Camera row above its keys. Whether the window is shown is remembered (`shots`).
- **Graph Editor**: closed by default. **View → Graph Editor** opens a window that shows the keyed curves of the selected objects overlaid on one graph (see [Graph Editor](#graph-editor)). Whether it is shown is remembered (`graph`).
- **Tools**: a small bar at the top left of the game view; drag it by its background to move it. **TRS**, **T**, **R** and **S** pick the gizmo, **Local** / **World** its axes, **Snap** switches gizmo snapping (right-click it for the steps), **Key** is Auto-key, **Bones** is posing mode, **Helpers** shows the helpers and **Free cam** switches between the editor camera and the shot or game view (see [Gizmo](#gizmo)).

## Timeline

Each object has one row: its header on the left and its clip on the right. The header shows the lanes arrow (see [Lanes](#lanes)), the colour swatch, a type icon (figure for an actor, camera body for the camera, a dot with rays for a light, three dots for a particle effect, folder), the name, the **S** (solo) button and the **M** (mute) button. An object without a name shows its type (**Actor**, **Camera**, **Light**, **Particle**). Folders are rows without a clip; their members follow them, indented, unless the folder is collapsed with its arrow. Actor animation blocks and camera clips are edited on the clip (see [Animation rows](#animation-rows) and [Camera clips](#camera-clips)).

The header column is resizable: drag the line between the headers and the clips (the cursor turns to a left-right arrow) to make it 140–600 px wide, or double-click the line to return to 220 px. The width is remembered (see [Settings](#settings)). Names and lists that do not fit end with an ellipsis.

Rows and lanes have their own heights, as in Ableton. The bottom edge of a row's header (the last 4 px; the cursor turns to an up-down arrow and a line lights up) resizes that row:

- An object row is 1–6 rows tall. Its clip, blocks, diamonds and labels grow with it; the top band keeps its height and the buttons stay on the first line. With several rows selected, dragging the edge of a selected row resizes all of them.
- A **Shakes N** sub-row is 1–6 rows tall, a lane 1–8 rows. `Ctrl` + wheel over the header column makes every open lane one row taller or shorter at once; over the clips it still zooms time.
- A double-click on the edge returns it to the default height; `Escape` or a right-click during the drag puts every height back.
- Heights are layout, not edits: they are not undo steps, but they mark the scene as changed (` *`) and are saved with it, only where they differ from the default. Folders keep one row.
- The defaults for rows and lanes (1 row each) are the `row_height` and `lane_height` settings (see [Settings](#settings)).

### Transport

| Control | Key | Result |
| --- | --- | --- |
| **\|<** | `Home` | Moves the playhead and the start marker to the start. |
| — | `End` | Moves the playhead and the start marker to the end of the scene (the end of the last clip). |
| — | `Left` / `Right` | Steps the playhead one frame back or forward; hold to repeat. |
| **Play** | `Space` | Plays from the **start marker** (the hollow triangle on the ruler). Playing at the end of the scene starts again from the start. While playing, `Space` stops and returns the playhead to the start marker. |
| **Pause** | `Shift+Space` | While playing, pauses where the playhead is. While stopped, `Shift+Space` continues from the playhead without moving the start marker. |
| **Stop** | `Space` while playing | Stops and returns the playhead to the start marker. |
| **Follow** | — | While on (default, remembered), the view pages along with the playhead during playback: when the playhead passes 90 % of the width it jumps to 10 %. Scrolling or zooming by hand during playback pauses Follow until the next Play. |
| **Loop** | `Ctrl+L` | Switches the loop brace on and off (see [Ruler and navigation](#ruler-and-navigation)). While on, playback repeats the brace's range; with no range yet, turning it on sets the range to the whole scene. While off, playback runs to the end of the scene and stops, and the range stays drawn grey. `Ctrl+L` with clips or keys selected loops them instead (see below). |
| Timecode | — | `mm:ss:ff / mm:ss:ff`: the playhead and the end of the scene, in frames of the scene frame rate (`frame_rate`, default 30). |
| Speed | — | Playback speed: 0.25x, 0.5x, 1x or 2x. |
| **Snap** + step | — | Snapping for scrubbing, clip moves and clip lengths; on by default. The step is 1, 2, 5 or 10 frames, or ½, 1 or 5 seconds (default 1 frame). |
| Zoom | — | Pixels per second, 4–2000. Drag it up to zoom in and down to zoom out (as Ableton's number fields; sideways movement does nothing), `Shift` for fine steps; double-click fits the whole scene. It zooms around the playhead when the playhead is in view, otherwise around the middle of the view, and pauses **Follow** paging until the next **Play**. |
| **Fit** | — | Zooms so that the whole scene fits the width and scrolls to the start. A double-click on the ruler's timecodes does the same. |
| **Auto-key** | `K` | While on (red text), inspector edits write keys instead of changing the value (see [Inspector](#inspector)). Off by default; not remembered between sessions. |
| Opacity slider | — | Background opacity of the editor's windows, 0.1–1 (default 0.5). Remembered. |
| **Playing** / **Paused** | — | The playback state. |
| **Camera busy** | — | Shown in red while another camera mode (`demo_record`, `demo_play`) holds the view, so neither the scene camera nor the editor camera can show (see [Camera and view](#camera-and-view)). |

On a narrow timeline the transport row wraps onto more lines instead of running past the window edge. The status line at the bottom ends with an ellipsis when it does not fit. The right end of the status line shows a hint: what a drag, a modifier key or a right-click does on whatever is under the mouse (a clip edge, a fade handle, a crossfade, a pose key, the gizmo, the game view). **View → Hints** turns the hints off; the choice is remembered (`hints`).

Editing keys:

| Key | Result |
| --- | --- |
| `Ctrl+Z` | Undo. |
| `Ctrl+Y`, `Ctrl+Shift+Z` | Redo. |
| `Ctrl+S` | Saves the open scene without a dialog; an untitled scene asks for a name. |
| `Delete` | Deletes the selection (see [Editing](#editing)). |
| `F2` | Renames the selected row (one object or a folder). |
| `Ctrl+C` | Copies the selection (see [Edit menu](#edit-menu)). |
| `Ctrl+V` | Pastes at the playhead. |
| `Ctrl+D` | Duplicates the selection right after itself. |
| `F` | Adds keys at the playhead, as one undo step. With the mouse over a lane, keys that lane at its current value (on a Camera row lane, in the block or shake under the playhead; on a camera Position or Rotation lane, a pose key with the path's pose there). Otherwise keys every selected object's transform at its current values: an actor's Position, Rotation and Scale; a Spot light's Position and Rotation; a Point light's Position; a particle effect's Position, Rotation and Scale. With the Camera row selected it acts like **Key from view**; while the shot is on screen it does nothing and says so. A key within half a frame of the playhead is replaced. |
| `K` | Switches **Auto-key** on and off. |
| `B` | Switches posing mode (**View → Bones**) on and off. |
| `G` | Switches **Safe areas** (the action and title safe frames and the centre cross) on and off. |
| `Z` | Zooms to the selection (selected clips, or selected keys after a click on a key), else to the loop range, else to the whole scene. |
| `+` / `=` and `-` | Zoom in and out ×1.25 around the playhead. |
| `Ctrl+L` | Loops the selection: the loop brace is set to the time span of the selected keys (after a click on a key) or of the selected clips, switched on, and the playhead moves to its start. With nothing selected, or a span shorter than a frame, it switches the brace on or off. |

`Ctrl+C`, `Ctrl+V`, `Ctrl+D`, `Ctrl+L`, `F` and `K` do nothing during a drag, a gizmo drag or a fly, and `Space` does not play or pause while the right button is held over the game view.

The keys work only while ImGui has the mouse (`Alt`) and either no window is focused (after a click in the game view) or one of the Cinema Editor windows is focused. They do nothing while a text field is being typed into, while a field, button or slider is held (for example while dragging **Offset**) or while another tool's window is focused, and they never reach the game. A press on the timeline itself (ruler, rows, lanes) does not block them.

### Ruler and navigation

| Gesture | Result |
| --- | --- |
| Click or drag on the ruler | Moves the playhead, snapped like a clip (see [Snapping](#snapping)); hold `Shift` for free positions. While stopped the start marker moves with it; while playing only the playhead moves and playback keeps running. |
| Double-click the ruler's timecodes | Zooms to fit the whole scene. |
| Overview strip (the thin strip above the ruler) | Shows the whole scene: a thin line per object, the playhead and a box for the visible part. Drag the box to scroll, drag its left or right edge to zoom, click outside it to centre the view there. |
| Right-drag on the ruler (at least 4 px) | Draws a new loop range and switches the loop on. While the loop is on, the range is shaded on the rows and playback repeats it; playback started before the range runs into it. |
| Drag the loop brace (the bar in the top strip of the ruler, 4 px) | Moves the range, keeping its length, snapped like a clip (`Shift` frees it). |
| Drag a loop brace end (4 px) | Moves that end, snapped like the playhead; the range never gets shorter than a frame. The cursor is a left-right arrow over an end. |
| Click the loop brace (moving less than 4 px) | Switches the loop on or off, like **Loop**. |
| Double-click the top strip of the ruler | Sets the range to the whole scene and switches the loop on. |
| `Escape` or right-click during a brace drag | Puts the range back. |
| Right-click on the ruler | **Add marker here** (at the clicked time, snapped), **Clear loop**. |
| Wheel over the rows | Scrolls the rows. |
| `Shift` + wheel | Scrolls the time. |
| `Ctrl` + wheel | Zooms around the mouse. |

The loop brace is bright while the loop is on and grey while it is off; moving it is not an undo step. Elsewhere in the top strip a click scrubs as on the rest of the ruler.

The ruler shows timecodes about every 90 px, in steps of 1, 2, 5 or 10 frames or of whole seconds. A thin line marks the end of the scene and a bright line the playhead.

Markers are named points in time saved with the scene, for example the start of a line of dialogue. Each one is a small downward flag at the bottom of the ruler with its name after it, below the timecodes (cut short at the next flag), and a faint line through the rows; the flag under the mouse is highlighted. New markers are named **Marker 1**, **Marker 2** and so on.

| Target | Gesture | Result |
| --- | --- | --- |
| Marker flag | click (moving less than 4 px) | Moves the playhead to the marker. |
| Marker flag | drag (4 px) | Moves the marker, snapped like the playhead (`Shift` frees it), never before 0 or after 60:00:00. `Escape` or a right-click during the drag puts it back. A marker dropped within half a frame of another goes back, with a status line. |
| Marker flag | double-click | Renames it in place. `Enter` or a click elsewhere keeps the name, `Escape` cancels. |
| Marker flag | right-click | **Rename**, **Remove**. |
| Marker flag | `Delete` while the mouse is over it | Removes the marker. |
| Empty ruler | right-click | **Add marker here**: a marker at the clicked time, snapped. |
| **Edit → Add marker** | — | A marker at the playhead. |

- Adding, moving, renaming and removing a marker are one undo step each.
- Two markers are never closer than half a frame; a scene holds at most 256. Past either limit the marker is refused with a status line.
- Markers may lie after the end of the scene; they do not change the end line.
- Markers have no keyboard shortcut of their own.

### Clips and blocks

- A clip is a bar in the object's colour from its start to its end. A muted clip is faint and its name grey.
- An actor's own row is a plain bar from the actor's start to the end of its last animation clip (or its own length while it has no clips); it moves the actor with everything on it. Its animation clips are on the **Animation N** rows under it (see [Animation rows](#animation-rows)), each clip with its animation name. A hatched part is a clip's **Hold** (the last frame frozen) or a gap between clips, where the frame shown before it holds. Where two clips on a row overlap, an X marks their crossfade. Shaded top corners show a clip's **Fade in** and **Fade out**; the clip under the mouse and the focused clip also show a small square handle for each at the top, a little inside the clip's edge so that it is never on the edge itself. Short marks at the top of a clip show where its loops repeat. With a **Time scale** (the static value or a lane) the clip edges are drawn where they play.
- An actor from an older scene shows its old sequence until it is converted (see [Older actors](#older-actors)): with **End** set to Loop or Ping-pong the sequence repeats until its clip ends, and Ping-pong draws every second pass mirrored.
- Motion marks of an animation (footsteps and other moments the game reacts to) show as short white ticks over the lower part of the block, at the moment each mark starts, also on repeated, mirrored and time-scaled passes. Hovering a tick names the mark. Ticks are not stored in the scene; they come from the animation.
- A row whose model cannot be loaded, or that has an animation that cannot be found, has its name in red. Without a model the whole clip is red and shows the error. The scene data is kept as it is and saved unchanged; nothing is removed.
- A block whose animation cannot be found has no length, so it is not drawn; a red mark shows where it would play in the sequence.
- The **Camera** row is always there, also in a new scene, and is the scene's one camera track. It shows its camera clips (shots) where they play, each a bar in the camera's colour, with gaps between them where the view passes through; above them a thin band from 0 to the end of the last clip or shake is the track's handle. A hatched clip has no pose keys. Where two clips touch, scissors just under the band mark a jump cut; where they overlap, an X over the overlap marks their crossfade (see [Camera clips](#camera-clips)). Under the Camera row, one **Shakes N** sub-row per shake row holds the camera's shake clips (see [Camera shakes](#camera-shakes)). Pose keys are diamonds on the clips, filled while the playhead is on them; a key past its clip's end is faint.
- Under every actor, one **Animation N** sub-row per animation row (at least one) shows that row's clips on a fainter bar the length of the actor (see [Animation rows](#animation-rows)).
- A row whose lanes are closed shows a small diamond along the bottom of its clip at every time one of its lanes has a key (on the Camera row, the keys of every block's lanes; pose keys are the row's own diamonds). A shake clip always shows its own lane keys this way.
- A selected clip has a bright outline in its own colour, and a selected row header is highlighted. The focused animation block has a thin bright outline.

## Editing

### Selection

There is one selection, shared by the timeline, the inspector and the game view (see [Viewport](#viewport)).

| Gesture | Result |
| --- | --- |
| Click a clip or a row name | Selects it alone. Pressing on a clip that is already selected keeps the whole selection, so it can be dragged; releasing without a drag then selects it alone. |
| `Ctrl` + click | Adds or removes the object. |
| `Shift` + click | Selects every object row from the last plain or `Ctrl` click to this one, in row order. |
| Click a folder name | Selects the folder only. |
| Click empty track space | Clears the selection. |
| Drag on empty track space (6 px) | Draws a selection box. Every clip it touches is selected, across rows (the Camera row, lights and particle effects included), replacing the selection; the clips light up while you drag. Near the top or bottom edge the rows scroll. |
| `Ctrl` + drag on empty track space (6 px) | Toggles every clip the box touches, like `Ctrl` + click. |
| `Escape` or right-click during a box | Cancels the box; the selection is the one from before it. |

Selection changes are not undo steps.

### Gestures

| Target | Gesture | Result |
| --- | --- | --- |
| Clip body | drag (6 px) | Moves every selected clip in time, snapped. Clips never go before 0. On the Camera row, only the top band is the clip body; below it a press acts on the camera clip (see [Camera clips](#camera-clips)). An actor's row is all band: it moves the actor with its animation rows and lanes, so the animation stays as it was; its clips are edited on its **Animation N** rows (see [Animation rows](#animation-rows)). The Camera row's band moves every camera clip and every shake by the same amount, never before 0. |
| Clip body | `Ctrl` + drag (6 px) | Copies the selected clips (new colours, same names) and drags the copies; the originals stay. The camera is never copied. |
| Clip right edge | drag | Changes the clip length, snapped. An actor with animation clips has no edge: it reaches from its start to the end of its last clip. An actor without clips, or one not converted yet, keeps the edge, and a length set by hand stops the automatic fit. The Camera row has no edge: it reaches from 0 to the end of its last clip or shake. |
| Row name | drag (6 px) | Moves the row. A line or a frame shows where it lands: on the middle of a folder name the row goes into the folder (at its end); between rows it goes there, inside or outside a folder; below the last row it goes to the end. Dragging a selected row moves every selected row that is shown (not those inside a collapsed folder) together, in their order on screen, also into a folder; one undo step. A folder can only move among the top-level rows, and always alone. |
| Row name | double-click, or `F2` | Renames in place. `Enter` or a click elsewhere keeps the name, `Escape` cancels. Renaming an object stops its name from following its model. |
| Colour swatch | click | Colour picker for that object. |
| **M** | click | Mutes or unmutes the object; on a selected row, the whole selection. A muted actor is hidden. |
| **S** | click | Solos or unsolos the object; on a selected row, the whole selection. While any object is soloed, every other object plays as if muted: actors are hidden, lights are off, particle effects stop and the camera passes the view through. Their clips and names are drawn faint. Solo is not saved, is not an undo step and is cleared when the editor closes or another scene opens. |
| Folder arrow | click | Collapses or expands the folder. |
| Clip or row name | right-click | Row menu: **Rename**, **Colour...**, **Mute**, **Reset length** (actors without animation clips), **Delete**. Mute, Reset length and Delete act on the selection; **Delete** is grey on the Camera row alone, which cannot be deleted. On an animation block the menu starts with the block items (see [Animation rows](#animation-rows)); on an actor it also has **Add row** (see [Animation rows](#animation-rows)). |
| Empty track space | right-click | **Add** pop-up at the clicked time. |
| Any drag | `Escape` or right-click | Cancels the drag and restores the clips; a cancelled copy removes the copies. |

**Reset length** (only on actors without animation clips or not converted yet) unlocks an actor clip and fits it to its sequence again (two passes with Loop or Ping-pong, through Time scale). The fit is skipped when the model or an animation is missing.

**Delete** removes the pose key under the mouse (see [Camera clips](#camera-clips)), else the selected lane keys after a click on a key (see [Lanes](#lanes)), else the focused animation block, camera clip or shake clip after a click on it (a neighbouring block is then focused), otherwise the selected objects. The Camera row itself is never deleted: with only the camera selected the status line says so, and with other objects it is left out. A click on a clip, a row name or empty track space returns `Delete` to the objects. With a folder selected it removes only the folder; its members move to the end of the top-level rows.

### Animation rows

An actor's animation lives on its **Animation N** rows, shown under the actor's row: **Animation 1** always exists, and an actor has up to 8. The rows work like the camera's **Shakes N** rows. Each row holds animation clips, each at its own time after the actor's start. A clip plays a range of one animation at its speed, repeats it for its **Loops**, then holds the last frame for its **Hold**; its **Fade in** and **Fade out** ramp it in and out with its ease. The actor's own row is its band: it moves the actor, and the actor is shown from the first clip to the last clip of its unmuted rows. An actor without any clip keeps a clip of its own, as before, and shows its idle pose there.

- **Drawing:** a row draws only its clips, each a box in the actor's colour (faint while muted or silenced); there is nothing before the first clip or after the last. Between two clips a dotted line at mid-height shows the held pose.
- **One row, two clips at a time.** Two clips on one row may partly overlap: the overlap is a crossfade, drawn as an X, with both clips moving and the later clip's ease. A clip inside another, or a third clip overlapping at once, does not fit on that row.
- **Animation 1** covers the whole body until parts are ticked (its list reads **Whole body**). Before its first clip it holds that clip's first frame, in a gap the frame before the gap, and after its last clip the last frame. With no clips it shows the idle pose.
- **Animation 2 and up** play from the start of their first clip to the end of their last; before and after that they are off and the rows below show. Between their clips they hold the previous clip's last frame at the row's weight, as faded as that clip ended (a clip that fades out stays out until the next clip). A clip with **Fade in** after a gap crossfades from the held pose. A fade shapes a row's first and last edges.
- **Body parts:** a row acts only on its parts, the parts the model's skeleton is divided into (usually legs, torso and head; a model without parts has one). The list in the row header (and in the inspector) ticks parts on and off; parts the model does not have are listed in red and can be unticked. A new row starts with every part of the model. Part names are kept by name, so changing the model keeps them; when a row covers no part of the model, its list shows **No body parts** in red. A row keeps at most 8 part names; ticking one more drops the red names first.
- **Weight** (0–1, default 1) is how strongly a row plays. It is keyable: the actor's **+ Lane** list offers **Animation N weight**, and the inspector row has the key diamond.
- **Mode** (right-click the row's name, or the **+** button in the inspector): **Override** (the default) covers the rows below on its parts by its weight: 1 replaces them, 0.5 is halfway. **Add** adds the row's clips on top of everything else as a change from the animation's first frame (frame 0 of the animation, not the clip's trimmed start), scaled by the weight: animations made as additive motions (a nod, breathing, recoil) work directly, and a normal animation adds its movement relative to its first pose. A trimmed (In above 0) or reversed Add clip adds the difference from frame 0 too, so it can start with an offset; the inspector warns about such clips. At most 4 rows can add; switching a fifth is refused with a status line. Add rows always apply after all Override rows, whatever their number. The first two Add rows add their change in each bone's own space; the third and fourth add it in the parent bone's space, so the same clip can look different there. An Add row reads **Animation N (add)**.
- **M** in the row header mutes the row; a muted row is drawn faint and does not play. **x** removes the row with its clips and its weight lane (not the last row); the rows after it move up one number. Right-clicking the row's name also offers **Add row** and **Remove row**. Each is an undo step.
- A body part that no playing row covers takes the idle animation, so a row limited to a part never leaves the rest of the body without a pose. This also holds when Animation 1 is muted or set to Add: the body falls back to idle, and Add rows still apply on top.

| Target | Gesture | Result |
| --- | --- | --- |
| Clip | click | Focuses the clip in the inspector and selects the actor. |
| Clip | drag (6 px) | Moves the clip in time, snapped to the grid, the playhead, markers, clip edges, motion marks and the other clips' edges (`Shift` frees it). It stays on its row until the mouse is clearly over another row (a quarter of a row past the edge); then it goes there. While you drag, an empty row shows under the last one, so the clip can go to a new row (up to 8). The clip lands where it is dropped and **overwrites** what is there: a clip it covers completely is removed, a clip it lands inside is cut around it, and where it only reaches over one end of a clip the overlap is a crossfade. While dragging, the clips that will be removed or cut show faintly and hatched in their old shape; moving away brings them back. It never goes before the actor's start. |
| Clip | `Ctrl` + drag (6 px) | Copies the clip; the original stays. |
| Clip left edge | drag | Trims the start of the range (the end that plays first; with a negative speed that is the range end). The end of the clip stays where it is. |
| Clip right edge | drag | Trims the end of the range. Past the end of the animation the clip **loops**: whole loops while **Snap** is on, any length while it is off. Dragging back removes loops first, then trims. |
| Clip right edge | `Shift` + drag | Changes the **Hold** (hatched) instead. |
| Either clip edge | `Ctrl` + drag | Changes the **Speed** so that the played part fills the new length; the other edge stays. The speed stays within 0.05–4 and keeps its sign. |
| Fade handle (small square at the top, near each end) | drag | Sets **Fade in** / **Fade out**, up to the clip's length. The handles show on the clip under the mouse and on the focused clip; a clip narrower than 24 px has none (use the inspector). |
| Crossfade (the X) | drag | Slides the later clip along its row: the crossfade gets longer or shorter, and past the end of the earlier clip it becomes a gap. |
| Crossfade (the X) | right-click | **Ease** (Linear, Ease in, Ease out, Ease in-out), **Remove crossfade** (the earlier clip is trimmed so that it ends where the later one starts). |
| Clip | right-click | **Replace animation...** (the chooser opens in the menu; the range resets to the whole animation; speed, Loops, Hold, fades, ease and start stay), **Split at playhead**, **Ease**, **Remove**, then the row items. |
| Empty row space, or the actor's row | right-click | **Add animation here...** opens the chooser: the clip goes at the clicked time on that row (on the actor's row: **Animation 1**) when nothing is there, otherwise on the first row where it overlaps nothing (a new row when needed). |
| Any clip drag | `Escape` or right-click | Cancels and restores the clips. |

- The part of a clip under the mouse lights up before you press: an edge as a bright bar, a fade handle filled, the body or a crossfade as an outline. The press takes exactly that part. An edge can also be grabbed up to 4 px outside the clip where no other clip is next to it, so a very short clip keeps its body for moving.
- Inside a crossfade, a press near the earlier clip's right edge trims or loops that clip, and a press near the later clip's left edge trims the later one.
- Trims, the Hold, fades and speed changes snap to whole frames; `Shift` or **Snap** off frees them. On the right edge `Shift` means Hold, so there only **Snap** off frees the loop count.
- A trim, Hold, loop or speed change stops where the clip would lie inside its neighbour or overlap two others: the edge stays at the last allowed place, and a red tick marks it while the mouse is past it.
- Moving, trimming and the other clip drags need the actor's model and every animation of its clips; while one is missing or still loading a drag only shows a status line.
- **Split at playhead** cuts the clip under the playhead at the frame being shown, also inside a loop: the first part keeps the loops played so far, and the rest continues where it was. It is disabled when the playhead is not inside the played part of that clip.
- **+ Animation** in the inspector adds the clip at the playhead, or right after the focused row's last clip when the playhead is before its end; paste and duplicate put the copied clips right after the focused clip (at the playhead when none is focused). All three use the focused row when nothing is there, otherwise the first row where the clip overlaps nothing (a new row when needed); when every row is taken, the status line says so.
- A clip trimmed to a single frame with a Hold shows that frame still for the Hold: a pose clip.
- A clip whose animation is missing, for example after its pack was removed, takes no time and shows as a red mark; the other clips keep their times. Its data is kept and comes back when the pack returns.
- The actor's Time scale applies to all its rows. Clicking a clip, **+ Animation**, `Ctrl+C`, `Ctrl+V` and `Ctrl+D` work on the row of the focused clip. The animation chooser's hover preview plays on the whole body.

### Camera clips

The Camera row is one camera track. It holds camera clips (shots) placed freely in time, and it may be empty. Each clip is a shot of its own: its pose keys with the moves between them, its own lanes (**Position**, **Rotation**, **FOV** and the depth-of-field values), its own attachment and the shake clips that belong to it (see [Camera shakes](#camera-shakes)). While the playhead is inside a clip with pose keys, that clip drives the view; in a gap between clips, outside every clip, inside a clip without keys or on an empty track the view passes through (the editor camera when it is on, otherwise the player's view). Every click on the Camera row (name, band, clip or diamond) brings the shot back after a fly.

A clip's path is two ordinary lanes: **Position** (X, Y, Z in metres) and **Rotation** (heading, pitch, bank in degrees), each axis a curve of its own with its own keys, interpolation and Bezier handles. The diamonds on the Camera row stand for every moment at which any of these six curves has a key; dragging a diamond moves all the axis keys at that moment together, and its menu (**Update from view**, **Interpolation**, **Remove**) acts on all of them. On the lanes each axis key moves on its own. A clip's **Time scale** (static or a lane) sets how fast the path is travelled: the Position and Rotation keys sit on the path, so a slower Time scale spreads them out on the timeline, the lanes and the diamonds are drawn where they play, and every key added at the playhead (Key from view, Pilot, recording, the gizmo, the inspector) lands on the path where the playhead is. Splits, trimming the left edge, Insert and Remove time and the bottom-right stretch keep the path where it plays. The graph editor shows Position and Rotation curves in path time.

- Two clips that touch make a **jump cut**: the view jumps from one shot to the next, marked with scissors.
- Two clips that overlap **crossfade** over the overlap, marked with an X: both shots keep moving (the earlier one holds its last pose past its last key), and the view goes from one to the other with the later clip's **Ease**; the shakes of both clips fade with their shot. Outside that crossfade a shake always plays at full strength, also past the end of the clip it belongs to.
- Clips never overlap more than two at a time, and a clip never lies wholly inside another (nor starts where another starts). A clip moved onto others overwrites them (see the gestures below); a resize that would break that stops at the nearest allowed place.

The gestures below work under the row's top band; the band moves every clip.

| Target | Gesture | Result |
| --- | --- | --- |
| Clip | click | Focuses the clip in the inspector and selects the Camera row. |
| Clip | drag (6 px) | Moves the clip in time, snapped to the grid, the playhead, markers and other clip edges (`Shift` frees it). It lands where it is dropped and **overwrites** what is there: a clip it covers completely is removed, a clip it lands inside is cut around it (the parts left keep their path, as after **Split at playhead**), and where it reaches over one end of a clip the overlap becomes a crossfade. The clips that will be removed or cut show faintly and hatched while dragging; moving away brings them back. Its shake clips move with it; the shakes of a removed clip stay where they were. |
| Clip | `Ctrl` + drag (6 px) | Copies the clip with its shakes to where it lands; the original stays. |
| Clip left edge | drag | Trims the start; the end stays. The shot stays where it is in time: its keys and lanes shift so what remains plays as before (a pose key is added at the new start with the pose the path has there). Dragging the edge earlier extends the shot, holding its first pose. Snapped like a move. |
| Clip right edge | drag | Changes the clip's length, snapped (`Shift` frees it). Pose and lane keys keep their times; keys past the new end turn faint and come back when the clip grows again. Dragged over the next clip, the overlap becomes a crossfade. |
| Clip right edge | `Ctrl` + drag (`Ctrl` held at the press) | Stretches the clip: the length, every pose key, every lane key and every Bezier handle scale together, so the same path plays slower or faster. Generators keep their own speed. Its shake clips stretch with it: their start, length and fades scale, their lane keys keep their times. |
| Crossfade (the X) | drag | Slides the later clip: the crossfade gets longer or shorter; past the end of the earlier clip it becomes a gap. |
| Crossfade (the X) | right-click | **Remove crossfade** (the earlier clip ends where the later one starts, a jump cut), **Ease** (Linear, Ease in, Ease out, Ease in-out). |
| Scissors (a jump cut, just under the band) | right-click | **Crossfade**: the earlier clip grows over the later one by the last crossfade length used in this session (0.5 s at first, at most the later clip's length). Refused with a status line where the clips would then overlap a third clip. |
| Pose diamond | click | Moves the playhead to the key; playback keeps running. |
| Pose diamond | drag (6 px) | Retimes the key, snapped to the grid, the playhead, clip edges and the other pose keys (`Shift` frees it); it never passes its neighbours. |
| Pose diamond | right-click | **Update from view** (the key takes the current view; disabled while the shot itself is on screen), **Interpolation** (Linear, Ease in, Ease out, Ease in-out, Bezier, Hold), **Remove** (disabled on a clip's last key). |
| Pose diamond | `Delete` while the mouse is over it | Removes the key (not a clip's last key). |
| Clip | right-click | **Key from view here** (adds or replaces the key at the clicked time from the current view; disabled while the shot is on screen), **Split at playhead**, **Remove clip**, **Add shake here**, then the row items. |
| Empty part of the Camera row | right-click | The **Add** pop-up, whose **Camera move** adds a clip there. |
| Any clip or diamond drag | `Escape` or right-click | Cancels and restores the clips. |

- The part under the mouse lights up before you press, as on animation clips. A pose diamond wins in the middle third of the row's height; above and below it, an edge at the same place wins.
- A pose key's interpolation belongs to the segment after it. **Bezier** makes a smooth path through the neighbouring keys; the other modes move along a straight line, eased in time. Old smooth cameras import as Bezier keys and play exactly as in the old editor.
- How the path bends at a key is set by the Bezier handles of the axis keys there, on the Position and Rotation lanes or with the handles in the game view (see [Path tangents](#path-tangents)).
- A new key takes the interpolation of the key before it (Ease in-out in an empty clip). A key within half a frame of the playhead is replaced instead of adding a second one.
- **Split at playhead** cuts the clip under the playhead in two touching clips. Both halves get a key at the cut with the pose and lane values there, so the path, FOV and Time scale continue across the cut and the jump cut between them shows nothing. A cut through a Bezier segment gives the new keys **Broken** handles that follow the curve at the cut, and turns the keys on either side **Broken** with their handles shortened to match, so the position path keeps its shape; a cut exactly on a key keeps that key's own handles; an eased segment eases each half on its own, and the turn is re-eased on each half. Shake clips are not cut; from then on each one belongs to the half it starts in. It is disabled when the playhead is within 0.01 s of the clip's edges.
- **Even speed** (clip menu; needs at least three pose keys) moves the keys between the first and the last in time so that each stretch of the path takes time in proportion to its length; the first and last key, the path's shape and the clip's length stay. Easing inside a stretch stays too, so use Linear or Bezier keys for a truly constant speed. Keys never come closer than one frame; when there is no room for that the status line says so. One undo step.
- **Key from view**, Pilot and `F` (see [Editor camera](#editor-camera)) key the clip under the playhead (in a crossfade, the later clip). In a gap or on an empty track they add a new clip there instead, 5 s long or shorter so it ends where the next clip starts, with one pose key from the view; with less than a frame of room they refuse with a status line. Neighbouring clips are never lengthened.
- **Remove clip** also removes the shake clips that belong to it, in the same undo step.
- A clip keeps at most 1024 pose keys; the camera keeps at most 256 clips. Past a limit the item is disabled or the edit is refused with a status line.

### Camera shakes

Shakes are clips of their own on up to 8 **Shakes N** sub-rows under the Camera row. A sub-row is shown for every row that holds a shake, plus an empty one below while a shake is dragged. A shake clip plays one of the 9 shake presets on top of the shot under it, from its start to its end, with its own lanes for **Influence**, **Position scale** and **Speed**. Two shakes on one sub-row may overlap: the overlap is a crossfade, drawn as an X. No more than two shakes overlap at a time, and a shake never lies wholly inside another on its sub-row.

A shake clip is drawn in the camera's colour with its preset name; a switched-off shake is faint. Its fades are shaded ramps at its start and end, each with a small handle at the top.

| Target | Gesture | Result |
| --- | --- | --- |
| Shake clip | click | Focuses it in the inspector (**Shake** group) and selects the Camera row. |
| Shake clip | drag (6 px) | Moves it in time, snapped (`Shift` frees it), and up or down to another sub-row once the mouse is clearly over it. It lands where it is dropped and **overwrites** the shakes on that sub-row: one it covers completely is removed, one it lands inside is cut around it, and over one end of a shake the overlap becomes a crossfade. |
| Shake clip | `Ctrl` + drag (6 px) | Copies it and drags the copy; the original stays. |
| Left or right edge | drag | Moves that edge, snapped; the other edge stays. Moving the left edge keeps the shake where it was in time: its lane keys and its **Time offset** shift by the same amount (a shake with **Stretch** keeps its offset), so the part that remains plays as before. An edge stops where the shake would lie inside another, hold one inside it or overlap two at once; a red tick marks the place while the mouse is past it. |
| Fade handle (small square at the top) | drag | Sets **Fade in** or **Fade out**, on whole frames (`Shift` frees it), at most the shake's length. A longer fade (from an imported scene) is kept until its handle is dragged; it is drawn cut at the clip end. |
| Crossfade X | drag (6 px) | Slides the later shake in time on its sub-row, changing the overlap (`Shift` frees the snapping). The X's outer 6 px still grab the shakes' edges and the top corners their fades. |
| Crossfade X | right-click | **Ease** (Linear, Ease in, Ease out, Ease in-out; default Ease in-out) and **Remove crossfade**, which moves the later shake to start where the earlier one ends (disabled when there is no room there). |
| Shake clip | right-click | **Preset** (the 9 presets), **Stretch**, **Remove**. |
| Sub-row header **Shakes N** | right-click | **Add** or **Override** (see below), as one undo step. |
| Empty part of a sub-row | right-click | **Add shake here**: a 2 s shake at the clicked time on that sub-row, with the first preset. |
| Empty part of a sub-row | drag | Draws a selection box like empty track space. |
| Any shake drag | `Escape` or right-click | Cancels and restores. |

- The header **Shakes N** has an **M** button that switches every shake on that sub-row off or back on, as one undo step.
- **Add shake here** on the Camera row's menu puts a 2 s shake on the first sub-row that is free at the clicked time. A new shake (**Add shake here** on either menu) never overlaps another: on a sub-row where it would, the item says no row is free.
- A shake clip belongs to the camera clip it starts in (in a crossfade, the later clip). Moving, copying or removing that clip moves, copies or removes the shake with it, and stretching the clip stretches it; trimming its left edge or changing its length leaves the shake in place. Shakes that start in no clip stay where they are. A shake counts as starting in a clip when its start is within 0.001 s of the clip's start or later. When a moved shake would overlap another on its sub-row the moved one goes to the next free sub-row, never a shake that stayed put; when all 8 are taken it stays where it is, overlapping, and the status line says so (once the drag or field edit ends).
- Fades are linear and multiply **Influence**; where the two fades overlap they multiply each other.
- In a crossfade the earlier shake's influence is multiplied by 1 − e and the later one's by e, where e runs from 0 to 1 over the overlap with the later shake's **Ease**. Fades still apply and multiply with it. A switched-off shake takes no part in a crossfade.
- Each sub-row has a mode. **Add** (the default) adds its shakes to the ones below, as before. **Override** fades out every sub-row below it by its own strength: with its shakes' influence (times fades and crossfades, added up and kept within 0–1) at w, the shakes below are scaled by 1 − w. For example a hard hit on **Shakes 3** set to **Override** briefly replaces a handheld wobble on **Shakes 1**, which returns as the hit fades. The header reads **Shakes N (override)**. A sub-row left with no shakes goes back to **Add** when the edit ends.
- **Stretch** plays the whole shake once over the clip; otherwise the shake runs at its **Speed** from **Time offset**.
- Shakes apply once, to the shot as it is after a crossfade. A shake that belongs to a clip is weighted by that clip's share of the view: fully while its clip plays alone, fading out with an outgoing clip and in with an incoming one, and not at all while another clip plays. A shake that starts in no clip shakes whichever clip is on screen at full strength. Shakes play only while a shot is on screen: in a gap, over a clip without pose keys or with the Camera row muted the view is not shaken.
- `Delete` removes the focused shake. `Ctrl+C` with a shake focused copies it; `Ctrl+V` puts it at the playhead on its sub-row, or on the next sub-row where it overlaps nothing; `Ctrl+D` puts the copy right after it.
- The camera holds at most 256 shakes.

### Add

The **Add** menu acts at the playhead, the **Add** pop-up at the clicked time.

| Item | Result |
| --- | --- |
| **Actor...** | Opens the model chooser in the pop-up. Picking a model adds an actor with a 5 s clip at that time, 3 m ahead of the view and dropped onto the ground below when there is ground within 5 m, upright and facing the camera. It is named after the model and selected. |
| **Actor from preset...** | Opens the preset list in the pop-up; disabled while there are no presets. Picking a preset adds an actor like **Actor...** and gives it the preset's model, item, scale, Time scale, packs and clips, the first clip at that time. One undo step. A preset whose model is missing adds nothing and says so in the status line. |
| **Particle...** | Opens the effect chooser in the pop-up. Picking an effect adds a particle effect with a 5 s clip at that time, placed like **Actor...** (3 m ahead, dropped onto the ground, facing the camera), named after the effect and selected. |
| **Decal...** | Opens the texture chooser in the pop-up. Picking a texture adds a decal with a 5 s clip at that time, placed like **Actor...** (on the ground, its up axis along the ground's normal), named after the texture and selected. |
| **Light** | Adds a Point light named **Point light** at that time, 3 m ahead of the view and turned along the view, so it points where you look when switched to Spot. Its clip runs to the end of the scene, or lasts 5 s when that is less than 1 s away. It is selected. |
| **Weather** | Adds a weather row with a 10 s clip at that time, set to the game's current weather and time of day (see [Weather rows](#weather-rows)). |
| **Post** | Adds a post-processing row with a 5 s clip at that time and nothing set (see [Post rows](#post-rows)). |
| **Camera move** | Adds a camera clip with one pose key taken from the current view: the editor camera while it is the view, otherwise the game view. From the editor camera with a field of view of its own (`R` / `T`), the clip takes that FOV. It starts at that time, or where the clip under that time ends, and lasts 5 s, or less so that it ends where the next clip starts; nothing else moves. With less than a frame of room it is refused with a status line. The new clip is focused in the inspector. |
| **Folder** | Adds an empty folder at the end of the rows and starts renaming it. |

### Lanes

An actor's animated values live in lanes under its row, like automation lanes in Ableton. The arrow at the left of the row header opens them; each lane shows one value (**Position**, **Rotation**, **Scale**, **Time scale**) across the clip. Keys are stored relative to the clip start, so they move with the clip. An opened row shows one lane for every animated value, plus a **+ Lane** list at the bottom that adds a lane for another value. A lane added this way stays empty until it gets a key or a generator, and is dropped when the row is collapsed. Opening a row that has no animated values shows one empty lane right away: **Position** for an actor, **FOV** for the Camera, **Intensity** for a light, **Speed** for a particle effect.

- The lane's height range fits its keys (on the camera's Position and Rotation lanes, its pose keys), Bezier handles and generator band with room above and below, at least a quarter of the value's range or 10 units, whichever is smaller, and never starts below the value's minimum when that can be avoided. The range stays still while the mouse is over the lane, so clicks and drags land where they point; it refits when the mouse leaves, and also while a pop-up (such as the generator pop-up) covers the lane, so generator edits show at once.

- The lane draws the curve in the object's colour; Position, Rotation and Scale draw X, Y and Z in red, green and blue. When the lane has a generator, the result with the generator (what plays) is drawn on top as a thinner, lighter line. The value range fits the keys, the Bezier handles and the generator band.
- Keys are diamonds; selected keys are white. Keys past the clip end are faint. Outside the clip the lane is empty.
- Position, Rotation, Scale and Color have one lane with all three lines in one graph. A click, double-click or right-click acts on the key of the line you clicked only (X, Y or Z); to work on all three keys at a time, box-select them. Dragging a key moves it in time and changes only its own line. The **X** **Y** **Z** chips (**R** **G** **B** for Color) at the start of the lane header choose which lines are edited: a click shows that line alone (a second click shows all three again), `Ctrl` + click adds or removes a line. Hidden lines stay drawn faint; their keys are not picked, boxed, grouped or keyed.
- On Position, Rotation, Scale and Color lanes each axis gets its own value range (like a normalised view), so a small move on one axis fills the lane even when the axes are far apart in value. The range labels show the first shown axis; show one axis alone with its chip to read its range. Position ranges are at least 2 m tall.
- The lane shows its value range: the top and bottom values at its left edge (on a one-row lane as one `low .. high` label) and a faint line at zero when zero is in range. On a lane two rows tall or more, the first and last key of each clip carry a small `time | value` tag.
- A **Time scale** lane changes how fast the sequence plays, and the animation blocks on the row are drawn where they play. Every Time scale lane edit fits a clip whose length was never set by hand.

Lights and particle effects have lanes the same way. A light offers **Position**, **Rotation**, **Color**, **Intensity**, **Range**, **Cone**, **Virtual size**, **Volume intensity**, **Volume distance** and **Volume quality**; a particle effect offers **Position**, **Rotation**, **Scale** and **Speed**. **Color** is one lane with three lines named R, G and B, and its axis chips and generator toggles read **R** **G** **B**. The light's colour is HDR: what lights the scene is Color times Intensity. A particle's **Speed** is how fast its effect runs, so the effect's own time is the area under the Speed lane. On a Point light the **+ Lane** list and the lane's parameter list leave out Rotation and Cone, and lanes it already has for them are hidden with their keys kept; they come back once the light is a Spot.

On the **Camera** row each camera clip has its own **Position**, **Rotation** and **FOV** lanes (and the depth-of-field lanes), and each shake clip its own **Influence**, **Position scale** and **Speed** lanes. The row shows one lane per value: Position, Rotation and FOV across all blocks, and "Shakes 2 Influence" and so on across the shakes of that sub-row. Each block or shake draws its own part, and the keys are stored relative to its start, so they move with it.

- The **+ Lane** list and the lane's parameter list offer Position, Rotation, FOV, Time scale and the three values of every shake sub-row that exists.
- A click adds the key at the clicked time (snapped) in the block or the shake under the click; in a shake lane outside every shake of that sub-row nothing is added and the status line says so.
- The generator fields edit the block or shake under the playhead (else the first one).
- **FOV** 0 means the game FOV. Adding a key or a generator to a block whose FOV is Game first sets the block's FOV to the current game FOV, so the shot starts from what the player sees.

The **Position** and **Rotation** lanes of the Camera row are ordinary lanes (X/Y/Z or heading/pitch/bank chips, range labels, Bezier handles on every key, the curve menu and the Graph Editor). On an attached clip the values are in the clip's own space, as in the inspector.

The lane header is one line: the axis chips (on a three-line lane), the parameter list (switches the lane to another value; the old value keeps its own lane), the generator type, **~** and **x** (removes the lane; the value stays where it was at the playhead). The parameter and generator lists share the width and end with an ellipsis when it is short. **~** opens the generator pop-up: on a three-line lane the **X** **Y** **Z** toggles (**R** **G** **B** for Color) that choose which components the generator edits, then **Amplitude**, **Frequency** (cycles per second), **Phase** (cycles), **Min** and **Max**. Each field drag there is one undo step. A lane is one row tall by default; drag the bottom edge of its header to resize it (see [Timeline](#timeline)).

Every lane takes a generator, Time scale included. The result is the curve plus the wave, kept within Min, Max and the value's own range. Min and Max always stay within the value's range, and Min never goes above Max.

The lane shows what a press will do: over a key or a Bezier handle a ring is drawn around it and the cursor is a hand; over the curve between two keys (within about 4 px) that segment is drawn bright and the cursor is an up-down arrow; anywhere else a press adds a key or starts a box.

| Target | Gesture | Result |
| --- | --- | --- |
| Empty lane | click (moving less than 6 px) | On release, adds a key at the clicked time, snapped (`Shift` frees it), with the value at the mouse height, kept within the value's range, and selects it (`Ctrl` adds it to the selection). On a three-line lane only the line nearest the click gets a key, at the click height; the other lines are not keyed. On the Camera row the key goes into the block under the click. Only inside the clip: snapping never moves the key out of the clip or block that was clicked, and a click outside the clip, or in a camera clip without that shake, says so in the status line. Drag the new key to move it. |
| Empty lane | drag (6 px) | Draws a selection box. Every key whose diamond is inside it is selected, in every lane it crosses on screen (shown axes only, Camera lanes included), replacing the key selection. Near the top or bottom edge the rows scroll. A box over nothing clears the key selection. |
| Empty lane | `Ctrl` + drag (6 px) | Toggles every key inside the box. |
| Key | click | Selects it alone. Pressing a selected key keeps the selection, so it can be dragged; releasing without a drag selects it alone. |
| Key | `Ctrl` + click | Adds or removes it. |
| Key | double-click | Removes it; on a three-line lane, the X, Y and Z keys at that time. A double-click on empty lane space adds one key and keeps it. |
| Key | drag (6 px) | Moves the selected keys in time and value. The time snaps like a clip (to the grid, the playhead, clip edges and the other keys of the dragged lanes); `Shift` frees it and gives fine value steps. Keys never go before the clip start; a key landing within half a frame of another replaces it. On a three-line lane, up and down change only the line of the key that was grabbed (in every selected lane of that value); the other keys move in time only. |
| Key | `Ctrl` + drag (6 px) | Copies the selected keys and drags the copies; the originals stay. |
| Segment (curve between two keys) | drag (6 px) | Moves both keys' values together, kept within the value's range; `Shift` gives fine steps. The two keys become the selection. On a three-line lane it is the segment of the line under the mouse. |
| Segment | `Ctrl` + drag (6 px) | Bends the segment: up bows the curve up, down bows it down. Its first key turns Bezier and both handles are set symmetrically; the curve never goes past the two key values and never turns back in time. `Shift` gives fine steps. On a three-line lane it bends the line under the mouse. |
| Segment | right-click | Key menu for the segment's first key, whose interpolation is the segment's. |
| Tangent handle | drag | Every key shows its two tangent handles while the mouse is near it or while it is selected, whatever its interpolation; a non-Bezier key shows the tangent it has now. Dragging a handle shapes the curve; on a segment that is not Bezier yet, the first press turns it Bezier with handles that keep its current shape, so nothing jumps. The opposite handle of the key turns with it (same slope, its own length) when that side is Bezier; `Ctrl` while dragging moves this handle alone. A handle stays on its own side of the key and within its segment. `Shift` gives fine steps. One undo step. |
| Key | right-click | Key menu for the selection (the clicked key is selected first): **Interpolation** (Linear, Ease in, Ease out, Ease in-out, Bezier, Hold), **Broken tangents** (the two handles move on their own; a `Ctrl` handle drag also sets it), **Easy ease**, **Remove**. |
| Empty lane | right-click | Curve menu for the lane under the click (the block or shake under the click on the Camera row), acting on the shown axes: **Copy curve** (keys and generator of every axis), **Paste curve** (onto a lane with the same number of axes; the first copied key lands at the clicked time, values kept within the value's range, keys within half a frame replaced), **Reverse** (mirrors the key times inside the first-to-last key span; Ease in and Ease out swap, Bezier handles mirror), **Scale time** x0.5 / x0.75 / x1.5 / x2 (around the first key, handles too), **Smooth keys** (each inner key value becomes a 1-2-1 average with its neighbours; the first and last keys stay), **Reduce keys** (removes keys that lie within 1 % of the lane's shown range of a straight line between the kept keys; first and last stay), **Presets** (Ease in-out, Overshoot, Bounce, Step: keys over 1 s from the clicked time, starting at the curve value there and moving by a quarter of the shown range; Wiggle: a smooth-noise generator, 4 Hz, a fifth of a quarter of the shown range). Each item is one undo step; the key selection is cleared. |
| Any key, segment, handle or box drag | `Escape` or right-click | Cancels and restores the keys and the selection. |

- Interpolation belongs to the segment after a key. Choosing **Bezier** shows two handles on the selected keys; a key set to Bezier for the first time gets handles at a third of the segment on the straight line, so the curve does not jump. The inspector **Key** row does the same.
- **Easy ease** makes the selected keys Bezier with flat handles at a third of the neighbouring segments, so the value slows into and out of each key.
- `Delete` removes the selected keys. When a component loses its last key, the value stays at that key's value; a lane with no keys and no generator is removed when the edit ends.
- Up and down drags of keys, segments and handles change the value at a fixed rate per pixel, however large the lane's range is: at most 1 cm for Position, 0.25° for Rotation, 0.005 for Scale and Size, and 1/200 of the full range for other values (slower when the lane shows a smaller range). `Shift` gives a tenth of that.
- Every lane gesture (a key drag, a copy, a segment move or bend, a handle drag, a generator field drag, an added or removed key, an interpolation change, a removed lane) is one undo step. A selection box is not.

### Graph Editor

**View → Graph Editor** shows the keyed curves of every selected object in one graph: each lane component (Position X, Rotation Y, Intensity…) is one curve in its axis colour or the object's colour. It edits the same keys as the lanes, so selection, `Delete`, undo and the key menu are shared.

- **Curve list** (left): one row per curve with a checkbox that shows or hides it. Click a row's name to show that curve alone; tick the others back to show them.
- **Graph** (right): time runs left to right, value bottom to top; a faint line marks 0 and a bright line the playhead.

| Control | Result |
| --- | --- |
| **Normalize** | Draws every curve scaled to its own range, so curves of very different values can be compared; drags then work in that scale. |
| **Frame** (`F` while the window is focused) | Fits the view to the selected keys, or to every shown curve with none selected. |
| **All** (`A` while the window is focused) | Fits the view to every shown curve. |
| **Auto** | Smooth tangents that never overshoot: a key that is a peak or a valley gets flat tangents, others the slope of their neighbours, clamped so the curve stays between them. |
| **Spline** | Smooth tangents with the slope of the neighbours (may overshoot). |
| **Linear** / **Step** | Straight segment / hold until the next key, after each selected key. |
| **Flat** | Horizontal tangents. |
| **Bezier** | Turns the key's segments into Bezier keeping their current shape, so the handles can be dragged in the lanes. |
| **Break** / **Unify** | Lets the two handles of a key move apart / turns the in handle back into line with the out handle. |
| **Pre** / **Post** | What a curve does before its first key / after its last key: **Constant** (holds, as before), **Linear** (continues the end slope), **Cycle** (repeats the keys), **Cycle offset** (repeats, each repeat continuing from where the last ended), **Oscillate** (repeats forward and backward). Applies to the curves of the selected keys, or to every shown curve with none selected. One undo step. |
| **Euler** | On Rotation curves, removes jumps of more than 180° between neighbouring keys by adding or removing whole turns, so rotations do not spin the long way. |
| **Buffer** | Stores a copy of the shown curves; it is drawn as a thin grey ghost curve behind them. |
| **Swap** | Exchanges the shown curves with the stored copy (one undo step), so an edit can be compared and taken back. |
| **Time** / **Value** fields | The first selected key's time (in scene seconds) and value; dragging or typing moves every selected key by the same amount. One undo step per drag. |

| Gesture | Result |
| --- | --- |
| Click a key | Selects it alone; `Ctrl` adds or removes it. Selected keys are white and show their tangent handles. |
| Drag a key (3 px) | Moves the selected keys in time and value (`Shift` for fine steps). A key never passes an unselected neighbour. One undo step. |
| Drag on empty graph | Draws a box that selects the keys inside it (`Ctrl` toggles them). A click on empty graph clears the selection. |
| Drag an edge of the box around 2+ selected keys | Left or right edge scales their times around the opposite edge; top or bottom edge scales their values around the opposite edge. One undo step. |
| Wheel | Zooms time and value around the mouse; `Ctrl` + wheel zooms only the value, `Shift` + wheel only the time. |
| Middle-drag | Pans the view. |
| `Escape` or right-click during a drag | Cancels it; the keys go back. |

- Tangent types are applied once: they set the key's Bezier handles, they do not stay live when neighbours move later.
- Tangent handles are shown in the Graph Editor but dragged in the lanes.
- Keys whose clip uses a Time scale lane do not refit the clip length when moved here.

### Snapping

With **Snap** on, a moved clip, a clip edge and the ruler playhead stick to the nearest edge within 8 px: the playhead, the start and end of every other clip, every camera clip edge, every marker and every motion-mark tick of the other actors (up to 4096 ticks). A moved clip sticks by whichever of its two edges is closer. Away from any edge they land on the grid step. `Shift` held during the drag turns both off.

### Edit menu

| Item | Key | Result |
| --- | --- | --- |
| **Undo**, **Redo** | `Ctrl+Z`, `Ctrl+Y` | See [Undo](#undo). |
| **Copy** | `Ctrl+C` | Copies the selection into the editor's clipboard (see below). |
| **Paste** | `Ctrl+V` | Pastes the clipboard at the playhead. Disabled while the clipboard is empty. |
| **Duplicate** | `Ctrl+D` | Copies the selection and pastes it right after itself, without touching what **Copy** holds. |
| **Delete** | `Delete` | See [Gestures](#gestures). |
| **Add marker** | — | Adds a marker at the playhead (see [Ruler and navigation](#ruler-and-navigation)). |
| **Insert time...** | — | Opens a dialog titled with the playhead's timecode: **Amount** (whole frames, 1 s at first, remembered until the game closes), **Insert**, **Cancel**. **Insert** makes everything at or after the playhead happen that much later (see below). |
| **Remove time...** | — | The same dialog with **Remove**: what plays from the playhead for that amount is cut out and everything after it happens that much earlier. |

What is copied follows the same order as `Delete`: the selected lane keys after a click on a key, else the focused animation block or camera clip after a click on it, else the selected objects.

| Copied | Paste puts it | Duplicate puts it |
| --- | --- | --- |
| Actors, lights, particle effects | New objects at the playhead, keeping their spacing in time, their rows, names, lanes and lengths, and their folders when pasted in the same scene while the folder still exists (else they go to the top level); each gets a new colour. A pasted copy shows under its original. | Right after the last selected clip ends. |
| Animation block | Into every selected actor, after its focused block (else the block under the playhead, else the last block). The block's pack is added to an actor that lacks it; an animation the actor's model cannot play turns red. | Right after itself, in the same actor. |
| Camera clip | Into the camera track at the playhead, keeping its length; the shakes that belong to it come along. Refused with a status line where it would lie inside another clip, overlap two others or pass 60:00:00. | Right after itself; refused the same way when the next clip is in the way. |
| The **Camera** row alone | All its clips with their shakes, keeping their spacing, pasted from the playhead (there is only one camera track). | Refused: duplicate a camera clip instead. |
| Shake clip | Into the camera at the playhead, on its sub-row or the next free one. | Right after itself. |
| Lane keys | Into every selected object of the same type, on the same lanes, starting at the playhead and keeping their spacing. Camera keys land in the block or the shake (of the same sub-row) under each key's time. | Into their own lanes, right after the last copied key. |

- The clipboard belongs to the editor (not the system clipboard). It is kept through **New**, **Open**, **Import** and level changes.
- With other objects and the Camera row both selected, only the other objects are copied, with a status line. The Camera row itself is never copied, pasted or duplicated as an object.
- Pasted key values are absolute: a Position key pasted onto another actor moves that actor to where the original was.
- Links between copied objects stay between the copies. Links to objects outside the copy are kept when pasting in the same scene and cleared in another scene or level. A link that would close a loop or make a chain deeper than 8 is dropped with a status line.
- Actors pasted near 60:00:00 start earlier so they fit. A camera that would end after 60:00:00 or hold more than 256 clips refuses the paste. Keys outside their target clip or shake are skipped and counted in the status line.
- Folders and pose keys are not copied; the members of a selected folder are, when they are selected themselves.
- Paste and Duplicate are one undo step each and work while playing.

Insert time and Remove time act at the playhead when **Insert** or **Remove** is pressed, also while playing.

| Content | Insert time | Remove time |
| --- | --- | --- |
| Markers, loop range | Those at or after the playhead move later. | Markers inside the removed range are deleted, later ones move earlier; a marker that lands within half a frame of an earlier one is deleted. Loop edges inside the range move to the playhead. |
| Clips ending at or before the playhead | Unchanged. | Unchanged. |
| Clips starting at or after the playhead | Move later. | Clips wholly inside the range, or that would keep less than 0.01 s, are deleted; clips after it move earlier. |
| Actor clip across the playhead | Grows by the amount. During the inserted time the actor holds the frame shown at the playhead: the animation block there is split and every block after the playhead starts later, so the frame there holds (in a Hold or a gap the next block simply starts later; after the last block its Hold grows). Its lanes hold their values: a Hold key at the playhead and a copy at the end of the inserted time. | Loses the part inside the range: the animation sequence and the lanes are cut there, and a key at the playhead joins the lane values on both sides. A clip that started inside the range now starts at the playhead. |
| Light or particle clip across the playhead | Grows by the amount; its lanes hold their values as an actor's do. A particle effect keeps running through the inserted time. | Loses the part inside the range; its lanes are cut as an actor's. A clip that started inside the range now starts at the playhead. |
| Camera clips | A clip across the playhead is split there (a jump cut) and its second half moves later with every clip after the playhead, so the inserted time is a gap on the camera track. Shake clips are treated like clips, also after the last camera clip: later ones move, one across the playhead grows and its lanes hold. | Clips inside the range are deleted, a clip across an edge of the range loses the part inside it, and every clip after the range moves earlier; shake clips the same way. Refused when two clips would then lie inside each other or three would overlap (a crossfade cut at both ends). |

- Actors whose **End** is Loop or Ping-pong, and actors whose model or an animation is missing, keep their sequence: only their clip and lanes change. The status line counts them and the deleted clips.
- An actor clip whose length was never set by hand fits its changed sequence.
- Links, folders and row order are not changed.
- Each is one undo step. It is refused with a status line, changing nothing, when the scene, a marker or a lane key would pass 60:00:00, when the camera would have more than 256 clips or an actor more than 256 blocks, when a lane would have more than 4096 keys, or while the previous edit is still being applied (try again a moment later).

### Undo

- Every edit is one undo step: a whole drag, a rename, a colour pick, a mute, a delete, an add, a paste, a duplicate, an offset, a marker edit, an Insert time or Remove time. A step that changes nothing is not recorded. Copying is not an undo step.
- 100 steps are kept; the 101st drops the oldest. **Edit** shows the name of the step that undo and redo would apply.
- Undo and redo are ignored while a drag is held. They clear the key selection; an edit that removes selected keys or their lane or object drops them from the selection.
- An automatic fit of a clip length belongs to the edit that caused it: undo restores the old length, redo the fitted one.
- Undo restores whole objects, so a moved or recoloured actor keeps its loaded model and never reloads it. An undone delete or add creates or removes the actor again.
- The ` *` in the title clears when undo or redo returns to the state that was last saved or opened.
- **New**, **Open**, **Import** and a level change clear the history.
- Applying a preset and adding an actor from a preset are one step each. Saving, renaming and deleting presets write the presets file at once and are not undo steps.

## Inspector

The inspector follows the selection. Several objects of one type are edited together (the header says **Actor x3**, **Light x2** and so on). Each type gets its own section, in the order actors, lights, particle effects, camera; the first section carries **Pin**. With nothing selected it shows the **Scene** settings.

- **Pin** (top right of the header) keeps the inspector on the objects shown when it was pressed, whatever is selected afterwards. It turns off by itself when every pinned object has been deleted.
- Header: the colour swatch (a picker; applies to every shown object), the name (one object only; `Enter` or a click elsewhere renames it), and the type. A name typed by hand is orange and no longer follows the model; right-click it → **Reset** to follow the model again (the camera goes back to **Camera**, a light to **Point light** or **Spot light**, a particle effect to its effect's name).
- Every row has its name on the left and its value on the right, aligned; animatable rows have a key diamond at the right edge. Values are the ones at the playhead and follow it while scrubbing and playing. In a narrow inspector a row name that does not fit ends with an ellipsis and shows in full when hovered; link warnings wrap onto more lines.
- An orange row name means the value differs from its default. Right-click the name → **Reset**, and **Remove animation** on a row that has keys or a generator (the value stays where it was at the playhead).
- With several objects of one type the rows show the first one's values. An edit writes only the component that changed (X, Y or Z) to every shown object of that type, as the same value; the other components stay as they were on each object.
- **Offset**, the first row, is always there. It moves the selected lane keys (after a click on a key) or else every selected clip by a number of whole frames, shown as a signed timecode (`+00:00:05`, `-00:01:12`). Drag it, or `Ctrl` + click to type frames; it reads `+00:00:00` again when released. Keys and clips stop at 0 and at 60:00:00, keys landing within half a frame of another key replace it, and one drag or one typed value is one undo step. It is disabled with nothing selected, and it moves the timeline selection even while the inspector is pinned.

### Actor rows

| Group | Row | Value | Default, Reset |
| --- | --- | --- | --- |
| — | **Preset** | Preset list (**Apply preset...**): a pick applies the preset to every shown actor (see [Presets](#presets)). **Save as** saves the shown actor as a preset; it is disabled unless exactly one actor with a model is shown. | Never orange, no Reset. |
| — | **Model** | Model chooser. The name follows the model unless renamed; blocks keep their animations, and the ones the new model lacks turn red. | — |
| — | **Item** | Item chooser, **None** first. | None |
| — | **Clip** | Start and length as `mm:ss:ff` (drag, or `Ctrl` + click to type frames). The start moves the whole actor, clips and lanes. With animation clips the length follows them and is grey; without clips, setting it by hand stops the automatic fit. | Orange while a length is set by hand (actors without clips); Reset fits it to the sequence again. |
| Transform | **Position** | X, Y, Z in metres. ◇ | No default, never orange. |
| Transform | **Rotation** | Heading, pitch, bank in degrees. ◇ | Orange when pitch or bank is not 0; Reset zeroes them and keeps the heading. |
| Transform | **Scale** | X, Y, Z, 0.1–5. ◇ | 1 |
| Transform | **Key** | Interpolation of every Position, Rotation and Scale key at the playhead: Linear, Ease in, Ease out, Ease in-out, Bezier, Hold. Always shown; disabled when no key is at the playhead. | — |
| Animation | **Time scale** | Speed of the whole sequence, 0–4. ◇ | 1 |
| Animation | **Add** | Animation chooser. Adds a clip at the playhead, or right after the focused row's last clip when the playhead is before its end, on every shown actor: on the focused row when nothing is there, otherwise on the first row where it overlaps nothing. | — |
| Rows | **Animation N** | Body part list, **+** (Add mode), **M** (mute) and **x** (remove; not the last row), as in the row header. One actor only. | Animation 1: whole body; new rows: every part of the model |
| Rows | **Weight** | How strongly row N plays, 0–1. ◇ | 1 |
| Rows | **+ Row** | Adds a row; disabled at 8. | — |
| Shaders | **Edit** | Which look the rows below edit, **+** (adds a look; at most 16) and **x** (removes the edited look). One actor only; the group starts closed. | Look 1 (default) |
| Shaders | **At playhead** | The look shown at the playhead; picking one keys it there on the actor's **Look** lane (see below). Disabled with one look. | Look 1 |
| Shaders | **Surface N**, **Item surface N** | Shader of the model's (or the held item's) surface N in the edited look, from the shader library (`shaders.xr`), **Original** first. Red when the name is not in the library (hand-edited file): the original shader is used then. The preview shows the original when not overridden. | Original |
| Shaders | **Texture** | Texture of that surface in the edited look, from the texture chooser, **Original** first. | Original |
| block | **Start** | Where the clip starts, in scene time. Moving it past a neighbour changes the order; a start that would put it inside a neighbour or over two others is refused with a status line. | — |
| block | **Animation** | Animation chooser. Replaces the block's animation; the range resets to the whole animation; speed, Loops, Hold and the crossfade ease stay, and the block keeps its start. | — |
| block | **Range** | In and Out in seconds of the animation, on whole frames. | Whole animation |
| block | **Speed** | −4 to 4, at least 0.05 either way; negative plays the range backwards. | 1 |
| block | **Loops** | How many times the range plays, 1–1000; a fraction stops part-way through the last loop. | 1 |
| block | **Hold** | Seconds the last frame is held. | 0 |
| block | **Fade** | **Fade in** and **Fade out** in seconds, eased with the clip's ease. | 0, 0 |
| block | **Crossfade** | The overlap with the previous block in seconds (read only; drag the block or its X on the timeline to change it), and the crossfade's ease (Linear, Ease in, Ease out, Ease in-out). | Ease in-out |

A **look** is a set of shader and texture overrides for the surfaces (meshes) of the actor's model; surfaces a look does not override keep the model's own shader and texture. **Look 1** is the default. The actor's **Look** lane picks the look over time: each key holds a look number (0 is Look 1) and switches at once (keys are made with Hold); before the first key the first key's look shows, and without keys Look 1 shows everywhere. Keying from **At playhead** on an empty lane also keys Look 1 at the clip's start, so the switch happens where the key is. The lane is an ordinary lane: its keys move, copy and delete like any other, and Insert/Remove time and trims move them. Removing a look makes its keys show Look 1 and renumbers the later looks' keys. A look applies while the actor is shown in the editor's frame; changing the model keeps the overrides by surface number, so they may land on other meshes. Up to 64 surfaces per model and 64 per held item are listed; an item whose model has no skeleton is one surface. Item overrides apply to whatever item the actor holds, by surface number.

The block group is titled with the animation name. It shows the focused block (the last one clicked on the timeline, on any layer), otherwise the main-sequence block under the playhead, otherwise the last block, and edits the first actor only. Its rows are disabled while the actor has no clips, and for an actor that is not converted yet. Editing the range, speed, Loops or Hold keeps every clip's start; an edit that would make a clip lie inside another or overlap two others is refused with a status line.

### Light rows

| Group | Row | Value | Default, Reset |
| --- | --- | --- | --- |
| — | **Type** | Point or Spot. A Spot shows **Rotation**, **Cone** and **Texture**; a Point hides them and keeps their values and lanes for when it is a Spot again. The name follows the type unless renamed. | Point |
| — | **Clip** | Start and length as `mm:ss:ff`. | — |
| Transform | **Position** | X, Y, Z in metres. ◇ | No default, never orange. |
| Transform | **Rotation** | Heading, pitch, bank in degrees; a Spot points along it. Spot only. ◇ | Orange when pitch or bank is not 0; Reset zeroes them. |
| Transform | **Key** | As the actor row, for the Position and Rotation keys. | — |
| Light | **Color** | Red, green and blue from 0 up (above 1 is brighter than white), with a colour picker, and **Pick** (see below). ◇ | 1, 1, 1 |
| Light | **Intensity** | 0–100; the drag is finer near 0. What lights the scene is Color times Intensity. ◇ | 1 |
| Light | **Range** | 0.1–1000 m. ◇ | 8 |
| Light | **Cone** | The full cone angle, 1–120°. Spot only. ◇ | 60 |
| Shadow | **Cast shadows** | The first row of the group: turns shadows on. Orange while on; right-click → **Reset** turns it off. | off |
| Shadow | **Texture** | Projector texture chooser, **None** first; applies on pick. Spot only. | None |
| Shadow | **Virtual size** | 0–100. ◇ | 0.1 |
| Volumetric | **Volumetric light** | The first row of the group: turns volumetric light on. The engine draws volumetric light only for lights that cast shadows, so turning it on also turns **Shadow** on, and turning **Shadow** off turns it off. It also needs the renderer's volumetric lights option (`r2_volumetric_lights`); without it nothing shows. | off |
| Volumetric | **Volume intensity** | 0–100. ◇ | 1 |
| Volumetric | **Volume distance** | 0.01–1. ◇ | 1 |
| Volumetric | **Volume quality** | 0.01–1. ◇ | 1 |

The **Attach** group follows Transform (see [Attach rows](#attach-rows)). **Cast shadows** and **Volumetric light** apply to every shown light, one undo step each, and are not animatable.

**Pick** is an eyedropper. Press it, then click the game view: the colour under the cursor becomes the Color of every light shown when **Pick** was pressed, written like any Color edit, in one undo step. It is the colour of the game picture you see (after tone mapping, 0–1); the editor's windows and helpers drawn over it are not part of it. The click waits for the graphics card once, so the game may hitch for a moment. The cursor is a hand while it waits; `Escape`, a right-click, **Pick** again or opening another scene cancels. While it waits, clicks in the game view do not pick objects, fly, open the viewport menu or grab the gizmo. A colour that cannot be read is reported in the status line.

### Particle rows

| Group | Row | Value | Default, Reset |
| --- | --- | --- | --- |
| — | **Effect** | Effect chooser. Replaces the effect on every shown particle effect; position, rotation, lanes, the clip and a name typed by hand stay, other names follow the new effect. While playing, the new effect starts at the playhead, rebuilt as after a seek, and playback goes on. | — |
| — | **Clip** | Start and length as `mm:ss:ff`. | — |
| Transform | **Position**, **Rotation** | As the actor rows. ◇ | as the actor rows |
| Transform | **Scale** | Size of the whole effect, 0.01–25. ◇ | 1 |
| Transform | **Key** | As the actor row, for the Position, Rotation and Scale keys. | — |
| Playback | **Speed** | How fast the effect runs, 0–4. ◇ | 1 |
| Playback | **Retrigger** | Plays a one-shot effect again each time it finishes, until the clip ends (see [Lights and particles](#lights-and-particles)). | off |

The **Attach** group follows Transform (see [Attach rows](#attach-rows)).

### Decal rows

| Group | Row | Value | Default, Reset |
| --- | --- | --- | --- |
| — | **Texture** | Texture chooser. The decal's image; the name follows it unless renamed. | None (no decal is drawn; the row names the problem) |
| — | **Shader** | Shader from the shader library; empty uses the game's wallmark shader `effects\wallmark`. Red when the name is not in the library: no decal is drawn then. | `effects\wallmark` |
| — | **Stamp** | Off: the decal shows for its whole clip and follows every edit. On: it leaves a mark once, when playback crosses the start of its clip (see [Decals](#decals)); **Clear stamps** removes the marks. | off |
| — | **Clip** | Start and length as `mm:ss:ff`. | — |
| Transform | **Position**, **Rotation** | As the actor rows. ◇ | as the actor rows |
| Transform | **Width**, **Height** | Size of the decal in metres, 0.01–20. ◇ | 0.5 |
| Transform | **Key** | As the actor row, for the Position and Rotation keys. | — |

The **Attach** group follows Transform: a decal can ride on an object or a bone, though it only lands on level geometry.

### Weather rows

A weather row changes the sky, the sun and the air while its clip plays. It is a row with one clip, like a light: move and trim the clip, mute and solo the row, put several in a folder. Outside every weather clip the game's own weather shows. A weather is a look only: the game clock, the game's own weather and what NPCs do are never changed, and the look ends with the clip.

| Group | Row | Value | Default |
| --- | --- | --- | --- |
| — | **Weather** | Which of the game's weathers supplies the sky and every value the scene does not set. **Game weather** keeps the one the game has now. | the game's weather when the row was added |
| — | **Fade in, out** | Seconds over which the weather comes in at the clip's start and goes at its end. | 0, 0 |
| Time | **Time of day** | Hours, 0–24. The sky, the sun's place and the light follow it. | the game's time when the row was added |
| Sun | **Sun heading**, **Sun height** | Where the sun is, in degrees; set them to place the sun apart from the time of day. | from the time of day |
| Sun | **Sun color**, **Sun brightness**, **Sun shafts** | The sun's colour, a multiplier on it (0–4) and the strength of the light shafts (0–1). | from the weather |
| Light | **Ambient**, **Hemisphere** | The two fill-light colours. | from the weather |
| Fog | **Fog color**, **Fog density**, **Fog distance**, **View distance** | Fog colour, strength (0–1), where it ends in metres, and how far the world is drawn. | from the weather |
| Rain and wind | **Rain**, **Wind speed**, **Wind direction** | Rain strength (0–1), wind speed and its direction in degrees. | from the weather |
| Sky | **Sky rotation** | Turns the sky, in degrees. | from the weather |

- Every value row has a tick box. Ticked, the scene sets the value; unticked, it comes from the weather and the row is grey. A value with keys counts as ticked.
- Every value is keyable and has a lane like any other (the row's **+ Lane** list offers them), so the time of day can run, the fog can roll in and the sun can swing during a shot.
- **Several weather rows at once:** where clips overlap, the lower row is on top, and its **Fade in** and **Fade out** blend its set values over the row above. The sky and the unset values come from one weather at a time: they switch to the row on top when its fade passes half way, also between the game's weather and a row. Use a fade of a second or more so the switch is hidden in the blend of colours, or put the change on a cut.
- A weather row has no place in the level, so it has no gizmo and no helper in the game view.
- Copy, paste, duplicate, undo, folders, mute and solo work as for any row.

### Post rows

A post row changes the finished picture while its clip plays: fades, colour, blur, noise and exposure. It is a row with one clip, like a weather row, and several can overlap. Outside every post clip the game's own picture shows; nothing is left behind when the clip ends.

| Group | Row | Value | Neutral |
| --- | --- | --- | --- |
| — | **Preset** | **Fade out** and **Fade in** tick **Fade** and key it across the clip (0 to 1, or 1 to 0); **Black & white** ticks **Gray** at 1. | — |
| — | **Fade in, out** | Seconds over which the whole row comes in at the clip's start and goes at its end. | 0, 0 |
| Fade | **Fade**, **Fade color** | Covers the picture with the fade colour: 0 none, 1 fully. Black by default; white or any colour works. | 0, black |
| Color | **Color base**, **Color add** | Multiplies the picture (0.5 leaves it as it is, lower darkens, higher brightens, per channel) and adds a colour on top. | 0.5, 0 |
| Color | **Gray**, **Gray tint** | How much colour is taken out (1 is black and white) and the weights of red, green and blue in the grey. | 0, 0.333 |
| Image | **Blur**, **Duality H**, **Duality V** | Blur, and a doubled picture shifted sideways and up. | 0 |
| Noise | **Noise**, **Noise grain**, **Noise rate** | Film-grain strength, grain size and how many times a second it changes. | 0, 1, 30 |
| Grade | **Exposure**, **Bloom**, **Tonemap**, **Middle gray** | Exposure bias in stops, bloom amount, how strongly the tone curve is applied and its middle grey. | the game's settings |
| Grade | **Motion blur** | Strength of the camera and object motion blur, 0 (none) to 1. It needs the game's motion blur quality (`r4_mblur_quality`) to be above Off: the quality sets how many blur steps are drawn and is not changed by the scene. | the game's `r4_mblur_power` |

- Every value row has a tick box, as on weather rows: ticked, the scene sets it; unticked, the game's own value shows. A value with keys counts as ticked. Every value is keyable and has a lane.
- Where post clips overlap, the lower row is on top; its **Fade in** and **Fade out** blend its ticked values over the rows above.
- The game's own effects (a hit, radiation, an anomaly) still add on top of a post row.
- The **Grade** values change the picture only in dynamic lighting. They never change the game's settings: the scene's values are used while the clip plays and the settings are untouched.
- **Render frames** writes the picture with the post rows applied.
- A post row has no place in the level, so it has no gizmo and no helper in the game view.

### Camera rows

| Group | Row | Value | Default, Reset |
| --- | --- | --- | --- |
| Clip N | **Start** | Where the clip starts, as `mm:ss:ff`. A start where it would lie inside another clip or overlap two goes to the nearest allowed place. | — |
| Clip N | **Length** | The clip's length as `mm:ss:ff`, kept within the same rule. | 5 s |
| Clip N | **Crossfade** | Shown only while the clip overlaps the one before it: the length of the overlap (read only) and the crossfade's ease (Linear, Ease in, Ease out, Ease in-out). | Ease in-out |
| Clip N | **Position** | X, Y, Z of the camera path at the playhead. ◇ | — |
| Clip N | **Rotation** | Heading, pitch, bank in degrees at the playhead. ◇ | — |
| Clip N | **Key from view** | Button: adds or replaces the pose key at the playhead from the current view, also while playing (playback never stops). Disabled while the shot itself is on screen. | — |
| Pose key | **Interpolation** | Interpolation of the pose key (Bezier = smooth path). | — |
| Pose key | **Tangent** | **Auto** (the handles follow the neighbouring keys), **Aligned** (the two handles stay opposite), **Broken** (each handle on its own). Switching from Auto keeps the current shape. | Auto |
| Pose key | **Reset tangents** | Button: back to Auto with 0, 0, 0. Disabled when the key already is. | — |
| Clip N | **Vertical FOV** | 1–179, or **Game** (0, the game FOV). The stored FOV and its lane. ◇ | Game |
| Clip N | **Horizontal FOV** | The same FOV across the frame: the frame-lines aspect when **View → Frame lines** is set, otherwise the game view's. For an aspect wider than the game view the frame is cut from the top and bottom, so its horizontal FOV is the game view's. Editing it changes **Vertical FOV** (kept within 1–179) the same way: on a key, offset, or keyed with Auto-key. Shows **Game** while the clip uses the game FOV; editing it then sets a FOV of its own. | — |
| Clip N | **Time scale** | Speed along the path, 0–4. ◇ | 1 |
| Clip N | **Smoothing** | Softens the path and turn over this many seconds, 0–2 (a centred blur of the final shot, shakes and crossfades included, the same whether playing or scrubbing; it never reaches past the clip's ends). ◇ | 0 |
| Clip N | **Path tension** | Scales all position handles of the clip, 0.1–3 (see [Path tangents](#path-tangents)). Not keyable. | 1 |
| Clip N | **Shortest turn** | Off: heading, pitch and bank each follow their own curve between keys. On: between two pose keys the camera makes one turn the shortest way round, eased with the earlier key's interpolation (Bezier counts as Linear), so a wide pan never wobbles or flips. The Rotation lanes still hold the keys; their curves between keys are not used while it is on. Not keyable. | off |
| Depth of field | **Mode** | **Off** (no depth of field), **Manual** (a focus distance) or **Focus on object** (the focus follows an object or one of its bones). | Off |
| Depth of field | **Lens** | **Classic**: sharp between Near range and Far range with a Blur size, the game's static depth of field. **Physical**: a real lens: how blurred things are comes from **Aperture**, **Focal length** and the focus distance, as on a camera. | Classic |
| Depth of field | **Autofocus** | Physical lens in Manual mode only: focus on whatever is at the centre of the picture instead of the **Focus** distance; the focus then eases over as the game's autofocus does. | off |
| Depth of field | **Aperture** ◇ | Physical lens: the f-number, f/0.7 to f/64. A low number gives a shallow depth of field (strong blur), a high one keeps more in focus. | f/4 |
| Depth of field | **Focal length** ◇ | Physical lens: 0 takes the focal length from the clip's field of view (a narrow FOV is a long lens and blurs more); 1 to 1000 mm sets it by hand, without changing the picture's framing. | 0 |
| Depth of field | **Target**, **Focus bone** | Focus on object only: the object and bone to keep sharp, with the same lists as Attach → Look at. A missing target or bone shows a red line under them; a missing target falls back to **Focus**. | None, Origin |
| Depth of field | **Focus** ◇ / **Focus offset** ◇ | Manual: the distance in front of the camera that is sharp, 0.05–10000 m. Focus on object: metres added to the target's distance, ±1000. | 5 m / 0 |
| Depth of field | **Near range** ◇ | How far in front of the focus the blur reaches its full strength, 0.01–1000 m. | 2 m |
| Depth of field | **Far range** ◇ | How far behind the focus the blur reaches its full strength, 0.01–10000 m. | 20 m |
| Depth of field | **Blur** ◇ | Blur strength of this clip, 0–10 (the renderer's kernel size). | 5 |
| Depth of field | **Pick focus** | Eyedropper (Manual only): click the game view and **Focus** takes the distance, along the view, to the level surface under the cursor, measured from the camera you are looking through. Written like any Focus edit (Auto-key adds a key). `Escape` or a right-click cancels it. | — |
| Shake – preset | header | A checkbox turns the shake on and off. Right-click the header → **Remove** removes the shake and its lanes. | on |
| Shake | **Preset** | One of the 9 shake presets. | the first preset |
| Shake | **Influence** | Strength, 0–4. ◇ | 1 |
| Shake | **Position scale** | Scale of the positional shake, 0–100. ◇ | 1 |
| Shake | **Speed** | Playback speed of the shake, 0–100. ◇ | 1 |
| Shake | **Time offset** | Seconds into the shake, ±3600. | 0 |
| Shake | **Stretch** | Plays the whole shake once over the clip. | off |
| Shake | **Fade in**, **Fade out** | Linear fades at the start and end as `mm:ss:ff`, at most the shake's length. | 0 |
| Shake | **Start**, **Length** | Where the shake starts on the timeline and how long it is, as `mm:ss:ff`. A value that would overlap another shake on the sub-row is refused with a status line. | — |

The **Shake** group is shown above the block group while a shake clip is focused (the last one clicked on the timeline). The block group shows the focused block, otherwise the block under the playhead, otherwise the last block. The Shake title shows the preset name after a plain hyphen.

The **Pose key** rows sit at the end of the block group under a **Pose key** line. They edit the pose key at the playhead, otherwise the key last picked in the game view or on the row's diamonds; they are disabled when there is neither. Each field drag is one undo step.

The pose rows edit the camera path of the block:

- With a pose key at the playhead (filled diamond) they edit that key.
- Off a key (hollow diamond) an edit moves the whole path of the block: every key by the same amount. With **Auto-key** on it adds a key at the playhead instead; while playing it records keys at most ten times per second, like the other rows.
- In a block without keys (empty diamond) the rows show the current view, and an edit adds the block's first key at the playhead.
- The diamond: empty or hollow adds a key at the playhead with the pose shown; filled removes the key at the playhead, except a block's last key.

### Scene rows

| Row | Value | Default |
| --- | --- | --- |
| **Frame rate** | 24, 25, 30 or 60. It changes only the timecode, the ruler and snapping; times stay in seconds. | 30 |
| **Cut distance**, **Cut angle** | How far the camera may move (0–1000 m) or turn (0–180°) in one frame before the frame counts as a camera cut; 0 switches that test off. | 5 m, 45° |
| **World follows camera** | While a shot is on screen, the game brings its NPCs and objects to life around the camera instead of around the player (see [Camera cuts](#camera-cuts)). | off |

### Keys and Auto-key

`K` switches **Auto-key** on and off, and `F` adds keys at the playhead (see [Transport](#transport)).

The diamond (◇) on an animatable row:

| Diamond | Meaning | Click |
| --- | --- | --- |
| empty (grey) | not animated | Creates the animation with one key at the playhead holding the current value. |
| hollow | animated, no key at the playhead | Adds a key at the playhead with the current value, on every component. |
| filled | a key at the playhead (within half a frame) | Removes the keys at the playhead. When no key and no generator is left, the animation is removed and the value stays where it was. |

What an edit of an animatable row writes is decided when the drag starts and kept until it is released, so a drag during playback never switches target as the playhead moves:

| Auto-key | Playback | Row | The edit |
| --- | --- | --- | --- |
| off | — | not animated | changes the value |
| off | — | key at the playhead | changes that key |
| off | — | animated, no key at the playhead | moves every key of that component by the same amount |
| on | stopped | any | adds or replaces a key at the playhead |
| on | playing | any | records: a key at most every 0.1 s at the moving playhead, replacing the keys it passes; releasing stops |

New keys ease in and out. Keys, Bezier handles and generators are edited on the lanes (see [Lanes](#lanes)). The camera clip rows **FOV**, **Time scale**, **Influence**, **Position scale** and **Speed** follow the same rules on the block's lanes; a key added to a Game FOV first sets the block's FOV to the current game FOV. **Reset** on FOV returns the block to Game.

Recording (Auto-key on, playing) is touch recording: it keeps writing while the drag is held, even when the mouse stops, at most ten keys per second, and removes the keys it passes after the first key it wrote. Only the components that were changed record. Playback never pauses. It writes only while the playhead is inside the clip; releasing stops it, and the keys after the release play as before.

### Choosers

The **Preset**, **Model**, **Item**, **Effect**, **Texture** and animation rows, the **Add** pop-up's **Actor...**, **Actor from preset...** and **Particle...** and the block menu's **Replace animation...** open a list with a filter field at the top (focused, its text selected).

- The filter is split at spaces; an entry is listed when every word appears in it, in any case. In animation lists a word may also match the animation's pack or model file. Entries where every word starts the text or follows a non-letter, non-digit character (`_`, `\`, `-`) come first, then the rest, each group in list order; a word found only in the pack counts as a weaker match.
- Each list keeps its own last filter for the session (models, items, animations, packs, presets, effects and textures separately), so reopening a list shows the last search.
- `Up` and `Down` move the highlight (the list scrolls to it) and keep the filter field focused, so typing continues; `Enter` picks the highlighted entry; `Escape` closes the list. Typing moves the highlight back to the first entry. The number of entries found is shown under the list.
- Names that do not fit end with an ellipsis.
- **Model** lists every `.ogf` under the meshes folder; the row shows the chosen model's path. **Item** lists the item sections that have a model and either weapon placement or attach-bone keys, with **None** first. Animation lists show each animation's name, then the file name of its source (the model or the pack), from the actor's model and its packs; FX animations are left out. Preset lists show the presets by name, with **(old)** after the ones that come from the old editor's file.
- **Effect** lists the game's particle effects and effect groups by name; particle animation curves are left out, and hovering never previews an effect. **Texture** lists every `.dds` under the textures folder, without the extension, with **None** first; it is built the first time it is opened, which can take a moment. Both leave out names a scene file cannot hold, textures also names longer than 239 bytes.
- A pick applies to every shown object of that type and closes the list.
- **Browse OMF...** at the bottom of an animation list lists every `.omf` in the meshes and the current level's folders. Model and pack lists leave out paths a scene file cannot hold (see the path rule under [File format](#file-format-hand-editing)). Picking one adds it as a pack to the shown actors (one undo step); its animations appear in the list a moment later. A pack made for another skeleton is converted once, when it is first used.
- Right-click an animation from a pack → **Remove pack** removes the pack from the shown actors (one undo step). Blocks that use it turn red and keep their data.
- **Favourites and folders** (model, item, animation, effect and texture lists): each row has a star at its left edge (shown on hover, gold when set); a click on the star marks or unmarks the entry as a favourite without picking it. Favourites are listed first. The shelf combo above the filter shows **All**, **Favourites** or one of your folders; the filter works inside it. **New folder...** in the combo (or in a row's right-click **Add to folder**) asks for a name and creates a folder; right-click a row → **Add to folder** ▸ a folder adds it, **Remove from folder** takes it out of the shown folder; right-click the shelf combo while a folder is shown → **Rename...** or **Delete**. Animations are remembered by pack and name. Up to 1024 favourites per list, 64 folders per list and 256 entries per folder. Favourites and folders are kept in `cinema_library.ltx` next to the editor settings and are written at once on every change; entries that no longer exist stay in the file and are simply not listed.
- **Texture preview:** resting the mouse on a texture (or highlighting it with `Up`/`Down`) loads it once and shows it centred in a fixed 128 px square under the list, with its name and size beside it; the square is always there in the texture list, so the window does not change size between textures of different shapes. Only on the D3D11 renderer.
- While playback is stopped, hovering an animation (or highlighting it with `Up`/`Down`) plays it in a loop on the actor, even outside its clip, until the list closes. While playing, hovering does nothing and a pick applies at once; playback never stops.
- The inspector's **Model** list previews the same way: while playback is stopped, resting the mouse on a model for a moment (or highlighting it with `Up`/`Down`) shows the first shown actor with that model, its blocks resolved against it, until the list closes; the actor shows even when the playhead is outside its clip or it is muted or not soloed. The highlight stays where it is while the model loads. Closing without a pick brings the actor's own model back; the preview never changes the scene or the undo history. The **Add** pop-ups' **Actor...** lists have no actor to show and do not preview. Loading a model the first time can take a moment.

## Presets

A preset is a named actor setup that can be applied to actors or used to add new ones. Presets are not tied to a level or a scene: one list serves the whole game and is kept between launches.

- A preset stores the model, the item, the scale, the **Time scale**, **End**, the packs and the animation blocks. It never stores the position, the rotation, lanes, the clip, links, the name lock or the colour.
- **Apply preset** (the inspector **Preset** row) replaces those values on every shown actor and keeps each actor's position, rotation, lanes, clip start and links. The name follows the new model unless it was typed by hand. An actor whose clip length was never set by hand fits the new sequence. A Scale or Time scale lane still overrides the new value. A preset whose model is missing is not applied; animations or packs the model lacks turn red, as after a model change.
- **Save as** (next to the **Preset** row) asks for a name, filled with the model's file name. The name turns red with "Replaces the existing preset." when a preset of that name exists; **Save** is disabled while the name is empty or longer than 127 bytes. Names may hold any characters.
- **Actor from preset...** in the **Add** menu, the timeline **Add** pop-up and the viewport menu adds a new actor from a preset (see [Add](#add)); in the viewport menu it lands at the cursor, facing the camera, and tilts with the surface while `Ctrl` is held, like **Actor...**.

The **Presets** window manages the list:

- a filter field and one row per preset, sorted by name; presets from the old editor's file show **(old)**;
- a click selects a row; a double-click renames it in place, committed with `Enter` or a click elsewhere. An empty name or the name of another preset is refused with a status line;
- right-click a row → **Rename** or **Delete...**; Delete asks "Delete preset "name"?" with **Delete** and **Cancel**, because it cannot be undone;
- `Delete` with the window focused still deletes the timeline selection, never a preset.

Files:

- The editor's presets live in `_appdata_ixray_\cinema_editor\actor_presets.ltx` (see [Presets file](#presets-file)).
- The old editor's presets in `_appdata_ixray_\cinema_editor\presets.ltx` are also read every time, converted in memory: the playlist and its trims become blocks, packs come from the `.omf` sources, muted and FX entries and shot lists are dropped with a warning. When both files hold a preset of the same name, the editor's own wins.
- The old file is never written. Renaming, deleting or saving over an old preset writes the result into `actor_presets.ltx`; a deleted or renamed old preset is hidden there by name. The old editor keeps its full list.
- Both files are read the first time presets are needed in a game session (the Presets window, the Preset list, Save as, or an Add menu). Warnings from either file are shown in one **Presets** dialog under the file path. A broken old file only warns. A broken `actor_presets.ltx` leaves the list without its presets, and saving presets is refused for the rest of the session so the file is never overwritten.
- A save that fails (read-only file, full disk, more than 512 presets) shows a dialog with the path, and the list stays as it was.

## Attachment

An object can follow another object and can turn toward another object. Every actor, light and particle effect has two independent slots, and so has every camera clip:

- **Parent**: the object rides on a bone or the origin of another object (an actor or the camera). **Lock** Roll and Pitch keep it level: the parent's bank and/or pitch are ignored, its heading and position still count.
- **Look at**: the object turns toward a bone or the origin of another object. Actors always turn on their heading only and stay upright; lights, particle effects and camera clips turn fully unless **Yaw only** is on.

Links belong to the whole clip of an object, and to one camera clip. To hand an object over to another parent, use two clips (or two camera clips).

The space of the object's own **Position** and **Rotation** (the inspector rows, the keys and the camera pose keys) depends on the slots:

| Parent | Look at | Position | Rotation |
| --- | --- | --- | --- |
| — | — | world | heading, pitch, bank in the world |
| set | — | relative to the parent | relative to the parent |
| — | set | world | offset from facing the target (0, 0, 0 = facing it) |
| set | set | relative to the parent | offset from facing the target |

The camera's shakes apply after both slots, and its FOV is never affected. A crossfade from or to an attached clip reads both clips' parents and targets at the current moment, so it has no jump.

### Attach rows

The **Attach** group follows **Transform** for actors, lights and particle effects, and the block rows for the focused camera clip.

| Row | Value | Default, Reset |
| --- | --- | --- |
| **Parent** | **None**, the Camera, then the scene's objects by name. | None |
| **Bone** | **Origin**, then the parent's bones, with a filter field. Only actor parents have bones. Hovering a bone marks it in the game view with a yellow circle (with **Helpers** on and no shot on screen). Bones whose names a scene file cannot hold are grey. | Origin |
| **Lock** | **Roll**, **Pitch**: ignore the parent's bank or pitch. Only with a parent. | off |
| **Attach** | How much the object follows its parent, 0–1; keyable (◇, lane **Attach**). 1 follows it; below 1 the parent is held where it was at the nearest moment the weight was (or will be) 1: after a drop, where it was at the release; before a pick-up, where it will be at the pick-up; with no such moment, at the clip start. So a Hold key 1 → 0 drops a carried prop exactly where it is, and 0 → 1 picks it up without a jump; eased keys blend. Only with a parent. The link line in the view is faint while below 1. | 1 |
| **Look at** | As **Parent**. | None |
| **Look bone** | As **Bone**, for the Look at target. | Origin |
| **Yaw only** | Turn on the heading only. Always on (and grey) for actors. | off |

- Choices that would make a loop (the object itself, or an object that already follows or looks at it, directly or through others) or a chain deeper than 8 links are grey. The camera counts as one object: the links of all its blocks together.
- Setting, changing or clearing a slot, a bone, a lock or Yaw only keeps the object where it was on screen: every Position and Rotation key is rewritten into the new space at its own moment, and a component without a key at one of those moments gets one there (with the interpolation of its previous key). Without keys the plain values are rewritten for the playhead. A camera clip rewrites its pose keys. Each change is one undo step.
- The rewrite is exact at the keys; between keys the motion follows the new space. A generator on Position or Rotation stays as it is (the status line warns).
- Objects that follow the changed object are not touched: they move with it.
- With several objects selected, a change applies to each one it can (a status message names the ones it cannot).

### What follows what

- A parent or target that is outside its clip, or muted, still drives the objects that follow it, from its pose at the nearest edge of its clip; it is not drawn.
- Lights and particle effects can follow a parent and turn toward a target like a camera clip (**Yaw only** is free), and they can be parents and targets themselves, by their origin (they have no bones). A particle effect that moves, with its parent or its Position lane, passes its motion to the particles it emits, so trails stream behind it.
- The Camera as a parent or target is the shot on screen inside a block with pose keys; elsewhere it is the current view (the editor camera while it is the view, otherwise the game view of the previous frame).
- Problems never stop playback. The row name and the clip label turn red and the **Attach** group says what is wrong:

| Problem | Result |
| --- | --- |
| The parent is not in the scene, or its model is missing | the actor, light or particle effect is hidden; a camera clip shows the view behind it, as a clip without keys |
| The bone is not on the parent's model | the parent's origin is used |
| The Look at target is not in the scene | Look at is ignored |
| The Look bone is not on the target's model | the target's origin is used |

- Deleting an object never changes other objects: their links to it stay and show the warnings above. Undo brings the object back and the links work again.

### Bone over time

An attached object can change the bone it hangs on during its clip, for example an item passing from the right hand to the left. Select one attached object: under the Attach rows, **Bone over time** lists the switches (time and bone) with **Go** (moves the playhead there) and **x** (removes it).

- Pick a bone of the parent, move the playhead to the moment of the hand-over and press **Switch to this bone at the playhead**. From that time on the object follows the new bone; before it, the bone of the **Bone** row or of the previous switch.
- **Keep place** (on by default) keys the object's Position and Rotation at the switch, with a held key one frame before, so the object stays exactly where it is at that moment instead of jumping to its old offset on the new bone. Untick it to keep the same offset on the new bone.
- A switch at a time that already has one replaces it. Up to 32 switches. Only the bone changes: the parent object stays the same, and camera clips have no switches.
- Saved with the scene as `attach_switches`, `attach_switch_NN` (seconds from the clip's start) and `attach_switch_NN_bone`.

### On the timeline and in the view

- An actor's clip label reads `name -> parent.bone @ target` (the parts that are set). A camera clip with links shows `-> parent.bone @ target` below its pose diamonds.
- With an object selected, a line goes from it to its attach point and another to its look-at point, in its colour.
- An attached camera clip's path, key circles and arrows are drawn through its parent and target as they are at the playhead.
- **Key from view**, **Update from view**, Pilot, the gizmo, **Move to cursor**, **Face camera** and **Snap to ground** take the pose you see and store it in the object's space at the playhead. With Look at set, the rotate gizmo edits the offset from facing the target.
- In **Local**, the gizmo of one object with a parent uses the parent's axes (scale keeps the object's own axes).
- With a parent and its child both selected, the group gizmo moves only the parent; the child follows.

## Posing

Posing mode (**View → Bones**, `B`) poses single bones of the selected actor on top of its animation, like the bone posing of Source Filmmaker or Maya. It is a mode of the editor, not saved; the poses are saved with the scene.

- **Bones in the view:** each bone of the first selected actor is drawn as a line from its parent with a dot at its origin: grey, orange when a pose row moves it, yellow under the mouse (its name shows), white when selected. A click on a bone selects it, `Ctrl` + click adds or removes it; a click beside every bone clears the bone selection (the next click beside every bone picks objects again).
- **Box select bones:** in posing mode, drag with the left button on empty game view (4 px) to draw a box; on release every bone whose joint is inside is selected, replacing the selection. With `Ctrl` the box toggles the bones it covers; with `Shift` it adds them to the selection. `Shift` + click a bone adds it (never removes); `Shift` + click on empty space keeps the selection. `Escape` or a right-click cancels the box.
- **Rig:** each actor model gets a rig built from its bone names: controls for the hips (`bip01_pelvis`), chest (`bip01_spine2`, else `spine1`/`spine`), head, hands, feet, eyes (`eye_left`, `eye_right`), jaw (`jaw_1`), the root (`bip01`, moves and turns the whole body), spine (`bip01_spine`, `bip01_spine1`), neck (`bip01_neck`), clavicles (`bip01_l_clavicle`), toes (`bip01_l_toe0`) and the base of every finger (`bip01_l_finger0` to `finger4`), each on both sides where it applies; a bone already used by an earlier control is not repeated (with no `spine2` the chest takes `spine1` and the `spine1` control is left out), chains for both arms (upper arm, forearm, hand) and legs (thigh, calf, foot), and left/right mirror pairs (`_l_`/`_r_`, `left`/`right` in the name). Missing bones are left out.
- **Rig handles:** in posing mode the controls are drawn as handles: a large square at the root, a square at the hips, circles at the chest, spine, neck, head and clavicles, diamonds at the hands, feet and toes, small circles at the eyes, jaw and finger bases; left side blue, right red, centre yellow, white when selected. Clicking within 2 px of a handle's edge selects its bone before any bone line; where handles overlap the nearest one wins. The gizmo then acts on it as on any bone.
- **Finger curl:** turning a finger handle with the gizmo gives the finger's next two joints the same turn, so the whole finger curls or spreads in one drag (one undo step); those joints join the active pose row. Moving a finger handle moves only its base.
- **Rig overrides:** the inspector **Posing** group has a **Rig (auto)** / **Rig (override)** node with a bone list for every control and the three bones of every chain. **Save rig** writes the current rig to `$game_config$\cinema\rigs\<model path with _ for \>.ltx` (sections `[rig]`, `[controls]`, `[chains]` as `a, b, c`, `[mirror]` as `left = right`); **Auto rig** deletes that file and rebuilds from the names. The file is read whenever an actor with that model is posed; names of missing bones are ignored with a log line. Rig edits are not undo steps and are not part of the scene.
- **Gizmo on a bone:** with bones selected, the gizmo acts on the first selected bone in its own axes: the rings turn it (snapped to **Rotate step**, `Ctrl` frees it), and the arrows move it (snapped to **Move step**), on every bone. The bone gizmo also has scale handles, the small boxes outside the rings (snapped to **Scale step**); the **S** button of the **Tools** bar shows the scale handles alone. They scale the bone along its own axes, and its children grow with it (their offsets and sizes), 0.01–100 per axis. A drag writes the bone's Rotation and Position of the active pose row like any inspector edit (on a key, off a key, **Auto-key**, recording) and is one undo step; `Escape` or a right-click cancels it. The bone joins the active pose row on its first edit; with no pose row, **Pose 1** is created.
- **IK** (inspector **Posing** group, **IK** node): one checkbox per rig chain (`arm_l`, `arm_r`, `leg_l`, `leg_r`; disabled when the rig lacks the chain). Turning a limb on puts its target on the hand or foot and its pole beside the elbow or knee as they are at the playhead, so nothing jumps; the limb then reaches for the target over everything else (animation and pose rows), the end bone takes the target's turn, and the elbow or knee bends towards the pole. Rows: **IK weight** (0–1, blends from the animated limb), **IK target** (metres), **IK turn** (degrees), **IK pole** (metres), all keyable. **Actor** / **World** picks the space: in **Actor** the target moves with the actor; in **World** it stays where it is in the level (a planted foot or a hand on a rail). Switching converts the values at the playhead; it is disabled while the limb has IK keys. Turning a limb off keeps its values and keys.
- **IK handles:** in posing mode every limb with IK on shows its target as a dot on a line from the hand or foot, and its pole as a small square on a line from the elbow or knee, in the side colour, like the camera path's tangent handles. Hovering one draws a yellow ring and names it; a click picks it (white, a little larger) and gives it its own gizmo, which replaces the bone and object gizmos: the pole moves, the target moves and turns (in **Local** the gizmo follows the target's axes, in **World** the world's), snapped to **Move step** / **Rotate step** (`Ctrl` frees it). Each drag is one undo step; `Escape` or a right-click mid-drag restores. A click on empty view or on a bone drops the handle. The arms and legs use the same two-bone solver as the first-person hands. A handle can also be dragged straight away: press it and move, and the target or pole follows the mouse in the plane facing the view (one undo step, `Escape` restores); after the release it stays picked with its gizmo for turning and axis moves. A handle always wins over the bone gizmo and the actor's gizmo under it: those turn grey while a handle is under the mouse, so the hand target can be grabbed with the hand bone selected. While a limb has IK on, its three bones follow the target and pole, so posing them by hand would do nothing: selecting the hand or foot of such a limb selects its target instead, and selecting the upper or lower bone selects its pole (the status line says so). Turn the limb's IK off to pose those bones directly.
- **Constraints** (inspector **Posing** group, **Constraints** node): up to 16 per actor. **+ Constraint** adds one for the single selected bone. Each has **Kind**, **Target** (any object or the camera, as in **Attach**: loops and too-deep chains are refused with a status line; the actor itself is not offered), **Target bone** (an actor target's bone, else its origin), a keyable **Weight** (0–1), **M** (mute) and **x** (remove).
  - **Parent** makes the bone follow the target as if it were the target's child; **Point** moves only its position, **Orient** only its turn; **Aim** turns the bone so that the direction it had towards the target keeps pointing at it.
  - Setting the kind or the target keeps the bone where it is (the offset to the target is taken at the playhead); **Keep pose** takes the offset again; **Snap** drops it so the bone lands on the target's origin and axes (not for **Aim**).
  - Constraints apply after the animation and pose rows and before IK, in their order; the weight blends from the pose underneath. A missing bone or target shows the bone name in red and the constraint does nothing. Presets leave constraints out (their targets belong to the scene).
- **Pose rows** (inspector **Posing** group): up to 4. Each has **Add** (turns and moves on top of the animation, scaled by the weight) or **Override** (replaces the animated bone, relative to its rest pose, by the weight), **M** (mute), **x** (removes the row and its lanes) and a keyable **Weight**. The radio button picks the active row that edits go to. **+ Pose row** adds one.
- **Bone list:** every bone of the model as a tree, with a filter field; click selects, `Ctrl` + click toggles. With one bone selected and posed on the active row, its **Rotation** (degrees), **Position** (metres) and **Scale** (factor per axis, 1 = unchanged) rows show, keyable like other values. Scale multiplies across pose rows, each blended from 1 by its row's weight, in **Add** and **Override** rows alike.
- **Key pose** (`F` with bones selected) keys the selected bones at the playhead with their current pose-row values (rotation, position and scale); **Reset** zeroes the selected bones on every pose row (scale back to 1) and removes their lanes. Both are one undo step.
- **Mirror** (Posing group, under **Key pose**): mirrors the selected bones of the active pose row (with none selected, every bone of the row) through the model's left-right plane: each left bone takes the mirrored values of its right partner and the other way round; a bone without a partner (spine, head) is mirrored onto itself, so a head turned left turns right. **Paste opposite** only copies the selected bones onto their partners, mirrored, and leaves the selected side alone. Partners come from the rig's mirror pairs; the mirroring uses each bone's rest axes, so rigs whose left and right bones are not exact mirror copies still come out right. Values are written at the playhead like any edit (keys where there are keys or with **Auto-key**). One undo step.
- **Pose library** (Posing group, closed node): **Save** stores the active row's selected bones (or all of its bones) at the playhead under the typed name (letters, digits and `_ . -` without spaces or `,;=[]#!`), replacing a pose of the same name; the list shows every saved pose with its bone count, **Apply** (writes its values to the same bones of the active row at the playhead), **Mirrored** (applies it to the partner bones, mirrored) and **x** (deletes it). Bones the model lacks are skipped with a status line. Poses are shared by every scene and model: `$app_data_root$\cinema\poses.ltx`, one section per pose, `bone = rx, ry, rz, ox, oy, oz, sx, sy, sz`; up to 256 poses of 64 bones; a line that is not 9 numbers is skipped with a log line.
- **Lanes:** every posed bone has a Rotation, a Position and a Scale lane on the actor (labelled with the bone name, in the actor's lanes), and every pose row a Weight lane, so poses are keyed, curved and edited like any other lane, also in the Graph Editor.
- **Pose rows on the timeline:** every pose row is a **Pose N** sub-row under the actor, after its Animation rows (**Pose N (add)** for an Add row), with **M** and **x** as in the inspector. It draws one clip in the actor's colour over the actor's whole clip (faint while muted), since a pose acts there with or without keys, and shows a diamond at every time one of its bones has a key, filled while selected. A click on a diamond selects all of the row's bone keys at that time (`Ctrl` toggles them); a drag retimes them together, snapped like lane keys (`Shift` frees it, `Ctrl` at the press copies, `Escape` cancels); `Delete` removes the selected ones; a right-click opens the key menu (Interpolation, Remove). A drag on empty row space draws the selection box, which does not pick up pose diamonds.
- **Pose row menu** (right-click the row's name or empty row space): **Add** / **Override**; **Key pose** keys every bone of the row at the clicked time (from the name: at the playhead) with the values it has there; **Save as animation...**; **Remove pose row**.
- **Save as animation** bakes the actor's motion into a new animation file. The dialog takes a **Name** (`a-z`, `0-9`, `_`, under 64 characters), a range (**Pose keys**: first to last key of the row; **Loop brace**; **Whole clip**: the actor's clip) and a source (**Pose only**: the pose rows over the rest pose, without the animation rows; **Final result**: the actor exactly as shown). **Pose only** bakes every unmuted pose row at its weight, not only the clicked one. The file is `$app_data_root$\cinema\motions\<name>.omf`, sampled at 30 frames per second, with one motion named like the file; it is added to the actor's animation packs in the same undo step, so the animation chooser offers it at once. Undo removes the pack from the actor, not the file.
- **Save as animation limits:** animation files hold no scale, so bone scale is left out of the saved motion; an existing name is refused (delete the file to reuse it); the range must be at least one frame and at most 10 minutes; an actor with 64 packs is refused. The motion keeps every bone and plays on the whole body. On an **Add** animation row it adds its change from its own first frame. The scene stores the pack by path, so a scene that uses a saved motion needs that file next to it on another machine.
- Several Override rows on one bone: the topmost one with weight wins. A pose on a bone whose name the model lacks does nothing.

## Viewport

The game view is the editor's viewport. Its gestures need ImGui mouse capture (`Alt`) and start only over the game view, never through a window or an open menu. The game never receives them. For flying and panning see [Editor camera](#editor-camera).

### Gizmo

The selected objects get one gizmo: an actor, a light or a particle effect at its position at the playhead, the camera at its path pose at the playhead (only inside a block with pose keys, and not while a shot is on screen).

- **TRS**, **T**, **R**, **S** on the **Tools** bar pick the universal gizmo (the default), move, rotate or scale; `Q`, `W`, `E` and `R` do the same from the keyboard (not while the right button is held, where those keys fly the camera, not while typing and not in the Graph Editor). The universal gizmo shows the move arrows, the rotation rings and the scale handles together; a drag does what its handle does and snaps with that handle's step. In it the camera and lights only move and turn, a Point light only moves, and in a group the left-out objects still follow the group's move and turn. A selected child of a selected light follows its parent when moved, and takes the turn or scale itself when its parent is left out of that (a Point light's child is turned by the rings). With **World**, an actor's scale handles in the universal gizmo scale evenly, so the arrows and rings keep the world axes; with **Local**, they scale each axis. The camera has no scale; with only the camera selected, **S** acts as move, and with actors and the camera selected, scaling leaves the camera out. A Point light has no rotation: rotating leaves it out, and with only Point lights selected **R** acts as move. Particle effects scale evenly, by whichever scale handle is dragged (with no actor selected the gizmo shows the even-scale handles). With an actor or a particle effect selected, scaling leaves lights out; with lights and no actor or particle effect, **S** shows the range handles instead (below). Selected children of a light left out this way take the gizmo themselves. The lit button shows the mode the gizmo is in for the whole selection.
- **Local** / **World** on the **Tools** bar, or `X`, picks the gizmo axes. It is remembered separately for one object (default Local) and for several objects (default World). A Point light has no rotation, so in **Local** its gizmo uses the world axes, or those of its parent or Look at target.
- Several objects share one gizmo at their mean position (turned like the first selected object in Local) and move, turn and scale together as one rigid group.
- **Snap** on the **Tools** bar switches gizmo snapping (on by default, remembered as `gizmo_snap`). Right-click it for the steps: move 0.01–10 m, rotate 0.5–180°, scale 0.01–1 and range 0.01–10 m (defaults 0.1 m, 5°, 0.1 and 0.1 m), all remembered. Holding `Ctrl` during a drag does the opposite of the button: free with **Snap** on, snapped with it off.
- **Surface snap:** hold `Shift` while dragging the move arrows or planes (**T**, or the arrows of **TRS**): the selection jumps to the level surface under the cursor (passable and actor-blocking surfaces are skipped); actors and particle effects sit on the point, lights 0.5 m off the surface, several objects keep their offsets from the gizmo, and camera clips never snap. `Ctrl`+`Shift` also turns actors and particle effects upright to the surface. With nothing under the cursor the normal axis drag continues; releasing `Shift` returns to the axis drag. It stays one undo step.
- A drag writes like an inspector edit (see [Keys and Auto-key](#keys-and-auto-key)): not animated, it changes the value; on a key, that key; off a key, every key of the component moves by the same amount (while playing, the animation keeps playing under the drag, shifted by the gizmo's move); with **Auto-key** on it adds a key, and while playing it records. On the camera the same rules apply to the pose keys of the block under the playhead. Only the components the drag changes are written: a move writes only Position, a rotation only Rotation, a scale only Scale, and a group also the positions.
- The gizmo is grey and cannot be grabbed while ImGui capture is off or a menu is open.
- Each drag is one undo step. `Escape` or a right-click during the drag restores the start pose and leaves no step; the gizmo stays grey until the left button is released.

Range handles: with **S** and only lights selected (the camera may be selected too and is left out), each selected light shows a round handle in its colour: on its range circle to the right of a Point light as seen from the camera, or at the end of a Spot light's cone. Press within 8 px of a handle and drag along that line: the Range of every selected light changes by the same amount, in steps of the **Range step** (hold `Ctrl` to drag freely). The drag writes Range like the gizmo writes its values (on a key, off a key, Auto-key, recording), is one undo step, and `Escape` or a right-click restores the ranges and leaves no step.

### Path tangents

While the Camera row is selected and helpers are on, every key of the clip under the playhead shows its in and out handles in the game view: a line from the key and a round handle (constant size on screen, outlined, drawn over the scene). The handle under the mouse (within 12 px) gets a yellow ring.

| Gesture | Result |
| --- | --- |
| Drag a handle | Moves the handle with the mouse, in the plane facing the view through where it was, and the position path bends live; the handle is white while dragged. An **Auto** key turns **Aligned** on the first drag without a jump; an **Aligned** key mirrors the other handle's direction and keeps its length. |
| `Ctrl` held when the drag starts | Makes the key **Broken**: the other handle stays where it is. |
| `Shift` while dragging | Fine steps (a tenth of the movement). |
| `Escape` or right-click during the drag | Puts the key back and leaves no step. |

- Each drag is one undo step, "Tangent". A handle is never further than 1000 m from its key.
- While a key circle is dragged, the handles on both sides of it (its own and the facing handles of the two neighbouring keys) grow and shrink with the distance to the neighbour, so a key moved close to another does not leave a loop behind. Handles you set by hand keep their direction.
- **Path tension** (camera clip row, 0.1–3, default 1) scales every handle of the clip: below 1 the path gets tighter around the keys, above 1 looser. It changes playback, the drawn path and the Position lane curves without editing any key; dragging a handle edits the key's own handle, shown scaled.
- The handles in the game view are the Bezier handles of the X, Y and Z Position keys at the tangent key's moment (only where all three have a key there); the turn is shaped on the Rotation lane.
- On an attached block the handles are drawn and dragged through the parent as it is at the playhead, and stored in the block's own space, so they follow the parent.
- The camera's Position and Rotation lanes draw the same path, so they show the effect of a handle or a tangent value too.
- Handles under the gizmo cannot be grabbed while the gizmo is hovered. There is no separate handle gizmo: a handle is dragged directly, without snapping.

### Picking

| Gesture | Result |
| --- | --- |
| Click an actor | Selects it alone. When nothing is under the cursor, the actor whose centre is within 32 px is selected. |
| `Ctrl` + click an actor | Adds or removes it. |
| Drag a camera key circle | With the Camera row selected and **Helpers** on, the keys of the focused clip (or the clip under the playhead) show as circles; hovering within 12 px draws a yellow ring. Pressing pauses playback, focuses the clip, makes the key the tangent key (see [Path tangents](#path-tangents)), leaves the shot for the editor camera and moves the playhead to the key. Dragging moves the key in the plane facing the view through its old place; with `Shift` held it lands on the level surface under the cursor instead. Only the position changes; the turn stays. One undo step; `Escape` or a right-click during the drag restores it. |
| Drag an object's path key circle | On a selected actor, light, particle effect or decal with **Helpers** on: hovering within 12 px of a key circle draws a yellow ring (the object's gizmo wins where they overlap). Pressing pauses playback and moves the playhead to the key; dragging moves the key in the plane facing the view through its old place, and the path and object follow live. One undo step; `Escape` or a right-click during the drag restores it. Components without a key at that time follow the usual on-key and **Auto-key** rules. `Shift` held: the key lands on the level surface under the cursor. |
| `Ctrl` + click on a path | With `Ctrl` held and no key circle under the mouse, a small yellow ring follows the nearest point of a drawn path (the focused camera clip's, or a selected object's) within 8 px. Clicking adds a key there with the value the path has at that moment, and the playhead goes to it. One undo step. On a camera clip the new key takes the interpolation of the key before it. |
| Click a light or a particle effect | Selects it alone, `Ctrl` adds or removes it, like an actor. It is hit within 0.3 m of its position, also while its helper is faint. Only with **Helpers** on and no shot on screen. |
| Click empty space | Clears the selection; with `Ctrl` it keeps it. |

A click is a press and release within 4 px. It never picks through a window, on a gizmo handle, during a gizmo drag or on the view cube. The nearest hit wins. Hidden and muted actors cannot be picked; lights and particle effects outside their clip or muted can.

### Viewport menu

Right-click the game view without dragging: the menu opens at the cursor. The point under the cursor is found on the level geometry (passable and actor-blocking surfaces are skipped); without a hit it is 3 m along the cursor ray.

| Item | Result |
| --- | --- |
| **Add here: Actor...** | Opens the model chooser; the picked model becomes an actor with a 5 s clip at the playhead, standing on the point, upright and facing the camera, named after the model and selected. |
| **Add here: Particle...** | Opens the effect chooser; the picked effect becomes a particle effect with a 5 s clip at the playhead on the point, upright and facing the camera, named after the effect and selected. |
| **Add here: Decal...** | Opens the texture chooser; the picked texture becomes a decal on the clicked point, its up axis along the surface's normal, so it lies on a wall or a floor. |
| **Add here: Light** | Adds a Point light at the playhead 0.5 m off the surface along its normal (on the point itself without a hit), turned along the view, with a clip to the end of the scene (5 s when that is under 1 s away). `Ctrl` does not tilt it. |
| **Add here: Camera move** | Adds a camera clip at the playhead (as **Add → Camera move**) whose key sits on the point, upright and facing the camera. |
| **Move to cursor** | Moves the selected objects to the point as a rigid group (their mean position lands on it); the camera moves its pose at the playhead. |
| **Face camera** | Turns the selected objects upright to face the view; a Spot light then shines at the camera, a Point light does not turn. |
| **Snap to ground** | Drops each selected actor, light and particle effect onto the ground below it (searched from 1.5 m above, 100 m down). |
| **Delete** | Deletes the selected objects. |
| **Frame on timeline** | Zooms and scrolls the timeline to the selected clips. |

Hold `Ctrl` when opening the menu or when picking an item: placing and **Move to cursor** then align the object's up axis to the surface normal and keep it facing the camera along the surface. Each item is one undo step. The selection items act on the selected actors, lights and particle effects and, inside a block with pose keys, on the camera pose at the playhead; a Point light only ever moves.

### View cube

While the editor camera is the view and helpers are on, a 128 px view cube sits at the top right of the game view, left of the inspector.

- Drag the cube to orbit a point 8 m in front of the camera.
- Click a face, an edge or a corner to turn smoothly to that view. Top and bottom views keep the heading and stop just short of straight down or up, so the camera never flips.

## Playback

- An actor appears in the world only while the playhead is inside its clip and the clip is not muted.
- Each block plays its range of the animation at its speed (a negative speed plays backwards) **Loops** times and then holds its last frame for **Hold**; a gap holds the frame before it. Where blocks overlap, the earlier block keeps playing (or holding) and fades into the later one with the later block's ease. Scrubbing either way and playing give the same pose at the same time.
- **End** decides what happens after the sequence: Hold keeps the last frame, Loop starts the sequence again, Ping-pong plays it backwards and forwards.
- **Time scale** slows down or speeds up the actor's sequence; 0 freezes it.
- Playback keeps running when the editor window is closed. When stopped, actors stay frozen at the playhead. While the game is paused the playhead holds and continues on unpause; scrubbing still works.
- The camera drives the view inside its blocks while playing, also with the editor window closed (see [Camera and view](#camera-and-view)).
- **Open** never changes a clip length: a saved scene keeps the lengths it was saved with. After **Import v4**, an unlocked actor clip is fitted to its sequence (two passes with Loop or Ping-pong, through Time scale); the length is kept when the model or an animation is missing. Imported clips of the old master timeline are locked, so they keep their old lengths.

The status line at the bottom shows the last result (new, opened, imported, saved) or the last error in red. The same text goes to the game log after `Cinema editor:`.

### Decals

- A decal is a wallmark on level geometry. It lies in the plane of its own X and Z axes and is projected along its down axis (its Y axis reversed) from 0.5 m above its origin to 0.5 m below; in the view it is drawn as a rectangle of its size with a short line down. Turning it about its up axis spins the image.
- Without **Stamp** the decal shows while the playhead is inside its clip and the decal is not muted; moving, turning, resizing or keying it re-lays it at once. It lands only on level geometry: surfaces the level marks as taking no wallmarks, objects and actors are skipped, and nothing is drawn when there is no geometry within reach.
- With **Stamp** it leaves a game wallmark once, each time playback (not scrubbing) crosses the start of its clip, like a bullet hole or a blood splash: on the nearest actor whose bounds the projection passes through (on the actor's skin; needs the game's dynamic wallmarks option and the actor within 50 m), otherwise on the level geometry below. Stamps fade like the game's wallmarks and are not undone; **Clear stamps** removes every static wallmark of the level, the game's own included.
- Decals are drawn by the game's wallmark system, so a level change clears them like any wallmark; the editor lays them again when they are shown.

### Lights and particles

- A light shines only while the playhead is inside its clip and the clip is not muted. Its colour, intensity, range, cone, virtual size and volumetric values follow the lanes; what lights the scene is Color times Intensity. A Spot light points along its rotation; a Point light ignores Rotation, Cone and Texture. Scrubbing either way and playing give the same light.
- The projector texture is used only by a Spot light with **Shadow**. Volumetric needs **Shadow** and the game's volumetric lights option (`r2_volumetric_lights`).
- A particle effect starts when the playhead enters its clip. After any jump into the clip (a seek, Home, a scrub, a loop range wrapping, a moved or resized clip, undo of a clip edit, a new effect) it is rebuilt: the effect is run for up to the last 10 s of its own time, so it looks as if it had been playing. While stopped it is frozen; Play continues from there. While stopped, dragging the playhead or a clip rebuilds a showing effect at most ten times a second; where the drag stops it matches a seek.
- The **Speed** lane sets how fast the effect runs; the effect's own time is the area under the lane, so seeking lands on the state that playing reaches. The transport speed multiplies it.
- When playback crosses the end of the clip, the effect stops emitting and its particles fade out; pausing freezes the fading particles. A seek, Home, **Mute** and **Delete** cut an effect at once.
- **Retrigger** plays a one-shot effect again each time it finishes, until the clip ends. Effects that loop by themselves ignore it.
- A missing or empty effect and a missing projector texture turn the row name red, and the clip turns red and shows the reason. The scene keeps playing: the light shines without the texture, the effect stays silent.
- A level change, **New** and **Open** remove every light and effect of the scene; nothing is left in the next level.

## Camera cuts

A camera cut is a frame on which the view jumps: a shot that starts where another ends without a crossfade, the start or the end of a shot, a jump of the playhead, and any frame on which the camera moves or turns more than the scene's **Cut distance** or **Cut angle** (for example across a Hold key).

- On a cut frame the renderer forgets the previous image: temporal anti-aliasing starts afresh, motion blur is skipped and the exposure takes the new brightness at once. Without this the first frames after a cut show a ghost of the old shot and the exposure creeps in.
- It works in playback with the editor closed and in **Render frames**.
- **World follows camera** (scene setting, off by default): the game keeps NPCs and objects alive only near the player, so a shot far from the player shows only what the scene itself places. With the setting on, while a shot is on screen the game keeps them alive around the camera instead. The player's own surroundings go to sleep meanwhile and wake again when the shot ends, continuing from where the game's background simulation has taken them. Things near a new far shot appear over the first moments after the cut.

## Camera and view

The **Camera** row is the scene camera: one track of camera clips. A clip with pose keys is a shot; a gap, an empty track or a clip without keys passes the view through.

Which view the screen shows:

| Situation | View |
| --- | --- |
| Playing, the playhead inside a shot | The shot. This also holds with the editor window closed, so a scene can be recorded without the editor on screen. |
| Editor open and stopped, the playhead inside a shot | The shot, so scrubbing shows the camera at every frame. |
| In a gap, outside every clip, in a clip without keys, on an empty track or with the Camera row muted | The editor camera when it is on, otherwise the player's own view. |
| Editor closed and stopped | The player's own view. |

- Touching clips cut hard (a jump cut). Overlapping clips crossfade position, direction and FOV over the overlap with the later clip's ease, both shots moving.
- **FOV** 0 means the game FOV; a block with a FOV switches to it for the shot.
- Shake clips play on top of the shot after any crossfade, with their fades and their own lanes (see [Camera shakes](#camera-shakes)).
- The camera and the actors are evaluated from the same playhead at the same moment of the frame, so they never drift apart. While the game is paused the shot still follows scrubbing.

Navigating with the editor camera leaves the shot: the view stays on the editor camera, also inside shots, until **Play**, a click on the **Camera** row (its name or its clip) or closing the editor brings the shot back. Closing the editor after a fly during playback therefore always gives the shot back for recording.

### Editor camera

The editor camera is a free camera, separate from the player. It needs ImGui mouse capture (`Alt`), and its gestures start only over the game view, not over a window. The game never receives these keys and buttons.

| Gesture | Result |
| --- | --- |
| Hold the right button and drag (4 px) | Fly: the mouse looks around without stopping at the screen edge; the cursor is hidden and stays where the button was pressed. Turning follows the field of view (slower when zoomed in), the game's invert-mouse setting and **View → Look sensitivity** (0.1–4, default 1, remembered). The first two frames of a fly ignore mouse motion, so it starts without a jump. Movement, turning, roll and FOV glide with **View → Fly smoothing** (0–0.5 s, default 0.08; 0 stops exactly), like `demo_record`; the camera settles after you let go. |
| `W` / `S`, `A` / `D` while flying | Forward and back, left and right, along the level view (the roll does not tilt them). |
| `Space` / `C` while flying | Up and down. |
| `Q` / `E` while flying | Rolls (banks) the camera: `Q` lowers its left side, `E` its right side, 45° per second; it wraps at ±180°. |
| `R` / `T` while flying | Narrows / widens the field of view, 30° per second, within 5–150°. The first press starts from the game FOV. |
| `Shift` while flying | Four times faster (also the roll and the FOV). |
| Wheel while the right button is held | Fly speed, ×1.25 per notch, 0.25–64 m/s (default 4). Remembered. |
| Hold the middle button and drag | Pans sideways and up and down. |
| Wheel | Moves forward and back along the view. |
| Right-click without a drag | Opens the viewport menu (see [Viewport](#viewport)). |

- The first time on a level the editor camera starts from the current view; after that it stays where it was left, also after closing and reopening the editor. Started while a shot is on screen, it starts from the shot, with the shot's roll and field of view.
- Mouse look turns around the world's vertical and tilts around the camera's level right axis; the roll is applied on top, so looking around never tilts the horizon. The editor camera's roll and FOV are kept with its position; they reset when the editor camera restarts from the current view (another level).
- **View → Game camera** returns to the player's view outside shots. Closing the editor does the same; the editor camera's position is kept for next time.
- The editor camera's roll goes into the pose key's bank. **Pilot** also keys the clip's **FOV** at the playhead when the fly changed the field of view; **Key from view** and `F` with the editor camera on screen key it when the editor camera's FOV differs from the clip's FOV there. A clip on the game FOV first takes the current game FOV.
- **Pilot**: with **Auto-key** on and the **Camera** row selected, releasing the right button after a fly adds or replaces the pose key at the playhead in the clip under it, from the editor camera; one undo step. **While playback runs**, Pilot records instead: every frame of the fly is captured, and on release (or when playback stops) the path is smoothed over time by **View → Record smoothing** (0–0.5 s, default 0.1), thinned to the keys needed to stay within 1 cm and 0.25° of it, and written as smooth (Bezier) pose keys, replacing the clip's keys inside the recorded span; FOV keys are written only when the FOV changed during the recording. In a gap the recording becomes a new clip covering it (up to the next clip); a recording that crosses clips writes into each. One undo step; `Escape` during the fly throws the recording away. At most 10 minutes at 60 frames per second are captured. In a gap or on an empty track it adds a new clip there instead (see [Camera clips](#camera-clips)). With Auto-key off, or with the Camera row not selected, flying never writes a key.

### View menu

| Item | Result |
| --- | --- |
| **Game camera** | The player's view shows outside shots (ticked while the editor camera is off). |
| **Editor camera** | The editor camera shows outside shots, and the current shot is left until Play or a click on the Camera row. |
| `V` | Flips between the shot and the editor camera: from the shot to the editor camera where it was left, and back. |
| **Reset FOV** | The editor camera uses the game FOV again. Disabled while it already does. |
| **Reset roll** | Levels the editor camera. Disabled while it is level. |
| **Helpers** | Shows or hides the world helpers: the camera path, light and particle markers, decal outlines, link lines, and the path of each selected object with Position keys (its whole clip in its colour, a circle and a facing arrow at every key, filled at the playhead; no arrows when attached). Remembered. The focused camera clip's path and each selected object's path also show a small dot per frame (fewer on long clips), so the spacing shows the speed, and a white dot where the playhead is. |
| **DOF preview** | While the editor camera is the view, the depth of field of the clip under the playhead is applied to it, its object focus measured from the editor camera. Off: the game's own depth of field. Remembered. |
| **Inspector** | Shows or hides the inspector. Remembered. |
| **Presets** | Shows or hides the Presets window. Remembered. |
| **Shots & Markers** | Shows or hides the Shots & Markers window. Remembered. |
| **Graph Editor** | Shows or hides the Graph Editor. Remembered. |
| **Onion skin** | Draws the selected actors' skeletons (up to four actors) as they are **Onion step** frames apart, **Onion count** times before (blue) and after (orange) the playhead, fainter the further away. Lines only, no meshes; recalculated when the playhead or the scene changes and while you edit. Remembered. |
| **Onion count**, **Onion step** | Ghosts on each side (1–5) and frames between them (1–30). Remembered. |
| **Motion paths** | Adds a dot per sample (one per frame, fewer when zoomed far out) along the path of each selected actor, light, particle effect and decal over the visible part of the timeline, to read its speed. Remembered. The path itself and its keys show with **Helpers**. |
| **Bones** (`B`) | Posing mode: shows the bones of the selected actor in the game view and the **Posing** group in the inspector (see [Posing](#posing)). Not remembered. |
| **Frame lines** | Camera frame lines for an aspect ratio (2.39:1, 21:9, 2:1, 1.85:1, 16:9, 4:3, 1:1, 9:16), centred in the game view, with the area outside darkened by **Frame mask** (0 clear, 1 black). Shown while a shot or the editor camera is the view, never on the plain game view. They stay on screen, with **Safe areas** and **Thirds**, while the ImGui tools are hidden and the Cinema Editor is still open, so a shot can be judged without the editor windows. Remembered. |
| **Safe areas** (`G`) | Action safe (90 %) and title safe (80 %) frames inside the frame lines (or the whole view when they are off), and a centre cross. Remembered. |
| **Thirds** | Rule-of-thirds lines inside the frame. Remembered. |

World helpers are drawn over the game view in each object's colour, lighter while its row is selected: a cross at each shown actor's pivot with its name; for a Point light a small circle and a circle of its range facing the camera, for a Spot light the outline of its cone (its Range and Cone at the playhead), for a particle effect a circle with a cross and its name, all faint while the playhead is outside the clip or the clip is muted; and the camera path of every shot (curved along Bezier keys, straight otherwise). With the Camera row selected, the keys of the block under the playhead also show as 0.12 m circles with a 0.6 m line in their view direction. Helpers are hidden while a shot is on screen.

### Other camera users

- **Cinema Film Maker Editor**: while the old editor's window is open, this editor never takes the view; the frame that window opens, the view is handed over to it. Closing the old window gives the view back.
- **`demo_record` / `demo_play`**: while one of them holds the view, the transport row shows **Camera busy** and neither the shots nor the editor camera show. This editor never takes the view from them.

## Settings

The editor's own settings live in `_appdata_ixray_\cinema_editor\editor.ltx`, section `[editor]`, separate from the old editor's `settings.ltx`. They are written when a value changes (not during a drag or a fly), through a temporary file like a scene.

| Key | Value | Default |
| --- | --- | --- |
| `snap` | on / off | on |
| `snap_step` | snap step: 0 = 1 f, 1 = 2 f, 2 = 5 f, 3 = 10 f, 4 = ½ s, 5 = 1 s, 6 = 5 s | 0 |
| `fly_speed` | editor camera speed, 0.25–64 m/s | 4 |
| `move_step` | gizmo move step, 0.01–10 m | 0.1 |
| `rotate_step` | gizmo rotate step, 0.5–180° | 5 |
| `scale_step` | gizmo scale step, 0.01–1 | 0.1 |
| `range_step` | light range handle step, 0.01–10 m | 0.1 |
| `header_width` | width of the timeline's header column, 140–600 px | 220 |
| `opacity` | background opacity of the editor windows, 0.1–1 | 0.5 |
| `row_height` | default height of object rows and shake sub-rows, 1–6 rows | 1 |
| `lane_height` | default height of lanes, 1–8 rows | 1 |
| `look_sensitivity` | editor camera mouse-look speed, 0.1–4 | 1 |
| `follow` | the transport **Follow** toggle | 1 |
| `fly_smoothing` | editor camera fly inertia, 0–0.5 s | 0.08 |
| `record_smoothing` | Pilot recording smoothing, 0–0.5 s | 0.1 |
| `render_warmup` | Render frames: frames thrown away after each camera cut, 0–30 | 8 |
| `helpers` | on / off | on |
| `hints` | on / off | on |
| `gizmo_snap` | on / off | on |
| `onion`, `paths` | on / off | off |
| `frame_aspect` | frame lines: 0 off, 1 2.39:1, 2 21:9, 3 2:1, 4 1.85:1, 5 16:9, 6 4:3, 7 1:1, 8 9:16 (the older `frame_ratio` key is no longer read) | 0 |
| `frame_mask` | darkening outside the frame lines, 0–1 | 0.5 |
| `safe_areas`, `thirds` | on / off | off |
| `onion_count` | 1–5 | 2 |
| `onion_step` | frames, 1–30 | 5 |
| `dof_preview` | on / off: **View → DOF preview** | on |
| `inspector` | on / off | on |
| `presets` | on / off: the Presets window | off |

A value that cannot be read keeps its default and logs a warning; a number out of range is clamped. A file that breaks the scene file rules (see [File format](#file-format-hand-editing)) or is larger than 64 KiB is ignored with a log line.

## File menu

| Item | Result |
| --- | --- |
| **New** | Starts an empty, untitled scene. |
| **Open...** | Lists the scenes of this level, plus **unsaved_&lt;level&gt; (recovered)** when there is unsaved work from an earlier session (see below). **Open** or a double-click loads one. |
| **Import v4...** | Lists the old editor's scenes of this level. **Import** converts one into a new, untitled scene. |
| **Save** (`Ctrl+S`) | Writes the open scene to its file without a dialog. An untitled scene asks for a name first. |
| **Save as...** | Asks for a name and writes the scene under it. |
| **Render frames...** | Renders the scene to numbered images (see [Render frames](#render-frames)). Disabled without a camera track. |

With unsaved changes (` *` in the title), **New**, **Open** and **Import** first ask **Discard** or **Cancel**. An imported scene and a recovered one count as unsaved until they are saved.

The **Edit** menu is described under [Edit menu](#edit-menu); the **Add** menu has **Actor...**, **Camera move** and **Folder** (see [Editing](#editing)); the **View** menu is described under [Camera and view](#camera-and-view).

### Render frames

The dialog picks the **range** (**Loop brace**, available while a loop brace is set, or **Whole scene**), the **Frame rate** (the scene's, 24, 25, 30 or 60) and the **Aspect** (**Window** or 2.39:1, 21:9, 2:1, 1.85:1, 16:9, 4:3, 1:1, 9:16; it starts at the **Frame lines** choice). **Render** plays the range through the scene camera one frame at a time and writes `_appdata_ixray_\cinema_renders\<scene>\frame_000000.tga`, `frame_000001.tga`, … (an untitled scene uses `untitled`; existing files are overwritten). **Name** (letters, digits, `_` and `-`; remembered until the game closes) replaces `frame` in the image names and the scene name in the video name, so several takes of one scene can sit in the same folder; left empty, the names are as described here. With **Output** set to **AVI video (MJPEG)** it writes one video, `_appdata_ixray_\cinema_renders\<scene>\<scene>.avi`, instead: Motion JPEG frames at the chosen frame rate, no sound, playable in common players and editors. Its width and height are rounded down to even numbers. A video stops growing at 1 GB and continues in `<scene>_2.avi`, `<scene>_3.avi`, …; the status line counts the extra parts. **Cut warm-up** (0–30 frames, default 8, remembered) is how many frames are rendered and thrown away after every camera cut before the frame is written, so textures, exposure and anti-aliasing have settled in the image; scene time stands still meanwhile, and the progress window says **warming up**.

- Every frame advances the game by exactly one frame time, whatever the real frame rate, so actors, particles, physics and shader time land on the same frames on every render. Real time is not used while rendering; sound plays out of step.
- The images hold the game view only: no editor windows, other ImGui tools, frame lines, helpers or gizmos.
- An aspect other than **Window** cuts the centred frame out of the game view at its pixel size: 21:9 in a 1920×1080 window gives 1920×823 images (top and bottom cut, the horizontal FOV kept); a narrower aspect cuts the sides and keeps the vertical FOV. **Horizontal FOV** in the inspector uses the same rule.
- A progress window shows the frame count; **Cancel** or `Escape` stops after the current frame. Rendering also stops when a frame cannot be written, the game is paused, the level unloads or the editor closes. The status line names the folder and the number of frames written.

### Unsaved work

When the level changes, a save is loaded or the game ends while the scene has unsaved changes, the editor first writes it to `_appdata_ixray_\cinema_editor\unsaved_<level>.ltx` and names the file in the log. **File → Open** on that level lists it as **unsaved_&lt;level&gt; (recovered)**; it opens as an untitled scene, so save it under a name to keep it. The next unsaved change on the same level overwrites the file.

## Scene files

Scenes live in `gamedata\configs\cinema_scenes\<level>\<name>.ltx`, so a mod can ship them. A level can have any number of scenes; File → Open lists only the current level's folder.

### Saving

- The name is typed in UTF-8 and converted to the Windows-1251 file name encoding of the engine. It is stored in lowercase.
- `\ / : * ? " < > |` and control characters are removed, leading spaces and trailing spaces or dots are trimmed.
- After conversion the name must be 1–64 bytes. A name with characters that cannot be converted (emoji, CJK) shows **The name has characters that cannot be used in a file name.** in red, and **Save** stays disabled. So does a name longer than 64 bytes.
- **Save as** starts with the open scene's name. When the name belongs to another scene of this level, the field turns red and says **Replaces the existing scene.**; **Save** then overwrites that scene.
- Saving writes `<name>.ltx.tmp` first, checks that the whole file reached the disk, and only then replaces `<name>.ltx`. If the target is read-only or locked, a dialog names both files; the previous scene is unchanged and the new data stays in the `.tmp` file.
- A saved scene shows in File → Open at once, and the title shows its name.
- A scene is refused, with a dialog, when it holds more than the format allows (see the limits below), when a model, item, pack or animation name has unsupported characters, or when an object, folder or marker name is longer than 127 bytes. Nothing is written then.

Open → Save writes a byte-identical file.

### Opening

- The scene replaces the current one. A scene saved on another level opens with a warning that positions may not match.
- Problems found while reading are listed in a **Warnings** window, at most 64 distinct lines plus "N more". Bad values are clamped or skipped; reading never stops the game.
- A file is refused, with a dialog naming the file and the line, when it is not a scene, comes from a newer editor, is larger than 16 MiB, or breaks the file rules below. The current scene is then kept.

### Importing old scenes

**Import v4** reads the scenes of the [Cinema Film Maker Editor](cinema-film-maker-editor.md) from `_appdata_ixray_\cinema_scenes\<level>\`. The old file is never written; the result is a new, untitled scene. Save it to keep it.

| Old data | New scene |
| --- | --- |
| Actor on the old master timeline | One row per master clip, with its start, duration, mute (the clip's, its track's or the parent track's), colour and label. The playlist and its trims become animation blocks; a trim blend between segments becomes a crossfade overlap. |
| Non-pattern master clip | One row with one animation block for the clip's source range and speed. |
| Actor not on the master timeline | One row from 0 s, fitted to its sequence after the import (5 s when the model or an animation is missing). |
| Camera clips | The **Camera** track. Each old clip becomes a camera clip at its old time; the time between them is a gap. Overlapping clips keep the old priority and the later conflicting one is dropped. Muted clips and clips without Preview become normal clips. |
| Camera offset curves, smooth paths, shakes | Offsets are baked into the pose keys, smooth cameras get smooth keys. Each old shake layer becomes a shake clip as long as its camera clip, on sub-row 1, 2 and so on (up to 8 per camera clip; more are dropped and counted). Constant fades become the clip's **Fade in** and **Fade out**; fade curves and enable curves become Influence keys. Animated stretch, smooth, preset or offset is reduced to its value at the clip start. |
| Shot lists, saved NPC poses, master transforms, track groups, solo | Dropped. |
| Light or particle on the old master timeline | One row per master clip, with its start, duration, colour and label; muted when the clip or its track is muted, and a light also when it was switched off (**Enabled**). Without a label the row is named "Point light", "Spot light" or after the effect. |
| Light or particle modulation | Becomes absolute lanes: offsets are added to the rest position, multipliers multiply the base value (Range in metres, Color as an HDR colour, Scale and Speed of a particle), and so do their waves and Min/Max. Exact, except rotation offsets of a light or particle whose rest pitch or bank is not 0: those are sampled at the keys. Values past the new ranges (for example a Range above 1000 m) are clamped, and the Min/Max of a wave set to None no longer limits the value; both are counted. |
| Light clip intensity and colour ramps | A constant ramp multiplies Intensity or the colour; a changing one becomes two Intensity or Color keys at the clip start and end. Intensity above 100 moves the excess into Color, so the light is as bright as before. A colour ramp over a colour channel that already had keys or a wave is applied at its keys only. |
| Light with its own clock (not on the master timeline) | One row from 0 s to the end of the imported scene (at least its modulation duration and 5 s). A looping modulation is repeated as keys over the whole row (up to 4096 keys per value); without a loop the values hold at the end of the modulation. Waves now run on: they no longer restart at each loop or stop at the end. |
| Particle with its own timeline | One row from its IN to its OUT, the modulation shifted so it plays as before. Its **Loop** becomes **Retrigger**; the repeat of the whole timeline at OUT is dropped. |
| Particle on a master clip with OUT shorter than the clip | The values hold from OUT, as before; waves keep moving after OUT. |
| Old particles without curves | Their start-to-end modulation becomes two keys per value. |
| Pattern lights and particles without a master clip | Dropped. |
| Light HUD mode, occlusion queries, indirect lighting, light and particle groups | Dropped. |

Every drop or approximation is counted in the Warnings window. Imported clips keep their old lengths.

### Older actors

Scenes of version 11 and older, presets of version 4 and older and imported old scenes store an actor's animation as a sequence: each block placed after the end of the previous one, inside the actor's clip, with **End** repeating it. The editor converts such an actor to animation rows as soon as its model and every animation of its blocks have loaded, once and without an undo step; the scene is saved in the new form the next time it is saved. The actor plays the same before and after:

- every block becomes a clip at the time it used to play;
- a gap before the first block becomes a pose clip of that block's first frame, so the actor still appears at its clip start;
- with Hold, the last clip holds until the old clip end; a clip that ran past the old clip end is shortened (its Hold first, then its loops, then its range), and clips that started after it are dropped;
- Loop and Ping-pong are repeated as copies of the clips until the old clip end; Ping-pong repeats play backwards, a Hold becomes a pose clip before the backward clip, and the crossfade eases are mirrored;
- the main sequence becomes **Animation 1** and layer k becomes **Animation k+1**, with its parts, mute and weight;
- a clip that would lie inside another or overlap two others on its row (this can happen where Ping-pong turns inside a crossfade) moves to the next row with the same parts and mode, a new one when needed, with a **Fade in** as long as the overlap, so it plays the same;
- lanes, the Time scale and the actor's start do not change.

The log names an actor whose conversion is not exact: repeats cut at 256 clips per row; layers that used to switch off between repeats and now hold their last frame there; an actor with an empty main row and layers, which now shows only while its other rows play; and an overlap left on a row because all 8 rows were taken, which may play differently.

Until it is converted, an actor plays and is saved exactly as before (`relative = 1`), and its clips cannot be edited: moving, trimming, adding, pasting, splitting, crossfading and removing clips say so in the status line, and the block rows of the inspector are grey. Its lanes, model, packs, mute, solo, name, colour, row and attachment can be edited; fixing the model or the packs lets it convert. Effect names are not checked on import: an effect that is not in the game keeps its row.

## File format (hand editing)

A scene is an ordinary `.ltx` file. The editor writes only a strict subset and reads only that subset.

File rules (a break refuses the file with "Line N: ..." or "Section [x] appears twice"):

- every line is shorter than 4000 bytes and has no control characters except tab;
- blank lines and `;` comments are allowed;
- a section is `[name]` with lowercase Latin letters, digits and `_`, at most 63 characters, once per file, without inheritance (`[a]:b`) or a leading `!`;
- every other line is `key = value` with the same key alphabet, so `#include` and other directives are refused;
- quotes only in pairs, and never on a line with `;` or `//`;
- nothing before the first section.

Values:

- Several numbers on one key are separated by commas. Numbers are written with up to 9 significant digits. NaN, infinity or a value out of range is replaced or clamped with a warning.
- Booleans are written as `on` / `off`; `true`, `false`, `yes`, `no`, `1` and `0` are also read, in any case. Anything else keeps the default with a warning.
- Names (`name`, `folder_NNN_name`, `marker_NNN_name`) are lowercase hex of the UTF-8 bytes, at most 127 bytes.
- Paths and identifiers (`visual`, `item`, `pack_NN`, `block_NNN_source`, `block_NNN_name`, `effect`, `texture`) are under 256 bytes, without spaces, `,` `;` `"` `|` `=` `[` `]` `#` `!` or `//`. Others are ignored with a warning.
- Indices count from 0: objects and blocks `NNN`, packs, shakes and lanes `NN`, keys `NNNN`.

`[scene]`

| Key | Value | Rule |
| --- | --- | --- |
| `version` | `19` | 1–4 is read as an old scene and imported; 5–11 are read and converted (see below and [Older actors](#older-actors)); 12 is read as is, with depth of field Off; 13–15 are read as is except that camera pose keys are converted to Position and Rotation curves (see below); 16 is read as is (no pose rows); 17 is read as is (no attach weight); 18 is read as is (no looks); above 19 is refused |
| `level` | level name | a different level gives a warning |
| `frame_rate` | 24, 25, 30 or 60 | otherwise 30 |
| `cut_jump` | **Cut distance** in metres and **Cut angle** in degrees | written only when not `5, 45` |
| `world_follows_camera` | **World follows camera** | `on`; written only when on |
| `next_id` | next object or folder id | raised above the largest object or folder id |
| `next_color` | palette position of the next new object | any |
| `folders`, `folder_NNN`, `folder_NNN_name` | count; `id, row`; hex name | up to 256; a zero, repeated or too large id skips the folder |
| `markers`, `marker_NNN`, `marker_NNN_name` | count; time in seconds; hex name | written only when the scene has markers; up to 256; time 0–3600, a missing or non-numeric time skips the marker; no name means an empty name. Markers are sorted by time on load, and one within half a frame of an earlier one is dropped with a warning |
| `objects` | count | up to 1024, sections `[object_NNN]` |

`[object_NNN]`, every object

| Key | Value | Rule |
| --- | --- | --- |
| `id` | number | zero or repeated gets a new id |
| `type` | `actor`, `camera`, `light`, `particle`, `decal`, `weather` or `post` | anything else, or a second camera, skips the object |
| `weather`, `weather_mask`, `weather_fade`, `set_<value>` | a weather row: the weather's name (absent for the game's weather), which values the scene sets (one bit per value, in the order of the inspector rows), fade in and out in seconds, and one line per value (`set_weather_time`, `set_sun_color` and so on) | values out of range are replaced by the defaults |
| `name`, `name_locked` | hex name; bool | an invalid hex name is dropped with a warning |
| `color` | `r, g, b` | each 0–255, otherwise a palette colour |
| `row`, `folder`, `muted` | display order; folder id; bool | an unknown folder moves the object to the root |
| `clip_start`, `clip_length`, `clip_locked` | seconds; seconds; bool | 0–3600; 0.01–3600 |
| `lanes`, `lane_NN_*` | automation lanes | see Lanes |
| `row_height` | the row's height in rows | 1–6; written only when set; anything else is ignored with a warning |
| `sub_N_height` | the height of sub-row N (shake row N + 1, or actor layer N + 1) | 1–6; N 0–7 |
| `lane_<param>_<layer>_height`, `lane_<param>_<layer>_<c>_height` | the height of a lane; the `<c>` form is from older builds, read and kept but no longer used | 1–8; `<param>` as `lane_NN_param`, `<layer>` the shake row for shake values, the layer number for `layer_weight`, otherwise 0; the camera's Position and Rotation lanes use `position` and `rotation` |

Actor keys

| Key | Value | Rule |
| --- | --- | --- |
| `visual` | model path | optional |
| `item` | item section | optional |
| `position`, `rotation`, `scale` | `x, y, z` | ±100000 m; ±3600° heading, pitch, bank; 0.1–5 |
| `time_scale` | rate | 0–4 |
| `relative` | `1` | written only for an actor that is not converted yet (see [Older actors](#older-actors)); its blocks then hold offsets, as in version 11 |
| `end` | `hold`, `loop` or `pingpong` | read and written only with `relative = 1`; otherwise `hold` |
| `packs`, `pack_NN` | extra `.omf` files | up to 64; repeats and non-`.omf` paths are skipped |
| `rows` | animation rows | 1–8; missing is 1 |
| `row_NN_parts`, `row_NN_part_N` | the row's body part names | up to 8 names, as identifiers; repeats are skipped; none on row 00 means the whole body; written only when there are any |
| `row_NN_mode` | `1` | **Add**; written only then; missing or 0 is **Override**; a fifth Add row is read as Override with a warning |
| `row_NN_muted` | 1 | written only when muted |
| `row_NN_weight` | the static weight | 0–1; written only when not 1 |
| `pose_rows` | pose rows | 0–4; written only when there are any |
| `pose_NN` | `mode, muted, weight` | mode 1 **Add**, 0 **Override**; muted 0/1; weight 0–1 |
| `pose_NN_bones`, `pose_NN_bone_NN` | the bones the row poses, by name | up to 64 per row; a name the model lacks is kept and does nothing |
| `pose_NN_bone_NN_rest` | `rx, ry, rz, ox, oy, oz` | the bone's static rotation (degrees, ±3600) and offset (metres, ±10); written only when not zero |
| `pose_NN_bone_NN_scale` | `sx, sy, sz` | the bone's static scale, 0.01–100; written only when not 1 |
| `looks` | looks | 0–16; written only when there are any |
| `look_NN_surfaces`, `look_NN_MM` | the surfaces look NN overrides, by surface number from 0; 64–127 are the held item's surfaces | up to 128 per look; a surface named twice keeps the first |
| `look_NN_MM_shader`, `look_NN_MM_texture` | the override | each written only when set; a surface with neither is skipped |
| `lane_NN_param` `look` | Look lane | layer 0; a key's value is the look number from 0 |
| `ik_NN_bones` | `root, middle, end` | the limb's three bones (NN 00–03: arm_l, arm_r, leg_l, leg_r); written once the limb was turned on; a limb without three names is ignored with a warning |
| `ik_NN` | `on, space, weight` | on 0/1; space 0 **Actor**, 1 **World**; weight 0–1 |
| `ik_NN_goal` | `tx, ty, tz, th, tp, tb, px, py, pz` | target position (metres), target turn (degrees, ±3600) and pole position (metres), in the limb's space |
| `lane_NN_param` `ik_target`, `ik_turn`, `ik_pole`, `ik_weight` | IK lanes | `lane_NN_layer` is the limb, 0–3 |
| `constraints` | constraints | 0–16; written only when there are any |
| `constraint_NN_bone` | the constrained bone | a name the model lacks is kept and does nothing |
| `constraint_NN_target` | `object id, bone` | as an attach link; missing means no target |
| `constraint_NN` | `kind, muted, weight, sx, sy, sz, th, tp, tb` | kind 0 Point, 1 Orient, 2 Parent, 3 Aim; the offset to the target (metres, degrees); for **Aim** `sx, sy, sz` is the aim direction in the bone's axes |
| `lane_NN_param` `constraint_weight` | constraint weight lanes | `lane_NN_layer` is the constraint, 0–15 |
| `lane_NN_param` `pose_weight`, `bone_rotation`, `bone_position`, `bone_scale` | pose lanes | `lane_NN_layer` is the pose row for `pose_weight`, and row × 64 + bone for the bone lanes; a lane pointing at a missing row or bone is dropped with a warning |
| `row_NN_blocks`, `row_NN_block_NNN_source`, `row_NN_block_NNN_name` | the row's clips | up to 256 per row; a clip without a source or name is skipped |
| `row_NN_block_NNN_time` | `in, out, speed, hold, start, ease, loops, fade_in, fade_out` | in 0–3600 s; out 0 (the animation end) or not before in (out equal to in, with a hold, is a pose clip); speed 0.05–4 either sign, negative plays backwards; hold 0–3600 s; start 0–3600 s after the actor's `clip_start` (clips are sorted by start on load, a negative start is moved to 0 with a warning); ease 0–3; loops 1–1000; fades 0–3600 s. With `relative = 1` the start field is an offset (±3600 s) from the end of the previous clip |
| `blocks`, `block_NNN_*`, `layers`, `layer_NN_*` | the version 5–11 sequence | read only for those versions: the blocks are the main sequence (field 5 an offset from the end of the previous block; versions 5–7 have `in, out, speed, hold, blend_in, blend_ease`, a blend becoming an overlap), `layer_NN_parts`, `layer_NN_part_N`, `layer_NN_muted`, `layer_NN_weight` and `layer_NN_block_*` the layers |

Camera keys

| Key | Value | Rule |
| --- | --- | --- |
| `blocks` | camera clips, sorted by start | up to 256 |
| `block_NNN` | `length, ease, fov, time_scale, start` | 0.01–3600 s; the crossfade ease when this clip overlaps the one before it, 0–3; 0 (game FOV) or 1–179; 0–4; start 0–3600 s in scene time. Clips are sorted by start; a clip inside another or overlapping two others is dropped with a warning. |
| `block_NNN_keys`, `block_NNN_key_NNNN` | read only from version 15 and older: pose keys `time, x, y, z, heading, pitch, bank, mode`, converted on load to keys on the clip's Position and Rotation lanes at the same moments (Bezier keys get handles that follow the old smooth path; the clip keeps its Time scale; a clip with a smooth turn or edited tangents is converted approximately and counted in one load note) | up to 1024; times 0–3600 s and increasing, otherwise the key is dropped; angles ±3600°; mode 0–5; a block may have no keys |
| `block_NNN_pose` | `x, y, z, heading, pitch, bank`: the clip's position and rotation where an axis has no keys | written only when not all 0 |
| `block_NNN_lens` | the clip's lens: `physical (0/1), aperture, focal_length, autofocus (0/1)` | written only when not the defaults `0, 4, 0, 0` |
| `block_NNN_dof` | the clip's depth of field: `mode, focus, focus_offset, near_range, far_range, blur` | written only when not the defaults `0, 5, 0, 2, 20, 5`; mode 0 Off, 1 Manual, 2 Focus on object; focus 0.05–10000, focus_offset ±1000, near_range 0.01–1000, far_range 0.01–10000, blur 0–10; out of range is clamped |
| `block_NNN_smooth` | the clip's path smoothing in seconds | 0–2; written only when not 0 (version 14) |
| `block_NNN_tension` | the clip's **Path tension** | 0.1–3; written only when not 1 |
| `block_NNN_short_turn` | the clip's **Shortest turn** | `on`; written only when on |
| `block_NNN_name` | the clip's name, hex text like the other names | written only when set (version 14) |
| `block_NNN_dof_target` | `id, bone`: the focus target for Focus on object | written only when set; an unreadable value drops the target with a warning |
| `block_NNN_tangent_NNNN` | read only from version 15 and older, folded into the converted handles: the tangent of pose key `NNNN`: `mode, tension, continuity, bias, in_x, in_y, in_z, out_x, out_y, out_z` | written only when not Auto with 0, 0, 0; mode 0 Auto, 1 Aligned, 2 Broken; tension, continuity, bias −1–1; handles ±1000 m from the key, in the block's own space |
| `block_NNN_lanes`, `block_NNN_lane_NN_*` | block lanes (`fov`, `time_scale`) | as Lanes |
| `block_NNN_parent`, `block_NNN_look_at` | the block's links | as Links |
| `shakes` | shake clips | up to 256; written only when there are shakes |
| `shake_NNN` | `start, length, fade_in, fade_out, row, ease` | start 0–3600 s in scene time (before version 11, from the camera clip start); length 0.01–3600 s; fades 0–3600 s; row 0–7; ease of the crossfade into this shake: 0 Linear, 1 Ease in, 2 Ease out, 3 Ease in-out (versions 6–9 have no ease and read as 3) |
| `shake_NNN_setup` | `preset, enabled, influence, position_scale, speed, offset, stretch` | preset 0–8; enabled 0/1; 0–4; 0–100; 0–100; ±3600 s; stretch 0/1 |
| `shake_NNN_lanes`, `shake_NNN_lane_NN_*` | the shake's lanes (`shake_influence`, `shake_position_scale`, `shake_speed`) | as Lanes; `lane_NN_layer` must be 0 |
| `shake_row_NN_mode` | `1` | sub-row `NN` (00–07) is **Override**; written only then; missing or 0 is **Add** |

Shakes that would lie inside another, hold another inside them, or overlap two others on their row are moved to the next free row, with a warning; when every row is taken they stay where they are. Two shakes overlapping partly stay on their row as a crossfade.

Versions 5–10 store the camera as blocks packed back to back from `clip_start`, with `block_NNN = length, blend_in, blend_ease, fov, time_scale`. They are converted on load: every block gets its start in scene time and shake starts move to scene time; a block without pose keys is dropped and its time becomes a gap (a warning counts them); a **Blend in** of b seconds between two blocks with keys becomes a crossfade by making the earlier block b seconds longer, which looks the same because that block holds its last pose there (a warning counts the blends where it can differ: pose or FOV keys past the earlier block's end, a FOV generator, or shakes, which now fade with their block); a Blend in after a block without keys did nothing and is dropped. When the camera used to end in a block without keys, the camera track keeps that end so the scene ends where it did. Version 5 files are also read with their old camera keys: `block_NNN_shakes` and `block_NNN_shake_NN` (`preset, enabled, influence, position_scale, speed, offset, stretch`, up to 32 per block) and shake lanes in the block with `lane_NN_layer` as the shake index. Each becomes a shake clip that starts and ends with its block, on the row of its index; shakes past the 8th of a block are dropped with a warning. Saving writes version 12; an older build refuses that file. Row and lane heights need no version: an older build ignores them.

Light keys

| Key | Value | Rule |
| --- | --- | --- |
| `kind` | `point` or `spot` | otherwise `point` with a warning |
| `position`, `rotation` | `x, y, z`; heading, pitch, bank | ±100000 m; ±3600° |
| `light` | `r, g, b, intensity, range, cone, virtual_size` | colour 0–100000 each (HDR, the light is colour × intensity); intensity 0–100; range 0.1–1000 m; cone 1–120°; virtual size 0–100 |
| `shadow`, `volumetric` | bool | default `off` |
| `volume` | `intensity, distance, quality` | 0–100; 0.01–1; 0.01–1 |
| `texture` | projector texture, under `$game_textures$`, without `.dds` | written only when set; at most 239 bytes and the path rule; used only by a Spot light with a shadow |

The light colour is the `light` key: `color` is the row colour.

Particle keys

| Key | Value | Rule |
| --- | --- | --- |
| `effect` | particle effect or group name | written only when set; the path rule; a name the game does not know is kept |
| `position`, `rotation` | as lights | |
| `particle` | `scale, speed` | 0.01–25; 0–4 |
| `retrigger` | bool | default `off`; replays a one-shot effect while its clip lasts |

Decals (`type = decal`)

| Key | Value | Rule |
| --- | --- | --- |
| `texture` | texture path | written only when set; the path rule |
| `shader` | shader name | written only when set; empty uses `effects\wallmark` |
| `position`, `rotation` | as lights | |
| `decal` | `width, height` | 0.01–20 each, default 0.5 |
| `stamp` | bool | default `off` |
| `lane_NN_param` `decal_width`, `decal_height` | size lanes | layer 0 |

Links (actors, lights and particles: `attach_parent`, `attach_look_at`; camera clips: `block_NNN_parent`, `block_NNN_look_at`)

| Key | Value | Rule |
| --- | --- | --- |
| `attach_parent` | `id, bone, roll_lock, pitch_lock` | written only with a parent |
| `attach_weight`, `block_NNN_weight` | static Attach weight, 0–1 | written only when not 1 |
| `attach_look_at` | `id, bone, yaw_only` | written only with a target; actors always turn on the heading only, lights and particles when `yaw_only` is 1 |

- `id` is the `id` of another object (the camera included). An id that is not in the file loads with a warning and is kept; the link then shows the warnings of [What follows what](#what-follows-what).
- `bone` may be empty (`12,,1,0`: the origin). Otherwise it follows the path rule above, so it has no comma; bone names are as in the model.
- The flags are `1` or `0` (`on`, `off`, `true`, `false`, `yes`, `no` are also read).
- A link with the wrong number of fields, an id of 0 or a broken value is dropped with a warning; the other values stay.
- Links are added in file order (objects in order; each actor's parent, then its look at; the camera's blocks in order). A link to the object itself, a link that would close a loop and a link that would make a chain deeper than 8 are dropped with a warning that names both objects.

Lanes

| Key | Value | Rule |
| --- | --- | --- |
| `lanes` | count | up to 16 (up to 98 on a version 5 camera block) |
| `lane_NN_param` | parameter | every placed object and camera clip: `attach_weight`; actor: `position`, `rotation`, `scale`, `time_scale`, `layer_weight`, `look`; light: `position`, `rotation`, `color`, `intensity`, `range`, `cone`, `virtual_size`, `volume_intensity`, `volume_distance`, `volume_quality`; particle: `position`, `rotation`, `size`, `speed`; camera clip: `fov`, `time_scale`, `dof_focus`, `dof_offset`, `dof_near`, `dof_far`, `dof_blur`, `camera_smooth`; shake clip: `shake_influence`, `shake_position_scale`, `shake_speed`; unknown, repeated or not of this owner skips the lane |
| `lane_NN_layer` | 0 | not written, except `layer_weight`, where it is the layer number 1–7 and must name an existing layer; in a version 5 camera block the shake index of a shake parameter |
| `lane_NN_cC` | key count of component C | up to 4096; C is 0–2 for x, y, z (r, g, b for `color`), 0 for single values |
| `lane_NN_cC_KKKK` | `time, value, mode, in_time, in_value, out_time, out_value[, broken]` | time 0–3600 s and increasing, otherwise dropped; value clamped to the parameter range; mode 0–5; in_time −3600–0; out_time 0–3600; handle values ±100000; `broken` = 1 is written only for a key whose tangent handles move independently (version 14) |
| `lane_NN_cC_gen` | `type, amplitude, frequency, phase, min, max` | type 0–10; all values within ±100000; min ≤ max and overlapping the parameter range; otherwise dropped |
| `lane_NN_infinity` | `pre, post` | 0 Constant, 1 Linear, 2 Cycle, 3 Cycle with offset, 4 Oscillate, whole numbers; applies to every component of the lane; written only when not both 0; anything else is ignored |

Parameter ranges: position ±100000, rotation ±3600, scale 0.1–5, time_scale 0–4, fov 1–179, color 0–100000, intensity 0–100, range 0.1–1000, cone 1–120, virtual_size 0–100, volume_intensity 0–100, volume_distance 0.01–1, volume_quality 0.01–1, size 0.01–25, speed 0–4, layer_weight 0–1, dof_focus 0.05–10000, dof_offset ±1000, dof_near 0.01–1000, dof_far 0.01–10000, dof_blur 0–10, shake_influence 0–4, shake_position_scale 0–100, shake_speed 0–100. A FOV between 0 and 1 is read as 1. A lane with no keys and no generator is not saved.

Modes: 0 linear, 1 ease in, 2 ease out, 3 ease in-out, 4 Bezier (on a pose key: a smooth path), 5 hold. Blend eases use 0–3. Generator types: 0 none, 1 sine, 2 triangle, 3 square, 4 sawtooth, 5 stepped noise, 6 smooth noise, 7 fractal noise, 8 Perlin noise, 9 continuous noise, 10 oscillator.

### Presets file

`actor_presets.ltx` follows the same file rules and value rules as a scene, and is limited to 16 MiB.

`[preset]`

| Key | Value | Rule |
| --- | --- | --- |
| `version` | `6` | missing or 1 is read as an old presets file; 2 is read with the old block line, converted as in scenes; 3 and 4 are read with block offsets and converted when applied (see [Older actors](#older-actors)); 5 is read as is (no looks); 6 is read as is; above 6 is refused |
| `presets` | count | up to 512, sections `[preset_NNN]` |
| `hidden`, `hidden_NNN` | count; hex name | old presets hidden by name; up to 512; written only when there are any |

`[preset_NNN]`: `name` (hex name, at most 127 bytes), then the actor keys `visual`, `item`, `scale`, `time_scale`, `relative`, `end`, `packs`, `pack_NN`, `rows`, `row_NN_*`, `looks` and `look_NN_*` with the same rules as in a scene (versions 2–4: `blocks`, `block_NNN_*`, `layers` and `layer_NN_*`). Clip starts count from the preset's first clip, which lands where the preset is applied. Weight lanes are not part of a preset: applying one keeps the actor's weight lanes for the layers it has and drops the rest. `position` and `rotation` are neither written nor read. A preset without a name or model, or with a name already used in the file, is skipped with a warning. Presets are written sorted by name, so saving an unchanged list writes the same bytes.

The old `presets.ltx` (`[preset] version = 1`, sections `[preset_NNN]` with a plain-text `name`, `visual`, `item`, `scale`, `loop`, `speed`, `pingpong`, `playlist_NNN`, `trim_NNN`) is read with the scene file rules too; up to 512 presets are taken, and a preset without a name or model, or with a repeated name, is skipped.

## Limitations

- Post rows cover fades, colour, blur, duality, noise, exposure, bloom and tonemap. Vignette, chromatic aberration, a separate saturation control, motion blur quality, sharpening and the game's `.ppe` effect files cannot be placed on the timeline yet.
- Sky and cloud pictures, the lightning and lens-flare sets of a weather cannot be blended; they switch at the middle of a fade. Two different weathers therefore show a jump in the sky half way through a fade, softened only by the colours blending.
- Rain drops, clouds and gusts run on real time, so two renders of the same frames differ in them. The sun's direction does nothing in static lighting. Level weather zones still apply on top of a weather row.
- The game's one-shot weather effects (surge and the like) cannot be placed on the timeline yet, and there is no sun handle in the game view.
- Depth of field needs the renderer's depth of field. When a clip's depth of field first shows, the editor switches the renderer's depth of field on by itself, without changing `r2_dof_enable` (the shaders are rebuilt at the start of the next frame, a short hitch), and switches it back when the editor closes, the level ends, or the shot ends with the editor closed. While it is switched on, the game's own depth of field shows outside shots too. When the shot hands the view back, the blur strength blends back with the focus values; a clip with depth of field **Off** next to one with depth of field blends against the game's own values and blur. In static lighting the renderer has no depth of field and nothing is switched. The sky uses `r2_dof_sky`, the HUD is never blurred, and a see-through surface takes the depth of what is behind it. **Pick focus** only hits level geometry.
- The camera gizmo is hidden while a shot is on screen; switch to the editor camera to move a shot with the gizmo.
- The camera's Position and Rotation lanes edit pose keys one at a time: their keys are not box-selected, copied or moved together, and a Rotation curve near a pitch of ±90° can jump where heading and bank swap.
- The loop range is not saved with the scene and is not an undo step (Insert time and Remove time still move it with the rest).
- The view cube sits left of the inspector; with the inspector moved to the left of the screen it sits at the left edge.
- Clicking the game view while a menu is open only closes the menu; click again to pick, grab the gizmo or open the viewport menu.
- **Split at playhead** re-eases an eased or Bezier segment through the cut on each half; elsewhere the shot continues unchanged.
- Shakes play only while a shot is on screen; a shake in a gap or over a clip without pose keys does nothing there.
- A version 5 scene whose block has both **Blend in** and shakes plays slightly differently inside the blend: the previous block's shakes, frozen at its end, no longer mix into the crossfade; only the shakes under the playhead shake the blended shot. The load warnings count these blocks.
- At most 8 shake rows; a version 5 block with more than 8 shakes loses the rest.
- Stretching a block stretches the shakes that start in it, but not their lane keys.
- Trimming a camera clip's left edge keeps the shot in place exactly only while its **Time scale** has no generator; extending it earlier holds the first pose at the clip's static Time scale.
- A Bezier segment cut by a left trim restarts from an **Auto** key at the new start, so its shape near the cut can change slightly.
- Insert time or Remove time with the playhead inside a camera crossfade is refused when it would leave two clips starting together.
- Lanes added from **+ Lane** without a key or a generator, the axis chips and the generator axis toggles are forgotten when the row is collapsed. A `Ctrl` handle drag marks the key's tangents broken, which is saved; **Broken tangents** in the key menu turns it back off.
- Animation block edges and crossfades work only on the first pass of a looping sequence.
- Body parts come from the model; layers keep part names the model lacks but do nothing on them. A layer can only cover whole body parts, not single bones.
- At most 7 layers per actor. Layers are evaluated in order, so a layer can only cover the ones below it.
- Removing a layer or applying a preset with fewer layers drops the weight lanes of the missing layers.
- An old **Blend in** (version 5–7 scenes, presets and imports) longer than the block before or after it becomes a full overlap of that length instead of the shorter crossfade it played, so that crossfade and the sequence end can shift slightly.
- **Auto-key** is not remembered between sessions.
- A block whose animation cannot be found has no length on the timeline.
- A cut from a shot with its own FOV to a block with the game FOV eases into the game FOV over a few frames (the game's own FOV smoothing).
- Row drags move one row, not the whole selection.
- A clip dragged onto other clips overwrites them without asking; `Escape` during the drag, or Undo after it, brings them back. Values typed in the inspector never overwrite: there an overlap is still refused.
- Overwriting the middle of an animation clip that loops can leave two clips for the part after the cut: the rest of the cut loop, then the whole loops.
- The red limit tick shows on animation clips and shakes; a camera clip's edge just stops.
- The gizmo buttons on the **Tools** bar show the mode for the object gizmo; while a bone, an IK handle or a tangent handle has the gizmo, they keep the last object mode.
- Unsaved work is kept in one file per level; there are no rotating autosaves and no History window yet.
- A dialog that quotes a file path with non-Latin characters may show them garbled.
- After a long-distance cut in live playback, textures and detail objects near the new place can still load for a moment; only **Render frames** waits for them (**Cut warm-up**). The game world (level particles, wind) keeps running during warm-up frames; the scene does not.
- **World follows camera** costs game time at each far cut and changes what the game simulates while the scene plays; leave it off for scenes near the player.
- A link holds for a whole actor clip or camera clip; to switch parents over time, use two clips or two blocks.
- A rebuilt particle effect is run for at most the last 10 s of its own time: an effect whose parts start later than that shows its state after 10 s of play. A seek in a scene with many effects can hitch once.
- A fading particle tail is cut by any jump, also by **Stop**, which returns the playhead to where playback started.
- An effect with its own time limit keeps counting that limit across a rebuild: rebuilt or restarted before the limit ran out, it can stop emitting early, so a seek into such a one-shot can differ from playing to it. It counts afresh once the limit has run out (Retrigger always starts it whole).
- The renderer applies range changes smaller than a tenth of the range in steps, so a slowly animated Range moves in small jumps.
- A shadowed Point light costs six shadow maps.
- **Pick** reads the colour on screen after tone mapping, 8 bits per channel, so it never picks a colour brighter than white.
- Imported rotation keys of tilted lights and particles and colour ramps over animated colour channels are approximate; the Warnings window counts them.
- Look at turns the whole actor on its heading; heads and eyes do not aim.
- After a link change the motion is exact only at the keys; between keys it follows the new space.
- An attached camera clip's path in the view is drawn with its parent and target as they are at the playhead, not as they move along the block.
- The Camera as a parent or target outside its shots follows the game view one frame late.
- In scenes with hundreds of objects, the **Parent** and **Look at** lists can slow the editor while they are open.
- Pasted keys are absolute: a Position key moves another actor to the original's place, and under a different parent to a different spot.
- `Ctrl+D` never duplicates the camera object, only its blocks.
- A marker is kept in seconds; after a frame-rate change, two markers that fall within half a frame of each other are reduced to one the next time the scene is opened.
- Motion-mark ticks show where a mark starts, not how long it lasts.
- The presets files are read once per game session: presets the old editor saves while the game runs appear after a restart.
- An old preset cannot be changed in its own file: renaming or deleting it hides it in `actor_presets.ltx`, so the old editor still lists it.
- Insert time and Remove time leave the sequence of looping actors alone, so after the playhead their animation runs ahead or behind by the amount.
- After Insert time or Remove time, an eased or Bezier lane segment across the playhead eases again on each side. After Remove time, a lane segment that ran into the removed range now runs to the value after it, so the values just before the playhead change too. Shakes, lane generators and particle effects keep running through inserted time, a shake with **Stretch** on the lengthened camera clip stretches over the longer block, and an actor whose **Time scale** lane has a generator holds only approximately.
- A camera cut within 0.01 s of a block edge is moved to that edge, so the camera can shift by less than a frame against the actors.
- Tangent handles are kept in metres: stretching a block keeps the handles' length, so a handle can reach past a neighbouring key and make the path loop (dragging a key circle in the view rescales them; moving a key with the gizmo or the inspector does not). **Reset tangents** brings it back to Auto.
- On a **Shortest turn** clip the gizmo between two keys still starts from the lane values, not from the turn on screen, so a key added there with the gizmo can show a small jump in the turn.
- `Ctrl` + click on a path adds a key with the path's value there; on an eased or Bezier stretch the shape on either side of the new key can change slightly.
- Aiming a camera clip at a moving point is done with **Look at** and an object; there is no aim point of its own and no keyable aim weight yet. Objects cannot follow their path's direction automatically.
- Row and lane heights belong to the object: a pasted or duplicated object starts with default heights, and heights are not part of undo.
- An animation clip cannot start before its actor's start; drag the actor's band to move the whole actor earlier.
- Add rows apply after every Override row, whatever their order, and at most 4 rows can add; the third and fourth add in parent-bone space. An Add clip changes the pose relative to the animation's frame 0, not its trimmed start, so a trimmed or reversed Add clip starts with an offset, and a normal animation on an Add row adds its whole movement.
- Rows above Animation 1 are off before their first clip and after their last.
- Insert time inside an actor whose Time scale changes over time moves its later clips by the inserted time measured at the playhead's rate, so they can land slightly off.
- An actor whose model or an animation is missing stays in its old form and its clips cannot be edited until it loads (see [Older actors](#older-actors)).

## Post-build checks

1. **Game → Editors → Cinema Editor** is the first item of the menu. With no level loaded, the window says **Load a level**.
2. On a level: the timeline opens as a bottom strip and the inspector on the right, both at 50 % background in the engine theme; the title is **Untitled**. Closing the timeline closes the editor; reopening shows the same scene.
3. **Import v4** lists the old scenes of this level. Importing one shows the Warnings window (dropped shots, poses, transforms, light and particle groups) and a status line. There is one row per actor clip of the old master timeline, one row per actor that was not on it, and one **Camera** row. A scene with a quoted actor name also imports.
4. **Save as** writes `gamedata\configs\cinema_scenes\<level>\<lowercase name>.ltx`; the title shows the name and **File → Open** lists it at once. **Save** on an untitled scene opens **Save as**; on a named scene it writes without a dialog.
5. In **Save as**: the name of another scene turns the field red; emoji or CJK show "cannot be used" and disable **Save**; a name over 64 bytes shows "longer than 64 bytes"; `\/:*?"<>|` are stripped; a Cyrillic name saves and lists back in Cyrillic.
6. Open → Save, and Import → Save → Open → Save, both give byte-identical files (`fc /b`).
7. Make the scene file read-only (or hold it open with an exclusive lock) and Save: the dialog names the file and the `.tmp`, the original is unchanged and the `.tmp` holds the new data.
8. Each of these hand edits is refused with "Line N: ..." or "appears twice", never with a Debug assertion, and the current scene stays: a 5000-character line; an odd quote; a quote together with `;`; `#include "x.ltx"`; `[object_000]:base`; a second `[object_000]`; `![scene]`; a key before the first section.
9. Each of these loads with warnings and no Debug assertion: NaN, inf or 1e40 values; `objects = 99999`; `muted = disabled`; `color = 300,0,0`; zero or repeated ids; `frame_rate = 17`; an unknown object type; a second camera; a lane with an unknown parameter; a generator with min above max.
10. **New** clears the rows and the title. A level change clears the scene and the title.
11. Old Cinema Film Maker Editor: the shake Preset combo lists the 9 presets, shakes look as in the previous build, and an old scene with shakes loads without an "invalid preset" warning.
12. Build Debug with unity build on and off.
13. Add a weather clip and confirm the inspector lists the available weather presets.

Playback and transport:

13. Import an old scene with actors: each actor appears only while the playhead is inside its clip and matches the old editor at hard cuts between animations.
14. Two overlapping blocks crossfade the same way when scrubbing forward, scrubbing backward and playing; both animations move during the crossfade.
15. **Hold** freezes the last frame; Loop and Ping-pong wrap; a static Time scale slows the actor and moves the drawn block edges.
16. Close the editor window while playing: playback keeps running. Stop: the actors stay frozen at the playhead. Pause the game while playing: the playhead holds and continues on unpause; scrubbing the paused game updates the pose.
17. A pack made for another skeleton is converted with one log line, not one per frame.
18. Change the model of an actor in the scene file to a missing one: its row is red and the actor is hidden; Save writes the same model path and clip length.
19. With many actors loaded, repeat Open, New and Import several times: no Debug assertion. A level change or a save load clears the scene, and File → Open and Save as then use the new level's folder.
20. `Space`, `Home`, `End`, `Left` and `Right` work after a click in the game view and with the timeline focused, never with another tool's window focused or while typing, and the game never reacts to them. **Stop** returns to where playback started.
21. The timecode follows the scene frame rate; ruler scrubbing lands on whole frames (free with `Shift`); a right-drag sets a loop range that playback repeats, and a right-click clears it; the zoom field, `Ctrl` + wheel and **Fit** work; the speed combo changes the playback rate.
22. Build Debug with unity build on and off.

Editing, undo and saving:

23. Each gesture is one undo step and redoes: a clip move, a `Ctrl` + drag copy, a clip resize, a row move into and out of a folder, a rename, a colour pick (one step for the whole picker drag), a mute, a delete, Add → Folder and Add → Camera move. **Edit** shows the step names.
24. `Escape` and a right-click during a clip move, copy or resize restore the clips; a cancelled copy leaves no copies and no undo step.
25. Make 101 edits: the first one can no longer be undone, the other 100 can.
26. Undo a clip move of an actor: no hitch and no new log line (the model is not reloaded).
27. Save, edit (the title shows ` *`), undo back to the saved state: the ` *` clears; redo: it returns. `Ctrl+S` on a named scene saves without a dialog.
28. With unsaved changes, **New**, **Open** and **Import** ask **Discard** / **Cancel**; Cancel keeps the scene.
29. Click, `Ctrl` + click and `Shift` + click select as described; the selected clips have a bright outline. Snap on: clips stick to the playhead and other clip edges within 8 px, else to the step; `Shift` frees them; the snap step combo changes the grid.
30. `Ctrl+Z`, `Ctrl+Y`, `Ctrl+Shift+Z`, `Ctrl+S`, `Delete` and `F2` work with the timeline focused and after a click in the game view, do nothing with another tool's window focused or while typing, and the game never reacts to them.
31. Resize an imported actor clip: the length stays where it was dropped; **Reset length** fits it to its sequence again, and one undo restores the dropped length.
32. Edit a scene, then change the level: `_appdata_ixray_\cinema_editor\unsaved_<old level>.ltx` is written and logged. Return to that level: **File → Open** lists **unsaved_&lt;level&gt; (recovered)**, which opens untitled with ` *`. Ending the game with unsaved changes writes the file too.
33. Delete a folder: its members move to the top-level rows; undo restores the folder and the members inside it.
34. Build Debug with unity build on and off.

Actor authoring:

35. Right-click empty track space → **Add** → **Actor...** → filter and pick a model: an actor appears at the clicked time, 3 m ahead and on the ground, upright and facing the camera, named after the model and selected. **Add** → **Actor...** in the menu bar does the same at the playhead. One undo removes it.
36. Add two or three animations with the inspector **Add** row. Drag a block later: it follows the mouse, snapped, and the other blocks stay; drag it past its neighbour: they swap places. `Ctrl` + drag copies it. Each is one undo step; `Escape` or a right-click during the drag leaves the blocks unchanged.
37. Drag a block's left and right edges: the range trims on whole frames (free with `Shift`) and the other blocks stay where they are. Drag the right edge past the end of the animation: the block loops in whole loops (marks at the top show each repeat; any length with **Snap** off), and dragging back removes loops first. `Shift` + right edge: the extra time becomes hatched Hold.
38. Right-click a block: **Split at playhead** cuts it at the frame shown, also in its third loop (disabled when the playhead is outside the block, in its Hold or in a gap), **Replace animation...** swaps the animation and keeps speed, Loops, Hold and its start, **Remove** deletes it and the other blocks stay in place. Click a block and press `Delete`: only that block goes and a neighbouring block is focused; once the last block is gone, `Delete` deletes the selected objects again.
39. On an actor clip never resized by hand, every block edit, a model change, a pack change, an **End** change and a **Time scale** edit fit the clip to the sequence (two passes with Loop or Ping-pong, so the repeat shows; switching **End** to Loop doubles a fitted clip). Resize the clip by hand: edits no longer change its length. **Reset length** fits it again. Open a saved scene: no length changes. Remove a pack that blocks use: the length stays.
40. With playback stopped, hover animations in the **Add** list: each plays in a loop on the actor, also with the playhead outside its clip; closing the list returns to the sequence. Start playback and hover: nothing plays; pick one: it is added and playback keeps running.
41. **Browse OMF...** lists `.omf` files; picking one adds the pack and its animations appear in the list. A pack made for another skeleton logs one conversion line. Right-click one of its animations → **Remove pack**: the blocks using it turn red and keep their names; undo brings them back. After a level change the list shows the new level's packs.
42. Fill an actor clip with blocks. Drag its top band: the clip moves, snapped; `Ctrl` + drag on the band copies it; the right end of the band resizes it. Below the band the same presses still move, trim, loop, crossfade and copy blocks; there is no Blend-in corner any more (crossfades are the X where blocks overlap).
43. On an actor clip never resized by hand, trim a block so the clip refits, then undo once: the block and the old clip length come back together; redo gives both again. **Reset length** on a clip that was saved unlocked at another length is one undo step.
44. Build Debug with unity build on and off.

Inspector:

45. Rows are aligned in two columns with the diamonds on the right. A non-default value turns the row name orange: Rotation with pitch or bank, Scale ≠ 1, Time scale ≠ 1, End ≠ Hold, an Item, a hand-set clip length, a renamed actor, a block's range, speed, Loops, Hold or crossfade ease. Right-click → **Reset** restores the default: Rotation keeps its heading, the clip length fits again, the name follows the model again.
46. Select three actors: the header says **Actor x3**. Drag only the Position Y field: every actor gets that Y and keeps its own X and Z. One undo restores all three.
47. Click an empty Position diamond: it fills and a key exists at the playhead. Move the playhead: the diamond turns hollow and the values follow the animation while scrubbing and playing. Click the hollow diamond: a second key. Click a filled diamond: the key goes; removing the last key removes the animation and the actor stays where it was.
48. With Auto-key off, drag Position X off a key on an animated actor: the whole animation shifts by the same amount. On a key: only that key changes. Turn **Auto-key** on (red) while stopped: an edit adds a key at the playhead. Hold a Position drag while playing with Auto-key on: keys appear at most ten per second along the played span and replace the keys there; releasing stops. Hold a drag while playing with Auto-key off on an animated row: the same target changes for the whole drag.
49. The **Key** row is always there; it is disabled off a key and changes the interpolation of every Transform key at the playhead when on one.
50. Press **Pin**, then select another actor or nothing: the inspector stays on the pinned actor. Press **Pin** again: it follows the selection.
51. Select nothing: the **Scene** section shows **Frame rate**. Change it: the timecode, the ruler steps and snapping change; clip and key times stay in place. Undo restores it.
52. Animate **Time scale** (a key at 1, a later key at 0.5) on an actor whose length was never set by hand: the clip fits the slowed sequence, and the block edges on the row follow the warp.
53. The block group follows the clicked block, or the block under the playhead; its rows are disabled for an actor without blocks and the group never disappears.
54. Build Debug with unity build on and off.

Camera runtime and editor camera:

55. Import an old scene with camera clips and play it: the shots drive the view, also with the editor window closed; shakes (one shake sub-row per old layer) and FOV match the old editor; the time between the old clips and outside them shows the editor camera or the player's view.
56. With the editor open and stopped, scrub inside a block: the view shows the shot at every frame. Pause the game and scrub: the shot still follows.
57. In Debug, unpaused, in first person and in third person: Play into a block, start the first fly, and open the old Cinema Film Maker Editor while a shot drives the view. None of them raises an "already updated" assertion.
58. Start a fly during playback, then close the editor: the shot comes back.
59. Right-drag over the game view: mouse-look continues past the screen edge and the cursor reappears where it was pressed; `W` `A` `S` `D` `Q` `E` move, `Shift` is faster, the wheel while the right button is held changes the speed; middle-drag pans; the wheel alone moves along the view; a right-click without a drag changes nothing. The game reacts to none of it.
60. Close and reopen the editor: the editor camera is off, and **View → Editor camera** shows it where it was left. **View → Game camera** returns to the player's view.
61. With the old editor's window open, this editor's shots and editor camera yield and the old editor's camera works; closing it gives the view back. Start `demo_record`: the transport row says **Camera busy**; stop it: the view comes back.
62. While playing, the camera and the actors stay in sync (no one-frame lag of the actors against the shot).
63. Change **Snap**, the snap step, the fly speed, **Helpers** and **Inspector**, restart the game: all five are kept in `_appdata_ixray_\cinema_editor\editor.ltx`. Put `fly_speed = 1000` and `helpers = maybe` in the file: the speed is clamped to 64 and Helpers stays on, with a log warning.
64. Select the Camera row: its path and the key circles of the block under the playhead show in its colour, lighter than when not selected; **View → Helpers** hides them; they are hidden while a shot is on screen.
65. **Add → Camera move** with the editor camera on takes the key from the editor camera; with it off, from the game view.
66. Build Debug with unity build on and off.

Lanes:

67. Animate an actor's Position (two keys) and Time scale. Open its row arrow: one lane per animated value, Position with X, Y, Z in red, green and blue; **+ Lane** adds an empty Scale lane, and collapsing the row drops it. Give a lane a Sine generator: the lighter result line is drawn over the curve and the actor plays the result.
68. Drag a key: it moves in time and value; the time snaps to the grid, the playhead and the other keys, and `Shift` frees it. `Ctrl` + drag (6 px) copies the selection and leaves the originals. `Escape` or a right-click during the drag restores the keys and the selection. Each drag is one undo step.
69. Right-click a key → **Interpolation**: each of the six modes changes the segment after it. **Bezier** shows handles on the selected keys at a third of the segment with no jump; dragging a handle shapes the curve and the handle never crosses its key. **Easy ease** flattens the curve into and out of the key. Each is one undo step.
70. On a collapsed Position lane, clicking a key selects the X, Y and Z keys at that time and a drag moves them together, in time only. Open the components arrow: a key on the Y lane moves alone.
71. In the lane's **~** pop-up, drag Min above the current Max, or below the value's range (Scale 0.1): it stops at the limit; Max never goes below Min. A generator field drag is one undo step.
72. Select keys and press `Delete`: they go. Remove every key of a lane without a generator: the lane disappears after the edit, the actor keeps its value, and Save writes no lane for it. **x** on a lane removes it and the value stays where it was at the playhead; undo brings it back and no key is shown selected. Select keys, then delete their actor and undo: no key is selected and `Delete` acts on the selected objects.
73. Play with **Auto-key** on and hold a Position drag in the inspector, then keep the mouse still: keys keep appearing at most ten per second at the moving playhead, the keys passed are replaced, and playback never pauses; releasing stops. Undo removes the whole recording in one step.
74. Add Time scale keys (1, then 0.25) on an actor clip never resized by hand: the clip fits, the block edges on the row follow the warp, and playback matches the drawn edges. Drag a Time scale key: the clip refits during the drag.
75. In a scene with many rows, open lanes on several actors and scroll: rows, lanes and the row-drag drop line stay in place.
76. Build Debug with unity build on and off.

Camera authoring:

77. **Add → Camera move** with the playhead inside a camera clip, in a gap and on an empty track: the new clip starts where the clip under the playhead ends, at the playhead, and at the playhead; it is 5 s long, or shorter so it ends where the next clip starts, and no other clip moves. With the playhead in a clip whose end touches the next clip, the item is refused with a status line.
78. Click a camera clip: it gets the focus outline, the inspector shows **Clip N**, and after a fly the shot returns. Drag a clip into a gap: it moves there and the others stay; drag it over the end of the clip before it: an X appears and the shots crossfade; drag it so it would lie inside another clip: it stops at the nearest allowed place. `Ctrl` + drag (6 px) copies the clip with its shakes; `Escape` or a right-click during the drag restores everything.
79. Give a clip three pose keys, a FOV lane with Bezier keys and a shake clip starting in it. Hold `Ctrl` and drag its right edge: the clip, its pose keys, its lane keys and the Bezier handles stretch together, the path is the same shape and plays slower or faster, and the shake clip's start, length and fades stretch with it.
80. Click a pose diamond: the playhead jumps to it and playback keeps running. Drag it: it retimes, snapped to the grid and to the other pose keys (`Shift` frees it), and stops before its neighbours. Right-click it: **Update from view** takes the current view, **Interpolation** changes the segment after it, **Remove** deletes it and is disabled on a block's last key. Hover a diamond and press `Delete`: that key goes. Each is one undo step.
81. Set every key of a block to Bezier: the path and the helper curve are smooth through the keys; set them to Linear or Ease in-out: the path is straight between keys, eased in time. Import an old scene with a smooth camera: it plays exactly as in the old editor.
82. While playing, press **Key from view** in the inspector: a key appears at the playhead without stopping playback; pressing it within half a frame of a key replaces that key. With the shot on screen the button is disabled. In a gap it adds a new clip at the playhead; the clips around it keep their times.
83. Pilot: turn **Auto-key** on, select the Camera row, fly and release the right button: a pose key is added or replaced at the playhead, as one undo step. With Auto-key off, or with another row selected, flying never writes a key. After a fly the view stays on the editor camera inside shots until **Play** or a click on the Camera row.
84. Inspector, Camera section: with no clip at the playhead the section says **No camera clip at the playhead** and offers **Key from view**. On a clip, **Start** and **Length** move and resize it, stopping at the nearest allowed place; while it overlaps the clip before it a **Crossfade** row shows the overlap and edits its ease; on a key, **Position** and **Rotation** edit that key; off a key, they show the path's pose and move every key.
85. On a block with the Game FOV, the FOV row reads **Game** and an open FOV lane shows a flat line at the game FOV. Click its diamond: the block's FOV becomes the game FOV with a key and the view does not jump. Drag the FOV value: the shot zooms. Right-click → **Reset**: Game again.
86. Click a shake clip: the **Shake** group shows it. The header checkbox turns it off (the clip turns faint); the title shows its preset; **Preset**, **Influence**, **Position scale**, **Speed**, **Time offset** and **Stretch** change the shake as in the old editor. Right-click the header → **Remove**: the shake and its lanes go; undo restores everything.
87. Open the Camera row's lanes with shakes on two sub-rows: FOV and Time scale run across the clips, "Shakes 2 Influence" across the shakes of sub-row 2; **+ Lane** lists FOV, Time scale and the values of every shake sub-row. Click a shake lane inside a shake: a key is added there, relative to the shake; outside the shakes of that sub-row nothing is added and the status line says so. Generator fields change the clip or shake under the playhead. A Time scale lane moves the diamonds on the row to where they play.
88. Right-click a clip → **Split at playhead**: two touching clips; the path, the FOV and the Time scale continue across the jump cut, which shows nothing; a shake across the cut keeps shaking without a jump; one undo restores the single clip. The item is disabled with the playhead outside the clip.
89. Undo and redo every camera gesture above: each is one step, and a cancelled drag leaves no step.
90. Build Debug with unity build on and off.

Viewport:

91. Select one actor: a move gizmo appears on it; **T**, **R**, **S** in the inspector switch it to rotate and scale, and no key switches it. **Local** is the default for one actor and **World** for two; changing one leaves the other as it was.
92. Drag the gizmo: it snaps by the inspector steps (0.1 m, 5°, 0.1 by default); hold `Ctrl`: it moves freely. Change the steps and restart the game: they are kept in `editor.ltx`. Put `move_step = 500` in the file: it is clamped to 10.
93. Select three actors and drag the gizmo: one gizmo at their middle moves, turns and scales them as one rigid group. One undo restores all three.
94. On an animated actor drag the gizmo off a key: the whole animation shifts; on a key: only that key; with **Auto-key** on: a key at the playhead; while playing with Auto-key on: keys are recorded. Play with Auto-key off and hold a move on an animated actor off a key: it keeps animating, offset by the move; after the release the whole animation is shifted by exactly the total move, also when the playhead looped during the drag. Turn an actor more than 180° in one drag: it keeps turning without a jump. A move along X writes no Y or Z keys, and a move writes no rotation or scale keys. Each drag is one undo step.
95. Select only the Camera row with the editor camera on and the playhead in a block with keys: **T** and **R** move and turn the shot through the gizmo with the same key rules (on a key, off a key, Auto-key), and the path helper follows. While the shot is on screen there is no camera gizmo.
96. During a gizmo drag press `Escape`, and in another drag right-click: the objects jump back to the start pose, no undo step is left, and neither a fly nor the menu starts.
97. Click an actor in the game view: it and its row are selected; `Ctrl` + click adds and removes actors; a click on empty space clears the selection, with `Ctrl` it does not. Clicks on the timeline or inspector, on a gizmo handle and on the view cube never pick. Each shown actor has a cross at its pivot and its name in its colour, lighter when selected; **Helpers** off hides them. Toggle capture off with `Alt` and fire over a selected actor's gizmo: nothing moves.
98. With the Camera row selected, click a key circle: the playhead jumps to the key, its block is focused, and the view stays on the editor camera. With another row selected, a click where a key of the camera sits (no circle drawn) picks the actor behind it or clears the selection.
99. Right-click the ground without moving: the menu opens. **Add here: Actor...** places the actor on the ground at the cursor, upright and facing the camera; with `Ctrl` held on a slope it tilts with the surface. **Add here: Camera move** adds a block whose key sits on the point. Each is one undo step.
100. **Move to cursor**, **Face camera**, **Snap to ground**, **Delete** and **Frame on timeline** act on the selection as described; each edit is one undo step, and Frame on timeline changes only the timeline view. With the menu open, press on a gizmo handle and drag: the menu closes and nothing moves. Select three actors with different headings, choose **World** and scale: the gizmo axes are the world axes.
101. With the editor camera on, the view cube shows at the top right, left of the inspector; it hides with **View → Game camera**, with **Helpers** off and while a shot is on screen. Dragging it orbits a point in front of the camera; clicking the top face looks down without flipping, and clicking a side face turns to it smoothly.
102. Build Debug with unity build on and off.

Object attachment:

103. Parent an actor to another actor's hand bone: it follows while both play, and scrubbing either way gives the same frames as playing.
104. Parent a camera clip to an actor's head with **Lock** Roll and Pitch on, then set its **Look at** to a second actor: the camera rides the head, stays level and keeps the second actor in frame.
105. Set an actor's **Look at** to the Camera: it turns on its heading only and stays upright.
106. Set, change and clear a **Parent** and a **Look at** on an animated actor and on a camera clip with pose keys: every key, and the plain value at the playhead, stays where it was on screen. Each change is one undo step and redoes.
107. Open the **Parent** and **Look at** lists: the object itself, objects that already follow it, and choices that would make a chain deeper than 8 are grey. Hovering a bone in **Bone** marks it in the game view.
108. Give the second of two overlapping camera clips a parent: the crossfade from the free clip into the attached one, and back, has no jump.
109. Mute a parent actor, or move the playhead outside its clip: its children still follow its pose at the nearest clip edge.
110. Delete a parent: its child is hidden and its row and the **Attach** group show a red warning; undo restores both. Set a bone the model lacks in the file: the origin is used with a warning. Delete a Look at target: Look at is ignored with a warning.
111. Select a parent and its child and drag the group gizmo: only the parent moves and the child follows. With one child selected in **Local**, the gizmo axes are the parent's.
112. Save, then Open: every link, bone and lock is kept and the file is byte-identical. Hand-edit a loop, a self link and a bone with a comma: the scene loads with warnings and those links dropped. A link to an id that is not in the file loads with a warning and is kept.
113. Build Debug with unity build on and off.

Markers, motion marks, clipboard and offset:

114. **Edit → Add marker** puts a flag at the playhead; right-click the ruler → **Add marker here** puts one at the clicked time. Drag a flag: it moves, snapped, `Shift` frees it, `Escape` or a right-click puts it back. Click a flag: the playhead jumps to it. Double-click: it renames in place. Right-click → **Remove**, and `Delete` with the mouse over a flag, remove it. Each is one undo step. Change an unlocked actor's block (its clip refits), then at once drag a flag and press `Escape`: only the flag goes back, and undo then reverts the block edit. Marker names never cover the ruler timecodes.
115. Clips, blocks, keys and the playhead snap to markers.
116. Save, then Open: the markers come back and Open → Save is byte-identical; a scene saved before markers existed still re-saves byte-identically. Hand-edit 300 markers, a NaN marker time and two markers on one frame: the scene loads with warnings and never stops the game.
117. An actor whose animations have motion marks (for example a stalker walk cycle with footsteps) shows ticks inside its blocks at the marked moments, also with a negative speed, on Loop and Ping-pong passes and with a Time scale lane; hovering a tick names the mark; dragged clips snap to the ticks of other actors.
118. Select two actors linked to each other, `Ctrl+C`, move the playhead, `Ctrl+V`: both appear at the playhead with new colours, linked to each other. Paste after **New** or in another scene: their links to objects outside the copy are cleared.
119. Click an actor block, `Ctrl+C`, select another actor, `Ctrl+V`: the block appears after that actor's focused block and the pack is added; an animation the model lacks turns red.
120. Select only the Camera row, `Ctrl+C`, move the playhead into an empty stretch after the last clip, `Ctrl+V`: all clips are pasted from the playhead with their spacing and shakes. Paste where they would land inside another clip: refused with a status line.
121. Select lane keys, `Ctrl+C`, select another actor, `Ctrl+V`: the keys land at the playhead on the same lanes. Camera FOV keys land in the block and shake keys in the shake of the same sub-row under each key's time; keys that cannot land are counted in the status line.
122. `Ctrl+D` on clips, on a block and on keys puts the copy right after the original. `Ctrl+D` never fires while flying with `D` held. `Ctrl+C`, `Ctrl+V` and `Ctrl+D` do nothing while a text field is being typed into or another tool's window is focused, and never reach the game. `Delete`, `Ctrl+Z` and `Space` do nothing while **Offset** or another inspector field is being dragged.
123. Inspector **Offset**: dragging and typed values move the selected clips or keys by whole frames, also when dragged slowly, one undo step each, stopping at 0 and 60:00:00; the row is disabled with nothing selected and the inspector never jumps. The **Clip** start and length and a camera clip's **Length** also step by whole frames when dragged slowly, and a dragged or typed length stops at one frame.
124. Build Debug with unity build on and off.

Presets:

125. With only the old editor's `presets.ltx` (no `actor_presets.ltx`), open **View → Presets**: its presets are listed with **(old)**, and **Actor from preset...** adds one with its playlist as blocks (trims as several blocks, negative speeds reversed) and its `.omf` sources as packs. `presets.ltx` is byte-identical afterwards and the old editor still lists every preset.
126. **Save as** on an actor writes `actor_presets.ltx`; typing an existing name turns it red with "Replaces the existing preset." and saving replaces it. Save as over an old preset's name: the list shows one entry without **(old)**. Restart and save again without changes: the file is byte-identical.
127. Select three actors, **Preset** → pick a preset: model, item, scale, Time scale, End, packs and blocks change on all three; position, rotation, lanes, clip start and links stay; an actor never resized by hand fits its new sequence; one undo step restores all three.
128. **Actor from preset...** from the timeline **Add** pop-up, the **Add** menu (at the playhead) and the viewport menu, with and without `Ctrl` on a slope: the actor appears at the clicked time or point, facing the camera, with the preset applied; one undo step removes it. With no presets the item is disabled.
129. In the Presets window: double-click renames, right-click → **Rename** and **Delete...** (asks first). Rename or delete an old preset: it is gone from the list after a restart too, while the old editor still lists it. Make `actor_presets.ltx` read-only and rename a preset: a dialog names the path and the list stays as it was. None of these is an undo step.
130. Hand-edit `actor_presets.ltx` with a quote, a 5000-byte line, `version = 3`: the file is refused with a dialog, the list holds only old presets, and **Save as** says presets are read-only. Hand-edit 600 presets, a preset without `visual`, two presets of the same name, a comma in a block name: the file loads with warnings, 512 presets at most, never fatal. A broken `presets.ltx` only warns.
131. Build Debug with unity build on and off.

Insert and remove time:

132. Put the playhead inside a camera clip, an actor's walk block and an actor's Position lane between two keys, then **Edit → Insert time...** with 1 s: everything after the playhead plays exactly 1 s later. During the inserted second the camera, the actor's pose and its lane values hold. Markers and later clips moved by 1 s; a loop range after the playhead moved with them.
133. **Remove time...** with 1 s: what follows plays 1 s earlier and matches what played 1 s after the playhead before. A clip wholly inside the second is deleted (status line count). An actor lane whose last keys fall inside the second continues with the value it had at the end of the second. With the range inside one camera clip, the camera shows a hard cut at the playhead; with the range across a block edge, the block inside is gone and the neighbours are trimmed.
134. **Split at playhead** on actor and camera clips behaves as before: same halves, same keys at the cut, one undo step.
135. A looping actor across the playhead keeps its sequence and the status line counts it. Insert time that would make a clip, a marker or a lane key pass 60:00:00 is refused with a status line and changes nothing.
136. Insert time and Remove time while playing use the playhead at the click. Each is one undo step; undo restores every clip, block, key and marker and the loop range, redo applies them again, and Open → Save after undo is byte-identical to the file before.
137. Build Debug with unity build on and off.

Lights and particles in files and import:

138. Hand-write a scene with a Point light, a Spot light with a `texture` and a particle effect, each with a lane and a link: it opens without warnings, the rows show the light and particle icons and the names, the lanes open (Color with **R** **G** **B** lines and toggles), **+ Lane** on a light offers only light values and on a particle only particle values, and Open → Save is byte-identical twice.
139. Hand-edit `type = lamp`, `kind = strobe`, `light = 1,1,1,nan,8,60,0.1`, a `cone` lane key of 500, a `texture` with a comma, a 300-byte `effect`, a `color` lane on a particle and `particle = 0,9`: the scene loads with one warning each, the lamp skipped, the light a Point with intensity 1, the cone key 120, no texture, no effect, the particle without the Color lane and with scale 0.01 and speed 4. Nothing stops the game and Debug raises no assertion.
140. **Import v4** a scene with a light on two master clips (with intensity and colour ramps), a free light with a looping modulation, a tilted Spot with rotation keys, a shadowed Spot with a texture, a light with HUD mode on, a particle on a master clip with OUT shorter than the clip, a free particle with IN/OUT and an old particle without curves: one row per clip with the labels and colours kept, switched-off lights muted, the Warnings window counts every approximation (tilted rotation, colour ramps, free loops, HUD mode, a Range or Cone multiplier past the new range), and the saved file holds absolute lanes (Range in metres, Position in world coordinates, Intensity at most 100). Save → Open → Save is byte-identical.
141. Open a file with lights and particles in the build before this one: it warns once per light or particle object and loads the rest.
142. Build Debug with unity build on and off.

Light and particle playback:

143. Play the scene imported in check 140 next to the old editor at the same times: colours, intensity ramps, range and cone flicker, spot direction, the projected texture on the shadowed Spot and volumetric (with `r2_volumetric_lights` on) match. Outside its clip and muted a light is off. Scrubbing either way gives the same frames as playing.
144. Pause inside a 20 s smoke clip and seek to 15 s: the smoke shows as it would after playing (the last 10 s rebuilt) and stays frozen; Play continues from there; seeking back rebuilds it; a loop range wrapping over the clip restarts it rebuilt. Stopped, drag the playhead back and forth over the clip for a few seconds: the smoke follows about ten times a second, the game stays responsive, and where the drag stops it looks as after a seek there.
145. Play across the end of a smoke clip: emission stops and the smoke fades; Pause freezes the fading smoke. Home, a seek, **Mute** and **Delete** cut it at once.
146. A one-shot explosion with **Retrigger** off plays once per clip; with it on, it repeats until the clip ends; a looping effect ignores it.
147. A Speed lane from 0.25 to 2 slows the effect down and speeds it up; seeking to a time shows the state playing reached.
148. Parent a light to an actor's hand bone and a particle trail to a moving actor (hand-edited `attach_parent`): they follow while playing and scrubbing, and the trail streams behind the motion. A Spot with a Look at on an actor keeps it lit. An actor parented to a light follows it.
149. Close the editor while playing: lights and particles go on. A level change, a game load, **New** and **Open** remove every light and effect (nothing left in the next level, no leftover smoke after a load).
150. Hand-edit an unknown `effect` and a missing `texture`: the rows turn red, the clip label shows "Effect not found" or "Texture not found", the light still shines and the scene keeps playing. Moving a key of a light's Color lane and undoing it causes no hitch.
151. Debug, 200 particle effects while playing and seeking quickly, then a quick load of a save on the same level while they play: no assertion and no crash, and no frozen smoke is left from before the load.
152. Build Debug with unity build on and off.

Light and particle authoring:

153. Timeline right-click on empty space → **Light** adds a Point light named "Point light" at the clicked time, 3 m ahead, turned along the view, with its clip reaching the end of the scene; **Add → Light** does the same at the playhead. **Particle...** opens a filtered effect list without particle animation curves; hovering never previews; a pick adds the effect at the clicked time, named after the effect, 5 s long. In the viewport menu, **Add here: Light** puts the light 0.5 m off the clicked wall and **Add here: Particle...** puts the effect on the point (tilted with `Ctrl`). Each add is one undo step, and undo removes the object.
154. Light inspector: **Type** Spot shows Rotation, Cone and Texture and renames an unrenamed light "Spot light" (a renamed one keeps its name); Point hides them again, hides the light's Rotation and Cone lanes on the timeline and drops their selected keys, and **+ Lane** then leaves out Rotation and Cone; Spot or an undo brings the lanes back with their keys. Color edits above 1; Intensity drags finer near 0 and covers 1 to 10 in a short drag; **Cast shadows** turns shadows on and **Volumetric light** turns Cast shadows on with it; **Texture** lists textures with **None** first (the first open may take a moment) and applies on pick. Non-default names are orange and right-click → **Reset** restores them; every row with a diamond creates its lane when the diamond is clicked.
155. Three lights selected: dragging Range writes only Range on all three, and the header reads "Light x3". A light and a particle effect selected show a Light and a Particle section, and **Pin** sits on the Light section. **Yaw only** in a light's Attach group can be switched; a light parented to an actor's hand follows it, set from the inspector.
156. Particle inspector: **Effect** replaces the effect on every selected particle effect, keeping position, lanes, clip and a renamed name; picked while playing, the new effect appears at the playhead and playback goes on. Scale, Speed and **Retrigger** change what plays; the Clip row moves and resizes the clip without locking its length.
157. **Auto-key** on and playing: dragging Intensity and Color writes keys at most ten per second; playback never pauses.
158. **Pick** next to Color, then click a red wall: the light turns reddish in one undo step; with two lights shown both change. `Escape` or a right-click while waiting cancels, and the click on the wall neither selects nor clears the selection.
159. Copy, paste and duplicate a linked light and particle effect: the copies keep their lanes and the link between them. **Insert time** inside a light clip lengthens it and moves its later keys; **Remove time** over a whole particle clip deletes it, and neither is refused for lights or particles.
160. Build Debug with unity build on and off.

Lights and particles in the viewport:

161. A Point light shows a small circle and its range circle, a Spot light its cone outline (the angle and length follow an animated Cone and Range), a particle effect a circle with a cross and its name, all in the row colour, lighter when selected and faint outside the clip or muted. **Helpers** off and a shot on screen hide them.
162. Click a light and a particle effect in the game view, also a faint one: each and its row are selected; `Ctrl` + click adds and removes them; a click on nothing clears the selection. With **Helpers** off they cannot be clicked.
163. **T** moves and **R** turns a Spot light and a particle effect. **R** is grey in the Light section of a Point light, and with only Point lights selected the gizmo moves and **T** is lit. A Point light turned before it became one shows world axes in **Local**. **S** on a particle effect scales it evenly with the ring and with each axis dot, in Scale steps. With an actor and a Point light selected and **R** picked, **R** is lit in both sections (grey in the light's). **S** with only lights selected shows a handle on each range circle or cone end; dragging one changes the Range of every selected light in Range steps, `Ctrl` frees it, `Escape` or a right-click restores the ranges, and each drag is one undo step. With **Auto-key** on and playing, the handle records Range keys.
164. Select an actor, a Spot light and a particle effect: the group gizmo moves and turns all three; **S** scales the actor and the effect and leaves the light alone. **Move to cursor**, **Face camera** and **Snap to ground** act on lights and particle effects; **Face camera** leaves a Point light as it was. Parent a particle effect to a light, select both and drag **S**: the effect scales and the light stays.
165. Change the **Range step** and restart the game: it is kept in `editor.ltx`. Put `range_step = 50` in the file: it is clamped to 10.
166. Build Debug with unity build on and off.

Lane editing, header column and box selection:

167. Narrow the header column to its minimum and open lanes on an actor inside a folder and on the Camera row with a second shake sub-row: row names, parameter lists ("Shakes 2 Position scale"), the generator type ("Noise continuous") and **+ Lane** end with an ellipsis, and no control overlaps another or the clips. **~** opens the generator pop-up with Amplitude, Frequency, Phase, Min and Max fully readable; with only X and Z lit on a Position lane, a field there changes X and Z only. Hovering a header control names it and nothing more.
168. Drag the line between the header column and the clips: it follows the mouse and stops at 140 and 600 px; the ruler, clips, lanes, the row-drag drop line and clicks on clips, keys and segments stay lined up. Double-click the line: 220 px. Restart the game: the width is kept in `editor.ltx`. Put `header_width = 5000` in the file: it is clamped to 600.
169. Click empty space on a scalar lane (Intensity, or a Position lane with only **Y** shown) at two heights: on release a key appears at each clicked time, snapped (`Shift` frees it), at the mouse height and selected; each is one undo step. On a Scale lane with one axis shown a click at the bottom gives at least 0.1. On a Position lane with all axes shown a click adds X, Y and Z keys with the curve's values at any height. On the Camera row a click in a block adds the key there, and in a block without that shake nothing is added and the status line says so.
170. Hover the curve between two keys: it brightens and the cursor is an up-down arrow; over a key a ring shows and the cursor is a hand. Drag the segment up: both keys rise together and stop at the value's limit; `Shift` moves finely; `Escape` puts them back exactly. On a collapsed Position lane nothing brightens.
171. `Ctrl` + drag a rising segment up, then down: the curve bows above, then below, the straight line; the first key turns Bezier and both handles show; the curve never goes above the higher key or below the lower one and never turns back. One undo step makes the segment as it was. On a Time scale lane the clip refits and the blocks redraw during the bend.
172. Double-click a key: it is removed (on a collapsed Position lane, X, Y and Z at that time); undo brings it back.
173. Drag a box from empty track space over the clips of three rows (an actor, the Camera and a light): all three light up while dragging and stay selected on release. `Ctrl` + drag a box over two of them: those two are deselected and the third stays. A plain click on empty track space clears the selection. Drag a box to the bottom edge of the rows: they scroll.
174. Drag a box from empty lane space over keys in two lanes, one with only **X** shown: exactly the keys inside are selected and `Delete` removes them; `Ctrl` + a box toggles keys. Start a box and press `Escape`, then start one and right-click: each time the selection is the one from before the box. A click on an empty lane without moving still adds a key.
175. Build Debug with unity build on and off.

Timeline fixes, solo and choosers:

176. Select three actor rows (one inside a folder) with `Ctrl` + click and drag one of their names onto the middle of another folder: all three move into it in their screen order; one undo puts all three back where they were. Drag them between two top-level rows: they land there together. Drag a single unselected row: only it moves. A folder still moves alone and only among the top-level rows.
177. Add one Intensity key on a light, then click high on the lane, then low: the first click gives a clearly higher value, the second a clearly lower one, and the curve does not rescale while the mouse stays over the lane; after the mouse leaves it refits around all keys with room above and below. Click right after a key: the click selects or adds without the lane jumping. Repeat on a FOV lane and on a Time scale lane: the Time scale range never shows values below 0 when the keys are near 0.
178. Click a lane with a slight hand movement (up to 5 px): a key is still added. Click a lane outside the clip: nothing is added and the status line says keys go inside the clip. Click a lane a few pixels inside the clip end with **Snap** on: the key lands inside the clip, not past it. On the Camera row, click in a block without Shake 2 on a Shake 2 lane: the status line names the missing shake.
179. Open the lanes of an actor, a light, a particle effect and the Camera, each without animated values: a Position, Intensity, Speed and FOV lane shows with **+ Lane** under it. Collapse and reopen without adding a key: the empty lane comes back once and is not saved.
180. Press **S** on an actor and play: every other actor is hidden, lights are off, particle effects stop and the Camera passes the view through; the other rows' names and clips are faint, muted rows stay faint. **S** on a selected row of three solos all three. Press **S** again: everything plays. Solo, then undo something: solo stays. Close the editor: everything plays again. Solo a light, save and reopen the scene: nothing is soloed and the file has no solo key.
181. Narrow the timeline window to about 500 px: the transport row wraps onto a second line and every control stays reachable; widen it and it goes back to one line. Make a long status line (open a scene with many warnings): it ends with an ellipsis.
182. Narrow the inspector: row names such as **Volume intensity** end with an ellipsis and hovering one shows the full name; the header name field never becomes wider than the window; a missing-parent warning wraps.
183. Open an actor's animation list and type `idle wpn`: only animations whose name or pack contains both words are listed, those where both words start a word part come first; the count under the list matches. Press `Down` three times: the highlight moves, the list scrolls with it and the actor plays the highlighted animation; `Enter` picks it and closes the list. Reopen: `idle wpn` is still there, selected, and typing replaces it. Open **Browse OMF...**: the pack list has its own filter. `Escape` closes the list without a pick. Long animation and pack names end with an ellipsis and never overlap.
184. Stop playback, open the **Model** row's list of an actor and rest the mouse on another model: after a moment the actor shows that model with its animation blocks; move to a third model: it switches; close the list with `Escape`: the actor's own model is back, the scene shows no unsaved change and undo has no new step. Pick a model: it applies as before. While playing, resting on a model does nothing. The **Add → Actor...** list never previews.
185. Open the editor and select an actor: the gizmo shows move arrows, rotation rings and scale handles at once, and **TRS** is lit. Drag an arrow: only Position is written, in **Move step** steps. Drag a ring: only Rotation, in **Rotate step** steps. Drag a scale handle: only Scale, in **Scale step** steps. Each drag is one undo step named after what it did. Select a Spot light, the camera and a particle effect together: rings turn all three and the scale handles scale only the particle effect, the light and camera still orbiting with the group. With only a Point light the gizmo has arrows only. **T**, **R**, **S** still give the single gizmos.
186. Open a version 5 scene whose camera has blocks with two shakes and shake lanes: two **Shakes** sub-rows appear, each shake starts and ends with its block, its Influence/Position scale/Speed keys are in its own lanes, and it plays as before outside blends. A block with Blend in and shakes is counted in the load warnings. Save: the file says `version = 6`, has `shakes` and `shake_NNN` keys and no `block_NNN_shakes`; reopen: nothing changes.
187. Right-click the Camera row → **Add shake here**: a 2 s shake appears on the first free sub-row at the click; right-click an empty part of a **Shakes** sub-row → **Add shake here** puts it on that sub-row (refused with a status line where it would overlap). Each is one undo step.
188. Drag a shake clip: it moves in time, snapped, and to another sub-row when dragged up or down (an empty sub-row appears below while dragging); dropped over a shorter shake it removes it, dropped inside a longer one it cuts it in two, and over one end it crossfades; `Escape` mid-drag brings them back. `Ctrl` + drag copies it. Its edges resize it; its top-corner handles set Fade in and Fade out, and the shaded ramps follow. `Escape` or a right-click during any of these restores it. Play: the shake fades in and out linearly.
189. Shake **M** on a sub-row header: every shake on it turns faint and stops shaking; again: they come back; undo works.
190. Put a shake that starts in no clip across a crossfade between two shots: it keeps shaking through the crossfade without a jump. Put one in a gap or over a clip without pose keys: nothing shakes there.
191. Give block 2 of three a shake. Lengthen block 1: block 2 and its shake move later together; shorten it again. Drag block 2 before block 1: the shake moves with it. `Ctrl` + drag block 2: the copy has its own shake. Right-click block 2 → **Remove block**: its shake goes too; one undo brings both back. Split block 2 at the playhead before the shake's start, then lengthen the first half: the shake, now in the second half, moves later with it.
192. Right-click the scissors between two touching clips → **Crossfade**: the earlier clip grows 0.5 s over the later one, an X appears and both shots and their shakes blend. Right-click the X → **Remove crossfade**: a jump cut again; the next **Crossfade** uses that length. **Ease** on the X changes the curve. Each is one undo step.
193. Focus a shake, `Ctrl+C`, move the playhead, `Ctrl+V`: a copy lands at the playhead on its sub-row (or the next free one). `Ctrl+D`: a copy right after it. `Delete`: the focused shake goes. Copy a camera clip with a shake and paste it elsewhere: the shake comes along.
194. Insert time inside a shake: it grows and its lanes hold; later shakes move later. Remove time across a shake: it loses that part; a shake wholly inside the range is deleted.
195. Import a v4 scene with a shake that has constant fade in and fade out: the shake clip shows the fades as ramps and plays as in the old editor; one with a fade curve gets Influence keys instead.
196. Click the loop brace at the top of the ruler (after a right-drag drew one): it turns grey and playback runs to the end of the scene and stops, without wrapping; click again: it is bright and playback repeats it. Drag its middle: it moves, keeping its length, snapped; drag either end: that end moves, never closer than a frame to the other; `Escape` or a right-click during a drag puts it back. Double-click the top strip: the brace covers the whole scene and is on. Untick **Loop** in the transport: the same as clicking the brace; tick it with no range: the range becomes the whole scene.
197. Select two clips and press `Ctrl+L`: the brace spans both, it is on and the playhead jumps to its start. Click a lane key, `Ctrl+click` another later key, `Ctrl+L`: the brace spans the two keys. With nothing selected `Ctrl+L` switches the brace on and off.
198. On an actor's Position lane (three lines), click empty space close to the green line: three keys appear, Y at the click height, X and Z on their curves. Drag the new Y key up: only Y moves in value while X and Z follow in time; `Escape` restores. Drag the segment of the red line between two keys: only the two X keys move; `Ctrl` + drag bends that segment. Select a Bezier key on the three-line lane and drag a handle up: the curve's value changes too.
199. Open lanes on an actor and the Camera: every lane header is one line (arrow, parameter, generator, **~**, **x**), lanes are two rows tall, and at the smallest header width the lists end with an ellipsis. **~** on the Position lane shows **X** **Y** **Z** toggles above Amplitude; the toggles still choose which lines the generator edits.
200. On the Camera row, **+ Lane** → **Position**: the lane draws X, Y and Z of the path through each block, with diamonds at the pose keys. Drag a diamond up: the shot moves along that axis and undo restores it; drag it sideways: the pose key moves in time like its row diamond. Click empty space in a block: a pose key is added there with the path's pose and the clicked value at the click height. Double-click a diamond: the pose key goes (not the last of a block). Right-click: the pose key menu. **Rotation** works the same in degrees.
201. Collapse an actor's lanes: small diamonds along the bottom of its clip mark every keyed time; expand: they go. A shake clip with Influence keys shows its keys the same way.
202. In an actor's **Model** list press `Down` several times: the highlight walks down the list while the actor shows each model, and `Enter` picks the highlighted one. In any list type, press `Down` and keep typing: the text goes into the filter. Preview a model with the playhead outside the actor's clip, or the actor muted: it still shows. Open the generator pop-up over a lane and change Amplitude: the lane's range follows at once.
203. Select an actor, set **World** and use the universal gizmo: the arrows and rings follow the world axes and the scale handles scale evenly; set **Local**: the gizmo turns with the actor and scales per axis. Select a Point light and an actor attached to it: dragging a ring turns the actor; dragging an arrow moves both together.
204. Drag a shake's left edge later: the remaining part shakes exactly as before (same moments, same keys at the same times); drag it earlier: the added part runs on from the shake before it, also with a speed lane. Put two shakes overlapping (all rows full) and resize or fade one: it works. Import a v4 scene with a fade longer than its clip: it keeps the slope. Move a block whose shake pushes into a full sub-row: the moved shake changes row, the one that stayed put does not. Put a shake after the end of the camera clip and Insert time before it: it moves later; Remove time across it: it loses that part. Select keys on a shake lane, make an edit and undo: the keys stay selected. Drag a camera clip's **Length** in the inspector with a shake in a later block: the shake moves with its block and no status flood appears.
205. Drag the bottom edge of an actor's row header down: the cursor is an up-down arrow, the row grows in whole pixels up to 6 rows, its blocks and labels grow and the top band stays; the title gets ` *`. Double-click the edge: the row is back to one row. Select three rows and drag one edge: all three resize together. Drag an edge and press `Escape`: every height is back.
206. Expand an actor's lanes and the Camera's shakes; resize a lane, a second lane and a **Shakes 1** sub-row. Save, close, reopen: every height is as it was; the object sections have `row_height`, `lane_position_0_height`, `lane_position_0_0_height` and `sub_0_height` only for the resized ones. Open the file in a build from before this change: it loads with default heights. Write `row_height = 50`: the scene loads with a warning and that row at the default.
207. Set `row_height = 2` and `lane_height = 3` in `editor.ltx`: new rows are two rows tall and new lanes three; a double-click on an edge returns to these values. Out-of-range values are clamped.
208. Drag a shake to another sub-row with sub-rows of different heights: it lands on the sub-row under the mouse. A box selection over a tall row selects its clip.
209. Open a scene saved before tangents with smooth (Bezier) cameras: the path, the turn and the timing are exactly as before (compare a frame by frame capture). Save it: no `block_NNN_tangent_NNNN` keys are written, and the file says version 7.
210. Click a camera key circle in a block with Bezier keys: handles appear on the key and on its neighbours' sides. Drag the out handle: the path bends live, the key turns **Aligned** in the inspector and its in handle turns to stay opposite; one undo step "Tangent" restores it. Hold `Ctrl` while dragging: the key becomes **Broken** and the other handle stays. `Shift` gives fine steps. `Escape` or a right-click during the drag restores the key and leaves no step; the gizmo stays usable after release.
211. In the inspector **Pose key** rows: **Tension** 1 makes a sharp corner at the key, −1 an overshoot; **Continuity** and **Bias** change the shape on both sides; the turn changes with them. **Reset tangents** returns to Auto, 0, 0, 0 and the old path. Switching **Tangent** from Auto to Aligned does not move the path.
212. Attach a camera clip to a moving actor and drag a handle: it moves with the parent afterwards and the path shape stays relative to it.
213. Split a block in the middle of a Bezier segment: both new keys are **Broken** and the position path near the cut looks as before. Undo restores one block.
214. With a tangent set, open the camera's Position lane: the curve shows the same bend as the path in the view.
215. Open a scene saved before this version with **Blend in** between two blocks: it plays exactly as before (compare a crossfade frame by frame); the timeline shows an X over the crossfade and the earlier block's Hold is longer by the blend. Save and reopen: unchanged. A preset saved before loads the same way.
216. Drag a block into empty space after the last block: the gap is hatched and the actor holds the last frame there. Drag the first block later: before it the actor holds its first frame.
217. Drag the second block over the tail of the first: an X shows over the overlap and both animations move through the crossfade; right-click the X → **Ease** changes the curve, **Remove crossfade** puts the block at the end of the first one. Drag the X: the overlap changes and becomes a gap past the first block's end.
218. `Ctrl` + drag a block's right edge: the speed changes so the block ends at the mouse, its start stays; `Ctrl` + left edge: its end stays. The inspector **Speed** follows, within 0.05–4.
219. In the inspector set **Loops** to 2.5 and **Start** to 3 s: the block plays two and a half loops from 3 s on and the next block stays where it was; set the speed: the block's start stays.
220. With an actor on Ping-pong, move a block on a mirrored pass: it moves the same way it does on the first pass. Insert time inside a block's second loop and inside a gap: the actor holds the frame at the playhead for the inserted time; Remove time across a gap and a block: what follows moves earlier by the removed time.
221. Split a camera clip in the middle of a Bezier segment between two Auto keys: the new keys and both keys beside the cut are **Broken**, and the position path is the same as before the split. Split exactly on a key: the key keeps its handles.
222. On a Rotation lane with a heading turning from 170° to −170°: the curve and its diamonds continue to 190° without a jump; a click on the curve adds a key there. Drag a pose-lane diamond straight up: the key keeps its time; move sideways 6 px: it retimes. `Shift` while dragging any key gives fine value steps.
223. Drag the bottom edge of a row, a shake sub-row and a lane: the strip is grabbable even over the name, parameter list and buttons; the first move does not jump. Right-click during the drag: the heights come back, no menu opens and the title's ` *` is as before the drag.
224. Click a pose key so its handles show, then add a key before it, undo, and split its block: the handles stay on the same key (or disappear when it is gone) and never jump to another key.
225. On a stalker, right-click the actor's row → **Add row**: an **Animation 2** sub-row appears with every body part ticked. With the new row focused, **+ Animation** adds a clip that starts at the playhead. Untick everything but the torso part and play a walk on the **Animation 1** with a gesture on the row: the legs walk and the upper body gestures; before the row's first clip and after its last one the whole body walks.
226. Add a **Animation 2 weight** lane, key it 0 → 1 → 0: the gesture fades in and out. Set the inspector **Weight** to 0.5: the torso is halfway between the walk and the gesture. Add a second row covering the same part with weight 1: it wins over row 1.
227. Put the same animation on the **Animation 1** and on a row at different times: both play at their own time. Mute the row (**M**): it stops and is drawn faint; the clip refits when the row was the longest. Remove it (**x**): its clips and weight lane go, a later row becomes Animation 2 with its weight lane; one undo restores all of it.
228. Drag a clip from the **Animation 1** down onto Animation 2, and back up: it moves between rows keeping its time; `Ctrl` copies; `Escape` restores. Trims, loops, Hold, crossfades, split and **Replace animation...** work on row clips as on the **Animation 1**, and the inspector shows the row clip that was clicked.
229. Change the model to one without the row's parts: the list shows those names in red and **No body parts**; the row does nothing. Change back: the row works again.
230. Save, reopen: rows, parts, mute, weight and weight lanes are kept. Save the actor as a preset and apply it to another actor: the rows come along. Open a version 8 scene: it loads unchanged. Open a version 8 scene with gaps or crossfades between clips, save, reopen: the gaps and crossfades are kept.
231. Insert time and Remove time across a row's clip: the row's clips move or are cut like the main ones; a row that ends before the cut is unchanged by an insert. A split inside an overlap only splits the clip that was right-clicked; Remove time across an overlap keeps the clips in order.
232. Drag a clip between two clips where it does not fit (the clip before it still plays past the next one): it goes to the next row with room; **Remove crossfade** is disabled when the next clip starts before the earlier clip ends.
233. Undo from the **Edit** menu right after moving a clip, then press `Delete` on lane keys: no crash; the inspector clip group never shows a clip that no longer exists, and after undoing a move between rows it shows the clip under the playhead. Press `Ctrl+Z` while dragging a clip: the undo happens when the drag ends. Mute the only row with clips on an actor without main clips: the clip keeps its length. Add an animation from a separate `.omf` pack while rows play: the pose stays intact. Copy weight keys of Layer 2 and paste them on an actor with one row: they are skipped and counted.
235. Put two shakes on **Shakes 1** and drag the second over the end of the first: an X shows over the overlap and the view blends from one shake into the other; outside the overlap each plays at full influence. Drag the X: the second shake slides and the X follows. Right-click the X → **Ease** → Linear: the blend changes; **Remove crossfade**: the second shake now starts where the first ends. Drag a third shake into the overlap, or a shake fully inside another: it stays at the last allowed place. Undo restores each step.
236. Put a slow wobble on **Shakes 1** across the whole camera clip and a short strong shake on **Shakes 2** with fades. Right-click the **Shakes 2** header → **Override**: the header reads **Shakes 2 (override)**; while the strong shake plays at full influence the wobble disappears, and it returns as the strong shake fades. **Add** brings back the sum. Save and reopen: the mode, and a crossfade ease other than Ease in-out, are kept (`shake_row_01_mode = 1`, a sixth value on `shake_NNN`). Open a version 9 scene with shakes: it plays as before and every shake has Ease in-out.
237. Press `K` with the cursor free: **Auto-key** turns red and back; its tooltip reads **Auto-key (K)**. While typing in a field, `K` types. Select an actor, a Spot light, a Point light and a particle effect, move the playhead and press `F`: one undo step adds Position, Rotation and Scale keys on the actor, Position and Rotation on the Spot light, only Position on the Point light, Position, Rotation and Scale on the particle effect, all at their current values; pressing `F` again on the same frame changes nothing. Hover a light's Intensity lane and press `F`: only Intensity is keyed. Hover a camera FOV lane over a block and press `F`: a FOV key appears in the block under the playhead. Select the Camera row with the editor camera on and press `F`: a pose key from the view is added; with the shot on screen the status line says it cannot.
238. Fly with the right button: `Space` moves up and `C` down, and `Space` does not start playback while the button is held. `Q` and `E` roll the view (left and right side down), `Shift` rolls faster; looking around with the mouse keeps the horizon at the rolled angle. `R` narrows and `T` widens the view between 5° and 150°. Release: the roll and the FOV stay. **View → Reset roll** levels the view and **View → Reset FOV** returns to the game FOV; both are grey while there is nothing to reset. The view cube turns the camera and keeps the roll.
239. With **Auto-key** on and the Camera row selected, fly, roll 20° and zoom to 40° (`R`), then release: the new pose key has the roll as its bank (the inspector shows it) and the block's FOV lane has a 40° key at the playhead, all in one undo step; playing through shows the rolled, zoomed shot. Fly again without changing the FOV and release: no FOV key is added, even though the view's FOV differs from a block on the game FOV. **Key from view** in the inspector, with the editor camera zoomed to a FOV different from the block's, adds the pose key and a FOV key. With no camera yet, zoom with `R`, then **Add → Camera move**: the new block's FOV is the zoomed one. Start a fly while a rolled, zoomed shot is on screen: the view does not jump.
240. On **Shakes 1** with a shake at 2–6 s, right-click that sub-row at 1 s → **Add shake here**: the status line says no row is free there (a 2 s shake would overlap); the Camera row's menu at 1 s puts it on **Shakes 2**; copy a shake and paste it where it would overlap another on its sub-row: it lands on the next sub-row free there. Two shakes crossfading: click their Influence lane in the overlap near one shake's curve: the key goes into that shake. Grab the earlier shake's right edge inside the X: it resizes that shake. Set **Shakes 2** to **Override**, then move its only shake to **Shakes 3** and release: **Shakes 2** reads Add again, and the undo step restores both.
241. A scene with camera clips that touch: the view jumps at the scissors, with no blend.
242. Open a version 10 scene with camera blocks, a block without keys between two shots, a block **Blend in** after a shot and shakes: the shots play at the same moments, the keyless block is a gap (a warning counts it), the blend is now an X over the end of the earlier clip and looks the same; a warning counts blends that can look different. A scene whose camera ended in a keyless block still ends at the same time. Save: the file is version 11 with `block_NNN = length, ease, fov, time_scale, start`.
243. **File → New**: the timeline shows an empty **Camera** row. Select it alone and press `Delete`: the status line says the camera track cannot be deleted; the row menu's **Delete** is grey. `Ctrl+D` and `Ctrl` + drag on the band never copy the camera. Playing the empty scene shows the editor or the player's view.
244. With **Auto-key** on and the Camera row selected, Pilot in a gap: a new 5 s clip appears at the playhead with one key from the view (shorter when the next clip is closer). Pilot inside a clip: the key goes into that clip; neighbouring clips never grow. With less than a frame before the next clip, Pilot and **Key from view** refuse with a status line.
245. Two clips crossfading, each with a shake that belongs to it, and one shake that starts in a gap and runs under both: through the X the first clip's shake fades out and the second's fades in with the shot, and the gap's shake shakes at full strength the whole time; in the gap itself nothing shakes.
246. Drag a clip's left edge later: the shot stays exactly where it was in time (same pose at the same moments), a key appears at the new start; drag it earlier: the shot holds its first pose before the old start. A shake that belongs to it stays where it is.
247. Drag the Camera row's band: every clip and every shake move together, never before 0; `Escape` restores them.
248. **Insert time** with the playhead inside a clip: it is split there (a jump cut that shows nothing), the second half and every later clip and shake move later. **Remove time** across two clips: the parts inside the range go, later clips move earlier and touch. With the playhead inside a crossfade, an insert that would leave two clips starting together is refused with a status line. Undo restores everything.
249. Copy a camera clip with its shakes, move the playhead into a gap, `Ctrl+V`: the clip and its shakes land there. `Ctrl+D` on a clip puts the copy right after it, or refuses when the next clip is in the way.
250. Import an old-editor scene with camera clips that have gaps between them: the camera track shows the clips at their old times with gaps (no empty filler clips), and playback matches the old editor.
251. Select a Point light: the Shadow and Volumetric groups start with plainly visible **Cast shadows** and **Volumetric light** checkbox rows. With Cast shadows off, tick **Volumetric light**: Cast shadows turns on as well, the light casts shadows from all sides and, with the renderer's volumetric lights option on, shows volumetric light; the Volumetric rows are never grey. Untick **Cast shadows**: Volumetric light turns off too. Each tick is one undo step.
252. Drag the **Opacity** slider on the transport row: the timeline, inspector and presets backgrounds follow it between nearly clear and solid; restart the editor: the value is kept; `opacity = 5` in the settings file loads as 1.
253. Open a version 11 scene with an actor that has a gap before its first block, a crossfade, Loops, a Hold, a Time scale lane and a layer: it plays exactly as in the previous build; the actor's row is a plain bar, **Animation 1** holds the main sequence (a pose clip in the lead gap) and **Animation 2** the layer with its parts and weight. Save, reopen: version 12 with `rows` keys, and it plays the same.
254. Open a version 11 scene with an actor set to Ping-pong whose blocks crossfade: it plays as before; a clip that would lie inside another at a turn sits on a second row with a Fade in. Open one whose model is missing: clip edits only show the status line and saving keeps `relative = 1`; restore the model and reopen: it converts.
255. On **Animation 1**, overlap two clips partly: an X shows and both move through the crossfade; **Ease** changes it; **Remove crossfade** trims the earlier clip. Drag a clip down past the row's edge: it goes to **Animation 2**, a new row shown under the last while dragging; a sideways drag never changes rows.
256. Drag a clip's top-left and top-right corner handles: **Fade in** and **Fade out** ramps show and the clip eases in and out; on a torso-only **Animation 2** over a walk, the torso eases into the gesture and back while the legs keep walking. Between two clips of that row the gesture's last frame holds; after its last clip the row is off.
257. Right-click **Animation 2**'s name → **Add** with a nod or breathing clip: it plays on top of **Animation 1**'s motion, scaled by the row's Weight lane; muting the row removes it. Switching a fifth row to **Add** is refused with a status line.
258. Right-click empty space on a row → **Add animation here...**: the clip lands at the click on that row; on a spot already taken it lands on the first row where it overlaps nothing. **+ Animation**, `Ctrl+V` and `Ctrl+D` place clips the same way.
259. **+ Row** and **x** in the inspector, **Add row** / **Remove row** in the row-name menu: rows are added up to 8 and removed with their clips and weight lane (never the last); removing **Animation 1** moves **Animation 2** up. Each is one undo step, and every clip drag, trim and fade is one undo step that `Escape` cancels.
260. Save the actor as a preset and apply it to another actor at 10 s: its rows, parts, modes, weights and clips with fades come along, the first clip at 10 s. A version 4 preset applies and converts once loaded.
261. Camera shakes behave exactly as before: overlap, crossfade, collision moving to the next free row, Add Shake and paste on a free row.
262. Insert and Remove time across an actor's clips on two rows: later clips move by the amount, a clip under the playhead is split or cut, and undo restores everything.
263. With `r2_dof_enable 0`, set a camera clip's **Mode** to **Manual** with Focus 5, Near range 2, Far range 20 and play it: after a short hitch things at 5 m are sharp and the background blurs, and the console still reports `r2_dof_enable 0`. Press F5 (save) and quit: `user.ltx` keeps `r2_dof_enable off`. Close the editor: the game's depth of field is off again. Start with `r2_dof_enable 1`: no hitch.
264. Key **Focus** from 2 to 30 m across a clip and play: a smooth focus pull. Key **Blur** from 0 to 10: the blur strength follows. Stop playback: the game's depth of field comes back smoothly; aim a weapon afterwards: the weapon's own depth of field works.
265. Set **Focus on object** with an actor's `bip01_head` as target and let the actor walk towards the camera: the head stays sharp. **Focus offset** 1 moves the sharp plane 1 m behind the head. Delete the actor: a red line says the target is missing and the clip uses **Focus**; undo brings the target back.
266. Overlap a Manual clip with an Off clip and play through the crossfade: the blur fades out over the overlap. Between two Manual clips with different focus: the focus slides from one to the other.
267. Select the Camera row with the editor camera on: the focused clip draws three frames in front of the shot's camera (focus bright, near and far faint) and "Focus 5.0 m" beside it; in Focus on object it reads `-> Name.bone`. Look through the shot: the frames are hidden.
268. Press **Pick focus**, click a wall in the game view: **Focus** becomes the wall's distance along the view; with **Auto-key** on a key is added at the playhead. `Escape` cancels the pick. In Focus on object the button is grey.
269. With the editor camera on and **View → DOF preview** ticked, fly in front of a Focus on object clip's target: the sharp plane follows the target as seen from the editor camera. Untick it: the game's own depth of field. The setting is remembered.
270. Save a scene with depth of field: `version = 13`, `block_NNN_dof` and `block_NNN_dof_target` keys and `dof_*` lanes; reopen: unchanged. Open a version 12 scene: every clip is Off. Undo every depth-of-field edit; copy **Focus** keys to another clip and paste.
271. Run `demo_record` while a shot with depth of field is on screen, change the level, and close the editor mid-shot: each time the game's depth of field is back to normal.
272. Remove the last animation row from its name's right-click menu (**Remove row**): no crash, the row is gone and undo brings it back.
273. Open a version 11 scene whose actor uses Ping-pong with overlapping blocks: after conversion it uses only as many new rows as it needs (not all 8), and a block moved to a new row fades in no longer than its own length, reaching full strength.
274. On Animation 2, give a clip **Fade out** 1 s followed by a gap and then a clip with **Fade in** 1 s: the row fades out and stays out in the gap, then fades in from the held pose without a pop. On Animation 1, a clip with Fade in at a frame boundary never flashes the idle pose.
275. Mute Animation 1 (or set it to Add) while an Add row plays: the actor shows the idle pose with the Add row on top, never the bind pose.
276. On an Add row, set a clip's In above 0 or its Speed negative: the inspector warns "Adds from the animation's first frame".
277. In a crossfade between two clips on one row, the earlier clip's fade-out corner can be grabbed; two clips starting at the same moment, or one ending exactly where the other ends inside it, are refused.
278. Camera: a shake that starts in clip A and runs past A's end into the touching clip B keeps shaking past the cut. Select the Camera row and an actor and drag the actor's band: both the actor and every camera clip and shake move.
279. Camera: with two overlapping clips at the playhead, **Add → Camera move** places the new clip after both, never inside them. Reorder clips by dragging while lane keys of one clip are selected: the selection and **Pick focus** still act on the same clip.
280. Fly with the right button for a few seconds, also right after pressing it: the view never jumps on the first frame, turns at the same speed every time, turns slower after `R` zooms in, and follows the game's invert-mouse setting. **View → Look sensitivity** at 2 turns twice as fast and is kept after a restart; `look_sensitivity = 9` loads as 4. Releasing the button leaves the cursor where it was pressed.
281. An actor with two clips on **Animation 1** and one on **Animation 2** with a gap: the rows show only the clip boxes in the actor's colour, nothing before the first clip or after the last, and a dotted line in each gap; Hold inside a clip is still hatched. Mute the row: the boxes turn faint.
282. Open an actor's lanes on a fresh settings file: lanes are one row tall and show `low .. high` at their left edge. Hold `Ctrl` and turn the wheel over the header column: every open lane grows by one row per notch (up to 8) and shrinks back (down to 1); over the clips the same gesture zooms time. On a lane two rows tall, each clip's first and last key show a `time | value` tag; a lane whose range crosses zero shows a faint zero line.
283. On a Position lane click **Y**: only the Y line is bright, X and Z are faint and cannot be picked or boxed; click **Y** again: all three are back. `Ctrl` + click **X** hides X only. With only Y shown, click empty lane space: a key is added on Y alone; drag a key: only its Y key moves. There is no components arrow any more.
284. Hover a key with Ease in-out: its two tangent handles appear, following the current curve. Drag the out handle: the segment turns Bezier without a jump and bends; the key's in handle turns with it when the previous segment is Bezier. Drag with `Ctrl`: only the grabbed handle moves. `Escape` during the drag restores the curve and mode; one undo step reverts it. Selected keys keep their handles while the mouse is away.
285. Stopped, click the ruler at 2 s: the playhead and the hollow start marker go there. Press `Space`: playback starts at 2 s; press `Space` again: it stops and the playhead returns to 2 s. Play, then `Shift+Space`: it pauses where it is; `Shift+Space` again continues from there and the marker stays at 2 s. While playing, a ruler click moves only the playhead. `Home` moves both to 0.
286. Drag on the ruler up, down or sideways: it only scrubs the playhead (and, while stopped, the start marker); it never zooms or scrolls the view. The start marker is a hollow triangle pointing down at the bottom of the ruler. Double-click the ruler: the view fits the whole scene.
287. The overview strip above the ruler shows a line per object and a box for the visible range: dragging the box scrolls, dragging its edges zooms, clicking outside centres the view there. Select two clips and press `Z`: the view fits them; with nothing selected and a loop range, `Z` fits the loop; `+` and `-` zoom around the playhead.
288. With **Follow** on, play a scene longer than the visible width: the view pages when the playhead reaches 90 % of the width. During playback, `Shift` + wheel or a zoom stops the paging until the next **Play**. Turn **Follow** off: the view stays put; it is remembered after a restart.
289. Fly with the right button and release: movement and turning start and stop with a short glide; **View → Fly smoothing** 0 stops exactly, 0.5 glides about half a second. Starting a fly from the shot on screen never swoops in from somewhere else.
290. With **Auto-key** on and the Camera row selected, play and fly for five seconds over a clip, then release: the clip's keys inside that span are replaced by a smooth path of a few dozen Bezier keys (not one per frame), the shot replays the flight without the hand jitter, and one undo restores the old keys. Do the same in a gap: a new clip covers the recording. `Escape` during the fly writes nothing. Zoom with `R` while recording: FOV keys appear; without zooming none.
291. Set a camera clip's **Smoothing** to 0.5 on a jerky path: playback is visibly smoother, and a shake on the clip is smoothed with it, scrubbing back and forth shows the same pose at the same frame as playing, and a **Smoothing** lane keys it per clip. Save: `block_NNN_smooth` is written only when not 0; the file is version 14 and an older build refuses it.
292. `Ctrl` + drag a key's tangent handle: only that handle moves and the key is marked **Broken tangents** in its menu; save and reopen: the key line has an 8th value `1` and the handles still move on their own; untick **Broken tangents**: dragging one handle turns the other again.
293. Open a model list, click the star of two models: they turn gold and move to the top; picking still needs a click on the name. Switch the shelf to **Favourites**: only those two are listed, and the filter works inside. Restart the game: the favourites are still there in `cinema_library.ltx` (`[fav_model]`).
294. Right-click an animation → **Add to folder** ▸ **New folder...**, name it `walks`: the folder exists and holds the animation; add a second one from another pack; show the folder in the shelf combo: both are listed. Right-click a row → **Remove from folder**; right-click the shelf combo → **Rename...** and **Delete**. Effects, items and textures have their own favourites and folders.
295. Open a light's **Texture** list and rest the mouse on a texture: after a moment a preview with its name and size appears under the list; moving with `Down` updates it; close the editor: no texture is left loaded (the next open loads afresh).
296. Right-click an empty part of an actor Position lane with keys: **Copy curve**, then right-click another actor's Position lane at 3 s and **Paste curve**: the same shape starts at 3 s; undo removes it in one step. **Reverse** plays the curve backwards between its first and last key, **Scale time** x2 doubles its length from the first key, **Smooth keys** softens a jagged curve, **Reduce keys** on a dense recorded curve leaves fewer keys with the same shape. **Presets > Bounce** adds keys over 1 s from the clicked time; **Wiggle** adds a noise generator. Paste onto a lane with a different number of axes (FOV after Position) is grey.
297. **View → Shots & Markers**: the window lists every camera clip (**Shot 1**, **Shot 2**…) with start, length, FOV and DOF mode, and every marker. Click a clip row: the playhead jumps to its start and the clip is focused in the inspector. Double-click it, type `Intro`, `Enter`: the name shows in the table and on the Camera row; undo restores it; save and reopen keeps it (`block_NNN_name`). Rename a marker the same way. Select a marker row and press `Delete` with the window focused: the marker is removed (one undo step); timeline shortcuts do not fire while this window is focused. Close and reopen the editor: the window stays shown.
298. On an actor far from the world origin, drag a Position key up by 100 px: the value changes by at most 1 m, not by tens of metres; a Rotation key by at most 25°; with `Shift`, a tenth.
299. Open a Position lane on a light far from the world origin whose X, Y and Z differ by hundreds of metres and key a 0.5 m dip on X: each axis fills the lane on its own, the dip is clearly visible, and clicking near a curve adds a key on that axis at the clicked height.
300. Select a walking actor and turn on **View → Onion skin**: blue skeletons trail behind it and orange ones lead ahead, 5 frames apart; change **Onion count** and **Onion step**: the ghosts follow; scrub and play: the actor itself still shows its own pose, never a ghost's. Select a second actor: both get ghosts.
301. Turn on **View → Motion paths** with a moving actor, light and particle effect selected: each draws its path over the visible time range with a dot per frame; zoom the timeline out: the path covers more time; an attached object follows its parent's current position.
302. Select a light with keyed Position and Intensity and open **View → Graph Editor**: the list shows Position X/Y/Z and Intensity in their colours, the graph draws them overlaid; untick one: it hides; click a name: only that curve shows. Press `A`: the view fits; tick **Normalize**: every curve fills the height.
303. In the Graph Editor click a key, `Ctrl` + click a second one, drag one up and right: both move together, never past an unselected neighbour; `Escape` mid-drag puts them back; `Ctrl+Z` after a drag undoes it. Box-select four keys and drag the box's right edge: their times stretch around the left edge. Press **Auto** on a peak key: its tangents are flat; press **Spline** on a middle key: the curve passes smoothly through it.
304. Set **Post** to **Cycle** on a two-second Position curve and play past its last key: the motion repeats; **Cycle offset** keeps climbing, **Oscillate** goes back and forth, **Linear** continues the end slope. Save: `version = 15` and `lane_NN_infinity` are written; reopen: unchanged. Press **Buffer**, change keys, press **Swap**: the old curve is back and the edited one is the grey ghost. On a Rotation curve keyed 170° then -170°, press **Euler**: the second key becomes 190°.
305. **View → Frame lines** 2.39:1 while looking through a shot: a centred frame with darkened bars above and below; **Frame mask** 1 makes them black; **Safe areas** adds the 90 % and 80 % frames and a centre cross, **Thirds** the third lines; on the plain game view nothing is drawn; restart: the choices are kept.
306. Open the camera's Position and Rotation lanes on a clip with three Bezier pose keys and hover the middle key: handles appear on each shown axis. Drag a Position handle up: the path bends in the viewport as with the viewport handles, the key becomes Aligned and the opposite handle stays in line; `Ctrl` + drag breaks it. Drag a Rotation handle up and outward: the key's **Bias** and **Tension** change in the inspector. `Escape` during a drag restores the key; each drag is one undo step.
307. On a Position lane with X, Y and Z keys at the same time, click the X key: only it is selected (white); drag it: only X moves; double-click the Y key: only Y is removed; a box over the three selects all three.
308. Place a smoke effect so its emitter is just outside a shot's view but its smoke drifts into it, and play through the shot: the smoke is visible and does not pop in when the emitter enters the view.
309. Open a version 15 scene with camera clips: each clip gets Position and Rotation lanes with keys at the old pose-key moments, the path plays as before, Time scale included (smooth turns may differ slightly, and the load notes count them), and the Camera row shows the same diamonds; save and reopen: no `block_NNN_key` lines are written and the path is unchanged.
310. On a camera Position lane drag only the X key of a moment sideways: only X moves in time, a new diamond appears on the Camera row at its new moment; drag a diamond on the Camera row: every axis key at that moment moves with it; **Remove** on a diamond removes all of them; one undo step each.
311. Select a camera key in the game view and drag a tangent handle: the X, Y and Z Position keys at that moment turn Bezier and their handles follow the drag (Aligned mirrors the other side, `Ctrl` breaks it); the Position lane shows the same handles. Key from view, Pilot and `F` key all six curves at the playhead.
312. Give a camera clip with four path keys a Time scale lane (1, then 0.5): the later diamonds and the Position/Rotation keys on the lanes move right, playback passes each key exactly at its diamond; Key from view at the playhead adds a key under the playhead; drag a diamond, split the clip between two keys and trim its left edge: the shot stays where it plays. Save and reopen: unchanged.
313. Set **View → Frame lines** 21:9 in a 16:9 window, select a camera clip with FOV 60: **Horizontal FOV** shows about 91.5 (the window's); switch Frame lines to 4:3: it shows about 75.2. Type 90 into **Horizontal FOV**: **Vertical FOV** becomes about 73.7, through a key when the playhead is on one, or a new key with Auto-key; one undo step. A clip on **Game** shows **Game** in both rows.
314. **File → Render frames...**, Whole scene, 30, 21:9 in a 1920×1080 window: the progress window counts frames, `cinema_renders\<scene>\` gets one 1920×823 `frame_NNNNNN.tga` per 1/30 s with no editor UI or frame lines; an actor walk and a smoke effect look the same in two renders (compare a frame); **Escape** midway stops and the playback clock runs normally again (the in-game time keeps moving forward, no jump back).
315. Select an actor and press `B`: its bones show as lines with dots; hovering one names it and turns it yellow; click the right forearm: it turns white and the gizmo sits on it with rings only; turn it: the forearm and hand turn with it, the animation keeps playing underneath, **Pose 1** appears in the inspector's **Posing** group and the actor's lanes gain a forearm Rotation lane; one undo step restores everything; `Escape` during the turn cancels it.
316. Select `bip01_pelvis`: the gizmo also has arrows; move it: the body moves. Switch **Pose 1** to **Override** and give it weight 0.5: the forearm sits halfway between the animation and the pose; **M** mutes the row.
317. With **Auto-key** off, the forearm posed and no keys: scrubbing keeps the pose. Press `F` at 0 s and at 2 s with different turns: two keys on the forearm lane, and playback blends between them; **Reset** removes them. Save and reopen: the pose rows, bones and lanes come back; a scene version 16 opens with no pose rows.
318. In posing mode drag a box over an actor's arm: its bones are selected (white); `Ctrl` + box over the hand toggles those; a plain click still picks one bone; `Escape` during the box cancels it.
319. Attach a prop to an actor's hand bone, key its **Attach** lane 1 at 2 s with Hold and 0 at 3 s, play: the prop follows the hand until 3 s and then stays exactly where it was in the world while the hand moves on; scrub back and forth over 3 s: the same pose each time. Key 0 at 4 s (Hold) and 1 at 5 s: it jumps back into the hand at 5 s without a jump before. Save and reopen: `version = 18`, the lane is kept.
320. In posing mode on a stalker: hips, chest, head, hand, foot, eye and jaw handles appear in their side colours; clicking a hand diamond selects `bip01_l_hand`/`bip01_r_hand`. In **Rig**, set **chest** to `bip01_spine1`, press **Save rig**: the file appears under `cinema\rigs`; reopen the editor: the override is kept and the node reads **Rig (override)**; **Auto rig** removes the file. Edit the file to name a missing bone: the log names it and the auto bone stays.
321. Drag an actor's move arrow and hold `Shift` over a table: the actor sits on the table under the cursor; `Ctrl`+`Shift` on a slope tilts it to the slope; a light hovers 0.5 m off the surface; with two objects selected both move, keeping their spacing; one undo step restores them.
322. Select the Camera row with a clip of four keys: every key shows two outlined handles; hover one: a yellow ring; click it: it turns white and gets a move gizmo, the camera's own gizmo disappears; drag an axis: the path bends, snapped to **Move step**, and the opposite handle mirrors; start a drag with `Ctrl`: only that handle moves; `Escape` mid-drag restores; click empty view: the handle gizmo goes away.
323. Set **Path tension** to 0.3 and to 2 on a smooth clip: the path tightens and loosens in the view, in playback and on the Position lane, while the keys' handles in the file are unchanged; save: `block_NNN_tension` is written, and absent at 1.
324. Pose an actor's forearm with keys at 0 s and 2 s: a **Pose 1 (add)** row shows under its Animation rows with two diamonds; click one: it fills and the forearm lane keys at that time are selected; drag it by 1 s: all of them move, one undo step; `Ctrl` + drag copies; right-click a diamond: the key menu; right-click empty row space at 3 s and choose **Key pose**: a diamond appears at 3 s; **M** mutes the row, **x** removes it with its lanes.
325. Right-click the pose row, **Save as animation...**: with an empty name **Save** is disabled; `Test 1` shows the name hint; with `wave_test`, **Pose keys** and **Pose only**, **Save**: the status line confirms, `cinema\motions\wave_test.omf` exists under the application data folder, and the animation chooser lists `wave_test`; place it on an Animation row with the pose row muted: the forearm moves as posed. Save again with the same name: refused as existing. **Final result** over the loop brace bakes the animation with the pose on top.
326. Select a stalker, open **Shaders**: one Surface row per mesh with its own shader and texture. Pick another shader for Surface 1: that mesh changes at once, the row label turns orange; **Reset** brings it back; one undo step each. Pick a texture: the mesh shows it. Save and reopen: the override is kept (`looks`, `look_00_00_shader` in the file).
327. Add Look 2, set another shader on Surface 1; at 3 s pick **Look 2** in **At playhead**: a **Look** lane appears with keys at 0 s and 3 s; playing switches the mesh at 3 s exactly, scrubbing back switches back. Drag the 3 s key to 5 s: the switch follows. Insert 2 s of time before it: the switch moves. Remove Look 2: the lane keys show Look 1. Hand-edit `look_00_00_shader = no_such_shader`: the row is red and the mesh keeps its own shader; the editor does not crash. A version 18 scene opens with no looks; an older build refuses a version 19 scene.
328. Close the editor with an actor in Look 2, reopen the level: no NPC with the same model shows the override.
329. With other ImGui tools open (the main menu bar, a docked inspector), render 10 TGA frames: no image shows any ImGui window, only the game view.
330. Render the loop brace with **Output** **AVI video (MJPEG)**, 30 fps, 21:9: `<scene>.avi` plays in a common video player at 30 fps with the right length, even width and height, and the game view only; **Cancel** halfway still leaves a playable file with the frames so far.
331. Pose a stalker (`B`): handles show at the root, spine, spine1 (or not, when the chest uses it), neck, clavicles, toes and every finger base in side colours. Click a finger base next to the hand handle: the finger is picked, not the hand. Turn it: the whole finger curls, one undo step; its three joints are in the pose row. Drag the root handle: the whole body moves. **Rig (auto)** lists the new controls; **Save rig** writes them.
332. Give an actor an item (a weapon), open **Shaders**: **Item surface N** rows follow the body's surfaces; override one: only the item changes, also in Look 2 over the timeline; save and reopen: kept (`look_00_NN = 64`).
333. Pose a stalker, open **IK**, tick `arm_r`: nothing moves; a cross shows at the right hand and a triangle by the elbow. Drag the cross: the arm follows, the elbow bends towards the triangle; drag the triangle: the elbow swings; turn the cross: the hand turns. **IK weight** 0.5 blends halfway. Key the target at 0 s and 2 s: playback moves the hand between them.
334. Hover an IK target: a yellow ring and `arm_r target`; click it: it turns white with its own gizmo and the bone gizmo goes away; drag: the hand follows; `Escape` mid-drag restores; with **World** gizmo axes, turn it: the hand turns from its current turn (no jump); click empty view: the gizmo goes away. Tick `leg_l`, switch it to **World**, play an animation that walks the actor: the left foot stays planted while the body moves. With IK keys on the limb the space button is disabled. Save and reopen: limbs, spaces, values and lanes are kept; the first-person hands IK in game still behaves as before.
335. Pose a stalker, select the head: small scale boxes show outside the gizmo rings (the **S** button shows them alone); drag one: the head grows along that axis, the jaw and eyes grow with it; one undo step; **Scale** shows the factor. Key it at 0 s (1) and 2 s (2): playback grows the head. Rotate and move a scaled bone with the universal gizmo: it turns and moves without changing size. Save and reopen: `pose_00_bone_00_scale` and the scale lane are kept; **Reset** returns it to 1.
336. Pose a stalker, select `bip01_r_hand`, **+ Constraint**, **Target** a crate: nothing moves; move the crate: the hand follows (Parent); **Point**: only its position follows; **Orient**: only its turn; **Aim** on the head with a moving light as target: the head keeps looking at it. **Weight** 0.5 goes halfway; keyed weight fades it in. **Snap** puts the hand on the crate's origin. Target the camera: the hand follows the shot. Pick a target that has the actor as its parent: refused with a status line. Remove the crate: the bone name turns red, nothing crashes. Save and reopen: constraints and weight lanes are kept; a preset saved from the actor has none.
337. Pose the left forearm and hand and turn the head left; press **Mirror**: the right arm takes the mirrored pose, the left arm the right one's (zero), the head turns right; one undo step. Select the left hand only, **Paste opposite**: only the right hand changes. Save the arm pose as `wave`, reset, **Apply**: the pose returns; **Mirrored**: it lands on the other arm. Reopen the game: `wave` is still listed; `cinema\poses.ltx` holds it. Apply `wave` to a dog: bones it lacks are counted in the status line.
338. **Add → Decal...**, pick `wm\wm_blood_1`: a blood mark lies on the ground 3 m ahead for 5 s; scrub out of the clip: it disappears; back in: it returns. Move, turn and resize it: the mark follows at once; key Width at 0 s (0.3) and 2 s (1.5): it grows while playing. Viewport menu **Add here: Decal...** on a wall: it lies on the wall. Set an unknown shader by hand in the file: the row is red, no mark, no crash. Place it over a gap with no geometry within 0.5 m: no mark, no crash.
339. Tick **Stamp**, put the decal under an actor, play across its start: a mark lands on the actor's body; move it off the actor: it lands on the ground; it stays after the clip and fades like a bullet hole; scrubbing across the start does not stamp; **Clear stamps** removes it. Save and reopen: `type = decal` with its keys is kept. A game level with a dynamic wallmark zone still shows and hides its mark.
340. Select an actor with Position keys at 0 s, 2 s and 4 s, helpers on: its whole path over the clip is drawn in its colour, like the camera path, with a circle and a facing arrow at each key; the circle at the playhead's key is filled; with an X-only key at 3 s a circle shows there too, once. Attach it to a moving parent: the path follows the parent's current place and the arrows are hidden. Deselect it: the path goes away. Lights, particle effects and decals with Position keys behave the same.
341. Select an actor with Position keys at 0 s and 2 s, playhead at 1 s, far from the camera: hover the 2 s key circle: a yellow ring shows from 12 px away; drag it: the playhead jumps to 2 s and the key follows the mouse, the path bends live; release: one undo step restores it; `Escape` mid-drag restores at once. Hover the key under the object's own gizmo: the gizmo wins. Drag the IK target: one undo step for the whole drag.
342. Hover an animation clip on a default-height row: its left edge lights as a bar over the whole height and the cursor is a left-right arrow; the body shows an outline and a hand; two small squares show at the top, inside the edges, and light up under the mouse. Press each: the edge trims, the square fades, the body moves; nothing else is ever taken. Zoom out until a clip is 10 px wide: its middle still moves it, and each edge can be grabbed just outside it. Do the same on a camera clip and a shake.
343. On **Animation 1** with three clips A, B, C: drag a long clip over B so that it covers it: B shows faint and hatched and is gone on release; move back before releasing: B returns. Drop a short clip into the middle of a long one: the long one is cut around it, both parts play what they played before. Drop it over A's end only: an X crossfade. `Escape` mid-drag and Undo after the drop both restore everything. `Ctrl` + drag a copy onto a neighbour: the same.
344. Drag an animation clip sideways along a row with the mouse wandering a few pixels up and down: it never changes rows. Drag it clearly onto the next row: it goes there; drag back: it returns.
345. Trim an animation clip's right edge into its neighbour's crossfade and on past the neighbour's end: the edge stops, a red tick shows at the limit, and no status error appears; drag back: it follows again. Resize a shake's edge across a third shake: the same.
346. On the Camera row with three shots: drag one over a shorter shot: that shot shows faint and is removed on release, its shakes stay in place; drop a short shot inside a long one: the long shot is cut around it and the view on both sides is as before; `Escape` restores. Hold `Ctrl`, press a shot's right edge and drag: it stretches with its keys (no corner triangle is drawn). Hover a pose diamond at a clip's start: in the middle of the row the diamond lights, near the top and bottom the edge does.
347. The **Tools** bar shows at the top left of the game view. Select an actor: `W`, `E`, `R`, `Q` switch the gizmo and the lit button follows; `X` flips **Local** / **World**. Hold the right button and press `W`: the camera flies and the gizmo does not change. **Snap** off: the gizmo drags freely and `Ctrl` snaps; on: the opposite. Right-click **Snap**: the four steps; change one, reopen the editor: kept. The inspector's Transform groups have no Gizmo or Steps rows. **Key**, **Bones**, **Helpers** and **Free cam** follow `K`, `B`, the View menu and the view.
348. Move the mouse over a clip edge, a fade handle, a crossfade, a pose diamond, the gizmo and the empty game view: the right end of the status line names the gestures for each; fly: it lists the fly keys. **View → Hints** off: no hints, and still off after reopening the editor.
349. Set **View → Frame lines** 2.39:1 and **Safe areas**, look through a shot, then hide the ImGui tools: the editor windows, the gizmo and the helpers go, the frame lines, mask and safe areas stay; play: they stay over the shot. Close the Cinema Editor and hide the tools again: no frame lines. **File → Render frames...** still writes images without them.
350. Select the Camera row on a clip with three Bezier keys, editor camera on: hover a key circle: yellow ring; drag it: the key follows the mouse, the path bends live, the playhead is on the key and the clip is focused; the turn at the key is unchanged; release: one undo step. Hold `Shift` while dragging: the key sits on the ground under the cursor. `Escape` mid-drag restores. Do the same with an actor's path key.
351. Drag a camera key close to its neighbour: the handles on that side shrink with it and the path makes no loop; drag it far away: they grow. A handle bent by hand keeps its direction.
352. Drag a tangent handle: it moves at once under the mouse with no gizmo, white while dragged; `Ctrl` at the press breaks the handles, `Shift` moves in fine steps, `Escape` restores; the object gizmo is back as soon as the button is released.
353. Hold `Ctrl` over a camera path between two keys: a small ring follows the path; click: a new diamond shows on the clip, the playhead is on it and the path keeps its place; undo removes it. The same on a selected actor's path adds Position keys.
354. With the Camera row selected, the path shows a dot per frame: close together where the camera is slow, far apart where it is fast; a white dot marks the playhead and moves while scrubbing. A selected actor with Position keys shows the same.
355. A clip with four keys at uneven distances: clip menu **Even speed**: the two inner diamonds move, the frame dots become evenly spaced with Linear keys, the first and last key and the path stay; undo restores. With two keys the item is grey.
356. Key a 300° pan with two keys: the camera wobbles or takes the long way. Tick **Shortest turn**: it turns the short way in one arc, in the view, in playback and through a crossfade; untick: as before. Save and reopen: `block_NNN_short_turn = on` and the tick are kept; an older scene opens with it off.
357. Look through a shot and press `V`: the editor camera shows from where it was left; `V` again: the shot. While typing a name, `V` types.
358. Two touching shots 200 m apart, temporal anti-aliasing on: play across the cut: the first frame of the second shot has no ghost of the first, and cutting from a dark interior to daylight shows the right exposure on the first frame. A slow dolly shows no flicker. A Hold key 100 m away inside one clip cuts as cleanly.
359. Scene settings: set **Cut distance** 0 and **Cut angle** 0: a jump inside one clip ghosts again, cuts between clips stay clean; Reset brings 5 m and 45°; save and reopen: `cut_jump` is kept only when changed.
360. **Render frames** across a far cut with **Cut warm-up** 8 and then 0: with 8 the progress window says warming up at the cut, the first written frame of the new shot is sharp and fully textured, and the frame count and the motion are the same as with 0 (no doubled or missing frame at the cut).
361. **World follows camera** on, a shot at a camp far from the player: its NPCs appear and act; off: the camp is empty. After the scene the player's surroundings are alive again and the player was never removed. Save and reopen: `world_follows_camera = on` is kept.
362. **Add → Weather**: a weather row with a 10 s clip appears, named after the game's weather, with **Time of day** ticked at the game's time; nothing changes on screen. Drag **Time of day** to 21: inside the clip the sky and light turn to evening, outside it the game's noon stays; scrub across the clip's edges: it switches.
363. Pick another weather in **Weather** (a storm): sky, fog and rain follow inside the clip. Set **Fade in** and **Fade out** to 3 s: colours and fog blend over 3 s and the sky switches half way. Mute the row: the game's weather is back.
364. Tick **Fog density**, key it 0.2 at the start and 0.9 at the end: the fog thickens while playing and the lane shows the curve; untick with keys present: the row stays active (keys count); remove the animation and untick: the weather's fog is back.
365. Tick **Sun heading** and **Sun height** and drag them: shadows swing and lengthen while the sky's time stays. **Sun brightness** 0: no sun light. Dynamic lighting only.
366. Two weather rows overlapping, the lower with **Fade in** 2 s and a ticked **Fog color**: over the overlap the fog colour blends from the upper row's to the lower row's. Save and reopen: `type = weather`, the weather name, the ticks and the values are kept; the game's own weather and clock are unchanged after closing the editor. Select only a weather row: no gizmo shows and the Tools bar buttons stay as they were.
367. **Add → Post**: a post row with a 5 s clip; nothing changes on screen. Press **Fade out**: **Fade** is ticked with two keys and the picture goes to black over the clip, in the editor camera, through a shot and in playback; **Fade in** reverses it; set **Fade color** white: it fades to white. After the clip the picture is normal at once.
368. Press **Black & white**: the picture inside the clip is grey; key **Gray** from 0 to 1: colour drains while playing. Tick **Blur** 0.5, **Noise** 0.3, **Duality H** 0.02: each shows; untick: each goes. Take a hit from an NPC inside the clip: the game's hit effect still shows on top.
369. Tick **Exposure** and drag it to -2 and +2: the picture darkens and brightens inside the clip only; `r2_autoexposure_bias` in the console still reads its old value. **Bloom** 1: strong glow. In static lighting these four rows change nothing.
370. Two overlapping post rows, the lower with **Fade in, out** 1 s and **Color base** 0.3: its darkening eases in and out over the upper row's look. Save and reopen: `type = post`, the ticks and values are kept. **Render frames** across a Fade out: the written frames go to black.
371. Pose an actor that plays an animation, IK on for `arm_r`, the hand bone selected so its gizmo sits on the IK target: hover the target: the bone gizmo turns grey and the target shows its ring; press and drag: the hand follows at once, one undo step; release: the target stays picked with its own gizmo. The same on a foot target next to the actor's gizmo.
372. IK on for `leg_l`: click the left foot bone, in the view or in the bone list: the IK target is picked instead with its gizmo, and its arrows and rings move and turn the foot; click the calf: the pole is picked. Turn `leg_l` off: the same clicks select the bones and their gizmo works.
373. Add several hundred camera Position keys at the same times on X, Y and Z, then a Rotation-only key at a new time. The Camera row still shows the new time; selecting and moving that diamond moves the corresponding curve keys. Repeated axis times must not consume the combined pose-key limit before they are merged.
374. Attach an actor to an animated parent and constrain a hand to another animated object. Bake **Final result** over a loop brace, once with the playhead at the start and once at the end: both OMF files contain the same motion. Onion-skin previews at neighbouring times also follow the animated dependencies; returning to the playhead restores the live pose.
431. Camera clip, Depth of field **Mode** Manual, **Lens** Physical: the rows are Autofocus, Focus, Aperture and Focal length (no Near range, Far range or Blur). **Aperture** f/1.4: background strongly blurred; f/16: nearly all sharp. Key Aperture from f/1.4 to f/16 across the clip and Focus from 1 m to 20 m: both animate while playing and while scrubbing paused, with no lag behind the playhead. **Lens** Classic: the old three rows return and look as before.
432. Physical lens, **Focal length** 0 and FOV keyed from 60 to 20: the blur grows as the FOV narrows. **Focal length** 85: stronger blur with unchanged framing. **Autofocus** on: the centre of the picture is sharp and refocuses when the camera pans to a nearer object. A crossfade between a Classic clip and a Physical clip switches the look at the middle without a frame of wrong blur.
433. Leave the clip (or close the editor): the game's own depth of field is back, `r4_ndof_mode` reads the value it had. With `r4_ndof_mode static` set in the console, a Physical clip still uses the physical lens. Save and reopen: `block_NNN_lens` keeps the lens, aperture, focal length and autofocus.
434. Console: `r4_ndof_sensor_height`, `r4_ndof_focus_point_x`, `r4_ndof_focus_point_y`, `r4_ndof_max_radius` and `r2_dof_lens_kernel_scale` are unknown commands; `r2_dof_kernel 5` and `10` change both the static blur and the largest bokeh size. The DOF options page of the main menu opens without the 3D scope blur slider, and the DOF Setup tool's General tab opens without the removed rows.
435. With `r4_mblur_quality` at Medium, tick **Motion blur** on a post row and set 1: fast camera moves inside the clip smear strongly; key it from 0 to 1: the smear grows while playing; outside the clip the game's own strength is back and `r4_mblur_power` in the console still reads its old value. With `r4_mblur_quality` Off the row changes nothing. A camera cut inside the clip shows no smear across the cut. Save and reopen: the tick and value are kept.
436. **File → Render frames...**: the **Name** box shows `frame` greyed and the output line ends in `frame_000000.tga`; type `take2`: the line ends in `take2_000000.tga`, and a space, a slash or a dot cannot be typed. Render: the folder gets `take2_000000.tga`, … next to the earlier `frame_…` files, which are untouched. Switch **Output** to AVI: with the name empty the hint is the scene name and the video is `<scene>.avi`; with `take2` it is `take2.avi` (a second part is `take2_2.avi`). Reopen the dialog: the name is still there.
