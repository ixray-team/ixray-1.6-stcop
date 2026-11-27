# Level viewers

**Game → Editors → Viewers** opens the available scene tools: Level Sandbox, OGF Preview, Material Editor, Particle Editor, and AI Editor. Opening a tool creates its session; no level is loaded when the game library starts. Closing the window stops that session and releases its scene resources.

These tools use the game's level, actor, physics, AI, UI, and rendering paths. They are useful for inspecting a small scene while the main menu or a normal game is also open. They are not separate processes, and shared device, file-system, sound, and immutable asset services still exist.

## Controls

- **Alt** toggles editor mouse capture. Use the viewport capture control before walking with the normal game movement keys.
- **Spawn actor** creates a local player without forcing one when the tool opens. **HUD and inventory** selects whether the normal player HUD is created.
- **Spawn at aim point** uses the section name in the editor. NPCs and monsters remain in a safe, non-navigating state when navigation data is unavailable.
- The viewport follows its docked size. Camera focus, rotation, weather, lighting, and presentation controls are available in the tool window.
- Each editor has its own settings and overlay switches. Changing the Level Inspector in the main game does not change a viewer, and changing a viewer does not change the main game's inspector.
- The OGF, material, and particle tools keep a bounded in-memory undo/redo window. Complete history is stored in the editor's `$appdata$` history file and can be reopened to traverse older records.

## Resources

Resource checkboxes select what a new session loads. **Resources** reports the current state of each resource: ready, not loaded, missing, or failed. Missing optional data is reported in the log and the session continues in a degraded mode; it does not mean that the whole level failed.

The live resource controls load and unload supported data independently: render geometry, collision, AI-map, game graph, patrol paths, level configuration, environment modifiers, particles, sounds, ambience, physics, spawn data, scripts, and HUD. Availability depends on the selected terrain or level. An AI-map without a matching graph or cross-table is still usable for local inspection, but navigation that requires the missing binding is disabled. Restoring an AI-map rebinds live NPCs to their nearest valid node when necessary, or leaves them without a path until a valid destination is available. Sound-geometry replacement is synchronized with live audio and AI occlusion queries; unloading it does not stop the sound mixer.

The sandbox can use flat terrain, analytic terrain, a height map, or an existing compiled level. The viewer is not a separate renderer: it uses the stock scene and rendering passes, including inspector geometry. Render-pass and wireframe controls affect only the selected viewport.

## Memory and history

The **Memory** section sets the GPU vertex/index/constant pool budget and the small CPU scratch arena. GPU and CPU resizing are separately opt-in. Resize requests at a frame boundary preserve the old pool when a new allocation cannot be created. The default GPU budget is 4 MiB; it can be changed before opening a session with `viewer_gpu_budget_kb`.

Scratch storage is reused each frame. Overlay data is streamed through bounded windows rather than accumulated in a large cache. History retains ten entries in memory; the complete append-only history remains in the editor's `$appdata$` directory.

## Verification and limits

- Tested with R4: opening a sandbox, clicking the real Spawn button, actor movement, HUD/inventory open and close, inspector isolation, pool resizing, resource combinations, and loading the controlled legacy save while the viewer remains alive.
- The lifecycle driver also exercises real dialogue, a stock task transition, tutorial flow, anomaly enter/leave, Jupiter save/reload, and Pripyat transition when those probes pass. These are focused gameplay checks, not a campaign completion.
- With each viewer while the main game is running, the OMF checks use normal SDL mouse events to open the real motion selector, choose its first non-additive motion inside the popup, and click **Reset motion** followed by **Idle**. If travel leaves the primary hands empty, the probe requests an existing weapon through its normal input path and waits for the HUD model. Editor-specific checks cover rotation, material application, particle playback/rate, AI navigation, and bounded history.
- The OGF, material, and particle checks click the real Undo and Redo buttons and verify the restored state before checking disk-backed history.
- The spawn/resource check uses real item, monster and NPC creation with the AI-map present, absent and restored, NPC combat without a player actor, and safe rejection with collision/physics disabled. Spawning is checked again after restoration.
- R1 and R2 are not verified by this setup. No claim is made that every level, quest, cutscene, renderer, or future resource format is covered.
- A missing required gameplay asset can disable the associated feature. Missing data is not a promise that every script or third-party addon will tolerate the same degraded mode.

For automated checks, use `viewer_lifecycle_test` and `level_selftest` from the normal game working directory. The lifecycle log contains `[editor-checks]`, `[lifecycle]`, and the actual gameplay probe results; inspect failures rather than treating a process that stayed open as a passing test.
