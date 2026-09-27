# Demo Record

> [!IMPORTANT]
> **Status**: Supported / In development (settings window) <br>
> **Minimal version**: 1.4 (settings window). The `demo_record` command is available since early versions

Demo Record is a built-in game tool for:

- recording camera flythrough paths (demos) with keyframes;
- taking screenshots, cubemaps and level maps;
- attaching the camera to model bones — both world objects and first-person hands/weapons.

---

## Starting and stopping

### How to start recording

Open the console (`` ` `` key) and type:

```
demo_record <file_name>
```

- the command only works on a loaded level;
- the console and the main menu close automatically;
- the result is saved to the game saves folder with the `.xrdemo` extension.

You can also move the camera to any point in the world:

```
demo_set_cam_position <x> <y> <z>
```

### What happens on start

- the first-person interface (hands, weapon) is hidden;
- the current field of view (FOV) is remembered and restored on exit;
- the camera starts from your current position and view angle;
- movement speeds are taken from the game settings (they can be changed in the Demo Record settings window).

### How to stop recording

Recording ends automatically when you:

- press `ESC`;
- the character dies;
- 1 hour passes (the recording time limit).

On exit everything returns to normal: interface, field of view, controls.

### Playing demos

Recorded keyframes can be played back: the game is launched with the special `demomode` key (see the "Launch keys" section).

---

## Camera modes

The camera has three mutually exclusive modes. The current mode is shown in the settings window.

### Free flight

The default mode: the camera flies around the scene like a free camera.

- **Mouse** — look around;
- **W/A/S/D** (or action bindings) — movement;
- **Q/E** — roll (tilt around the view axis).

Individual axes can be "locked" — the camera will keep the horizon level, look horizontally or keep its original heading (configured in the settings window, "Lock Axes" section).

### Look at a point

The camera always faces a chosen target: either a point in space or a model bone.

- enable with `J` (or by choosing the mode in the settings window);
- if a skeletal model is in the center of the screen — the camera will follow its bone;
- if the crosshair is over a surface — a point on it is locked;
- disable with `J` again.

Camera rotation is smooth (with inertia), with no sharp "jumps" over 180°. The camera can still be moved (W/A/S/D, up/down) — it travels while continuing to face the target.

### Bone attachment

The camera is fully attached to a bone of the selected model and moves with it. You can attach to:

- world objects (aim the crosshair and press `U`);
- first-person hands;
- the weapon in the left or right hand;
- an item playing an animation.

What you can do in this mode:

- **W/A/S/D**, crouch/jump — move the camera relative to the bone;
- **Q/E** — rotate the camera around the bone;
- **Mouse + `Shift` (either one)** — pan the view around the bone;
- **Z** — reset all offsets.

If the model disappears (destroyed, weapon holstered, etc.), the camera detaches automatically and returns to free flight. Detach manually with `U`.

---

## Input schemas

There are two control schemes. The **old** schema is active by default; the **new** one is enabled through the engine extension system (`engine_external`) — in the `engine_external.ltx` file in the `gamedata/configs` folder, in the `[gameplay]` section:

```ini
[gameplay]
NewDemoRecordInputSchema = true
```

The change takes effect after restarting the game. During recording you can also switch between schemas "on the fly" — in the settings window (Input → New input schema).

### New schema

| Action | Key |
| --- | --- |
| Move forward/left/backward/right | `W`/`A`/`S`/`D` |
| Down / up | Crouch / Jump |
| Roll left/right | `Q` / `E` |
| Look around | Mouse |
| Increase / decrease FOV | `R` / `T` |
| Change camera speed | Mouse wheel (with `Shift` — larger step) |
| Record a keyframe | `F` |
| Attach/detach bone | `U` |
| Lock look-at a point | `J` |
| Show skeleton | `K` |
| Reset FOV | `Z` |
| Pass control to the character | `0` |
| Help | `F1` |
| Level map / high-quality map | `F11` / `Ctrl+F11` |
| Screenshot | `F12` |
| Cubemap | `Backspace` |
| Quit | `ESC` |
| Pause | `Pause` |
| Console | `` ` `` |

A single movement speed applies to all directions; it is changed with the mouse wheel or in the settings window. The minimum speed is 0.01; below 1 the speed changes in small steps, above 1 in large steps (with `Shift` the step is even larger).

### Old schema

| Action | Key |
| --- | --- |
| Move | `W`/`A`/`S`/`D`, plus `LMB` (forward) and `RMB` (backward) |
| Slow movement | `Shift` (hold) |
| Fast movement | `Alt` (hold) |
| Acceleration | `Ctrl` (hold) |
| Roll | `Q` / `E` |
| Record a keyframe | `Space` |
| Everything else | Same as the new schema |

The three speed levels are configured in the settings window; the base speed (no modifiers) is fixed.

### Gamepad

| Control | Action |
| --- | --- |
| Left stick | Movement |
| Right stick | Camera rotation |
| Triggers | Down / up |
| `EAST` (B/Circle) | Quit recording |
| Left stick press | Toggle acceleration |
| Right bumper (`RIGHT_SHOULDER`) | Record a keyframe |

In bone attachment mode the sticks work differently: the left stick moves the camera relative to the bone, the right one rotates it around the bone.

### Passing control to the character (`0` key)

A useful mode for recording demos with a live character: all input (keyboard, mouse, mouse wheel and gamepad) is forwarded to the game character as if demo record weren't there, while the camera keeps writing the demo. Pressing `0` again returns control to the camera — the `0` key itself is never forwarded to the character, so you can always take control back.

---

## Keyframes

- recording: `F` (new schema), `Space` (old schema), right bumper (gamepad);
- each keyframe remembers the camera position and view at the moment of pressing;
- a marker appears in the world: a sphere with the `Keyframe #N` label;
- the keyframe sequence is saved to the demo file and played back in `demomode`;
- the keyframe count is visible in the `F1` help and in the settings window.

---

## Screenshots, cubemaps and level maps

All these operations run in several stages across frames, so the image may "blink" when the key is pressed — this is normal.

### Screenshot (`F12`)

Takes a screenshot without the first-person interface: the HUD is hidden for one frame, then restored.

### Cubemap (`Backspace`)

Sequentially captures six frames in six directions (+X/−X/+Y/−Y/+Z/−Z) and saves them as a set of numbered files. Used to create the environment (skybox/reflections) for game objects.

### Level map (`F11`)

- the game temporarily switches to a special mode and renders the whole level from above;
- the result is an image of the entire map in the `map_<level_name>` file;
- after the capture everything returns to normal.

### High-quality level map (`Ctrl+F11`)

Same, but the map is captured in parts — 4 tiles (`map_<level>#0` … `#3`) — to get a higher resolution than a single image allows. Map bounds are taken from the level settings (the `bound_rect` parameter of the `[level_map]` section) or computed automatically from level objects.

---

## Skeleton display (`K`)

When enabled, the skeleton of the model in the center of the screen (or closest to the view center) is drawn: bones and links between them. The skeletons of HUD models — hands and weapons — are also shown if visible. Useful for seeing which bone to attach the camera to.

---

## The "Demo Record" settings window

Opened from the top menu bar of the game: **Game → Demo record**. If no recording is active, the window shows "No active demo recording".

### Global

- **Slider step** — precision of the window sliders. Lower value = finer control. Ctrl+click a slider to type an exact value.

### Options

- **Disable time factor influence** — if enabled, camera movement is not affected by game time slowdown (e.g. slow motion or pause);
- **Draw skeleton** — the same as the `K` key.

### Input

- **New input schema** — toggles the old and new control schemes (which schema is active on start is set in `engine_external.ltx`);
- movement speeds: in the new schema — one common slider, in the old one — three (slow/fast/acceleration). Each has an `R` button to reset to the default value;
- **Controls reference** — a built-in cheat sheet of all hotkeys for the active schema.

### Camera

- **Mode** — camera mode selection: free flight / look at a point / bone attachment (the same actions as the `J` and `U` keys);
- **Smoothing** — rotation and movement smoothness (inertia). `0` = instant response, the closer to `1` the smoother and "softer" the camera;
- **Orientation** — direct input of camera rotation angles (unavailable in bone attachment mode);
- **Lock Axes** — axis locks: the camera always keeps the horizon / looks horizontally / keeps its heading;
- **Field of View** — the current FOV; the `R` button restores the value from the moment recording started;
- **Auto FOV change speed** — if enabled, the R/T FOV change speed depends on camera movement speed; if disabled, it is set manually.

### Bone Attachment

- if the camera is attached to a bone — the top of the section shows the bone itself and a **Detach** button;
- **Offsets** — camera offset and rotation relative to the bone (only in attachment mode), reset with `Z`;
- **World Objects** — the object in the center of the screen (or the object the camera is attached to): name and bone count. **Bone tree** — a tree of all model bones; clicking a bone attaches the camera to it. The current bone is marked with `[*]`;
- **Player HUD** — bone trees of first-person models: hands, weapons in both hands, the item with an active animation. Clicking a bone also attaches the camera. Shown while the corresponding model is visible on screen.

### State

- **Keyframes** — how many keyframes have been recorded;
- **Acceleration** — whether acceleration is enabled;
- **Redirect input** — where input goes: `DEMO RECORD` (camera) or `LEVEL` (character).

### Actions

Buttons duplicating the hotkeys: Screenshot, Cubemap, Level Map (standard and high quality).

---

## Lifecycle and feature interactions

1. **Start.** The `demo_record` command starts recording: the HUD is hidden, the FOV is remembered, the camera starts from the player's current view.
2. **Main loop.** Every frame the camera is updated according to the current mode (free flight / look at a point / bone attachment), with smoothing, axis locks and current speed settings applied.
3. **Keyframes.** At any moment you can record a keyframe — it is saved to the demo file and marked with a sphere in the world.
4. **Captures.** Screenshot, cubemap and level map are temporary operations: they take over the camera for a few frames, perform the capture and return control. While a capture is in progress, camera movement is not processed.
5. **Mode interactions.** Camera modes are mutually exclusive: bone attachment is unavailable while looking at a point and vice versa; leaving any mode returns to free flight. The settings window always reflects the current mode and lets you switch it without hotkeys.
6. **Stop.** On `ESC`, character death or the one-hour limit, recording ends: control returns to the game, the HUD and FOV are restored, the demo file is closed and stays in the saves folder.
