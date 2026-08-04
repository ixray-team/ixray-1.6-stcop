> [!IMPORTANT]
> **Status**: In development <br>
> **Minimum version**: rolling <br>
> **Last update**: 2026-07-31

# Sleep dialog (CUISleepWnd)

## Overview

The native sleep dialog `CUISleepWnd` loads layout from `configs/ui/ui_sleep_dialog.xml`. It keeps vanilla CoP layout compatibility and extends it through an optional `sleep_params` node and extra XML widgets.

The window is created with the HUD (`CUIGameSP`) and controlled from Lua via `get_hud()` / globals next to `show_sleep_dialog*`: open, query, session overrides, `ForceSleep` / `AbortSleep`, and veto through `actor_on_can_sleep`.

Without new API calls, behavior matches the previous native path (XML + presets).

> [!NOTE]
> The vanilla entry path `xr_effects.sleep` → `ui_sleep_dialog.sleep()` still opens the Lua dialog by default. To use the C++ window, route the entry point to `ShowSleepDialog` (see [Opening from scripts](#opening-from-scripts)).

> [!WARNING]
> Do not edit the legacy file in `gamedata/configs/ui/ui_sleep_dialog.xml` directly. Use XML Override / DLTX override in your addon.

## Base XML nodes

Required nodes (vanilla CoP):

| Node | Purpose |
|------|---------|
| `background` | Dialog background |
| `sleep_static` | Sky panorama (created twice from one node) |
| `static_cover` | Frame over panorama |
| `st_marker` | Selected-hours marker (child of `static_cover`) |
| `sleep_st_1` ... `sleep_st_24` | Hour labels (legacy) |
| `time_track` | Sleep duration track bar |
| `btn_sleep` | Confirm |
| `btn_cancel` | Cancel |

Optional:

| Node | Purpose |
|------|---------|
| `sleep_params` | All extended parameters |
| `sleep_hours_strip` | Auto hour strip instead of `sleep_st_*` |
| `sleep_preset_btn` | Preset button template |
| `st_time_now` | Current game time |
| `st_sleep_duration` | Sleep duration text |
| `st_wake_time` | Wake-up time |

## sleep_params: full attribute list

All attributes are optional. Without `sleep_params`, defaults match vanilla behavior.

```xml
<sleep_params
  panorama_texture="ui_inGame2_sky_panorama"
  panorama_mode="0"
  panorama_hours="24"
  panorama_bind="current"
  panorama_smooth_speed="0"
  hour_label_suffix="st_sleep_hours"
  warning_box_template="message_box_ok"
  warning_bleeding="sleep_warning_bleeding"
  warning_radiation="sleep_warning_radiation"
  warning_both="sleep_warning_all_pleasures"
  allow_sleep_with_bleeding="0"
  min_hours="1"
  max_hours="24"
  cam_anm="camera_effects\sleep.anm"
  pp_effector="sleep_fade.ppe"
  cam_id="10"
  pp_id="11"
  marker_min_x="5"
  restore_power="1"
  mute_music="1"
  mute_effects="1"
  preset_spacing="4"
  presets_confirm="0"
  fmt_time_now="%s"
  fmt_sleep_duration="%d%s"
  fmt_wake_time="%s"
  time_separator=":"
  time_precision="minutes"
  snd_open=""
  snd_close=""
  snd_sleep=""
  snd_cancel=""
  snd_track=""
  snd_preset=""
  snd_warning=""
/>
```

| Attribute | Type | Default | Description |
|-----------|------|---------|-------------|
| `panorama_texture` | texture id | `ui_inGame2_sky_panorama` | Panorama texture. Missing id falls back to vanilla |
| `panorama_mode` | `0` / `1` | `0` | Ignored (legacy attr) |
| `panorama_hours` | int >= 1 | `24` | Hours fitting the widget width |
| `panorama_bind` | `current` / `wake` | `current` | `current` = game hour; `wake` = wake hour (`now + duration`) |
| `panorama_smooth_speed` | float | `0` | Smooth wrap scroll (`0` = instant, vanilla) |
| `panorama_scale` | `stretch` / `native` | `stretch` | `stretch` = fit full day into window; `native` = 1:1 design px, window clips the strip |
| `panorama_tex_scale` | float >= 1 | `1` | Same as textures_descr file `scale` (HD: `2`) |
| `panorama_wrap` | `0` / `1` | `1` | `1` = dual-static wrap; `0` = clamp without seam (native) |
| `hour_label_suffix` | string table id | `st_sleep_hours` | Label suffix (`%d` + translated text) |
| `warning_box_template` | message box id | `message_box_ok` | Warning MB template |
| `warning_bleeding` | string table id | `sleep_warning_bleeding` | Bleeding warning text |
| `warning_radiation` | string table id | `sleep_warning_radiation` | Radiation warning text |
| `warning_both` | string table id | `sleep_warning_all_pleasures` | Both conditions text |
| `allow_sleep_with_bleeding` | `0` / `1` | `0` | `1` = bleeding does not block sleep |
| `min_hours` | int >= 1 | `1` | Track bar lower bound |
| `max_hours` | int | `24` | Track bar upper bound |
| `cam_anm` | path | `camera_effects\sleep.anm` | Cam effector |
| `pp_effector` | ppe name | `sleep_fade.ppe` | Postprocess effector |
| `cam_id` | int | `10` | Cam effector id |
| `pp_id` | int | `11` | PP effector id |
| `marker_min_x` | float | `5` | Marker X at 1 hour (when delta is 0) |
| `restore_power` | float | `1` | Actor power after sleep |
| `mute_music` | `0` / `1` | `1` | Mute music during sleep |
| `mute_effects` | `0` / `1` | `1` | Mute effects during sleep |
| `preset_spacing` | float | `4` | Gap between preset buttons |
| `presets_confirm` | `0` / `1` | `0` | `1` = preset click starts sleep immediately |
| `fmt_time_now` | printf / st id | `%s` | Current time format |
| `fmt_sleep_duration` | printf / st id | `%d%s` | Duration format (`%d` + suffix) |
| `fmt_wake_time` | printf / st id | `%s` | Wake time format |
| `time_separator` | char | `:` | Hours/minutes separator |
| `time_precision` | string | `minutes` | `hours` / `minutes` / `seconds` |
| `snd_*` | sound name | empty | UI sounds (see below) |

---

## Feature examples

### 1. Vanilla-compatible override with no extensions

Do not add `sleep_params`. Keep the CoP layout (`background`, `sleep_static`, `sleep_st_1..24`, `time_track`, buttons) via XML Override.

→ Behavior matches vanilla: split panorama, 1..24 hours, stock warning strings.

### 2. Custom panorama texture

```xml
<sleep_params panorama_texture="ui_mod_sky_panorama_hd"/>
```

→ Both panorama widgets use this texture. If TextureMaster has no such id, the engine logs a warning and falls back to `ui_inGame2_sky_panorama`.

### 3. Smooth wrap panorama with hour binding

Dual-static wrap (`panorama_wrap="1"`) is the vanilla strip cut. A solid 24h texture scrolls inside the `sleep_static` window.

For HD strips with strong landmarks on both edges (moon at 0 and at 24), wrap shows a visible double. Use:

```xml
<sleep_params
  panorama_texture="ui_sleep_wnd_new_panorama"
  panorama_hours="24"
  panorama_bind="wake"
  panorama_smooth_speed="10"
  panorama_scale="native"
  panorama_tex_scale="2"
  panorama_wrap="1"
/>
```

→ `panorama_wrap="1"` gives continuous dual-static scroll through midnight. If the same moon sits on both strip edges, the viewport shows a double: keep the landmark on one edge only. `panorama_wrap="0"` clamps without a seam (possible jump at the day boundary). `panorama_bind="wake"` tracks wake time.

### 4. Limit sleep duration

```xml
<sleep_params min_hours="2" max_hours="8"/>
```

→ Track bar accepts 2..8 only. Presets outside the range are skipped.

For a one-shot limit on a single show (quest / specific bed) without editing XML, use the session API `SetSleepHoursRange` (see [Session overrides](#session-overrides)).

### 5. Auto hour strip instead of 24 nodes

Replace `sleep_st_1` ... `sleep_st_24` with one node:

```xml
<sleep_hours_strip
  x="40" y="163" width="591" height="15"
  count="12"
  padding="0"
  spacing="0"
  complex_mode="0"
>
  <text align="c" vert_align="c" font="letterica16" r="80" g="80" b="80"/>
</sleep_hours_strip>
```

Strip attributes:

| Attribute | Description |
|-----------|-------------|
| `count` | Label count (1..24), default 24 |
| `padding` / `pad_left` / `pad_right` | Inner padding |
| `spacing` | Gap between labels |
| `label_width` | Fixed width; if `0`, computed automatically |

→ Labels are filled as `(current_hour + i + 1) % 24` + `hour_label_suffix` (same as vanilla Lua).

### 6. Time info labels

```xml
<sleep_params
  fmt_time_now="st_sleep_now_fmt"
  fmt_sleep_duration="st_sleep_dur_fmt"
  fmt_wake_time="st_sleep_wake_fmt"
  time_separator=":"
  time_precision="minutes"
/>

<st_time_now x="40" y="190" width="180" height="16" format="Now: %s">
  <text font="letterica16" align="l" r="170" g="170" b="170"/>
</st_time_now>

<st_sleep_duration x="230" y="190" width="180" height="16">
  <text font="letterica16" align="c" r="170" g="170" b="170"/>
</st_sleep_duration>

<st_wake_time x="420" y="190" width="180" height="16" format="Wake: %s">
  <text font="letterica16" align="r" r="170" g="170" b="170"/>
</st_wake_time>
```

→ Node `format` overrides the matching `fmt_*` from `sleep_params`. Values refresh when the track bar changes.

String table examples:

```xml
<text id="st_sleep_now_fmt">Now: %s</text>
<text id="st_sleep_dur_fmt">Sleep: %d%s</text>
<text id="st_sleep_wake_fmt">Wake: %s</text>
```

### 7. Warnings and sleep while bleeding

```xml
<sleep_params
  warning_box_template="message_box_ok"
  warning_bleeding="st_mod_sleep_bleed"
  warning_radiation="st_mod_sleep_rad"
  warning_both="st_mod_sleep_both"
  allow_sleep_with_bleeding="1"
/>
```

→ With `allow_sleep_with_bleeding="1"`, bleeding does not show a warning and does not block the dialog. Radiation still shows a warning MB instead of the sleep dialog.

### 8. Custom cam / PP / power restore / mute

```xml
<sleep_params
  cam_anm="camera_effects\mod_sleep.anm"
  pp_effector="mod_sleep_fade.ppe"
  cam_id="10"
  pp_id="11"
  restore_power="0.85"
  mute_music="1"
  mute_effects="0"
/>
```

→ On confirm: `disable_ui`, cam+pp, info `actor_is_sleeping`, mute by flags. After the second cam phase: `enable_ui`, restore volumes, info `tutorial_sleep`, clear `actor_is_sleeping` / `sleep_active`.

### 9. UI sounds

```xml
<sleep_params
  snd_open="interface\sleep_open"
  snd_close="interface\sleep_close"
  snd_sleep="interface\sleep_confirm"
  snd_cancel="interface\sleep_cancel"
  snd_track="interface\sleep_tick"
  snd_preset="interface\sleep_preset"
  snd_warning="interface\sleep_warning"
/>
```

| Sound | When |
|-------|------|
| `snd_open` | Successful dialog open |
| `snd_warning` | Warning MB shown |
| `snd_sleep` | Sleep button |
| `snd_cancel` | Cancel (falls back to `snd_close` if empty) |
| `snd_close` | Cancel fallback |
| `snd_track` | Track bar step (keyboard / gamepad) |
| `snd_preset` | Preset click |

### 10. Hour preset buttons

XML template (required, otherwise presets are not built):

```xml
<sleep_params preset_spacing="6" presets_confirm="0"/>

<sleep_preset_btn x="40" y="212" width="48" height="24" check_mode="0">
  <text font="letterica16" align="c"/>
  <texture>ui_inGame2_Mp_bigbuttone</texture>
</sleep_preset_btn>
```

Lua (once at mod start or before show):

```lua
local hud = get_hud()
hud:SetSleepHourPresets({ 1, 3, 6, 8, 12 })
hud:ShowSleepDialog()
```

→ Buttons are laid out to the right of template `x` with step `width + preset_spacing`. Hours outside `min_hours`..`max_hours` are skipped.

Instant sleep on preset:

```xml
<sleep_params presets_confirm="1"/>
```

```lua
get_hud():SetSleepHourPresets({ 6, 8 })
get_hud():ShowSleepDialog()
```

→ Preset click hides the dialog and starts the sleep pipeline immediately.

Clear:

```lua
get_hud():ClearSleepHourPresets()
```

### 11. Open with a preselected duration

```lua
get_hud():ShowSleepDialogAtHour(6)
-- or
show_sleep_dialog_at_hour(6)
```

→ Value is clamped to `min_hours`..`max_hours`, then the dialog (or warning) is shown.

### 12. Full ready mod layout (compact)

Example XML Override `ui_sleep_dialog.xml` with strip, time labels and presets:

```xml
<w>
  <sleep_params
    panorama_mode="0"
    min_hours="1"
    max_hours="12"
    preset_spacing="4"
    presets_confirm="0"
    fmt_time_now="Now: %s"
    fmt_sleep_duration="Sleep: %d%s"
    fmt_wake_time="Wake: %s"
    snd_track="interface\inv_slot"
  />

  <background x="173" y="220" width="677" height="310">
    <texture>ui_inGame2_message_box</texture>
  </background>

  <sleep_static x="40" y="32" width="591" height="128">
    <texture>ui_inGame2_sky_panorama</texture>
  </sleep_static>

  <static_cover x="37" y="32" width="601" height="128">
    <texture>ui_inGame2_panorama_window</texture>
  </static_cover>

  <st_marker x="0" y="0" width="30" height="118">
    <texture>ui_inGame2_marker</texture>
  </st_marker>

  <sleep_hours_strip x="40" y="163" width="591" height="15" count="12" complex_mode="0">
    <text align="c" vert_align="c" font="letterica16" r="80" g="80" b="80"/>
  </sleep_hours_strip>

  <time_track x="37" y="177" width="601" height="16" is_integer="1" step="1">
    <options_item entry="g_sleep_time" group="sleep" depend="runtime"/>
  </time_track>

  <st_time_now x="40" y="198" width="180" height="16">
    <text font="letterica16" align="l" r="170" g="170" b="170"/>
  </st_time_now>
  <st_sleep_duration x="230" y="198" width="180" height="16">
    <text font="letterica16" align="c" r="170" g="170" b="170"/>
  </st_sleep_duration>
  <st_wake_time x="420" y="198" width="180" height="16">
    <text font="letterica16" align="r" r="170" g="170" b="170"/>
  </st_wake_time>

  <sleep_preset_btn x="40" y="222" width="52" height="24" check_mode="0">
    <text font="letterica16" align="c"/>
    <texture>ui_inGame2_Mp_bigbuttone</texture>
  </sleep_preset_btn>

  <btn_sleep x="209" y="260" width="127" height="28" check_mode="0">
    <window_name>button_yes</window_name>
    <text font="letterica18">sleep_ok_button</text>
    <texture>ui_inGame2_Mp_bigbuttone</texture>
  </btn_sleep>

  <btn_cancel x="341" y="260" width="127" height="28" check_mode="0">
    <window_name>button_no</window_name>
    <text font="letterica18">Btn_Cancel</text>
    <texture>ui_inGame2_Mp_bigbuttone</texture>
  </btn_cancel>
</w>
```

Preset init script:

```lua
function bind_sleep_presets()
  local hud = get_hud()
  if hud and hud.SetSleepHourPresets then
    hud:SetSleepHourPresets({ 1, 2, 4, 6, 8, 12 })
  end
end
```

---

## Opening from scripts

### Syntax: HUD API and globals

Methods are on `get_hud()` (`CUIGameCustom`). Several globals mirror common calls.

#### Dialog

| Method / global | Description |
|-----------------|-------------|
| `IsSleepDialogReady` / `is_sleep_dialog_ready` | Layout loaded (`time_track` present) |
| `IsSleepDialogShown` / `is_sleep_dialog_shown` | Dialog is on screen |
| `ShowSleepDialog` / `show_sleep_dialog` | Open with current track value |
| `ShowSleepDialogAtHour(n)` / `show_sleep_dialog_at_hour(n)` | Open with `n` hours selected |
| `HideSleepDialog` / `hide_sleep_dialog` | Close as Cancel |
| `CancelSleepDialog` / `cancel_sleep_dialog` | Same as Hide |
| `GetSleepSelectedHours` / `get_sleep_selected_hours` | Current hours on the track |
| `SetSleepSelectedHours(n)` / `set_sleep_selected_hours(n)` | Set hours (without opening, or while open) |
| `ConfirmSleep` / `confirm_sleep` | Programmatic Sleep button (if dialog is shown) |
| `SetSleepHourPresets({...})` | Set presets (number table) |
| `ClearSleepHourPresets` | Clear presets |

There is no Lua access to the `CUITrackBar` widget itself: only hours value, range, and presets.

#### Sleep lifecycle

| Method / global | Description |
|-----------------|-------------|
| `IsActorSleeping` / `is_actor_sleeping` | `m_camPhase != 0` and/or info `actor_is_sleeping` |
| `GetSleepPhase` / `get_sleep_phase` | `0` idle, `1` fade-in before time jump, `2` fade-out after jump |
| `ForceSleep(n)` / `force_sleep(n)` | Sleep without UI: set hours and Confirm immediately |
| `AbortSleep` / `abort_sleep` | Cancel: dialog = Cancel; phase 1 = remove cam/PP with no time jump; phase 2 = early WakeUp (time already advanced) |

#### Session overrides

Applied on top of XML `sleep_params` for one show / one `ForceSleep`. Cleared on WakeUp, Cancel/Hide, Abort, and warning-box OK without sleep.

| Method | Description |
|--------|-------------|
| `SetSleepHoursRange(min, max)` / `ClearSleepHoursRange` | Temporary track min/max |
| `SetSleepAllowBleeding(bool)` / `ClearSleepAllowBleeding` | Allow sleep while bleeding |
| `SetSleepRestorePower(float)` / `ClearSleepRestorePower` | Power after sleep |
| `SetSleepMute(mute_music, mute_effects)` / `ClearSleepMute` | Mute during sleep |
| `ClearSleepSessionOverrides` | Clear all session fields at once |

Not exposed as runtime API (stay in XML): panorama, `fmt_*`, cam/ppe names, UI sounds, button layout.

#### Sleep block

| Method / global | Description |
|-----------------|-------------|
| `SetSleepBlocked(bool [, warning_text])` / `set_sleep_blocked(...)` | C++ flag; when `true`, dialog does not open (same as veto) |

### Examples

Scenario 1: quest bed limited to 1..3 hours

```lua
get_hud():SetSleepHoursRange(1, 3)
get_hud():SetSleepHourPresets({ 1, 2, 3 })
show_sleep_dialog()
```

→ Track and presets stay in 1..3 until Cancel / WakeUp / Abort.

Scenario 2: cutscene without the window

```lua
force_sleep(6)
```

→ Starts phase 1 immediately (disable_ui + cam/pp), no dialog.

Scenario 3: alarm during fade-in

```lua
if get_hud():GetSleepPhase() == 1 then
  get_hud():AbortSleep()
end
```

→ Cam/PP removed with no time jump; UI/volumes restored; `actor_on_sleep_aborted(1)` is sent.

Scenario 4: set hours while the dialog is open

```lua
get_hud():SetSleepSelectedHours(4)
get_hud():ConfirmSleep()
```

### Route vanilla entry to the C++ window

In an override of `ui_sleep_dialog.script` (or your sleep entry):

```lua
function sleep()
  show_sleep_dialog()
end
```

Or from an effect / zone:

```lua
function open_sleep_from_bed(actor, obj)
  get_hud():ShowSleepDialog()
end
```

> [!WARNING]
> Do not call Lua callbacks (`SendScriptCallback`) from inside an already active Lua effect stack (`xr_effects` → tutorial `use`) if the callback re-enters engine Lua. Open the window with a direct `ShowSleepDialog` call without nested luabind proxies. `actor_on_can_sleep` is invoked from C++ via raw `lua_State` + `SendScriptCallback` (no nested luabind functor).

---

## Script callbacks

| Callback | When | Can stop | Args |
|----------|------|----------|------|
| `actor_on_can_sleep` | Before dialog show (after bleed/rad, only on successful open path) | yes (`flags.allow`) | `hours`, `flags` |
| `actor_on_before_sleep` | After first cam phase, before time forward | no | `hours` |
| `actor_on_sleep` | After time forward, weather and `SetPower` | no | `hours` |
| `actor_on_sleep_aborted` | After `AbortSleep` | no | `phase` (`0` dialog, `1` fade-in, `2` fade-out) |

```lua
RegisterScriptCallback("actor_on_before_sleep", function(hours)
  printf("before sleep: %s h", hours)
end)

RegisterScriptCallback("actor_on_sleep", function(hours)
  printf("slept: %s h", hours)
end)

RegisterScriptCallback("actor_on_sleep_aborted", function(phase)
  printf("sleep aborted, phase=%s", phase)
end)
```

### actor_on_can_sleep

Called from `TestAndShow` after bleeding/radiation checks. If bleed/rad already showed a warning, can_sleep is not called.

```lua
-- flags = { allow = true, warning_text = "" }
RegisterScriptCallback("actor_on_can_sleep", function(hours, flags)
  if in_danger_zone() then
    flags.allow = false
    flags.warning_text = "st_cannot_sleep_here"
  end
end)
```

→ If `allow == false`: non-empty `warning_text` shows a warning MB; otherwise the dialog simply does not open.

Without a callback subscription: `SetSleepBlocked(true, "st_cannot_sleep_here")` is checked together with can_sleep.

See also [IXR signals](../scripting/ixr-framework/modules/ixr_signals.md).

---

## Sleep pipeline (short)

1. Open: bleeding/radiation → (if ok) `SetSleepBlocked` + `actor_on_can_sleep` → warning MB or `ShowDialog`.
2. Confirm / ForceSleep: `xr_effects.disable_ui`, cam+pp, `actor_is_sleeping`, mute.
3. End of first cam: `actor_on_before_sleep` → second cam → `ChangeGameTime` → weather/surge managers → `SetPower` → `actor_on_sleep`.
4. End of second cam: `enable_ui`, unmute, `tutorial_sleep`, clear sleep infos, clear session overrides.
5. AbortSleep: see lifecycle table; then `actor_on_sleep_aborted(phase)`.

Surge/weather:

1. Before sleep: `surge_manager.skip_message = false`.
2. After time forward: `time_forwarded = true`, `forced_weather_change`.
3. If surge started and `weather_fx` exists: `StopWFX` + weather change again.

---

## Gamepad

| Action | Behavior |
|--------|----------|
| `kUI_LEFT` / `kUI_RIGHT` | Track step + `snd_track`, with ActionRepeaters hold |
| `kUI_ACCEPT` | Same as `btn_sleep` |
| `kUI_BACK` | Same as `btn_cancel` |

---

## Recommendations

✔️ Correct usage:

1. Change UI via XML Override / DLTX, do not touch legacy `gamedata`.
2. Prefer `sleep_hours_strip` over 24 manual nodes for compact layouts.
3. Set presets from Lua after HUD is ready.
4. Open the C++ window via `ShowSleepDialog` when you need the new features.
5. One-shot quest limits via `SetSleepHoursRange`; permanent layout via XML `min_hours`/`max_hours`.
6. Cutscenes without UI via `ForceSleep`; cancel before time jump via `AbortSleep` in phase 1.

⚠️ Limitations:

1. Vanilla `ui_sleep_dialog.sleep()` is still Lua by default.
2. Without `sleep_preset_btn`, `SetSleepHourPresets` creates no buttons.
3. Without `time_track`, layout is considered uninitialized and `ShowSleepDialog` is skipped (with a log).
4. No Lua access to the track bar widget itself (hours / range / presets only).
5. `ForceSleep` bypasses `actor_on_can_sleep` / `SetSleepBlocked` (UI skip).

✖️ Anti-patterns:

1. Editing the original `gamedata/configs/ui/ui_sleep_dialog.xml` in the engine repo.
2. Calling `ShowSleepDialog` from a deeply nested Lua stack wrapped in extra luabind proxies.
3. Expecting `sleep_st_*` to work while `sleep_hours_strip` is present (strip wins).
4. Expecting time rollback on `AbortSleep` in phase 2 (time already advanced).

## Related

[UI overview](ui-advanced-features.md), [IXR signals](../scripting/ixr-framework/modules/ixr_signals.md)
