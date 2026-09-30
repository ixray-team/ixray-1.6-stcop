> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.4 <br>
> **Last updated**: 2026-09-28

# Horizontal compass, minimap and motion icon features for the minimap

## Overview

This feature configures the HUD navigation block: the minimap or the horizontal compass. The motion icon works next to this block and shows the actor's movement state and visibility.

The compass bar is an **optional** UI element. Until it is activated via `SetNavigationMode(true)` or the deprecated boot hint `UseCompassBar`, it is not created, does not load `compass_bar.xml`, and does not affect the minimap, motion icon, or PDA online.

Switching between the minimap and compass bar happens at runtime without reloading the level.

## Default mode and runtime switching

1. Engine default: **minimap**.
2. `UseCompassBar` in `configs/engine_external.ltx` (**deprecated**) is a boot-time hint for mods without Lua. Prefer the Lua API or IXR Options.
3. `hud_minimap` controls **visibility** of the active navigation block.
4. Runtime switching: `ActorMenu.get_maingame():SetNavigationMode(bool)`, where `true` is compass bar and `false` is minimap.
5. Persisting the navigation mode in save/user.ltx is **not implemented**.

## Lua API

```lua
local maingame = ActorMenu.get_maingame()
if maingame then
    maingame:SetNavigationMode(true)   -- compass bar (lazy init)
    maingame:SetNavigationMode(false)  -- minimap
    local isCompass = maingame:IsCompassBarMode()
end
```

Readonly fields `UIZoneMap` and `UICompassBar` are available on `CUIMainIngameWnd`. `UICompassBar` may be `nil` until the compass bar is activated.

`UICompassBar.visible` is synced with `Show()` so a hidden compass is skipped by the child-walk `Update`.

## compass_bar.xml unit contract

Layout and positioning (`x` `y` `width` `height` `padding` `offset_*` `size`) use parent-relative fractions only. Modern XML must not use absolute px.

Silent legacy for stock: `*_px` / `size_px` / `draw_offset_*`, or shared attrs with `abs(v) > 1`, read as UI px. Stock `gamedata` needs no edits.

Non-layout (unchanged units): `fov_angle`, speeds, alpha/tint, `circumference_px` / `tex_width`, `altitude_deadzone`, fonts, angles.

| Node | Attributes | Units | Notes |
|------|------------|-------|-------|
| `compass_bar` | `x` `y` `width` `height` | UI base fractions | always relative |
| `background` / `dial` / `strip` | `x` `y` `width` `height` | bar fractions | |
| `dial:texture` | `width`/`height` | scale vs clip (when `stretch`) | not atlas crop; silent: `scale_*` |
| `dial:texture` | `x`/`y` | clip fractions | silent px: `offset_*_px` / `draw_offset_*` / `abs>1` |
| `dial` / `strip` | `circumference_px` / `tex_width` | logical circumference px | not layout |
| `cardinals` hosts | `x` `y` `width` `height` | dial clip fractions | |
| tick / marker | `size` / `width`/`height` / `offset_y` | host fractions | silent: `size_px` / `offset_y_px` |
| `spots` offsets | `x`/`y` | dial clip fractions | |
| `spots:defaults` | `size` / `width`/`height` | dial clip fractions | silent: `size_px` |
| `active_target` | `width`/`height` | bar fractions | |
| `active_target` | `offset_y` / `padding` | bar height / strip width fractions | silent: `*_px` |
| `active_target` children | `x` `y` `width` `height` | UI base fractions | marker / distance_text / altitude_arrow |
| `altitude_arrow` | `altitude_deadzone` | meters | not layout |

## Modern schema (optional)

Dual-read parser: modern relative attrs win. Stock stays via silent px fallback.

| Modern | Legacy alias | Where |
|--------|--------------|-------|
| `compass_bar:dial` | `compass_bar:strip` / `compass_dial:strip` | dial path |
| texture `width`/`height` | `scale_*` / `draw_scale*` | dial texture draw scale |
| texture `x`/`y` (relative) | `offset_*_px` / `draw_offset_*` | dial texture draw offset |
| `circumference_px` | `tex_width` | strip/dial (not layout) |
| `loop` | `tex_loop` | strip/dial |
| `heading_bias_deg` / `phase_deg` | - | dial UV phase |
| `fit="parent"` | full-bleed force | background |
| `dial:draw` / `background:draw` | `*:texture` | look-only visuals |
| `offset_y` | `offset_y_px` / `active_offset_y` | active_target |
| `padding` | `padding_px` / `active_target_padding` | active_target |
| `smoothing` | `smoothing_speed` | active_target |
| `size` / `width`/`height` | `size_px` | marker / spots defaults |
| `offset_y` | `offset_y_px` | cardinal marker/tick |
| `spots:defaults` | `spots:spot_template` | spot size/shadow |
| `style_sheet` | local `<shadows>` | default shadow |
| `cardinals` + `<point id angle_deg text>` | `cardinal_points` / `main_cardinals` / `inter_cardinals` | directions |

## Scale labels

Labels are split into three groups. Each has its own `show` and `<text>` style.

```xml
<cardinals>
  <main show="true">
    <text font="font_rubik_16" .../>
    <point id="n" text="N" r="227" g="79" b="56"/>
    <point id="e" text="E" align="r"/>
    <point id="s" text="S"/>
    <point id="w" text="W" align="l"/>
  </main>
  <intermediate show="true">
    <text font="font_rubik_12" .../>
    <point id="ne" text="NE"/>
    <point id="se" text="SE"/>
    <point id="sw" text="SW"/>
    <point id="nw" text="NW"/>
  </intermediate>
  <degrees show="true" step="15" y="0.25">
    <text font="font_rubik_12" .../>
  </degrees>
  <tick .../>
</cardinals>
```

| Node | Purpose |
|------|---------|
| `main` | `N E S W`; `show` toggles |
| `intermediate` | `NE SE SW NW`; shared text style on the group |
| `degrees` | numeric scale; `show`, `step`, `y` |
| `point` | caption/color/align override on top of group style |

Defaults: main on, intermediate off, degrees off, `step=30`.

Priority: code defaults → INI `[compass]` → flat attrs (`show_cardinal`...) → groups `main`/`intermediate`/`degrees`.

If groups or flat attrs / INI are present, labels are generated. Otherwise legacy `<point>` / `main_cardinals` is used.

Coincidence rule: with `main` enabled, labels `0°/90°/180°/270°` are skipped.

INI example (DLTX):

```ini
[compass]
show_cardinal = true
show_degrees = true
show_intermediate_cardinal = false
degree_step = 30
```

Modern fragment example:

```xml
<compass_bar x="0.5" y="0.07" width="0.5" height="0.04" fov_angle="45">
  <style_sheet>
    <shadows thickness="0.5" r="0" g="0" b="0" a="100"/>
  </style_sheet>
  <dial x="0.5" y="0.4" width="1.0" height="1.0" circumference_px="1024" loop="1">
    <texture x="0" y="0.02" width="0.8" height="0.12">ui_inGame2_compass_dial</texture>
  </dial>
  <cardinals>
    <point id="n" text="N"/>
    <point id="e" text="E" align="r"/>
  </cardinals>
  <spots show="1">
    <defaults size="0.08 0.08"/>
  </spots>
  <active_target offset_y="0.03" padding="0.02" smoothing="10"/>
</compass_bar>
```

Texture names are read from the `texture=` attribute or child text **up to the first `<`** (guards against nested markup in text nodes).

## Atlas and compass_bar.xml components

### compass_bar root

| Attribute | Purpose | Default |
|-----------|---------|---------|
| `fov_angle` | Strip field of view in degrees | `45` |
| `fade_in_speed` | Spot fade-in speed | `6` |
| `fade_out_speed` | Spot fade-out speed | `5` |
| `min_visible_alpha` | Minimum visible alpha threshold | `0.01` |
| `fov_fade_inner` | Inner FOV edge fade boundary | `0.30` |
| `fov_fade_outer` | Outer FOV edge fade boundary | `0.70` |
| `fov_fade_edge_lo` | Lower normalized fade edge | `0.05` |
| `fov_fade_edge_hi` | Upper normalized fade edge | `0.95` |

### background

Purpose: panel background and frame. Does not affect mark projection or dial UV.

Geometry relative to `compass_bar`:

| Attribute | Purpose | Default |
|-----------|---------|---------|
| `x` `y` `width` `height` | parent-relative fractions | `0 0 1 1` |
| `fit` | `parent` / `bar` fills the whole bar | |
| `alignment` / `align` | `l` or `c` | `l` |

Visuals: `background:draw:texture` (legacy: `background:texture`).

### dial / strip

Functional vs draw split:

**Functional** (`dial`/`strip` root) drives compass behavior:

| Attribute | Purpose | Default |
|-----------|---------|---------|
| `x` `y` `width` `height` | clip/viewport for marks and UV | |
| `circumference_px` / `tex_width` | logical circle length in px | `1024` |
| `loop` / `tex_loop` | seamless wrap / clamp | `1` |
| `heading_bias_deg` / `phase_deg` | scale phase | `0` |
| `fov_angle` (on `compass_bar`) | mark projection FOV | `45` |

**Draw** (`dial:draw` / `dial:draw:texture`, legacy `dial:texture`) is look-only:

| Attribute | Purpose |
|-----------|---------|
| texture name | strip art |
| `width`/`height` | draw size inside the clip (scale when `stretch`) |
| `x`/`y` | draw offset as clip fractions |
| `stretch` | static stretch |
| `a` `r` `g` `b` / `color` | tint |

UV window uses clip width (`dial` size), not texture `width`/`height`. So draw scale/offset/tint do not move marks or change FOV.

### cardinal_points

Purpose: text direction labels.

| Attribute | Purpose | Default |
|-----------|---------|---------|
| `fake_target_distance` | Projection distance for N/E/S/W labels | `1000` |

### spots

| Attribute | Purpose | Default |
|-----------|---------|---------|
| `collect_interval` | Map spot collect interval in seconds | `0.1` |
| `show` | Show spots on the strip | `1` |

### active_target

Purpose: selected target marker, distance, and vertical offset.

**Functional** (root and child layout) drives behavior:

| Attribute | Purpose | Default |
|-----------|---------|---------|
| `show` | enable/disable the whole block | `true` if node exists |
| `offset_y` | container vertical offset, bar-height fraction | `0` |
| `padding` | strip edge padding, strip-width fraction | |
| `smoothing` | container motion smoothing | `10` |
| `altitude_deadzone` | altitude arrow threshold, m | `1.8` |
| `width` `height` | container size, bar fractions | |

Child nodes: `marker`, `altitude_arrow`, `distance_text` each have their own `show` and layout (`x/y/width/height` as UI base fractions).

**Draw** (`*:draw`) is look-only and does not change strip projection:

```xml
<active_target offset_y="0.03" padding="0.02" smoothing="10" altitude_deadzone="1.8">
  <marker show="true" x="0" y="-0.02" width="0.04" height="0.05">
    <draw>
      <texture stretch="1">ui_inGame2_hint_wnd_main_window</texture>
      <shadows .../>
    </draw>
  </marker>
  <altitude_arrow show="true" x="-0.004" y="-0.023" width="0.012" height="0.016">
    <draw stretch="1" texture_up="..." texture_down="...">
      <shadows .../>
    </draw>
  </altitude_arrow>
  <distance_text show="true" x="0.007" y="-0.026" width="0.078" height="0.018">
    <text font="..." .../>
  </distance_text>
</active_target>
```

Legacy without `<draw>` / `show` remains valid.

#### distance_text

| Attribute | Purpose | Default |
|-----------|---------|---------|
| `format` / `text_format` | sprintf distance format | `"%.0f m"` |
| `st_format` | String table ID instead of format | - |

## Acceptance invariants

Compass Bar refactors must preserve:

1. Default is minimap; compass is not created until activated.
2. Missing `compass_bar.xml` soft-fails and keeps minimap working.
3. Minimap and compass are mutually exclusive.
4. The `compass_bar.xml` unit contract above stays intact, including legacy aliases.
5. Spot enable: `(ShowOnCompass || HasCompassConfig) && SpotEnabled && same level && texture`.
6. Active task is not duplicated in the spot pool.
7. Heading is camera yaw.
8. `hud_minimap` controls visibility of the active nav block.
9. Lua API `SetNavigationMode` / `IsCompassBarMode` / `UICompassBar` stays compatible.
10. Ownership smoke (`RunNavigationOwnershipSmoke`) must pass after mode switches.

## Motion icon

### Legacy (posture / awareness)

1. `state_normal`, `state_crouch`, `state_creep`, `state_climb`, `state_run`, `state_sprint` show the current movement type.
2. `power_progress` shows stamina.
3. `luminosity_overlay` and `noise_overlay` apply visual noise and dimming.
4. Luminosity/noise overlays are created for minimap mode and hidden in compass bar mode. When switching back to the minimap, overlays are restored without recreating the HUD.

### Status glow (opt-in, CUIMotionIcon)

Shared soft-glow status lives in `CUIMotionIcon`, not on the compass bar. One white texture (`background` / `status_icon` in `motion_icon.xml`); color and intensity come from state.

Opt-in: without `[motion_icon] enabled` / `status_tint` and without a tintable static the feature stays off. Stock `motion_icon.xml` without background is unchanged.

State priority: `Enemy > Anomaly > SafeZone > None`.

| State | Source | Look |
|-------|--------|------|
| Enemy | `GetThreatNormalized` (`SetActorVisibility`) | red + optional pulse |
| Anomaly | `CActorCondition::GetZoneDanger` | orange |
| SafeZone | actor inside a restrictor from `safe_zones` (`Position` probe, same as `actor_in_zone`); empty list disables SafeZone; legacy: `safe_zone_source = camp` | green |
| None | otherwise | alpha 0 |

Config: INI `[motion_icon]` + `motion_icon.xml` (`background status_tint="1"` or `status_icon` node).

```xml
<background x="0" y="0" width="500" height="35" stretch="1" status_tint="1">
  <texture>ui_inGame2_compass_motion_icon</texture>
</background>
```

```ini
[motion_icon]
enabled = true
enemy_color = 255, 13, 8, 255
safe_color = 38, 255, 64, 255
anomaly_color = 255, 140, 13, 255
default_color = 255, 255, 255, 0
enemy_intensity = 1.0
safe_intensity = 0.7
anomaly_intensity = 0.9
color_transition_speed = 6.0
pulse_enemy = true
safe_zones = "zat_a2_sr_noweap, jup_a6_sr_noweap, jup_b41_sr_noweap, pri_a16_sr_noweap"
```

Colors: `R, G, B` or `R, G, B, A` in `0..255`. Without A, alpha defaults to 255. Legacy float `0..1` is still accepted when every channel is <= 1.

Stock `system.ltx` has no `[motion_icon]`: create it in the addon with plain `[motion_icon]`, not `![motion_icon]` (overriding a missing section fails with DLTX ERROR and the config never applies).

SafeZone is one path. `safe_zones` lists space restrictor names (CoP hubs: noweap). Check matches `actor_in_zone`: `restrictor->inside(Position)`. Empty list means no green. Campfires do not light by themselves. Legacy: `safe_zone_source = camp` (+ `max_safe_distance`).

Helper: `level.actor_in_restrictor(name)`.

Log line: `motion_icon: safe_zones=4 camp=0`.

Tint target: `status_icon` if present, otherwise `_compassBackground`.

## Examples

Scenario 1: Activation via Lua (recommended)

```lua
ActorMenu.get_maingame():SetNavigationMode(true)
```

Scenario 2: Legacy boot hint via DLTX (deprecated)

```ini
[ui]
UseCompassBar = true
```

Related material: [UI overview](ui-advanced-features.md).
