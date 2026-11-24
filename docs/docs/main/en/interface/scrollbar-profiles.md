# Scrollbar profiles

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

A profile defines scrollbar geometry and resources. Profiles live in `gamedata/configs/ui/scroll_bar.xml` and can be shared by multiple lists and windows.

## Syntax

```xml
<logs_list x="45" y="176" width="936" height="523" item_height="35"
           scroll_profile="pda_logs" always_show_scroll="1"/>
```

Set `scroll_profile` on a list or scroll-view node. Its name must match an element in `scroll_bar.xml`. The default profile name is `default`. The bundled CoP and CoC data sets define `pda_logs`. For another data set, use a profile from its `scroll_bar.xml`.

| Profile attribute | Effect |
| --- | --- |
| `layout` | Selects `auto`, `stretch`, `fixed`, or `styled` layout. In `auto` mode, the layout is fixed when the chosen axis has a positive width and height. |
| `width`, `height` | Dimensions of a fixed horizontal scrollbar. |
| `width_v`, `height_v` | Dimensions of a fixed vertical scrollbar. |
| `hold_delay` | Auto-scroll delay in milliseconds when holding a button. |
| `scroll_box_offset_x`, `scroll_box_offset_y` | Thumb travel area offsets. |
| `thumb` | Fixed-layout thumb type: `auto`, `button`, or `box`. |

A profile can define arrows (`up_arrow`, `down_arrow`, `left_arrow`, `right_arrow`), the track (`back` or `back_v`), and the thumb (`box` or `box_v`). Profiles also use `dec`, `inc`, `track`, and `thumb` nodes.

## Bundled profiles and aspect ratios

| Set | Profiles in `scroll_bar.xml` and `scroll_bar_16.xml` |
| --- | --- |
| SoC and CS | `default`, `pda` |
| CoP and CoC | `default`, `pda`, `pda_logs` |

The widescreen loader prefers an existing `scroll_bar_16.xml`. On ultrawide it tries `scroll_bar_21.xml`, then `_16`, then the base file. None of these bundled sets provides the `_21` file. Profile lookup runs inside the selected file. An omitted or empty `scroll_profile` uses `default`. A nonempty unknown name causes initialization with that profile to fail.

## Styled scrollbars (`layout="styled"`)

The `styled` mode builds the scrollbar only from the parts the profile defines. Profiles without `layout="styled"` keep the legacy behaviour (`stretch`, `fixed`, `auto`).

```xml
<slim layout="styled" thickness="4" hover_thickness="8" min_thumb="24"
      wheel_lines="3" track_click="jump" idle_alpha="0.25" idle_delay="1200">
    <track color="255,255,255,60" color_hover="255,255,255,90">
        <texture>ui_scroll_back</texture>
    </track>
    <thumb color="255,255,255,160" color_hover="255,255,255,220" color_pressed="255,205,120,255">
        <texture>ui_scroll_box</texture>
    </thumb>
</slim>
```

Parts:

| Node | Purpose |
| --- | --- |
| `track` | Background. Optional |
| `thumb` | Thumb |
| `dec`, `inc` | 3t buttons (`texture_e`, `texture_h`, `texture_t`). Without them there are no arrows |
| `vertical`, `horizontal` | Per-axis overrides: attributes and parts inside the node win over the shared ones |

If the `texture` has `_b`, `_back`, `_e` variants, the part is drawn as a frame line, otherwise as a stretched image. `track` and `thumb` take `color`, `color_hover`, `color_pressed`, `color_disabled` as `r,g,b[,a]`. `color_disabled` is used when there is nothing to scroll.

| Profile attribute | Effect |
| --- | --- |
| `thickness` | Scrollbar thickness |
| `hover_thickness` | Thickness while hovered or dragged. The scrollbar window uses the larger value |
| `track_cross`, `thumb_cross` | Track and thumb thickness across the axis |
| `thumb_margin` | Thumb margin when `thumb_cross` is not set |
| `track_inset` | Track inset from the ends (or from the arrows) |
| `cross_align` | Edge alignment: `near`, `center`, `far` (default `far`, the window edge) |
| `min_thumb` | Minimum thumb length |
| `wheel_lines` | Wheel step in list steps |
| `track_click` | `page`: page toward the cursor, repeating while held; `jump`: move the thumb under the cursor and drag |
| `repeat_delay`, `hold_delay` | Delay before repeat and repeat interval while a button or the track is held, ms |
| `idle_alpha`, `idle_delay`, `fade_speed` | Idle opacity, time before fading (ms), fade speed |
| `resize_speed` | Thickness change speed |

The thumb drag captures the mouse and does not drop when the cursor leaves the scrollbar. The wheel over the scrollbar scrolls the list.

`scroll_bar.xml` is loaded once and cached; when the screen aspect changes, the matching file (`scroll_bar_16.xml`, `scroll_bar_21.xml`) is used. Addons can add profiles through [XMLOverride](../configs/xml-override.md).

## Examples

### Switching profiles from Lua

```lua
local view = xml:InitScrollView("my_scroll", self)
view:SetScrollBarProfile("pda")
view:ReinitScrollBar()

local bar = view:ScrollBar()
if bar then
    bar:SetScrollPos(10)
end
```

For a standalone scrollbar, use `InitScrollBarFixed(x, y, horizontal, profile)` or `InitScrollBarStretch(x, y, length, horizontal, profile)`. The return value indicates whether initialization succeeded.

## Recommendations and limitations

- `SetFixedScrollBar(bool)` controls whether a list always shows its scrollbar. The profile’s `layout` controls geometry.
- `ui.QueryScrollBarProfileLayout(name, horizontal)` returns `-1` for a missing profile, `0` for `stretch`, `1` for `fixed`, and `2` for `styled`.

## See also

- [Widgets and XML](general-information.md)
