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
| `layout` | Selects `auto`, `stretch`, or `fixed` layout. In `auto` mode, the layout is fixed when the chosen axis has a positive width and height. |
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
- `ui.QueryScrollBarProfileLayout(name, horizontal)` returns `-1` for a missing profile, `0` for `stretch`, and `1` for `fixed`.

## See also

- [Widgets and XML](general-information.md)
