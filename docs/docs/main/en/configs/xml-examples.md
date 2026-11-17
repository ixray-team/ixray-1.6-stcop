# Configuring the UI with XML

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

This CoP example adds a label, a progress bar, and a scroll view to an existing window. XML declares the layout and Lua creates the widgets. Register the `example_status_font` from the [LTX example](ltx-examples.md).

## Syntax

The XML root contains the window layout. Use `#include "path"` to pull markup from a separate file. An XMLOverride file named `mod_<base filename without extension>_<suffix>.xml` replaces an element marked with `override="replace"`.

## Examples

### Build the layout from two files

Put this element in `gamedata/configs/ui/config_example/status.xml` without an outer `<w>`. The `#include` directive inserts it into the main file’s root.

```xml
<status_label x="20" y="20" width="240" height="52">
    <text font="example_status_font" align="l" complex_mode="1" />
</status_label>
```

Save the main layout in `gamedata/configs/ui/config_example.xml`.

```xml
<w>
    #include "ui\config_example\status.xml"

    <meter x="20" y="80" width="220" height="16" mode="horz"
           min="0" max="100" pos="40" inertion="0">
        <progress stretch="1">
            <texture>ui_inGame2_inventory_progress_bar</texture>
        </progress>
        <min_color r="220" g="50" b="50" a="255" />
        <middle_color r="235" g="190" b="45" a="255" />
        <max_color r="80" g="210" b="100" a="255" />
    </meter>

    <entries x="20" y="110" width="240" height="140"
             scroll_profile="default" always_show_scroll="0"
             top_indent="4" bottom_indent="4" vert_interval="6" />
</w>
```

| Attribute or element | Meaning |
| --- | --- |
| `x`, `y`, `width`, `height` | Position and size relative to the parent, in UI coordinates. |
| `text font` | LTX font section name. `align="l"` aligns the text to the left and `complex_mode="1"` enables multiline layout. |
| `meter mode`, `min`, `max`, `pos` | Orientation, range, and starting value. Here, `40` out of `100` fills 40% of the bar. |
| `inertion` | Inertia when the value changes. With `0`, the fill moves towards the target at its fastest rate. |
| `progress/texture` | Fill texture. The example uses an ID from the bundled CoP texture definitions. |
| `min_color`, `middle_color`, `max_color` | Colors at the start, midpoint, and end of the range. `min_color` enables value-based coloring. |
| `scroll_profile` | Profile name in `scroll_bar.xml`. The example uses `default`. |
| `always_show_scroll` | With `1` the scrollbar stays visible. With `0` it is shown when the contents need scrolling. |
| `top_indent`, `bottom_indent`, `vert_interval` | Top padding, bottom padding, and spacing between items in the scroll view. |

The `#include` path starts with `ui\` relative to `$game_config$`. [File inclusion rules](group-file-connection-using-masks.md)

### Create the widgets in an existing window

Pass an existing `CUIWindow` or subclass as the parent to `build_example`. `ParseFile` looks for the file in `configs/ui`. `InitStatic`, `InitProgressBar`, and `InitScrollView` create their widgets and attach them to the parent.

```lua
local function build_example(parent)
    local xml = CScriptXmlInit()
    xml:ParseFile("config_example.xml")

    local status = xml:InitStatic("status_label", parent)
    status:SetText("Signal found\\nScanning area")

    local meter = xml:InitProgressBar("meter", parent)
    local entries = xml:InitScrollView("entries", parent)
    return status, meter, entries
end
```

Call `build_example(self)` during window initialization if `self` is the parent window. The widget splits the label into two lines at the literal `\n` marker, which Lua passes as `\\n`. The bar fills to 40%. The scroll view starts empty. To try scrolling, add enough rows with `entries:AddWindow(row, true)`, where `row` is a row window you created and `true` lets the scroll view delete the widget.

To display player health in an in-game window, add `expression="floor(fltPlayerHealth * 100.0)"` to `meter`. The bar then updates its target each frame on a `0–100` scale. [XML Expression variables and functions](../interface/xml-expression.md)

### Change the existing layout with XMLOverride

Create `mod_config_example_tall.xml` next to `config_example.xml`. It replaces the entire `entries` node. Carry over the attributes you want to keep.

```xml
<w>
    <entries override="replace"
             x="20" y="110" width="240" height="200"
             scroll_profile="default" always_show_scroll="1"
             top_indent="4" bottom_indent="4" vert_interval="6" />
</w>
```

On the next game start, the list is 60 UI units taller and the scrollbar stays visible when the list is empty. [Node matching and replacement rules](xml-override.md)

## See also

- [Scrollbar profiles](../interface/scrollbar-profiles.md)
- [Interface settings](../interface/general-information.md)
