# XMLOverride

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 0.10

## Overview

A file named `mod_<base_file_name>_<edit_name>.xml` changes individual nodes in an XML file. Place it in the same configuration directory. For example, use `configs/ui/mod_hud_myaddon.xml` to edit `configs/ui/hud.xml`.

## Syntax

### Selecting a node

The engine follows element names from the root. If an element in the edit file has an `id`, the engine looks for that value. Without an `id`, it selects the first child with the matching tag. Position and other attributes do not participate in matching. For siblings with the same tag, use existing unique IDs from the original XML.

| Attribute | Result |
| --- | --- |
| `override="add"` | Appends a new child to the matched parent. |
| `override="replace"` | Removes the matched element and appends its replacement to the same parent. Include all required attributes, text, and nested elements. |
| `override="remove"` | Removes the matched element. |

For partial XML edits, put `override` on child elements. At the root, `replace` replaces the full tree, including text nodes, and `remove` deletes it. The engine skips `add` on the root and processes its children.

## Examples

In the original `hud.xml`:

```xml
<w>
  <background>
    <auto_static id="river" x="432" y="353" width="460" height="416">
      <texture>ui\video_water_crop</texture>
    </auto_static>
    <auto_static id="panel" x="0" y="0" width="104" height="768">
      <texture>ui\ui_inGame2_left_widepanel</texture>
    </auto_static>
  </background>
</w>
```

In `mod_hud_myaddon.xml`:

```xml
<w>
  <background>
    <auto_static id="river" x="432" y="353" width="460" height="416" override="replace">
      <texture>ui\my_river</texture>
    </auto_static>
  </background>
</w>
```

The `river` element receives a new texture and moves after `panel`. If a widget uses a child index, account for the change in order.

## See also

- [File masks and includes](group-file-connection-using-masks.md)
- [Widgets and XML](../interface/general-information.md)
- [Configuring the UI with XML](xml-examples.md)
