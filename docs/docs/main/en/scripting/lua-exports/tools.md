# Lua in SDK tools

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

This page covers the Lua property editor API for server entities. The `xrSE_Factory` build registers it for use in `FillProps(prefix, items)` when the property panel is built. [SDK registration](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrServerEntities/script_engine_export.cpp#L126-L128)

## Syntax

`properties_helper()` returns the shared `properties_list_helper`. Call its methods with a colon, as in `helper:create_float(...)`. `prop_item_vec` is the `items` list supplied by the editor. `prop_value` is the callback’s `sender` type. The result types in the tables represent editor-created properties. This registration exposes no constructors or methods on those result types. [Property types and factory](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L44-L74)

`key` is the property panel path, with `\` as the separator. `owner` is the server entity, usually `self` in `FillProps`. Initialize `owner[field]` with the type listed in the table before calling. Square brackets denote successive overloads with additional arguments.

### Adding properties

| Call | Result | Purpose | Source |
| --- | --- | --- | --- |
| `create_caption(items, key, text)` | `caption_value` | Adds a caption row. | [111](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L111) |
| `create_canvas(items, key, text, height)` | `canvas_value` | Adds a drawing area of the given height. The Lua registration exposes no drawing callback. | [112](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L112) |
| `create_button(items, key, labels, flags)` | `button_value` | Adds buttons named by the comma-separated labels. Pass `0` for a plain button row. This call does not install a click handler. | [113](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L113) |
| `create_choose(items, key, owner, field, mode [, path [, fill_param [, max_items]]])` | `choose_value` | Opens a resource picker for a string field. `path` is the initial choice when the field is empty, `fill_param` is a string supplied to the mode’s handler, and `max_items` is the selection limit, defaulting to `1`. | [115–118](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L115-L118) |
| `create_s16(items, key, owner, field [, min [, max [, step]]])` | `s16_value` | Signed 16-bit integer. | [125–128](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L125-L128) |
| `create_s32(items, key, owner, field [, min [, max [, step]]])` | `s32_value` | Signed 32-bit integer. | [130–133](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L130-L133) |
| `create_u8(items, key, owner, field [, min [, max [, step]]])` | `u8_value` | Unsigned 8-bit integer. | [135–138](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L135-L138) |
| `create_u16(items, key, owner, field [, min [, max [, step]]])` | `u16_value` | Unsigned 16-bit integer. | [140–143](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L140-L143) |
| `create_u32(items, key, owner, field [, min [, max [, step]]])` | `u32_value` | Unsigned 32-bit integer. | [145–148](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L145-L148) |
| `create_float(items, key, owner, field [, min [, max [, step [, decimals]]]])` | `float_value` | Floating-point number. | [150–154](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L150-L154) |
| `create_bool(items, key, owner, values, field)` | `bool_value` | Checkbox bound to an existing boolean at `values[field]`. `owner` is the server entity and `values` holds the field. | [156](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L156) |
| `create_vector(items, key, owner, field [, min [, max [, step [, decimals]]]])` | `vector_value` | Edits all three components of a `vector`, with shared limits across axes. | [158–162](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L158-L162) |
| `create_flag8(items, key, owner, field, mask [, off [, on [, flags]]])` | `flag8_value` | Edits a mask in a `Flags8` object. The method is registered, while this Lua export set has no `flags8()` constructor. | [164–167](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L164-L167) |
| `create_flag16(items, key, owner, field, mask [, off [, on [, flags]]])` | `flag16_value` | Edits a mask in a `flags16` object. | [169–172](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L169-L172) |
| `create_flag32(items, key, owner, field, mask [, off [, on [, flags]]])` | `flag32_value` | Edits a mask in a `flags32` object. | [174–177](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L174-L177) |
| `create_token8(items, key, owner, field, tokens)` | `token8_value` | Displays names from a `token_list` and stores the ID as `u8`. | [179](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L179) |
| `create_token16(items, key, owner, field, tokens)` | `token16_value` | Displays names from a `token_list` and stores the ID as `u16`. | [180](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L180) |
| `create_token32(items, key, owner, field, tokens)` | `token32_value` | Displays names from a `token_list` and stores the ID as `u32`. | [181](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L181) |
| `create_list(items, key, owner, field, options)` | `list_value` | Displays strings from an `rtoken_list` and stores the selected string. | [187](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L187) |
| `create_color(items, key, owner, field)` | `u32_value` | Color picker for a packed 32-bit ARGB integer. | [189](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L189) |
| `create_fcolor(items, key, owner, field)` | `color_value` | Color picker for an `fcolor` value with four components. | [190](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L190) |
| `create_vcolor(items, key, owner, field)` | `vector_value` | Color picker for a `vector` with RGB components from `0` to `1`. | [191](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L191) |
| `create_text(items, key, owner, field)` | `text_value` | Editable string. | [193](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L193) |
| `create_time(items, key, owner, field [, min [, max]])` | `float_value` | Edits seconds as `HH:MM:SS`. The default bounds are `0` and `86400`. | [195–197](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L195-L197) |
| `create_angle(items, key, owner, field [, min [, max [, step [, decimals]]]])` | `float_value` | Stores the angle and takes its bounds in radians. The editor displays degrees. | [199–203](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L199-L203) |
| `create_vangle(items, key, owner, field [, min [, max [, step [, decimals]]]])` | `vector_value` | Three angles stored in a `vector`, using the same units as `create_angle`. | [205–209](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L205-L209) |

Integer calls default `min, max, step` to `0, 100, 1`. `create_float` and `create_vector` default `min, max, step, decimals` to `0, 1, 0.01, 2`. Angle properties default to the full `float` range, a step of `0.01`, and two decimal places. `off` and `on` label the two states of a bitmask property. `flags` is the display flag mask and defaults to `0`. [Default values](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Editors/Public/PropertiesListHelper.h)

### Edit handlers

The SDK attaches the angle handlers when it creates an angle property. A direct call requires a `sender` supplied by the editor with the matching property type.

| Call | Result | Purpose | Source |
| --- | --- | --- | --- |
| `vector_on_before_edit(sender, value)` | — | Converts the supplied `vector` and the property limits from radians to degrees. | [102](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L102) |
| `vector_on_after_edit(sender, value)` | `boolean` | Converts the supplied `vector` and the property limits from degrees to radians. Returns `true`. | [101](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L101) |
| `float_on_before_edit(sender, value)` | `number` | Returns the angle in degrees and converts the property limits to degrees. | [105](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L105) |
| `float_on_after_edit(sender, value)` | `boolean, number` | Returns `true` and the angle in radians. Converts the property limits to radians as well. | [104](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L104) |
| `name_before_edit(sender)` | `string` | The native handler extracts the last component of a name. The Lua binding takes no input name, so it cannot perform that conversion on a supplied string. | [108](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L108) |
| `name_after_edit(sender)` | `boolean, string` | The native handler validates a proposed list-item name. The Lua binding hides the input argument and supplies an empty value to the validator. A direct Lua call is unsafe. | [107](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L107) |

### Resource picker modes

| Mode | What it selects |
| --- | --- |
| `choose_type.custom` | Custom list population mode. `create_choose` takes no Lua population callback. |
| `choose_type.sound_source` | Sound source. |
| `choose_type.sound_environment` | Sound environment. |
| `choose_type.library_object` | Object from the SDK library. |
| `choose_type.engine_shader` | Engine shader. |
| `choose_type.compiler_shader` | Compiler shader. |
| `choose_type.particle_effect` | Particle effect. |
| `choose_type.particle_system` | Particle system. |
| `choose_type.texture` | Texture. |
| `choose_type.entity` | Entity type. |
| `choose_type.spawn_item` | Level Editor spawn item. `fill_param` can specify a group key such as `spawn_group`. |
| `choose_type.light_animation` | Light animation. |
| `choose_type.visual` | Visual model. |
| `choose_type.skeleton_animations` | Skeleton animation. Supply the relative model path as `fill_param`. |
| `choose_type.skeleton_bones` | Skeleton bone. Supply the relative model path as `fill_param`. |
| `choose_type.material` | Game material. |
| `choose_type.game_animation` | Game animation. |
| `choose_type.game_motion` | Game object motion. |
| `choose_type.particle_anim_curve` | Particle animation curve. |

The active editor provides the resource list for each mode. `create_choose` registers a string property and the picker opens when the property is edited. [Mode registration](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L74-L99)

### Value lists

Populate lists before passing them to `create_token8`, `create_token16`, `create_token32`, or `create_list`. Store them on the entity and leave them intact while the editor uses the properties. The properties keep a pointer into the list data.

| Call | Purpose |
| --- | --- |
| `token_list()` | Creates a list of names and numeric IDs. |
| `token_list:add(name, id)` | Adds a pair. The name and ID must be unique. |
| `token_list:remove(name)` | Removes an existing entry by name. |
| `token_list:clear()` | Removes all entries. |
| `token_list:id(name)` | Returns the ID for an existing name. |
| `token_list:name(id)` | Returns the name for an existing ID. |
| `rtoken_list()` | Creates a string list. |
| `rtoken_list:add(value)` | Appends a string. |
| `rtoken_list:remove(index)` | Removes a string by zero-based index. An out-of-range index has no effect. |
| `rtoken_list:clear()` | Clears the list. |
| `rtoken_list:count()` | Returns the number of strings. |
| `rtoken_list:get(index)` | Returns a string by zero-based index, or `nil` out of range. |

## Examples

### Number, checkbox, and animation picker

Call `fill_editor_fields(prefix, items, self)` from your entity’s `FillProps` after its base implementation. Initialize the numeric `self.intensity`, the string `self.animation`, and the `self.preview` table holding a boolean `enabled` field. Three properties appear in the `Light` group.

```lua
local function fill_editor_fields(prefix, items, self)
    local helper = properties_helper()
    local key = prefix .. "\\Light\\"

    helper:create_float(items, key .. "Intensity", self, "intensity", 0, 10, 0.1, 2)
    helper:create_bool(items, key .. "Enabled", self, self.preview, "enabled")
    helper:create_choose(items, key .. "Animation", self, "animation", choose_type.light_animation)
end
```

### Selecting a labeled ID and a string

Call `fill_editor_choices(prefix, items, self)` from `FillProps` on its own or alongside the previous example. Initialize `self.quality` to `0` or `1` and `self.preview_mode` to `"Disabled"` or `"Enabled"`. The first property stores an ID and the second stores a string.

```lua
local function fill_editor_choices(prefix, items, self)
    if not self.quality_modes then
        self.quality_modes = token_list()
        self.quality_modes:add("Low", 0)
        self.quality_modes:add("High", 1)

        self.preview_modes = rtoken_list()
        self.preview_modes:add("Disabled")
        self.preview_modes:add("Enabled")
    end

    local helper = properties_helper()
    helper:create_token8(items, prefix .. "\\Quality", self, "quality", self.quality_modes)
    helper:create_list(items, prefix .. "\\Preview", self, "preview_mode", self.preview_modes)
end
```

## See also

- [Bundled CoP `FillProps` example](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/gamedata/scripts/se_new_attachable_item.script#L331)
- [New Lua exports](../lua-exports.md)
