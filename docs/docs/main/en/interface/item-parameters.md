# Item Parameters
## Knife Parameters

### Overview

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.0

Added the `CUIKnifeParams` class, which adds display of various properties and characteristics of knives in the game UI (`actor_menu`)

Resource files:
* `ui/actor_menu_item.xml`
* `ui/actor_menu_item_16.xml`
* `text/rus/ui_st_new.xml`

### Usage Example:

```xml
<knife_params x="0" y="0" width="212" height="70">
    <!-- Line separating knife parameters -->
    <prop_line x="0" y="0" width="212" height="9" stretch="1">
        <texture>ui_inGame2_hint_wnd_Properties</texture>
    </prop_line>

    <!-- Damage icon -->
    <static_damage x="0" y="10" width="14" height="18" stretch="1">
        <texture>ui_wp_prop_damage</texture>
    </static_damage>

    <!-- Handling icon-->
    <static_handling x="0" y="28" width="14" height="18" stretch="1">
        <texture>ui_wp_prop_ergonomics</texture>
    </static_handling>

    <!-- Strike distance icon -->
    <static_dist x="0" y="46" width="14" height="18" stretch="1">
        <texture>ui_wp_prop_distantion</texture>
    </static_dist>

    <!-- "Damage" label -->
    <cap_damage x="18" y="12" width="80" height="8">
        <text font="letterica16" color="ui_2" align="l">ui_inv_damage</text>
    </cap_damage>

    <!-- "Handling" label -->
    <cap_handling x="18" y="30" width="80" height="8">
        <text font="letterica16" color="ui_2" align="l">ui_inv_handling</text>
    </cap_handling>

    <!-- "Distance" label -->
    <cap_dist x="18" y="48" width="80" height="8">
        <text font="letterica16" color="ui_2" align="l">ui_inv_dist</text>
    </cap_dist>

    <!-- Damage progress bar -->
    <progress_damage x="104" y="14" width="101" height="9" horz="1">
        <progress>
            <texture a="150">ui_inGame2_hint_wnd_bar_alfa_line_16</texture>
        </progress>
        <background>
            <texture>ui_inGame2_hint_wnd_bar_16</texture>
        </background>
        <color_less color="pda_red"  />
        <color_more color="pda_green"/>
    </progress_damage>

    <!-- Handling progress bar -->
    <progress_handling x="104" y="32" width="101" height="9" horz="1">
        <progress>
            <texture a="150">ui_inGame2_hint_wnd_bar_alfa_line_16</texture>
        </progress>
        <background>
            <texture>ui_inGame2_hint_wnd_bar_16</texture>
        </background>
        <color_less color="pda_red"  />
        <color_more color="pda_green"/>
    </progress_handling>

    <!-- First strike distance value -->
    <value_dist1 x="104" y="48" width="10" height="8">
        <text font="letterica16" align="l"/>
    </value_dist1>

    <!-- Second strike distance value -->
    <value_dist2 x="123" y="48" width="10" height="8">
        <text font="letterica16" align="l"/>
    </value_dist2>

    <!-- Separator between distance values -->
    <value_dist_delimiter x="119" y="48" width="1" height="8">
        <text font="letterica16" color="ui_2" align="l"/>
    </value_dist_delimiter>

    <!-- Units of measurement -->
    <meters_name x="137" y="48" width="80" height="8">
        <text font="letterica16" color="ui_2" align="l"/>
    </meters_name>
</knife_params>
```

## Tags and Text on Icons

### Overview

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.0

You can add custom tags and text on top of inventory items

- Setting additional information (optional)

```ini
item_custom_text = st_text ; Text
item_custom_text_clr_inv = 100,250,100 ; Text color
item_custom_text_font = graffiti22 ; Font
item_custom_text_offset = -2.0, -2.0 ; Offsets (x, y)
```

- Setting additional icon (optional)

```ini
item_custom_mark = true
item_custom_mark_offset = -2.0, 0.0
item_custom_mark_size = 10, 10
item_custom_mark_texture = ui_item_count_back
item_custom_mark_clr = 100,250,100
```
## Extended Item Highlighting

### Overview

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.3

Setting highlighting of dependent items on mouse hover (comma-separated listing is supported) (optional)

Usage example:

```ini
[root_item_section]:identity_immunities
highlight_related_sections = related_section_a, related_section_b; item sections that will be highlighted as dependent on this item
...

[related_section_a]:identity_immunities
highlight_related_sections = root_item_section; you can add reverse highlighting of the parent item when hovering over the child if needed
...
```

## Automatic Parameter Icon Coloring

Automatic coloring is enabled per parameter row and is **disabled when neither the row nor its template defines `colorize`**. The stock XML configurations already enable effect coloring with `colorize="1"`; condition, container count, and effect duration retain their original appearance. Without it, icons retain their original DDS colors and value styling stays unchanged: modern artefact rows use gray/red for useful/harmful effects, legacy text uses green/red, and consumable values remain gray.

This option applies to artefacts, additional equipment bonuses, and consumables. Edit the corresponding files in your game's `gamedata/configs/ui`:

- `af_params.xml` and `af_params_16.xml` for artefacts and equipment bonuses;
- `booster_params.xml` and `booster_params_16.xml` for consumables.

For layouts with `caption` and `value` children, add **`colorize="1"` to `value`**, not to `caption` or `texture`:

```xml
<health_restore_speed x="0" y="0" width="257" height="20">
    <caption x="0" y="2" width="257" height="20" complex_mode="0">
        <texture>ui_am_propery_05</texture>
        <text color="ui_3" font="letterica16" vert_align="c" x="22" y="0"/>
    </caption>
    <value x="180" y="2" width="30" height="20" magnitude="6600" colorize="1">
        <text font="letterica16" vert_align="c"/>
    </value>
</health_restore_speed>
```

Keep the dimensions, magnitude, and units from your existing XML. Consumable rows use the same attribute on their `value` node:

```xml
<value x="140" y="0" width="30" height="20" magnitude="10000" colorize="1">
    <text font="letterica16" vert_align="c"/>
</value>
```

When enabled, useful effects color both the icon and value blue (`44, 172, 246`); harmful effects color them red (`210, 50, 50`). Caption text keeps its XML color. Usefulness is evaluated after applying `magnitude`. The `value` attribute `sign_inverse="1"` reverses usefulness without changing the displayed sign, for example for radiation accumulation:

```xml
<value x="180" y="2" width="30" height="20" magnitude="1000" sign_inverse="1" colorize="1">
    <text font="letterica16" vert_align="c"/>
</value>
```

Here negative values are useful and blue; positive values are harmful and red.

In legacy `static_<parameter>` layouts, put the attribute directly on the row:

```xml
<static_health_restore_speed x="0" y="0" width="257" height="20" complex_mode="1" colorize="1">
    <texture>ui_inv_icon_health_restore_speed</texture>
    <text font="letterica16" x="20" y="0"/>
</static_health_restore_speed>
```

Coloring uses the icon's alpha channel and replaces its original RGB. Both white and colored DDS icons with proper transparency work; repainting the atlas is unnecessary. For `caption`/`value` layouts, the optional `texture_minus` alternate icon for harmful effects remains supported and can be combined with coloring, but is not required for it.

Remove `colorize` or set `colorize="0"` to restore the original appearance. Update both aspect-ratio XML variants when needed. This option does not change numeric signs or grouping: built-in container counts still omit `+` by default, and the extra separator within outfit characteristics stays removed.

## Shared XML Row Templates

The engine supports shared XML templates as an optional capability. Stock `af_params.xml` and `booster_params.xml` currently use standalone rows without `template`. To unify custom configurations, create `ui/item_params.xml` and its widescreen version `ui/item_params_16.xml`. Rows reference a `template`, while `param_templates` owns their dimensions, spacing, fonts, and icons.

Include the definitions inside each panel:

```xml
<af_params x="0" y="0" width="260" height="20">
#include "ui\item_params.xml"
    <health_restore_speed template="health_restore_speed">
        <value magnitude="6600" colorize="1"/>
    </health_restore_speed>
</af_params>
```

A consumable references the same presentation with its own magnitude:

```xml
<boost_health_restore template="health_restore_speed">
    <value magnitude="10000" colorize="1"/>
</boost_health_restore>
```

Shared definitions use this structure:

```xml
<param_templates>
    <row x="0" y="0" width="257" height="19">
        <caption x="0" y="0" width="18" height="18" stretch="1">
            <text font="letterica16" x="22" y="0" vert_align="c" r="170" g="170" b="170"/>
        </caption>
        <value x="180" y="0" width="30" height="18">
            <text font="letterica16" vert_align="c"/>
        </value>
    </row>
    <health_restore_speed template="row">
        <caption>
            <texture>ui_am_propery_05</texture>
        </caption>
    </health_restore_speed>
</param_templates>
```

Change `row` to adjust geometry and fonts for every row. Change `health_restore_speed:caption:texture` to replace the health icon for both artefacts and consumables. Weight, radiation, and protection templates work the same way.

Local row attributes and children take precedence. Children are merged recursively: `<value magnitude="10000"/>` overrides only the magnitude and retains shared coordinates and fonts. Local overrides can also change `caption`, `texture`, `text`, `unit_str`, `sign_inverse`, `show_sign`, and `colorize`; only explicitly overridden properties differ from the template.

References are resolved in the current panel's `param_templates`, with up to 16 templates in an inheritance chain. Missing references or cycles skip the row and log the reason. Existing rows without `template` remain supported. Legacy `static_<parameter>` rows can also reference templates: their icon comes from `caption:texture`, while legacy value calculations and complex text remain intact.

Stock configurations set `colorize="1"` directly on effect rows. Custom shared templates can move that setting to `row:value`, keeping local `colorize="0"` overrides for absolute values and text-only effects. The C++ fallback remains disabled when the attribute is absent.

To color only the icon, set `text_colorize="0"` on `value`. By default, `text_colorize` follows `colorize`. Stock `additional_weight` and `boost_max_weight` rows use this option: positive carrying-weight values remain gray while the icon is colored. Legacy layouts specify the option on `static_<parameter>`.
