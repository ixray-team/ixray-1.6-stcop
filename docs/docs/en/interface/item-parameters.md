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
