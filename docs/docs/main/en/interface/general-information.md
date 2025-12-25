# General Information
## Overview

### XML Scaling
> [!IMPORTANT]
> **Status**: Supported  
> **Minimum Version**: 1.2.2
* Added the ability to set UI texture scaling in XML using the `scale` attribute.

Implementation example:
```xml
<file name="ui\ui_mainMenu2" scale="2">
```

### CUIStackPanel
> [!IMPORTANT]
> **Status**: Supported  
> **Minimum Version**: 1.3
* A simple vertical `StackPanel` implementation. It lays out visible elements one after another automatically.

![image](https://github.com/user-attachments/assets/086aa7e8-8c8f-45ca-81a1-17c20c00397a)

```xml
<indicator_start_line x="568" y="689" width="0" height="0" stretch="1" right="true"> <!-- right="true": right align -->
 <!-- Elements -->
</indicator_start_line>
```

* tag `spacing="2` - spacing of 2 pixels (optional)

```lua
local sp = xml:InitStackPanel("indicator_start_line", self) -- Create a panel (parent: CUIWindows)
sp:SetRightAlign(true)            -- set right alignment
local IsRight = sp:IsAlignRight() -- get alignment
```

### CUICursor
> [!IMPORTANT]
> **Status**: Supported  
> **Minimum Version**: 1.0
* Added options to configure the UI cursor element. See `cursor.xml` for details.

### CUIDoubleProgressBar
> [!IMPORTANT]
> **Status**: Supported  
> **Minimum Version**: 1.0
* Support for setting a minimum <-> maximum color range
```xml
<first_min_color color="pda_red" />
<first_middle_color color="pda_green" />
<first_max_color color="pda_red" />
```

### CUIMainInGameWnd
> [!IMPORTANT]
> **Status**: Supported  
> **Minimum Version**: 1.3
* Support for custom colors for active/inactive ammunition types (static_fmj_ammo, static_ap_ammo, etc.)
```xml
<inactive_ammo_color color="pda_green" />
<active_ammo_color color="pda_red" />
```

## Sliders

### CUITrackBar
> [!IMPORTANT]
> **Status**: Supported  
> **Minimum Version**: 1.0
* Added configuration options for the UI element. See `trackbar.xml` for details.
* Supports showing a numeric value to the right of the slider when the `show_value="1"` attribute is set.

![image](https://github.com/ixray-team/ixray-1.6-stcop/assets/13867290/d53f08a2-1d18-4942-b669-fd7eb132956f)

### CUICustomSpin
> [!IMPORTANT]
> **Status**: Supported  
> **Minimum Version**: 1.0
* Added configuration options for the UI element. See `custom_spin.xml` for details.
* Added support for horizontal sliders when the `horz="1"` attribute is set. Horizontal sliders are configured in `custom_spin_horz.xml`.

## Other

### CUIStatic | CUITextWnd

> [!IMPORTANT]
> **Status**: Supported  
> **Minimum Version**: 1.3

* Added support for changing text color on mouse hover, similar to Shadow of Chernobyl builds.

- `highlight_text` — enable text highlighting
- `hA` — highlight alpha channel
- `hR` — highlight red channel
- `hG` — highlight green channel
- `hB` — highlight blue channel

Implementation example:
```xml
<cap_screenshot_format x="26" y="3" width="108" height="24" highlight_text="1" hA="255" hR="255" hG="0" hB="0">
    <text r="170" g="170" b="170" font="letterica16" align="r" vert_align="c">ui_mm_screenshot_format</text>
</cap_screenshot_format>
```

> [!IMPORTANT]
> **Status**: Supported  
> **Minimum Version**: 1.3
* Added support for font gradients when the `gradient="1"` attribute is set.

You can also change gradient direction with the `gradient_mode` attribute. Accepted values:
- `vert` — top to bottom (default)
- `horz` — left to right
- `back` — right to left
- `down` — bottom to top

The gradient color can be set with the `gradient_color` attribute. Works like the regular `color` attribute.

![image](https://github.com/user-attachments/assets/041a244d-fee1-48bc-a1d5-d705fda5d248)

Example:
```xml
<caption x="0" y="20" width="467" height="30">
    <text font="graffiti32" align="c" color="red" gradient="1" gradient_mode="vert" gradient_color="blue">ui_mm_load_game</text>
</caption>
```

### CUIMotionIcon
> [!IMPORTANT]
> **Status**: Supported  
> **Minimum Version**: 1.3
* Added support for `motion_icon.xml` from Shadow of Chernobyl.

Example:
```xml
<?xml version='1.0' encoding="UTF-8"?>
<window x="0" y="0" w="640" h="480" r="227" g="121" b="222" texture="1">

    <background  x="0" y="640" width="94" height="125" stretch="1">
        <texture>ui_hud_stamina_full</texture>
    </background>
    
    <state_normal x="20" y="22" width="50" height="75" stretch="1">
        <texture>ui_hud_soldier_normal</texture>
    </state_normal>

    <state_crouch x="20" y="22" width="50" height="75" stretch="1">
        <texture>ui_hud_soldier_crouch</texture>
    </state_crouch>

    <state_creep x="20" y="22" width="50" height="75" stretch="1">
        <texture>ui_hud_soldier_creep</texture>
    </state_creep>

    <state_climb x="20" y="22" width="50" height="75" stretch="1">
        <texture>ui_hud_soldier_climb</texture>
    </state_climb>

    <state_run x="20" y="22" width="50" height="75" stretch="1">
        <texture>ui_hud_soldier_run</texture>
    </state_run>

    <state_sprint x="20" y="22" width="50" height="75" stretch="1">
        <texture>ui_hud_soldier_sprint</texture>
    </state_sprint>

    <power_progress  x="24" y="94" width="43" height="7" horz="1" min="0" max="100" pos="50">
        <progress>
            <texture>ui_hud_shk_stamina</texture>
        </progress>
    </power_progress>
    
    <luminosity_progress x="79" y="34" width="8" height="71" horz="0" min="0" max="200" pos="100">
        <progress>
            <texture>ui_hud_shk_light</texture>
        </progress>
    </luminosity_progress>
    
    <noise_progress x="8" y="34" width="8" height="71" horz="0" min="0" max="400" pos="100">
        <progress>
            <texture>ui_hud_shk_noise</texture>
        </progress>
    </noise_progress>
</window>
```

### CUIZoneMap
> [!IMPORTANT]
> **Status**: Supported  
> **Minimum Version**: 1.3
* Added support for a square minimap, as in Clear Sky / Shadow of Chernobyl.

![image](https://github.com/user-attachments/assets/9473739e-71c0-4d11-8dd9-6a1322901095)

Example:
```xml
<window>
    <minimap>
        <level_frame x="17" y="14" width="137" height="166"/>
        
        <background x="3" y="3" width="164" height="191" stretch="1"> 
            <texture>ui_hud_map</texture>
            
            <dist_text x="116" y="4" width="38" height="14">
                <text align="r" font="arial_14" color="ui_3"/>
            </dist_text>
        </background>
        
        <compass x="117" y="18" width="31" height="31" heading="1" s_tretch="0">
            <texture a="170">ui_hud_compas</texture>
        </compass>
        
        <center width="3" height="4" alignment="c" stretch="1"> 
            <texture>ui_minimap_point</texture>
        </center>

        <static_counter x="133" y="167" width="29" height="29" light_anim="ui_pda_contacts" la_cyclic="0" la_texture="0" la_text="1" la_alpha="1" stretch="1">
            <texture>ui_hud_map_counter</texture>
            <text_static x="7" y="7" width="12" height="14">
                <text  align="c" font="graffiti19" color="ui_7"/>
            </text_static>
        </static_counter>
    </minimap>
</window>
```

### CHUDTarget

> [!IMPORTANT]
> **Status**: Supported   
> **Minimum Version**: 1.3
* Now font, colors, texture and shader can be changed in the `hud_target.xml` file.

Example:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<w>
    <shader>hud\cursor</shader>
    <texture>ui\cursor</texture>
    <enemy_color r="255" g="0" b="0" a="128"/>
    <friend_color r="0" g="255" b="0" a="128"/>
    <neutral_color r="255" g="255" b="128" a="128"/>
    <default_color r="255" g="255" b="255" a="128"/>
    <target_font font="letterica18"/>
</w>
```
