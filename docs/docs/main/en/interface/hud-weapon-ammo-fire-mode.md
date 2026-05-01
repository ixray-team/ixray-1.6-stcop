> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.4

# Ammo, weapon icon, and fire mode

## Overview

The `hud_states` section in `maingame.xml` supports three related blocks:

1. adaptive ammo widget
2. fire mode display using `mode_mapping`
3. multiple `static_wpn_icon` modes

## static_ammo_adaptive

1. Enabled by the `static_ammo_adaptive` node.
2. Uses `clip_text` and `total_text`.
3. The separator is set by `separator`.
4. In this mode, the regular ammo fields are hidden.

## static_fire_mode and mode_mapping

1. `use_icon="1"` converts the fire mode into icons.
2. `mode_mapping` links the mode text to an icon name.

Example:

```xml
<static_fire_mode x="899" y="684" width="16" height="20" stretch="1" use_icon="1">
  <mode_mapping text="1" icon="ui_inGame2_icon_fmode_single"/>
  <mode_mapping text="A" icon="ui_inGame2_icon_fmode_auto"/>
  <mode_mapping text="2" icon="ui_inGame2_icon_fmode_2burst"/>
  <mode_mapping text="3" icon="ui_inGame2_icon_fmode_3burst"/>
  <text font="letterica18" align="c" vert_align="c"/>
</static_fire_mode>
```

## static_wpn_icon

1. Legacy mode: ammunition icon.
2. `display_mode="text"`: text instead of an icon.
3. `caliber="1"`: caliber mode via `hud_group_catalog`.

Additional notes:

1. `WeaponIconScale` in `engine_external.ltx`.
2. `active_ammo_color` and `inactive_ammo_color` for ammo type colors.

Related material: [general information](general-information.md), [UI overview](ui-advanced-features.md).
