# Contextual HUD and pause overlay

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

The contextual HUD shows health and stamina when they change and shows the weapon block when switching weapons, firing, reloading, or changing ammunition or fire mode. XML controls the fade timing. A separate XML file defines the pause overlay.

## Syntax

Run `hud_contextual_status 1` to enable the contextual HUD. Add the settings under the existing `hud_states` node in `gamedata/configs/ui/maingame.xml` or its aspect-ratio variant.

```xml
<contextual_display show_speed="4" hide_speed="3"
                    hide_delay="2.5" health_threshold="0.002"/>
```

| Attribute | Default | Effect |
| --- | --- | --- |
| `show_speed` | `4` | Opacity gained per second. Minimum `0.1`. |
| `hide_speed` | `3` | Opacity lost per second. Minimum `0.1`. |
| `hide_delay` | `2.5` | Seconds after the last event for the block. Minimum `0.1`. |
| `health_threshold` | `0.002` | A health or stamina change must exceed this value to show the block. |

Omitting `contextual_display` uses these defaults. Run `hud_contextual_status 0` to restore normal display. Contextual mode also enables automatic hiding of the quick-slot panel.

## Examples

### Customize the pause overlay

Edit `pause_static` in `gamedata/configs/ui/backend/pause.xml`. This example displays white text in the center of the screen.

```xml
<w>
    <pause_static x="0" y="0" width="1024" height="768">
        <text font="graffiti50" r="255" g="255" b="255" a="220"
              align="c" vert_align="c">st_game_paused</text>
    </pause_static>
</w>
```

Dimensions use UI coordinates. If the file fails to load, the HUD draws the standard pause label. The main menu and some tutorials can hide the pause indicator.

## See also

- [HUD console commands](../main/console-commands.md)
- [Weapon HUD](hud-weapon-ammo-fire-mode.md)
- [Quick slots](hud-quick-slots-panel.md)
