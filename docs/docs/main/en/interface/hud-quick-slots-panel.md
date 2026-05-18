> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.4

# Quick slots panel

## Overview

The panel contains 4 quick slots and supports auto-hide, smooth fade-in, and red highlighting of empty slots.

## Ready XML: panel behavior

```xml
<quick_slots_panel
  show_speed="3.0"
  hide_speed="4.0"
  hide_delay="2.0"
  empty_red_glow_counter="1"
  empty_red_glow_icon="1"
  empty_red_intensity="1.0"
/>
```

## Usage

1. Add blocks to `configs/ui/maingame.xml`.
2. Check `hud_hide_quick_slots`.
3. If needed, bind `show_quick_slots` in `configs/ui/ui_keybinding.xml`.

Related material: [UI overview](ui-advanced-features.md).
