> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.4

# Radial progress shapes

## Overview

`CUIProgressShape` allows drawing circular progress indicators for boosters and other timers.

## Key parameters

1. `sector_count` — number of circle segments.
2. `begin_angle` — starting angle.
3. `end_angle` — ending angle.
4. `clockwise` — filling direction.
5. `blend` — blending mode.
6. `back` and `front` — background and active layers.

## Example

```xml
<indicator_booster_health x="0" y="0" width="32" height="32" sector_count="16" begin_angle="0" end_angle="360">
  <back>...</back>
  <front>...</front>
</indicator_booster_health>
```

## Usage

1. Add the node to the HUD XML.
2. Configure `sector_count` and angles.
3. Test rendering at different UI scales.

Related material: [general information](general-information.md), [UI overview](ui-advanced-features.md).
