# HUD wristwatch model

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

The HUD can render a separate wristwatch model with the hands. The watch follows a selected hands bone during HUD animations.

## Syntax

Add the keys to the active LTX section for the player hands, next to `visual`. Replace the example path and bone name with those from your models.

```ini
visual_watches = dynamics\devices\hud_watch.ogf
watches_bone = bip01_l_hand
watches_pos = 0, 0, 0
watches_rot = 0, 0, 0
watches_scale = 1.0
```

| Key | Default | Effect |
| --- | --- | --- |
| `visual_watches` | absent | Name of the skeletal watch model. Adding the key enables model loading. |
| `watches_bone` | required with `visual_watches` | Bone name from the **hands** model. |
| `watches_pos` | `0, 0, 0` | Local offset relative to the bone. |
| `watches_rot` | `0, 0, 0` | Local rotation in degrees, in heading, pitch, bank order. |
| `watches_scale` | `1.0` | Uniform scale multiplier. |

## Examples

### Fit the watch to the wrist

Check the bone attachment with the offset and rotation set to `0, 0, 0`. Then adjust `watches_pos`, `watches_rot`, and `watches_scale`. Check that the watch follows the wrist while moving and reloading. Builds with the HUD editor expose the values in HUD Adjust.

## See also

- [HUD Animator](../animation-system/hud-animator.md)
- [Contextual HUD](hud-display-options.md)
