# General Information
## HUD motions
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.0

* Added the `m_anim_speed` parameter to the `player_hud_motion` structure. Each animation can now have its own speed multiplier, configurable in the game resources.

### Usage example

Let's assume we have a configuration file where we specify an animation for an HUD item:

```ini
anm_idle_moving_empty = colt1911_idle_moving, colt1911_opened
```

Now you can specify a third parameter to define the playback speed:

```ini
anm_idle_moving_empty = colt1911_idle_moving, colt1911_opened, 1.9
```

## HUD objects

Added the `hud_fov` parameter, which is specified in the `hud` section of an item. It allows setting the HUD FOV individually for the active item.

```ini
hud_fov = float
```
