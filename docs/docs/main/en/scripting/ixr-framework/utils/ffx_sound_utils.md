# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

### ffx_sound_utils: `\gamedata\scripts\ixr_framework\utils\ffx_sound_utils.script`
Utilities for working with sound:
* `play_sound_async`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_sound_utils.script]
--// Play a sound asynchronously at the actor position. Store the sound object in an internal table to prevent garbage collection during playback.
play_sound_async(path, vol)
args:
  path (string)(required) - sound file path (OGG or WAV).
  vol (number)(required) - volume level (0.0 — silent, 1.0 — maximum).
retval: (none)
```
:::

### Usage examples:
```lua
--// Play a shot sound at volume 0.8.
ffx_sound_utils.play_sound_async("weapons\\shotgun_fire.ogg", 0.8)

--// Play a footstep sound at low volume.
ffx_sound_utils.play_sound_async("footsteps\\step_grass.ogg", 0.3)

--// Play a notification sound.
ffx_sound_utils.play_sound_async("ui\\click.ogg", 1.0)
```
