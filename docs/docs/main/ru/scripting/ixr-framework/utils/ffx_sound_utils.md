# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

### ffx_sound_utils: `\gamedata\scripts\ixr_framework\utils\ffx_sound_utils.script`
Утилиты для работы со звуком:
* `play_sound_async`

#### Описание методов:

```lua
--// Воспроизвести звук асинхронно на позиции актёра. Звуковой объект сохраняется во внутренней таблице, чтобы не уничтожаться сборщиком мусора во время воспроизведения.
play_sound_async(path, vol)
args:
  path (string)(required) - путь к звуковому файлу (OGG или WAV).
  vol (number)(required) - уровень громкости (0.0 – тихо, 1.0 – максимально).
retval: (none)
```

### Примеры использований:
```lua
--// Воспроизвести звук выстрела с громкостью 0.8
ffx_sound_utils.play_sound_async("weapons\\shotgun_fire.ogg", 0.8)

--// Воспроизвести звук шага с низкой громкостью
ffx_sound_utils.play_sound_async("footsteps\\step_grass.ogg", 0.3)

--// Воспроизвести звук уведомления
ffx_sound_utils.play_sound_async("ui\\click.ogg", 1.0)
```
