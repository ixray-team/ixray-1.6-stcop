# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

### ffx_particles_utils: `\gamedata\scripts\ixr_framework\utils\ffx_particles_utils.script`
Утилиты для управления системами частиц (pg-эффекты):
* `play_async`
* `destroy`
* `set_pos_and_dir_x_bazis`
* `set_pos_and_dir_y_bazis`
* `set_pos_and_dir_z_bazis`

#### Описание методов:

```lua
--// Создать или переиспользовать систему частиц. В текущей реализации параметр id игнорируется – новый объект создаётся с автоматическим индексом, после чего проигрывается в указанной позиции.
play_async(id, pg_path, pos)
args:
  id (number)(required) - идентификатор (не используется, перезаписывается внутри).
  pg_path (string)(required) - путь к файлу системы частиц (.pg).
  pos (vector)(required) - позиция для воспроизведения эффекта.
retval: (none)

--// Остановить и удалить систему частиц с указанным id.
destroy(id)
args:
  id (number)(required) - идентификатор системы частиц.
retval: (none)

--// Установить позицию и ориентацию объекта частиц, используя направление по оси X (базис X).
set_pos_and_dir_x_bazis(pg_object, position, direction)
args:
  pg_object (userdata)(required) - объект системы частиц.
  position (vector)(required) - новая позиция.
  direction (vector)(required) - вектор направления для базиса X.
retval: (none)

--// Установить позицию и ориентацию объекта частиц, используя направление по оси Y (базис Y).
set_pos_and_dir_y_bazis(pg_object, position, direction)
args:
  pg_object (userdata)(required) - объект системы частиц.
  position (vector)(required) - новая позиция.
  direction (vector)(required) - вектор направления для базиса Y.
retval: (none)

--// Установить позицию и ориентацию объекта частиц, используя направление по оси Z (базис Z).
set_pos_and_dir_z_bazis(pg_object, position, direction)
args:
  pg_object (userdata)(required) - объект системы частиц.
  position (vector)(required) - новая позиция.
  direction (vector)(required) - вектор направления для базиса Z.
retval: (none)
```

### Примеры использований:
```lua
--// Воспроизвести эффект дыма в позиции актёра
local actor_pos = db.actor:position()
ffx_particles_utils.play_async(1, "effects\\smoke.pg", actor_pos)

--// Остановить и удалить эффект по id
ffx_particles_utils.destroy(1)

--// Создать объект частиц и настроить его ориентацию
local pg_obj = particles_object("effects\\fire.pg")
local pos = vector():set(0, 0, 0)
local dir = vector():set(1, 0, 0)
ffx_particles_utils.set_pos_and_dir_x_bazis(pg_obj, pos, dir)

--// Установить позицию и направление по Y
ffx_particles_utils.set_pos_and_dir_y_bazis(pg_obj, pos, vector():set(0, 1, 0))

--// Установить позицию и направление по Z
ffx_particles_utils.set_pos_and_dir_z_bazis(pg_obj, pos, vector():set(0, 0, 1))
```
