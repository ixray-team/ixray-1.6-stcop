# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

### ffx_vector_utils: `\gamedata\scripts\ixr_framework\utils\ffx_vector_utils.script`
Утилиты для работы с трёхмерными векторами (позиции, направления, клонирование, нормализация, вращение):
* `mul_in_direction`
* `clone`
* `get_device_position`
* `get_device_direction`
* `normalize`
* `rotate_quaternion`

#### Описание методов:

```lua
--// Переместить позицию в заданном направлении на указанное расстояние. Возвращает новый вектор.
mul_in_direction(position, direction, length)
args:
  position (vector)(required) - начальная позиция (объект vector).
  direction (vector)(required) - вектор направления.
  length (number)(required) - расстояние.
retval: (vector) - новая позиция (клон).

--// Создать глубокую копию вектора (новый объект vector).
clone(vec)
args:
  vec (vector)(required) - вектор для клонирования.
retval: (vector) - новый вектор с теми же координатами.

--// Получить текущую позицию камеры как новый вектор.
get_device_position()
args: (none)
retval: (vector) - позиция камеры.

--// Получить текущее направление камеры как новый вектор.
get_device_direction()
args: (none)
retval: (vector) - направление камеры.

--// Нормализовать вектор (возвращает таблицу с полями x,y,z, а не объект vector).
normalize(v)
args:
  v (table|vector)(required) - вектор с компонентами x,y,z.
retval: (table) - нормализованный вектор как таблица {x=..., y=..., z=...}, или нулевой вектор, если длина равна 0.

--// Повернуть вектор вокруг указанной оси (base_vector) на заданный угол в радианах (используя кватернионное вращение).
rotate_quaternion(vector_a, base_vector, angle)
args:
  vector_a (table|vector)(required) - поворачиваемый вектор.
  base_vector (table|vector)(required) - ось вращения (нормализуется внутри).
  angle (number)(required) - угол поворота в радианах.
retval: (table) - повёрнутый вектор как таблица {x, y, z}.
```

### Примеры использований:
```lua
--// Перемещение вперёд на 10 метров
local pos = db.actor:position()
local dir = db.actor:direction()
local new_pos = ffx_vector_utils.mul_in_direction(pos, dir, 10)
db.actor:set_position(new_pos)

--// Клонирование вектора
local original = vector():set(1, 2, 3)
local copy = ffx_vector_utils.clone(original)
copy.x = 5
--// original остаётся (1,2,3)

--// Получение позиции/направления камеры
local cam_pos = ffx_vector_utils.get_device_position()
local cam_dir = ffx_vector_utils.get_device_direction()
SemiLog(string.format("Камера: %.2f,%.2f,%.2f", cam_pos.x, cam_pos.y, cam_pos.z))

--// Нормализация
local raw = {x=3, y=4, z=0}
local norm = ffx_vector_utils.normalize(raw)
--// norm = {x=0.6, y=0.8, z=0}

--// Поворот вектора вокруг оси Y на 90 градусов (PI/2)
local v = {x=1, y=0, z=0}
local axis = {x=0, y=1, z=0}
local rotated = ffx_vector_utils.rotate_quaternion(v, axis, math.pi/2)
--// rotated ~= {x=0, y=0, z=-1} (в зависимости от системы координат)
```
