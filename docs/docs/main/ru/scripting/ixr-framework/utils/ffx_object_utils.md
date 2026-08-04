# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

### ffx_object_utils: `\gamedata\scripts\ixr_framework\utils\ffx_object_utils.script`
Утилиты для работы с игровыми (клиентскими) и серверными объектами, включая проверки, преобразования, управление условиями и положение костей:
* `has_actor`
* `se_object_by_id_or_false`
* `se_object_or_false`
* `game_object_or_false`
* `is_actor_alive`
* `safe_release`
* `release`
* `create`
* `get_inv_name`
* `safe_bone_pos`
* `safe_release_by_id`
* `set_condition_by_id`
* `set_condition`
* `to_game_object`
* `test_to_game_object`

#### Описание методов:

```lua
--// Проверить, существует ли актёр (с кешированием результата).
has_actor()
args:
  (none)
retval: (boolean) - true, если актёр существует, иначе false.

--// Вернуть серверный объект по ID или false, если не найден.
se_object_by_id_or_false(id)
args:
  id (number)(required) - ID серверного объекта.
retval: (server_object|false) - серверный объект, если валидный, иначе false.

--// Вернуть серверный объект, если он валидный, иначе false.
se_object_or_false(se_obj)
args:
  se_obj (userdata)(required) - серверный объект для проверки.
retval: (server_object|false) - исходный объект, если валидный, иначе false.

--// Вернуть клиентский (игровой) объект, если он валидный, иначе false.
game_object_or_false(g_obj)
args:
  g_obj (userdata)(required) - клиентский объект для проверки.
retval: (game_object|false) - исходный объект, если валидный, иначе false.

--// Проверить, жив ли актёр (существует и alive()).
is_actor_alive()
args:
  (none)
retval: (boolean) - true, если актёр существует и жив, иначе false.

--// Безопасно освободить серверный объект по таблице с ID. При force = true освобождает сразу, иначе сначала пытается перевести в офлайн.
safe_release(p, force)
args:
  p (table)(required) - таблица с ID объекта в индексе 1 (например, {id}).
  force (boolean)(optional) - если true, принудительное освобождение; иначе освобождение после перевода в офлайн (по умолчанию false).
retval: (boolean) - true, если объект успешно освобождён или уже освобождён; false, если объект ещё онлайн и не удалось переключить в офлайн.

--// Освободить серверный объект напрямую.
release(se_obj)
args:
  se_obj (userdata)(required) - серверный объект для освобождения.
retval: (none)

--// Создать новый объект через систему ALife.
create(tbl)
args:
  tbl (table)(required) - таблица с параметрами: {section, позиция_вектор, level_vertex_id, game_vertex_id}.
retval: (none)

--// Получить инвентарное имя для секции (из системного .ini, поле inv_name). Если не найдено, возвращает имя секции.
get_inv_name(section)
args:
  section (string)(required) - имя секции.
retval: (string) - локализованное инвентарное имя или имя секции.

--// Безопасно получить позицию указанной кости NPC. Если кость не найдена или объект не сталкер, возвращает позицию над текущей позицией.
safe_bone_pos(npc, bone)
args:
  npc (userdata)(required) - клиентский объект (NPC).
  bone (string)(optional) - имя кости (по умолчанию "bip01_spine").
retval: (vector) - вектор позиции.

--// Безопасно освободить серверный объект по его ID.
safe_release_by_id(id)
args:
  id (number)(required) - ID объекта для освобождения.
retval: (none)

--// Установить состояние (condition) клиентского объекта по его серверному ID. Использует client_spawn_manager для ожидания появления объекта онлайн.
set_condition_by_id(id, float_condition)
args:
  id (number)(required) - серверный ID объекта.
  float_condition (number)(required) - значение состояния (обрезается до 0..1).
retval: (none)

--// Установить состояние клиентского объекта напрямую, обрезая значение от 0 до 1.
set_condition(gobj_client, float_condition)
args:
  gobj_client (userdata)(required) - клиентский объект.
  float_condition (number)(required) - значение состояния (обрезается до 0..1).
retval: (none)

--// Преобразовать серверный или клиентский объект в унифицированную обёртку с безопасными методами. Если входной объект невалидный, возвращает фиктивную обёртку, возвращающую значение по умолчанию.
to_game_object(obj, def_value)
args:
  obj (userdata|table)(required) - объект (серверный или клиентский) для обёртывания.
  def_value (any)(optional) - значение, возвращаемое методами фиктивной обёртки (по умолчанию nil).
retval: (table) - таблица-обёртка с методами: id(), section(), name(), alive(), level_vertex_id(), game_vertex_id(), position(), is_valid(), is_client_object(), is_server_object(), await_online(callback).

--// Тестовая функция для to_game_object; выводит различные свойства обёрнутого объекта. Используется для отладки.
test_to_game_object(_gobj)
args:
  _gobj (userdata)(optional) - объект для теста; если не указан, используется актёр.
retval: (none)
```

### Примеры использований:
```lua
--// Проверка существования актёра
if ffx_object_utils.has_actor() then
    SemiLog("Актёр существует")
end

--// Получение серверного объекта по ID
local se_obj = ffx_object_utils.se_object_by_id_or_false(12345)
if se_obj then
    SemiLog(string.format("Найден серверный объект ID: %d", se_obj.id))
end

--// Проверка клиентского объекта
local g_obj = ffx_object_utils.game_object_or_false(db.actor)
if g_obj then
    SemiLog("Актёр валидный клиентский объект")
end

--// Проверка, жив ли актёр
if ffx_object_utils.is_actor_alive() then
    SemiLog("Актёр жив")
end

--// Безопасное освобождение объекта по таблице с ID
local obj_table = { se_obj_id }
ffx_object_utils.safe_release(obj_table, true)  --// принудительно

--// Создание нового объекта
local position = vector():set(100, 0, 50)
ffx_object_utils.create({"ammo_5.56x45_ss109", position, 0, 0})

--// Получение инвентарного имени
local inv_name = ffx_object_utils.get_inv_name("wpn_ak74")
SemiLog(string.format("Название: %s", inv_name))

--// Получение позиции кости
local pos = ffx_object_utils.safe_bone_pos(db.actor, "bip01_head")
SemiLog(string.format("Позиция головы: %.2f, %.2f, %.2f", pos.x, pos.y, pos.z))

--// Установка состояния (health) по ID
ffx_object_utils.set_condition_by_id(actor_id, 0.8)

--// Использование унифицированной обёртки to_game_object
local wrapped = ffx_object_utils.to_game_object(db.actor)
if wrapped:is_valid() then
    SemiLog(string.format("Объект: %s, ID: %d", wrapped:section(), wrapped:id()))
end

--// Ожидание появления объекта онлайн через await_online
wrapped:await_online(function(gobj)
    SemiLog("Объект теперь онлайн")
end)

--// Тестовая функция (для отладки)
ffx_object_utils.test_to_game_object(db.actor)
```
