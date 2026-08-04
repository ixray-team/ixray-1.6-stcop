# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

### ffx_spawn_utils: `\gamedata\scripts\ixr_framework\utils\ffx_spawn_utils.script`
Утилиты для спавна объектов (создания новых игровых сущностей):
* `spawn_on_ground_by_actor_pos`
* `actor_multiple_spawn_to_backpack`
* `create_item_on_story_object`
* `actor_spawn_random_section_to_backpack`
* `spawn_by_section`
* `spawn_on_ground_for_current_level`
* `actor_spawn_to_backpack`
* `npc_spawn_to_backpack`

#### Описание методов:

```lua
--// Создать указанное количество объектов секции на позиции актёра на земле.
spawn_on_ground_by_actor_pos(section, count)
args:
  section (string)(required) - имя секции объекта.
  count (number)(optional) - количество объектов (по умолчанию 1).
retval: (table|false) - таблица созданных серверных объектов (se_object) или false при ошибке.

--// Создать указанное количество предметов секции в инвентарь актёра.
actor_multiple_spawn_to_backpack(section, count)
args:
  section (string)(required) - имя секции предмета.
  count (number)(optional) - количество (по умолчанию 1).
retval: (table|false) - таблица серверных объектов или false при ошибке.

--// Создать предметы на сюжетном объекте (по story_id). При spawn_into = true предметы помещаются внутрь объекта (в его инвентарь).
create_item_on_story_object(section, story_id, count, spawn_into)
args:
  section (string)(required) - имя секции предмета.
  story_id (string)(required) - сюжетный ID целевого объекта.
  count (number)(optional) - количество (по умолчанию 1).
  spawn_into (boolean)(optional) - если true, создаёт внутри объекта (указывает parent ID); иначе на позиции объекта (по умолчанию false).
retval: (table|false) - таблица серверных объектов или false при ошибке.

--// Создать случайный предмет из списка секций в инвентарь актёра.
actor_spawn_random_section_to_backpack(items_sections)
args:
  items_sections (table)(required) - массив имён секций (строки).
retval: (server_object|false) - созданный серверный объект или false, если список пуст или ошибка.

--// Базовый спавн. Создаёт объект секции в указанной позиции с указанными вершинами и опциональным родительским ID.
spawn_by_section(section, position, lv_id, gv_id, id)
args:
  section (string)(required) - имя секции.
  position (vector)(required) - вектор позиции.
  lv_id (number)(required) - ID вершины уровня (level_vertex_id).
  gv_id (number)(required) - ID игровой вершины (game_vertex_id).
  id (number)(optional) - ID родительского объекта (для помещения в инвентарь/контейнер).
retval: (server_object|false) - серверный объект при успехе, иначе false.

--// Создать объект на земле в указанной позиции на текущем уровне (использует вершины актёра).
spawn_on_ground_for_current_level(section, position)
args:
  section (string)(required) - имя секции.
  position (vector)(required) - вектор позиции.
retval: (server_object|false) - серверный объект или false.

--// Создать предмет в инвентарь актёра.
actor_spawn_to_backpack(section)
args:
  section (string)(required) - имя секции предмета.
retval: (server_object|false) - серверный объект или false при ошибке.

--// Создать предмет в инвентарь указанного NPC (по его серверному ID).
npc_spawn_to_backpack(section, npc_id)
args:
  section (string)(required) - имя секции предмета.
  npc_id (number)(required) - серверный ID NPC.
retval: (server_object|false) - серверный объект или false при ошибке.
```

### Примеры использований:
```lua
--// Создать 3 аптечки на земле около актёра
local spawned = ffx_spawn_utils.spawn_on_ground_by_actor_pos("medkit", 3)
if spawned then
    SemiLog(string.format("Создано объектов: %d", #spawned))
end

--// Добавить 5 патронов в рюкзак актёра
local items = ffx_spawn_utils.actor_multiple_spawn_to_backpack("ammo_5.56x45_ss109", 5)

--// Создать предмет на сюжетном объекте (внутрь ящика)
local box_story = "st_weapon_box_1"
local result = ffx_spawn_utils.create_item_on_story_object("wpn_ak74", box_story, 1, true)

--// Случайный предмет из списка
local random_item = ffx_spawn_utils.actor_spawn_random_section_to_backpack({"medkit", "bandage", "antirad"})

--// Базовый спавн в конкретной позиции
local pos = vector():set(100, 0, 50)
local obj = ffx_spawn_utils.spawn_by_section("ammo_9x19_fmj", pos, actor:level_vertex_id(), actor:game_vertex_id())

--// Спавн на земле в переданной позиции (текущий уровень)
local obj2 = ffx_spawn_utils.spawn_on_ground_for_current_level("grenade_f1", pos)

--// Добавить гранату в рюкзак актёра
local grenade = ffx_spawn_utils.actor_spawn_to_backpack("grenade_f1")

--// Добавить предмет в рюкзак NPC по ID
local npc_id = 12345
local item = ffx_spawn_utils.npc_spawn_to_backpack("medkit", npc_id)
```
