# Новые функции
## Глобальное пространство

```lua
--// Скрипт был вызван на Dedicated сервере
IsDedicated()
retval: boolean

--// Скрипт был вызван на стороне клиента
OnClient()
retval: boolean

--// Скрипт был вызван на стороне сервера
OnServer()
retval: boolean

--// Получить позицию курсора
GetCursorPosition(cursor_pos)
retval: vector2
args: cursor_pos (vector2)

--// Установить позицию курсора
SetCursorPosition(pos)
retval: none
args: pos (vector2)
```

## player_hud

```lua
--// показать/скрыть ноги
player_hud.show_legs(value)
retval: none
args: value (boolean)
```

## CActor

### Новое

* Статус ГГ
```lua
--// Находится ли ГГ в "режиме бога"
db.actor:is_god_mode()
retval: boolean

--// Установить "Режим Бога"
db.actor:SetInvulnerable(value)
retval: none
args: value (boolean)

--// Проверяет, в прыжке ли актер в данный момент
db.actor:ActorIsJump()
retval: boolean

--// Возвращает максимальный вес, который актер может нести
db.actor:GetActorMaxWeight()
retval: number

--// Устанавливает максимальный вес, который актер может нести
db.actor:SetActorMaxWeight(max_weight)
retval: none
args: max_weight (number)

--// Возвращает максимальный вес, при котором актер может передвигаться
db.actor:GetActorMaxWalkWeight()
retval: number

--// Устанавливает максимальный вес, при котором актер может передвигаться
db.actor:SetActorMaxWalkWeight(max_walk_weight)
retval: none
args: max_walk_weight (number)

--// Возвращает дополнительный вес, который костюм актера позволяет нести сверх стандартного лимита
db.actor:GetAdditionalMaxWeight()
retval: number

--// Устанавливает дополнительный вес, который костюм актера позволяет нести сверх стандартного лимита
db.actor:SetAdditionalMaxWeight(add_max_weight)
retval: none
args: add_max_weight (number)

--// Возвращает дополнительный вес, при котором актер может передвигаться
db.actor:GetAdditionalMaxWalkWeight()
retval: number

--// Устанавливает дополнительный вес, при котором актер может передвигаться
db.actor:SetAdditionalMaxWalkWeight(add_max_walk_weight)
retval: none
args: add_max_walk_weight (number)

--// Прямая установка сонливости гг
db.actor:set_actor_sleepiness(value)
retval: none
args: value (number)

--// Прямая установка голода гг
db.actor:set_actor_satiety(value)
retval: none
args: value (number)

--// Прямая установка жажды гг
db.actor:set_actor_thirst(value)
retval: none
args: value (number)

--// Прямая установка здоровья гг
db.actor:set_actor_health(value)
retval: none
args: value (number)

--// Прямая установка стамины гг
db.actor:set_actor_power(value)
retval: none
args: value (number)

--// Прямая установка радиации гг
db.actor:set_actor_radiation(value)
retval: none
args: value (number)

--// Прямая установка пси здоровья гг
db.actor:set_actor_psy_health(value)
retval: none
args: value (number)

--// Прямая установка морали гг
db.actor:set_actor_morale(value)
retval: none
args: value (number)
```
* Камера
```lua
--// Активна ли камера "От первого лица"
db.actor:is_first_person()
retval: boolean

--// Установить камеру "От первого лица"
db.actor:set_first_person()
retval: none

--// Установить камеру "От третьего лица"
db.actor:set_third_person()
retval: none
```
* [Бустеры](/scripting/exported-enums#eboostparams)
```lua
--// Влияет ли бустер на актора (параметр буста из EBoostParams)
db.actor:is_booster_influence(boost_param)
retval: boolean
args: boost_param (EBoostParams)

--// Получить время влияния бустера на актора (параметр буста из EBoostParams)
db.actor:get_booster_influence_time(boost_param)
retval: number
args: boost_param (EBoostParams)

--// Применить бустер (имя секции с параметрами бустера)
db.actor:apply_booster(section_name)
retval: none
args: section_name (string)

--// Установить время влияния бустера (время, параметр буста из EBoostParams)
db.actor:set_booster_time(time, boost_param)
retval: none
args: time (number), boost_param (EBoostParams)

--// Возвращает время действия активного eBoostPowerRestore
db.actor:get_actor_power_boost_time()
retval: number
```
* Тень от ГГ
```lua
--// Проверить включена ли тень от ГГ
db.actor:is_actor_shadow()
retval: boolean

--// Включить/выключить тень от ГГ
db.actor:set_actor_shadow(value)
retval: none
args: value (boolean)
```
* Движение
```lua
--// Получить состояние движения актора (первое - тип движения, например, желаемый (eWishful); второе - команда движения, например спринт (mcSprint))
db.actor:get_movement_state(movement_type, move_command)
retval: boolean
args: movement_type (EMovementStates), move_command (EMoveCommand)

--// Установить состояние команды движения актора (первое - тип движения; второе - команда движения; третье - статус, true\false)
db.actor:set_movement_state(movement_type, move_command, status)
retval: none
args: movement_type (EMovementStates), move_command (EMoveCommand), status (boolean)
```
* Инвентарь
```lua
--// Выключить/включить PDA
db.actor:set_pda_disabled(value)
retval: none
args: value (boolean)

--// Доступен ли PDA
db.actor:is_pda_disabled()
retval: boolean

--// Выключить/включить инвентарь
db.actor:set_inventory_disabled(value)
retval: none
args: value (boolean)

--// Доступен ли инвентарь
db.actor:is_inventory_disabled()
retval: boolean
```
* Взаимодействия
```lua
--// Посадить актера в машину
db.actor:attach_vehicle(car, force)
retval: none
args: car (CScriptGameObject), force (boolean)

--// Высадить актера из машины
db.actor:detach_vehicle(force)
retval: none
args: force (boolean)

--// Возвращает текущий Holder, в котором находится актер
db.actor:get_attached_vehicle()
retval: CScriptGameObject

--// Находится ли актер на лестнице
db.actor:is_ladder()
retval: boolean

--// Возвращает название визуала во время активной Cut Scene
db.actor:get_cutscene_visual()
retval: string

--// Установить активную цель
db.actor:set_best_enemy(enemy)
retval: none
args: enemy (CScriptGameObject)
```
## CoC Extended
* Сервервный alife объект `local se_obj = alife():object(obj_id)`
```lua
--// Узнать иконку персонажа
se_obj:character_icon()
retval: string

--// Установить ранг персонажа
se_obj:set_rank(rank)
retval: none
args: rank (number)

--// Установить профиль персонажа
se_obj:set_profile_name(profile)
retval: none
args: profile (string)

--// Узнать имя персонажа
se_obj:character_name()
retval: string

--// Установить имя персонажа
se_obj:set_character_name(name)
retval: none
args: name (string)
```

* Клиентский alife объект `local npc = level.object_by_id(npc_id) or db.actor`
```lua
--// Установить ранг персонажа
npc:change_character_rank(char_rank)
retval: none
args: char_rank (number)

--// Перебирать объекты вокруг актера до нахождения нужного
npc:iterate_feel_touch(callback)
retval: none
args: callback (function) -- function(obj_id: number): boolean
```

* CActor
```lua
--// Убрать детектор
db.actor:hide_detector()
```

* Weapon `local weapon = db.actor:active_item()`
```lua
--// Получить подсостояние оружия
weapon:get_weapon_substate()
retval: number

--// Получить количество патронов определенного типа
weapon:get_ammo_count_for_type(type)
retval: number
args: type (number)

--// Получить тип основного оружия
weapon:get_main_weapon_type()
retval: number

--// Получить тип оружия
weapon:get_weapon_type()
retval: number

--// Получить секцию патронов для типа
weapon:weapon_get_ammo_section(ammo_type)
retval: string
args: ammo_type (number)

--// Прикрепить аддон к оружию
weapon:weapon_addon_attach(obj)
retval: none
args: obj (CScriptGameObject)

--// Открепить аддон от оружия
weapon:weapon_addon_detach(section, bSpawnToInventory)
retval: none
args: section (string), bSpawnToInventory (boolean)

--// Установить апгрейд
weapon:install_upgrade(name)
retval: boolean
args: name (string)

--// Проверить наличие апгрейда
weapon:has_upgrade(name)
retval: boolean
args: name (string)

--// Итерировать установленные апгрейды с помощью callback-функции
weapon:iterate_installed_upgrades(callback)
retval: none
args: callback (function) -- function(upgrade_sect: string, wpn_obj: self): boolean

--// Проверить предмет на поясе
weapon:IsOnBelt(other_obj)
retval: boolean
args: other_obj (CScriptGameObject)

--// Проиграть анимацию на худе
weapon:play_hud_motion(name, use_mix, state)
retval: number
args: name (string), use_mix (boolean), state (number)

--// Переключить state оружия
weapon:switch_state(state)
retval: none
args: state (number)

--// Получить state худового предмета
weapon:get_state()
retval: number

--// Получить количество патронов 
weapon:ammo_get_count()
retval: number

--// Установить кол-во патронов
weapon:ammo_set_count(count)
retval: none
args: count (number)

--// Получить кол-во патронов в 1 пачке 
weapon:ammo_box_size()
retval: number

--// Число боеприпасов снаряжённых в магазин и патронник
weapon:get_ammo_in_magazine_and_chamber()
retval: number

--// Использует ли оружие патронник
weapon:is_weapon_use_chamber()
retval: boolean
```
* Items
```lua
--// Получить предмет по ID
item:item_on_belt(item_id)
retval: CScriptGameObject
args: item_id (number)
```

## Управление видимостью костей
`local obj = level.object_by_id(obj_id) or db.actor`
```lua
--// Видна ли кость на мировом визуале объекта
obj:is_world_object_bone_visible(boneName)
retval: boolean
args: boneName (string)

--// Установить видимость кости на мировом визуале объекта
obj:set_world_object_bone_visibility(boneName, bVisibility)
retval: boolean
args: boneName (string), bVisibility (boolean)

--// Видна ли кость на худовом визуале объекта
obj:is_hud_object_bone_visible(boneName)
retval: boolean
args: boneName (string)

--// Установить видимость кости на худовом визуале объекта
obj:set_hud_object_bone_visibility(boneName, bVisibility)
retval: boolean
args: boneName (string), bVisibility (boolean)
```

## CCar
`local car = level.object_by_id(car_id)`
```lua
--// Добавить топливо (с учётом предела m_fuel_tank)
car:AddFuel(amount)
retval: none
args: amount (number)

--// Получить текущее топливо
car.fuel
--// Установить текущее топливо
car.fuel = value

--// Получить размер топливного бака
car.fuel_tank
--// Установить размер топливного бака  
car.fuel_tank = value
```

## From Lost Alpha
```lua
--// получить предыдущую погоду
level.get_past_wdesc()
retval: string

--// получить следующую погоду
level.get_next_wdesc()
retval: string

--// получить время исполнения прошлой погоды
level.get_past_wdesc_execution_time()
retval: number

--// получить время следующей погоды
level.get_next_wdesc_execution_time()
retval: number

--// получить погодное время
level.get_weather_game_time()
retval: number

--// установить предыдущую погоду
level.set_past_wdesc(WeatherSection)
retval: none
args: WeatherSection (string)

--// установить следующую погоду
level.set_next_wdesc(WeatherSection)
retval: none
args: WeatherSection (string)
```

## CUIGameCustom
```lua
--// Вывести сообщение на экран
get_hud():AddHudMessage(text)
retval: none
args: text (string)
```

## alife_simulator
```lua
--// Переместить актера на локацию
alife():jump_to_level(name)
retval: none
args: name (string)

--// Переместить alife объект
alife():teleport_object(id, gv_id, lv_id, pos)
retval: none
args: id (number), gv_id (number), lv_id (number), pos (vector)

--// Итерировать информацию объекта
alife():iterate_info(id, callback)
retval: none
args: id (number), callback (function)

--// Репроцессинг спавна
alife():reprocess_spawn(sobj)
retval: none
args: sobj (CScriptGameObject)

--// Установить количество объектов для обновления
alife():set_objects_per_update(count)
retval: none
args: count (number)

--// Установить время обработки
alife():set_process_time(time)
retval: none
args: time (number)

--// Получить дочерние объекты
alife():get_children(sobj)
retval: table
args: sobj (CScriptGameObject)

--// Итератор по alife объектам (Lost Alpha)
alife():objects()
retval: iterator
-- Пример:
-- for id, se_obj in alife():objects() do
--     -- id (number), se_obj (CScriptGameObject)
-- end
```

## CTime
```lua
--// Сохраняет время в сжатом виде (4 байта)
game.get_game_time():save(packet)
retval: none
args: packet (net_packet)

--// Загружает время в сжатом виде (4 байта)
game.get_game_time():load(packet)
retval: none
args: packet (net_packet)
```

## `save`
```lua
--// Передать название текущего чанка в движок (отладочная информация)
save.set_stage(name)
retval: none
args: name (string)

--// Вызвать ошибку при сохранении (отладочная информация)
save.call_error()
retval: none
args: none
```

## [animslot](/animation-system/hud-animator)
```lua
--// Проиграть анимацию на худе
animslot.play(section, anim)
retval: none
args: section (string), anim (string)
```

## [CEatableItem](/gameplay/general/items-used) `local bread = level.object_by_id(item_id)`
```lua
--// Предмет больше не может быть использован
bread:Empty()
retval: boolean

--// Предмет будет удалён, если его нельзя будет использовать
bread:CanDelete()
retval: boolean

--// Кол-во максимальных использований
bread:GetMaxUses()
retval: number

--// Кол-во оставшихся использований
bread:GetRemainingUses()
retval: number

--// Установить кол-во оставшихся использований
bread:SetRemainingUses(count)
retval: none
args: count (number)

--// Текущий вес предмета
bread:Weight()
retval: number

--// Стоимость предмета
bread:Cost()
retval: number

--// Предмет будет удалён, если его нельзя будет использовать (свойство)
bread.m_bRemoveAfterUse
--// Установить значение свойства
bread.m_bRemoveAfterUse = value

--// Начальный вес предмета (свойство)
bread.m_fWeightFull
--// Установить значение свойства
bread.m_fWeightFull = value

--// Вес пустого предмета (свойство)
bread.m_fWeightEmpty
--// Установить значение свойства
bread.m_fWeightEmpty = value
```

## CMapManager
```lua
--// Удаляет указанную локацию на карте
level.map_manager():RemoveMapLocation(ml)
retval: none
args: ml (CMapLocation)

--// Удаляет локацию на карте по идентификатору объекта
level.map_manager():RemoveMapLocationByObjectID(id)
retval: none
args: id (number)

--// Отключает все указатели на карте
level.map_manager():DisableAllPointers()
retval: none
args: none

--// Выполняет функцию для каждой локации с указанным типом и идентификатором
level.map_manager():MapLocationsForEach(spot_type, id, functor)
retval: none
args:
  spot_type (string)
  id        (number)
  functor   (function) -> bool
    true  - продолжить перебор
    false - остановить

--// Выполняет функцию для всех локаций на карте
level.map_manager():AllLocationsForEach(functor)
retval: none
args:
  functor (function) -> bool
    true  - продолжить перебор
    false - остановить

```

## CScriptGameObject `local object = level.object_by_id(item_id) or db.actor`
```lua
--// сделать НПС механиком
object.mechanic = true/false

--// Включить / выключить стрельбу
object:set_fire(bool)
retval: none
args: bool

--// Устанавливает максимальный вес, который InventoryOwner может нести
object:SetCharacterMaxWeight(float)
retval: none
args: float

--// Возвращает суммарный вес инвентаря InventoryOwner'а
object:GetTotalWeight()
retval: float

--// Возвращает вес конкретного предмета
object:Weight()
retval: float

--// Возвращает скорость прыжка актёра
object:GetActorJumpSpeed()
retval: float

--// Устанавливает скорость прыжка актёра
object:SetActorJumpSpeed(float)
retval: none
args: float

--// Возвращает коэффициент спринта актёра
object:GetActorSprintKoef()
retval: float

--// Устанавливает коэффициент спринта актёра
object:SetActorSprintKoef(float)
retval: none
args: float

--// Возвращает коэффициент бега актёра
object:GetActorRunCoef()
retval: float

--// Устанавливает коэффициент бега актёра
object:SetActorRunCoef(float)
retval: none
args: float

--// Возвращает коэффициент бега назад актёра
object:GetActorRunBackCoef()
retval: float

--// Устанавливает коэффициент бега назад актёра
object:SetActorRunBackCoef(float)
retval: none
args: float

--// Меняет здоровье энтити напрямую (минуя стандартный health с дельтой)
object:set_health_ex()
retval: none

--// Добавить кастомный текст к иконке предмета в инвентаре
object:set_sub_inventory_icon_text(text, color, font, offset)
retval: none
args: string, int, string, vector2

--// Добавить кастомную текстуру к иконке предмета в инвентаре
object:set_sub_inventory_icon(mark, offset, size, texture, color)
retval: none
args: bool, vector2, vector2, string, int

```
## CConsole
```lua
--// Регистрация LUA команды в консоли (нужно вызывать каждый раз на старте уровня)
  get_console():register_lua_command(name_command, callable_fn, tips_string)

retval: none
args: 
  name_command, -- Имя команды в консоли
  callable_fn, -- lua функция принимает таблицу аргументов
  tips_string -- подсказки в консоли напротив команды разделитель запятая
```
#### Пример кода Регистрация LUA команды в консоли
```lua
--// Регистрация LUA команды в консоли
-- Пример создания команды которая будет спавнить секцию указанную в первом аргументе в инвентарь к гг
  get_console():register_lua_command(
    "spawn_item_to_actor",
    function (args)
      alife():create(
        args[1],
        db.actor:position(),
        db.actor:level_vertex_id(),
        db.actor:game_vertex_id(),
        db.actor:id()
      )
    end,
    "wpn_pm, wpn_ak_74, bandage" -- подсказки для консоли разделитель запятая
  )
```

## ActorMenu
```lua
--// Получить UI класс ПДА
ActorMenu.get_pda_menu()
retval: CUIPdaWnd

--// Получить UI класс актёра
ActorMenu.get_actor_menu()
retval: CUIActorMenu

--// Получить ID текущего UI
ActorMenu.get_menu_mode()
retval: number

--// Получить UI-класс maingame
ActorMenu.get_maingame()
retval: CUIMainIngameWnd
```

## CUIListBox
```cpp
--// Выделить элемент списка по индексу
listbox:SetSelectedIndex(id)
retval: none
args: int
```

## CScriptGameObject
Теперь можно добавлять кастомный вычисляемый дополнительный текст к описанию предмета.
Полезно для авто генерации дополнительных динамических характеристик предмета.

```lua
--// Получить строку дополнительного описания установленного на инвентарный предмет
object:get_item_additional_description()
retval: string

--// Установить строку дополнительного описания на инвентарный предмет
object:set_item_additional_description(text)
retval: none
args: string

--// Очистить строку дополнительного описания установленного на инвентарный предмет
object:unset_item_additional_description()
retval: none

--// Проверить, установлена ли строка дополнительного описания на инвентарный предмет
object:is_item_used_additional_description()
retval: bool
```

::: details Пример оптимизированного варианта установки дополнительного описания к предмету через скрипт

```lua
  -- Требуется: IXR FRAMEWORK: ^1.0 (в случае отсутствия можно напрямую использовать из _G CUIActorMenu_OnItemFocusReceive)
  -- Подписываемся на коллбек наведения на предмет мыши (IXR FRAMEWORK)
	function on_game_start()
		RegisterScriptCallback("CUIActorMenu_OnItemFocusReceive", this.on_item_focus_receive)
	end

-- Устанавливаем дополнительный текст к описанию предмета с конкретной секцией при наведении мыши на него (в реальном геймплее можно значительно усложнить код и к примеру генерировать характеристики предмета в зависимости от состояния и тому подобное)
	function on_item_focus_receive(item_game_object)
		local trigger_section = "itm_repair_kit_03" -- Секция
		if item_game_object and item_game_object:id() and item_game_object:section() == trigger_section  then
			local min_repair_condition = 30 -- Наше кастомное изменяемое со скрипта значение в нашем переведенном тексте
			local characteristics = {
				game.translate_string("st_additional_characteristics"),
				game.translate_string("st_characteristic_category_repair_kit"),
				game.translate_string("st_characteristic_min_condition_repair_kit") .. tostring(min_repair_condition) .. " %",
			}
			
                        -- Текст можно передавать в метод напрямую но для удобства сделан пример разворачивания из таблицы строковых значений
			item_game_object:set_item_additional_description(table.concat(characteristics, ""))
		end
	end

  ```

  Регистрируем фрагменты строк через языковой файл для корректных переводов
  ```xml
    <string id="st_additional_characteristics">
		<text> \n%c[255,255,255,255]Характеристики: </text>
	</string>

	<string id="st_characteristic_category_repair_kit">
		<text> \n%c[255,255,255,255]• %c[255,255,255,255] Ремонт снаряжения </text>
	</string>

	<string id="st_characteristic_min_condition_repair_kit">
		<text> \n%c[255,255,255,255]• %c[255,255,255,255] Минимальный порог использования: </text>
	</string>
  ```
  
  На выходе получим дополнение к описанию предмета в таком виде с возможностью менять значение(я) в нем при необходимости из скрипта
  ```xml
	Предшествующее описание из конфига ...
	Характеристики: 
		•  Ремонт снаряжения 
		•  Минимальный порог использования: 30 %
  ```
:::

## level
```lua
--// enable/disable rain effector
level.enable_rain(value)
retval: none
args: value (boolean)

--// Switch wallmark visible
level.switch_wallmark(wallmark_object, status)
retval: none
args: wallmark_object(script game object), status (boolean)

--// Get fog distance from current env
level.get_fog_distance()
retval: float

```

## level
```lua
--// Поиск онлайн обьектов по eSpatial в сфере
level.search_online_objects_by_sphere(center_position, radius, table_spatial_types)
retval: none
args: 
  center_position (vector3),
  radius (float),
  table_spatial_types (table)
  
  
--// Допустимые E_SPATIAL типы
level.e_spatial_type.NONE
level.e_spatial_type.INVALIDSECTOR
level.e_spatial_type.RENDERABLE
level.e_spatial_type.LIGHTSOURCE
level.e_spatial_type.LIGHTSOURCEHEMI
level.e_spatial_type.PHYSIC
level.e_spatial_type.SHAPE
level.e_spatial_type.PARTICLE

level.e_spatial_type.COLLIDEABLE
level.e_spatial_type.VISIBLEFORAI
level.e_spatial_type.REACTTOSOUND
level.e_spatial_type.OBSTACLE
level.e_spatial_type.RENDERABLESHADOW

level.e_spatial_type.LADDER

level.e_spatial_type.ACTOR
level.e_spatial_type.ACTOR_DEAD
level.e_spatial_type.ACTOR_ALIVE

level.e_spatial_type.AI
level.e_spatial_type.AI_DEAD
level.e_spatial_type.AI_ALIVE

level.e_spatial_type.STALKER
level.e_spatial_type.STALKER_WOUNDED
level.e_spatial_type.STALKER_DEAD
level.e_spatial_type.STALKER_ALIVE

level.e_spatial_type.MONSTER
level.e_spatial_type.MONSTER_DEAD
level.e_spatial_type.MONSTER_ALIVE

level.e_spatial_type.CROW
level.e_spatial_type.CROW_DEAD
level.e_spatial_type.CROW_ALIVE

level.e_spatial_type.ITEM
level.e_spatial_type.WEAPON
level.e_spatial_type.MISSILE
level.e_spatial_type.ROCKET
level.e_spatial_type.ARTEFACT
level.e_spatial_type.ANOMALY_DETECTOR

level.e_spatial_type.CAR
level.e_spatial_type.HELI

level.e_spatial_type.PHYSIC_OBJECT
level.e_spatial_type.PHYSIC_SHELL_HOLDER
level.e_spatial_type.PHYSIC_OBJECT_DESTR
level.e_spatial_type.PHYSIC_OBJECT_BRKBL
level.e_spatial_type.PHYSIC_MOVEMENT

level.e_spatial_type.INV_BOX

level.e_spatial_type.AI_DOOR

level.e_spatial_type.LIGHT_LAMP

level.e_spatial_type.LEVEL_CHANGER
level.e_spatial_type.SPACE_RESTRICTOR
level.e_spatial_type.ANOMALY_ZONE
level.e_spatial_type.SIM_FACTION
level.e_spatial_type.SMART_TERRAIN
level.e_spatial_type.CAMP_ZONE
level.e_spatial_type.SMART_COVER
level.e_spatial_type.ANOMAL_ZONE_LOGIC
```

## Пример поиск онлайн обьектов по eSpatial в сфере
```lua
--// Центр сферы относительно которого будет произведен поиск
local center = db.actor:position()
--// Радиус поиска
local radius = 120
--// Перечисление типов обьектов для фильтрации поиска
local spatial_types = {
  level.e_spatial_type.SHAPE,
  level.e_spatial_type.STALKER,
}

--// Поиск и печать списка найденных онлайн обьектов
for obj in level.search_online_objects_by_sphere(center, radius, spatial_types) do
    if obj then
      SemiLog(tostring( obj:name() )) --// Распечатать имена обьектов из результата поиска
    end
end
```

## level (runtime storage)
```lua

--// Специальная обертка для проброса строковых луа таблиц между перезагрузками луа машины

--// Проверить наличие таблицы строк в хранилище по имени
level.is_exists_named_stash_string_vector(key_name)
retval: bool
args: key_name (string)

--// Получить таблицу строк из хранилища по имени
level.get_named_stash_string_vector(key_name)
retval: lua table
args: key_name (string)

--// Заполнить таблицу строк в хранилище по имени
level.set_named_stash_string_vector(key_name, table)
retval: void
args:
 key_name (string)
 table (lua table)

--// Удалить таблицу строк из хранилища по имени
level.remove_named_stash_string_vector(key_name)
retval: void
args: key_name (string)

--// Удалить все таблицы строк из хранилища
level.remove_all_named_stash_string_vectors()
retval: void

```

## Примеры (runtime storage)
```lua
--// Проверить наличие таблицы строк в хранилище по имени
if level.is_exists_named_stash_string_vector("my-data") then

end

--// Получить таблицу строк из хранилища по имени
local data = level.get_named_stash_string_vector(key_name)
  
--// Заполнить таблицу строк в хранилище по имени
level.set_named_stash_string_vector(
  "my-data", --// имя хранилища
  {"test", "best"} --// хранимая таблица
)

--// Удалить таблицу строк из хранилища по имени
level.remove_named_stash_string_vector("my-data")

--// Удалить все таблицы строк из хранилища
level.remove_all_named_stash_string_vectors()


--// Комплексное использование
if level.is_exists_named_stash_string_vector("my-data") then
  --// Таблица есть в хранилище - выведем в лог
	SemiLog(tostring(ffx_dump_utils.var_export(
	    level.get_named_stash_string_vector("my-data"))
  ))
else
  --// Таблицы нет в хранилище - заполняем
	level.set_named_stash_string_vector(
    "my-data", 
    {"test", "best"}
	)
end

```
