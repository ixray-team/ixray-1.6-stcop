# Новые функции
## Глобальное пространство

```cpp
bool IsDedicated(); //-- Скрипт был вызван на Dedicated сервере
bool OnClient();    //-- Скрипт был вызван на стороне клиента 
bool OnServer();    //-- Скрипт был вызван на стороне сервера

Fvector2 GetCursorPosition()     //-- Получить позицию курсора
void SetCursorPosition(Fvector2) //-- Установить позицию курсора
```

## player_hud

```cpp
void show_legs(bool) //-- показать/скрыть ноги
```

## CActor

### Новое

* Статус ГГ
```cpp
bool is_god_mode()
//-- Находится ли ГГ в "режиме бога"

void SetInvulnerable(bool)
//-- Установить "Режим Бога"

bool ActorIsJump();
//-- Проверяет, в прыжке ли актер в данный момент.

float GetActorMaxWeight() const;
//-- Возвращает максимальный вес, который актер может нести.

void SetActorMaxWeight(float max_weight);
//-- Устанавливает максимальный вес, который актер может нести.

float GetActorMaxWalkWeight() const;
//-- Возвращает максимальный вес, при котором актер может передвигаться.

void SetActorMaxWalkWeight(float max_walk_weight);
//-- Устанавливает максимальный вес, при котором актер может передвигаться.

float GetAdditionalMaxWeight() const;
//-- Возвращает дополнительный вес, который костюм актера позволяет нести сверх стандартного лимита.

void SetAdditionalMaxWeight(float add_max_weight);
//-- Устанавливает дополнительный вес, который костюм актера позволяет нести сверх стандартного лимита.

float GetAdditionalMaxWalkWeight() const;
//-- Возвращает дополнительный вес, при котором актер может передвигаться.

void SetAdditionalMaxWalkWeight(float add_max_walk_weight);
//-- Устанавливает дополнительный вес, при котором актер может передвигаться.
```
* Камера
```cpp
bool is_first_person();  //-- Активна ли камера "От первого лица"
void set_first_person(); //-- Установить камеру "От первого лица"
void set_third_person(); //-- Установить камеру "От третьего лица"
```
* [Бустеры](/scripting/exported-enums#eboostparams)
```cpp
bool is_booster_influence(EBoostParams);        //-- Влияет ли бустер на актора (параметр буста из EBoostParams)
float get_booster_influence_time(EBoostParams); //-- Получить время влияния бустера на актора (параметр буста из EBoostParams)

void apply_booster(string);                     //-- Применить бустер (имя секции с параметрами бустера)
void set_booster_time(number, EBoostParams);    //-- Установить время влияния бустера (время, параметр буста из EBoostParams)
float get_actor_power_boost_time()              //-- Возвращает время действия активного eBoostPowerRestore 
```
* Тень от ГГ
```cpp
bool is_actor_shadow();      //-- Проверить включена ли тень от ГГ
void set_actor_shadow(bool); //-- Включить/выключить тень от ГГ
```
* Движение
```cpp
bool get_movement_state(EMovementStates, EMoveCommand);       //-- Получить состояние движения актора (первое - тип движения, например, желаемый (eWishful); второе - команда движения, например спринт (mcSprint))
void set_movement_state(EMovementStates, EMoveCommand, bool); //-- Установить состояние команды движения актора (первое - тип движения; второе - команда движения; третье - статус, true\false)
```
* Инвентарь
```cpp
void set_pda_disabled(bool)
//-- Выключить/включить PDA

bool is_pda_disabled()
//-- Доступен ли PDA

void set_inventory_disabled(bool)
//-- Выключить/включить инвентарь

bool is_inventory_disabled()
//-- Доступен ли инвентарь
```
* Взаимодействия
```cpp
void attach_vehicle(CScriptGameObject* Car, bool force)
 //-- Посадить актера в машину

void detach_vehicle(bool force)
//-- Высадить актера из машины

CScriptGameObject* get_attached_vehicle()
//-- Возвращает текущий Holder, в котором находится актер

bool is_ladder()
//-- Находится ли актер на лестнице

string get_cutscene_visual()
//-- Возвращает название визуала во время активной Cut Scene

void set_best_enemy(CScriptGameObject*)
//-- Установить активную цель
```
## CoC Extended
* Weapon
```cpp
//-- State
u8 get_weapon_substate();
int get_ammo_count_for_type(type);
u32 get_main_weapon_type();
u32 get_weapon_type();

string weapon_get_ammo_section(ammo_type);
void weapon_addon_attach(obj);
void weapon_addon_detach(section, bSpawnToInventory);

//-- Upgrades
bool install_upgrade(name);
bool has_upgrade(name);
void iterate_installed_upgrades();
```
* Other
```cpp
void set_character_icon(icon)               //-- Установить иконку персонажа
void change_character_rank(char_rank)       //-- Установить ранг персонажа
LPCSTR character_name()                     //-- Узнать имя персонажа
LPCSTR character_icon()                     //-- Узнать иконку персонажа
void set_rank(rank)                         //-- Установить ранг персонажа
void set_profile_name(profile)              //-- Установить профиль персонажа
void set_character_name(name)               //-- Установить имя персонажа
void iterate_feel_touch(function bool(id)); //-- Перебирать объекты вокруг актера до нахождения нужного
void hide_detector();                       //-- Убрать детектор
bool IsOnBelt(obj);                         //-- Проверить предмет на поясе
obj item_on_belt(ItemID);                   //-- Получить предмет по ID
u32 play_hud_motion(Name, UseMix, state);   //-- Проиграть анимацию на худе
void switch_state(state);                   //-- Переключить state оружия
u32 get_state();                            //-- Получить state худового предмета
u16 ammo_get_count();                       //-- Получить количество патронов 
void AmmoSetCount(count);                   //-- Установить кол-во патронов
int AmmoBoxSize();                          //-- Получить кол-во патронов в 1 пачке 
int get_ammo_in_magazine_and_chamber()      //-- Число боеприпасов снаряжённых в магазин и патронник
bool is_weapon_use_chamber()                //-- Использует ли оружие патронник
```
## CScriptGameObject управление видимостью костей
```cpp
bool is_world_object_bone_visible(string boneName)                        //-- Видна ли кость на мировом визуале обьекта
bool set_world_object_bone_visibility(string boneName, bool bVisibility)  //-- Установить видимость кости на мировом визуале обьекта
bool is_hud_object_bone_visible(string boneName)                          //-- Видна ли кость на худовом визуале обьекта
bool set_hud_object_bone_visibility(string boneName, bool bVisibility)    //-- Установить видимость кости на худовом визуале обьекта
```
## CCar
```cpp
void AddFuel(float);   //-- Добавить топливо (с учётом предела m_fuel_tank)
property fuel;         //-- Свойство объекта: текущее топливо
property fuel_tank;    //-- Свойство объекта: размер топливного бака
```

### From Lost Alpha
```cpp
LPCSTR get_past_wdesc();                    //-- получить предыдущую погоду
LPCSTR get_next_wdesc();                    //-- получить следующую погоду
float get_past_wdesc_execution_time();      //-- получить время исполнения прошлой погоды
float get_next_wdesc_execution_time();      //-- получить время следующей погоды
float get_weather_game_time();              //-- получить погодное время
void set_past_wdesc(LPCSTR WeatherSection); //-- установить предыдущую погоду
void set_next_wdesc(LPCSTR WeatherSection); //-- установить следующую погоду
```

## CUIGameCustom
```lua
AddHudMessage(string) -- Вывести сообщение на экран 
```

## alife_simulator
```lua
jump_to_level(name)                    //-- Переместить актера на локацию 
teleport_object(id, gv_id, lv_id, pos) //-- Переместить alife объект 
iterate_info(id, function)
reprocess_spawn(sobj) 
set_objects_per_update(count)
set_process_time(time)
get_children(sobj)
```
* Object Iterator (Lost Alpha)
```lua
for id, se_obj in alife():objects() do
    ...
```

## game
### CTime
```lua
save(packet) --// Сохраняет время в сжатом виде (4 байта) 
load(packet) --// Загружает время в сжатом виде (4 байта) 
```
## `save`
```lua
set_stage(name) --// Передать название текущего чанка в движок (отладочная информация)
call_error()    --// Вызвать ошибку при сохранении (отладочная информация)
```
## [animslot](/animation-system/hud-animator)
```lua
animslot.play(section, anim) //-- Проиграть анимацию на худе 
```

## [CEatableItem](/gameplay/general/items-used)
* Функции
```cpp
bool Empty();              //-- Предмет больше не может быть использован
bool CanDelete();          //-- Предмет будет удалён, если его нельзя будет использовать
bool GetMaxUses();         //-- Кол-во максимальных использований 
u8 GetRemainingUses();     //-- Кол-во оставшихся использований
void SetRemainingUses(u8); //-- Установить кол-во оставшихся использований
float Weight();            //-- Текущий вес предмета
int Cost();                //-- Стоимость предмета
```
* Свойства 
```cpp
bool m_bRemoveAfterUse;  //-- Предмет будет удалён, если его нельзя будет использовать
float m_fWeightFull;     //-- Начальный вес предмета 
float m_fWeightEmpty;    //-- Вес пустого предмета 
```

## CMapManager
* Функции
```cpp
void RemoveMapLocation(CMapLocation* ml); 
//-- Удаляет указанную локацию на карте

void RemoveMapLocationByObjectID(u16 id); 
//-- Удаляет локацию на карте по идентификатору объекта

void DisableAllPointers(); 
//-- Отключает все указатели на карте

void MapLocationsForEach(LPCSTR spot_type, u16 id, const luabind::functor<bool>& functor); 
//-- Выполняет заданную функцию для каждой локации на карте с указанным типом и идентификатором

void AllLocationsForEach(const luabind::functor<bool>& functor); 
//-- Выполняет заданную функцию для всех локаций на карте
```

::: details Примеры

```lua
  -- Пример использования RemoveMapLocationByObjectID
  mapManager:RemoveMapLocationByObjectID(123)

  -- Пример использования RemoveMapLocation
  local location = mapManager:GetMapLocation("spot_type", 123)
  mapManager:RemoveMapLocation(location)

  -- Пример использования DisableAllPointers
  mapManager:DisableAllPointers()

  -- Пример использования MapLocationsForEach
  mapManager:MapLocationsForEach("spot_type", 123, function(location)
	  print(location:GetHint())
	  return false -- возвращает true, если нужно прервать итерацию
  end)

  -- Пример использования AllLocationsForEach
  mapManager:AllLocationsForEach(function(location)
	  print(location:GetHint())
	  return false -- возвращает true, если нужно прервать итерацию
  end)
  ```
:::

## CMapLocation
* Функции
```cpp
bool HintEnabled(); 
//-- Проверяет, включены ли подсказки для локаций на карте

LPCSTR GetHint(); 
//-- Возвращает подсказку, связанную с локацией на карте

void SetHint(const shared_str& hint); 
//-- Устанавливает подсказку для локации на карте

bool PointerEnabled(); 
//-- Проверяет, включены ли указатели для локаций на карте

void EnablePointer(); 
//-- Включает указатель для локации на карте

void DisablePointer(); 
//-- Отключает указатель для локации на карте

LPCSTR GetType() const; 
//-- Возвращает тип локации на карте

Fvector2 SpotSize(); 
//-- Возвращает размер метки на карте

bool IsUserDefined() const; 
//-- Проверяет, является ли локация на карты объявленной пользователем

void SetUserDefinedFlag(BOOL state); 
//-- Устанавливает флаг объявления локации пользовательской 

void HighlightSpot(bool state, const Fcolor& color); 
//-- Подсвечивает метку на карте

bool Collidable() const; 
//-- Проверяет, является ли локация на каре коллидирующей

bool SpotEnabled(); 
//-- Проверяет, включена ли метка для локации на карте

void EnableSpot(); 
//-- Включает метку для локации на карте

void DisableSpot(); 
//-- Отключает метку для локации на карте

const shared_str& GetLevelName(); 
//-- Возвращает название уровня, связанного с локацией на карте

const Fvector2& GetPosition(); 
//-- Возвращает позицию локации на карте

u16 ObjectID(); 
//-- Возвращает идентификатор объекта локации на карте

Fvector GetLastPosition(); 
//-- Возвращает последнюю известную позицию локации на карте
```
::: details Примеры

```lua
  -- Пример использования HintEnabled
  if location:HintEnabled() then
      print("Подсказка включена")
  end

  -- Пример использования GetHint
  local hint = location:GetHint()
  print("Подсказка: " .. hint)

  -- Пример использования SetHint
  location:SetHint("Новая подсказка")

  -- Пример использования PointerEnabled
  if location:PointerEnabled() then
      print("Указатель включен")
  end

  -- Пример использования EnablePointer
  location:EnablePointer()

  -- Пример использования DisablePointer
  location:DisablePointer()

  -- Пример использования GetType
  local type = location:GetType()
  print("Тип местоположения: " .. type)

  -- Пример использования SpotSize
  local size = location:SpotSize()
  print("Размер метки: " .. size.x .. ", " .. size.y)

  -- Пример использования IsUserDefined
  if location:IsUserDefined() then
      print("Пользовательское местоположение")
  end

  -- Пример использования SetUserDefinedFlag
  location:SetUserDefinedFlag(true)

  -- Пример использования HighlightSpot
  local color = {r = 1, g = 0, b = 0, a = 1}
  location:HighlightSpot(true, color)

  -- Пример использования Collidable
  if location:Collidable() then
      print("Местоположение коллидирующее")
  end

  -- Пример использования SpotEnabled
  if location:SpotEnabled() then
      print("Метка включена")
  end

  -- Пример использования EnableSpot
  location:EnableSpot()

  -- Пример использования DisableSpot
  location:DisableSpot()

  -- Пример использования GetLevelName
  local levelName = location:GetLevelName()
  print("Название уровня: " .. levelName)

  -- Пример использования GetPosition
  local position = location:GetPosition()
  print("Позиция: " .. position.x .. ", " .. position.y)

  -- Пример использования ObjectID
  local id = location:ObjectID()
  print("ID объекта: " .. id)

  -- Пример использования GetLastPosition
  local lastPosition = location:GetLastPosition()
  print("Последняя позиция: " .. lastPosition.x .. ", " .. lastPosition.y .. ", " .. lastPosition.z)
  ```
:::

## CScriptGameObject

* Свойства

```csharp
property bool mechanic //-- set - сделать НПС механником; get - является ли NPC механником
```

* Функции

```cpp
void set_fire(bool)
//-- Включить/выключить стрельбу

void SetCharacterMaxWeight(float)
//-- Устанавливает максимальный вес, который InventoryOwner может нести.

float GetTotalWeight() const;
//-- Возвращает суммарный вес инвентаря InventoryOwner'a.

float Weight() const;
//-- Возвращает вес конкретного предмета.

float GetActorJumpSpeed() const;
//-- Возвращает скорость прыжка актера.

void SetActorJumpSpeed(float jump_speed);
//-- Устанавливает скорость прыжка актера.

float GetActorSprintKoef() const;
//-- Возвращает коэффициент спринта актера.

void SetActorSprintKoef(float sprint_koef);
//-- Устанавливает коэффициент спринта актера.

float GetActorRunCoef() const;
//-- Возвращает коэффициент бега актера.

void SetActorRunCoef(float run_coef);
//-- Устанавливает коэффициент бега актера.

float GetActorRunBackCoef() const;
//-- Возвращает коэффициент бега назад актера.

void SetActorRunBackCoef(float run_back_coef);
//-- Устанавливает коэффициент бега назад актера.

void set_health_ex()
//-- Меняем здоровье ентити напрямую, а не через стандартный health (с дельтой)

void set_sub_inventory_icon_text(LPCSTR m_custom_text, int item_custom_text_clr_inv, LPCSTR item_custom_text_font, Fvector2 m_custom_text_offset);
//-- Добавить кастомный текст к иконке предмета в инвентаре

void set_sub_inventory_icon(bool m_custom_mark, Fvector2 m_custom_mark_offset, Fvector2 m_custom_mark_size, LPCSTR m_custom_mark_texture, int m_custom_mark_clr);
//-- Добавить кастомную текстуру к иконке предмета в инвентаре
```

::: details Примеры

```lua
  -- Пример использования Добавить кастомный текст к иконке предмета в инвентаре
  item:set_sub_inventory_icon_text("22123", GetARGB(255, 128, 155, 255), "font_product_sans_14", vector2():set(10, 10))
  
  -- Пример использования Добавить кастомную текстуру к иконке предмета в инвентаре
  item:set_sub_inventory_icon(true, vector2():set(3, 3), vector2():set(15, 15), "ui_inGame2_inventory_status_bar", GetARGB(255, 128, 155, 255))

  -- Пример использования ActorIsJump
  if actor:ActorIsJump() then
      print("Актер прыгает")
  end

  -- Пример использования GetActorMaxWeight
  local maxWeight = actor:GetActorMaxWeight()
  print("Максимальный вес актера: " .. maxWeight)

  -- Пример использования SetActorMaxWeight
  actor:SetActorMaxWeight(100.0)

  -- Пример использования GetActorMaxWalkWeight
  local maxWalkWeight = actor:GetActorMaxWalkWeight()
  print("Максимальный вес для ходьбы: " .. maxWalkWeight)

  -- Пример использования SetActorMaxWalkWeight
  actor:SetActorMaxWalkWeight(80.0)

  -- Пример использования GetAdditionalMaxWeight
  local additionalWeight = actor:GetAdditionalMaxWeight()
  print("Дополнительный вес: " .. additionalWeight)

  -- Пример использования SetAdditionalMaxWeight
  actor:SetAdditionalMaxWeight(20.0)

  -- Пример использования GetAdditionalMaxWalkWeight
  local additionalWalkWeight = actor:GetAdditionalMaxWalkWeight()
  print("Дополнительный вес для ходьбы: " .. additionalWalkWeight)

  -- Пример использования SetAdditionalMaxWalkWeight
  actor:SetAdditionalMaxWalkWeight(15.0)

  -- Пример использования GetTotalWeight
  local totalWeight = actor:GetTotalWeight()
  print("Суммарный вес инвентаря: " .. totalWeight)

  -- Пример использования Weight
  local itemWeight = actor:Weight()
  print("Вес предмета: " .. itemWeight)

  -- Пример использования GetActorJumpSpeed
  local jumpSpeed = actor:GetActorJumpSpeed()
  print("Скорость прыжка актера: " .. jumpSpeed)

  -- Пример использования SetActorJumpSpeed
  actor:SetActorJumpSpeed(5.0)

  -- Пример использования GetActorSprintKoef
  local sprintKoef = actor:GetActorSprintKoef()
  print("Коэффициент спринта: " .. sprintKoef)

  -- Пример использования SetActorSprintKoef
  actor:SetActorSprintKoef(1.5)

  -- Пример использования GetActorRunCoef
  local runCoef = actor:GetActorRunCoef()
  print("Коэффициент бега: " .. runCoef)

  -- Пример использования SetActorRunCoef
  actor:SetActorRunCoef(1.2)

  -- Пример использования GetActorRunBackCoef
  local runBackCoef = actor:GetActorRunBackCoef()
  print("Коэффициент бега назад: " .. runBackCoef)

  -- Пример использования SetActorRunBackCoef
  actor:SetActorRunBackCoef(0.8)
  ```
:::

## ActorMenu
```lua
ActorMenu.get_pda_menu()   //-- Получить UI класс ПДА
ActorMenu.get_actor_menu() //-- Получить UI класс Актера
ActorMenu.get_menu_mode()  //-- Получить ID текущего UI
ActorMenu.get_maingame()   //-- Получить ID класс maingame
```

## CUIListBox
```cpp
void SetSelectedIndex(id) //-- Выделить элемент
```

## CScriptGameObject
Теперь можно добавлять кастомный вычисляемый дополнительный текст к описанию предмета.
Полезно для авто генерации дополнительных динамических характеристик предмета.

```lua
-- Получить строку дополнительного описания установленного на инвентарный предмет.
string get_item_additional_description()

-- Установить строку дополнительного описания на инвентарный предмет.
void set_item_additional_description(string)

-- Очистить строку дополнительного описания установленного на инвентарный предмет.
void unset_item_additional_description()

-- Установлена ли строка дополнительного описания на инвентарный предмет.
bool is_item_used_additional_description()
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
