# Lua API для конфигов, объектов и UI

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4

## Обзор

Здесь описаны экспорты для чтения LTX, работы с костями, экипировкой и полосами прогресса. Имена и сигнатуры соответствуют регистрации Lua в ветке `default`.

## Синтаксис

### Конфиги и объекты уровня

| Вызов | Результат |
| --- | --- |
| `get_cached_ini(path)` | Возвращает общий объект `ini_file` для чтения. Путь относительно `$game_config$`, повторный вызов с той же строкой возвращает объект из кэша. |
| `sim.net_find(id)` | Возвращает `game_object` зарегистрированного объекта текущего уровня или `nil`. |
| `level.enable_rain(value)` | Включает эффекты дождя и молний с `true` и выключает с `false`. Осадки в самом погодном цикле задаются погодными секциями. |

Не храните ссылку на кэшированный `ini_file` после переинициализации ALife. Для доступа из другого сеанса получите объект заново.

### Кости

Методы ниже принимают `game_object` со скелетной моделью. Передавайте имя существующей кости или корректный ID.

| Вызов | Результат |
| --- | --- |
| `obj:bone_position(id_or_name)` | `vector` с мировой позицией кости. |
| `obj:bone_direction(id_or_name)` | `vector` с направлением локальной оси Z кости в мировых координатах. |
| `obj:get_bone_id_by_name(name)` | Числовой ID или `65535`, если кость отсутствует. |
| `obj:get_bone_name_by_id(id)` | Имя кости. |
| `obj:get_root_bone_id()` | ID корневой кости или `65535`, если скелета нет. |
| `obj:get_root_bone_name()` | Имя корневой кости. |

### Экипировка

| Вызов | Результат |
| --- | --- |
| `obj:cast_CustomOutfit()` | Возвращает `CCustomOutfit` или `nil`. |
| `obj:cast_Helmet()` | Возвращает `CHelmet` или `nil`. Имя экспорта — `cast_Helmet`. |
| `armor:GetDefHitTypeProtection(hit_type)` | Числовая защита от типа повреждения из перечисления ALife. |
| `armor:GetBoneArmor(bone_id)` | Числовая броня кости. |

`armor:GetHitTypeProtection(hit_type, bone_id)` принимает числовой ID кости. Передайте `65535` для расчёта без поправки на кость. Коэффициент учитывает состояние предмета и режим баланса брони в ТЧ, ЧН или ЗП.

### Полоса прогресса

| Вызов | Действие |
| --- | --- |
| `bar:ShowBackground(enabled)` | Показывает или скрывает фон виджета. |
| `bar:UseColor(enabled)` | Включает или выключает вычисление цвета по значению прогресса. |
| `bar:SetColor(argb)` | Задаёт цвет текстуры заполнения. При автоматическом выборе цвета движок может его перезаписать. |
| `bar:SetMinColor(argb)` | Задаёт минимальный цвет. |
| `bar:SetMiddleColor(argb)`, `bar:SetMaxColor(argb)` | Задают цвет середины и максимума соответственно. Для цветов градиента в XML используются узлы `min_color`, `middle_color` и `max_color`. |
| `bar:GetProgressStatic()` | Возвращает внутренний `CUIStatic` заполнения. Он принадлежит полосе прогресса. |
| `bar:SnapProgressPos()` | Сразу переводит отображаемую позицию в целевую после `SetProgressPos`. |

### Окна и сортировка инвентаря

| Вызов | Действие |
| --- | --- |
| `window:FindWindow(name)` | Ищет окно по имени в текущем окне и его потомках. Возвращает `CUIWindow` или `nil`. |
| `window:FindStatic(name)` | Выполняет тот же поиск. Возвращает `CUIStatic` или `nil`, если первый найденный элемент нельзя привести к `CUIStatic`. |
| `menu:SetInventorySortSystem(system)` | Переключает сортировку. `"ordering"` выбирает сортировку по порядку, `"categories"` — по категориям. Пустая строка оставляет текущий режим. |
| `menu:GetInventorySortSystem()` | Возвращает `"ordering"` или `"categories"`. |

Вызывайте `ActorMenu.get_actor_menu()` после создания игрового интерфейса. Если меню доступно, используйте возвращённый объект `CUIActorMenu`.

### Медикаментозная интоксикация

`CEntityCondition:ChangeIntoxication(delta)` прибавляет числовую дельту к интоксикации актёра, если включён `[gameplay] EnableMedIntoxication`. Значение ограничивается диапазоном `0–1`. `CEntityCondition:GetIntoxication()` возвращает текущее число. Экспорт `CActor:conditions()` даёт доступ к объекту состояния.

### Классы объектов и активные предметы

`clsid_table` — глобальная Lua-таблица. Ключи содержат скриптовые имена классов, значения — числовые ID текущей регистрации. Таблицу можно обходить через `pairs`.

`owner:iterate_activeitems(callback, context)` вызывает `callback(owner, item)` для предмета в активном слоте актёра и для предмета в `DEVICE_SLOT`, когда они есть. Оба аргумента колбэка имеют тип `game_object`. `context` принимается, но в колбэк не передаётся.

## Примеры

### Прочитать настройку из LTX

```lua
local ini = get_cached_ini("engine_external.ltx")
if ini:line_exist("gameplay", "Enable3DIcons") then
    local enabled = ini:r_bool("gameplay", "Enable3DIcons")
    printf("Enable3DIcons = %s", tostring(enabled))
end
```

### Мгновенно обновить полосу

```lua
local function show_progress(bar, value)
    bar:ShowBackground(true)
    bar:UseColor(false)
    bar:SetColor(GetARGB(255, 255, 255, 255))
    bar:SetProgressPos(value)
    bar:SnapProgressPos()
end
```

Передайте инициализированный `CUIProgressBar` и значение в его диапазоне.

## См. также

- [Справочник Lua API](new-functions.md)
- [Идентификаторы и колбэки объектов](lua-callbacks.md)
- [Вес и полосы прогресса](../interface/weight-progress-bar.md)
