# Справка по механике объема инвентаря

## Область применения

Документ описывает механику объема инвентаря и ее поведение в рантайме.
Механика добавляет второе ограничение поверх обычного веса.
Каждый предмет имеет собственный объем, рюкзак и костюм задают доступную емкость.
При превышении базовой емкости начинаются плавные штрафы, при достижении жесткого предела блокируется спринт и подбор предметов.

Главная формула в одну строку:

overload = volume_in_ruck / capacity

Где capacity складывается из базовой емкости инвентаря актора, переопределений от костюма и переопределений от рюкзака.

## Включение

Механика выключена по умолчанию. Активируется одним флагом в `gamedata/configs/engine_external.ltx`:

```
[gameplay]
EnableInventoryVolume = true
```

При `false` система не читает конфиг, не считает объем и не накладывает штрафов. Все API возвращают нейтральные значения. UI игнорирует элементы объема даже если они присутствуют в xml.

## Расположение конфигурации

Все параметры лежат в одном файле:

```
gamedata/configs/volume_system.ltx
```

Файл поддерживает DLTX-переопределение. Моды могут переопределять отдельные секции и значения через `mod_volume_system_*.ltx` рядом с базовым файлом.

## Секции конфигурации

### `[volume_system]`

Базовые параметры системы.

```
base_actor_volume                  = 56.0
default_weight_volume_multiplier   = 1.0
hard_overload_limit                = 1.4
block_pickup_at_hard_limit         = true
recursive_container_volume         = false
```

- `base_actor_volume` начальная емкость актора без рюкзака и костюма. Если 0, берется `[inventory] max_ruck` из `system.ltx`.
- `default_weight_volume_multiplier` множитель для предметов без явного объема. Объем считается как `Weight() * default_weight_volume_multiplier`.
- `hard_overload_limit` верхняя граница перегруза. При `overload >= hard_overload_limit` срабатывает блокировка подбора и достигается максимум штрафов.
- `block_pickup_at_hard_limit` если `true`, при достижении жесткого предела `CanAddToRuck` возвращает `false`.
- `recursive_container_volume` опциональный учет вложенных предметов. Подробности в разделе "Опции рекурсии".

### `[soft_penalties]`

Базовые значения мягких штрафов. К итоговому значению применяется множитель `smoothstep` от 1.0 до `hard_overload_limit`.

```
stamina_power_penalty              = 0.25
max_walk_weight_penalty            = 0.0
aim_sway_penalty                   = 0.0
sprint_block_factor                = 1.2
```

- `stamina_power_penalty` добавка к расходу стамины при перегрузе.
- `max_walk_weight_penalty` вычитается из `max_walk_weight`. Влияет на порог "не могу идти".
- `aim_sway_penalty` множитель разброса для разброса оружия.
- `sprint_block_factor` бинарный порог. При `overload >= sprint_block_factor` спринт отключается.

### `[item_volumes]`

Явные значения объема по секции предмета. Имеют наивысший приоритет.

```
[item_volumes]
medkit          = 0.6
bandage         = 0.2
af_medusa       = 0.3
```

Предмет, не указанный в секции, получает объем `Weight() * default_weight_volume_multiplier`.

### `[container_profiles]`

Именованные профили емкости. Используются для группировки одинаковых по емкости предметов.

```
[container_profiles]
outfit_light     = 56.0
outfit_medium    = 56.0
outfit_heavy     = 56.0
small_backpack   = 45.0
medium_backpack  = 61.0
large_backpack   = 80.0
```

### `[outfit_sections]` и `[container_sections]`

Привязка секции костюма или рюкзака к профилю.

```
[outfit_sections]
stalker_outfit          = outfit_medium
exo_outfit              = outfit_heavy

[container_sections]
backpack_small          = small_backpack
backpack_large          = large_backpack
```

### `[outfit_capacity_overrides]` и `[container_capacity_overrides]`

Точечное переопределение емкости для конкретной секции. Имеет приоритет над профилем.

```
[outfit_capacity_overrides]
exo_outfit              = 64.0
```

## Формула расчета

1. Объем предмета:

```
volume(item) = item_volumes[section]                       если задан
             | Weight(kg) * default_weight_volume_multiplier  иначе
```

2. Текущий объем рюкзака:

```
ruck_volume = sum( volume(item) для item в m_ruck )
```

3. Емкость:

```
capacity = base_actor_volume
         | applyOutfit(outfit.section)      через outfit_sections и outfit_capacity_overrides
         | applyBackpack(backpack.section)  через container_sections и container_capacity_overrides
```

Внутри `applyOutfit` и `applyBackpack` сначала применяется профиль, затем точечный override.

4. Фактор перегруза:

```
overload = ruck_volume / capacity
```

5. Кривая мягкого штрафа:

```
curve = smoothstep(1.0, hard_overload_limit, overload)
```

`smoothstep` это кубическая интерполяция вида `x * x * (3.0 - 2.0 * x)` после линейного масштабирования.

6. Итоговые штрафы:

```
stamina_power_penalty  = config.stamina_power_penalty   * curve
max_walk_weight_penalty = config.max_walk_weight_penalty * curve
aim_sway_penalty       = config.aim_sway_penalty        * curve
block_sprint           = overload >= sprint_block_factor
block_pickup           = overload >= hard_overload_limit и block_pickup_at_hard_limit
```

7. Проверка возможности положить новый предмет:

```
canAdd = (ruck_volume + volume(item)) <= capacity * hard_overload_limit
```

## Поведение в геймплее

- Меньше 1.0: эффектов нет.
- От 1.0 до `hard_overload_limit`: стамина расходуется быстрее, прицел гуляет сильнее, ходьба может быть ограничена.
- При `overload >= sprint_block_factor`: спринт мгновенно отключается.
- При `overload >= hard_overload_limit`: достигнут максимум штрафов, новые предметы не подбираются если `block_pickup_at_hard_limit = true`.

Все штрафы возвращаются через структуру `SInventoryVolumePenalty`:

```
overloadFactor
curve
staminaPowerPenalty
maxWalkWeightPenalty
aimSwayPenalty
blockSprint
blockPickup
```

## Область применения

Система объема инвентаря применяется только к игроку (`CActor`). НПС, торговцы, монстры и машины используют классическую логику без объемного ограничения. Это сделано осознанно на данном этапе:

- НПС инициализируются через логику, в которой задано конкретное оружие и снаряжение. Применение объемного лимита ломало бы эту инициализацию и приводило к подбору только легкого оружия.
- Торговля в окне трейда не блокируется по объему со стороны партнера-НПС. Игрок при этом проверяется как обычно.
- Подбор предметов сталкерами и трупоедством не ограничивается.

Реализация: `CInventoryVolumeSystem::CanAddToRuck` и `GetPenalty` внутри проверяют `cast_actor()` владельца инвентаря и сразу возвращают нейтральные значения для не-актора. Виртуальный диспетчер уже есть в иерархии `CInventoryOwner`, дополнительной настройки не требуется.

## Опции рекурсии

`recursive_container_volume` это опциональный учет вложенных предметов через `CAttachmentOwner::attached_objects()`.

Защита от двойного учета:

- Если для предмета задан явный объем в `[item_volumes]`, он считается финальным. Рекурсия для него не запускается.
- Если объем считается по весу, объемы дочерних предметов добавляются сверху.

Ограничения:

- Максимальная глубина рекурсии 8 уровней.
- При выключенном флаге код рекурсии не вызывается.
- Применяется только к предметам, реализующим `CAttachmentOwner`.

В чистом сборе движка без модов-контейнеров рекурсия даст 0, так как стандартные предметы не имеют `attached_objects` в `m_ruck`. Опция предназначена для модов, добавляющих собственные предметы-контейнеры.

## UI окна инвентаря

Все UI-узлы объема опциональны и независимы. Можно подключать любую их комбинацию или не подключать ничего, если объемная система используется только как геймплейный модификатор.

Поддерживаются две раскладки одновременно: вложенная в контейнер `actor_weight_row` и плоская в корне меню. Движок выбирает ветку через `NavigateToNode`.

### Раскладка с контейнером `actor_weight_row`

Подходит для модов, делающих собственное оформление с группой "вес плюс объем" в одной строке.

Поддерживаемые дочерние узлы:

- `actor_weight_caption` подпись веса.
- `actor_weight` текст текущего веса.
- `weight_status_bar` прогресс-бар веса. Если задан, заменяет `actor_weight`.
- `actor_weight_max` текст максимального веса.
- `volume_caption` подпись объема.
- `volume_status_bar` прогресс-бар объема, привязан к фактору перегруза `ruck_volume / capacity`.
- `actor_volume` текст текущего объема, формат `"15.5"`.
- `actor_volume_max` текст максимального объема, формат `"/ 50.0"`.

### Плоская раскладка в корне

Если `actor_weight_row` отсутствует, движок берет узлы из корня `actor_menu`. Это поведение совместимо с legacy-разметкой:

- `actor_weight_caption`, `actor_weight`, `actor_weight_max` для веса.
- `volume_caption`, `volume_status_bar`, `actor_volume`, `actor_volume_max` для объема.

### Тултип

При наведении на `weight_status_bar` или `volume_status_bar` показывается тултип со значениями веса, объема, емкости и фактора перегруза.

### Базовые override-файлы

```
gamedata/configs/ui/mod_actor_menu_volume.xml
gamedata/configs/ui/mod_actor_menu_16_volume.xml
```

Файл 4:3 добавляет полный `actor_weight_row` с подписью объема и баром через `override="add"`. Файл 16:9 добавляет `volume_caption` и `volume_status_bar` напрямую в корень, без обертки, чтобы аккуратно встроиться в существующий блок веса legacy-разметки.

### Архитектура C++

Инициализация UI разнесена на две функции:

- `CUIActorMenu::InitActorWeightSection` создает только узлы веса.
- `CUIActorMenu::InitActorVolumeSection` создает только узлы объема, прикрепляет их к контейнеру `actor_weight_row` если он создан, иначе к корню.

Обе функции вызываются друг за другом в `UIActorMenuInitialize.cpp`. Любое расширение блока веса или блока объема выполняется в своей функции и не задевает соседнюю.

## HUD-индикатор перегруза

Помимо UI инвентаря, доступен значок поверх внутриигрового HUD, аналогичный `indicator_overweight`, `indicator_radiation`, `indicator_bleeding`.

Узел: `indicator_overvolume`.

Логика отображения:

- Скрыт, если объемная система выключена или `overload < 0.9`.
- При `0.9 <= overload < 1.0`: текстура `ui_inGame2_circle_overvolume_yellow` - предупреждение "место кончается".
- При `1.0 <= overload < 1.25`: текстура `ui_inGame2_circle_overvolume_orange` - мягкий перегруз, активны штрафы стамины и прицела.
- При `overload >= 1.25`: текстура `ui_inGame2_circle_overvolume_red` - близко к жесткому пределу, скоро сработает блокировка спринта и подбора.

Текстуры описаны в `gamedata/configs/ui/textures_descr/ui_add_indicators.xml` и не требуют отдельной сборки.

Подключается через также через override-файлы:

```
gamedata/configs/ui/mod_maingame_volume.xml
gamedata/configs/ui/mod_maingame_16_volume.xml
```

Узел добавляется в корень `maingame.xml` через `override="add"`. Если файл не подключен, индикатор просто не создается, движок не ругается.

## Lua API

Методы `CScriptGameObject` для актора или владельца инвентаря:

```
get_inventory_volume()           возвращает текущий объем рюкзака
get_inventory_volume_capacity()  возвращает текущую емкость
get_inventory_volume_overload()  возвращает фактор перегруза
get_item_volume(item)            возвращает объем конкретного предмета
```

При выключенной системе все методы возвращают 0.

## Рекомендуемые безопасные диапазоны

Это практические рекомендации, не жесткие лимиты движка.

1. `base_actor_volume` от 40.0 до 70.0
2. `hard_overload_limit` от 1.2 до 1.6
3. `sprint_block_factor` от 1.05 до `hard_overload_limit`
4. `stamina_power_penalty` от 0.0 до 0.5
5. `aim_sway_penalty` от 0.0 до 0.5
6. Объемы предметов в `[item_volumes]` от 0.05 до 80.0
7. Профили рюкзаков в `[container_profiles]` от 40.0 до 120.0

## Минимальная активация

Для включения механики достаточно:

1. В `gamedata/configs/engine_external.ltx` выставить `EnableInventoryVolume = true`.
2. Убедиться, что присутствует `gamedata/configs/volume_system.ltx`.
3. По желанию добавить override-файлы UI инвентаря: `mod_actor_menu_volume.xml` и `mod_actor_menu_16_volume.xml`.
4. По желанию добавить override-файлы HUD-индикатора: `mod_maingame_volume.xml` и `mod_maingame_16_volume.xml`.
5. По желанию настроить мягкие штрафы, порог `hard_overload_limit` и емкости рюкзаков и костюмов под свой геймплей.

Все шаги после первых двух необязательны. Без UI и HUD механика работает чисто как геймплейный модификатор и проявляется только через падение стамины, разброс прицела, блокировку спринта и подбора при перегрузе.
