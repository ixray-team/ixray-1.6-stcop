# Gameplay: Инвентарные предметы
## Любой инвентарный предмет (CInventoryItem)
> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

```ini
;Позволяет задать одну или несколько инвентарных секций предметов, 
;которые будут подсвечены при наведении на текущий предмет с этим перечнем (подобно тому как подсвечиваются боеприпасы для оружия в инвентаре)
highlight_related_sections = related_section_a, related_section_b
```

## Любой инвентарный предмет (CInventoryItem)
> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

```ini
is_draw_cost = true ;Отображать ли цену предмета в UI (true|false)
```


::: details Пример реализации

```ini
[root_item_section]:identity_immunities ; Родительский предмет с настроенной подсветкой дочерних предметов
  highlight_related_sections = related_section_a, related_section_b; секции предметов которые будут подсвечены как зависимые от этого предмета

[related_section_a]:identity_immunities; Подсвечиваемый предмет при наведении на root_item_section
  highlight_related_sections = root_item_section; можно добавить обратную подсветку родительского предмета при наведении на дочерний если требуется
  ...

[related_section_b]:identity_immunities; Подсвечиваемый предмет при наведении на root_item_section
    ...
```
:::

## Шлем (CHelmet)

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0

```ini
use_condition = false; Имеет ли предмет прогрессбар износа
```

> [!IMPORTANT]
> **Минимальная версия**: 1.2.2 <br>

* Консольные команды r_use_gasmask и r_use_rain_drops

```ini
hud_gas_mask_avaliable = true; Доступна ли отрисовка капель дождя
hud_rain_drops_avaliable = true; Доступна ли отрисовка газ маски
```

> [!IMPORTANT]
> **Минимальная версия**: 1.4

```ini
physic_strike_protection = 0.7; physic strike hit protection 
```

## Броня (CCustomOutfit)

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0

```ini
use_condition = false      ; Имеет ли предмет прогрессбар износа
forbid_change_skin = false ; Меняет ли визуал (руки/ноги)
```

> [!IMPORTANT]
> **Минимальная версия**: 1.2.2 <br>
* Консольные команды r_use_gasmask и r_use_rain_drops
```ini
hud_gas_mask_avaliable = true; Доступна ли отрисовка капель дождя
hud_rain_drops_avaliable = true; Доступна ли отрисовка газ маски
```

> [!IMPORTANT]
> **Минимальная версия**: 1.4
```ini
physic_strike_protection = 0.7; physic strike hit protection 
```
> Также поддерживается и для апгрейдов

## Фонарь (CTorch)

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0

```ini
snd_click = device\torch_click; Опциональный звук включения/выключения фонаря
```

## Сумки (CBackpack)

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

* Опциональные инвентарные рюкзаки со своим слотом

```ini
[backpack_test]:identity_immunities
  GroupControlSection = spawn_group
  $spawn = "devices\dev_backpack"
  $prefetch = 16
  cform = skeleton
  visual = additions\sumka1.ogf
  inv_grid_width = 2
  inv_grid_height = 2
  inv_grid_x = 4
  inv_grid_y = 23
  slot = 12
  default_to_ruck = false
  inv_weight = 0.5
  inv_name = backpack_test_name
  inv_name_short = backpack_test_name
  description = backpack_test_descr
  cost = 1000
  can_trade = true
  
  ; Дополнительный вес в инвентаре (по аналогии с бронёй)
  additional_inventory_weight = 15
  additional_inventory_weight2 = 15
  
  ; Скорость восстановления выносливости
  power_restore_speed = 0.0
  
  ; Класс нашей сумки
  class = EQ_BAKPK
```



## Противогазы (CHelmet & CCustomOutfit): IAntigas

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

* Опциональные противогазы со сменными фильтрами (можно настроить либо для шлема либо для бронежилета если у него встроенный шлем)
* Класс предмета E_HLMET
* Класс предмета E_STLK

* Имеет контекстное меню для извлечения фильтра
```ini
[antigas_helmet_01]:helm_tactic ; наследуем от любого шлема или бронежилета
  class = E_HLMET ; или E_STLK
  is_antigas = true ; Задействовать логику противогаза (true|false)
  antigas_allow_filter_sections = filter_01, filter_02; Перечень секций фильтров которые совместимы с этим противогазом
  antigas_breath_sounds_with_filter	= gasmask\gas_breath_7_1.ogg, gasmask\gas_breath_7_2.ogg, gasmask\gas_breath_7_3.ogg; Звуки дыхания с надетым фильтром
  antigas_breath_sounds_no_filter	= gasmask\gas_breath_1_1.ogg, gasmask\gas_breath_1_2.ogg, gasmask\gas_breath_1_3.ogg; Звуки дыхания без фильтра
```

## Фильтры для противогазов (AntigasFilter)

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

* Опциональные фильтры для противогазов (имеют в себе настройки иммунитетов которые применяются при вкручивании фильтра в противогаз и снижаются по мере расхода фильтра)
* Класс предмета ATG_FLTR

* Вставляется в противогаз драг дропом фильтра на противогаз
```ini
[filter_01]:identity_immunities
  class = ATG_FLTR
  ...
  is_antigas_filter = true ; Задействовать логику фильтра для противогаза
  
  antigas_filter_protection_burn = 0.01; Добавочный параметр защиты от горячего воздуха для противогаза (условность от горячего воздуха не ставить слишком высокие цифры)
  antigas_filter_protection_radiation	= 0.1; Добавочный параметр защиты от радиоактивной пыли для противогаза
  antigas_filter_protection_chemical_burn = 0.01; Добавочный параметр защиты от химических испарений для противогаза
  
  antigas_filter_coeff_damage_burn = 0.01; Кофицент урона который получает фильтр противогаза при воздействии горячего воздуха
  antigas_filter_coeff_damage_radiation = 0.01; Кофицент урона который получает фильтр противогаза при воздействии радиоактивной пыли
  antigas_filter_coeff_damage_chemical_burn	= 0.01; Кофицент урона который получает фильтр противогаза при воздействии химических испарений
```

## Элемент питания (PowerCell)

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

* Опциональные батарейки для работы приборов
* Класс для конфигов PWR_CELL
* Совместимо с (CNVG, CEliteDetector, CCustomDetector, CAdvancedDetector, CSimpleDetector, CPda, CTorch, PowerBank)
* Взаимодействие с приборами (драг дроп в инвентаре на совместимое устройство)

```ini
[pwr_cell_01]:identity_immunities
    ...
    class = PWR_CELL; Класс батарейки
    power_cell_max_energy_value = 5000; Максимальная ёмкость элемента питания
    power_cell_current_energy_value = 5000; Текущий остаток энергии (если нужно не полную то указываем здесь значение меньше чем максимальное иначе делаем равным максимальному если нужна новая)
```

## Повер банк (Аггрегатор элементов питания) (PowerBank) с поддержкой потребления энергии

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

* Опциональный переходник для батареек (аггрегатор элементов питания) (повербанк)
* Класс для конфигов PWR_BANK
* Принимает в качестве ячеек питания (PWR_CELL)
* Совместимо c приборами (CNVG, CEliteDetector, CCustomDetector, CAdvancedDetector, CSimpleDetector, CPda, CTorch) как источник питания если вставлен в слот ГГ
* Взаимодействие (драг дроп батареек на повербанк для установки)
* Взаимодействие (контекстное меню на повербанке для извлечения батареек если они вставлены)

```ini
[pwr_bank]:identity_immunities
  ...                    
  class  = PWR_BANK
  default_to_ruck = true
  slot = 13; Инвентарный слот (регистрировать самостоятельно в system.ltx)
  allowed_power_cells_sections = pwr_cell_01, pwr_cell_02, pwr_cell_03; Секции совместимых батареек
  max_count_power_cells = 4; максимальное колличество баатреек которые можно установить за раз
```

## Опциональное ПНВ с собственным слотом (CNVG) и поддержкой потребления энергии

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

* Опциональные инвентарные ПНВ со своим слотом
* Класс для конфигов D_NVG
* Совместимо с (PowerCell, PowerBank)

```ini
[device_nvg]:identity_immunities
  ...                      
  class = D_NVG; класс ПНВ
  slot = 14; Инвентарный слот (регистрировать самостоятельно в system.ltx)
  use_power_cells = true; Имеет ли слоты под внутреннюю батарейку (автоматически начинает требовать питание)
  allowed_power_cells_sections = pwr_cell_01, pwr_cell_02, pwr_cell_03; Секции совместимых батареек
  use_power_bank = true; Имеет ли совместимость в повербанком (автоматически начинает требовать питание) если есть не пустой повербанк в слоте то энергия будет брать приоритетно из него и только потом из внутренней бабарейки если активны обе опции и батарейка и повербанк
  power_drain_value = 5; Потребление энергии когда активен (подбирать имперически)
```

## PDA (CPda) с поддержкой потребления энергии

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

* Опциональная потребность в энергии
* Совместимо с (PowerCell, PowerBank)

```ini
[device_pda]
  ...
  use_power_cells = true; Имеет ли слоты под внутреннюю батарейку (автоматически начинает требовать питание)
  allowed_power_cells_sections = pwr_cell_01, pwr_cell_02, pwr_cell_03; Секции совместимых батареек
  use_power_bank = true; Имеет ли совместимость в повербанком (автоматически начинает требовать питание) если есть не пустой повербанк в слоте то энергия будет брать приоритетно из него и только потом из внутренней бабарейки если активны обе опции и батарейка и повербанк
  power_drain_value = 5; Потребление энергии когда активен (подбирать имперически)
```

## Фонарик (CTorch) с поддержкой потребления энергии

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

* Опциональная потребность в энергии
* Совместимо с (PowerCell, PowerBank)

```ini
[device_torch]
  ...
  use_power_cells = true; Имеет ли слоты под внутреннюю батарейку (автоматически начинает требовать питание)
  allowed_power_cells_sections = pwr_cell_01, pwr_cell_02, pwr_cell_03; Секции совместимых батареек
  use_power_bank = true; Имеет ли совместимость в повербанком (автоматически начинает требовать питание) если есть не пустой повербанк в слоте то энергия будет брать приоритетно из него и только потом из внутренней бабарейки если активны обе опции и батарейка и повербанк
  power_drain_value = 5; Потребление энергии когда активен (подбирать имперически)
```

## Детекторы (CEliteDetector, CCustomDetector, CAdvancedDetector, CSimpleDetector) с поддержкой потребления энергии

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

* Опциональная потребность в энергии
* Совместимо с (PowerCell, PowerBank)

```ini
[detector_advanced]
  ...
  use_power_cells = true; Имеет ли слоты под внутреннюю батарейку (автоматически начинает требовать питание)
  allowed_power_cells_sections = pwr_cell_01, pwr_cell_02, pwr_cell_03; Секции совместимых батареек
  use_power_bank = true; Имеет ли совместимость в повербанком (автоматически начинает требовать питание) если есть не пустой повербанк в слоте то энергия будет брать приоритетно из него и только потом из внутренней бабарейки если активны обе опции и батарейка и повербанк
  power_drain_value = 5; Потребление энергии когда активен (подбирать имперически)
```


## Интерактивные обьекты (CInteractiveObject) полу лутабельная динамика без коллизии

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

* Неосязаемая динамика над которой можно вызывать активное действия получая предметы при этом скрывая кости на нем (облутывание яблок с дерева как пример)
* Класс для конфига INTERACT

```ini
[interact_obj_01]
  ...
  class = INTERACT; Класс
  visual = dynamics\ffx0001\kust_02.ogf; Путь к визуалу
  items_bones_names = link1, link2, link3, link4, link5; скрываемые кости (каждое использование скрывает одну из костей начиная с первой по списку)
  use_tip_text = "use_interactive_obj"; Текст при наведении (поддерживает перевод)
  use_sounds = FFx0001\use_tree; звуки облутывания через запятую
  spawn_section_name = brevno_04, brevno_03, brevno_02, brevno_01; спавн секции в инвентарь при лутании на активную клавишу (берется первая секция если выключены доп режимы облутывания)
  use_spawn_random_sections = false; спавнить в инвентарь случайную из перечисленных секций предметов при взаимодействии
  use_spawn_at_bone_index_sections = false; спавнить в инвентарь последовательно перечисленные секции предметов при взаимодействии в зависимости от того какая по индексу кость скрыта такая секция и будет заспавнена
```



## Противогазы (CHelmet & CCustomOutfit): IAntigas

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

* Опциональные противогазы со сменными фильтрами (можно настроить либо для шлема либо для бронежилета если у него встроенный шлем)
* Класс предмета E_HLMET
* Класс предмета E_STLK

* Имеет контекстное меню для извлечения фильтра
```ini
[antigas_helmet_01]:helm_tactic ; наследуем от любого шлема или бронежилета
class = E_HLMET ; или E_STLK
is_antigas = true ; Задействовать логику противогаза (true|false)
antigas_allow_filter_sections = filter_01, filter_02; Перечень секций фильтров которые совместимы с этим противогазом
antigas_breath_sounds_with_filter	= gasmask\gas_breath_7_1.ogg, gasmask\gas_breath_7_2.ogg, gasmask\gas_breath_7_3.ogg; Звуки дыхания с надетым фильтром
antigas_breath_sounds_no_filter	= gasmask\gas_breath_1_1.ogg, gasmask\gas_breath_1_2.ogg, gasmask\gas_breath_1_3.ogg; Звуки дыхания без фильтра
```

## Фильтры для противогазов (AntigasFilter)

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

* Опциональные фильтры для противогазов (имеют в себе настройки иммунитетов которые применяются при вкручивании фильтра в противогаз и снижаются по мере расхода фильтра)
* Класс предмета ATG_FLTR

* Вставляется в противогаз драг дропом фильтра на противогаз
```ini
[filter_01]:identity_immunities
class = ATG_FLTR

is_antigas_filter = true ; Задействовать логику фильтра для противогаза

antigas_filter_protection_burn = 0.01; Добавочный параметр защиты от горячего воздуха для противогаза (условность от горячего воздуха не ставить слишком высокие цифры)
antigas_filter_protection_radiation	= 0.1; Добавочный параметр защиты от радиоактивной пыли для противогаза
antigas_filter_protection_chemical_burn = 0.01; Добавочный параметр защиты от химических испарений для противогаза

antigas_filter_coeff_damage_burn = 0.01; Кофицент урона который получает фильтр противогаза при воздействии горячего воздуха
antigas_filter_coeff_damage_radiation = 0.01; Кофицент урона который получает фильтр противогаза при воздействии радиоактивной пыли
antigas_filter_coeff_damage_chemical_burn	= 0.01; Кофицент урона который получает фильтр противогаза при воздействии химических испарений
```
