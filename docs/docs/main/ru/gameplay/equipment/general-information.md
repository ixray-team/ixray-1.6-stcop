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

is_antigas_filter = true ; Задействовать логику фильтра для противогаза

antigas_filter_protection_burn = 0.01; Добавочный параметр защиты от горячего воздуха для противогаза (условность от горячего воздуха не ставить слишком высокие цифры)
antigas_filter_protection_radiation	= 0.1; Добавочный параметр защиты от радиоактивной пыли для противогаза
antigas_filter_protection_chemical_burn = 0.01; Добавочный параметр защиты от химических испарений для противогаза

antigas_filter_coeff_damage_burn = 0.01; Кофицент урона который получает фильтр противогаза при воздействии горячего воздуха
antigas_filter_coeff_damage_radiation = 0.01; Кофицент урона который получает фильтр противогаза при воздействии радиоактивной пыли
antigas_filter_coeff_damage_chemical_burn	= 0.01; Кофицент урона который получает фильтр противогаза при воздействии химических испарений
```
