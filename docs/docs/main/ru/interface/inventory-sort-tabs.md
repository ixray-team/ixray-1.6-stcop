> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4 <br>
> **Последнее обновление**: 2026-07-24

# Сортировка инвентаря

## Обзор

Система вкладок управляет отображением предметов в инвентаре, торговле и окне трупа. Поддерживаются два режима, выбираемые в LTX: **categories** (фильтрация по категориям) и **ordering** (сортировка без фильтрации). Переключение режима выполняется только на этапе конфигурации мода. Порядок предметов в `m_ruck` в памяти не меняется, меняется только UI-список.

Дополнительно реализована возможность использовать горячие клавиши kQ/kE.

## Выбор режима

В `configs/mod_system_ixray.ltx` или своем mod LTX:

```ltx
[inventory_sort]
system = categories   ; categories | ordering
```

| Значение | UI-узел XML | Поведение |
|----------|-------------|-----------|
| `categories` | `inventory_sort_tabs` | Фильтрация по категориям + сортировка по размеру в сетке |
| `ordering` | `inventory_sort_order_tabs` | Только сортировка, все предметы видны |

Если XML-узел для выбранного режима отсутствует, вкладки сортировки не показываются.

## Режим categories

1. Категории определяются в `CInventorySorter`.
2. Базовые категории: `all`, `weapons`, `ammo`, `armor`, `devices`, `consumables`, `artefacts`, `attachments`.
3. Кастомные категории читаются из `inventory_sort_custom`.
4. Подписи и иконки категорий читаются из `inventory_sort_categories`.

### XML

1. Добавьте `inventory_sort_tabs` в mod override `configs/ui/actor_menu.xml`.
2. Для разных режимов меню можно добавить контейнеры:
   1. `inventory_sort_tabs_container_upgrade`
   2. `inventory_sort_tabs_container_trade_actor_bag`
   3. `inventory_sort_tabs_container_trade_partner_bag`
   4. `inventory_sort_tabs_container_deadbody_bag`

## Режим ordering

Все предметы всегда видны. Вкладки меняют только порядок в UI-списке.

| Вкладка | id | Порядок | Cycle / Alt |
|---------|----|---------|-------------|
| Общее | `general` | `GreaterRoomInRuck` (как "Все" в categories) | нет |
| По типу | `by_type` | Группы по `GetItemCategory`, внутри группы - grid sort | да - цикл фокуса типа |
| По весу | `by_weight` | `Weight()`, по умолчанию по убыванию | да - тяжелый / легкий |
| По состоянию | `by_condition` | `GetCondition()` для предметов с condition, остальные в конце | да - лучше / хуже |
| По цене | `by_cost` | `Cost()`, по умолчанию по убыванию | да - дорогой / дешевый |
| По важности | `by_importance` | `IsQuestItem()` сверху, внутри группы - grid sort | нет |
| По новизне | `by_novelty` | `GetTakenTime()`, по умолчанию новые сверху | да - новый / старый |

### Cycle направления

Для режимов с инверсией (`by_weight`, `by_condition`, `by_cost`, `by_novelty`) и для цикла типа (`by_type`):

1. Повторный клик по уже активной вкладке.
2. Клавиша `kINV_SORT_CYCLE` (по умолчанию Alt).

Подпись вкладки для веса / состояния / цены / новизны дополняется суффиксом `v` (убывание) или `^` (возрастание). Для `by_type` подпись меняется на имя текущего типа в цикле.

### Время взятия (новизна)

Поле `CInventoryItem::m_dwTakenTime` (`ALife::_TIME_ID`):

1. При первом `CInventory::Take`, если метка равна `0`, записывается `Level().GetGameTime()`.
2. Сериализуется в `CInventoryItem::save` / `load` до early-return при `H_Parent()`, рядом с condition.
3. Значение `0` (нет метки, в том числе старые сейвы без поля) при сортировке по убыванию уходит в конец списка.

### LTX для ordering

```ltx
[inventory_sort]
system = ordering

[inventory_sort:ordering]
weight_desc = true
condition_desc = true
cost_desc = true
novelty_desc = true

[inventory_sort_order]
general = 1
by_type = 1
by_weight = 1
by_condition = 1
by_cost = 1
by_importance = 1
by_novelty = 1

[inventory_sort_order:general]
name = st_inv_sort_order_general
hint = st_inv_sort_order_general_hint
```

### XML для ordering

Mod override файлы (пример в ixray):

1. `configs/ui/mod_actor_menu_sort_order.xml`
2. `configs/ui/mod_actor_menu_16_sort_order.xml`

Узел `inventory_sort_order_tabs` содержит 7 кнопок: `general`, `by_type`, `by_weight`, `by_condition`, `by_cost`, `by_importance`, `by_novelty`. Контейнеры для trade/deadbody/upgrade зеркалируют categories-режим.

Строки: `configs/text/rus/ui_st_inventory_sort.xml`, `configs/text/eng/ui_st_inventory_sort.xml`.

## Рекомендации

✔️ Правильное использование:

1. Выберите один режим (`categories` или `ordering`) на мод.
2. Добавляйте только XML-узел, соответствующий выбранному режиму.
3. Для categories используйте `inventory_sort_custom` для нестандартных групп предметов.

⚠️ Ограничения:

1. Гранаты в слоте GRENADE_SLOT показываются в рюкзаке только в categories-режиме при фильтрах `all` или `ammo`.
2. Runtime-переключение режима не поддерживается.
3. Смена направления ordering идет только через `CycleActiveOrderOption` (повторный клик / `kINV_SORT_CYCLE`), отдельный Alt-обработчик не нужен.

✖️ Анти-паттерны:

1. Одновременное добавление `inventory_sort_tabs` и `inventory_sort_order_tabs` в один UI.
2. Ожидание изменения физического порядка предметов в инвентаре актора.

## Связанные разделы

[слоты инвентаря](inventory-slots.md), [обзор UI](ui-advanced-features.md)
