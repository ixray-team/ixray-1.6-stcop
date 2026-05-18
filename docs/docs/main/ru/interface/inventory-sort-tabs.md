> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

# Сортировка инвентаря

## Обзор

Система вкладок сортирует предметы в инвентаре, торговле и окне трупа по преднастроенным или пользовательским категориям. Дополнительно реализована возможность использовать горячие клавиши kQ/kE.

## Как это работает

1. Категории определяются в `CInventorySorter`.
2. Базовые категории: `all`, `weapons`, `ammo`, `armor`, `devices`, `consumables`, `artefacts`, `attachments`.
3. Кастомные категории читаются из `inventory_sort_custom`.
4. Подписи и иконки категорий читаются из `inventory_sort_categories`.

## XML и конфиг

1. В `configs/ui/actor_menu.xml` добавьте `inventory_sort_tabs`.
2. Для разных режимов можно добавить:
   1. `inventory_sort_tabs_container_upgrade`
   2. `inventory_sort_tabs_container_trade_actor_bag`
   3. `inventory_sort_tabs_container_trade_partner_bag`
   4. `inventory_sort_tabs_container_deadbody_bag`
3. В LTX опишите секции `inventory_sort_categories` и `inventory_sort_custom`.

Смежный материал: [слоты инвентаря](inventory-slots.md), [обзор UI](ui-advanced-features.md).
