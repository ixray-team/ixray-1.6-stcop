# Инвентарные слоты

::: important Поддержка
**Статус:** Поддерживается · **Минимальная версия:** 1.0
:::

С версии **1.3** доступен отдельный слот для пистолетов, расположенный после слота для шлема.

## Добавление слота

В секции `[inventory]` файла `system.ltx` добавьте параметры `slot_persistent_N` и `slot_active_N`. Вместо `N` укажите **следующий порядковый номер слота**.

Чтобы отобразить слот в инвентаре, добавьте элемент `<slot>` в конец блока `inventory_slot_wnd` файла `actor_menu.xml`.

::: code-group

```ini [system.ltx]
slot_persistent_14  = false ;helmet
slot_active_14      = false
```

```xml [actor_menu.xml]
<slot> <!-- New Slot-->
  <slot_highlight x="472" y="17" width="76" height="98" stretch="1">
    <texture>ui_inGame2_helmet_highlighter</texture>
  </slot_highlight>
  <slot_progress x="488" y="118" width="47" height="5" horz="1" min="0" max="1" pos="0">
    <progress stretch="1">
      <texture r="142" g="149" b="149">ui_inGame2_inventory_status_bar</texture>
    </progress>
    <min_color r="196" g="18" b="18"/>
    <middle_color r="255" g="255" b="118"/>
    <max_color r="107" g="207" b="119"/>
  </slot_progress>
  <!-- [!code ++:3] -->
  <slot_dragdrop x="469" y="14" width="85" height="110"
    cell_width="33" cell_height="41" rows_num="2" cols_num="2" custom_placement="0" a="0" virtual_cells="1"
    vc_vert_align="c" vc_horiz_align="c" />
</slot>
```
:::

::: tip Нумерация слотов
В `system.ltx` отсчёт начинается с **0**, а в списке `inventory_slot_wnd` — с **1**. Слоту `14` соответствует **15-й** элемент `<slot>`.
:::

## Размещение оружия и порядок переключения

Параметры читаются из секции `[inventory]` игрового конфига. Для переопределения через DLTX используйте `![inventory]`.

| Ключ | Действие |
| --- | --- |
| `inventory_secondary_slot_pairing_strict` | При `true` меню размещает оружие второго и третьего слотов в соответствующих ячейках. При `false` меню допускает взаимозаменяемость этих ячеек. Резерв в коде — `false`. |
| `weapon_cycle_slots` | Список числовых ID слотов через запятую в порядке переключения. Если список отсутствует или ни один ID не подошёл, используется штатный порядок. |

В поставляемых LTX первый ключ равен `true` для ТЧ и ЧН, `false` для ЗП и CoC. Штатный порядок переключения включает нож, отдельный пистолетный слот при `slot_active_16 = true`, затем оружейные слоты 2 и 3, гранаты и слот артефакта.

Чтобы оставить при циклическом переключении только две оружейные ячейки и закрепить их за соответствующими слотами, добавьте в DLTX:

```ini
![inventory]
inventory_secondary_slot_pairing_strict = true
weapon_cycle_slots = 2, 3
```
