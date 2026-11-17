# Профили полос прокрутки

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4

## Обзор

Профиль задаёт геометрию и ресурсы полосы прокрутки. Профили хранятся в `gamedata/configs/ui/scroll_bar.xml`. Один профиль можно использовать в нескольких списках и окнах.

## Синтаксис

```xml
<logs_list x="45" y="176" width="936" height="523" item_height="35"
           scroll_profile="pda_logs" always_show_scroll="1"/>
```

Укажите `scroll_profile` в узле списка или прокручиваемого контейнера. Название соответствует дочернему элементу в `scroll_bar.xml`. Без атрибута используется `default`. Профиль `pda_logs` есть в штатных ресурсах ЗП и CoC. Для других комплектов укажите профиль из их `scroll_bar.xml`.

| Атрибут профиля | Назначение |
| --- | --- |
| `layout` | Режим `auto`, `stretch` или `fixed`. В `auto` полоса фиксирована, если ширина и высота для выбранной оси положительны. |
| `width`, `height` | Габариты фиксированной горизонтальной полосы. |
| `width_v`, `height_v` | Габариты фиксированной вертикальной полосы. |
| `hold_delay` | Задержка автопрокрутки при удержании кнопки в миллисекундах. |
| `scroll_box_offset_x`, `scroll_box_offset_y` | Отступ рабочей области бегунка. |
| `thumb` | Тип бегунка фиксированной полосы: `auto`, `button` или `box`. |

Профиль может задавать стрелки (`up_arrow`, `down_arrow`, `left_arrow`, `right_arrow`), трек (`back` или `back_v`) и бегунок (`box` или `box_v`). В профилях также используются узлы `dec`, `inc`, `track` и `thumb`.

## Штатные профили и пропорции экрана

| Комплект | Профили в `scroll_bar.xml` и `scroll_bar_16.xml` |
| --- | --- |
| ТЧ и ЧН | `default`, `pda` |
| ЗП и CoC | `default`, `pda`, `pda_logs` |

Если интерфейс широкоформатный, загрузчик сначала выбирает существующий `scroll_bar_16.xml`. Для сверхширокого формата сначала ищет `scroll_bar_21.xml`, затем версию `_16` и обычную. В перечисленных штатных комплектах версии `_21` нет. Профиль ищется внутри выбранного файла. Пропущенный или пустой `scroll_profile` использует `default`. Если передать непустое неизвестное имя, инициализация полосы с этим профилем завершится ошибкой.

## Примеры

### Смена профиля из Lua

```lua
local view = xml:InitScrollView("my_scroll", self)
view:SetScrollBarProfile("pda")
view:ReinitScrollBar()

local bar = view:ScrollBar()
if bar then
    bar:SetScrollPos(10)
end
```

Для отдельной полосы вызовите `InitScrollBarFixed(x, y, horizontal, profile)` или `InitScrollBarStretch(x, y, length, horizontal, profile)`. Результат показывает, удалось ли инициализировать полосу.

## Рекомендации и ограничения

- `SetFixedScrollBar(bool)` управляет постоянным отображением полосы у списка. Геометрию задаёт `layout` в профиле.
- `ui.QueryScrollBarProfileLayout(name, horizontal)` возвращает `-1` для не найденного профиля, `0` для `stretch` и `1` для `fixed`.

## См. также

- [Виджеты и XML](general-information.md)
