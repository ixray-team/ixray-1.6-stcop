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
| `layout` | Режим `auto`, `stretch`, `fixed` или `styled`. В `auto` полоса фиксирована, если ширина и высота для выбранной оси положительны. |
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

## Стилизованные полосы (`layout="styled"`)

Режим `styled` собирает полосу только из тех частей, которые описаны в профиле. Профили без `layout="styled"` работают по-старому (Legacy: `stretch`, `fixed`, `auto`).

```xml
<slim layout="styled" thickness="4" hover_thickness="8" min_thumb="24"
      wheel_lines="3" track_click="jump" idle_alpha="0.25" idle_delay="1200">
    <track color="255,255,255,60" color_hover="255,255,255,90">
        <texture>ui_scroll_back</texture>
    </track>
    <thumb color="255,255,255,160" color_hover="255,255,255,220" color_pressed="255,205,120,255">
        <texture>ui_scroll_box</texture>
    </thumb>
</slim>
```

Части полосы:

| Узел | Назначение |
| --- | --- |
| `track` | Подложка. Необязательна |
| `thumb` | Бегунок |
| `dec`, `inc` | Кнопки 3t (`texture_e`, `texture_h`, `texture_t`). Без них стрелок нет |
| `vertical`, `horizontal` | Переопределения для оси: атрибуты и части внутри узла важнее общих |

Если у `texture` есть варианты `_b`, `_back`, `_e`, часть рисуется как frame line, иначе как растянутая картинка. У `track` и `thumb` есть цвета `color`, `color_hover`, `color_pressed`, `color_disabled` в формате `r,g,b[,a]`. `color_disabled` используется, когда прокручивать нечего.

| Атрибут профиля | Назначение |
| --- | --- |
| `thickness` | Толщина полосы |
| `hover_thickness` | Толщина при наведении и перетаскивании. Окно полосы берёт большую из двух |
| `track_cross`, `thumb_cross` | Толщина подложки и бегунка поперёк оси |
| `thumb_margin` | Отступ бегунка от краёв, если `thumb_cross` не задан |
| `track_inset` | Отступ подложки от концов (или от стрелок) |
| `cross_align` | Прижатие к краю: `near`, `center`, `far` (по умолчанию `far`, к краю окна) |
| `min_thumb` | Минимальная длина бегунка |
| `wheel_lines` | Шаг колеса в шагах списка |
| `track_click` | `page`: листать страницами к курсору с повтором при удержании, `jump`: перенести бегунок под курсор и тянуть |
| `repeat_delay`, `hold_delay` | Задержка перед повтором и интервал повтора при удержании кнопки или подложки, мс |
| `idle_alpha`, `idle_delay`, `fade_speed` | Прозрачность в покое, время до затухания (мс), скорость |
| `resize_speed` | Скорость изменения толщины |

Бегунок перетаскивается с захватом мыши и не теряется при уходе курсора с полосы. Колесо над полосой прокручивает список.

`scroll_bar.xml` загружается один раз и кешируется; при смене соотношения сторон экрана берётся подходящий файл (`scroll_bar_16.xml`, `scroll_bar_21.xml`). Аддоны могут добавлять профили через [XMLOverride](../configs/xml-override.md).

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
- `ui.QueryScrollBarProfileLayout(name, horizontal)` возвращает `-1` для не найденного профиля, `0` для `stretch`, `1` для `fixed` и `2` для `styled`.

## См. также

- [Виджеты и XML](general-information.md)
