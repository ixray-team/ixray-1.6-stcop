# Настройка интерфейса через XML

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4

## Обзор

Пример для ЗП создаёт надпись, полосу заполнения и контейнер прокрутки внутри уже существующего окна. XML задаёт разметку, а Lua создаёт виджеты. Зарегистрируйте шрифт `example_status_font` из [примера LTX](ltx-examples.md).

## Синтаксис

Корневой узел XML содержит разметку окна. Вложенную разметку можно вынести в отдельный файл и подключить директивой `#include "путь"`. Файл XMLOverride с именем `mod_<имя исходного файла без расширения>_<суффикс>.xml` позволяет заменить узел через `override="replace"`.

## Примеры

### Собрать разметку из двух файлов

В `gamedata/configs/ui/config_example/status.xml` поместите один узел без внешнего `<w>`. Директива `#include` вставит его в корень основного файла.

```xml
<status_label x="20" y="20" width="240" height="52">
    <text font="example_status_font" align="l" complex_mode="1" />
</status_label>
```

Сохраните основную разметку в `gamedata/configs/ui/config_example.xml`.

```xml
<w>
    #include "ui\config_example\status.xml"

    <meter x="20" y="80" width="220" height="16" mode="horz"
           min="0" max="100" pos="40" inertion="0">
        <progress stretch="1">
            <texture>ui_inGame2_inventory_progress_bar</texture>
        </progress>
        <min_color r="220" g="50" b="50" a="255" />
        <middle_color r="235" g="190" b="45" a="255" />
        <max_color r="80" g="210" b="100" a="255" />
    </meter>

    <entries x="20" y="110" width="240" height="140"
             scroll_profile="default" always_show_scroll="0"
             top_indent="4" bottom_indent="4" vert_interval="6" />
</w>
```

| Атрибут или узел | Что задаёт |
| --- | --- |
| `x`, `y`, `width`, `height` | Положение и размер относительно родительского окна в координатах интерфейса. |
| `text font` | Имя LTX-секции шрифта. `align="l"` выравнивает текст влево, `complex_mode="1"` включает многострочную раскладку. |
| `meter mode`, `min`, `max`, `pos` | Ориентация, диапазон и начальное значение полосы. Здесь `40` из `100` даёт заполнение на 40%. |
| `inertion` | Инерция при изменении значения. При `0` заполнение движется к заданному значению с максимальной скоростью. |
| `progress/texture` | Текстура заполнения. В примере используется ID из штатного описания текстур ЗП. |
| `min_color`, `middle_color`, `max_color` | Цвета в начале, середине и конце диапазона. `min_color` включает выбор цвета по значению. |
| `scroll_profile` | Имя профиля в `scroll_bar.xml`. Здесь используется `default`. |
| `always_show_scroll` | При `1` полоса остаётся видимой, при `0` появляется, когда содержимое требует прокрутки. |
| `top_indent`, `bottom_indent`, `vert_interval` | Отступ сверху, отступ снизу и интервал между элементами контейнера. |

Путь директивы `#include` в этом примере начинается с `ui\` относительно `$game_config$`. [Правила подключения файлов](group-file-connection-using-masks.md)

### Создать виджеты в текущем окне

Передайте в `build_example` уже открываемое родительское окно `CUIWindow` или его наследника. `ParseFile` ищет файл в `configs/ui`. `InitStatic`, `InitProgressBar` и `InitScrollView` создают и присоединяют виджеты к родителю.

```lua
local function build_example(parent)
    local xml = CScriptXmlInit()
    xml:ParseFile("config_example.xml")

    local status = xml:InitStatic("status_label", parent)
    status:SetText("Signal found\\nScanning area")

    local meter = xml:InitProgressBar("meter", parent)
    local entries = xml:InitScrollView("entries", parent)
    return status, meter, entries
end
```

Вызовите `build_example(self)` при инициализации своего окна, если `self` — это родительское окно. Надпись разбивается на две строки, потому что виджет распознаёт буквальный маркер `\n`, который передаётся из Lua как `\\n`. Заполнение полосы дойдёт до 40%. Контейнер начинается пустым. Чтобы проверить прокрутку, добавьте достаточно строк через `entries:AddWindow(row, true)`, где `row` — созданное окно строки, а `true` разрешает контейнеру удалить этот виджет.

Чтобы показывать здоровье игрока в игровом окне, добавьте к `meter` атрибут `expression="floor(fltPlayerHealth * 100.0)"`. Полоса начнёт обновлять целевое значение каждый кадр в диапазоне `0–100`. [Переменные и функции XML Expression](../interface/xml-expression.md)

### Изменить готовую разметку через XMLOverride

Создайте рядом с `config_example.xml` файл `mod_config_example_tall.xml`. Он заменит узел `entries` целиком. Перенесите все атрибуты, которые должны сохраниться.

```xml
<w>
    <entries override="replace"
             x="20" y="110" width="240" height="200"
             scroll_profile="default" always_show_scroll="1"
             top_indent="4" bottom_indent="4" vert_interval="6" />
</w>
```

После перезапуска игры область списка станет выше на 60 единиц интерфейса, а полоса прокрутки будет отображаться и для пустого списка. [Правила выбора и замены узлов](xml-override.md)

## См. также

- [Профили полос прокрутки](../interface/scrollbar-profiles.md)
- [Параметры интерфейса](../interface/general-information.md)
