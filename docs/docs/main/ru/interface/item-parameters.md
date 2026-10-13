# Параметры предметов
## Параметры ножа

### Обзор

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0

Добавлен класс `CUIKnifeParams`, добавляющий отображение различных свойств и характеристик ножей в игровом UI (`actor_menu`)

Файлы ресурсов:
* `ui/actor_menu_item.xml`
* `ui/actor_menu_item_16.xml`
* `text/rus/ui_st_new.xml`

### Пример использования:

```xml
<knife_params x="0" y="0" width="212" height="70">
    <!-- Линия разделения параметров ножа -->
    <prop_line x="0" y="0" width="212" height="9" stretch="1">
        <texture>ui_inGame2_hint_wnd_Properties</texture>
    </prop_line>

    <!-- Иконка урона -->
    <static_damage x="0" y="10" width="14" height="18" stretch="1">
        <texture>ui_wp_prop_damage</texture>
    </static_damage>

    <!-- Иконка удобности-->
    <static_handling x="0" y="28" width="14" height="18" stretch="1">
        <texture>ui_wp_prop_ergonomics</texture>
    </static_handling>

    <!-- Иконка дистанции удара -->
    <static_dist x="0" y="46" width="14" height="18" stretch="1">
        <texture>ui_wp_prop_distantion</texture>
    </static_dist>

    <!-- Надпись "Урон" -->
    <cap_damage x="18" y="12" width="80" height="8">
        <text font="letterica16" color="ui_2" align="l">ui_inv_damage</text>
    </cap_damage>

    <!-- Надпись "Удобность" -->
    <cap_handling x="18" y="30" width="80" height="8">
        <text font="letterica16" color="ui_2" align="l">ui_inv_handling</text>
    </cap_handling>

    <!-- Надпись "Дистанция" -->
    <cap_dist x="18" y="48" width="80" height="8">
        <text font="letterica16" color="ui_2" align="l">ui_inv_dist</text>
    </cap_dist>

    <!-- Прогресс-бар урона -->
    <progress_damage x="104" y="14" width="101" height="9" horz="1">
        <progress>
            <texture a="150">ui_inGame2_hint_wnd_bar_alfa_line_16</texture>
        </progress>
        <background>
            <texture>ui_inGame2_hint_wnd_bar_16</texture>
        </background>
        <color_less color="pda_red"  />
        <color_more color="pda_green"/>
    </progress_damage>

    <!-- Прогресс-бар удобности -->
    <progress_handling x="104" y="32" width="101" height="9" horz="1">
        <progress>
            <texture a="150">ui_inGame2_hint_wnd_bar_alfa_line_16</texture>
        </progress>
        <background>
            <texture>ui_inGame2_hint_wnd_bar_16</texture>
        </background>
        <color_less color="pda_red"  />
        <color_more color="pda_green"/>
    </progress_handling>

    <!-- Значение первой дистанции удара -->
    <value_dist1 x="104" y="48" width="10" height="8">
        <text font="letterica16" align="l"/>
    </value_dist1>

    <!-- Значение второй дистанции удара -->
    <value_dist2 x="123" y="48" width="10" height="8">
        <text font="letterica16" align="l"/>
    </value_dist2>

    <!-- Разделитель между значениями дистанции -->
    <value_dist_delimiter x="119" y="48" width="1" height="8">
        <text font="letterica16" color="ui_2" align="l"/>
    </value_dist_delimiter>

    <!-- Единицы измерения -->
    <meters_name x="137" y="48" width="80" height="8">
        <text font="letterica16" color="ui_2" align="l"/>
    </meters_name>
</knife_params>
```

## Метки и текст на иконках

### Обзор

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0

На предметы можно добавлять кастомные метки и текст поверх инвентарного предмета

- Установка дополнительной информации (опционально)

```ini
item_custom_text = st_text ; Текст
item_custom_text_clr_inv = 100,250,100 ; Цвет текста
item_custom_text_font = graffiti22 ; Шрифт
item_custom_text_offset = -2.0, -2.0 ; Смещения (x, y)
```

- Установка дополнительной иконки (опционально)

```ini
item_custom_mark = true
item_custom_mark_offset = -2.0, 0.0
item_custom_mark_size = 10, 10
item_custom_mark_texture = ui_item_count_back
item_custom_mark_clr = 100,250,100
```
## Расширенная подсветка предметов

### Обзор

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

Установка подсветки зависимых предметов при наведении мыши (поддерживается перечисление через запятую) (опционально)

Пример использования:

```ini
[root_item_section]:identity_immunities
highlight_related_sections = related_section_a, related_section_b; секции предметов которые будут подсвечены как зависимые от этого предмета
...

[related_section_a]:identity_immunities
highlight_related_sections = root_item_section; можно добавить обратную подсветку родительского предмета при наведении на дочерний если требуется
...
```

## Автоматическая окраска иконок параметров

Автоматическая окраска включается отдельно для каждой строки параметра. Если ни строка, ни её шаблон не задают атрибут `colorize`, окраска **выключена**: сохраняются цвета исходной DDS-текстуры и прежнее оформление значений. В штатных XML-конфигах окраска эффектов уже включена через `colorize="1"`; состояние предмета, число контейнеров и время действия остаются без окраски. Без этой настройки значения артефактов в современном формате серые для полезного эффекта и красные для вредного, в старом текстовом формате — зелёные/красные; значения бустеров остаются серыми.

Настройка поддерживается для параметров артефактов, дополнительных бонусов экипировки и бустеров. Изменяйте соответствующие файлы в `gamedata/configs/ui` вашей сборки:

- `af_params.xml` и `af_params_16.xml` — артефакты и дополнительные бонусы экипировки;
- `booster_params.xml` и `booster_params_16.xml` — бустеры и расходники.

В формате с дочерними узлами `caption` и `value` добавьте **`colorize="1"` к узлу `value`**, а не к `caption` или `texture`. Например, существующая строка восстановления здоровья артефакта может выглядеть так:

```xml
<health_restore_speed x="0" y="0" width="257" height="20">
    <caption x="0" y="2" width="257" height="20" complex_mode="0">
        <texture>ui_am_propery_05</texture>
        <text color="ui_3" font="letterica16" vert_align="c" x="22" y="0"/>
    </caption>
    <value x="180" y="2" width="30" height="20" magnitude="6600" colorize="1">
        <text font="letterica16" vert_align="c"/>
    </value>
</health_restore_speed>
```

Сохраняйте размеры, `magnitude` и единицы измерения из своего XML. Для бустера настройка добавляется аналогично к его `value`:

```xml
<value x="140" y="0" width="30" height="20" magnitude="10000" colorize="1">
    <text font="letterica16" vert_align="c"/>
</value>
```

При включённой окраске полезный эффект окрашивает иконку и значение в синий (`44, 172, 246`), вредный — в красный (`210, 50, 50`). Цвет подписи `caption:text` сохраняется из XML. Цвет определяется после применения `magnitude`; `sign_inverse="1"` на `value` меняет оценку полезности, но не знак отображаемого числа. Например, для накопления радиации:

```xml
<value x="180" y="2" width="30" height="20" magnitude="1000" sign_inverse="1" colorize="1">
    <text font="letterica16" vert_align="c"/>
</value>
```

Здесь отрицательное значение полезно и окрашивается в синий, положительное вредно и окрашивается в красный.

В старом формате `static_<имя_параметра>` атрибут задаётся непосредственно на строке:

```xml
<static_health_restore_speed x="0" y="0" width="257" height="20" complex_mode="1" colorize="1">
    <texture>ui_inv_icon_health_restore_speed</texture>
    <text font="letterica16" x="20" y="0"/>
</static_health_restore_speed>
```

Для окраски используется альфа-канал иконки: её исходный RGB заменяется цветом эффекта. Подойдут как белые, так и уже цветные DDS-иконки с корректной прозрачностью; перекрашивать атлас не требуется. В формате `caption`/`value` узел `texture_minus` остаётся необязательной альтернативной иконкой для вредного эффекта и совместим с окраской, но для самой окраски не нужен.

Чтобы вернуть прежнее оформление строки, удалите `colorize` или задайте `colorize="0"`. Добавьте настройку в обе версии XML, если она должна работать в обоих вариантах соотношения сторон. Она не меняет знак чисел и группировку характеристик: встроенные контейнеры по умолчанию отображаются без `+`, а лишний разделитель внутри характеристик костюма не возвращается.

## Общие XML-шаблоны строк

Движок поддерживает общие XML-шаблоны как дополнительную возможность. Штатные `af_params.xml` и `booster_params.xml` пока используют обычные строки без `template`. Чтобы унифицировать собственные конфиги, создайте `ui/item_params.xml` и его широкоформатную версию `ui/item_params_16.xml`. В строках задайте ссылку `template`, а общие размеры, отступы, шрифты и иконки вынесите в `param_templates`.

Подключение внутри панели:

```xml
<af_params x="0" y="0" width="260" height="20">
#include "ui\item_params.xml"
    <health_restore_speed template="health_restore_speed">
        <value magnitude="6600" colorize="1"/>
    </health_restore_speed>
</af_params>
```

Бустер ссылается на ту же строку оформления, но сохраняет свой масштаб:

```xml
<boost_health_restore template="health_restore_speed">
    <value magnitude="10000" colorize="1"/>
</boost_health_restore>
```

Файл общих шаблонов имеет следующий формат:

```xml
<param_templates>
    <row x="0" y="0" width="257" height="19">
        <caption x="0" y="0" width="18" height="18" stretch="1">
            <text font="letterica16" x="22" y="0" vert_align="c" r="170" g="170" b="170"/>
        </caption>
        <value x="180" y="0" width="30" height="18">
            <text font="letterica16" vert_align="c"/>
        </value>
    </row>
    <health_restore_speed template="row">
        <caption>
            <texture>ui_am_propery_05</texture>
        </caption>
    </health_restore_speed>
</param_templates>
```

Измените `row`, чтобы одновременно настроить геометрию и шрифт всех строк. Измените `health_restore_speed:caption:texture`, чтобы заменить иконку здоровья у артефактов и бустеров. Шаблоны переносимого веса, радиации и защит используются аналогично.

Атрибуты и дочерние узлы самой строки имеют приоритет над шаблоном. Переопределение дочернего узла объединяется с шаблоном: `<value magnitude="10000"/>` меняет только масштаб и сохраняет общие координаты и шрифт. Можно также локально переопределить `caption`, `texture`, `text`, `unit_str`, `sign_inverse`, `show_sign` и `colorize`. Такая строка будет отличаться от общего оформления только явно указанными свойствами.

Ссылки ищутся в `param_templates` текущей панели; наследование ограничено 16 шаблонами. При отсутствующем шаблоне или цикле строка не создаётся, а причина записывается в лог. Старые строки без `template` поддерживаются. Старый формат `static_<параметр>` также принимает `template`: общая иконка берётся из `caption:texture`, а его расчёт значений и комплексный текст сохраняются.

В штатных конфигурациях `colorize="1"` указан непосредственно в строках эффектов. В собственном общем шаблоне эту настройку можно вынести в `row:value`, оставив локальные `colorize="0"` для абсолютных значений и текстовых эффектов. Дефолт в C++ при отсутствии атрибута остаётся выключенным.

Чтобы окрашивать только иконку, добавьте `text_colorize="0"` на `value`. По умолчанию `text_colorize` наследует значение `colorize`. Штатные строки `additional_weight` и `boost_max_weight` используют эту настройку: положительное число переносимого веса остаётся серым, иконка окрашивается. В старом формате настройка задаётся на узле `static_<параметр>`.
