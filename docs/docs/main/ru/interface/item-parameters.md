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

