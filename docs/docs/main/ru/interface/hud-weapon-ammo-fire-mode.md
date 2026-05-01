> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

# Патроны, оружие, режим огня

## Обзор

Секция `hud_states` в `maingame.xml` поддерживает три связанных блока:

1. адаптивный виджет патронов
2. отображение режима огня через `mode_mapping`
3. несколько режимов `static_wpn_icon`

## static_ammo_adaptive

1. Включается узлом `static_ammo_adaptive`.
2. Использует `clip_text` и `total_text`.
3. Разделитель задается `separator`.
4. В этом режиме обычные поля патронов скрываются.

## static_fire_mode и mode_mapping

1. `use_icon="1"` переводит режим огня в иконки.
2. `mode_mapping` связывает строку режима и имя иконки.

Пример:

```xml
<static_fire_mode x="899" y="684" width="16" height="20" stretch="1" use_icon="1">
  <mode_mapping text="1" icon="ui_inGame2_icon_fmode_single"/>
  <mode_mapping text="A" icon="ui_inGame2_icon_fmode_auto"/>
  <mode_mapping text="2" icon="ui_inGame2_icon_fmode_2burst"/>
  <mode_mapping text="3" icon="ui_inGame2_icon_fmode_3burst"/>
  <text font="letterica18" align="c" vert_align="c"/>
</static_fire_mode>
```

## static_wpn_icon

1. Legacy режим: иконка боеприпаса.
2. `display_mode="text"`: текст вместо иконки.
3. `caliber="1"`: "калиберный" режим через `hud_group_catalog`.

Дополнительно:

1. `WeaponIconScale` в `engine_external.ltx`.
2. `active_ammo_color` и `inactive_ammo_color` для цветов типов патронов.

Смежный материал: [общая информация](general-information.md), [обзор UI](ui-advanced-features.md).
