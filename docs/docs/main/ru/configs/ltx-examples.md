# Настройка LTX и DLTX

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4

## Обзор

Параметры читаются из разных LTX. Для правки предмета или шрифта можно создать отдельный файл DLTX рядом с исходным конфигом. Параметры движка задаются в `engine_external.ltx` активного комплекта ресурсов.

## Синтаксис

Имя файла DLTX имеет вид `mod_<имя исходного файла без расширения>_<суффикс>.ltx`. Секция `![имя]` изменяет существующую секцию, а `[новая_секция]:родитель` создаёт секцию и наследует ключи родителя.

| Что меняется | Где движок ищет ключи |
| --- | --- |
| Настройки конкретного оружия | Секция оружия, например `[wpn_gauss]`. |
| Настройки шрифта | Секция шрифта, которую запрашивает XML или код интерфейса. |
| Глобальные настройки движка | Соответствующая секция `configs/engine_external.ltx`. |

## Примеры

### Настроить вибрацию оружия

Для штатной винтовки Гаусса в ЗП создайте `gamedata/configs/weapons/mod_w_gauss_config_example.ltx`. Движок применит его к `w_gauss.ltx`.

```ini
![wpn_gauss]
vibration_time = 0.15
vibration_factor_left = 0.6
vibration_factor_right = 0.25
```

| Ключ | Значение |
| --- | --- |
| `vibration_time` | Длительность вибрации выстрела в секундах. |
| `vibration_factor_left`, `vibration_factor_right` | Интенсивность левого и правого моторов в диапазоне `0–1`. |

При включённой вибрации геймпада выстрел даст короткий отклик. Левый мотор будет сильнее правого. `![wpn_gauss]` переопределяет перечисленные ключи существующей секции. [Другие параметры оружия](../weapon-system/weapon-config-options.md)

### Добавить шрифт для XML

Создайте `gamedata/configs/mod_system_config_example.ltx`. `system.ltx` подключает `fonts.ltx`, поэтому новой секции доступны параметры `font_default`. Для новой секции используется `[example_status_font]` без `!`.

```ini
[example_status_font]:font_default
size = 16
letter_spacing = 0.5
line_spacing = 3
```

| Ключ | Значение |
| --- | --- |
| `size` | Размер, передаваемый движком при подготовке шрифта. |
| `letter_spacing` | Дополнительный интервал, который прибавляется к ширине символа при расчёте строки. |
| `line_spacing` | Дополнительный интервал, который прибавляется к высоте строки при расчёте многострочного текста. |

Чтобы использовать шрифт, укажите `font="example_status_font"` во вложенном `<text>`. [Пример XML с этим шрифтом](xml-examples.md)

### Включить медикаментозную интоксикацию

Замените значение `EnableMedIntoxication` в существующей секции `[gameplay]` файла `configs/engine_external.ltx`. Ниже показан фрагмент файла. После изменения перезапустите игру.

```ini
[gameplay]
EnableMedIntoxication = true
```

Переключатель включает механику. Величина эффекта конкретной дозы берётся из `eat_intoxication` секции употребляемого предмета. Для настройки порога и изменения состояния используются ключи `intoxication_*` секции актёра. [Параметры и пример для радиопротектора](../gameplay/equipment/general-information.md#медикаментозная-интоксикация)

## См. также

- [DLTX и наследование](dltx.md)
- [Справочник engine_external.ltx](../main/engine-extension-configuration.md)

## Настроить покачивание при движении

Движок читает `[bobbing_effector]` из настроек `system.ltx`. База хранится в `configs/misc/effectors.ltx`, а штатный `mod_system_ixray.ltx` добавляет свои значения. Для каждого режима пара `*_amplitude` и `*_speed` задаёт амплитуду и скорость набора фазы. Префикс `zoom_` задаёт отдельную пару при прицеливании. При пропуске числа загрузчик использует `0`.

| Режим | Без прицеливания | В прицеле |
| --- | --- | --- |
| Шагом | `walk_amplitude`, `walk_speed` | `zoom_walk_amplitude`, `zoom_walk_speed` |
| Бегом | `run_amplitude`, `run_speed` | `zoom_run_amplitude`, `zoom_run_speed` |
| Пригнувшись | `crouch_amplitude`, `crouch_speed` | `zoom_crouch_amplitude`, `zoom_crouch_speed` |
| Медленно пригнувшись | `slow_crouch_amplitude`, `slow_crouch_speed` | `zoom_slow_crouch_amplitude`, `zoom_slow_crouch_speed` |
| Хромота | `limp_amplitude`, `limp_speed` | `zoom_limp_amplitude`, `zoom_limp_speed` |
| Спринт | `sprint_amplitude`, `sprint_speed` | Используется та же пара. |

`amplitude_delta` задаёт скорость перехода между амплитудами за секунду, резерв `1.0`. `affect_hud` включает применение эффекта к HUD, резерв `true`. Чтобы убрать покачивание при ходьбе в прицеле, в существующей секции задайте `zoom_walk_amplitude = 0`.
