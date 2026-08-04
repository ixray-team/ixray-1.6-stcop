> [!IMPORTANT]
> **Статус**: В разработке <br>
> **Минимальная версия**: rolling <br>
> **Последнее обновление**: 2026-07-31

# Окно сна (CUISleepWnd)

## Обзор

Нативное окно сна `CUISleepWnd` читает layout из `configs/ui/ui_sleep_dialog.xml`, поддерживает vanilla-раскладку CoP и расширяется через опциональный узел `sleep_params` и дополнительные XML-узлы.

Окно создается вместе с HUD (`CUIGameSP`) и управляется из Lua через `get_hud()` / глобалы рядом с `show_sleep_dialog*`: открытие, query, session-оверрайды, `ForceSleep` / `AbortSleep`, veto через `actor_on_can_sleep`.

Без вызовов нового API поведение совпадает с прежним native-путем (XML + пресеты).

> [!NOTE]
> Vanilla-вход `xr_effects.sleep` → `ui_sleep_dialog.sleep()` по умолчанию все еще открывает Lua-диалог. Чтобы использовать C++ окно, переведите точку входа на `ShowSleepDialog` (см. раздел [Открытие из скриптов](#открытие-из-скриптов)).

> [!WARNING]
> Не правьте legacy-файл в `gamedata/configs/ui/ui_sleep_dialog.xml` напрямую. Используйте XML Override / DLTX override своего аддона.

## Базовые XML-узлы

Обязательные узлы (как в vanilla CoP):

| Узел | Назначение |
|------|------------|
| `background` | Фон диалога |
| `sleep_static` | Панорама неба (создается дважды из одного узла) |
| `static_cover` | Рамка поверх панорамы |
| `st_marker` | Маркер выбранных часов (дочерний к `static_cover`) |
| `sleep_st_1` ... `sleep_st_24` | Подписи часов (legacy) |
| `time_track` | Трекбар длительности сна |
| `btn_sleep` | Подтверждение |
| `btn_cancel` | Отмена |

Опциональные:

| Узел | Назначение |
|------|------------|
| `sleep_params` | Все расширенные параметры |
| `sleep_hours_strip` | Авто-лента часов вместо `sleep_st_*` |
| `sleep_preset_btn` | Шаблон кнопок пресетов |
| `st_time_now` | Текущее игровое время |
| `st_sleep_duration` | Длительность сна |
| `st_wake_time` | Время пробуждения |

## sleep_params: полный список атрибутов

Все атрибуты опциональны. Если узла `sleep_params` нет, используются значения по умолчанию (vanilla-поведение).

```xml
<sleep_params
  panorama_texture="ui_inGame2_sky_panorama"
  panorama_mode="0"
  panorama_hours="24"
  panorama_bind="current"
  panorama_smooth_speed="0"
  hour_label_suffix="st_sleep_hours"
  warning_box_template="message_box_ok"
  warning_bleeding="sleep_warning_bleeding"
  warning_radiation="sleep_warning_radiation"
  warning_both="sleep_warning_all_pleasures"
  allow_sleep_with_bleeding="0"
  min_hours="1"
  max_hours="24"
  cam_anm="camera_effects\sleep.anm"
  pp_effector="sleep_fade.ppe"
  cam_id="10"
  pp_id="11"
  marker_min_x="5"
  restore_power="1"
  mute_music="1"
  mute_effects="1"
  preset_spacing="4"
  presets_confirm="0"
  fmt_time_now="%s"
  fmt_sleep_duration="%d%s"
  fmt_wake_time="%s"
  time_separator=":"
  time_precision="minutes"
  snd_open=""
  snd_close=""
  snd_sleep=""
  snd_cancel=""
  snd_track=""
  snd_preset=""
  snd_warning=""
/>
```

| Атрибут | Тип | По умолчанию | Описание |
|---------|-----|--------------|----------|
| `panorama_texture` | texture id | `ui_inGame2_sky_panorama` | Текстура панорамы. Если id нет в TextureMaster - fallback на vanilla |
| `panorama_mode` | `0` / `1` | `0` | Игнорируется (legacy attr) |
| `panorama_hours` | int >= 1 | `24` | Сколько часов умещается в ширину виджета |
| `panorama_bind` | `current` / `wake` | `current` | `current` = игровой час; `wake` = час подъема (`now + duration`) |
| `panorama_smooth_speed` | float | `0` | Плавный скролл wrap (`0` = мгновенно, как vanilla) |
| `panorama_scale` | `stretch` / `native` | `stretch` | `stretch` = весь день в окно; `native` = 1:1 design px, окно клипает полоску |
| `panorama_tex_scale` | float >= 1 | `1` | Как `scale` у файла в textures_descr (HD: `2`) |
| `panorama_wrap` | `0` / `1` | `1` | `1` = dual-static wrap; `0` = clamp без склейки (native) |
| `hour_label_suffix` | string table id | `st_sleep_hours` | Суффикс подписей (`%d` + перевод) |
| `warning_box_template` | message box id | `message_box_ok` | Шаблон warning MB |
| `warning_bleeding` | string table id | `sleep_warning_bleeding` | Текст при кровотечении |
| `warning_radiation` | string table id | `sleep_warning_radiation` | Текст при радиации |
| `warning_both` | string table id | `sleep_warning_all_pleasures` | Текст при обоих |
| `allow_sleep_with_bleeding` | `0` / `1` | `0` | `1` = кровотечение не блокирует сон |
| `min_hours` | int >= 1 | `1` | Нижняя граница трекбара |
| `max_hours` | int | `24` | Верхняя граница трекбара |
| `cam_anm` | path | `camera_effects\sleep.anm` | Cam effector |
| `pp_effector` | ppe name | `sleep_fade.ppe` | Postprocess effector |
| `cam_id` | int | `10` | ID cam effector |
| `pp_id` | int | `11` | ID PP effector |
| `marker_min_x` | float | `5` | X маркера при 1 часе (когда delta = 0) |
| `restore_power` | float | `1` | Сила актора после сна |
| `mute_music` | `0` / `1` | `1` | Глушить музыку на время сна |
| `mute_effects` | `0` / `1` | `1` | Глушить эффекты на время сна |
| `preset_spacing` | float | `4` | Отступ между кнопками пресетов |
| `presets_confirm` | `0` / `1` | `0` | `1` = клик по пресету сразу запускает сон |
| `fmt_time_now` | printf / st id | `%s` | Формат текущего времени |
| `fmt_sleep_duration` | printf / st id | `%d%s` | Формат длительности (`%d` + суффикс) |
| `fmt_wake_time` | printf / st id | `%s` | Формат времени пробуждения |
| `time_separator` | char | `:` | Разделитель часов/минут |
| `time_precision` | string | `minutes` | `hours` / `minutes` / `seconds` |
| `snd_*` | sound name | пусто | UI-звуки (см. ниже) |

---

## Примеры по фичам

### 1. Vanilla-совместимый override без расширений

Ничего не добавляйте в `sleep_params`. Достаточно XML Override с исходной раскладкой CoP (`background`, `sleep_static`, `sleep_st_1..24`, `time_track`, кнопки).

→ Окно работает как vanilla: split-панорама, 1..24 часа, стандартные warning-строки.

### 2. Своя текстура панорамы

```xml
<sleep_params panorama_texture="ui_mod_sky_panorama_hd"/>
```

→ `sleep_static` / `sleep_static2` инициализируются этой текстурой. При отсутствии id в TextureMaster движок пишет warning и берет `ui_inGame2_sky_panorama`.

### 3. Плавный wrap панорамы с привязкой к часам

Dual-static wrap (`panorama_wrap="1"`) - ванильный разрез полоски. Цельная 24h-текстура скроллится внутри окна `sleep_static`.

Для HD-полосок с яркими ориентирами у обоих краев (луна у 0 и у 24) wrap дает видимый дубль. Тогда:

```xml
<sleep_params
  panorama_texture="ui_sleep_wnd_new_panorama"
  panorama_hours="24"
  panorama_bind="wake"
  panorama_smooth_speed="10"
  panorama_scale="native"
  panorama_tex_scale="2"
  panorama_wrap="1"
/>
```

→ `panorama_wrap="1"` дает непрерывный dual-static скролл через полночь. Если у обоих краев полоски одна и та же луна, в окне будет дубль: оставьте ориентир только у одного края. `panorama_wrap="0"` - clamp без склейки (возможен скачок на стыке суток). `panorama_bind="wake"` - панорама едет к времени подъема.

### 4. Ограничение длительности сна

```xml
<sleep_params min_hours="2" max_hours="8"/>
```

→ Трекбар принимает только 2..8. Пресеты вне диапазона игнорируются.

Для разового ограничения на один показ (квест / конкретная койка) без правки XML используйте session API `SetSleepHoursRange` (см. [Session-оверрайды](#session-оверрайды)).

### 5. Авто-лента часов вместо 24 узлов

Вместо `sleep_st_1` ... `sleep_st_24` добавьте один узел:

```xml
<sleep_hours_strip
  x="40" y="163" width="591" height="15"
  count="12"
  padding="0"
  spacing="0"
  complex_mode="0"
>
  <text align="c" vert_align="c" font="letterica16" r="80" g="80" b="80"/>
</sleep_hours_strip>
```

Атрибуты ленты:

| Атрибут | Описание |
|---------|----------|
| `count` | Число лейблов (1..24), по умолчанию 24 |
| `padding` / `pad_left` / `pad_right` | Внутренние отступы |
| `spacing` | Зазор между лейблами |
| `label_width` | Фиксированная ширина; если `0`, считается автоматически |

→ Подписи заполняются как `(текущий_час + i + 1) % 24` + `hour_label_suffix` (как в vanilla Lua).

### 6. Текстовые индикаторы времени

```xml
<sleep_params
  fmt_time_now="st_sleep_now_fmt"
  fmt_sleep_duration="st_sleep_dur_fmt"
  fmt_wake_time="st_sleep_wake_fmt"
  time_separator=":"
  time_precision="minutes"
/>

<st_time_now x="40" y="190" width="180" height="16" format="Сейчас: %s">
  <text font="letterica16" align="l" r="170" g="170" b="170"/>
</st_time_now>

<st_sleep_duration x="230" y="190" width="180" height="16">
  <text font="letterica16" align="c" r="170" g="170" b="170"/>
</st_sleep_duration>

<st_wake_time x="420" y="190" width="180" height="16" format="Подъем: %s">
  <text font="letterica16" align="r" r="170" g="170" b="170"/>
</st_wake_time>
```

→ Атрибут `format` у узла перекрывает соответствующий `fmt_*` из `sleep_params`. Значения обновляются при смене трекбара.

Примеры string table:

```xml
<text id="st_sleep_now_fmt">Сейчас: %s</text>
<text id="st_sleep_dur_fmt">Сон: %d%s</text>
<text id="st_sleep_wake_fmt">Пробуждение: %s</text>
```

### 7. Предупреждения и сон при кровотечении

```xml
<sleep_params
  warning_box_template="message_box_ok"
  warning_bleeding="st_mod_sleep_bleed"
  warning_radiation="st_mod_sleep_rad"
  warning_both="st_mod_sleep_both"
  allow_sleep_with_bleeding="1"
/>
```

→ При `allow_sleep_with_bleeding="1"` кровотечение не показывает warning и не блокирует окно. Радиация по-прежнему показывает warning MB вместо диалога сна.

### 8. Свои cam / PP / восстановление силы / mute

```xml
<sleep_params
  cam_anm="camera_effects\mod_sleep.anm"
  pp_effector="mod_sleep_fade.ppe"
  cam_id="10"
  pp_id="11"
  restore_power="0.85"
  mute_music="1"
  mute_effects="0"
/>
```

→ На подтверждении: `disable_ui`, cam+pp, info `actor_is_sleeping`, mute по флагам. После второй фазы cam: `enable_ui`, восстановление громкости, info `tutorial_sleep`, снятие `actor_is_sleeping` / `sleep_active`.

### 9. UI-звуки

```xml
<sleep_params
  snd_open="interface\sleep_open"
  snd_close="interface\sleep_close"
  snd_sleep="interface\sleep_confirm"
  snd_cancel="interface\sleep_cancel"
  snd_track="interface\sleep_tick"
  snd_preset="interface\sleep_preset"
  snd_warning="interface\sleep_warning"
/>
```

| Звук | Когда |
|------|-------|
| `snd_open` | Успешное открытие диалога |
| `snd_warning` | Показ warning MB |
| `snd_sleep` | Кнопка «Спать» |
| `snd_cancel` | Отмена (если пусто - `snd_close`) |
| `snd_close` | Fallback для отмены |
| `snd_track` | Шаг трекбара (клавиатура / геймпад) |
| `snd_preset` | Клик по пресету |

### 10. Кнопки-пресеты часов

XML-шаблон (обязателен, иначе пресеты не строятся):

```xml
<sleep_params preset_spacing="6" presets_confirm="0"/>

<sleep_preset_btn x="40" y="212" width="48" height="24" check_mode="0">
  <text font="letterica16" align="c"/>
  <texture>ui_inGame2_Mp_bigbuttone</texture>
</sleep_preset_btn>
```

Lua (один раз при старте мода или перед показом):

```lua
local hud = get_hud()
hud:SetSleepHourPresets({ 1, 3, 6, 8, 12 })
hud:ShowSleepDialog()
```

→ Кнопки раскладываются вправо от `x` шаблона с шагом `width + preset_spacing`. Часы вне `min_hours`..`max_hours` пропускаются.

Мгновенный сон по пресету:

```xml
<sleep_params presets_confirm="1"/>
```

```lua
get_hud():SetSleepHourPresets({ 6, 8 })
get_hud():ShowSleepDialog()
```

→ Клик по пресету сразу прячет диалог и запускает sleep pipeline.

Сброс:

```lua
get_hud():ClearSleepHourPresets()
```

### 11. Открытие с заранее выбранным временем

```lua
get_hud():ShowSleepDialogAtHour(6)
-- или
show_sleep_dialog_at_hour(6)
```

→ Значение клампится в `min_hours`..`max_hours`, затем показывается диалог (или warning).

### 12. Полный готовый mod-layout (компактный)

Пример XML Override `ui_sleep_dialog.xml` с лентой, временем и пресетами:

```xml
<w>
  <sleep_params
    panorama_mode="0"
    min_hours="1"
    max_hours="12"
    preset_spacing="4"
    presets_confirm="0"
    fmt_time_now="Сейчас: %s"
    fmt_sleep_duration="Сон: %d%s"
    fmt_wake_time="Подъем: %s"
    snd_track="interface\inv_slot"
  />

  <background x="173" y="220" width="677" height="310">
    <texture>ui_inGame2_message_box</texture>
  </background>

  <sleep_static x="40" y="32" width="591" height="128">
    <texture>ui_inGame2_sky_panorama</texture>
  </sleep_static>

  <static_cover x="37" y="32" width="601" height="128">
    <texture>ui_inGame2_panorama_window</texture>
  </static_cover>

  <st_marker x="0" y="0" width="30" height="118">
    <texture>ui_inGame2_marker</texture>
  </st_marker>

  <sleep_hours_strip x="40" y="163" width="591" height="15" count="12" complex_mode="0">
    <text align="c" vert_align="c" font="letterica16" r="80" g="80" b="80"/>
  </sleep_hours_strip>

  <time_track x="37" y="177" width="601" height="16" is_integer="1" step="1">
    <options_item entry="g_sleep_time" group="sleep" depend="runtime"/>
  </time_track>

  <st_time_now x="40" y="198" width="180" height="16">
    <text font="letterica16" align="l" r="170" g="170" b="170"/>
  </st_time_now>
  <st_sleep_duration x="230" y="198" width="180" height="16">
    <text font="letterica16" align="c" r="170" g="170" b="170"/>
  </st_sleep_duration>
  <st_wake_time x="420" y="198" width="180" height="16">
    <text font="letterica16" align="r" r="170" g="170" b="170"/>
  </st_wake_time>

  <sleep_preset_btn x="40" y="222" width="52" height="24" check_mode="0">
    <text font="letterica16" align="c"/>
    <texture>ui_inGame2_Mp_bigbuttone</texture>
  </sleep_preset_btn>

  <btn_sleep x="209" y="260" width="127" height="28" check_mode="0">
    <window_name>button_yes</window_name>
    <text font="letterica18">sleep_ok_button</text>
    <texture>ui_inGame2_Mp_bigbuttone</texture>
  </btn_sleep>

  <btn_cancel x="341" y="260" width="127" height="28" check_mode="0">
    <window_name>button_no</window_name>
    <text font="letterica18">Btn_Cancel</text>
    <texture>ui_inGame2_Mp_bigbuttone</texture>
  </btn_cancel>
</w>
```

И скрипт инициализации пресетов:

```lua
function bind_sleep_presets()
  local hud = get_hud()
  if hud and hud.SetSleepHourPresets then
    hud:SetSleepHourPresets({ 1, 2, 4, 6, 8, 12 })
  end
end
```

---

## Открытие из скриптов

### Синтаксис: API HUD и глобалы

Методы доступны на `get_hud()` (`CUIGameCustom`). Ряд глобалов дублирует частые вызовы.

#### Диалог

| Метод / глобал | Описание |
|----------------|----------|
| `IsSleepDialogReady` / `is_sleep_dialog_ready` | Layout загружен (`time_track` есть) |
| `IsSleepDialogShown` / `is_sleep_dialog_shown` | Диалог на экране |
| `ShowSleepDialog` / `show_sleep_dialog` | Открыть с текущим значением трекбара |
| `ShowSleepDialogAtHour(n)` / `show_sleep_dialog_at_hour(n)` | Открыть с выбранными `n` часами |
| `HideSleepDialog` / `hide_sleep_dialog` | Закрыть как Cancel |
| `CancelSleepDialog` / `cancel_sleep_dialog` | То же, что Hide |
| `GetSleepSelectedHours` / `get_sleep_selected_hours` | Текущие часы на треке |
| `SetSleepSelectedHours(n)` / `set_sleep_selected_hours(n)` | Выставить часы (без открытия или при открытом диалоге) |
| `ConfirmSleep` / `confirm_sleep` | Программный аналог кнопки Sleep (если диалог показан) |
| `SetSleepHourPresets({...})` | Задать пресеты (таблица чисел) |
| `ClearSleepHourPresets` | Убрать пресеты |

Прямого доступа к виджету `CUITrackBar` из Lua нет: только значение часов, диапазон и пресеты.

#### Жизненный цикл сна

| Метод / глобал | Описание |
|----------------|----------|
| `IsActorSleeping` / `is_actor_sleeping` | `m_camPhase != 0` и/или info `actor_is_sleeping` |
| `GetSleepPhase` / `get_sleep_phase` | `0` idle, `1` fade-in до прыжка, `2` fade-out после прыжка |
| `ForceSleep(n)` / `force_sleep(n)` | Сон без UI: выставить часы и сразу Confirm |
| `AbortSleep` / `abort_sleep` | Отмена: dialog = Cancel; phase 1 = снять cam/PP без прыжка времени; phase 2 = досрочный WakeUp (время уже сдвинуто) |

#### Session-оверрайды

Применяются поверх XML `sleep_params` на один показ / один `ForceSleep`. Сбрасываются в WakeUp, Cancel/Hide, Abort, OK warning-box без сна.

| Метод | Описание |
|-------|----------|
| `SetSleepHoursRange(min, max)` / `ClearSleepHoursRange` | Временный min/max трекбара |
| `SetSleepAllowBleeding(bool)` / `ClearSleepAllowBleeding` | Разрешить сон при кровотечении |
| `SetSleepRestorePower(float)` / `ClearSleepRestorePower` | Сила после сна |
| `SetSleepMute(mute_music, mute_effects)` / `ClearSleepMute` | Mute на время сна |
| `ClearSleepSessionOverrides` | Сбросить все session-поля разом |

Не вынесены в runtime API (остаются в XML): панорама, `fmt_*`, cam/ppe имена, UI-звуки, layout кнопок.

#### Запрет сна

| Метод / глобал | Описание |
|----------------|----------|
| `SetSleepBlocked(bool [, warning_text])` / `set_sleep_blocked(...)` | C++-флаг; при `true` диалог не открывается (как veto) |

### Примеры

Сценарий 1: квест - только 1..3 часа на койке

```lua
get_hud():SetSleepHoursRange(1, 3)
get_hud():SetSleepHourPresets({ 1, 2, 3 })
show_sleep_dialog()
```

→ Трекбар и пресеты ограничены 1..3 до Cancel / WakeUp / Abort.

Сценарий 2: катсцена без окна

```lua
force_sleep(6)
```

→ Сразу phase 1 (disable_ui + cam/pp), без показа диалога.

Сценарий 3: тревога во время fade-in

```lua
if get_hud():GetSleepPhase() == 1 then
  get_hud():AbortSleep()
end
```

→ Cam/PP снимаются без прыжка времени, UI/громкость восстанавливаются, шлется `actor_on_sleep_aborted(1)`.

Сценарий 4: выставить часы при открытом диалоге

```lua
get_hud():SetSleepSelectedHours(4)
get_hud():ConfirmSleep()
```

### Перевод vanilla-входа на C++ окно

В override `ui_sleep_dialog.script` (или точке, где вызывается сон):

```lua
function sleep()
  show_sleep_dialog()
end
```

Или из эффекта / зоны:

```lua
function open_sleep_from_bed(actor, obj)
  get_hud():ShowSleepDialog()
end
```

> [!WARNING]
> Не вызывайте Lua-колбэки (`SendScriptCallback`) изнутри уже активного Lua-стека эффекта (`xr_effects` → tutorial `use`), если колбэк снова дергает engine Lua. Для открытия окна используйте прямой вызов `ShowSleepDialog` без промежуточных nested proxy. `actor_on_can_sleep` вызывается из C++ через raw `lua_State` + `SendScriptCallback` (без nested luabind functor).

---

## Script callbacks

| Callback | Когда | Можно остановить | Аргументы |
|----------|-------|------------------|-----------|
| `actor_on_can_sleep` | Перед показом диалога (после bleed/rad, только на пути успешного открытия) | да (`flags.allow`) | `hours`, `flags` |
| `actor_on_before_sleep` | После 1-й фазы cam, до перемотки времени | нет | `hours` |
| `actor_on_sleep` | После перемотки времени, погоды и `SetPower` | нет | `hours` |
| `actor_on_sleep_aborted` | После `AbortSleep` | нет | `phase` (`0` dialog, `1` fade-in, `2` fade-out) |

```lua
RegisterScriptCallback("actor_on_before_sleep", function(hours)
  printf("before sleep: %s h", hours)
end)

RegisterScriptCallback("actor_on_sleep", function(hours)
  printf("slept: %s h", hours)
end)

RegisterScriptCallback("actor_on_sleep_aborted", function(phase)
  printf("sleep aborted, phase=%s", phase)
end)
```

### actor_on_can_sleep

Вызывается из `TestAndShow` после проверок bleeding/radiation. Если bleed/rad уже показали warning - can_sleep не вызывается.

```lua
-- flags = { allow = true, warning_text = "" }
RegisterScriptCallback("actor_on_can_sleep", function(hours, flags)
  if in_danger_zone() then
    flags.allow = false
    flags.warning_text = "st_cannot_sleep_here"
  end
end)
```

→ При `allow == false`: непустой `warning_text` показывает warning MB, иначе диалог просто не открывается.

Дополнительно без подписки на callback: `SetSleepBlocked(true, "st_cannot_sleep_here")` проверяется вместе с can_sleep.

См. также [сигналы IXR](../scripting/ixr-framework/modules/ixr_signals.md).

---

## Sleep pipeline (кратко)

1. Открытие: bleeding/radiation → (если ок) `SetSleepBlocked` + `actor_on_can_sleep` → warning MB или `ShowDialog`.
2. Confirm / ForceSleep: `xr_effects.disable_ui`, cam+pp, `actor_is_sleeping`, mute.
3. Конец 1-й cam: `actor_on_before_sleep` → вторая cam → `ChangeGameTime` → weather/surge managers → `SetPower` → `actor_on_sleep`.
4. Конец 2-й cam: `enable_ui`, unmute, `tutorial_sleep`, снятие sleep info, сброс session-оверрайдов.
5. AbortSleep: см. таблицу жизненного цикла; затем `actor_on_sleep_aborted(phase)`.

Surge/weather:

1. Перед сном: `surge_manager.skip_message = false`.
2. После перемотки: `time_forwarded = true`, `forced_weather_change`.
3. Если surge started и есть `weather_fx` - `StopWFX` + повторный weather change.

---

## Геймпад

| Действие | Поведение |
|----------|-----------|
| `kUI_LEFT` / `kUI_RIGHT` | Шаг трекбара + `snd_track`, с repeat через ActionRepeaters |
| `kUI_ACCEPT` | Как `btn_sleep` |
| `kUI_BACK` | Как `btn_cancel` |

---

## Рекомендации

✔️ Правильное использование:

1. Меняйте UI через XML Override / DLTX, не трогая legacy `gamedata`.
2. Для компактного UI используйте `sleep_hours_strip` вместо 24 узлов.
3. Пресеты задавайте из Lua после загрузки HUD.
4. Открывайте C++ окно через `ShowSleepDialog`, а не через старый Lua class, если нужны новые фичи.
5. Разовые квестовые лимиты - через `SetSleepHoursRange`, постоянный layout - через XML `min_hours`/`max_hours`.
6. Катсцены без окна - `ForceSleep`; отмена до прыжка времени - `AbortSleep` в phase 1.

⚠️ Ограничения:

1. Vanilla `ui_sleep_dialog.sleep()` по умолчанию все еще Lua.
2. Без узла `sleep_preset_btn` `SetSleepHourPresets` не создаст кнопки.
3. Без `time_track` layout считается неинициализированным, `ShowSleepDialog` пропускается (с логом).
4. Нет Lua-доступа к самому виджету трекбара (только часы / range / пресеты).
5. `ForceSleep` не проходит gate `actor_on_can_sleep` / `SetSleepBlocked` (обход UI).

✖️ Анти-паттерны:

1. Правка оригинального `gamedata/configs/ui/ui_sleep_dialog.xml` в репозитории движка.
2. Вызов `ShowSleepDialog` из глубоко nested Lua-стека с дополнительными luabind proxy вокруг.
3. Одновременное наличие `sleep_hours_strip` и ожидание работы `sleep_st_*` (strip имеет приоритет).
4. Ожидание отката времени при `AbortSleep` в phase 2 (время уже сдвинуто).

## Связанные разделы

[обзор UI](ui-advanced-features.md), [сигналы IXR](../scripting/ixr-framework/modules/ixr_signals.md), [геймпады](../gameplay/general/gamepads.md)
