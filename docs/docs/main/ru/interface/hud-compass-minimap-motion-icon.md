> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4 <br>
> **Последнее обновление**: 2026-09-28

# Горизонтальный компас, миникарта и новые возможности motion icon для мини-карты

## Обзор

Фича задает навигационный блок HUD: миникарта или горизонтальный компас. Motion icon работает рядом с этим блоком и показывает состояние движения и заметности актора.

Compass bar является **опциональным** UI-элементом. Пока он не активирован через `SetNavigationMode(true)` или устаревший boot-hint `UseCompassBar`, он не создается, не грузит `compass_bar.xml` и не влияет на миникарту, motion icon и PDA online.

Переключение между миникартой и compass bar выполняется в runtime без перезагрузки уровня.

## Режим по умолчанию и runtime-переключение

1. Движковый дефолт: **миникарта**.
2. `UseCompassBar` в `configs/engine_external.ltx` (**deprecated**) задает boot-time hint для модов без Lua. Рекомендуется использовать Lua API или IXR Options.
3. `hud_minimap` управляет **видимостью** активного навигационного блока.
4. Runtime-переключение: `ActorMenu.get_maingame():SetNavigationMode(bool)`, где `true` - compass bar, `false` - миникарта.
5. Сохранение выбора режима в save/user.ltx **не реализовано**.

## Lua API

```lua
local maingame = ActorMenu.get_maingame()
if maingame then
    maingame:SetNavigationMode(true)   -- compass bar (lazy init)
    maingame:SetNavigationMode(false)  -- minimap
    local isCompass = maingame:IsCompassBarMode()
end
```

Доступны readonly-поля `UIZoneMap` и `UICompassBar` на `CUIMainIngameWnd`. `UICompassBar` может быть `nil`, пока compass bar не активирован.

Поле `UICompassBar.visible` синхронизировано с `Show()`: скрытый компас не участвует в child-walk `Update`.

## Контракт единиц compass_bar.xml

Разметка и позиционирование (`x` `y` `width` `height` `padding` `offset_*` `size`) задаются только долями родителя. Целые px в modern XML не используются.

Silent legacy для stock: `*_px` / `size_px` / `draw_offset_*`, либо значение с `abs(v) > 1` на shared-атрибутах, читаются как UI px. Stock `gamedata` не требует правок.

Не-layout (без смены единиц): `fov_angle`, speeds, alpha/tint, `circumference_px` / `tex_width`, `altitude_deadzone`, шрифты, углы.

| Узел | Атрибуты | Единицы | Примечание |
|------|----------|---------|------------|
| `compass_bar` | `x` `y` `width` `height` | доли UI base | всегда relative |
| `background` / `dial` / `strip` | `x` `y` `width` `height` | доли bar | |
| `dial:texture` | `width`/`height` | scale к clip (при `stretch`) | не atlas crop; silent: `scale_*` |
| `dial:texture` | `x`/`y` | доли clip | silent px: `offset_*_px` / `draw_offset_*` / `abs>1` |
| `dial` / `strip` | `circumference_px` / `tex_width` | px логической окружности | не layout |
| `cardinals` hosts | `x` `y` `width` `height` | доли dial clip | |
| tick / marker | `size` / `width`/`height` / `offset_y` | доли host | silent: `size_px` / `offset_y_px` |
| `spots` offsets | `x`/`y` | доли dial clip | |
| `spots:defaults` | `size` / `width`/`height` | доли dial clip | silent: `size_px` |
| `active_target` | `width`/`height` | доли bar | |
| `active_target` | `offset_y` / `padding` | доли bar height / strip width | silent: `*_px` |
| `active_target` children | `x` `y` `width` `height` | доли UI base | marker / distance_text / altitude_arrow |
| `altitude_arrow` | `altitude_deadzone` | метры | не layout |

## Modern schema (optional)

Parser dual-read: modern relative attrs имеют приоритет. Stock сохраняется через silent px-fallback.

| Modern | Legacy alias | Где |
|--------|--------------|-----|
| `compass_bar:dial` | `compass_bar:strip` / `compass_dial:strip` | dial path |
| texture `width`/`height` | `scale_*` / `draw_scale*` | dial texture draw scale |
| texture `x`/`y` (relative) | `offset_*_px` / `draw_offset_*` | dial texture draw offset |
| `circumference_px` | `tex_width` | strip/dial (не layout) |
| `loop` | `tex_loop` | strip/dial |
| `heading_bias_deg` / `phase_deg` | - | dial UV phase |
| `fit="parent"` | full-bleed force | background |
| `dial:draw` / `background:draw` | `*:texture` | look-only visuals |
| `offset_y` | `offset_y_px` / `active_offset_y` | active_target |
| `padding` | `padding_px` / `active_target_padding` | active_target |
| `smoothing` | `smoothing_speed` | active_target |
| `size` / `width`/`height` | `size_px` | marker / spots defaults |
| `offset_y` | `offset_y_px` | cardinal marker/tick |
| `spots:defaults` | `spots:spot_template` | spot size/shadow |
| `style_sheet` | локальные `<shadows>` | default shadow |
| `cardinals` + `<point id angle_deg text>` | `cardinal_points` / `main_cardinals` / `inter_cardinals` | стороны света |

## Разметка шкалы (labels)

Подписи делятся на три группы. У каждой свой `show` и свой `<text>`.

```xml
<cardinals>
  <main show="true">
    <text font="font_rubik_16" .../>
    <point id="n" text="N" r="227" g="79" b="56"/>
    <point id="e" text="E" align="r"/>
    <point id="s" text="S"/>
    <point id="w" text="W" align="l"/>
  </main>
  <intermediate show="true">
    <text font="font_rubik_12" .../>
    <point id="ne" text="NE"/>
    <point id="se" text="SE"/>
    <point id="sw" text="SW"/>
    <point id="nw" text="NW"/>
  </intermediate>
  <degrees show="true" step="15" y="0.25">
    <text font="font_rubik_12" .../>
  </degrees>
  <tick .../>
</cardinals>
```

| Узел | Назначение |
|------|------------|
| `main` | `N E S W`; `show` вкл/выкл |
| `intermediate` | `NE SE SW NW`; общий стиль текста на группе |
| `degrees` | числовая шкала; `show`, `step`, `y` |
| `point` | override подписи/цвета/align поверх стиля группы |

Defaults: main on, intermediate off, degrees off, `step=30`.

Приоритет: код defaults → INI `[compass]` → flat attrs (`show_cardinal`...) → группы `main`/`intermediate`/`degrees`.

Если есть группы или flat attrs / INI, метки строит генератор. Иначе legacy `<point>` / `main_cardinals`.

Правило совпадения: при включенном `main` подписи `0°/90°/180°/270°` не создаются.

Пример INI (DLTX):

```ini
[compass]
show_cardinal = true
show_degrees = true
show_intermediate_cardinal = false
degree_step = 30
```

Пример modern fragment:

```xml
<compass_bar x="0.5" y="0.07" width="0.5" height="0.04" fov_angle="45">
  <style_sheet>
    <shadows thickness="0.5" r="0" g="0" b="0" a="100"/>
  </style_sheet>
  <dial x="0.5" y="0.4" width="1.0" height="1.0" circumference_px="1024" loop="1">
    <texture x="0" y="0.02" width="0.8" height="0.12">ui_inGame2_compass_dial</texture>
  </dial>
  <cardinals>
    <point id="n" text="N"/>
    <point id="e" text="E" align="r"/>
  </cardinals>
  <spots show="1">
    <defaults size="0.08 0.08"/>
  </spots>
  <active_target offset_y="0.03" padding="0.02" smoothing="10"/>
</compass_bar>
```

Имя texture читается из атрибута `texture=` или child text **до первого `<`** (защита от вложенной разметки в text-node).

## Атлас и компоненты compass_bar.xml

### Корневой узел compass_bar

| Атрибут | Назначение | Default |
|---------|------------|---------|
| `fov_angle` | Угол обзора полосы в градусах | `45` |
| `fade_in_speed` | Скорость появления меток | `6` |
| `fade_out_speed` | Скорость исчезновения меток | `5` |
| `min_visible_alpha` | Порог видимости alpha | `0.01` |
| `fov_fade_inner` | Внутренняя граница fade по краям FOV | `0.30` |
| `fov_fade_outer` | Внешняя граница fade по краям FOV | `0.70` |
| `fov_fade_edge_lo` | Нижний край нормализованной зоны fade | `0.05` |
| `fov_fade_edge_hi` | Верхний край нормализованной зоны fade | `0.95` |

### background

Цель: фон и рамка панели. Не влияет на проекцию меток и UV dial.

Геометрия относительно `compass_bar`:

| Атрибут | Назначение | Default |
|---------|------------|---------|
| `x` `y` `width` `height` | доли родителя | `0 0 1 1` |
| `fit` | `parent` / `bar` - растянуть на весь бар | |
| `alignment` / `align` | `l` или `c` | `l` |

Визуал: `background:draw:texture` (legacy: `background:texture`).

### dial / strip

Разделение functional / draw:

**Functional** (корень `dial`/`strip`) влияет на работу компаса:

| Атрибут | Назначение | Default |
|---------|------------|---------|
| `x` `y` `width` `height` | clip/viewport меток и UV | |
| `circumference_px` / `tex_width` | логическая длина круга в px | `1024` |
| `loop` / `tex_loop` | бесшовный круг / clamp | `1` |
| `heading_bias_deg` / `phase_deg` | фаза шкалы | `0` |
| `fov_angle` (на `compass_bar`) | FOV проекции меток | `45` |

**Draw** (`dial:draw` / `dial:draw:texture`, legacy `dial:texture`) только внешний вид:

| Атрибут | Назначение |
|---------|------------|
| имя texture | арт ленты |
| `width`/`height` | размер отрисовки внутри clip (scale при `stretch`) |
| `x`/`y` | сдвиг отрисовки, доли clip |
| `stretch` | stretch static |
| `a` `r` `g` `b` / `color` | tint |

UV-окно считается по ширине clip (`dial` size), а не по `width`/`height` texture. Поэтому draw scale/offset/tint не сдвигают метки и не меняют FOV.

### cardinal_points

Цель: текстовые подписи направлений.

| Атрибут | Назначение | Default |
|---------|------------|---------|
| `fake_target_distance` | Дистанция для проекции N/E/S/W | `1000` |

### spots

| Атрибут | Назначение | Default |
|---------|------------|---------|
| `collect_interval` | Интервал сбора map spots в секундах | `0.1` |
| `show` | Показывать spots на полосе | `1` |

### active_target

Цель: маркер выбранной цели, дистанция, вертикальное отклонение.

**Functional** (корень и layout детей) влияет на поведение:

| Атрибут | Назначение | Default |
|---------|------------|---------|
| `show` | вкл/выкл весь блок | `true` если узел есть |
| `offset_y` | вертикальный offset контейнера, доли bar height | `0` |
| `padding` | отступ от краев strip, доли strip width | |
| `smoothing` | сглаживание движения контейнера | `10` |
| `altitude_deadzone` | порог высоты для стрелки, м | `1.8` |
| `width` `height` | размер контейнера, доли bar | |

Дочерние узлы: `marker`, `altitude_arrow`, `distance_text` - у каждого свой `show` и layout (`x/y/width/height` в долях UI base).

**Draw** (`*:draw`) только внешний вид, не меняет проекцию на strip:

```xml
<active_target offset_y="0.03" padding="0.02" smoothing="10" altitude_deadzone="1.8">
  <marker show="true" x="0" y="-0.02" width="0.04" height="0.05">
    <draw>
      <texture stretch="1">ui_inGame2_hint_wnd_main_window</texture>
      <shadows .../>
    </draw>
  </marker>
  <altitude_arrow show="true" x="-0.004" y="-0.023" width="0.012" height="0.016">
    <draw stretch="1" texture_up="..." texture_down="...">
      <shadows .../>
    </draw>
  </altitude_arrow>
  <distance_text show="true" x="0.007" y="-0.026" width="0.078" height="0.018">
    <text font="..." .../>
  </distance_text>
</active_target>
```

Legacy без `<draw>` и без `show` остается валидным.

#### distance_text

| Атрибут | Назначение | Default |
|---------|------------|---------|
| `format` / `text_format` | Формат sprintf дистанции | `"%.0f m"` |
| `st_format` | ID строки из string table вместо format | - |

## Acceptance invariants

Рефакторинг и правки Compass Bar не должны нарушать:

1. Default = миникарта; без активации compass не грузится.
2. Нет `compass_bar.xml` = soft-fail, миникарта остается рабочей.
3. Миникарта и compass взаимоисключают друг друга.
4. Контракт единиц `compass_bar.xml` из таблицы выше сохраняется, включая legacy aliases.
5. Spot enable: `(ShowOnCompass || HasCompassConfig) && SpotEnabled && same level && texture`.
6. Active task не дублируется в spot-pool.
7. Heading = camera yaw.
8. `hud_minimap` управляет visibility активного nav-блока.
9. Lua API `SetNavigationMode` / `IsCompassBarMode` / `UICompassBar` без breaking changes.
10. Ownership smoke (`RunNavigationOwnershipSmoke`) должен проходить после смены режима.

## Motion icon

### Legacy (поза / заметность)

1. `state_normal`, `state_crouch`, `state_creep`, `state_climb`, `state_run`, `state_sprint` показывают текущий тип движения.
2. `power_progress` показывает выносливость.
3. `luminosity_overlay` и `noise_overlay` накладывают визуальный шум и затемнение.
4. Оверлеи luminosity/noise создаются для режима миникарты и скрываются в режиме compass bar. При возврате на миникарту оверлеи восстанавливаются без пересоздания HUD.

### Status glow (opt-in, CUIMotionIcon)

Общий soft-glow статуса живет в `CUIMotionIcon`, не в compass bar. Одна белая текстура (`background` / `status_icon` в `motion_icon.xml`), цвет и интенсивность задаются состоянием.

Opt-in: без `[motion_icon] enabled` / `status_tint` и без tintable static feature выключена. Stock `motion_icon.xml` без background остается как раньше.

Приоритет состояний: `Enemy > Anomaly > SafeZone > None`.

| Состояние | Источник | Свечение |
|-----------|----------|----------|
| Enemy | `GetThreatNormalized` (`SetActorVisibility`) | красный + optional pulse |
| Anomaly | `CActorCondition::GetZoneDanger` | оранжевый |
| SafeZone | актор внутри restrictor из `safe_zones` (probe `Position`, как `actor_in_zone`); без списка SafeZone выключен; legacy: `safe_zone_source = camp` | зеленый |
| None | иначе | alpha 0 |

Конфиг: INI `[motion_icon]` + `motion_icon.xml` (`background status_tint="1"` или узел `status_icon`).

```xml
<background x="0" y="0" width="500" height="35" stretch="1" status_tint="1">
  <texture>ui_inGame2_compass_motion_icon</texture>
</background>
```

```ini
[motion_icon]
enabled = true
enemy_color = 255, 13, 8, 255
safe_color = 38, 255, 64, 255
anomaly_color = 255, 140, 13, 255
default_color = 255, 255, 255, 0
enemy_intensity = 1.0
safe_intensity = 0.7
anomaly_intensity = 0.9
color_transition_speed = 6.0
pulse_enemy = true
safe_zones = "zat_a2_sr_noweap, jup_a6_sr_noweap, jup_b41_sr_noweap, pri_a16_sr_noweap"
```

Цвета: `R, G, B` или `R, G, B, A` в диапазоне `0..255`. Без A альфа = 255. Старый float `0..1` тоже читается (если все каналы <= 1).

Секции `[motion_icon]` в stock `system.ltx` нет: в аддоне создавай её обычным `[motion_icon]`, не `![motion_icon]` (override несуществующей секции падает с DLTX ERROR и конфиг не применяется).

SafeZone: один путь. Список `safe_zones` - имена space restrictors (хабы CoP: noweap). Проверка как `actor_in_zone`: `restrictor->inside(Position)`. Пустой список = зеленый не зажигается. Кемпы сами по себе не светят. Legacy-опция: `safe_zone_source = camp` (+ `max_safe_distance`).

Вспомогательно: `level.actor_in_restrictor(name)`.

В логе: `motion_icon: safe_zones=4 camp=0`.

Tint target: `status_icon` если есть, иначе `_compassBackground`.

## Примеры

Сценарий 1: Активация через Lua (рекомендуется)

```lua
ActorMenu.get_maingame():SetNavigationMode(true)
```

Сценарий 2: Legacy boot-hint через DLTX (deprecated)

```ini
[ui]
UseCompassBar = true
```

Смежный материал: [обзор UI](ui-advanced-features.md).
