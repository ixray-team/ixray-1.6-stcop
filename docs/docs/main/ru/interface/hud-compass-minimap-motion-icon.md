> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4 <br>
> **Последнее обновление**: 2026-07-22

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

| Узел | Атрибуты | Единицы | Примечание |
|------|----------|---------|------------|
| `compass_bar` | `x` `y` `width` `height` | доли родителя (UI base) | всегда relative, без эвристики `<= 1` |
| `strip` / `cardinal_points` | `x` `y` `width` `height` | доли родителя | strip хранит `_stripRel*`, cardinals - доли clip-окна |
| `strip:texture` | `draw_scale_x/y` или `draw_scale`; legacy `width`/`height` | scale относительно clip / native | не atlas crop |
| `strip:texture` | `draw_offset_x/y`; legacy `x`/`y` | px offset отрисовки | |
| `strip` | `tex_width` | px логической окружности | при сильном расхождении с atlas width - warning в лог |
| `marker` кардинала | `width`/`height` | `<= 1` relative к host, иначе px; `offset_y` px | |
| `active_target` | window `width`/`height`/`x` | px | |
| `active_target` | `active_offset_y` / `offset_y` / legacy `y` | px вертикальный offset контейнера | приоритет: `active_offset_y` > `offset_y` > `y` |
| `altitude_arrow` | `altitude_deadzone` | метры | override значения контейнера `active_target` |
| `distance_text` / arrows / marker | `x` `y` `width` `height` | px | |

Пример HD HUD (без правок XML): `strip` `width="0.88"`, texture `width="0.9" height="0.22" y="9"`, `tex_width="1024"`, cardinal tick `width="4"`, `active_target y="0"` как offset.

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

Цель: фон и рамка панели.

### strip

Цель: лента направлений.  
Логика: движок сдвигает UV в зависимости от поворота камеры.

### strip:texture

Цель: scale и offset отрисовки dial (не crop атласа).

| Атрибут | Назначение | Default |
|---------|------------|---------|
| `draw_scale` / `draw_scale_x` / `draw_scale_y` | явный scale | legacy `width`/`height` |
| `draw_offset_x` / `draw_offset_y` | явный offset px | legacy `x`/`y` |
| `width` / `height` / `x` / `y` | legacy aliases | `1` / `1` / `0` / `0` |

### tex_width

Цель: логическая ширина шкалы в пикселях.  
Если задано неверно, скорость движения меток не совпадает с углом обзора.

### tex_loop

Цель: циклическая прокрутка.  
`1` бесшовный круг, `0` зажим по краям.

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

| Атрибут | Назначение | Default |
|---------|------------|---------|
| `active_offset_y` / `offset_y` / `y` | вертикальный offset контейнера, px | `0` |
| `altitude_deadzone` | порог высоты для стрелки | `1.8` |
| `padding` | отступ от краев strip | `8` |

#### distance_text

| Атрибут | Назначение | Default |
|---------|------------|---------|
| `format` / `text_format` | Формат sprintf дистанции | `"%.0f m"` |
| `st_format` | ID строки из string table вместо format | - |

## Motion icon

1. `state_normal`, `state_crouch`, `state_creep`, `state_climb`, `state_run`, `state_sprint` показывают текущий тип движения.
2. `power_progress` показывает выносливость.
3. `luminosity_overlay` и `noise_overlay` накладывают визуальный шум и затемнение.
4. Оверлеи luminosity/noise создаются для режима миникарты и скрываются в режиме compass bar. При возврате на миникарту оверлеи восстанавливаются без пересоздания HUD.

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
