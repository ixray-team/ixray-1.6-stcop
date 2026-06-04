> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

# Горизонтальный компас, миникарта и новые возможности motion icon для мини-карты

## Обзор

Фича задает навигационный блок HUD: миникарта или горизонтальный компас. Motion icon работает рядом с этим блоком и показывает состояние движения и заметности актора.

Оба виджета инициализируются при загрузке HUD. Переключение между ними выполняется в runtime без перезагрузки уровня.

## Режим по умолчанию и runtime-переключение

1. `UseCompassBar` в `configs/engine_external.ltx` задает **стартовый режим** для нового профиля, если пользовательский выбор еще не сохранен.
2. `UseCompassBar = true` включает горизонтальный компас по умолчанию.
3. `UseCompassBar = false` включает миникарту по умолчанию.
4. `hud_minimap` управляет **видимостью** активного навигационного блока.
5. Runtime-переключение типа навигации выполняется через Lua API `ActorMenu.get_maingame():SetNavigationMode(bool)`, где `true` означает compass bar, `false` миникарту.

## Lua API

```lua
local maingame = ActorMenu.get_maingame()
if maingame then
    maingame:SetNavigationMode(true)   -- compass bar
    maingame:SetNavigationMode(false)  -- minimap
    local isCompass = maingame:IsCompassBarMode()
end
```

Доступны readonly-поля `UIZoneMap` и `UICompassBar` на `CUIMainIngameWnd`.

## Атлас и компоненты compass_bar.xml

### background

Цель: фон и рамка панели.

### strip

Цель: лента направлений.  
Логика: движок сдвигает UV в зависимости от поворота камеры.

### strip:texture

Цель: прямоугольник выборки из атласа.  
Параметры: `x`, `y`, `width`, `height`.

### tex_width

Цель: реальная ширина шкалы в пикселях.  
Если задано неверно, скорость движения меток не совпадает с углом обзора.

### tex_loop

Цель: циклическая прокрутка.  
`1` бесшовный круг, `0` зажим по краям.

### cardinal_points

Цель: текстовые подписи направлений.

### active_target

Цель: маркер выбранной цели, дистанция, вертикальное отклонение.

## Motion icon

1. `state_normal`, `state_crouch`, `state_creep`, `state_climb`, `state_run`, `state_sprint` показывают текущий тип движения.
2. `power_progress` показывает выносливость.
3. `luminosity_overlay` и `noise_overlay` накладывают визуальный шум и затемнение.
4. Оверлеи luminosity/noise создаются для режима миникарты и скрываются в режиме compass bar. При возврате на миникарту оверлеи восстанавливаются без пересоздания HUD.

## Пример включения compass bar по умолчанию

```ini
[ui]
UseCompassBar = true
```

Смежный материал: [обзор UI](ui-advanced-features.md).
