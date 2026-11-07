# Общие методы UI

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.5.0

* `Show`
* `IsShown`
* `IsCursorOverWindow`
* `ToFront`
* `ToBack`
* `SetTreeHeading`
* `SetSmothRotate`
* `EnableHeading`

#### Описание методов:

### Видимость и наведение

`IsShown` проверяет собственную видимость.
`IsCursorOverWindow(true)` учитывает вложенность и перекрытие;
без аргумента проверяется попадание в прямоугольник при обновлении.
Обе проверки наведения и события входа/выхода учитывают поворот контрола
и его родителей. Прозрачные пиксели текстуры не учитываются.

```lua
--// Показать или скрыть контрол.
control:Show(visible)
args:
  visible (boolean) - true показывает и включает контрол; false скрывает и отключает
retval: (void)

--// Получить собственный флаг видимости.
control:IsShown()
retval: (boolean) - собственная видимость контрола; видимость родителей не проверяется

--// Проверить наведение с необязательной проверкой перекрытия.
control:IsCursorOverWindow(precise)
args:
  precise (boolean, optional) - true проверяет текущие координаты, вложенность, перекрытие, захват мыши и активный диалог; false или отсутствие аргумента возвращает наведение, рассчитанное при Update с учётом поворота
retval: (boolean) - true, если условие наведения выполнено; дочерний контрол считается частью родителя
```

### Порядок отображения

Меняет порядок отображения и приоритет мыши среди детей одного родителя.
Вложенность, координаты и компоновка сохраняются.

```lua
--// Поднять контрол над соседями текущего родителя.
control:ToFront()
retval: (boolean) - true при допустимом общем родителе; false для корневого окна или отсутствия в списке детей

--// Опустить контрол под соседей текущего родителя.
control:ToBack()
retval: (boolean) - true при допустимом общем родителе; false для корневого окна или отсутствия в списке детей
```

### Каскадный поворот

`SetTreeHeading` поворачивает весь контрол с текстом и дочерними элементами.
Локальные позиции и углы сохраняются. Повороты вложенных веток складываются.
`EnableHeading` — общий переключатель локального и каскадного поворота:
отключение сохраняет параметры обоих способов.
Области отсечения контейнеров остаются прямоугольными.

```lua
--// Задать целевой угол ветки, точку вращения и смещение одним вызовом.
control:SetTreeHeading(angle, pivot, offset)
args:
  angle (number) - угол в радианах; положительный против часовой стрелки
  pivot (vector2) - точка вращения от левого верхнего угла контрола в единицах UI
  offset (vector2) - дополнительное смещение всей ветки; x вправо, y вниз
retval: (void)

--// Включить или отключить оба способа поворота, сохранив настройки.
control:EnableHeading(enabled)
args:
  enabled (boolean) - true включает повороты; false отключает локальный и каскадный поворот этого контрола
retval: (void)
```

### Плавность поворота

`SetSmothRotate` настраивает оба способа поворота этого контрола.
По умолчанию `factor = 1.0`: угол меняется мгновенно.
Скорость перехода учитывает межкадровую дельту и не зависит от FPS.

При смене целевого угла переход продолжается от текущего угла.
Полные обороты сохраняются. Сглаживается только угол; точка вращения
и смещение меняются сразу. Переход выполняется при обычном обновлении UI.
`EnableHeading(false)` приостанавливает движение до включения поворота.

```lua
--// Задать общий режим сглаживания локального и каскадного поворота.
control:SetSmothRotate(enabled, factor)
args:
  enabled (boolean) - true включает сглаживание; false сразу устанавливает целевые углы
  factor (number) - резкость от 0 до 1; 0 удерживает текущий угол, 1 меняет его мгновенно; чем ближе к 1, тем быстрее переход
retval: (void)
```

### Примеры использований:

#### Видимость

```lua
control:Show(true)
local visible = control:IsShown()
```

#### Проверка наведения

```lua
local hovered = control:IsCursorOverWindow()
local precise_hovered = control:IsCursorOverWindow(true)
```

#### Порядок отображения

```lua
control:ToFront()
control:ToBack()
```

#### Поворот ветки вокруг нижней середины

```lua
control:SetWndSize(vector2():set(200, 100))
control:SetTreeHeading(
    ffx_angle_utils.degrees_to_radians(45),
    vector2():set(100, 100),
    vector2():set(0, 0)
)
control:EnableHeading(true)
```

#### Локальный угол дочернего элемента

```lua
child:SetHeading(ffx_angle_utils.degrees_to_radians(15))
child:EnableHeading(true)

control:SetWndSize(vector2():set(200, 100))
control:SetTreeHeading(
    ffx_angle_utils.degrees_to_radians(45),
    vector2():set(100, 100),
    vector2():set(0, 0)
)
control:EnableHeading(true)
```

#### Отключение обоих поворотов

```lua
control:EnableHeading(false)
```

#### Плавный поворот ветки

```lua
control:EnableHeading(true)
control:SetSmothRotate(true, 0.1)
control:SetTreeHeading(
    ffx_angle_utils.degrees_to_radians(90),
    vector2():set(100, 100),
    vector2():set(0, 0)
)
```

#### Плавный локальный поворот

```lua
control:EnableHeading(true)
control:SetSmothRotate(true, 0.1)
control:SetHeading(ffx_angle_utils.degrees_to_radians(90))
```

#### Мгновенный поворот

```lua
control:SetSmothRotate(true, 1.0)
control:SetHeading(ffx_angle_utils.degrees_to_radians(45))
```
