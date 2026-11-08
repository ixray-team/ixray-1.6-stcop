# Окна, контролы и события UI

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.5.0

* `CreateScriptWindow`
* `GetCurrentDialogHolder`
* `TopInputReceiver`
* `GetMainMenuWindow`
* `IsMainMenuActive`
* `InitNoXmlStatic`
* `CUIStatic`
* `SetAutoDelete`
* `AttachChild`
* `InitTexture`
* `SetStretchTexture`
* `SetTextureColor`
* `SetBackgroundColorARGB`
* `TextureOn`
* `TextureOff`
* `SetFontByName`
* `SetFont`
* `HighlightText`
* `SetHighlightColor`
* `TextOn`
* `TextOff`
* `AddSelfCallback`
* `Register`
* `AddCallback`
* `AddDialogToRender`
* `RemoveDialogToRender`
* `ShowDialog`
* `HideDialog`
* `EnableHeading`
* `Heading`
* `SetHeading`
* `GetHeading`
* `SetHeadingPivot`
* `ResetHeadingPivot`
* `SetConstHeading`
* `GetConstHeading`

#### Описание методов:

### Создание скриптового окна

Создаёт `CUIScriptWnd` без класса. Для отображения вызовите
`ShowDialog(false)` или `Show(true)` и `AddDialogToRender`.
Сохраняйте ссылку на окно, пока оно используется.

```lua
--// Создать скриптовое окно без объявления класса. Само в отрисовку не добавляется.
CreateScriptWindow()
retval: (CUIScriptWnd) - новое окно; сохраняйте ссылку, пока оно используется
```

### Получение держателя UI

Возвращает держатель активного главного меню, иначе доступного HUD.
`TopInputReceiver` возвращает текущее окно ввода.

```lua
--// Получить текущий держатель UI без открытия диалога.
GetCurrentDialogHolder()
retval: (CDialogHolder | nil) - держатель активного меню, иначе доступного HUD; nil при отсутствии UI

--// Получить верхнее окно, принимающее ввод.
holder:TopInputReceiver()
retval: (CUIDialogWnd | nil) - текущий получатель ввода; nil, если стек пуст
```

### Доступ к главному меню

Возвращает существующее окно меню без изменения его скрипта.
Активность меню проверяется отдельно через `IsMainMenuActive`.
После пересоздания меню получите ссылку заново.

```lua
--// Получить существующее корневое скриптовое окно главного меню.
GetMainMenuWindow()
retval: (CUIScriptWnd | nil) - существующее окно; nil, если подходящего окна нет

--// Проверить активность главного меню.
IsMainMenuActive()
retval: (boolean) - true, если главное меню активно
```

### Создание и прикрепление статиков

`InitNoXmlStatic` вызывается у `CScriptXmlInit` без `ParseFile`.
Статик прикрепляется к родителю с автоматическим удалением; повторное
прикрепление не требуется. При ручном создании через `CUIStatic()`
установите `SetAutoDelete(true)` перед `AttachChild`.

```lua
--// Создать статик и передать владение родителю без чтения XML.
xml:InitNoXmlStatic(name, x, y, width, height, parent)
args:
  name (string) - имя контрола; для AddSelfCallback регистрация этого имени не нужна
  x (number) - позиция по горизонтали относительно родителя в единицах UI
  y (number) - позиция по вертикали относительно родителя в единицах UI
  width (number) - ширина в единицах UI
  height (number) - высота в единицах UI
  parent (CUIWindow) - обязательный родитель; прикрепление и автоматическое удаление задаются методом
retval: (CUIStatic | nil) - прикреплённый статик с системным шрифтом; nil при parent == nil

--// Создать самостоятельный статик без XML и без родителя.
CUIStatic()
retval: (CUIStatic) - новый статик; сохраняйте ссылку, пока он используется

--// Задать автоматическое удаление контрола родителем.
control:SetAutoDelete(enabled)
args:
  enabled (boolean) - true передаёт удаление родителю при прикреплении; false оставляет владение у вызывающего кода
retval: (void)

--// Прикрепить существующий контрол к родителю.
parent:AttachChild(control)
args:
  control (CUIWindow) - дочерний контрол; при ручном создании заранее установите SetAutoDelete(true)
retval: (void)
```

### Цвет статика

Задаёт заливку `CUIStatic` без текстуры в 2D и 3D UI.
Метод вызывается одинаково; режим определяется местом отрисовки окна.
По умолчанию цвет полностью прозрачный: ARGB `(0, 0, 0, 0)`.

Включённая текстура имеет приоритет перед цветом. Если текстуры нет
или вызван `TextureOff()`, рисуется заливка. Повороты и сглаживание
применяются к заливке так же, как к изображению.

```lua
--// Задать цвет заливки без текстуры.
control:SetBackgroundColorARGB(a, r, g, b)
args:
  a (number) - прозрачность от 0 до 255; 0 полностью прозрачно, 255 непрозрачно
  r (number) - красный компонент от 0 до 255
  g (number) - зелёный компонент от 0 до 255
  b (number) - синий компонент от 0 до 255
retval: (void)
```

### Текстура статика

Задаёт текстуру, растягивание, цвет и видимость изображения.
`GetARGB(a, r, g, b)` принимает компоненты от 0 до 255.

```lua
--// Задать текстуру контрола.
control:InitTexture(name)
args:
  name (string) - имя существующей текстуры или элемента UI-атласа
retval: (void)

--// Управлять растягиванием текстуры под размер контрола.
control:SetStretchTexture(enabled)
args:
  enabled (boolean) - true растягивает текстуру; false использует размер выбранного участка
retval: (void)

--// Задать цвет и прозрачность текстуры.
control:SetTextureColor(color)
args:
  color (number) - ARGB, например GetARGB(alpha, red, green, blue); компоненты от 0 до 255
retval: (void)

--// Включить отрисовку текстуры.
control:TextureOn()
retval: (void)

--// Отключить отрисовку текстуры.
control:TextureOff()
retval: (void)
```

### Текст и шрифт

`SetFontByName` принимает имя секции `fonts.ltx`, `SetFont` — объект `CGameFont`.
Подсветка меняет цвет текста при наведении. `TextOn` и `TextOff` управляют текстом.

```lua
--// Выбрать шрифт по секции конфигурации шрифтов.
control:SetFontByName(name)
args:
  name (string) - существующая секция fonts.ltx; это не путь к файлу шрифта
retval: (void)

--// Установить готовый объект шрифта.
control:SetFont(font)
args:
  font (CGameFont) - объект, например результат GetFontLetterica16Russian(); не имя секции
retval: (void)

--// Включить подсветку текста при наведении.
control:HighlightText(enabled)
args:
  enabled (boolean) - true включает подсветку; false отключает
retval: (void)

--// Задать цвет подсветки текста.
control:SetHighlightColor(color)
args:
  color (number) - упакованный ARGB, например GetARGB(alpha, red, green, blue); компоненты от 0 до 255
retval: (void)

--// Включить отрисовку текста.
control:TextOn()
retval: (void)

--// Отключить отрисовку текста.
control:TextOff()
retval: (void)
```

### Подписки и события

`AddSelfCallback` не требует имени и `Register`.
Без объекта обработчик получает контрол-источник первым аргументом.
С объектом вызов имеет вид `handler(object, control)`: объект становится `self`,
контрол-источник передаётся вторым аргументом. `nil` равносилен отсутствию объекта.
Подписки удаляются при уничтожении контрола. Для именованного `AddCallback`
сначала вызовите `Register(control, name)`.

Клики и колесо требуют ввода через `ShowDialog`. `WINDOW_UPDATE` работает
только при явной подписке и обычном обновлении окна, без дополнительных тиков.

При двойном клике первое нажатие вызывает `MOUSE_LEFT_CLICK`, второе —
`MOUSE_DOUBLE_CLICK`. Второе событие `MOUSE_LEFT_CLICK` не отправляется.
Имена `on_mouse_*` сохранены как алиасы соответствующих `MOUSE_*`.

События:

* `ui_events.MOUSE_LEFT_CLICK` — Нажатие ЛКМ внутри контрола
* `ui_events.MOUSE_DOUBLE_CLICK` — Двойной клик ЛКМ внутри контрола
* `ui_events.MOUSE_RIGHT_CLICK` — Нажатие ПКМ внутри контрола
* `ui_events.MOUSE_ENTER` — Курсор вошёл в прямоугольник контрола
* `ui_events.MOUSE_LEAVE` — Курсор покинул прямоугольник после входа
* `ui_events.MOUSE_DRAG` — Движение внутри контрола с зажатой ЛКМ или ПКМ
* `ui_events.MOUSE_MOVE` — Движение внутри контрола, в том числе при зажатой кнопке
* `ui_events.MOUSE_WHEEL_UP` — Прокрутка колеса вверх внутри контрола
* `ui_events.MOUSE_WHEEL_DOWN` — Прокрутка колеса вниз внутри контрола
* `ui_events.WINDOW_FOCUS_RECEIVED` — Экспорт существующего события входа курсора; то же значение, что `MOUSE_ENTER`
* `ui_events.WINDOW_FOCUS_LOST` — Экспорт существующего события выхода курсора; то же значение, что `MOUSE_LEAVE`
* `ui_events.WINDOW_UPDATE` — Вызов подписанного обработчика при существующем обновлении окна или контрола

Классы, которым доступен `AddSelfCallback`:

* `CUIWindow`
* `CUIDialogWnd`
* `CUIScriptWnd`
* `CUIStatic`
* `CUIArrow`
* `CUIFrameWindow`
* `CUIFrameLineWnd`
* `CUIStackPanel`
* `CUIGamepadLegend`
* `UIHint`
* `CUIButton`
* `CUI3tButton`
* `CUICheckButton`
* `CUITabButton`
* `CUICustomSpin`
* `CUISpinNum`
* `CUISpinFlt`
* `CUISpinText`
* `CUICustomEdit`
* `CUIEditBox`
* `CUIScrollView`
* `CUIListBox`
* `CUIListBoxItem`
* `CUIListBoxItemMsgChain`
* `CUIListWnd`
* `CUIListItem`
* `CUIListItemEx`
* `CUITrackBar`
* `CUIArrowStepper`
* `CUIScrollBar`
* `CUITabControl`
* `CUIComboBox`
* `CUIPropertiesBox`
* `CUIProgressBar`
* `CUIMessageBox`
* `CUIMessageBoxEx`
* `CUIActorMenu`
* `CUIPdaWnd`
* `CUIMainIngameWnd`
* `CUIMotionIcon`
* `CUIHudStatesWnd`
* `CServerList`
* `CUIMapList`
* `CUIMMShniaga`

```lua
--// Подписаться на собственное событие без имени и Register.
control:AddSelfCallback(event, handler)
args:
  event (number) - значение из ui_events; доступность события зависит от класса контрола
  handler (function) - обработчик function(control); получает контрол-источник; результат не используется
retval: (void)

--// Подписаться на собственное событие с объектом обработчика.
control:AddSelfCallback(event, handler, object)
args:
  event (number) - значение события из ui_events
  handler (function) - обработчик function(self, control); результат не используется
  object (any, optional) - объект обработчика; передаётся первым аргументом; nil равносилен отсутствию аргумента
retval: (void)

--// Задать получателя именованных событий контрола.
window:Register(control, name)
args:
  control (CUIWindow) - регистрируемый контрол
  name (string) - имя для поиска обработчика через AddCallback; при вызове Register(control) сохраняется прежнее имя
retval: (void)

--// Подписаться по имени контрола без обязательного объекта обработчика.
window:AddCallback(name, event, handler)
args:
  name (string) - имя контрола-источника события
  event (number) - значение события из ui_events
  handler (function) - обработчик; эта перегрузка не передаёт объект self
retval: (void)

--// Прежняя подписка по имени с объектом обработчика.
window:AddCallback(name, event, handler, object)
args:
  name (string) - имя контрола-источника события
  event (number) - значение события из ui_events
  handler (function) - обработчик; object передаётся ему первым аргументом
  object (any) - объект обработчика, обычно self
retval: (void)
```

### Отрисовка, ввод и время жизни

`AddDialogToRender` добавляет объект в отрисовку и обновление без захвата ввода.
Для интерактивного окна используйте `ShowDialog`, для закрытия — `HideDialog`.
Перед освобождением ссылки снимите самостоятельный объект с отрисовки.

При выгрузке актора или HUD окна снимаются с отрисовки, скрываются и отключаются.
Сохранённые ссылки остаются действительными, в том числе при `keep_lua`.
Для повторного отображения добавьте окно в новый HUD.

```lua
--// Добавить существующий контрол или окно в отрисовку и обновление.
holder:AddDialogToRender(control)
args:
  control (CUIWindow) - объект для отрисовки; владение не передаётся и ввод не захватывается
retval: (void)

--// Снять объект с отрисовки и обновления.
holder:RemoveDialogToRender(control)
args:
  control (CUIWindow) - ранее добавленный объект; вызовите до освобождения ссылки
retval: (void)

--// Открыть интерактивный диалог.
window:ShowDialog(hideIndicators)
args:
  hideIndicators (boolean) - true скрывает HUD-индикаторы на время диалога; false сохраняет их
retval: (void)

--// Закрыть интерактивный диалог и вернуть ввод предыдущему получателю.
window:HideDialog()
retval: (void)
```

### Вращение текстуры

`SetHeading` поворачивает текстуру или заливку, не меняя текст и дочерние элементы.
Для поворота всей ветки используется `SetTreeHeading`.
Наведение и события мыши учитывают оба вида поворота.
Угол задаётся в радианах. `SetSmothRotate(enabled, factor)` задаёт сглаживание
для обоих способов поворота. По умолчанию `factor = 1.0`: мгновенная смена угла.

`point` задаёт точку вращения в единицах UI от левого верхнего угла текстуры:
X вправо, Y вниз. Для размера 200 × 100 центр — `(100, 50)`,
левый верхний угол — `(0, 0)`, правый нижний — `(200, 100)`.
`offset` дополнительно переносит текстуру и точку вращения в экранных осях.
Для обычного вращения передавайте `fixedLT = false`.

`ResetHeadingPivot` возвращает автоматический центр, но сохраняет угол и `offset`.
Явно заданная точка не пересчитывается при изменении размера.

```lua
--// Включить или выключить локальный и каскадный поворот, сохранив параметры.
control:EnableHeading(enabled)
args:
  enabled (boolean) - true включает оба способа поворота; false отключает без сброса настроек
retval: (void)

--// Проверить, включён ли режим вращения текстуры.
control:Heading()
retval: (boolean) - true, если вращение включено

--// Задать угол; вращение отдельно включается EnableHeading(true).
control:SetHeading(angle)
args:
  angle (number) - угол в радианах; math.pi / 2 = 90 градусов; положительный угол против часовой стрелки, отрицательный по часовой
retval: (void)

--// Получить текущий угол текстуры, включая промежуточный при сглаживании.
control:GetHeading()
retval: (number) - текущий угол в радианах

--// Задать точку вращения и перенос текстуры; все аргументы обязательны.
control:SetHeadingPivot(point, offset, fixedLT)
args:
  point (vector2) - точка относительно левого верхнего угла отображаемой текстуры в единицах UI; x вправо, y вниз; не UV-координаты
  offset (vector2) - дополнительный перенос всей текстуры после вращения в единицах UI; x вправо, y вниз; сам вектор не вращается
  fixedLT (boolean) - false для обычного вращения; true меняет местами ширину и высоту растягиваемой текстуры при включённом вращении
retval: (void)

--// Вернуть автоматический центр и выключить fixedLT; угол и offset сохраняются.
control:ResetHeadingPivot()
retval: (void)

--// Задать флаг постоянного направления для кода маркеров карты.
control:SetConstHeading(enabled)
args:
  enabled (boolean) - true запрещает автоматический пересчёт направления маркером карты; false снимает флаг; не отключает анимации и SetHeading
retval: (void)

--// Получить флаг постоянного направления.
control:GetConstHeading()
retval: (boolean) - установлен ли флаг постоянного направления
```

### Примеры использований:

#### Создание окна

```lua
window = CreateScriptWindow()
window:SetWndSize(vector2():set(1024, 768))
window:ShowDialog(false)
```

#### Контрол без XML

```lua
local xml = CScriptXmlInit()
local control = xml:InitNoXmlStatic("control", 30, 30, 300, 40, window)
```

#### События контрола

```lua
control:AddSelfCallback(ui_events.MOUSE_LEFT_CLICK, function(control)
    control:SetText("Click")
end)

control:AddSelfCallback(ui_events.MOUSE_DOUBLE_CLICK, function(control)
    control:SetText("Double click")
end)
```

#### Подписка на метод объекта

```lua
local object = {}

function object:on_click(control)
    self.source = control
end

control:AddSelfCallback(ui_events.MOUSE_LEFT_CLICK, object.on_click, object)
```


#### Обновление окна

```lua
window:AddSelfCallback(ui_events.WINDOW_UPDATE, function(window)
    -- Обновление окна.
end)
```

#### Вращение вокруг угла

```lua
control:EnableHeading(true)
control:SetHeadingPivot(vector2():set(0, 0), vector2():set(0, 0), false)
control:SetHeading(math.pi / 4)
```


#### Статик с заливкой без текстуры

```lua
local xml = CScriptXmlInit()
local control = xml:InitNoXmlStatic("control", 30, 30, 200, 100, window)
control:SetBackgroundColorARGB(255, 255, 0, 0)
```

#### Полупрозрачная заливка

```lua
control:SetBackgroundColorARGB(128, 255, 0, 0)
control:TextureOff()
```

#### Прозрачная заливка

```lua
control:SetBackgroundColorARGB(0, 0, 0, 0)
```
