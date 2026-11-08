# События мыши и обновления контролов

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.5.0

Доступные методы:

* `AddSelfCallback`
* `Register`
* `AddCallback`

#### Описание методов:

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

### Примеры использований:

#### Подписка на клики

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

#### Проверка наведения

```lua
local visible = control:IsShown()
local hovered = control:IsCursorOverWindow(true)
```

#### Обновление

```lua
window:AddSelfCallback(ui_events.WINDOW_UPDATE, function(window)
    -- Обновление окна.
end)
```

