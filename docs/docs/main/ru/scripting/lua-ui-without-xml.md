# UI без XML и объявления Lua-класса

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.5.0

Доступные методы:

* `CreateScriptWindow`
* `GetCurrentDialogHolder`
* `TopInputReceiver`
* `AddDialogToRender`
* `RemoveDialogToRender`
* `ShowDialog`
* `HideDialog`
* `InitNoXmlStatic`
* `CUIStatic`
* `SetAutoDelete`
* `AttachChild`
* `InitTexture`
* `SetStretchTexture`
* `SetTextureColor`
* `TextureOn`
* `TextureOff`
* `SetFontByName`
* `SetFont`
* `HighlightText`
* `SetHighlightColor`
* `TextOn`
* `TextOff`

#### Описание методов:

### Создание скриптового окна

Создаёт `CUIScriptWnd` без Lua-класса. Для отображения вызовите
`ShowDialog(false)` или `Show(true)` и `AddDialogToRender`.
Сохраняйте ссылку на окно, пока оно используется.

```lua
--// Создать скриптовое окно без объявления Lua-класса. Само в отрисовку не добавляется.
CreateScriptWindow()
retval: (CUIScriptWnd) - новое окно; сохраняйте Lua-ссылку, пока оно используется
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

### Отрисовка, ввод и время жизни

`AddDialogToRender` добавляет объект в отрисовку и обновление без захвата ввода.
Для интерактивного окна используйте `ShowDialog`, для закрытия — `HideDialog`.
Перед освобождением ссылки снимите самостоятельный объект с отрисовки.

При выгрузке актора или HUD окна снимаются с отрисовки, скрываются и отключаются.
Сохранённые Lua-ссылки остаются действительными, в том числе при `keep_lua`.
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
  control (CUIWindow) - ранее добавленный объект; вызовите до освобождения Lua-ссылки
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
retval: (CUIStatic) - новый статик; сохраняйте Lua-ссылку, пока он используется

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

### Примеры использований:

#### Окно без класса с контролами без XML

Первый контрол меняет текст при одиночном и двойном клике.
`Close` скрывает окно. `close_window()` закрывает окно и освобождает ссылку.

```lua
function close_window()
    if not ui_state then
        return
    end

    ui_state.window:HideDialog()
    ui_state = nil
end

function show_window()
    close_window()
    assert(GetCurrentDialogHolder(), "Интерфейс недоступен")

    local window = CreateScriptWindow()
    window:SetWndPos(vector2():set(0, 0))
    window:SetWndSize(vector2():set(1024, 768))
    window:SetStopAnyMove(false)

    local xml = CScriptXmlInit()
    local control = xml:InitNoXmlStatic("control", 30, 30, 350, 45, window)
    control:SetFont(GetFontLetterica16Russian())
    control:SetText("Click")
    control:SetTextColor(GetARGB(255, 255, 255, 255))

    control:AddSelfCallback(ui_events.MOUSE_LEFT_CLICK, function(control)
        control:SetText("Left click")
    end)

    control:AddSelfCallback(ui_events.MOUSE_DOUBLE_CLICK, function(control)
        control:SetText("Double click")
    end)

    local close_control = xml:InitNoXmlStatic("close", 30, 100, 350, 45, window)
    close_control:SetFont(GetFontLetterica16Russian())
    close_control:SetText("Close")
    close_control:SetTextColor(GetARGB(255, 255, 255, 255))
    close_control:AddSelfCallback(ui_events.MOUSE_LEFT_CLICK, function(control)
        control:GetParent():HideDialog()
    end)

    ui_state = { window = window }
    window:ShowDialog(false)
end

show_window()
```

#### Закрытие и освобождение

```lua
close_window()
```

#### Обновление окна

```lua
ui_state.window:AddSelfCallback(ui_events.WINDOW_UPDATE, function(window)
    -- Обновление окна.
end)
```
