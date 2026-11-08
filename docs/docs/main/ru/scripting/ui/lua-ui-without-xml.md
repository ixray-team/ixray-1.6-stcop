# UI без XML и объявления класса

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.5.0

Доступные методы:

* `CreateScriptWindow`
* `GetCurrentDialogHolder`
* `TopInputReceiver`
* `GetMainMenuWindow`
* `IsMainMenuActive`
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
* `SetBackgroundColorARGB`
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

### Примеры использований:

#### Окно без класса с контролами без XML

```lua
window = CreateScriptWindow()
window:SetWndSize(vector2():set(1024, 768))

local xml = CScriptXmlInit()
local control = xml:InitNoXmlStatic("control", 30, 30, 300, 40, window)
control:SetText("Text")

window:ShowDialog(false)
```

#### Закрытие и освобождение

```lua
window:HideDialog()
window = nil
```

#### Самостоятельный статик без окна

Статик добавляется в отрисовку без захвата ввода. Ссылка сохраняется до удаления.

```lua
local holder = GetCurrentDialogHolder()
assert(holder, "Интерфейс недоступен")

local control = CUIStatic()
control:SetWndPos(vector2():set(0, 0))
control:SetWndSize(vector2():set(300, 60))
control:SetFont(GetFontLetterica16Russian())
control:SetText("Text")
control:SetTextColor(GetARGB(255, 255, 255, 255))

-- Сохраняем ссылку на статик, пока он отображается.
ui_state = { holder = holder, control = control }
holder:AddDialogToRender(control)
```

#### Удаление самостоятельного статика

```lua
ui_state.holder:RemoveDialogToRender(ui_state.control)
ui_state = nil
```

#### Обновление окна

```lua
window:AddSelfCallback(ui_events.WINDOW_UPDATE, function(window)
    -- Обновление окна.
end)
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
