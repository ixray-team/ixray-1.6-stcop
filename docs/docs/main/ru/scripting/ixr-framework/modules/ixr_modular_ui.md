# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.5.0

## Модуль интерфейсов IXR MODULAR UI

Модуль предоставляет именованные UI, управление их отображением и библиотеку создания движковых контролов. Поддерживаются создание кодом и загрузка параметров из XML. Пользовательские классы на основе `CUIWindow` также можно регистрировать в UI.

Имя UI уникально в пределах модуля, включая обращения из разных скриптов. Имена контролов уникальны внутри выбранного UI. Состояние интерфейсов хранится в памяти и не записывается в сохранение игры.

### Типы результатов и правила вызова

| Результат | Значение |
| --- | --- |
| `table` из `GetOrCreateUI()` или `SetUI()` | Таблица методов конкретного UI. Далее обозначается `ui`. |
| `table` из `GetUIControllsLibrary()` | Библиотека `make_*` и `xml.make_*`. Далее обозначается `library`. |
| `table` из `ui.xml(path)` | XML-фабрика, связанная с выбранным UI и файлом. Далее обозначается `factory`. |
| `CUIWindow` и его наследники | Исходный движковый контрол. Тип и доступные методы сохраняются. |
| `false` | Операция не выполнена: неверные аргументы, удалённый UI или недоступный способ показа. |
| `nil` из `ui.get_controll()` | Контрол с указанным именем не зарегистрирован. |

Успешные методы изменения UI возвращают ту же таблицу `ui`, поэтому поддерживают цепочки. `remove()` также возвращает эту таблицу, но после удаления она больше не пригодна для работы. Методы чтения возвращают данные. Методы создания контролов, включая `factory.set_controll()`, возвращают контрол, а не `ui`.

Методы `ui`, `library` и `factory` вызываются через точку. Штатные методы движкового контрола вызываются через двоеточие. Проверки модуля записывают ошибки через `IXRLog`. Ошибки самого движка, в том числе ошибки загрузки XML, не перехватываются.

## Описание методов

### Глобальные функции

```lua
--// Получить существующий UI или создать пустой.
GetOrCreateUI(key_name)
args:
  key_name (string)(required) - непустое имя UI. Имя уникально в пределах модуля.
retval: (table|false) - объект UI; false, если имя неверно или модуль не подключён.
```

```lua
--// Получить библиотеку создания контролов.
GetUIControllsLibrary()
args:
  (none)
retval: (table|false) - библиотека с методами make_* и разделом xml; false, если модуль не подключён.
```

```lua
--// Проверить наличие UI без его создания.
IsUiExists(key_name)
args:
  key_name (string)(required) - имя UI.
retval: (bool) - существует ли UI. При отсутствии модуля возвращает false.

--// Создать пустой UI вместо прежнего UI с тем же именем.
SetUI(key_name, x, y, width, height)
args:
  key_name (string)(required) - непустое имя UI.
  x (number)(optional) - положение по горизонтали. nil сохраняет начальную позицию.
  y (number)(optional) - положение по вертикали. nil сохраняет начальную позицию.
  width (number)(optional) - ширина в единицах UI. nil сохраняет начальную ширину 1024.
  height (number)(optional) - высота в единицах UI. nil сохраняет начальную высоту 768.
retval: (table|false) - новый UI; false при неверном имени или отсутствии модуля.
```

`GetOrCreateUI()` возвращает существующий UI без изменения его контролов. Новый UI скрыт, имеет начальный размер 1024 × 768 и начальную позицию конструктора движка.

`SetUI()` удаляет прежний UI с тем же именем вместе с контролами и вложенными UI. Затем создаёт новый UI и применяет переданные координаты и размеры. Для пропуска отдельного аргумента используется `nil`.

### Методы UI

```lua
--// Добавить готовый контрол под своим именем.
ui.set_controll(name, control)
args:
  name (string)(required) - имя контрола внутри этого UI.
  control (CUIWindow)(required) - готовый движковый контрол или объект своего класса на основе CUIWindow.
retval: (table|false) - тот же ui; false при ошибке.
```

```lua
--// Добавить контрол без отдельного имени.
ui.set_controll(control)
args:
  control (CUIWindow)(required) - готовый контрол. Если у него есть движковое имя, используется оно. Иначе контрол хранится без строкового имени.
retval: (table|false) - тот же ui; false при ошибке.
```

```lua
--// Получить зарегистрированный контрол для дальнейшей настройки.
ui.get_controll(name)
args:
  name (string)(required) - имя, указанное при регистрации.
retval: (CUIWindow|nil|false) - исходный контрол с методами его класса; nil, если имя не найдено; false, если UI уже удалён.
```

```lua
--// Показать UI фоном или снять его с показа.
ui.draw(holder_ref, visible)
args:
  holder_ref (CDialogHolder)(optional) - место показа. Например, get_hud(). nil выбирает держатель активного главного меню, иначе доступного HUD.
  visible (bool)(optional) - true показывает UI, false скрывает. По умолчанию true. При false ссылка holder_ref не используется.
retval: (table|false) - тот же ui; false, если UI удалён или подходящего держателя нет.
```

```lua
--// Открыть UI как диалог с управлением и курсором.
ui.draw_dialog(holder_ref, hide_indicators)
args:
  holder_ref (CDialogHolder)(optional) - держатель диалога. nil выбирает текущий держатель, как в draw().
  hide_indicators (bool)(optional) - true временно скрывает HUD-индикаторы, false оставляет их. По умолчанию true.
retval: (table|false) - тот же ui; false, если UI удалён или подходящего держателя нет.
```

```lua
--// Показать UI внутри существующего окна.
ui.draw_parent(wnd_ref)
args:
  wnd_ref (CUIWindow)(required) - игровое окно или get_window() другого UI. Само окно не должно находиться внутри присоединяемого UI.
retval: (table|false) - тот же ui; false, если ссылка отсутствует, возникла круговая вложенность или UI удалён.
```

```lua
--// Изменить видимость всего UI.
ui.show(visible)
args:
  visible (bool)(required) - true показывает UI, false скрывает.
retval: (table|false) - тот же ui; false, если UI удалён или его сейчас нельзя показать.
```

```lua
--// Проверить собственную видимость UI.
ui.is_visible()
args:
  (none)
retval: (bool) - собственный флаг видимости; для диалога учитывается и его окно показа. false также возвращается для удалённого UI.
```

```lua
--// Получить размер окна UI.
ui.get_size()
args:
  (none)
retval: (vector2|false) - x содержит ширину, y — высоту; false, если UI удалён.
```

```lua
--// Задать размер окна UI.
ui.set_size(size)
args:
  size (vector2)(required) - ширина в x и высота в y, в единицах UI.
retval: (table|false) - тот же ui; false, если UI удалён.
```

```lua
--// Получить положение окна UI.
ui.get_position()
args:
  (none)
retval: (vector2|false) - x и y относительно родителя; false, если UI удалён.
```

```lua
--// Задать положение окна UI.
ui.set_position(position)
args:
  position (vector2)(required) - x вправо, y вниз, относительно родителя. Значения заданы в единицах UI.
retval: (table|false) - тот же ui; false, если UI удалён.
```

```lua
--// Поднять ветку UI над соседями.
ui.push_to_forward()
args:
  (none)
retval: (table|false) - тот же ui; false, если UI удалён.
```

```lua
--// Опустить ветку UI под соседей.
ui.push_to_back()
args:
  (none)
retval: (table|false) - тот же ui; false, если UI удалён.
```

```lua
--// Изменить прозрачность самого окна UI.
ui.SetOpacity(value)
args:
  value (number)(required) - от 0 (прозрачно) до 1 (непрозрачно).
retval: (table|false) - та же таблица методов UI; false, если UI удалён.

--// Изменить прозрачность UI вместе со всеми дочерними контролами.
ui.SetOpacityTree(value)
args:
  value (number)(required) - от 0 до 1. Вложенные значения прозрачности перемножаются.
retval: (table|false) - та же таблица методов UI; false, если UI удалён.

--// Повернуть текстуру или заливку корневого контрола UI. Текст и дочерние контролы не поворачиваются.
ui.SetRotate(angle)
args:
  angle (number)(required) - угол в радианах. Например, math.rad(30).
retval: (table|false) - та же таблица методов UI; false, если UI удалён.

--// Повернуть UI вместе с дочерними контролами.
ui.SetRotateTree(angle, pivot, offset)
args:
  angle (number)(required) - угол в радианах.
  pivot (vector2)(required) - точка вращения от левого верхнего угла UI.
  offset (vector2)(required) - дополнительное смещение. Для отсутствия смещения задайте vector2():set(0, 0).
retval: (table|false) - та же таблица методов UI; false, если UI удалён.

--// Задать рамку и скругление окна UI.
ui.SetBorder(thickness, radius, color)
args:
  thickness (number)(required) - толщина рамки в единицах UI. Ноль скрывает линию рамки.
  radius (number)(required) - радиус скругления в единицах UI. Ноль задаёт прямые углы.
  color (number)(required) - цвет рамки GetARGB(a, r, g, b).
retval: (table|false) - та же таблица методов UI; false, если UI удалён.

--// Получить исходное движковое окно для любой дальнейшей настройки.
ui.GetReference()
args:
  (none)
retval: (CUIStatic|false) - корневое окно UI или false, если UI удалён.
```

```lua
--// Получить корневое движковое окно UI.
ui.get_window()
args:
  (none)
retval: (CUIStatic|false) - корневой контрол UI, доступный также через GetReference(); false, если UI удалён.
```

```lua
--// Полностью выгрузить UI.
ui.remove()
args:
  (none)
retval: (table|false) - прежний объект ui после успешного удаления; false, если он уже был удалён.
```

### Условия выполнения методов UI

* `set_controll(name, control)` регистрирует готовый контрол или ветку контролов. Если контрол уже находится внутри этого UI, вложенность сохраняется. Для поиска дочернего контрола по имени нужна его отдельная регистрация.
* Повторная регистрация того же контрола под тем же именем допустима. Другой контрол с занятым именем не заменяет существующий. Для анонимной регистрации используется `set_controll(control)`.
* `draw()` добавляет UI в обновление и отрисовку без захвата ввода. `draw_dialog()` открывает диалог с вводом. `draw_parent()` встраивает UI в переданное окно и не меняет остальные элементы этого окна.
* `show(true)` использует прежний способ показа. Если способ ещё не выбран, используется фоновый показ. Скрытый самостоятельный UI повторно открывается в текущем держателе; для конкретного держателя нужен явный вызов `draw(holder_ref)` или `draw_dialog(holder_ref)`.
* `is_visible()` проверяет собственную видимость UI и его диалоговой оболочки. Видимость остальных родительских окон не проверяется.
* `push_to_forward()` и `push_to_back()` меняют порядок веток внутри окон. Порядок самостоятельных диалогов не изменяется. При отсутствии эффекта метод всё равно возвращает `ui`.
* `SetOpacity()` влияет на оформление корневого контрола. `SetOpacityTree()` влияет также на его потомков. Множители вложенных веток перемножаются.
* `SetRotate()` задаёт локальный угол фона. `SetRotateTree()` поворачивает всю ветку. Углы задаются в радианах, координаты и размеры — в единицах UI.
* `SetBorder()` задаёт рамку корневого контрола. Скругление также отсекает дочернее содержимое по его контуру.
* `get_window()` и `GetReference()` возвращают один и тот же исходный `CUIStatic`. Настройка полученной ссылки меняет корневой контрол UI.
* При пересоздании игрового родителя UI сохраняется. Для подключения к новому окну нужен новый `draw_parent(wnd_ref)`.
* `remove()` удаляет контролы, вложенные UI и запись с именем UI. Все прежние ссылки на удалённые контролы становятся недействительными. Повторный `remove()` возвращает `false`.

### XML-фабрика выбранного UI

```lua
--// Получить фабрику для создания контролов из XML.
ui.xml(path)
args:
  path (string)(optional) - имя XML-файла, например ui_ixr_options.xml. При первом вызове обязательно. nil использует последний выбранный файл этого UI.
retval: (table|false) - фабрика с методом set_controll; false при неверном пути или удалённом UI.
```

```lua
--// Создать контрол из выбранного XML и сразу добавить его в UI.
factory.set_controll(name, kind, node, ...)
args:
  name (string)(optional) - имя контрола в UI. nil использует движковое имя, если оно есть; иначе контрол остаётся без строкового имени.
  kind (string)(required) - тип контрола: static, text, button, text_box, frame_window, frame_line, list_box, scroll_view, gamepad_legend, track_bar, check_box или combo_box.
  node (string)(required) - путь нужного узла внутри XML.
  ... (arguments)(optional) - оставшиеся аргументы соответствующего XML-метода после xml_name и node. Обычно parent, font. Для button: parent, callback, callback_self, font.
retval: (CUIWindow|false) - созданный контрол с методами его класса; false при ошибке проверки. Это не объект ui.
```

XML загружается по имени файла и переиспользуется из общего кеша. Каждый вызов создания возвращает отдельный контрол. Выбор другого XML-файла не создаёт новую область имён контролов: имена остаются общими для выбранного UI.

`factory.set_controll(name, kind, node, ...)` использует XML-метод выбранного типа, затем регистрирует результат в UI. При ошибке регистрации созданный контрол удаляется. Результат успешного вызова — сам контрол.

## Библиотека контролов

::: details Создание без XML: сигнатуры, аргументы и результаты

Позиционные аргументы необязательны. Пропущенные координаты и размеры сохраняют значения конструктора движка. Стандартный шрифт — результат `GetFontLetterica16Russian()`. Переданный `font` назначается после стандартного. `CGameFont` — объект шрифта, а не строковое имя его секции.

```lua
local library = GetUIControllsLibrary()

--// Создать статик: изображение, фон или группу для других контролов.
library.make_static(x, y, width, height, texture, color, parent, font)
args:
  x (number)(optional) - положение по горизонтали. nil оставляет исходное значение.
  y (number)(optional) - положение по вертикали. nil оставляет исходное значение.
  width (number)(optional) - ширина. nil оставляет исходное значение.
  height (number)(optional) - высота. nil оставляет исходное значение.
  texture (string)(optional) - имя текстуры или элемента UI-атласа. nil не назначает текстуру. Переданная текстура растягивается под размер контрола.
  color (number)(optional) - цвет текстуры GetARGB(a, r, g, b). nil оставляет исходный цвет.
  parent (CUIWindow)(optional) - родительское окно. nil создаёт отдельный контрол для последующего set_controll().
  font (CGameFont)(optional) - готовый объект шрифта. nil оставляет стандартный шрифт.
retval: (CUIStatic) - созданный контрол.
```

```lua
local library = GetUIControllsLibrary()

--// Создать надпись на основе статика.
library.make_text(text, x, y, width, height, font, color, parent)
args:
  text (string)(optional) - текст. nil не задаёт текст.
  x (number)(optional) - положение по горизонтали. nil оставляет исходное значение.
  y (number)(optional) - положение по вертикали. nil оставляет исходное значение.
  width (number)(optional) - ширина. nil оставляет исходное значение.
  height (number)(optional) - высота. nil оставляет исходное значение.
  font (CGameFont)(optional) - готовый объект шрифта. nil оставляет стандартный шрифт.
  color (number)(optional) - цвет текста GetARGB(a, r, g, b). nil оставляет исходный цвет.
  parent (CUIWindow)(optional) - родитель. nil создаёт отдельный контрол.
retval: (CUIStatic) - созданная надпись.
```

```lua
local library = GetUIControllsLibrary()

--// Создать кнопку, в том числе без надписи.
library.make_button(text, x, y, width, height, callback, callback_self, parent, font)
args:
  text (string)(optional) - надпись. nil создаёт кнопку без заданного текста.
  x (number)(optional) - положение по горизонтали. nil оставляет исходное значение.
  y (number)(optional) - положение по вертикали. nil оставляет исходное значение.
  width (number)(optional) - ширина. nil оставляет исходное значение.
  height (number)(optional) - высота. nil оставляет исходное значение.
  callback (function)(optional) - функция нажатия BUTTON_CLICKED. nil не добавляет обработчик.
  callback_self (any)(optional) - объект, который обработчик получит первым аргументом. nil означает вызов без такого объекта.
  parent (CUIWindow)(optional) - родитель кнопки. nil создаёт отдельный контрол. Этот аргумент не связан с callback_self.
  font (CGameFont)(optional) - готовый объект шрифта. nil оставляет стандартный шрифт.
retval: (CUIButton) - созданная кнопка.
```

```lua
local library = GetUIControllsLibrary()

--// Создать поле ввода текста.
library.make_text_box(text, x, y, width, height, texture, font, parent)
args:
  text (string)(optional) - начальное значение. nil не задаёт текст.
  x (number)(optional) - положение по горизонтали. nil оставляет исходное значение.
  y (number)(optional) - положение по вертикали. nil оставляет исходное значение.
  width (number)(optional) - ширина. nil оставляет исходное значение.
  height (number)(optional) - высота. nil оставляет исходное значение.
  texture (string)(optional) - текстура поля. nil не назначает текстуру.
  font (CGameFont)(optional) - готовый объект шрифта. nil оставляет стандартный шрифт.
  parent (CUIWindow)(optional) - родитель поля. nil создаёт отдельный контрол.
retval: (CUIEditBox) - созданное поле ввода.
```

:::

::: details Создание из XML: сигнатуры, аргументы и результаты

`xml_name` — имя файла для загрузки через общий XML-кеш. Ручной парсер не требуется. Настройки берутся из узла `node`. Необязательный `font` заменяет шрифт из XML; при `nil` шрифт узла сохраняется. При переданном `parent` контрол присоединяется к нему движком, в остальных случаях создаётся отдельно.

```lua
local library = GetUIControllsLibrary()

--// Создать статик из XML.
library.xml.make_static(xml_name, node, parent, font)
args:
  xml_name (string)(required) - имя XML-файла, например ui_ixr_options.xml. Модуль сам загрузит файл или возьмёт парсер из своего кеша.
  node (string)(required) - путь узла с настройками этого контрола.
  parent (CUIWindow)(optional) - родительское окно. По умолчанию nil, контрол создаётся отдельно.
  font (CGameFont)(optional) - готовый объект шрифта. nil сохраняет шрифт из XML.
retval: (CUIStatic) - созданный контрол.
```

```lua
local library = GetUIControllsLibrary()

--// Создать текстовый статик из XML.
library.xml.make_text(xml_name, node, parent, font)
args:
  xml_name (string)(required) - имя XML-файла, например ui_ixr_options.xml. Модуль сам загрузит файл или возьмёт парсер из своего кеша.
  node (string)(required) - путь узла с настройками этого контрола.
  parent (CUIWindow)(optional) - родительское окно. По умолчанию nil, контрол создаётся отдельно.
  font (CGameFont)(optional) - готовый объект шрифта. nil сохраняет шрифт из XML.
retval: (CUIStatic) - созданный контрол.
```

```lua
local library = GetUIControllsLibrary()

--// Создать кнопку из XML.
library.xml.make_button(xml_name, node, parent, callback, callback_self, font)
args:
  xml_name (string)(required) - имя XML-файла, например ui_ixr_options.xml. Модуль сам загрузит файл или возьмёт парсер из своего кеша.
  node (string)(required) - путь узла с настройками этого контрола.
  parent (CUIWindow)(optional) - родительское окно. По умолчанию nil, контрол создаётся отдельно.
  callback (function)(optional) - функция нажатия BUTTON_CLICKED. nil не добавляет обработчик.
  callback_self (any)(optional) - объект для первого аргумента обработчика. nil означает вызов без объекта. Не зависит от parent.
  font (CGameFont)(optional) - готовый объект шрифта. nil сохраняет шрифт из XML.
retval: (CUI3tButton) - созданный контрол.
```

```lua
local library = GetUIControllsLibrary()

--// Создать поле ввода из XML.
library.xml.make_text_box(xml_name, node, parent, font)
args:
  xml_name (string)(required) - имя XML-файла, например ui_ixr_options.xml. Модуль сам загрузит файл или возьмёт парсер из своего кеша.
  node (string)(required) - путь узла с настройками этого контрола.
  parent (CUIWindow)(optional) - родительское окно. По умолчанию nil, контрол создаётся отдельно.
  font (CGameFont)(optional) - готовый объект шрифта. nil сохраняет шрифт из XML.
retval: (CUIEditBox) - созданный контрол.
```

```lua
local library = GetUIControllsLibrary()

--// Создать оконную рамку из XML.
library.xml.make_frame_window(xml_name, node, parent, font)
args:
  xml_name (string)(required) - имя XML-файла, например ui_ixr_options.xml. Модуль сам загрузит файл или возьмёт парсер из своего кеша.
  node (string)(required) - путь узла с настройками этого контрола.
  parent (CUIWindow)(optional) - родительское окно. По умолчанию nil, контрол создаётся отдельно.
  font (CGameFont)(optional) - готовый объект шрифта. nil сохраняет шрифт из XML.
retval: (CUIFrameWindow) - созданный контрол.
```

```lua
local library = GetUIControllsLibrary()

--// Создать полоску рамки из XML.
library.xml.make_frame_line(xml_name, node, parent, font)
args:
  xml_name (string)(required) - имя XML-файла, например ui_ixr_options.xml. Модуль сам загрузит файл или возьмёт парсер из своего кеша.
  node (string)(required) - путь узла с настройками этого контрола.
  parent (CUIWindow)(optional) - родительское окно. По умолчанию nil, контрол создаётся отдельно.
  font (CGameFont)(optional) - готовый объект шрифта. nil сохраняет шрифт из XML.
retval: (CUIFrameLineWnd) - созданный контрол.
```

```lua
local library = GetUIControllsLibrary()

--// Создать список из XML.
library.xml.make_list_box(xml_name, node, parent, font)
args:
  xml_name (string)(required) - имя XML-файла, например ui_ixr_options.xml. Модуль сам загрузит файл или возьмёт парсер из своего кеша.
  node (string)(required) - путь узла с настройками этого контрола.
  parent (CUIWindow)(optional) - родительское окно. По умолчанию nil, контрол создаётся отдельно.
  font (CGameFont)(optional) - готовый объект шрифта. nil сохраняет шрифт из XML.
retval: (CUIListBox) - созданный контрол.
```

```lua
local library = GetUIControllsLibrary()

--// Создать область прокрутки из XML.
library.xml.make_scroll_view(xml_name, node, parent, font)
args:
  xml_name (string)(required) - имя XML-файла, например ui_ixr_options.xml. Модуль сам загрузит файл или возьмёт парсер из своего кеша.
  node (string)(required) - путь узла с настройками этого контрола.
  parent (CUIWindow)(optional) - родительское окно. По умолчанию nil, контрол создаётся отдельно.
  font (CGameFont)(optional) - готовый объект шрифта. nil сохраняет шрифт из XML.
retval: (CUIScrollView) - созданный контрол.
```

```lua
local library = GetUIControllsLibrary()

--// Создать подсказки кнопок геймпада из XML.
library.xml.make_gamepad_legend(xml_name, node, parent, font)
args:
  xml_name (string)(required) - имя XML-файла, например ui_ixr_options.xml. Модуль сам загрузит файл или возьмёт парсер из своего кеша.
  node (string)(required) - путь узла с настройками этого контрола.
  parent (CUIWindow)(optional) - родительское окно. По умолчанию nil, контрол создаётся отдельно.
  font (CGameFont)(optional) - готовый объект шрифта. nil сохраняет шрифт из XML.
retval: (CUIGamepadLegend) - созданный контрол.
```

```lua
local library = GetUIControllsLibrary()

--// Создать ползунок из XML.
library.xml.make_track_bar(xml_name, node, parent, font)
args:
  xml_name (string)(required) - имя XML-файла, например ui_ixr_options.xml. Модуль сам загрузит файл или возьмёт парсер из своего кеша.
  node (string)(required) - путь узла с настройками этого контрола.
  parent (CUIWindow)(optional) - родительское окно. По умолчанию nil, контрол создаётся отдельно.
  font (CGameFont)(optional) - готовый объект шрифта. nil сохраняет шрифт из XML.
retval: (CUITrackBar) - созданный контрол.
```

```lua
local library = GetUIControllsLibrary()

--// Создать переключатель с галочкой из XML.
library.xml.make_check_box(xml_name, node, parent, font)
args:
  xml_name (string)(required) - имя XML-файла, например ui_ixr_options.xml. Модуль сам загрузит файл или возьмёт парсер из своего кеша.
  node (string)(required) - путь узла с настройками этого контрола.
  parent (CUIWindow)(optional) - родительское окно. По умолчанию nil, контрол создаётся отдельно.
  font (CGameFont)(optional) - готовый объект шрифта. nil сохраняет шрифт из XML.
retval: (CUICheckButton) - созданный контрол.
```

```lua
local library = GetUIControllsLibrary()

--// Создать выпадающий список из XML.
library.xml.make_combo_box(xml_name, node, parent, font)
args:
  xml_name (string)(required) - имя XML-файла, например ui_ixr_options.xml. Модуль сам загрузит файл или возьмёт парсер из своего кеша.
  node (string)(required) - путь узла с настройками этого контрола.
  parent (CUIWindow)(optional) - родительское окно. По умолчанию nil, контрол создаётся отдельно.
  font (CGameFont)(optional) - готовый объект шрифта. nil сохраняет шрифт из XML.
retval: (CUIComboBox) - созданный контрол.
```

:::

### Сигнатуры обработчика кнопки

| Условие | Вызов обработчика |
| --- | --- |
| `callback_self == nil` | `callback(control)` |
| `callback_self` передан | `callback(callback_self, control)` |

Подписка выполняется на `ui_events.BUTTON_CLICKED`. `control` — кнопка-источник. `callback_self` не связан с родительским окном `parent`. Возвращаемое обработчиком значение не используется. Прочие события подключаются штатным `AddSelfCallback()` полученного контрола.

## Методы исходного оконного референса

`ui.GetReference()` и `ui.get_window()` возвращают `CUIStatic`, наследующий `CUIWindow`. Ниже перечислены имена методов из текущих Lua-экспортов этих двух классов. Это методы самого движкового объекта, а не дополнительные методы таблицы `ui`.

| Класс Lua | Доступные методы |
| --- | --- |
| `CUIWindow` | `AddSelfCallback`, `SetTreeHeading`, `SetSmothRotate`, `SetOpacity`, `SetTreeOpacity`, `EnableHeading`, `SetUseAspectRatioCorrection` |
| `CUIWindow` | `ToFront`, `ToBack`, `ToFrontTree`, `ToBackTree`, `AttachChild`, `DetachChild`, `SetAutoDelete` |
| `CUIWindow` | `IsAutoDelete`, `IsCursorOverWindow`, `FocusReceiveTime`, `GetAbsoluteRect`, `Init`, `SetWndRect`, `SetWndPos` |
| `CUIWindow` | `SetWndSize`, `GetWndPos`, `GetWidth`, `SetWidth`, `GetHeight`, `SetHeight`, `Enable` |
| `CUIWindow` | `IsEnabled`, `Show`, `IsShown`, `SetFont`, `GetFont`, `WindowName`, `SetWindowName` |
| `CUIWindow` | `SetPPMode`, `ResetPPMode`, `GetParent`, `FindWindow`, `FindStatic` |
| `CUIStatic` | `SetTextureColor`, `SetBlurredTextureShadow`, `SetBlurredTextureShadowOpacityRange`, `SetBlurredTextureShadowFalloff`, `SetBlurredTextureShadowLightRadius`, `SetBlurredTextureShadowHeight`, `SetBlurredTextureShadowColor` |
| `CUIStatic` | `GetBlurredTextureShadowEnabled`, `SetBackgroundColorARGB`, `SetBorder`, `SetBackgroundOpacity`, `SetBorderOpacity`, `GetTextureColor`, `AdjustHeightToText` |
| `CUIStatic` | `AdjustWidthToText`, `GetStretchTexture`, `TextControl`, `SetHint`, `SetText`, `SetTextST`, `GetText` |
| `CUIStatic` | `SetFont`, `SetFontByName`, `HighlightText`, `SetHighlightColor`, `TextureOn`, `TextureOff`, `TextOn` |
| `CUIStatic` | `TextOff`, `SetTextX`, `SetTextY`, `GetTextX`, `GetTextY`, `SetColor`, `GetColor` |
| `CUIStatic` | `SetTextColor`, `GetTextColor`, `InitTexture`, `InitTextureEx`, `ResetOriginalRect`, `SetTextureOffset`, `SetTextureRect` |
| `CUIStatic` | `GetTextureRect`, `SetOriginalRect`, `GetOriginalRect`, `SetStretchTexture`, `SetTextAlign`, `GetTextAlign`, `SetHeading` |
| `CUIStatic` | `GetHeading`, `EnableHeading`, `Heading`, `SetHeadingPivot`, `ResetHeadingPivot`, `SetConstHeading`, `GetConstHeading` |
| `CUIStatic` | `SetElipsis`, `SetEllipsis`, `SetTextAlignment`, `SetVTextAlignment`, `SetTextComplexMode`, `GetFont`, `SetTextOffset` |

`TextControl()` возвращает отдельный `CUILines`. У него экспортированы `SetFont`, `SetText`, `SetTextST`, `GetText`, `SetElipsis` и `SetTextColor`.

Методы специализированных окон сюда не относятся. В частности, `ShowDialog`, `HideDialog`, `Register` и `AddCallback` не являются методами возвращаемого `CUIStatic`. Для показа используйте методы `ui`. Ручное удаление его корня или смена владения через `SetAutoDelete` нарушает управление UI модулем; для удаления предназначен `ui.remove()`.

Сигнатуры расширений: [общие методы UI](../../ui/lua-ui-common-methods.md), [создание и оформление](../../ui/lua-ui-without-xml.md), [события](../../ui/lua-static-mouse-events.md), [прозрачность](../../ui/lua-static-opacity.md), [рамка](../../ui/lua-static-border.md).

## Примеры использования

Ниже — самостоятельные фрагменты Lua, а не полные игровые скрипты. Выполняйте выбранный фрагмент после загрузки Framework. Для вызовов показа также должен быть доступен игровой UI. Это может быть ваш обработчик события или функция открытия окна. Переменные из предыдущих примеров не требуются.

### Без XML

#### Получить UI и проверить его наличие

```lua
local ui = GetOrCreateUI("example.status")
if not ui then return end

if IsUiExists("example.status") then
    ui.show(false)
end
```

#### Создать UI заново и добавить надпись

```lua
local ui = SetUI("example.title", 100, 80, 400, 200)
local library = GetUIControllsLibrary()
if not ui or not library then return end

ui.set_controll("title", library.make_text("Заголовок", 20, 20, 250, 30))
ui.get_controll("title"):SetText("Новый заголовок")
ui.draw()
```

#### Группа контролов и регистрация без имени

```lua
local ui = SetUI("example.group")
local library = GetUIControllsLibrary()
if not ui or not library then return end

local panel = library.make_static(100, 100, 300, 150)
panel:SetBackgroundColorARGB(220, 30, 30, 30)
local title = library.make_text("Внутри группы", 20, 20, 250, 30, nil, nil, panel)
ui.set_controll("panel", panel).set_controll("title", title)
ui.set_controll(library.make_static(10, 10, 20, 20)).draw()
```

#### Анонимная шкала из 100 полосок

Строковые имена не нужны, если контролы будут удаляться или пересоздаваться целиком. `set_controll(control)` регистрирует такой контрол без придуманного имени. Если у контрола уже есть движковое имя, используется оно.

```lua
local ui = SetUI("example.health_ticks", 40, 600, 400, 24)
local library = GetUIControllsLibrary()
if not ui or not library then return end

local filled = 65
for index = 1, 100 do
    local strip = library.make_static((index - 1) * 4, 0, 2, 24)
    if index <= filled then
        strip:SetBackgroundColorARGB(255, 70, 200, 90)
    else
        strip:SetBackgroundColorARGB(120, 60, 60, 60)
    end
    ui.set_controll(strip)
end
ui.draw()
```

Здесь `filled` — число заполненных делений. Повторное выполнение фрагмента с другим значением удалит прежние полоски через `SetUI()` и создаст новую шкалу. Для полного удаления достаточно `GetOrCreateUI("example.health_ticks").remove()` после проверки результата получения UI. Пересборка нужна при смене данных, а не на каждом кадре.

#### Кнопка и поле ввода в самостоятельном диалоге

```lua
local ui = SetUI("example.input")
local library = GetUIControllsLibrary()
if not ui or not library then return end

ui.set_controll("input", library.make_text_box("Значение", 100, 100, 200, 30))
ui.set_controll("accept", library.make_button("Прочитать", 100, 150, 200, 30,
    function(button)
        button:SetText(ui.get_controll("input"):GetText())
    end
)).draw_dialog(nil, false)
```

#### Фоновый показ и видимость

```lua
local ui = GetOrCreateUI("example.visibility")
if not ui then return end

ui.draw(nil, true)
local visible = ui.is_visible()
ui.draw(nil, false)
ui.show(true).show(false)
```

#### Координаты и размеры

```lua
local ui = SetUI("example.geometry")
if not ui then return end

ui.set_position(vector2():set(100, 80)).set_size(vector2():set(400, 250))
local position = ui.get_position()
local size = ui.get_size()
IXRLog(string.format("x=%g y=%g w=%g h=%g", position.x, position.y, size.x, size.y))
```

#### Прозрачность и рамка

```lua
local ui = SetUI("example.opacity", 100, 100, 400, 250)
if not ui then return end

ui.GetReference():SetBackgroundColorARGB(255, 40, 40, 40)
ui.SetOpacity(0.9).SetOpacityTree(0.6)
    .SetBorder(2, 16, GetARGB(255, 200, 200, 200))
    .draw()
```

#### Локальный поворот и поворот всей ветки

```lua
local ui = SetUI("example.rotation", 100, 100, 400, 250)
if not ui then return end

ui.GetReference():SetBackgroundColorARGB(220, 40, 40, 40)
ui.SetRotate(math.rad(10))
ui.SetRotateTree(math.rad(30), vector2():set(200, 125), vector2():set(0, 0)).draw()
```

#### Встраивание одного UI в другой

```lua
local ui = SetUI("example.parent", 100, 100, 400, 250)
local child = SetUI("example.child", 20, 20, 200, 100)
if not ui or not child then return end

ui.GetReference():SetBackgroundColorARGB(220, 30, 30, 30)
child.GetReference():SetBackgroundColorARGB(255, 100, 70, 40)
ui.draw_dialog(nil, false)
child.draw_parent(ui.get_window()).push_to_forward().push_to_back()
```

#### Встраивание в существующее главное меню

```lua
local ui = SetUI("example.menu")
local library = GetUIControllsLibrary()
local parent = GetMainMenuWindow()
if not ui or not library or not parent then return end

ui.set_controll("title", library.make_text("Дополнение меню", 100, 100, 300, 30))
    .draw_parent(parent)
```

#### Удаление UI

```lua
local ui = SetUI("example.temporary")
if not ui then return end

ui.remove()
local exists = IsUiExists("example.temporary") -- false
local replacement = GetOrCreateUI("example.temporary") -- новый пустой UI
```

### С XML

Все фрагменты используют существующие файлы разметки контролов. Имя файла передаётся строкой; разбор и повторное использование XML выполняются модулем. Каждый фрагмент можно выполнить независимо.

#### library.xml.make_static()

```lua
local ui = SetUI("example.xml_static")
local library = GetUIControllsLibrary()
if not ui or not library then return end

local control = library.xml.make_static("ui_ixr_options.xml", "black_texture")
ui.set_controll("control", control).draw_dialog(nil, false)
```

#### library.xml.make_text()

```lua
local ui = SetUI("example.xml_text")
local library = GetUIControllsLibrary()
if not ui or not library then return end

local control = library.xml.make_text("ui_ixr_options.xml", "ixr_product_sans_16_l")
control:SetText("Надпись из XML")
control:SetWndSize(300, 30)
ui.set_controll("control", control).draw_dialog(nil, false)
```

#### library.xml.make_button()

```lua
local ui = SetUI("example.xml_button")
local library = GetUIControllsLibrary()
if not ui or not library then return end

local control = library.xml.make_button("ui_ixr_options.xml", "ixr_options_close_button", nil, function() ui.show(false) end)
ui.set_controll("control", control).draw_dialog(nil, false)
```

#### library.xml.make_text_box()

```lua
local ui = SetUI("example.xml_text_box")
local library = GetUIControllsLibrary()
if not ui or not library then return end

local control = library.xml.make_text_box("ui_ixr_options.xml", "text_box")
ui.set_controll("control", control).draw_dialog(nil, false)
```

#### library.xml.make_frame_window()

```lua
local ui = SetUI("example.xml_frame_window")
local library = GetUIControllsLibrary()
if not ui or not library then return end

local control = library.xml.make_frame_window("ui_ixr_options.xml", "frame_window")
ui.set_controll("control", control).draw_dialog(nil, false)
```

#### library.xml.make_frame_line()

```lua
local ui = SetUI("example.xml_frame_line")
local library = GetUIControllsLibrary()
if not ui or not library then return end

local control = library.xml.make_frame_line("ui_mm_mp_tabdemo.xml", "cap_demo_list")
ui.set_controll("control", control).draw_dialog(nil, false)
```

#### library.xml.make_list_box()

```lua
local ui = SetUI("example.xml_list_box")
local library = GetUIControllsLibrary()
if not ui or not library then return end

local control = library.xml.make_list_box("ui_ixr_options.xml", "addons_list")
ui.set_controll("control", control).draw_dialog(nil, false)
```

#### library.xml.make_scroll_view()

```lua
local ui = SetUI("example.xml_scroll_view")
local library = GetUIControllsLibrary()
if not ui or not library then return end

local control = library.xml.make_scroll_view("ui_ixr_options.xml", "scroll_v")
ui.set_controll("control", control).draw_dialog(nil, false)
```

#### library.xml.make_gamepad_legend()

```lua
local ui = SetUI("example.xml_gamepad_legend")
local library = GetUIControllsLibrary()
if not ui or not library then return end

local control = library.xml.make_gamepad_legend("ui_ixr_options.xml", "gamepad_legend")
ui.set_controll("control", control).draw_dialog(nil, false)
```

#### library.xml.make_track_bar()

```lua
local ui = SetUI("example.xml_track_bar")
local library = GetUIControllsLibrary()
if not ui or not library then return end

local control = library.xml.make_track_bar("ui_ixr_options.xml", "track_bar")
ui.set_controll("control", control).draw_dialog(nil, false)
```

#### library.xml.make_check_box()

```lua
local ui = SetUI("example.xml_check_box")
local library = GetUIControllsLibrary()
if not ui or not library then return end

local control = library.xml.make_check_box("ui_mm_mp_tabclient.xml", "check_empty")
ui.set_controll("control", control).draw_dialog(nil, false)
```

#### library.xml.make_combo_box()

```lua
local ui = SetUI("example.xml_combo_box")
local library = GetUIControllsLibrary()
if not ui or not library then return end

local control = library.xml.make_combo_box("ui_mm_mp_tabserver.xml", "spin_game_mode")
ui.set_controll("control", control).draw_dialog(nil, false)
```

#### Создание через XML-фабрику выбранного UI

```lua
local ui = SetUI("example.xml_factory")
if not ui then return end

local factory = ui.xml("ui_ixr_options.xml")
if not factory then return end
local title = factory.set_controll("title", "text", "ixr_product_sans_16_l")
title:SetText("Создано и зарегистрировано в UI")
title:SetWndSize(350, 30)
local same_factory = ui.xml() -- последний выбранный файл
ui.draw()
```

