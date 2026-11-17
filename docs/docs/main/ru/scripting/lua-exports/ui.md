# Окна, виджеты и HUD

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4

## Обзор

Здесь перечислены регистрации UI и HUD. Доступ к игровым окнам зависит от их создания и игрового режима.

## Синтаксис

| Вызов | Аргументы и результат |
| --- | --- |
| `window:FindWindow(name)` | Ищет текущий узел и его потомков по имени окна. Возвращает `CUIWindow` или `nil`. |
| `window:FindStatic(name)` | Выполняет тот же поиск и возвращает `CUIStatic`. Если узел отсутствует или имеет другой тип, возвращает `nil`. |
| `view:HasWindow(item)` | Проверяет, принадлежит ли окно содержимому `CUIScrollView`. |
| `view:ScrollToItem(item, offset)` | Прокручивает к переданному дочернему окну, добавляя целочисленное вертикальное смещение. |
| `bar:ShowBackground(enabled)` | Показывает или скрывает фон `CUIProgressBar`. |
| `bar:SnapProgressPos()` | Переносит отображаемое заполнение в последнюю позицию, заданную через `SetProgressPos`. |

## Примеры

### Обновить текст по имени виджета

Передайте корневое окно диалога, в котором есть `CUIStatic` с именем `status_label`.

```lua
local function set_status(window, text)
    local label = window:FindStatic("status_label")
    if not label then return false end
    label:SetText(text)
    return true
end
```

### Прокрутить к строке

Передайте инициализированный `CUIScrollView` и добавленное в него окно.

```lua
local function reveal_item(view, item)
    if not view:HasWindow(item) then return false end
    view:ScrollToItem(item, 0)
    return true
end
```

### Обновить заполнение без интерполяции

Передайте инициализированный `CUIProgressBar`. Значение должно относиться к его диапазону, например `40` для полосы `0–100`.

```lua
local function update_bar(bar, value)
    bar:ShowBackground(true)
    bar:SetProgressPos(value)
    bar:SnapProgressPos()
end
```

## Регистрации

### Глобальная область

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `abort_sleep()` | Функция | Досрочно прерывает текущий сон. | [UIGameCustom_script.cpp:213](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L213) |
| `cancel_sleep_dialog()` | Функция | Закрывает диалог сна с отменой. | [UIGameCustom_script.cpp:210](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L210) |
| `confirm_sleep()` | Функция | Подтверждает выбранную длительность и запускает сон при допустимом состоянии. | [UIGameCustom_script.cpp:211](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L211) |
| `CUIActionRepeatersManager` | Класс | Менеджер повторов действий при удержании ввода. | [UIActionRepeaters_script.cpp:22](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L22) |
| `CUIActionRepeatersOwner` | Класс | Тип владельца зарегистрированных повторами действий, например виджета. | [UIActionRepeaters_script.cpp:13](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L13) |
| `CUICaption` | Класс | Виджет для вывода именованных текстовых сообщений. | [ui_export_script.cpp:96](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L96) |
| `CUICompassBar()` | Класс | Полоса компаса на HUD. | [UIActorMenu_script.cpp:465](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L465) |
| `CUIZoneMap()` | Класс | Мини-карта текущей зоны для HUD. | [UIActorMenu_script.cpp:458](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L458) |
| `EDDListType` | Класс | Набор типов списков инвентаря для перетаскивания. | [UIActorMenu_script.cpp:379](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L379) |
| `FactionState` | Класс | Состояние группировки, используемое экраном фракций. | [FactionState.cpp:65](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L65) |
| `FitInRect(window, visible_rect, border, widescreen_x_offset)` | Функция | Размещает окно подсказки рядом с курсором внутри области, с указанным отступом и поправкой по X для широкого экрана. Возвращает `false`, если курсор вне области. | [UIWindow_script.cpp:128](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L128) |
| `force_sleep(hours)` | Функция | Запускает принудительный сон на указанное число игровых часов. | [UIGameCustom_script.cpp:212](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L212) |
| `get_sleep_phase()` | Функция | Возвращает числовую фазу текущего сна. | [UIGameCustom_script.cpp:219](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L219) |
| `get_sleep_selected_hours()` | Функция | Возвращает выбранную в диалоге длительность сна в игровых часах. | [UIGameCustom_script.cpp:216](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L216) |
| `GetCursorPosition()` | Функция | Возвращает позицию курсора. Результат `vector2`. Реализация `return GetUICursor().GetCursorPosition();`. | [UIWindow_script.cpp:126](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L126) |
| `GetFontArial14()` | Функция | Возвращает объект `CGameFont` для шрифта Arial 14. | [UIWindow_script.cpp:117](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L117) |
| `GetFontArial21()` | Функция | Возвращает объект `CGameFont` для шрифта Arial 21. | [UIWindow_script.cpp:118](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L118) |
| `GetTextureInfo(name, fallback_name)` | Функция | Возвращает запись `TEX_INFO` для ID текстуры. Если основной ID отсутствует, ищет запасной. | [UIWindow_script.cpp:137](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L137) |
| `GetTextureName(texture_id)` | Функция | Возвращает путь к файлу для зарегистрированного ID текстуры. | [UIWindow_script.cpp:135](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L135) |
| `GetTextureRect(texture_id)` | Функция | Возвращает прямоугольник области текстуры по её ID. | [UIWindow_script.cpp:136](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L136) |
| `hide_sleep_dialog()` | Функция | Скрывает диалог сна. | [UIGameCustom_script.cpp:209](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L209) |
| `is_actor_sleeping()` | Функция | Проверяет, идёт ли сон актёра. | [UIGameCustom_script.cpp:218](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L218) |
| `is_sleep_dialog_ready()` | Функция | Проверяет готовность диалога сна. | [UIGameCustom_script.cpp:214](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L214) |
| `is_sleep_dialog_shown()` | Функция | Проверяет, показан ли диалог сна. | [UIGameCustom_script.cpp:215](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L215) |
| `set_sleep_blocked(blocked [, warning_text])` | Функция | Блокирует или разрешает сон. Вторая перегрузка принимает текст предупреждения. | [UIGameCustom_script.cpp:220](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L220), [UIGameCustom_script.cpp:221](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L221) |
| `set_sleep_selected_hours(hours)` | Функция | Задаёт выбранную в диалоге длительность сна. | [UIGameCustom_script.cpp:217](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L217) |
| `SetARGB(rgba, a)` | Функция | Задаёт `argb`. Аргументы `rgba` (`number`), `a` (`number`). Результат `number`. Реализация `return subst_alpha(rgba, a);`. | [UIWindow_script.cpp:129](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L129) |
| `SetCursorPosition(pos)` | Функция | Задаёт позицию курсора. Аргументы `pos` (`vector2`). Реализация `GetUICursor().SetUICursorPosition(pos);`. | [UIWindow_script.cpp:127](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L127) |
| `show_sleep_dialog()` | Функция | Пытается открыть диалог выбора длительности сна. | [UIGameCustom_script.cpp:207](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L207) |
| `show_sleep_dialog_at_hour(hours)` | Функция | Открывает диалог сна с заданной начальной длительностью, если его разметка загружена. | [UIGameCustom_script.cpp:208](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L208) |
| `system_depends` | Класс | Тип `enum_exporter` для работы с понятием «системы `depends`» в скрипте. | [UIOptionsManagerScript.cpp:59](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIOptionsManagerScript.cpp#L59) |
| `TEX_INFO` | Класс | Описание файла и прямоугольной области зарегистрированной текстуры интерфейса. | [UIWindow_script.cpp:131](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L131) |

### `ActorMenu`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `ActorMenu.get_actor_menu()` | Функция | Возвращает актёра меню. Результат `CUIActorMenu`. Реализация `return CurrentGameUI()->ActorMenu();`. | [UIActorMenu_script.cpp:494](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L494) |
| `ActorMenu.get_maingame()` | Функция | Возвращает `maingame`. Результат `CUIMainIngameWnd`. Реализация `if (CUIGameCustom* ui = CurrentGameUI()) return ui->UIMainIngameWnd; return nullptr;`. | [UIActorMenu_script.cpp:496](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L496) |
| `ActorMenu.get_menu_mode()` | Функция | Возвращает режим меню. Результат `number`. Реализация `return (u8)(CurrentGameUI()->ActorMenu()->GetMenuMode());`. | [UIActorMenu_script.cpp:495](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L495) |
| `ActorMenu.get_pda_menu()` | Функция | Возвращает `pda` меню. Результат `CUIPdaWnd`. Реализация `return CurrentGameUI()->PdaMenu();`. | [UIActorMenu_script.cpp:493](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L493) |

### `CDialogHolder`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CDialogHolder:SetMainInputReceiver(ir, find_remove)` | Метод | Задаёт основной ввода получателя. Аргументы `ir` (`CUIDialogWnd`), `find_remove` (`boolean`). | [UIWindow_script.cpp:205](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L205) |
| `CDialogHolder:start_stop_menu(pDialog, bDoHideIndicators)` | Метод | Запускает `stop` меню. Аргументы `pDialog` (`CUIDialogWnd`), `bDoHideIndicators` (`boolean`). | [UIWindow_script.cpp:203](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L203) |

### `CGameFont`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CGameFont.alJustify` | Константа | Значение перечисления `CGameFont` для режима «`al` `justify`». `CGameFont::alJustify`. | [ui_export_script.cpp:93](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L93) |

### `CScriptXmlInit`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CScriptXmlInit:GetColor(path, index)` | Метод | Возвращает цвет. Аргументы `path` (`string`), `index` (`number`). Результат `number`. Реализация `return CUIXmlInit::GetColor(m_xml, path, index, 0xFFFFFFFF);`. | [ScriptXMLInit.cpp:373](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L373) |
| `CScriptXmlInit:GetNodesNum(path, index, tag_name)` | Метод | Возвращает узлы номер. Аргументы `path` (`string`), `index` (`number`), `tag_name` (`string`). Результат `number`. Реализация `return m_xml.GetNodesNum(path, index, tag_name);`. | [ScriptXMLInit.cpp:366](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L366) |
| `CScriptXmlInit:InitArrow(path, parent)` | Метод | Инициализирует стрелки. Аргументы `path` (`string`), `parent` (`CUIWindow`). Результат `CUIArrow`. Реализация `CUIArrow* pWnd = new CUIArrow(); pWnd->init_from_xml (m_xml, path, parent); return pWnd;`. | [ScriptXMLInit.cpp:405](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L405) |
| `CScriptXmlInit:InitArrowStepper(path, parent)` | Метод | Инициализирует стрелки пошагового регулятора. Аргументы `path` (`string`), `parent` (`CUIWindow`). Результат `CUIArrowStepper`. | [ScriptXMLInit.cpp:396](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L396) |
| `CScriptXmlInit:InitGamepadLegend(path, parent)` | Метод | Инициализирует геймпада подсказок управления. Аргументы `path` (`string`), `parent` (`CUIWindow`). Результат `CUIGamepadLegend`. | [ScriptXMLInit.cpp:382](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L382) |
| `CScriptXmlInit:InitLabel(path, parent)` | Метод | Инициализирует `label`. Аргументы `path` (`string`), `parent` (`CUIWindow`). Результат `CUIFrameLineWnd`. | [ScriptXMLInit.cpp:378](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L378) |
| `CScriptXmlInit:InitList(path, parent)` | Метод | Инициализирует списка. Аргументы `path` (`string`), `parent` (`CUIWindow`). Результат `CUIListWnd`. | [ScriptXMLInit.cpp:403](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L403) |
| `CScriptXmlInit:InitStackPanel(path, parent)` | Метод | Инициализирует стопки `panel`. Аргументы `path` (`string`), `parent` (`CUIWindow`). Результат `CUIStackPanel`. | [ScriptXMLInit.cpp:381](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L381) |
| `CScriptXmlInit:NavigateToNode(path, index)` | Метод | Возвращает `navigate` `to` узел. Аргументы `path` (`string`), `index` (`number`). Результат `boolean`. | [ScriptXMLInit.cpp:367](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L367) |
| `CScriptXmlInit:NavigateToNode_ByAttribute(tag_name, attrib_name, attrib_value)` | Метод | Возвращает `navigate` `to` узел `by` атрибут. Аргументы `tag_name` (`string`), `attrib_name` (`string`), `attrib_value` (`string`). Результат `boolean`. | [ScriptXMLInit.cpp:368](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L368) |
| `CScriptXmlInit:NavigateToNode_ByPath(path, index, tag_name, attrib, attrib_value_pattern)` | Метод | Возвращает `navigate` `to` узел `by` пути. Аргументы `path` (`string`), `index` (`number`), `tag_name` (`string`), `attrib` (`string`), `attrib_value_pattern` (`string`). Результат `boolean`. | [ScriptXMLInit.cpp:369](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L369) |
| `CScriptXmlInit:NavigateToRoot()` | Метод | Возвращает корень контекста поиска к корню загруженного XML. Реализация `m_xml.SetLocalRoot(m_xml.GetRoot());`. | [ScriptXMLInit.cpp:370](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L370) |
| `CScriptXmlInit:NodeExist(path, index)` | Метод | Возвращает узел `exist`. Аргументы `path` (`string`), `index` (`number`). Результат `boolean`. Реализация `if (m_xml.NavigateToNode(path, index)) { return true; } return false;`. | [ScriptXMLInit.cpp:365](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L365) |
| `CScriptXmlInit:ParseDirFile(xml_dir, xml_file)` | Метод | Загружает XML из указанного подкаталога конфигурации. Аргументы `xml_dir` (`string`), `xml_file` (`string`). Реализация `m_xml.Load(CONFIG_PATH, xml_dir, xml_file);`. | [ScriptXMLInit.cpp:363](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L363) |
| `CScriptXmlInit:ReadAttribute(path, index, attrib)` | Метод | Считывает атрибут. Аргументы `path` (`string`), `index` (`number`), `attrib` (`string`). Результат `string`. Реализация `return m_xml.ReadAttrib(path, index, attrib, "");`. | [ScriptXMLInit.cpp:372](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L372) |
| `CScriptXmlInit:ReadValue(path, index)` | Метод | Считывает значение. Аргументы `path` (`string`), `index` (`number`). Результат `string`. Реализация `return m_xml.Read(path, index, "");`. | [ScriptXMLInit.cpp:371](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L371) |

### `CUI3tButton`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUI3tButton:OnClick()` | Метод | Обработчик события «`click`». Реализация `CUIButton::OnClick (); PlaySoundT ();`. | [UIButton_script.cpp:54](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L54) |
| `CUI3tButton:SetHighlighted(val)` | Метод | Задаёт подсветку. Аргументы `val` (`boolean`). Реализация `m_bForceHighlight = val;`. | [UIButton_script.cpp:55](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L55) |

### `CUIActionRepeatersManager`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIActionRepeatersManager:CanRepeatActionNow(owner, actionId)` | Метод | Проверяет, истекла ли задержка до следующего повтора действия у данного владельца. Аргументы `owner` (`ActionRepeatersOwnerKey`), `actionId` (`number`). Результат `boolean`. | [UIActionRepeaters_script.cpp:29](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L29) |
| `CUIActionRepeatersManager:IsActionStarted(owner, actionId)` | Метод | Проверяет, запущен ли повтор действия для указанного владельца. Аргументы `owner` (`ActionRepeatersOwnerKey`), `actionId` (`number`). Результат `boolean`. | [UIActionRepeaters_script.cpp:31](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L31) |
| `CUIActionRepeatersManager:Reset(actionId)`, `CUIActionRepeatersManager:Reset(owner, actionId)` | Метод | Сбрасывает повтор указанного действия. Перегрузка с владельцем ограничивает сброс его регистрацией. | [UIActionRepeaters_script.cpp:27](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L27), [UIActionRepeaters_script.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L28) |
| `CUIActionRepeatersManager:ResetAll(skipOneOwner)` | Метод | Сбрасывает `all`. Аргументы `skipOneOwner` (`CUIActionRepeatersOwner`). | [UIActionRepeaters_script.cpp:26](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L26) |
| `CUIActionRepeatersManager:SetActionStarted(owner, actionId)` | Метод | Задаёт действие запуска. Аргументы `owner` (`ActionRepeatersOwnerKey`), `actionId` (`number`). | [UIActionRepeaters_script.cpp:30](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L30) |
| `CUIActionRepeatersManager:UnregisterOwner(owner)` | Метод | Снимает с регистрации `owner`. Аргументы `owner` (`ActionRepeatersOwnerKey`). | [UIActionRepeaters_script.cpp:25](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L25) |

### `CUIActorMenu`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIActorMenu:CallMessageBoxOK(text)` | Метод | Показывает диалог с текстом и кнопкой подтверждения. Аргументы `text` (`string`). Реализация `m_bShowInfoWnds = false; m_message_box_ok->SetText(text); m_message_box_ok->ShowDialog(false);`. | [UIActorMenu_script.cpp:402](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L402) |
| `CUIActorMenu:CallMessageBoxYesNo(text)` | Метод | Показывает диалог подтверждения с кнопками «Да» и «Нет». Аргументы `text` (`string`). | [UIActorMenu_script.cpp:401](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L401) |
| `CUIActorMenu:get_drag_item()` | Метод | Возвращает `drag` предмета. Результат `game_object`. | [UIActorMenu_script.cpp:396](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L396) |
| `CUIActorMenu:GetInvBox()` | Метод | Возвращает контейнер инвентаря. Результат `game_object`. | [UIActorMenu_script.cpp:411](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L411) |
| `CUIActorMenu:GetInventorySortSystem()` | Метод | Возвращает инвентаря сортировки системы. Результат `string`. | [UIActorMenu_script.cpp:417](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L417) |
| `CUIActorMenu:GetMenuMode()` | Метод | Возвращает режим меню. Результат `EMenuMode`. | [UIActorMenu_script.cpp:409](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L409) |
| `CUIActorMenu:GetPartner()` | Метод | Возвращает партнёра. Результат `game_object`. | [UIActorMenu_script.cpp:410](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L410) |
| `CUIActorMenu:highlight_for_each_in_slot(functor, type, slot_id)` | Метод | Подсвечивает `for` `each` `in` слот. Аргументы `functor` (`function`), `type` (`number`), `slot_id` (`number`). | [UIActorMenu_script.cpp:398](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L398) |
| `CUIActorMenu:highlight_section_in_slot(section, type, slot_id)` | Метод | Подсвечивает секцию `in` слот. Аргументы `section` (`string`), `type` (`number`), `slot_id` (`number`). | [UIActorMenu_script.cpp:397](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L397) |
| `CUIActorMenu:refresh_current_cell_item()` | Метод | Обновляет текущий ячейки предмета. | [UIActorMenu_script.cpp:399](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L399) |
| `CUIActorMenu:SetActor(GO)` | Метод | Задаёт актёра. Аргументы `GO` (`game_object`). Реализация `menu->SetActor(Actor()->cast_inventory_owner());`. | [UIActorMenu_script.cpp:414](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L414) |
| `CUIActorMenu:SetInvBox(GO)` | Метод | Задаёт контейнер инвентаря. Аргументы `GO` (`game_object`). Реализация `CInventoryBox* inv_box = GO->object().cast_inventory_box(); if (inv_box) { menu->SetInvBox(inv_box); }`. | [UIActorMenu_script.cpp:413](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L413) |
| `CUIActorMenu:SetInventorySortSystem(system)` | Метод | Задаёт инвентаря сортировки системы. Аргументы `system` (`string`). | [UIActorMenu_script.cpp:416](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L416) |
| `CUIActorMenu:SetMenuMode(mode)` | Метод | Задаёт режим меню. Аргументы `mode` (`EMenuMode`). | [UIActorMenu_script.cpp:408](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L408) |
| `CUIActorMenu:SetPartner(GO)` | Метод | Задаёт партнёра. Аргументы `GO` (`game_object`). Реализация `CInventoryOwner* io = GO->object().cast_inventory_owner(); if (io) menu->SetPartner(io);`. | [UIActorMenu_script.cpp:412](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L412) |
| `CUIActorMenu:ShowGamepadLegend(b)` | Метод | Показывает геймпада подсказок управления. Аргументы `b` (`boolean`). Реализация `if (!menu) { return; } menu->m_should_show_gamepad_legend = b;`. | [UIActorMenu_script.cpp:415](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L415) |
| `CUIActorMenu:ToBelt(GO, b_use_cursor_pos)` | Метод | Возвращает `to` пояса. Аргументы `GO` (`game_object`), `b_use_cursor_pos` (`boolean`). Результат `boolean`. | [UIActorMenu_script.cpp:407](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L407) |
| `CUIActorMenu:ToSlot(GO, force_place, slot_id)` | Метод | Возвращает `to` слот. Аргументы `GO` (`game_object`), `force_place` (`boolean`), `slot_id` (`number`). Результат `boolean`. | [UIActorMenu_script.cpp:406](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L406) |
| `CUIActorMenu:UpdateConditionProgressBars()` | Метод | Обновляет состояние предмета или персонажа прогресса `bars`. | [UIActorMenu_script.cpp:400](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L400) |

### `CUIArrow`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIArrow:GetPos()` | Метод | Возвращает позицию. Результат `number`. | [UIArrow.cpp:80](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIArrow.cpp#L80) |
| `CUIArrow:SetNewValue(new_value)` | Метод | Задаёт `new` значение. Аргументы `new_value` (`number`). | [UIArrow.cpp:78](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIArrow.cpp#L78) |
| `CUIArrow:SetPos(pos)` | Метод | Задаёт позицию. Аргументы `pos` (`number`). Реализация `m_pos = pos; inherited::SetHeading( m_angle_begin + m_pos * m_angle_range );`. | [UIArrow.cpp:79](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIArrow.cpp#L79) |

### `CUIArrowStepper`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIArrowStepper:GetCheck()` | Метод | Возвращает флажка. Результат `boolean`. | [UIButton_script.cpp:107](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L107) |
| `CUIArrowStepper:GetFValue()` | Метод | Возвращает дробное значение. Результат `number`. | [UIButton_script.cpp:110](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L110) |
| `CUIArrowStepper:GetIValue()` | Метод | Возвращает целочисленное значение. Результат `number`. | [UIButton_script.cpp:109](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L109) |
| `CUIArrowStepper:SetCheck(b)` | Метод | Задаёт флажка. Аргументы `b` (`boolean`). | [UIButton_script.cpp:108](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L108) |
| `CUIArrowStepper:SetCurrentValue()` | Метод | Задаёт текущий значение. | [UIButton_script.cpp:111](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L111) |

### `CUIButton`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIButton:EnableTextHighlighting(enabled)` | Метод | Разрешает подсветку текста кнопки при `true`, отключает при `false`. | [UIButton_script.cpp:49](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L49) |

### `CUICaption`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUICaption:addCustomMessage(msg_name, x, y, font_size, pFont, al, color, def_str)` | Метод | Добавляет `custom` сообщение. Аргументы `msg_name` (`string`), `x` (`number`), `y` (`number`), `font_size` (`number`), `pFont` (`CGameFont`), `al` (`CGameFont::EAligment`), `color` (`number`), `def_str` (`string`). | [ui_export_script.cpp:97](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L97) |
| `CUICaption:setCaption(msg_name, message_to_out, color, replaceColor)` | Метод | Задаёт `caption`. Аргументы `msg_name` (`string`), `message_to_out` (`string`), `color` (`number`), `replaceColor` (`boolean`). | [ui_export_script.cpp:98](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L98) |

### `CUICheckButton`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUICheckButton:AssignProps(entry, group)` | Метод | Назначает `props`. Аргументы `entry` (`string`), `group` (`string`). Реализация `btn->AssignProps(entry, group);`. | [UIButton_script.cpp:64](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L64) |
| `CUICheckButton:SetSystemDepends(depend)` | Метод | Задаёт системы `depends`. Аргументы `depend` (`number`). Реализация `btn->SetSystemDepends((CUIOptionsItem::ESystemDepends)depend);`. | [UIButton_script.cpp:65](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L65) |
| `CUICheckButton:Toggle()` | Метод | Инвертирует состояние флажка. Реализация `btn->SetCheck(!btn->GetCheck()); btn->SendClickCallback();`. | [UIButton_script.cpp:66](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L66) |

### `CUIComboBox`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIComboBox:AssignProps(entry, group)` | Метод | Назначает `props`. Аргументы `entry` (`string`), `group` (`string`). Реализация `cb->AssignProps(entry, group);`. | [UIComboBox_script.cpp:48](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L48) |
| `CUIComboBox:GetCurrentIdx()` | Метод | Возвращает индекс текущего элемента. Результат `number`. Реализация `return m_list_box.GetSelectedIDX();`. | [UIComboBox_script.cpp:47](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L47) |
| `CUIComboBox:GetListBox()` | Метод | Возвращает виджет списка. Результат `CUIListBox`. | [UIComboBox_script.cpp:50](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L50) |
| `CUIComboBox:GetSize()` | Метод | Возвращает размер. Результат `number`. Реализация `return m_list_box.GetSize();`. | [UIComboBox_script.cpp:55](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L55) |
| `CUIComboBox:GetValueOf(index)` | Метод | Возвращает значение `of`. Аргументы `index` (`number`). Результат `string`. Реализация `if (u32(index) >= GetSize()) return ""; auto token = GetOptToken(); return token[index].name;`. | [UIComboBox_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L42) |
| `CUIComboBox:IsListShown()` | Метод | Возвращает true, если список вариантов раскрыт. Результат `boolean`. Реализация `return m_list_frame.IsShown();`. | [UIComboBox_script.cpp:53](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L53) |
| `CUIComboBox:OnListItemSelect()` | Метод | Обработчик события «списка предмета `select`». | [UIComboBox_script.cpp:51](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L51) |
| `CUIComboBox:SetCurrentIdx(idx)` | Метод | Задаёт индекс текущего элемента. Аргументы `idx` (`number`). Реализация `m_list_box.SetSelectedIDX(idx);`. | [UIComboBox_script.cpp:46](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L46) |
| `CUIComboBox:SetHighlighted(val)` | Метод | Задаёт подсветку. Аргументы `val` (`boolean`). | [UIComboBox_script.cpp:54](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L54) |
| `CUIComboBox:SetItemIDX(idx)` | Метод | Задаёт предмета индекс. Аргументы `idx` (`number`). | [UIComboBox_script.cpp:56](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L56) |
| `CUIComboBox:SetSystemDepends(depend)` | Метод | Задаёт системы `depends`. Аргументы `depend` (`number`). Реализация `cb->SetSystemDepends((CUIOptionsItem::ESystemDepends)depend);`. | [UIComboBox_script.cpp:49](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L49) |
| `CUIComboBox:ShowList(bShow)` | Метод | Показывает списка. Аргументы `bShow` (`boolean`). | [UIComboBox_script.cpp:52](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L52) |

### `CUICompassBar`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUICompassBar:Background()` | Метод | Возвращает фона. Результат `CUIStatic`. | [UIActorMenu_script.cpp:468](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L468) |
| `CUICompassBar:GetFrame()` | Метод | Возвращает рамки. Результат `CUIWindow`. | [UIActorMenu_script.cpp:469](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L469) |

### `CUIGameCustom`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIGameCustom:AbortSleep()` | Метод | Досрочно прерывает текущий сон. | [UIGameCustom_script.cpp:189](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L189) |
| `CUIGameCustom:AddCustomMessage(id, x, y, font_size, pFont, alignment, color, flicker)` | Метод | Добавляет `custom` сообщение. Аргументы `id` (`string`), `x` (`number`), `y` (`number`), `font_size` (`number`), `pFont` (`CGameFont`), `alignment` (`number`), `color` (`number`), `flicker` (`number`). | [UIGameCustom_script.cpp:160](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L160), [UIGameCustom_script.cpp:161](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L161) |
| `CUIGameCustom:AddHudMessage(text, text2, id, trnslate_second_text, time, bSingleInstance)` | Метод | Добавляет HUD сообщение. Аргументы `text` (`string`), `text2` (`string`), `id` (`string`), `trnslate_second_text` (`boolean`), `time` (`number`), `bSingleInstance` (`boolean`). Результат `SDrawStaticStruct`. | [UIGameCustom_script.cpp:165](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L165) |
| `CUIGameCustom:CancelSleepDialog()` | Метод | Закрывает диалог сна с отменой. | [UIGameCustom_script.cpp:186](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L186) |
| `CUIGameCustom:ClearSleepAllowBleeding()` | Метод | Очищает правило сна при кровотечении. Реализация `if (SleepWnd != nullptr) SleepWnd->ClearSleepAllowBleeding();`. | [UIGameCustom_script.cpp:197](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L197) |
| `CUIGameCustom:ClearSleepHourPresets()` | Метод | Очищает варианты длительности сна. Реализация `if (SleepWnd != nullptr) SleepWnd->ClearHourPresets();`. | [UIGameCustom_script.cpp:182](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L182) |
| `CUIGameCustom:ClearSleepHoursRange()` | Метод | Очищает диапазон длительности сна. Реализация `if (SleepWnd != nullptr) SleepWnd->ClearSleepHoursRange();`. | [UIGameCustom_script.cpp:195](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L195) |
| `CUIGameCustom:ClearSleepMute()` | Метод | Очищает отключение звука на время сна. Реализация `if (SleepWnd != nullptr) SleepWnd->ClearSleepMute();`. | [UIGameCustom_script.cpp:201](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L201) |
| `CUIGameCustom:ClearSleepRestorePower()` | Метод | Очищает восстановление выносливости во сне. Реализация `if (SleepWnd != nullptr) SleepWnd->ClearSleepRestorePower();`. | [UIGameCustom_script.cpp:199](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L199) |
| `CUIGameCustom:ClearSleepSessionOverrides()` | Метод | Очищает переопределения для сеанса сна. Реализация `if (SleepWnd != nullptr) SleepWnd->ClearSleepSessionOverrides();`. | [UIGameCustom_script.cpp:202](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L202) |
| `CUIGameCustom:ConfirmSleep()` | Метод | Подтверждает выбранную длительность и запускает сон при допустимом состоянии. | [UIGameCustom_script.cpp:187](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L187) |
| `CUIGameCustom:CurrentItemAtCell()` | Метод | Возвращает текущий предмета `at` ячейки. Результат `game_object`. | [UIGameCustom_script.cpp:171](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L171) |
| `CUIGameCustom:CustomMessageOut(id, msg, color)` | Метод | Выводит сообщение по указанному ID с заданным текстом и цветом. Аргументы `id` (`string`), `msg` (`string`), `color` (`number`). Реализация `GameCaptions()->setCaption(id,msg,color,true);`. | [UIGameCustom_script.cpp:162](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L162) |
| `CUIGameCustom:ForceSleep(hours)` | Метод | Запускает принудительный сон на указанное число игровых часов. | [UIGameCustom_script.cpp:188](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L188) |
| `CUIGameCustom:GetSleepPhase()` | Метод | Возвращает числовую фазу текущего сна. | [UIGameCustom_script.cpp:193](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L193) |
| `CUIGameCustom:GetSleepSelectedHours()` | Метод | Возвращает выбранную в диалоге длительность сна в игровых часах. | [UIGameCustom_script.cpp:190](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L190) |
| `CUIGameCustom:HideSleepDialog()` | Метод | Скрывает диалог сна. | [UIGameCustom_script.cpp:185](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L185) |
| `CUIGameCustom:IsActorSleeping()` | Метод | Проверяет, идёт ли сон актёра. | [UIGameCustom_script.cpp:192](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L192) |
| `CUIGameCustom:IsSleepDialogReady()` | Метод | Проверяет готовность диалога сна. | [UIGameCustom_script.cpp:183](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L183) |
| `CUIGameCustom:IsSleepDialogShown()` | Метод | Проверяет, показан ли диалог сна. | [UIGameCustom_script.cpp:184](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L184) |
| `CUIGameCustom:RemoveCustomMessage(id)` | Метод | Удаляет `custom` сообщение. Аргументы `id` (`string`). Реализация `GameCaptions()->removeCustomMessage(id);`. | [UIGameCustom_script.cpp:163](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L163) |
| `CUIGameCustom:SetMainInputReceiver(ir, find_remove)` | Метод | Задаёт основной ввода получателя. Аргументы `ir` (`CUIDialogWnd`), `find_remove` (`boolean`). | [UIGameCustom_script.cpp:153](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L153) |
| `CUIGameCustom:SetSleepAllowBleeding(allow)` | Метод | Задаёт правило сна при кровотечении. Аргументы `allow` (`boolean`). Реализация `if (SleepWnd != nullptr) SleepWnd->SetSleepAllowBleeding(allow);`. | [UIGameCustom_script.cpp:196](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L196) |
| `CUIGameCustom:SetSleepBlocked(blocked [, warning_text])` | Метод | Блокирует или разрешает сон. Вторая перегрузка принимает текст предупреждения. | [UIGameCustom_script.cpp:203](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L203), [UIGameCustom_script.cpp:204](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L204) |
| `CUIGameCustom:SetSleepHourPresets(tbl)` | Метод | Задаёт варианты длительности сна. Аргументы `tbl` (`Lua value`). | [UIGameCustom_script.cpp:181](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L181) |
| `CUIGameCustom:SetSleepHoursRange(minHours, maxHours)` | Метод | Задаёт диапазон длительности сна. Аргументы `minHours` (`number`), `maxHours` (`number`). Реализация `if (SleepWnd != nullptr) SleepWnd->SetSleepHoursRange(minHours, maxHours);`. | [UIGameCustom_script.cpp:194](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L194) |
| `CUIGameCustom:SetSleepMute(muteMusic, muteEffects)` | Метод | Задаёт отключение звука на время сна. Аргументы `muteMusic` (`boolean`), `muteEffects` (`boolean`). Реализация `if (SleepWnd != nullptr) SleepWnd->SetSleepMute(muteMusic, muteEffects);`. | [UIGameCustom_script.cpp:200](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L200) |
| `CUIGameCustom:SetSleepRestorePower(power)` | Метод | Задаёт восстановление выносливости во сне. Аргументы `power` (`number`). Реализация `if (SleepWnd != nullptr) SleepWnd->SetSleepRestorePower(power);`. | [UIGameCustom_script.cpp:198](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L198) |
| `CUIGameCustom:SetSleepSelectedHours(hours)` | Метод | Задаёт выбранную в диалоге длительность сна. | [UIGameCustom_script.cpp:191](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L191) |
| `CUIGameCustom:ShowActorMenu()` | Метод | Показывает актёра меню. Результат `boolean`. | [UIGameCustom_script.cpp:169](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L169) |
| `CUIGameCustom:ShowSleepDialog()` | Метод | Пытается открыть диалог выбора длительности сна. | [UIGameCustom_script.cpp:179](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L179) |
| `CUIGameCustom:ShowSleepDialogAtHour(hours)` | Метод | Открывает диалог сна с заданной начальной длительностью, если его разметка загружена. | [UIGameCustom_script.cpp:180](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L180) |
| `CUIGameCustom:UpdateActorMenu()` | Метод | Обновляет актёра меню. | [UIGameCustom_script.cpp:170](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L170) |

### `CUIGamepadLegend`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIGamepadLegend:ReloadLegend()` | Метод | Перезагружает подсказок управления. Реализация `for (CUIWindow* W : m_ChildWndList) { if (!W \|\| !W->ui_cast_static()) { continue; } W->ui_cast_static()->ReloadText(); }`. | [UIWindow_script.cpp:234](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L234) |

### `CUIHudStatesWnd`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIHudStatesWnd.m_back` | Поле для чтения | Виджет HUD или его параметр «`back`». Доступно для чтения. | [UIActorMenu_script.cpp:476](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L476) |
| `CUIHudStatesWnd.m_fire_mode` | Поле для чтения | Виджет HUD или его параметр «стрельбы режим». Доступно для чтения. | [UIActorMenu_script.cpp:483](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L483) |
| `CUIHudStatesWnd.m_radia_damage` | Поле для чтения | Виджет HUD или его параметр «`radia` урон». Доступно для чтения. | [UIActorMenu_script.cpp:488](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L488) |
| `CUIHudStatesWnd.m_ui_grenade` | Поле для чтения | Виджет HUD или его параметр «интерфейса гранаты». Доступно для чтения. | [UIActorMenu_script.cpp:484](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L484) |
| `CUIHudStatesWnd.m_ui_health_bar` | Поле для чтения | Виджет HUD или его параметр «интерфейса здоровье полосы». Доступно для чтения. | [UIActorMenu_script.cpp:486](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L486) |
| `CUIHudStatesWnd.m_ui_stamina_bar` | Поле для чтения | Виджет HUD или его параметр «интерфейса `stamina` полосы». Доступно для чтения. | [UIActorMenu_script.cpp:487](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L487) |
| `CUIHudStatesWnd.m_ui_weapon_ammo_color_active` | Поле для чтения | Виджет HUD или его параметр «интерфейса оружия боеприпаса цвет активный». Доступно для чтения. | [UIActorMenu_script.cpp:477](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L477) |
| `CUIHudStatesWnd.m_ui_weapon_ammo_color_inactive` | Поле для чтения | Виджет HUD или его параметр «интерфейса оружия боеприпаса цвет `inactive`». Доступно для чтения. | [UIActorMenu_script.cpp:478](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L478) |
| `CUIHudStatesWnd.m_ui_weapon_ap_ammo` | Поле для чтения | Виджет HUD или его параметр «интерфейса оружия `ap` боеприпаса». Доступно для чтения. | [UIActorMenu_script.cpp:481](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L481) |
| `CUIHudStatesWnd.m_ui_weapon_cur_ammo` | Поле для чтения | Виджет HUD или его параметр «интерфейса оружия `cur` боеприпаса». Доступно для чтения. | [UIActorMenu_script.cpp:479](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L479) |
| `CUIHudStatesWnd.m_ui_weapon_fmj_ammo` | Поле для чтения | Виджет HUD или его параметр «интерфейса оружия `fmj` боеприпаса». Доступно для чтения. | [UIActorMenu_script.cpp:480](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L480) |
| `CUIHudStatesWnd.m_ui_weapon_icon` | Поле для чтения | Виджет HUD или его параметр «интерфейса оружия значка». Доступно для чтения. | [UIActorMenu_script.cpp:485](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L485) |
| `CUIHudStatesWnd.m_ui_weapon_third_ammo` | Поле для чтения | Виджет HUD или его параметр «интерфейса оружия `third` боеприпаса». Доступно для чтения. | [UIActorMenu_script.cpp:482](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L482) |

### `CUIListBox`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIListBox:GetItemHeight()` | Метод | Возвращает предмета высоту. Результат `number`. | [UIListBox_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListBox_script.cpp#L42) |
| `CUIListBox:NextItem(selectOnly)` | Метод | Переходит к следующему значению. Аргументы `selectOnly` (`boolean`). | [UIListBox_script.cpp:49](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListBox_script.cpp#L49) |
| `CUIListBox:PrevItem(selectOnly)` | Метод | Переходит к предыдущему значению. Аргументы `selectOnly` (`boolean`). | [UIListBox_script.cpp:48](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListBox_script.cpp#L48) |
| `CUIListBox:SetItemHeight(h)` | Метод | Задаёт предмета высоту. Аргументы `h` (`number`). Реализация `m_def_item_height = h;`. | [UIListBox_script.cpp:41](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListBox_script.cpp#L41) |
| `CUIListBox:SetSelectedIndex(idx)` | Метод | Задаёт индекс выбранного элемента. Аргументы `idx` (`number`). Реализация `SetSelected(GetItemByIDX(idx));`. | [UIListBox_script.cpp:40](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListBox_script.cpp#L40) |

### `CUIListItemEx`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIListItemEx:SetSelectionColor(dwColor)` | Метод | Задаёт выделения цвет. Аргументы `dwColor` (`number`). Реализация `m_dwSelectionColor = dwColor;`. | [UIListWnd_script.cpp:74](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L74) |

### `CUIListWnd`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIListWnd:ActivateList(activity)` | Метод | Разрешает или отключает активность списка. Аргументы `activity` (`boolean`). Реализация `m_bListActivity = activity;`. | [UIListWnd_script.cpp:56](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L56) |
| `CUIListWnd:AddItem(item)` | Метод | Добавляет предмета. Аргументы `item` (`CUIListItem`). Результат `boolean`. Реализация `return AddItem(item, -1);`. | [UIListWnd_script.cpp:37](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L37) |
| `CUIListWnd:EnableScrollBar(enable)` | Метод | Включает полосу прокрутки. Аргументы `enable` (`boolean`). Реализация `m_ScrollBar->SetEnabled(enable); UpdateScrollBar();`. | [UIListWnd_script.cpp:43](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L43) |
| `CUIListWnd:GetFocusedItem()` | Метод | Возвращает элемент списка в фокусе. Результат `number`. | [UIListWnd_script.cpp:61](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L61) |
| `CUIListWnd:GetItem(index)` | Метод | Возвращает предмета. Аргументы `index` (`number`). Результат `CUIListItem`. | [UIListWnd_script.cpp:48](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L48) |
| `CUIListWnd:GetItemPos(pItem)` | Метод | Возвращает предмета позицию. Аргументы `pItem` (`CUIListItem`). Результат `number`. | [UIListWnd_script.cpp:49](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L49) |
| `CUIListWnd:GetSelectedItem()` | Метод | Возвращает выбранный элемент списка. Результат `number`. | [UIListWnd_script.cpp:66](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L66) |
| `CUIListWnd:GetSize()` | Метод | Возвращает число элементов списка. Результат `number`. Реализация `return m_ItemList.size();`. | [UIListWnd_script.cpp:50](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L50) |
| `CUIListWnd:GetVertFlip()` | Метод | Возвращает `vert` `flip`. Результат `boolean`. | [UIListWnd_script.cpp:59](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L59) |
| `CUIListWnd:IsListActive()` | Метод | Проверяет, активен ли список. Результат `boolean`. | [UIListWnd_script.cpp:57](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L57) |
| `CUIListWnd:IsScrollBarEnabled()` | Метод | Проверяет, включена ли полоса прокрутки списка. Результат `boolean`. Реализация `return m_ScrollBar->GetEnabled();`. | [UIListWnd_script.cpp:44](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L44) |
| `CUIListWnd:NextItem(selectOnly, loop)` | Метод | Переходит к следующему значению. Аргументы `selectOnly` (`boolean`), `loop` (`boolean`). Результат `boolean`. | [UIListWnd_script.cpp:64](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L64) |
| `CUIListWnd:PrevItem(selectOnly, loop)` | Метод | Переходит к предыдущему значению. Аргументы `selectOnly` (`boolean`), `loop` (`boolean`). Результат `boolean`. | [UIListWnd_script.cpp:63](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L63) |
| `CUIListWnd:ReinitScrollBar()` | Метод | Переинициализирует полосу прокрутки. Результат `boolean`. | [UIListWnd_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L42) |
| `CUIListWnd:RemoveAll()` | Метод | Удаляет все строки списка. | [UIListWnd_script.cpp:39](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L39) |
| `CUIListWnd:RemoveItem(index)` | Метод | Удаляет предмета. Аргументы `index` (`number`). | [UIListWnd_script.cpp:38](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L38) |
| `CUIListWnd:ResetFocusCapture()` | Метод | Сбрасывает фокуса `capture`. Реализация `m_bForceFocusedItem = false;`. | [UIListWnd_script.cpp:67](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L67) |
| `CUIListWnd:ScrollBar()` | Метод | Прокручивает полосы. Результат `CUIScrollBar`. | [UIListWnd_script.cpp:41](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L41) |
| `CUIListWnd:ScrollToBegin()` | Метод | Прокручивает `to` начала. Реализация `m_ScrollBar->SetScrollPos((s16)m_ScrollBar->GetMinRange()); m_iFirstShownIndex = m_ScrollBar->GetScrollPos(); UpdateList();`. | [UIListWnd_script.cpp:45](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L45), [UIListWnd_script.cpp:51](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L51) |
| `CUIListWnd:ScrollToEnd()` | Метод | Прокручивает `to` конца. | [UIListWnd_script.cpp:46](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L46), [UIListWnd_script.cpp:52](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L52) |
| `CUIListWnd:ScrollToPos(position)` | Метод | Прокручивает `to` позицию. Аргументы `position` (`number`). | [UIListWnd_script.cpp:53](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L53) |
| `CUIListWnd:SetFocusedItem(iNewFocusedItem)` | Метод | Задаёт элемент списка в фокусе. Аргументы `iNewFocusedItem` (`number`). | [UIListWnd_script.cpp:60](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L60) |
| `CUIListWnd:SetItemHeight(iItemHeight)` | Метод | Задаёт предмета высоту. Аргументы `iItemHeight` (`number`). Реализация `m_iItemHeight = iItemHeight; m_iRowNum = iFloor(GetHeight() / iItemHeight);`. | [UIListWnd_script.cpp:47](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L47) |
| `CUIListWnd:SetScrollBarProfile(profile)` | Метод | Задаёт профиль полосы прокрутки. Аргументы `profile` (`string`). Реализация `m_scrollbar_profile = profile;`. | [UIListWnd_script.cpp:40](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L40) |
| `CUIListWnd:SetVertFlip(vert_flip)` | Метод | Задаёт `vert` `flip`. Аргументы `vert_flip` (`boolean`). Реализация `m_bVertFlip = vert_flip;`. | [UIListWnd_script.cpp:58](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L58) |
| `CUIListWnd:SetWidth(width)` | Метод | Задаёт ширину. Аргументы `width` (`number`). Реализация `inherited::SetWidth(width);`. | [UIListWnd_script.cpp:54](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L54) |
| `CUIListWnd:ShowSelectedItem(show)` | Метод | Показывает выбранный элемент списка. Аргументы `show` (`boolean`). Реализация `m_bShowSelectedItem = show; if (show) { create_active_back(); } else destroy_active_back();`. | [UIListWnd_script.cpp:62](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L62) |

### `CUIMainIngameWnd`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIMainIngameWnd.m_ind_bleeding` | Поле для чтения | Виджет HUD или его параметр «`ind` кровотечение». Доступно для чтения. | [UIActorMenu_script.cpp:441](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L441) |
| `CUIMainIngameWnd.m_ind_boost_chem` | Поле для чтения | Виджет HUD или его параметр «`ind` усиление `chem`». Доступно для чтения. | [UIActorMenu_script.cpp:451](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L451) |
| `CUIMainIngameWnd.m_ind_boost_health` | Поле для чтения | Виджет HUD или его параметр «`ind` усиление здоровье». Доступно для чтения. | [UIActorMenu_script.cpp:454](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L454) |
| `CUIMainIngameWnd.m_ind_boost_power` | Поле для чтения | Виджет HUD или его параметр «`ind` усиление выносливость». Доступно для чтения. | [UIActorMenu_script.cpp:455](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L455) |
| `CUIMainIngameWnd.m_ind_boost_psy` | Поле для чтения | Виджет HUD или его параметр «`ind` усиление пси-». Доступно для чтения. | [UIActorMenu_script.cpp:449](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L449) |
| `CUIMainIngameWnd.m_ind_boost_rad` | Поле для чтения | Виджет HUD или его параметр «`ind` усиление `rad`». Доступно для чтения. | [UIActorMenu_script.cpp:456](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L456) |
| `CUIMainIngameWnd.m_ind_boost_radia` | Поле для чтения | Виджет HUD или его параметр «`ind` усиление `radia`». Доступно для чтения. | [UIActorMenu_script.cpp:450](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L450) |
| `CUIMainIngameWnd.m_ind_boost_weight` | Поле для чтения | Виджет HUD или его параметр «`ind` усиление вес». Доступно для чтения. | [UIActorMenu_script.cpp:453](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L453) |
| `CUIMainIngameWnd.m_ind_boost_wound` | Поле для чтения | Виджет HUD или его параметр «`ind` усиление рану». Доступно для чтения. | [UIActorMenu_script.cpp:452](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L452) |
| `CUIMainIngameWnd.m_ind_helmet_broken` | Поле для чтения | Виджет HUD или его параметр «`ind` шлема `broken`». Доступно для чтения. | [UIActorMenu_script.cpp:445](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L445) |
| `CUIMainIngameWnd.m_ind_outfit_broken` | Поле для чтения | Виджет HUD или его параметр «`ind` костюма `broken`». Доступно для чтения. | [UIActorMenu_script.cpp:446](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L446) |
| `CUIMainIngameWnd.m_ind_overvolume` | Поле для чтения | Виджет HUD или его параметр «`ind` `overvolume`». Доступно для чтения. | [UIActorMenu_script.cpp:448](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L448) |
| `CUIMainIngameWnd.m_ind_overweight` | Поле для чтения | Виджет HUD или его параметр «`ind` `overweight`». Доступно для чтения. | [UIActorMenu_script.cpp:447](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L447) |
| `CUIMainIngameWnd.m_ind_radiation` | Поле для чтения | Виджет HUD или его параметр «`ind` радиацию». Доступно для чтения. | [UIActorMenu_script.cpp:442](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L442) |
| `CUIMainIngameWnd.m_ind_starvation` | Поле для чтения | Виджет HUD или его параметр «`ind` `starvation`». Доступно для чтения. | [UIActorMenu_script.cpp:443](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L443) |
| `CUIMainIngameWnd.m_ind_weapon_broken` | Поле для чтения | Виджет HUD или его параметр «`ind` оружия `broken`». Доступно для чтения. | [UIActorMenu_script.cpp:444](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L444) |
| `CUIMainIngameWnd.m_ui_hud_states` | Поле для чтения | Виджет HUD или его параметр «интерфейса HUD `states`». Доступно для чтения. | [UIActorMenu_script.cpp:440](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L440) |
| `CUIMainIngameWnd.UICompassBar` | Поле для чтения | Виджет HUD или его параметр «интерфейса компаса полосы». Доступно для чтения. | [UIActorMenu_script.cpp:437](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L437) |
| `CUIMainIngameWnd.UIMotionIcon` | Поле для чтения | Виджет HUD или его параметр «интерфейса движения значка». Доступно для чтения. | [UIActorMenu_script.cpp:435](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L435) |
| `CUIMainIngameWnd.UIStaticDiskIO` | Поле для чтения | Виджет HUD или его параметр «интерфейса статического виджета `disk` `io`». Доступно для чтения. | [UIActorMenu_script.cpp:433](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L433) |
| `CUIMainIngameWnd.UIStaticQuickHelp` | Поле для чтения | Виджет HUD или его параметр «интерфейса статического виджета `quick` `help`». Доступно для чтения. | [UIActorMenu_script.cpp:434](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L434) |
| `CUIMainIngameWnd.UIZoneMap` | Поле для чтения | Виджет HUD или его параметр «интерфейса зоны карты». Доступно для чтения. | [UIActorMenu_script.cpp:436](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L436) |
| `CUIMainIngameWnd:IsCompassBarMode()` | Метод | Проверяет, выбран ли компас как режим навигации на HUD. Результат `boolean`. Реализация `return m_navigationHud->IsCompassBarMode();`. | [UIActorMenu_script.cpp:439](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L439) |
| `CUIMainIngameWnd:SetNavigationMode(compassBar)` | Метод | Задаёт `navigation` режим. Аргументы `compassBar` (`boolean`). Реализация `m_navigationHud->SetNavigationModeBool(compassBar);`. | [UIActorMenu_script.cpp:438](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L438) |

### `CUIMessageBoxEx`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIMessageBoxEx:Init(xml_template)` | Метод | Инициализирует окно сообщения по имени шаблона в XML. Аргументы `xml_template` (`string`). | [UIMessageBox_script.cpp:22](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIMessageBox_script.cpp#L22) |

### `CUIMMShniaga`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIMMShniaga:GetPage()` | Метод | Возвращает идентификатор текущей страницы главного меню. | [UIMMShniaga.cpp:476](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIMMShniaga.cpp#L476) |

### `CUIPdaWnd`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIPdaWnd:GetActiveDialog()` | Метод | Возвращает активный диалога. Результат `CUIWindow`. | [UIActorMenu_script.cpp:426](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L426) |
| `CUIPdaWnd:GetActiveSection()` | Метод | Возвращает активный секцию. Результат `string`. Реализация `return m_sActiveSection.c_str();`. | [UIActorMenu_script.cpp:427](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L427) |
| `CUIPdaWnd:GetTabControl()` | Метод | Возвращает вкладки `control`. Результат `CUITabControl`. | [UIActorMenu_script.cpp:428](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L428) |
| `CUIPdaWnd:SetActiveDialog(pUI)` | Метод | Задаёт активный диалога. Аргументы `pUI` (`CUIWindow`). Реализация `m_pActiveDialog = pUI;`. | [UIActorMenu_script.cpp:425](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L425) |
| `CUIPdaWnd:SetActiveSubdialog(section)` | Метод | Задаёт активный вложенного диалога. Аргументы `section` (`string`). Реализация `SetActiveSubdialog((const shared_str&)section);`. | [UIActorMenu_script.cpp:424](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L424) |
| `CUIPdaWnd:SwitchZoom()` | Метод | Переключает приближения. | [UIActorMenu_script.cpp:429](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L429) |

### `CUIProgressBar`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIProgressBar:GetProgressStatic()` | Метод | Возвращает статический виджет заполнения полосы. Результат `CUIStatic`. | [UIProgressBar_script.cpp:25](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L25) |
| `CUIProgressBar:SetColor(color)` | Метод | Задаёт цвет. Аргументы `color` (`number`). Реализация `m_UIProgressItem.SetTextureColor(color);`. | [UIProgressBar_script.cpp:20](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L20) |
| `CUIProgressBar:SetMaxColor(color)` | Метод | Задаёт максимальный цвет. Аргументы `color` (`number`). Реализация `m_maxColor.set(color);`. | [UIProgressBar_script.cpp:24](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L24) |
| `CUIProgressBar:SetMiddleColor(color)` | Метод | Задаёт `middle` цвет. Аргументы `color` (`number`). Реализация `m_middleColor.set(color);`. | [UIProgressBar_script.cpp:23](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L23) |
| `CUIProgressBar:SetMinColor(color)` | Метод | Задаёт минимальный цвет. Аргументы `color` (`number`). Реализация `m_minColor.set(color);`. | [UIProgressBar_script.cpp:22](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L22) |
| `CUIProgressBar:ShowBackground(enabled)` | Метод | Показывает фон полосы при `true`, скрывает при `false`. | [UIProgressBar_script.cpp:19](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L19) |
| `CUIProgressBar:SnapProgressPos()` | Метод | Сразу переводит видимое заполнение на последнее значение из `SetProgressPos`. | [UIProgressBar_script.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L28) |
| `CUIProgressBar:UseColor(status)` | Метод | Включает или отключает окрашивание полосы по текущему значению. Аргументы `status` (`boolean`). Реализация `m_bUseColor = status;`. | [UIProgressBar_script.cpp:21](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L21) |

### `CUIPropertiesBox`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIPropertiesBox:GetSelectedItem()` | Метод | Возвращает выбранный элемент списка. Результат `CUIListBoxItem`. Реализация `return m_UIListWnd.GetSelectedItem();`. | [UIPropertiesBox_script.cpp:21](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIPropertiesBox_script.cpp#L21) |
| `CUIPropertiesBox:InitPropertiesBox(pos, size)` | Метод | Инициализирует `properties` контейнера. Аргументы `pos` (`vector2`), `size` (`vector2`). | [UIPropertiesBox_script.cpp:24](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIPropertiesBox_script.cpp#L24) |

### `CUIScriptWnd`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIScriptWnd:GetCheck(name)` | Метод | Ищет зарегистрированный элемент управления по имени и возвращает `CUICheckButton` при совпадении типа. | [UIScriptWnd_script.cpp:62](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L62) |
| `CUIScriptWnd:GetComboBox(name)` | Метод | Ищет зарегистрированный элемент управления по имени и возвращает `CUIComboBox` при совпадении типа. | [UIScriptWnd_script.cpp:61](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L61) |
| `CUIScriptWnd:GetDialogWnd(name)` | Метод | Ищет зарегистрированный элемент управления по имени и возвращает `CUIDialogWnd` при совпадении типа. | [UIScriptWnd_script.cpp:54](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L54) |
| `CUIScriptWnd:GetEditBox(name)` | Метод | Ищет зарегистрированный элемент управления по имени и возвращает `CUIEditBox` при совпадении типа. | [UIScriptWnd_script.cpp:53](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L53) |
| `CUIScriptWnd:GetFrameLineWnd(name)` | Метод | Ищет зарегистрированный элемент управления по имени и возвращает `CUIFrameLineWnd` при совпадении типа. | [UIScriptWnd_script.cpp:56](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L56) |
| `CUIScriptWnd:GetFrameWindow(name)` | Метод | Ищет зарегистрированный элемент управления по имени и возвращает `CUIFrameWindow` при совпадении типа. | [UIScriptWnd_script.cpp:55](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L55) |
| `CUIScriptWnd:GetListWnd(name)` | Метод | Ищет зарегистрированный элемент управления по имени и возвращает `CUIListWnd` при совпадении типа. | [UIScriptWnd_script.cpp:59](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L59) |
| `CUIScriptWnd:GetProgressBar(name)` | Метод | Ищет зарегистрированный элемент управления по имени и возвращает `CUIProgressBar` при совпадении типа. | [UIScriptWnd_script.cpp:57](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L57) |
| `CUIScriptWnd:GetStatic(name)` | Метод | Ищет зарегистрированный элемент управления по имени и возвращает `CUIStatic` при совпадении типа. | [UIScriptWnd_script.cpp:52](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L52) |
| `CUIScriptWnd:GetTabControl(name)` | Метод | Ищет зарегистрированный элемент управления по имени и возвращает `CUITabControl` при совпадении типа. | [UIScriptWnd_script.cpp:58](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L58) |
| `CUIScriptWnd:GetTrackBar(name)` | Метод | Ищет зарегистрированный элемент управления по имени и возвращает `CUITrackBar` при совпадении типа. | [UIScriptWnd_script.cpp:60](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L60) |
| `CUIScriptWnd:OnGamepadKey(key, action)` | Метод | Обрабатывает кнопку геймпада. `key` берётся из `gamepad_keys`, `action` — код UI-события нажатия или отпускания. Верните `true`, если окно обработало событие. | [uiscriptwnd_script2.cpp:29](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/uiscriptwnd_script2.cpp#L29) |
| `CUIScriptWnd:OnGamepadKeyHold(key)` | Метод | Обрабатывает удержание кнопки геймпада. Верните `true`, если окно обработало событие. | [uiscriptwnd_script2.cpp:30](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/uiscriptwnd_script2.cpp#L30) |
| `CUIScriptWnd:OnGamepadStick(key, value, action)` | Метод | Обрабатывает стик геймпада. `value` — двумерный вектор, `action` — код события интерфейса. Верните `true`, если окно обработало событие. | [uiscriptwnd_script2.cpp:31](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/uiscriptwnd_script2.cpp#L31) |
| `CUIScriptWnd:OnGyroscope(value)` | Метод | Обрабатывает данные гироскопа, переданные трёхмерным вектором. Верните `true`, если окно обработало событие. | [uiscriptwnd_script2.cpp:32](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/uiscriptwnd_script2.cpp#L32) |
| `CUIScriptWnd:OnKeyboardHold(key)` | Метод | Обрабатывает удержание клавиши с указанным кодом. Верните `true`, если окно обработало событие. | [uiscriptwnd_script2.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/uiscriptwnd_script2.cpp#L28) |
| `CUIScriptWnd:OnTouchpad(value)` | Метод | Обрабатывает данные сенсорной панели, переданные двумерным вектором. Верните `true`, если окно обработало событие. | [uiscriptwnd_script2.cpp:33](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/uiscriptwnd_script2.cpp#L33) |

### `CUIScrollBar`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIScrollBar.fixed` | Константа | Значение перечисления `CUIScrollBar` для режима «фиксированный». `1`. | [UIScrollBar_script.cpp:60](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L60) |
| `CUIScrollBar.stretch` | Константа | Значение перечисления `CUIScrollBar` для режима «растягиваемый». `0`. | [UIScrollBar_script.cpp:59](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L59) |
| `CUIScrollBar:GetEnabled()` | Метод | Возвращает включения. Результат `boolean`. | [UIScrollBar_script.cpp:56](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L56) |
| `CUIScrollBar:GetMaxRange()` | Метод | Возвращает максимальный диапазон. Результат `number`. | [UIScrollBar_script.cpp:43](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L43) |
| `CUIScrollBar:GetMinRange()` | Метод | Возвращает минимальный диапазон. Результат `number`. | [UIScrollBar_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L42) |
| `CUIScrollBar:GetPageSize()` | Метод | Возвращает размер страницы. Результат `number`. | [UIScrollBar_script.cpp:47](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L47) |
| `CUIScrollBar:GetScrollPos()` | Метод | Возвращает позицию прокрутки. Результат `number`. Реализация `return std::max(_minPos, _scrollPos);`. | [UIScrollBar_script.cpp:45](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L45) |
| `CUIScrollBar:GetStepSize()` | Метод | Возвращает величину шага. Результат `number`. | [UIScrollBar_script.cpp:49](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L49) |
| `CUIScrollBar:InitScrollBarFixed(x, y, isHorizontal, profile)` | Метод | Инициализирует полосу прокрутки фиксированный. Аргументы `x` (`number`), `y` (`number`), `isHorizontal` (`boolean`), `profile` (`string`). Результат `boolean`. Реализация `const Fvector2 pos = Fvector2().set(x, y); return self->InitScrollBar(pos, isHorizontal, profile);`. | [UIScrollBar_script.cpp:40](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L40) |
| `CUIScrollBar:InitScrollBarStretch(x, y, length, isHorizontal, profile)` | Метод | Инициализирует полосу прокрутки растягиваемый. Аргументы `x` (`number`), `y` (`number`), `length` (`number`), `isHorizontal` (`boolean`), `profile` (`string`). Результат `boolean`. Реализация `const Fvector2 pos = Fvector2().set(x, y); return self->InitScrollBar(pos, length, isHorizontal, profile);`. | [UIScrollBar_script.cpp:39](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L39) |
| `CUIScrollBar:IsFixedLayout()` | Метод | Проверяет, использует ли полоса фиксированную компоновку. Результат `boolean`. Реализация `return _layoutMode == ScrollLayoutMode::Fixed;`. | [UIScrollBar_script.cpp:53](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L53) |
| `CUIScrollBar:IsInitialized()` | Метод | Проверяет готовность полосы прокрутки после инициализации. Результат `boolean`. | [UIScrollBar_script.cpp:54](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L54) |
| `CUIScrollBar:Refresh()` | Метод | Пересчитывает отображение полосы после изменения состояния прокрутки. Реализация `SyncThumbFromScrollPos();`. | [UIScrollBar_script.cpp:52](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L52) |
| `CUIScrollBar:SetEnabled(enabled)` | Метод | Задаёт включения. Аргументы `enabled` (`boolean`). Реализация `_enabled = enabled; if (!_enabled) { Show(_enabled); }`. | [UIScrollBar_script.cpp:55](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L55) |
| `CUIScrollBar:SetPageSize(page)` | Метод | Задаёт размер страницы. Аргументы `page` (`number`). Реализация `_pageSize = std::max(0, page); UpdateScrollBar();`. | [UIScrollBar_script.cpp:46](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L46) |
| `CUIScrollBar:SetRange(minPos, maxPos)` | Метод | Задаёт диапазон. Аргументы `minPos` (`number`), `maxPos` (`number`). | [UIScrollBar_script.cpp:41](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L41) |
| `CUIScrollBar:SetScrollPos(pos)` | Метод | Задаёт позицию прокрутки. Аргументы `pos` (`number`). Реализация `SetScrollPosClamped(pos); UpdateScrollBar();`. | [UIScrollBar_script.cpp:44](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L44) |
| `CUIScrollBar:SetStepSize(step)` | Метод | Задаёт величину шага. Аргументы `step` (`number`). Реализация `_stepSize = step; UpdateScrollBar();`. | [UIScrollBar_script.cpp:48](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L48) |
| `CUIScrollBar:TryScrollDec(byScrollbox)` | Метод | Пытается уменьшить позицию прокрутки. Аргумент выбирает прокрутку страницей или шагом. Аргументы `byScrollbox` (`boolean`). Реализация `if (ScrollDec(byScrollbox)) { NotifyScrollChanged(); }`. | [UIScrollBar_script.cpp:51](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L51) |
| `CUIScrollBar:TryScrollInc(byScrollbox)` | Метод | Пытается увеличить позицию прокрутки. Аргумент выбирает прокрутку страницей или шагом. Аргументы `byScrollbox` (`boolean`). Реализация `if (ScrollInc(byScrollbox)) { NotifyScrollChanged(); }`. | [UIScrollBar_script.cpp:50](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L50) |

### `CUIScrollView`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIScrollView:GetPadSize()` | Метод | Возвращает размер области содержимого. Результат `vector2`. Реализация `if(m_flags.test (eNeedRecalc) ) RecalcSize (); return m_pad->GetWndSize();`. | [UIWindow_script.cpp:258](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L258) |
| `CUIScrollView:HasWindow(item)` | Метод | Возвращает `true`, если окно находится внутри прокручиваемой области. | [UIWindow_script.cpp:259](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L259) |
| `CUIScrollView:ReinitScrollBar()` | Метод | Переинициализирует полосу прокрутки. Результат `boolean`. | [UIWindow_script.cpp:256](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L256) |
| `CUIScrollView:ScrollBar()` | Метод | Прокручивает полосы. Результат `CUIScrollBar`. | [UIWindow_script.cpp:255](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L255) |
| `CUIScrollView:ScrollToItem(item, offset)` | Метод | Прокручивает список к дочернему окну с целочисленным смещением по вертикали. | [UIWindow_script.cpp:260](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L260) |
| `CUIScrollView:SetFixedScrollBar(b)` | Метод | Задаёт фиксированный полосу прокрутки. Аргументы `b` (`boolean`). Реализация `m_flags.set(eFixedScrollBar, b);`. | [UIWindow_script.cpp:253](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L253) |
| `CUIScrollView:SetScrollBarProfile(profile)` | Метод | Задаёт профиль полосы прокрутки. Аргументы `profile` (`string`). Реализация `m_scrollbar_profile = profile;`. | [UIWindow_script.cpp:254](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L254) |

### `CUIStackPanel`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIStackPanel:IsAlignRight()` | Метод | Проверяет, включено ли выравнивание элементов панели по правому краю. Результат `boolean`. Реализация `return SPAlignment == eRight;`. | [UIWindow_script.cpp:230](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L230) |
| `CUIStackPanel:SetRightAlign(Val)` | Метод | Задаёт вправо выравнивание. Аргументы `Val` (`boolean`). Реализация `SPAlignment = Val ? eRight : eLeft;`. | [UIWindow_script.cpp:229](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L229) |

### `CUIStatic`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIStatic:AdjustHeightToText()` | Метод | Подгоняет высоту `to` текст. | [UIStatic_script.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L28) |
| `CUIStatic:AdjustWidthToText()` | Метод | Подгоняет ширину `to` текст. | [UIStatic_script.cpp:29](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L29) |
| `CUIStatic:GetColor()` | Метод | Возвращает цвет текстуры. Результат `number`. Реализация `return m_UIStaticItem.GetTextureColor();`. | [UIStatic_script.cpp:46](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L46) |
| `CUIStatic:GetFont()` | Метод | Возвращает шрифт виджета. Результат `CGameFont`. Реализация `return TextItemControl()->GetFont();`. | [UIStatic_script.cpp:80](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L80) |
| `CUIStatic:GetOriginalRect()` | Метод | Возвращает текстуры прямоугольник. Результат `Frect`. Реализация `return &m_UIStaticItem.GetTextureRect();`. | [UIStatic_script.cpp:64](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L64) |
| `CUIStatic:GetStretchTexture()` | Метод | Возвращает растягиваемый текстуры. Результат `boolean`. | [UIStatic_script.cpp:30](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L30), [UIStatic_script.cpp:67](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L67) |
| `CUIStatic:GetText()` | Метод | Возвращает текст. Результат `string`. Реализация `return TextItemControl()->GetText();`. | [UIStatic_script.cpp:36](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L36) |
| `CUIStatic:GetTextAlign()` | Метод | Возвращает выравнивание текста. Результат `number`. Реализация `return static_cast<u32>(TextItemControl()->GetTextAlignment());`. | [UIStatic_script.cpp:70](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L70) |
| `CUIStatic:GetTextColor()` | Метод | Возвращает цвет текста. Результат `number`. Реализация `return TextItemControl()->GetTextColor();`. | [UIStatic_script.cpp:50](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L50) |
| `CUIStatic:GetTextureColor()` | Метод | Возвращает цвет текстуры. Результат `number`. Реализация `return m_UIStaticItem.GetTextureColor();`. | [UIStatic_script.cpp:27](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L27) |
| `CUIStatic:GetTextX()` | Метод | Возвращает текст `x`. Результат `number`. Реализация `return TextItemControl()->m_TextOffset.x;`. | [UIStatic_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L42) |
| `CUIStatic:GetTextY()` | Метод | Возвращает текст `y`. Результат `number`. Реализация `return TextItemControl()->m_TextOffset.y;`. | [UIStatic_script.cpp:43](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L43) |
| `CUIStatic:InitTextureEx(texture, shader, fatal)`, `CUIStatic:InitTextureEx(texture, shader)` | Метод | Инициализирует текстуры `ex`. Результат `boolean`. | [UIStatic_script.cpp:54](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L54), [UIStatic_script.cpp:55](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L55) |
| `CUIStatic:ResetOriginalRect()` | Метод | Сбрасывает исходный прямоугольник. Реализация `m_UIStaticItem.ResetOriginalRect();`. | [UIStatic_script.cpp:56](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L56) |
| `CUIStatic:SetColor(color)` | Метод | Задаёт цвет текстуры. Аргументы `color` (`number`). Реализация `m_UIStaticItem.SetTextureColor(color);`. | [UIStatic_script.cpp:45](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L45) |
| `CUIStatic:SetElipsis(pos, indent)` | Метод | Задаёт `elipsis`. Аргументы `pos` (`number`), `indent` (`number`). Реализация `TextItemControl()->SetEllipsis(pos != 0);`. | [UIStatic_script.cpp:75](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L75) |
| `CUIStatic:SetEllipsis(mode)` | Метод | Задаёт `ellipsis`. Аргументы `mode` (`boolean`). Реализация `TextItemControl()->SetEllipsis(mode);`. | [UIStatic_script.cpp:76](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L76) |
| `CUIStatic:SetOriginalRect(pr)` | Метод | Задаёт исходный прямоугольник. Аргументы `pr` (`Frect`). Реализация `m_UIStaticItem.SetTextureRect(*pr);`. | [UIStatic_script.cpp:63](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L63) |
| `CUIStatic:SetTextAlign(align)` | Метод | Задаёт выравнивание текста. Аргументы `align` (`number`). | [UIStatic_script.cpp:69](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L69) |
| `CUIStatic:SetTextAlignment(al)` | Метод | Задаёт текст выравнивание. Аргументы `al` (`ETextAlignment`). Реализация `TextItemControl()->SetTextAlignment(al);`. | [UIStatic_script.cpp:77](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L77) |
| `CUIStatic:SetTextComplexMode(mode)` | Метод | Задаёт режим многострочной раскладки текста. Аргументы `mode` (`boolean`). Реализация `TextItemControl()->SetTextComplexMode(mode);`. | [UIStatic_script.cpp:79](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L79) |
| `CUIStatic:SetTextOffset(x, y)` | Метод | Задаёт смещение текста. Аргументы `x` (`number`), `y` (`number`). Реализация `TextItemControl()->m_TextOffset.x = x; TextItemControl()->m_TextOffset.y = y;`. | [UIStatic_script.cpp:81](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L81) |
| `CUIStatic:SetTextST(txt)` | Метод | Задаёт текст `st`. Аргументы `txt` (`string`). Реализация `TextItemControl()->SetTextST(txt);`. | [UIStatic_script.cpp:34](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L34) |
| `CUIStatic:SetTextureOffset(x, y)` | Метод | Задаёт смещение текстуры. Аргументы `x` (`number`), `y` (`number`). Реализация `m_TextureOffset.set(x, y);`. | [UIStatic_script.cpp:58](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L58) |
| `CUIStatic:SetTextX(x)` | Метод | Задаёт текст `x`. Аргументы `x` (`number`). Реализация `TextItemControl()->m_TextOffset.x = x;`. | [UIStatic_script.cpp:40](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L40) |
| `CUIStatic:SetTextY(y)` | Метод | Задаёт текст `y`. Аргументы `y` (`number`). Реализация `TextItemControl()->m_TextOffset.y = y;`. | [UIStatic_script.cpp:41](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L41) |
| `CUIStatic:SetVTextAlignment(al)` | Метод | Задаёт вертикальное выравнивание текста. Аргументы `al` (`EVTextAlignment`). Реализация `TextItemControl()->SetVTextAlignment(al);`. | [UIStatic_script.cpp:78](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L78) |

### `CUITabControl`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUITabControl:GetActiveIndex()` | Метод | Возвращает активный индекс. Результат `number`. | [UITabControl_script.cpp:23](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L23) |
| `CUITabControl:GetButtonByIndex(index)` | Метод | Возвращает кнопки `by` индекс. Аргументы `index` (`number`). Результат `CUITabButton`. | [UITabControl_script.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L28) |
| `CUITabControl:NextTab(bAllowLoop)` | Метод | Переходит к следующему значению. Аргументы `bAllowLoop` (`boolean`). | [UITabControl_script.cpp:29](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L29) |
| `CUITabControl:PrevTab(bAllowLoop)` | Метод | Переходит к предыдущему значению. Аргументы `bAllowLoop` (`boolean`). | [UITabControl_script.cpp:30](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L30) |
| `CUITabControl:RemoveItem(index)`, `CUITabControl:RemoveItem(id)` | Метод | Удаляет предмета. | [UITabControl_script.cpp:19](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L19), [UITabControl_script.cpp:20](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L20) |
| `CUITabControl:SetNewActiveTab(index)` | Метод | Задаёт `new` активный вкладки. Аргументы `index` (`number`). | [UITabControl_script.cpp:26](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L26) |

### `CUITrackBar`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUITrackBar:AssignProps(entry, group)` | Метод | Назначает `props`. Аргументы `entry` (`string`), `group` (`string`). Реализация `track->AssignProps(entry, group);`. | [UIButton_script.cpp:93](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L93) |
| `CUITrackBar:CurrentID()` | Метод | Возвращает ID текущего значения. Результат `number`. Реализация `return (m_i_val - 1);`. | [UIButton_script.cpp:91](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L91) |
| `CUITrackBar:GetInvert()` | Метод | Возвращает инверсию. Результат `boolean`. | [UIButton_script.cpp:97](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L97) |
| `CUITrackBar:SetCurrentID(val_id)` | Метод | Задаёт ID текущего значения. Аргументы `val_id` (`number`). Реализация `m_i_val = val_id + 1;`. | [UIButton_script.cpp:92](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L92) |
| `CUITrackBar:SetDrawingValue(enabled)` | Метод | Управляет выводом текущего значения рядом с ползунком. | [UIButton_script.cpp:99](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L99) |
| `CUITrackBar:SetFValue(val)` | Метод | Задаёт дробное значение. Аргументы `val` (`number`). Реализация `m_f_val = val; UpdatePos();`. | [UIButton_script.cpp:87](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L87) |
| `CUITrackBar:SetHighlighted(value)` | Метод | Задаёт подсветку. Аргументы `value` (`boolean`). Реализация `m_pSlider->SetHighlighted(value);`. | [UIButton_script.cpp:103](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L103) |
| `CUITrackBar:SetInvert(v)` | Метод | Задаёт инверсию. Аргументы `v` (`boolean`). Реализация `m_b_invert=v;`. | [UIButton_script.cpp:98](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L98) |
| `CUITrackBar:SetIValue(val)` | Метод | Задаёт целочисленное значение. Аргументы `val` (`number`). Реализация `m_i_val = val; UpdatePos();`. | [UIButton_script.cpp:85](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L85) |
| `CUITrackBar:SetMagnitude(magnitude)` | Метод | Задаёт `magnitude`. Аргументы `magnitude` (`number`). Реализация `m_f_magnitude = magnitude;`. | [UIButton_script.cpp:95](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L95) |
| `CUITrackBar:SetNumOfSigns(num_of_signs)` | Метод | Задаёт число отображаемых знаков после запятой. Аргументы `num_of_signs` (`number`). Реализация `m_i_num_of_signs = num_of_signs;`. | [UIButton_script.cpp:96](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L96) |
| `CUITrackBar:SetStep(step)` | Метод | Задаёт шага. Аргументы `step` (`number`). Реализация `if (IsFltMode()) m_f_step = step; else m_i_step = iFloor(step);`. | [UIButton_script.cpp:94](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L94) |
| `CUITrackBar:SetSystemDepends(depend)` | Метод | Задаёт системы `depends`. Аргументы `depend` (`number`). Реализация `track->SetSystemDepends((CUIOptionsItem::ESystemDepends)depend);`. | [UIButton_script.cpp:100](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L100) |
| `CUITrackBar:StepLeft()` | Метод | Сдвигает ползунок на один шаг влево. | [UIButton_script.cpp:101](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L101) |
| `CUITrackBar:StepRight()` | Метод | Сдвигает ползунок на один шаг вправо. | [UIButton_script.cpp:102](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L102) |

### `CUIWindow`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIWindow:FindStatic(name)` | Метод | Ищет окно по имени среди текущего узла и его потомков. Возвращает `CUIStatic` или `nil` при отсутствии или несовпадении типа. | [UIWindow_script.cpp:196](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L196) |
| `CUIWindow:FindWindow(name)` | Метод | Ищет окно по имени среди текущего узла и его потомков. Возвращает `CUIWindow` или `nil`. | [UIWindow_script.cpp:195](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L195) |
| `CUIWindow:FocusReceiveTime()` | Метод | Возвращает фокуса `receive` время. Результат `number`. | [UIWindow_script.cpp:147](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L147) |
| `CUIWindow:GetAbsoluteRect(out_rect)` | Метод | Записывает абсолютный прямоугольник окна в переданный объект `Frect`. Возвращаемого значения нет. | [UIWindow_script.cpp:148](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L148) |
| `CUIWindow:GetFont()` | Метод | Возвращает шрифт виджета. Результат `CGameFont`. Реализация `if(m_pFont) return m_pFont; if(m_pParentWnd== NULL) return m_pFont; else return m_pParentWnd->GetFont();`. | [UIWindow_script.cpp:188](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L188) |
| `CUIWindow:Init(x, y, width, height)`, `CUIWindow:Init(rect)` | Метод | Задаёт начальные координаты и размеры окна. Перегрузка принимает готовый прямоугольник. Реализация `self->SetWndPos(Fvector2().set(x, y)); self->SetWndSize(Fvector2().set(width, height));`. | [UIWindow_script.cpp:150](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L150), [UIWindow_script.cpp:155](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L155) |
| `CUIWindow:SetHeight(height)` | Метод | Задаёт высоту окна в координатах интерфейса. | [UIWindow_script.cpp:181](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L181) |
| `CUIWindow:SetWidth(width)` | Метод | Задаёт ширину окна в координатах интерфейса. | [UIWindow_script.cpp:179](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L179) |

### `CUIZoneMap`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `CUIZoneMap.disabled` | Поле для чтения и записи | Виджет HUD или его параметр «отключения». Доступно для чтения и записи. | [UIActorMenu_script.cpp:460](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L460) |
| `CUIZoneMap:Background()` | Метод | Возвращает фона. Результат `CUIStatic`. | [UIActorMenu_script.cpp:463](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L463) |
| `CUIZoneMap:MapFrame()` | Метод | Возвращает карты рамки. Результат `CUIWindow`. | [UIActorMenu_script.cpp:462](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L462) |

### `EDDListType`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `EDDListType.iActorBag` | Константа | Тип списка перетаскивания `iActorBag` в интерфейсе инвентаря. | [UIActorMenu_script.cpp:382](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L382) |
| `EDDListType.iActorBelt` | Константа | Тип списка перетаскивания `iActorBelt` в интерфейсе инвентаря. | [UIActorMenu_script.cpp:383](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L383) |
| `EDDListType.iActorSlot` | Константа | Тип списка перетаскивания `iActorSlot` в интерфейсе инвентаря. | [UIActorMenu_script.cpp:384](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L384) |
| `EDDListType.iActorTrade` | Константа | Тип списка перетаскивания `iActorTrade` в интерфейсе инвентаря. | [UIActorMenu_script.cpp:385](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L385) |
| `EDDListType.iDeadBodyBag` | Константа | Тип списка перетаскивания `iDeadBodyBag` в интерфейсе инвентаря. | [UIActorMenu_script.cpp:386](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L386) |
| `EDDListType.iInvalid` | Константа | Тип списка перетаскивания `iInvalid` в интерфейсе инвентаря. | [UIActorMenu_script.cpp:387](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L387) |
| `EDDListType.iPartnerTrade` | Константа | Тип списка перетаскивания `iPartnerTrade` в интерфейсе инвентаря. | [UIActorMenu_script.cpp:388](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L388) |
| `EDDListType.iPartnerTradeBag` | Константа | Тип списка перетаскивания `iPartnerTradeBag` в интерфейсе инвентаря. | [UIActorMenu_script.cpp:389](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L389) |
| `EDDListType.iQuickSlot` | Константа | Тип списка перетаскивания `iQuickSlot` в интерфейсе инвентаря. | [UIActorMenu_script.cpp:390](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L390) |
| `EDDListType.iTrashSlot` | Константа | Тип списка перетаскивания `iTrashSlot` в интерфейсе инвентаря. | [UIActorMenu_script.cpp:391](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L391) |

### `FactionState`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `FactionState.actor_goodwill` | Свойство | Свойство объекта `FactionState`. Возвращает актёра `goodwill`. Результат `number`. | [FactionState.cpp:72](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L72) |
| `FactionState.bonus` | Поле для чтения и записи | Хранит `bonus`. Доступно для чтения и записи. | [FactionState.cpp:69](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L69) |
| `FactionState.faction_id` | Свойство | Свойство объекта `FactionState`. Возвращает `faction` идентификатор. Результат `string`. | [FactionState.cpp:71](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L71) |
| `FactionState.icon_big` | Свойство | Свойство объекта `FactionState`. Возвращает значка `big`. Результат `string`. | [FactionState.cpp:76](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L76) |
| `FactionState.location` | Свойство | Свойство объекта `FactionState`. Возвращает локации. Результат `string`. | [FactionState.cpp:79](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L79) |
| `FactionState.member_count` | Поле для чтения и записи | Хранит участника количество. Доступно для чтения и записи. | [FactionState.cpp:66](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L66) |
| `FactionState.power` | Поле для чтения и записи | Хранит выносливость. Доступно для чтения и записи. | [FactionState.cpp:68](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L68) |
| `FactionState.resource` | Поле для чтения и записи | Хранит `resource`. Доступно для чтения и записи. | [FactionState.cpp:67](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L67) |
| `FactionState.target_desc` | Свойство | Свойство объекта `FactionState`. Возвращает цели `desc`. Результат `string`. | [FactionState.cpp:78](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L78) |
| `FactionState.war_state1` | Свойство | Свойство объекта `FactionState`. Возвращает `war` `state1`. Результат `string`. | [FactionState.cpp:81](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L81) |
| `FactionState.war_state2` | Свойство | Свойство объекта `FactionState`. Возвращает `war` `state2`. Результат `string`. | [FactionState.cpp:82](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L82) |
| `FactionState.war_state3` | Свойство | Свойство объекта `FactionState`. Возвращает `war` `state3`. Результат `string`. | [FactionState.cpp:83](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L83) |
| `FactionState.war_state4` | Свойство | Свойство объекта `FactionState`. Возвращает `war` `state4`. Результат `string`. | [FactionState.cpp:84](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L84) |
| `FactionState.war_state5` | Свойство | Свойство объекта `FactionState`. Возвращает `war` `state5`. Результат `string`. | [FactionState.cpp:85](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L85) |
| `FactionState.war_state_hint1` | Свойство | Свойство объекта `FactionState`. Возвращает `war` состояние `hint1`. Результат `string`. | [FactionState.cpp:87](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L87) |
| `FactionState.war_state_hint2` | Свойство | Свойство объекта `FactionState`. Возвращает `war` состояние `hint2`. Результат `string`. | [FactionState.cpp:88](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L88) |
| `FactionState.war_state_hint3` | Свойство | Свойство объекта `FactionState`. Возвращает `war` состояние `hint3`. Результат `string`. | [FactionState.cpp:89](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L89) |
| `FactionState.war_state_hint4` | Свойство | Свойство объекта `FactionState`. Возвращает `war` состояние `hint4`. Результат `string`. | [FactionState.cpp:90](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L90) |
| `FactionState.war_state_hint5` | Свойство | Свойство объекта `FactionState`. Возвращает `war` состояние `hint5`. Результат `string`. | [FactionState.cpp:91](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L91) |

### `GAME_TYPE`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `GAME_TYPE.eGameIDFreeMp` | Константа | Значение перечисления `GAME_TYPE` для режима «игровой идентификатор `free` `mp`». `eGameIDFreeMP`. | [UIMapList.cpp:553](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIMapList.cpp#L553) |
| `GAME_TYPE.GAME_ARTEFACTHUNT` | Константа | Значение перечисления `GAME_TYPE` для режима «игровой `artefacthunt`». `eGameIDArtefactHunt`. | [UIMapList.cpp:551](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIMapList.cpp#L551) |
| `GAME_TYPE.GAME_DEATHMATCH` | Константа | Значение перечисления `GAME_TYPE` для режима «игровой `deathmatch`». `eGameIDDeathmatch`. | [UIMapList.cpp:549](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIMapList.cpp#L549) |
| `GAME_TYPE.GAME_TEAMDEATHMATCH` | Константа | Значение перечисления `GAME_TYPE` для режима «игровой `teamdeathmatch`». `eGameIDTeamDeathmatch`. | [UIMapList.cpp:550](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIMapList.cpp#L550) |

### `main_menu`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `main_menu.in_controller_mode()` | Функция | Возвращает `in` контроллера режим. Результат `boolean`. Реализация `return pInput->GetControllerMode();`. | [ui_export_script.cpp:122](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L122) |

### `system_depends`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `system_depends.sdApplyOnChange` | Константа | Режим применения настройки `sdApplyOnChange` для элемента интерфейса. | [UIOptionsManagerScript.cpp:66](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIOptionsManagerScript.cpp#L66) |
| `system_depends.sdNothing` | Константа | Режим применения настройки `sdNothing` для элемента интерфейса. | [UIOptionsManagerScript.cpp:62](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIOptionsManagerScript.cpp#L62) |
| `system_depends.sdSndRestart` | Константа | Режим применения настройки `sdSndRestart` для элемента интерфейса. | [UIOptionsManagerScript.cpp:65](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIOptionsManagerScript.cpp#L65) |
| `system_depends.sdSystemRestart` | Константа | Режим применения настройки `sdSystemRestart` для элемента интерфейса. | [UIOptionsManagerScript.cpp:64](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIOptionsManagerScript.cpp#L64) |
| `system_depends.sdVidRestart` | Константа | Режим применения настройки `sdVidRestart` для элемента интерфейса. | [UIOptionsManagerScript.cpp:63](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIOptionsManagerScript.cpp#L63) |

### `TEX_INFO`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `TEX_INFO:get_file_name()` | Метод | Возвращает файла имя. Результат `string`. | [UIWindow_script.cpp:132](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L132) |
| `TEX_INFO:get_rect()` | Метод | Возвращает прямоугольник. Результат `Frect`. | [UIWindow_script.cpp:133](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L133) |

### `ui`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `ui.gamepad_prefix()` | Функция | Возвращает геймпада `prefix`. Результат `string`. Реализация `return pInput->GamepadPrefix();`. | [ui_export_script.cpp:128](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L128) |
| `ui.get_current_kx()` | Функция | Возвращает текущий `kx`. Результат `number`. Реализация `return UI().get_current_kx();`. | [ui_export_script.cpp:126](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L126) |
| `ui.is_widescreen()` | Функция | Проверяет, использует ли интерфейс широкоформатную раскладку. Результат `boolean`. Реализация `return UI().is_widescreen();`. | [ui_export_script.cpp:127](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L127) |
| `ui.QueryScrollBarProfileLayout(profile, isHorizontal)` | Функция | Запрашивает профиль полосы прокрутки компоновки. Аргументы `profile` (`string`), `isHorizontal` (`boolean`). Результат `number`. | [ui_export_script.cpp:129](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L129) |

### `ui_events`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `ui_events.LIST_ITEM_UNSELECT` | Константа | Код сообщения виджета «списка предмета `unselect`» для обработчика UI. | [UIWindow_script.cpp:301](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L301) |
| `ui_events.TRACK_VALUE_CHANGED` | Константа | Код сообщения виджета «`track` значение `changed`» для обработчика UI. | [UIWindow_script.cpp:319](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L319) |

### `UIHint`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `UIHint:GetHintText()` | Метод | Возвращает подсказку текст. Результат `string`. Реализация `return m_text->GetText();`. | [UIWindow_script.cpp:241](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L241) |
| `UIHint:SetHeight(height)` | Метод | Задаёт высоту окна в координатах интерфейса. | [UIWindow_script.cpp:239](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L239) |
| `UIHint:SetHintText(text)` | Метод | Задаёт подсказку текст. Аргументы `text` (`string`). | [UIWindow_script.cpp:240](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L240) |
| `UIHint:SetWidth(width)` | Метод | Задаёт ширину окна в координатах интерфейса. | [UIWindow_script.cpp:238](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L238) |

## См. также

- [Новые экспорты Lua](../lua-exports.md)
