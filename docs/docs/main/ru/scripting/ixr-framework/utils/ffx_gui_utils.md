# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0


### ffx_gui_utils: `\gamedata\scripts\ixr_framework\utils\ffx_gui_utils.script`
Утилиты для работы с графическим интерфейсом (GUI) и диалогами:
* `run_gui`

---

#### Описание методов:

```lua
--// Запустить GUI-диалог с возможностью скрыть инвентарь и оружие.
run_gui(gui, close_inv)
args:
  gui (table/object) - объект GUI (должен содержать метод ShowDialog)
  close_inv (boolean, optional) - если true, скрывает меню инвентаря (game_hide_menu()) и убирает отображение оружия (level.show_weapon(false))
retval: (none)
```

### Примеры использований:
```lua
-- Пример вызова GUI без скрытия инвентаря
local my_gui = some_gui_object  -- предположим, это объект ScriptWnd
ffx_gui_utils.run_gui(my_gui, false)

-- Пример с автоматическим скрытием инвентаря и оружия
ffx_gui_utils.run_gui(my_gui, true)
```
