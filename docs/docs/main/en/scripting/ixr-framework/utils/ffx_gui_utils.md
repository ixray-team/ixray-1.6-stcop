# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0


### ffx_gui_utils: `\gamedata\scripts\ixr_framework\utils\ffx_gui_utils.script`
Utilities for graphical interfaces (GUI) and dialogs:
* `run_gui`

---

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_gui_utils.script]
--// Open a GUI dialog, optionally hiding the inventory and weapon.
run_gui(gui, close_inv)
args:
  gui (table/object) - GUI object that must provide a ShowDialog method
  close_inv (boolean, optional) - if true, hide the inventory menu with game_hide_menu() and hide the weapon with level.show_weapon(false)
retval: (none)
```
:::

### Usage examples:
```lua
-- Open a GUI without hiding the inventory.
local my_gui = some_gui_object  -- assume this is a ScriptWnd object
ffx_gui_utils.run_gui(my_gui, false)

-- Automatically hide the inventory and weapon.
ffx_gui_utils.run_gui(my_gui, true)
```
