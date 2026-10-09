# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

### ffx_path_utils: `\gamedata\scripts\ixr_framework\utils\ffx_path_utils.script`
Utilities for working with file paths:
* `get_file_name`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_path_utils.script]
--// Extract the file name from a full path, optionally removing the extension.
get_file_name(file_path, remove_ext)
args:
  file_path (string)(required) - full file path, which may contain backslashes.
  remove_ext (boolean)(optional) - if true, remove the file extension (default false).
retval: (string) - extracted file name, with or without extension.
```
:::

### Usage examples:
```lua
--// Get the file name with its extension.
local full_name = ffx_path_utils.get_file_name("C:\\games\\scripts\\my_script.script")
SemiLog(full_name) --// "my_script.script"

--// Get the file name without its extension.
local name_no_ext = ffx_path_utils.get_file_name("C:\\games\\scripts\\my_script.script", true)
SemiLog(name_no_ext) --// "my_script"

--// Path without slashes.
local name = ffx_path_utils.get_file_name("config.ltx")
SemiLog(name) --// "config.ltx"

--// Remove the extension when there is no dot.
local name2 = ffx_path_utils.get_file_name("datafile", true)
SemiLog(name2) --// "datafile"
```
