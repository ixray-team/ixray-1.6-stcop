# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0


### ffx_ltx_utils: `\gamedata\scripts\ixr_framework\utils\ffx_ltx_utils.script`
Utilities for `.ltx` configuration files through the system INI object `system_ini()`, and for arbitrary INI objects.

---

#### Method descriptions (in one block):

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_ltx_utils.script]
--// Check whether a section exists in the global INI, with caching.
has_section(section)
args: section (string) - section name
retval: (boolean) - true if the section exists; otherwise false

--// Check whether a parameter exists in a section (no caching; unsafe if the section does not exist).
has_line(section, parameter)
args: section (string) - section name, parameter (string) - parameter name
retval: (boolean) - true if the parameter exists; otherwise false

--// Check whether a parameter exists after first checking the section (safe).
has_line_in_section(section, parameter)
args: section (string) - section name, parameter (string) - parameter name
retval: (boolean) - true if the section and parameter exist; otherwise false

--// Get a boolean parameter value with a default.
cfg_get_bool(section, parameter, def_value)
args: section (string) - section name, parameter (string) - parameter name, def_value (boolean) - default value
retval: (boolean) - parameter value or def_value

--// Get a string parameter value with a default.
cfg_get_string(section, parameter, def_value)
args: section (string) - section name, parameter (string) - parameter name, def_value (string) - default value
retval: (string) - parameter value or def_value

--// Get a numeric (float) parameter value with a default.
cfg_get_float(section, parameter, def_value)
args: section (string) - section name, parameter (string) - parameter name, def_value (number) - default value
retval: (number) - parameter value or def_value

--// Parse a parameter containing comma-separated strings into a table of strings.
cfg_parse_separated_strings(section, parameter, def_value)
args: section (string) - section name, parameter (string) - parameter name, def_value (any) - default value returned if the parameter is missing or cannot be parsed
retval: (table|any) - table of strings without separators, or def_value on error

--// Parse a parameter containing comma-separated numbers into a table of numbers.
cfg_parse_separated_numbers(section, parameter, def_value)
args: section (string) - section name, parameter (string) - parameter name, def_value (any) - default value
retval: (table|any) - table of numbers, or def_value on error

--// Parse a parameter containing comma-separated boolean values (true/false, 1/0, yes/no, on/off) into a table of booleans.
cfg_parse_separated_bools(section, parameter, def_value)
args: section (string) - section name, parameter (string) - parameter name, def_value (any) - default value
retval: (table|any) - table of booleans, or def_value on error

--// Translate a string using game.translate_string.
translate(str)
args: str (string) - string to translate
retval: (string) - translated string, or the original if no translation is found

--// Get a translated section name using inv_name if present; otherwise return the section name.
get_translated_section_name(real_sect)
args: real_sect (string) - section name
retval: (string) - translated inv_name, or the original section name

--// Parse all entries in an INI section into a table of the form {[key] = value, ...}.
parse_section_to_array(ini, section)
args: ini (table) - INI object from system_ini() or another source, section (string) - section name
retval: (table) - table of key-value pairs with trimmed values, or nil if the section does not exist

--// Check whether a section exists in the supplied INI object.
ini_section_exists(_ini, section)
args: _ini (table) - INI-INI object, section (string) - section name
retval: (boolean) - true if the section exists; otherwise false

--// Check whether a parameter exists in a section in the supplied INI object.
ini_line_exists(_ini, section, parameter)
args: _ini (table) - INI-INI object, section (string) - section name, parameter (string) - parameter name
retval: (boolean) - true if the parameter exists; otherwise false

--// Check whether a parameter exists after checking the section in the supplied INI object.
ini_has_line_in_section(_ini, section, parameter)
args: _ini (table) - INI-INI object, section (string) - section name, parameter (string) - parameter name
retval: (boolean) - true if the section and parameter exist; otherwise false

--// Get a string parameter value from the supplied INI object with a default.
ini_get_string(_ini, section, parameter, def_value)
args: _ini (table) - INI-INI object, section (string) - section name, parameter (string) - parameter name, def_value (string) - default value
retval: (string) - parameter value or def_value

--// Parse a comma-separated string parameter from the supplied INI object into a table of strings.
ini_parse_separated(_ini, section, parameter, def_value)
args: _ini (table) - INI-INI object, section (string) - section name, parameter (string) - parameter name, def_value (any) - default value
retval: (table|any) - table of trimmed strings, or def_value on error
```
:::

### Usage examples:
```lua
--// Check whether a section exists.
if ffx_ltx_utils.has_section("game_info") then
    SemiLog(string.format("Section game_info exists"))
end

--// Get a parameter with a default.
local enable = ffx_ltx_utils.cfg_get_bool("video", "fullscreen", true)
local name = ffx_ltx_utils.cfg_get_string("profile", "nick", "Player")
local volume = ffx_ltx_utils.cfg_get_float("sound", "music_vol", 0.5)

--// Parse lists.
local weapons = ffx_ltx_utils.cfg_parse_separated_strings("inventory", "weapons", {})
for _, w in ipairs(weapons) do
    SemiLog(string.format("Weapon: %s", w))
end

local coords = ffx_ltx_utils.cfg_parse_separated_numbers("location", "coords", {})
if #coords >= 3 then
    local x, y, z = coords[1], coords[2], coords[3]
    SemiLog(string.format("Coords: %f, %f, %f", x, y, z))
end

local flags = ffx_ltx_utils.cfg_parse_separated_bools("options", "enabled_flags", {})
if flags[1] then
    SemiLog("The first flag is enabled")
end

--// Translate a string.
local translated = ffx_ltx_utils.translate("st_hello_world")
SemiLog(string.format("Translation: %s", translated))

--// Translate a section name.
local sect_name = ffx_ltx_utils.get_translated_section_name("wpn_ak74")
SemiLog(string.format("Section name: %s", sect_name))

--// Parse an entire section into a table.
local ini = system_ini()
local data = ffx_ltx_utils.parse_section_to_array(ini, "dialog_manager")
if data then
    for key, val in pairs(data) do
        SemiLog(string.format("%s = %s", key, val))
    end
end

--// Work with a supplied INI object, such as another file.
local another_ini = ... --// loaded INI object
if ffx_ltx_utils.ini_section_exists(another_ini, "some_section") then
    local value = ffx_ltx_utils.ini_get_string(another_ini, "some_section", "param", "default")
    SemiLog(string.format("Value: %s", value))
end
```
