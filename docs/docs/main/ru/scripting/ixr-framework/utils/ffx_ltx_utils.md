# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0


### ffx_ltx_utils: `\gamedata\scripts\ixr_framework\utils\ffx_ltx_utils.script`
Утилиты для работы с конфигурационными файлами `.ltx` (через системный INI-объект `system_ini()`), а также с произвольными INI-объектами.

---

#### Описание методов (все в одном блоке):

```lua
--// Проверить существование секции в глобальном INI (с кэшированием).
has_section(section)
args: section (string) - имя секции
retval: (boolean) - true, если секция существует, иначе false

--// Проверить существование параметра в секции (без кэша, небезопасно, если секция не существует).
has_line(section, parameter)
args: section (string) - имя секции, parameter (string) - имя параметра
retval: (boolean) - true, если параметр существует, иначе false

--// Проверить существование параметра в секции с предварительной проверкой секции (безопасно).
has_line_in_section(section, parameter)
args: section (string) - имя секции, parameter (string) - имя параметра
retval: (boolean) - true, если секция и параметр существуют, иначе false

--// Получить булево значение параметра с указанием значения по умолчанию.
cfg_get_bool(section, parameter, def_value)
args: section (string) - имя секции, parameter (string) - имя параметра, def_value (boolean) - значение по умолчанию
retval: (boolean) - значение параметра или def_value

--// Получить строковое значение параметра с указанием значения по умолчанию.
cfg_get_string(section, parameter, def_value)
args: section (string) - имя секции, parameter (string) - имя параметра, def_value (string) - значение по умолчанию
retval: (string) - значение параметра или def_value

--// Получить числовое значение параметра (float) с указанием значения по умолчанию.
cfg_get_float(section, parameter, def_value)
args: section (string) - имя секции, parameter (string) - имя параметра, def_value (number) - значение по умолчанию
retval: (number) - значение параметра или def_value

--// Разобрать строку параметра, содержащую разделённые запятыми строки, в таблицу строк.
cfg_parse_separated_strings(section, parameter, def_value)
args: section (string) - имя секции, parameter (string) - имя параметра, def_value (any) - значение по умолчанию (возвращается, если параметр отсутствует или не удалось разобрать)
retval: (table|any) - таблица строк (без разделителей), либо def_value в случае ошибки

--// Разобрать строку параметра, содержащую разделённые запятыми числа, в таблицу чисел.
cfg_parse_separated_numbers(section, parameter, def_value)
args: section (string) - имя секции, parameter (string) - имя параметра, def_value (any) - значение по умолчанию
retval: (table|any) - таблица чисел, либо def_value в случае ошибки

--// Разобрать строку параметра, содержащую разделённые запятыми булевы значения (true/false, 1/0, yes/no, on/off), в таблицу булевых.
cfg_parse_separated_bools(section, parameter, def_value)
args: section (string) - имя секции, parameter (string) - имя параметра, def_value (any) - значение по умолчанию
retval: (table|any) - таблица булевых значений, либо def_value в случае ошибки

--// Перевести строку через игровой переводчик (game.translate_string).
translate(str)
args: str (string) - строка для перевода
retval: (string) - переведённая строка (или исходная, если перевод не найден)

--// Получить переведённое имя секции (использует параметр inv_name, если он есть, иначе возвращает имя секции).
get_translated_section_name(real_sect)
args: real_sect (string) - имя секции
retval: (string) - переведённое имя (из inv_name) или исходное имя секции

--// Разобрать все строки указанной секции INI-объекта в таблицу вида {[key] = value, ...}.
parse_section_to_array(ini, section)
args: ini (table) - объект INI (полученный через system_ini() или другой), section (string) - имя секции
retval: (table) - таблица с парами ключ-значение (значения обрезаются) или nil, если секция не существует

--// Проверить существование секции в переданном INI-объекте.
ini_section_exists(_ini, section)
args: _ini (table) - INI-объект, section (string) - имя секции
retval: (boolean) - true, если секция существует, иначе false

--// Проверить существование параметра в секции в переданном INI-объекте.
ini_line_exists(_ini, section, parameter)
args: _ini (table) - INI-объект, section (string) - имя секции, parameter (string) - имя параметра
retval: (boolean) - true, если параметр существует, иначе false

--// Проверить существование параметра в секции (с проверкой секции) в переданном INI-объекте.
ini_has_line_in_section(_ini, section, parameter)
args: _ini (table) - INI-объект, section (string) - имя секции, parameter (string) - имя параметра
retval: (boolean) - true, если секция и параметр существуют, иначе false

--// Получить строковое значение параметра из переданного INI-объекта с указанием значения по умолчанию.
ini_get_string(_ini, section, parameter, def_value)
args: _ini (table) - INI-объект, section (string) - имя секции, parameter (string) - имя параметра, def_value (string) - значение по умолчанию
retval: (string) - значение параметра или def_value

--// Разобрать строку параметра, содержащую разделённые запятыми строки, в таблицу строк (из переданного INI-объекта).
ini_parse_separated(_ini, section, parameter, def_value)
args: _ini (table) - INI-объект, section (string) - имя секции, parameter (string) - имя параметра, def_value (any) - значение по умолчанию
retval: (table|any) - таблица строк (обрезанных) или def_value в случае ошибки
```

### Примеры использований:
```lua
--// Проверка существования секции
if ffx_ltx_utils.has_section("game_info") then
    SemiLog(string.format("Секция game_info существует"))
end

--// Получение параметра с дефолтом
local enable = ffx_ltx_utils.cfg_get_bool("video", "fullscreen", true)
local name = ffx_ltx_utils.cfg_get_string("profile", "nick", "Player")
local volume = ffx_ltx_utils.cfg_get_float("sound", "music_vol", 0.5)

--// Парсинг списков
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
    SemiLog("Первый флаг включён")
end

--// Перевод
local translated = ffx_ltx_utils.translate("st_hello_world")
SemiLog(string.format("Перевод: %s", translated))

--// Перевод имени секции
local sect_name = ffx_ltx_utils.get_translated_section_name("wpn_ak74")
SemiLog(string.format("Имя секции: %s", sect_name))

--// Парсинг всей секции в таблицу
local ini = system_ini()
local data = ffx_ltx_utils.parse_section_to_array(ini, "dialog_manager")
if data then
    for key, val in pairs(data) do
        SemiLog(string.format("%s = %s", key, val))
    end
end

--// Работа с переданным INI-объектом (например, другой файл)
local another_ini = ... --// загруженный INI
if ffx_ltx_utils.ini_section_exists(another_ini, "some_section") then
    local value = ffx_ltx_utils.ini_get_string(another_ini, "some_section", "param", "default")
    SemiLog(string.format("Значение: %s", value))
end
```
