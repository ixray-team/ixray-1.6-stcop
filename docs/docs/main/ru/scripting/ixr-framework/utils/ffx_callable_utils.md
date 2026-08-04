# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

### ffx_callable_utils: `\gamedata\scripts\ixr_framework\utils\ffx_callable_utils.script`
Утилиты для проверки наличия скриптов и функций, получения информации о стеке вызовов и безопасного выполнения кода в песочнице:
* `is_script_present_in_g_file`
* `has_script_function_exists`
* `is_script_callable_by_name`
* `is_function_callable_by_name`
* `is_function_args_count_equal_by_ref`
* `is_function_args_count_equal_by_name`
* `find_caller_source`
* `find_caller_source_tracy`
* `get_call_stack_trace`
* `sandbox`

#### Описание методов:

```lua
--// Проверить, существует ли глобальный скрипт с указанным именем.
is_script_present_in_g_file(script_name)
args:
  script_name (string)(required) - имя скрипта.
retval: (boolean) - true, если скрипт присутствует в _G, иначе false.

--// Проверить, существует ли функция с указанным именем в скрипте.
has_script_function_exists(script_name, function_name)
args:
  script_name (string)(required) - имя скрипта.
  function_name (string)(required) - имя функции.
retval: (boolean) - true, если функция существует в скрипте, иначе false.

--// Проверить, является ли глобальный скрипт вызываемым (существует и является таблицей).
is_script_callable_by_name(script_name)
args:
  script_name (string)(required) - имя скрипта.
retval: (boolean) - true, если скрипт является таблицей и доступен, иначе false.

--// Проверить, существует ли функция и является ли она вызываемой в указанном скрипте.
is_function_callable_by_name(script_name, function_name)
args:
  script_name (string)(required) - имя скрипта.
  function_name (string)(required) - имя функции.
retval: (boolean) - true, если функция существует и вызываема, иначе false.

--// Проверить, имеет ли ссылка на функцию ровно указанное количество аргументов.
is_function_args_count_equal_by_ref(_func, count_args)
args:
  _func (function)(required) - ссылка на функцию.
  count_args (number)(required) - ожидаемое количество аргументов.
retval: (boolean) - true, если количество совпадает, иначе false.

--// Проверить, имеет ли именованная функция в скрипте указанное количество аргументов.
is_function_args_count_equal_by_name(script_name, function_name, count_args)
args:
  script_name (string)(required) - имя скрипта.
  function_name (string)(required) - имя функции.
  count_args (number)(required) - ожидаемое количество аргументов.
retval: (boolean) - true, если количество совпадает, иначе false.

--// Найти источник (имя файла) вызывающего кода на заданном уровне стека.
find_caller_source(level)
args:
  level (number)(required) - уровень стека (0 — текущая функция, 1 — вызывающая и т.д.).
retval: (string) - идентификатор источника (путь к файлу без расширения) или "unknown", если не найден.

--// Найти источник (имя файла и, опционально, номера строк) для интеграции с Tracy.
find_caller_source_tracy(level, use_line)
args:
  level (number)(required) - уровень стека для проверки.
  use_line (boolean)(optional) - если true, в возвращаемую таблицу добавляются поля line_begin и line_end (по умолчанию false).
retval: (table) - таблица с полем file_name (строка) и, если use_line=true, полями line_begin и line_end.

--// Получить строковое представление текущего стека вызовов.
get_call_stack_trace()
args:
  (none)
retval: (string) - стек вызовов, разделённый " -> ", например "script1.func1(...) -> script2.func2(...)".

--// Безопасно выполнить функцию из указанного скрипта в песочнице (с pcall). Возвращает результат или nil при ошибке.
sandbox(file_name, function_name, ...)
args:
  file_name (string)(required) - имя файла скрипта (без расширения).
  function_name (string)(required) - имя функции для вызова.
  ... (any)(optional) - аргументы, передаваемые в функцию.
retval: (any) - возвращаемые значения вызванной функции или nil в случае ошибки.
```

Примеры использований:
```lua
--// Проверяем наличие скрипта
if ffx_callable_utils.is_script_present_in_g_file("my_script") then
    SemiLog("Скрипт my_script загружен")
end

--// Проверяем существование функции в скрипте
local has_func = ffx_callable_utils.has_script_function_exists("my_script", "do_something")
if has_func then
    SemiLog("Функция do_something существует")
end

--// Проверяем, вызываема ли функция
if ffx_callable_utils.is_function_callable_by_name("my_script", "do_something") then
    my_script.do_something()
end

--// Проверяем количество аргументов функции по ссылке
local func_ref = function(a, b) return a + b end
local ok = ffx_callable_utils.is_function_args_count_equal_by_ref(func_ref, 2)
SemiLog(string.format("Ожидается 2 аргумента: %s", tostring(ok)))

--// Проверяем количество аргументов функции по имени
local equal = ffx_callable_utils.is_function_args_count_equal_by_name("my_script", "do_something", 3)

--// Получаем имя файла вызывающего кода
local caller_file = ffx_callable_utils.find_caller_source(2)
SemiLog(string.format("Вызвано из: %s", caller_file))

--// Получаем информацию для Tracy
local tracy_data = ffx_callable_utils.find_caller_source_tracy(3, true)
SemiLog(string.format("[%s (%d,%d)]", tracy_data.file_name, tracy_data.line_begin, tracy_data.line_end))

--// Получаем полный стек вызовов
local trace = ffx_callable_utils.get_call_stack_trace()
SemiLog(string.format("Стек: %s", trace))

--// Безопасно вызываем функцию из другого скрипта
local result = ffx_callable_utils.sandbox("math_utils", "add", 5, 3)
if result then
    SemiLog(string.format("Результат: %s", tostring(result)))
else
    SemiLog("Ошибка выполнения")
end
```
