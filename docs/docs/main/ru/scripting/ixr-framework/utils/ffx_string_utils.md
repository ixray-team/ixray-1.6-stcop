# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

### ffx_string_utils: `\gamedata\scripts\ixr_framework\utils\ffx_string_utils.script`
Утилиты для работы со строками (поиск, замена, разбиение, обрезка, регистр и т.д.):
* `get_length`
* `trim`
* `contains`
* `split`
* `start_with`
* `end_with`
* `collapse`
* `explode`
* `escape_regex_special_chars`
* `chunk`
* `ucfirst`
* `lcfirst`
* `normalize_spaces`
* `replace`
* `remove_substring`

#### Описание методов:

```lua
--// Возвращает длину строки, или 0, если входные данные nil.
get_length(str)
args:
  str (string|nil)(optional) - строка для измерения.
retval: (number) - длина строки или 0.

--// Удаляет пробельные символы в начале и конце строки.
trim(str)
args:
  str (string)(required) - строка для обработки.
retval: (string) - обрезанная строка, или пустая строка, если входные данные nil/пустые.

--// Проверяет, содержит ли строка указанную подстроку.
contains(str, substr)
args:
  str (string)(required) - строка, в которой искать.
  substr (string)(required) - искомая подстрока.
retval: (boolean) - true, если подстрока найдена, иначе false.

--// Разбивает строку по подстроке (обёртка для explode). Обрезает части и включает остаток.
split(text, substr)
args:
  text (string)(required) - строка для разбиения.
  substr (string)(required) - разделитель.
retval: (table) - массив обрезанных сегментов.

--// Проверяет, начинается ли строка с указанного префикса.
start_with(text, prefix)
args:
  text (string)(required) - проверяемая строка.
  prefix (string)(required) - префикс для поиска.
retval: (boolean) - true, если строка начинается с префикса, иначе false.

--// Проверяет, заканчивается ли строка указанным суффиксом.
end_with(text, suffix)
args:
  text (string)(required) - проверяемая строка.
  suffix (string)(required) - суффикс для поиска.
retval: (boolean) - true, если строка заканчивается суффиксом, иначе false.

--// Схлопывает повторяющиеся вхождения подстроки в одно.
collapse(text, substr)
args:
  text (string)(required) - исходная строка.
  substr (string)(required) - подстрока для схлопывания.
retval: (string) - строка с заменой повторяющихся подстрок на одно вхождение.

--// Разбивает строку по подстроке, обрезает части и включает остаток.
explode(str, substr)
args:
  str (string)(required) - строка для разбиения.
  substr (string)(required) - разделитель.
retval: (table) - массив обрезанных сегментов.

--// Экранирует спецсимволы регулярных выражений для использования в качестве литерального шаблона.
escape_regex_special_chars(str)
args:
  str (string)(required) - строка для экранирования.
retval: (string) - экранированная строка (спецсимволы с префиксом %).

--// Разбивает строку на фрагменты заданного размера.
chunk(text, size)
args:
  text (string)(required) - строка для разбиения.
  size (number)(required) - размер каждого фрагмента (если <= 0, используется 1).
retval: (table) - массив фрагментов строки.

--// Преобразует первый символ строки в верхний регистр.
ucfirst(text)
args:
  text (string)(required) - исходная строка.
retval: (string) - строка с заглавной первой буквой, или пустая строка, если входные данные nil/пустые.

--// Преобразует первый символ строки в нижний регистр.
lcfirst(text)
args:
  text (string)(required) - исходная строка.
retval: (string) - строка со строчной первой буквой, или пустая строка, если входные данные nil/пустые.

--// Заменяет множественные пробельные символы на один пробел.
normalize_spaces(text)
args:
  text (string)(required) - исходная строка.
retval: (string) - строка с нормализованными пробелами.

--// Заменяет все вхождения подстроки на другую подстроку.
replace(text, search, replace)
args:
  text (string)(required) - исходная строка.
  search (string)(required) - искомая подстрока.
  replace (string)(optional) - подстрока для замены (по умолчанию "").
retval: (string) - строка со всеми заменами.

--// Удаляет все вхождения подстроки из строки.
remove_substring(text, substring)
args:
  text (string)(required) - исходная строка.
  substring (string)(required) - подстрока для удаления.
retval: (string) - строка без указанных вхождений.
```

### Примеры использований:
```lua
--// Длина строки
local len = ffx_string_utils.get_length("Hello") --// 5
SemiLog(string.format("Длина: %d", len))

--// Обрезка
local trimmed = ffx_string_utils.trim("  text  ") --// "text"

--// Проверка содержания
local has = ffx_string_utils.contains("hello world", "world") --// true

--// Разбиение
local parts = ffx_string_utils.split("one,two,three", ",")
--// parts = {"one", "two", "three"}

--// Проверка начала/конца
if ffx_string_utils.start_with("Hello", "He") then
    SemiLog("Начинается с He")
end

if ffx_string_utils.end_with("Hello", "lo") then
    SemiLog("Заканчивается на lo")
end

--// Схлопывание
local collapsed = ffx_string_utils.collapse("a---b---c", "---") --// "a-b-c"

--// Экранирование спецсимволов
local escaped = ffx_string_utils.escape_regex_special_chars("(test)") --// "%(test%)"

--// Разбиение на чанки
local chunks = ffx_string_utils.chunk("abcdef", 2) --// {"ab", "cd", "ef"}

--// Регистр первой буквы
local upper = ffx_string_utils.ucfirst("hello") --// "Hello"
local lower = ffx_string_utils.lcfirst("HELLO") --// "hELLO"

--// Нормализация пробелов
local normalized = ffx_string_utils.normalize_spaces("a  b   c") --// "a b c"

--// Замена
local replaced = ffx_string_utils.replace("hello world", "world", "there") --// "hello there"

--// Удаление подстроки
local removed = ffx_string_utils.remove_substring("abc123abc", "abc") --// "123"
```
