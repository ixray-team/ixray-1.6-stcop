# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0


### ffx_compare_utils: `\gamedata\scripts\ixr_framework\utils\ffx_compare_utils.script`
Утилиты для проверки типов и сравнения:
* `is_table`
* `is_empty_flat_or_assoc_table`
* `is_function`
* `is_userdata`
* `is_not_empty_string`
* `is_empty_string`
* `is_number`
* `has_pattern`

#### Описание методов:
```lua
--// Проверить, является ли объект таблицей.
is_table(object)
args:
  object (any) - проверяемый объект
retval: (boolean) - true, если объект - таблица, иначе false

--// Проверить, пуста ли таблица на корневом уровне (не рекурсивно). Если передан не-таблица, возвращает true.
is_empty_flat_or_assoc_table(input_table)
args:
  input_table (table) - проверяемая таблица
retval: (boolean) - true, если таблица пуста или это не таблица, иначе false

--// Проверить, является ли объект функцией.
is_function(object)
args:
  object (any) - проверяемый объект
retval: (boolean) - true, если объект - функция, иначе false

--// Проверить, является ли объект пользовательскими данными (userdata).
is_userdata(object)
args:
  object (any) - проверяемый объект
retval: (boolean) - true, если объект - userdata, иначе false

--// Проверить, что строка не nil и не пуста (после приведения к строке).
is_not_empty_string(str)
args:
  str (string) - проверяемая строка
retval: (boolean) - true, если строка не пуста (и не nil), иначе false

--// Проверить, что строка равна nil или пуста (после приведения к строке).
is_empty_string(str)
args:
  str (string) - проверяемая строка
retval: (boolean) - true, если строка пуста или nil, иначе false

--// Проверить, является ли объект числом.
is_number(object)
args:
  object (any) - проверяемый объект
retval: (boolean) - true, если объект - число, иначе false

--// Проверить, соответствует ли строка шаблону с wildcard ('*' по умолчанию).
--// Wildcard заменяет любую последовательность символов (включая пустую).
--// Несколько подряд идущих wildcard'ов схлопываются в один.
--// Специальные regex-символы в шаблоне экранируются автоматически.
--// Если wildcard в шаблоне отсутствует - выполняется точное сравнение.
--// Если шаблон состоит только из wildcard'ов - подходит любая строка (включая пустую).
has_pattern(str, pattern, wildcard)
args:
  str (string) - строка для проверки
  pattern (string) - шаблон с литералами и wildcard'ами
  wildcard (string, optional) - символ wildcard, по умолчанию '*'
retval: (boolean) - true, если строка соответствует шаблону, иначе false
```

```lua
--// Проверка типа
if ffx_compare_utils.is_table({}) then
  print("Это таблица")
end

--// Проверка на пустоту таблицы
local t = {}
if ffx_compare_utils.is_empty_flat_or_assoc_table(t) then
  print("Таблица пуста")
end

--// Проверка строк
local s = "hello"
if ffx_compare_utils.is_not_empty_string(s) then
  print("Строка не пуста")
end

--// Проверка числа
local n = 42
if ffx_compare_utils.is_number(n) then
  print("Это число")
end

--// Сопоставление с wildcard
if ffx_compare_utils.has_pattern("se_test", "se_*") then
  print("Строка начинается с 'se_'")
end

if ffx_compare_utils.has_pattern("start_end", "start*end") then
  print("Строка содержит 'start' в начале и 'end' в конце")
end

--// Пример с несколькими wildcard'ами и пустыми совпадениями
if ffx_compare_utils.has_pattern("ac", "a*b*c") then
  print("'ac' соответствует 'a*b*c' (wildcard'ы съели пустоту)")
end

--// Точное сравнение без wildcard
if ffx_compare_utils.has_pattern("hello", "hello") then
  print("Точное совпадение")
end
```
