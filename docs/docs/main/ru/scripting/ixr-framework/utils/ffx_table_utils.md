# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

### ffx_table_utils: `\gamedata\scripts\ixr_framework\utils\ffx_table_utils.script`
Утилиты для работы с таблицами (поиск, клонирование, сортировка, обход, получение ключей и значений):
* `is_value_exists_by_key`
* `push_back_by_key`
* `clone`
* `get_size`
* `sort_assoc_tables`
* `deep_concat`
* `is_contains`
* `get_contains_index`
* `get_contains_element_number`
* `get_last_value`
* `get_max_key_index`
* `get_min_key_index`
* `get_array_keys`

#### Описание методов:

```lua
--// Проверить, есть ли в массиве таблиц элемент, у которого значение по указанному ключу равно искомому.
is_value_exists_by_key(tbl, key, value)
args:
  tbl (table)(required) - массив таблиц.
  key (any)(required) - ключ для проверки в каждой подтаблице.
  value (any)(required) - искомое значение.
retval: (boolean) - true, если найдено совпадение, иначе false.

--// Добавить значение в подтаблицу по указанному ключу, создавая её, если она отсутствует.
push_back_by_key(tbl, key, value)
args:
  tbl (table)(required) - родительская таблица.
  key (any)(required) - ключ, значение которого должно быть таблицей.
  value (any)(required) - значение для добавления в подтаблицу.
retval: (none)

--// Создать глубокую копию таблицы (рекурсивно, с сохранением метатаблиц).
clone(tbl)
args:
  tbl (table)(required) - таблица для клонирования.
retval: (table) - глубокая копия таблицы.

--// Вернуть количество элементов в таблице (включая ассоциативные ключи).
get_size(tbl)
args:
  tbl (table)(required) - таблица для подсчёта.
retval: (number) - количество пар ключ-значение.

--// Отсортировать ассоциативную таблицу по ключам (преобразованным в строки) и вернуть массив значений в этом порядке.
sort_assoc_tables(tbl)
args:
  tbl (table)(required) - ассоциативная таблица.
retval: (table) - массив значений, отсортированных по ключам.

--// Рекурсивно обойти таблицу, применить функцию-замыкание к каждому значению (не являющемуся таблицей) и объединить результаты в строку. Ключи сортируются для детерминированного порядка.
deep_concat(tbl, closure_fn)
args:
  tbl (table)(required) - обрабатываемая таблица.
  closure_fn (function)(required) - функция, принимающая значение и возвращающая строку.
retval: (string) - объединённая строка из всех обработанных значений.

--// Проверить, содержит ли плоская (не вложенная) таблица указанное значение.
is_contains(t, item)
args:
  t (table)(required) - плоская таблица.
  item (any)(required) - искомое значение.
retval: (boolean) - true, если значение найдено, иначе false.

--// Вернуть ключ, соответствующий значению в плоской таблице, или false, если не найдено.
get_contains_index(t, item)
args:
  t (table)(required) - плоская таблица.
  item (any)(required) - искомое значение.
retval: (any|false) - ключ, если найден, иначе false.

--// Вернуть числовой индекс (начиная с 1) значения в плоской таблице (массиве), или false, если не найдено.
get_contains_element_number(t, value)
args:
  t (table)(required) - плоская таблица (массив).
  value (any)(required) - искомое значение.
retval: (number|false) - индекс, если найден, иначе false.

--// Получить последнее значение на корневом уровне таблицы (с наибольшим числовым ключом). Если числовых ключей нет, возвращает default_value.
get_last_value(input_table, default_value)
args:
  input_table (table)(required) - таблица для проверки.
  default_value (any)(optional) - значение, возвращаемое при отсутствии числовых ключей.
retval: (any) - последнее найденное значение или default_value.

--// Найти максимальный числовой ключ в таблице (опционально рекурсивно до указанной глубины).
get_max_key_index(input_table, default_value, depth)
args:
  input_table (table)(required) - таблица для проверки.
  default_value (any)(optional) - значение, возвращаемое, если числовые ключи не найдены.
  depth (number)(optional) - глубина рекурсии: nil или 1 — только корень, >1 — вложенные, -1 — без ограничений (по умолчанию 1).
retval: (number|any) - максимальный числовой ключ или default_value.

--// Найти минимальный числовой ключ в таблице (опционально рекурсивно до указанной глубины).
get_min_key_index(input_table, default_value, depth)
args:
  input_table (table)(required) - таблица для проверки.
  default_value (any)(optional) - значение, возвращаемое, если числовые ключи не найдены.
  depth (number)(optional) - глубина рекурсии: nil или 1 — только корень, >1 — вложенные (по умолчанию 1).
retval: (number|any) - минимальный числовой ключ или default_value.

--// Получить массив всех ключей таблицы (включая строковые и числовые).
get_array_keys(tbl)
args:
  tbl (table)(required) - таблица для извлечения ключей.
retval: (table) - массив ключей.
```

### Примеры использований:
```lua
--// Проверка существования значения по ключу в массиве таблиц
local items = {{id=1, name="apple"}, {id=2, name="banana"}}
if ffx_table_utils.is_value_exists_by_key(items, "name", "banana") then
    SemiLog("Найден банан")
end

--// Добавление в подтаблицу
local data = {}
ffx_table_utils.push_back_by_key(data, "players", "John")
--// data = { players = {"John"} }

--// Глубокое клонирование
local original = {a=1, b={c=2}}
local copy = ffx_table_utils.clone(original)
copy.b.c = 3
--// original.b.c остаётся 2

--// Размер таблицы
local sz = ffx_table_utils.get_size({x=10, y=20, z=30}) --// 3

--// Сортировка ассоциативной таблицы
local assoc = {z=1, a=2, m=3}
local sorted_vals = ffx_table_utils.sort_assoc_tables(assoc)
--// sorted_vals = {2,3,1}  (по ключам a, m, z)

--// Глубокое объединение
local tbl = {x=1, y={a="hello", b="world"}}
local result = ffx_table_utils.deep_concat(tbl, function(v) return tostring(v) end)
--// результат: "1helloworld" (порядок зависит от сортировки ключей)

--// Проверка вхождения
local flat = {10, 20, 30}
if ffx_table_utils.is_contains(flat, 20) then
    SemiLog("20 найдено")
end

--// Получение индекса
local idx = ffx_table_utils.get_contains_index({a=1, b=2}, 2) --// "b"
local numIdx = ffx_table_utils.get_contains_element_number({10, 20, 30}, 20) --// 2

--// Последнее значение
local last = ffx_table_utils.get_last_value({10, 20, 30}, nil) --// 30

--// Максимальный числовой ключ
local maxKey = ffx_table_utils.get_max_key_index({[1]=10, [5]=20, [2]=30}, nil, 1) --// 5

--// Минимальный числовой ключ
local minKey = ffx_table_utils.get_min_key_index({[1]=10, [5]=20, [2]=30}, nil, 1) --// 1

--// Получение всех ключей
local keys = ffx_table_utils.get_array_keys({name="John", age=30, city="NY"})
--// keys = {"name", "age", "city"} (порядок не гарантирован)
```
