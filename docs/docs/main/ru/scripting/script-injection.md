# Точечная подмена Lua-методов

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.5.0

Добавление и замена методов в загруженных неймспейсах и Lua-классах,
изменение и создание приватных переменных без повторного выполнения файла назначения.

### Новые экспорты lua

* `override_or_create_method`
* `override_or_create_local_variable`
* `get_local_variables`
* `get_local_functions`
* `get_callable_info`
* `resolve_injected_method`
* `has_new_overrides`

#### Описание методов:

```lua [Глобальное пространство]
--// Добавить или заменить метод неймспейса либо Lua-класса.
override_or_create_method(target_reference, method_name, callable, source_locals)
args:
  target_reference (table|userdata)(required) - таблица загруженного неймспейса или ссылка на Lua-класс.
  method_name (string)(required) - имя добавляемого или заменяемого метода.
  callable (function)(required) - Lua-функция с новой реализацией.
  source_locals (table)(optional) - массив имён локальных переменных, захваты которых нужно сохранить из исходного контекста callable (по умолчанию {}). Остальные внешние захваты связываются с одноимёнными приватными переменными назначения.
retval: (none)

--// Изменить или создать приватную переменную неймспейса либо файла объявления класса.
override_or_create_local_variable(target_reference, variable_name, value)
args:
  target_reference (table|userdata)(required) - таблица загруженного неймспейса или ссылка на Lua-класс.
  variable_name (string)(required) - имя приватной переменной.
  value (any)(required) - новое значение; допускается nil.
retval: (none)

--// Получить список приватных переменных неймспейса либо файла объявления класса и их текущие значения.
get_local_variables(target_reference)
args:
  target_reference (table|userdata)(required) - таблица загруженного неймспейса или ссылка на Lua-класс.
retval: (table) - отсортированный по имени массив записей {name = "имя", value = значение}.

--// Получить текущее значение одной приватной переменной.
get_local_variables(target_reference, variable_name)
args:
  target_reference (table|userdata)(required) - таблица загруженного неймспейса или ссылка на Lua-класс.
  variable_name (string)(required) - имя переменной.
retval: (any) - значение переменной; nil, если переменная отсутствует или её значение равно nil.

--// Получить текущие значения выбранных приватных переменных.
get_local_variables(target_reference, variable_names)
args:
  target_reference (table|userdata)(required) - таблица загруженного неймспейса или ссылка на Lua-класс.
  variable_names (table)(required) - плоский массив имён переменных.
retval: (table) - массив записей {name = "имя", value = значение} в порядке запрошенных имён. Отсутствующие имена пропускаются; существующая переменная со значением nil сохраняется как запись с name. Пустой массив имён возвращает пустой результат.

--// Получить локальные функции неймспейса либо файла объявления класса.
get_local_functions(target_reference)
args:
  target_reference (table|userdata)(required) - таблица загруженного неймспейса или ссылка на Lua-класс.
retval: (table) - отсортированный по имени массив записей {name = "имя", value = функция}. Включает только приватные переменные, текущее значение которых является функцией.

--// Получить ссылку на одну локальную функцию с сохранением её захватов.
get_local_functions(target_reference, function_name)
args:
  target_reference (table|userdata)(required) - таблица загруженного неймспейса или ссылка на Lua-класс.
  function_name (string)(required) - имя локальной функции.
retval: (function|nil) - ссылка на функцию; nil, если имя отсутствует или значение не является функцией.

--// Получить выбранные локальные функции.
get_local_functions(target_reference, function_names)
args:
  target_reference (table|userdata)(required) - таблица загруженного неймспейса или ссылка на Lua-класс.
  function_names (table)(required) - плоский массив имён локальных функций.
retval: (table) - массив записей {name = "имя", value = функция} в порядке запроса. Отсутствующие имена и значения других типов пропускаются.

--// Получить идентификатор и описание функции либо строкового адреса без её выполнения.
get_callable_info(callable)
args:
  callable (function|string|table)(required) - ссылка на функцию, строка "namespace.method" или пара {"namespace", "method"}.
retval: (table) - таблица с полями name_string, memory_id, pure_callable, string_callable, uptime_microseconds.

--// Совместимость с прежним API: ссылки уже обновляются автоматически.
resolve_injected_method(function_reference)
args:
  function_reference (function)(required) - текущая или прежняя ссылка на функцию.
retval: (function, boolean) - переданная функция и false. Для обычных вызовов и подписок этот экспорт не требуется.

--// Проверить наличие новых подмен функций и сбросить признак изменений.
has_new_overrides()
args: (none)
retval: (boolean) - true, если после предыдущей проверки были подменены функции; иначе false. Каждый вызов сбрасывает признак. Сами связи между прежними и текущими функциями сохраняются.
```

### Примеры использования:

Исходный скрипт назначения:

```lua [gamedata\scripts\my_test.script]
local c = 3
local k = 5

function foo(a, b)
    return a + b + c
end

function get_c()
    return c
end

class "Example"

function Example:__init(value)
    self.value = value
end

function Example:calculate(x)
    return self.value + x + c
end
```

Инъекция из другого скрипта через Lua Framework:

```lua [gamedata\scripts\my_injection.script]
local c = 100
local k = 200

function on_game_start()
    --// Меняем приватную переменную назначения. Изменение видят и оригинальные методы.
    override_or_create_local_variable(my_test, "c", 10)
    my_test.get_c() -- 10

    --// Сохраняем c из исходного контекста; k берём из my_test.
    override_or_create_method(my_test, "foo", function(a, b)
        return a * b + c + k
    end, {"c"})

    my_test.foo(2, 3) -- 111: 2 * 3 + 100 + 5

    --// Меняем k назначения без повторной установки метода.
    override_or_create_local_variable(my_test, "k", 7)
    my_test.foo(2, 3) -- 113

    --// Меняем захваченную переменную исходного контекста.
    c = 200
    my_test.foo(2, 3) -- 213

    --// Создаём новую приватную переменную и новый метод.
    override_or_create_local_variable(my_test, "counter", 0)
    override_or_create_method(my_test, "next_counter", function()
        counter = counter + 1
        return counter
    end)

    my_test.next_counter() -- 1
    my_test.next_counter() -- 2
    -- counter не становится публичным полем my_test.

    --// Заменяем метод класса. c связываем с переменной назначения,
    --// хотя выше в этом скрипте тоже объявлен local c.
    local class_reference = my_test.Example
    local instance = class_reference(4)

    override_or_create_method(class_reference, "calculate", function(self, x)
        return self.value * x + c
    end, {})

    instance:calculate(3) -- 22: 4 * 3 + 10

    --// Класс и неймспейс разделяют приватные переменные своего файла.
    override_or_create_local_variable(class_reference, "c", 30)
    instance:calculate(3) -- 42
    my_test.get_c() -- 30

    --// Получаем списки приватных переменных.
    local namespace_variables = get_local_variables(my_test)
    local class_variables = get_local_variables(class_reference)
    local namespace_functions = get_local_functions(my_test)
    local class_functions = get_local_functions(class_reference)
    local selected_variables = get_local_variables(my_test, {"k", "c"})
    local one_variable = get_local_variables(my_test, "c")
    -- one_variable == 30

    for _, entry in ipairs(namespace_variables) do
        printf("%s = %s", entry.name, tostring(entry.value))
    end
end
```

### Особенности использования

Полученные функции сохраняют свои исходные захваты. Присваивание новой функции локальной переменной вызывающего скрипта или полю `value` полученной записи не меняет функцию в целевом файле. Для замены приватной функции используйте `override_or_create_local_variable(target_reference, function_name, callable)`: этот экспорт сохраняет захваты переданной функции из её исходного контекста.

* `source_locals` содержит только имена локалок, которые переданная функция
  действительно захватила. Повторы, пропуски в массиве и неизвестные имена вызывают
  ошибку. Если внешний захват не указан в списке, соответствующая приватная
  переменная должна существовать в назначении, иначе установка завершится ошибкой.
* Параметры и локалки внутри внедряемой функции работают как обычно.
  Свободные имена, которые функция не захватила, ищутся среди приватных переменных
  назначения, затем в его публичном окружении и глобальном пространстве.
  Приватное значение `nil` тоже перекрывает одноимённое публичное значение.
* Список `source_locals` не меняет переменные назначения. Выбранные исходные
  переменные и переменные назначения сохраняют живую связь: изменение значения
  не требует повторной установки метода. Сама переданная функция `callable`
  сохраняет своё исходное поведение при отдельном вызове.
* Доступны локалки уровня файла и приватные переменные, созданные предыдущими
  инъекциями. Локалки отдельных вызовов функций, циклов и вложенных блоков
  недоступны. При повторном объявлении одного имени новая инъекция использует
  последнее исполненное объявление.
* Для класса приватными считаются переменные файла его объявления.
  Метод получает `self` при обычном вызове `instance:method(...)`.
  Замену видят новые и уже созданные экземпляры, если у экземпляра нет собственного
  поля с тем же именем. Сохранённые ссылки на заменяемую функцию обновляются
  автоматически, включая унаследованные копии той же функции.
* Экспорты принимают загруженные неймспейсы и Lua-классы. C++-классы, обычные
  таблицы, `_G` и глобальные скрипты, загруженные через `LoadScriptToGlobal`,
  в качестве назначения не поддерживаются.
* Списки переменных — снимки значений. Для `nil` запись с `name` присутствует,
  а поле `value` отсутствует. Изменение записи списка не меняет переменную;
  используйте `override_or_create_local_variable`. Ссылочные значения, например
  таблицы, возвращаются без копирования.
* При последовательных заменах одного метода действует последняя установка.
  Остальные методы и состояние назначения сохраняются.

### Информация о функции

`get_callable_info` возвращает описание в следующем формате:

```lua
{
    name_string = "my_test:12",
    memory_id = "callable:42",
    pure_callable = my_test.foo,
    string_callable = nil,
    uptime_microseconds = 0
}
```

Для прямой ссылки `name_string` содержит источник и строку объявления, если эти
сведения доступны; иначе используется текстовое представление функции.
Название не обязано быть уникальным. `memory_id` идентифицирует функцию в текущем
Lua-сеансе. Новая реализация, созданная инжектором, наследует ID заменяемой функции;
ранее присвоенный ID существующей функции не перезаписывается.

Для строкового адреса используются поля:

```lua
get_callable_info("my_test.foo")
-- {
--     name_string = "my_test.foo",
--     memory_id = "my_test.foo",
--     pure_callable = nil,
--     string_callable = {"my_test", "foo"},
--     uptime_microseconds = 0
-- }
```

Строковый адрес разбирается через `ffx_string_utils`; целевой метод не вызывается.
`uptime_microseconds` пока всегда равен `0`: измерение времени выполнения
добавляется отдельно. Получение описания само по себе не измеряет и не выполняет
функцию. При отсутствии значения `nil` соответствующее поле отсутствует в таблице
по обычным правилам Lua.

### Подписки и сохранённые ссылки

Существующие вызовы `RegisterScriptCallback` и `UnregisterScriptCallback`
работают без изменений. Сохранённые ссылки в подписках, переменных, таблицах,
замыканиях и `luabind::functor` автоматически переключаются на новую реализацию.
Пересобирать подписки и вызывать `resolve_injected_method` не требуется.

```lua
local saved = my_test.foo
local callbacks = {my_test.foo}

override_or_create_method(my_test, "foo", function(a, b)
    return a * b
end)

saved(2, 3)       -- 6
callbacks[1](2, 3) -- 6
my_test.foo(2, 3) -- 6
```

Если одна функция использовалась под несколькими именами, все сохранённые ссылки
на неё обновляются вместе. Уже выполняющийся вызов завершает прежнее тело функции;
последующие вызовы через сохранённые ссылки используют новую реализацию.
При замене приватной переменной с одной функции на другую обновляются также
ранее сохранённые ссылки на прежнее значение.

`has_new_overrides()` можно использовать для собственного отслеживания изменений,
но для корректной работы обработчиков его вызывать не требуется.
