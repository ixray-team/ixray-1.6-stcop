
# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

## Модуль логирования IXR TRACY PROFILER
* Предназначен для управлением системой профилирования луа скриптов
* Репозиторий с профайлером https://github.com/wolfpld/tracy скачивать с релизов

Управление через глобальные методы:
```lua
--// Начать событие профилирования с указанным именем. Событие будет отображаться в Tracy, если профилировщик подключён. Вызов применяется к контексту текущего скрипта.
PROF_EVENT_BEGIN(name)
args:
  name (string)(required) - имя события профилирования.
retval: (none) - функция ничего не возвращает.

--// Завершить текущее событие профилирования. Должна вызываться после соответствующего PROF_EVENT_BEGIN. Вызов применяется к контексту текущего скрипта.
PROF_EVENT_END()
args:
  (none)
retval: (none) - функция ничего не возвращает.

--// Выполнить переданную функцию внутри профилируемого события. Если профилировщик не подключён, функция вызывается напрямую. Возвращает результат(ы) функции. Вызов применяется к контексту текущего скрипта.
PROF_EVENT_CLOSURE(name, callable)
args:
  name (string)(required) - имя события профилирования.
  callable (function)(required) - функция, которую нужно выполнить и профилировать.
retval: (*) - возвращаемые значения callable, или nil, если callable не передана.
```

Примеры реализации:
```lua
--// Пример использования PROF_EVENT_BEGIN и PROF_EVENT_END
--// Начинаем профилирование блока кода, выполняем операцию и завершаем событие.
PROF_EVENT_BEGIN("Загрузка данных из файла") -- начало профилирования
local data = load_data_from_file("config.json")
process_data(data)
PROF_EVENT_END() -- конец профилирования

--// Пример использования PROF_EVENT_CLOSURE
--// Оборачиваем вызов функции в профилируемое событие. Если профилировщик активен, событие будет записано, иначе функция выполнится без профилирования.
local result = PROF_EVENT_CLOSURE("Сохранение данных", function()
    return save_to_database(data)
end)

--// Пример вложенных событий
--// Можно создавать вложенные зоны для детального анализа.
PROF_EVENT_BEGIN("Обработка запроса")
    PROF_EVENT_BEGIN("Парсинг параметров")
    local params = parse_request(request)
    PROF_EVENT_END()

    PROF_EVENT_BEGIN("Основная логика")
    local response = handle_request(params)
    PROF_EVENT_END()

    PROF_EVENT_BEGIN("Формирование ответа")
    local output = format_response(response)
    PROF_EVENT_END()
PROF_EVENT_END()
```

### Примеры профилирования с вовзращаемым значением оригинального метода
```lua
--// Пример: функция, вычисляющая факториал, с профилированием через замыкание.
--// Результат работы функции возвращается наружу, а внутри она обёрнута в PROF_EVENT_CLOSURE.
--// Аргументы метода будут корректно переданы в замыкание без необходимости дополнительных манипуляций
function calculate_factorial(n)
    return PROF_EVENT_CLOSURE("Вычисление факториала", function()
        if n <= 1 then return 1 end
        local result = 1
        for i = 2, n do
            result = result * i
        end
        return result
    end)
end

--// Вызов функции — результат будет получен, а профилирование произойдёт автоматически.
SemiLog("Факториал 10 =" .. calculate_factorial(10))

```

### Частный случай:
```lua
-- Профилируемая обёртка, которая принимает любые аргументы, передаёт их в функцию,
-- и возвращает все результаты, сохраняя профилирование.
function profiled_call(func, ...)
    local args = {...}
    return PROF_EVENT_CLOSURE("Вызов функции " .. tostring(func), function()
        return func(table.unpack(args)) -- извлекаем аргументы ... в том же порядке
    end)
end

-- Пример использования:
local result = profiled_call(math.max, 10, 20, 30, 40)
print("Максимум =", result)
```
