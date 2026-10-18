
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

--// Вызвать callable(...) внутри профилируемого события. Если профилировщик не подключён, функция вызывается напрямую. Возвращает все результаты callable. Вызов применяется к контексту текущего скрипта.
--// Передавайте функцию, созданную один раз, а не замыкание на каждый вызов: так ничего не выделяется на вызов и LuaJIT может скомпилировать вызывающий код.
PROF_EVENT_CALL(name, callable, ...)
args:
  name (string)(required) - имя события профилирования.
  callable (function)(required) - функция, которую нужно вызвать и профилировать.
  ... - аргументы для callable.
retval: (*) - все возвращаемые значения callable.
```

### Примеры реализации:
```lua
--// Пример использования PROF_EVENT_BEGIN и PROF_EVENT_END
--// Начинаем профилирование блока кода, выполняем операцию и завершаем событие.
PROF_EVENT_BEGIN("Загрузка данных из файла") -- начало профилирования
local data = load_data_from_file("config.json")
process_data(data)
PROF_EVENT_END() -- конец профилирования

--// Пример использования PROF_EVENT_CALL
--// Оборачиваем вызов функции в профилируемое событие. Если профилировщик активен, событие будет записано, иначе функция выполнится без профилирования.
local result = PROF_EVENT_CALL("Сохранение данных", save_to_database, data)

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
--// Пример: функция, вычисляющая факториал, с профилированием.
--// Тело — функция, созданная один раз; публичная функция передаёт ей аргументы через PROF_EVENT_CALL.
local function calculate_factorial_impl(n)
    if n <= 1 then return 1 end
    local result = 1
    for i = 2, n do
        result = result * i
    end
    return result
end

function calculate_factorial(n)
    return PROF_EVENT_CALL("Вычисление факториала", calculate_factorial_impl, n)
end

--// Вызов функции — результат будет получен, а профилирование произойдёт автоматически.
SemiLog("Факториал 10 =" .. calculate_factorial(10))

```

### Частный случай:
```lua
-- Профилируемая обёртка, которая принимает любые аргументы, передаёт их в функцию,
-- и возвращает все результаты, сохраняя профилирование.
function profiled_call(func, ...)
    return PROF_EVENT_CALL("Вызов функции", func, ...)
end

-- Пример использования:
local result = profiled_call(math.max, 10, 20, 30, 40)
print("Максимум =", result)
```
