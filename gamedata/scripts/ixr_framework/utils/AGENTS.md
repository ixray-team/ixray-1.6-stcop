# Утилиты IX-Ray Framework

Эта папка содержит общие Lua-утилиты Framework. Перед добавлением вспомогательной
функции проверь существующие реализации в этой папке и `libs/`.
Не дублируй готовые парсеры, проверки, поиск по таблицам и работу с кешем.

## Пользовательская документация и примеры

- [Правила написания и обновления документации Framework](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/AGENTS.md) — оформление функций, аргументы и результаты, примеры, версии и навигация.

- [Каталог утилит](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils.md)
- [ffx_actor_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_actor_utils.md)
- [ffx_cache_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_cache_utils.md)
- [ffx_callable_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_callable_utils.md)
- [ffx_compare_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_compare_utils.md)
- [ffx_console_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_console_utils.md)
- [ffx_crypto_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_crypto_utils.md)
- [ffx_dump_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_dump_utils.md)
- [ffx_gui_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_gui_utils.md)
- [ffx_io_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_io_utils.md)
- [ffx_ltx_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_ltx_utils.md)
- [ffx_math_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_math_utils.md)
- [ffx_object_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_object_utils.md)
- [ffx_particles_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_particles_utils.md)
- [ffx_path_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_path_utils.md)
- [ffx_sound_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_sound_utils.md)
- [ffx_spawn_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_spawn_utils.md)
- [ffx_string_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_string_utils.md)
- [ffx_table_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_table_utils.md)
- [ffx_vector_utils](../../../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/utils/ffx_vector_utils.md)

Начинай с документации нужного API и примеров, затем сверяй сигнатуры, значения и жизненный цикл с текущей реализацией. Если старая формулировка расходится с актуальными правилами владельца проекта (например, допускает имя файла модуля вместо alias), соблюдай актуальные правила из родительских AGENTS.md. Само обнаружение расхождения не является задачей на изменение скриптов или документации. Минимальную версию проверяй на конкретной странице.

## Связанные инструкции

- [Корень Framework](../AGENTS.md) — архитектура, жизненный цикл, глобальные API и общие правила.
- [Библиотеки JSON и Base64](libs/AGENTS.md) — дочерняя папка `libs/`, её API и совместимость форматов.

Утилиты образуют общее публичное API для модов, оригинальных игровых скриптов и самого фреймворка. Они переиспользуются через неймспейсы; подключение к менеджеру модулей ради вызова утилиты не требуется.

## Как выбирать готовый метод

Ищи также комбинации уже доступного публичного API Framework. Нужная механика не всегда называется как задача: ограничение частоты работы внутри существующего update можно сделать глобальной `IsActionThrottled`, не создавая таймер или собственное хранилище времён. Различай отсечку текущих вызовов и планирование будущего вызова; пример находится в [карте модулей](../modules/AGENTS.md).

1. Найди подходящие функции через `rg -n '^function ' <имя_скрипта>`.
2. Прочитай реализацию и места вызова: порядок аргументов, возвращаемые значения,
   обработка `nil`, разделителей и Lua-паттернов имеют значение.
3. Используй существующую утилиту через её неймспейс, например
   `ffx_string_utils.explode(value, ".")`.
4. Если готового метода действительно нет, добавляй его в соответствующий
   существующий модуль. Новый модуль нужен только для отдельной ответственности.

## Где искать

| Задача | Скрипт и готовые методы |
| --- | --- |
| Строки и разделители | `ffx_string_utils.script`: `trim`, `split`, `explode`, `escape_regex_special_chars` |
| Списки из строк | `ffx_string_utils.script`: `parse_separated_strings`, `parse_separated_numbers`, `parse_separated_bools` |
| Проверки типов и пустых значений | `ffx_compare_utils.script`: `is_table`, `is_function`, `is_userdata`, `is_number`, `is_empty_string`, `is_empty_flat_or_assoc_table` |
| Таблицы | `ffx_table_utils.script`: `clone`, `get_size`, `is_contains`, `get_contains_index`, `get_last_value`, `get_array_keys` |
| Функции и скрипты | `ffx_callable_utils.script`: `is_script_present_in_g_file`, `is_function_callable_by_name`, `find_caller_source`, `get_call_stack_trace` |
| Имена файлов | `ffx_path_utils.script`: `get_file_name(file_path, remove_ext)` |
| LTX и INI | `ffx_ltx_utils.script`: `cfg_get_*`, `cfg_parse_separated_*`, `ini_get_string`, `ini_parse_separated` |
| Кеш | `ffx_cache_utils.script`: `get_cached`, `invalidate_cache` |
| Логи и проверки с указанием вызывающего кода | `ffx_dump_utils.script` |
| Кодирование | `libs/ffx_json_lib.script`, `libs/ffx_base64_lib.script`, `ffx_crypto_utils.script` |
| Векторы, углы, математика | `ffx_vector_utils.script`, `ffx_angle_utils.script`, `ffx_math_utils.script` |
| Объекты, актор, спавн | `ffx_object_utils.script`, `ffx_actor_utils.script`, `ffx_spawn_utils.script` |
| Звук, частицы, лучи | `ffx_sound_utils.script`, `ffx_particles_utils.script`, `ffx_ray.script` |
| Остальное | `ffx_io_utils.script`, `ffx_console_utils.script`, `ffx_ctime_utils.script`, `ffx_gui_utils.script` |

## Парсеры и строки

- `split(text, substr)` вызывает `explode(text, substr)`. Разделитель передаётся
  вторым аргументом. Для точки используй `"."`, а не самостоятельно экранированный
  Lua-паттерн: `explode` экранирует разделитель через готовый метод.
- `explode` обрезает пробелы в частях. Не подменяй этот контракт другим
  поведением при использовании или расширении метода.
- Для типизированных списков используй соответствующий `parse_separated_*`,
  для значений из конфигурации — `cfg_parse_separated_*`.
- Не считай, что все строковые методы используют буквальный поиск:
  некоторые принимают Lua-паттерны. Проверяй тело конкретной функции.
- В этой папке есть строковые парсеры, но нет полноценного синтаксического
  парсера Lua. Разбиение строки не заменяет анализ исходного Lua-кода.

## Стиль изменений

- Сохраняй стиль окружающего кода: табы, имена в `snake_case`, существующее
  оформление `if`, циклов и таблиц. Не переформатируй весь файл ради одного метода.
- Делай минимальные изменения по задаче. Не добавляй новые очереди, обёртки,
  индексы и дополнительное состояние без необходимости для запрошенного поведения.
- Сохраняй существующие имена экспортов, аргументы, возвращаемые значения и
  соглашения об ошибках. Исправление общего метода требует проверки его потребителей.
- Не меняй массивы на ассоциативные таблицы и обратно без учёта потребителей.
  Для последовательных массивов сохраняй порядок и индексы. `pairs` не гарантирует
  порядок регистрации; функция в качестве ключа не является числовым индексом.
- Не выполняй переданную функцию ради получения её описания или идентификатора.
- Не добавляй постоянные тестовые файлы и стенды в проект без запроса пользователя.
  Проверки запускай отдельно, не изменяя рабочие данные игры.
- Если правка касается рабочей копии `gamedata` рядом с каталогом `IX-RAY`,
  сверяй версии перед синхронизацией. Не перезаписывай отличающийся пользовательский
  файл копией из репозитория без проверки различий.

## Документация

Пользовательская документация описывает использование, а не историю разработки
или внутреннее устройство движка. Формат: название, статус, минимальная версия,
список Lua-экспортов, сигнатуры с `args` и `retval`, затем примеры использования.
Не ограничивай общие утилиты применением в аддонах.
