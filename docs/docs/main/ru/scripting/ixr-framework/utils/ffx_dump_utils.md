# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0


### ffx_dump_utils: `\gamedata\scripts\ixr_framework\utils\ffx_dump_utils.script`
Утилиты для дампа данных, логирования и отладки:
* `var_export`
* `var_dump_to_console_log`
* `write_to_console_log`
* `var_dump_to_file_log`
* `write_to_file_log`
* `send_message`
* `AssertWithCaller`

---

#### Описание методов:

```lua
--// Генерирует строковое представление таблицы с поддержкой рекурсии, функций и метаданных.
var_export(tbl, indent, visited, is_subtable)
args:
  tbl (table) - таблица для дампа (если передано не-таблица, оборачивается в таблицу {tbl})
  indent (number, optional) - текущий уровень отступа (по умолчанию 0)
  visited (table, optional) - внутренний параметр для отслеживания уже посещённых таблиц (предотвращает зацикливание)
  is_subtable (boolean, optional) - внутренний флаг, указывающий, является ли текущая таблица вложенной
retval: (string) - отформатированная строка с дампом таблицы

--// Выводит дамп объекта в консоль игры (через SemiLog) с префиксом-источником (по умолчанию – вызывающий скрипт).
var_dump_to_console_log(object, log_prefix)
args:
  object (any) - объект для дампа
  log_prefix (string, optional) - префикс для строки лога (если не указан, определяется автоматически через ffx_callable_utils.find_caller_source)
retval: (boolean) - всегда true

--// Записывает произвольный текст в консоль игры с префиксом.
write_to_console_log(text, log_prefix)
args:
  text (string) - текст для вывода
  log_prefix (string, optional) - префикс (если не указан, определяется автоматически)
retval: (boolean) - всегда true

--// Сохраняет дамп объекта в текстовый файл в папке логов игры.
var_dump_to_file_log(object, overwrite, log_prefix)
args:
  object (any) - объект для дампа
  overwrite (boolean, optional) - если true, файл перезаписывается, иначе данные добавляются в конец (по умолчанию false)
  log_prefix (string, optional) - префикс для имени файла (если не указан, определяется автоматически)
retval: (boolean) - true, если запись успешна, иначе false

--// Записывает произвольный текст в файл лога.
write_to_file_log(text, overwrite, log_prefix)
args:
  text (string) - текст для записи
  overwrite (boolean, optional) - если true, файл перезаписывается, иначе данные добавляются (по умолчанию false)
  log_prefix (string, optional) - префикс для имени файла (если не указан, определяется автоматически)
retval: (boolean) - true, если запись успешна, иначе false

--// Отправляет всплывающее сообщение (подсказку) актёру (если существует).
send_message(text)
args:
  text (string) - текст сообщения
retval: (none)

--// Логирует критическую ошибку с указанием источника и стека вызовов, затем вызывает assert(false) и завершает игру (exit(0)).
AssertWithCaller(level, error_source, error_message)
args:
  level (number) - уровень стека для определения вызывающего скрипта (передаётся в ffx_callable_utils.find_caller_source)
  error_source (string) - идентификатор источника ошибки (например, имя модуля)
  error_message (string) - текст ошибки
retval: (none) – функция не возвращает управление
```

### Примеры использований:
```lua
--// Дамп таблицы в консоль
local data = {a = 1, b = {x = 10, y = 20}, func = function() end}
ffx_dump_utils.var_dump_to_console_log(data, "MyDump")

--// Запись текста в консоль
ffx_dump_utils.write_to_console_log("Hello from script", "Info")

--// Дамп в файл (с перезаписью)
ffx_dump_utils.var_dump_to_file_log(data, true, "dump_prefix")

--// Запись текста в файл (добавление)
ffx_dump_utils.write_to_file_log("Some log line", false, "log_prefix")

--// Отправка сообщения игроку
ffx_dump_utils.send_message("Внимание! Квест обновлён.")

--// Генерация ошибки с контекстом
ffx_dump_utils.AssertWithCaller(3, "MyModule", "Неверное значение аргумента")
```
