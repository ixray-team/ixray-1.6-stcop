# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0


### ffx_console_utils: `\gamedata\scripts\ixr_framework\utils\ffx_console_utils.script`
Утилиты для работы с игровой консолью:
* `register_command`
* `execute_command`

---

#### Описание методов:

```lua
--// Зарегистрировать новую команду в консоли.
register_command(name, _callable, tips_table)
args:
  name (string) - имя команды (будет вводиться в консоли)
  _callable (function) - функция, которая будет вызвана при выполнении команды. Получает аргументы, переданные в консоли, как отдельные параметры (через unpack).
  tips_table (table, optional) - таблица строк с подсказками для автодополнения (каждая строка выводится в консоли по отдельности); по умолчанию пустая таблица.
retval: (boolean) - true, если команда успешно зарегистрирована, иначе false (если не указано имя или callable).

--// Выполнить команду в консоли с одним аргументом.
execute_command(command, arg)
args:
  command (string) - команда (имя зарегистрированной команды или встроенная)
  arg (string|number|boolean) - аргумент команды. Если передана строка "true", она преобразуется в число 1; строка "false" → 0. Остальные значения приводятся к строке.
retval: (none)
```

#### Примеры использований:
```lua
-- Регистрация команды без подсказок
ffx_console_utils.register_command("mycmd", function(...)
  print("Аргументы:", ...)
end)

-- Регистрация команды с подсказками
ffx_console_utils.register_command("teleport", function(x, y, z)
  -- телепортировать игрока
  print("Телепорт в", x, y, z)
end, {"x coordinate", "y coordinate", "z coordinate"})

-- Выполнение команды
ffx_console_utils.execute_command("mycmd", "hello")  -- выполнит mycmd с аргументом "hello"
ffx_console_utils.execute_command("teleport", "true") -- передаст 1 (так как "true" → 1)
ffx_console_utils.execute_command("teleport", "false")-- передаст 0
ffx_console_utils.execute_command("quit", "")        -- выполнит встроенную команду quit
```
