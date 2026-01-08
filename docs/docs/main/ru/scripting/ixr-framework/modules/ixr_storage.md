
# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

## Модуль хранилища IXR STORAGE

  Позволяет хранить любые луа данные для скриптов в отдельном маршал файле синхронизированном с сохранением текущим игры не ломая этим файлы сохранений игры.
  * Не ограниченный размер хранимых данных.
  * Не используются оригинальные файлы сохранений или нет пакеты или кастом дата игровых обьектов.
  * Порядок загрузки и считывания данных не играет никакой роли (теперь не нужно опасаться что сохранение не загрузится если пропущена одна из операций чтения или записи).
  * Данные теперь хранятся именованно и ассоциативно т.е не трубется соблюдать порядок считывания или укладки данных с хранилище * можно обращаться по именам ключей.
  * Полная изоляция данных в рамках скрипта который запросил сохранение данных.
  * Поддерживаются глобально доступные хранимые данные при необходимости.
  * Поддерживается сериализация сложных обьектов для сериализации данных которые ранее харнили нет пакеты в биндерах скриптов и подобных вещах.
  * Поддерживается хранение любых LUA типов.


Методы приватного хранилища изолированного по неймспейсу скрипта который производил вызов (область видимости хранимого значения ограничена неймспейсом скрипта)
```lua
--// Проверить наличие переменной в изолированном хранилище
HasStorageVar(var_name)
args:
  var_name (string)(required) - ключ переменной.
retval: (bool) - существует ли значение.

--// Получить переменную из изолированного хранилища
GetStorageVar(var_name, default_value)
args:
  var_name (string)(required) - ключ переменной.
  default_value (string|number|bool|table)(required) - значение по умолчанию.
retval: (string|number|bool|table) - значение или default_value.

--// Сохранить переменную в изолированное хранилище
SetStorageVar(var_name, var_value, var_type)
args:
  var_name (string)(required) - ключ переменной.
  var_value (string|number|bool|table)(required) - значение для сохранения.
  var_type (string)(required) - тип переменной (опциональный, определяется автоматически).
retval: (bool) - успешность сохранения.

--// Удалить переменную из изолированного хранилища
UnsetStorageVar(var_name)
args:
  var_name (string)(required) - ключ переменной.
retval: (void)
```

Примеры: (приватного хранилища изолированного по неймспейсу скрипта)
```lua
--// Проверить наличие переменной в изолированном хранилище
if HasStorageVar(var_name) then
  ...
end

--// Получить переменную из изолированного хранилища
local var = GetStorageVar("my-var", nil)

--// Сохранить переменную в изолированное хранилище
SetStorageVar("my-var", 123456)

--// Удалить переменную из изолированного хранилища
UnsetStorageVar("my-var")

```


Методы приватного чанк хранилища изолированного по неймспейсу скрипта и идентификатору обьекта (область видимости хранимого значения ограничена неймспейсом скрипта и конкретным обьектом) используется для хранения значений нет пакетов из биндеров с теми же принципами но добавлена изоляция пообьектно.
```lua
--// Проверить наличие переменной объекта в чанковом хранилище
HasStorageObjectVar(object_id, var_name)
args:
  object_id (string)(required) - идентификатор объекта.
  var_name (string)(required) - ключ переменной.
retval: (bool) - существует ли значение.

--// Получить переменную объекта из чанкового хранилища
GetStorageObjectVar(object_id, var_name, default_value, retrive_raw_with_type)
args:
  object_id (string)(required) - идентификатор объекта.
  var_name (string)(required) - ключ переменной.
  default_value (mixed)(required) - значение по умолчанию.
  retrive_raw_with_type (bool)(required) - возвращать сырые данные с типом.
retval: (mixed) - значение или default_value.

--// Сохранить переменную объекта в чанковое хранилище
SetStorageObjectVar(object_id, var_name, var_value, var_type)
args:
  object_id (string)(required) - идентификатор объекта.
  var_name (string)(required) - ключ переменной.
  var_value (mixed)(required) - значение для сохранения.
  var_type (string)(required) - тип переменной.
retval: (bool) - успешность сохранения.

--// Удалить переменную объекта из чанкового хранилища
UnsetStorageObjectVar(object_id, var_name)
args:
  object_id (string)(required) - идентификатор объекта.
  var_name (string)(required) - ключ переменной.
retval: (bool) - успешность удаления.
```


Примеры: (приватного чанк хранилища изолированного по неймспейсу скрипта и идентификатору обьекта)
```lua
--// Проверить наличие переменной объекта в чанковом хранилище
if HasStorageObjectVar(self.object:id(), "game_difficulty") then
  ...
end

--// Получить переменную объекта из чанкового хранилища
local game_difficulty = GetStorageObjectVar(self.object:id(), "game_difficulty", 0)

--// Сохранить переменную объекта в чанковое хранилище
SetStorageObjectVar(self.object:id(), "game_difficulty", level.get_game_difficulty())

--// Удалить переменную объекта из чанкового хранилища
UnsetStorageObjectVar(elf.object:id(), "game_difficulty")

```

Методы общедоступного хранилища (область видимости глобальная - имя должно быть уникально в рамках всех скриптов -- не конфликтует с именами приватных хранилищь)
```lua
--// Проверить наличие переменной в общем хранилище
HasStorageSharedVar(var_name)
args:
  var_name (string)(required) - ключ переменной.
retval: (bool) - существует ли значение.

--// Получить переменную из общего хранилища
GetStorageSharedVar(var_name, default_value)
args:
  var_name (string)(required) - ключ переменной.
  default_value (string|number|bool|table)(required) - значение по умолчанию.
retval: (string|number|bool|table) - значение или default_value.

--// Сохранить переменную в общее хранилище
SetStorageSharedVar(var_name, var_value, var_type)
args:
  var_name (string)(required) - ключ переменной.
  var_value (string|number|bool|table)(required) - значение для сохранения.
  var_type (string)(required) - тип переменной.
retval: (bool) - успешность сохранения.

--// Удалить переменную из общего хранилища
UnsetStorageSharedVar(var_name)
args:
  var_name (string)(required) - ключ переменной.
retval: (void)
 ```

Примеры: (общедоступного хранилища)
```lua
--// Проверить наличие переменной в общем хранилище
if HasStorageSharedVar("my-shared-name") then
  ...
end

--// Получить переменную из общего хранилища
local shared_value = GetStorageSharedVar("my-shared-name", nil)

--// Сохранить переменную в общее хранилище
SetStorageSharedVar("my-shared-name", 123456)

--// Удалить переменную из общего хранилища
UnsetStorageSharedVar("my-shared-name")

```
