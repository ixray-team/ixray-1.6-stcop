# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

### ffx_path_utils: `\gamedata\scripts\ixr_framework\utils\ffx_path_utils.script`
Утилиты для работы с путями к файлам:
* `get_file_name`

#### Описание методов:

```lua
--// Извлечь имя файла из полного пути. Опционально удалить расширение.
get_file_name(file_path, remove_ext)
args:
  file_path (string)(required) - полный путь к файлу (может содержать обратные слэши).
  remove_ext (boolean)(optional) - если true, удаляет расширение из имени (по умолчанию false).
retval: (string) - извлечённое имя файла (с расширением или без).
```

### Примеры использований:
```lua
--// Получить имя файла с расширением
local full_name = ffx_path_utils.get_file_name("C:\\games\\scripts\\my_script.script")
SemiLog(full_name) --// "my_script.script"

--// Получить имя файла без расширения
local name_no_ext = ffx_path_utils.get_file_name("C:\\games\\scripts\\my_script.script", true)
SemiLog(name_no_ext) --// "my_script"

--// Путь без слэшей
local name = ffx_path_utils.get_file_name("config.ltx")
SemiLog(name) --// "config.ltx"

--// Удаление расширения, когда точка отсутствует
local name2 = ffx_path_utils.get_file_name("datafile", true)
SemiLog(name2) --// "datafile"
```
