# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0


### ffx_io_utils: `\gamedata\scripts\ixr_framework\utils\ffx_io_utils.script`
Утилиты для операций ввода/вывода с файлами (текстовые и бинарные):
* `write_string_file`
* `read_string_file_`
* `write_binary_file`
* `read_binary_file`

#### Описание методов:
```lua
--// Записать текстовую строку в файл.
write_string_file(text, file_path, mode)
args:
  text (string) - содержимое для записи
  file_path (string) - путь к файлу (абсолютный или относительно корня игры)
  mode (string, optional) - режим открытия файла (по умолчанию "w+" – перезапись; можно указать "a+" для добавления)
retval: (boolean) - true, если запись успешна, иначе false

--// Прочитать текстовую строку из файла.
read_string_file_(file_path, def_value)
args:
  file_path (string) - путь к файлу
  def_value (any) - значение по умолчанию, возвращаемое, если файл не существует или не читается
retval: (string|any) - содержимое файла как строка, либо def_value в случае ошибки

--// Записать бинарные данные в файл.
write_binary_file(binary_data, file_path)
args:
  binary_data (string) - бинарные данные (строка с произвольными байтами)
  file_path (string) - путь к файлу
retval: (boolean) - true, если запись успешна, иначе false

--// Прочитать бинарные данные из файла.
read_binary_file(file_path, def_value)
args:
  file_path (string) - путь к файлу
  def_value (any) - значение по умолчанию, возвращаемое, если файл не существует или не читается
retval: (string|any) - бинарные данные в виде строки, либо def_value в случае ошибки
```

### Примеры использований:
```lua
--// Запись текста в файл (перезапись)
ffx_io_utils.write_string_file("Hello, world!", "my_log.txt")

--// Запись с добавлением в конец
ffx_io_utils.write_string_file("Another line\n", "my_log.txt", "a+")

--// Чтение текстового файла (если не существует, вернёт "default")
local content = ffx_io_utils.read_string_file_("my_log.txt", "empty")
print(content)

--// Запись бинарных данных (например, закодированное изображение или сериализованный объект)
local binary = string.char(0x00, 0xFF, 0xAB, 0xCD)
ffx_io_utils.write_binary_file(binary, "data.bin")

--// Чтение бинарного файла
local loaded = ffx_io_utils.read_binary_file("data.bin", nil)
if loaded then
    --// работа с бинарными данными
end
```
