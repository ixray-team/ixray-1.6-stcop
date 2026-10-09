# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0


### ffx_io_utils: `\gamedata\scripts\ixr_framework\utils\ffx_io_utils.script`
Utilities for text and binary file input/output:
* `write_string_file`
* `read_string_file_`
* `write_binary_file`
* `read_binary_file`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_io_utils.script]
--// Write a text string to a file.
write_string_file(text, file_path, mode)
args:
  text (string) - content to write
  file_path (string) - file path, absolute or relative to the game root
  mode (string, optional) - file open mode (default "w+" to overwrite; use "a+" to append)
retval: (boolean) - true if the write succeeded; otherwise false

--// Read a text string from a file.
read_string_file_(file_path, def_value)
args:
  file_path (string) - file path
  def_value (any) - default value returned if the file is missing or unreadable
retval: (string|any) - file content as a string, or def_value on error

--// Write binary data to a file.
write_binary_file(binary_data, file_path)
args:
  binary_data (string) - binary data as a string of arbitrary bytes
  file_path (string) - file path
retval: (boolean) - true if the write succeeded; otherwise false

--// Read binary data from a file.
read_binary_file(file_path, def_value)
args:
  file_path (string) - file path
  def_value (any) - default value returned if the file is missing or unreadable
retval: (string|any) - binary data as a string, or def_value on error
```
:::

### Usage examples:
```lua
--// Write text to a file, overwriting it.
ffx_io_utils.write_string_file("Hello, world!", "my_log.txt")

--// Append to the end of the file.
ffx_io_utils.write_string_file("Another line\n", "my_log.txt", "a+")

--// Read a text file; return "default" if it does not exist.
local content = ffx_io_utils.read_string_file_("my_log.txt", "empty")
print(content)

--// Write binary data, such as an encoded image or serialized object.
local binary = string.char(0x00, 0xFF, 0xAB, 0xCD)
ffx_io_utils.write_binary_file(binary, "data.bin")

--// Read a binary file.
local loaded = ffx_io_utils.read_binary_file("data.bin", nil)
if loaded then
    --// process the binary data
end
```
