# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0


### json: \gamedata\scripts\ixr_framework\utils\libs\ffx_json_lib.script
Библиотека для работы с JSON:
* `json_decode(input): string`
* `json_encode(input): string`

#### Описание методов:
```lua
--// Закодировать данные в JSON.
json_encode(input)
args:
  (input) (required table) --// Входная таблица данных
retval: (string) --// Закодированная в JSON строка

--// Декодировать данные из JSON.
json_decode(input)
args:
  (input) (required string) --// Входная закодированная в JSON строка
retval: (string) --// Расскодированная из JSON таблица
```

#### Примеры использований:
```lua
local original = {true, false, 0, 1, 2, 3, 4, 5.2, "test"}
local encoded = ffx_json_lib.json_encode(original)
local decoded = ffx_json_lib.json_decode(encoded)

SemiLog("original:" ..tostring(ffx_dump_utils.var_export(original)))
SemiLog("encoded:" ..tostring(encoded))
SemiLog("decoded:" ..tostring(ffx_dump_utils.var_export(decoded)))
```
