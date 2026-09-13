# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0


### base64: \gamedata\scripts\ixr_framework\utils\libs\ffx_base64_lib.script
Библиотека для работы с Base64:
* `encode(input): string`
* `decode(input): string`

#### Описание методов:
```lua
--// Закодировать данные в Base64.
encode(input)
args:
  (input) --// Входная строка данных
retval: (string) --// Закодированная в base64 строка

--// Декодировать данные из Base64.
decode(input)
args:
  (input) --// Входная закодированная в base64 строка
retval: (string) --// Расскодированная из base64 строка
```

#### Примеры использований:
```lua
local original = "Hello World"
local encoded = ffx_base64_lib.encode(original)
local decoded = ffx_base64_lib.decode(encoded)

SemiLog("original:" ..tostring(original))
SemiLog("encoded:" ..tostring(encoded))
SemiLog("decoded:" ..tostring(decoded))
```
