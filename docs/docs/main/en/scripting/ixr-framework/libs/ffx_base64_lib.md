# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0


### base64: \gamedata\scripts\ixr_framework\utils\libs\ffx_base64_lib.script
Library for working with Base64:
* `encode(input): string`
* `decode(input): string`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\libs\ffx_base64_lib.script]
--// Encode data as Base64.
encode(input)
args:
  (input) --// Input data string
retval: (string) --// Base64-encoded string

--// Decode Base64 data.
decode(input)
args:
  (input) --// Input Base64-encoded string
retval: (string) --// Decoded string
```
:::

### Usage examples:
```lua
local original = "Hello World"
local encoded = ffx_base64_lib.encode(original)
local decoded = ffx_base64_lib.decode(encoded)

SemiLog("original:" ..tostring(original))
SemiLog("encoded:" ..tostring(encoded))
SemiLog("decoded:" ..tostring(decoded))
```
