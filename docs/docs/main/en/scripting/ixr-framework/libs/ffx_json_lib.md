# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0


### json: \gamedata\scripts\ixr_framework\utils\libs\ffx_json_lib.script
Library for working with JSON:
* `json_decode(input): string`
* `json_encode(input): string`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\libs\ffx_json_lib.script]
--// Encode data as JSON.
json_encode(input)
args:
  (input) (required table) --// Input data table
retval: (string) --// JSON-encoded string

--// Decode JSON data.
json_decode(input)
args:
  (input) (required string) --// Input JSON-encoded string
retval: (string) --// Decoded table
```
:::

### Usage examples:
```lua
local original = {true, false, 0, 1, 2, 3, 4, 5.2, "test"}
local encoded = ffx_json_lib.json_encode(original)
local decoded = ffx_json_lib.json_decode(encoded)

SemiLog("original:" ..tostring(ffx_dump_utils.var_export(original)))
SemiLog("encoded:" ..tostring(encoded))
SemiLog("decoded:" ..tostring(ffx_dump_utils.var_export(decoded)))
```
