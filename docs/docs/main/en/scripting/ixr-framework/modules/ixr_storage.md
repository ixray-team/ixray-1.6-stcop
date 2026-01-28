# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

## IXR STORAGE Module

Allows you to store any Lua data for scripts in a separate marshal file synchronized with the current game save, without breaking the game's save files.
* Unlimited data size.
* Does not use original save files, net packets, or custom game object data.
* Load and read order does not matter (no more worrying about missing a read/write operation and breaking saves).
* Data is now named and associative, so you don't need to maintain order—just use key names.
* Full data isolation within the script namespace that requested the save.
* Globally accessible stored data is supported if needed.
* Serialization of complex objects is supported (for data previously stored in net packets, binders, etc).
* Any LUA types can be stored.

Private storage methods (isolated by the script's namespace):
```lua
--// Check if a variable exists in private storage
HasStorageVar(var_name)
args:
  var_name (string)(required) - variable key.
retval: (bool) - does the value exist.

--// Get a variable from private storage
GetStorageVar(var_name, default_value)
args:
  var_name (string)(required) - variable key.
  default_value (string|number|bool|table)(required) - default value.
retval: (string|number|bool|table) - value or default_value.

--// Save a variable to private storage
SetStorageVar(var_name, var_value, var_type)
args:
  var_name (string)(required) - variable key.
  var_value (string|number|bool|table)(required) - value to save.
  var_type (string)(required) - variable type (optional, auto-detected).
retval: (bool) - success.

--// Remove a variable from private storage
UnsetStorageVar(var_name)
args:
  var_name (string)(required) - variable key.
retval: (void)
```

Examples (private storage, script namespace isolated):
```lua
--// Check if a variable exists in private storage
if HasStorageVar(var_name) then
  ...
end

--// Get a variable from private storage
local var = GetStorageVar("my-var", nil)

--// Save a variable to private storage
SetStorageVar("my-var", 123, "number")

--// Remove a variable from private storage
UnsetStorageVar("my-var")
```

Methods for private chunk storage, isolated by script namespace and object ID (used for storing values from net packets and binders, now with per-object isolation):
```lua
--// Check if an object variable exists in chunk storage
HasStorageObjectVar(object_id, var_name)
args:
  object_id (string)(required) - object identifier.
  var_name (string)(required) - variable key.
retval: (bool) - does the value exist.

--// Get an object variable from chunk storage
GetStorageObjectVar(object_id, var_name, default_value, retrive_raw_with_type)
args:
  object_id (string)(required) - object identifier.
  var_name (string)(required) - variable key.
  default_value (mixed)(required) - default value.
  retrive_raw_with_type (bool)(required) - return raw data with type.
retval: (mixed) - value or default_value.

--// Save an object variable to chunk storage
SetStorageObjectVar(object_id, var_name, var_value, var_type)
args:
  object_id (string)(required) - object identifier.
  var_name (string)(required) - variable key.
  var_value (mixed)(required) - value to save.
  var_type (string)(required) - variable type.
retval: (bool) - success.

--// Remove an object variable from chunk storage
UnsetStorageObjectVar(object_id, var_name)
args:
  object_id (string)(required) - object identifier.
  var_name (string)(required) - variable key.
retval: (bool) - success.
```

Examples (private chunk storage, script namespace and object ID isolated):
```lua
--// Check if an object variable exists in chunk storage
if HasStorageObjectVar(self.object:id(), "game_difficulty") then
  ...
end

--// Get an object variable from chunk storage
local game_difficulty = GetStorageObjectVar(self.object:id(), "game_difficulty", 0)

--// Save an object variable to chunk storage
SetStorageObjectVar(self.object:id(), "game_difficulty", level.get_game_difficulty())

--// Remove an object variable from chunk storage
UnsetStorageObjectVar(self.object:id(), "game_difficulty")
```
