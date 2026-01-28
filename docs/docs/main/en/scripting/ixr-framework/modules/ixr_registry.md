# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

## IXR REGISTRY Module
A dynamic real-time Key *> Value store (a convenient wrapper over a global associative key-value table, for cases where using the callback system is overkill).

* Destroyed on location change.
* Instant data access speed.
* Can store any LUA types, including class references and functors.
* Global scope (pass data between scripts via shared memory, no direct references needed).
* Can be used as a mutex or semaphore for stepwise initialization of complex mechanics where you need to wait for parts to complete.
* Not saved in game saves.

```lua
--// Check if a value exists in the registry
HasRegistryValue(key, subkey)
args:
  key (string)(required) - main key.
  subkey (string)(required) - subkey (nested).
retval: (bool) - does the value exist.

--// Get a value from the registry
GetRegistryValue(key, subkey, def_value)
args:
  key (string)(required) - main key.
  subkey (string)(required) - subkey.
  def_value (mixed)(required) - default value.
retval: (mixed) - value or def_value.

--// Set a value in the registry
SetRegistryValue(key, subkey, value)
args:
  key (string)(required) - main key.
  subkey (string)(required) - subkey.
  value (mixed)(required) - value to set.
retval: (bool) - success.

--// Remove a value from the registry
UnsetRegistryValue(key, subkey)
args:
  key (string)(required) - main key.
  subkey (string)(required) - subkey.
retval: (bool) - success.
```

Examples:
```lua
--// Set a value in the registry
SetRegistryValue("my-key", "my-subkey", 123456)

--// Check if a value exists
if HasRegistryValue("my-key", "my-subkey") then
  ...
end

--// Get a value from the registry
local value = GetRegistryValue("my-key", "my-subkey", 0)

--// Remove a value from the registry
UnsetRegistryValue("my-key", "my-subkey")
```
