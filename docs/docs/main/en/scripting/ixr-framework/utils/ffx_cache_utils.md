# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

### ffx_cache_utils: `\gamedata\scripts\ixr_framework\utils\ffx_cache_utils.script`
Data caching utilities:
* `get_cached`
* `invalidate_cache`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_cache_utils.script]
--// Get a cached value. If the key is missing or expired, call fn_closure to get a fresh value, then cache and return it.
get_cached(key_name, fn_closure, time_invalidate, default_value, skip_errors)
args:
  key_name (string)(required) - cache key.
  fn_closure (function)(required) - function returning the value to cache.
  time_invalidate (number)(optional) - cache lifetime in milliseconds (default 60000).
  default_value (any)(optional) - default value returned on error if skip_errors = true.
  skip_errors (boolean)(optional) - if true, suppress validation errors (default false).
retval: (any) - cached or freshly computed value; on error with skip_errors = true, return default_value.

--// Invalidate (remove) a cached value by key.
invalidate_cache(key_name)
args:
  key_name (string)(required) - cache key to remove.
retval: (none)
```
:::

### Usage examples:
```lua
--// Get data and cache it for 30 seconds (30000 ms).
local data = ffx_cache_utils.get_cached("user_profile", function()
    return load_user_profile()
end, 30000)

--// Invalidate the cache after updating the profile.
ffx_cache_utils.invalidate_cache("user_profile")

--// Suppress errors and return "default".
local result = ffx_cache_utils.get_cached("expensive_calc", expensive_function, nil, 0, true)

--// Explicitly set skip_errors = false (the default).
local value = ffx_cache_utils.get_cached("settings", load_settings, 60000, nil, false)
```
