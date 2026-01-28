# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

The framework system supports a modular architecture. Each module contains self-sufficient logic and does not reference other modules internally for atomicity and independence.

To be recognized as a module, a script must implement the following interface:
```lua
function get_module_info()
	return {
		-- (string) module alias for framework access
		alias_name = "modulename",
		-- (string) category for info: ["ixr_system", "sub_system", "sub_gameplay", "sub_utils"] or custom
		category = "sub_system",
		-- (float) version
		version = 1.1,
		-- (string) entry point method name
		init_function_name = "init",
		-- (string|string[]) author(s)
		authors = "nickname",
		-- (string) short description
		description = "text",
	}
end

local is_init = false --// initialization flag, default false

--// method called by the framework (subscribe to events, get data, etc.)
function init()
  if is_initialized() then
    return true --// prevent re-initialization
  end

  is_init = true
  return is_initialized() --// return current state for system info
end

--[[
Description: Check initialization.
Return: (bool) - true if module is initialized
]]
function is_initialized()
  return is_init
end
```

Next, to make the framework see your module, you must explicitly list it in the override settings file compatible with the addon system: __ixr_override_framework_load_sub_modules.script
```lua
function configure(_ref_ixr_framework)
  -- use concrete script names for include to framework, after module allow by alias name included in module info in module code or script name is included
  --------------------------------------------
  -- Modules loaded first ->
  --------------------------------------------
  _ref_ixr_framework.load_module_by_script_name("ixr_module_signals") -- alias:[ixr_signals]
  _ref_ixr_framework.load_module_by_script_name("ixr_module_global_registry") -- alias:[ixr_registry]
  _ref_ixr_framework.load_module_by_script_name("ixr_module_options") -- alias:[ixr_options]
  _ref_ixr_framework.load_module_by_script_name("ixr_module_storage") -- alias:[ixr_storage]
```
