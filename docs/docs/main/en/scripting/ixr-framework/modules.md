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

Next, to make the framework see your module, explicitly list it in the override settings file compatible with the addon system:
::: code-group
```lua {20} [gamedata\__ixr_override_framework_load_sub_modules.script]
function configure(_ref_ixr_framework)
  -- use concrete script names for include to framework, after module allow by alias name included in module info in module code or script name is included
  --------------------------------------------
  -- Modules loaded first ->
  --------------------------------------------
  _ref_ixr_framework.load_module_by_script_name("ixr_module_signals") -- alias:[ixr_signals]
  _ref_ixr_framework.load_module_by_script_name("ixr_module_global_registry") -- alias:[ixr_registry]
  _ref_ixr_framework.load_module_by_script_name("ixr_module_options") -- alias:[ixr_options]
  _ref_ixr_framework.load_module_by_script_name("ixr_module_storage") -- alias:[ixr_storage]

  --------------------------------------------
  -- Modules loaded in the middle ->
  --------------------------------------------
  _ref_ixr_framework.load_module_by_script_name("ixr_module_timers") -- alias:[ixr_timers]
  _ref_ixr_framework.load_module_by_script_name("ixr_module_triggers") -- alias:[ixr_triggers]

  --------------------------------------------
  -- Modules loaded last ->
  --------------------------------------------
  _ref_ixr_framework.load_module_by_script_name("ixr_module_autoloader") -- alias:[ixr_autoloader] !!! required register this module latest (autoload scripts entry points after register other modules)
end


--// Pass the full script file name. The module can then be accessed through the alias declared in get_module_info().
```
:::

Global framework methods for accessing modules:
```lua
--// Check whether a module is loaded.
IsModuleLoaded(script_or_alias_name)
args:
  script_or_alias_name (string)(required) - script name or module alias.
retval: (bool) - whether the module is loaded.

--// Get a reference to a module.
GetModule(script_or_alias_name)
args:
  script_or_alias_name (string)(required) - script name or module alias.
retval: (reference|exception) - module reference or an exception.

--// Invoke a callback if the module exists.
ClosureModuleIsExists(script_or_alias_name, callback_fn, def_value)
args:
  script_or_alias_name (string)(required) - script name or module alias.
  callback_fn (function)(required) - callback to invoke if the module exists.
  def_value (mixed)(required) - default value if the module does not exist.
retval: (mixed|false) - callback_fn result or def_value.
```

### Examples of accessing modules by name:
```lua
if IsModuleLoaded("my-module") then
    GetModule("my-module").my_method_in_module() --// A known method in the module.
end

--// Invoke a callback if the module exists.
ClosureModuleIsExists("my-module",
  function(module)
    module.my_method_in_module() --// A known method in the module.
  end
)
```
