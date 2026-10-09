# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

### ffx_callable_utils: `\gamedata\scripts\ixr_framework\utils\ffx_callable_utils.script`
Utilities for checking scripts and functions, inspecting the call stack and safely executing code in a sandbox:
* `is_script_present_in_g_file`
* `has_script_function_exists`
* `is_script_callable_by_name`
* `is_function_callable_by_name`
* `is_function_args_count_equal_by_ref`
* `is_function_args_count_equal_by_name`
* `find_caller_source`
* `find_caller_source_tracy`
* `get_call_stack_trace`
* `sandbox`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_callable_utils.script]
--// Check whether a global script with the given name exists.
is_script_present_in_g_file(script_name)
args:
  script_name (string)(required) - script name.
retval: (boolean) - true if the script exists in _G; otherwise false.

--// Check whether a function with the given name exists in a script.
has_script_function_exists(script_name, function_name)
args:
  script_name (string)(required) - script name.
  function_name (string)(required) - function name.
retval: (boolean) - true if the function exists in the script; otherwise false.

--// Check whether a global script is callable (exists and is a table).
is_script_callable_by_name(script_name)
args:
  script_name (string)(required) - script name.
retval: (boolean) - true if the script is an available table; otherwise false.

--// Check whether a function exists and is callable in the given script.
is_function_callable_by_name(script_name, function_name)
args:
  script_name (string)(required) - script name.
  function_name (string)(required) - function name.
retval: (boolean) - true if the function exists and is callable; otherwise false.

--// Check whether a function reference has exactly the given number of arguments.
is_function_args_count_equal_by_ref(_func, count_args)
args:
  _func (function)(required) - function reference.
  count_args (number)(required) - expected number of arguments.
retval: (boolean) - true if the argument count matches; otherwise false.

--// Check whether a named function in a script has the given number of arguments.
is_function_args_count_equal_by_name(script_name, function_name, count_args)
args:
  script_name (string)(required) - script name.
  function_name (string)(required) - function name.
  count_args (number)(required) - expected number of arguments.
retval: (boolean) - true if the argument count matches; otherwise false.

--// Find the source file of the calling code at the given stack level.
find_caller_source(level)
args:
  level (number)(required) - stack level (0 — current function, 1 — caller, and so on).
retval: (string) - source identifier (file path without extension), or "unknown" if not found.

--// Find the source file and optional line numbers for Tracy integration.
find_caller_source_tracy(level, use_line)
args:
  level (number)(required) - stack level to inspect.
  use_line (boolean)(optional) - if true, include line_begin and line_end in the returned table (default false).
retval: (table) - table containing file_name (string), plus line_begin and line_end if use_line=true.

--// Get a string representation of the current call stack.
get_call_stack_trace()
args:
  (none)
retval: (string) - call stack separated by " -> ", for example "script1.func1(...) -> script2.func2(...)".

--// Safely execute a function from the given script in a sandbox using pcall. Returns the result, or nil on error.
sandbox(file_name, function_name, ...)
args:
  file_name (string)(required) - script file name without extension.
  function_name (string)(required) - function name to call.
  ... (any)(optional) - arguments passed to the function.
retval: (any) - return values of the called function, or nil on error.
```
:::

### Usage examples:
```lua
--// Check whether a script exists.
if ffx_callable_utils.is_script_present_in_g_file("my_script") then
    SemiLog("Script my_script is loaded")
end

--// Check whether a function exists in the script.
local has_func = ffx_callable_utils.has_script_function_exists("my_script", "do_something")
if has_func then
    SemiLog("Function do_something exists")
end

--// Check whether the function is callable.
if ffx_callable_utils.is_function_callable_by_name("my_script", "do_something") then
    my_script.do_something()
end

--// Check the argument count of a function reference.
local func_ref = function(a, b) return a + b end
local ok = ffx_callable_utils.is_function_args_count_equal_by_ref(func_ref, 2)
SemiLog(string.format("Expected 2 arguments: %s", tostring(ok)))

--// Check the argument count of a named function.
local equal = ffx_callable_utils.is_function_args_count_equal_by_name("my_script", "do_something", 3)

--// Get the calling source file name.
local caller_file = ffx_callable_utils.find_caller_source(2)
SemiLog(string.format("Called from: %s", caller_file))

--// Get Tracy information.
local tracy_data = ffx_callable_utils.find_caller_source_tracy(3, true)
SemiLog(string.format("[%s (%d,%d)]", tracy_data.file_name, tracy_data.line_begin, tracy_data.line_end))

--// Get the full call stack.
local trace = ffx_callable_utils.get_call_stack_trace()
SemiLog(string.format("Stack: %s", trace))

--// Safely call a function from another script.
local result = ffx_callable_utils.sandbox("math_utils", "add", 5, 3)
if result then
    SemiLog(string.format("Result: %s", tostring(result)))
else
    SemiLog("Execution error")
end
```
