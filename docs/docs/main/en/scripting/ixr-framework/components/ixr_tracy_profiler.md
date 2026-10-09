
# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

## IXR TRACY PROFILER component
* Controls profiling of Lua scripts.
* Download the profiler from the releases in the [Tracy repository](https://github.com/wolfpld/tracy).

Control through global methods:
```lua
--// Begin a profiling event with the given name. The event appears in Tracy if the profiler is connected. Applies to the current script context.
PROF_EVENT_BEGIN(name)
args:
  name (string)(required) - profiling event name.
retval: (none) - the function returns nothing.

--// End the current profiling event. Call after the corresponding PROF_EVENT_BEGIN. Applies to the current script context.
PROF_EVENT_END()
args:
  (none)
retval: (none) - the function returns nothing.

--// Execute a function inside a profiling event. If the profiler is not connected, the function is called directly. Returns the function results. Applies to the current script context.
PROF_EVENT_CLOSURE(name, callable)
args:
  name (string)(required) - profiling event name.
  callable (function)(required) - function to execute and profile.
retval: (*) - return values of callable, or nil if callable is not provided.
```

### Implementation examples:
```lua
--// Example using PROF_EVENT_BEGIN and PROF_EVENT_END.
--// Start profiling a code block, perform an operation and end the event.
PROF_EVENT_BEGIN("Load data from file") -- begin profiling
local data = load_data_from_file("config.json")
process_data(data)
PROF_EVENT_END() -- end profiling

--// Example using PROF_EVENT_CLOSURE.
--// Wrap a function call in a profiling event. If the profiler is connected, the event is recorded; otherwise the function runs without profiling.
local result = PROF_EVENT_CLOSURE("Save data", function()
    return save_to_database(data)
end)

--// Nested event example.
--// Create nested zones for detailed analysis.
PROF_EVENT_BEGIN("Process request")
    PROF_EVENT_BEGIN("Parse parameters")
    local params = parse_request(request)
    PROF_EVENT_END()

    PROF_EVENT_BEGIN("Main logic")
    local response = handle_request(params)
    PROF_EVENT_END()

    PROF_EVENT_BEGIN("Build response")
    local output = format_response(response)
    PROF_EVENT_END()
PROF_EVENT_END()
```

### Profiling examples that preserve the original return value
```lua
--// Example: compute a factorial with profiling through a closure.
--// The function result is returned to the caller; the calculation is wrapped in PROF_EVENT_CLOSURE.
--// Function arguments are available inside the closure without additional handling.
function calculate_factorial(n)
    return PROF_EVENT_CLOSURE("Calculate factorial", function()
        if n <= 1 then return 1 end
        local result = 1
        for i = 2, n do
            result = result * i
        end
        return result
    end)
end

--// Call the function: the result is returned and profiling runs automatically.
SemiLog("Factorial 10 =" .. calculate_factorial(10))

```

### Special case:
```lua
-- A profiled wrapper that accepts arbitrary arguments, passes them to the function,
-- and returns all results while preserving profiling.
function profiled_call(func, ...)
    local args = {...}
    return PROF_EVENT_CLOSURE("Call function " .. tostring(func), function()
        return func(table.unpack(args)) -- unpack the arguments ... in their original order
    end)
end

-- Usage example:
local result = profiled_call(math.max, 10, 20, 30, 40)
print("Maximum =", result)
```
