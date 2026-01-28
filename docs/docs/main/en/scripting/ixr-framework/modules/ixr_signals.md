# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

## IXR SIGNALS Module
A system for centralized real-time game event broadcasting, greatly simplifying gameplay feature development by structuring chaos into a compact, elegant solution.
* Overridable interceptor options for the addon system
* Controls event connections via allowed interceptors list
* Supports real-time editing of interceptors
* Subscribe to events
* Broadcast events
* Unsubscribe from events
* Supports closures as functors for subscriptions
* Easily implement your own events for your project
* Synergizes with the autoloader module, allowing wireless event connections for scripts (just put the script file in the game folder)

External overridable settings are stored in __ixr_override_signals_intercepts.script, compatible with the addon system for overriding event interceptors:
```lua
--// Example event interceptor configuration for __ixr_override_signals_intercepts.script
function configure(_ref_ixr_signals)
  --// Example event interceptor registration
  _ref_ixr_signals.add_intercept("on_item_take", {"item_game_object", "is_actor_exists_bool"}) -- actor
  ...
end

--// Method signature for registering an interceptor: _ref_ixr_signals.add_intercept
args:
  event_name (string)(required) - event name
  args_table (table)(required) - argument list (used as documentation)
retval: (void)
```

Events without a registered interceptor by name will be ignored. This is intentional so interceptors can be used to disable unwanted event processing.

When an event is triggered by name via SendScriptCallback, it is broadcast to all subscribers who registered a callback with RegisterScriptCallback.

```lua
--// Add a callback interceptor
AddIntercept(name, args_map)
args:
  name (string)(required) - callback name to intercept.
  args_map (table)(required) - argument mapping table.
retval: (bool) - success.

--// Remove a callback interceptor
RemoveIntercept(name)
args:
  name (string)(required) - callback name.
retval: (bool) - success.

--// Register a script callback
RegisterScriptCallback(name, func_or_userdata)
args:
  name (string)(required) - callback name.
  func_or_userdata (function|userdata)(required) - function or object to call.
retval: (bool) - success.

--// Unregister a script callback
UnregisterScriptCallback(name, func_or_userdata)
args:
  name (string)(required) - callback name.
  func_or_userdata (function|userdata)(required) - function or object to remove.
retval: (bool) - success.

--// Call a script callback
SendScriptCallback(name, ...)
args:
  name (string)(required) - callback name.
  ... (variadic list) - arguments to pass to the callback.
retval: (mixed) - result of the callback execution.
```

Examples:
```lua
--// Add a callback interceptor
AddIntercept("my-event-name", {"game_object"})

--// Remove a callback interceptor
RemoveIntercept("my-event-name")

--// Register a script callback
RegisterScriptCallback("my-event-name", test_event)

--// Unregister a script callback
UnregisterScriptCallback("my-event-name", test_event)

--// Call a script callback with arbitrary arguments
SendScriptCallback("my-event-name", ...)

--// Example implementation with autoloader
function on_game_start()
  --// subscribe to event and set closure function (method 1: inline)
  RegisterScriptCallback("my-event-name",
    function (obj)
      ...
    end
   )

  --// subscribe to event and set closure function (method 2: by reference)
  RegisterScriptCallback("my-event-name", this.test_event)
end

function test_event(obj)
  --// will be called when SendScriptCallback("my-event-name", ...) is triggered
end
```

List of events with arguments:
```ini
("level_input_on_key_press",        {})
("on_key_press",                    {"dik_key_number"})
("on_key_release",                  {"dik_key_number"})
("on_key_hold",                     {"dik_key_number"})
("on_mouse_move",                   {"dx_number", "dy_number"})
```
