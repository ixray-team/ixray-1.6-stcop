
# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

## IXR THROTTLERS module
Limits call frequency to optimize code, replacing simple timers used to skip frequent calls.

```lua
--// Check whether a call is blocked by throttling.
IsActionThrottled(name, interval_ms)
args:
  name (string)(required) - Unique throttler name.
  interval_ms (int)(required) - Minimum interval between calls, in milliseconds.
retval: (bool) (returns true if calls occur more often than the interval specified in the second argument)


--// Check whether a call is allowed by throttling (the inverse of the method above).
IsNotActionThrottled(name, interval_ms)
args:
  name (string)(required) - Unique throttler name.
  interval_ms (int)(required) - Minimum interval between calls, in milliseconds.
retval: (bool) (returns true when the call is allowed; false while throttled)
```

### Usage examples:
```lua
--// Subscribe to actor updates for this example.
function on_game_start(callbackRegistrator)
	RegisterScriptCallback("actor_on_update", actor_on_update)
end

--// Actor update handler that runs once every 250 ms.
function actor_on_update()
  if IsActionThrottled("my_test_throttler_01", 250) then
    return --// Stop if updates occur more often than once every 250 milliseconds.
  end
  
  lazy_tick()
end

--// Example of a call executed once every 250 ms.
local n=0
function lazy_tick()
  n = n +1
  SemiLog("TICK:"..tostring(n))
end
```
