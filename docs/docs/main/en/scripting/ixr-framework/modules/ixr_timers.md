# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

## IXR TIMERS Module
Allows you to create timers (saved in game saves) with a payload as a script function to be executed when the timer expires.

Timer objects are saved in the game save.

```lua
--// Create a timer
TimerCreate(name, loop, auto_play, left_milliseconds, callable_fn, ...)
args:
  name (string)(required) - unique timer name.
  loop (bool)(required) - repeat flag.
  auto_play (bool)(required) - auto start flag.
  left_milliseconds (int)(required) - time to trigger in ms.
  callable_fn (function)(...) - function to call on timer end (receives ... as arguments).
  ... (arguments for callable_fn)

retval: (table) returned as OOP methods of the timer:
  self:name(): string - get timer name.
  self:exists(): bool - check if timer exists.
  self:play(): bool - start timer.
  self:stop(): bool - stop timer.
  self:delete(): bool - delete timer.

--// Check if a timer exists by name
IsTimerExists(name)
args:
  name (string)(required) - unique timer name.
retval: (bool) true if timer exists, else false

--// Start a timer by name (ignored if not found or already running)
TimerPlay(name)
args:
  name (string)(required) - unique timer name.
retval: (bool) true if started, else false

--// Stop a timer by name (ignored if not found or not running)
TimerStop(name)
args:
  name (string)(required) - unique timer name.
retval: (bool) true if stopped, else false

--// Delete a timer by name (ignored if not found)
TimerDelete(name)
args:
  name (string)(required) - unique timer name.
retval: (bool) true if deleted, else false
```

Examples:
```lua
--// wait for new game event
function on_game_start(callbackRegistrator)
  RegisterScriptCallback("new_game_created", this.on_new_game_started)
end

function on_new_game_started()
  --// here you can define timers, e.g. at the start of a new game
end

--// Automatically starts and repeats every 5 seconds.
TimerCreate(
  "my-timer-01", true, true, 5000, 
  function (a, b) 
    ...
  end, 
  "Variable A", "Variable B"
 )

--// Automatically starts and fires once after 5 seconds.
TimerCreate(
  "my-timer-02", false, true, 5000, 
  function (a, b) 
    ...
  end, 
  "Variable A", "Variable B"
 )

--// Creates a timer that fires once after 5 seconds but is not started immediately (can be started later, allows for complex delayed chains).
TimerCreate(
  "my-timer-02", false, false, 5000, 
  function (a, b) 
    ...
  end, 
  "Variable A", "Variable B"
):play()

--// Check if a timer exists by name
if IsTimerExists("my-timer-02") then
  ...
end

--// Start a timer by name
TimerPlay("my-timer-02")

--// Stop a timer by name
TimerStop("my-timer-02")

--// Delete a timer by name
TimerDelete("my-timer-02")
```
