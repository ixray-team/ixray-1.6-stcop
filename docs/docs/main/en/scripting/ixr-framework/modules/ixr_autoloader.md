# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

## IXR AUTOLOADER Module
A system for automatic script initialization. Any script not filtered out by the ignore list and containing a method definition on_game_start() will be automatically executed at location initialization.

* Supports file exclusion settings using wildcard patterns (name*file.*) for mass exclusion from auto-launch.
* Settings can be overridden via the module system.
* Synergizes with the signals system.
* To trigger automatically, just add a function named on_game_start() to your script.

Override settings file compatible with the addon system: __ixr_override_autoload_system.script
```lua
function configure(_ref_ixr_autoloader)

  -- single ignore by file names
  -- Ignore script files from autoloader callback search by script file names (without extension)
  _ref_ixr_autoloader.ignore_script("bind_*")
  _ref_ixr_autoloader.ignore_script("smart_*")
end

--// Supported pattern examples for filtering
text* -- text starts with phrase
text*text -- text starts and ends with phrase
*text -- text ends with phrase
text*text*text -- complex stepwise substring matching
```

Example:
```lua
--// Example implementation
function on_game_start()
  ... --// any code here will run at location initialization before the actor is spawned, so be careful what you put here
end
```
