# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0


### ffx_console_utils: `\gamedata\scripts\ixr_framework\utils\ffx_console_utils.script`
Utilities for working with the game console:
* `register_command`
* `execute_command`

---

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_console_utils.script]
--// Register a new console command.
register_command(name, _callable, tips_table)
args:
  name (string) - command name entered in the console
  _callable (function) - function called when the command executes. Receives console arguments as separate parameters through unpack.
  tips_table (table, optional) - table of autocomplete hint strings, each displayed separately in the console; defaults to an empty table.
retval: (boolean) - true if the command was registered successfully; otherwise false if name or callable is missing.

--// Execute a console command with one argument.
execute_command(command, arg)
args:
  command (string) - command (a registered or built-in command name)
  arg (string|number|boolean) - command argument. The string "true" becomes 1; "false" becomes 0. Other values are converted to strings.
retval: (none)
```
:::

### Usage examples:
```lua
-- Register a command without hints.
ffx_console_utils.register_command("mycmd", function(...)
  print("Arguments:", ...)
end)

-- Register a command with hints.
ffx_console_utils.register_command("teleport", function(x, y, z)
  -- teleport the player
  print("Teleport to", x, y, z)
end, {"x coordinate", "y coordinate", "z coordinate"})

-- Execute a command.
ffx_console_utils.execute_command("mycmd", "hello")  -- execute mycmd with argument "hello"
ffx_console_utils.execute_command("teleport", "true") -- pass 1 because "true" becomes 1
ffx_console_utils.execute_command("teleport", "false")-- pass 0
ffx_console_utils.execute_command("quit", "")        -- execute the built-in quit command
```
