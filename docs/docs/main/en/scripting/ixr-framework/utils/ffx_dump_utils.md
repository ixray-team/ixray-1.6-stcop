# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0


### ffx_dump_utils: `\gamedata\scripts\ixr_framework\utils\ffx_dump_utils.script`
Utilities for dumping data, logging and debugging:
* `var_export`
* `var_dump_to_console_log`
* `write_to_console_log`
* `var_dump_to_file_log`
* `write_to_file_log`
* `send_message`
* `AssertWithCaller`

---

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_dump_utils.script]
--// Generate a string representation of a table with support for recursion, functions and metadata.
var_export(tbl, indent, visited, is_subtable)
args:
  tbl (table) - table to dump (non-table input is wrapped in {tbl})
  indent (number, optional) - current indentation level (default 0)
  visited (table, optional) - internal parameter tracking visited tables to prevent cycles
  is_subtable (boolean, optional) - internal flag indicating whether the current table is nested
retval: (string) - formatted table dump string

--// Print an object dump to the game console through SemiLog with a source prefix (the calling script by default).
var_dump_to_console_log(object, log_prefix)
args:
  object (any) - object to dump
  log_prefix (string, optional) - log line prefix; if omitted, determined automatically through ffx_callable_utils.find_caller_source
retval: (boolean) - always true

--// Print arbitrary text to the game console with a prefix.
write_to_console_log(text, log_prefix)
args:
  text (string) - text to print
  log_prefix (string, optional) - prefix; determined automatically if omitted
retval: (boolean) - always true

--// Save an object dump to a text file in the game log directory.
var_dump_to_file_log(object, overwrite, log_prefix)
args:
  object (any) - object to dump
  overwrite (boolean, optional) - if true, overwrite the file; otherwise append (default false)
  log_prefix (string, optional) - file name prefix; determined automatically if omitted
retval: (boolean) - true if the write succeeded; otherwise false

--// Write arbitrary text to a log file.
write_to_file_log(text, overwrite, log_prefix)
args:
  text (string) - text to write
  overwrite (boolean, optional) - if true, overwrite the file; otherwise append (default false)
  log_prefix (string, optional) - file name prefix; determined automatically if omitted
retval: (boolean) - true if the write succeeded; otherwise false

--// Send a popup message (hint) to the actor, if present.
send_message(text)
args:
  text (string) - message text
retval: (none)

--// Log a critical error with the source and call stack, then call assert(false) and exit the game with exit(0).
AssertWithCaller(level, error_source, error_message)
args:
  level (number) - stack level used to identify the calling script through ffx_callable_utils.find_caller_source
  error_source (string) - error source identifier, such as a module name
  error_message (string) - error message
retval: (none) – the function does not return control
```
:::

### Usage examples:
```lua
--// Dump a table to the console.
local data = {a = 1, b = {x = 10, y = 20}, func = function() end}
ffx_dump_utils.var_dump_to_console_log(data, "MyDump")

--// Write text to the console.
ffx_dump_utils.write_to_console_log("Hello from script", "Info")

--// Dump to a file, overwriting it.
ffx_dump_utils.var_dump_to_file_log(data, true, "dump_prefix")

--// Append text to a file.
ffx_dump_utils.write_to_file_log("Some log line", false, "log_prefix")

--// Send a message to the player.
ffx_dump_utils.send_message("Attention! Quest updated.")

--// Generate an error with context.
ffx_dump_utils.AssertWithCaller(3, "MyModule", "Invalid argument value")
```
