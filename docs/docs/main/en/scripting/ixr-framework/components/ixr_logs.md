
# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

## IXR LOGS logging component
* Provides centralized logging for scripts, with settings tied to the calling script. Each script can choose file output, console output or both without implementing its own logging system.
* Simplifies logging configuration without additional logic in each script.
* Logs are written to `$logs$\ixr_framework_logs\` by default. The `$logs$` path is configured in `fsgame.ltx`.
* Each calling script gets a named log, for example `__log__my_script.txt`.
* All methods in this section apply to the script that calls them and do not affect other scripts. This allows individual logging settings for each script.

Centralized logging features:
* Logging settings are isolated and apply to the script that calls one of the following methods:
  * IXRLogUseFileLog
  * IXRLogUseConsoleLog
  * IXRLogUseTimeInLog
  * IXRLogEmptyLine
  * IXRLogClear
  * IXRLog

Control through global methods:
```lua

--// Enable or disable file logging for the calling script, rather than globally.
IXRLogUseFileLog(flag, custom_script)
args:
  flag (boolean)(required)          - true — enable; false — disable.
  custom_script (string)(optional)  - target script identifier (file name). If omitted, the calling script is used.
retval: (any) - result of ixr_logs.set_use_file_log.

--// Enable or disable console output for the calling script, rather than globally.
IXRLogUseConsoleLog(flag, custom_script)
args:
  flag (boolean)(required)          - true — enable; false — disable.
  custom_script (string)(optional)  - target script identifier (file name). If omitted, the calling script is used.
retval: (any) - result of ixr_logs.set_use_console_log.

--// Enable or disable timestamps for the calling script, rather than globally.
IXRLogUseTimeInLog(flag, custom_script)
args:
  flag (boolean)(required)          - true — include timestamps; false — omit them.
  custom_script (string)(optional)  - target script identifier (file name). If omitted, the calling script is used.
retval: (any) - result of ixr_logs.set_use_time.

--// Write a message to the calling script log. Uses its current file, console and timestamp settings, which are enabled by default. Disabling any of them through IXRLogUse... changes the output.
IXRLog(text, clear_log, custom_script)
args:
  text (string)(required)           - message to write.
  clear_log (boolean)(optional)     - if true, clear the log before writing (default false).
  custom_script (string)(optional)  - target script identifier (file name). If omitted, the calling script is used.
retval: (any) - result of ixr_logs.log.

--// Insert one or more empty lines into the calling script log.
IXRLogEmptyLine(pereat_cnt, custom_script)
args:
  pereat_cnt (number)(required)     - number of empty lines to insert.
  custom_script (string)(optional)  - target script identifier (file name). If omitted, the calling script is used.
retval: (any) - result of ixr_logs.empty_line.

--// Clear the entire log for the calling script.
IXRLogClear(custom_script)
args:
  custom_script (string)(optional)  - target script identifier (file name). If omitted, the calling script is used.
retval: (any) - result of ixr_logs.clear_log.
```

### Implementation examples:
```lua
--// Define constants at the beginning of the script for convenience.
local USE_FILE_LOG = true
local USE_CONSOLE_LOG = false
local USE_TIME_IN_LOGS = true

--// Initialize settings once. They can be updated later, but avoid repeatedly applying unchanged settings.
IXRLogUseFileLog(USE_FILE_LOG)
IXRLogUseConsoleLog(USE_CONSOLE_LOG)
IXRLogUseTimeInLog(USE_TIME_IN_LOGS)

--// Use the Autoloader module to initialize the settings once.
function on_game_start()
  --// This function runs when the main menu starts and again when level simulation begins.
  IXRLogUseFileLog(USE_FILE_LOG)
  IXRLogUseConsoleLog(USE_CONSOLE_LOG)
  IXRLogUseTimeInLog(USE_TIME_IN_LOGS)
end

--// Write a message from an arbitrary function.
function some_method()
  IXRLog("Hello world") -- with these settings, output goes to the file; console output is disabled.
end

--// With no explicit configuration, output goes to the file and console with date and time.
--// Settings can also be changed directly without defining variables:
IXRLogUseFileLog(true)
IXRLogUseConsoleLog(true)
IXRLogUseTimeInLog(true)

--// All methods here apply to the calling script and do not affect other scripts, allowing individual logging settings.
```
