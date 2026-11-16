# Launch arguments

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.0

## Overview

Pass launch arguments on the executable command line or in the shortcut arguments field. `-ltx` selects a console command script. Pass launch arguments separately. The values for `-fsltx`, `-ltx`, `-overlaypath`, and `-demomode` are single tokens without spaces.

## Syntax

```powershell
.\xrEngine.exe -fsltx fsgame.ltx -ltx user.ltx -r4
```

## Game and file system

| Argument | Effect |
| --- | --- |
| `-fsltx <file>` | Selects the startup file-system configuration. |
| `-ltx <file>` | Selects the startup console command script. |
| `-overlaypath <directory>` | Redirects the `$app_data_root$` and `$logs$` roots. Supply a directory. |
| `-auto_load_arch` | Allows game archives to be mounted automatically. |
| `-nolog` | Disables the log file in the game file-system locator. |
| `-start <arguments>` | Passes arguments to the `start` console command on startup. |
| `-load <save>` | Passes arguments to the `load` console command on startup. |
| `-demomode <file>` | Reads a list of demo scenarios at startup. Supply a file name. |
| `-noprefetch` | Skips game resource prefetching in `IGame_Persistent`. |
| `-nosound` | Starts the game without the normal sound subsystem. |
| `-designer` | Enables a server mode that keeps some ALife objects from switching online. |


Put `-start` or `-load` at the end. The engine passes the remainder of the command line to the console.

## Renderer and debugging

| Argument | Effect |
| --- | --- |
| `-r4`, `-r2` | Select R4 or R2 from available renderers. If both are supplied, startup checks R4 first. With neither, startup reads `renderer` from the LTX. |
| `-no_center_screen` | Skips centering when creating the window. |
| `-perfhud_hack` | Uses the PerfHUD branch for renderer enumeration. |
| `-dxdebug` | Requests D3D11 device debug mode. |
| `-disasm` | Enables shader disassembly. |
| `-nonvs` | Disables NVIDIA depth/stencil in R2. This capability is already disabled in R4. |
| `-noshadows` | Sets the no-shadows flag in R2 and R4. |
| `-nodistort` | Disables the distortion pass in R2 and R4. |
| `-no_occq` | Disables queries in the renderer's occlusion query manager. |
| `-renderdoc` | Preserves D3D11 resource debug names in code paths that use the flag. Start captures from RenderDoc. |
| `-xclsx` | Suppresses red diagnostics text. |
| `-nes_texture_storing` | Records used textures when unloading a level. |
| `-skinw` | Sets `forceskinw` in R1 and R2. |
| `-tsh` | Sets `Tshadows` in R2. |
| `-no_debug_panel` | Skips initialization of the ImGui debug panel. |
| `-debug_ge` | Sets the game event debugging flag. |
| `-silent_error_mode` | Exits with code `-1` before crash reporting in the Windows termination handler. |
| `-no_call_stack_assert` | Skips the error window in the Windows assertion reporting path. |

## Lua

| Argument | Effect |
| --- | --- |
| `-keep_lua` | Skips Lua reinitialization in `restart_all()` when restarting ALife. |
| `-use_callstack` | Enables call stacks for Lua errors. |
| `-ldbg` | Enables LDBG handling in builds with Lua debugger support. |

## Automated tests

Available from IX-Ray Platform 1.4.

| Argument | Effect |
| --- | --- |
| `-autotest` | Runs automated frame measurements and writes a report. |
| `-autotest_frames N` | Measured frames. Default `120`. |
| `-autotest_warmup N` | Warmup frames. Default `150`. |
| `-autotest_timeout S` | Timeout in seconds. Default `300`. |
| `-autotest_out <path>` | Report path. Default `$logs$/autotest.csv`. |
| `-autotest_hash` | Includes the frame image CRC in the report. |
| `-autotest_shot [N]` | Writes a TGA every `N` measured frames. Omitting `N` captures every frame. |
| `-autotest_post_cmd <command>` | Executes one console command on the first frame after warmup. Place it before `-autotest_cmd`. |
| `-autotest_cmd <commands>` | Executes semicolon-separated startup commands. Consumes the rest of the argument string. |

```bat
xrEngine.exe -r4 -autotest -autotest_frames 120 -autotest_cmd start server(jupiter/single/alife/new) client(localhost)
```

An autotest returns `0` when it reaches the requested frame count without logging any `[error]` lines. Otherwise it returns `1`.

## SDK and utilities

| Executable | Arguments and reference |
| --- | --- |
| `LevelEditor.exe` | `-ltx <file>` selects the startup LTX. Shared core flags `-build` and `-ebuild` enable build-time file copying modes. |
| `xrLevelBuilder.exe` | `-fsltx <file>` selects the file-system configuration. Geometry, light, and AI settings are configured in the [compiler UI](../utilities/compilers.md). `-clear_temp_files` deletes the input `build.aimap` after the AI loader reads it. |
| `xrCompress.exe` | See the separate syntax for [packing, unpacking, and diff](../utilities/compressor.md). |
| `xrServerCLI.exe` | See [dedicated server startup](../platforms/multiplayer/dedicated-server.md). Options include `-fsltx`, `-ltx`, `-start`, and `-svcfg` for the server configuration. |

## See also

- [Console commands](console-commands.md)
- [Engine configuration](engine-extension-configuration.md)
