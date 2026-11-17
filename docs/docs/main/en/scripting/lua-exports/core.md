# Core types, configs, and engine

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

This section lists core types and engine functions. Tools and editors may load a different set of exports.

## Syntax

| Call | Arguments and result |
| --- | --- |
| `get_cached_ini(path)` | Opens an LTX file relative to `$game_config$`. Reusing the same path string returns the shared `ini_file`. |
| `external_ini()` | Returns an `ini_file` for the current `engine_external.ltx`, or `nil` if it is unavailable. |
| `ini:section_for_each(callback)` | Calls `callback(section_name)` for each section. Return `true` to stop the traversal or `false` to continue. |

## Examples

### Read a setting

Check the key to support different configurations. Obtain the cached object again after ALife is reinitialized.

```lua
local function use_3d_icons()
    local ini = get_cached_ini("engine_external.ltx")
    return ini:line_exist("gameplay", "Enable3DIcons")
        and ini:r_bool("gameplay", "Enable3DIcons")
end
```

### Find a section

The callback stops when it finds `shaders_options`.

```lua
local function find_shader_section()
    local ini = external_ini()
    if not ini then return nil end

    local found
    ini:section_for_each(function(name)
        if name == "shaders_options" then
            found = name
            return true
        end
        return false
    end)
    return found
end
```

### Create an LTX file and read it back

The example writes two settings to `example_mod_preferences.ltx` under `$app_data_root$` and reads back `110`. Re-running it overwrites that file. `alias` is a registered FS alias including both `$` delimiters, and `filename` is relative to that alias.

```lua
local function save_mod_preferences()
    local alias = "$app_data_root$"
    local filename = "example_mod_preferences.ltx"
    local ini = create_ini(alias, filename)
    ini:save_at_end(false)

    ini:w_string("preferences", "theme", "dark", "")
    ini:w_u32("preferences", "scale", 110, "")
    assert(ini:save_as(""), "Could not save " .. filename)

    local loaded = read_ini(alias, filename)
    assert(loaded:line_exist("preferences", "scale"))
    return loaded:r_u32("preferences", "scale")
end
```

Both functions allocate a separate object, and their bindings do not transfer ownership to Lua. For frequent reads of an unchanged config under `$game_config$`, use `get_cached_ini`. The `create_ini_file(text)` function parses LTX text from memory and gives Lua ownership of the resulting object.

### Write and read binary data

This example writes a local packet and reads its fields in the same order. It returns `4, -7`.

```lua
local function packet_round_trip()
    local packet = net_packet()
    packet:w_begin(0)
    packet:w_vec4(vector4():set(1, 2, 3, 4))
    packet:w_s8(-7)

    packet:r_begin()
    local v = packet:r_vec4()
    return v.w, packet:r_s8()
end
```

## Registrations

### Global scope

| Lua name | Kind | Behavior | Registration |
| --- | --- | --- | --- |
| `create_ini(alias, filename)` | Function | Returns an empty writable `ini_file` named relative to the given FS alias. It skips loading any existing file. Saving with `save_as("")` writes the new contents over that file. | [script_ini_file_script.cpp:183](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L183) |
| `debug_print(message)` | Function | Logs a nonempty string with the `DBG` tag and current game time. | [script_engine_script.cpp:350](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L350) |
| `device()` | Function | Returns the game’s `Device` object. | [script_engine_script.cpp:357](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L357) (`#ifdef XRGAME_EXPORTS`) |
| `editor()` | Function | Returns `true` in builds without `XRGAME_EXPORTS` and `false` in the game build. | [script_engine_script.cpp:346](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L346) |
| `error_log(message)` | Function | Logs an error and the Lua stack, then triggers an engine assertion. | [script_engine_script.cpp:342](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L342) |
| `external_ini()` | Function | Returns an `ini_file` for the current `engine_external.ltx`, or `nil` if unavailable. | [script_ini_file_script.cpp:181](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L181) |
| `flush()` | Function | Flushes the engine and Lua log buffers in `DEBUG` builds. | [script_engine_script.cpp:343](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L343) |
| `get_cached_ini(filename)` | Function | Opens an LTX relative to `$game_config$` and returns the cached `ini_file` for that exact path string. | [script_ini_file_script.cpp:188](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L188) |
| `help()` | Function | Logs classes in the current Lua VM, including properties, constants, and method overloads. | [script_engine_script.cpp:354](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L354) |
| `is_cop()` | Function | Checks for Call of Pripyat mode and returns a boolean. | [script_ini_file_script.cpp:176](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L176) |
| `is_cs()` | Function | Checks for Clear Sky mode and returns a boolean. | [script_ini_file_script.cpp:177](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L177) |
| `is_soc()` | Function | Checks for Shadow of Chernobyl mode and returns a boolean. | [script_ini_file_script.cpp:178](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L178) |
| `IsEditor()` | Function | Checks the engine device’s editor mode. Returns `false` if the device is unavailable. | [script_engine_script.cpp:353](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L353) |
| `IsSupportMP()` | Function | Returns `true` in builds with `XR_MP_BUILD`. | [script_engine_script.cpp:352](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L352) |
| `IsTracyConnected()` | Function | Checks whether a Tracy client is connected. Returns `false` in builds without the Tracy profiler. | [script_engine_script.cpp:363](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L363) |
| `load_script_to_global(filename)` | Function | Runs a script from `$game_scripts$` in the current VM’s global environment. The Lua call returns no load status. | [script_engine_script.cpp:358](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L358) (`#ifdef XRGAME_EXPORTS`) |
| `log(message)` | Function | Writes a Lua log message. This registration runs in editor mode and output is omitted in `MASTER_GOLD`. | [script_engine_script.cpp:370](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L370) |
| `log1(message)` | Function | Writes a string to the engine log. | [script_engine_script.cpp:341](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L341) |
| `prefetch(module_name)` | Function | Asks the script engine to process a module by name. | [script_engine_script.cpp:344](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L344) |
| `r_file_as_string(path)` | Function | Reads an entire file into a Lua string. The path must refer to an existing readable file. The binding does not check for open failure. | [script_reader_script.cpp:118](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L118) |
| `read_ini(alias, filename)` | Function | Loads the LTX relative to the given FS alias and returns a read-only `ini_file`. If opening fails, the returned object is empty. Check expected sections and keys before reading. | [script_ini_file_script.cpp:184](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L184) |
| `reader_base` | Class | Base type for binary reads. The engine supplies instances. | [script_reader_script.cpp:89](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L89) |
| `reload_system_ini()` | Function | Reloads the global `system.ltx` from `$game_config$` and returns an `ini_file` wrapper around the current settings. Loaded objects can keep values read at initialization. | [script_ini_file_script.cpp:182](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L182) |
| `SemiLog(message)` | Function | Logs through `Msg`. The string is a format, so pass a literal `%` as `%%`. | [script_engine_script.cpp:349](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L349) |
| `time_global()` | Function | Returns game time in milliseconds. Returns `0` in builds without `XRGAME_EXPORTS`. | [script_engine_script.cpp:348](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L348) |
| `time_global_async()` | Function | Returns the device’s asynchronous timer in milliseconds. Returns `0` in builds without `XRGAME_EXPORTS`. | [script_engine_script.cpp:351](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L351) |
| `TinyLog(message)` | Function | Writes a string to the engine log. | [script_engine_script.cpp:359](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L359) (`#ifdef XRGAME_EXPORTS`) |
| `trigger_assert(message)` | Function | Forces an engine assertion. The implementation ignores the supplied message. | [script_engine_script.cpp:360](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L360) (`#ifdef XRGAME_EXPORTS`) |
| `trigger_vs_log(message)` | Function | Sends a string to debugger output on Windows. Does nothing on other platforms. | [script_engine_script.cpp:361](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L361) (`#ifdef XRGAME_EXPORTS`) |
| `try_load_file(path)` | Function | Registers the file in the FS index if it exists on disk and is not already indexed. The Lua call returns no status. | [script_engine_script.cpp:355](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L355) |
| `user_name()` | Function | Returns the user name recorded by the engine core. | [script_engine_script.cpp:347](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L347) |
| `vector4()` | Class | Creates a zero-initialized four-component vector. Use `v:set(x, y, z, w)` to initialize it. | [script_fvector_script.cpp:95](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L95) |
| `verify_if_thread_is_running()` | Function | Checks that a script thread is active. Used to validate the context for `coroutine.yield()`. | [script_engine_script.cpp:345](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L345) |

### `FS`

| Lua name | Kind | Behavior | Registration |
| --- | --- | --- | --- |
| `FS:rescan_path(alias)` | Method | Marks the directory for a registered FS alias for rescanning. | [fs_registrator_script.cpp:230](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/fs_registrator_script.cpp#L230) |

### `ini_file`

The `w_*` methods take all four arguments and return no values. Pass an empty string as `comment` when no comment is supplied. The current serializer does not retain this field. To replace an existing key, call `ini:set_override_names(true)` first.

| Lua name | Kind | Behavior | Registration |
| --- | --- | --- | --- |
| `ini_file:remove_line(section, key)` | Method | Removes an existing key from a writable INI. Returns no values. | [script_ini_file_script.cpp:155](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L155) |
| `ini_file:save_as(filename)` | Method | Writes current contents to disk. Supply a full path as `filename` to save under a different name. An empty string uses the current path. Returns `true` on success or `false` if opening the output failed. | [script_ini_file_script.cpp:153](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L153) |
| `ini_file:save_at_end(enabled)` | Method | Controls whether a writable object is saved when destroyed. Use `save_as` to write immediately. Returns no values. | [script_ini_file_script.cpp:154](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L154) |
| `ini_file:section_count()` | Method | Returns the number of sections. | [script_ini_file_script.cpp:157](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L157) |
| `ini_file:section_for_each(callback)` | Method | Calls `callback(section_name)` for each section. Return `true` to stop or `false` to continue. The method itself returns no values. | [script_ini_file_script.cpp:158](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L158) |
| `ini_file:set_override_names(enabled)` | Method | With `true`, allows the `w_*` methods to replace an existing key. Returns no values. | [script_ini_file_script.cpp:156](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L156) |
| `ini_file:set_readonly(enabled)` | Method | Enables read-only mode with `true` and allows editing with `false`. Returns no values. | [script_ini_file_script.cpp:159](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L159) |
| `ini_file:w_bool(section, key, value, comment)` | Method | Writes a boolean. The section and key must already exist. | [script_ini_file_script.cpp:137](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L137) |
| `ini_file:w_color(section, key, value, comment)` | Method | Writes a packed ARGB color. The section and key must already exist. | [script_ini_file_script.cpp:138](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L138) |
| `ini_file:w_fcolor(section, key, value, comment)` | Method | Writes an `fcolor` value. The section and key must already exist. | [script_ini_file_script.cpp:139](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L139) |
| `ini_file:w_float(section, key, value, comment)` | Method | Writes a floating-point number. The section and key must already exist. | [script_ini_file_script.cpp:140](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L140) |
| `ini_file:w_fvector2(section, key, value, comment)` | Method | Writes a two-component `vector2`. The section and key must already exist. | [script_ini_file_script.cpp:141](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L141) |
| `ini_file:w_fvector3(section, key, value, comment)` | Method | Writes a three-component `vector`. Creates a missing section or key. | [script_ini_file_script.cpp:142](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L142) |
| `ini_file:w_fvector4(section, key, value, comment)` | Method | Writes a four-component `vector4`. Creates a missing section or key. | [script_ini_file_script.cpp:143](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L143) |
| `ini_file:w_s16(section, key, value, comment)` | Method | Writes a signed 16-bit integer. Creates a missing section or key. | [script_ini_file_script.cpp:144](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L144) |
| `ini_file:w_s32(section, key, value, comment)` | Method | Writes a signed 32-bit integer. Creates a missing section or key. | [script_ini_file_script.cpp:145](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L145) |
| `ini_file:w_s64(section, key, value, comment)` | Method | Writes a signed 64-bit integer. Creates a missing section or key. | [script_ini_file_script.cpp:146](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L146) |
| `ini_file:w_s8(section, key, value, comment)` | Method | Writes a signed 8-bit integer. Creates a missing section or key. | [script_ini_file_script.cpp:147](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L147) |
| `ini_file:w_string(section, key, value, comment)` | Method | Writes a string. Creates a missing section or key. | [script_ini_file_script.cpp:148](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L148) |
| `ini_file:w_u16(section, key, value, comment)` | Method | Writes a unsigned 16-bit integer. Creates a missing section or key. | [script_ini_file_script.cpp:149](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L149) |
| `ini_file:w_u32(section, key, value, comment)` | Method | Writes a unsigned 32-bit integer. Creates a missing section or key. | [script_ini_file_script.cpp:150](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L150) |
| `ini_file:w_u64(section, key, value, comment)` | Method | Writes a unsigned 64-bit integer. The section and key must already exist. | [script_ini_file_script.cpp:151](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L151) |
| `ini_file:w_u8(section, key, value, comment)` | Method | Writes a unsigned 8-bit integer. The section and key must already exist. | [script_ini_file_script.cpp:152](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L152) |

### `net_packet`

| Lua name | Kind | Behavior | Registration |
| --- | --- | --- | --- |
| `net_packet:r_object_id()` | Method | Reads an object ID and advances the read cursor. | [script_net_packet_script.cpp:155](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_net_packet_script.cpp#L155) |
| `net_packet:r_vec4()` | Method | Reads four numbers and returns a `vector4`. | [script_net_packet_script.cpp:137](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_net_packet_script.cpp#L137) |
| `net_packet:w_object_id(id)` | Method | Writes a numeric object ID. | [script_net_packet_script.cpp:126](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_net_packet_script.cpp#L126) |
| `net_packet:w_s8(value)` | Method | Writes a signed 8-bit integer. | [script_net_packet_script.cpp:114](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_net_packet_script.cpp#L114) |
| `net_packet:w_vec4(value)` | Method | Writes the four components of a `vector4`. | [script_net_packet_script.cpp:105](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_net_packet_script.cpp#L105) |

### `reader`

| Lua name | Kind | Behavior | Registration |
| --- | --- | --- | --- |
| `reader:r_object_id()` | Method | Reads a numeric object ID from binary data. | [script_reader_script.cpp:115](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L115) |
| `reader:r_vec4(out)` | Method | Stores four read components in the supplied `vector4`. Returns no values. | [script_reader_script.cpp:110](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L110) |

### `reader_base`

| Lua name | Kind | Behavior | Registration |
| --- | --- | --- | --- |
| `reader_base:r_angle16()` | Method | Reads a 16-bit quantized angle and returns radians in the range from `0` to `2π`. | [script_reader_script.cpp:101](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L101) |
| `reader_base:r_angle8()` | Method | Reads a 8-bit quantized angle and returns radians in the range from `0` to `2π`. | [script_reader_script.cpp:102](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L102) |
| `reader_base:r_dir(out)` | Method | Stores a decompressed direction in the supplied `vector`. | [script_reader_script.cpp:103](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L103) |
| `reader_base:r_float()` | Method | Reads a single-precision floating-point number. | [script_reader_script.cpp:90](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L90) |
| `reader_base:r_float_q16(min, max)` | Method | Reads a 16-bit value and decodes it to a number in the `min`–`max` range. | [script_reader_script.cpp:99](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L99) |
| `reader_base:r_float_q8(min, max)` | Method | Reads a 8-bit value and decodes it to a number in the `min`–`max` range. | [script_reader_script.cpp:100](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L100) |
| `reader_base:r_s16()` | Method | Reads a signed 16-bit integer. | [script_reader_script.cpp:96](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L96) |
| `reader_base:r_s32()` | Method | Reads a signed 32-bit integer. | [script_reader_script.cpp:94](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L94) |
| `reader_base:r_s64()` | Method | Reads a signed 64-bit integer. | [script_reader_script.cpp:92](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L92) |
| `reader_base:r_s8()` | Method | Reads a signed 8-bit integer. | [script_reader_script.cpp:98](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L98) |
| `reader_base:r_sdir(out)` | Method | Reads a compressed direction and scale, then stores the result in the supplied `vector`. | [script_reader_script.cpp:104](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L104) |
| `reader_base:r_u16()` | Method | Reads a unsigned 16-bit integer. | [script_reader_script.cpp:95](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L95) |
| `reader_base:r_u32()` | Method | Reads a unsigned 32-bit integer. | [script_reader_script.cpp:93](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L93) |
| `reader_base:r_u64()` | Method | Reads a unsigned 64-bit integer. | [script_reader_script.cpp:91](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L91) |
| `reader_base:r_u8()` | Method | Reads a unsigned 8-bit integer. | [script_reader_script.cpp:97](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L97) |

### `save`

| Lua name | Kind | Behavior | Registration |
| --- | --- | --- | --- |
| `save.call_error()` | Function | Triggers the save-size assertion and reports the stage set by `save.set_stage`. | [script_engine_script.cpp:376](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L376) |
| `save.set_stage(name)` | Function | Stores the current stage name for save-size error diagnostics. | [script_engine_script.cpp:377](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L377) |

### `vector4`

| Lua name | Kind | Behavior | Registration |
| --- | --- | --- | --- |
| `vector4.w` | Read/write field | Fourth component, readable and writable. | [script_fvector_script.cpp:99](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L99) |
| `vector4.z` | Read/write field | Third component, readable and writable. | [script_fvector_script.cpp:98](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L98) |
| `vector4:add(value)`, `vector4:add(a, value)` | Method | Computes `self + value` or `a + value` component-wise, stores the result in this vector, and returns it. `value` can be a vector or scalar. | [script_fvector_script.cpp:103](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L103), [script_fvector_script.cpp:104](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L104), [script_fvector_script.cpp:105](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L105), [script_fvector_script.cpp:106](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L106) |
| `vector4:div(value)`, `vector4:div(a, value)` | Method | Computes `self / value` or `a / value` component-wise, stores the result in this vector, and returns it. `value` can be a vector or scalar. Divisors must be nonzero. | [script_fvector_script.cpp:115](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L115), [script_fvector_script.cpp:116](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L116), [script_fvector_script.cpp:117](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L117), [script_fvector_script.cpp:118](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L118) |
| `vector4:lerp(a, b, t)` | Method | Stores component-wise interpolation from `a` to `b` at `t` and returns the same vector. | [script_fvector_script.cpp:120](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L120) |
| `vector4:magnitude()` | Method | Returns the four-dimensional vector length. | [script_fvector_script.cpp:121](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L121) |
| `vector4:magnitude_sqr()` | Method | Returns the squared vector length. | [script_fvector_script.cpp:122](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L122) |
| `vector4:mul(value)`, `vector4:mul(a, value)` | Method | Computes `self * value` or `a * value` component-wise, stores the result in this vector, and returns it. `value` can be a vector or scalar. | [script_fvector_script.cpp:111](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L111), [script_fvector_script.cpp:112](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L112), [script_fvector_script.cpp:113](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L113), [script_fvector_script.cpp:114](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L114) |
| `vector4:normalize()` | Method | Divides all components by the nonzero vector length and returns the vector. | [script_fvector_script.cpp:123](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L123) |
| `vector4:normalize_as_plane()` | Method | Divides all four components by the length of the nonzero normal `(x, y, z)` and returns the vector. | [script_fvector_script.cpp:124](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L124) |
| `vector4:similar(other, epsilon)` | Method | Returns `true` if the absolute difference between each pair of components is below `epsilon`. | [script_fvector_script.cpp:119](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L119) |
| `vector4:sub(value)`, `vector4:sub(a, value)` | Method | Computes `self - value` or `a - value` component-wise, stores the result in this vector, and returns it. `value` can be a vector or scalar. | [script_fvector_script.cpp:107](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L107), [script_fvector_script.cpp:108](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L108), [script_fvector_script.cpp:109](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L109), [script_fvector_script.cpp:110](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L110) |

## See also

- [New Lua exports](../lua-exports.md)
