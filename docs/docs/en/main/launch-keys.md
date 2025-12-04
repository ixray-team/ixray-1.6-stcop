# Launch Keys

> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.0

## Overview

Launch keys allow you to configure engine behavior before starting the game. Parameters are grouped by functional categories and used to configure rendering, debugging, performance, and game functions

## Syntax

Launch keys are specified in the command line or configuration file in the format:

```
-<key>
```

or

```
-<key1> -<key2> -<key3>
```

## Parameter Categories

### Core

| Key | Description |
|------|----------|
| **`auto_load_arch`** | Automatically load game archives at startup |
| **`nolog`** | **Disable logging to file** |

### Engine

| Key | Description |
|------|----------|
| **`xclsx`** | **Disable rendering of red text in debug** |
| **`no_center_screen`** | **Do not center window on startup** |
| **`dxdebug`** | **Enable DirectX debug mode** |
| **`perfhud_hack`** | **Force activate all renderers** |
| **`noprefetch`** | **Disable data preloading** |
| **`demomode`** | **Demo recording playback** |
| **`nosound`** | **Completely disable the sound subsystem** |
| **`r2`** | **Force enable R2 renderer** |
| **`r4`** | **Force enable R4 renderer** |

### Render

| Key | Description |
|------|----------|
| **`disasm`** | **Enable shader disassembly** |
| **`nonvs`** | **No nvidia depthstencil (dx9 only)** |
| **`noshadows`** | **Completely disable shadow rendering** |
| **`no_occq`** | **Disable occq optimization** |
| **`nodistort`** | **Disable distortion effects (heat, smoke)** |

### Game

| Key | Description |
|------|----------|
| **`designer`** | Put all alive objects offline |
| **`debug_ge`** | Enable game event debug mode |

### API

| Key | Description |
|------|----------|
| **`renderdoc`** | **Activate RenderDoc integration for frame capture** |

### Lua

| Key | Description |
|------|----------|
| **`keep_lua`** | **Do not unload Lua on level change** |
| **`use_callstack`** | **Enable callstack output on LUA errors** |

### Interface

| Key | Description |
|------|----------|
| **`no_debug_panel`** | **Disable ImGui debug panel** |

## Usage Examples

### Example 1: Complex Graphics Debugging

```
-dxdebug -disasm -renderdoc
```

→ Activates DirectX debug, shader disassembly, and RenderDoc capture

### Example 2: Script Debugging

```
-use_callstack -keep_lua
```

→ Preserves Lua state and displays call stack on errors

## **Recommendations**

- ⚠ **Limitations**:   <br>
    The keys `r2` and `r4` are mutually exclusive - use only one of them <br>
    `noprefetch` can cause stuttering when loading new zones <br>
    `nonvs` only works in DirectX 9 mode <br>
    `nosound` completely disables all sound effects and music <br>

- ✖ **Anti-patterns**:   <br>
    Avoid combining `nolog` + `use_callstack` when debugging scripts <br>
    Don't activate all rendering keys at once <br>
    Don't use `demomode` in normal gameplay <br>
