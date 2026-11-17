# New Lua exports

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

In the tables, `Class:Method` designates an instance call, and `module.name` designates a function or value in a table. The source links point to the registration, including individual overloads. Preprocessor guards appear next to their registrations. Availability also depends on the VM, build options, and engine initialization.

Each section contains examples and reference tables covering all listed exports. Method rows show the available call forms. Field and constant rows describe the values they expose. Object types use their Lua or C++ names according to their registrations.

## Sections

| Section | New exports | Examples |
| --- | ---: | --- |
| [Core types, configs, and engine](lua-exports/core.md) | 93 | [Open](lua-exports/core.md#examples) |
| [Level, objects, and gameplay](lua-exports/game.md) | 1236 | [Open](lua-exports/game.md#examples) |
| [Windows, widgets, and HUD](lua-exports/ui.md) | 388 | [Open](lua-exports/ui.md#examples) |
| [Multiplayer classes and events](lua-exports/mp.md) | 132 | [Open](lua-exports/mp.md#examples) |
| [Shader Lua VM](lua-exports/shader.md) | 7 | [Open](lua-exports/shader.md#examples) |
| [Lua in tools](lua-exports/tools.md) | 2 | [Open](lua-exports/tools.md#examples) |

The SDK tools reference also covers the original property editor and value list methods.

## See also

- [Lua for configs, objects, and UI](runtime-exports.md)
- [Lua API examples](new-functions.md)
- [Lua registration history](https://github.com/ixray-team/ixray-1.6-stcop/tree/default/docs/engine/audit)
