# Engine

Source of truth for `src/`. Modder docs stay in `docs/docs`.

DeepWiki (`ixray-team/ixray-1.6-stcop`) still describes `xrRender_R1`/`xrRender_R2` and `WinMain` in `xrPlay.cpp`. Current entry is `CApplication::Run` in `src/xrPlay/Application.cpp`. Client renderer is `xrRender_R4`.

| Doc | Contents |
| --- | --- |
| [architecture.md](architecture.md) | DLL load, globals, registrators, factory |
| [lifecycle.md](lifecycle.md) | Load stages, `on_idle`, threads |
| [core.md](core.md) | Memory, `FS`, ltx, console, input, sound |
| [rendering.md](rendering.md) | Lighting mode, dynamic and static frames |
| [gameplay.md](gameplay.md) | Level, ALife, Lua, class files |
| [build.md](build.md) | Targets and CMake options |
| [style.md](style.md) | RAII, types, platform split |

Agent entry: [AGENTS.md](../../AGENTS.md).
