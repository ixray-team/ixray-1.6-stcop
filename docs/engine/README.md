# Engine

Source of truth for `src/`. Modder docs stay in `docs/docs`.

Current branch uses separate `xrRender_R1`, `xrRender_R2`, and `xrRender_R4` client renderers. Entry is `CApplication::Run` in `src/xrPlay/Application.cpp`. Verify external documentation against the current sources.

| Doc | Contents |
| --- | --- |
| [architecture.md](architecture.md) | DLL load, globals, registrators, factory |
| [lifecycle.md](lifecycle.md) | Load stages, `on_idle`, threads |
| [core.md](core.md) | Memory, `FS`, ltx, console, input, sound |
| [rendering.md](rendering.md) | R1/R2/R4 selection, APIs, dynamic and static frames |
| [gameplay.md](gameplay.md) | Level, ALife, Lua, class files |
| [build.md](build.md) | Targets and CMake options |
| [style.md](style.md) | RAII, types, platform split |

Agent entry: [AGENTS.md](../../AGENTS.md).
