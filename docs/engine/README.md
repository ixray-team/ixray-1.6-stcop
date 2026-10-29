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
| [actor-light-perception.md](actor-light-perception.md) | Gunslinger torch detection, AI memory integration, caching |
| [camera-focus.md](camera-focus.md) | Actor camera focus, Lua API, input ownership, transitions |
| [item-parameters.md](item-parameters.md) | Inventory parameter rows, signs, icon colors |
| [ai-grid-editor.md](ai-grid-editor.md) | In-game AI grid editor |
| [cinema-film-maker-editor.md](cinema-film-maker-editor.md) | In-game Cinema Film Maker Editor: fake NPCs, held items, animation playlists, scene save/load |
| [build.md](build.md) | Targets and CMake options |
| [style.md](style.md) | RAII, types, platform split |

Agent entry: [AGENTS.md](../../AGENTS.md).
