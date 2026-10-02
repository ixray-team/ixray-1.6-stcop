# Core

`Core._initialize` in stage 1 (`src/xrCore/xrCore.cpp`). `Core._destroy` in `EndPlay`. `init_counter` guards re-entry.

Init order: `CECSManager`, INI cache, Windows COM + `Core.Params`/`LoadParams`, paths and user name, `CPU::Detect`, `Memory._initialize`, `xrLogger::InitLog`, `FS` = `CLocatorAPI`, `EFS`. `FS._initialize` scans the app root. `-build` / `-ebuild` add copy-out flags. `CFilewatcher` is enabled at the start of stage 1.

## Memory

`Memory` (`src/xrCore/memory/xrMemory.h`): `xr_alloc` / `xr_free` / `xr_new` / `xr_delete`. Luabind uses the same heap (`setup_luabind_allocator` in `xrGameInitialize`).

`shared_str` is an interned pointer. Use it for section names and keys. `mem_compact()` runs on the primary thread after large loads.

Containers: `xr_vector`, `xr_map`, `xr_hash_map`, `xr_string`. Map in [doc/code-style-cpp.md](../../doc/code-style-cpp.md).

## Files

`FS.update_path` + roots from `fsgame.ltx` (`$game_config$`, `$game_data$`, `$fs_root$`). Override file: `-fsltx`.

`CLocatorAPI::file`: loose file, or archive (`vfs`, offset, `size_compressed`, crc). `wrap` is set when an addon replaces the entry. Read with `IReader`.

`GAddonsManager` (`src/xrCore/xrAddons.h`): collect, `ReadMetaInfo`, dependency order, `MountAddons`. `AddonInfo::ScriptInit` has no `.script` suffix.

## Config

| Global | File | Loaded |
| --- | --- | --- |
| `pSettings` | `system.ltx` | `InitSettings` |
| `pGameIni` | `game.ltx` | `InitSettings` |
| `pGameGlobals` | `game_global.ltx` | `xrGameInitialize` |
| `Console->ConfigFile` | `user.ltx` | `execUserScript` in stage 4 |

Dedicated: `user_dedicated.ltx`. `-ltx <file>` replaces it. Includes pass `allow_include_func_t` installed by `InitSettings`.

DLTX: partial section overrides on `CInifile` (`xr_ini.h`, `DLTXCurrentFileName`). XML override is separate from `CInifile`.

Console commands: `CCC_Float`, `CCC_Integer`, `CCC_Boolean`, `CCC_Token` in `src/xrEngine/xr_ioc_cmd.h`. Engine commands register before `xrGame` loads. Game commands register in `CCC_RegisterCommands`. `destroyConsole` runs `cfg_save`.

`game_global.ltx` `[callbacks]` names Lua functions. Read with `LoadCallbackGlobals`.

## Log, asserts, input, sound

| API | Rule |
| --- | --- |
| `Msg` | async log. `xrLogger::FlushLog` in `destroyEngine` |
| `R_ASSERT` / `CHECK_OR_EXIT` | kept in all configs |
| `VERIFY` | stripped when `MASTER_GOLD` (Release, Shipping) |
| `DEBUG_DRAW` | Debug + RelWithDebInfo. ImGui via `InitDebugTools` |

`CInput` (`Xr_input.cpp`) delivers SDL events to the active `IInputReceiver` (level, menu, or console). `default_controls` runs before `user.ltx`. Mouse grab in `on_idle`: focused, not minimized, menu closed, ImGui not capturing. Dedicated server does not grab.

Sound: `InitSound1` allocates `CSoundRender_Core` (`-nosound` → `bPresent = false`). `InitSound2` opens the SDL device after `user.ltx`. Update on `GameThread` with `mView_saved` / saved camera. `Pause` pauses emitters. Decode: Ogg/Vorbis. Reverb: Resonance Audio if `IXRAY_ENABLE_RESONANCEAUDIO`. Voice: Opus/Speex if `IXRAY_MP`. Backend: `src/xrSound/New/SoundBackend.cpp`.

`g_pEventManager`: deferred queue. Disconnect posts `kernel:disconnect` from `CLevel::OnFrame`. `Event.Dump()` in `EngineLoopAndDestroy` before subscribers die.
