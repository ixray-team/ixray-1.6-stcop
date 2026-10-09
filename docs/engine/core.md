# Core

`Core._initialize` in stage 1 (`src/xrCore/xrCore.cpp`). `Core._destroy` in `EndPlay`. `init_counter` gates most initialization and final destruction; ECS manager and INI cache allocation occur before that guard.

Init order: `CECSManager`, INI cache, Windows COM + `Core.Params`/`LoadParams`, paths and user/computer names, `CPU::Detect`, `Memory._initialize`, `xrLogger::InitLog`, CPU/RTC initialization, `FS` = `CLocatorAPI`, `EFS`, expression manager, Discord. When filesystem initialization is requested, `FS._initialize` uses `flScanAppRoot`; `-build` / `-ebuild` add copy-out flags. There is no `CFilewatcher` startup in this branch.

## Memory

`Memory` (`src/xrCore/memory/xrMemory.h`): `xr_alloc` / `xr_free` / `xr_new` / `xr_delete`. Luabind uses the same heap in game mode (`setup_luabind_allocator` in `xrGameInitialize` skips editor mode).

`shared_str` is an interned pointer. Use it for section names and keys. `Memory.mem_compact()` is called during device create/destroy, precache completion, and some AI/ALife operations.

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

Dedicated: `user_dedicated.ltx`. `-ltx <file>` replaces it. `CInifile` supports an optional `allow_include_func_t`. `InitSettings` constructs an authentication predicate but does not pass it to either INI constructor in this branch.

DLTX: partial section overrides on `CInifile` (`xr_ini.h`, `DLTXCurrentFileName`). XML override is separate from `CInifile`.

Console commands: `CCC_Float`, `CCC_Integer`, `CCC_Boolean`, `CCC_Token` in `src/xrEngine/xr_ioc_cmd.h`. Engine commands register before `xrGame` loads. Game commands register in `CCC_RegisterCommands`. `destroyConsole` runs `cfg_save`.

`game_global.ltx` `[callbacks]` names Lua functions. Read with `LoadCallbackGlobals`.

## Log, asserts, input, sound

| API | Rule |
| --- | --- |
| `Msg` | async log. `xrLogger::FlushLog` in `destroyEngine` |
| `R_ASSERT` | active unless `SHIPPING_BUILD`; Shipping removes it |
| `CHECK_OR_EXIT` | explicit fatal check in all configs |
| `VERIFY` | active only with `DEBUG`; removed in RelWithDebInfo, Release, Shipping |
| `DEBUG_DRAW` | Debug + RelWithDebInfo. ImGui via `InitDebugTools` |

`CInput` (`Xr_input.cpp`) delivers SDL events to the active `IInputReceiver` (level, menu, or console). `default_controls` runs before `user.ltx`. Mouse grab in `on_idle`: non-dedicated and either fullscreen, or not minimized with menu closed and ImGui not capturing. The local variable named `Focus` does not test SDL keyboard focus.

Sound: `InitSound1` allocates `CSoundRender_CoreA` and enumerates OpenAL devices (`-nosound` → `bPresent = false`). `InitSound2` opens the OpenAL device/context after the user config. `_SoundProcessor` in `EngineApp.cpp` uses saved camera position/direction/top; `mtSound` selects `seqFrameMT` (`GameThread`), otherwise `seqFrame` (primary). `Pause` can pause emitters when its sound argument is true. Decode: Ogg/Vorbis. Reverb uses OpenAL effects in `SoundRender_CoreA.cpp`. Voice links Opus/SpeexDSP with `IXRAY_MP`. There is no Resonance Audio option or `New/SoundBackend.cpp` in this branch.

`g_pEventManager`: deferred queue. Disconnect posts `kernel:disconnect` from `CLevel::OnFrame`. `Event.Dump()` runs in `EngineLoopAndDestroy` after persistent/app destruction and before input/settings/console/sound/engine destruction.

Sources: [core initialization](../../src/xrCore/xrCore.cpp), [assert macros](../../src/xrCore/xrDebug_macros.h), [sound registration](../../src/xrEngine/EngineApp.cpp), [OpenAL backend](../../src/xrSound/SoundRender_CoreA.cpp).
