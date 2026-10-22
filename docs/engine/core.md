# Core

`Core._initialize` in stage 1 (`src/xrCore/xrCore.cpp`). `Core._destroy` in `EndPlay`. `init_counter` gates shared initialization and final destruction, including the ECS manager and INI cache. Nested initialization retains these owners. `CEngineAPI::Destroy` calls `ClearCachedIniFiles` after game shutdown and before unloading `xrGame`: cached polymorphic objects need their defining modules' RTTI and vtables. ALife restart uses the same cleanup. It detaches entries before deletion because INI destructors may unregister themselves. Final Core destruction clears any remaining entries before shutting down the filesystem and deletes the cache owner.

Init order: `CECSManager`, INI cache, Windows COM + `Core.Params`/`LoadParams`, paths and user/computer names, `CPU::Detect`, `Memory._initialize`, `xrLogger::InitLog`, CPU/RTC initialization, `FS` = `CLocatorAPI`, `EFS`, expression manager, Discord. When filesystem initialization is requested, `FS._initialize` uses `flScanAppRoot`; `-build` / `-ebuild` add copy-out flags. There is no `CFilewatcher` startup in this branch.

## Memory

`Memory` (`src/xrCore/memory/xrMemory.h`): `xr_alloc` / `xr_free` / `xr_new` / `xr_delete`. Luabind uses the same heap in game mode (`setup_luabind_allocator` in `xrGameInitialize` skips editor mode).

The engine heap defaults to mimalloc 3.5.3 (`IXRAY_USE_MIMALLOC=ON`); ASAN builds always use CRT. The release source archive is SHA-256-pinned and fetched by CMake, or supplied offline with `FETCHCONTENT_SOURCE_DIR_MIMALLOC`. A single static copy is linked privately into `xrCore`; the license is copied to `licenses/mimalloc-LICENSE.txt` beside the binaries. CRT interception is disabled. All `Memory` allocations, including calls before its constructor and after `Memory._destroy`, use the same compile-time-selected backend. Do not pass CRT/library-owned pointers to `xr_free` or `xr_realloc`.

Ordinary, sized and nothrow new/delete use `Memory`; standard over-aligned new/delete retain their matching runtime allocator. `xr_alloc` and `xr_delete` do not support over-aligned types. LuaJIT's VM still uses `luaL_newstate()` and its own allocator. `mem_usage()` with mimalloc returns a statistics snapshot of allocated block bytes (including size-class rounding), saturated to the legacy `u32` range; `pBlocksUsed` is the snapshot's allocated block count and `pBlocksFree` is zero (unavailable). It excludes CRT allocations and GPU memory. `mem_compact()` cleans shared containers before asking mimalloc to collect unused memory; collection does not guarantee that every worker's cached memory is returned immediately.

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

Task pool (`TaskPool.h`, `TaskPool.cpp`): `xrCore::_initialize` starts a fixed set of workers ("X-Ray Worker N") that live until `_destroy`; the engine has no ConcRT or TBB tasks (`xr_concurrent_vector` and `xr_concurrent_unordered_map` remain containers). With 6 cores or more, the three fastest cores (highest efficiency class) give one logical processor each to the primary thread (`TaskPool::Initialize`), `GameThread` and `PreRenderThread` (`XRayWorkerThread` role); every other thread made by `thread_spawn` and the workers get the remaining processors, one worker per processor. A worker with no task sleeps on an atomic counter (`std::atomic::wait`), woken one per queued task. `xr_task_group` (`run`, `wait`) queues `Frame` tasks by default and `Background` tasks for loading (models, prefetch, ini read-ahead, textures, collision, patrols, `thm` reload, save writing); workers take `Frame` first and run at most `BackgroundLoad::WorkerCount()` background tasks at once, one worker less than the pool. `wait` runs the queued tasks of its group on the calling thread, then sleeps on the group's counter, so nested waits never block the pool. `xr_parallel_for` splits the range into chunks taken in turn by the caller and up to all workers; inside `xrBackgroundWorkScope` it runs in place. Level geometry, the AI preload, the asynchronous `net_start2` and sound decoding (one decoder at a time) are pool tasks. `xr_std_parallel_for` (threads per call) is left for xrLC only.

Crash dumps (Windows): `xrEngine.exe` starts `xrCrashDumpHelper.exe` with `-crash_dump_helper` after `InitEngine` (`CrashDumpHelper::Start`). The helper is a separate executable built next to `xrEngine.exe` and must be shipped with it; it links neither the engine nor the profilers, so it does not appear as a profiler client. It waits on an event and on the engine process. `UnhandledFilter` lets one thread report (another failing thread sleeps), puts the thread id and exception pointers into a shared mapping without allocating, and signals the helper, which writes `<logs>\IXRay_<user>_<time>_small.mdmp` (stacks) and then `<time>.mdmp` with the private memory, or the referenced memory if the private memory cannot be read. Without the helper `save_mini_dump` writes them in process. The stack trace comes after the dumps: a broken heap can end the process in the symbol engine. The `crash` console command (not in `MASTER_GOLD`) checks this.

`CInput` (`Xr_input.cpp`) delivers SDL events to the active `IInputReceiver` (level, menu, or console). `default_controls` runs before `user.ltx`. Mouse grab in `on_idle`: non-dedicated and either fullscreen, or not minimized with menu closed and ImGui not capturing. The local variable named `Focus` does not test SDL keyboard focus.

Sound: `InitSound1` allocates `CSoundRender_CoreA` and enumerates OpenAL devices (`-nosound` → `bPresent = false`). `InitSound2` opens the OpenAL device/context after the user config. `_SoundProcessor` in `EngineApp.cpp` uses saved camera position/direction/top; `mtSound` selects `seqFrameMT` (`GameThread`), otherwise `seqFrame` (primary). `Pause` can pause emitters when its sound argument is true. Decode: Ogg/Vorbis. Reverb uses OpenAL effects in `SoundRender_CoreA.cpp`. Voice links Opus/SpeexDSP with `IXRAY_MP`. There is no Resonance Audio option or `New/SoundBackend.cpp` in this branch.

`g_pEventManager`: deferred queue. Disconnect posts `kernel:disconnect` from `CLevel::OnFrame`. `Event.Dump()` runs in `EngineLoopAndDestroy` after persistent/app destruction and before input/settings/console/sound/engine destruction.

Sources: [core initialization](../../src/xrCore/xrCore.cpp), [assert macros](../../src/xrCore/xrDebug_macros.h), [sound registration](../../src/xrEngine/EngineApp.cpp), [OpenAL backend](../../src/xrSound/SoundRender_CoreA.cpp).
