# Native Apple Silicon macOS

The macOS R4 path builds ARM64 Mach-O binaries and uses DXMT for D3D11-to-Metal
rendering and vkd3d for native HLSL compilation/reflection. It does not require
Wine or Rosetta. Windows retains its Direct3D SDK path; Linux retains DXVK.
R1/R2 are disabled in the macOS preset: stock MoltenVK does not provide the
geometry-shader feature required by the retained upstream DXVK backend.

## Build prerequisites

Use an Apple Silicon Mac with full Xcode and its Metal toolchain. DXMT requires
macOS Sonoma or newer; this development configuration was exercised on macOS
26.2 with Apple Clang 17. LLVM **15** headers and static libraries are required
by DXMT's shader translator, even when a newer compiler builds the engine.

```sh
brew install cmake ninja pkg-config meson ispc tbb glslang spirv-headers \
  molten-vk vulkan-loader vulkan-headers openal-soft libogg libvorbis freetype \
  bison make lzo
brew install --force-bottle llvm@15
DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer \
  xcodebuild -downloadComponent MetalToolchain
```

From the repository root:

```sh
cmake --preset Engine-arm64-macOS \
  -DIXRAY_APPLE_DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer \
  -DIXRAY_LLVM15_ROOT="$(brew --prefix llvm@15)"
cmake --build --preset Engine-arm64-macOS-Debug
```

The preset uses Ninja, Debug, non-unity builds, R4 and no profiler. It supplies
the CMake 4 policy compatibility required by the pinned yaml-cpp dependency.
Six parallel jobs are the preset default. Dependencies and their logs remain
inside `build/arm64/Engine-macOS`; DXMT and vkd3d use pinned revisions and their
own bounded build parallelism. The pinned LuaJIT ARM64 build selects its internal
unwinder because its Mach-O VM does not emit external unwind metadata. The native DXMT build selects Objective-C for
its C bridge sources, without modifying upstream code.

Build `xrEngine`, which also builds `xrGame` and `xrRender_R4` on macOS. The output
is `build/arm64/Engine-macOS/bin/Debug/`. Its post-build step supplies LuaJIT,
Steamworks and the DXMT runtime libraries. Retain the build tree and Homebrew
dependencies when running this development build; it is not a standalone signed
application bundle. See [dependency notices](../../doc/MacOS-Dependencies.md).

## Focused checks

```sh
cmake --build --preset Engine-arm64-macOS-Debug \
  --target TestMacOSPlatform TestMacOSShaderCompiler TestMacOSLua TestMacOSTexture
build/arm64/Engine-macOS/bin/Debug/TestMacOSPlatform 'MiXeD Case'
build/arm64/Engine-macOS/bin/Debug/TestMacOSShaderCompiler
build/arm64/Engine-macOS/bin/Debug/TestMacOSLua
build/arm64/Engine-macOS/bin/Debug/TestMacOSLua src/test/macos/CoCCompatibility.lua .
build/arm64/Engine-macOS/bin/Debug/TestMacOSTexture
python3 src/test/macos/StageCoCSmoke.py
```

These check native module initialization, bounded string copies, case-insensitive comparisons, empty/missing
absolute paths, input-layout strides, Lua protected errors/hot loops, native shader macros/include callbacks, DXBC reflection, input
signatures and compiler error reporting. The platform check also exercises
concurrent path normalization, aliased path inputs, 32-bit atomics and UI shader-cache
key ordering. The CoC script check executes the actual overlay readers and evaluator
with controlled packets/NPC state: omitted cover flags, explicit disabled loopholes,
optional remaining-use bytes and post-combat gathering. The texture check verifies native BGRA DDS encode/decode, synchronous level-map/cubemap TGA color order without changing the source image, and accepts an optional
save-thumbnail path to verify engine output. The staging checks use temporary
placeholder archives to verify ownership checks, rejection of writable marker/filesystem/appdata/user-setting symlinks and mutable-setting preservation.
These checks do not establish gameplay.

## Call of Chernobyl data

Use a complete, legally obtained CoC **1.4.12 base plus 1.4.22 patch**. A patch-only
directory containing `database/config` and `database/patch` lacks the base maps,
textures, models and sounds. The engine's CoC overlay is `gamedata_coc`; its
`configs/engine_external.ltx` uses the CoP engine platform mode. Use
`fsgame_coc.ltx`, rather than a vanilla CoP filesystem layout.

For the original CoC base archive, extract its database into the ignored build
directory. This leaves the downloaded archive unchanged:

```sh
mkdir -p build/runtime/coc-base
bsdtar -xf "/path/to/Call_of_Chernobyl_1_4_12.7z" \
  -C build/runtime/coc-base "S.T.A.L.K.E.R. - Call of Chernobyl/database"
python3 util/StageMacOSCoC.py \
  --base "build/runtime/coc-base/S.T.A.L.K.E.R. - Call of Chernobyl" \
  --patch "/path/to/extracted/CoC-1.4.22-patch"
```

The staging tool links the original archives, gives patch files precedence,
links the repository overlay except for a private copy of `configs`, copies the
CoC filesystem configuration and creates initial R4/windowed settings. Generated settings, saves, shader caches and logs
belong to `build/runtime/coc/_appdata_ixray_`. CoC writes its `axr_options.ltx` and
AtmosFear settings into the staging overlay's `gamedata/configs`; those files are
retained when staging runs again. Existing user settings are retained
on subsequent staging runs. The tool refuses to overwrite an unrelated output
directory or a regular file with an archive symlink.

```sh
cd build/runtime/coc
../../arm64/Engine-macOS/bin/Debug/xrEngine -r4
```

For automated level checks, select a CoC start position before starting
`all/single/alife/new`. A bare `start` stays in CoC's empty `fake_start` map.
The following selects the stalker Cordon start through the same native helper
used by faction selection:

```sh
../../arm64/Engine-macOS/bin/Debug/xrEngine -r4 -autotest \
  -autotest_warmup 10 -autotest_frames 60 -autotest_timeout 240 \
  -autotest_hash -autotest_shot 30 \
  -autotest_cmd 'run_string set_start_position_from_smart("esc_smart_terrain_2_12");run_string axr_main.config:w_value("character_creation","new_game_faction","stalker");start server(all/single/alife/new) client(localhost)'
```

To test a save/reload cycle, add `-autotest_post_cmd 'save macos_validation'`
before `-autotest_cmd` in the new-game command. To load that save from the menu:

```sh
../../arm64/Engine-macOS/bin/Debug/xrEngine -r4 -autotest \
  -autotest_warmup 150 -autotest_frames 1800 -autotest_timeout 240 \
  -autotest_hash -autotest_shot 600 \
  -autotest_cmd 'start server(macos_validation/single/alife/load) client(localhost)'
```

The `load <name>` console command requires an already running ALife simulation.
Use `start server(<name>/single/alife/load) client(localhost)` when starting from
the menu. A normal, visible save load uses:

```sh
../../arm64/Engine-macOS/bin/Debug/xrEngine -r4 \
  -start 'server(macos_validation/single/alife/load) client(localhost)'
```

Autotest screenshots are uncompressed TGA files in `_appdata_ixray_/logs`.
For visual inspection on macOS, convert a captured frame with:

```sh
sips -s format png _appdata_ixray_/logs/autotest_0000.tga --out frame.png
```

Inspect frames after warmup and later in the run: a valid menu, an empty
`fake_start` level or a HUD over black pixels does not verify world rendering.
Autotest disables input capture and sound and exits without the normal engine
shutdown path. Check keyboard/mouse, audio and normal exit in a visible session.

Save thumbnails use DDS encoding on macOS, matching the runtime texture loader.
After saving, pass the generated `_appdata_ixray_/savedgames/<name>.dds` path to
`TestMacOSTexture` to check the file header and decoded dimensions.

For an independent menu capture, use `-autotest -autotest_menu
-autotest_warmup 5 -autotest_frames 3 -autotest_hash -autotest_shot 1`.
Menu mode measures the menu and does not prove level loading.

Keep `-autotest_cmd` last. Inspect the process exit status, logs and
`_appdata_ixray_/logs/autotest.csv`; successful library compilation or the
standalone graphics proof is not evidence of a loaded, playable game.

## Validation recorded on Apple M4

The Debug ARM64 build was checked on macOS 26.2 with the staged CoC 1.4.12
base, 1.4.22 patch and repository overlay. Native platform, shader compiler,
Lua, CoC script, DDS and staging checks passed. A fresh Cordon simulation
completed 3,600 measured frames, saved 23,603 objects and exited 0; reloading
that save completed another 1,800 frames and exited 0. Both runs reported zero
autotest errors, and captured frames retained the textured world, hand and HUD.
The generated save thumbnail decoded as a 128×128 DDS.

A normal loaded-level session exited 0 through a queued SDL window-close event,
and a normal menu exited 0 through a queued global SDL quit event. These checks
exercise normal teardown without desktop input. After the desktop was unlocked,
the user reported that movement, mouse look and game audio worked. A held OS
Escape event visibly opened the in-game menu. Repeatable automated movement and
interaction attribution remained inconclusive: other OS movement-key and mouse
events arrived during a quiet baseline, and their source was not identified.
Those unsolicited changes are not attributed to the automated test.
The tested OpenAL context reports 255 mono plus 1 stereo source. The engine's
992-target allocation request reaches that limit and logs a source-creation
warning, retaining 256 targets; the normal menu had one playing source with
three queued buffers. This verifies API playback state; audible output is supported
by the user report, without an independent audio recording.
Windows and Linux builds were not run on this Mac.

## Current platform boundaries

Use windowed rendering. DXMT's native upstream monitor/fullscreen helpers and
some Win32 semaphore/priority helpers remain incomplete; its native swapchain
receives a Cocoa Metal view from the engine's RAII platform bridge. DLSS and the
Windows FSR SDK integration are unavailable in this configuration. Validate
rendering, level loading, input, sound and save/load separately when changing
the backend or the asset set. [Autotest details](lifecycle.md#autotest).

