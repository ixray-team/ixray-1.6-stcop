# Native macOS development runs

Develop includes an ARM64 macOS port: SDL3, the pinned DXMT Metal backend and
vkd3d's shader compiler. Use the existing R4 renderer (`-r4` / `renderer_r4`).
These instructions build and run locally without installing or replacing an app.
The CoC staging path is separate from the CoP `levels`/`resources` packaging flow
in `build-macos.command`.

## Toolchain and build

Use full Xcode with its Metal Toolchain installed. Command Line Tools alone are
not sufficient for compiling DXMT's Metal library. Select Xcode for this shell:

```sh
export DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer
xcrun --find metal
```

The macOS CI dependency list is in `.github/workflows/build-engine.yml`. Install
CMake (3.26 or newer), Ninja, Meson, pkg-config, LLVM 15, bison, Vulkan headers,
SPIR-V headers, TBB, lzo, libogg, libvorbis, theora, ISPC, MoltenVK and the Vulkan
loader. vkd3d configure also requires Perl's JSON module. For a standard ARM64
Homebrew installation:

```sh
HOMEBREW_NO_AUTOREMOVE=1 brew install cmake ninja meson pkg-config llvm@15 bison \
  vulkan-headers spirv-headers tbb lzo libogg libvorbis theora ispc \
  molten-vk vulkan-loader cpanminus
cpanm --notest --local-lib "$HOME/perl5" JSON
export PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:$PERL5LIB}"
export PATH="$(brew --prefix bison)/bin:$PATH"
perl -MJSON -e 'print "JSON $JSON::VERSION\n"'
cmake --preset Engine-arm64-macOS -DDXMT_LLVM_PATH="$(brew --prefix llvm@15)"
cmake --build --preset Engine-arm64-macOS-RelWithDebInfo
```

The preset follows the existing macOS CI configuration: AppleClang, ARM64,
RelWithDebInfo, ResonanceAudio and profiling disabled, and explicit engine/game/R4
targets. OpenAL remains enabled. Build parallelism defaults to four jobs.
Output is `build/arm64/Engine-Darwin/bin/RelWithDebInfo/`; retain its neighboring
libraries, including `libsteam_api.dylib`. CMake fetches the remaining pinned
source dependencies. These local binaries retain Homebrew dependencies; this is
not a verified standalone distribution recipe.

## CoC 1.4.22 data and launch

Provide a legitimately obtained, extracted CoC 1.4.12 base and extracted 1.4.22
patch. The base must contain `database/{config,maps,resource,sound}` and the patch
must contain `database/config/scripts.db`. Do not pass a CoP installation.

From the repository root:

```sh
python3 util/StageMacOSCoC.py \
  --base '/path/to/extracted/Call of Chernobyl' \
  --patch '/path/to/extracted/1.4.22 patch' \
  --output "$PWD/build/runtime/coc"
engine="$PWD/build/arm64/Engine-Darwin/bin/RelWithDebInfo/xrEngine"
cd build/runtime/coc
"$engine" -r4 -nointro
```

Staging links immutable database files, applies patch precedence, and uses this
checkout's `gamedata_coc` overlay. Configs are private copies; restaging preserves
existing user, AXR and AtmosFear settings. The runtime owns `_appdata_ixray_`
(settings, saves, screenshots, logs and caches). **Keep this directory when
cleaning build outputs.** A custom persistent `--output` is supported; run from
that directory instead. Keep the source archives and overlay available while
using linked assets. Do not edit source assets through runtime links.

For close terrain comparisons, use `texture_lod 0` in the runtime's `user.ltx`
before launch. The initial engine setting `texture_lod 1` deliberately drops a
texture mip and visibly softens CoC's ground detail. `r__tf_aniso 16` improves
oblique sampling if desired. Staging preserves existing settings; it does not
silently promote them. Record resolution, LOD and anisotropy with visual captures.

Use one engine instance at a time. For real keyboard/mouse and audible output
validation, reserve the desktop with its user before supplying input; automated
readback frames do not establish usable controls or audible sound.

## Focused checks

```sh
python3 src/test/macos/StageCoCSmoke.py
luajit src/test/macos/CoCCompatibility.lua "$PWD"
cmake --preset Engine-arm64-macOS -DIXRAY_TESTS=ON
cmake --build build/arm64/Engine-Darwin --target TestUIShaderCache -j4
build/arm64/Engine-Darwin/bin/RelWithDebInfo/TestUIShaderCache
```

The Lua fixtures exercise the actual CoC scripts; LuaJIT is needed only for that
standalone check. The C++ check uses the production UI shader-cache comparator.

The develop autotest supports commands at startup and after level warmup.
`-autotest_cmd` must be last; flags preceding it must not be embedded in its
command string. Example from the staged runtime:

```sh
"$engine" -r4 -nointro -autotest -autotest_warmup 30 \
  -autotest_frames 180 -autotest_timeout 240 -autotest_hash -autotest_shot 60 \
  -autotest_post_cmd 'save develop_validation' \
  -autotest_cmd 'run_string set_start_position_from_smart("esc_smart_terrain_2_12");run_string axr_main.config:w_value("character_creation","new_game_faction","stalker");start server(all/single/alife/new) client(localhost)'
```

A later run can use `-autotest_cmd 'start server(develop_validation/single/alife/load) client(localhost)'`.
Read the process exit status as well as `RESULT`, CSV and actual TGA images under
`_appdata_ixray_/logs`. An autotest reaches `exit()` after reporting and therefore
does not validate full normal level teardown. A menu-only `quit` under autotest
has no measured level frames and is not a passing rendering test.
