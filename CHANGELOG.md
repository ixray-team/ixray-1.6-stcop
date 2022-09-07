# Changelog

Full changelog of _IX-Ray_ 1.6 project

## Release 0.4 (September 2022)

### Common

- Added basic editorconfig
- Normalized line endings for the root files

### Engine

- Disabled use cases of `MSAA_ATEST_DX10_1_NATIVE`
- Disabled `r3_minmax_sm` by default
- Fixed crash when rendering volumetric fog on __Direct3D 11__
- Fixed sequence of initialization of API videocards
- Fixed screen resolution selection on missed `user.ltx`
- Fixed engine closing from taskbar menu
- Fixed displaying of transparent surfaces on HUD
- Fixed sunshafts for different sun quality
- Fixed transparent on static lightning
- Fixed comparison resulting in endless creation of new objects on __Direct3D 10+__
- Fixed camera glitches
- Implemented ability to switch entry point to a specific version of __Shader Model__
- Enabled teleport using `demo_record`
- Enabled `reload_dof` on weapons reloading
- Enabled __EAX__
- Added support for capturing cube map and location map on __Direct3D 10+__
- Added terrain mask support on static lighting
- Added inventory for ransacking monsters
- Added additional `set_weather` console command
- Added additional `read_sun_config` console command
- Added `trees_amplitude` option to weather settings
- Set FPS limit on UI rendering
- Increased range of near cascade

### Resources

- Normalized encoding of shaders
- Fixed _MSAA_ (redefinition of `Texture2DMS`)
- Fixed dynamic wet surfaces
- Fixed displaying of water foam
- Fixed skycube displaying on water surface
- Added shader to correct displaying of _LODs_ when _MSAA_ alphatest is enabled in classic __Direct3D 10__
- Added hint for ransacking monsters
- Implemented skyblend accumulation for sunshafts
- Implemented fog accounting for water on __Direct3D 9__
- Implemented light accounting for water foam
- Overrided entry point in _3D Fluid_ shaders
- Enabled `water_soft` shader for `water_studen` and `water_ryaska`

## Release 0.3 (May 2022)

### Common

- Migration to __Visual Studio 2022__
- Fixed compilation errors
- Fixed a lot of issues with linking
- Enabled multicore building for all projects
- Enabled __x86-64__ toolchain for all projects
- Enabled __GitHub Actions__
- Disabled debug and incremental info for all projects

### Core

- Replaced custom `xr_deque<T>` and `xr_vector<T>` with aliases of `std::deque<T>` and `std::vector<T>`
- Placed `clear_and_reserve()` method of `xr_vector<T>` class in a separate function
- Partially replaced STL extension aliases with `using` analogs
- Deleted `DEF_*` and `DEFINE_*` macroses from STL extensions

### Engine

- Replaced path to `shaders_cache` in all renders
- Fixed bug with exporting light to render
- Fixed __VSync__ on all renders
- Fixed blurring fonts on D3D11
- Fixed dialog window of level changer

### Dependencies

- Replaced `Flobbster.Windows.Forms` binary
- Replaced `dockpanelsuite` and bumped to `3.1`
- Deleted unused __Intel VTune__ functionality
- Deleted unused __OpenAutomate__ functionality
- Bumped `TargetFramework` to __.NET Framework__ 4.7.2

### Resources

- Added resources
- Normalized encoding of scripts

## Release 0.2 (November 2011)

### Common

- Migration to __Visual Studio 2015__
- Fixed compilation errors
- Replaced deprecated functions to safe and modern analogs
- Replaced some custom functions and types to standard library analog
- Disabled hardcoded _GUID_ of __DirectX__
- Replaced `debug::make_final<T>` class to *C++11* `final` specifier

### Core

- Removed __BugTrap__ and __minizip__
- Fixed `Debug` configuration workability
- Fixed window focus error

### Engine

- Fixed `-nointro` key
- Unlocked console commands: `hud_fov`, `fov`, `jump_to_level`, `g_god`, `g_unlimitedammo`, `run_script`, `run_string`, `time_factor`
- Changed viewing angle coefficient to `67.5`

### Utilities

- Incompletely integrated __DirectXTex__

## Release 0.1 (March 2021)

### Common

- Migration to __Visual Studio 2013__
- Fixed compilation errors
- Fixed windows displaying in editor projects
- Configured engine and editor projects building
- Configured audio libraries and __OpenAutomate__ projects building

### Core

- Fixed looping and breaking the stack on `NODEFAULT`

### Engine

- Fixed skyboxes stretching
