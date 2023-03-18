# Changelog

Full changelog of _IX-Ray_ 1.6 project

## Current

### Common

- Enabled assets packing
- Replaced `dxerr` with Windows SDK analog
- Replaced `stricmp` with POSIX analog
- Simplificated `get-dependencies` script launching
- Incompletely integrated __DirectXMesh__

### Engine

- Fixed autosaves
- Fixed switching ingame console language layout
- Fixed playing animation of getting weapons
- Fixed character info for deadbody color
- Fixed progress bar for optional using of `middle_color`
- Enabled screenshots capturing in windowed mode on __Direct3D 9__
- Enabled clearing highlight lists on each inventory action
- Enabled changing items condition by Num7 and Num8
- Replaced crashing with warning when there is no sound
- Disabled ammo highlights for knife and binocular
- Disabled stats by class and not by section

### Render

- Fixed inverted sky colors and sky bluring on __Direct3D 10+__
- Fixed potential memory leak in __DirectX 10__ resource manager
- Fixed fog accounting for campfire and anomalies
- Fixed water displaying on static lightning
- Fixed particles displaying on shooting
- Fixed `sload` for correct nearest bumps displaying
- Fixed `s_distort` parameter for `particles_xadd` blender on __Direct3D 10+__
- Enabled fog accounting for grass on static lightning
- Enabled fog accounting for wallmarks on static lightning
- Enabled fog accounting for particles
- Enabled static sun shadows on disabled cascades
- Enabled `Ldynamic_dir` counting for `accum_direct_volumetric()` on __Direct3D 9__
- Restored grass shadow
- Implemented __Direct3D 10__ initialization over __Direct3D 11__
- Implemented actor shadow
- Deleted __xrRender_R3__
- Deleted old TSM algorithms in all renders
- Deleted`r2_shadow_cascede_old` console command

### Dependencies

- Replaced `delete` operator with `xr_delete()` function in __xrXMLParser__

### Resources

- Implemented knife parameters class and and related features
- Implemented `use_condition` parameter
- Implemented notification restarting for `r2_sun` command

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
- Replaced `debug::make_final<T>` class to _C++11_ `final` specifier

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
