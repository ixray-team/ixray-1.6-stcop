# Changelog

Full changelog of _IX-Ray_ 1.6 project

## Release 0.6 (August 2023)

### Common

- Fixed most part of warnings
- Fixed ignoring temp and metadata files by locator
- Fixed error skipping process troubles
- Fixed `std::unique_ptr` use cases
- Added __Visual Studio__ solution filters
- Added __Visual Studio Install__ config
- Enabled NuGet packages getting and caching in __GitHub Actions__
- Enabled output log for debugger connected
- Set latest __Windows SDK__ version
- Splitted project configuration paths
- Refactored color math module
- Replaced `D3DCOLOR_RGBA`, `D3DCOLOR_ARGB` and `D3DCOLOR_XRGB` macroses
- Replaced legacy __DirectX Math__ with __DirectXMath__ analog
- Replaced `__uuidof` with `IID_PPV_ARGS`
- Replaced `__interface` keyword with `class`
- Replaced `STATIC_CHECK` with `static_assert`
- Replaced `__asm int 3` with `__debugbreak`
- Replaced `__asm pause` with `_mm_pause`
- Replaced `GetCLK` function with unified analog
- Replaced `GetTickCount` with `GetTickCount64` function
- Replaced `_snprintf` with `_snprintf_s` function
- Replaced `std::auto_ptr` pointers with `std::unique_ptr`
- Replaced `std::bind1st` and `std::bind2nd` with lambda functions
- Replaced linker directives with project references
- Replaced `FS_DEBUG` macro with `DEBUG`
- Renamed `xrDebugNew` module to `xrDebug`
- Deleted pragma deprecations
  - `strcpy`, `strcpy_s`, `sprintf`, `sprintf_s`, `strcat`, `strcat_s`
- Deleted `get_ref_count()` function
- Deleted scripts for getting dependencies
- Deleted __ATI MGPU__ library with related code
- Deleted __NVAPI__ library with related code
- Deleted redundant `DEBUG_INVOKE`
- Deleted `boost::noncopyable` use cases from editor and xrPhysics
- Deleted `std::binary_function` use cases
- Deleted `std::unary_function` use cases
- Deleted unused `dwFrame` field of `xrCore` class
- Deleted unused `_GPA_ENABLED` blocks

### Packages

- Replaced __Flobbster.Windows.Forms__ with NuGet package
- Replaced __DockPanelSuite__ with NuGet package
- Replaced __DirectX SDK__ with NuGet package and __Windows SDK__
- Replaced __DirectXTex__ with NuGet package
- Replaced __DirectXMesh__ with NuGet package
- Replaced __Xiph__ libraries with NuGet packages
- Replaced __zlib__ with NuGet package

### Engine

- Fixed `cam_inert` console command
- Fixed bones synchronization
- Fixed OGG related code of xrSound
- Deleted `xrTheora_Surface_mmx` module
- Deleted __SecuROM__ related code
- Deleted unused modules from xrSound
- Deleted `mailSlot` module
- Deleted launcher related code
- Implemented `hud_fov` item parameter
- Implemented `g_info` and `d_info` console commands
- Implemented `g_money` console command
- Implement `g_spawn` and `g_spawn_inv` console command
- Integrated __OpenAL Soft__ with __EFX__ extensions
- Unified xrEngine refactoring with __IX-Ray 1.5__
- Splitted engine and server applications
- Replaced `-nointro` key with `keypress_on_start` command
- Refactored some sound modules

### Editors

- Fixed floating types converting in weather editor
- Fixed weather editor project
- Deleted unused resource files from weather editor

### Render

- Fixed wrong attenuation of far sun shadows
- Fixed __HDAO__ crashing on disabled G-Buffer optimization
- Fixed texture quality changing in D3D11
- Fixed sun flares for __FXAA__
- Fixed `MaxAnisotropy` parameter for __Shader Model 5__
- Fixed some memory leaks
- Fixed enumeration of option definitions on __Direct3D 10+__
- Fixed sunshafts on enabled `accum_sun_near_nomsaa_minmax` shader
- Fixed transparent on static lightning
- Fixed indentation of texture memory message
- Added `r2_use_bump` command to disable bumps on R2+ renderers
- Activated `R2FLAG_USE_BUMP` flag by default
- Set `DXGI_ENUM_MODES_INTERLACED` flag
- Replaced `asm` block with standard math functions
- Replaced legacy __DirectX Tex__ with __DirectXTex__ analog
- Replaced deprecated shader compiler with __Windows SDK__ analog
- Refactored `dx11Texture` class
- Decomposed screenshot creation methods
- Deleted duplicated `END_EPS` constant
- Deleted __Direct3D 10__ dependencies
- Deleted unused mipped noise
- Deleted __Intel GMA__ related code
- Deleted unused `sunfilter` option
- Deleted unused `accum_direct` methods
- Deleted `bug` option in all renders
- Deleted `sjitter` option in all renders
- Deleted `depth16` option in all renders
- Improved depth buffer format handling
- Implemented disabling of anisotropic filtering in __Direct3D 10+__
- Implemented `mipLodBias` property setter for __Direct3D 10+__
- Implemented textures reloading directly in game
- Implemented __FXAA__ support
- Implemented __SMAA__ support
- Implemented shader-based fog on static lightning
- Unlocked __MSAA x8__

### Gameplay

- Fixed refreshing of trade list
- Fixed actor visibility indicator after quickload
- Fixed offset for answer numbers
- Fixed detector animation playing
- Fixed stuttering after reloading grenade launcher
- Fixed rows and columns of artifact belt
- Fixed animation playing on attached grenade launcher
- Fixed playing idle animation on empty state
- Fixed reload animation playing on active detector
- Fixed switch animation playing
- Fixed grenade launcher action
- Fixed crashing of sliding type doors
- Fixed crashing in adjust hud mode
- Fixed flickering after game item usage
- Fixed loading ammo in grenade mode
- Fixed flight grenade
- Enabled inertia control from HUD section
- Implemented 100x100 icons support
- Implemented custom autoreloading
- Implemented custom autoreloading for grenade launcher
- Implemented full and partial weapon reloading
- Implemented misfire of weapon
- Implemented reloadings in grenade launcher weapons
- Implemented delay before reloading on active detector
- Implemented scripted key blocking
- Implemented blocking actor movement
- Refactored some weapons and detectors methods

### Resources

- Fixed text line alignment for talk dialogs
- Fixed `object_alive` condition in `bind_monster` script
- Set weapon autoreloading settings
- Implemented __SMAA__ support in assets
- Implemented __FXAA__ support in assets
- Updated shaders to Shader Model 3.0

## Release 0.5 (March 2023)

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
- Implemented `ui_reload` command
- Allowed to use min and max colors for double progress bar
- Allowed to change upgrade icon color by config
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
- Prevented writing alpha-blended geometry to depth buffer
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

## Release 0.2 (November 2021)

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
