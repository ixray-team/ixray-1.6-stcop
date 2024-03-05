# Changelog

Full changelog of _IX-Ray_ 1.6 project

## Release 0.7 (March 2024)

### Common

- Improved projects structure
- Enabled C++20 for all projects
- Replace 3D SDK with actual version
- Enabled Unicode globally
- Fixed x64 runtime issues
- Fixed utilities building issues
- Restructured render folders with refactoring
- Restructured building workflows and pipelines
- Enabled printing compressor log on pipeline
- Replaced Luabind with non-Boost version
- Deleted Boost files
- Reworked memory allocator and memory modules

### Packages

- Replaced LuaJIT with NuGet package
- Replaced DirectXTex with nvtt in xrDXT project
- Replaced FreeImage with NuGet package

### Engine

- Disable reading `user.ltx` config in root folder
- Moved xrXMLParser to xrCore
- Rewritten CPUID module
- Rewritten CRC32 without Boost
- Implemented new stack walker
- Deleted `ttapi` module
- Rewritten renders iterator
- Optimized weapon sounds update
- Added invalidate state for rain
- Fixed using temp sound buffer
- Fixed rain sound after loading or changing level
- Enabled window minimization on `do_exit` and `terminate` calls
- Set special function for terminate
- Fixed window minimization unhandled error
- Fixed infinite unhandled crash handler execution
- Fixed game closing after error on not connected debugger
- Fixed weather cycles loading
- Enabled weather logging macros on non `MASTER_GOLD` configurations
- Fixed sound buffer size
- Fixed wrong variable name assignment in sound environment
- Rewritten timers
- Added defferred `R_ASSERT` event
- Fixed drop FPS for not valid spawn position of object
- Enabled printing Lua stack information and variables to log
- Enabled printing class ID error to log on release configuration
- Fixed to use normal system devices by OpenAL Soft
- Deleted hardcode for level box

### Render

- Implemented FXAA on static lightning
- Fixed crash on bones synchronization
- Implemented SMAA in xrRender_R2
- Implemented disabling of shader cache reading
- Fixed mipmap loading
- Fixed distorted glass displaying on static lighting
- Deleted unused `ConsoleRender` module
- Deleted `xrSkin2W_SSE` module
- Deleted `advancedpp` option
- Deleted `*_nomsaa` related shaders
- Implemented rendertarget resolution acquisition
- Deleted `albedo_wo` option
- Deleted `blur` option
- Implemented `r2_cloud_shadows` command
- Fixed sun shadows on xrRender_R2
- Implemented `r2_def_aref_quality` command
- Implemented displaying window node name by cursor focus
- Implemented `D3D_FEATURE_LEVEL_11_1` support
- Deleted MSAA in resources
- Deleted MSAA in render
- Deleted `r3_msaa_alphatest` command
- Deleted `r__supersample` command
- Implemented RenderDoc support
- Implemented optional parallel textures loading
- Added support volumemap to D3D11 render
- Implemented texture stagging control for all renders
- Added `-dxdebug` key instead DirectX debug flag
- Increased value of `rsDVB_Size` variable
- Fixed wallmarks on dynamic objects
- Fixed memory leak in `CDetailManager` class
- Set maximum textures quality by default
- Fixed double `ShaderResourceView` creation
- Set initial values of `CROS_impl` class variables

### Gameplay

- Implemented crosshair control in adjust interface
- Fixed spawn grenade after grenade change
- Fixed `hud_fov` command
- Added `checkout_bones` parameter for `CMissile` class
- Fixed opening doors by NPC
- Implemented custom mark feature
- Implemented custom text feature
- Fixed incorrect head rotation of trader
- Fixed load last save button behavior after loading level
- Added developer float commands
- Implemented customization of `UICursor` control
- Implemented customization of `UITrackBar` control
- Fixed crashes on `UIItemInfo` non-existent node
- Added method for quick adding any custom static
- Fixed NPC dispersion by rank
- Deleted R_ASSERT2 in `CUIDialogWnd::HideDialog` method
- Deleted duplicated `Fvector2` in `CUICellContainer::PlaceItemAtPos` method
- Fixed moving items to fast slot if grid larger than grid of slot
- Fixed weapon highlights
- Added left hand transform matrix
- Fixed displaying addons on weapons
- Implemented rain and thunderbolt starting delay
- Made IK stop further than 100m from actor position
- Fixed `get_wnd_pos` function export
- Added exception message to `SetCharacterCommunity` method
- Fixed incorrect type passed `GameObject::eDeath` callback
- Added object existence check
- Fixed spawn paths related errors
- Fixed message output in `show_dialog` function
- Deleted `dump_infos` command from `actor_binder:update` function
- Fixed mutants logic in restrictors broke down
- Fixed triggering of adding monsters contacts
- Enabled ragdoll for deadbody
- Implemented new external features system
- Fixed level graph invalid vertex ID error
- Implemented `hud_fov_zoom` weapon parameter
- Added underrun buffer checking to fix playback bug
- Fixed walking in `mcLookout` states
- Set initial values of `CStreamReader` class variables
- Fixed HUD models clipping
- Implemented grenade explosion on hit
- Implemented stopping animation of getting detector
- Replaced missing sound with stub on using scripts
- Moved actor to beginning of spawn
- Fixed calculating position of items in inventory grid
- Implemented timer for new game and save loading

### Utilities

- Fixed EFC building issues
- Added EFC utility source code
- Implemented new lightmap saving algorithm
- Deleted threads limit in utilities
- Added MagicFM CMake project
- Ported MagicFM to C++20
- Replaceed `-keep_temp_files` with `-clear_temp_files` key
- Implemented show of all missing TGA textures and THM files
- Replaced MMX with SSE in `Place_Perpixel` function
- Fixed loop in `CGraphMerger` method
- Deleted duplicated and dead files from xrAI
- Added `-all` key to compilers and enable `-do -ai -lc` modes
- Merged compilers into one project
- Fixed saving `build.cform` file in xrLC
- Implemented skip invalid faces in xrLC
- Fixed xrCompress to support multibyte WinAPI functions

### Plugins

- Updated 3D SDK
- Ported plugins to х64
- Ported plugins to C++20
- Updated LW Server project
- Created plugin folder in CMake
- Applied minor fixes for Max Export plugin
- Ported Max Material plugin to C++20
- Added Max Material CMake project
- Applied patches to plugins

## Release 0.6.1 (September 2023)

### Common

- Added logo icons and TortoiseGit config (@acidicMercury8)
- Fixed incorrect including of `FastDelegate` header (@Drombeys)
- Fixed project references and links (@acidicMercury8)
- Replaced `_snprintf` with `_snprintf_s` in ODE (@Drombeys)
- Deleted `IsPCAccessAllowed` function (@Drombeys)
- Deleted `ComputeModuleHash` function (@Drombeys)
- Deleted `is_enough_address_space_available` function (@Drombeys)
- Deleted unused `ttapi` includes (@Drombeys)
- Deleted unused `pSettingsAuth` pointer (@Drombeys)

### Engine

- Deleted `CopyProtection` module (@Drombeys)

### Render

- Fixed incorrect including of header files in renders projects (@Drombeys)

### Gameplay

- Fixed shotgun reload (@Shtrecker)
- Fixed incorrect including of header files in xrGame project (@Drombeys)
- Deleted `boost::noncopyable` related code from xrGame (@Drombeys)

### Resources

- Fixed fog displaying on static lighting (@Hozar2002)
- Unified shaders refactoring with IX-Ray 1.5 (@Drombeys)

## Release 0.6 (August 2023)

### Common

- Fixed most part of warnings (@acidicMercury8, @Drombeys)
- Fixed ignoring temp and metadata files by locator (@MAYLAYSHEZ)
- Fixed error skipping process troubles (@MAYLAYSHEZ)
- Fixed `std::unique_ptr` use cases (@acidicMercury8)
- Added __Visual Studio__ solution filters (@acidicMercury8)
- Added __Visual Studio Install__ config (@acidicMercury8)
- Enabled NuGet packages getting and caching in __GitHub Actions__ (@acidicMercury8)
- Enabled output log for debugger connected (@MAYLAYSHEZ)
- Set latest __Windows SDK__ version (@acidicMercury8)
- Splitted project configuration paths (@acidicMercury8)
- Refactored color math module (@Drombeys)
- Replaced `D3DCOLOR_RGBA`, `D3DCOLOR_ARGB` and `D3DCOLOR_XRGB` macroses (@Drombeys)
- Replaced legacy __DirectX Math__ with __DirectXMath__ analog (@Drombeys)
- Replaced `__uuidof` with `IID_PPV_ARGS` (@Drombeys)
- Replaced `__interface` keyword with `class` (@Drombeys)
- Replaced `STATIC_CHECK` with `static_assert` (@Drombeys)
- Replaced `__asm int 3` with `__debugbreak` (@Drombeys)
- Replaced `__asm pause` with `_mm_pause` (@Drombeys)
- Replaced `GetCLK` function with unified analog (@Drombeys)
- Replaced `GetTickCount` with `GetTickCount64` function (@Drombeys)
- Replaced `_snprintf` with `_snprintf_s` function (@Drombeys)
- Replaced `std::auto_ptr` pointers with `std::unique_ptr` (@Drombeys)
- Replaced `std::bind1st` and `std::bind2nd` with lambda functions (@Drombeys)
- Replaced linker directives with project references (@acidicMercury8)
- Replaced `FS_DEBUG` macro with `DEBUG` (@Drombeys)
- Renamed `xrDebugNew` module to `xrDebug` (@MAYLAYSHEZ)
- Deleted pragma deprecations (@acidicMercury8)
  - `strcpy`, `strcpy_s`, `sprintf`, `sprintf_s`, `strcat`, `strcat_s`
- Deleted `get_ref_count` function (@Drombeys)
- Deleted scripts for getting dependencies (@acidicMercury8)
- Deleted __ATI MGPU__ library with related code (@Drombeys)
- Deleted __NVAPI__ library with related code (@Drombeys)
- Deleted redundant `DEBUG_INVOKE` (@MAYLAYSHEZ)
- Deleted `boost::noncopyable` use cases from editor and xrPhysics (@Drombeys)
- Deleted `std::binary_function` use cases (@Drombeys)
- Deleted `std::unary_function` use cases (@Drombeys)
- Deleted unused `dwFrame` field of `xrCore` class (@Drombeys)
- Deleted unused `_GPA_ENABLED` blocks (@Drombeys)

### Packages

- Replaced __Flobbster.Windows.Forms__ with NuGet package (@acidicMercury8)
- Replaced __DockPanelSuite__ with NuGet package (@acidicMercury8)
- Replaced __DirectX SDK__ with NuGet package and __Windows SDK__ (@acidicMercury8, @Drombeys)
- Replaced __DirectXTex__ with NuGet package (@acidicMercury8, @Drombeys)
- Replaced __DirectXMesh__ with NuGet package (@acidicMercury8, @Drombeys)
- Replaced __Xiph__ libraries with NuGet packages (@acidicMercury8)
- Replaced __zlib__ with NuGet package (@acidicMercury8)

### Engine

- Fixed `cam_inert` console command (@MAYLAYSHEZ)
- Fixed bones synchronization (@xrLil-Batya)
- Fixed OGG related code of xrSound (@vadvalskiy)
- Deleted `xrTheora_Surface_mmx` module (@Drombeys)
- Deleted __SecuROM__ related code (@Drombeys)
- Deleted unused modules from xrSound (@vadvalskiy)
- Deleted `mailSlot` module (@Drombeys)
- Deleted launcher related code (@Drombeys)
- Deleted `no_single` module (@Drombeys)
- Deleted `dedicated_server_only` module and `PROTECT_API` macros (@Drombeys)
- Implemented `hud_fov` item parameter (@Shtrecker)
- Implemented `g_info` and `d_info` console commands (@Drombeys)
- Implemented `g_money` console command (@Drombeys)
- Implemented `g_spawn` and `g_spawn_inv` console command (@Drombeys, @Hozar2002)
- Integrated __OpenAL Soft__ with __EFX__ extensions (@johncurley)
- Unified xrEngine refactoring with __IX-Ray 1.5__ (@Drombeys)
- Splitted engine and server applications (@acidicMercury8)
- Replaced `-nointro` key with `keypress_on_start` command (@Drombeys)
- Refactored some sound modules (@johncurley, @vadvalskiy)

### Editors

- Fixed floating types converting in weather editor (@vadvalskiy)
- Fixed weather editor project (@vadvalskiy)
- Deleted unused resource files from weather editor (@vadvalskiy)

### Render

- Fixed wrong attenuation of far sun shadows (@Hozar2002)
- Fixed __HDAO__ crashing on disabled G-Buffer optimization (@morrazzzz)
- Fixed texture quality changing in D3D11 (@Drombeys, @Hozar2002, @mortany)
- Fixed sun flares for __FXAA__ (@OldSerpskiStalker)
- Fixed `MaxAnisotropy` parameter for __Shader Model 5__ (@OldSerpskiStalker)
- Fixed some memory leaks (@OldSerpskiStalker, @Drombeys)
- Fixed enumeration of option definitions on __Direct3D 10+__ (@OldSerpskiStalker)
- Fixed sunshafts on enabled `accum_sun_near_nomsaa_minmax` shader (@OldSerpskiStalker)
- Fixed transparent on static lightning (@Hozar2002)
- Fixed indentation of texture memory message (@MAYLAYSHEZ)
- Added `r2_use_bump` command to disable bumps on R2+ renderers (@DanceManiac)
- Activated `R2FLAG_USE_BUMP` flag by default (@Drombeys)
- Set `DXGI_ENUM_MODES_INTERLACED` flag (@Drombeys)
- Replaced `asm` block with standard math functions (@Drombeys)
- Replaced legacy __DirectX Tex__ with __DirectXTex__ analog (@Drombeys)
- Replaced deprecated shader compiler with __Windows SDK__ analog (@Drombeys)
- Refactored `dx11Texture` class (@Drombeys)
- Decomposed screenshot creation methods (@Drombeys)
- Deleted duplicated `END_EPS` constant (@Drombeys)
- Deleted __Direct3D 10__ dependencies (@Drombeys)
- Deleted unused mipped noise (@Drombeys)
- Deleted __Intel GMA__ related code (@Drombeys)
- Deleted unused `sunfilter` option (@Drombeys)
- Deleted unused `accum_direct` methods (@Drombeys)
- Deleted `bug` option in all renders (@Drombeys)
- Deleted `sjitter` option in all renders (@Drombeys)
- Deleted `depth16` option in all renders (@Drombeys)
- Improved depth buffer format handling (@Drombeys)
- Implemented disabling of anisotropic filtering in __Direct3D 10+__ (@Drombeys)
- Implemented `mipLodBias` property setter for __Direct3D 10+__ (@Drombeys)
- Implemented textures reloading directly in game (@MAYLAYSHEZ)
- Implemented __FXAA__ support (@OldSerpskiStalker, @Drombeys, @Hozar2002)
- Implemented __SMAA__ support (@OldSerpskiStalker, @Drombeys, @Hozar2002)
- Implemented shader-based fog on static lightning (@Hozar2002)
- Unlocked __MSAA x8__ (@OldSerpskiStalker)

### Gameplay

- Fixed refreshing of trade list (@mortany)
- Fixed actor visibility indicator after quickload (@Hrusteckiy)
- Fixed offset for answer numbers (@Hrusteckiy)
- Fixed detector animation playing (@Shtrecker)
- Fixed stuttering after reloading grenade launcher (@Shtrecker)
- Fixed rows and columns of artifact belt (@DanceManiac)
- Fixed animation playing on attached grenade launcher (@Shtrecker)
- Fixed playing idle animation on empty state (@Shtrecker)
- Fixed reload animation playing on active detector (@Shtrecker)
- Fixed switch animation playing (@Shtrecker)
- Fixed grenade launcher action (@Shtrecker)
- Fixed crashing of sliding type doors (@Drombeys)
- Fixed crashing in adjust hud mode (@Shtrecker)
- Fixed flickering after game item usage (@Drombeys)
- Fixed loading ammo in grenade mode (@Shtrecker)
- Fixed flight grenade (@Shtrecker)
- Enabled inertia control from HUD section (@Hozar2002, @Drombeys)
- Implemented 100x100 icons support (@Hrusteckiy, @DanceManiac)
- Implemented custom autoreloading for weapons (@Shtrecker)
- Implemented custom autoreloading for grenade launcher (@Shtrecker)
- Implemented full and partial weapon reloading (@Shtrecker)
- Implemented misfire of weapon (@Shtrecker)
- Implemented reloadings in grenade launcher weapons (@Shtrecker)
- Implemented delay before reloading on active detector (@Shtrecker)
- Implemented scripted key blocking (@Drombeys)
- Implemented blocking actor movement (@Drombeys)
- Refactored some weapons and detectors methods (@Shtrecker)

### Resources

- Fixed text line alignment for talk dialogs (@Hrusteckiy)
- Fixed `object_alive` condition in `bind_monster` script (@Hozar2002)
- Set weapon autoreloading settings (@Shtrecker)
- Implemented __SMAA__ support in assets (@OldSerpskiStalker, @Drombeys, @Hozar2002)
- Implemented __FXAA__ support in assets (@OldSerpskiStalker, @Drombeys, @Hozar2002)
- Updated shaders to Shader Model 3.0 (@Drombeys, @Hozar2002)

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
