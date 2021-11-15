# Changelog

Full changelog of *IX-Ray* 1.6 project

<!-- markdownlint-disable MD024 -->

## Release 0.2 (November 2011)

### Common

- Migration to **Visual Studio 2015**
- Fixed compilation errors
- Replaced deprecated functions to safe and modern analogs
- Replaced some custom functions and types to standard library analog
- Disabled hardcoded *GUID* of *DirectX*
- Replaced `debug::make_final<T>` class to *C++11* `final` specifier

### Core

- Removed **BugTrap** and **minizip**
- Fixed `Debug` configuration workability
- Fixed window focus error

### Engine

- Fixed `-nointro` key
- Unlocked console commands: `hud_fov`, `fov`, `jump_to_level`, `g_god`, `g_unlimitedammo`, `run_script`, `run_string`, `time_factor`
- Changed viewing angle coefficient to 67.5

### Utilities

- Incompletely integrated **DirectXTex**

## Release 0.1 (March 2021)

### Common

- Migration to **Visual Studio 2013**
- Fixed compilation errors
- Fixed windows displaying in editor projects
- Configured engine and editor projects building
- Configured audio libraries and **OpenAutomate** projects building

### Core

- Fixed looping and breaking the stack on `NODEFAULT`

### Engine

- Fixed skyboxes stretching

<!-- markdownlint-enable MD024 -->
