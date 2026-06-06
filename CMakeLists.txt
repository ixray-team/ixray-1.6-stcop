cmake_minimum_required(VERSION 3.26)
project(IXRay)

# CMake options
option(IXRAY_UTILS "Apply utils to solution" OFF)
option(IXRAY_UTILS_OPTIX "Enable NVIDIA OptiX support" OFF)
option(IXRAY_EDITORS "Apply editors to solution" OFF)
option(IXRAY_MP "Apply multiplayer xrGame and multiplayer utilities to solution" OFF)
option(IXRAY_MP_SQL "Use SQL Database system for FreeMP mode" OFF)
option(IXRAY_PLUGINS "Enable X-Ray Plugins" OFF)
option(IXRAY_ASAN "Enable Address Sanitizer" OFF)
option(IXRAY_LDEBUG "Enable LUA_DEBUG mode" OFF)
option(IXRAY_USE_R1 "Enable DirectX 9 static render" ON)
option(IXRAY_USE_R2 "Enable DirectX 9 dynamic render" ON)
option(IXRAY_USE_GL "Enable OpenGL render" ON)
option(IXRAY_USE_COMPRESSOR "Enable xrCompresor" ON)
option(IXRAY_UNITYBUILD "Enable UnityBuild for xrGame" ON)
option(IXRAY_PROFILER "Enable Optick for perf debug" ON)
option(IXRAY_PROFILER_TRACY "Enable Tracy for perf debug" OFF)
set(IXRAY_BUILD_BATCH_SIZE "32" CACHE STRING "UNITY_BUILD_BATCH_SIZE")

# CI trash
option(IXRAY_COMPRESSOR_ONLY "Build only compressor" OFF)
option(IXRAY_CI "GitHub Actions build" OFF)
set(IXR_TEST_CI NOT IXRAY_UTILS OR NOT IXRAY_CI)

# CMake Dev options
option(DEVIXRAY_ENABLE_SHIPPING "Enable shipping build cfg" OFF)
option(DEVIXRAY_ENABLE_OGL_RENDER "Enable OGL Render" OFF)

# CXX options
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    include("cmake/clang.cmake")
elseif (MSVC)
    include("cmake/msvc.cmake")
else()
    include("cmake/gcc.cmake")
endif()

# Add new build types
include("cmake/shipping.cmake")

# Wrap CMake general configs 
set(CMAKE_CONFIGURATION_TYPES ${IXR_CONFIGURATIONS_STR} CACHE STRING "" FORCE)
set(PREDEFINED_TARGETS_FOLDER "CustomTargets")

# ASan
if (IXRAY_ASAN)
    add_compile_options(-fsanitize=address)
endif()

## Disable user-cheats for Release/Shipping build
add_compile_definitions("$<$<CONFIG:Debug>:DEBUG>")
add_compile_definitions("$<$<CONFIG:Debug>:DEBUG_DRAW>")
add_compile_definitions("$<$<CONFIG:RelWithDebInfo>:DEBUG_DRAW>")
add_compile_definitions("$<$<CONFIG:Release>:MASTER_GOLD>")
if (DEVIXRAY_ENABLE_SHIPPING)
    add_compile_definitions("$<$<CONFIG:Shipping>:MASTER_GOLD>")
    add_compile_definitions("$<$<CONFIG:Shipping>:SHIPPING_BUILD>")
endif()

# Download API from GitHub Releases
include("cmake/github.cmake")

# Help functions
include("cmake/functions.cmake")

# Configure Package Manager
if (WIN32)
	include("cmake/packages/nuget/nuget.cmake")
elseif (LINUX)
	include("cmake/packages/linux/manager.cmake")
endif()

# Formatter
if (NOT IXRAY_CI)
    include("cmake/clangtidy.cmake")
endif()

# SDK folders
set(IXRAY_SDK_LIB ${CMAKE_CURRENT_SOURCE_DIR}/sdk/libraries/${CMAKE_VS_PLATFORM_NAME}/)
set(IXRAY_SDK_BIN ${CMAKE_CURRENT_SOURCE_DIR}/sdk/binaries/${CMAKE_VS_PLATFORM_NAME}/)
set(IXRAY_SDK_INC ${CMAKE_CURRENT_SOURCE_DIR}/sdk/include/)

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
	set(IXRAY_OUTPUT ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
else()
	set(IXRAY_OUTPUT ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/)
endif()

# Modules (Nuget or Src)
set(EXTERNAL_INCLUDE_DIR ${CMAKE_BINARY_DIR}/_deps_header_only/)
file(MAKE_DIRECTORY "${EXTERNAL_INCLUDE_DIR}")
include("cmake/modules.cmake")

# CMake targets
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# 3rd libraries
add_subdirectory("src/3rd-party")

# Engine libraries
add_subdirectory("src")

# Plugins 
if (IXRAY_PLUGINS)
    add_subdirectory("src/plugins")
endif()

# Utils
add_subdirectory("src/utils")

# Editors
add_subdirectory("src/Editors")

# Startup project (Visual Studio only)
if (WIN32)
    if (IXRAY_COMPRESSOR_ONLY)
        if (TARGET xrCompress)
            set_property(DIRECTORY ${CMAKE_SOURCE_DIR}
                PROPERTY VS_STARTUP_PROJECT xrCompress)
        endif()
    else()
        if (TARGET xrEngine)
            set_property(DIRECTORY ${CMAKE_SOURCE_DIR}
                PROPERTY VS_STARTUP_PROJECT xrEngine)
        endif()
    endif()
endif()