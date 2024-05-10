cmake_minimum_required(VERSION 3.15)
project(IXRay)

# CMake options
option(IXRAY_UTILS "Apply utils to solution" OFF)
option(IXRAY_EDITORS "Apply editors to solution" OFF)
option(IXRAY_MP "Apply multiplayer xrGame and multiplayer utilities to solution" OFF)
option(IXRAY_COMPRESSOR_ONLY "Build only compressor" OFF)
option(IXRAY_PLUGIN_MAX "Enable X-Ray Plugins for Autodesk 3ds Max" OFF)
option(IXRAY_PLUGIN_LW "Enable X-Ray Plugins for Lightwave 2020" OFF)
option(IXRAY_ASAN "Enable Address Sanitizer" OFF)
option(IXRAY_USE_R1 "Enable DirectX 9 static render" ON)
option(IXRAY_USE_R2 "Enable DirectX 9 dynamic render" ON)
option(IXRAY_USE_COMPRESSOR "Enable xrCompresor" ON)
option(IXRAY_UNITYBUILD "Enable UnityBuild for xrGame" ON)

# CMake Dev options
option(DEVIXRAY_ENABLE_SHIPPING "Enable shipping build cfg" OFF)
option(DEVIXRAY_ENABLE_OGL_RENDER "Enable OGL Render" OFF)

# VS options
set(CMAKE_EXECUTABLE_ENABLE_EXPORTS ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if (MSVC)
    include("cmake/msvc.cmake")
endif()

# Add new build types
include("cmake/shipping.cmake")

# Wrap CMake general configs 
set(CMAKE_CONFIGURATION_TYPES ${IXR_CONFIGURATIONS_STR} CACHE STRING "" FORCE)
set(PREDEFINED_TARGETS_FOLDER "CustomTargets")

# Output target
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Set LLVM config
if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    include("cmake/clang.cmake")
endif()

# Asan
if (IXRAY_ASAN)
    add_compile_options(-fsanitize=address)
endif()

# Configure NuGet
include("cmake/nuget.cmake")

# Download API from GitHub Releases
include("cmake/github.cmake")

# SDK folders
set(IXRAY_SDK_LIB ${CMAKE_CURRENT_SOURCE_DIR}/sdk/libraries/${CMAKE_VS_PLATFORM_NAME}/)
set(IXRAY_SDK_BIN ${CMAKE_CURRENT_SOURCE_DIR}/sdk/binaries/${CMAKE_VS_PLATFORM_NAME}/)
set(IXRAY_SDK_INC ${CMAKE_CURRENT_SOURCE_DIR}/sdk/include/)

# Configure dependencies
set(RENDERDOC_API "${CMAKE_CURRENT_SOURCE_DIR}/src/3rd Party/renderdoc")

# CMake targets
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# 3rd libraries
add_subdirectory("src/3rd party")

# Engine libraries
add_subdirectory("src")

# Plugins 
add_subdirectory("src/plugins")

# Utils
add_subdirectory("src/utils")

# Editors
if (IXRAY_EDITORS)
    add_subdirectory("src/Editors")
endif()