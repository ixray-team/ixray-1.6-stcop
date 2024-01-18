cmake_minimum_required(VERSION 3.15)
project(IXRay)

# CMake CI options
option(CI_IXRAY_UTILS "Build only utils" OFF)
option(CI_IXRAY_COMPRESSOR_ONLY "Build only compressor" OFF)

# CMake options
option(IXRAY_UTILS "Apply utils to solution" OFF)
option(IXRAY_MP "Apply multiplayer xrGame and multiplayer utilities to solution" OFF)
option(IXRAY_PLUGIN_MAX "Enable X-Ray Plugins for Autodesk 3ds Max" OFF)
option(IXRAY_PLUGIN_LW "Enable X-Ray Plugins for Lightwave 2020" OFF)
option(IXRAY_ASAN "Enable Address Sanitizer" OFF)
option(IXRAY_USE_R1 "Enable DirectX 9 static render" ON)
option(IXRAY_USE_R2 "Enable DirectX 9 dynamic render" ON)
option(IXRAY_UNITYBUILD "Enable UnityBuild for xrGame" ON)
option(IXRAY_USE_OPTICK "Enable support Optick Profiler" OFF)

# CMake Dev options
option(DEVIXRAY_ENABLE_SHIPPING "Enable shipping build cfg" OFF)

# VS options
set(CMAKE_EXECUTABLE_ENABLE_EXPORTS ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if (MSVC)
    set(CMAKE_CXX_FLAGS_DEBUG "/MD")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /UMBCS /D_UNICODE /DUNICODE")
endif()

# Add new build types
## Shipping
if (DEVIXRAY_ENABLE_SHIPPING)
    SET(CMAKE_CXX_FLAGS_SHIPPING "${CMAKE_CXX_FLAGS}" CACHE STRING "Flags used by the C++ compiler during coverage builds." FORCE )
    SET(CMAKE_C_FLAGS_SHIPPING "${CMAKE_C_FLAGS}" CACHE STRING "Flags used by the C compiler during coverage builds." FORCE )
    SET(CMAKE_EXE_LINKER_FLAGS_SHIPPING "" CACHE STRING "Flags used for linking binaries during coverage builds." FORCE )
    SET(CMAKE_SHARED_LINKER_FLAGS_SHIPPING "" CACHE STRING "Flags used by the shared libraries linker during coverage builds." FORCE )
    MARK_AS_ADVANCED(CMAKE_CXX_FLAGS_SHIPPING CMAKE_C_FLAGS_SHIPPING CMAKE_EXE_LINKER_FLAGS_SHIPPING CMAKE_SHARED_LINKER_FLAGS_SHIPPING )
    
    SET(IXR_CONFIGURATIONS_STR "Debug;RelWithDebInfo;Release;Shipping" CACHE STRING "" FORCE)
else()
    SET(IXR_CONFIGURATIONS_STR "Debug;RelWithDebInfo;Release" CACHE STRING "" FORCE)
endif()

# Win32 Extensions
if (MSVC AND CMAKE_SIZEOF_VOID_P EQUAL 4)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /LARGEADDRESSAWARE")
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /LARGEADDRESSAWARE")
    ADD_DEFINITIONS(/arch:SSE2)
endif()

# Wrap CMake general configs 
set(CMAKE_CONFIGURATION_TYPES ${IXR_CONFIGURATIONS_STR} CACHE STRING "" FORCE)
set(PREDEFINED_TARGETS_FOLDER "CustomTargets")

# Output target
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Enable gcc/clang style for MSVC
if (MSVC)
    add_compile_options(/permissive- /fp:fast /wd4073 /wd4390 /wd4273)

    string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
    add_compile_options("$<$<CONFIG:DEBUG>:/Od>""$<$<CONFIG:DEBUG>:/MD>" "/Ob1" "$<$<CONFIG:DEBUG>:/EHsc>"
                    "$<$<CONFIG:RELEASE>:/Ot>"  "$<$<CONFIG:RELEASE>:/Ob2>" "$<$<CONFIG:RELWITHDEBINFO>:/wd4577>")
    add_compile_options($<$<CXX_COMPILER_ID:MSVC>:/MP>)
    add_compile_options("$<$<CONFIG:RELEASE>:/wd4530>" "$<$<CONFIG:DEBUG>:/wd4275>" "$<$<CONFIG:DEBUG>:/wd4251>" "$<$<CONFIG:RELWITHDEBINFO>:/wd4530>" /wd4595 /wd4996)
endif()

# Set LLVM config
if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    add_compile_options(-Wno-reorder-ctor -Wno-switch -Wno-macro-redefined -Wno-implicit-exception-spec-mismatch -Wno-deprecated-volatile 
                        -Wno-inline-new-delete -Wno-c++11-narrowing -Wno-unused-local-typedef -Wno-defaulted-function-deleted -Wno-undefined-inline)
    if (WIN32)
        add_compile_options(--Wno-microsoft-template-shadow)
    else()
        add_compile_options(-fms-extensions -Wno-null-conversion)
    endif()
endif()

# Shipping config
if (DEVIXRAY_ENABLE_SHIPPING)
    add_compile_options("$<$<CONFIG:Shipping>:/wd4530>" "$<$<CONFIG:Shipping>:/wd4251>" "$<$<CONFIG:Shipping>:/wd4275>" "$<$<CONFIG:Shipping>:/wd4577>" "$<$<CONFIG:Shipping>:/Ob2>" )
    add_compile_options("$<$<CONFIG:Shipping>:/WX>")
endif()

if (IXRAY_ASAN)
    add_compile_options(-fsanitize=address)
endif()

## Edit and Continue mode
if (WIN32)
    if (IXRAY_ASAN)
        add_compile_options("$<$<CONFIG:DEBUG>:/Zi>" "$<$<CONFIG:RELWITHDEBINFO>:/Zi>" "$<$<CONFIG:RELEASE>:/Zi>")
    else()
        add_compile_options("$<$<CONFIG:DEBUG>:/ZI>" "$<$<CONFIG:RELWITHDEBINFO>:/Zi>" "$<$<CONFIG:RELEASE>:/Zi>")
    endif()
endif()

# Configure NuGet
find_program(NUGET_COMMAND nuget)
if(NOT NUGET_COMMAND)
    message("NuGet not found in PATH!")
    message("Downloading NuGet...")
    if(NOT EXISTS "${CMAKE_BINARY_DIR}/dep/nuget")
        execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/dep/nuget")
        file(DOWNLOAD https://dist.nuget.org/win-x86-commandline/latest/nuget.exe
             "${CMAKE_BINARY_DIR}/dep/nuget/nuget.exe")
    endif()
    set(NUGET_COMMAND "${CMAKE_BINARY_DIR}/dep/nuget/nuget.exe")
    message("NuGet downloaded: ${NUGET_COMMAND}")
else()
    message("NuGet found: ${NUGET_COMMAND}")
endif()

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
