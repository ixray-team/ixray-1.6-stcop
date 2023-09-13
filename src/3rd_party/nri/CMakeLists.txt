cmake_minimum_required (VERSION 3.15)

# Options
option (NRI_DISABLE_XLIB_SUPPORT "Disable 'xlib' support" OFF)
option (NRI_DISABLE_WAYLAND_SUPPORT "Disable 'wayland' support" OFF)
option (NRI_STATIC_LIBRARY "Build static library" ON)

if (WIN32)
    option (NRI_ENABLE_STREAMLINE_SUPPORT "Enables streamline wrapper for streamline integration. Requires NRI_STREAMLINE_SDK_PATH to point to a valid streamline package" OFF)
    option(NRI_USE_DEVELOPMENT_STREAMLINE_LIBRARY "Makes the project use development streamline libraries instead of regular ones." OFF)
    set (NRI_STREAMLINE_SDK_PATH "" CACHE STRING "Path to a directory containing streamline sdk package. Must not include the name of the library 'sl.interposer.lib'. Must not be empty when NRI_ENABLE_STREAMLINE_SUPPORT is on.")
endif ()

# Is submodule?
if (${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_CURRENT_SOURCE_DIR})
    set (IS_SUBMODULE OFF)
else()
    set (IS_SUBMODULE ON)
endif()

# Cached
if (NOT IS_SUBMODULE)
    set (CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "")
endif()

set (GLOBAL_BIN_OUTPUT_PATH "${IXRAY_SDK_BIN}" CACHE STRING "")

# Create project
file (READ "${CMAKE_CURRENT_SOURCE_DIR}/Include/NRI.h" ver_h)
string (REGEX MATCH "NRI_VERSION_MAJOR ([0-9]*)" _ ${ver_h})
set (VERSION_MAJOR ${CMAKE_MATCH_1})
string (REGEX MATCH "NRI_VERSION_MINOR ([0-9]*)" _ ${ver_h})
set (VERSION_MINOR ${CMAKE_MATCH_1})

message ("NRI v${VERSION_MAJOR}.${VERSION_MINOR}")
project (NRI LANGUAGES C CXX)

# Download dependencies using Packman
if (WIN32)
    set (PACKMAN_EXT ".cmd")
else ()
    set (PACKMAN_EXT ".sh")
endif ()

if ((CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64") OR(CMAKE_SYSTEM_PROCESSOR MATCHES "AMD64"))
    if (WIN32)
        set (PACKMAN_PLATFORM "windows-x86_64")
    else ()
        set (PACKMAN_PLATFORM "linux-x86_64")
    endif ()
elseif ((CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64") OR(CMAKE_SYSTEM_PROCESSOR MATCHES "arm64"))
    set (PACKMAN_PLATFORM "linux-aarch64")
else ()
    message (FATAL_ERROR "Unsupported architecture: '${CMAKE_SYSTEM_PROCESSOR}'")
endif ()

message ("Packman platform: ${PACKMAN_PLATFORM}")

execute_process (COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/External/Packman/packman${PACKMAN_EXT}" pull "${CMAKE_CURRENT_SOURCE_DIR}/Dependencies.xml" -p ${PACKMAN_PLATFORM}
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    RESULT_VARIABLE PACKMAN_RESULT)

if (NOT "${PACKMAN_RESULT}" STREQUAL "0")
    message (FATAL_ERROR "Packman failed(code = ${PACKMAN_RESULT})")
endif ()

# Setup dependencies
if (WIN32)
    if (CMAKE_SIZEOF_VOID_P EQUAL 8)
        set (BIN_ARCHITECTURE "x64")
        set (NVAPI_BIN_ARCHITECTURE "amd64")
    else ()
        set (BIN_ARCHITECTURE "x86")
        set (NVAPI_BIN_ARCHITECTURE "x86")
    endif ()

    # External
    find_library (INPUT_LIB_NVAPI NAMES nvapi64 nvapi PATHS "External/NVAPI/${NVAPI_BIN_ARCHITECTURE}" REQUIRED)
    find_library (INPUT_LIB_AGS NAMES amd_ags_${BIN_ARCHITECTURE} PATHS "External/AGS/lib" REQUIRED)

    # System
    if ("$ENV{WIN10_SDK_PATH}$ENV{WIN10_SDK_VERSION}" STREQUAL "")
        get_filename_component (WINDOWS_SDK_PATH "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots;KitsRoot10]" ABSOLUTE CACHE)
        set (WINDOWS_SDK_VERSION ${CMAKE_VS_WINDOWS_TARGET_PLATFORM_VERSION})

        if ("${WINDOWS_SDK_VERSION}" STREQUAL "")
            set (WINDOWS_SDK_VERSION ${CMAKE_SYSTEM_VERSION})
            message ("Using CMAKE_SYSTEM_VERSION as WindowsSDK version.(${CMAKE_SYSTEM_VERSION})")
        endif ()
    else ()
        set (WINDOWS_SDK_PATH $ENV{WIN10_SDK_PATH})
        set (WINDOWS_SDK_VERSION $ENV{WIN10_SDK_VERSION})
    endif ()

    if ("${WINDOWS_SDK_PATH}" STREQUAL "")
        message (FATAL_ERROR "Failed to detect WindowsSDK path. Please add enviroment variables WIN10_SDK_PATH and WIN10_SDK_VERSION or use MSVC")
    endif ()

    if ("${WINDOWS_SDK_VERSION}" STREQUAL "")
        message (FATAL_ERROR "Failed to detect WindowsSDK version. Please add enviroment variables WIN10_SDK_PATH and WIN10_SDK_VERSION or use MSVC")
    endif ()

    if (IS_DIRECTORY "${WINDOWS_SDK_PATH}/Include/${WINDOWS_SDK_VERSION}.0")
        set (WINDOWS_SDK_VERSION "${WINDOWS_SDK_VERSION}.0")
    endif ()

    set (WINDOWS_SDK_LIB_PATH "${WINDOWS_SDK_PATH}/Lib/${WINDOWS_SDK_VERSION}/um/${BIN_ARCHITECTURE}")

    find_library (INPUT_LIB_D3D11 NAMES d3d11 HINTS ${WINDOWS_SDK_LIB_PATH} REQUIRED)
    find_library (INPUT_LIB_D3D12 NAMES d3d12 HINTS ${WINDOWS_SDK_LIB_PATH} REQUIRED)
    find_library (INPUT_LIB_DXGI NAMES dxgi HINTS ${WINDOWS_SDK_LIB_PATH} REQUIRED)
    find_library (INPUT_LIB_DXGUID NAMES dxguid HINTS ${WINDOWS_SDK_LIB_PATH} REQUIRED)
endif ()

# Globals?
set_property (GLOBAL PROPERTY USE_FOLDERS ON)

set (CMAKE_CXX_STANDARD 17)
set (CMAKE_CXX_STANDARD_REQUIRED ON)
set (CMAKE_CXX_VISIBILITY_PRESET hidden)
set (CMAKE_CXX_EXTENSIONS OFF)
set (CMAKE_POSITION_INDEPENDENT_CODE ON)
set (CMAKE_POSITION_INDEPENDENT_BINARIES ON)
set (CMAKE_VISIBILITY_INLINES_HIDDEN ON)

if (MSVC)
    set (CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
endif ()

# Compile options
if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    set (COMPILE_OPTIONS -msse4.1 -Wextra)
elseif (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    set (COMPILE_OPTIONS -msse4.1 -Wextra)
elseif (MSVC)
    set (COMPILE_OPTIONS /W4 /WX /wd4324)
else ()
    message (WARNING "Unknown compiler!")
endif ()

# Compile definitions
set (COMPILE_DEFINITIONS NRI_USE_VULKAN=1)

if (WIN32)
    set (COMPILE_DEFINITIONS ${COMPILE_DEFINITIONS} WIN32_LEAN_AND_MEAN NOMINMAX _CRT_SECURE_NO_WARNINGS _UNICODE UNICODE _ENFORCE_MATCHING_ALLOCATORS=0)
    set (COMPILE_DEFINITIONS ${COMPILE_DEFINITIONS} NRI_USE_D3D11=1 NRI_USE_D3D12=1)
endif ()

# Shared
file (GLOB SHARED_SOURCE "Source/Shared/*.cpp" "Source/Shared/*.h" "Source/Shared/*.hpp")
source_group ("" FILES ${SHARED_SOURCE})
add_library (NRI_Shared STATIC ${SHARED_SOURCE})
target_include_directories (NRI_Shared PRIVATE "Include" "Source/Shared")
target_compile_definitions (NRI_Shared PRIVATE ${COMPILE_DEFINITIONS})
target_compile_options (NRI_Shared PRIVATE ${COMPILE_OPTIONS})
set_property (TARGET NRI_Shared PROPERTY FOLDER ${PROJECT_NAME})

if (NOT NRI_ENABLE_STREAMLINE_SUPPORT)
    set (INPUT_LIBS_D3D11 ${INPUT_LIB_D3D11} ${INPUT_LIB_DXGI})
    set (INPUT_LIBS_D3D12 ${INPUT_LIB_D3D12} ${INPUT_LIB_DXGI})
endif ()

# D3D11
if (WIN32)
    file (GLOB D3D11_SOURCE "Source/D3D11/*.cpp" "Source/D3D11/*.h" "Source/D3D11/*.hpp")
    source_group ("" FILES ${D3D11_SOURCE})
    file (GLOB D3D11_NVAPI "External/NVAPI/*.h")
    source_group ("External/NVAPI" FILES ${D3D11_NVAPI})
    file (GLOB D3D11_AMDAGS "External/AGS/inc/*.h")
    source_group ("External/AMDAGS" FILES ${D3D11_AMDAGS})
    add_library (NRI_D3D11 STATIC ${D3D11_SOURCE} ${D3D11_NVAPI} ${D3D11_AMDAGS})
    target_include_directories (NRI_D3D11 PRIVATE "Include" "Source/Shared" "External")
    target_compile_definitions (NRI_D3D11 PRIVATE ${COMPILE_DEFINITIONS})
    target_compile_options (NRI_D3D11 PRIVATE ${COMPILE_OPTIONS})
    target_link_libraries (NRI_D3D11 PRIVATE NRI_Shared ${INPUT_LIBS_D3D11} ${INPUT_LIB_DXGUID} ${INPUT_LIB_NVAPI})
    set_property (TARGET NRI_D3D11 PROPERTY FOLDER ${PROJECT_NAME})
endif ()

# D3D12
if (WIN32)
    file (GLOB D3D12_SOURCE "Source/D3D12/*.cpp" "Source/D3D12/*.h" "Source/D3D12/*.hpp")
    source_group ("" FILES ${D3D12_SOURCE})
    add_library (NRI_D3D12 STATIC ${D3D12_SOURCE})
    target_include_directories (NRI_D3D12 PRIVATE "Include" "Source/Shared")
    target_compile_definitions (NRI_D3D12 PRIVATE ${COMPILE_DEFINITIONS})
    target_compile_options (NRI_D3D12 PRIVATE ${COMPILE_OPTIONS})
    target_link_libraries (NRI_D3D12 PRIVATE NRI_Shared ${INPUT_LIBS_D3D12})
    set_property (TARGET NRI_D3D12 PROPERTY FOLDER ${PROJECT_NAME})
endif ()

# VK
file (GLOB VK_SOURCE "Source/VK/*.cpp" "Source/VK/*.h" "Source/VK/*.hpp")
source_group ("" FILES ${VK_SOURCE})
add_library (NRI_VK STATIC ${VK_SOURCE})
target_include_directories (NRI_VK PRIVATE "Include" "Source/Shared" "External/vulkan/include")
target_compile_definitions (NRI_VK PRIVATE ${COMPILE_DEFINITIONS})
target_compile_options (NRI_VK PRIVATE ${COMPILE_OPTIONS})
target_link_libraries (NRI_VK PRIVATE NRI_Shared)
set_property (TARGET NRI_VK PROPERTY FOLDER ${PROJECT_NAME})

if (WIN32)
    target_compile_definitions (NRI_VK PRIVATE VK_USE_PLATFORM_WIN32_KHR)
elseif (APPLE)
    target_compile_definitions (NRI_VK PRIVATE VK_USE_PLATFORM_METAL_EXT)
else ()
    if (NOT NRI_DISABLE_XLIB_SUPPORT)
        find_path (XLIB_HEADERS NAMES "X11/Xlib.h")

        if (NOT XLIB_HEADERS)
            message (FATAL_ERROR "Can't find 'X11/xlib.h'.(libx11-dev is not installed?)")
        endif ()

        target_compile_definitions (NRI_VK PRIVATE VK_USE_PLATFORM_XLIB_KHR)
    endif ()

    if (NOT NRI_DISABLE_WAYLAND_SUPPORT)
        find_path (WAYLAND_HEADERS NAMES "wayland-client.h")

        if (NOT WAYLAND_HEADERS)
            message (FATAL_ERROR "Can't find 'wayland-client.h'.(libwayland-dev is not installed?)")
        endif ()

        target_compile_definitions (NRI_VK PRIVATE VK_USE_PLATFORM_WAYLAND_KHR)
    endif ()
endif ()

# Validation
file (GLOB NRI_VALIDATION_SOURCE "Source/Validation/*.cpp" "Source/Validation/*.h" "Source/Validation/*.hpp")
source_group ("" FILES ${NRI_VALIDATION_SOURCE})
add_library (NRI_Validation STATIC ${NRI_VALIDATION_SOURCE})
target_include_directories (NRI_Validation PRIVATE "Include" "Source/Shared")
target_compile_definitions (NRI_Validation PRIVATE ${COMPILE_DEFINITIONS})
target_compile_options (NRI_Validation PRIVATE ${COMPILE_OPTIONS})
target_link_libraries (NRI_Validation PRIVATE NRI_Shared)
set_property (TARGET NRI_Validation PROPERTY FOLDER ${PROJECT_NAME})

# NRI
file (GLOB NRI_HEADERS "Include/*.h" "Include/*.hpp")
source_group ("Include" FILES ${NRI_HEADERS})
file (GLOB NRI_EXTENSIONS "Include/Extensions/*.h" "Include/Extensions/*.hpp")
if (NRI_ENABLE_STREAMLINE_SUPPORT)
    file(GLOB NRI_EXTENSIONS ${NRI_EXTENSIONS} "Include/Extensions/Optional/NRIStreamlineWrapper.h")
endif()

source_group ("Include/Extensions" FILES ${NRI_EXTENSIONS})
file (GLOB NRI_SOURCE "Source/Creation/*.cpp" "Source/Creation/*.h")
if (NRI_ENABLE_STREAMLINE_SUPPORT)
    file (GLOB NRI_SOURCE ${NRI_SOURCE} "Source/Streamline/*.cpp")
endif ()

source_group ("Sources" FILES ${NRI_SOURCE})
file (GLOB NRI_RESOURCES "Resources/*")
source_group ("Resources" FILES ${NRI_RESOURCES})

if (NRI_STATIC_LIBRARY)
    add_library (${PROJECT_NAME} STATIC ${NRI_SOURCE} ${NRI_HEADERS} ${NRI_RESOURCES} ${NRI_EXTENSIONS})
else ()
    add_library (${PROJECT_NAME} SHARED ${NRI_SOURCE} ${NRI_HEADERS} ${NRI_RESOURCES} ${NRI_EXTENSIONS})

    if (WIN32)
        target_compile_definitions (${PROJECT_NAME} PRIVATE "NRI_API=extern \"C\" __declspec(dllexport)")
        target_compile_definitions (${PROJECT_NAME} PRIVATE "NRIC_API=extern \"C\" __declspec(dllexport)")
    else ()
        target_compile_definitions (${PROJECT_NAME} PRIVATE "NRI_API=extern \"C\" __attribute__((visibility(\"default\")))")
        target_compile_definitions (${PROJECT_NAME} PRIVATE "NRIC_API=extern \"C\" __attribute__((visibility(\"default\")))")
    endif ()
endif ()

if (NRI_ENABLE_STREAMLINE_SUPPORT)
    set (SL_INCLUDE_DIRECTORY "${NRI_STREAMLINE_SDK_PATH}/include/")
    set(SL_INTERPOSER_LINK_PATH "${NRI_STREAMLINE_SDK_PATH}/lib/x64/sl.interposer.lib")
    if (NRI_USE_DEVELOPMENT_STREAMLINE_LIBRARY)
        set(SL_PLUGINS_DIRECTORY "${NRI_STREAMLINE_SDK_PATH}/bin/x64/development")
    else()
        set(SL_PLUGINS_DIRECTORY "${NRI_STREAMLINE_SDK_PATH}/bin/x64")
    endif()

    target_include_directories(${PROJECT_NAME} PUBLIC ${SL_INCLUDE_DIRECTORY})
    target_link_libraries (${PROJECT_NAME} PRIVATE ${SL_INTERPOSER_LINK_PATH})
    target_compile_definitions (${PROJECT_NAME} PRIVATE SL_PLUGINS_DIRECTORY=${SL_PLUGINS_DIRECTORY})

    file (TO_NATIVE_PATH ${SPARKLE_BIN_DIR}/$<CONFIG> BIN_OUTPUT_PATH_NATIVE)
    file (TO_NATIVE_PATH ${SL_PLUGINS_DIRECTORY} SL_PLUGINS_PATH_NATIVE)

    add_custom_command (TARGET ${PROJECT_NAME} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${BIN_OUTPUT_PATH_NATIVE} ${SL_PLUGINS_PATH_NATIVE}
        COMMENT "Copying '${BIN_OUTPUT_PATH_NATIVE}' to '${SL_PLUGINS_PATH_NATIVE}'"
        VERBATIM
    )
endif ()

target_include_directories (${PROJECT_NAME} PRIVATE "Source/Shared" "External/vulkan/include")
target_include_directories (${PROJECT_NAME} PUBLIC "Include")
target_compile_definitions (${PROJECT_NAME} PRIVATE ${COMPILE_DEFINITIONS})
target_compile_options (${PROJECT_NAME} PRIVATE ${COMPILE_OPTIONS})

target_link_libraries (${PROJECT_NAME} PRIVATE NRI_Shared NRI_VK NRI_Validation)

if (WIN32)
    target_link_libraries (${PROJECT_NAME} PRIVATE NRI_D3D11 NRI_D3D12)
elseif (UNIX)
    target_link_libraries (${PROJECT_NAME} PRIVATE ${CMAKE_DL_LIBS})
endif ()

set_property (TARGET ${PROJECT_NAME} PROPERTY FOLDER ${PROJECT_NAME})

set_target_properties (${PROJECT_NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${IXRAY_SDK_BIN}/$<0:>")
message ("NRI output path: '${GLOBAL_BIN_OUTPUT_PATH}'")
