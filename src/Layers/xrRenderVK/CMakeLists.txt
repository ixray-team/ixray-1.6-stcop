project(xrRender_VK)
set(XRVK_SRC_FILES)

# Files import
file(GLOB_RECURSE XRVK_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)

# Exclude test files from main build
list(FILTER XRVK_SOURCE_ALL_FILES EXCLUDE REGEX "test_.*")

file(GLOB_RECURSE XRVK_SOURCE_COMP_FILES CONFIGURE_DEPENDS
    "../xrRender/*.cpp"
    "../xrRender/*.h"
)
file(GLOB_RECURSE XRVK_SOURCE_INCL_FILES CONFIGURE_DEPENDS
    "../../Include/*.h"
)

# Source groups
source_group("render/src" FILES ${XRVK_SOURCE_ALL_FILES})
source_group("render/include" FILES ${XRVK_SOURCE_INCL_FILES})
source_group("layers" FILES ${XRVK_SOURCE_COMP_FILES})

# Apply list
list(APPEND XRVK_SRC_FILES ${XRVK_SOURCE_ALL_FILES})
list(APPEND XRVK_SRC_FILES ${XRVK_SOURCE_COMP_FILES})
list(APPEND XRVK_SRC_FILES ${XRVK_SOURCE_INCL_FILES})

# xrRender_VK project
add_library(xrRender_VK SHARED ${XRVK_SRC_FILES})
target_precompile_headers(xrRender_VK PRIVATE "stdafx.h")

# Include directories
target_include_directories(xrRender_VK PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrRender_VK PUBLIC "${IXRAY_SDK_INC}")

# Find Vulkan (robust)
# Try environment variable first
if(DEFINED ENV{VULKAN_SDK})
    set(_VULKAN_SDK $ENV{VULKAN_SDK})
    if (EXISTS "${_VULKAN_SDK}/Include/vulkan/vulkan.h")
        list(APPEND CMAKE_PREFIX_PATH "${_VULKAN_SDK}")
        message(STATUS "Vulkan SDK detected via VULKAN_SDK=${_VULKAN_SDK}")
    endif()
endif()

# Common Windows fallback paths (GitHub Actions + default install)
if (WIN32 AND NOT DEFINED _VULKAN_SDK)
    file(GLOB _vk_candidates "C:/VulkanSDK/*")
    list(SORT _vk_candidates DESCENDING)
    list(GET _vk_candidates 0 _vk_latest_dir)
    if(_vk_latest_dir AND EXISTS "${_vk_latest_dir}/Include/vulkan/vulkan.h")
        set(_VULKAN_SDK ${_vk_latest_dir})
        list(APPEND CMAKE_PREFIX_PATH "${_VULKAN_SDK}")
        message(STATUS "Vulkan SDK detected at ${_VULKAN_SDK}")
    endif()
endif()

find_package(Vulkan QUIET)

if (NOT Vulkan_FOUND AND _VULKAN_SDK)
    # Manual include/lib setup if module failed but headers exist
    set(Vulkan_INCLUDE_DIR "${_VULKAN_SDK}/Include")
    if (WIN32)
        # Prefer static libs (lib) or import libs
        if (EXISTS "${_VULKAN_SDK}/Lib/vulkan-1.lib")
            set(Vulkan_LIBRARY "${_VULKAN_SDK}/Lib/vulkan-1.lib")
        elseif(EXISTS "${_VULKAN_SDK}/Lib/vulkan-1.dll")
            set(Vulkan_LIBRARY "${_VULKAN_SDK}/Lib/vulkan-1.dll")
        endif()
    else()
        if (EXISTS "${_VULKAN_SDK}/lib/libvulkan.so")
            set(Vulkan_LIBRARY "${_VULKAN_SDK}/lib/libvulkan.so")
        elseif (EXISTS "${_VULKAN_SDK}/lib64/libvulkan.so")
            set(Vulkan_LIBRARY "${_VULKAN_SDK}/lib64/libvulkan.so")
        endif()
    endif()
    if (Vulkan_INCLUDE_DIR AND Vulkan_LIBRARY)
        set(Vulkan_FOUND TRUE)
        set(Vulkan_INCLUDE_DIRS ${Vulkan_INCLUDE_DIR})
        set(Vulkan_LIBRARIES ${Vulkan_LIBRARY})
        message(WARNING "Vulkan module not found, using manual paths: include=${Vulkan_INCLUDE_DIR} lib=${Vulkan_LIBRARY}")
    endif()
endif()

if (NOT Vulkan_FOUND)
    # Fallback: try system loader discovery (Windows implicit, Linux via system package)
    find_path(Vulkan_INCLUDE_DIR_FALLBACK NAMES vulkan/vulkan.h)
    find_library(Vulkan_LIBRARY_FALLBACK NAMES vulkan vulkan-1)
    if (Vulkan_INCLUDE_DIR_FALLBACK AND Vulkan_LIBRARY_FALLBACK)
        set(Vulkan_FOUND TRUE)
        set(Vulkan_INCLUDE_DIRS ${Vulkan_INCLUDE_DIR_FALLBACK})
        set(Vulkan_LIBRARIES ${Vulkan_LIBRARY_FALLBACK})
        message(STATUS "Vulkan system fallback located: include=${Vulkan_INCLUDE_DIR_FALLBACK} lib=${Vulkan_LIBRARY_FALLBACK}")
    endif()
endif()

if (NOT Vulkan_FOUND)
    # Final ultra-light fallback: headers only (will rely on dynamic loader at runtime)
    find_path(Vulkan_INCLUDE_DIR_HEADERS_ONLY NAMES vulkan/vulkan.h)
    if (Vulkan_INCLUDE_DIR_HEADERS_ONLY)
        set(Vulkan_INCLUDE_DIRS ${Vulkan_INCLUDE_DIR_HEADERS_ONLY})
        set(Vulkan_LIBRARIES "")
        target_compile_definitions(xrRender_VK PRIVATE USE_VULKAN_HEADERS_ONLY)
        message(WARNING "Using Vulkan headers-only fallback (no linked loader). Runtime may fail if loader not present.")
        set(Vulkan_FOUND TRUE)
    endif()
endif()

if (NOT Vulkan_FOUND)
    message(FATAL_ERROR "Vulkan SDK not found. Provide VULKAN_SDK env path (e.g. /path/to/VulkanSDK/<ver>/x86_64) or install system packages (e.g. 'sudo apt-get install -y libvulkan-dev') or download from https://vulkan.lunarg.com/sdk/home")
endif()

message(STATUS "Vulkan include dirs: ${Vulkan_INCLUDE_DIRS}")
message(STATUS "Vulkan libraries: ${Vulkan_LIBRARIES}")

target_include_directories(xrRender_VK PUBLIC ${Vulkan_INCLUDE_DIRS})

# Project defines
target_compile_definitions(xrRender_VK PRIVATE XRRENDER_VK_EXPORTS)
target_compile_definitions(xrRender_VK PRIVATE USE_VULKAN)
# Force fallback (no D3D) only on non-Windows platforms; keep D3D available on Windows to avoid disabling DX for users.
if(NOT WIN32 OR IXRAY_VK_STRICT_NO_D3D)
    target_compile_definitions(xrRender_VK PRIVATE XR_FORCE_NO_D3D)
endif()
target_compile_definitions(xrRender_VK PRIVATE _USRDLL)
target_compile_definitions(xrRender_VK PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrRender_VK PUBLIC xrAbstractions)
target_link_libraries(xrRender_VK PUBLIC xrParticles)
target_link_libraries(xrRender_VK PUBLIC ${Vulkan_LIBRARIES})