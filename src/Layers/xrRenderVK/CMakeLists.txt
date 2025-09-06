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

# Find Vulkan
find_package(Vulkan REQUIRED)
if (NOT Vulkan_FOUND)
    message(FATAL_ERROR "Vulkan SDK not found. Please install Vulkan SDK from https://vulkan.lunarg.com/sdk/home")
endif()
target_include_directories(xrRender_VK PUBLIC ${Vulkan_INCLUDE_DIRS})

# Project defines
target_compile_definitions(xrRender_VK PRIVATE XRRENDER_VK_EXPORTS)
target_compile_definitions(xrRender_VK PRIVATE USE_VULKAN)
target_compile_definitions(xrRender_VK PRIVATE _USRDLL)
target_compile_definitions(xrRender_VK PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrRender_VK PUBLIC xrAbstractions)
target_link_libraries(xrRender_VK PUBLIC xrParticles)
target_link_libraries(xrRender_VK PUBLIC ${Vulkan_LIBRARIES})