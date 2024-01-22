project(xrRender_OGL)
set(XROGL_SRC_FILES)

# Files import
file(GLOB_RECURSE XROGL_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
)
file(GLOB_RECURSE XROGL_SOURCE_CONFIG_FILES "CMakeLists.txt")

# Source groups
source_group("render/src" FILES ${XROGL_SOURCE_ALL_FILES})

# Apply list
list(APPEND XROGL_SRC_FILES ${XROGL_SOURCE_ALL_FILES})
list(APPEND XROGL_SRC_FILES ${XROGL_SOURCE_CONFIG_FILES})

# Remove unused files
#list(FILTER XROGL_SRC_FILES EXCLUDE REGEX "../xrRender/HW.cpp")

# xrRender_OGL project
add_library(xrRender_OGL SHARED ${XROGL_SRC_FILES})
target_precompile_headers(xrRender_OGL PRIVATE "stdafx.h")

# Include directories
target_include_directories(xrRender_OGL PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrRender_OGL PUBLIC "${IXRAY_SDK_INC}")
target_include_directories(xrRender_OGL PUBLIC "../")

# Project defines
target_compile_definitions(xrRender_OGL PRIVATE XRRENDER_OGL_EXPORTS)
target_compile_definitions(xrRender_OGL PRIVATE USE_OGL)
target_compile_definitions(xrRender_OGL PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrRender_OGL PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrRender_OGL PUBLIC xrAbstractions)
target_link_libraries(xrRender_OGL PUBLIC xrParticles)
target_link_libraries(xrRender_OGL PUBLIC glew)

# Semi gl jokes
target_compile_options(xrRender_OGL PRIVATE "/wd4101")
target_compile_options(xrRender_OGL PRIVATE "/wd4267")
target_compile_options(xrRender_OGL PRIVATE "/wd4838")

# NuGet
target_include_directories(xrRender_OGL PUBLIC "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_win10.2023.4.28.1/include/")