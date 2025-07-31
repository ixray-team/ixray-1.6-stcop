project(xrRender_DS0)
set(XRDS0_SRC_FILES)

# Files import
file(GLOB_RECURSE XRDSR0_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)

# Source groups
source_group("render/src" FILES ${XRDSR0_SOURCE_ALL_FILES})

# Apply list
list(APPEND XRDS0_SRC_FILES ${XRDSR0_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER XRDS0_SRC_FILES EXCLUDE REGEX "../xrRender/r__pixel_calculator.cpp")

# xrRender_DS0 project
add_library(xrRender_DS0 SHARED ${XRDS0_SRC_FILES})
target_precompile_headers(xrRender_DS0 PRIVATE "stdafx.h")

# Include directories
target_include_directories(xrRender_DS0 PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(xrRender_DS0 PRIVATE xrRender_DS0_EXPORTS)
target_compile_definitions(xrRender_DS0 PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrRender_DS0 PRIVATE xrAbstractions)
target_link_libraries(xrRender_DS0 PRIVATE xrParticles)

## RenderDoc
target_include_directories(xrRender_DS0 PUBLIC "${RENDERDOC_API}")
