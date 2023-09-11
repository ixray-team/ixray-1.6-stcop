project(xrDO_Light)
set(DOL_SRC_FILES)

# Files import
file(GLOB_RECURSE DOL_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
)

# Source groups
source_group("kernel" FILES ${DOL_SOURCE_ALL_FILES})

# Apply list
list(APPEND DOL_SRC_FILES ${DOL_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER DOL_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

# xrDO_Light project
add_executable(xrDO_Light WIN32 ${DOL_SRC_FILES})
target_include_directories(xrDO_Light PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(xrDO_Light PUBLIC "stdafx.h")

# Project defines
target_compile_definitions(xrDO_Light PRIVATE XRCDB_EXPORTS)
target_compile_definitions(xrDO_Light PRIVATE _WINDOWS)
target_compile_definitions(xrDO_Light PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrDO_Light PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrDO_Light PUBLIC xrCore)
target_link_libraries(xrDO_Light PUBLIC xrCDB)
target_link_libraries(xrDO_Light PUBLIC xrLC_Light)
