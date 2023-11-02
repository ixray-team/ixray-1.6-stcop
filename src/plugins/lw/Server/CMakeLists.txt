project(xrLW_Server)

# Files import
file(GLOB_RECURSE LW_SERVERSOURCE_ALL_FILES
    "*.*"
)

set(LW_SERVERSRC_FILES)

source_group("Src" FILES ${LW_SERVERSOURCE_ALL_FILES})

# Apply list
list(APPEND LW_SERVERSRC_FILES ${LW_SERVERSOURCE_ALL_FILES})

# Remove unused files
#list(FILTER LW_SERVERSRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# xrLW_Server project
add_library(xrLW_Server STATIC ${LW_SERVERSRC_FILES})
target_include_directories(xrLW_Server PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrLW_Server PUBLIC "${IXR_LW_SDK}")

# Project defines
target_compile_definitions(xrLW_Server PRIVATE _WINDOWS)
target_compile_definitions(xrLW_Server PUBLIC _MSWIN)
target_compile_definitions(xrLW_Server PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrLW_Server PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list

# Move precompiled header
set_target_properties(xrLW_Server PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
