project(lwo)

# Files import
file(GLOB_RECURSE LW_SERVERSOURCE_ALL_FILES
    "*.*"
)

set(LWO_FILES)

source_group("Src" FILES ${LW_SERVERSOURCE_ALL_FILES})

# Apply list
list(APPEND LWO_FILES ${LW_SERVERSOURCE_ALL_FILES})

# Remove unused files
#list(FILTER LWO_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# lwo project
add_library(lwo STATIC ${LWO_FILES})
target_include_directories(lwo PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(lwo PUBLIC "${IXR_LW_SDK}/include")

# Project defines
target_compile_definitions(lwo PRIVATE _WINDOWS)
target_compile_definitions(lwo PUBLIC _MSWIN)
target_compile_definitions(lwo PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(lwo PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list

# Move precompiled header
set_target_properties(lwo PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)