project(xrCompressor)
set(COMPR_SRC_FILES)

# Files import
file(GLOB_RECURSE COMPR_SOURCE_ALL_FILES
    "*.cpp"
    "*.c"
    "*.h"
)
file(GLOB_RECURSE COMPR_SOURCE_LZO_FILES
    "lzo/*.*"
)

# Source groups
source_group("kernel" FILES ${COMPR_SOURCE_ALL_FILES})
source_group("lzo" FILES ${COMPR_SOURCE_LZO_FILES})

# Apply list
list(APPEND COMPR_SRC_FILES ${COMPR_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER COMPR_SRC_FILES EXCLUDE REGEX "lzo/*.ch")

# xrCompressor project
add_executable(xrCompressor ${COMPR_SRC_FILES})
target_include_directories(xrCompressor PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# Move precompiled header
set_target_properties(xrCompressor PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
set_target_properties(xrCompressor PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
target_precompile_headers(xrCompressor PUBLIC "stdafx.h")

file(GLOB_RECURSE COMPRESSOR_PCH_FILES "stdafx.*")
file(GLOB_RECURSE COMPRESSOR_SOURCE_ALL_C_FILES "*.c")

set_source_files_properties(${COMPRESSOR_SOURCE_ALL_C_FILES} PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
source_group("pch" FILES ${COMPRESSOR_PCH_FILES})

# Project defines
target_compile_definitions(xrCompressor PRIVATE _WINDOWS)
target_compile_definitions(xrCompressor PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrCompressor PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrCompressor PUBLIC xrCore)
target_link_libraries(xrCompressor PRIVATE "winmm.lib")
