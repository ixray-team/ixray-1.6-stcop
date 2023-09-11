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
target_precompile_headers(xrCompressor PRIVATE "stdafx.h")

# project defines 
target_compile_definitions(xrCompressor PRIVATE _WINDOWS)
target_compile_definitions(xrCompressor PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrCompressor PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrCompressor PUBLIC xrCore)
target_link_libraries(xrCompressor PRIVATE "winmm.lib")