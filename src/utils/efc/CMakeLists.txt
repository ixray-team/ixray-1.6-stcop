project(EFC)
set(EFC_SRC_FILES)

# Files import
file(GLOB_RECURSE EFC_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
)

# Source groups
source_group("kernel" FILES ${EFC_SOURCE_ALL_FILES})

# Apply list
list(APPEND EFC_SRC_FILES ${EFC_SOURCE_ALL_FILES})

# EFC project
add_executable(EFC ${EFC_SRC_FILES})
target_include_directories(EFC PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(EFC PRIVATE "stdafx.h")
add_compile_options(/fp:fast)

# Project defines
target_compile_definitions(EFC PRIVATE "$<$<CONFIG:Debug>:DEBUG>")