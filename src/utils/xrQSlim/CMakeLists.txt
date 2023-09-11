project(QSlim)
set(QSL_SRC_FILES)

# Files import
file(GLOB_RECURSE QSL_SOURCE_ALL_FILES
    "src/*.cpp"
    "src/*.h"
)

# Source groups
source_group("kernel" FILES ${QSL_SOURCE_ALL_FILES})

# Apply list
list(APPEND QSL_SRC_FILES ${QSL_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER QSL_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

# QSlim project
add_library(QSlim STATIC ${QSL_SRC_FILES})
target_include_directories(QSlim PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(QSlim PRIVATE _WINDOWS)

add_compile_options(/fp:fast)
target_compile_definitions(QSlim PRIVATE "$<$<CONFIG:Debug>:DEBUG>")
