project(mp_screenshots_info)
set(MPSI_SRC_FILES)

# Files import
file(GLOB_RECURSE MPSI_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
)

# Source groups
source_group("kernel" FILES ${MPSI_SOURCE_ALL_FILES})

# Apply list
list(APPEND MPSI_SRC_FILES ${MPSI_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER MPSI_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

# mp_screenshots_info project
add_executable(mp_screenshots_info ${MPSI_SRC_FILES})
target_include_directories(mp_screenshots_info PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(mp_screenshots_info PRIVATE MPSI_EXPORTS)
target_compile_definitions(mp_screenshots_info PRIVATE _WINDOWS)

add_compile_options(/fp:fast)
target_compile_definitions(mp_screenshots_info PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(mp_screenshots_info PUBLIC xrCore)
target_link_libraries(mp_screenshots_info PUBLIC crypto)
