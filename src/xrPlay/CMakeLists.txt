project(xrPlay)
set(XPLAY_SRC_FILES)

# Files import
file(GLOB_RECURSE XPLAY_SOURCE_ALL_FILES
    "*.*"
)
file(GLOB_RECURSE XPLAY_SOURCE_CPP_FILES
    "*.cpp"
    "*.h"
)
file(GLOB_RECURSE XPLAY_SOURCE_RES_FILES
    "*.ico"
    "*.rc"
    "*.bmp"
)

# File groups
source_group("Source Files" FILES ${XPLAY_SOURCE_CPP_FILES})
source_group("Resources" FILES ${XPLAY_SOURCE_RES_FILES})

# Apply list
list(APPEND XPLAY_SRC_FILES ${XPLAY_SOURCE_ALL_FILES})

# Remove files
#list(FILTER CPIPE_SRC_FILES EXCLUDE REGEX "")

# xrPlay project
add_executable(xrPlay WIN32 ${XPLAY_SOURCE_ALL_FILES})
target_include_directories(xrPlay PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrPlay PRIVATE "${IXRAY_SDK_INC}")

# Project defines
#target_compile_definitions(xrPlay PRIVATE XRPLAY_EXPORT)

add_compile_options(/fp:fast)
target_compile_definitions(xrPlay PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrPlay PUBLIC xrEngine)
