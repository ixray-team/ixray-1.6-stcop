project(xrScripts)
set(XRPS_SRC_FILES)

# Files import
file(GLOB_RECURSE XRPS_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.c"
    "*.h"
)

# Source groups
source_group("core" FILES ${XRPS_SOURCE_ALL_FILES})

# Apply list
list(APPEND XRPS_SRC_FILES ${XRPS_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER XRPS_SRC_FILES EXCLUDE REGEX ".*")

# xrScripts project
add_library(xrScripts SHARED ${XRPS_SRC_FILES})

# Include directories
target_include_directories(xrScripts PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrScripts PUBLIC "${IXRAY_SDK_INC}")

# Project defines
target_compile_definitions(xrScripts PRIVATE XR_SCRIPTS_EXPORTS)
target_compile_definitions(xrScripts PRIVATE XRGAME_EXPORTS)
target_compile_definitions(xrScripts PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrScripts PRIVATE "$<$<CONFIG:Debug>:DEBUG>")
set_target_properties(xrScripts PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

# Linker list
target_link_libraries(xrScripts PUBLIC xrCore)
target_link_libraries(xrScripts PUBLIC xrAbstractions)
