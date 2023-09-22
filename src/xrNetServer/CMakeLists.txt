project(xrNetServer)
set(NETSERV_SRC_FILES)

# Files import
file(GLOB_RECURSE NETSERV_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
)

# Source groups
source_group("kernel" FILES ${NETSERV_SOURCE_ALL_FILES})

# Apply list
list(APPEND NETSERV_SRC_FILES ${NETSERV_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER NETSERV_SRC_FILES EXCLUDE REGEX ".cpp$")

# xrNetServer project
add_library(xrNetServer SHARED ${NETSERV_SRC_FILES})
target_include_directories(xrNetServer PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(xrNetServer PRIVATE XR_NETSERVER_EXPORTS)
target_compile_definitions(xrNetServer PRIVATE _WINDOWS)
target_compile_definitions(xrNetServer PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrNetServer PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrNetServer PUBLIC xrCore)
target_link_libraries(xrNetServer PUBLIC "Ws2_32.lib")

# Move precompiled header
set_target_properties(xrNetServer PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
set_target_properties(xrNetServer PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
target_precompile_headers(xrNetServer PRIVATE "stdafx.h")

file(GLOB_RECURSE NETSERV_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${NETSERV_SOURCE_PCH_FILES})
