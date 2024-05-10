project(xrEProps)

# Files import
file(GLOB_RECURSE EPROPS_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.*"
)

set(EPROPS_SRC_FILES)

source_group("src" FILES ${EPROPS_SOURCE_ALL_FILES})

# Apply list
list(APPEND EPROPS_SRC_FILES ${EPROPS_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER EPROPS_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# xrEProps project
add_library(xrEProps SHARED ${EPROPS_SRC_FILES})
target_include_directories(xrEProps PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(xrEProps PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrEProps PRIVATE XREPROPS_EXPORTS)
target_compile_definitions(xrEProps PRIVATE _WINDOWS)
target_compile_definitions(xrEProps PRIVATE _USRDLL)
target_compile_definitions(xrEProps PUBLIC XR_EDITOR_NEW)

add_compile_options(/fp:fast)
target_compile_definitions(xrEProps PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrEProps PUBLIC xrEUI)

# Move precompiled header
set_target_properties(xrEProps PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

file(GLOB_RECURSE EPROPS_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${EPROPS_SOURCE_PCH_FILES})
