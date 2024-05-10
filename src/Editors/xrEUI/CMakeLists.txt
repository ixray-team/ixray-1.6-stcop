project(xrEUI)

# Files import
file(GLOB_RECURSE EUI_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.*"
)

set(EUI_SRC_FILES)

source_group("src" FILES ${EUI_SOURCE_ALL_FILES})

# Apply list
list(APPEND EUI_SRC_FILES ${EUI_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER EUI_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# xrEUI project
add_library(xrEUI SHARED ${EUI_SRC_FILES})
target_include_directories(xrEUI PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(xrEUI PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrEUI PRIVATE XREUI_EXPORTS)
target_compile_definitions(xrEUI PRIVATE _WINDOWS)
target_compile_definitions(xrEUI PRIVATE _USRDLL)
target_compile_definitions(xrEUI PUBLIC XR_EDITOR_NEW)

add_compile_options(/fp:fast)
target_compile_definitions(xrEUI PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrEUI PUBLIC xrCore)
#target_link_libraries(xrEUI PUBLIC imgui)

# Move precompiled header
set_target_properties(xrEUI PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

file(GLOB_RECURSE EUI_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${EUI_SOURCE_PCH_FILES})
