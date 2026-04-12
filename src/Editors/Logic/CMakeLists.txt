project(LogicEditor)

# Files import
file(GLOB_RECURSE LGCE_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.*"
)

set(LGCE_SRC_FILES)

source_group("src" FILES ${LGCE_SOURCE_ALL_FILES})

# Apply list
list(APPEND LGCE_SRC_FILES ${LGCE_SOURCE_ALL_FILES})

# LogicEditor project
add_executable(LogicEditor WIN32 ${LGCE_SRC_FILES})
target_include_directories(LogicEditor PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_compile_definitions(LogicEditor PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(LogicEditor PUBLIC xrCore)
target_link_libraries(LogicEditor PUBLIC xrEUI)
target_link_libraries(LogicEditor PUBLIC xrEProps)
target_link_libraries(LogicEditor PUBLIC imgui_node_editor)

# Move precompiled header
set_target_properties(LogicEditor PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)