project(EditorCore)
set(ED_CORE_SRC_FILES)

# Files import
file(GLOB_RECURSE ED_CORE_SOURCE_SRC_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)

source_group("src" FILES ${ED_CORE_SOURCE_SRC_FILES})

# Apply list
list(APPEND ED_CORE_SRC_FILES ${ED_CORE_SOURCE_ALL_FILES})
list(APPEND ED_CORE_SRC_FILES ${ED_CORE_SOURCE_SRC_FILES})

# Remove unused files
list(FILTER ED_CORE_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# EditorCore project
add_library(EditorCore ${ED_CORE_SRC_FILES})
target_include_directories(EditorCore PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(EditorCore PRIVATE _EDITOR)
target_compile_definitions(EditorCore PRIVATE dSINGLE)

# Linker list
target_link_libraries(EditorCore PUBLIC EditorRender)
target_link_libraries(EditorCore PUBLIC xrPhysics)