project(EditorProps)
set(ED_PROPS_SRC_FILES)

# Files import
file(GLOB_RECURSE ED_PROPS_SOURCE_SRC_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)

source_group("src" FILES ${ED_PROPS_SOURCE_SRC_FILES})

# Apply list
list(APPEND ED_PROPS_SRC_FILES ${ED_PROPS_SOURCE_ALL_FILES})
list(APPEND ED_PROPS_SRC_FILES ${ED_PROPS_SOURCE_SRC_FILES})

# Remove unused files
list(FILTER ED_PROPS_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# EditorProps project
add_library(EditorProps ${ED_PROPS_SRC_FILES})
target_include_directories(EditorProps PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# Linker list
target_link_libraries(EditorProps PUBLIC EditorEngineAPI)