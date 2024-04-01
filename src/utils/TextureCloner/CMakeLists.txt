project(TextureCloner)
set(TC_SRC_FILES)

# Files import
file(GLOB_RECURSE TC_SOURCE_ALL_FILES
    "../../Layers/xrRender/ETextureParams.*"
    "*.cpp"
    "*.h"
)

# Source groups
source_group("kernel" FILES ${TC_SOURCE_ALL_FILES})

# Apply list
list(APPEND TC_SRC_FILES ${TC_SOURCE_ALL_FILES})

# TextureCloner project
add_executable(TextureCloner ${TC_SRC_FILES})
target_include_directories(TextureCloner PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(TextureCloner PRIVATE TC_EXPORTS)

# Linker list
target_link_libraries(TextureCloner PUBLIC xrCore)
