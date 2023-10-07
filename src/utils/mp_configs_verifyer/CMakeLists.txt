project(mp_configs_verifyer)
set(MPCF_SRC_FILES)

# Files import
file(GLOB_RECURSE MPCF_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
)

# Source groups
source_group("kernel" FILES ${MPCF_SOURCE_ALL_FILES})

# Apply list
list(APPEND MPCF_SRC_FILES ${MPCF_SOURCE_ALL_FILES})

# mp_configs_verifyer project
add_executable(mp_configs_verifyer ${MPCF_SRC_FILES})
target_include_directories(mp_configs_verifyer PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(mp_configs_verifyer PRIVATE MPCF_EXPORTS)
target_compile_definitions(mp_configs_verifyer PRIVATE _WINDOWS)

add_compile_options(/fp:fast)
target_compile_definitions(mp_configs_verifyer PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(mp_configs_verifyer PUBLIC xrCore)
target_link_libraries(mp_configs_verifyer PUBLIC crypto)
