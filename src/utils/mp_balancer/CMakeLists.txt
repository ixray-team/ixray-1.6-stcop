project(mp_balancer)
set(MPB_SRC_FILES)

# Files import
file(GLOB_RECURSE MPB_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
)

# Source groups
source_group("kernel" FILES ${MPB_SOURCE_ALL_FILES})

# Apply list
list(APPEND MPB_SRC_FILES ${MPB_SOURCE_ALL_FILES})

# mp_balancer project
add_executable(mp_balancer ${MPB_SRC_FILES})
target_include_directories(mp_balancer PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(mp_balancer PRIVATE MPB_EXPORTS)
target_compile_definitions(mp_balancer PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(mp_balancer PUBLIC xrCore)
