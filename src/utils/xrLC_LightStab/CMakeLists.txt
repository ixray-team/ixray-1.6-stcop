project(xrLC_LightStab)
set(LCLS_SRC_FILES)

# Files import
file(GLOB_RECURSE LCLS_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
)

# Source groups
source_group("kernel" FILES ${LCLS_SOURCE_ALL_FILES})

# Apply list
list(APPEND LCLS_SRC_FILES ${LCLS_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER LCLS_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

# xrLC_LightStab project
add_library(xrLC_LightStab STATIC ${LCLS_SRC_FILES})

target_include_directories(xrLC_LightStab PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrLC_LightStab PUBLIC "${IXRAY_SDK_INC}/")

# Project defines
target_compile_definitions(xrLC_LightStab PRIVATE _WINDOWS)
target_compile_definitions(xrLC_LightStab PRIVATE _USE_MATH_DEFINES)
target_compile_definitions(xrLC_LightStab PRIVATE _WINDOWS)

add_compile_options(/fp:fast)
target_compile_definitions(xrLC_LightStab PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrLC_LightStab PUBLIC xrCore)
