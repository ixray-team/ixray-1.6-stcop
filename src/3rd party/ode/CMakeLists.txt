project(ODE)
set(ODE_SRC_FILES)

# Files import
file(GLOB ODE_SOURCE_ALL_FILES
    "ode/src/*.cpp"
    "ode/src/*.c"
    "include/ode/*.h"
)

# Source groups
source_group("kernel" FILES ${ODE_SOURCE_ALL_FILES})

# Apply list
list(APPEND ODE_SRC_FILES ${ODE_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER ODE_SRC_FILES EXCLUDE REGEX ".cpp$")

# ODE project
add_library(ODE STATIC ${ODE_SRC_FILES})

# Include directories
target_include_directories(ODE PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(ODE PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/include/")
target_include_directories(ODE PUBLIC "${IXRAY_SDK_INC}")

# Project defines
target_compile_definitions(ODE PRIVATE _WINDOWS)
target_compile_definitions(ODE PRIVATE dSINGLE)
target_compile_definitions(ODE PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(ODE PRIVATE "$<$<CONFIG:Debug>:DEBUG>")
