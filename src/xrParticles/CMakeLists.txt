project(xrParticles)
set(XRP_SRC_FILES)

# Files import
file(GLOB_RECURSE XRP_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
    "*.c"
)

# Source groups
source_group("core" FILES ${XRP_SOURCE_ALL_FILES})

# Apply list
list(APPEND XRP_SRC_FILES ${XRP_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER XRP_SRC_FILES EXCLUDE REGEX ".*")

# xrParticles project
add_library(xrParticles SHARED ${XRP_SRC_FILES})
target_precompile_headers(xrParticles PUBLIC "stdafx.h")

# Include directories
target_include_directories(xrParticles PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrParticles PUBLIC "${IXRAY_SDK_INC}")

# Project defines
target_compile_definitions(xrParticles PRIVATE XR_PARTICLES_EXPORTS)
target_compile_definitions(xrParticles PRIVATE _WINDOWS)
target_compile_definitions(xrParticles PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrParticles PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrParticles PUBLIC xrCore)
target_link_libraries(xrParticles PUBLIC xrCDB)
target_link_libraries(xrParticles PUBLIC xrCPU_Pipe)
