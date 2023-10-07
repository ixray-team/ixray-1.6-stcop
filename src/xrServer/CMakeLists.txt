project(xrServer)
set(XRDS_SRC_FILES)

# Files import
file(GLOB_RECURSE XRDS_SOURCE_ALL_FILES
    "*.*"
)

# File groups
source_group("core" FILES ${XRDS_SOURCE_ALL_FILES})

# Apply list
list(APPEND XRDS_SRC_FILES ${XRDS_SOURCE_ALL_FILES})

# Remove files
#list(FILTER CPIPE_SRC_FILES EXCLUDE REGEX "")

# xrCPU_Pipe project
add_executable(xrServer WIN32 ${XRDS_SOURCE_ALL_FILES})
target_include_directories(xrServer PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrServer PRIVATE "${IXRAY_SDK_INC}")

# Precompiled header
target_precompile_headers(xrServer PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrServer PRIVATE XRSERVER_EXPORT)

add_compile_options(/fp:fast)
target_compile_definitions(xrServer PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrServer PUBLIC xrAbstractions)
