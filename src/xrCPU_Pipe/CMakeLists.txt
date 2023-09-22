project(xrCPU_Pipe)
set(CPIPE_SRC_FILES)

# Files import
file(GLOB_RECURSE CPIPE_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
)

# File groups
source_group("core" FILES ${CPIPE_SOURCE_ALL_FILES})

# Apply list
list(APPEND CPIPE_SRC_FILES ${CPIPE_SOURCE_ALL_FILES})

# Remove files
#list(FILTER CPIPE_SRC_FILES EXCLUDE REGEX "")

# xrCPU_Pipe project
add_library(xrCPU_Pipe SHARED ${CPIPE_SRC_FILES})
target_include_directories(xrCPU_Pipe PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrCPU_Pipe PUBLIC "${IXRAY_SDK_INC}")

# Project defines
target_compile_definitions(xrCPU_Pipe PRIVATE XRCPU_PIPE_EXPORTS)

add_compile_options(/fp:fast)
target_compile_definitions(xrCPU_Pipe PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrCPU_Pipe PUBLIC xrCore)

# Move precompiled header
set_target_properties(xrCPU_Pipe PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
set_target_properties(xrCPU_Pipe PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
target_precompile_headers(xrCPU_Pipe PRIVATE "stdafx.h")

file(GLOB_RECURSE CPIPE_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${CPIPE_SOURCE_PCH_FILES})
