project(xrCPU_Pipe)
set(CPIPE_SRC_FILES)

# Files import
file(GLOB_RECURSE CPIPE_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
	"*.h"
)

# File groups
source_group("core" FILES ${CPIPE_SOURCE_ALL_FILES})

# Apply list
list(APPEND CPIPE_SRC_FILES ${CPIPE_SOURCE_ALL_FILES})

# Remove borlands files
#list(FILTER CPIPE_SRC_FILES EXCLUDE REGEX "")

# xrCPU_Pipe project
add_library(xrCPU_Pipe SHARED ${CPIPE_SRC_FILES})
target_include_directories(xrCPU_Pipe PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrCPU_Pipe PUBLIC "${IXRAY_SDK_INC}")

# PCH
target_precompile_headers(xrCPU_Pipe PUBLIC "stdafx.h")

# project defines 
target_compile_definitions(xrCPU_Pipe PRIVATE XRCPU_PIPE_EXPORTS)

add_compile_options(/fp:fast)
target_compile_definitions(xrCPU_Pipe PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrCPU_Pipe PUBLIC xrCore)