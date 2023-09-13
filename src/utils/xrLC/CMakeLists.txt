project(xrLC)
set(XLC_SRC_FILES)

# Files import
file(GLOB_RECURSE XLC_SOURCE_ALL_FILES
    "*.cpp"
	"*.h"
    "../xrDXT/tga.cpp"
    "../../xrEngine/xrLoadSurface.cpp"
    "../../common/NvMender2003/NVMeshMender.cpp"
)

# Source groups
source_group("kernel" FILES ${XLC_SOURCE_ALL_FILES})

# Apply list
list(APPEND XLC_SRC_FILES ${XLC_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER XLC_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

# xrLC project
add_executable(xrLC WIN32 ${XLC_SRC_FILES})
target_include_directories(xrLC PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(xrLC PRIVATE "stdafx.h")

# project defines 
target_compile_definitions(xrLC PRIVATE _WINDOWS)
target_compile_definitions(xrLC PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrLC PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrLC PUBLIC xrCore)
target_link_libraries(xrLC PUBLIC xrCDB)
target_link_libraries(xrLC PUBLIC xrLC_Light)
target_link_libraries(xrLC PUBLIC QSlim)
target_link_libraries(xrLC PUBLIC ETools)