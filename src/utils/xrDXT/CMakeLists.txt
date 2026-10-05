project(xrDXT)
set(DXT_SRC_FILES)

# Files import
file(GLOB_RECURSE DXT_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
    "*.hpp"
)

file(GLOB_RECURSE DXT_SOURCE_NVTT_FILES CONFIGURE_DEPENDS "nvtt/*.*")
file(GLOB_RECURSE DXT_SOURCE_RI_FILES CONFIGURE_DEPENDS "RedImage/*.*")
file(GLOB_RECURSE DXT_SOURCE_GPU_FILES CONFIGURE_DEPENDS "GPU/*.*")

# Source groups
source_group("kernel" FILES ${DXT_SOURCE_ALL_FILES})
source_group("backends/nvtt" FILES ${DXT_SOURCE_NVTT_FILES})
source_group("backends/redimage" FILES ${DXT_SOURCE_RI_FILES})
source_group("backends/gpu" FILES ${DXT_SOURCE_GPU_FILES})

# Apply list
list(APPEND DXT_SRC_FILES ${DXT_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER DXT_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

# xrDXT project
add_library(xrDXT SHARED ${DXT_SRC_FILES})
target_include_directories(xrDXT PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_link_directories(xrDXT PUBLIC "${IXRAY_SDK_LIB}/")
target_precompile_headers(xrDXT PRIVATE "StdAfx.h")

# Project defines
target_compile_definitions(xrDXT PRIVATE DXT_EXPORTS)
target_compile_definitions(xrDXT PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrDXT PUBLIC xrEngineCore)
target_link_libraries(xrDXT PUBLIC RedImage::RedImage)
target_link_libraries(xrDXT PRIVATE NVTT::nvtt)

if(WIN32)
    target_link_libraries(xrDXT PRIVATE DirectX::Tex)
    target_link_libraries(xrDXT PRIVATE d3d11.lib)
endif()
