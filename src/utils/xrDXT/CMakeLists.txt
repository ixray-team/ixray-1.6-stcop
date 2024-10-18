project(xrDXT)
set(DXT_SRC_FILES)

#set(USE_NVTT ON)

# Files import
file(GLOB_RECURSE DXT_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
    "*.hpp"
)

file(GLOB_RECURSE DXT_SOURCE_NVTT_FILES CONFIGURE_DEPENDS "nvtt/*.*")
file(GLOB_RECURSE DXT_SOURCE_RI_FILES CONFIGURE_DEPENDS "RedImage/*.*")

# Source groups
source_group("kernel" FILES ${DXT_SOURCE_ALL_FILES})
source_group("backends/nvtt" FILES ${DXT_SOURCE_NVTT_FILES})
source_group("backends/redimage" FILES ${DXT_SOURCE_RI_FILES})

# Apply list
list(APPEND DXT_SRC_FILES ${DXT_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER DXT_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

#if (USE_NVTT)
#    list(FILTER DXT_SRC_FILES EXCLUDE REGEX "RedImage/*.*")
#else()
#    list(FILTER DXT_SRC_FILES EXCLUDE REGEX "nvtt/*.*")
#endif()

# xrDXT project
add_library(xrDXT SHARED ${DXT_SRC_FILES})
target_include_directories(xrDXT PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_link_directories(xrDXT PUBLIC "${IXRAY_SDK_LIB}/")
target_precompile_headers(xrDXT PUBLIC "stdafx.h")

# Project defines
target_compile_definitions(xrDXT PRIVATE DXT_EXPORTS)
target_compile_definitions(xrDXT PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrDXT PUBLIC xrCore)
target_link_libraries(xrDXT PUBLIC xrCDB)

# NVTT
target_link_libraries(xrDXT PRIVATE ${NVTT}runtimes/win-x64/native/Release/nvtt.lib)
target_include_directories(xrDXT PRIVATE "${NVTT}build/native/include/")

## Copy binaries
add_custom_command(TARGET ${PROJECT_NAME} 
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${NVTT}runtimes/win-x64/native/Release/nvtt.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)

# RedImage
target_link_libraries(xrDXT PRIVATE ${REDIMAGE_PATH}/runtimes/win-x64/native/Release/RedImageTool.lib)
target_include_directories(xrDXT PRIVATE "${REDIMAGE_PATH}/build/native/include/")

## Copy binaries
add_custom_command(TARGET ${PROJECT_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${REDIMAGE_PATH}/runtimes/win-x64/native/Release/RedImageTool.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)


#target_include_directories(xrDXT PUBLIC "${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2024.6.5.1/include/")
#target_link_libraries(xrDXT PUBLIC ${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2024.6.5.1/native/lib/${NUGET_PACKAGE_PLATFORM}/release/DirectXTex.lib)
