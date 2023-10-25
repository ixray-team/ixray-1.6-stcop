project(xrDXT)
set(DXT_SRC_FILES)

# Files import
file(GLOB_RECURSE DXT_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
    "*.hpp"
)

# Source groups
source_group("kernel" FILES ${DXT_SOURCE_ALL_FILES})

# Apply list
list(APPEND DXT_SRC_FILES ${DXT_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER DXT_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

# xrDXT project
add_library(xrDXT SHARED ${DXT_SRC_FILES})
target_include_directories(xrDXT PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_link_directories(xrDXT PUBLIC "${IXRAY_SDK_LIB}/")
target_precompile_headers(xrDXT PUBLIC "stdafx.h")

# Project defines
target_compile_definitions(xrDXT PRIVATE DXT_EXPORTS)
target_compile_definitions(xrDXT PRIVATE _WINDOWS)
target_compile_definitions(xrDXT PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrDXT PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrDXT PUBLIC xrCore)
target_link_libraries(xrDXT PUBLIC xrCDB)
target_link_libraries(xrDXT PRIVATE "nvtt.lib")

# Nuget
add_custom_command(TARGET ${PROJECT_NAME}
    PRE_BUILD
    COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/Packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
)

# nvtt
add_custom_command(TARGET ${PROJECT_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXRAY_SDK_BIN}nvtt.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)

## DirectX SDK
target_include_directories(xrDXT PUBLIC "${CMAKE_BINARY_DIR}/packages/directxtex_desktop_win10.2023.6.14.1/include/")
if(NOT "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
    target_link_libraries(xrDXT PUBLIC ${CMAKE_BINARY_DIR}/packages/directxtex_desktop_win10.2023.6.14.1/native/lib/x86/release/DirectXTex.lib)
else()
    target_link_libraries(xrDXT PUBLIC ${CMAKE_BINARY_DIR}/packages/directxtex_desktop_win10.2023.6.14.1/native/lib/x64/release/DirectXTex.lib)
endif()

