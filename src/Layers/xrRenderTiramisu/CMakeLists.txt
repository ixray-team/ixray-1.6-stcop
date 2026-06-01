project(xrRenderTiramisu)
set(XRDS0_SRC_FILES)

# Files import
file(GLOB_RECURSE XRDSR0_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)

# Source groups
source_group("render/src" FILES ${XRDSR0_SOURCE_ALL_FILES})

# Apply list
list(APPEND XRDS0_SRC_FILES ${XRDSR0_SOURCE_ALL_FILES})

# xrRenderTiramisu project
add_library(xrRenderTiramisu SHARED ${XRDS0_SRC_FILES})
target_precompile_headers(xrRenderTiramisu PRIVATE "stdafx.h")

# Include directories
target_include_directories(xrRenderTiramisu PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(xrRenderTiramisu PRIVATE xrRenderTiramisu_EXPORTS)
target_compile_definitions(xrRenderTiramisu PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrRenderTiramisu PRIVATE xrCore)
target_link_libraries(xrRenderTiramisu PRIVATE xrEngineCore)

## RenderDoc
target_include_directories(xrRenderTiramisu PUBLIC "${RENDERDOC_API}")

target_link_libraries(xrRenderTiramisu PRIVATE DirectX::ShaderCompiler)
target_link_libraries(xrRenderTiramisu PRIVATE NRI)
target_compile_definitions(xrRenderTiramisu PRIVATE IXR_ENABLE_SHADER_COMPILER)

if (WIN32)
    # RedImage
    target_link_libraries(xrRenderTiramisu PUBLIC ${REDIMAGE_LIB})
    target_include_directories(xrRenderTiramisu PUBLIC "${REDIMAGE_INCL}")

    ## Copy binaries
    add_custom_command(TARGET ${PROJECT_NAME}
            POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${REDIMAGE_BIN} ${IXRAY_OUTPUT}
    )
endif()