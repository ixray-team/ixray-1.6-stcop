project(xrRenderInterface)
set(XR_RHI_SRC_FILES)

# Files import
file(GLOB XR_RHI_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)
file(GLOB XR_RHI_SOURCE_DX11_FILES CONFIGURE_DEPENDS
    "Dx11/*.*"
)
file(GLOB XR_RHI_SOURCE_DXGI_FILES CONFIGURE_DEPENDS
    "DxGI/*.*"
)

# Source groups
source_group("render/src" FILES ${XR_RHI_SOURCE_ALL_FILES})
source_group("render/dx11" FILES ${XR_RHI_SOURCE_DX11_FILES})
source_group("render/dxgi" FILES ${XR_RHI_SOURCE_DXGI_FILES})

# Apply list
list(APPEND XR_RHI_SRC_FILES ${XR_RHI_SOURCE_ALL_FILES})
list(APPEND XR_RHI_SRC_FILES ${XR_RHI_SOURCE_DX11_FILES})
list(APPEND XR_RHI_SRC_FILES ${XR_RHI_SOURCE_DXGI_FILES})

# xrRenderInterface project
add_library(xrRenderInterface SHARED ${XR_RHI_SRC_FILES})
target_precompile_headers(xrRenderInterface PRIVATE "stdafx.h")

# Include directories
target_include_directories(xrRenderInterface PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(xrRenderInterface PRIVATE xrRenderInterface_EXPORTS)
target_compile_definitions(xrRenderInterface PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrRenderInterface PRIVATE xrCore)
target_link_libraries(xrRenderInterface PRIVATE "d3dcompiler.lib")
target_link_libraries(xrRenderInterface PRIVATE "d3d11.lib")
target_link_libraries(xrRenderInterface PRIVATE "dxgi.lib")
target_link_libraries(xrRenderInterface PRIVATE "dxguid.lib")

## DirectX SDK
#target_include_directories(xrRenderInterface PUBLIC "${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2023.10.31.1/include/")
#target_link_libraries(xrRenderInterface PUBLIC ${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2023.10.31.1/native/lib/${NUGET_PACKAGE_PLATFORM}/release/DirectXTex.lib)

#target_include_directories(xrRenderInterface PUBLIC "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_win10.2023.4.28.1/include/")

## RenderDoc
target_include_directories(xrRenderInterface PUBLIC "${RENDERDOC_API}")
