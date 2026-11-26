project(xrRender_IXR)
set(XRR4_SRC_FILES)

# Files import
file(GLOB_RECURSE XRR4_SOURCE_ALL_FILES CONFIGURE_DEPENDS
	"*.cpp"
	"*.h"
)
file(GLOB_RECURSE XRR4_SOURCE_COMP_FILES CONFIGURE_DEPENDS
	"*.cpp"
	"*.h"
)
file(GLOB XRR4_SOURCE_BLEND_FILES
	"Blender_*.cpp"
	"Blender_*.h"
	"Blender_*.cpp"
	"Blender_*.h"
)
file(GLOB_RECURSE XRR4_SOURCE_DX11_FILES CONFIGURE_DEPENDS
	"*.cpp"
	"*.h"
)
file(GLOB XRR4_SOURCE_OVERLAY_FILES "OverlayAPI/*.*")
file(GLOB_RECURSE XRR4_SOURCE_INCL_FILES CONFIGURE_DEPENDS "../../Include/*.h")

file(GLOB_RECURSE XRR4_SOURCE_PARTICLE_CORE CONFIGURE_DEPENDS
	"particle_core/*.cpp"
	"particle_core/*.h"
)

# Source groups
source_group("render/overlay" FILES ${XRR4_SOURCE_OVERLAY_FILES})
source_group("render/src" FILES ${XRR4_SOURCE_ALL_FILES})
source_group("render/include" FILES ${XRR4_SOURCE_INCL_FILES})
source_group("layers" FILES ${XRR4_SOURCE_COMP_FILES})
source_group("general_dx11" FILES ${XRR4_SOURCE_DX11_FILES})
source_group("blend" FILES ${XRR4_SOURCE_BLEND_FILES})
source_group("particle_core" FILES ${XRR4_SOURCE_PARTICLE_CORE})

# Apply list
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_ALL_FILES})
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_COMP_FILES})
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_DX11_FILES})
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_INCL_FILES})

# xrRender_IXR project
if(APPLE)
    list(FILTER XRR4_SRC_FILES EXCLUDE REGEX "ixr_rendertarget_phase_(dlss|fsr)\\.cpp$")
    list(APPEND XRR4_SRC_FILES "${CMAKE_CURRENT_SOURCE_DIR}/ixr_rendertarget_phase_upscalers_macos.cpp")
endif()

add_library(xrRender_IXR SHARED ${XRR4_SRC_FILES})
target_precompile_headers(xrRender_IXR PRIVATE "stdafx.h")

# Include directories
target_include_directories(xrRender_IXR PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrRender_IXR PUBLIC "${IXRAY_SDK_INC}")

# Project defines
target_compile_definitions(xrRender_IXR PRIVATE XRRENDER_IXR_EXPORTS)

# Render internals are exported for the editors, which link xrRender_IXR directly
target_compile_definitions(xrRender_IXR PRIVATE RENDER_API_EXPORTS)
target_compile_definitions(xrRender_IXR INTERFACE RENDER_API_IMPORTS)
target_compile_definitions(xrRender_IXR PRIVATE _USRDLL)
target_compile_definitions(xrRender_IXR PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrRender_IXR PUBLIC xrEngineCore)
target_link_libraries(xrRender_IXR PRIVATE xrRHI)
target_link_libraries(xrRender_IXR PUBLIC NvTriStrip)
target_link_libraries(xrRender_IXR PUBLIC lunasvg)
target_link_libraries(xrRender_IXR PRIVATE meshoptimizer)
# DDS files are read by the renderer and made into NRI textures (GpuResource)
target_link_libraries(xrRender_IXR PUBLIC DirectX::Tex)
# clusterlod.h: the cluster LOD build that ships with meshoptimizer as a single header
target_include_directories(xrRender_IXR PRIVATE "${meshoptimizer_SOURCE_DIR}/demo")

if (NOT WIN32)
    target_compile_definitions(xrRender_IXR PRIVATE XR_USE_NATIVE_D3D11=1)
    if(LINUX)
        target_compile_definitions(xrRender_IXR PRIVATE DXVK_WSI_DRIVER_SDL3=1)
    endif()
    target_link_libraries(xrRender_IXR PUBLIC DirectX::Mesh)
endif()

## RenderDoc
target_include_directories(xrRender_IXR PUBLIC "${RENDERDOC_API}")
