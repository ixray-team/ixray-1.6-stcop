project(xrRender_R4)
set(XRR4_SRC_FILES)

# Files import
file(GLOB_RECURSE XRR4_SOURCE_ALL_FILES CONFIGURE_DEPENDS
	"*.cpp"
	"*.h"
)
file(GLOB_RECURSE XRR4_SOURCE_COMP_FILES CONFIGURE_DEPENDS
	"../xrRender/*.cpp"
	"../xrRender/*.h"
)
file(GLOB XRR4_SOURCE_BLEND_FILES
	"../xrRender/Blender_*.cpp"
	"../xrRender/Blender_*.h"
	"Blender_*.cpp"
	"Blender_*.h"
)
file(GLOB_RECURSE XRR4_SOURCE_DX11_FILES CONFIGURE_DEPENDS
	"../xrRenderDX10/*.cpp"
	"../xrRenderDX10/*.h"
)
file(GLOB XRR4_SOURCE_LUA_FILES CONFIGURE_DEPENDS
	"../../xrEngine/ai_script_lua*.cpp"
	"../../xrEngine/ai_script_lua*.h"
)
file(GLOB XRR4_SOURCE_OVERLAY_FILES "OverlayAPI/*.*")
file(GLOB_RECURSE XRR4_SOURCE_INCL_FILES CONFIGURE_DEPENDS "../../Include/*.h")

file(GLOB_RECURSE XRR4_SOURCE_PARTICLE_CORE CONFIGURE_DEPENDS
	"../xrRender/particle_core/*.cpp"
	"../xrRender/particle_core/*.h"
)

# Source groups
source_group("render/overlay" FILES ${XRR4_SOURCE_OVERLAY_FILES})
source_group("render/src" FILES ${XRR4_SOURCE_ALL_FILES})
source_group("render/include" FILES ${XRR4_SOURCE_INCL_FILES})
source_group("layers" FILES ${XRR4_SOURCE_COMP_FILES})
source_group("general_dx11" FILES ${XRR4_SOURCE_DX11_FILES})
source_group("lua_ext" FILES ${XRR4_SOURCE_LUA_FILES})
source_group("blend" FILES ${XRR4_SOURCE_BLEND_FILES})
source_group("particle_core" FILES ${XRR4_SOURCE_PARTICLE_CORE})

# Apply list
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_ALL_FILES})
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_COMP_FILES})
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_DX11_FILES})
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_LUA_FILES})
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_INCL_FILES})

# xrRender_R4 project
add_library(xrRender_R4 SHARED ${XRR4_SRC_FILES})
target_precompile_headers(xrRender_R4 PRIVATE "stdafx.h")

# Include directories
target_include_directories(xrRender_R4 PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrRender_R4 PUBLIC "${IXRAY_SDK_INC}")

# Project defines
target_compile_definitions(xrRender_R4 PRIVATE XRRENDER_R4_EXPORTS)
target_compile_definitions(xrRender_R4 PRIVATE USE_DX11)
target_compile_definitions(xrRender_R4 PRIVATE _USRDLL)
target_compile_definitions(xrRender_R4 PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrRender_R4 PUBLIC xrEngineCore)

if (WIN32)
	target_link_libraries(xrRender_R4 PUBLIC AMD::FSR2)
	target_link_libraries(xrRender_R4 PUBLIC AMD::FSR2_DX11)
	target_link_libraries(xrRender_R4 PUBLIC NVIDIA::DLSS)
	target_link_libraries(xrRender_R4 PUBLIC DirectX::Tex)
	target_link_libraries(xrRender_R4 PUBLIC DirectX::Mesh)
	target_link_libraries(xrRender_R4 PRIVATE WindowsSDK::D3D11)
	target_link_libraries(xrRender_R4 PRIVATE WindowsSDK::DXGI)
	target_link_libraries(xrRender_R4 PRIVATE WindowsSDK::DXGUID)
	target_link_libraries(xrRender_R4 PRIVATE WindowsSDK::D3DCompiler)
endif()

## RenderDoc
target_include_directories(xrRender_R4 PUBLIC "${RENDERDOC_API}")

## Intel XESS
#target_include_directories(xrRender_R4 PUBLIC "${INTEL_XESS}")
#target_link_libraries(xrRender_R4 PUBLIC "${INTEL_XESS_LIB}")
#target_link_libraries(xrRender_R4 PUBLIC "${INTEL_XESS_DX11_LIB}")
#
#add_custom_command(TARGET ${PROJECT_NAME}
#	POST_BUILD
#	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${INTEL_XESS_BIN} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
#	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${INTEL_XESS_DX11_BIN} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
#)

## lunaSVG
target_link_libraries(xrRender_R4 PUBLIC lunasvg)
