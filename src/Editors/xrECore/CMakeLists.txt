project(xrECore)

# Files import
file(GLOB_RECURSE ECORE_SOURCE_ALL_FILES CONFIGURE_DEPENDS
	"*.h"
	"*.cpp"
)

file(GLOB_RECURSE ECORE_SOURCE_ENGINE_FILES CONFIGURE_DEPENDS
	"Engine/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_EDITOR_FILES CONFIGURE_DEPENDS
	"Editor/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_EDITOR_PUBLIC_FILES CONFIGURE_DEPENDS
	"../Public/VIMP_Processor.*"
	"../Public/object_sliding.*"
)

file(GLOB_RECURSE ECORE_SOURCE_WM_FILES CONFIGURE_DEPENDS
	"WildMagic/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_RENDER_FILESEX CONFIGURE_DEPENDS
	"Render/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_ERR_FILES CONFIGURE_DEPENDS
	"dxerr/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_RES_FILES 
	"*.ico"
	"*.rc"
	"*.bmp"
)

file(GLOB_RECURSE ECORE_SOURCE_RENDER_FILES CONFIGURE_DEPENDS
	"../../Layers/xrRenderPC_R4/dx10r_constants_cache.*"
	"../../Layers/xrRenderPC_R4/dx10r_constants.*"
	"../../Layers/xrRenderPC_R4/dx10ConstantBuffer.*"
	"../../Layers/xrRenderPC_R4/Animation.*"
	"../../Layers/xrRenderPC_R4/blenders/Blender.*"
	"../../Layers/xrRenderPC_R4/blenders/Blender_Palette.*"
	"../../Layers/xrRenderPC_R4/blenders/Blender_Recorder.*"
	"../../Layers/xrRenderPC_R4/Blender_BmmD.*"
	"../../Layers/xrRenderPC_R4/Blender_detail_still.*"
	"../../Layers/xrRenderPC_R4/Blender_Editor_Selection.*"
	"../../Layers/xrRenderPC_R4/Blender_Editor_Wire.*"
	"../../Layers/xrRenderPC_R4/Blender_Lm(EbB).*"
	"../../Layers/xrRenderPC_R4/Blender_Model_EbB.*"
	"../../Layers/xrRenderPC_R4/Blender_Particle.*"
	"../../Layers/xrRenderPC_R4/Blender_Recorder_R2.*"
	"../../Layers/xrRenderPC_R4/Blender_Recorder_StandartBinding.*"
	"../../Layers/xrRenderPC_R4/Blender_Screen_SET.*"
	"../../Layers/xrRenderPC_R4/Blender_tree.*"
	"../../Layers/xrRenderPC_R4/DetailModel.*"
	"../../Layers/xrRenderPC_R4/dxApplicationRender.*"
	"../../Layers/xrRenderPC_R4/dxConsoleRender.*"
	"../../Layers/xrRenderPC_R4/dxDebugRender.*"
	"../../Layers/xrRenderPC_R4/dxStatGraphRender.*"
	"../../Layers/xrRenderPC_R4/dxEnvironmentRender.*"
	"../../Layers/xrRenderPC_R4/dxFontRender.*"
	"../../Layers/xrRenderPC_R4/dxLensFlareRender.*"
	"../../Layers/xrRenderPC_R4/dxObjectSpaceRender.*"
	"../../Layers/xrRenderPC_R4/dxRainRender.*"
	"../../Layers/xrRenderPC_R4/dxRenderDeviceRender.*"
	"../../Layers/xrRenderPC_R4/dxRenderFactory.*"
	"../../Layers/xrRenderPC_R4/dxStatsRender.*"
	"../../Layers/xrRenderPC_R4/dxThunderboltDescRender.*"
	"../../Layers/xrRenderPC_R4/dxThunderboltRender.*"
	"../../Layers/xrRenderPC_R4/dxUIRender.*"
	"../../Layers/xrRenderPC_R4/dxUISequenceVideoItem.*"
	"../../Layers/xrRenderPC_R4/dxUIShader.*"
	"../../Layers/xrRenderPC_R4/TextureAtlas.*"
	"../../Layers/xrRenderPC_R4/dxWallMarkArray.*"
	"../../Layers/xrRenderPC_R4/ETextureParams.*"
	"../../Layers/xrRenderPC_R4/FBasicVisual.*"
	"../../Layers/xrRenderPC_R4/FHierrarhyVisual.*"
	"../../Layers/xrRenderPC_R4/FProgressive.*"
	"../../Layers/xrRenderPC_R4/FSkinned.*"
	"../../Layers/xrRenderPC_R4/FVisual.*"
	"../../Layers/xrRenderPC_R4/dx11HWCaps.*"
	"../../Layers/xrRenderPC_R4/ModelPool.*"
	"../../Layers/xrRenderPC_R4/ParticleEffect.*"
	"../../Layers/xrRenderPC_R4/ParticleEffectDef.*"
	"../../Layers/xrRenderPC_R4/ParticleGroup.*"
	"../../Layers/xrRenderPC_R4/ParticleAnimCurve.*"
	"../../Layers/xrRenderPC_R4/PSLibrary.*"
	"../../Layers/xrRenderPC_R4/ResourceManager.*"
	"../../Layers/xrRenderPC_R4/ResourceManager_Loader.*"
	"../../Layers/xrRenderPC_R4/ResourceManager_Reset.*"
	"../../Layers/xrRenderPC_R4/dx11ResourceManager_Resources.*"
	"../../Layers/xrRenderPC_R4/dx11XMLBlendCompiler.*"
	"../../Layers/xrRenderPC_R4/dx11ResourceManager_Scripting.*"
	"../../Layers/xrRenderPC_R4/dx10StateUtils.*"
	"../../Layers/xrRenderPC_R4/dx10BufferUtils.*"
	"../../Layers/xrRenderPC_R4/dx10FixedConstants.*"
	"../../Layers/xrRenderPC_R4/StateManager/*.*"
	"../../Layers/xrRenderPC_R4/R_Backend.*"
	"../../Layers/xrRenderPC_R4/Utils/*.*"
	"../../Layers/xrRenderPC_R4/R_Backend_LOD.*"
	"../../Layers/xrRenderPC_R4/OverlayAPI/DLSSWrapper.*"
	"../../Layers/xrRenderPC_R4/R_Backend_DBG.*"
	"../../Layers/xrRenderPC_R4/R_Backend_hemi.*"
	"../../Layers/xrRenderPC_R4/R_Backend_Runtime.*"
	"../../Layers/xrRenderPC_R4/R_Backend_tree.*"
	"../../Layers/xrRenderPC_R4/R_Backend_xform.*"
	"../../Layers/xrRenderPC_R4/r_constants.*"
	"../../Layers/xrRenderPC_R4/R_DStreams.*"
	"../../Layers/xrRenderPC_R4/Shader.*"
	"../../Layers/xrRenderPC_R4/SH_Atomic.*"
	"../../Layers/xrRenderPC_R4/SH_Constant.*"
	"../../Layers/xrRenderPC_R4/SH_Matrix.*"
	"../../Layers/xrRenderPC_R4/SH_Texture.*"
	"../../Layers/xrRenderPC_R4/SH_RT.*"
	"../../Layers/xrRenderPC_R4/SkeletonAnimated.*"
	"../../Layers/xrRenderPC_R4/SkeletonCustom.*"
	"../../Layers/xrRenderPC_R4/SkeletonRigid.*"
	"../../Layers/xrRenderPC_R4/SkeletonX.*"
	"../../Layers/xrRenderPC_R4/RenderPhaseAccumPointGeom.*"
	"../../Layers/xrRenderPC_R4/RenderPhaseAccumSpotGeom.*"
	"../../Layers/xrRenderPC_R4/stats_manager.*"
	"../../Layers/xrRenderPC_R4/dx11Texture.*"
	"../../Layers/xrRenderPC_R4/TextureDescrManager.*"
	"../../Layers/xrRenderPC_R4/tga.*"
	"../../Layers/xrRenderPC_R4/tss_def.*"
	"../../Layers/xrRenderPC_R4/xrRender_console.*"
	"../../Layers/xrRenderPC_R4/Blender_Blur.*"
	"../../Layers/xrRenderPC_R4/BlenderDefault.*"
	"../../Layers/xrRenderPC_R4/Blender_default_aref.*"
	"../../Layers/xrRenderPC_R4/Blender_LaEmB.*"
	"../../Layers/xrRenderPC_R4/Blender_Model.*"
	"../../Layers/xrRenderPC_R4/Blender_Screen_GRAY.*"
	"../../Layers/xrRenderPC_R4/Blender_Shadow_World.*"
	"../../Layers/xrRenderPC_R4/Blender_Vertex_aref.*"
	"../../Layers/xrRenderPC_R4/Blender_Vertex.*"
	"../../Layers/xrRenderPC_R4/smol-atlas.*"
	"../../Layers/xrRenderPC_R4/particle_core/particle_core.*"
	"../../Layers/xrRenderPC_R4/particle_core/noise.*"
	"../../Layers/xrRenderPC_R4/particle_core/particle_actions_collection.*"
	"../../Layers/xrRenderPC_R4/particle_core/particle_actions_collection_io.cpp"
	"../../Layers/xrRenderPC_R4/particle_core/particle_holder.h"
	"../../Layers/xrRenderPC_R4/particle_core/psystem.h"
)

file(GLOB_RECURSE ECORE_SOURCE_EXT_FILES CONFIGURE_DEPENDS
	"../../xrEngine/ObjectAnimator.cpp"
	"../../xrEngine/ai_script_lua_debug.cpp"
	"../../xrEngine/ai_script_lua_extension.cpp"
	"../../xrServerEntities/LevelGameDef.cpp"
	"../../xrPlay/Splash.*"
)

set(ECORE_SRC_FILES)

source_group("Engine" FILES ${ECORE_SOURCE_ENGINE_FILES})
source_group("Editor" FILES ${ECORE_SOURCE_EDITOR_FILES})
source_group("MeshTools" FILES ${ECORE_SOURCE_EDITOR_PUBLIC_FILES})
source_group("WildMagic" FILES ${ECORE_SOURCE_WM_FILES})
source_group("Render" FILES ${ECORE_SOURCE_RENDER_FILES})
source_group("RenderEx" FILES ${ECORE_SOURCE_RENDER_FILESEX})
source_group("Other"  FILES ${ECORE_SOURCE_EXT_FILES})
source_group("dxerr"  FILES ${ECORE_SOURCE_ERR_FILES})
source_group("Resources"  FILES ${ECORE_SOURCE_RES_FILES})

# Apply list
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_ALL_FILES})
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_RENDER_FILES})
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_EXT_FILES})
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_RES_FILES})
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_EDITOR_PUBLIC_FILES})

# Remove unused files
list(FILTER ECORE_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# xrECore project
add_library(xrECore SHARED ${ECORE_SRC_FILES})
target_include_directories(xrECore PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrECore PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrECore PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/Public")
target_include_directories(xrECore PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/../")
target_include_directories(xrECore PUBLIC "${IXRAY_SDK_INC}/lua/")
target_precompile_headers(xrECore PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrECore PRIVATE XRECORE_EXPORTS)
target_compile_definitions(xrECore PRIVATE _WINDOWS)
target_compile_definitions(xrECore PRIVATE _USRDLL)
target_compile_definitions(xrECore PUBLIC XR_EDITOR_NEW)
target_compile_definitions(xrECore PUBLIC _EDITOR)

target_compile_options(xrECore PRIVATE /W0)
target_compile_definitions(xrECore PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrECore PUBLIC xrEUI)
target_link_libraries(xrECore PUBLIC xrEProps)
target_link_libraries(xrECore PUBLIC luabind)
target_link_libraries(xrECore PUBLIC xrEngineCore)
target_link_libraries(xrECore PUBLIC xrPhysics)
target_link_libraries(xrECore PUBLIC xrDXT)
target_link_libraries(xrECore PUBLIC MagicFM)
target_link_libraries(xrECore PUBLIC NvMender2003)

target_link_libraries(xrECore PRIVATE Ogg::ogg)
target_link_libraries(xrECore PRIVATE Vorbis::vorbis)
target_link_libraries(xrECore PRIVATE Vorbis::vorbisfile)

if (WIN32)
	target_link_libraries(xrECore PUBLIC DirectX::Mesh)
	target_link_libraries(xrECore PUBLIC DirectX::Tex)
	target_link_libraries(xrECore PRIVATE WindowsSDK::D3D11)
	target_link_libraries(xrECore PRIVATE WindowsSDK::DXGI)
	target_link_libraries(xrECore PRIVATE WindowsSDK::DXGUID)
	target_link_libraries(xrECore PRIVATE WindowsSDK::D3DCompiler)
	target_link_libraries(xrECore PUBLIC NVIDIA::DLSS)
endif()

target_validate_pch(xrECore ${CMAKE_CURRENT_SOURCE_DIR})

## lunaSVG
target_link_libraries(${PROJECT_NAME} PUBLIC lunasvg)