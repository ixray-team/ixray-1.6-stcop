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
	"../../Layers/xrRenderDX9/dx9r_constants_cache.*"
	"../../Layers/xrRender/Animation.*"
	"../../Layers/xrRender/blenders/Blender.*"
	"../../Layers/xrRender/blenders/Blender_Palette.*"
	"../../Layers/xrRender/blenders/Blender_Recorder.*"
	"../../Layers/xrRender/Blender_BmmD.*"
	"../../Layers/xrRender/Blender_detail_still.*"
	"../../Layers/xrRender/Blender_Editor_Selection.*"
	"../../Layers/xrRender/Blender_Editor_Wire.*"
	"../../Layers/xrRender/Blender_Lm(EbB).*"
	"../../Layers/xrRender/Blender_Model_EbB.*"
	"../../Layers/xrRender/Blender_Particle.*"
	"../../Layers/xrRender/Blender_Recorder_R2.*"
	"../../Layers/xrRender/Blender_Recorder_StandartBinding.*"
	"../../Layers/xrRender/Blender_Screen_SET.*"
	"../../Layers/xrRender/Blender_tree.*"
	"../../Layers/xrRender/DetailModel.*"
	"../../Layers/xrRender/dxApplicationRender.*"
	"../../Layers/xrRender/dxConsoleRender.*"
	"../../Layers/xrRender/dxDebugRender.*"
	"../../Layers/xrRender/dxStatGraphRender.*"
	"../../Layers/xrRender/dxEnvironmentRender.*"
	"../../Layers/xrRender/dxFontRender.*"
	"../../Layers/xrRender/dxLensFlareRender.*"
	"../../Layers/xrRender/dxObjectSpaceRender.*"
	"../../Layers/xrRender/dxParticleCustom.*"
	"../../Layers/xrRender/dxRainRender.*"
	"../../Layers/xrRender/dxRenderDeviceRender.*"
	"../../Layers/xrRender/dxRenderFactory.*"
	"../../Layers/xrRender/dxStatsRender.*"
	"../../Layers/xrRender/dxThunderboltDescRender.*"
	"../../Layers/xrRender/dxThunderboltRender.*"
	"../../Layers/xrRender/dxUIRender.*"
	"../../Layers/xrRender/dxUISequenceVideoItem.*"
	"../../Layers/xrRender/dxUIShader.*"
	"../../Layers/xrRender/dxWallMarkArray.*"
	"../../Layers/xrRender/ETextureParams.*"
	"../../Layers/xrRender/FBasicVisual.*"
	"../../Layers/xrRender/FHierrarhyVisual.*"
	"../../Layers/xrRender/FProgressive.*"
	"../../Layers/xrRender/FSkinned.*"
	"../../Layers/xrRender/FVisual.*"
	"../../Layers/xrRenderDX9/dx9HW.*"
	"../../Layers/xrRenderDX9/dx9HWCaps.*"
	"../../Layers/xrRender/ModelPool.*"
	"../../Layers/xrRender/ParticleEffect.*"
	"../../Layers/xrRender/ParticleEffectDef.*"
	"../../Layers/xrRender/ParticleGroup.*"
	"../../Layers/xrRender/PSLibrary.*"
	"../../Layers/xrRender/ResourceManager.*"
	"../../Layers/xrRender/ResourceManager_Loader.*"
	"../../Layers/xrRender/ResourceManager_Reset.*"
	"../../Layers/xrRenderDX9/dx9ResourceManager_Resources.*"
	"../../Layers/xrRenderDX9/dx9ResourceManager_Scripting.*"
	"../../Layers/xrRender/R_Backend.*"
	"../../Layers/xrRender/R_Backend_DBG.*"
	"../../Layers/xrRender/R_Backend_hemi.*"
	"../../Layers/xrRender/R_Backend_Runtime.*"
	"../../Layers/xrRender/R_Backend_tree.*"
	"../../Layers/xrRender/R_Backend_xform.*"
	"../../Layers/xrRender/r_constants.*"
	"../../Layers/xrRender/R_DStreams.*"
	"../../Layers/xrRender/Shader.*"
	"../../Layers/xrRender/SH_Atomic.*"
	"../../Layers/xrRender/SH_Constant.*"
	"../../Layers/xrRender/SH_Matrix.*"
	"../../Layers/xrRenderDX9/dx9SH_RT.*"
	"../../Layers/xrRenderDX9/dx9SH_Texture.*"
	"../../Layers/xrRenderDX9/DDSTextureLoader9.*"
	"../../Layers/xrRender/SkeletonAnimated.*"
	"../../Layers/xrRender/SkeletonCustom.*"
	"../../Layers/xrRender/SkeletonRigid.*"
	"../../Layers/xrRender/SkeletonX.*"
	"../../Layers/xrRender/stats_manager.*"
	"../../Layers/xrRenderDX9/dx9Texture.*"
	"../../Layers/xrRender/TextureDescrManager.*"
	"../../Layers/xrRender/tga.*"
	"../../Layers/xrRender/tss_def.*"
	"../../Layers/xrRender/xrRender_console.*"
	"../../Layers/xrRenderPC_R1/Blender_Blur.*"
	"../../Layers/xrRenderPC_R1/BlenderDefault.*"
	"../../Layers/xrRenderPC_R1/Blender_default_aref.*"
	"../../Layers/xrRenderPC_R1/Blender_LaEmB.*"
	"../../Layers/xrRenderPC_R1/Blender_Model.*"
	"../../Layers/xrRenderPC_R1/Blender_Screen_GRAY.*"
	"../../Layers/xrRenderPC_R1/Blender_Shadow_World.*"
	"../../Layers/xrRenderPC_R1/Blender_Vertex_aref.*"
	"../../Layers/xrRenderPC_R1/Blender_Vertex.*"
)

file(GLOB_RECURSE ECORE_SOURCE_EXT_FILES CONFIGURE_DEPENDS
 #   "../../xrEngine/motion.cpp"
 #  "../../xrEngine/fmesh.cpp"
	"../../xrEngine/ObjectAnimator.cpp"
	"../../xrEngine/ai_script_lua_debug.cpp"
	"../../xrEngine/ai_script_lua_extension.cpp"
	"../../xrServerEntities/LevelGameDef.cpp"
)

set(ECORE_SRC_FILES)

source_group("Engine" FILES ${ECORE_SOURCE_ENGINE_FILES})
source_group("Editor" FILES ${ECORE_SOURCE_EDITOR_FILES})
source_group("WildMagic" FILES ${ECORE_SOURCE_WM_FILES})
source_group("Render" FILES ${ECORE_SOURCE_RENDER_FILES})
source_group("RenderEx" FILES ${ECORE_SOURCE_RENDEREX_FILES})
source_group("Other"  FILES ${ECORE_SOURCE_EXT_FILES})
source_group("dxerr"  FILES ${ECORE_SOURCE_ERR_FILES})
source_group("Resources"  FILES ${ECORE_SOURCE_RES_FILES})

# Apply list
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_ALL_FILES})
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_RENDER_FILES})
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_EXT_FILES})
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_RES_FILES})

# Remove unused files
list(FILTER ECORE_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# xrECore project
add_library(xrECore SHARED ${ECORE_SRC_FILES})
target_include_directories(xrECore PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
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
target_link_libraries(xrECore PUBLIC xrCore)
target_link_libraries(xrECore PUBLIC xrEUI)
target_link_libraries(xrECore PUBLIC xrEProps)
target_link_libraries(xrECore PUBLIC luabind)
target_link_libraries(xrECore PUBLIC xrParticles)
target_link_libraries(xrECore PUBLIC xrSound)
target_link_libraries(xrECore PUBLIC ETools)
target_link_libraries(xrECore PUBLIC xrAbstractions)
target_link_libraries(xrECore PUBLIC xrPhysics)
target_link_libraries(xrECore PUBLIC xrDXT)
target_link_libraries(xrECore PUBLIC MagicFM)
target_link_libraries(xrECore PUBLIC NvMender2003)
if(WIN32)
	target_link_libraries(xrECore PUBLIC "d3d9.lib")
	target_link_libraries(xrECore PUBLIC "d3dcompiler.lib")
endif()

target_validate_pch(xrECore ${CMAKE_CURRENT_SOURCE_DIR})


## DirectX SDK and related Windows-only libs
if(WIN32)
	if(NOT "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
		target_link_libraries(xrECore PUBLIC ${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/release/lib/x86/d3dx9.lib)
	else()
		target_link_libraries(xrECore PUBLIC ${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/release/lib/x64/d3dx9.lib)
	endif()

	target_include_directories(xrECore PUBLIC "${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2024.6.5.1/include/")
	target_include_directories(xrECore PUBLIC "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_2019.2024.6.5.1/include/")
	target_include_directories(xrECore PUBLIC "${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/include/")
	target_link_libraries(xrECore PUBLIC ${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_2019.2024.6.5.1/native/lib/x64/Release/DirectXMesh.lib)
	target_link_libraries(xrECore PUBLIC ${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2024.6.5.1/native/lib/${NUGET_PACKAGE_PLATFORM}/release/DirectXTex.lib)
else()
	# On Linux we don't use DirectX SDK. Prefer NVTT (nvtt) if available for texture processing
	find_package(PkgConfig QUIET)
	if(PkgConfig_FOUND)
		pkg_check_modules(NVTT nvtt QUIET)
		if(NVTT_FOUND)
			target_include_directories(xrECore PUBLIC ${NVTT_INCLUDE_DIRS})
			target_link_libraries(xrECore PUBLIC ${NVTT_LIBRARIES})
		endif()
	endif()
endif()

# Nuget
set(SNDE_OGG ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/)
set(SNDE_OAL ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.OpenALSoft.1.23.1.1/)
set(SNDE_VOB ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibVorbis.1.3.7.4/)

## Audio libraries: Windows via NuGet, otherwise prefer system packages
if(WIN32)
	## LibOGG
	target_link_libraries(xrECore PRIVATE ${SNDE_OGG}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libogg.lib)
	target_include_directories(xrECore PRIVATE "${SNDE_OGG}/native/include/")

	## Vorbis
	target_link_libraries(xrECore PRIVATE ${SNDE_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbisfile.lib)
	target_link_libraries(xrECore PRIVATE ${SNDE_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbis.lib)
	target_include_directories(xrECore PRIVATE "${SNDE_VOB}/native/include/")

	## OpenAL
	target_link_libraries(xrECore PRIVATE ${SNDE_OAL}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/OpenAL32.lib)
	target_include_directories(xrECore PRIVATE "${SNDE_OAL}/native/include/")
else()
	find_package(PkgConfig REQUIRED)

	pkg_check_modules(OGG ogg QUIET)
	if(OGG_FOUND)
		target_include_directories(xrECore PRIVATE ${OGG_INCLUDE_DIRS})
		target_link_libraries(xrECore PRIVATE ${OGG_LIBRARIES})
	else()
		find_library(OGG_LIB ogg)
		if(OGG_LIB)
			target_link_libraries(xrECore PRIVATE ${OGG_LIB})
		else()
			message(WARNING "xrECore: libogg not found on system; audio features may be degraded.")
		endif()
	endif()

	pkg_check_modules(VORBIS vorbisfile vorbis QUIET)
	if(VORBIS_FOUND)
		target_include_directories(xrECore PRIVATE ${VORBIS_INCLUDE_DIRS})
		target_link_libraries(xrECore PRIVATE ${VORBIS_LIBRARIES})
	else()
		find_library(VORBISFILE_LIB vorbisfile)
		find_library(VORBIS_LIB vorbis)
		if(VORBISFILE_LIB)
			target_link_libraries(xrECore PRIVATE ${VORBISFILE_LIB} ${VORBIS_LIB})
		else()
			message(WARNING "xrECore: libvorbis not found on system; some features disabled.")
		endif()
	endif()

	find_package(OpenAL QUIET)
	if(OpenAL_FOUND)
		target_include_directories(xrECore PRIVATE ${OPENAL_INCLUDE_DIR})
		target_link_libraries(xrECore PRIVATE ${OPENAL_LIBRARY})
	else()
		find_library(OPENAL_LIB OpenAL)
		if(OPENAL_LIB)
			target_link_libraries(xrECore PRIVATE ${OPENAL_LIB})
		else()
			message(WARNING "xrECore: OpenAL not found on system; audio output may be unavailable.")
		endif()
	endif()
endif()
