project(xrECore)

# Files import
file(GLOB_RECURSE ECORE_SOURCE_ALL_FILES CONFIGURE_DECORENDS
    "*.h"
    "*.cpp"
)

file(GLOB_RECURSE ECORE_SOURCE_ENGINE_FILES CONFIGURE_DECORENDS
    "Engine/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_EDITOR_FILES CONFIGURE_DECORENDS
    "Editor/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_WM_FILES CONFIGURE_DECORENDS
    "WildMagic/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_RENDER_FILESEX CONFIGURE_DECORENDS
    "Render/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_ERR_FILES CONFIGURE_DECORENDS
    "dxerr/*.*"
)

file(GLOB_RECURSE ECORE_SOURCE_RENDER_FILES CONFIGURE_DECORENDS
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

file(GLOB_RECURSE ECORE_SOURCE_EXT_FILES CONFIGURE_DECORENDS
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

# Apply list
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_ALL_FILES})
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_RENDER_FILES})
list(APPEND ECORE_SRC_FILES ${ECORE_SOURCE_EXT_FILES})

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
#target_compile_definitions(xrECore PRIVATE _EDITOR)
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
target_link_libraries(xrECore PUBLIC xrCDB)
target_link_libraries(xrECore PUBLIC luabind)
target_link_libraries(xrECore PUBLIC xrParticles)
target_link_libraries(xrECore PUBLIC xrSound)
target_link_libraries(xrECore PUBLIC ETools)
target_link_libraries(xrECore PUBLIC xrAbstractions)
target_link_libraries(xrECore PUBLIC xrPhysics)
target_link_libraries(xrECore PUBLIC xrDXT)
target_link_libraries(xrECore PUBLIC MagicFM)
target_link_libraries(xrECore PUBLIC "d3d9.lib")
target_link_libraries(xrECore PUBLIC "d3dcompiler.lib")

# UnityBuild Preferences
#if (IXRAY_UNITYBUILD)
#	set_target_properties(xrECore PROPERTIES UNITY_BUILD ON UNITY_BUILD_BATCH_SIZE 32)
#	target_compile_options(xrECore PRIVATE /bigobj)
#    
#	file(GLOB_RECURSE ECORE_SOURCE_ALL_C_FILES "*.c")
#	set_source_files_properties(${ECORE_SOURCE_ALL_C_FILES} PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
#else()
    # Move precompiled header
    target_validate_pch(xrECore ${CMAKE_CURRENT_SOURCE_DIR})
#endif()


## DirectX SDK
if(NOT "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
    target_link_libraries(xrECore PUBLIC ${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/release/lib/x86/d3dx9.lib)
else()
    target_link_libraries(xrECore PUBLIC ${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/release/lib/x64/d3dx9.lib)
endif()

target_include_directories(xrECore PUBLIC "${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/include/")
target_include_directories(xrECore PUBLIC "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_win10.2023.4.28.1/include/")

## DirectX SDK
target_include_directories(xrECore PUBLIC "${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2024.6.5.1/include/")
target_link_libraries(xrECore PUBLIC ${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2024.6.5.1/native/lib/${NUGET_PACKAGE_PLATFORM}/release/DirectXTex.lib)

# Nuget
set(SNDE_OGG ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/)
set(SNDE_OAL ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.OpenALSoft.1.23.1.1/)
set(SNDE_VOB ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibVorbis.1.3.7.4/)

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

## RedImage
target_link_libraries(xrECore PRIVATE ${REDIMAGE_PATH}/runtimes/win-x64/native/Release/RedImageTool.lib)
target_include_directories(xrECore PRIVATE "${REDIMAGE_PATH}/build/native/include/")

### Copy binaries
add_custom_command(TARGET ${PROJECT_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${REDIMAGE_PATH}/runtimes/win-x64/native/Release/RedImageTool.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)
