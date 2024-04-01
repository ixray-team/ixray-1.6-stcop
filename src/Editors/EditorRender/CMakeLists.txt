project(EditorRender)

# Files import
file(GLOB_RECURSE ED_RENDER_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "../../Layers/xrRenderPC_R1/BlenderDefault.*"
    "../../Layers/xrRenderPC_R1/Blender_default_aref.*"
    "../../Layers/xrRenderPC_R1/Blender_Vertex.*"
    "../../Layers/xrRenderPC_R1/Blender_vertex_aref.*"
    "../../Layers/xrRenderPC_R1/Blender_screen_gray.*"
    "../../Layers/xrRenderPC_R1/Blender_LaEmB.*"
    "../../Layers/xrRenderPC_R1/Blender_Shadow_World.*"
    "../../Layers/xrRenderPC_R1/Blender_Model.*"
    "../../Layers/xrRenderPC_R1/Blender_Blur.*"
    "../../Layers/xrRender/FBasicVisual.*"
    "../../Layers/xrRender/FHierrarhyVisual.*"
    "../../Layers/xrRender/SkeletonAnimated.*"
    "../../Layers/xrRender/SkeletonCustom.*"
    "../../Layers/xrRender/SkeletonRigid.*"
    "../../Layers/xrRender/SkeletonX.*"
    "../../Layers/xrRender/ResourceManager_Reset.*"
    "../../Layers/xrRender/Blender_Editor_Selection.*"
    "../../Layers/xrRender/Blender_Screen_SET.*"
    "../../Layers/xrRender/Blender_Lm(EbB).*"
    "../../Layers/xrRender/Blender_Blur.*"
    "../../Layers/xrRender/Blender_Editor_Wire.*"
    "../../Layers/xrRender/Blender_BmmD.*"
    "../../Layers/xrRender/Blender_Model_EbB.*"
    "../../Layers/xrRender/Blender_detail_still.*"
    "../../Layers/xrRender/Blender_tree.*"
    "../../Layers/xrRender/Blender_particle.*"
    "../../Layers/xrRender/FProgressive.*"
    "../../Layers/xrRender/FSkinned.*"
    "../../Layers/xrRender/dxDebugRender.*"
    "../../Layers/xrRender/Animation.*"
    "../../Layers/xrRender/dxRenderFactory.*"
    "../../Layers/xrRender/dxFontRender.*"
    "../../Layers/xrRender/R_Backend.*"
    "../../Layers/xrRender/R_Backend_xform.*"
    "../../Layers/xrRender/R_Backend_DBG.*"
    "../../Layers/xrRender/R_Backend_Runtime.*"
    "../../Layers/xrRender/r_constants.*"
    "../../Layers/xrRender/ModelPool.*"
    "../../Layers/xrRender/R_DStreams.*"
    "../../Layers/xrRender/ResourceManager.*"
    "../../Layers/xrRender/ResourceManager_Loader.*"
    "../../Layers/xrRender/ParticleEffectDef.*"
    "../../Layers/xrRender/ParticleGroup.*"
    "../../Layers/xrRender/ParticleEffect.*"
    "../../Layers/xrRender/PSLibrary.*"
    "../../Layers/xrRender/SH_Atomic.*"
    "../../Layers/xrRender/SH_Constant.*"
    "../../Layers/xrRender/SH_Matrix.*"
    "../../Layers/xrRender/Shader.*"
    "../../Layers/xrRender/DetailModel.*"
    "../../Layers/xrRender/tss_def.*"
    "../../Layers/xrRender/blenders/Blender_Recorder.*"
    "../../Layers/xrRender/blenders/Blender.*"
    "../../Layers/xrRender/blenders/Blender_Palette.*"
    "../../Layers/xrRender/blenders/Blender_Recorder_StandartBinding.*"
    "../../Layers/xrRender/TextureDescrManager.*"
    "../../Layers/xrRender/ETextureParams.*"
    "../../Layers/xrRender/Blender_Recorder_R2.*"
    "../../Layers/xrRender/R_Backend_hemi.*"
    "../../Layers/xrRender/FVisual.*"
    "../../Layers/xrRender/Blender_Recorder_StandartBinding.*"
    "../../Layers/xrRender/R_Backend_tree.*"
    "../../Layers/xrRenderDX9/dx9HW.*"
    "../../Layers/xrRenderDX9/dx9HWCaps.*"
    "../../Layers/xrRenderDX9/dx9r_constants_cache.*"
    "../../Layers/xrRenderDX9/dx9ResourceManager_Resources.*"
    "../../Layers/xrRenderDX9/dx9Texture.*"
    "../../Layers/xrRenderDX9/DDSTextureLoader9.*"
    "../../Layers/xrRenderDX9/dx9SH_Texture.*"
    "../../Layers/xrRenderDX9/dx9SH_RT.*"
)

file(GLOB_RECURSE ED_RENDER_SOURCE_SRC_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)

set(ED_RENDER_SRC_FILES)

source_group("src" FILES ${ED_RENDER_SOURCE_SRC_FILES})
source_group("api" FILES ${ED_RENDER_SOURCE_ALL_FILES})

# Apply list
list(APPEND ED_RENDER_SRC_FILES ${ED_RENDER_SOURCE_ALL_FILES})
list(APPEND ED_RENDER_SRC_FILES ${ED_RENDER_SOURCE_SRC_FILES})

# Remove unused files
list(FILTER ED_RENDER_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# EditorRender project
add_library(EditorRender ${ED_RENDER_SRC_FILES})
target_include_directories(EditorRender PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(EditorRender PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(EditorRender PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(EditorRender PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(EditorRender PUBLIC EditorEngineAPI)
target_link_libraries(EditorRender PUBLIC "d3dcompiler.lib")

# Move precompiled header
set_target_properties(EditorRender PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
set_target_properties(EditorRender PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
target_precompile_headers(EditorRender PRIVATE "stdafx.h")

file(GLOB_RECURSE ED_RENDER_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${ED_RENDER_SOURCE_PCH_FILES})

## DirectX SDK
target_include_directories(EditorRender PUBLIC "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_win10.2023.4.28.1/include/")
target_include_directories(EditorRender PUBLIC "${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2023.10.31.1/include/")
if(NOT "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
    target_link_libraries(EditorRender PUBLIC ${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2023.10.31.1/native/lib/x86/release/DirectXTex.lib)
else()
    target_link_libraries(EditorRender PUBLIC ${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2023.10.31.1/native/lib/x64/release/DirectXTex.lib)
endif()
