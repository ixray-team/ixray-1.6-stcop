project(LevelEditor)

# Files import
file(GLOB_RECURSE ELE_SOURCE_ALL_FILES CONFIGURE_DEPENDS "*.*")
file(GLOB ELE_SOURCE_ENTRY_CODE_FILES CONFIGURE_DEPENDS "*.cpp" "*.h")
file(GLOB ELE_SOURCE_ENTRY_RES_FILES CONFIGURE_DEPENDS "*.manifest" "*.rc" "*.aps" "*.ico")
file(GLOB_RECURSE ELE_SOURCE_AI_FILES CONFIGURE_DEPENDS "AI/*.*")
file(GLOB_RECURSE ELE_SOURCE_UI_FILES CONFIGURE_DEPENDS "UI/*.*")
file(GLOB_RECURSE ELE_SOURCE_EDITOR_FILES CONFIGURE_DEPENDS "Editor/*.*")
file(GLOB_RECURSE ELE_SOURCE_NODES_FILES CONFIGURE_DEPENDS "Nodes/*.*")
file(GLOB_RECURSE ELE_SOURCE_PLUGINS_FILES CONFIGURE_DEPENDS "Plugins/*.*")
file(GLOB_RECURSE ELE_SOURCE_EDITOR_TOOLS_FILES CONFIGURE_DEPENDS "Editor/Tools/*.*")
file(GLOB_RECURSE ELE_SOURCE_EDITOR_UTILS_FILES CONFIGURE_DEPENDS "Editor/Utils/*.*")
file(GLOB_RECURSE ELE_SOURCE_ENGINE_FILES CONFIGURE_DEPENDS "Engine/*.*")
file(GLOB_RECURSE ELE_SOURCE_PARTICLE_EDITOR_FILES CONFIGURE_DEPENDS "ParticleEditor/*.*")
file(GLOB_RECURSE ELE_SOURCE_ACTOR_EDITOR_FILES CONFIGURE_DEPENDS "ActorEditor/*.*")
file(GLOB_RECURSE ELE_SOURCE_SHADER_EDITOR_FILES CONFIGURE_DEPENDS "ShaderEditor/*.*")

file(GLOB_RECURSE ELE_SOURCE_SERVER_ENTITIES_WRAPPER_FILES CONFIGURE_DEPENDS
    "../../xrServerEntities/server_entity_wrapper.h"
    "../../xrServerEntities/server_entity_wrapper.cpp"
    "../../xrServerEntities/server_entity_wrapper_inline.h"
    )

file(GLOB ELE_SOURCE_POSTPROCESS_FILES CONFIGURE_DEPENDS "../../xrGame/PostprocessAnimator.*")

set(ELE_SRC_FILES)

source_group("editor/ai" FILES ${ELE_SOURCE_AI_FILES})
source_group("editor/server_entity_wrapper" FILES ${ELE_SOURCE_SERVER_ENTITIES_WRAPPER_FILES})
source_group("editor/tools" FILES ${ELE_SOURCE_EDITOR_TOOLS_FILES})
source_group("editor/utils" FILES ${ELE_SOURCE_EDITOR_UTILS_FILES})
source_group("editor/plugins" FILES ${ELE_SOURCE_PLUGINS_FILES})
source_group("editor/core" FILES ${ELE_SOURCE_EDITOR_FILES})
source_group("editor/ui/nodes" FILES ${ELE_SOURCE_NODES_FILES})
source_group("editor/ui" FILES ${ELE_SOURCE_UI_FILES})
source_group("engine/core" FILES ${ELE_SOURCE_ENGINE_FILES})
source_group("editor/particles" FILES ${ELE_SOURCE_PARTICLE_EDITOR_FILES})
source_group("editor/actor" FILES ${ELE_SOURCE_ACTOR_EDITOR_FILES})
source_group("editor/shaders" FILES ${ELE_SOURCE_SHADER_EDITOR_FILES})
source_group("editor/actor/postprocess" FILES ${ELE_SOURCE_POSTPROCESS_FILES})
source_group("le/src" FILES ${ELE_SOURCE_ENTRY_CODE_FILES})
source_group("le/res" FILES ${ELE_SOURCE_ENTRY_RES_FILES})

# Apply list
list(APPEND ELE_SRC_FILES ${ELE_SOURCE_ALL_FILES})
list(APPEND ELE_SRC_FILES ${ELE_SOURCE_SERVER_ENTITIES_WRAPPER_FILES})
list(APPEND ELE_SRC_FILES ${ELE_SOURCE_POSTPROCESS_FILES})

# Remove unused files
list(FILTER ELE_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# LevelEditor project
add_executable(LevelEditor WIN32 ${ELE_SRC_FILES})
target_include_directories(LevelEditor PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(LevelEditor PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../../xrCore/")
target_precompile_headers(LevelEditor PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(LevelEditor PRIVATE _LEVEL_EDITOR)
# Post Process Editor of the Actor Editor workspace
target_compile_definitions(LevelEditor PRIVATE _PP_EDITOR_)

target_compile_options(LevelEditor PRIVATE /W0)
target_compile_definitions(LevelEditor PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(LevelEditor PUBLIC xrCore)
target_link_libraries(LevelEditor PUBLIC xrEUI)
target_link_libraries(LevelEditor PUBLIC xrEProps)
target_link_libraries(LevelEditor PUBLIC xrECore)
target_link_libraries(LevelEditor PUBLIC xrSound)
target_link_libraries(LevelEditor PUBLIC xrEngineCore)
target_link_libraries(LevelEditor PUBLIC xrScripts)
target_link_libraries(LevelEditor PUBLIC imgui_node_editor)
target_link_libraries(LevelEditor PUBLIC xrSE_Factory)
target_link_libraries(LevelEditor PUBLIC PCore)
target_link_libraries(LevelEditor PUBLIC "Shell32.lib")
add_dependencies(LevelEditor xrGame)

# VS input-signature reflection of the Shader Editor preview (CPreviewObject) needs the D3D11 reflection API
target_link_libraries(LevelEditor PUBLIC d3d11.lib d3dcompiler.lib dxguid.lib)

# Move precompiled header
target_validate_pch(LevelEditor ${CMAKE_CURRENT_SOURCE_DIR})

## LibOGG
target_link_libraries(LevelEditor PRIVATE ${SND_OGG}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libogg.lib)
target_include_directories(LevelEditor PRIVATE "${SND_OGG}/native/include/")

## Vorbis
target_link_libraries(LevelEditor PRIVATE ${SND_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbisfile.lib)
target_link_libraries(LevelEditor PRIVATE ${SND_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbis.lib)
target_include_directories(LevelEditor PRIVATE "${SND_VOB}/native/include/")
