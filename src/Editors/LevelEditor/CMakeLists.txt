project(LevelEditor)

# Files import
file(GLOB_RECURSE ELE_SOURCE_ALL_FILES CONFIGURE_DEPENDS "*.*")
file(GLOB_RECURSE ELE_SOURCE_AI_FILES CONFIGURE_DEPENDS "AI/*.*")
file(GLOB_RECURSE ELE_SOURCE_UI_FILES CONFIGURE_DEPENDS "UI/*.*")
file(GLOB_RECURSE ELE_SOURCE_EDITOR_FILES CONFIGURE_DEPENDS "Editor/*.*")
file(GLOB_RECURSE ELE_SOURCE_EDITOR_TOOLS_FILES CONFIGURE_DEPENDS "Editor/Tools/*.*")
file(GLOB_RECURSE ELE_SOURCE_ENGINE_FILES CONFIGURE_DEPENDS "Engine/*.*")

file(GLOB_RECURSE ELE_SOURCE_RENDER_FILES CONFIGURE_DECORENDS
    "../../Layers/xrRender/DetailManager*"
    "../../Layers/xrRender/stats_manager*"
)
set(ELE_SRC_FILES)

source_group("ai" FILES ${ELE_SOURCE_AI_FILES})
source_group("editor/core" FILES ${ELE_SOURCE_EDITOR_FILES})
source_group("editor/tools" FILES ${ELE_SOURCE_EDITOR_TOOLS_FILES})
source_group("engine" FILES ${ELE_SOURCE_ENGINE_FILES})
source_group("ui" FILES ${ELE_SOURCE_UI_FILES})
source_group("render" FILES ${ELE_SOURCE_RENDER_FILES})

# Apply list
list(APPEND ELE_SRC_FILES ${ELE_SOURCE_ALL_FILES})
list(APPEND ELE_SRC_FILES ${ELE_SOURCE_RENDER_FILES})

# Remove unused files
list(FILTER ELE_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# LevelEditor project
add_executable(LevelEditor WIN32 ${ELE_SRC_FILES})
target_include_directories(LevelEditor PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(LevelEditor PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(LevelEditor PRIVATE _LEVEL_EDITOR)

target_compile_options(LevelEditor PRIVATE /W0)
target_compile_definitions(LevelEditor PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(LevelEditor PUBLIC xrCore)
target_link_libraries(LevelEditor PUBLIC xrEUI)
target_link_libraries(LevelEditor PUBLIC xrEProps)
target_link_libraries(LevelEditor PUBLIC xrECore)
target_link_libraries(LevelEditor PUBLIC xrSound)
target_link_libraries(LevelEditor PUBLIC xrAbstractions)
target_link_libraries(LevelEditor PUBLIC "Shell32.lib")
add_dependencies(LevelEditor xrGame)
add_dependencies(LevelEditor xrSE_Factory)

# Move precompiled header
target_validate_pch(LevelEditor ${CMAKE_CURRENT_SOURCE_DIR})


## LibOGG
target_link_libraries(LevelEditor PRIVATE ${SND_OGG}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libogg.lib)
target_include_directories(LevelEditor PRIVATE "${SND_OGG}/native/include/")

## Vorbis
target_link_libraries(LevelEditor PRIVATE ${SND_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbisfile.lib)
target_link_libraries(LevelEditor PRIVATE ${SND_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbis.lib)
target_include_directories(LevelEditor PRIVATE "${SND_VOB}/native/include/")
