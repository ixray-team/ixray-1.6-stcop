project(EditorEngineAPI)

# Files import
file(GLOB_RECURSE ED_ENGINE_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)

file(GLOB_RECURSE ED_ENGINE_SOURCE_API_FILES CONFIGURE_DEPENDS
    "../../xrEngine/defines.*"
    "../../xrEngine/Envelope.*"
    "../../xrEngine/fmesh.*"
    "../../xrEngine/GameFont.*"
    "../../xrEngine/GameMtlLib.*"
    "../../xrEngine/IGame_Persistent.*"
    "../../xrEngine/IInputReceiver.*"
    "../../xrEngine/interp.*"
    "../../xrEngine/motion.*"
    "../../xrEngine/pure.*"
    "../../xrEngine/Xr_input.*"
    "../../xrEngine/xrImage_Resampler.*"
    "../../xrEngine/SkeletonMotions.*"
    "../../xrEngine/tntQAVI.*"
    "../../xrEngine/perlin.*"
    "../../xrEngine/xrTheora_Surface.*"
    "../../xrEngine/xrTheora_Stream.*"
    "../../xrEngine/LightAnimLibrary.*"
    "../../xrEngine/xrHemisphere.*"
    "../../xrEngine/device.*"
    "../../xrEngine/Device_create_render.*"
    "../../xrEngine/Device_create_render_dx9.*"
)

set(ED_ENGINE_SRC_FILES)

source_group("src" FILES ${ED_ENGINE_SOURCE_ALL_FILES})
source_group("api" FILES ${ED_ENGINE_SOURCE_API_FILES})

# Apply list
list(APPEND ED_ENGINE_SRC_FILES ${ED_ENGINE_SOURCE_ALL_FILES})
list(APPEND ED_ENGINE_SRC_FILES ${ED_ENGINE_SOURCE_API_FILES})

# Remove unused files
list(FILTER ED_ENGINE_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# EditorEngineAPI project
add_library(EditorEngineAPI ${ED_ENGINE_SRC_FILES})
target_include_directories(EditorEngineAPI PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(EditorEngineAPI PRIVATE "${IXRAY_SDK_INC}/freetype")

# Project defines
target_compile_definitions(EditorEngineAPI PRIVATE _USRDLL)

target_compile_definitions(EditorEngineAPI PRIVATE "$<$<CONFIG:Debug>:DEBUG>")
target_compile_definitions(EditorEngineAPI PUBLIC "_EDITOR")

# Linker list
target_link_libraries(EditorEngineAPI PUBLIC xrCore)
target_link_libraries(EditorEngineAPI PUBLIC xrCDB)
target_link_libraries(EditorEngineAPI PUBLIC imgui)
target_link_libraries(EditorEngineAPI PUBLIC "Vfw32.lib")
target_link_libraries(EditorEngineAPI PUBLIC "Winmm.lib")

# Move precompiled header
set_target_properties(EditorEngineAPI PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
set_target_properties(EditorEngineAPI PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
target_precompile_headers(EditorEngineAPI PRIVATE "stdafx.h")

file(GLOB_RECURSE ED_ENGINE_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${ED_ENGINE_SOURCE_PCH_FILES})

## Theora
target_link_libraries(EditorEngineAPI PUBLIC ${ENGINE_THRA}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libtheora.lib)
target_include_directories(EditorEngineAPI PUBLIC "${ENGINE_THRA}/native/include/")

## OGG
target_link_libraries(EditorEngineAPI PUBLIC ${SND_OGG}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libogg.lib)
target_include_directories(EditorEngineAPI PUBLIC "${SND_OGG}/native/include/")

## FreeType
target_link_libraries(EditorEngineAPI PUBLIC ${ENGINE_FREETYPE}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/freetype.lib)
target_include_directories(EditorEngineAPI PUBLIC "${ENGINE_FREETYPE}native/include/")