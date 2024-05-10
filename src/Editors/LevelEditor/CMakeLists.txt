project(LevelEditor)

# Files import
file(GLOB_RECURSE ELE_SOURCE_ALL_FILES CONFIGURE_DEPENDS "*.*")
file(GLOB_RECURSE ELE_SOURCE_AI_FILES CONFIGURE_DEPENDS "AI/*.*")
file(GLOB_RECURSE ELE_SOURCE_UI_FILES CONFIGURE_DEPENDS "UI/*.*")
file(GLOB_RECURSE ELE_SOURCE_EDITOR_FILES CONFIGURE_DEPENDS "Editor/*.*")
file(GLOB_RECURSE ELE_SOURCE_ENGINE_FILES CONFIGURE_DEPENDS "Engine/*.*")

set(ELE_SRC_FILES)

source_group("ai" FILES ${ELE_SOURCE_AI_FILES})
source_group("editor" FILES ${ELE_SOURCE_EDITOR_FILES})
source_group("engine" FILES ${ELE_SOURCE_ENGINE_FILES})
source_group("ui" FILES ${ELE_SOURCE_ENGINE_FILES})

# Apply list
list(APPEND ELE_SRC_FILES ${ELE_SOURCE_ALL_FILES})

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

# Move precompiled header
set_target_properties(LevelEditor PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

file(GLOB_RECURSE ELE_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${ELE_SOURCE_PCH_FILES})

target_include_directories(LevelEditor PUBLIC "${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/include/")
target_include_directories(LevelEditor PUBLIC "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_win10.2023.4.28.1/include/")