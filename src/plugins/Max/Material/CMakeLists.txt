project(xrMax_Materials)

# Files import
file(GLOB_RECURSE MAX_MATERIAL_SOURCE_ALL_FILES
    "*.*"
)
file(GLOB_RECURSE MAX_MATERIAL_SHARED_FILES
    "../../Shared/BlenderListLoader.cpp"
    "../../Shared/BlenderListLoader.h"
    "../../Shared/ELog.cpp"
    "../../Shared/ELog.h"
    "../../Shared/GameMaterial.cpp"
    "../../Shared/GameMaterial.h"
    "../../Shared/GameMtlLib.cpp"
    "../../Shared/GameMtlLib.h"
    "../../Shared/TexMaps.cpp"
    "../../Shared/Shader_xrLC.h"
    "../../Shared/GameMaterial.def"
    "../../Shared/BlenderListLoader.cpp"
    "../../Shared/BlenderListLoader.h"
    "../../Shared/ELog.cpp"
    "../../Shared/ELog.h"
    "../../Shared/GameMaterial.cpp"
    "../../Shared/GameMaterial.h"
    "../../Shared/GameMtlLib.cpp"
    "../../Shared/GameMtlLib.h"
    "../../Shared/TexMaps.*"
    "../../Shared/MaxStringAdapter.*"
    "../../Shared/Shader_xrLC.h"
    "../../Shared/GameMaterial.def"
)

set(MAX_MATERIAL_SRC_FILES)

source_group("Shared" FILES ${MAX_MATERIAL_SHARED_FILES})
source_group("Src" FILES ${MAX_MATERIAL_SOURCE_ALL_FILES})

# Apply list
list(APPEND MAX_MATERIAL_SRC_FILES ${MAX_MATERIAL_SOURCE_ALL_FILES})
list(APPEND MAX_MATERIAL_SRC_FILES ${MAX_MATERIAL_SHARED_FILES})

# Remove unused files
list(FILTER MAX_MATERIAL_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# xrMax_Materials project
add_library(xrMax_Materials SHARED ${MAX_MATERIAL_SRC_FILES})
target_include_directories(xrMax_Materials PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrMax_Materials PUBLIC "${IXR_3DS_MAX_SDK}/include")
target_link_directories(xrMax_Materials PUBLIC "${IXR_3DS_MAX_SDK}/lib/x64/Release")
target_precompile_headers(xrMax_Materials PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrMax_Materials PRIVATE _MAX_MATERIAL)
target_compile_definitions(xrMax_Materials PRIVATE _MAX_PLUGIN)
target_compile_definitions(xrMax_Materials PRIVATE _WINDOWS)
target_compile_definitions(xrMax_Materials PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrMax_Materials PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrMax_Materials PUBLIC xrCore)
target_link_libraries(xrMax_Materials PUBLIC "mesh.lib")
target_link_libraries(xrMax_Materials PUBLIC "core.lib")
target_link_libraries(xrMax_Materials PUBLIC "maxutil.lib")
target_link_libraries(xrMax_Materials PUBLIC "maxscrpt.lib")
target_link_libraries(xrMax_Materials PUBLIC "ManipSys.lib")
target_link_libraries(xrMax_Materials PUBLIC "paramblk2.lib")
target_link_libraries(xrMax_Materials PUBLIC "geom.lib")
target_link_libraries(xrMax_Materials PUBLIC "gfx.lib")
target_link_libraries(xrMax_Materials PUBLIC "comctl32.lib")

# Move precompiled header
set_target_properties(xrMax_Materials PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
set_target_properties(xrMax_Materials PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
target_precompile_headers(xrMax_Materials PRIVATE "stdafx.h")

file(GLOB_RECURSE MAX_MATERIAL_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${MAX_MATERIAL_SOURCE_PCH_FILES})
