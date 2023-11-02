project(xrMax_Export)

# Files import
file(GLOB_RECURSE MAX_EXPORT_SOURCE_ALL_FILES
    "*.*"
)
file(GLOB_RECURSE MAX_EXPORT_SHARED_FILES
    "../../../editors/ECore/Editor/EditMesh.cpp"
    "../../../editors/ECore/Editor/EditMeshIO.cpp"
    "../../../editors/ECore/Editor/EditMeshModify.cpp"
    "../../../editors/ECore/Editor/EditObject.cpp"
    "../../../editors/ECore/Editor/EditObjectIO.cpp"
    "../../../editors/ECore/Editor/EditObjectMotions.cpp"
    "../../../editors/ECore/Editor/ExportObjectOGF.cpp"
    "../../Shared/ELog.cpp"
    "../../Shared/bone.cpp"
    "../../Shared/Envelope.cpp"
    "../../Shared/Helper.cpp"
    "../../Shared/interp.cpp"
    "../../Shared/motion.cpp"
    "../../Shared/MaxStringAdapter.*"
)

set(MAX_EXPORT_SRC_FILES)

source_group("Shared" FILES ${MAX_EXPORT_SHARED_FILES})
source_group("Src" FILES ${MAX_EXPORT_SOURCE_ALL_FILES})

# Apply list
list(APPEND MAX_EXPORT_SRC_FILES ${MAX_EXPORT_SOURCE_ALL_FILES})
list(APPEND MAX_EXPORT_SRC_FILES ${MAX_EXPORT_SHARED_FILES})

# Remove unused files
list(FILTER MAX_EXPORT_SRC_FILES EXCLUDE REGEX "ExportKeys.cpp$")

# xrMax_Export project
add_library(xrMax_Export SHARED ${MAX_EXPORT_SRC_FILES})
target_include_directories(xrMax_Export PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrMax_Export PUBLIC "../../Shared/")
target_include_directories(xrMax_Export PUBLIC "${IXR_3DS_MAX_SDK}/include")
target_include_directories(xrMax_Export PUBLIC "${IXR_3DS_MAX_SDK}/include/cs")
target_link_directories(xrMax_Export PUBLIC "${IXR_3DS_MAX_SDK}/lib/x64/Release")
target_precompile_headers(xrMax_Export PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrMax_Export PRIVATE _MAX_EXPORT)
target_compile_definitions(xrMax_Export PRIVATE _MAX_PLUGIN)
target_compile_definitions(xrMax_Export PRIVATE _WINDOWS)
target_compile_definitions(xrMax_Export PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrMax_Export PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrMax_Export PUBLIC xrCore)
target_link_libraries(xrMax_Export PUBLIC "mesh.lib")
target_link_libraries(xrMax_Export PUBLIC "core.lib")
target_link_libraries(xrMax_Export PUBLIC "maxutil.lib")
target_link_libraries(xrMax_Export PUBLIC "maxscrpt.lib")
target_link_libraries(xrMax_Export PUBLIC "ManipSys.lib")
target_link_libraries(xrMax_Export PUBLIC "paramblk2.lib")
target_link_libraries(xrMax_Export PUBLIC "geom.lib")
target_link_libraries(xrMax_Export PUBLIC "gfx.lib")
target_link_libraries(xrMax_Export PUBLIC "comctl32.lib")

# Move precompiled header
set_target_properties(xrMax_Export PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
set_target_properties(xrMax_Export PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
target_precompile_headers(xrMax_Export PRIVATE "stdafx.h")

file(GLOB_RECURSE MAX_EXPORT_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${MAX_EXPORT_SOURCE_PCH_FILES})
