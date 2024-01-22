project(xrLW_Export)

# Files import
file(GLOB_RECURSE LW_EXPORT_SOURCE_ALL_FILES
    "bone_lw.cpp"          
    "envtest.c"            
    "LW_export_keys.cpp"   
    "LW_export_motion.cpp" 
    "LW_export_object.cpp" 
    "motion_lw.cpp"        
    "scenscan/objectdb.cpp"
    "scenscan/ptsearch.cpp"
    "scenscan/surface.cpp" 
    "scenscan/vecmat.cpp"  
    "scenscan/vmap.cpp"    
    "stdafx.cpp"           
)

file(GLOB_RECURSE LW_EXPORT_SHARED_FILES
    "../../../editors/ECore/Editor/EditMesh.cpp"
    "../../../editors/ECore/Editor/EditMeshIO.cpp"
    "../../../editors/ECore/Editor/EditMeshModify.cpp"
    "../../../editors/ECore/Editor/EditObject.cpp"
    "../../../editors/ECore/Editor/EditObjectImport.cpp"
    "../../../editors/ECore/Editor/EditObjectIO.cpp"
    "../../../editors/ECore/Editor/EditObjectMotions.cpp"
    "../../../editors/ECore/Editor/ExportObjectOGF.cpp"
    "../../../editors/ECore/Editor/ExportSkeleton.cpp"
    "../../Shared/ELog.cpp"
    "../../Shared/bone.cpp"
    "../../Shared/Envelope.cpp"
    "../../Shared/interp.cpp"
    "../../Shared/motion.cpp"
)

set(LW_EXPORT_SRC_FILES)

source_group("Shared" FILES ${LW_EXPORT_SHARED_FILES})
source_group("Src" FILES ${LW_EXPORT_SOURCE_ALL_FILES})

# Apply list
list(APPEND LW_EXPORT_SRC_FILES ${LW_EXPORT_SOURCE_ALL_FILES})
list(APPEND LW_EXPORT_SRC_FILES ${LW_EXPORT_SHARED_FILES})

# Remove unused files
#list(FILTER LW_EXPORT_SRC_FILES EXCLUDE REGEX "ExportKeys.cpp$")

# xrLW_Export project
add_library(xrLW_Export SHARED ${LW_EXPORT_SRC_FILES})
target_include_directories(xrLW_Export PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrLW_Export PUBLIC "../../Shared/")
target_include_directories(xrLW_Export PUBLIC "${IXR_LW_SDK}/include")

set_target_properties(
    xrLW_Export
    PROPERTIES
        OUTPUT_NAME "ixray_export"
        SUFFIX ".p"
)

# Project defines
target_compile_definitions(xrLW_Export PRIVATE _USRDLL)
target_compile_definitions(xrLW_Export PRIVATE _LW_EXPORT)
target_compile_definitions(xrLW_Export PRIVATE _LW_PLUGIN)

add_compile_options(/fp:fast)
target_compile_definitions(xrLW_Export PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrLW_Export PUBLIC xrCore)
target_link_libraries(xrLW_Export PUBLIC lwo)
target_link_libraries(xrLW_Export PUBLIC xrLW_Server)

# Move precompiled header
set_target_properties(xrLW_Export PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

file(GLOB_RECURSE LW_EXPORT_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${LW_EXPORT_SOURCE_PCH_FILES})
