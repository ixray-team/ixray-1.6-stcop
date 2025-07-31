project(xrLC)
set(XLC_SRC_FILES)

# Files import
file(GLOB_RECURSE XLC_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
    "../xrDXT/tga.cpp"
    "../../xrEngine/xrLoadSurface.cpp"
    "../../common/NvMender2003/NVMeshMender.cpp"
)

file(GLOB_RECURSE XLC_SOURCE_RES_FILES
    "*.ico"
    "*.rc"
    "*.bmp"
)

file(GLOB_RECURSE XLC_SOURCE_BUILD_FILES
    "xrBuild*"
    "xrPhase_*"
    "Build*"
    "Build_*"
    "xrLight.*"
    "xrSectors.*"
    "xrOptimizeCFORM_qslim.*"
    "xrOptimizeCFORM_qslim.*"
    "xrPreOptimize.*"
)

file(GLOB_RECURSE XLC_SOURCE_NVIDIA_FILES
    "nv_*"
    "nv*"
    "VertexCache.*"
    "xrHemisphere.h"
    "xrOcclusion.h"
    "MeshMender*"
)

file(GLOB_RECURSE XLC_SOURCE_OGF_FILES
    "OGF_*"
    "xrFlex2OGF.*"
    "fmesh.*"
    "progmesh.*"
)

file(GLOB_RECURSE XLC_SOURCE_MUMODELS_FILES
    "xrMU_*"
)
 
# Source groups
source_group("kernel" FILES ${XLC_SOURCE_ALL_FILES})
source_group("res" FILES ${XLC_SOURCE_RES_FILES})
source_group("build" FILES ${XLC_SOURCE_BUILD_FILES})
source_group("nvidia" FILES ${XLC_SOURCE_NVIDIA_FILES})
source_group("ogf" FILES ${XLC_SOURCE_OGF_FILES})
source_group("ogf\\mumodels" FILES ${XLC_SOURCE_MUMODELS_FILES})


# Apply list
list(APPEND XLC_SRC_FILES ${XLC_SOURCE_ALL_FILES})
list(APPEND XLC_SRC_FILES ${XLC_SOURCE_RES_FILES})
list(APPEND XLC_SRC_FILES ${XLC_SOURCE_BUILD_FILES})
list(APPEND XLC_SRC_FILES ${XLC_SOURCE_NVIDIA_FILES})
list(APPEND XLC_SRC_FILES ${XLC_SOURCE_OGF_FILES})


# Remove unused files
list(FILTER XLC_SRC_FILES EXCLUDE REGEX "xrGameMaterials.h")
list(FILTER XLC_SRC_FILES EXCLUDE REGEX "PropSlimTools.h")
list(FILTER XLC_SRC_FILES EXCLUDE REGEX "std_classes.h")
list(FILTER XLC_SRC_FILES EXCLUDE REGEX "cl_collector.*")
list(FILTER XLC_SRC_FILES EXCLUDE REGEX "cl_intersect.*")

# xrLC project
add_library(xrLC ${XLC_SRC_FILES})
target_include_directories(xrLC PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(xrLC PRIVATE "stdafx.h")

# Linker list
target_link_libraries(xrLC PUBLIC xrCore)
target_link_libraries(xrLC PUBLIC xrCDB)
target_link_libraries(xrLC PUBLIC xrLC_Light)
target_link_libraries(xrLC PUBLIC QSlim)
target_link_libraries(xrLC PUBLIC ETools)
target_link_libraries(xrLC PUBLIC MagicFM)
target_link_libraries(xrLC PUBLIC NvMender2003)